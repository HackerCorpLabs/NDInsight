using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Packet;

namespace NDInsight.Sintran.Xmsg.Node.Services
{
    /// <summary>
    /// The node-side host that routes XMSG server traffic to registered <see cref="IXmsgServer"/>s and
    /// gives them the low-level <see cref="IXmsgTransport"/> to reply with. It owns the per-remote-node
    /// links (seed + continuous Flags 1), allocates session ports and numbers, and performs the XROUT
    /// port-0 dispatch on the XMCSM low byte.
    /// </summary>
    /// <remarks>
    /// <para><b>Dispatch (XMSG-PROTOCOL.md section 7 / 22).</b></para>
    /// Server requests arrive at port 0 and fork on the XMCSM low "service" byte:
    ///  - <c>0x41</c> (XSLET, "send a letter") - the letter is parsed for its target name and routed to
    ///    the matching registered server. A connect-to and a list-systems query are both XSLET letters.
    ///  - <c>0x4B</c> (XSGSY, "get routing info") - list-route; answered by the node's routing server, not
    ///    a named server, so this host returns nothing for it and the node handles it.
    /// After the accept, session traffic is ports-only: a datagram to a non-zero port is routed to the
    /// server that <see cref="IXmsgServer.OwnsPort"/>s it (the server's well-known reply-from port for the
    /// session-setup, then the session port the accept advertised for terminal data).
    /// </remarks>
    public sealed class XmsgServerHost : IXmsgTransport
    {
        private readonly ushort _nodeNumber;
        private readonly IResponderSequenceStore _store;
        private readonly List<IXmsgServer> _servers;
        private readonly Dictionary<ushort, XmsgLink> _links;

        // Session port allocation preserves the live-verified first-session value 0x0211 and increments
        // the incarnation for each further session, so ports are unique across servers and sessions.
        private ushort _nextSessionPort;
        private int _nextSessionNumber;

        // XMCSM low-byte "service" code (XMSG-PROTOCOL.md section 9.1): send a letter (connect / list-systems).
        private const byte XsletServiceByte = 0x41;

        // The connect letter (and every server request) is addressed to XROUT's well-known port 0.
        private const ushort XroutRequestPort = 0x0000;

        /// <summary>
        /// Initialises the host for a node.
        /// </summary>
        /// <param name="nodeNumber">
        /// This node's number (for example 102).
        /// </param>
        /// <param name="store">
        /// The persistent outgoing-sequence store (per remote node). When null, a non-persisting store is
        /// used (every link starts at 0x0000).
        /// </param>
        public XmsgServerHost(ushort nodeNumber, IResponderSequenceStore? store = null)
        {
            _nodeNumber = nodeNumber;
            _store = store ?? new NullResponderSequenceStore();
            _servers = new List<IXmsgServer>();
            _links = new Dictionary<ushort, XmsgLink>();
            _nextSessionPort = (ushort)((4 << 7) | 0x11);   // 0x0211, the live-verified layout
            _nextSessionNumber = 1;
        }

        /// <summary>
        /// Gets this node's number.
        /// </summary>
        public ushort NodeNumber
        {
            get { return _nodeNumber; }
        }

        /// <summary>
        /// Registers a server (for example the TAD <c>*TADADM</c> server).
        /// </summary>
        /// <param name="server">
        /// The server to register.
        /// </param>
        public void Register(IXmsgServer server)
        {
            if (server == null)
            {
                throw new ArgumentNullException(nameof(server));
            }

            _servers.Add(server);
        }

        /// <summary>
        /// Routes an incoming datagram to the owning server and returns its reply frames. Returns an
        /// empty list when no server owns the datagram (for example an XSGSY list-route request, which
        /// the node answers itself).
        /// </summary>
        /// <param name="incoming">
        /// The received datagram (subtype Data).
        /// </param>
        /// <returns>
        /// The reply frames, in order (empty when unrouted).
        /// </returns>
        public IReadOnlyList<XmsgFrame> Route(XmsgFrame incoming)
        {
            if (incoming == null || incoming.Header == null || incoming.SubHeader == null)
            {
                return Array.Empty<XmsgFrame>();
            }

            // Learn / refresh the link seed from any data frame so BuildDatagram can derive counters.
            EnsureLink(incoming);

            ushort destPort = incoming.SubHeader.DestinationPort;
            if (destPort == XroutRequestPort)
            {
                // Port 0: fork on the XMCSM low "service" byte.
                byte serviceByte = (byte)(incoming.SubHeader.ControlService & 0xFF);
                if (serviceByte == XsletServiceByte)
                {
                    // XSLET letter - route by the target name inside the letter.
                    string name = ExtractLetterName(incoming);
                    IXmsgServer? byName = FindByName(name);
                    if (byName != null)
                    {
                        return byName.Handle(incoming, this);
                    }
                }

                // XSGSY (list-route) and unknown letters are not server-routed here.
                return Array.Empty<XmsgFrame>();
            }

            // Non-zero port: session traffic, routed to the server that owns that port.
            IXmsgServer? byPort = FindByPort(destPort);
            if (byPort != null)
            {
                return byPort.Handle(incoming, this);
            }

            return Array.Empty<XmsgFrame>();
        }

        /// <summary>
        /// Drains every registered server's queued asynchronous output (tty inject / wall) into frames.
        /// The node calls this each pump cycle so queued text flushes to the remote clients.
        /// </summary>
        /// <returns>
        /// The queued frames from all servers, in order (empty when nothing is pending).
        /// </returns>
        public IReadOnlyList<XmsgFrame> DrainPending()
        {
            List<XmsgFrame> all = new List<XmsgFrame>();
            for (int i = 0; i < _servers.Count; i++)
            {
                IReadOnlyList<XmsgFrame> part = _servers[i].DrainPending(this);
                for (int j = 0; j < part.Count; j++)
                {
                    all.Add(part[j]);
                }
            }

            return all;
        }

        /// <summary>
        /// Records that a remote node ACKed one of our frames, advancing the persisted next-sequence for
        /// that link to <c>ackedFlags1 + 1</c> (never past what was actually received).
        /// </summary>
        /// <param name="remoteNode">
        /// The node that ACKed.
        /// </param>
        /// <param name="ackedFlags1">
        /// The Flags 1 the ACK echoes.
        /// </param>
        public void ConfirmDelivered(ushort remoteNode, ushort ackedFlags1)
        {
            if (ackedFlags1 == 0xFFFF)
            {
                return;
            }

            ushort next = (ushort)(ackedFlags1 + 1);
            ushort current = _store.LoadNextFlags1(remoteNode);
            if (next > current)
            {
                _store.SaveNextFlags1(remoteNode, next);
            }
        }

        /// <summary>
        /// Resets a link's outgoing sequence to 0x0000 (a peer XMSG restart), dropping any in-memory
        /// link state so the next contact reloads from the reset store.
        /// </summary>
        /// <param name="remoteNode">
        /// The node that (re)started.
        /// </param>
        public void ResetSequence(ushort remoteNode)
        {
            _store.SaveNextFlags1(remoteNode, 0x0000);
            _links.Remove(remoteNode);
        }

        /// <summary>
        /// Allocates a globally-unique session port.
        /// </summary>
        /// <returns>
        /// A fresh session port.
        /// </returns>
        public ushort AllocateSessionPort()
        {
            ushort port = _nextSessionPort;
            _nextSessionPort = (ushort)(_nextSessionPort + 1);
            return port;
        }

        /// <summary>
        /// Allocates a monotonic session number (the operator-visible ttyN / TAD number).
        /// </summary>
        /// <returns>
        /// A fresh 1-based session number.
        /// </returns>
        public int AllocateSessionNumber()
        {
            int number = _nextSessionNumber;
            _nextSessionNumber++;
            return number;
        }

        /// <summary>
        /// Builds one outgoing datagram to a client endpoint, assigning the per-link Flags 1 and deriving
        /// the Counter and channel from the envelope model.
        /// </summary>
        /// <param name="remoteNode">The client's node number.</param>
        /// <param name="clientSystem">The client's system number.</param>
        /// <param name="clientPort">The client's port.</param>
        /// <param name="sourcePort">Our source port.</param>
        /// <param name="controlService">The XMCSM control/service word.</param>
        /// <param name="frameFlags">The sub-header frame-flags byte.</param>
        /// <param name="role">The sub-header role byte.</param>
        /// <param name="payload">The trailer payload bytes.</param>
        /// <returns>The assembled datagram.</returns>
        /// <exception cref="InvalidOperationException">
        /// Thrown when no link to <paramref name="remoteNode"/> exists.
        /// </exception>
        public XmsgFrame BuildDatagram(
            ushort remoteNode,
            ushort clientSystem,
            ushort clientPort,
            ushort sourcePort,
            uint controlService,
            byte frameFlags,
            byte role,
            byte[] payload)
        {
            if (!_links.TryGetValue(remoteNode, out XmsgLink? link))
            {
                throw new InvalidOperationException($"No XMSG link to node {remoteNode} (no seed learned).");
            }

            // Frame class is the top 16 bits of the XMCSM word (VERIFIED, 601/601 data frames).
            ushort frameClass = (ushort)(controlService >> 16);
            ushort f1 = link.NextFlags1;
            byte ctr = XmsgEnvelope.ComputeCounter(link.Seed, f1, frameClass);
            SintranProtocolId channel = XmsgEnvelope.DeriveChannel(link.Seed, f1, frameClass, controlService);

            // Advance the single continuous per-link sequence for the next originated frame.
            link.NextFlags1 = (ushort)(f1 + 1);

            XmsgFrame frame = new XmsgFrame();
            frame.Header.Marker1 = SintranHeader.Marker1Value;
            frame.Header.Marker2 = SintranHeader.Marker2Normal;
            frame.Header.PacketType = 0x00;
            frame.Header.Subtype = SintranPacketSubtype.Data;
            frame.Header.DestinationNode = remoteNode;
            frame.Header.SourceNode = _nodeNumber;
            frame.Header.Flags1 = f1;
            frame.Header.Flags2 = frameClass;
            frame.Header.ProtocolId = channel;

            XmsgSubHeader sub = new XmsgSubHeader();
            sub.Counter = ctr;
            sub.FrameFlags = frameFlags;
            sub.Role = role;
            sub.DestinationSystem = clientSystem;
            sub.DestinationPort = clientPort;
            sub.SourceSystem = _nodeNumber;
            sub.SourcePort = sourcePort;
            sub.ControlService = controlService;
            sub.Pad = 0x00;
            sub.UserDataLength = (byte)payload.Length;

            frame.SubHeader = sub;
            frame.TrailingBytes = payload;
            frame.ClearRawBytes();
            return frame;
        }

        /// <summary>
        /// Gets the seed the host learned for a link (for the node to seed its secure-ACK model), or a
        /// fallback of 0 when the link is unknown.
        /// </summary>
        /// <param name="remoteNode">
        /// The remote node.
        /// </param>
        /// <returns>
        /// The link seed, or 0 when unknown.
        /// </returns>
        public byte SeedFor(ushort remoteNode)
        {
            return _links.TryGetValue(remoteNode, out XmsgLink? link) ? link.Seed : (byte)0;
        }

        /// <summary>
        /// Ensures a link exists for the incoming frame's source node, learning the seed and loading the
        /// outgoing sequence from the store on first contact; refreshes the seed on later frames.
        /// </summary>
        /// <param name="incoming">
        /// The received data frame.
        /// </param>
        private void EnsureLink(XmsgFrame incoming)
        {
            if (incoming.Header!.Subtype != SintranPacketSubtype.Data || incoming.SubHeader == null)
            {
                return;
            }

            if (incoming.Header.Flags1 == 0xFFFF)
            {
                return;
            }

            byte seed = XmsgEnvelope.LearnSeed(
                incoming.Header.Flags1, incoming.SubHeader.Counter, incoming.Header.Flags2);
            ushort node = incoming.Header.SourceNode;

            if (_links.TryGetValue(node, out XmsgLink? link))
            {
                link.Seed = seed;
                return;
            }

            _links[node] = new XmsgLink(node, seed, _store.LoadNextFlags1(node));
        }

        /// <summary>
        /// Finds a registered server by name (case-insensitive), or null.
        /// </summary>
        /// <param name="name">
        /// The target name from an XSLET letter (for example <c>*TADADM</c>).
        /// </param>
        /// <returns>
        /// The matching server, or null.
        /// </returns>
        private IXmsgServer? FindByName(string name)
        {
            if (name.Length == 0)
            {
                return null;
            }

            for (int i = 0; i < _servers.Count; i++)
            {
                if (string.Equals(_servers[i].Name, name, StringComparison.OrdinalIgnoreCase))
                {
                    return _servers[i];
                }
            }

            return null;
        }

        /// <summary>
        /// Finds the registered server that owns a given session/reply port, or null.
        /// </summary>
        /// <param name="port">
        /// The destination wire port.
        /// </param>
        /// <returns>
        /// The owning server, or null.
        /// </returns>
        private IXmsgServer? FindByPort(ushort port)
        {
            for (int i = 0; i < _servers.Count; i++)
            {
                if (_servers[i].OwnsPort(port))
                {
                    return _servers[i];
                }
            }

            return null;
        }

        /// <summary>
        /// Extracts the target server name from an XSLET letter's trailer (the first <c>*</c>-prefixed
        /// printable-ASCII run, for example <c>*TADADM</c>).
        /// </summary>
        /// <param name="incoming">
        /// The XSLET letter frame.
        /// </param>
        /// <returns>
        /// The name including the leading <c>*</c>, or an empty string when none is found.
        /// </returns>
        private static string ExtractLetterName(XmsgFrame incoming)
        {
            byte[]? trailer = incoming.TrailingBytes;
            if (trailer == null)
            {
                return string.Empty;
            }

            System.Text.StringBuilder run = new System.Text.StringBuilder();
            for (int i = 0; i <= trailer.Length; i++)
            {
                byte b = i < trailer.Length ? trailer[i] : (byte)0x00;
                if (b >= 0x20 && b <= 0x7E)
                {
                    run.Append((char)b);
                    continue;
                }

                if (run.Length >= 2 && run[0] == '*')
                {
                    return run.ToString();
                }

                run.Clear();
            }

            return string.Empty;
        }
    }
}
