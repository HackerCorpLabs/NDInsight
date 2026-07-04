using System;

using NDInsight.Sintran.Xmsg.Packet;

namespace NDInsight.Sintran.Xmsg.Node.Tad
{
    /// <summary>
    /// The connect-to CLIENT (asker) side of a TAD terminal session: builds the frames that drive a
    /// connect-to against a <see cref="TadTerminalResponder"/> — the directory letter, the
    /// session-setup, the terminal-setup negotiation, and the typed keystroke lines — and tracks its
    /// own outgoing datagram sequence. The mirror of the server-side responder.
    /// </summary>
    /// <remarks>
    /// <para><b>Envelope model</b></para>
    /// Every frame is stamped with the shared seed model (<see cref="XmsgEnvelope"/>): the Counter and
    /// channel are derived from the link seed plus this client's own Flags1 sequence, exactly as the
    /// responder derives its own. Both nodes share the seed, so an in-memory pairing of a client and a
    /// responder is a coherent conversation without a real machine.
    /// <para><b>TAD content</b></para>
    /// All TAD chains are built through <see cref="TadMessageBuilder"/> (typed, word-aligned) — no
    /// hand-built byte arrays. The concrete negotiation values (TMOD flags, terminal type, OPSV
    /// version) are OBSERVED from a single capture and will be refined as the TAD spec firms up; they
    /// are isolated here so that refinement is a local change.
    /// </remarks>
    public sealed class TadConnectClient
    {
        private const uint XroutSetupControlService = 0x04000041u;   // XSLET directory letter
        private const uint SessionSetupControlService = 0x04000000u; // 06/1B/1C/FF negotiation chain
        private const uint TerminalDataControlService = 0x01080000u; // DC/TAD terminal data

        private readonly ushort _clientNode;
        private readonly ushort _serverNode;
        private readonly ushort _clientPort;
        private readonly byte _seed;

        private ushort _flags1;
        private ushort _serverSessionPort;

        /// <summary>
        /// Initialises the client for a client/server node pair.
        /// </summary>
        /// <param name="clientNode">
        /// This client's node number (for example 100).
        /// </param>
        /// <param name="serverNode">
        /// The server's node number (for example 102).
        /// </param>
        /// <param name="clientPort">
        /// The client's session-source port (its allocated TAD port on the wire).
        /// </param>
        /// <param name="seed">
        /// The shared link seed (100↔102 = <c>0x14</c>); the responder learns the same value from the
        /// connect frame this client builds.
        /// </param>
        public TadConnectClient(ushort clientNode, ushort serverNode, ushort clientPort, byte seed)
        {
            _clientNode = clientNode;
            _serverNode = serverNode;
            _clientPort = clientPort;
            _seed = seed;
            _flags1 = 0;
            // Default until learned from the port-assign; the responder does not validate the dest port.
            _serverSessionPort = 0x0211;
        }

        /// <summary>
        /// Gets the next Flags1 (datagram sequence) this client will send.
        /// </summary>
        public ushort NextFlags1
        {
            get { return _flags1; }
        }

        /// <summary>
        /// Builds the connect-to request: an XSLET directory letter naming the remote (opens the session).
        /// </summary>
        /// <param name="remoteName">
        /// The target remote name (for example <c>"D102"</c>).
        /// </param>
        /// <returns>
        /// The connect-request frame.
        /// </returns>
        public XmsgFrame BuildConnect(string remoteName)
        {
            byte[] letter = TadSession.BuildDirectoryLetterBody(remoteName);
            // role low nibble 4 = asker; frame-class 0x0400 (control), XMCSM XSLET.
            return Assemble(frameClass: 0x0400, controlService: XroutSetupControlService,
                frameFlags: 0x86, role: 0xE4, sourcePort: _clientPort, destinationPort: 0x0000, payload: letter);
        }

        /// <summary>
        /// Builds the session-setup frame (the 06/1B/1C/FF negotiation chain) sent after the accept.
        /// </summary>
        /// <returns>
        /// The session-setup frame.
        /// </returns>
        public XmsgFrame BuildSessionSetup()
        {
            byte[] tad = new TadMessageBuilder()
                .Raw(0x06, ReadOnlySpan<byte>.Empty)
                .Raw(0x1B, ReadOnlySpan<byte>.Empty)
                .Raw(0x1C, new byte[] { 0x00 })
                .Raw(0xFF, ReadOnlySpan<byte>.Empty)
                .Build();
            return Assemble(frameClass: 0x0400, controlService: SessionSetupControlService,
                frameFlags: 0x86, role: 0x84, sourcePort: _clientPort, destinationPort: 0x0156, payload: tad);
        }

        /// <summary>
        /// Builds the terminal-setup frame (TMOD/TTYP/DESC/OPSV) that triggers the server's MOTD burst.
        /// </summary>
        /// <returns>
        /// The terminal-setup frame.
        /// </returns>
        public XmsgFrame BuildTerminalSetup()
        {
            // OBSERVED negotiation values (single capture): TMOD 0x08, TTYP 0x0000, DESC 0x1B,
            // OPSV L(0x4C)/0x01/0x04. Refined later as the TAD spec firms up.
            byte[] tad = new TadMessageBuilder()
                .Tmod(0x08)
                .Ttyp(0x0000)
                .Desc(0x1B)
                .Opsv(0x4C, 0x01, 0x04)
                .Build();
            return Assemble(frameClass: 0x0108, controlService: TerminalDataControlService,
                frameFlags: 0x86, role: 0x84, sourcePort: _clientPort, destinationPort: _serverSessionPort, payload: tad);
        }

        /// <summary>
        /// Builds a keystroke line as a BDAT terminal-input frame.
        /// </summary>
        /// <param name="text">
        /// The line the user typed (for example <c>"help"</c>). Sent as 7-bit ASCII; the server strips
        /// the parity bit either way.
        /// </param>
        /// <returns>
        /// The terminal-input frame.
        /// </returns>
        public XmsgFrame BuildInput(string text)
        {
            byte[] tad = new TadMessageBuilder().BdatText(text).Build();
            return Assemble(frameClass: 0x0108, controlService: TerminalDataControlService,
                frameFlags: 0x96, role: 0x84, sourcePort: _clientPort, destinationPort: _serverSessionPort, payload: tad);
        }

        /// <summary>
        /// Learns session state from a frame the server sent (currently the server's session port,
        /// taken from the source port of its terminal-data frames).
        /// </summary>
        /// <param name="frame">
        /// A frame received from the server.
        /// </param>
        public void NoteServerFrame(XmsgFrame frame)
        {
            if (frame?.SubHeader == null)
            {
                return;
            }

            // The server's terminal-data frames (XMCSM 0x01080000) carry its session port as the source.
            if (frame.SubHeader.ControlService == TerminalDataControlService && frame.SubHeader.SourcePort != 0)
            {
                _serverSessionPort = frame.SubHeader.SourcePort;
            }
        }

        /// <summary>
        /// Assembles one client frame with the shared seed-model envelope, then advances Flags1.
        /// </summary>
        /// <param name="frameClass">
        /// The Flags2 frame-class word (also the XMCSM top half).
        /// </param>
        /// <param name="controlService">
        /// The XMCSM control/service word.
        /// </param>
        /// <param name="frameFlags">
        /// The sub-header frame-flags byte.
        /// </param>
        /// <param name="role">
        /// The sub-header role byte.
        /// </param>
        /// <param name="sourcePort">
        /// Our source port.
        /// </param>
        /// <param name="destinationPort">
        /// The server destination port.
        /// </param>
        /// <param name="payload">
        /// The trailer payload (letter body or TAD chain).
        /// </param>
        /// <returns>
        /// The assembled frame.
        /// </returns>
        private XmsgFrame Assemble(
            ushort frameClass, uint controlService, byte frameFlags, byte role,
            ushort sourcePort, ushort destinationPort, byte[] payload)
        {
            ushort f1 = _flags1;
            byte counter = XmsgEnvelope.ComputeCounter(_seed, f1, frameClass);
            SintranProtocolId channel = XmsgEnvelope.DeriveChannel(_seed, f1, frameClass, controlService);
            _flags1 = (ushort)(f1 + 1);

            XmsgFrame frame = new XmsgFrame();
            frame.Header.Marker1 = SintranHeader.Marker1Value;
            frame.Header.Marker2 = SintranHeader.Marker2Normal;
            frame.Header.PacketType = 0x00;
            frame.Header.Subtype = SintranPacketSubtype.Data;
            frame.Header.DestinationNode = _serverNode;
            frame.Header.SourceNode = _clientNode;
            frame.Header.Flags1 = f1;
            frame.Header.Flags2 = frameClass;
            frame.Header.ProtocolId = channel;

            XmsgSubHeader sub = new XmsgSubHeader();
            sub.Counter = counter;
            sub.FrameFlags = frameFlags;
            sub.Role = role;
            sub.DestinationSystem = _serverNode;
            sub.DestinationPort = destinationPort;
            sub.SourceSystem = _clientNode;
            sub.SourcePort = sourcePort;
            sub.ControlService = controlService;
            sub.Pad = 0x00;
            sub.UserDataLength = (byte)payload.Length;

            frame.SubHeader = sub;
            frame.TrailingBytes = payload;
            frame.ClearRawBytes();
            return frame;
        }
    }
}
