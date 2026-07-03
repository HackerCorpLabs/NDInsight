using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.ListRouting;
using NDInsight.Sintran.Xmsg.Node.Tad;

namespace NDInsight.Sintran.Xmsg.Node
{
    /// <summary>
    /// The XMSG application-layer runtime for one node: it answers reachability requests,
    /// acknowledges data frames with the secure-delivery <c>0x03</c> ACK, and holds the
    /// configurable remote-name alias table (the DEF-REMOTE / XSDRN model).
    /// </summary>
    /// <remarks>
    /// <para><b>Provenance</b></para>
    /// The reachability handshake (request subtype <c>0x19</c> answered by reply subtype
    /// <c>0x13</c> with swapped Dest/Src, <c>Flags1 = 0xFFFF</c>, <c>Flags2 = 0x0001</c>)
    /// and the ACK mechanics (subtype <c>0x03</c>, <c>Flags1</c> echoes the data frame's
    /// datagram sequence, <c>Flags2 = 0x0001</c>) are VERIFIED against captures
    /// (XMSG-PROTOCOL.md sections 5.1 and 6). The reachability-reply command byte
    /// (<c>0x0E</c>) is OBSERVED from the single captured request/reply pair. Port/magic
    /// allocation is not modelled here — INFERRED / stubbed.
    /// </remarks>
    public sealed class XmsgNode
    {
        /// <summary>
        /// The trailing command byte placed in a reachability reply.
        /// </summary>
        /// <remarks>
        /// OBSERVED: the captured 100-&gt;102 request carries trailing byte <c>0x08</c>
        /// (Sync-Request) and the 102-&gt;100 reply carries <c>0x0E</c> (Sync-Response)
        /// (XMSG-PROTOCOL.md sections 5.1 and 9.1). Only one request/reply pair exists in
        /// the corpus, so this is a fixed constant rather than a proven function of the
        /// request.
        /// </remarks>
        public const byte ReachabilityReplyCommand = 0x0E;

        /// <summary>
        /// When true (default), a received data frame is answered with the <c>0x03</c> secure
        /// ACK. Set false for observe-only operation against a live kernel that crashes on an
        /// unexpected/malformed ACK (see the note in <see cref="HandleFrame"/>).
        /// </summary>
        public bool AcknowledgeData { get; set; } = true;

        /// <summary>
        /// Optional routing table. When set, an incoming list-route (<c>XSGSY</c>) request
        /// (<c>XMCSM 0x0100014B</c>) is answered with the byte-validated
        /// <see cref="ListRoutingServer"/> reply instead of a secure ACK.
        /// </summary>
        public IRoutingTable? RoutingTable { get; set; }

        /// <summary>
        /// Optional TAD terminal responder. When set, an incoming SYSTEM-TAD connect request
        /// (<c>XMCSM 0x04000041</c>) opens a simulated remote-machine terminal session, and
        /// subsequent terminal-data (BDAT) frames drive its menu.
        /// </summary>
        public TadTerminalResponder? TadResponder { get; set; }

        /// <summary>
        /// When true, secure-ACK (subtype <c>0x03</c>, echoing Flags1) each TAD connect and
        /// session frame from the peer. DEFAULT FALSE: the experiment showed that sending a
        /// <c>0x03</c> ACK (echoing the connect's <c>DA</c> channel) CRASHES 100 (XXPER) — a real
        /// responder ACKs the connect on the <c>DD</c> (TAD) channel, so a naive echo-channel ACK
        /// is malformed. Left as a toggle for a future correctly-channelled ACK experiment.
        /// </summary>
        public bool AcknowledgeTadFrames { get; set; } = false;

        private readonly ListRoutingServer _routingServer = new ListRoutingServer();

        private readonly ushort _nodeNumber;
        private readonly Dictionary<string, ushort> _remoteNames;
        private readonly SecureDatagramReceiver _receiver;

        private ushort _outgoingDatagramSequence;

        /// <summary>
        /// Raised when a data frame is delivered to this node's application layer.
        /// </summary>
        /// <param name="datagramSequence">
        /// The datagram sequence (Flags 1) of the delivered frame.
        /// </param>
        public delegate void DataDelivered(ushort datagramSequence);

        /// <summary>
        /// Occurs when a data frame is delivered locally (before its ACK is built).
        /// </summary>
        public event DataDelivered? OnDataDelivered;

        /// <summary>
        /// Initialises a node with its number and the seed for its ACK counter.
        /// </summary>
        /// <param name="nodeNumber">
        /// This node's number (for example 100, 102 or 103).
        /// </param>
        /// <param name="ackCounter">
        /// The starting value of the per-direction counter placed in each secure ACK. It
        /// decrements per ACK (XMSG-PROTOCOL.md section 6).
        /// </param>
        public XmsgNode(ushort nodeNumber, byte ackCounter)
        {
            _nodeNumber = nodeNumber;
            _remoteNames = new Dictionary<string, ushort>(StringComparer.OrdinalIgnoreCase);
            _receiver = new SecureDatagramReceiver(ackCounter);
            _receiver.OnReceived += RaiseDelivered;
        }

        /// <summary>
        /// Gets this node's number.
        /// </summary>
        public ushort NodeNumber
        {
            get { return _nodeNumber; }
        }

        /// <summary>
        /// Gets the current per-direction ACK counter value (the byte placed in the next ACK).
        /// </summary>
        public byte AckCounter
        {
            get { return _receiver.Counter; }
        }

        /// <summary>
        /// Gets the next outgoing datagram-sequence value that a locally originated data
        /// frame would carry.
        /// </summary>
        /// <remarks>
        /// The sequence increments per data message on a direction (XMSG-PROTOCOL.md
        /// section 4.2). This node is currently receive/reply oriented, so the counter is
        /// advanced through <see cref="NextDatagramSequence"/> rather than automatically.
        /// </remarks>
        public ushort OutgoingDatagramSequence
        {
            get { return _outgoingDatagramSequence; }
        }

        /// <summary>
        /// Defines (or re-points) a remote-node name alias.
        /// </summary>
        /// <param name="name">
        /// The alias, matched case-insensitively (for example <c>"D102"</c> or <c>"main"</c>).
        /// </param>
        /// <param name="systemNumber">
        /// The system (node) number the alias resolves to. Many aliases may map to the same
        /// system number.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="name"/> is null.
        /// </exception>
        public void DefineRemote(string name, ushort systemNumber)
        {
            if (name == null)
            {
                throw new ArgumentNullException(nameof(name));
            }

            _remoteNames[name] = systemNumber;
        }

        /// <summary>
        /// Resolves a remote-node name alias to its system number.
        /// </summary>
        /// <param name="name">
        /// The alias to resolve.
        /// </param>
        /// <param name="systemNumber">
        /// When this method returns <c>true</c>, the resolved system number; otherwise 0.
        /// </param>
        /// <returns>
        /// <c>true</c> when the alias is defined; otherwise <c>false</c>.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="name"/> is null.
        /// </exception>
        public bool TryResolveRemote(string name, out ushort systemNumber)
        {
            if (name == null)
            {
                throw new ArgumentNullException(nameof(name));
            }

            return _remoteNames.TryGetValue(name, out systemNumber);
        }

        /// <summary>
        /// Advances and returns the outgoing datagram-sequence counter (modulo 16 bits).
        /// </summary>
        /// <returns>
        /// The value the next locally originated data frame should carry.
        /// </returns>
        public ushort NextDatagramSequence()
        {
            ushort value = _outgoingDatagramSequence;
            _outgoingDatagramSequence = (ushort)(_outgoingDatagramSequence + 1);
            return value;
        }

        /// <summary>
        /// Processes a received frame and returns ALL frames to send in response (zero or more).
        /// This is the multi-frame entry point the live node uses: a TAD connect needs several
        /// frames (secure ACK + connect-accept + greeting), which the single-frame
        /// <see cref="HandleFrame"/> cannot express.
        /// </summary>
        /// <param name="incoming">The decoded received frame.</param>
        /// <returns>The response frames, in transmit order (possibly empty).</returns>
        public IReadOnlyList<XmsgFrame> HandleFrames(XmsgFrame incoming)
        {
            if (incoming == null)
            {
                throw new ArgumentNullException(nameof(incoming));
            }

            if (incoming.Header == null)
            {
                throw new ArgumentNullException(nameof(incoming), "Frame header is null.");
            }

            List<XmsgFrame> result = new List<XmsgFrame>();

            // TAD terminal-responder path: only for data frames while a responder is attached.
            if (TadResponder != null
                && incoming.Header.Subtype == SintranPacketSubtype.Data
                && incoming.SubHeader != null)
            {
                // (a) A SYSTEM-TAD connect request opens the session: secure-ACK it, then send the
                //     connect-accept and the terminal greeting.
                if (TadTerminalResponder.IsConnectRequest(incoming))
                {
                    // Run OnConnect FIRST so the responder learns the per-session ACK channel
                    // (connect-channel + 4) from this frame; the ACK we build below must ride that
                    // channel, NOT the connect's own channel (echoing the connect channel is the
                    // malformed +0 ACK that crashed 100 with XXPER).
                    IReadOnlyList<XmsgFrame> setup = TadResponder.OnConnect(incoming);

                    // Seed the ACK counter to connect-counter + 0x0A so the first ACK trailing byte
                    // matches the captured value (VALIDATED: 8/13 captures, both connect captures:
                    // 0x0D->0x17, 0xCE->0xD8) and the per-ACK decrement reproduces the captured
                    // sequence. Without this the trailing byte is 0x00 — a malformed ACK that
                    // crashed 100 (XXPER).
                    if (incoming.SubHeader != null)
                    {
                        _receiver.SeedCounter((byte)(incoming.SubHeader.Counter + 0x0A));
                    }

                    // Secure-ACK the connect (subtype 0x03, echoes Flags1) on the session ACK
                    // channel so 100 stops retransmitting and advances. ACK first, then the
                    // byte-verified connect-accept.
                    if (AcknowledgeTadFrames)
                    {
                        result.Add(_receiver.ReceiveDataFrame(incoming, TadResponder.AckChannel));
                    }

                    for (int i = 0; i < setup.Count; i++)
                    {
                        result.Add(setup[i]);
                    }

                    return result;
                }

                // (a2) The session-setup that follows an accepted connect: ACK it, then reply with
                //      the port-assignment (tells 100 our session port).
                if (TadResponder.IsSessionSetup(incoming))
                {
                    // ACK on the session-constant channel (connect+4) learned at connect time.
                    if (AcknowledgeTadFrames)
                    {
                        result.Add(_receiver.ReceiveDataFrame(incoming, TadResponder.AckChannel));
                    }

                    IReadOnlyList<XmsgFrame> assign = TadResponder.OnSessionSetup(incoming);
                    for (int i = 0; i < assign.Count; i++)
                    {
                        result.Add(assign[i]);
                    }

                    return result;
                }

                // (b) Any session data frame while connected (terminal negotiation or the user's
                //     typed line), except list-route which has its own reply: secure-ACK it so the
                //     handshake proceeds, and if it carries terminal input (BDAT) send the menu
                //     response too.
                if (TadResponder.IsConnected
                    && incoming.SubHeader.ControlService != ListRoutingServer.XmcsmXsgsyRequest)
                {
                    // Session data frames arrive on various channels (DC/DD/DE) but every ACK
                    // rides the one session-constant channel (connect+4), so pass it explicitly.
                    result.Add(_receiver.ReceiveDataFrame(incoming, TadResponder.AckChannel));
                    if (TadResponder.IsDisconnect(incoming))
                    {
                        // 100's graceful teardown (DCON) — e.g. its 1-minute "TAD not logged in" idle
                        // timeout. ACK it (above) and close our session so the next connect is clean.
                        TadResponder.CloseSession();
                    }
                    else if (HasBdat(incoming))
                    {
                        IReadOnlyList<XmsgFrame> replies = TadResponder.OnTerminalInput(incoming, out bool _);
                        for (int i = 0; i < replies.Count; i++)
                        {
                            result.Add(replies[i]);
                        }
                    }
                    else if (TadResponder.IsTerminalSetup(incoming))
                    {
                        // 100's TMOD/TTYP negotiation (arrives after our DUMM). Answer with the burst
                        // that produces the login screen: control 0x20, RESE, RESE, then the MOTD.
                        IReadOnlyList<XmsgFrame> bringup = TadResponder.OnTerminalSetup(incoming);
                        for (int i = 0; i < bringup.Count; i++)
                        {
                            result.Add(bringup[i]);
                        }
                    }

                    return result;
                }
            }

            // Fallback: the single-frame handler covers reachability, list-route (XSGSY) and the
            // optional secure-ACK behaviour.
            XmsgFrame? single = HandleFrame(incoming);
            if (single != null)
            {
                result.Add(single);
            }

            return result;
        }

        /// <summary>
        /// Returns true when a frame's decoded TAD chain contains at least one BDAT (terminal
        /// character-data) message.
        /// </summary>
        private static bool HasBdat(XmsgFrame frame)
        {
            if (frame.Tad == null)
            {
                return false;
            }

            IReadOnlyList<SubProtocol.TadMessage> messages = frame.Tad.Messages;
            for (int i = 0; i < messages.Count; i++)
            {
                if (messages[i].Opcode == 0x01)
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Processes a received XMSG frame and returns the single frame to send in response, if
        /// any (reachability reply, list-route reply, or secure ACK). Use
        /// <see cref="HandleFrames"/> for paths that emit more than one frame.
        /// </summary>
        /// <param name="incoming">The decoded received frame (its header selects the behaviour).</param>
        /// <returns>The response frame, or <c>null</c> when none is needed.</returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="incoming"/> or its header is null.
        /// </exception>
        public XmsgFrame? HandleFrame(XmsgFrame incoming)
        {
            if (incoming == null)
            {
                throw new ArgumentNullException(nameof(incoming));
            }

            if (incoming.Header == null)
            {
                throw new ArgumentNullException(nameof(incoming), "Frame header is null.");
            }

            switch (incoming.Header.Subtype)
            {
                case SintranPacketSubtype.Ack:
                    // 100's delivery ACK confirms it received our frame (its Flags1 echoes ours).
                    // Persist ackedFlags1 + 1 as our next sequence, so a restart continues exactly
                    // where 100 expects — never ahead of what it actually received.
                    TadResponder?.ConfirmDelivered(incoming.Header.SourceNode, incoming.Header.Flags1);
                    return null;

                case (SintranPacketSubtype)0x07:
                    // Network error. A XENSE (Flags2 0xFFDE) reject of our accept means our sequence
                    // was AHEAD of 100's expected-from-us (drift). Step the accept DOWN and re-send;
                    // one step per XENSE converges on 100's exact value with no restart. Only while
                    // the accept is still un-confirmed (before the session-setup).
                    if (TadResponder != null && TadResponder.CanResyncAccept)
                    {
                        return TadResponder.ResyncAcceptDown();
                    }

                    return null;

                case SintranPacketSubtype.ReachabilityRequest:
                    // A ReachabilityRequest is the peer's XMSG (re)start signal: its per-node-pair
                    // expected-from-us has just zeroed. Reset our persisted outgoing sequence for that
                    // node so our next session starts at 0x0000 in step with it (LIVE-VERIFIED: after
                    // an XMSG restart 100 sends this, then connects at Flags1 0x0000). No-op for a bare
                    // link restart, where 100 continues its sequence and sends no reachability.
                    TadResponder?.ResetSequence(incoming.Header.SourceNode);
                    return BuildReachabilityReply(incoming);

                case SintranPacketSubtype.Data:
                    // A list-route (XSGSY) request is answered from the routing table with the
                    // byte-validated ListRoutingServer reply (this is the structurally correct
                    // response, unlike a bare 0x03 ACK).
                    if (RoutingTable != null
                        && incoming.SubHeader != null
                        && incoming.SubHeader.ControlService == ListRoutingServer.XmcsmXsgsyRequest)
                    {
                        // Match the real 103 XSGSY response (device-online capture): the stateless
                        // reply ECHOES the request's transport counters so the asker can correlate
                        // it and advance. VERIFIED from the capture: for each request the response
                        // carried the SAME Flags1 (datagram sequence) and the SAME counter byte
                        // (e.g. req f1=0000/ctr=0x13 -> resp f1=0000/ctr=0x13; req f1=0001/ctr=0x12
                        // -> resp f1=0001/ctr=0x12). It also echoes the request's Protocol ID
                        // channel (DD here, not the DC default).
                        byte requestCounter = incoming.SubHeader != null ? incoming.SubHeader.Counter : (byte)0;
                        byte[] reply = _routingServer.Handle(
                            incoming,
                            RoutingTable,
                            counter: requestCounter,
                            flags1: incoming.Header.Flags1,
                            protocolId: incoming.Header.ProtocolId);
                        return XmsgFrame.Parse(reply);
                    }

                    // Otherwise: the secure ACK echoes Flags 1 and swaps direction (section 6),
                    // but only when configured to respond. Against the LIVE kernel, injecting an
                    // unrequested 0x03 ACK crashed XMSG (XXPER), so observe-only returns null.
                    return AcknowledgeData ? _receiver.ReceiveDataFrame(incoming) : null;

                default:
                    // ACK (0x03) and reachability reply (0x13) are terminal here.
                    return null;
            }
        }

        /// <summary>
        /// Builds the reachability reply (subtype <c>0x13</c>) for a received request.
        /// </summary>
        /// <param name="requestFrame">
        /// The received reachability request frame (subtype <c>0x19</c>); its trailing
        /// routing-counter byte drives the reply counter.
        /// </param>
        /// <returns>
        /// The reply frame with Dest/Src swapped, <c>Flags1 = 0xFFFF</c>,
        /// <c>Flags2 = 0x0001</c> and the reachability-reply command trailing byte.
        /// </returns>
        private XmsgFrame BuildReachabilityReply(XmsgFrame requestFrame)
        {
            SintranHeader request = requestFrame.Header;
            XmsgFrame reply = new XmsgFrame();

            // Copy the fixed markers/type verbatim so re-encode is byte-identical.
            reply.Header.Marker1 = request.Marker1;
            reply.Header.Marker2 = request.Marker2;
            reply.Header.PacketType = request.PacketType;
            reply.Header.Subtype = SintranPacketSubtype.ReachabilityReply;   // 0x13
            reply.Header.DestinationNode = request.SourceNode;               // swap direction
            reply.Header.SourceNode = request.DestinationNode;
            reply.Header.Flags1 = 0xFFFF;                                    // broadcast marker
            reply.Header.Flags2 = 0x0001;
            reply.Header.ProtocolId = request.ProtocolId;                    // 0xDE (ROUTING)

            // INFERRED: the reply's trailing routing-counter byte = request counter + 6.
            // Confirmed byte-identical against BOTH captured pairs (node 102: request 0x08
            // -> reply 0x0E; node 103: request 0x07 -> reply 0x0D). The earlier fixed
            // constant 0x0E was only correct for node 102. If a future capture contradicts
            // the +6 rule, this is the line to revisit.
            byte[]? reqTrailer = requestFrame.TrailingBytes;
            byte requestCounter = (reqTrailer != null && reqTrailer.Length > 0) ? reqTrailer[0] : ReachabilityReplyCommand;
            reply.TrailingBytes = new byte[] { (byte)((requestCounter + 6) & 0xFF) };
            return reply;
        }

        /// <summary>
        /// Forwards the receiver's delivery event to this node's own event.
        /// </summary>
        /// <param name="datagramSequence">
        /// The datagram sequence of the delivered data frame.
        /// </param>
        private void RaiseDelivered(ushort datagramSequence)
        {
            OnDataDelivered?.Invoke(datagramSequence);
        }
    }
}
