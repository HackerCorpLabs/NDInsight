using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg.Packet;
using NDInsight.Sintran.Xmsg.SubProtocol;

namespace NDInsight.Sintran.Xmsg.Live.Tad
{
    /// <summary>
    /// The remote-machine simulation: answers an incoming <c>connect-to</c> (SYSTEM-TAD) session
    /// and drives the <see cref="TadTerminalMenu"/> terminal over it. This is the SERVER/answering
    /// side (the role our node 103 plays when machine 100 connects to it), the mirror of the
    /// client-oriented <see cref="TadSession"/>.
    /// </summary>
    /// <remarks>
    /// <para><b>Provenance — what is VERIFIED vs INFERRED.</b></para>
    /// The frame shapes are modelled on the captured <c>102 -&gt; 100</c> responder side of
    /// <c>conn-to-d102-from-100.pcapng</c> (connect-accept: proto <c>0xD8</c>, role <c>0x40</c>,
    /// <c>XMCSM 0x04000041</c>, param trailer <c>01 02 0000 02 02 000A</c>, replying from the
    /// TADADM well-known port 2 = wire <c>0x0156</c>). Those field VALUES are VERIFIED from the
    /// capture. What is INFERRED (and may need live tuning) is the transport SEQUENCING for OUR
    /// direction: we ECHO the request's Flags1 and counter (the pattern proven to work for the
    /// stateless list-route reply), because the responder's own independent sequence base is not
    /// recoverable from a one-sided view. Every such choice is commented at its use site.
    /// </remarks>
    public sealed class TadTerminalResponder
    {
        // The XMSG SYSTEM-TAD / directory-service control word (XSLET letter to TADADM),
        // VERIFIED as the connect dispatch value in every conn-to capture.
        private const uint SystemTadControlService = 0x04000041u;

        // The TADADM well-known service port. VERIFIED: logical port 2 with the fixed low-7
        // component 0x56 -> wire value 0x0156 (342), seen on systems 100/102/103 alike.
        private const ushort TadAdminWirePort = 0x0156;

        // TAD opcode for terminal character data (BDAT). VERIFIED from TadOpcodes / captures.
        private const byte BdatOpcode = 0x01;

        private readonly ushort _nodeNumber;
        private readonly TadTerminalMenu _menu;
        private readonly Func<DateTime> _clock;

        // Our dynamically-allocated session port (logical port << 7 | random low-7). Assigned on
        // connect. INFERRED starting value; only the (logical<<7)|random LAYOUT is VERIFIED.
        private ushort _sessionWirePort;

        // The remote (machine 100) endpoint learned from the connect request.
        private ushort _clientSystem;
        private ushort _clientPort;

        // The per-session secure-ACK channel = connect-channel + 4. VERIFIED from both connect
        // captures (asker side): a D9-rooted session ACKs on DD, a DA-rooted session ACKs on DE,
        // and the ACK channel stays constant for the whole session (even when the acknowledged
        // data frame was on DC). Learned on connect; default Tad (0xDD) until then. The old code
        // echoed the data channel (+0), which is the malformed ACK that crashed 100 (XXPER).
        private SintranProtocolId _ackChannel = SintranProtocolId.Tad;

        private bool _connected;

        /// <summary>
        /// When true, the (currently blocked) terminal bring-up frames are sent after the
        /// port-assign. Default false: sending them crashes 100's XMSG because the session
        /// channels/counters are per-session-allocated and not yet reconstructable. Left as a
        /// switch so the replay can be re-enabled once a bidirectional live capture is available.
        /// </summary>
        public bool SendTerminalBringup { get; set; }

        /// <summary>
        /// Initialises the responder for a given node with a clock (injected for deterministic
        /// tests; the live runner passes <c>() =&gt; DateTime.Now</c>).
        /// </summary>
        /// <param name="nodeNumber">This node's number (for example 103).</param>
        /// <param name="clock">Supplies the current time for the MOTD and the Time/Date commands.</param>
        public TadTerminalResponder(ushort nodeNumber, Func<DateTime> clock)
        {
            _nodeNumber = nodeNumber;
            _menu = new TadTerminalMenu();
            _clock = clock ?? throw new ArgumentNullException(nameof(clock));
        }

        /// <summary>
        /// Gets a value indicating whether a terminal session is currently established.
        /// </summary>
        public bool IsConnected
        {
            get { return _connected; }
        }

        /// <summary>
        /// Gets the per-session secure-ACK channel (Protocol-ID) that every subtype-<c>0x03</c>
        /// delivery ACK for this session must ride: <em>connect-channel + 4</em>. Learned from the
        /// connect frame in <see cref="OnConnect"/>; before a connect it is the TAD default
        /// (<c>0xDD</c>). See the field comment for the capture provenance.
        /// </summary>
        public SintranProtocolId AckChannel
        {
            get { return _ackChannel; }
        }

        /// <summary>
        /// Returns true when the frame is a SYSTEM-TAD connect request addressed to this node
        /// (the <c>*TADADM</c> letter that opens a <c>connect-to</c>).
        /// </summary>
        /// <param name="frame">The decoded incoming data frame.</param>
        /// <returns>True when this is a connect request we should answer.</returns>
        public static bool IsConnectRequest(XmsgFrame frame)
        {
            return frame != null
                && frame.SubHeader != null
                && frame.SubHeader.ControlService == SystemTadControlService
                // role low-nibble 4 = asker (the connecting side). VERIFIED nibble convention.
                && (frame.SubHeader.Role & 0x0F) == 0x04;
        }

        /// <summary>
        /// Handles an incoming connect request: allocates a session port and produces the
        /// connect-accept plus the terminal greeting (MOTD + menu + prompt) as BDAT.
        /// </summary>
        /// <param name="request">The connect-request frame.</param>
        /// <returns>The frames to transmit, in order.</returns>
        public IReadOnlyList<XmsgFrame> OnConnect(XmsgFrame request)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            // Learn the client endpoint from the request's SOURCE fields.
            _clientSystem = request.SubHeader!.SourceSystem;
            _clientPort = request.SubHeader.SourcePort;

            // Learn the per-session secure-ACK channel = connect-channel + 4 (VERIFIED across both
            // connect captures: D9->DD, DA->DE). Every 0x03 ACK we send for this session rides this
            // constant channel, NOT the channel the acknowledged data arrived on.
            _ackChannel = (SintranProtocolId)(byte)((byte)request.Header.ProtocolId + 4);

            // Allocate our session port: logical port 4 with a fixed low-7 for now. The random
            // part is deliberately FIXED (not random) so the wire bytes are reproducible while we
            // iterate live; the magic-number model only requires the (logical<<7)|low7 layout,
            // which this satisfies (4<<7 | 0x11 = 0x0211). [INFERRED value; VERIFIED layout.]
            _sessionWirePort = (ushort)((4 << 7) | 0x11);
            _connected = true;

            // The captured 102 responder handshake is, in order:
            //   1. connect-accept  (proto D8, role 40, XMCSM 04000041, param trailer)
            //   2. port-assign     (proto D8, role 40, XMCSM 04000000, TAD 0x07 = our session
            //                       endpoint, so 100 learns where to send terminal data)
            // Only AFTER these does 100 drive terminal negotiation (TMOD/TTYP/…). Sending the
            // greeting immediately (on a port 100 has not been told about) makes the emulator drop
            // the link, so the greeting is deferred until negotiation is observed. [Learned live.]
            // ISOLATION STEP: send ONLY the byte-verified connect-accept for now. The port-assign
            // and terminal-setup frames are deferred until we confirm the accept alone is
            // structurally accepted by 100 (previous multi-frame attempts crashed XMSG with XXPER,
            // so we add one verified frame at a time).
            outgoing.Add(BuildConnectAccept(request));

            return outgoing;
        }

        /// The XMSG control word 100 uses for the session-setup / port-negotiation frames
        /// (the TAD 06/1B/1C/FF chain and our port-assign reply). VERIFIED from captures.
        private const uint SessionSetupControlService = 0x04000000u;

        /// <summary>
        /// Returns true when the frame is the session-setup that follows an accepted connect
        /// (XMCSM <c>0x04000000</c> carrying the <c>06/1B/1C/FF</c> negotiation chain).
        /// </summary>
        public bool IsSessionSetup(XmsgFrame frame)
        {
            return _connected
                && frame != null
                && frame.SubHeader != null
                && frame.SubHeader.ControlService == SessionSetupControlService;
        }

        /// <summary>
        /// Handles the session-setup frame by replying with the port-assignment: a TAD <c>0x07</c>
        /// message carrying our session endpoint (system + session port) so 100 learns where to
        /// send terminal data, followed by the OPSV/0B/15/FF options copied from the capture.
        /// </summary>
        /// <remarks>
        /// The frame ECHOES the session-setup's proto / Flags1 / counter (the same reply pattern
        /// the accept uses — VERIFIED from conn-to-102-from103-via100, where the responder's
        /// port-assign echoed the asker's session-setup transport fields). The 24-byte trailer is
        /// the captured 102 trailer with the system byte and session-port bytes substituted for
        /// ours; the remaining option bytes are copied verbatim (not yet decoded).
        /// </remarks>
        /// <param name="request">The session-setup frame.</param>
        /// <returns>The port-assignment frame.</returns>
        public IReadOnlyList<XmsgFrame> OnSessionSetup(XmsgFrame request)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            // Captured 102 trailer (24 bytes):
            //   00 | 07 05 [00 00 66 03 13] | 1F 03 4C 00 00 | 00 | 0B 02 03 00 | 15 02 01 08 | FF 00
            // where 0x07's data = 00 00 <system> <sessionPortHi> <sessionPortLo>. Substitute ours.
            byte sysByte = (byte)_nodeNumber;
            byte portHi = (byte)(_sessionWirePort >> 8);
            byte portLo = (byte)(_sessionWirePort & 0xFF);
            byte[] trailer = new byte[]
            {
                0x00,
                0x07, 0x05, 0x00, 0x00, sysByte, portHi, portLo,
                0x1F, 0x03, 0x4C, 0x00, 0x00,
                0x00,
                0x0B, 0x02, 0x03, 0x00,
                0x15, 0x02, 0x01, 0x08,
                0xFF, 0x00,
            };

            TadFrameContext ctx = new TadFrameContext
            {
                DestinationNode = request.Header.SourceNode,
                SourceNode = _nodeNumber,
                DatagramSequence = request.Header.Flags1,           // echo (VERIFIED pattern)
                FrameClass = 0x0400,
                ProtocolId = request.Header.ProtocolId,             // echo the channel (DA)
                Counter = request.SubHeader!.Counter,               // echo
                FrameFlags = 0x86,
                Role = 0x40,
                DestinationSystem = request.SubHeader.SourceSystem,
                DestinationPort = request.SubHeader.SourcePort,
                SourceSystem = _nodeNumber,
                SourcePort = TadAdminWirePort,
                ControlService = SessionSetupControlService,
            };

            outgoing.Add(AssembleDataFrame(ctx, trailer));

            // POST-PORT-ASSIGN BRING-UP (derived-channel path).
            // 100 waits for our session-data burst before it drives TMOD/TTYP -> MOTD (that is the
            // beep). The OLD approach replayed a canned session's channels/counters, which have the
            // wrong Base for THIS session and crash 100's XMSG (XXPER). We now COMPUTE the channel
            // from the universal envelope model instead (VERIFIED: it reproduces the captured
            // conn-to-d102 DUMM/MOTD frames byte-for-byte). See XmsgEnvelope / CreateSessionData.
            if (SendTerminalBringup)
            {
                // Initialise our 102->100 session-data stream to the Base a REAL 100 accepted in the
                // conn-to-d102 capture: flags1 0x0131 + counter 0xDB = Base 0x020C -> derived channel
                // DB. Within the stream flags1++ / counter-- holds Base (and the channel) constant.
                // [LIVE PROBE: if 100 rejects this absolute Base, this pair is the knob to adjust.]
                _dataFlags1 = 0x0131;
                _dataCounter = 0xDB;

                // ISOLATION: emit ONLY the first bring-up frame (DUMM) so 100's response — a delivery
                // ACK (accepted) versus XXPER (rejected) — tells us whether the derived channel/Base
                // is right BEFORE we send the rest of the burst (ctrl 0x20, RESE, RESE, then MOTD).
                outgoing.Add(BuildSessionData(
                    request,
                    controlService: 0x01080000u,
                    frameFlags: 0x92,
                    role: 0x00,
                    tadPayload: new byte[] { 0x18, 0x00 }));   // TAD DUMM (0x18) count=0
            }

            return outgoing;
        }

        // Our 102->100 session-data stream sequence: flags1 increments and counter decrements per
        // frame so that Base = flags1 + counter stays constant (the envelope model). Seeded when the
        // bring-up starts (see OnSessionSetup).
        private ushort _dataFlags1;
        private byte _dataCounter;

        /// <summary>
        /// Builds one 102-&gt;100 session-data frame on the channel DERIVED from the envelope model
        /// (<see cref="XmsgEnvelope.DeriveChannel"/> of the current stream Flags1/Counter and the
        /// XMCSM), addressed to 100's session endpoint, then advances the stream (Flags1++, Counter--
        /// keeping Base constant). This is the responder counterpart of
        /// <see cref="XmsgPacketBuilder.CreateSessionData"/>, kept here so the frame flows through the
        /// existing <see cref="AssembleDataFrame"/> path.
        /// </summary>
        /// <param name="request">The session-setup frame (source addressing = 100's endpoint).</param>
        /// <param name="controlService">The XMCSM control/service word (selects the class byte).</param>
        /// <param name="frameFlags">The sub-header frame-flags byte for this frame type.</param>
        /// <param name="role">The sub-header role byte (0x00 for server data-phase frames).</param>
        /// <param name="tadPayload">The TAD chain payload bytes.</param>
        /// <returns>The assembled session-data frame.</returns>
        private XmsgFrame BuildSessionData(XmsgFrame request, uint controlService, byte frameFlags, byte role, byte[] tadPayload)
        {
            ushort f1 = _dataFlags1;
            byte ctr = _dataCounter;

            // VERIFIED envelope identity: Base = f1 + ctr; channel = 0xDE - (XMCSM>>24) - (Base>>8).
            SintranProtocolId channel = XmsgEnvelope.DeriveChannel(f1, ctr, controlService);

            TadFrameContext ctx = new TadFrameContext
            {
                DestinationNode = request.Header.SourceNode,        // 100
                SourceNode = _nodeNumber,                           // 102
                DatagramSequence = f1,
                FrameClass = 0x0108,                                // VERIFIED data-frame class word
                ProtocolId = channel,                              // DERIVED, not canned
                Counter = ctr,
                FrameFlags = frameFlags,
                Role = role,
                DestinationSystem = _clientSystem,                   // 100 (learned at connect)
                DestinationPort = request.SubHeader!.SourcePort,     // 100's session-setup port
                SourceSystem = _nodeNumber,                          // 102
                SourcePort = _sessionWirePort,                       // our assigned session port
                ControlService = controlService,
            };

            // Advance the stream, holding Base constant (flags1++ / counter--).
            _dataFlags1 = (ushort)(f1 + 1);
            _dataCounter = (byte)(ctr - 1);

            return AssembleDataFrame(ctx, tadPayload);
        }

        /// <summary>
        /// Handles a terminal-data frame (the user's typed line) during an active session and
        /// produces the menu response. Returns an empty list when the frame is not terminal input.
        /// </summary>
        /// <param name="frame">The incoming data frame.</param>
        /// <param name="disconnect">Set true when the session should close after the response.</param>
        /// <returns>The response frames.</returns>
        public IReadOnlyList<XmsgFrame> OnTerminalInput(XmsgFrame frame, out bool disconnect)
        {
            disconnect = false;
            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            if (!_connected || frame.SubHeader == null)
            {
                return outgoing;
            }

            // Extract the typed text from any BDAT messages in the incoming TAD chain.
            string input = ExtractBdatText(frame);

            TadMenuResult result = _menu.Handle(input, _clock());
            outgoing.Add(BuildTerminalText(frame, result.Output));

            if (result.Disconnect)
            {
                disconnect = true;
                _connected = false;
            }

            return outgoing;
        }

        /// <summary>
        /// Builds the connect-accept frame (proto <c>0xD8</c>, role <c>0x40</c>, the SYSTEM-TAD
        /// param trailer) that a real responder sends to acknowledge the connect.
        /// </summary>
        private XmsgFrame BuildConnectAccept(XmsgFrame request)
        {
            // Param trailer VERIFIED from the 102 capture connect-accept: two parameter blocks
            // 01 02 0000 (param 1 = 0) and 02 02 000A (param 2 = 0x000A). Meaning of 0x000A is not
            // yet decoded; copied verbatim from the captured accept. [VERIFIED bytes; semantics TBD]
            byte[] trailer = new byte[] { 0x01, 0x02, 0x00, 0x00, 0x02, 0x02, 0x00, 0x0A };

            TadFrameContext ctx = new TadFrameContext
            {
                DestinationNode = request.Header.SourceNode,        // back to 100
                SourceNode = _nodeNumber,                           // from us (103)
                // VERIFIED from conn-to-102-from103-via100 (the DA connect that matches our role):
                // the accept ECHOES the request's proto, Flags1 and counter (the stateless-reply
                // pattern, exactly like the list-route reply) — it does NOT use an independent
                // sequence. Request f1=0x0004/ctr=0x0D/proto DA -> accept f1=0x0004/ctr=0x0D/DA.
                DatagramSequence = request.Header.Flags1,
                FrameClass = 0x0400,                                // VERIFIED: setup-frame class word
                ProtocolId = request.Header.ProtocolId,             // VERIFIED: echo the request channel (DA)
                Counter = request.SubHeader!.Counter,               // VERIFIED echo
                FrameFlags = 0x86,                                  // VERIFIED common value
                Role = 0x40,                                        // VERIFIED responder role
                DestinationSystem = request.SubHeader.SourceSystem, // 100
                DestinationPort = request.SubHeader.SourcePort,     // 100's port (so it correlates)
                SourceSystem = _nodeNumber,                         // 103
                SourcePort = TadAdminWirePort,                      // our TADADM port 2 (0x0156)
                ControlService = SystemTadControlService,
            };

            return AssembleDataFrame(ctx, trailer);
        }

        /// <summary>
        /// Builds a terminal-text frame carrying <paramref name="text"/> as a BDAT TAD message on
        /// the session port. Long text is truncated to a single 255-byte BDAT for now (the menu
        /// text is well under that; chunking is a later refinement). [INFERRED envelope; iterate.]
        /// </summary>
        private XmsgFrame BuildTerminalText(XmsgFrame request, string text)
        {
            byte[] ascii = Encoding.ASCII.GetBytes(text);
            if (ascii.Length > 255)
            {
                byte[] clamped = new byte[255];
                Array.Copy(ascii, clamped, 255);
                ascii = clamped;
            }

            TadChain chain = new TadChain();
            chain.Add(BdatOpcode, ascii);
            byte[] trailer = chain.ToBytes();

            TadFrameContext ctx = new TadFrameContext
            {
                DestinationNode = request.Header.SourceNode,
                SourceNode = _nodeNumber,
                // NOTE: terminal-text sequencing is not yet finalised — the captured session frames
                // use the SESSION port's own advancing f1 / decrementing counter (not echo). This
                // path is not reached until the negotiation completes; for now echo the triggering
                // frame so it compiles and is a sane placeholder. [TO REVISIT once negotiation works.]
                DatagramSequence = request.Header.Flags1,
                FrameClass = 0x0108,                                // VERIFIED: DC/TAD data class word
                ProtocolId = SintranProtocolId.Db,                  // VERIFIED: server terminal text used 0xDB
                Counter = request.SubHeader!.Counter,
                FrameFlags = 0x86,
                Role = 0x00,                                        // VERIFIED: server data-phase role 0x00
                DestinationSystem = request.SubHeader!.SourceSystem,
                DestinationPort = _clientPort,
                SourceSystem = _nodeNumber,
                SourcePort = _sessionWirePort,                      // our session port
                ControlService = 0x01080000u,                      // VERIFIED: DC/TAD terminal data
            };

            return AssembleDataFrame(ctx, trailer);
        }

        /// <summary>
        /// Reads the concatenated ASCII text of every BDAT message in a frame's TAD chain.
        /// </summary>
        private static string ExtractBdatText(XmsgFrame frame)
        {
            if (frame.Tad == null)
            {
                return string.Empty;
            }

            StringBuilder sb = new StringBuilder();
            IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
            for (int i = 0; i < messages.Count; i++)
            {
                if (messages[i].Opcode == BdatOpcode)
                {
                    byte[] data = messages[i].Data;
                    for (int j = 0; j < data.Length; j++)
                    {
                        // Strip the high (parity) bit terminals sometimes set, and keep printable
                        // characters; CR/LF are handled by the menu's Trim.
                        byte b = (byte)(data[j] & 0x7F);
                        sb.Append((char)b);
                    }
                }
            }

            return sb.ToString();
        }

        /// <summary>
        /// Assembles a subtype-<c>0x0E</c> data frame from an envelope context and trailer bytes
        /// (mirror of <see cref="TadSession"/>'s private assembler, duplicated here so the
        /// responder is self-contained).
        /// </summary>
        private static XmsgFrame AssembleDataFrame(TadFrameContext context, byte[] trailer)
        {
            XmsgFrame frame = new XmsgFrame();
            frame.Header.Marker1 = SintranHeader.Marker1Value;
            frame.Header.Marker2 = SintranHeader.Marker2Normal;
            frame.Header.PacketType = 0x00;
            frame.Header.Subtype = SintranPacketSubtype.Data;
            frame.Header.DestinationNode = context.DestinationNode;
            frame.Header.SourceNode = context.SourceNode;
            frame.Header.Flags1 = context.DatagramSequence;
            frame.Header.Flags2 = context.FrameClass;
            frame.Header.ProtocolId = context.ProtocolId;

            XmsgSubHeader sub = new XmsgSubHeader();
            sub.Counter = context.Counter;
            sub.FrameFlags = context.FrameFlags;
            sub.Role = context.Role;
            sub.DestinationSystem = context.DestinationSystem;
            sub.DestinationPort = context.DestinationPort;
            sub.SourceSystem = context.SourceSystem;
            sub.SourcePort = context.SourcePort;
            sub.ControlService = context.ControlService;
            sub.Pad = 0x00;
            sub.UserDataLength = (byte)trailer.Length;

            frame.SubHeader = sub;
            frame.TrailingBytes = trailer;
            frame.ClearRawBytes();
            return frame;
        }
    }
}
