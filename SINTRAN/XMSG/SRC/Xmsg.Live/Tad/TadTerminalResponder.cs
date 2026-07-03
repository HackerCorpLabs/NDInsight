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

        // Persists our outgoing datagram sequence per remote node across process restarts, so it
        // stays in step with 100's persistent expected-from-us (XSRSQ). Default = non-persisting
        // (starts every remote at 0x0000); the live runner injects a file-backed store.
        private readonly IResponderSequenceStore _sequenceStore;

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
        /// <param name="sequenceStore">
        /// Persists our outgoing datagram sequence per remote node across restarts. When null, a
        /// non-persisting store is used (every remote starts at 0x0000) — correct for tests and for
        /// a first-ever contact, but a live node against a long-running peer should pass a
        /// <see cref="FileResponderSequenceStore"/> so it does not fall behind the peer's XSRSQ.
        /// </param>
        public TadTerminalResponder(ushort nodeNumber, Func<DateTime> clock, IResponderSequenceStore? sequenceStore = null)
        {
            _nodeNumber = nodeNumber;
            _menu = new TadTerminalMenu();
            _clock = clock ?? throw new ArgumentNullException(nameof(clock));
            _sequenceStore = sequenceStore ?? new NullResponderSequenceStore();
        }

        /// <summary>
        /// Gets a value indicating whether a terminal session is currently established.
        /// </summary>
        public bool IsConnected
        {
            get { return _connected; }
        }

        /// <summary>
        /// Resets our persisted outgoing datagram sequence for a remote node back to 0x0000. Called
        /// when that node signals an XMSG (re)start — a <b>ReachabilityRequest</b> — which zeroes its
        /// per-node-pair expected-from-us. LIVE-VERIFIED signal: after an XMSG restart 100 sends a
        /// ReachabilityRequest and its subsequent connect arrives at Flags1 0x0000; a bare HDLC link
        /// restart does NOT send one and 100 continues its sequence. Resetting here keeps our sequence
        /// in step across 100's restarts without any manual state-file surgery.
        /// </summary>
        /// <param name="remoteNode">The node that (re)started, e.g. 100.</param>
        public void ResetSequence(ushort remoteNode)
        {
            _sequenceStore.SaveNextFlags1(remoteNode, 0x0000);
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
            _motdSent = false;

            // LEARN the per-link seed from 100's connect frame: seed = (Counter + Flags1 + (Flags2 &
            // 0xFF)) & 0xFF (100<->102 = 0x14). Every frame we originate is then fully determined by
            // seed + our own Flags1 via the VERIFIED envelope arithmetic (XmsgEnvelope): the Counter
            // is (seed - (Flags2&0xFF) - Flags1) and the channel is 0xDE - (XMCSM>>24) - epoch. Our
            // OWN outgoing datagram sequence starts at 0x0000 (a fresh direction) and advances +1 per
            // frame across accept, port-assign and all terminal-data — one sequence, epoch 0, so
            // terminal data rides 0xDD (NOT DB: DB was an epoch-2 artifact of a high running sequence).
            _seed = XmsgEnvelope.LearnSeed(
                request.Header.Flags1, request.SubHeader.Counter, request.Header.Flags2);
            // Start our outgoing datagram sequence where our previous frames to THIS node left off.
            // 100 keeps a persistent per-node-pair expected-from-us (XSRSQ) = the count of Data frames
            // we have sent it; it does NOT reset when we restart. Resetting to 0x0000 makes our frames
            // behind-sequence (silently dropped) — the Run B failure. The store persists our next
            // Flags1 per remote node across restarts, so we continue in step. A first-ever contact
            // (no stored value) correctly starts at 0x0000. See
            // XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md option (a).
            _respFlags1 = _sequenceStore.LoadNextFlags1(_clientSystem);

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

            // Port-assign continues our sequence (Flags1 0x0001), a CONTROL-class frame: seed model
            // gives Counter = seed - 0x0001 and channel DA. Same bytes the echo scheme produced.
            outgoing.Add(BuildResponderFrame(
                request,
                frameClass: 0x0400,
                controlService: SessionSetupControlService,
                frameFlags: 0x86,
                role: 0x40,
                sourcePort: TadAdminWirePort,
                payload: trailer));

            // POST-PORT-ASSIGN BRING-UP.
            // 100 waits for our session-data burst before it drives TMOD/TTYP -> MOTD (the beep). The
            // channel and Counter are COMPUTED from the verified seed model (XmsgEnvelope): for a fresh
            // responder (epoch 0) a terminal-data frame (XMCSM 0x01080000) rides 0xDD with Counter
            // (seed - 8 - Flags1). NOT DB — DB was an epoch-2 artifact of a high running sequence, and
            // the earlier DC/DD crashes were a wrong COUNTER (fixed-Base), not a wrong channel.
            if (SendTerminalBringup)
            {
                // First terminal-data frame (DUMM): Flags1 0x0002 (continues the sequence), Flags2
                // 0x0108 -> Counter 0x0A, channel DD. Built via the TAD API. ISOLATION: emit only the
                // DUMM; 100's response tells us whether to send the rest (ctrl 0x20, RESE, RESE, MOTD).
                byte[] dumm = new TadChain().Add(TadOp.Dumm, null).ToBytes();
                outgoing.Add(BuildResponderFrame(
                    request,
                    frameClass: 0x0108,
                    controlService: TerminalDataControlService,
                    frameFlags: 0x92,
                    role: 0x00,
                    sourcePort: _sessionWirePort,
                    payload: dumm));
            }

            return outgoing;
        }

        /// <summary>XMCSM control/service word for a TAD terminal-data frame. VERIFIED from captures.</summary>
        private const uint TerminalDataControlService = 0x01080000u;

        /// <summary>
        /// The per-link seed byte, learned from 100's connect frame (see <see cref="OnConnect"/>).
        /// With our own <see cref="_respFlags1"/> it fully determines every frame we originate.
        /// </summary>
        private byte _seed;

        /// <summary>
        /// The responder's own outgoing datagram sequence to 100, starting at 0x0000 (a fresh
        /// direction) and advancing +1 per frame across accept, port-assign and terminal-data — one
        /// sequence, which is what 100 validates for in-order delivery (out-of-order = XENSE).
        /// </summary>
        private ushort _respFlags1;

        /// <summary>True once we have sent the MOTD, so we do not re-send it on repeated setup frames.</summary>
        private bool _motdSent;

        /// <summary>
        /// The MOTD frame's TAD payload, VERIFIED from conn-to-d102 frame 62: BMMX / ECKM / a BDAT
        /// banner (date, "SINTRAN III - VSX/500", "--- RETROCORE EMULATED ID:102 ---") / SYCN / a BDAT
        /// "ENTER " prompt / RFI. Copied verbatim (the "ID:102" already matches our node number).
        /// </summary>
        private static readonly byte[] MotdPayload = Convert.FromHexString(
            "0004030100000003010101600D0A2032322E32372E32322020202020203820415052494C202020313939380D0A"
            + "2053494E5452414E20494949202D205653582F353030204C0D0A2D2D2D20524554524F434F524520454D554C4154"
            + "4544204C2049443A313032202D2D2D0D0A1302000201080D0A454E544552200200");

        /// <summary>
        /// Returns true when the frame is 100's terminal-setup (it carries a TMOD message), i.e. the
        /// negotiation 100 sends after our DUMM. Answering it with the MOTD burst is the last step.
        /// </summary>
        /// <param name="frame">The incoming data frame.</param>
        /// <returns>True when this is the terminal-setup that should trigger the MOTD.</returns>
        public bool IsTerminalSetup(XmsgFrame frame)
        {
            if (!_connected || _motdSent || frame == null || frame.Tad == null)
            {
                return false;
            }

            IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
            for (int i = 0; i < messages.Count; i++)
            {
                if (messages[i].Opcode == TadOp.Tmod)
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Answers 100's terminal-setup with the responder burst that produces the login screen, each
        /// frame CONTINUING our one datagram sequence (so the channels/counters are the seed-model
        /// values): control <c>0x20</c>, RESE, RESE, then the MOTD. VERIFIED shapes from conn-to-d102
        /// frames 57/58/60/62.
        /// </summary>
        /// <param name="request">The terminal-setup (TMOD) frame; its source is 100's session endpoint.</param>
        /// <returns>The burst frames, in transmit order.</returns>
        public IReadOnlyList<XmsgFrame> OnTerminalSetup(XmsgFrame request)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            // control 0x20 (XMCSM 0x00080000): TAD opcode 0x20, empty. Seed model -> channel DE at epoch 0.
            outgoing.Add(BuildResponderFrame(
                request, frameClass: 0x0008, controlService: 0x00080000u, frameFlags: 0x86, role: 0x00,
                sourcePort: _sessionWirePort, payload: new TadChain().Add(0x20, null).ToBytes()));

            // RESE, RESE (XMCSM 0x01080000): TAD RESE, empty. Channel DD at epoch 0.
            outgoing.Add(BuildResponderFrame(
                request, frameClass: 0x0108, controlService: TerminalDataControlService, frameFlags: 0x96, role: 0x00,
                sourcePort: _sessionWirePort, payload: new TadChain().Add(TadOp.Rese, null).ToBytes()));
            outgoing.Add(BuildResponderFrame(
                request, frameClass: 0x0108, controlService: TerminalDataControlService, frameFlags: 0x92, role: 0x00,
                sourcePort: _sessionWirePort, payload: new TadChain().Add(TadOp.Rese, null).ToBytes()));

            // MOTD (XMCSM 0x01080000): the banner + ENTER prompt chain. Channel DD at epoch 0.
            outgoing.Add(BuildResponderFrame(
                request, frameClass: 0x0108, controlService: TerminalDataControlService, frameFlags: 0x96, role: 0x00,
                sourcePort: _sessionWirePort, payload: MotdPayload));

            _motdSent = true;
            return outgoing;
        }

        /// <summary>
        /// Builds one frame we originate for the session using the VERIFIED envelope seed model
        /// (<see cref="XmsgEnvelope"/>): the Counter is <c>(seed - (Flags2&amp;0xFF) - Flags1)</c> and
        /// the channel is <c>0xDE - (XMCSM&gt;&gt;24) - epoch</c>, with Flags1 the next value of our own
        /// single outgoing datagram sequence. Then advances the sequence. Control-class frames
        /// (Flags2 0x0400) land on DA at epoch 0; terminal-data frames (Flags2 0x0108) on DD at epoch 0.
        /// </summary>
        /// <param name="request">The triggering frame (source addressing = 100's endpoint).</param>
        /// <param name="frameClass">The Flags 2 frame-class word (0x0400 control, 0x0108 data) — also the XMCSM top half.</param>
        /// <param name="controlService">The XMCSM control/service word.</param>
        /// <param name="frameFlags">The sub-header frame-flags byte for this frame type.</param>
        /// <param name="role">The sub-header role byte (0x40 setup, 0x00 data-phase).</param>
        /// <param name="sourcePort">Our source port (TADADM for control, session port for data).</param>
        /// <param name="payload">The trailer payload bytes (param blocks or TAD chain).</param>
        /// <returns>The assembled frame on the derived channel with the computed counter.</returns>
        private XmsgFrame BuildResponderFrame(
            XmsgFrame request,
            ushort frameClass,
            uint controlService,
            byte frameFlags,
            byte role,
            ushort sourcePort,
            byte[] payload)
        {
            ushort f1 = _respFlags1;
            // Counter and channel from the verified seed model — NOT a fixed Base (that fixed-Base
            // Counter was the cause of the XXPER crashes on the terminal-data frames).
            byte ctr = XmsgEnvelope.ComputeCounter(_seed, f1, frameClass);
            SintranProtocolId channel = XmsgEnvelope.DeriveChannel(_seed, f1, frameClass, controlService);

            // One continuous sequence for ALL our frames (accept, port-assign, data) -> 100 sees them
            // in order (out-of-order = XENSE). Persist it per remote node so a restart continues in
            // step with 100's expected-from-us (only Data frames advance it; secure ACKs do not).
            _respFlags1 = (ushort)(f1 + 1);
            _sequenceStore.SaveNextFlags1(_clientSystem, _respFlags1);

            TadFrameContext ctx = new TadFrameContext
            {
                DestinationNode = request.Header.SourceNode,        // 100
                SourceNode = _nodeNumber,                           // us
                DatagramSequence = f1,
                FrameClass = frameClass,
                ProtocolId = channel,                              // DERIVED, not canned
                Counter = ctr,
                FrameFlags = frameFlags,
                Role = role,
                DestinationSystem = _clientSystem,                   // 100 (learned at connect)
                DestinationPort = request.SubHeader!.SourcePort,     // 100's port on the triggering frame
                SourceSystem = _nodeNumber,
                SourcePort = sourcePort,
                ControlService = controlService,
            };

            return AssembleDataFrame(ctx, payload);
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

            // The accept is the FIRST frame of our own sequence (Flags1 0x0000), a CONTROL-class frame
            // (Flags2 0x0400): the seed model gives Counter = seed - 0x0000 = 0x14 and channel DA
            // (0xDE - 4 - epoch0). This equals the value 100 accepts — the old echo scheme produced
            // the SAME bytes for the accept because 100's connect was itself Flags1 0x0000 / Counter
            // seed; the model just makes accept, port-assign and terminal-data one coherent sequence.
            return BuildResponderFrame(
                request,
                frameClass: 0x0400,
                controlService: SystemTadControlService,
                frameFlags: 0x86,
                role: 0x40,
                sourcePort: TadAdminWirePort,
                payload: trailer);
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
