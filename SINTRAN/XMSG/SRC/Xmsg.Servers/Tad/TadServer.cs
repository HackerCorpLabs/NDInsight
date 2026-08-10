using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Text;

using NDInsight.Sintran.Xmsg.Node;
using NDInsight.Sintran.Xmsg.Node.Services;
using NDInsight.Sintran.Xmsg.Node.Tad;
using NDInsight.Sintran.Xmsg.SubProtocol;

namespace NDInsight.Sintran.Xmsg.Servers.Tad
{
    /// <summary>
    /// How a logged-in reply longer than one terminal buffer is streamed to the connect-to client.
    /// </summary>
    public enum TadOutputMode
    {
        /// <summary>
        /// N consecutive COMPLETE BDAT segments (each at most <c>SegmentChunk</c> bytes), window-of-1,
        /// only the final segment carrying SYCN 000A + prompt + RFI. Backed by the Ghidra decode of the
        /// receiver (no count==0xFF semantics; each element renders its own bytes) — see
        /// COS-CONN-TO-E02-Analysis.md §5b. Default: the 255-sentinel stream never rendered on real 100.
        /// </summary>
        CompleteSegments,

        /// <summary>
        /// Bare 255-byte continuation pairs (count 0xFF) spaced ~46 ms, then a short final frame with
        /// SYCN + prompt + RFI (TAD-Message-Formats.md §22.16). Byte-faithful to the real host capture
        /// but empirically renders ONLY the final chunk on real 100 — retained for A/B comparison.
        /// </summary>
        SentinelStream,
    }

    /// <summary>
    /// The TAD terminal server - the named XROUT server <c>*TADADM</c> that answers <c>connect-to</c>.
    /// It plugs into the node as an <see cref="IXmsgServer"/>, manages one tty session per concurrent
    /// connect, and builds every reply through the node's <see cref="IXmsgServerTransport"/> so it owns no
    /// envelope/sequencing state.
    /// </summary>
    /// <remarks>
    /// <para><b>Provenance.</b></para>
    /// The frame shapes (connect-accept, port-assign, MOTD, the login/menu chains and the teardown
    /// ladder) are the live-verified bytes from <c>TadTerminalResponder</c>, moved here unchanged; only
    /// the transport now assigns Flags 1 / Counter / channel. See XMSG-PROTOCOL.md section 22.
    /// </remarks>
    public sealed class TadServer : IXmsgServer
    {
        private readonly Func<DateTime> _clock;
        private readonly TadUserDirectory _users;
        private readonly TadTerminalMenu _menu;
        private readonly Dictionary<uint, TadServerSession> _sessions;
        private readonly Dictionary<ushort, TadServerSession> _sessionByPort;
        // A parallel list of the live sessions, so DrainPending can iterate with an index for-loop (no
        // foreach over the dictionary). Kept in sync with _sessions in OnConnect / CloseSession.
        private readonly List<TadServerSession> _sessionList;
        // Maps (remoteNode << 16 | Flags1) of a sent-but-unacked OUTPUT chunk to its owning session, so an
        // incoming ACK (which carries only the node + Flags1) can advance that session's output window.
        private readonly Dictionary<uint, TadServerSession> _outputAckIndex;
        // The configurable middle banner line (replaces the stock "SINTRAN III - VSX/500"). Null/empty
        // falls back to the built-in version banner. The date line and the HOST-id line are generated.
        private readonly string _motdLine;

        // The *TADADM registered name and its well-known logical port (COSMOS list-servers shows Port 2).
        private const string ServerName = "*TADADM";
        private const int ServerLogicalPort = 2;

        // The wire reply-from port for *TADADM: logical port 2 with the fixed low-7 0x56 -> 0x0156 (342).
        private const ushort TadAdminWirePort = 0x0156;

        // The maximum concurrent TAD sessions (the "Free SPs" capacity).
        private const int MaxSessions = 10;

        // The maximum wrong-credential attempts before the session is torn down.
        private const int MaxLoginFaults = 3;

        // The logged-in command prompt. Emitted as its OWN BDAT after the SYCN in the burst trailer
        // (TAD-Message-Formats.md 22.6), not folded into the content BDAT.
        private const string TerminalPrompt = "# ";

        /// <summary>
        /// How multi-buffer logged-in replies are streamed. Defaults to
        /// <see cref="TadOutputMode.CompleteSegments"/> — the receiver decode (COS-CONN-TO-E02-Analysis.md
        /// §5b) shows the client has no 255-sentinel concept and renders each BDAT element's bytes, so N
        /// complete segments should each render; the old <see cref="TadOutputMode.SentinelStream"/> is kept
        /// for A/B testing against real 100.
        /// </summary>
        public TadOutputMode OutputMode { get; set; } = TadOutputMode.CompleteSegments;

        /// <summary>
        /// Segmented output (<see cref="TadOutputMode.CompleteSegments"/>) advances window-of-1 on 100's
        /// ACK, so the host must drain on an incoming ACK. The sentinel stream advances on 100's 7DUMM
        /// commit instead, so it must NOT drain on an ACK (that sent the terminator before the DUMM
        /// committed the prior continuation - the historical multi-chunk drop).
        /// </summary>
        public bool AdvancesOutputOnAck
        {
            get { return OutputMode == TadOutputMode.CompleteSegments; }
        }

        // TAD opcode bytes we test for on the RX side.
        private const byte BdatOpcode = 0x01;
        private const byte EscaOpcode = 0x08;   // 7ESCA - the asker's escape signal (bring-up step 2)
        private const byte DconOpcode = 0x09;
        private const byte TmodOpcode = 0x0C;
        private const byte TtypOpcode = 0x0D;
        private const byte DescOpcode = 0x0F;
        private const byte RecoOpcode = 0x17;   // 7RECO - reset confirm (bring-up steps 3/4)
        private const byte DummOpcode = 0x18;   // 7DUMM - 100's per-continuation consumption/display signal
        private const byte OpsvOpcode = 0x1F;

        // The port-assign 7LUN value byte (the TAD Logical Unit index; LU = 768 + value). Real captures
        // carry 0x04 (conn-to-d102) and 0x02 (new-conn) — the allocation rule is UNKNOWN, but 0x00 was
        // NEVER observed, so emit a real observed value rather than zero
        // (XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md checklist point 3).
        private const byte LunIndexByte = 0x02;

        // XMCSM control/service words (their high half is the frame class). VERIFIED from captures.
        private const uint XsletLetterControlService = (uint)XmcsmService.XsletLetter;      // connect / accept (0x04000041)
        private const uint SessionSetupControlService = (uint)XmcsmService.SessionSetup;    // 0x04000000
        private const uint TerminalDataControlService = (uint)XmcsmService.TerminalData;    // 0x01080000
        private const uint BareTadControlService = (uint)XmcsmService.BareTadControl;       // 0x00080000
        private const uint SessionNotifyControlService = (uint)XmcsmService.SessionNotify;  // 0x00060000

        // TAD full-buffer SENTINEL size (TAD-Message-Formats.md section 22.6, 33/33 captures + NPL
        // corroboration). There is NO max element length. A non-final terminal-output BDAT MUST be
        // EXACTLY 255 data bytes (count 0xFF = "buffer full, more follows"), sent BARE (no RFI) as its
        // own datagram. 100 reads output elements until it gets one SHORTER than 255 - that short element
        // is the terminator and MUST carry the RFI. A short non-final frame with no RFI is structurally
        // impossible and is exactly what crashed 100 with "Illegal element length" (RFIRUT). This is why
        // chunking at 128/240 failed identically: both are short non-final frames with no RFI. Total
        // reply length is unbounded when chunked at 255.
        private const int FullBufferChunk = 255;

        // Segmented-output mode chunk size (mode CompleteSegments). Any value < 255 works: the connect-to
        // RECEIVER (cos-conn-to-e02.prog, tad_rx_BDAT_01 @ram:2b62, Ghidra-decoded 2026-07-08) renders
        // `count` bytes per BDAT element with NO special-casing of count==0xFF — i.e. the "255 = more
        // follows" sentinel is a fiction, the binary has no such concept. 240 is even and leaves headroom
        // for the trailing SYCN/prompt/RFI on the final segment. See COS-CONN-TO-E02-Analysis.md §5b.
        private const int SegmentChunk = 240;

        // Intra-pair spacing for streamed terminal output. The real host transmits the two 255-byte
        // continuations of a pair ~45-47 ms apart and NEVER back-to-back (TAD-Message-Formats.md 22.16,
        // measured in two independent captures). This gap is the prime suspect for why a byte-identical
        // emulated burst rendered only its final chunk; DrainSessionOutput holds the second chunk of a pair
        // until this many milliseconds have elapsed since the first (the periodic pump re-drains to send it).
        private const double ContinuationPairGapMillis = 46.0;

        // The MOTD frame's TAD message STRUCTURE is VERIFIED from conn-to-d102 frame 62: BMMX(01,0000) /
        // ECKM(01) / a BDAT banner / SYCN(0002 WaitUser) / a BDAT "ENTER " prompt / RFI. The banner text
        // itself is now generated per-session (dynamic date, configurable MOTD line, HOST id) by
        // BuildMotdPayload; the surrounding chain and its pads are reproduced byte-for-byte by the builder.

        // The connect-accept parameter trailer, VERIFIED from the 102 capture: two parameter blocks
        // 01 02 0000 (param 1 = 0) and 02 02 000A (param 2 = 0x000A).
        private static readonly byte[] AcceptTrailer = { 0x01, 0x02, 0x00, 0x00, 0x02, 0x02, 0x00, 0x0A };

        // The command registry: the single source of truth the "help" command lists. Descriptions are terse
        // and 1..4 are grouped so the whole listing fits ONE < 255-byte terminal frame - multi-chunk
        // terminal output is not rendered by 100 (it displays only the final chunk), so every command reply
        // must stay under one buffer. Numbered aliases (1..4) dispatch through the terminal menu; the rest
        // are handled directly in HandleCommand.
        private static readonly CommandDoc[] CommandRegistry =
        {
            new CommandDoc("1-4", "time / date / echo / disc"),
            new CommandDoc("stat", "session info"),
            new CommandDoc("who", "list users"),
            new CommandDoc("tell N txt", "message a user"),
            new CommandDoc("wall txt", "broadcast"),
            new CommandDoc("list", "servers | service | route"),
            new CommandDoc("help", "this list"),
        };

        /// <summary>
        /// Notifies a listener that a TAD session opened or closed (for observability and swap-out).
        /// </summary>
        /// <param name="tadNumber">
        /// The session's TAD number (ttyN).
        /// </param>
        /// <param name="clientSystem">
        /// The client system (node) that owns the session.
        /// </param>
        public delegate void SessionLifecycle(int tadNumber, ushort clientSystem);

        /// <summary>
        /// Supplies the registered-server snapshot for the <c>list servers</c> command (the host wires this
        /// to its <c>XmsgServerHost.DescribeServers</c>).
        /// </summary>
        /// <returns>
        /// The registered servers, or an empty list.
        /// </returns>
        public delegate IReadOnlyList<XmsgServerInfo> ServerDirectoryQuery();

        /// <summary>
        /// Supplies the routing-table text for the <c>list route</c> command (the host wires this to its
        /// topology / routing table).
        /// </summary>
        /// <returns>
        /// The route report lines (without a trailing prompt).
        /// </returns>
        public delegate string RouteReportQuery();

        /// <summary>
        /// Occurs when a new TAD session is opened.
        /// </summary>
        public event SessionLifecycle? SessionOpened;

        /// <summary>
        /// Occurs when a TAD session is closed.
        /// </summary>
        public event SessionLifecycle? SessionClosed;

        /// <summary>
        /// Gets or sets the callback that lists registered servers for <c>list servers</c>; when null the
        /// command reports only this server.
        /// </summary>
        public ServerDirectoryQuery? ServerDirectory { get; set; }

        /// <summary>
        /// Gets or sets the callback that supplies the route table for <c>list route</c>; when null the
        /// command reports that routing is unavailable.
        /// </summary>
        public RouteReportQuery? RouteReport { get; set; }

        /// <summary>
        /// Initialises the TAD server.
        /// </summary>
        /// <param name="clock">
        /// Supplies the current time for the MOTD/date/time commands (injected for deterministic tests).
        /// </param>
        /// <param name="users">
        /// The login accounts. When null, the default <c>SYSTEM</c>/<c>SYSTEM</c> directory is used.
        /// </param>
        /// <param name="motdLine">
        /// The middle banner line of the login greeting. When null or blank, the built-in
        /// <c>Emulated TAD server version vN.N.N</c> banner (the assembly version) is used.
        /// </param>
        public TadServer(Func<DateTime> clock, TadUserDirectory? users = null, string? motdLine = null)
        {
            _clock = clock ?? throw new ArgumentNullException(nameof(clock));
            _users = users ?? new TadUserDirectory();
            _menu = new TadTerminalMenu();
            _sessions = new Dictionary<uint, TadServerSession>();
            _sessionByPort = new Dictionary<ushort, TadServerSession>();
            _sessionList = new List<TadServerSession>();
            _outputAckIndex = new Dictionary<uint, TadServerSession>();
            string line = string.IsNullOrWhiteSpace(motdLine)
                ? "Emulated TAD server version " + ServerVersion()
                : motdLine.Trim();
            // Keep the banner well under the terminal element-length cap (see MenuReplyChunk): the date and
            // host-id lines add ~55 bytes, so cap the configurable line at 64 to stay inside one safe frame.
            _motdLine = line.Length > 64 ? line.Substring(0, 64) : line;
        }

        /// <summary>
        /// Gets the registered server name (<c>*TADADM</c>).
        /// </summary>
        public string Name
        {
            get { return ServerName; }
        }

        /// <summary>
        /// Gets the well-known logical port (2).
        /// </summary>
        public int LogicalPort
        {
            get { return ServerLogicalPort; }
        }

        /// <summary>
        /// Gets the wire reply-from port (<c>0x0156</c>).
        /// </summary>
        public ushort WirePort
        {
            get { return TadAdminWirePort; }
        }

        /// <summary>
        /// Gets the number of currently-active sessions.
        /// </summary>
        public int SessionCount
        {
            get { return _sessions.Count; }
        }

        /// <summary>
        /// Gets the maximum concurrent sessions (Free SPs capacity).
        /// </summary>
        public int SessionCapacity
        {
            get { return MaxSessions; }
        }

        /// <summary>
        /// Handles a datagram routed to this server: an XSLET connect letter (to port 0) or session data
        /// (to our wire port or a session port).
        /// </summary>
        /// <param name="incoming">
        /// The received datagram.
        /// </param>
        /// <param name="transport">
        /// The node transport used to build reply frames.
        /// </param>
        /// <returns>
        /// The reply frames, in order.
        /// </returns>
        public IReadOnlyList<XmsgFrame> Handle(XmsgFrame incoming, IXmsgServerTransport transport)
        {
            if (incoming == null || incoming.SubHeader == null)
            {
                return Array.Empty<XmsgFrame>();
            }

            // A connect letter is addressed to port 0 with XMCSM XSLET (0x04000041).
            if (incoming.SubHeader.DestinationPort == 0x0000
                && incoming.ControlService == XsletLetterControlService)
            {
                return OnConnect(incoming, transport);
            }

            // Session traffic: find the session by the stable client source endpoint.
            TadServerSession? session = FindSession(incoming);
            if (session == null)
            {
                return Array.Empty<XmsgFrame>();
            }

            if (incoming.ControlService == SessionSetupControlService)
            {
                return OnSessionSetup(session, incoming, transport);
            }

            if (HasOpcode(incoming, TmodOpcode) && !session.MotdSent)
            {
                return OnTerminalSetup(session, incoming, transport);
            }

            // Bring-up ladder, client-driven (XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md §1, VERIFIED
            // in both reference captures): the client's ESCA is answered with ESRS + RESE#1; its first
            // RECO with RESE#2; its second RECO with the MOTD banner. The real host NEVER volunteers
            // these — each step waits for the client frame.
            if (HasOpcode(incoming, EscaOpcode) && !session.MotdSent)
            {
                return OnEscape(session, transport);
            }

            if (HasOpcode(incoming, RecoOpcode) && !session.MotdSent)
            {
                return OnResetConfirm(session, transport);
            }

            if (HasOpcode(incoming, DconOpcode))
            {
                CloseSession(session);
                return Array.Empty<XmsgFrame>();
            }

            if (HasOpcode(incoming, BdatOpcode))
            {
                return OnTerminalInput(session, incoming, transport);
            }

            // A 7DUMM (opcode 0x18) is 100's CONSUMPTION signal during a burst: it sends one back per
            // continuation it has displayed (1:1). Count it so DrainSessionOutput may release the next
            // continuation / the final terminator only after the prior chunk is actually on screen.
            if (HasOpcode(incoming, DummOpcode) && session.OutputActive)
            {
                session.NoteDummConsumed();
                List<XmsgFrame> drained = new List<XmsgFrame>();
                DrainSessionOutput(session, transport, drained);
                return drained;
            }

            // CERS / DUMM and other bare control frames need no reply (the node ACKs them).
            return Array.Empty<XmsgFrame>();
        }

        /// <summary>
        /// Returns true when the port is this server's wire port or one of its session ports.
        /// </summary>
        /// <param name="port">
        /// The destination wire port.
        /// </param>
        /// <returns>
        /// True when this server owns the port.
        /// </returns>
        public bool OwnsPort(ushort port)
        {
            return port == TadAdminWirePort || _sessionByPort.ContainsKey(port);
        }

        /// <summary>
        /// Sends the next window-permitted output chunk(s) for every session with a burst in progress.
        /// </summary>
        /// <remarks>
        /// The node calls this after processing each incoming frame (an ACK that opened the window, or a
        /// 7DUMM), so a multi-chunk terminal reply streams out as 100 acknowledges the outstanding chunks.
        /// </remarks>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The output frames now permitted by each session's flow-control window (empty when none).
        /// </returns>
        public IReadOnlyList<XmsgFrame> DrainPending(IXmsgServerTransport transport)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            for (int i = 0; i < _sessionList.Count; i++)
            {
                TadServerSession session = _sessionList[i];

                // Advance any command-reply burst first (this may complete it and clear OutputActive)...
                if (session.OutputActive)
                {
                    DrainSessionOutput(session, transport, outgoing);
                }

                // ...then, if the session is now idle and a tell / wall message is queued, begin the
                // asynchronous inject burst (the queued text + a redrawn "# " prompt) AND send its first
                // batch in this same drain, so an inject lands on the next 7DUMM rather than lagging a cycle.
                if (!session.OutputActive && session.IsLoggedIn && session.HasPendingOutput)
                {
                    StartOutputBurst(session, session.TakePendingOutput() + TerminalPrompt);
                    DrainSessionOutput(session, transport, outgoing);
                }
            }

            return outgoing;
        }

        /// <summary>
        /// Notifies the server that a remote node ACKed one of our frames, so the owning session can
        /// release its flow-control window if the ACK was for an outstanding output chunk.
        /// </summary>
        /// <param name="remoteNode">
        /// The node that ACKed.
        /// </param>
        /// <param name="ackedFlags1">
        /// The Flags 1 the ACK echoes.
        /// </param>
        public void NotifyAck(ushort remoteNode, ushort ackedFlags1)
        {
            uint key = OutputAckKey(remoteNode, ackedFlags1);
            if (_outputAckIndex.TryGetValue(key, out TadServerSession? session))
            {
                _outputAckIndex.Remove(key);
                session.ConfirmOutputAck(ackedFlags1);
            }
        }

        /// <summary>
        /// Opens a session for a connect letter and returns the connect-accept.
        /// </summary>
        /// <param name="request">
        /// The connect letter.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The accept frame (or empty when at capacity).
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnConnect(XmsgFrame request, IXmsgServerTransport transport)
        {
            if (_sessions.Count >= MaxSessions)
            {
                // No free SPs. A refusal letter is a later refinement; for now do not open the session.
                return Array.Empty<XmsgFrame>();
            }

            ushort clientSystem = request.SubHeader!.SourceSystem;
            ushort clientPort = request.SubHeader.SourcePort;

            // A re-connect (same asker system+port) must tear down the old session, not leak its wire
            // port. GOD-LLM answer: minting a fresh terminal port per session is normal, but the old one
            // must be released or 100 keeps addressing a stale port and its magic-number creation fails.
            if (_sessions.TryGetValue(SessionKey(clientSystem, clientPort), out TadServerSession? stale))
            {
                CloseSession(stale);
            }

            // The wire port keeps incrementing (a fresh port per session is normal); the TAD number
            // (ttyN) is the LOWEST free 1..MaxSessions so a freed tty is reused, not left climbing.
            ushort sessionPort = transport.AllocateSessionPort();
            int tadNumber = AllocateTadNumber();

            TadServerSession session = new TadServerSession(
                request.Header.SourceNode, clientSystem, clientPort, sessionPort, tadNumber);
            ExtractConnectStrings(request, session);

            _sessions[SessionKey(clientSystem, clientPort)] = session;
            _sessionByPort[sessionPort] = session;
            _sessionList.Add(session);
            SessionOpened?.Invoke(tadNumber, clientSystem);

            // The connect-accept: XMCSM 0x04000041, frame-flags Setup, role WakeOnStatus, from the TADADM
            // wire port, with the verified parameter trailer.
            //
            // ANSWERING the connect letter: echo its Flags 1. CHECKED against a real SINTRAN on
            // 2026-08-09, after an audit claimed this was the same defect as the FA connection
            // confirm (which must NOT echo). It is not. From
            // DOC/captures/ARCHIVE-2026-07/ethernet-conn-to-D100-from-102-WORKING-2026-08-01.pcapng,
            // node 102 asking node 100:
            //
            //   f1   102->100  F1=0021  connect letter to *TADADM
            //   f17  100->102  F1=0021  the accept          <- SAME number
            //   f33  102->100  F1=0022  session-setup
            //   f49  100->102  F1=0022  the port assignment <- SAME number
            //   f57  100->102  F1=0023  the priming DUMM    <- NEXT number, nobody asked for it
            //
            // LIMIT OF THAT EVIDENCE: the server's three frames run 0021, 0022, 0023, which an
            // own-counter starting at 0x21 would also produce. So the capture confirms the BYTES we
            // emit are right and does NOT separate "echo" from "own counter that happens to line
            // up". Do not restate it as proof of the rule. Separating them needs a capture where the
            // two sides' counters have diverged - see task #32.
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            outgoing.Add(transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, XsletLetterControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.WakeOnStatus, AcceptTrailer,
                request.Header!.Flags1));
            return outgoing;
        }

        /// <summary>
        /// Answers the session-setup with the port-assignment (TAD 0x07 carrying our session endpoint).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="request">
        /// The session-setup frame.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The port-assignment frame.
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnSessionSetup(TadServerSession session, XmsgFrame request, IXmsgServerTransport transport)
        {
            session.SessionSetupSeen = true;

            // Captured 102 trailer (24 bytes) with our system + session-port bytes substituted:
            //   00 | 07 05 00 00 <sys> <portHi> <portLo> | 1F 03 4C 00 00 | 00 | 0B 02 03 00 | 15 02 01 08 | FF 00
            byte sysByte = (byte)transport.NodeNumber;
            byte portHi = (byte)(session.SessionWirePort >> 8);
            byte portLo = (byte)(session.SessionWirePort & 0xFF);
            byte[] trailer =
            {
                0x00,
                0x07, 0x05, 0x00, 0x00, sysByte, portHi, portLo,
                0x1F, 0x03, 0x4C, 0x00, 0x00,
                0x00,
                0x0B, 0x02, 0x03, LunIndexByte,
                0x15, 0x02, 0x01, 0x08,
                0xFF, 0x00,
            };

            // The port assignment ANSWERS the session-setup, so it echoes. The priming DUMM below
            // does NOT - nothing asked for it, it is bring-up we start ourselves - so it takes the
            // next number. Two data frames sharing one Flags 1 is not something any capture shows.
            //
            // MEASURED against a real SINTRAN 2026-08-09: f33 setup F1=0022 -> f49 assignment
            // F1=0022 -> f57 priming DUMM F1=0023. Exactly this split. See the fuller note and its
            // limit on the connect-accept above, and
            // DOC/captures/ARCHIVE-2026-07/ethernet-conn-to-D100-from-102-WORKING-2026-08-01.pcapng.
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            outgoing.Add(transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, SessionSetupControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.WakeOnStatus, trailer,
                request.Header!.Flags1));

            // Post-port-assign bring-up: the priming DUMM (terminal-data class 0x0108) so 100 drives its
            // TMOD/TTYP negotiation, which we then answer with the MOTD burst (OnTerminalSetup). This was
            // the SendTerminalBringup=true behaviour the live runner used.
            outgoing.Add(BuildSession(session, transport, TerminalDataControlService,
                (byte)XmsgFrameFlags.DataB, (byte)XmsgSendOptions.None,
                new TadMessageBuilder().Dumm().Build()));
            return outgoing;
        }

        /// <summary>
        /// Captures 100's terminal-setup (TMOD chain) parameters. The real host answers NOTHING here —
        /// the bring-up continues only when the client sends its ESCA (see <see cref="OnEscape"/>).
        /// The earlier one-shot burst (0x20 + RESE + RESE + MOTD, all unprompted) deviated from every
        /// captured session, where each step is client-driven
        /// (XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md §1).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="request">
        /// The terminal-setup frame.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// No frames — the host is silent until the client's ESCA.
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnTerminalSetup(TadServerSession session, XmsgFrame request, IXmsgServerTransport transport)
        {
            CaptureNegotiation(request, session);
            return Array.Empty<XmsgFrame>();
        }

        /// <summary>
        /// Answers the client's ESCA (bring-up): ESRS (0x20, class 0x0008, ff 0x86) followed by RESE #1
        /// (class 0x0108, ff 0x96), both from the session port — the exact reply pair in both reference
        /// captures (ESRS answers the escape; the RESE opens the reset/confirm exchange).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// ESRS + RESE #1.
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnEscape(TadServerSession session, IXmsgServerTransport transport)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            // ESRS (XMCSM 0x00080000): TAD opcode 0x20, empty, ff 0x86 (Setup).
            outgoing.Add(BuildSession(session, transport, BareTadControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.None,
                new TadMessageBuilder().Raw((TadOp)0x20, ReadOnlySpan<byte>.Empty).Build()));

            // RESE #1 (XMCSM 0x01080000), ff 0x96 (DataA) — first of the observed 96/92 alternation.
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                new TadMessageBuilder().Rese().Build()));

            session.BringupRecoCount = 0;
            return outgoing;
        }

        /// <summary>
        /// Advances the bring-up on the client's RECO: the first RECO is answered with RESE #2
        /// (ff 0x92), the second with the MOTD banner (ff 0x96) — completing the captured ladder
        /// ESCA → ESRS+RESE, RECO → RESE, RECO → BANNER.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// RESE #2, or the MOTD, or nothing when the ladder is already complete.
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnResetConfirm(TadServerSession session, IXmsgServerTransport transport)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            session.BringupRecoCount++;

            if (session.BringupRecoCount == 1)
            {
                // RESE #2, ff 0x92 (DataB) — the second of the observed 96/92 alternation.
                outgoing.Add(BuildSession(session, transport, TerminalDataControlService,
                    (byte)XmsgFrameFlags.DataB, (byte)XmsgSendOptions.None,
                    new TadMessageBuilder().Rese().Build()));
                return outgoing;
            }

            // Second RECO: the MOTD banner (ff 0x96), banner generated for this host and tty number.
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                BuildMotdPayload(transport.NodeNumber, session.TadNumber)));
            session.MotdSent = true;
            return outgoing;
        }

        /// <summary>
        /// Builds the MOTD login banner payload for this host.
        /// </summary>
        /// <param name="nodeNumber">
        /// This host's node (CPU) id, shown in the <c>--- HOST ID:nnn TAD:n ---</c> line.
        /// </param>
        /// <param name="tadNumber">
        /// This session's TAD (tty) number, shown in the host-id line.
        /// </param>
        /// <returns>
        /// The TAD message chain: BMMX / ECKM / banner BDAT / SYCN / "ENTER " BDAT / RFI.
        /// </returns>
        /// <remarks>
        /// The chain structure and its intrinsic pads are the capture-verified MOTD; only the three banner
        /// lines are generated (dynamic date/time, the configurable MOTD line, and the host id / tty).
        /// </remarks>
        private byte[] BuildMotdPayload(ushort nodeNumber, int tadNumber)
        {
            // Raw 0x01 strategy bytes are the captured MOTD values (BMMX 04 03 01 00 00, ECKM 00 03 01 01).
            return new TadMessageBuilder()
                .Bmmx(0x01, 0x0000)
                .Eckm(0x01)
                .BdatText(BuildBanner(nodeNumber, tadNumber))
                .Sycn(SycnState.WaitingForUsername)
                .BdatText("\r\nENTER ")
                .Rfi()
                .Build();
        }

        /// <summary>
        /// Builds the three-line banner text: the SINTRAN-style date/time, the MOTD line, and the
        /// host-id / tty line.
        /// </summary>
        /// <param name="nodeNumber">
        /// This host's node (CPU) id.
        /// </param>
        /// <param name="tadNumber">
        /// This session's TAD (tty) number.
        /// </param>
        /// <returns>
        /// The banner string (leading and trailing CRLF, matching the captured layout).
        /// </returns>
        private string BuildBanner(ushort nodeNumber, int tadNumber)
        {
            DateTime now = _clock();
            string month = now.ToString("MMMM", CultureInfo.InvariantCulture).ToUpperInvariant();
            string time = now.ToString("HH.mm.ss", CultureInfo.InvariantCulture);
            string day = now.Day.ToString(CultureInfo.InvariantCulture).PadLeft(2);

            // Reproduce the observed SINTRAN date layout with current values:
            //   " HH.MM.SS     {day,2} MONTH   YYYY"  (5 spaces + right-aligned day = 6 before a 1-digit day).
            string dateLine = " " + time + "     " + day + " " + month + "   "
                + now.Year.ToString(CultureInfo.InvariantCulture);

            // Banner = CRLF + date + CRLF + " " + MOTD + CRLF + "--- HOST ID:nnn TAD:n ---" + CRLF. Same
            // layout as the captured banner (date / "SINTRAN III - VSX/500" / "--- ... ID:102 ---"), text now
            // generated; the tty number lets the user see which TAD line this session was assigned.
            StringBuilder banner = new StringBuilder(96);
            banner.Append("\r\n").Append(dateLine);
            banner.Append("\r\n ").Append(_motdLine);
            banner.Append("\r\n--- HOST ID:").Append(nodeNumber.ToString(CultureInfo.InvariantCulture))
                  .Append(" TAD:").Append(tadNumber.ToString(CultureInfo.InvariantCulture)).Append(" ---");
            banner.Append("\r\n");
            return banner.ToString();
        }

        /// <summary>
        /// Gets the assembly version as <c>vMajor.Minor.Build</c> for the default MOTD banner.
        /// </summary>
        /// <returns>
        /// The version string (for example <c>v0.0.1</c>).
        /// </returns>
        private static string ServerVersion()
        {
            Version? version = typeof(TadServer).Assembly.GetName().Version;
            return version == null ? "v0.0.1" : "v" + version.ToString(3);
        }

        /// <summary>
        /// Handles a typed line (BDAT) during login or the command loop.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="frame">
        /// The input frame.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The reply frames.
        /// </returns>
        private IReadOnlyList<XmsgFrame> OnTerminalInput(TadServerSession session, XmsgFrame frame, IXmsgServerTransport transport)
        {
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            string line = ExtractBdatText(frame).Trim();

            switch (session.Phase)
            {
                case TadServerLoginPhase.Username:
                    HandleUsername(session, transport, outgoing, line);
                    break;

                case TadServerLoginPhase.Password:
                    HandlePassword(session, transport, outgoing, line);
                    break;

                default:
                    HandleCommand(session, transport, outgoing, line);
                    break;
            }

            return outgoing;
        }

        /// <summary>
        /// Handles the username line: passwordless accounts log straight in, everyone else is prompted
        /// for a password (never revealing whether the username was valid).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list.
        /// </param>
        /// <param name="line">
        /// The typed username.
        /// </param>
        private void HandleUsername(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string line)
        {
            session.PendingUsername = line;

            if (_users.TryGet(line, out TadUser user) && !user.RequiresPassword)
            {
                // Passwordless account: log straight in.
                LogIn(session, transport, outgoing, user);
                return;
            }

            // Ask for the password with echo off (ECKM FF). No logged-in state asserted yet.
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("\r\n").Sycn(SycnState.UsernameAccepted).Cesc(CescState.EscapeDisabled).Build()));
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("PASSWORD: ").Eckm(EchoStrategy.NoEcho).Rfi().Build()));
            session.Phase = TadServerLoginPhase.Password;
        }

        /// <summary>
        /// Handles the password line: validates against the pending user's account.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list.
        /// </param>
        /// <param name="line">
        /// The typed password.
        /// </param>
        private void HandlePassword(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string line)
        {
            bool valid = _users.TryGet(session.PendingUsername, out TadUser user)
                && string.Equals(line, user.Password, StringComparison.OrdinalIgnoreCase);

            if (valid)
            {
                LogIn(session, transport, outgoing, user);
                return;
            }

            if (session.LoginFaults + 1 >= MaxLoginFaults)
            {
                // Third strike: taunt and tear the session down (0xFD -> asker DCON).
                // Instant disconnect, same path as menu "4": the teardown ladder + the host DCON (0x09),
                // NOT the bare 0xFD notification (which leaves 100's 1-minute idle timer running).
                session.LoginFaults++;
                AppendTeardownLadder(session, transport, outgoing, "\r\nBYE HACKER!\r\n");
                outgoing.Add(BuildDconIndication(session, transport));
                CloseSession(session);   // free the session after the forced teardown
                return;
            }

            // Wrong credentials: restore echo, report the failure, back to the username prompt.
            session.LoginFaults++;
            session.Phase = TadServerLoginPhase.Username;
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("\r\nInvalid user/password").Eckm(EchoStrategy.LocalEcho)
                .Sycn(SycnState.WaitingForUsername).BdatText("\r\nENTER ").Rfi().Build()));
        }

        /// <summary>
        /// Completes a login: restores echo, confirms "OK", asserts the logged-in state and prompt.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list.
        /// </param>
        /// <param name="user">
        /// The account that logged in.
        /// </param>
        private void LogIn(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, TadUser user)
        {
            session.LoginFaults = 0;
            session.Phase = TadServerLoginPhase.LoggedIn;
            session.Username = user.Username;

            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("\r\n").Eckm(EchoStrategy.LocalEcho).BdatText("OK")
                .Sycn(SycnState.PasswordAccepted).Cesc(CescState.EscapeEnabled).Build()));
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("\r\n").Sycn(SycnState.LoggedIn).BdatText("# ").Rfi().Build()));
        }

        /// <summary>
        /// Handles a logged-in command line: "stat", or the menu (with its teardown modes).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list.
        /// </param>
        /// <param name="line">
        /// The typed command.
        /// </param>
        private void HandleCommand(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string line)
        {
            if (string.Equals(line, "stat", StringComparison.OrdinalIgnoreCase))
            {
                EmitMenuReply(session, transport, outgoing, BuildStatReport(session, transport));
                return;
            }

            // "3" / "echo": a DIAGNOSTIC that deliberately renders as three 255-sentinel frames, each with
            // a distinct "ECHO FRAME n OF 3" marker + a repeated digit, so a live run shows exactly which
            // frame(s) 100 renders (and which the multi-chunk flow-control handshake is dropping).
            if (string.Equals(line, "3", StringComparison.OrdinalIgnoreCase)
                || string.Equals(line, "echo", StringComparison.OrdinalIgnoreCase))
            {
                EmitMenuReply(session, transport, outgoing, BuildEchoDiagnostic());
                return;
            }

            // tty subsystem commands: who (list sessions), tell N text (message one user), wall text
            // (broadcast). tell / wall queue the message into the target session(s) - it is pushed to those
            // terminals asynchronously by DrainPending, NOT returned to the caller.
            if (string.Equals(line, "who", StringComparison.OrdinalIgnoreCase))
            {
                EmitMenuReply(session, transport, outgoing, BuildWhoReport(session));
                return;
            }

            if (StartsWithCommand(line, "tell"))
            {
                HandleTell(session, transport, outgoing, line.Substring(4).Trim());
                return;
            }

            if (StartsWithCommand(line, "wall"))
            {
                HandleWall(session, transport, outgoing, line.Substring(4).Trim());
                return;
            }

            // Introspection: help (the command registry) and list servers / list service / list route.
            if (string.Equals(line, "help", StringComparison.OrdinalIgnoreCase))
            {
                EmitMenuReply(session, transport, outgoing, BuildHelpReport());
                return;
            }

            if (StartsWithCommand(line, "list"))
            {
                EmitMenuReply(session, transport, outgoing, BuildListReport(line.Substring(4).Trim()));
                return;
            }

            TadMenuResult result = _menu.Handle(line, _clock());
            switch (result.Mode)
            {
                case TadDisconnectMode.Ladder:
                    AppendTeardownLadder(session, transport, outgoing, result.Output);
                    CloseSession(session);   // free the session (who / TAD-number reuse) - we initiated the teardown
                    break;

                case TadDisconnectMode.LadderThenDcon:
                    AppendTeardownLadder(session, transport, outgoing, result.Output);
                    outgoing.Add(BuildDconIndication(session, transport));
                    CloseSession(session);   // free the session; the teardown frames are already queued above
                    break;

                default:
                    EmitMenuReply(session, transport, outgoing, result.Output);
                    break;
            }
        }

        /// <summary>
        /// Queues a message to the session on the given TAD (tty) number; it is pushed to that terminal
        /// asynchronously by <see cref="DrainPending"/>. Public so another server / subsystem can inject.
        /// </summary>
        /// <param name="tadNumber">
        /// The target session's TAD number.
        /// </param>
        /// <param name="text">
        /// The message text to display.
        /// </param>
        /// <returns>
        /// 1 when a logged-in session with that TAD number received the message; 0 when none matched.
        /// </returns>
        public int InjectToTad(int tadNumber, string text)
        {
            for (int i = 0; i < _sessionList.Count; i++)
            {
                TadServerSession session = _sessionList[i];
                if (session.TadNumber == tadNumber && session.IsLoggedIn)
                {
                    session.Enqueue(text);
                    return 1;
                }
            }

            return 0;
        }

        /// <summary>
        /// Queues a message to every logged-in session of the given user (case-insensitive).
        /// </summary>
        /// <param name="username">
        /// The target username.
        /// </param>
        /// <param name="text">
        /// The message text to display.
        /// </param>
        /// <returns>
        /// The number of sessions that received the message.
        /// </returns>
        public int InjectToUser(string username, string text)
        {
            int count = 0;
            for (int i = 0; i < _sessionList.Count; i++)
            {
                TadServerSession session = _sessionList[i];
                if (session.IsLoggedIn && string.Equals(session.Username, username, StringComparison.OrdinalIgnoreCase))
                {
                    session.Enqueue(text);
                    count++;
                }
            }

            return count;
        }

        /// <summary>
        /// Queues a message to every logged-in session (a wall broadcast, sender included).
        /// </summary>
        /// <param name="text">
        /// The message text to display.
        /// </param>
        /// <returns>
        /// The number of sessions that received the message.
        /// </returns>
        public int Broadcast(string text)
        {
            int count = 0;
            for (int i = 0; i < _sessionList.Count; i++)
            {
                if (_sessionList[i].IsLoggedIn)
                {
                    _sessionList[i].Enqueue(text);
                    count++;
                }
            }

            return count;
        }

        /// <summary>
        /// Builds the "who" listing: every logged-in session with its tty and user, the caller marked
        /// with a <c>===></c> arrow (SINTRAN style). Kept under one 255-byte buffer.
        /// </summary>
        /// <param name="caller">
        /// The session that ran "who".
        /// </param>
        /// <returns>
        /// The listing text (ending with the prompt).
        /// </returns>
        private string BuildWhoReport(TadServerSession caller)
        {
            StringBuilder sb = new StringBuilder(256);
            sb.Append("\r\n  TAD   User\r\n");
            for (int i = 0; i < _sessionList.Count; i++)
            {
                TadServerSession session = _sessionList[i];
                if (!session.IsLoggedIn)
                {
                    continue;
                }

                sb.Append(session == caller ? "===> " : "     ");
                sb.Append("tty").Append(session.TadNumber)
                  .Append("  ").Append(session.Username.Length != 0 ? session.Username : "(anon)")
                  .Append("\r\n");
            }

            sb.Append("# ");
            return sb.ToString();
        }

        /// <summary>
        /// Handles "tell ttyN|user text": injects the message into the target session(s) and
        /// confirms to the caller.
        /// </summary>
        /// <param name="caller">
        /// The session that ran "tell".
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The caller's reply list.
        /// </param>
        /// <param name="args">
        /// The command arguments after "tell ".
        /// </param>
        private void HandleTell(TadServerSession caller, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string args)
        {
            int space = args.IndexOf(' ');
            if (space <= 0)
            {
                EmitMenuReply(caller, transport, outgoing, "\r\nusage: tell <ttyN|user> <text>\r\n# ");
                return;
            }

            string target = args.Substring(0, space);
            string text = args.Substring(space + 1).Trim();
            string message = FormatInterUserMessage(caller, text);

            // A "ttyN" or bare-number target addresses a TAD number; anything else is a username.
            string numeric = target.StartsWith("tty", StringComparison.OrdinalIgnoreCase) ? target.Substring(3) : target;
            int delivered = int.TryParse(numeric, out int tad)
                ? InjectToTad(tad, message)
                : InjectToUser(target, message);

            string reply = delivered > 0
                ? "\r\nsent to " + delivered + " session(s)\r\n# "
                : "\r\nno such user/tty: " + target + "\r\n# ";
            EmitMenuReply(caller, transport, outgoing, reply);
        }

        /// <summary>
        /// Handles "wall text": broadcasts the message to every logged-in session (sender included)
        /// and confirms to the caller.
        /// </summary>
        /// <param name="caller">
        /// The session that ran "wall".
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The caller's reply list.
        /// </param>
        /// <param name="text">
        /// The broadcast text after "wall ".
        /// </param>
        private void HandleWall(TadServerSession caller, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string text)
        {
            if (text.Length == 0)
            {
                EmitMenuReply(caller, transport, outgoing, "\r\nusage: wall <text>\r\n# ");
                return;
            }

            int delivered = Broadcast(FormatInterUserMessage(caller, text));
            EmitMenuReply(caller, transport, outgoing, "\r\nbroadcast to " + delivered + " session(s)\r\n# ");
        }

        /// <summary>
        /// Formats a Unix-style inter-user message: <c>Message from user at TAD n: text</c>.
        /// </summary>
        /// <param name="caller">
        /// The sending session.
        /// </param>
        /// <param name="text">
        /// The message body.
        /// </param>
        /// <returns>
        /// The formatted message (leading and trailing CRLF).
        /// </returns>
        private static string FormatInterUserMessage(TadServerSession caller, string text)
        {
            return "\r\nMessage from " + (caller.Username.Length != 0 ? caller.Username : "?")
                + " at TAD " + caller.TadNumber + ": " + text + "\r\n";
        }

        /// <summary>
        /// Returns true when a command line is the given command word, alone or followed by arguments
        /// (case-insensitive), for example "tell" or "tell 2 hi".
        /// </summary>
        /// <param name="line">
        /// The command line.
        /// </param>
        /// <param name="command">
        /// The command word to match.
        /// </param>
        /// <returns>
        /// True on a match.
        /// </returns>
        private static bool StartsWithCommand(string line, string command)
        {
            return line.Length >= command.Length
                && string.Compare(line, 0, command, 0, command.Length, StringComparison.OrdinalIgnoreCase) == 0
                && (line.Length == command.Length || line[command.Length] == ' ');
        }

        /// <summary>
        /// Builds the "help" listing from the command registry: every command with a one-line description.
        /// </summary>
        /// <returns>
        /// The help text (ending with the prompt).
        /// </returns>
        private static string BuildHelpReport()
        {
            StringBuilder sb = new StringBuilder(256);
            sb.Append("\r\n----- COMMANDS -----\r\n");
            for (int i = 0; i < CommandRegistry.Length; i++)
            {
                sb.Append("  ").Append(CommandRegistry[i].Name.PadRight(12))
                  .Append(CommandRegistry[i].Description).Append("\r\n");
            }

            sb.Append("# ");
            return sb.ToString();
        }

        /// <summary>
        /// Builds a "list servers | service | route" report for the given sub-command.
        /// </summary>
        /// <param name="sub">
        /// The sub-command text after "list ".
        /// </param>
        /// <returns>
        /// The report text (ending with the prompt).
        /// </returns>
        private string BuildListReport(string sub)
        {
            if (StartsWithCommand(sub, "servers") || string.Equals(sub, "server", StringComparison.OrdinalIgnoreCase))
            {
                return BuildServerList();
            }

            if (StartsWithCommand(sub, "service") || string.Equals(sub, "services", StringComparison.OrdinalIgnoreCase))
            {
                return BuildServiceList();
            }

            if (StartsWithCommand(sub, "route") || string.Equals(sub, "routes", StringComparison.OrdinalIgnoreCase))
            {
                string report = RouteReport != null ? RouteReport() : "(routing unavailable)";
                return "\r\n----- ROUTE -----\r\n" + report + "\r\n# ";
            }

            return "\r\nusage: list servers | list service | list route\r\n# ";
        }

        /// <summary>
        /// Builds the "list servers" report (COSMOS list-servers shape: name, port, sessions, free SPs).
        /// </summary>
        /// <returns>
        /// The report text (ending with the prompt).
        /// </returns>
        private string BuildServerList()
        {
            IReadOnlyList<XmsgServerInfo> servers = ServerDirectory != null
                ? ServerDirectory()
                : new XmsgServerInfo[] { new XmsgServerInfo(ServerName, ServerLogicalPort, TadAdminWirePort, SessionCount, SessionCapacity) };

            StringBuilder sb = new StringBuilder(320);
            sb.Append("\r\n----- SERVERS -----\r\n");
            sb.Append("  Name        Port  Sess  Free\r\n");
            for (int i = 0; i < servers.Count; i++)
            {
                XmsgServerInfo info = servers[i];
                sb.Append("  ").Append(info.Name.PadRight(12))
                  .Append(info.LogicalPort.ToString(CultureInfo.InvariantCulture).PadRight(6))
                  .Append(info.SessionCount.ToString(CultureInfo.InvariantCulture).PadRight(6))
                  .Append(info.FreeSlots.ToString(CultureInfo.InvariantCulture)).Append("\r\n");
            }

            sb.Append("# ");
            return sb.ToString();
        }

        /// <summary>
        /// Builds the "list service" report from the known XROUT services (mnemonic, code, description).
        /// </summary>
        /// <returns>
        /// The report text (ending with the prompt).
        /// </returns>
        private static string BuildServiceList()
        {
            // Compact mnemonic=code tokens, wrapped, TRUNCATED to fit one < 255-byte frame (the full XROUT
            // table is ~30 verbs; 100 renders only the last chunk of a multi-frame reply, so a long list
            // cannot be shown across frames). "help" points to this as a summary.
            IReadOnlyList<XmsgServiceInfo> services = XmsgKnownServices.All();
            StringBuilder sb = new StringBuilder(256);
            sb.Append("\r\n----- XROUT SERVICES -----\r\n");
            const int Budget = 230;   // leave room for the "...\r\n# " terminator under 255
            int onLine = 0;
            for (int i = 0; i < services.Count; i++)
            {
                string token = services[i].Mnemonic + "=" + services[i].ServiceByte.ToString("X2") + "  ";
                if (sb.Length + token.Length > Budget)
                {
                    sb.Append("...");
                    break;
                }

                sb.Append(token);
                if (++onLine == 4)
                {
                    sb.Append("\r\n");
                    onLine = 0;
                }
            }

            sb.Append("\r\n# ");
            return sb.ToString();
        }

        /// <summary>
        /// Appends the five-frame teardown ladder (farewell + CESC 00; BMMX/ECKM/CESC 00;
        /// "--EXIT--" + SYCN 000B; CESC 01; then the 0xFD notification).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list.
        /// </param>
        /// <param name="farewell">
        /// The farewell text for the first ladder frame.
        /// </param>
        private void AppendTeardownLadder(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string farewell)
        {
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText(farewell).Cesc(CescState.EscapeDisabled).Build()));
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .Bmmx(0x00, 0x0000).Eckm(EchoStrategy.Teardown).Cesc(CescState.EscapeDisabled).Build()));
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText("\r\n--EXIT--\r\n").Sycn(SycnState.LoggedOut).Build()));
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .Cesc(CescState.EscapeEnabled).Build()));
            outgoing.Add(BuildFdNotification(session, transport));
        }

        /// <summary>
        /// Begins a logged-in reply as a windowed output burst and sends the first batch of frames.
        /// </summary>
        /// <remarks>
        /// Output is streamed under the verified 22.16 output-queue algorithm: bare 255-byte continuation
        /// pairs spaced ~46 ms apart, then a short final frame carrying SYCN 000A + prompt BDAT + RFI. Only
        /// the first chunk goes out here; the remaining chunks are sent by <see cref="DrainSessionOutput"/>
        /// as the intra-pair timer elapses and 100 ACKs the outstanding ones.
        /// </remarks>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The reply list (receives the first windowed batch).
        /// </param>
        /// <param name="text">
        /// The full reply text (with its trailing prompt).
        /// </param>
        private void EmitMenuReply(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string text)
        {
            StartOutputBurst(session, text);
            DrainSessionOutput(session, transport, outgoing);
        }

        /// <summary>
        /// Begins a windowed output burst for a session: splits the trailing "# " prompt off the content
        /// so the burst trailer is the verified BDAT(content) + SYCN 000A + BDAT(prompt) + RFI.
        /// </summary>
        /// <remarks>
        /// Shared by command replies (<see cref="EmitMenuReply"/>) and asynchronous tty injects
        /// (tell / wall), so both stream under the same flow-control handshake.
        /// </remarks>
        /// <param name="session">
        /// The session whose output burst to begin.
        /// </param>
        /// <param name="text">
        /// The full reply text (with its trailing "# " prompt).
        /// </param>
        private static void StartOutputBurst(TadServerSession session, string text)
        {
            string body = text ?? string.Empty;

            // Separate the trailing "# " prompt from the content. The VERIFIED burst trailer
            // (TAD-Message-Formats.md 22.6 line 1420 / 22.8) is BDAT(content remainder) + [pad] + SYCN 000A
            // + BDAT(prompt) + RFI - the prompt is its OWN BDAT AFTER the SYCN, not part of the content BDAT.
            string prompt = TerminalPrompt;
            if (body.EndsWith(prompt, StringComparison.Ordinal))
            {
                body = body.Substring(0, body.Length - prompt.Length);
            }
            else
            {
                prompt = string.Empty;
            }

            session.BeginOutput(body, prompt);
        }

        /// <summary>
        /// Sends as many of a session's pending output chunks as the flow-control window allows.
        /// </summary>
        /// <remarks>
        /// Emits bare 255-byte continuations (count 0xFF, no RFI) in pairs spaced by
        /// <see cref="ContinuationPairGapMillis"/>, waiting between pairs for both chunks to be acked (and
        /// their 7DUMMs seen). The last continuation goes out ALONE, and the final (short, under 255) frame -
        /// which carries SYCN 000A + the prompt BDAT + RFI - is sent only once that last continuation is
        /// acked (outstanding count 0). Each sent frame's Flags 1 is recorded so an ACK can release the
        /// window; the periodic pump re-drives this to release a pair's second chunk once the gap elapses.
        /// </remarks>
        /// <param name="session">
        /// The session whose output to drain.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The list that receives the frames to send.
        /// </param>
        private void DrainSessionOutput(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing)
        {
            if (!session.OutputActive)
            {
                return;
            }

            if (OutputMode == TadOutputMode.CompleteSegments)
            {
                DrainSegmentedOutput(session, transport, outgoing);
                return;
            }

            // The VERIFIED real-host output-queue algorithm (TAD-Message-Formats.md 22.16): stream the
            // reply as 255-byte continuation PAIRS spaced ~46 ms apart, waiting between pairs for both
            // chunks to be acked (and their 7DUMMs seen); send the last continuation ALONE; then the FINAL
            // (short) chunk carrying SYCN 000A + prompt BDAT + RFI, only after that last continuation's ACK.
            DateTime now = _clock();
            while (true)
            {
                bool nextIsFinal = session.OutputContent.Length - session.OutputOffset < FullBufferChunk;

                if (nextIsFinal)
                {
                    if (session.OutputFinalSent)
                    {
                        session.OutputActive = false;   // burst complete
                        return;
                    }

                    // The final must not go out mid-pair, and only after the last continuation is DELIVERED
                    // (acked -> window drained to 0). It is NOT gated on the last continuation's 7DUMM: 22.16
                    // shows the final precedes the last DUMM (the final chunk itself triggers no DUMM).
                    if (session.PairAwaitingSecond || session.OutstandingOutputCount > 0)
                    {
                        return;
                    }

                    string tail = session.OutputContent.Substring(session.OutputOffset);
                    TadMessageBuilder builder = new TadMessageBuilder().BdatText(tail).Sycn(SycnState.LoggedIn);
                    if (session.OutputPrompt.Length != 0)
                    {
                        builder.BdatText(session.OutputPrompt);
                    }

                    builder.Rfi();
                    XmsgFrame frame = BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, builder.Build());
                    TrackOutput(session, frame);
                    session.OutputFinalSent = true;
                    session.OutputActive = false;   // burst complete the moment the terminator is sent, so a
                                                    // queued tell/wall inject can start on the very next drain
                    outgoing.Add(frame);
                    return;
                }

                if (session.PairAwaitingSecond)
                {
                    // SECOND chunk of the current pair: gated ONLY by the ~46 ms intra-pair timer, never by an
                    // ACK (22.16: the second chunk of a pair is transmitted before either chunk is acked). The
                    // gap is the prime suspect for why a byte-identical burst rendered only its last chunk.
                    if ((now - session.LastContinuationAt).TotalMilliseconds < ContinuationPairGapMillis)
                    {
                        return;   // wait for the pump to re-drain once the gap has elapsed
                    }

                    SendContinuation(session, transport, outgoing, now);
                    session.PairAwaitingSecond = false;   // pair complete; the barrier below now holds until
                    continue;                             // both chunks are acked (and their DUMMs seen)
                }

                // FIRST chunk of a NEW pair: require the previous pair fully settled - both delivered (acked)
                // AND their 7DUMMs seen - so we reproduce the real inter-pair cadence, not just fire-and-forget.
                if (session.OutstandingOutputCount > 0 || session.DummsConsumed < session.ContinuationsSent)
                {
                    return;
                }

                SendContinuation(session, transport, outgoing, now);

                // If the piece that now remains is under one buffer, this continuation was the LAST one and
                // it goes ALONE (22.16); otherwise we owe a second chunk after the intra-pair gap.
                bool loneLast = session.OutputContent.Length - session.OutputOffset < FullBufferChunk;
                session.PairAwaitingSecond = !loneLast;
                return;   // wait: either the 46 ms gap (second chunk) or, if lone, the final after its ACK
            }
        }

        /// <summary>
        /// Streams a logged-in reply as N COMPLETE BDAT segments (mode
        /// <see cref="TadOutputMode.CompleteSegments"/>): each non-final segment is a plain
        /// <c>BDAT(at most SegmentChunk)</c> with NO count-0xFF sentinel, NO SYCN and NO RFI; the final
        /// segment carries <c>BDAT(tail) + SYCN 000A + prompt BDAT + RFI</c>. A strict window-of-1
        /// (next segment only after the previous is ACKed) keeps 100's element buffer from overrunning.
        /// </summary>
        /// <remarks>
        /// Rationale [Ghidra 2026-07-08, COS-CONN-TO-E02-Analysis.md §5b]: the connect-to receiver
        /// (<c>tad_rx_BDAT_01</c>) renders <c>count</c> bytes per BDAT element with no special handling of
        /// <c>count==0xFF</c> — there is no "255 = more follows" concept in the binary. So a long reply
        /// delivered as consecutive complete elements should render each element, exactly like the login
        /// banner and command replies (which already render). This is the untested construct the decode
        /// points to, distinct from the sentinel stream that renders only its final chunk on real 100.
        /// </remarks>
        /// <param name="session">
        /// The session whose output to drain.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The list that receives the frame(s) to send.
        /// </param>
        private void DrainSegmentedOutput(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing)
        {
            // Window-of-1: wait for the previous segment to be ACKed before sending the next. This is the
            // simplest correct pacing; unlike the sentinel stream there is no pair/gap/DUMM choreography.
            if (session.OutstandingOutputCount > 0)
            {
                return;
            }

            int remaining = session.OutputContent.Length - session.OutputOffset;
            bool isFinal = remaining <= SegmentChunk;

            if (isFinal)
            {
                if (session.OutputFinalSent)
                {
                    session.OutputActive = false;   // burst complete
                    return;
                }

                string tail = session.OutputContent.Substring(session.OutputOffset);
                TadMessageBuilder builder = new TadMessageBuilder().BdatText(tail).Sycn(SycnState.LoggedIn);
                if (session.OutputPrompt.Length != 0)
                {
                    builder.BdatText(session.OutputPrompt);
                }

                builder.Rfi();
                XmsgFrame finalFrame = BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, builder.Build());
                TrackOutput(session, finalFrame);
                session.OutputOffset = session.OutputContent.Length;
                session.OutputFinalSent = true;
                session.OutputActive = false;   // complete the moment the terminator is sent, so a queued
                                                // tell/wall inject can start on the very next drain
                outgoing.Add(finalFrame);
                return;
            }

            // A non-final COMPLETE segment: plain BDAT, no SYCN, no RFI, no 0xFF sentinel.
            string piece = session.OutputContent.Substring(session.OutputOffset, SegmentChunk);
            XmsgFrame segment = BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                new TadMessageBuilder().BdatText(piece).Build());
            TrackOutput(session, segment);
            session.OutputOffset += SegmentChunk;
            outgoing.Add(segment);
            // Return with a segment outstanding; the next segment is released by DrainPending once 100 ACKs.
        }

        /// <summary>
        /// Emits one bare 255-byte continuation chunk (BDAT count 0xFF, no RFI), tracks it for the ACK
        /// window, advances the burst offset, and stamps the send time for the intra-pair gap.
        /// </summary>
        /// <param name="session">
        /// The session whose burst to advance.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="outgoing">
        /// The list that receives the continuation frame.
        /// </param>
        /// <param name="now">
        /// The current clock time, stamped as this continuation's send time.
        /// </param>
        private void SendContinuation(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, DateTime now)
        {
            string piece = session.OutputContent.Substring(session.OutputOffset, FullBufferChunk);
            XmsgFrame chunk = BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                new TadMessageBuilder().BdatText(piece).Build());
            TrackOutput(session, chunk);
            session.MarkContinuationSent();
            session.LastContinuationAt = now;
            session.OutputOffset += FullBufferChunk;
            outgoing.Add(chunk);
        }

        /// <summary>
        /// Records a sent output frame as outstanding (by its Flags 1) so an ACK can release the window.
        /// </summary>
        /// <param name="session">
        /// The owning session.
        /// </param>
        /// <param name="frame">
        /// The sent output frame.
        /// </param>
        private void TrackOutput(TadServerSession session, XmsgFrame frame)
        {
            ushort flags1 = frame.Header.Flags1;
            session.MarkOutputSent(flags1);
            _outputAckIndex[OutputAckKey(session.RemoteNode, flags1)] = session;
        }

        /// <summary>
        /// Composes the <see cref="_outputAckIndex"/> key from a remote node and a Flags 1.
        /// </summary>
        /// <param name="remoteNode">
        /// The remote node.
        /// </param>
        /// <param name="flags1">
        /// The frame's Flags 1.
        /// </param>
        /// <returns>
        /// The composite key.
        /// </returns>
        private static uint OutputAckKey(ushort remoteNode, ushort flags1)
        {
            return ((uint)remoteNode << 16) | flags1;
        }

        /// <summary>
        /// Builds the 0xFD session-state notification (asks the asker to disconnect).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The 0xFD frame.
        /// </returns>
        private XmsgFrame BuildFdNotification(TadServerSession session, IXmsgServerTransport transport)
        {
            byte[] tad = new TadMessageBuilder().Raw((TadOp)0xFD, ReadOnlySpan<byte>.Empty).Build();
            // ORIGINATED: the host raises this, nothing asked for it.
            return transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, SessionNotifyControlService,
                (byte)XmsgFrameFlags.ControlBare,
                (byte)(XmsgSendOptions.WakeOnStatus | XmsgSendOptions.Bounce | XmsgSendOptions.RoutedLetter),
                tad,
                XmsgAnsweredFlags1.None);
        }

        /// <summary>
        /// Builds a host-initiated DCON indication (the LIVE-VERIFIED instant-disconnect trigger).
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// The DCON frame.
        /// </returns>
        private XmsgFrame BuildDconIndication(TadServerSession session, IXmsgServerTransport transport)
        {
            byte[] tad = new TadMessageBuilder().Raw((TadOp)DconOpcode, ReadOnlySpan<byte>.Empty).Build();
            return BuildSession(session, transport, BareTadControlService,
                (byte)XmsgFrameFlags.ControlBare, (byte)XmsgSendOptions.None, tad);
        }

        /// <summary>
        /// Builds a terminal-data frame (class 0x0108) carrying a TAD chain on the session port.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <param name="frameFlags">
        /// The sub-header frame-flags byte.
        /// </param>
        /// <param name="tadChain">
        /// The TAD chain payload.
        /// </param>
        /// <returns>
        /// The terminal-data frame.
        /// </returns>
        private XmsgFrame BuildTerminal(TadServerSession session, IXmsgServerTransport transport, byte frameFlags, byte[] tadChain)
        {
            return BuildSession(session, transport, TerminalDataControlService,
                frameFlags, (byte)XmsgSendOptions.None, tadChain);
        }

        /// <summary>
        /// Builds a session frame with the given class/flags/role from the session port.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport.
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
        /// <param name="payload">
        /// The trailer payload.
        /// </param>
        /// <returns>
        /// The assembled frame.
        /// </returns>
        private XmsgFrame BuildSession(TadServerSession session, IXmsgServerTransport transport, uint controlService, byte frameFlags, byte role, byte[] payload)
        {
            // ORIGINATED. Terminal output is the case the echo rule does NOT describe: one
            // keystroke produces a multi-frame segmented burst released across several ACKs, so
            // there is no one request for one frame to answer. The captured TAD frames agree -
            // the conn-to-d102 DUMM is Flags 1 0x0131 and its MOTD 0x0135, four apart, which is
            // the responder counting its own frames rather than echoing one value.
            return transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                session.SessionWirePort, controlService, frameFlags, role, payload,
                XmsgAnsweredFlags1.None);
        }

        /// <summary>
        /// Closes a session and removes it from the lookups.
        /// </summary>
        /// <param name="session">
        /// The session to close.
        /// </param>
        private void CloseSession(TadServerSession session)
        {
            _sessions.Remove(SessionKey(session.ClientSystem, session.ClientPort));
            _sessionByPort.Remove(session.SessionWirePort);
            _sessionList.Remove(session);
            SessionClosed?.Invoke(session.TadNumber, session.ClientSystem);
        }

        /// <summary>
        /// Allocates the lowest free TAD (tty) number in <c>1..MaxSessions</c>, so a freed tty is reused.
        /// </summary>
        /// <returns>
        /// The lowest tty number not currently held by a live session.
        /// </returns>
        private int AllocateTadNumber()
        {
            for (int n = 1; n <= MaxSessions; n++)
            {
                bool used = false;
                for (int i = 0; i < _sessionList.Count; i++)
                {
                    if (_sessionList[i].TadNumber == n)
                    {
                        used = true;
                        break;
                    }
                }

                if (!used)
                {
                    return n;
                }
            }

            return MaxSessions + 1;   // unreachable: OnConnect refuses at capacity before allocating
        }

        /// <summary>
        /// Finds the session for an incoming frame by its stable client source endpoint.
        /// </summary>
        /// <param name="incoming">
        /// The received frame.
        /// </param>
        /// <returns>
        /// The session, or null when none matches.
        /// </returns>
        private TadServerSession? FindSession(XmsgFrame incoming)
        {
            uint key = SessionKey(incoming.SubHeader!.SourceSystem, incoming.SubHeader.SourcePort);
            return _sessions.TryGetValue(key, out TadServerSession? session) ? session : null;
        }

        /// <summary>
        /// Builds the "stat" report of session/terminal metadata.
        /// </summary>
        /// <param name="session">
        /// The session.
        /// </param>
        /// <param name="transport">
        /// The node transport (for this node's number).
        /// </param>
        /// <returns>
        /// The report text.
        /// </returns>
        private string BuildStatReport(TadServerSession session, IXmsgServerTransport transport)
        {
            // COMPACT report kept under ONE 255-byte terminal buffer, so it rides a single frame that 100
            // displays reliably (like Time/help/MOTD). WHY single-frame: a multi-chunk (255-sentinel) reply
            // is delivered and ACKed correctly by 100 (verified on the wire) but 100 only DISPLAYS the final
            // chunk - it drops the first continuation from the screen despite acking it, so the top of a long
            // report (the tty line) never appears. Until that 100-side display behaviour is understood, all
            // command output stays under one buffer. The 255-sentinel + flow-control windowing (EmitMenuReply
            // / DrainSessionOutput) is retained for when a working long-output path is confirmed. Labels use
            // parentheses/plain text, never square brackets (0x5B/0x5D render as Norwegian AE/AA on the ND).
            StringBuilder sb = new StringBuilder(256);
            sb.Append("\r\n--- SESSION STATUS ---\r\n");
            sb.Append("  TAD number  : tty").Append(session.TadNumber).Append("\r\n");
            sb.Append("  From node   : ").Append(session.ClientSystem)
              .Append(" -> ").Append(transport.NodeNumber)
              .Append(" (D").Append(transport.NodeNumber).Append(")\r\n");
            sb.Append("  Service     : ")
              .Append(session.ConnectService.Length != 0 ? session.ConnectService : "(none)")
              .Append("  ->  ")
              .Append(session.ConnectTargetName.Length != 0 ? session.ConnectTargetName : "(none)").Append("\r\n");
            sb.Append("  Client port : 0x").Append(session.ClientPort.ToString("X4"))
              .Append(" (log ").Append(session.ClientPort >> 7)
              .Append(", inc ").Append(session.ClientPort & 0x7F).Append(")\r\n");

            if (session.NegotiationSeen)
            {
                sb.Append("  Terminal    : TTYP 0x").Append(session.TerminalType.ToString("X4"))
                  .Append(" TMOD 0x").Append(session.TerminalMode.ToString("X2"))
                  .Append(" ESC ").Append(session.EscapeChar)
                  .Append(" OPSV ").Append(FormatHexBytes(session.OsVersion)).Append("\r\n");
            }
            else
            {
                sb.Append("  Terminal    : (not yet negotiated)\r\n");
            }

            sb.Append("\r\n# ");
            return sb.ToString();
        }

        /// <summary>
        /// Builds a three-frame diagnostic reply (~590 bytes) that the 255-sentinel chunker splits into two
        /// full continuations plus a short final frame. Each frame starts with a distinct
        /// <c>===== ECHO FRAME n OF 3 =====</c> marker and is filled with the digit n, so a live run makes
        /// it obvious which frame(s) 100 renders and which the flow-control handshake is dropping.
        /// </summary>
        /// <returns>
        /// The diagnostic reply text (ending with the prompt).
        /// </returns>
        private static string BuildEchoDiagnostic()
        {
            StringBuilder sb = new StringBuilder(640);
            AppendEchoFrame(sb, 1, '1');   // exactly 255 bytes -> continuation chunk 1
            AppendEchoFrame(sb, 2, '2');   // exactly 255 bytes -> continuation chunk 2
            // Frame 3 is the short final chunk (< 255) - the RFI terminator rides here.
            sb.Append("\r\n===== ECHO FRAME 3 OF 3 =====\r\n");
            sb.Append('3', 32);
            sb.Append("\r\n===== END OF 3-FRAME ECHO =====\r\n# ");
            return sb.ToString();
        }

        /// <summary>
        /// Appends exactly 255 bytes: a <c>===== ECHO FRAME n OF 3 =====</c> marker line then the digit
        /// <paramref name="fill"/> repeated to fill the buffer, so the 255-sentinel split lands the next
        /// marker at a chunk boundary.
        /// </summary>
        /// <param name="sb">
        /// The builder to append to.
        /// </param>
        /// <param name="frameNo">
        /// The frame number shown in the marker.
        /// </param>
        /// <param name="fill">
        /// The digit used to fill the frame body.
        /// </param>
        private static void AppendEchoFrame(StringBuilder sb, int frameNo, char fill)
        {
            int start = sb.Length;
            sb.Append("\r\n===== ECHO FRAME ").Append(frameNo).Append(" OF 3 =====\r\n");
            while (sb.Length - start < FullBufferChunk)
            {
                sb.Append(fill);
            }

            sb.Length = start + FullBufferChunk;   // trim to exactly 255 for a clean continuation chunk
        }

        /// <summary>
        /// Parses the connect letter's trailer for the service and target-name strings.
        /// </summary>
        /// <param name="request">
        /// The connect letter.
        /// </param>
        /// <param name="session">
        /// The session to populate.
        /// </param>
        private static void ExtractConnectStrings(XmsgFrame request, TadServerSession session)
        {
            // Scan the FULL serialized frame (byte-identical whether the letter is in TrailingBytes or
            // Body): the first '*'-run is the service (*TADADM), and the run right after it is the target
            // name (D102). Capturing the target only AFTER the service avoids spurious header runs.
            byte[] bytes = request.ToArray();
            StringBuilder run = new StringBuilder();
            for (int i = 0; i <= bytes.Length; i++)
            {
                byte b = i < bytes.Length ? bytes[i] : (byte)0x00;
                if (b >= 0x20 && b <= 0x7E)
                {
                    run.Append((char)b);
                    continue;
                }

                if (run.Length >= 2)
                {
                    string s = run.ToString();
                    if (s[0] == '*' && session.ConnectService.Length == 0)
                    {
                        session.ConnectService = s;
                    }
                    else if (session.ConnectService.Length != 0 && session.ConnectTargetName.Length == 0)
                    {
                        session.ConnectTargetName = s;
                    }
                }

                run.Clear();
            }
        }

        /// <summary>
        /// Captures 100's terminal parameters from the TMOD/TTYP/DESC/OPSV chain.
        /// </summary>
        /// <param name="request">
        /// The terminal-setup frame.
        /// </param>
        /// <param name="session">
        /// The session to populate.
        /// </param>
        private static void CaptureNegotiation(XmsgFrame request, TadServerSession session)
        {
            if (request.Tad == null)
            {
                return;
            }

            IReadOnlyList<TadMessage> messages = request.Tad.Messages;
            for (int i = 0; i < messages.Count; i++)
            {
                TadMessage m = messages[i];
                byte[] d = m.Data;
                switch (m.Opcode)
                {
                    case TmodOpcode:
                        if (d.Length >= 1)
                        {
                            session.TerminalMode = d[0];
                        }

                        break;
                    case TtypOpcode:
                        if (d.Length >= 2)
                        {
                            session.TerminalType = (ushort)((d[0] << 8) | d[1]);
                        }

                        break;
                    case DescOpcode:
                        if (d.Length >= 1)
                        {
                            session.EscapeChar = d[0];
                        }

                        break;
                    case OpsvOpcode:
                        session.OsVersion = (byte[])d.Clone();
                        break;
                }
            }

            session.NegotiationSeen = true;
        }

        /// <summary>
        /// Returns true when the frame's TAD chain contains a message with the given opcode.
        /// </summary>
        /// <param name="frame">
        /// The frame.
        /// </param>
        /// <param name="opcode">
        /// The opcode to find.
        /// </param>
        /// <returns>
        /// True when present.
        /// </returns>
        private static bool HasOpcode(XmsgFrame frame, byte opcode)
        {
            if (frame.Tad == null)
            {
                return false;
            }

            IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
            for (int i = 0; i < messages.Count; i++)
            {
                if (messages[i].Opcode == opcode)
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Reads the concatenated 7-bit ASCII text of every BDAT message in a frame's TAD chain.
        /// </summary>
        /// <param name="frame">
        /// The frame.
        /// </param>
        /// <returns>
        /// The text.
        /// </returns>
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
                        sb.Append((char)(data[j] & 0x7F));
                    }
                }
            }

            return sb.ToString();
        }

        /// <summary>
        /// Formats a byte array as space-separated two-digit hex.
        /// </summary>
        /// <param name="bytes">
        /// The bytes.
        /// </param>
        /// <returns>
        /// The hex string, or "(none)" when empty.
        /// </returns>
        private static string FormatHexBytes(byte[] bytes)
        {
            if (bytes == null || bytes.Length == 0)
            {
                return "(none)";
            }

            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < bytes.Length; i++)
            {
                if (i != 0)
                {
                    sb.Append(' ');
                }

                sb.Append(bytes[i].ToString("X2"));
            }

            return sb.ToString();
        }

        /// <summary>
        /// Composes the session-lookup key from a client endpoint.
        /// </summary>
        /// <param name="clientSystem">
        /// The client system.
        /// </param>
        /// <param name="clientPort">
        /// The client port.
        /// </param>
        /// <returns>
        /// The key.
        /// </returns>
        private static uint SessionKey(ushort clientSystem, ushort clientPort)
        {
            return ((uint)clientSystem << 16) | clientPort;
        }

        /// <summary>
        /// One entry in the command registry: a command's name (with argument shape) and a one-line
        /// description, listed by the <c>help</c> command.
        /// </summary>
        private readonly struct CommandDoc
        {
            /// <summary>
            /// The command name / usage shown in help.
            /// </summary>
            public readonly string Name;

            /// <summary>
            /// The one-line description.
            /// </summary>
            public readonly string Description;

            /// <summary>
            /// Initialises a command-registry entry.
            /// </summary>
            /// <param name="name">
            /// The command name / usage.
            /// </param>
            /// <param name="description">
            /// The one-line description.
            /// </param>
            public CommandDoc(string name, string description)
            {
                Name = name;
                Description = description;
            }
        }
    }
}
