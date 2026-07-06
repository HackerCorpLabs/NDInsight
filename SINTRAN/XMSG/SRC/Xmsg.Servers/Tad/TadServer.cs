using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg.Node;
using NDInsight.Sintran.Xmsg.Node.Services;
using NDInsight.Sintran.Xmsg.Node.Tad;
using NDInsight.Sintran.Xmsg.SubProtocol;

namespace NDInsight.Sintran.Xmsg.Servers.Tad
{
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

        // The *TADADM registered name and its well-known logical port (COSMOS list-servers shows Port 2).
        private const string ServerName = "*TADADM";
        private const int ServerLogicalPort = 2;

        // The wire reply-from port for *TADADM: logical port 2 with the fixed low-7 0x56 -> 0x0156 (342).
        private const ushort TadAdminWirePort = 0x0156;

        // The maximum concurrent TAD sessions (the "Free SPs" capacity).
        private const int MaxSessions = 10;

        // The maximum wrong-credential attempts before the session is torn down.
        private const int MaxLoginFaults = 3;

        // TAD opcode bytes we test for on the RX side.
        private const byte BdatOpcode = 0x01;
        private const byte DconOpcode = 0x09;
        private const byte TmodOpcode = 0x0C;
        private const byte TtypOpcode = 0x0D;
        private const byte DescOpcode = 0x0F;
        private const byte OpsvOpcode = 0x1F;

        // XMCSM control/service words (their high half is the frame class). VERIFIED from captures.
        private const uint XsletLetterControlService = (uint)XmcsmService.XsletLetter;      // connect / accept (0x04000041)
        private const uint SessionSetupControlService = (uint)XmcsmService.SessionSetup;    // 0x04000000
        private const uint TerminalDataControlService = (uint)XmcsmService.TerminalData;    // 0x01080000
        private const uint BareTadControlService = (uint)XmcsmService.BareTadControl;       // 0x00080000
        private const uint SessionNotifyControlService = (uint)XmcsmService.SessionNotify;  // 0x00060000

        // Chunk terminal replies well under the single-BDAT 255-byte limit, on an even boundary.
        private const int MenuReplyChunk = 240;

        // The MOTD frame's TAD payload, VERIFIED from conn-to-d102 frame 62: BMMX / ECKM / a BDAT banner
        // (date, "SINTRAN III - VSX/500", "--- RETROCORE EMULATED ID:102 ---") / SYCN / a BDAT "ENTER "
        // prompt / RFI. Copied verbatim.
        private static readonly byte[] MotdPayload = Convert.FromHexString(
            "0004030100000003010101600D0A2032322E32372E32322020202020203820415052494C202020313939380D0A"
            + "2053494E5452414E20494949202D205653582F353030204C0D0A2D2D2D20524554524F434F524520454D554C4154"
            + "4544204C2049443A313032202D2D2D0D0A1302000201080D0A454E544552200200");

        // The connect-accept parameter trailer, VERIFIED from the 102 capture: two parameter blocks
        // 01 02 0000 (param 1 = 0) and 02 02 000A (param 2 = 0x000A).
        private static readonly byte[] AcceptTrailer = { 0x01, 0x02, 0x00, 0x00, 0x02, 0x02, 0x00, 0x0A };

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
        /// Occurs when a new TAD session is opened.
        /// </summary>
        public event SessionLifecycle? SessionOpened;

        /// <summary>
        /// Occurs when a TAD session is closed.
        /// </summary>
        public event SessionLifecycle? SessionClosed;

        /// <summary>
        /// Initialises the TAD server.
        /// </summary>
        /// <param name="clock">
        /// Supplies the current time for the MOTD/date/time commands (injected for deterministic tests).
        /// </param>
        /// <param name="users">
        /// The login accounts. When null, the default <c>SYSTEM</c>/<c>SYSTEM</c> directory is used.
        /// </param>
        public TadServer(Func<DateTime> clock, TadUserDirectory? users = null)
        {
            _clock = clock ?? throw new ArgumentNullException(nameof(clock));
            _users = users ?? new TadUserDirectory();
            _menu = new TadTerminalMenu();
            _sessions = new Dictionary<uint, TadServerSession>();
            _sessionByPort = new Dictionary<ushort, TadServerSession>();
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
                && incoming.SubHeader.ControlService == XsletLetterControlService)
            {
                return OnConnect(incoming, transport);
            }

            // Session traffic: find the session by the stable client source endpoint.
            TadServerSession? session = FindSession(incoming);
            if (session == null)
            {
                return Array.Empty<XmsgFrame>();
            }

            if (incoming.SubHeader.ControlService == SessionSetupControlService)
            {
                return OnSessionSetup(session, incoming, transport);
            }

            if (HasOpcode(incoming, TmodOpcode) && !session.MotdSent)
            {
                return OnTerminalSetup(session, incoming, transport);
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
        /// Drains queued asynchronous output. Phase 1 emits command replies directly from
        /// <see cref="Handle"/>, so there is nothing async yet; the tty inject / wall path (Phase 3)
        /// will drain the per-session output queues here.
        /// </summary>
        /// <param name="transport">
        /// The node transport.
        /// </param>
        /// <returns>
        /// An empty list.
        /// </returns>
        public IReadOnlyList<XmsgFrame> DrainPending(IXmsgServerTransport transport)
        {
            return Array.Empty<XmsgFrame>();
        }

        /// <summary>
        /// Opens a session for a connect letter and returns the connect-accept.
        /// </summary>
        /// <param name="request">The connect letter.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The accept frame (or empty when at capacity).</returns>
        private IReadOnlyList<XmsgFrame> OnConnect(XmsgFrame request, IXmsgServerTransport transport)
        {
            if (_sessions.Count >= MaxSessions)
            {
                // No free SPs. A refusal letter is a later refinement; for now do not open the session.
                return Array.Empty<XmsgFrame>();
            }

            ushort clientSystem = request.SubHeader!.SourceSystem;
            ushort clientPort = request.SubHeader.SourcePort;
            ushort sessionPort = transport.AllocateSessionPort();
            int tadNumber = transport.AllocateSessionNumber();

            TadServerSession session = new TadServerSession(
                request.Header.SourceNode, clientSystem, clientPort, sessionPort, tadNumber);
            ExtractConnectStrings(request, session);

            _sessions[SessionKey(clientSystem, clientPort)] = session;
            _sessionByPort[sessionPort] = session;
            SessionOpened?.Invoke(tadNumber, clientSystem);

            // The connect-accept: XMCSM 0x04000041, frame-flags Setup, role WakeOnStatus, from the TADADM
            // wire port, with the verified parameter trailer.
            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            outgoing.Add(transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, XsletLetterControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.WakeOnStatus, AcceptTrailer));
            return outgoing;
        }

        /// <summary>
        /// Answers the session-setup with the port-assignment (TAD 0x07 carrying our session endpoint).
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="request">The session-setup frame.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The port-assignment frame.</returns>
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
                0x0B, 0x02, 0x03, 0x00,
                0x15, 0x02, 0x01, 0x08,
                0xFF, 0x00,
            };

            List<XmsgFrame> outgoing = new List<XmsgFrame>();
            outgoing.Add(transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, SessionSetupControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.WakeOnStatus, trailer));
            return outgoing;
        }

        /// <summary>
        /// Answers 100's terminal-setup (TMOD chain) with the login-screen burst: control 0x20, RESE,
        /// RESE, then the MOTD.
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="request">The terminal-setup frame.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The burst frames.</returns>
        private IReadOnlyList<XmsgFrame> OnTerminalSetup(TadServerSession session, XmsgFrame request, IXmsgServerTransport transport)
        {
            CaptureNegotiation(request, session);

            List<XmsgFrame> outgoing = new List<XmsgFrame>();

            // control 0x20 (XMCSM 0x00080000): TAD opcode 0x20, empty.
            outgoing.Add(BuildSession(session, transport, BareTadControlService,
                (byte)XmsgFrameFlags.Setup, (byte)XmsgSendOptions.None,
                new TadMessageBuilder().Raw(0x20, ReadOnlySpan<byte>.Empty).Build()));

            // RESE, RESE (XMCSM 0x01080000).
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                new TadMessageBuilder().Rese().Build()));
            outgoing.Add(BuildSession(session, transport, TerminalDataControlService,
                (byte)XmsgFrameFlags.DataB, (byte)XmsgSendOptions.None,
                new TadMessageBuilder().Rese().Build()));

            // MOTD (XMCSM 0x01080000): the banner + ENTER prompt chain.
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, MotdPayload));

            session.MotdSent = true;
            return outgoing;
        }

        /// <summary>
        /// Handles a typed line (BDAT) during login or the command loop.
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="frame">The input frame.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The reply frames.</returns>
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
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="line">The typed username.</param>
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
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="line">The typed password.</param>
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
                session.LoginFaults++;
                outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                    .BdatText("\r\nBYE HACKER!\r\n").Sycn(SycnState.LoggedOut).Build()));
                outgoing.Add(BuildFdNotification(session, transport));
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
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="user">The account that logged in.</param>
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
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="line">The typed command.</param>
        private void HandleCommand(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string line)
        {
            if (string.Equals(line, "stat", StringComparison.OrdinalIgnoreCase))
            {
                EmitMenuReply(session, transport, outgoing, BuildStatReport(session, transport));
                return;
            }

            TadMenuResult result = _menu.Handle(line, _clock());
            switch (result.Mode)
            {
                case TadDisconnectMode.Ladder:
                    AppendTeardownLadder(session, transport, outgoing, result.Output);
                    break;

                case TadDisconnectMode.LadderThenDcon:
                    AppendTeardownLadder(session, transport, outgoing, result.Output);
                    outgoing.Add(BuildDconIndication(session, transport));
                    break;

                default:
                    EmitMenuReply(session, transport, outgoing, result.Output);
                    break;
            }
        }

        /// <summary>
        /// Appends the five-frame teardown ladder (farewell + CESC 00; BMMX/ECKM/CESC 00;
        /// "--EXIT--" + SYCN 000B; CESC 01; then the 0xFD notification).
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="farewell">The farewell text for the first ladder frame.</param>
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
        /// Emits a logged-in reply, splitting output longer than one BDAT across frames; only the final
        /// frame re-asserts the logged-in state and grants input (SYCN 000A + RFI).
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="outgoing">The reply list.</param>
        /// <param name="text">The full reply text.</param>
        private void EmitMenuReply(TadServerSession session, IXmsgServerTransport transport, List<XmsgFrame> outgoing, string text)
        {
            string body = text ?? string.Empty;
            int offset = 0;
            while (body.Length - offset > MenuReplyChunk)
            {
                string piece = body.Substring(offset, MenuReplyChunk);
                outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA,
                    new TadMessageBuilder().BdatText(piece).Build()));
                offset += MenuReplyChunk;
            }

            string tail = body.Substring(offset);
            outgoing.Add(BuildTerminal(session, transport, (byte)XmsgFrameFlags.DataA, new TadMessageBuilder()
                .BdatText(tail).Sycn(SycnState.LoggedIn).Rfi().Build()));
        }

        /// <summary>
        /// Builds the 0xFD session-state notification (asks the asker to disconnect).
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The 0xFD frame.</returns>
        private XmsgFrame BuildFdNotification(TadServerSession session, IXmsgServerTransport transport)
        {
            byte[] tad = new TadMessageBuilder().Raw(0xFD, ReadOnlySpan<byte>.Empty).Build();
            return transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                TadAdminWirePort, SessionNotifyControlService,
                (byte)XmsgFrameFlags.ControlBare,
                (byte)(XmsgSendOptions.WakeOnStatus | XmsgSendOptions.Bounce | XmsgSendOptions.RoutedLetter),
                tad);
        }

        /// <summary>
        /// Builds a host-initiated DCON indication (the LIVE-VERIFIED instant-disconnect trigger).
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <returns>The DCON frame.</returns>
        private XmsgFrame BuildDconIndication(TadServerSession session, IXmsgServerTransport transport)
        {
            byte[] tad = new TadMessageBuilder().Raw(DconOpcode, ReadOnlySpan<byte>.Empty).Build();
            return BuildSession(session, transport, BareTadControlService,
                (byte)XmsgFrameFlags.ControlBare, (byte)XmsgSendOptions.None, tad);
        }

        /// <summary>
        /// Builds a terminal-data frame (class 0x0108) carrying a TAD chain on the session port.
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="frameFlags">The sub-header frame-flags byte.</param>
        /// <param name="tadChain">The TAD chain payload.</param>
        /// <returns>The terminal-data frame.</returns>
        private XmsgFrame BuildTerminal(TadServerSession session, IXmsgServerTransport transport, byte frameFlags, byte[] tadChain)
        {
            return BuildSession(session, transport, TerminalDataControlService,
                frameFlags, (byte)XmsgSendOptions.None, tadChain);
        }

        /// <summary>
        /// Builds a session frame with the given class/flags/role from the session port.
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport.</param>
        /// <param name="controlService">The XMCSM control/service word.</param>
        /// <param name="frameFlags">The sub-header frame-flags byte.</param>
        /// <param name="role">The sub-header role byte.</param>
        /// <param name="payload">The trailer payload.</param>
        /// <returns>The assembled frame.</returns>
        private XmsgFrame BuildSession(TadServerSession session, IXmsgServerTransport transport, uint controlService, byte frameFlags, byte role, byte[] payload)
        {
            return transport.BuildDatagram(
                session.RemoteNode, session.ClientSystem, session.ClientPort,
                session.SessionWirePort, controlService, frameFlags, role, payload);
        }

        /// <summary>
        /// Closes a session and removes it from the lookups.
        /// </summary>
        /// <param name="session">The session to close.</param>
        private void CloseSession(TadServerSession session)
        {
            _sessions.Remove(SessionKey(session.ClientSystem, session.ClientPort));
            _sessionByPort.Remove(session.SessionWirePort);
            SessionClosed?.Invoke(session.TadNumber, session.ClientSystem);
        }

        /// <summary>
        /// Finds the session for an incoming frame by its stable client source endpoint.
        /// </summary>
        /// <param name="incoming">The received frame.</param>
        /// <returns>The session, or null when none matches.</returns>
        private TadServerSession? FindSession(XmsgFrame incoming)
        {
            uint key = SessionKey(incoming.SubHeader!.SourceSystem, incoming.SubHeader.SourcePort);
            return _sessions.TryGetValue(key, out TadServerSession? session) ? session : null;
        }

        /// <summary>
        /// Builds the "stat" report of session/terminal metadata.
        /// </summary>
        /// <param name="session">The session.</param>
        /// <param name="transport">The node transport (for this node's number).</param>
        /// <returns>The report text.</returns>
        private string BuildStatReport(TadServerSession session, IXmsgServerTransport transport)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append("\r\n--- TAD SESSION STATUS ---\r\n\r\n");
            sb.Append("Connect letter (XMCSM 04000041):\r\n");
            sb.Append("  From node    : ").Append(session.ClientSystem)
              .Append("  ->  this node ").Append(transport.NodeNumber)
              .Append(" (D").Append(transport.NodeNumber).Append(")\r\n");
            sb.Append("  TAD number   : tty").Append(session.TadNumber).Append("\r\n");
            sb.Append("  Service      : ")
              .Append(session.ConnectService.Length != 0 ? session.ConnectService : "(none)").Append("\r\n");
            sb.Append("  Target name  : ")
              .Append(session.ConnectTargetName.Length != 0 ? session.ConnectTargetName : "(none)").Append("\r\n");
            sb.Append("  Client port  : 0x").Append(session.ClientPort.ToString("X4"))
              .Append("  (logical ").Append(session.ClientPort >> 7)
              .Append(", incarnation ").Append(session.ClientPort & 0x7F).Append(")\r\n\r\n");

            if (session.NegotiationSeen)
            {
                sb.Append("Terminal negotiation (sent by your connect-to):\r\n");
                sb.Append("  Terminal type: ").Append(session.TerminalType)
                  .Append("  (octal ").Append(Convert.ToString(session.TerminalType, 8))
                  .Append(", hex 0x").Append(session.TerminalType.ToString("X4")).Append(")   [TTYP]\r\n");
                sb.Append("  Terminal mode: ").Append(session.TerminalMode)
                  .Append("  (0x").Append(session.TerminalMode.ToString("X2")).Append(")   [TMOD]\r\n");
                sb.Append("  Escape char  : ").Append(session.EscapeChar)
                  .Append("  (octal ").Append(Convert.ToString(session.EscapeChar, 8))
                  .Append(session.EscapeChar == 0x1B ? ", ESC" : string.Empty).Append(")   [DESC]\r\n");
                sb.Append("  Host OS ver  : ").Append(FormatHexBytes(session.OsVersion)).Append("   [OPSV]\r\n");
            }
            else
            {
                sb.Append("Terminal negotiation: not yet received.\r\n");
            }

            return sb.ToString();
        }

        /// <summary>
        /// Parses the connect letter's trailer for the service and target-name strings.
        /// </summary>
        /// <param name="request">The connect letter.</param>
        /// <param name="session">The session to populate.</param>
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
        /// <param name="request">The terminal-setup frame.</param>
        /// <param name="session">The session to populate.</param>
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
        /// <param name="frame">The frame.</param>
        /// <param name="opcode">The opcode to find.</param>
        /// <returns>True when present.</returns>
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
        /// <param name="frame">The frame.</param>
        /// <returns>The text.</returns>
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
        /// <param name="bytes">The bytes.</param>
        /// <returns>The hex string, or "(none)" when empty.</returns>
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
        /// <param name="clientSystem">The client system.</param>
        /// <param name="clientPort">The client port.</param>
        /// <returns>The key.</returns>
        private static uint SessionKey(ushort clientSystem, ushort clientPort)
        {
            return ((uint)clientSystem << 16) | clientPort;
        }
    }
}
