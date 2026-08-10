using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg.Codec;
using NDInsight.Sintran.Xmsg.Node.Seam;
using NDInsight.Sintran.Xmsg.Node.Services;
using NDInsight.Sintran.Xmsg.Node.Tad;
using NDInsight.Sintran.Xmsg.Packet;
using NDInsight.Sintran.Xmsg.Servers.Tad;
using NDInsight.Sintran.Xmsg.SubProtocol;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Node.Tests
{
    /// <summary>
    /// Two simulated machines talking with no real hardware and no LAPB: a <see cref="TadConnectClient"/>
    /// (node 100, the asker) drives a connect-to against a <see cref="TadTerminalResponder"/> (node 102,
    /// the server) over an in-memory XMSG pipe. Both sides share the seed model, so the conversation is
    /// coherent. This locks in the word-alignment fix end to end: an odd-length command reply (the menu)
    /// is delivered instead of hanging.
    /// </summary>
    public sealed class TwoNodeTerminalTests
    {
        private static readonly Func<DateTime> FixedClock = () => new DateTime(2026, 7, 2);

        /// <summary>
        /// The client connects and types "help"; the server returns the menu, and it is delivered back
        /// to the client (odd-length reply, previously a hang) with the RFI credit intact.
        /// </summary>
        [Fact]
        public void ClientConnectsAndTypesHelp_ServerReturnsMenu()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec);

            // Drive: connect-to D102, log in as SYSTEM/SYSTEM, then type "help".
            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            LogIn(client, clientCodec);
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("help")));

            string screen = terminal.Text;
            Assert.Contains("PASSWORD", screen);   // the login prompted for a password
            Assert.Contains("Time", screen);
            Assert.Contains("Echo", screen);
            Assert.Contains("Disconnect", screen);
        }

        /// <summary>
        /// A valid SYSTEM/SYSTEM login is accepted ("OK"); wrong credentials are rejected and re-prompt;
        /// after three failed attempts the session is torn down with "BYE HACKER!" and the 0xFD.
        /// </summary>
        [Fact]
        public void Login_ValidAccepts_WrongRejects_ThreeFaultsDisconnect()
        {
            // Valid login.
            TerminalCapture okScreen = new TerminalCapture();
            TadConnectClient okClient = BuildPairedNodes(okScreen, out XmsgCodec okCodec);
            okCodec.SendPacket(new XmsgPacket(okClient.BuildConnect("D102")));
            LogIn(okClient, okCodec);
            Assert.Contains("OK", okScreen.Text);

            // Three wrong password attempts -> "BYE HACKER!" + 0xFD teardown.
            TerminalCapture badScreen = new TerminalCapture();
            bool sawFd = false;
            TadConnectClient badClient = BuildPairedNodes(badScreen, out XmsgCodec badCodec, frame =>
            {
                if (frame.SubHeader != null && frame.ControlService == 0x00060000u)
                {
                    sawFd = true;
                }
            });
            badCodec.SendPacket(new XmsgPacket(badClient.BuildConnect("D102")));
            for (int attempt = 0; attempt < 3; attempt++)
            {
                badCodec.SendPacket(new XmsgPacket(badClient.BuildInput("SYSTEM")));   // username
                badCodec.SendPacket(new XmsgPacket(badClient.BuildInput("nope")));     // wrong password
            }

            Assert.Contains("Invalid user/password", badScreen.Text);
            Assert.Contains("BYE HACKER!", badScreen.Text);
            Assert.True(sawFd, "the third fault should tear the session down with 0xFD");
        }

        /// <summary>
        /// A 7CERS control frame arriving mid-login (as the real client sends answering each CESC) is
        /// NOT treated as a password line — the login still completes (spec 22.15 frames 3/6a).
        /// </summary>
        [Fact]
        public void CersMidLogin_IsIgnored_LoginStillCompletes()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildCers()));            // answers our CESC — not input
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password

            // If the CERS had been mis-read as the password, login would have failed; it did not.
            Assert.Contains("OK", terminal.Text);
        }

        /// <summary>
        /// Sends the SYSTEM username then the SYSTEM password to reach the logged-in command loop.
        /// </summary>
        /// <param name="client">
        /// The connect-to client.
        /// </param>
        /// <param name="codec">
        /// The client codec.
        /// </param>
        private static void LogIn(TadConnectClient client, XmsgCodec codec)
        {
            codec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            codec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password -> logged in
        }

        /// <summary>
        /// The Echo command (an odd-length reply, "IPSUM LORUM") is delivered — the exact case that hung
        /// before the alignment fix.
        /// </summary>
        [Fact]
        public void ClientTypesEcho_OddLengthReply_IsDelivered()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            LogIn(client, clientCodec);
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("3")));

            Assert.Contains("IPSUM LORUM", terminal.Text);
        }

        /// <summary>
        /// A command whose reply exceeds one 255-byte BDAT is split across several terminal-data frames
        /// and delivered in full, rather than throwing. Regression for the live runner crash
        /// "A single TAD message carries at most 255 data bytes" that killed the session on a long reply.
        /// </summary>
        [Fact]
        public void LongReply_IsChunkedAcrossFrames_NotThrown()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            LogIn(client, clientCodec);

            // 250 chars of input (under the client's own single-BDAT limit) echo back as
            // "unknown command: " + 250 = 267 bytes > 255, forcing the responder to split the reply.
            string longLine = new string('X', 250);
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput(longLine)));

            // No throw, and the entire echoed line arrives reassembled at the terminal.
            Assert.Contains("unknown command: " + longLine, terminal.Text);
        }

        /// <summary>
        /// The "stat" command reports the session metadata - including the terminal parameters captured
        /// from the client's TMOD/TTYP/DESC negotiation (mode 0x08, type 0x0000, escape 0x1B).
        /// </summary>
        [Fact]
        public void ClientTypesStat_ServerShowsNegotiatedTerminalMetadata()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            // Send the terminal-setup chain so the server captures TMOD/TTYP/DESC/OPSV (the live client
            // always sends this; the login-only path in other tests skips it).
            clientCodec.SendPacket(new XmsgPacket(client.BuildTerminalSetup()));
            LogIn(client, clientCodec);
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("stat")));

            string screen = terminal.Text;
            Assert.Contains("TAD SESSION STATUS", screen);
            // Captured from the client's negotiation chain (TadConnectClient sends TMOD 0x08 / TTYP
            // 0x0000 / DESC 0x1B), and shown in decimal.
            Assert.Contains("Terminal type: 0", screen);
            Assert.Contains("Terminal mode: 8", screen);
            Assert.Contains("Escape char  : 27", screen);
        }

        /// <summary>
        /// Typing "4" (Disconnect) makes the server emit the FULL host teardown ladder AND a
        /// host-initiated DCON: BDAT(farewell)+CESC 00, BMMX/ECKM/CESC 00, BDAT("--EXIT--")+SYCN 000B,
        /// CESC 01, the 0xFD (class 0x00060000), then TAD 0x09 (DCON). The DCON is the LIVE-VERIFIED
        /// (2026-07-05 vs real 100) instant-disconnect trigger - 100 prints "-- DISCONNECTED BY TAD --"
        /// immediately instead of waiting on its 1-minute idle timer. This asserts the SYCN 000B logout,
        /// the 0xFD, and the host DCON are all present.
        /// </summary>
        [Fact]
        public void ClientTypesDisconnect_ServerSendsFullTeardownLadder()
        {
            bool sawFd = false;
            bool sawLogoutSycn = false;
            bool sawHostDcon = false;
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildPairedNodes(terminal, out XmsgCodec clientCodec, frame =>
            {
                if (frame.SubHeader == null)
                {
                    return;
                }

                // Frame 5: the 0xFD session-state notification rides the 0x00060000 class.
                if (frame.ControlService == 0x00060000u)
                {
                    sawFd = true;
                }

                // The host-initiated DCON (TAD opcode 0x09) - the instant-disconnect trigger.
                if (frame.Tad != null)
                {
                    IReadOnlyList<TadMessage> dconMsgs = frame.Tad.Messages;
                    for (int i = 0; i < dconMsgs.Count; i++)
                    {
                        if (dconMsgs[i].Opcode == 0x09)
                        {
                            sawHostDcon = true;
                        }
                    }
                }

                // Frame 3: a terminal-data frame (0x01080000) carrying SYCN 000B (LoggedOut) - the logout
                // signal that actually makes 100 send DCON. The decoded chain is on frame.Tad.
                if (frame.ControlService == 0x01080000u && frame.Tad != null)
                {
                    IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
                    for (int i = 0; i < messages.Count; i++)
                    {
                        TadMessage message = messages[i];
                        if (message.Opcode == 0x13 && message.Data.Length == 2
                            && message.Data[0] == 0x00 && message.Data[1] == 0x0B)
                        {
                            sawLogoutSycn = true;
                        }
                    }
                }
            });

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            LogIn(client, clientCodec);
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("4")));

            Assert.True(sawLogoutSycn, "Disconnect must send the SYCN 000B (LoggedOut) logout signal");
            Assert.True(sawFd, "Disconnect must send the 0xFD session-state notification");
            Assert.True(sawHostDcon, "Disconnect must send the host-initiated DCON (the instant-disconnect trigger)");
        }

        /// <summary>
        /// The reactive asker driver (TadAskerSession) drives the connect-to handshake against the
        /// responder to the greeting: connect → accept → session-setup → port-assign → DUMM →
        /// terminal-setup → RESE/RECO → banner. The asker renders the MOTD "ENTER " prompt, proving the
        /// standalone client's state machine end to end in-memory.
        /// </summary>
        [Fact]
        public void AskerDriver_DrivesHandshake_ToBanner()
        {
            PipeTransport serverToClient = new PipeTransport();
            PipeTransport clientToServer = new PipeTransport();

            XmsgCodec serverCodec = new XmsgCodec("server", serverToClient);
            XmsgLayer serverLayer = new XmsgLayer(serverCodec, 102, 0x00);
            TadTerminalResponder responder = new TadTerminalResponder(102, FixedClock);
            responder.SendTerminalBringup = true;   // send the priming DUMM so the asker proceeds
            serverLayer.TadResponder = responder;
            serverLayer.AcknowledgeTadFrames = true;

            XmsgCodec clientCodec = new XmsgCodec("client", clientToServer);
            TadAskerSession asker = new TadAskerSession(100, 102, 0x0283, seed: 0x14, "D102");
            StringBuilder banner = new StringBuilder();
            asker.TerminalText += text => banner.Append(text);

            void SendAll(System.Collections.Generic.IReadOnlyList<XmsgFrame> frames)
            {
                for (int i = 0; i < frames.Count; i++)
                {
                    clientCodec.SendPacket(new XmsgPacket(frames[i]));
                }
            }

            clientCodec.PacketReceived += delegate (string id, XmsgPacketInfo packet)
            {
                SendAll(asker.OnReceive(packet.Frame));
            };

            clientToServer.Target = bytes => serverCodec.ProcessBytes(bytes);
            serverToClient.Target = bytes => clientCodec.ProcessBytes(bytes);

            SendAll(asker.Start());

            // The banner burst ends with BDAT("\r\nENTER ") — the asker rendered it.
            Assert.Contains("ENTER", banner.ToString());
        }

        /// <summary>
        /// Wires a client node and a server node through a synchronous in-memory XMSG pipe: each side's
        /// outbound bytes are fed straight into the other's codec.
        /// </summary>
        /// <param name="terminal">
        /// Collects the BDAT terminal text the server sends back.
        /// </param>
        /// <param name="clientCodec">
        /// Receives the client-side codec the caller sends packets through.
        /// </param>
        /// <param name="onServerFrame">
        /// An optional observer invoked for every frame the server sends back (for asserting non-text
        /// frames such as the 0xFD notification).
        /// </param>
        /// <returns>
        /// The connect-to client bound to the server.
        /// </returns>
        private static TadConnectClient BuildPairedNodes(TerminalCapture terminal, out XmsgCodec clientCodec, Action<XmsgFrame>? onServerFrame = null)
        {
            PipeTransport serverToClient = new PipeTransport();
            PipeTransport clientToServer = new PipeTransport();

            // Server node 102: codec -> layer -> TAD responder.
            XmsgCodec serverCodec = new XmsgCodec("server", serverToClient);
            XmsgLayer serverLayer = new XmsgLayer(serverCodec, 102, 0x00);
            serverLayer.TadResponder = new TadTerminalResponder(102, FixedClock);
            serverLayer.AcknowledgeTadFrames = true;

            // Client node 100: codec; collect the server's BDAT terminal text as it arrives.
            clientCodec = new XmsgCodec("client", clientToServer);
            TadConnectClient client = new TadConnectClient(100, 102, 0x0283, seed: 0x14);
            XmsgCodec capturedClientCodec = clientCodec;
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                client.NoteServerFrame(packet.Frame);
                terminal.Append(packet.Frame);
                onServerFrame?.Invoke(packet.Frame);
            };

            // Close the loop: client bytes -> server codec, server bytes -> client codec.
            clientToServer.Target = bytes => serverCodec.ProcessBytes(bytes);
            serverToClient.Target = bytes => capturedClientCodec.ProcessBytes(bytes);

            return client;
        }

        /// <summary>
        /// Wires the client node against a server node running the NEW framework path: an
        /// <see cref="XmsgServerHost"/> with a registered <see cref="TadServer"/> (*TADADM), instead of
        /// the legacy <see cref="TadTerminalResponder"/>.
        /// </summary>
        /// <param name="terminal">
        /// Collects the BDAT terminal text the server sends back.
        /// </param>
        /// <param name="clientCodec">
        /// Receives the client-side codec.
        /// </param>
        /// <returns>
        /// The connect-to client bound to the server.
        /// </returns>
        /// <summary>
        /// A passwordless account (empty password) logs straight in after the username - the server never
        /// prompts for a password. Verifies the config-driven accounts path (a "ronny"/no-password user).
        /// </summary>
        [Fact]
        public void ServerHost_PasswordlessAccount_LogsInWithoutPasswordPrompt()
        {
            TadUserDirectory users = new TadUserDirectory(new List<TadUser> { new TadUser("ronny", string.Empty) });
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, users, out XmsgCodec clientCodec, out _, out _);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("ronny")));   // username only - no password

            string screen = terminal.Text;
            Assert.Contains("OK", screen);                 // logged in
            Assert.EndsWith("# ", screen);                 // straight to the command prompt
            Assert.DoesNotContain("PASSWORD", screen);     // the password prompt was skipped
        }

        /// <summary>
        /// Two connect-to requests from different client ports open two independent sessions, each with its
        /// own ttyN, up to the capacity. Guards the multi-session manager (unique ports / session numbers).
        /// </summary>
        [Fact]
        public void ServerHost_TwoConnects_OpenTwoIndependentSessions()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client1 = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out _, out TadServer server);

            List<int> openedTtys = new List<int>();
            server.SessionOpened += (tadNumber, clientSystem) => openedTtys.Add(tadNumber);

            // Two connects with DISTINCT client ports -> two distinct sessions.
            TadConnectClient client2 = new TadConnectClient(100, 102, 0x02C6, seed: 0x14);
            clientCodec.SendPacket(new XmsgPacket(client1.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client2.BuildConnect("D102")));

            Assert.Equal(2, server.SessionCount);
            Assert.Equal(2, openedTtys.Count);
            Assert.NotEqual(openedTtys[0], openedTtys[1]);   // distinct ttyN per session
        }

        /// <summary>
        /// The inject API queues a message to sessions matched by TAD number, by username (case-insensitive)
        /// or to all (broadcast), returning how many logged-in sessions received it. Foundation of tell/wall.
        /// </summary>
        [Fact]
        public void ServerHost_InjectApi_QueuesToMatchingLoggedInSessions()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out _, out TadServer server);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password -> logged in

            Assert.Equal(1, server.Broadcast("x"));              // the one logged-in session
            Assert.Equal(1, server.InjectToTad(1, "x"));         // tty1 exists
            Assert.Equal(0, server.InjectToTad(99, "x"));        // no such tty
            Assert.Equal(1, server.InjectToUser("system", "x")); // case-insensitive match on SYSTEM
            Assert.Equal(0, server.InjectToUser("nobody", "x"));
        }

        /// <summary>
        /// An injected message is pushed to the target terminal asynchronously by DrainPending (as would
        /// happen on the next 7DUMM from 100), not returned to whoever sent it.
        /// </summary>
        [Fact]
        public void ServerHost_InjectedMessage_IsPushedToTheTerminal()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out XmsgServerHost host, out TadServer server);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            terminal.Clear();

            server.InjectToTad(1, "\r\nMessage from ronny at TAD 9: ping\r\n");

            // The async push happens on the next DrainPending (a DUMM in the live loop); route it to the client.
            IReadOnlyList<XmsgFrame> pushed = host.DrainPending();
            for (int i = 0; i < pushed.Count; i++)
            {
                clientCodec.ProcessBytes(pushed[i].ToArray());
            }

            Assert.Contains("Message from ronny at TAD 9: ping", terminal.Text);
        }

        /// <summary>
        /// Disconnecting (menu "4") frees the session - it leaves the session table - and the freed TAD
        /// number is reused by the next connect (lowest-free), instead of the counter climbing forever.
        /// </summary>
        [Fact]
        public void ServerHost_Disconnect_FreesSessionAndReusesTadNumber()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient c1 = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out _, out TadServer server);

            clientCodec.SendPacket(new XmsgPacket(c1.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(c1.BuildInput("SYSTEM")));
            clientCodec.SendPacket(new XmsgPacket(c1.BuildInput("SYSTEM")));
            Assert.Equal(1, server.SessionCount);

            clientCodec.SendPacket(new XmsgPacket(c1.BuildInput("4")));   // disconnect (immediate)
            Assert.Equal(0, server.SessionCount);                        // session freed, not lingering

            int reusedTad = -1;
            server.SessionOpened += (tadNumber, clientSystem) => reusedTad = tadNumber;
            TadConnectClient c2 = new TadConnectClient(100, 102, 0x02C6, seed: 0x14);
            clientCodec.SendPacket(new XmsgPacket(c2.BuildConnect("D102")));
            Assert.Equal(1, server.SessionCount);
            Assert.Equal(1, reusedTad);   // the freed tty1 is reused, not tty2
        }

        /// <summary>
        /// "who" lists the logged-in sessions and marks the caller with a "===>" arrow.
        /// </summary>
        [Fact]
        public void ServerHost_Who_ListsSessionsAndMarksCaller()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            terminal.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("who")));

            string screen = terminal.Text;
            Assert.Contains("===> tty1", screen);   // the caller, arrow-marked
            Assert.Contains("SYSTEM", screen);       // the logged-in user
            Assert.EndsWith("# ", screen);
        }

        /// <summary>
        /// "help" lists the command registry - the numbered menu plus stat / who / tell / wall / list / help.
        /// (The reply spans several 255-byte frames; the harness captures the first windowed batch, which
        /// holds the header and most commands.)
        /// </summary>
        [Fact]
        public void ServerHost_Help_ListsTheCommandRegistry()
        {
            string screen = RunLoggedInCommand("help");
            Assert.Contains("COMMANDS", screen);
            Assert.Contains("stat", screen);
            Assert.Contains("who", screen);
            Assert.Contains("wall", screen);
        }

        /// <summary>
        /// "list service" lists the known XROUT services (XSLET, ...) from the code table.
        /// </summary>
        [Fact]
        public void ServerHost_ListService_ListsXroutServices()
        {
            string screen = RunLoggedInCommand("list service");
            Assert.Contains("SERVICES", screen);
            Assert.Contains("XSLET", screen);
        }

        /// <summary>
        /// help and list service MUST each fit one terminal frame shorter than 255 bytes - 100 renders only the final
        /// chunk of a multi-frame reply, so a long listing would show only its tail. Guards that regression.
        /// </summary>
        [Theory]
        [InlineData("help")]
        [InlineData("list service")]
        public void ServerHost_IntrospectionCommand_IsSingleFrame(string command)
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            terminal.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput(command)));

            IReadOnlyList<TadFrameShape> frames = terminal.TadFrames;
            Assert.Single(frames);   // exactly one frame - 100 will render it whole
            Assert.True(frames[0].BdatBytes < 255, $"'{command}' reply is {frames[0].BdatBytes} bytes; must fit one buffer");
            Assert.True(frames[0].HasRfi, "the single frame must carry the RFI");
        }

        /// <summary>
        /// "list servers" lists the registered servers - here the fallback *TADADM entry (no directory wired).
        /// </summary>
        [Fact]
        public void ServerHost_ListServers_ListsTadAdmin()
        {
            string screen = RunLoggedInCommand("list servers");
            Assert.Contains("SERVERS", screen);
            Assert.Contains("*TADADM", screen);
        }

        /// <summary>
        /// Logs a session in (SYSTEM/SYSTEM), clears the capture, runs one command, and returns the captured
        /// terminal text (the first windowed batch for a multi-frame reply).
        /// </summary>
        /// <param name="command">
        /// The command to run.
        /// </param>
        /// <returns>
        /// The terminal text captured for the command.
        /// </returns>
        private static string RunLoggedInCommand(string command)
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));
            terminal.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput(command)));
            return terminal.Text;
        }

        private static TadConnectClient BuildViaServerHost(TerminalCapture terminal, out XmsgCodec clientCodec)
        {
            return BuildViaServerHost(terminal, null, out clientCodec, out _, out _);
        }

        private static TadConnectClient BuildViaServerHost(TerminalCapture terminal, out XmsgCodec clientCodec, out XmsgServerHost serverHost)
        {
            return BuildViaServerHost(terminal, null, out clientCodec, out serverHost, out _);
        }

        private static TadConnectClient BuildViaServerHost(
            TerminalCapture terminal, TadUserDirectory? users, out XmsgCodec clientCodec, out XmsgServerHost serverHost, out TadServer tadServer)
        {
            PipeTransport serverToClient = new PipeTransport();
            PipeTransport clientToServer = new PipeTransport();

            XmsgCodec serverCodec = new XmsgCodec("server", serverToClient);
            XmsgLayer serverLayer = new XmsgLayer(serverCodec, 102, 0x00);
            // Match the live runner's flags EXACTLY: AcknowledgeData=false (the legacy generic path is
            // off) and AcknowledgeTadFrames=true (session data is secure-ACKed). This reproduces the live
            // condition so the ServerHost_SessionData_IsSecureAcked regression actually bites - the old
            // buggy gate (if AcknowledgeData) would emit NO ACK here, exactly as it did against 100.
            serverLayer.AcknowledgeData = false;
            serverLayer.AcknowledgeTadFrames = true;
            XmsgServerHost host = new XmsgServerHost(102);
            TadServer server = new TadServer(FixedClock, users);
            host.Register(server);
            serverLayer.ServerHost = host;
            serverHost = host;
            tadServer = server;

            clientCodec = new XmsgCodec("client", clientToServer);
            TadConnectClient client = new TadConnectClient(100, 102, 0x0283, seed: 0x14);
            XmsgCodec capturedClientCodec = clientCodec;
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                client.NoteServerFrame(packet.Frame);
                terminal.Append(packet.Frame);
            };

            clientToServer.Target = bytes => serverCodec.ProcessBytes(bytes);
            serverToClient.Target = bytes => capturedClientCodec.ProcessBytes(bytes);

            return client;
        }

        /// <summary>
        /// The framework path (XmsgServerHost + TadServer, the *TADADM server) reproduces the connect-to
        /// session end to end: connect, log in SYSTEM/SYSTEM, then the "help" menu - the Phase 1 gate that
        /// the port preserved behavior. Same scenario as ClientConnectsAndTypesHelp, new server dispatch.
        /// </summary>
        [Fact]
        public void ServerHost_ClientConnectsAndTypesHelp_ReturnsMenu()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("help")));

            string screen = terminal.Text;
            Assert.Contains("PASSWORD", screen);   // login prompted for a password
            Assert.Contains("OK", screen);         // login accepted
            Assert.Contains("COMMANDS", screen);   // the help command registry rendered
            Assert.Contains("stat", screen);
        }

        /// <summary>
        /// An out-of-model incoming frame (its Counter implies a DIFFERENT link seed) must NOT poison
        /// the host's learned seed: every frame the host originates afterwards still satisfies the
        /// envelope invariant <c>Counter + Flags1.low + Flags2.low == link seed</c> (0x14 here).
        /// Regression for the live 2026-07-07 failure: the per-frame seed refresh let one bad received
        /// frame push the link seed to 0x16, so the next output chunk went out with an invalid Counter
        /// (violating the invariant that held 753/753 in the capture corpus).
        /// </summary>
        [Fact]
        public void ServerHost_OutOfModelFrame_DoesNotPoisonLinkSeed()
        {
            TerminalCapture terminal = new TerminalCapture();
            List<XmsgFrame> fromServer = new List<XmsgFrame>();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                fromServer.Add(packet.Frame);
            };

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password

            // Poison attempt: a command frame that is valid EXCEPT its Counter, which implies link
            // seed 0x16 instead of 0x14 (the measured live poisoning signature). The server must
            // answer it — and everything after it — with envelopes still derived from the TRUE seed.
            XmsgFrame poisoned = client.BuildInput("help");
            poisoned.Header!.Counter = (byte)(poisoned.Header.Counter + 2);
            poisoned.ClearRawBytes();
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(poisoned));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("time")));

            int checkedFrames = 0;
            for (int i = 0; i < fromServer.Count; i++)
            {
                XmsgFrame frame = fromServer[i];
                if (frame.SubHeader == null)
                {
                    continue;   // secure-ACKs carry no sub-header; the invariant applies to data frames
                }

                byte implied = XmsgEnvelope.LearnSeed(
                    frame.Header!.Flags1, frame.Header.Counter, frame.Header.Flags2);
                Assert.Equal(0x14, implied);
                checkedFrames++;
            }

            Assert.True(checkedFrames > 0, "no server data frames captured — the scenario did not run");
        }

        /// <summary>
        /// The framework dispatch path MUST secure-ACK (subtype <c>0x03</c>) each session data frame it
        /// receives. Regression guard for the XmsgNode flag bug: the dispatch block gated its ACK on
        /// AcknowledgeData (false in the runner) instead of AcknowledgeTadFrames, so the live 100 got no
        /// ACKs, retransmitted, and crashed with XEIMA (invalid magic) on the first multi-frame reply.
        /// </summary>
        [Fact]
        public void ServerHost_SessionData_IsSecureAcked()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            // A connect plus one keystroke line: each inbound data frame must draw a subtype-0x03 ACK.
            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));

            int ackCount = 0;
            for (int i = 0; i < terminal.Subtypes.Count; i++)
            {
                if (terminal.Subtypes[i] == SintranPacketSubtype.Ack)
                {
                    ackCount++;
                }
            }

            Assert.True(ackCount > 0, "framework path emitted no secure ACK (subtype 0x03) for session data");
        }

        /// <summary>
        /// The "stat" reply is a SINGLE terminal frame (kept under one 255-byte buffer) that shows the tty
        /// and ends with the "# " prompt. Single-frame output is the reliably-displayed path: a multi-chunk
        /// (255-sentinel) reply is delivered and ACKed correctly by 100 but 100 only displays the FINAL
        /// chunk, dropping the first continuation (and with it the top of the report) from the screen. So
        /// stat stays under one buffer; the flow-control windowing is retained for a future confirmed
        /// long-output path. Labels use parentheses / plain text, never square brackets.
        /// </summary>
        [Fact]
        public void ServerHost_Stat_IsSingleFrameShowingTty()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();                                                     // isolate the stat reply
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("stat")));

            // Exactly one terminal frame, short (< 255) and carrying the RFI - so 100 displays it whole.
            IReadOnlyList<TadFrameShape> frames = terminal.TadFrames;
            Assert.Single(frames);
            Assert.True(frames[0].BdatBytes < 255, $"stat reply is {frames[0].BdatBytes} bytes; must fit one buffer");
            Assert.True(frames[0].HasRfi, "the stat reply must carry the RFI");

            string screen = terminal.Text;
            Assert.Contains("SESSION STATUS", screen);
            Assert.Contains("TAD number  : tty", screen);   // the tty IS shown
            Assert.EndsWith("# ", screen);
            Assert.DoesNotContain("[", screen);             // 0x5B renders as AE on the ND terminal
            Assert.DoesNotContain("]", screen);             // 0x5D renders as AA
        }

        /// <summary>
        /// The "3" / echo diagnostic streams as 255-byte continuation PAIRS spaced ~46 ms apart (the
        /// verified 22.16 output-queue algorithm). On the command only the FIRST chunk of the first pair
        /// goes out (one bare 255-byte BDAT, no RFI); the SECOND chunk is held until the intra-pair gap
        /// elapses (released by the runner's periodic pump on the live link - the constant test clock never
        /// advances it), and the final RFI terminator waits until the last continuation is acked. The
        /// distinct "ECHO FRAME n OF 3" markers let a live run show which frames 100 actually displays.
        /// </summary>
        [Fact]
        public void ServerHost_EchoDiagnostic_FirstChunkIsOneContinuation()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out _, out TadServer tadServer);
            tadServer.OutputMode = TadOutputMode.SentinelStream;   // this test pins the 255-sentinel behavior

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();                                                     // isolate the echo reply
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("3")));

            IReadOnlyList<TadFrameShape> first = terminal.TadFrames;
            Assert.Single(first);                            // only the pair's first chunk; the second waits on the gap timer
            Assert.Equal(255, first[0].BdatBytes);           // a full 255-byte continuation
            Assert.False(first[0].HasRfi, "continuation must not carry an RFI");

            // Frame 1's marker rode the first continuation; frames 2 and 3 wait for the intra-pair gap / ACKs.
            string screen = terminal.Text;
            Assert.Contains("ECHO FRAME 1 OF 3", screen);
            Assert.DoesNotContain("ECHO FRAME 2 OF 3", screen);
            Assert.DoesNotContain("ECHO FRAME 3 OF 3", screen);
        }

        /// <summary>
        /// In the default <see cref="TadOutputMode.CompleteSegments"/> mode the FIRST frame of a
        /// >255-byte reply is a COMPLETE BDAT segment of at most 240 bytes — NOT a 255-byte sentinel —
        /// and carries no RFI (more follows). This is the receiver-decode-backed construct
        /// (COS-CONN-TO-E02-Analysis.md §5b): no count==0xFF anywhere, plain complete elements, window-of-1.
        /// (The in-memory harness is single-shot — it does not pump the client ACKs that release later
        /// segments — so only the first frame is observable here, as in the sentinel test above; whole-reply
        /// delivery is a live-machine property.)
        /// </summary>
        [Fact]
        public void ServerHost_SegmentedOutput_FirstFrameIsCompleteSegment_NotSentinel()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out _, out TadServer tadServer);
            Assert.Equal(TadOutputMode.CompleteSegments, tadServer.OutputMode);   // the default

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();                                                     // isolate the echo reply
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("3")));        // ~560-byte 3-frame echo

            IReadOnlyList<TadFrameShape> frames = terminal.TadFrames;
            Assert.Single(frames);                                    // window-of-1: next waits on the ACK
            Assert.True(frames[0].BdatBytes <= 240, $"segment is {frames[0].BdatBytes} bytes; must be <= 240");
            Assert.NotEqual(255, frames[0].BdatBytes);                // never the 0xFF sentinel
            Assert.False(frames[0].HasRfi, "a non-final segment must not carry an RFI");

            // The first segment carries the start of the reply (frame 1), not the tail.
            string screen = terminal.Text;
            Assert.Contains("ECHO FRAME 1 OF 3", screen);
            Assert.DoesNotContain("ECHO FRAME 3 OF 3", screen);
        }

        /// <summary>
        /// Segmented output advances on the remote's ACK: after 100 ACKs segment 1, the host releases
        /// segment 2 (window-of-1). This exercises the live-critical path the single-shot pipe cannot -
        /// XmsgNode drains ACK-advancing servers (<see cref="XmsgServerHost.DrainOnAck"/>) after applying
        /// an ACK - by driving the ServerHost directly. Without it the burst would stall after segment 1
        /// on the real machine.
        /// </summary>
        [Fact]
        public void ServerHost_SegmentedOutput_ReleasesNextSegmentOnAck()
        {
            TerminalCapture terminal = new TerminalCapture();
            List<XmsgFrame> fromServer = new List<XmsgFrame>();
            TadConnectClient client = BuildViaServerHost(terminal, null, out XmsgCodec clientCodec, out XmsgServerHost serverHost, out _);
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                fromServer.Add(packet.Frame);
            };

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("3")));        // ~560-byte 3-frame echo

            // Segment 1 went out and was rendered; capture its Flags1 from the parsed header (the frame
            // 100 will ACK). The received frame's TAD bytes live in the capture, not TrailingBytes, so
            // assert its content via the terminal text.
            XmsgFrame segment1 = LastTerminalDataFrame(fromServer);
            ushort seg1Flags1 = segment1.Header.Flags1;
            Assert.Contains("ECHO FRAME 1", terminal.Text);
            Assert.DoesNotContain("ECHO FRAME 2", terminal.Text);   // segment 2 not sent yet (window-of-1)

            // 100 ACKs segment 1 -> the node applies it (ConfirmDelivered) then drains ACK-advancing
            // servers. Simulate that pair: release the window, then DrainOnAck.
            serverHost.ConfirmDelivered(100, seg1Flags1);
            IReadOnlyList<XmsgFrame> released = serverHost.DrainOnAck();

            Assert.Single(released);                                  // exactly the next segment
            Assert.Contains("ECHO FRAME 2", TadText(released[0]));    // frame 2, released by the ACK (built frame)
            Assert.NotEqual((ushort)seg1Flags1, released[0].Header.Flags1);
        }

        /// <summary>
        /// Returns the last terminal-data (XMCSM 0x01080000) frame in a captured list.
        /// </summary>
        /// <param name="frames">
        /// The captured frames.
        /// </param>
        /// <returns>
        /// The last terminal-data frame.
        /// </returns>
        private static XmsgFrame LastTerminalDataFrame(List<XmsgFrame> frames)
        {
            XmsgFrame? found = null;
            for (int i = 0; i < frames.Count; i++)
            {
                if (frames[i].SubHeader != null && frames[i].ControlService == 0x01080000u)
                {
                    found = frames[i];
                }
            }

            Assert.NotNull(found);
            return found!;
        }

        /// <summary>
        /// Renders the printable ASCII of a frame's BDAT bytes, for asserting which echo-frame it carries.
        /// </summary>
        /// <param name="frame">
        /// The terminal-data frame.
        /// </param>
        /// <returns>
        /// The frame's BDAT text (7-bit ASCII).
        /// </returns>
        private static string TadText(XmsgFrame frame)
        {
            StringBuilder sb = new StringBuilder();
            byte[] payload = frame.GetBodyBytes();

            // CORRECTED 2026-08-04: TrailingBytes is now the whole MESSAGE BODY from wire 28, and a
            // TAD frame's body opens with the same four bytes a letter's does - serial 0x00,
            // service 0x00, then the big-endian length of the chain. The chain itself starts at 32.
            // VERIFIED on the captured conn-to-d102 DUMM frame: body 0000 0002 1800.
            int i = payload.Length >= XroutMessage.HeaderSize ? XroutMessage.HeaderSize : payload.Length;
            while (i < payload.Length)
            {
                if (payload[i] == 0x00) { i++; continue; }   // skip pads / 16-bit-op prefixes
                byte op = payload[i];
                int count = i + 1 < payload.Length ? payload[i + 1] : 0;
                if (op == 0x01)   // BDAT
                {
                    for (int j = 0; j < count && i + 2 + j < payload.Length; j++)
                    {
                        int c = payload[i + 2 + j] & 0x7F;
                        if (c >= 32 && c < 127) { sb.Append((char)c); }
                    }
                }

                i += 2 + count;
            }

            return sb.ToString();
        }

        /// <summary>
        /// The MOTD banner is generated per host: a dynamic date/time line (from the injected clock), the
        /// configurable MOTD line (default = the assembly version banner), and a "--- HOST ID:nnn ---" line
        /// built from this node's number. Guards the replacement of the old hardcoded 1998/RETROCORE banner.
        /// </summary>
        [Fact]
        public void ServerHost_Motd_HasDynamicDateVersionAndHostId()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            // Connect, then the full client-driven bring-up ladder as every real capture shows it:
            // TMOD chain (host stays silent) → ESCA (host: ESRS + RESE#1) → RECO (host: RESE#2)
            // → RECO (host: the MOTD banner). See XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md §1.
            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildTerminalSetup()));
            clientCodec.SendPacket(new XmsgPacket(client.BuildEsca()));
            clientCodec.SendPacket(new XmsgPacket(client.BuildReco()));
            clientCodec.SendPacket(new XmsgPacket(client.BuildReco()));

            string screen = terminal.Text;
            Assert.Contains("--- HOST ID:102 TAD:1 ---", screen);           // host id + this session's tty number
            Assert.Contains("Emulated TAD server version v", screen);       // default MOTD line (assembly version)
            Assert.Contains("JULY", screen);                               // dynamic date, FixedClock = 2026-07-02
            Assert.Contains("2026", screen);
            Assert.DoesNotContain("RETROCORE", screen);                     // the old hardcoded banner is gone
            Assert.DoesNotContain("VSX/500", screen);
        }

        /// <summary>
        /// The bring-up follows the captured client-driven ladder byte-for-byte in shape
        /// (XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md §1): the host is SILENT after the TMOD chain;
        /// ESCA is answered by ESRS (ff 0x86) + RESE#1 (ff 0x96); the first RECO by RESE#2 (ff 0x92);
        /// the second RECO by the banner (ff 0x96). Regression for the old unprompted
        /// 0x20+RESE+RESE+MOTD burst, which no real capture contains.
        /// </summary>
        [Fact]
        public void ServerHost_BringupLadder_IsClientDriven_WithAlternatingFrameFlags()
        {
            TerminalCapture terminal = new TerminalCapture();
            List<XmsgFrame> fromServer = new List<XmsgFrame>();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                fromServer.Add(packet.Frame);
            };

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));

            // TMOD chain: the host must answer NOTHING (its only frames so far are the ACKs and the
            // connect-time accept/port-assign/DUMM, which arrive before this point).
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildTerminalSetup()));
            int dataAfterTmod = CountDataFrames(fromServer);
            Assert.Equal(0, dataAfterTmod);

            // ESCA -> ESRS (0x20, class 0x0008, ff 0x86) + RESE#1 (class 0x0108, ff 0x96).
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildEsca()));
            List<XmsgFrame> escaReplies = DataFrames(fromServer);
            Assert.Equal(2, escaReplies.Count);
            Assert.Equal(0x00080000u, escaReplies[0].ControlService);
            Assert.Equal(0x86, escaReplies[0].SubHeader!.FrameFlags);
            Assert.Equal(0x01080000u, escaReplies[1].ControlService);
            Assert.Equal(0x96, escaReplies[1].SubHeader!.FrameFlags);

            // First RECO -> RESE#2, ff 0x92.
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildReco()));
            List<XmsgFrame> reco1Replies = DataFrames(fromServer);
            Assert.Single(reco1Replies);
            Assert.Equal(0x92, reco1Replies[0].SubHeader!.FrameFlags);

            // Second RECO -> the banner, ff 0x96, ending with SYCN 0002 + "ENTER " + RFI.
            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(client.BuildReco()));
            List<XmsgFrame> reco2Replies = DataFrames(fromServer);
            Assert.Single(reco2Replies);
            Assert.Equal(0x96, reco2Replies[0].SubHeader!.FrameFlags);
            Assert.Contains("ENTER", terminal.Text);
        }

        /// <summary>
        /// A XENSE reject (subtype 0x07, code 0xFFDE in Flags2) of our connect-accept — the peer's
        /// XMSG restarted while our persisted sequence had climbed — must be answered by re-sending
        /// the accept ONE Flags1 lower with a formula-consistent envelope (step-down convergence).
        /// Regression for the live 2026-07-07 hang: the ServerHost path had no XENSE recovery (only
        /// the legacy TadResponder did), so the connect-to stalled forever.
        /// </summary>
        [Fact]
        public void ServerHost_XenseRejectOfAccept_ResendsAcceptOneLower()
        {
            TerminalCapture terminal = new TerminalCapture();
            List<XmsgFrame> fromServer = new List<XmsgFrame>();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);
            clientCodec.PacketReceived += delegate (string linkId, XmsgPacketInfo packet)
            {
                fromServer.Add(packet.Frame);
            };

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));

            // Find the accept (the only XSLET-class frame the host sends).
            XmsgFrame? accept = null;
            for (int i = 0; i < fromServer.Count; i++)
            {
                if (fromServer[i].SubHeader != null && fromServer[i].ControlService == 0x04000041u)
                {
                    accept = fromServer[i];
                }
            }

            Assert.NotNull(accept);
            ushort acceptF1 = accept!.Header.Flags1;

            // 100's XENSE reject: subtype 0x07, Flags1 echoing the rejected accept, the error code
            // riding the Flags2 field (0xFFDE = -34 = XENSE "network sequencing error").
            XmsgFrame xense = new XmsgFrame();
            xense.Header.Subtype = SintranPacketSubtype.NetworkError;
            xense.Header.DestinationNode = 102;
            xense.Header.SourceNode = 100;
            xense.Header.Flags1 = acceptF1;
            xense.Header.Flags2 = unchecked((ushort)XmsgError.XENSE);
            xense.Header.ProtocolId = SintranProtocolId.Routing;
            xense.ClearRawBytes();

            fromServer.Clear();
            clientCodec.SendPacket(new XmsgPacket(xense));

            // The host re-sends the accept one Flags1 lower, with a correct header checksum.
            List<XmsgFrame> replies = DataFrames(fromServer);
            Assert.Single(replies);
            XmsgFrame resent = replies[0];
            Assert.Equal(0x04000041u, resent.ControlService);
            Assert.Equal((ushort)(acceptF1 - 1), resent.Header.Flags1);

            // UPDATED 2026-08-05. This used to assert LearnSeed(...) == 0x14 - that the resent
            // frame's word 6 still satisfied the fitted seed model. XmsgServerHost now DERIVES
            // word 6 as the carved ones-complement checksum, so that expectation is stale: it
            // described the arithmetic we used to invent the value with, not a property of the
            // wire.
            //
            // The low byte moved by one here, which is the end-around carry - when the sum's high
            // half changes, the carry into the low half can change with it. (On the FA listing,
            // between nodes 100 and 19999, the low bytes happened to agree exactly; that was luck
            // of the arithmetic, not a rule.)
            //
            // What matters for THIS test is unchanged and still checked above: the accept is
            // re-sent, once, one Flags 1 lower. The checksum is now asserted as the checksum.
            SintranHeader header = resent.Header;
            Assert.Equal(
                XmsgEnvelope.ComputeHeaderChecksum(
                    (ushort)((header.Marker1 << 8) | header.Marker2),
                    (ushort)((header.PacketType << 8) | (byte)header.Subtype),
                    header.DestinationNode,
                    header.SourceNode,
                    header.Flags1,
                    header.Flags2),
                header.Checksum);
        }

        /// <summary>
        /// Counts the data frames (frames with a sub-header — ACKs carry none) in a captured list.
        /// </summary>
        /// <param name="frames">
        /// The captured frames.
        /// </param>
        /// <returns>
        /// The number of data frames.
        /// </returns>
        private static int CountDataFrames(List<XmsgFrame> frames)
        {
            return DataFrames(frames).Count;
        }

        /// <summary>
        /// Filters a captured list down to the data frames (frames with a sub-header).
        /// </summary>
        /// <param name="frames">
        /// The captured frames.
        /// </param>
        /// <returns>
        /// The data frames, in order.
        /// </returns>
        private static List<XmsgFrame> DataFrames(List<XmsgFrame> frames)
        {
            List<XmsgFrame> data = new List<XmsgFrame>();
            for (int i = 0; i < frames.Count; i++)
            {
                if (frames[i].SubHeader != null)
                {
                    data.Add(frames[i]);
                }
            }

            return data;
        }

        /// <summary>
        /// A one-directional in-memory transport: forwards each frame's bytes to a target codec.
        /// </summary>
        private sealed class PipeTransport : IXmsgTransport
        {
            /// <summary>
            /// The sink that receives forwarded bytes (the other node's codec).
            /// </summary>
            public Action<byte[]>? Target { get; set; }

            /// <summary>
            /// Forwards the outbound bytes to the target.
            /// </summary>
            /// <param name="bytes">
            /// The serialised frame bytes.
            /// </param>
            public void Send(ReadOnlySpan<byte> bytes)
            {
                Target?.Invoke(bytes.ToArray());
            }
        }

        /// <summary>
        /// Accumulates the ASCII text of every BDAT message in the frames the server sends.
        /// </summary>
        private sealed class TerminalCapture
        {
            private readonly StringBuilder _text = new StringBuilder();
            private readonly List<SintranPacketSubtype> _subtypes = new List<SintranPacketSubtype>();
            private readonly List<TadFrameShape> _tadFrames = new List<TadFrameShape>();

            /// <summary>
            /// Gets the accumulated terminal text.
            /// </summary>
            public string Text
            {
                get { return _text.ToString(); }
            }

            /// <summary>
            /// Gets the SINTRAN subtype of every frame the server sent, in arrival order.
            /// </summary>
            /// <remarks>
            /// Used to assert the framework path secure-ACKs session traffic (a subtype
            /// <see cref="SintranPacketSubtype.Ack"/> frame must appear).
            /// </remarks>
            public IReadOnlyList<SintranPacketSubtype> Subtypes
            {
                get { return _subtypes; }
            }

            /// <summary>
            /// Gets the shape (first BDAT byte-count and whether an RFI is present) of every terminal-data
            /// frame that carried a BDAT, in arrival order. Used to assert the 255-byte sentinel chunking.
            /// </summary>
            public IReadOnlyList<TadFrameShape> TadFrames
            {
                get { return _tadFrames; }
            }

            /// <summary>
            /// Clears the accumulated text, subtypes and frame shapes (to isolate a later exchange).
            /// </summary>
            public void Clear()
            {
                _text.Clear();
                _subtypes.Clear();
                _tadFrames.Clear();
            }

            /// <summary>
            /// Appends the BDAT text of a received frame (high bit stripped).
            /// </summary>
            /// <param name="frame">
            /// A frame received from the server.
            /// </param>
            public void Append(XmsgFrame frame)
            {
                if (frame?.Header != null)
                {
                    _subtypes.Add(frame.Header.Subtype);
                }

                if (frame?.Tad == null)
                {
                    return;
                }

                IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
                int firstBdatBytes = -1;
                bool hasRfi = false;
                for (int i = 0; i < messages.Count; i++)
                {
                    if (messages[i].Opcode == 0x02)   // RFI
                    {
                        hasRfi = true;
                    }

                    if (messages[i].Opcode != 0x01)   // BDAT
                    {
                        continue;
                    }

                    byte[] data = messages[i].Data;
                    if (firstBdatBytes < 0)
                    {
                        firstBdatBytes = data.Length;
                    }

                    for (int j = 0; j < data.Length; j++)
                    {
                        _text.Append((char)(data[j] & 0x7F));
                    }
                }

                if (firstBdatBytes >= 0)
                {
                    ushort flags1 = frame?.Header != null ? frame.Header.Flags1 : (ushort)0;
                    _tadFrames.Add(new TadFrameShape(firstBdatBytes, hasRfi, flags1));
                }
            }
        }

        /// <summary>
        /// The shape of one terminal-data frame: its first BDAT's data length, whether it carries an RFI,
        /// and its Flags 1 (so a test can ACK it to advance the flow-control window).
        /// </summary>
        private readonly struct TadFrameShape
        {
            /// <summary>
            /// The number of data bytes in the frame's first BDAT element.
            /// </summary>
            public readonly int BdatBytes;

            /// <summary>
            /// Whether the frame contains an RFI (ready-for-input) message.
            /// </summary>
            public readonly bool HasRfi;

            /// <summary>
            /// The frame's Flags 1 (the value a matching ACK echoes).
            /// </summary>
            public readonly ushort Flags1;

            /// <summary>
            /// Initialises the frame shape.
            /// </summary>
            /// <param name="bdatBytes">
            /// The first BDAT's data length.
            /// </param>
            /// <param name="hasRfi">
            /// Whether an RFI is present.
            /// </param>
            /// <param name="flags1">
            /// The frame's Flags 1.
            /// </param>
            public TadFrameShape(int bdatBytes, bool hasRfi, ushort flags1)
            {
                BdatBytes = bdatBytes;
                HasRfi = hasRfi;
                Flags1 = flags1;
            }
        }
    }
}
