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
                if (frame.SubHeader != null && frame.SubHeader.ControlService == 0x00060000u)
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
        /// <param name="client">The connect-to client.</param>
        /// <param name="codec">The client codec.</param>
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
                if (frame.SubHeader.ControlService == 0x00060000u)
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
                if (frame.SubHeader.ControlService == 0x01080000u && frame.Tad != null)
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
        private static TadConnectClient BuildViaServerHost(TerminalCapture terminal, out XmsgCodec clientCodec)
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
            host.Register(new TadServer(FixedClock));
            serverLayer.ServerHost = host;

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
            Assert.Contains("Time", screen);       // the menu rendered
            Assert.Contains("Disconnect", screen);
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
        /// The full rich "stat" report renders and ends with the "# " prompt (so the prompt returns
        /// without an Enter). Long output is now legal via the 255-byte sentinel chunking. Labels use
        /// parentheses, never square brackets (0x5B/0x5D render as Norwegian letters on the ND terminal).
        /// </summary>
        [Fact]
        public void ServerHost_Stat_RendersRichReportAndEndsWithPrompt()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();                                                     // isolate the stat reply
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("stat")));

            string screen = terminal.Text;
            Assert.Contains("TAD SESSION STATUS", screen);   // the rich report rendered
            Assert.Contains("Client port", screen);
            Assert.EndsWith("# ", screen);                   // prompt returns without needing an Enter
            Assert.DoesNotContain("[", screen);              // no 0x5B - renders as AE on the ND terminal
            Assert.DoesNotContain("]", screen);              // no 0x5D - renders as AA
        }

        /// <summary>
        /// A terminal reply longer than one buffer is streamed as the TAD full-buffer sentinel: every
        /// non-final frame is a BARE 255-byte BDAT (no RFI), and the final frame is a short (&lt; 255) BDAT
        /// that carries the RFI. This is the verified rule (TAD-Message-Formats.md section 22.6) that the
        /// earlier 128/240-byte chunking violated - a short non-final frame with no RFI is what crashed 100.
        /// </summary>
        [Fact]
        public void ServerHost_LongReply_Uses255ByteSentinelChunking()
        {
            TerminalCapture terminal = new TerminalCapture();
            TadConnectClient client = BuildViaServerHost(terminal, out XmsgCodec clientCodec);

            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // username
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("SYSTEM")));   // password
            terminal.Clear();                                                     // isolate the stat reply
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("stat")));

            IReadOnlyList<TadFrameShape> frames = terminal.TadFrames;
            Assert.True(frames.Count >= 2, "the stat reply (> 255 bytes) must span a continuation plus a final frame");

            // Every non-final frame: a bare 255-byte continuation with NO RFI.
            for (int i = 0; i < frames.Count - 1; i++)
            {
                Assert.Equal(255, frames[i].BdatBytes);
                Assert.False(frames[i].HasRfi, "a 255-byte continuation must not carry an RFI");
            }

            // The final frame: a short (< 255) terminator that carries the RFI.
            TadFrameShape last = frames[frames.Count - 1];
            Assert.True(last.BdatBytes < 255, "the final frame must be shorter than 255 bytes");
            Assert.True(last.HasRfi, "the final frame must carry the RFI");
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

            // Connect, then the terminal-setup (TMOD) frame that triggers the server's MOTD burst.
            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildTerminalSetup()));

            string screen = terminal.Text;
            Assert.Contains("--- HOST ID:102 ---", screen);                 // host id from the node number
            Assert.Contains("Emulated TAD server version v", screen);       // default MOTD line (assembly version)
            Assert.Contains("JULY", screen);                               // dynamic date, FixedClock = 2026-07-02
            Assert.Contains("2026", screen);
            Assert.DoesNotContain("RETROCORE", screen);                     // the old hardcoded banner is gone
            Assert.DoesNotContain("VSX/500", screen);
        }

        /// <summary>
        /// A one-directional in-memory transport: forwards each frame's bytes to a target codec.
        /// </summary>
        private sealed class PipeTransport : IXmsgTransport
        {
            /// <summary>The sink that receives forwarded bytes (the other node's codec).</summary>
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

            /// <summary>Gets the accumulated terminal text.</summary>
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
                    _tadFrames.Add(new TadFrameShape(firstBdatBytes, hasRfi));
                }
            }
        }

        /// <summary>
        /// The shape of one terminal-data frame: its first BDAT's data length and whether it carries an RFI.
        /// </summary>
        private readonly struct TadFrameShape
        {
            /// <summary>The number of data bytes in the frame's first BDAT element.</summary>
            public readonly int BdatBytes;

            /// <summary>Whether the frame contains an RFI (ready-for-input) message.</summary>
            public readonly bool HasRfi;

            /// <summary>
            /// Initialises the frame shape.
            /// </summary>
            /// <param name="bdatBytes">
            /// The first BDAT's data length.
            /// </param>
            /// <param name="hasRfi">
            /// Whether an RFI is present.
            /// </param>
            public TadFrameShape(int bdatBytes, bool hasRfi)
            {
                BdatBytes = bdatBytes;
                HasRfi = hasRfi;
            }
        }
    }
}
