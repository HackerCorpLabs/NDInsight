using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg.Codec;
using NDInsight.Sintran.Xmsg.Node.Seam;
using NDInsight.Sintran.Xmsg.Node.Tad;
using NDInsight.Sintran.Xmsg.Packet;
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

            /// <summary>Gets the accumulated terminal text.</summary>
            public string Text
            {
                get { return _text.ToString(); }
            }

            /// <summary>
            /// Appends the BDAT text of a received frame (high bit stripped).
            /// </summary>
            /// <param name="frame">
            /// A frame received from the server.
            /// </param>
            public void Append(XmsgFrame frame)
            {
                if (frame?.Tad == null)
                {
                    return;
                }

                IReadOnlyList<TadMessage> messages = frame.Tad.Messages;
                for (int i = 0; i < messages.Count; i++)
                {
                    if (messages[i].Opcode != 0x01)
                    {
                        continue;
                    }

                    byte[] data = messages[i].Data;
                    for (int j = 0; j < data.Length; j++)
                    {
                        _text.Append((char)(data[j] & 0x7F));
                    }
                }
            }
        }
    }
}
