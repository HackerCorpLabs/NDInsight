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

            // Drive: connect-to D102, then type "help".
            clientCodec.SendPacket(new XmsgPacket(client.BuildConnect("D102")));
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("help")));

            string screen = terminal.Text;
            Assert.Contains("Time", screen);
            Assert.Contains("Echo", screen);
            Assert.Contains("Disconnect", screen);
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
            clientCodec.SendPacket(new XmsgPacket(client.BuildInput("3")));

            Assert.Contains("IPSUM LORUM", terminal.Text);
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
        /// <returns>
        /// The connect-to client bound to the server.
        /// </returns>
        private static TadConnectClient BuildPairedNodes(TerminalCapture terminal, out XmsgCodec clientCodec)
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
