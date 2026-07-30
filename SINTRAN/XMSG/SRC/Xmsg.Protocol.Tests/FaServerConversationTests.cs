using System;
using System.Collections.Generic;
using System.IO;
using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Protocol.Fa;
using NDInsight.Sintran.Xmsg.Tests;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Replays node 100's side of the captured conversations through
    /// <see cref="FaServerConversation"/> and requires every message to match byte for byte.
    /// </summary>
    /// <remarks>
    /// Together with <c>FaClientConversationTests</c> this covers both halves of the exchange: the
    /// client driver reproduces every request the real client sent, and this reproduces every
    /// message the real server answered with.
    /// </remarks>
    public sealed class FaServerConversationTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test.
        /// </summary>
        /// <param name="output">
        /// xUnit output sink.
        /// </param>
        public FaServerConversationTests(ITestOutputHelper output)
        {
            _output = output;
        }

        private const string DeleteCapture = "claude-delete-file-102-to-100-2026-07-29.pcapng";

        /// <summary>
        /// The conversation number the client used in the DELETE-FILE capture.
        /// </summary>
        private const ushort DeleteConversation = 0x0048;

        /// <summary>
        /// Frame offset at which the message body begins.
        /// </summary>
        private const int BodyOffsetFullHeader = 28;

        /// <summary>
        /// Rebuilds every reply the server sent and compares it with the captured bytes.
        /// </summary>
        [Fact]
        public void RebuiltReplies_AreByteIdenticalToTheCapturedReplies()
        {
            List<byte[]> serverBodies = ReadServerBodies();
            if (serverBodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            FaServerConversation server = new FaServerConversation(DeleteConversation);
            int rebuilt = 0;

            for (int i = 0; i < serverBodies.Count; i++)
            {
                byte[] captured = serverBodies[i];

                ushort messageType = (ushort)((captured[0] << 8) | captured[1]);
                if (messageType != FaExchangeCodec.MessageTypeRequest) { continue; }

                FaOperation operation;
                ushort sequence;
                Assert.True(FaExchangeCodec.TryReadOperation(captured, out operation, out sequence));

                int payloadOffset = FaExchangeCodec.QformOffset + 6;
                ReadOnlySpan<byte> payload = new ReadOnlySpan<byte>(
                    captured, payloadOffset, captured.Length - payloadOffset);

                byte[] reply = server.BuildReply(operation, sequence, payload);

                _output.WriteLine("reply " + (rebuilt + 1)
                    + ": operation=" + operation + " (0x" + ((ushort)operation).ToString("x4") + ")"
                    + " sequence=" + sequence
                    + " length=" + captured.Length);

                Assert.Equal(captured, reply);
                rebuilt++;
            }

            _output.WriteLine("rebuilt " + rebuilt + " replies, all byte-identical");
            Assert.Equal(3, rebuilt);
        }

        /// <summary>
        /// Rebuilds the server's short acknowledgements, connection confirmation and close.
        /// </summary>
        [Fact]
        public void RebuiltControlMessages_AreByteIdenticalToTheCapturedOnes()
        {
            List<byte[]> serverBodies = ReadServerBodies();
            if (serverBodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            FaServerConversation server = new FaServerConversation(DeleteConversation);

            List<byte[]> shortAcks = new List<byte[]>();
            byte[]? confirm = null;
            byte[]? close = null;

            for (int i = 0; i < serverBodies.Count; i++)
            {
                byte[] body = serverBodies[i];
                ushort messageType = (ushort)((body[0] << 8) | body[1]);

                if (messageType == FaExchangeCodec.MessageTypeShortAck) { shortAcks.Add(body); }
                else if (messageType == FaServerConversation.MessageTypeConnectionConfirm) { confirm = body; }
                else if (messageType == FaServerConversation.MessageTypeClose) { close = body; }
            }

            // The connection confirmation carries the CLIENT's conversation number and the system
            // number 100 - this is what proves the confirmation is matched to the right request.
            Assert.NotNull(confirm);
            _output.WriteLine("confirm: " + Hex(confirm!));
            Assert.Equal(confirm, server.BuildConnectionConfirm(100));

            Assert.NotNull(close);
            _output.WriteLine("close:   " + Hex(close!));
            Assert.Equal(close, server.BuildClose());

            Assert.Equal(3, shortAcks.Count);
            for (int i = 0; i < shortAcks.Count; i++)
            {
                byte[] expected = server.BuildShortAck((byte)(i + 1), true);
                _output.WriteLine("ack " + (i + 1) + ":   " + Hex(shortAcks[i]));
                Assert.Equal(shortAcks[i], expected);
            }
        }

        /// <summary>
        /// Guards the client/server asymmetry: the first reply must NOT use the client's
        /// first-exchange token.
        /// </summary>
        /// <remarks>
        /// If the server were built symmetric with the client it would send <c>0x0001</c> here. The
        /// capture says <c>0x9081</c> from the very first reply, so this pins the difference.
        /// </remarks>
        [Fact]
        public void FirstReply_UsesTheResponderTokenNotTheFirstExchangeToken()
        {
            FaServerConversation server = new FaServerConversation(DeleteConversation);
            byte[] reply = server.BuildReply(FaOperation.ReserveFileEntry, 1, new byte[] { 0xF2, 0x00, 0xFF });

            ushort token = (ushort)((reply[FaExchangeCodec.SessionHeaderOffset + 2] << 8)
                | reply[FaExchangeCodec.SessionHeaderOffset + 3]);

            Assert.Equal(FaExchangeCodec.SessionTokenResponder, token);
            Assert.NotEqual(FaExchangeCodec.SessionTokenFirst, token);

            // And it answers with its own conversation number, not the client's.
            ushort conversation = (ushort)((reply[FaExchangeCodec.ConversationOffset] << 8)
                | reply[FaExchangeCodec.ConversationOffset + 1]);
            Assert.Equal(FaExchangeCodec.ResponderConversation, conversation);
            Assert.NotEqual(DeleteConversation, conversation);
        }

        /// <summary>
        /// Formats bytes as spaced hex.
        /// </summary>
        /// <param name="data">
        /// The bytes to format.
        /// </param>
        private static string Hex(byte[] data)
        {
            System.Text.StringBuilder text = new System.Text.StringBuilder(data.Length * 3);
            for (int i = 0; i < data.Length; i++)
            {
                text.Append(data[i].ToString("x2"));
                if ((i % 2) == 1) { text.Append(' '); }
            }

            return text.ToString();
        }

        /// <summary>
        /// Reads the bodies node 100 sent, in wire order.
        /// </summary>
        private static List<byte[]> ReadServerBodies()
        {
            List<byte[]> bodies = new List<byte[]>();

            string? path = PcapFiles.File(DeleteCapture);
            if (path == null) { return bodies; }

            IReadOnlyList<LapbFrame> frames = HdlcPcap.ReadFramesInCaptureOrder(path);
            for (int i = 0; i < frames.Count; i++)
            {
                LapbFrame frame = frames[i];
                if (frame.Kind != LapbFrameKind.Information || frame.Info.Length <= BodyOffsetFullHeader)
                {
                    continue;
                }

                ReadOnlySpan<byte> info = frame.Info.Span;
                if (info[3] != (byte)SintranPacketSubtype.Data) { continue; }

                // Node 100 is the listener, so its frames are the ones leaving port 10362.
                if (frame.Key.SourcePort != 10362) { continue; }

                bodies.Add(info.Slice(BodyOffsetFullHeader).ToArray());
            }

            return bodies;
        }

    }
}
