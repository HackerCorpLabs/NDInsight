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
    /// Verifies the shared file-server envelope against the FILE-STATISTICS and DELETE-FILE captures.
    /// </summary>
    public sealed class FaExchangeCodecTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test.
        /// </summary>
        /// <param name="output">
        /// xUnit output sink.
        /// </param>
        public FaExchangeCodecTests(ITestOutputHelper output)
        {
            _output = output;
        }

        private const string FileStatCapture = "claude-file-stat-102-to-100-2026-07-29.pcapng";
        private const string DeleteCapture = "claude-delete-file-102-to-100-2026-07-29.pcapng";

        /// <summary>
        /// Frame offset at which the message body begins.
        /// </summary>
        private const int BodyOffsetFullHeader = 28;

        /// <summary>
        /// The central claim: the two operations open with byte-identical requests apart from the
        /// conversation number, so the operation is NOT named in the opening exchange.
        /// </summary>
        /// <remarks>
        /// If this ever fails, the "conversation number, not opcode" conclusion is wrong and every
        /// codec resting on it needs revisiting - which is exactly why it is asserted rather than
        /// merely written down.
        /// </remarks>
        [Fact]
        public void FileStatAndDelete_OpenWithIdenticalRequestsApartFromTheConversationNumber()
        {
            List<byte[]> statBodies = ReadBodies(FileStatCapture);
            List<byte[]> deleteBodies = ReadBodies(DeleteCapture);

            if (statBodies.Count == 0 || deleteBodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            byte[] statOpen = FindOpeningRequest(statBodies);
            byte[] deleteOpen = FindOpeningRequest(deleteBodies);

            Assert.NotNull(statOpen);
            Assert.NotNull(deleteOpen);
            Assert.Equal(statOpen.Length, deleteOpen.Length);

            // Compare every byte except the conversation number at offsets 2 and 3.
            int differences = 0;
            for (int i = 0; i < statOpen.Length; i++)
            {
                if (i == FaExchangeCodec.ConversationOffset || i == FaExchangeCodec.ConversationOffset + 1)
                {
                    continue;
                }

                if (statOpen[i] != deleteOpen[i]) { differences++; }
            }

            _output.WriteLine("opening request length " + statOpen.Length
                + "; differences outside the conversation number: " + differences);

            Assert.Equal(0, differences);

            // And the conversation numbers really are different, so the test is not vacuous.
            ushort statConversation = (ushort)((statOpen[2] << 8) | statOpen[3]);
            ushort deleteConversation = (ushort)((deleteOpen[2] << 8) | deleteOpen[3]);
            _output.WriteLine("conversations: FILE-STAT 0x" + statConversation.ToString("x4")
                + ", DELETE 0x" + deleteConversation.ToString("x4"));
            Assert.NotEqual(statConversation, deleteConversation);
        }

        /// <summary>
        /// Checks that the operation code and exchange sequence read correctly, and that DELETE-FILE
        /// is distinguished by the operation code of its SECOND exchange.
        /// </summary>
        [Fact]
        public void DeleteConversation_IsDistinguishedByItsSecondExchange()
        {
            List<byte[]> bodies = ReadBodies(DeleteCapture);
            if (bodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            List<ushort> requestOperations = new List<ushort>();

            for (int i = 0; i < bodies.Count; i++)
            {
                byte[] body = bodies[i];

                ushort messageType;
                ushort conversation;
                byte sequenceByte;
                ushort token;
                if (!FaExchangeCodec.TryReadEnvelope(body, out messageType, out conversation, out sequenceByte, out token))
                {
                    continue;
                }

                if (messageType != FaExchangeCodec.MessageTypeRequest) { continue; }
                if (FaExchangeCodec.IsReply(body)) { continue; }

                ushort operation;
                ushort sequence;
                if (!FaExchangeCodec.TryReadOperation(body, out operation, out sequence)) { continue; }

                _output.WriteLine("request: operation=0x" + operation.ToString("x4")
                    + " sequence=" + sequence
                    + " sessionByte=0x" + sequenceByte.ToString("x2")
                    + " token=0x" + token.ToString("x4"));

                requestOperations.Add(operation);
            }

            Assert.Equal(3, requestOperations.Count);
            Assert.Equal(FaExchangeCodec.OperationOpenSpec, requestOperations[0]);
            Assert.Equal(FaExchangeCodec.OperationDelete, requestOperations[1]);
            Assert.Equal(FaExchangeCodec.OperationClose, requestOperations[2]);
        }

        /// <summary>
        /// Checks that the exchange sequence counts from one and that the session token switches from
        /// the first-exchange value to the steady-state value.
        /// </summary>
        [Fact]
        public void SessionToken_SwitchesAfterTheFirstExchange()
        {
            List<byte[]> bodies = ReadBodies(DeleteCapture);
            if (bodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            bool sawFirst = false;

            for (int i = 0; i < bodies.Count; i++)
            {
                byte[] body = bodies[i];

                ushort messageType;
                ushort conversation;
                byte sequenceByte;
                ushort token;
                if (!FaExchangeCodec.TryReadEnvelope(body, out messageType, out conversation, out sequenceByte, out token))
                {
                    continue;
                }

                if (messageType != FaExchangeCodec.MessageTypeRequest) { continue; }
                if (FaExchangeCodec.IsReply(body)) { continue; }

                if (sequenceByte == 0x80)
                {
                    Assert.Equal(FaExchangeCodec.SessionTokenFirst, token);
                    sawFirst = true;
                }
                else
                {
                    Assert.Equal(FaExchangeCodec.SessionTokenAsker, token);
                }
            }

            Assert.True(sawFirst, "No opening exchange was found.");
        }

        /// <summary>
        /// Finds the opening request: the first non-reply request whose session byte is 0x80.
        /// </summary>
        /// <param name="bodies">
        /// The message bodies from a capture.
        /// </param>
        /// <returns>
        /// The opening request body, or null.
        /// </returns>
        private static byte[] FindOpeningRequest(List<byte[]> bodies)
        {
            for (int i = 0; i < bodies.Count; i++)
            {
                byte[] body = bodies[i];

                ushort messageType;
                ushort conversation;
                byte sequenceByte;
                ushort token;
                if (!FaExchangeCodec.TryReadEnvelope(body, out messageType, out conversation, out sequenceByte, out token))
                {
                    continue;
                }

                if (messageType == FaExchangeCodec.MessageTypeRequest
                    && !FaExchangeCodec.IsReply(body)
                    && sequenceByte == 0x80)
                {
                    return body;
                }
            }

            return null!;
        }

        /// <summary>
        /// Reads every data-frame body from a capture, in wire order.
        /// </summary>
        /// <param name="captureName">
        /// The capture file name.
        /// </param>
        private static List<byte[]> ReadBodies(string captureName)
        {
            List<byte[]> bodies = new List<byte[]>();

            string? path = PcapFiles.File(captureName);
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

                bodies.Add(info.Slice(BodyOffsetFullHeader).ToArray());
            }

            return bodies;
        }

    }
}
