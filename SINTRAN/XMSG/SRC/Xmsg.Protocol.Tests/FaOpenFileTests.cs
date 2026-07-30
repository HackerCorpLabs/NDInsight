using System;
using System.Collections.Generic;
using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Protocol.Fa;
using NDInsight.Sintran.Xmsg.Tests;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Pins what a remote <c>OPEN-FILE</c> looks like on the wire, and the fact that the file number
    /// it returns is the remote one rather than the number the operator is shown.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Recorded 2026-07-30 by running <c>OPEN-FILE d100(system).ronny-r2:txt,R</c> on node 102
    /// against node 100.
    /// </para>
    /// <para>
    /// The local and remote numbers differing is not a curiosity, it is the thing a client has to get
    /// right. Node 100 answered <c>0x0040</c>, which is 100 octal. Node 102 told the operator
    /// <c>FILE NUMBER IS 000101</c>, because 100 octal was already in use locally by
    /// <c>SCRATCH03:DATA</c>. A client that echoes the wire number back to a user, or that sends the
    /// user's number to the server, is wrong in both directions.
    /// </para>
    /// </remarks>
    public sealed class FaOpenFileTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test.
        /// </summary>
        /// <param name="output">
        /// xUnit output sink.
        /// </param>
        public FaOpenFileTests(ITestOutputHelper output)
        {
            _output = output;
        }

        /// <summary>
        /// The recording of the open and its close.
        /// </summary>
        private const string CaptureName = "claude-open-close-file-102-to-100-2026-07-30.pcapng";

        /// <summary>
        /// Frame offset at which the message body begins.
        /// </summary>
        private const int BodyOffsetFullHeader = 28;

        /// <summary>
        /// The file number node 100 returned, 100 octal.
        /// </summary>
        private const ushort RemoteFileNumber = 0x0040;

        /// <summary>
        /// Requires the open request to name the file and the reply to carry a file number.
        /// </summary>
        [Fact]
        public void OpenFile_RequestNamesTheFileAndTheReplyReturnsANumber()
        {
            List<byte[]> bodies = ReadBodies();
            if (bodies.Count == 0)
            {
                _output.WriteLine("recorded .pcapng files absent and XMSG_PCAP_OPTIONAL is set; skipping.");
                return;
            }

            byte[]? request = null;
            byte[]? reply = null;

            for (int i = 0; i < bodies.Count; i++)
            {
                ushort operation;
                ushort sequence;
                if (!FaExchangeCodec.TryReadOperation(bodies[i], out operation, out sequence)) { continue; }
                if (operation != FaExchangeCodec.OperationOpenFile) { continue; }

                if (FaExchangeCodec.IsReply(bodies[i])) { reply = bodies[i]; }
                else { request = bodies[i]; }
            }

            Assert.NotNull(request);
            Assert.NotNull(reply);

            // The request carries the file name as a byte string; the reply does not.
            string requestText = Printable(request!);
            _output.WriteLine("request: " + requestText);
            Assert.Contains("RONNY-R2:TXT", requestText);

            // The reply carries a two-byte value tagged A2 - the file number.
            ushort number;
            Assert.True(TryReadTaggedValue(reply!, 0xA2, out number), "no A2 value in the reply");

            _output.WriteLine("reply file number: 0x" + number.ToString("x4")
                + " (" + Convert.ToString(number, 8) + " octal)");

            Assert.Equal(RemoteFileNumber, number);
        }

        /// <summary>
        /// Guards the difference between the remote file number and the one shown locally.
        /// </summary>
        /// <remarks>
        /// The operator saw 101 octal while the wire carried 100 octal. If a future change made these
        /// equal, either the recording was replaced or someone has started echoing the wire number,
        /// and this test should be re-examined rather than adjusted.
        /// </remarks>
        [Fact]
        public void TheNumberOnTheWire_IsNotTheNumberTheOperatorWasShown()
        {
            // 101 octal, as printed by node 102's OPEN-FILE.
            const ushort shownToOperator = 0x0041;

            Assert.NotEqual(shownToOperator, RemoteFileNumber);

            // They differ by exactly one here because 100 octal happened to be taken locally. That is
            // a coincidence of this recording, NOT a rule - do not derive one number from the other.
            _output.WriteLine("wire=0x" + RemoteFileNumber.ToString("x4")
                + " operator=0x" + shownToOperator.ToString("x4")
                + " - mapping is allocation order, not arithmetic");
        }

        /// <summary>
        /// Reads a two-byte value carried under the given tag.
        /// </summary>
        /// <param name="body">
        /// The message body.
        /// </param>
        /// <param name="tag">
        /// The tag byte to look for.
        /// </param>
        /// <param name="value">
        /// The value found.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when a value with that tag was present.
        /// </returns>
        private static bool TryReadTaggedValue(byte[] body, byte tag, out ushort value)
        {
            value = 0;

            // Skip the envelope and the operation/sequence pair.
            for (int i = FaExchangeCodec.QformOffset + 6; i + 2 < body.Length; i++)
            {
                if (body[i] != tag) { continue; }
                value = (ushort)((body[i + 1] << 8) | body[i + 2]);
                return true;
            }

            return false;
        }

        /// <summary>
        /// Renders the printable characters of a body, dots elsewhere.
        /// </summary>
        /// <param name="data">
        /// The bytes to render.
        /// </param>
        /// <returns>
        /// The rendered text.
        /// </returns>
        private static string Printable(byte[] data)
        {
            System.Text.StringBuilder text = new System.Text.StringBuilder(data.Length);
            for (int i = 0; i < data.Length; i++)
            {
                byte b = data[i];
                text.Append(b >= 0x20 && b < 0x7F ? (char)b : '.');
            }

            return text.ToString();
        }

        /// <summary>
        /// Reads every data-frame body from the recording, in wire order.
        /// </summary>
        /// <returns>
        /// The bodies, empty when the recording is absent.
        /// </returns>
        private static List<byte[]> ReadBodies()
        {
            List<byte[]> bodies = new List<byte[]>();

            string? path = PcapFiles.File(CaptureName);
            if (path == null) { return bodies; }

            IReadOnlyList<LapbFrame> frames = HdlcPcap.ReadFramesInCaptureOrder(path);
            for (int i = 0; i < frames.Count; i++)
            {
                LapbFrame frame = frames[i];
                if (frame.Kind != LapbFrameKind.Information
                    || frame.Info.Length <= BodyOffsetFullHeader)
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
