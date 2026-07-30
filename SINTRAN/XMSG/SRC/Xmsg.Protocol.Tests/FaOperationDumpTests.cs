using System;
using System.Collections.Generic;
using System.IO;
using NDInsight.Sintran.Xmsg.Hdlc;
using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Prints the complete message body of every file-server exchange, in the order the frames
    /// completed on the wire.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Reporting only - it makes no claims. It exists because the earlier dump helper printed only
    /// the first 48 bytes of each information field and grouped frames by flow, which is enough to
    /// spot a message type but not enough to write a codec against: the tail of every request was
    /// invisible, and request and reply never appeared next to each other.
    /// </para>
    /// <para>
    /// Bodies start at offset 28 - see <c>TransferFragmentationTests</c> for how that offset was
    /// established - and the first two words of every body are the message type and the conversation
    /// number.
    /// </para>
    /// </remarks>
    public sealed class FaOperationDumpTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test.
        /// </summary>
        /// <param name="output">
        /// xUnit output sink.
        /// </param>
        public FaOperationDumpTests(ITestOutputHelper output)
        {
            _output = output;
        }

        /// <summary>
        /// Frame offset at which the message body begins on a fully addressed frame.
        /// </summary>
        private const int BodyOffsetFullHeader = 28;

        /// <summary>
        /// Dumps every data-frame body in a capture, with direction and an ASCII column.
        /// </summary>
        /// <param name="captureName">
        /// The capture to dump.
        /// </param>
        [Theory]
        [InlineData("claude-file-stat-102-to-100-2026-07-29.pcapng")]
        [InlineData("claude-delete-file-102-to-100-2026-07-29.pcapng")]
        public void DumpEveryMessageBody(string captureName)
        {
            string? path = LocateCapture(captureName);
            if (path == null)
            {
                _output.WriteLine("capture not found; skipping (set XMSG_PCAP_DIR).");
                return;
            }

            _output.WriteLine("=== " + captureName);

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

                ReadOnlySpan<byte> body = info.Slice(BodyOffsetFullHeader);
                string dir = frame.Key.SourcePort == 45164 ? "102->100" : "100->102";

                _output.WriteLine(dir + "  len=" + body.Length);
                _output.WriteLine("   hex: " + Hex(body));
                _output.WriteLine("   txt: " + Ascii(body));
            }

            Assert.True(frames.Count > 0);
        }

        /// <summary>
        /// Formats bytes as spaced hex words.
        /// </summary>
        /// <param name="data">
        /// The bytes to format.
        /// </param>
        private static string Hex(ReadOnlySpan<byte> data)
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
        /// Renders printable bytes, replacing the rest with a dot.
        /// </summary>
        /// <param name="data">
        /// The bytes to render.
        /// </param>
        private static string Ascii(ReadOnlySpan<byte> data)
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
        /// Finds a capture by name, or null when the corpus is not present.
        /// </summary>
        /// <param name="captureName">
        /// The capture file name.
        /// </param>
        private static string? LocateCapture(string captureName)
        {
            string? fromEnv = Environment.GetEnvironmentVariable("XMSG_PCAP_DIR");
            if (!string.IsNullOrEmpty(fromEnv))
            {
                string direct = Path.Combine(fromEnv!, captureName);
                if (File.Exists(direct)) { return direct; }
            }

            DirectoryInfo? dir = new DirectoryInfo(AppContext.BaseDirectory);
            while (dir != null)
            {
                string candidate = Path.Combine(dir.FullName, "X25Emulator", "pcap", captureName);
                if (File.Exists(candidate)) { return candidate; }
                dir = dir.Parent;
            }

            return null;
        }
    }
}
