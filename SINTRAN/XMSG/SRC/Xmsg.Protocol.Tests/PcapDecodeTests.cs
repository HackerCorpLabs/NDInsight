using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Diagnostics;
using NDInsight.Sintran.Xmsg.Hdlc;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// End-to-end test that de-frames every capture, decodes each SINTRAN information
    /// frame, and proves the decode/re-encode and JSON round-trips are byte-identical.
    /// </summary>
    /// <remarks>
    /// The captures live in the sibling <c>X25Emulator/pcap</c> repository. When they
    /// cannot be located the test passes with a clear log message rather than failing, so
    /// it is portable to machines without the corpus.
    /// </remarks>
    public sealed class PcapDecodeTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test with the xUnit output sink.
        /// </summary>
        /// <param name="output">
        /// The per-test output helper used for the summary log.
        /// </param>
        public PcapDecodeTests(ITestOutputHelper output)
        {
            _output = output;
        }

        /// <summary>
        /// Decodes every SINTRAN I-frame in every capture and asserts byte-identical
        /// round-trips for both direct re-serialisation and JSON reconstruction.
        /// </summary>
        [Fact]
        public void AllCaptures_DecodeAndRoundTrip_ByteIdentical()
        {
            string? pcapDir = LocatePcapDirectory();
            if (pcapDir == null)
            {
                _output.WriteLine("pcap directory not found (set XMSG_PCAP_DIR or place X25Emulator next to NDInsight); skipping.");
                return;
            }

            string[] files = Directory.GetFiles(pcapDir, "*.pcapng");
            Array.Sort(files, StringComparer.OrdinalIgnoreCase);
            Assert.NotEmpty(files);

            StringBuilder report = new StringBuilder();
            report.Append("XMSG pcap decode report\n");
            report.Append("Source: ").Append(pcapDir).Append('\n');
            report.Append("Generated: ").Append(DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")).Append('\n');
            report.Append("================================================================\n\n");

            int grandFcsValid = 0;
            int grandSintran = 0;
            int grandRoundTripOk = 0;

            for (int f = 0; f < files.Length; f++)
            {
                string file = files[f];
                string name = Path.GetFileName(file);

                IReadOnlyList<LapbFrame> frames = HdlcPcap.ReadFrames(file);

                int fcsValid = frames.Count;
                int sintran = 0;
                int roundTripOk = 0;

                report.Append("### ").Append(name).Append('\n');

                for (int i = 0; i < frames.Count; i++)
                {
                    LapbFrame frame = frames[i];
                    if (frame.Kind != LapbFrameKind.Information)
                    {
                        continue;
                    }

                    if (!frame.IsSintranInfo)
                    {
                        continue;
                    }

                    ReadOnlyMemory<byte> info = frame.Info;
                    if (info.Length < SintranHeader.Size)
                    {
                        // Marker present but too short for a full SINTRAN header - skip.
                        continue;
                    }

                    sintran++;

                    byte[] original = info.ToArray();
                    XmsgFrame decoded = XmsgFrame.Parse(info.Span);

                    // (a) direct decode -> re-serialise must be byte-identical.
                    byte[] reserialized = decoded.ToArray();
                    Assert.Equal(original, reserialized);

                    // (b) JSON round-trip must also re-serialise byte-identically.
                    string json = XmsgJson.ToJson(decoded);
                    XmsgFrame fromJson = XmsgJson.FromJson(json);
                    byte[] jsonReserialized = fromJson.ToArray();
                    Assert.Equal(original, jsonReserialized);

                    roundTripOk++;

                    // Record the first few frames per file in the human-readable report.
                    if (sintran <= 8)
                    {
                        report.Append("--- frame ").Append(sintran)
                              .Append("  N(S)=").Append(frame.SendSequence)
                              .Append(" N(R)=").Append(frame.ReceiveSequence)
                              .Append("  ").Append(frame.Key.ToString()).Append('\n');
                        report.Append(XmsgDump.ToText(decoded));
                        report.Append('\n');
                    }
                }

                report.Append("summary: FCS-valid=").Append(fcsValid)
                      .Append("  SINTRAN I-frames=").Append(sintran)
                      .Append("  round-trip OK=").Append(roundTripOk).Append('\n');
                report.Append("----------------------------------------------------------------\n\n");

                _output.WriteLine(
                    $"{name}: FCS-valid={fcsValid}  SINTRAN I-frames={sintran}  round-trip OK={roundTripOk}");

                grandFcsValid += fcsValid;
                grandSintran += sintran;
                grandRoundTripOk += roundTripOk;
            }

            report.Append("================================================================\n");
            report.Append("TOTAL FCS-valid frames  : ").Append(grandFcsValid).Append('\n');
            report.Append("TOTAL SINTRAN I-frames  : ").Append(grandSintran).Append('\n');
            report.Append("TOTAL round-trip OK     : ").Append(grandRoundTripOk).Append('\n');

            // Write the eyeball report next to the solution.
            string? reportPath = LocateReportPath();
            if (reportPath != null)
            {
                File.WriteAllText(reportPath, report.ToString());
                _output.WriteLine("Report written to: " + reportPath);
            }

            _output.WriteLine(
                $"TOTAL: FCS-valid={grandFcsValid}  SINTRAN I-frames={grandSintran}  round-trip OK={grandRoundTripOk}");

            // Every decoded SINTRAN frame must have round-tripped.
            Assert.Equal(grandSintran, grandRoundTripOk);

            // The corpus must actually have decoded real frames.
            Assert.True(grandSintran > 0, "Expected to decode at least one SINTRAN I-frame across the captures.");
        }

        /// <summary>
        /// Resolves the pcap capture directory.
        /// </summary>
        /// <returns>
        /// The directory path, or <c>null</c> when the captures cannot be located.
        /// </returns>
        private static string? LocatePcapDirectory()
        {
            string? fromEnv = Environment.GetEnvironmentVariable("XMSG_PCAP_DIR");
            if (!string.IsNullOrEmpty(fromEnv) && Directory.Exists(fromEnv))
            {
                return fromEnv;
            }

            // Walk up from the test assembly looking for a sibling X25Emulator/pcap.
            DirectoryInfo? dir = new DirectoryInfo(AppContext.BaseDirectory);
            while (dir != null)
            {
                string candidate = Path.Combine(dir.FullName, "X25Emulator", "pcap");
                if (Directory.Exists(candidate))
                {
                    return candidate;
                }

                dir = dir.Parent;
            }

            return null;
        }

        /// <summary>
        /// Resolves the path of the eyeball report file next to the solution.
        /// </summary>
        /// <returns>
        /// The report file path, or <c>null</c> when the solution folder cannot be found.
        /// </returns>
        private static string? LocateReportPath()
        {
            DirectoryInfo? dir = new DirectoryInfo(AppContext.BaseDirectory);
            while (dir != null)
            {
                string slnx = Path.Combine(dir.FullName, "Xmsg.Protocol.slnx");
                if (File.Exists(slnx))
                {
                    return Path.Combine(dir.FullName, "pcap-decode-report.txt");
                }

                dir = dir.Parent;
            }

            return null;
        }
    }
}
