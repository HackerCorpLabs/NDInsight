using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Packet;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Generator-conformance gate: proves the <see cref="XmsgEnvelope"/> closed-form model
    /// REPRODUCES the sub-header Counter and the sub-protocol Channel of EVERY captured SINTRAN
    /// Data frame - i.e. what our responder emits is byte-correct, not merely what our decoder
    /// reads back.
    /// </summary>
    /// <remarks>
    /// <para><b>Why this exists in addition to <see cref="PcapDecodeTests"/>.</b></para>
    /// The decode test parses bytes and re-serialises them - it proves the READER round-trips, but
    /// it never recomputes the Counter/Channel, so a wrong generator model would still pass it. This
    /// test drives the GENERATOR path: for every Data frame it learns the per-frame seed from
    /// (Flags1, Counter, Flags2) via <see cref="XmsgEnvelope.LearnSeed"/>, then asserts
    /// <see cref="XmsgEnvelope.DeriveChannel(byte, ushort, ushort, uint)"/> predicts the SAME channel
    /// the capture carries. Since the channel derivation runs Counter -> seed -> epoch -> channel,
    /// this is an independent cross-check of the whole envelope arithmetic, per frame.
    /// </para>
    /// <para>
    /// Coverage includes the climbed-reconnect capture (epoch-1 letters on <c>0xD9</c> and terminal
    /// data on <c>0xDC</c>), the exact epoch-1 sequencing our responder was getting wrong live. The
    /// same portability policy as the other pcap tests applies: when the corpus is absent the test
    /// passes with a log line rather than failing.
    /// </para>
    /// <para>
    /// Only subtype <c>Data</c> frames follow this model. Routing ACK frames (subtype <c>Ack</c>,
    /// channel <c>0xDE</c>) carry their own trailing-byte scheme and are excluded, as is the
    /// reachability broadcast (Flags1 <c>0xFFFF</c>).
    /// </para>
    /// </remarks>
    public sealed class EnvelopeConformanceTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test with the xUnit output sink.
        /// </summary>
        /// <param name="output">
        /// The per-test output helper used for the per-capture summary.
        /// </param>
        public EnvelopeConformanceTests(ITestOutputHelper output)
        {
            _output = output;
        }

        /// <summary>
        /// Asserts the envelope model reproduces the Channel of every SINTRAN Data frame in every
        /// capture, from that frame's own learned seed, Flags1, Flags2 and XMCSM.
        /// </summary>
        [Fact]
        public void AllCaptures_EnvelopeModel_ReproducesEveryDataFrameChannel()
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

            int totalChecked = 0;
            int totalMismatch = 0;
            StringBuilder mismatches = new StringBuilder();

            for (int f = 0; f < files.Length; f++)
            {
                string file = files[f];
                string name = Path.GetFileName(file);

                IReadOnlyList<LapbFrame> frames = HdlcPcap.ReadFrames(file);
                int checkedHere = 0;

                for (int i = 0; i < frames.Count; i++)
                {
                    LapbFrame frame = frames[i];
                    if (frame.Kind != LapbFrameKind.Information || !frame.IsSintranInfo)
                    {
                        continue;
                    }

                    ReadOnlyMemory<byte> info = frame.Info;
                    if (info.Length < SintranHeader.Size)
                    {
                        continue;
                    }

                    XmsgFrame decoded = XmsgFrame.Parse(info.Span);

                    // Only subtype Data frames obey the seed model. Routing ACKs (subtype Ack) use a
                    // different trailing-byte scheme, and the 0xFFFF reachability marker is not a real
                    // datagram sequence.
                    if (decoded.Header.Subtype != SintranPacketSubtype.Data
                        || decoded.SubHeader == null
                        || decoded.Header.Flags1 == 0xFFFF)
                    {
                        continue;
                    }

                    ushort flags1 = decoded.Header.Flags1;
                    ushort flags2 = decoded.Header.Flags2;
                    byte counter = decoded.SubHeader.Counter;
                    uint xmcsm = decoded.SubHeader.ControlService;
                    SintranProtocolId actualChannel = decoded.Header.ProtocolId;

                    // Learn the seed from THIS frame, then predict the channel back from it. If the
                    // closed form holds, the prediction equals the channel actually on the wire.
                    byte seed = XmsgEnvelope.LearnSeed(flags1, counter, flags2);
                    SintranProtocolId predicted = XmsgEnvelope.DeriveChannel(seed, flags1, flags2, xmcsm);

                    // Sanity: the Counter recomputed from the learned seed must equal the wire Counter
                    // (LearnSeed is its inverse, so this guards against an arithmetic regression).
                    byte recomputedCounter = XmsgEnvelope.ComputeCounter(seed, flags1, flags2);

                    totalChecked++;
                    checkedHere++;

                    if (predicted != actualChannel || recomputedCounter != counter)
                    {
                        totalMismatch++;
                        if (totalMismatch <= 20)
                        {
                            mismatches.Append(name)
                                .Append("  F1=0x").Append(flags1.ToString("X4"))
                                .Append(" F2=0x").Append(flags2.ToString("X4"))
                                .Append(" XMCSM=0x").Append(xmcsm.ToString("X8"))
                                .Append(" seed=0x").Append(seed.ToString("X2"))
                                .Append("  wire ch=0x").Append(((byte)actualChannel).ToString("X2"))
                                .Append(" ctr=0x").Append(counter.ToString("X2"))
                                .Append("  predicted ch=0x").Append(((byte)predicted).ToString("X2"))
                                .Append(" ctr=0x").Append(recomputedCounter.ToString("X2"))
                                .Append('\n');
                        }
                    }
                }

                _output.WriteLine($"{name}: Data frames checked={checkedHere}");
            }

            _output.WriteLine($"TOTAL Data frames checked={totalChecked}  mismatches={totalMismatch}");
            if (totalMismatch > 0)
            {
                _output.WriteLine("First mismatches:\n" + mismatches.ToString());
            }

            Assert.True(totalChecked > 0, "Expected to check at least one SINTRAN Data frame.");
            Assert.Equal(0, totalMismatch);
        }

        /// <summary>
        /// Resolves the pcap capture directory (env var override, else a sibling X25Emulator/pcap).
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
    }
}
