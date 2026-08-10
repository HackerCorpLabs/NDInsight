using NDInsight.Sintran.Xmsg.Node.Tad;
using NDInsight.Sintran.Xmsg.Packet;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Node.Tests
{
    /// <summary>
    /// Asks whether a TAD-built frame carries a REAL header word 6.
    /// </summary>
    /// <remarks>
    /// <para><b>Why this exists</b></para>
    /// Word 6 is a ones-complement checksum over words 0-5, carved from the kernel and confirmed on
    /// 3595/3595 captured frames. A fabricated one kills D100 with <c>XMSG ERROR CODE 24</c>, and
    /// getting that wrong on the file-access path cost most of a day.
    /// <para>
    /// <c>TadSession.AssembleDataFrame</c> does not compute it. It sets
    /// <c>Header.ProtocolId</c> and <c>Header.Counter</c> from <see cref="TadFrameContext"/>, and
    /// those two properties are compatibility views over the checksum's HIGH and LOW bytes. So a TAD
    /// frame's word 6 is whatever the caller put in the context.
    /// </para>
    /// <para><b>What this test is for</b></para>
    /// To say plainly which it is, rather than leaving it as a suspicion in a comment. If the
    /// assertion below holds, TAD computes a real checksum somewhere and there is nothing to fix. If
    /// it fails, TAD ships a fabricated word 6 and works today only because nodes 100 and 103 are
    /// small numbers - exactly how the same defect hid on the file-access path until node 19999.
    /// </remarks>
    public sealed class TadHeaderChecksumTests
    {
        private readonly ITestOutputHelper _output;

        /// <summary>
        /// Initialises the test.
        /// </summary>
        /// <param name="output">
        /// xUnit output sink.
        /// </param>
        public TadHeaderChecksumTests(ITestOutputHelper output)
        {
            _output = output;
        }

        /// <summary>
        /// A frame built by <see cref="TadSession"/> carries the carved checksum in word 6.
        /// </summary>
        /// <remarks>
        /// The context deliberately carries a <c>ProtocolId</c> and <c>Counter</c> that are NOT the
        /// checksum, which is what a caller does today. Large node numbers are used because that is
        /// where a fabricated value and a real one diverge: with both nodes under 256 the sum's high
        /// half contributes nothing and the two can agree by accident.
        /// </remarks>
        [Fact]
        public void ATadBuiltFrameCarriesTheCarvedHeaderChecksum()
        {
            TadSession session = new TadSession(TadSessionRole.Server);

            TadFrameContext context = new TadFrameContext
            {
                DestinationNode = 19999,
                SourceNode = 103,
                DatagramSequence = 0x0042,
                FrameClass = 0x000E,
                ProtocolId = SintranProtocolId.Tad,
                Counter = 0x55,
                FrameFlags = 0x82,
                Role = 0x84,
                DestinationSystem = 100,
                DestinationPort = 0x044D,
                SourceSystem = 103,
                SourcePort = 0x0211,
                ControlService = 0x00080000,
            };

            XmsgFrame frame = session.BuildControlFrame(context, TadOp.Bdat, new byte[] { 0x41, 0x42 });

            SintranHeader header = frame.Header!;
            ushort carved = XmsgEnvelope.ComputeHeaderChecksum(
                (ushort)((header.Marker1 << 8) | header.Marker2),
                (ushort)((header.PacketType << 8) | (byte)header.Subtype),
                header.DestinationNode,
                header.SourceNode,
                header.Flags1,
                header.Flags2);

            _output.WriteLine($"word 6 on the frame : 0x{header.Checksum:X4}");
            _output.WriteLine($"carved checksum     : 0x{carved:X4}");

            Assert.Equal(carved, header.Checksum);
        }
    }
}
