using NDInsight.Sintran.Xmsg.Packet;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Locks in the carved definition of SINTRAN header word 6: a ones-complement checksum over the
    /// other six header words.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Carved 2026-07-31 from the XMSG kernel routine at <c>137314</c> and verified against every
    /// FCS-valid frame in <c>E:\Dev\Ronny\X25Emulator\pcap</c> - <b>3595 of 3595</b>, across all
    /// eight observed subtypes, both directions and every link, with no special cases.
    /// </para>
    /// <para>
    /// The vectors below are real frames lifted from those captures, chosen to cover one of each
    /// subtype including the two that no earlier model addressed at all. Each row is the six header
    /// words followed by the word 6 that actually appeared on the wire.
    /// </para>
    /// <para>
    /// Why this matters beyond arithmetic: word 6's HIGH byte is what the codebase has historically
    /// called the "Protocol ID" or "channel" and its LOW byte is the "Counter". Neither is a free
    /// choice, and a peer that receives a wrong word 6 is receiving a corrupt header - which is the
    /// documented XMSG 24B / PERF_CONNCT crash. Doc:
    /// <c>SINTRAN/XMSG/DOC/XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md</c>.
    /// </para>
    /// </remarks>
    public class HeaderChecksumTests
    {
        /// <summary>
        /// Real captured frames: the checksum reproduces the wire's word 6 exactly.
        /// </summary>
        /// <param name="markers">Word 0 - the two marker bytes.</param>
        /// <param name="typeAndSubtype">Word 1 - packet type and subtype.</param>
        /// <param name="destinationNode">Word 2 - destination node.</param>
        /// <param name="sourceNode">Word 3 - source node.</param>
        /// <param name="flags1">Word 4 - the datagram sequence.</param>
        /// <param name="flags2">Word 5 - equal to the 16-bit XMCSM.</param>
        /// <param name="expectedWord6">The word 6 observed on the wire.</param>
        [Theory]
        // Data (0x0E) - the APPEND-REMOTE-BATCH request, 102 -> 100.
        [InlineData(0x2113, 0x000E, 0x0064, 0x0066, 0x03D3, 0x0080, 0xD9C1)]
        // Ack (0x03) - the acknowledgement of that same frame, 100 -> 102.
        [InlineData(0x2113, 0x0003, 0x0066, 0x0064, 0x03D3, 0x0001, 0xDA4B)]
        // Bulk file-transfer data (0x0A), 100 -> 102. No earlier model covered these.
        [InlineData(0x2113, 0x000A, 0x0066, 0x0064, 0x01A1, 0x0406, 0xD871)]
        // Bulk file-transfer data (0x0C), 100 -> 102. Likewise uncovered before.
        [InlineData(0x2113, 0x000C, 0x0066, 0x0064, 0x01A1, 0x0252, 0xDA23)]
        // The 0xFD/0xFE family - note Marker2 is 0xFE here, NOT the usual 0x13, and the checksum
        // still holds. That is a useful independent check: the rule does not depend on the markers
        // taking their normal values.
        [InlineData(0x21FE, 0x0017, 0x0066, 0x0067, 0xFFFF, 0xFFFD, 0xDD1F)]
        [InlineData(0x21FD, 0x0017, 0x0066, 0x0067, 0xFFFF, 0xFFFD, 0xDD20)]
        public void ComputeHeaderChecksum_ReproducesCapturedWord6(
            int markers,
            int typeAndSubtype,
            int destinationNode,
            int sourceNode,
            int flags1,
            int flags2,
            int expectedWord6)
        {
            ushort actual = XmsgEnvelope.ComputeHeaderChecksum(
                (ushort)markers,
                (ushort)typeAndSubtype,
                (ushort)destinationNode,
                (ushort)sourceNode,
                (ushort)flags1,
                (ushort)flags2);

            Assert.Equal((ushort)expectedWord6, actual);
        }

        /// <summary>
        /// The high byte is the wire's offset-12 "Protocol ID" and the low byte its offset-13
        /// "Counter" - the two fields the codebase long treated as independent.
        /// </summary>
        [Fact]
        public void Word6_SplitsIntoTheHistoricalProtocolIdAndCounter()
        {
            // The APPEND-REMOTE-BATCH request again: the wire carried protocol ID 0xD9 at offset 12
            // and counter 0xC1 at offset 13, which are one 16-bit checksum.
            ushort checksum = XmsgEnvelope.ComputeHeaderChecksum(
                0x2113, 0x000E, 0x0064, 0x0066, 0x03D3, 0x0080);

            Assert.Equal(0xD9, (byte)(checksum >> 8));
            Assert.Equal(0xC1, (byte)(checksum & 0xFF));
        }

        /// <summary>
        /// The end-around carry is real: a sum that overflows 16 bits folds the carry back in.
        /// </summary>
        /// <remarks>
        /// Without the fold this pair would produce a different result, so this pins the
        /// <c>RADD ADC</c> in the kernel loop rather than a plain truncating add.
        /// </remarks>
        [Fact]
        public void ComputeHeaderChecksum_FoldsTheEndAroundCarry()
        {
            // 0xFFFF + 0x0002 overflows; with end-around carry the running sum is 0x0002, so the
            // complement is 0xFFFD. A truncating add would give 0x0001 and complement 0xFFFE.
            ushort actual = XmsgEnvelope.ComputeHeaderChecksum(0xFFFF, 0x0002, 0, 0, 0, 0);

            Assert.Equal((ushort)0xFFFD, actual);
        }
    }
}
