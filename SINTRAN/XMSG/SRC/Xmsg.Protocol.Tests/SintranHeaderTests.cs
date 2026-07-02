using NDInsight.Sintran.Xmsg;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Verifies SINTRAN header parsing against real captured fixtures
    /// (XMSG-PROTOCOL.md sections 5.1 and 6).
    /// </summary>
    public sealed class SintranHeaderTests
    {
        [Fact]
        public void ReachabilityRequest_100_To_102()
        {
            byte[] frame = TestHex.Parse("21 13 00 19 00 66 00 64 FF FF 00 01 DE 08");
            SintranHeader header = SintranHeader.Parse(frame);

            Assert.Equal(SintranHeader.Marker1Value, header.Marker1);
            Assert.Equal(SintranHeader.Marker2Normal, header.Marker2);
            Assert.Equal(SintranPacketSubtype.ReachabilityRequest, header.Subtype);
            Assert.Equal(102, header.DestinationNode);
            Assert.Equal(100, header.SourceNode);
            Assert.Equal(0xFFFF, header.Flags1);
            Assert.Equal(0x0001, header.Flags2);
            Assert.Equal(SintranProtocolId.Routing, header.ProtocolId);
        }

        [Fact]
        public void ReachabilityReply_102_To_100()
        {
            byte[] frame = TestHex.Parse("21 13 00 13 00 64 00 66 FF FF 00 01 DE 0E");
            SintranHeader header = SintranHeader.Parse(frame);

            Assert.Equal(SintranPacketSubtype.ReachabilityReply, header.Subtype);
            Assert.Equal(100, header.DestinationNode);
            Assert.Equal(102, header.SourceNode);
            Assert.Equal(0xFFFF, header.Flags1);
            Assert.Equal(0x0001, header.Flags2);
            Assert.Equal(SintranProtocolId.Routing, header.ProtocolId);
        }

        [Fact]
        public void DeliveryAck_EchoesDatagramSequence()
        {
            // 0x03 ACK; Flags1 = 0x0004 echoes the acked data frame's datagram seq.
            byte[] frame = TestHex.Parse("21 13 00 03 00 66 00 67 00 04 00 01 DE 17");
            XmsgFrame decoded = XmsgFrame.Parse(frame);

            Assert.Equal(SintranPacketSubtype.Ack, decoded.Header.Subtype);
            Assert.Equal(102, decoded.Header.DestinationNode);
            Assert.Equal(103, decoded.Header.SourceNode);
            Assert.Equal(0x0004, decoded.Header.Flags1);
            Assert.Equal(0x0001, decoded.Header.Flags2);
            Assert.Equal(SintranProtocolId.Routing, decoded.Header.ProtocolId);

            // The single trailing byte is the routing/connection-step command byte.
            Assert.Single(decoded.TrailingBytes);
            Assert.Equal(0x17, decoded.TrailingBytes[0]);
        }

        [Fact]
        public void ReachabilityFixtures_RoundTripByteIdentical()
        {
            string[] fixtures =
            {
                "21 13 00 19 00 66 00 64 FF FF 00 01 DE 08",
                "21 13 00 13 00 64 00 66 FF FF 00 01 DE 0E",
                "21 13 00 03 00 66 00 67 00 04 00 01 DE 17",
            };

            for (int i = 0; i < fixtures.Length; i++)
            {
                byte[] original = TestHex.Parse(fixtures[i]);
                XmsgFrame decoded = XmsgFrame.Parse(original);
                byte[] reserialized = decoded.ToArray();
                Assert.Equal(original, reserialized);
            }
        }
    }
}
