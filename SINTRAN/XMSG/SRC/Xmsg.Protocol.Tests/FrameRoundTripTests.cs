using NDInsight.Sintran.Xmsg;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Round-trip tests for a full data frame built with the fluent builders.
    /// </summary>
    public sealed class FrameRoundTripTests
    {
        [Fact]
        public void BuildDataFrame_Serialize_Decode_Equal()
        {
            XroutMessage body = new XroutMessageBuilder()
                .WithSerial(42)
                .WithService(XroutService.XSGSY)
                .AddInteger16(1, 102)                 // first system number
                .AddInteger16(2, (ushort)XroutConnectionType.Via)
                .Build();

            XmsgFrame frame = new XmsgFrameBuilder()
                .Between(102, 100)
                .WithDatagramSequence(7)
                .WithProtocol(SintranProtocolId.Routing)
                .WithFlags2(0x0400)
                .WithSubHeaderControl(0x04, 0x82, 0xC4)
                .WithEndpoints(102, 0x02C1, 100, 0x02AD)
                .WithControlService(0x0100014B)
                .WithBody(body)
                .Build();

            byte[] wire = frame.ToArray();

            XmsgFrame decoded = XmsgFrame.Parse(wire);

            // Header equality.
            Assert.Equal(SintranPacketSubtype.Data, decoded.Header.Subtype);
            Assert.Equal(102, decoded.Header.DestinationNode);
            Assert.Equal(100, decoded.Header.SourceNode);
            Assert.Equal(7, decoded.Header.Flags1);
            Assert.Equal(0x0400, decoded.Header.Flags2);
            Assert.Equal(SintranProtocolId.Routing, decoded.Header.ProtocolId);

            // Sub-header equality.
            Assert.NotNull(decoded.SubHeader);
            Assert.Equal(0x04, decoded.SubHeader!.Counter);
            Assert.Equal(0x82, decoded.SubHeader.FrameFlags);
            Assert.Equal(0xC4, decoded.SubHeader.Role);
            Assert.Equal(102, decoded.SubHeader.DestinationSystem);
            Assert.Equal(0x02C1, decoded.SubHeader.DestinationPort);
            Assert.Equal(100, decoded.SubHeader.SourceSystem);
            Assert.Equal(0x02AD, decoded.SubHeader.SourcePort);
            Assert.Equal(0x0100014Bu, decoded.SubHeader.ControlService);

            // Body equality. The XROUT header is NOT carried on this transport, so the serial
            // and service the builder set never reach the wire and come back as zero; the
            // service the receiver acts on is the XMCSM word asserted above. What must survive
            // is the parameter blocks. See XroutMessageFraming for the capture evidence.
            Assert.NotNull(decoded.Body);
            Assert.Equal(0, decoded.Body!.Serial);
            Assert.Equal(0, decoded.Body.Service);
            Assert.Equal(2, decoded.Body.Parameters.Count);
            Assert.Equal(1, decoded.Body.Parameters[0].ParameterNumber);
            Assert.Equal(new byte[] { 0x00, 0x66 }, decoded.Body.Parameters[0].Data);
            Assert.Equal(2, decoded.Body.Parameters[1].ParameterNumber);
            Assert.Equal(new byte[] { 0x00, (byte)XroutConnectionType.Via }, decoded.Body.Parameters[1].Data);

            // Decode -> re-serialize must be byte-identical.
            Assert.Equal(wire, decoded.ToArray());
        }
    }
}
