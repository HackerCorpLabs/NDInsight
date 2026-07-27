using System;
using System.Text;

using NDInsight.Sintran.Xmsg;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Proves that XROUT messages carried in XMSG data frames have NO four-byte header, using two
    /// frames taken verbatim from the HDLC captures.
    /// </summary>
    /// <remarks>
    /// Both frames decode, header-free, into exactly the parameters the COSMOS Programmer Guide
    /// specifies for their service - which is the argument that the wire form is body-only. Read
    /// with a header assumed, the connect letter's leading <c>FF 07</c> is mistaken for a serial
    /// and a service and every parameter is lost, and the routing reply loses its parameter 1.
    /// </remarks>
    public sealed class XroutLetterFramingTests
    {
        /// <summary>
        /// The connect letter from the capture, decoded as a whole frame.
        /// </summary>
        /// <remarks>
        /// Frame 0 of the connect-to capture: node 103 asking XROUT on node 102 to deliver a
        /// letter to the named server. XMCSM low byte 0x41 is XSLET and the destination port is 0,
        /// the XROUT sink.
        /// </remarks>
        [Fact]
        public void ConnectLetter_DecodesToServerNameAndSystemName()
        {
            // Captured bytes, verbatim. Layout: 13-byte SINTRAN header, 19-byte XMSG sub-header
            // (XMCSM 0x04000041 = XSLET, XMLEN 16), then the 16-byte letter body:
            //   FF 07 2A "TADADM"   string parameter 1 = "*TADADM"  (0x2A is the asterisk)
            //   00                  even-alignment fill
            //   FE 04 "D102"        string parameter 2 = the system name
            byte[] wire = FromHex(
                "2113000E0066006700040400DA0D210086E4006600000067024504000041" +
                "0010FF072A54414441444D00FE0444313032");

            XmsgFrame frame = XmsgFrame.Parse(wire);

            Assert.NotNull(frame.SubHeader);
            Assert.Equal(0u, (uint)frame.SubHeader!.DestinationPort);
            Assert.Equal(0x04000041u, frame.SubHeader.ControlService);

            Assert.NotNull(frame.Body);
            Assert.Equal(2, frame.Body!.Parameters.Count);

            XroutParameter name = frame.Body.Parameters[0];
            Assert.Equal(1, name.ParameterNumber);
            Assert.True(name.IsString);
            Assert.Equal("*TADADM", name.AsText());

            XroutParameter system = frame.Body.Parameters[1];
            Assert.Equal(2, system.ParameterNumber);
            Assert.True(system.IsString);
            Assert.Equal("D102", system.AsText());
        }

        /// <summary>
        /// The routing-information reply from the capture yields all four documented parameters.
        /// </summary>
        /// <remarks>
        /// Appendix B section 3.15: OUT 1 = the first system found, 2 = connection type,
        /// 3 = type-dependent extra info, 4 = network info. Assuming a header swallows parameter 1
        /// and shifts the rest, which is exactly the corruption this test guards against.
        /// </remarks>
        [Fact]
        public void RoutingReply_DecodesToAllFourParameters()
        {
            byte[] wire = FromHex(
                "2113000E0066006400020100DD" +
                "1221008660" +
                "006602D2006602D2" +
                "01000100" + "0010" +
                "010200640202000403020064" + "04020000");

            XmsgFrame frame = XmsgFrame.Parse(wire);

            Assert.NotNull(frame.Body);
            Assert.Equal(4, frame.Body!.Parameters.Count);

            Assert.Equal(new byte[] { 0x00, 0x64 }, frame.Body.Parameters[0].Data);   // system 100
            Assert.Equal(new byte[] { 0x00, 0x04 }, frame.Body.Parameters[1].Data);   // type Local
            Assert.Equal(new byte[] { 0x00, 0x64 }, frame.Body.Parameters[2].Data);   // extra 100
            Assert.Equal(new byte[] { 0x00, 0x00 }, frame.Body.Parameters[3].Data);   // network 0
        }

        /// <summary>
        /// A decoded letter re-serialises to the exact captured bytes.
        /// </summary>
        [Fact]
        public void ConnectLetter_ReSerialisesByteIdentically()
        {
            byte[] wire = FromHex(
                "2113000E0066006700040400DA0D210086E4006600000067024504000041" +
                "0010FF072A54414441444D00FE0444313032");

            XmsgFrame frame = XmsgFrame.Parse(wire);

            Assert.Equal(wire, frame.ToArray());
        }

        /// <summary>
        /// The two framings are distinguishable and each round-trips through its own form.
        /// </summary>
        [Fact]
        public void BothFramings_RoundTripThroughTheirOwnForm()
        {
            XroutMessage message = new XroutMessageBuilder()
                .WithSerial(42)
                .WithService(XroutService.XSLET)
                .AddString(1, "*TADADM")
                .Build();

            byte[] framed = message.ToArray(XroutMessageFraming.WithHeader);
            byte[] bare = message.ToArray(XroutMessageFraming.BodyOnly);

            Assert.Equal(bare.Length + XroutMessage.HeaderSize, framed.Length);
            Assert.Equal(42, framed[0]);
            Assert.Equal((byte)XroutService.XSLET, framed[1]);

            XroutMessage fromFramed = XroutMessage.Parse(framed, XroutMessageFraming.WithHeader);
            XroutMessage fromBare = XroutMessage.Parse(bare, XroutMessageFraming.BodyOnly);

            Assert.Equal("*TADADM", fromFramed.Parameters[0].AsText());
            Assert.Equal("*TADADM", fromBare.Parameters[0].AsText());

            // The header values simply are not present in the bare form.
            Assert.Equal(42, fromFramed.Serial);
            Assert.Equal(0, fromBare.Serial);
        }

        /// <summary>
        /// Converts a hexadecimal string to bytes.
        /// </summary>
        /// <param name="hex">
        /// The hexadecimal text, without separators.
        /// </param>
        /// <returns>
        /// The decoded bytes.
        /// </returns>
        private static byte[] FromHex(string hex)
        {
            byte[] result = new byte[hex.Length / 2];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
            }

            return result;
        }
    }
}
