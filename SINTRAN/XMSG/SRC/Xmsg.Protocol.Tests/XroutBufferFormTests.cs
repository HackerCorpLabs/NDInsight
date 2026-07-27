using System;
using System.Text;

using NDInsight.Sintran.Xmsg;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Protocol.Tests
{
    /// <summary>
    /// Parses XROUT service messages captured from a running SINTRAN's own memory, proving the
    /// message-buffer form carries the four-byte header the wire form omits.
    /// </summary>
    /// <remarks>
    /// Bytes taken verbatim from a MON 200 XFWRI trace on the BIGDISK0-L image - the buffers tasks
    /// hand to XROUT, before anything reaches a wire. Evidence and method:
    /// DOC/XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md.
    /// </remarks>
    public sealed class XroutBufferFormTests
    {
        /// <summary>
        /// The name registration a server sends to XROUT: XSNAM plus its name.
        /// </summary>
        /// <remarks>
        /// This is the call that publishes a server. It never crosses a wire, so it could only
        /// ever be seen from inside the machine.
        /// </remarks>
        [Fact]
        public void XsnamRegistration_ParsesWithTheHeader()
        {
            byte[] buffer = FromHex("534200" + "0A" + "FF082A584D2D4649444F");

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal(0x53, message.Serial);
            Assert.Equal((byte)XroutService.XSNAM, message.Service);
            Assert.Equal(10, message.Length);

            Assert.Single(message.Parameters);
            Assert.Equal(1, message.Parameters[0].ParameterNumber);
            Assert.True(message.Parameters[0].IsString);
            Assert.Equal("*XM-FIDO", message.Parameters[0].AsText());
        }

        /// <summary>
        /// A reply carrying a magic number decomposes with the carved layout, from guest memory
        /// rather than from the wire.
        /// </summary>
        [Fact]
        public void ReplyMagicNumber_DecomposesAsPortAndRandom()
        {
            byte[] buffer = FromHex("01000010" + "0104006401F8" + "FE082A584D2D4649444F");

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutError.XRSOK, message.Service);
            Assert.Equal(2, message.Parameters.Count);

            uint magic;
            Assert.True(message.Parameters[0].TryGetUInt32(out magic));
            Assert.Equal(0x006401F8u, magic);

            // system 100, port word 504 = (3 << 7) | 120
            Assert.Equal(100u, magic >> 16);
            ushort portWord = (ushort)(magic & 0xFFFF);
            Assert.Equal(504, (int)portWord);

            int portNumber;
            int random;
            Assert.True(XmsgPortWordAllocator.TrySplit(portWord, out portNumber, out random));
            Assert.Equal(3, portNumber);
            Assert.Equal(120, random);

            Assert.Equal("*XM-FIDO", message.Parameters[1].AsText());
        }

        /// <summary>
        /// The define-remote-name command produces exactly the parameters appendix B section 3.12
        /// specifies: the system name, then the system number.
        /// </summary>
        [Fact]
        public void XsdrnDefineRemoteName_MatchesTheManual()
        {
            byte[] buffer = FromHex("0149000A" + "FF0444313030" + "02020064");

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSDRN, message.Service);
            Assert.Equal(2, message.Parameters.Count);

            Assert.True(message.Parameters[0].IsString);
            Assert.Equal("D100", message.Parameters[0].AsText());

            uint systemNumber;
            Assert.True(message.Parameters[1].TryGetUInt32(out systemNumber));
            Assert.Equal(100u, systemNumber);
        }

        /// <summary>
        /// Our own builder produces the same bytes the guest wrote, which is the round trip that
        /// makes the builders trustworthy.
        /// </summary>
        [Fact]
        public void OurBuilder_ReproducesTheCapturedDefineRemoteName()
        {
            byte[] captured = FromHex("0149000A" + "FF0444313030" + "02020064");

            XroutMessage built = new XroutMessageBuilder()
                .WithSerial(0x01)
                .WithService(XroutService.XSDRN)
                .AddString(1, "D100")
                .AddInteger16(2, 100)
                .Build();

            Assert.Equal(captured, built.ToArray(XroutMessageFraming.WithHeader));
        }

        /// <summary>
        /// Our own builder reproduces the captured registration too.
        /// </summary>
        [Fact]
        public void OurBuilder_ReproducesTheCapturedRegistration()
        {
            byte[] captured = FromHex("5342000A" + "FF082A584D2D4649444F");

            XroutMessage built = new XroutMessageBuilder()
                .WithSerial(0x53)
                .WithService(XroutService.XSNAM)
                .AddString(1, "*XM-FIDO")
                .Build();

            Assert.Equal(captured, built.ToArray(XroutMessageFraming.WithHeader));
        }

        /// <summary>
        /// The walk service starts from a magic number and the next call resumes one past the
        /// answer, which is how "greater than or equal" becomes an enumeration.
        /// </summary>
        [Fact]
        public void XsgniWalk_ResumesPastThePreviousAnswer()
        {
            XroutMessage first = XroutMessage.Parse(
                FromHex("01450004" + "01020000"), XroutMessageFraming.WithHeader);
            XroutMessage next = XroutMessage.Parse(
                FromHex("01450006" + "0104006401F9"), XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSGNI, first.Service);
            Assert.Equal((byte)XroutService.XSGNI, next.Service);

            uint from;
            Assert.True(next.Parameters[0].TryGetUInt32(out from));

            // One past the 0x006401F8 the previous answer reported.
            Assert.Equal(0x006401F9u, from);
        }

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
