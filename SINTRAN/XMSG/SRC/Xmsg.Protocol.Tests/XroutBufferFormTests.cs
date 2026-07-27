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

        /// <summary>
        /// The file-transfer server creates its connection port with the NAME ALONE - no count
        /// parameter at all.
        /// </summary>
        /// <remarks>
        /// Captured 2026-07-27 while *XFTRA started. The manual lists three parameters for XSCRS;
        /// this proves parameters 2 and 3 are genuinely optional.
        /// </remarks>
        [Fact]
        public void XscrsForXftra_CarriesTheNameOnly()
        {
            byte[] buffer = FromHex("5350 0008 FF06 2A5846545241");

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSCRS, message.Service);
            Assert.Single(message.Parameters);
            Assert.Equal("*XFTRA", message.Parameters[0].AsText());
        }

        /// <summary>
        /// The file-access servers create their connection ports with an initial count of ZERO,
        /// then raise it one service point at a time.
        /// </summary>
        /// <remarks>
        /// This is the mechanism behind the "Free SPs" column: XSCRS does not set the total. Note
        /// the pad byte after the odd-length name - the integer block that follows is even-aligned.
        /// </remarks>
        [Theory]
        [InlineData("5350 000E FF07 2A46412D465341 00 0202 0000", "*FA-FSA")]
        [InlineData("5350 0010 FF09 2A46412D4653412D49 00 0202 0000", "*FA-FSA-I")]
        [InlineData("5350 0010 FF0A 2A46412D534552564552 0202 0000", "*FA-SERVER")]
        public void XscrsForFileAccess_StartsAtZeroConnections(string hex, string expectedName)
        {
            byte[] buffer = FromHex(hex);

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSCRS, message.Service);
            Assert.Equal(2, message.Parameters.Count);
            Assert.Equal(expectedName, message.Parameters[0].AsText());

            uint initialConnections;
            Assert.True(message.Parameters[1].TryGetUInt32(out initialConnections));
            Assert.Equal(0u, initialConnections);
        }

        /// <summary>
        /// Each service point is added by its own XSNSP of exactly +1.
        /// </summary>
        /// <remarks>
        /// Captured once for *XFTRA, twice for *FA-FSA and thirty times for *FA-SERVER - matching
        /// the 1 / 2 / 30 the registry then reported.
        /// </remarks>
        [Fact]
        public void Xsnsp_AddsOneServicePoint()
        {
            byte[] buffer = FromHex("54510004" + "01020001");

            XroutMessage message = XroutMessage.Parse(buffer, XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSNSP, message.Service);
            Assert.Single(message.Parameters);

            uint delta;
            Assert.True(message.Parameters[0].TryGetUInt32(out delta));
            Assert.Equal(1u, delta);
        }

        /// <summary>
        /// A registry walk answers with a third parameter - the free-connection count - but only
        /// for connection ports.
        /// </summary>
        /// <remarks>
        /// The *FA-SERVER entry reports 0x1E = 30, the total its thirty XSNSP calls built up. A
        /// plain XSNAM port such as *XM-FIDO answers with no parameter 3 at all, which is how a
        /// caller tells the two port kinds apart.
        /// </remarks>
        [Fact]
        public void XsgniReply_ReportsFreeConnectionsForAConnectionPort()
        {
            byte[] connectionPort = FromHex(
                "0100 0016 0104 006405CC FE0A 2A46412D534552564552 0302 001E");

            XroutMessage message = XroutMessage.Parse(connectionPort, XroutMessageFraming.WithHeader);

            Assert.Equal(3, message.Parameters.Count);
            Assert.Equal("*FA-SERVER", message.Parameters[1].AsText());

            uint freeConnections;
            Assert.True(message.Parameters[2].TryGetUInt32(out freeConnections));
            Assert.Equal(30u, freeConnections);

            // ... and the magic number decomposes as port 11, which is where the registry listed it.
            uint magic;
            Assert.True(message.Parameters[0].TryGetUInt32(out magic));
            int portNumber;
            int random;
            Assert.True(XmsgPortWordAllocator.TrySplit((ushort)(magic & 0xFFFF), out portNumber, out random));
            Assert.Equal(11, portNumber);
            Assert.Equal(76, random);
        }

        /// <summary>
        /// The named-port answer for *XM-FIDO has no free-connection parameter.
        /// </summary>
        [Fact]
        public void XsgniReply_HasNoFreeConnectionsForANamedPort()
        {
            byte[] namedPort = FromHex(
                "01000010" + "0104006401F8" + "FE082A584D2D4649444F");

            XroutMessage message = XroutMessage.Parse(namedPort, XroutMessageFraming.WithHeader);

            Assert.Equal(2, message.Parameters.Count);
            Assert.Equal("*XM-FIDO", message.Parameters[1].AsText());
        }

        /// <summary>
        /// Looking a SYSTEM name up with XSGIN answers with the system number as parameter 2 and
        /// no parameter 1.
        /// </summary>
        /// <remarks>
        /// Captured 2026-07-27 from the XMSG command program's Get-System-Name-or-Number. The
        /// absent parameter 1 is the point: the manual makes the port number optional, returned
        /// only when the name is a port name. See
        /// DOC/XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md.
        /// </remarks>
        [Fact]
        public void XsginForASystemName_AnswersWithTheSystemNumberOnly()
        {
            XroutMessage request = XroutMessage.Parse(
                FromHex("0152 0006 FF04 44313032"), XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSGIN, request.Service);
            Assert.Single(request.Parameters);
            Assert.Equal("D102", request.Parameters[0].AsText());

            XroutMessage reply = XroutMessage.Parse(
                FromHex("0100 0004 0202 0066"), XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutError.XRSOK, reply.Service);
            Assert.Single(reply.Parameters);
            Assert.Equal(2, reply.Parameters[0].ParameterNumber);

            uint systemNumber;
            Assert.True(reply.Parameters[0].TryGetUInt32(out systemNumber));
            Assert.Equal(102u, systemNumber);
        }

        /// <summary>
        /// Looking a PORT name up answers with both outputs - port number as parameter 1, system
        /// number as parameter 2.
        /// </summary>
        /// <remarks>
        /// `*TADADM` sat on port 4 of system 100 in that boot, which is exactly what came back.
        /// This is the form that resolves a name without needing privilege, and without ever
        /// yielding a magic number.
        /// </remarks>
        [Fact]
        public void XsginForAPortName_AnswersWithPortAndSystem()
        {
            XroutMessage request = XroutMessage.Parse(
                FromHex("0152 000A FF07 2A5441444144 4D 00"), XroutMessageFraming.WithHeader);

            Assert.Equal((byte)XroutService.XSGIN, request.Service);
            Assert.Equal("*TADADM", request.Parameters[0].AsText());

            XroutMessage reply = XroutMessage.Parse(
                FromHex("0100 0008 0102 0004 0202 0064"), XroutMessageFraming.WithHeader);

            Assert.Equal(2, reply.Parameters.Count);

            uint portNumber;
            Assert.True(reply.Parameters[0].TryGetUInt32(out portNumber));
            Assert.Equal(4u, portNumber);

            uint systemNumber;
            Assert.True(reply.Parameters[1].TryGetUInt32(out systemNumber));
            Assert.Equal(100u, systemNumber);
        }

        /// <summary>
        /// An unknown name comes back with the service byte overwritten by a status, and no
        /// parameters at all.
        /// </summary>
        /// <remarks>
        /// Captured by asking for "D10", an abbreviation that is not a defined name. The console
        /// then printed "System name D10 is not known".
        /// </remarks>
        [Fact]
        public void XsginForAnUnknownName_AnswersWithAStatusAndNoParameters()
        {
            XroutMessage reply = XroutMessage.Parse(
                FromHex("0102 0000"), XroutMessageFraming.WithHeader);

            Assert.Equal(0x02, reply.Service);
            Assert.Empty(reply.Parameters);
        }

        /// <summary>
        /// Parses a hex string, ignoring spaces so a capture can be written with its field
        /// boundaries visible.
        /// </summary>
        private static byte[] FromHex(string hex)
        {
            string packed = hex.Replace(" ", string.Empty);
            byte[] result = new byte[packed.Length / 2];
            for (int i = 0; i < result.Length; i++)
            {
                result[i] = Convert.ToByte(packed.Substring(i * 2, 2), 16);
            }

            return result;
        }
    }
}
