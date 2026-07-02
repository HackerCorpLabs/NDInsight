using NDInsight.Sintran.Xmsg;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Spot-checks that generated enum values match the authoritative JSON constants.
    /// </summary>
    public sealed class EnumValueTests
    {
        [Fact]
        public void XfSec_IsBit9_Mask512()
        {
            // option_bits values are BIT NUMBERS; XFSEC bit 9 -> mask 1<<9 = 512.
            Assert.Equal(512, (int)XmsgOption.XFSEC);
            Assert.Equal(1 << 9, (int)XmsgOption.XFSEC);
        }

        [Fact]
        public void XfSnd_Is12()
        {
            Assert.Equal(12, (int)XmsgFunction.XFSND);
        }

        [Fact]
        public void XsGsy_Is75()
        {
            Assert.Equal(75, (int)XroutService.XSGSY);
        }

        [Fact]
        public void XeIma_IsNegative19()
        {
            Assert.Equal(-19, (int)XmsgError.XEIMA);
        }

        [Fact]
        public void XrNro_Is12()
        {
            Assert.Equal(12, (int)XroutError.XRNRO);
        }

        [Fact]
        public void ProtocolAndSubtype_MatchWire()
        {
            Assert.Equal(0xDE, (int)SintranProtocolId.Routing);
            Assert.Equal(0x03, (int)SintranPacketSubtype.Ack);
            Assert.Equal(0x0E, (int)SintranPacketSubtype.Data);
            Assert.Equal(0x13, (int)SintranPacketSubtype.ReachabilityReply);
            Assert.Equal(0x19, (int)SintranPacketSubtype.ReachabilityRequest);
        }

        [Fact]
        public void ConnectionType_And_LinkState_Values()
        {
            Assert.Equal(2, (int)XroutConnectionType.Via);
            Assert.Equal(4, (int)XroutConnectionType.Local);
            Assert.Equal(4, (int)XmsgLinkState.Run);
        }

        [Fact]
        public void Subservices_AreGroupedWithoutCollision()
        {
            // Different parent services reuse parameter-1 values 1..5; the split enums
            // keep them distinct.
            Assert.Equal(1, (int)XroutSetCrashInfoSubservice.XSDAR);
            Assert.Equal(1, (int)XroutGetAttributeSubservice.XSGXV);
            Assert.Equal(1, (int)XroutDefineAttributeSubservice.XSDFR);
        }
    }
}
