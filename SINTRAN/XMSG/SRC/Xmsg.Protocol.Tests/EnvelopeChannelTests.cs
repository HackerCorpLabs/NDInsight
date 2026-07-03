using System;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Packet;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Verifies the universal envelope channel derivation (<see cref="XmsgEnvelope.DeriveChannel"/>)
    /// against the captured <c>conn-to-d102-from-100</c> responder frames and our own live-accepted
    /// accept. The channel is DERIVED from Flags1 + Counter + XMCSM, not allocated — this is the
    /// primitive that lets the responder build session-data frames on the correct channel.
    /// </summary>
    public sealed class EnvelopeChannelTests
    {
        // Each row: Flags1, Counter, XMCSM, expected Protocol ID — taken directly from the capture
        // (102 -> 100 responder frames) and from our live accept.
        [Theory]
        // Capture conn-to-d102-from-100, 102's responder side:
        [InlineData(0x012F, 0xE5, 0x04000041u, (byte)SintranProtocolId.D8)]    // frame 51 connect-accept
        [InlineData(0x0130, 0xE4, 0x04000000u, (byte)SintranProtocolId.D8)]    // frame 53 port-assign
        [InlineData(0x0131, 0xDB, 0x01080000u, (byte)SintranProtocolId.Db)]    // frame 54 DUMM
        [InlineData(0x0132, 0xDA, 0x00080000u, (byte)SintranProtocolId.Dc)]    // frame 57 control 0x20
        [InlineData(0x0135, 0xD7, 0x01080000u, (byte)SintranProtocolId.Db)]    // frame 62 MOTD
        [InlineData(0x0137, 0xD5, 0x01080000u, (byte)SintranProtocolId.Db)]    // frame 66 PASSWORD prompt
        // Our own live session accept (Base 0x0014) — 100 accepted this exact channel:
        [InlineData(0x0004, 0x10, 0x04000041u, (byte)SintranProtocolId.Pad)]   // live accept -> DA
        public void DeriveChannel_MatchesCapturedResponderFrames(int flags1, int counter, uint xmcsm, byte expected)
        {
            SintranProtocolId channel = XmsgEnvelope.DeriveChannel((ushort)flags1, (byte)counter, xmcsm);
            Assert.Equal((SintranProtocolId)expected, channel);
        }

        /// <summary>
        /// Base is Flags1 + Counter, and within one stream Flags1↑ / Counter↓ keep it constant.
        /// </summary>
        [Fact]
        public void ComputeBase_IsConstantAcrossAStream()
        {
            // Capture frames 54 and 57 belong to the same session-data stream; both have Base 0x020C.
            Assert.Equal(0x020C, XmsgEnvelope.ComputeBase(0x0131, 0xDB));
            Assert.Equal(0x020C, XmsgEnvelope.ComputeBase(0x0132, 0xDA));
            Assert.Equal(0x020C, XmsgEnvelope.ComputeBase(0x0135, 0xD7));
        }

        /// <summary>
        /// The session-data builder derives the channel and reproduces the captured DUMM (frame 54)
        /// and MOTD (frame 62) responder frames byte-for-byte — proof the builder can emit the real
        /// bring-up frames once the session's Flags1/Counter stream is chosen.
        /// </summary>
        [Fact]
        public void CreateSessionData_ReproducesCapturedDummAndMotd()
        {
            // Frame 54: DUMM on the derived DB channel.
            XmsgDataFields dumm = new XmsgDataFields
            {
                DestinationNode = 0x0064,
                SourceNode = 0x0066,
                Flags1 = 0x0131,
                Flags2 = 0x0108,
                Counter = 0xDB,
                FrameFlags = 0x92,
                Role = 0x00,
                DestinationSystem = 0x0064,
                DestinationPort = 0x02AB,
                SourceSystem = 0x0066,
                SourcePort = 0x04C2,
                ControlService = 0x01080000u,
                Payload = Convert.FromHexString("1800"),   // TAD DUMM (0x18) count=0
            };
            Assert.Equal(
                Convert.FromHexString("2113000E0064006601310108DBDB21009200006402AB006604C20108000000021800"),
                XmsgPacketBuilder.CreateSessionData(dumm).ToBytes());

            // Frame 62: the combined terminal-setup + MOTD chain on the derived DB channel.
            XmsgDataFields motd = new XmsgDataFields
            {
                DestinationNode = 0x0064,
                SourceNode = 0x0066,
                Flags1 = 0x0135,
                Flags2 = 0x0108,
                Counter = 0xD7,
                FrameFlags = 0x96,
                Role = 0x00,
                DestinationSystem = 0x0064,
                DestinationPort = 0x02AB,
                SourceSystem = 0x0066,
                SourcePort = 0x04C2,
                ControlService = 0x01080000u,
                Payload = Convert.FromHexString(
                    "0004030100000003010101600D0A2032322E32372E32322020202020203820415052494C202020"
                    + "313939380D0A2053494E5452414E20494949202D205653582F353030204C0D0A2D2D2D2052455452"
                    + "4F434F524520454D554C41544544204C2049443A313032202D2D2D0D0A1302000201080D0A454E54"
                    + "4552200200"),
            };
            Assert.Equal(
                Convert.FromHexString(
                    "2113000E0064006601350108DBD721009600006402AB006604C201080000007C0004030100000003"
                    + "010101600D0A2032322E32372E32322020202020203820415052494C202020313939380D0A205349"
                    + "4E5452414E20494949202D205653582F353030204C0D0A2D2D2D20524554524F434F524520454D55"
                    + "4C41544544204C2049443A313032202D2D2D0D0A1302000201080D0A454E544552200200"),
                XmsgPacketBuilder.CreateSessionData(motd).ToBytes());
        }
    }
}
