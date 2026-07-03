using System;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Live.Tad;

using Xunit;
using Xunit.Abstractions;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// Verifies the TAD connect-accept our responder builds for the exact live connect request
    /// captured from machine 100, byte-comparing it to the structure of the known-good responder
    /// accept from the conn-to-102-from103-via100 capture (the DA connect matching our role).
    /// </summary>
    public sealed class TadConnectAcceptTests
    {
        private readonly ITestOutputHelper _output;

        public TadConnectAcceptTests(ITestOutputHelper output)
        {
            _output = output;
        }

        [Fact]
        public void ConnectAccept_EchoesProtoFlags1AndCounter()
        {
            // The exact live connect request 100 -> 103 (proto DA, f1=0x0000, counter=0x13).
            byte[] reqBytes = Convert.FromHexString(
                "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033");
            XmsgFrame request = XmsgFrame.Parse(reqBytes);

            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2));
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames = responder.OnConnect(request);

            Assert.Single(frames);
            byte[] accept = frames[0].ToArray();
            _output.WriteLine("accept: " + Convert.ToHexString(accept));

            // Expected accept: echo proto DA / f1 0x0000 / counter 0x13, role 0x40, swap addressing,
            // reply from our TADADM port 0x0156 to 100's port 0x02F7, params 01020000 0202000A.
            byte[] expected = Convert.FromHexString(
                "2113000E00640067000004" + "00DA" + "13" + "2100" + "86" + "40"
                + "0064" + "02F7" + "0067" + "0156" + "04000041" + "00" + "08"
                + "01020000" + "0202000A");

            Assert.Equal(expected, accept);
        }

        [Fact]
        public void ConnectAck_RidesConnectChannelPlus4_EchoesFlags1()
        {
            // The exact live connect request 100 -> 103 on the PAD channel DA (0xDA), f1=0x0000.
            // VERIFIED rule: the secure ACK for this session rides connect-channel + 4 = DE (0xDE),
            // NOT the connect's own DA channel (the old +0 echo crashed 100 with XXPER).
            byte[] reqBytes = Convert.FromHexString(
                "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033");
            XmsgFrame request = XmsgFrame.Parse(reqBytes);

            // Node 103; the ACK trailing byte is derived from the connect counter, not the seed.
            XmsgNode node = new XmsgNode(103, 0x00);
            node.TadResponder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2));
            node.AcknowledgeTadFrames = true;

            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames = node.HandleFrames(request);

            // First frame must be the 0x03 delivery ACK; the second the connect-accept.
            Assert.True(frames.Count >= 2);
            byte[] ack = frames[0].ToArray();
            _output.WriteLine("ack: " + Convert.ToHexString(ack));

            // ACK layout (14 bytes): 21 13 00 03 | dst=0064 | src=0067 | f1=0000 | flags2=0001
            //   | proto=DE (connect DA + 4) | trailing=1D (connect counter 0x13 + 0x0A, VERIFIED
            //   rule). NOT the seed and NOT 0x00 (0x00 is the malformed ACK that crashed 100).
            byte[] expected = Convert.FromHexString("211300030064006700000001" + "DE" + "1D");
            Assert.Equal(expected, ack);

            // And the ACK channel the responder learned is exactly DE.
            Assert.Equal(SintranProtocolId.Routing, node.TadResponder.AckChannel);
        }

        [Fact]
        public void PortAssign_EchoesSessionSetupAndAssignsOurPort()
        {
            // Establish the session first (accept), then feed the session-setup 100 sends next.
            byte[] connectBytes = Convert.FromHexString(
                "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033");
            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2));
            responder.OnConnect(XmsgFrame.Parse(connectBytes));

            // The exact session-setup 100 -> 103 (proto DA, f1=0x0001, counter=0x12, XMCSM 04000000).
            byte[] setupBytes = Convert.FromHexString(
                "2113000E0067006400010400DA122100868400670156006402F704000000000906001B001C0100FF00");
            XmsgFrame setup = XmsgFrame.Parse(setupBytes);
            Assert.True(responder.IsSessionSetup(setup));

            byte[] assign = responder.OnSessionSetup(setup)[0].ToArray();
            _output.WriteLine("port-assign: " + Convert.ToHexString(assign));

            // Echo proto DA / f1 0x0001 / counter 0x12, role 0x40, from TADADM 0x0156 to 100's port
            // 0x02F7; trailer assigns our system 0x67 and session port 0x0211.
            byte[] expected = Convert.FromHexString(
                "2113000E00640067000104" + "00DA" + "12" + "2100" + "86" + "40"
                + "0064" + "02F7" + "0067" + "0156" + "04000000" + "00" + "18"
                + "00" + "0705" + "00006702" + "11" + "1F03" + "4C0000"
                + "00" + "0B02" + "0300" + "1502" + "0108" + "FF00");

            Assert.Equal(expected, assign);
        }

        [Fact]
        public void SessionBringup_EmitsDummOnDerivedChannel()
        {
            byte[] connectBytes = Convert.FromHexString(
                "2113000E0067006400000400DA13210086E400670000006402F7040000410010FF072A54414441444D00FE0444313033");
            TadTerminalResponder responder = new TadTerminalResponder(103, () => new DateTime(2026, 7, 2));
            // Enable the post-port-assign bring-up. It now COMPUTES the session-data channel from the
            // envelope model (VERIFIED), instead of replaying a canned session's channels (which had
            // the wrong Base and crashed 100 with XXPER). Isolation: only the first frame (DUMM).
            responder.SendTerminalBringup = true;
            responder.OnConnect(XmsgFrame.Parse(connectBytes));

            byte[] setupBytes = Convert.FromHexString(
                "2113000E0067006400010400DA122100868400670156006402F704000000000906001B001C0100FF00");
            System.Collections.Generic.IReadOnlyList<XmsgFrame> frames =
                responder.OnSessionSetup(XmsgFrame.Parse(setupBytes));

            // port-assign + exactly one bring-up frame (the DUMM).
            Assert.Equal(2, frames.Count);

            byte[] dumm = frames[1].ToArray();
            _output.WriteLine("dumm: " + Convert.ToHexString(dumm));

            // DUMM built on the DERIVED channel: Flags1 0x0131 + Counter 0xDB = Base 0x020C,
            // XMCSM 0x01080000 -> channel 0xDE-1-2 = DB. Addressed to 100 from our session port 0x0211.
            byte[] expected = Convert.FromHexString(
                "2113000E00640067" + "0131" + "0108" + "DB" + "DB" + "2100" + "92" + "00"
                + "0064" + "02F7" + "0067" + "0211" + "01080000" + "00" + "02" + "1800");
            Assert.Equal(expected, dumm);

            // Sanity on the derived channel byte (offset 12) and the preserved TAD trailer.
            Assert.Equal((byte)SintranProtocolId.Db, dumm[12]);
            Assert.EndsWith("1800", Convert.ToHexString(dumm));
            Assert.Equal(dumm.Length - 32, dumm[31]);         // XMLEN (offset 31) == trailer length
        }
    }
}
