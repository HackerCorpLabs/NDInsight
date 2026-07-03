using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Live;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// Layer 2 proof: the driven LAPB link answers SABM with UA and evolves V(S)/V(R)
    /// exactly as the captured link does.
    /// </summary>
    public sealed class LapbLinkTests
    {
        /// <summary>
        /// Connect() must emit a SABM carrying this node's number and move to SabmSent,
        /// then a received UA completes the link.
        /// </summary>
        [Fact]
        public void Connect_EmitsSabm_ThenUaConnects()
        {
            LapbLink link = new LapbLink(ownNode: 100);
            List<byte[]> sent = new List<byte[]>();
            link.OnTransmit += delegate (byte[] body) { sent.Add(body); };

            link.Connect(currentTicks: 0);

            Assert.Equal(LapbLinkState.SabmSent, link.State);
            Assert.Single(sent);
            // SABM: addr 0x01, ctrl 0x3F, info = node 100 (00 64).
            Assert.Equal(new byte[] { 0x01, 0x3F, 0x00, 0x64 }, sent[0]);

            // Peer's UA (addr 01 ctrl 73 info 00 64) completes the link.
            link.OnFrameReceived(MakeFrame(0x01, 0x73, new byte[] { 0x00, 0x64 }));
            Assert.Equal(LapbLinkState.Connected, link.State);
            Assert.Equal(0, link.SendVariable);
            Assert.Equal(0, link.ReceiveVariable);
        }

        /// <summary>
        /// Replays a captured link from node 102's perspective: peer SABM -&gt; UA reply and
        /// Connected, then two in-order I-frames advance V(R) 0 -&gt; 1 -&gt; 2 with an RR ack each.
        /// </summary>
        [Fact]
        public void ReplayCapturedLink_AnswersSabm_AndAdvancesReceiveVariable()
        {
            LapbLink link = new LapbLink(ownNode: 102);
            List<byte[]> sent = new List<byte[]>();
            List<byte[]> delivered = new List<byte[]>();
            link.OnTransmit += delegate (byte[] body) { sent.Add(body); };
            link.OnInformation += delegate (ReadOnlyMemory<byte> info) { delivered.Add(info.ToArray()); };

            // 1) Peer initiates: SABM addr 01 ctrl 3F info 00 64 (node 100).
            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));

            Assert.Equal(LapbLinkState.Connected, link.State);
            Assert.Equal(0, link.SendVariable);
            Assert.Equal(0, link.ReceiveVariable);
            // The ND data link is a SYMMETRIC balanced link: both stations issue their OWN SABM.
            // VERIFIED from device-online-100-102-103.pcapng / start-li-li-1err.pcapng — the
            // initiator keeps re-sending SABM until it RECEIVES the answerer's own SABM, so the
            // answer to a peer SABM is our own SABM FIRST, then UA, then RR (the observed on-wire
            // order SABM -> UA -> RR). All three carry our node number 0x0066.
            Assert.Equal(3, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x3F, 0x00, 0x66 }, sent[0]);   // our own SABM
            Assert.Equal(new byte[] { 0x01, 0x73, 0x00, 0x66 }, sent[1]);   // UA
            Assert.Equal(new byte[] { 0x09, 0x01, 0x00, 0x66 }, sent[2]);   // RR N(R)=0

            sent.Clear();

            // 2) In-order I-frame N(S)=0, N(R)=0 -> ctrl 0x00, carrying the reachability request.
            byte[] reqInfo = LiveTestHex.Parse("21 13 00 19 00 66 00 64 FF FF 00 01 DE 08");
            link.OnFrameReceived(MakeFrame(0x09, 0x00, reqInfo));

            Assert.Equal(1, link.ReceiveVariable);          // V(R): 0 -> 1
            Assert.Single(delivered);
            Assert.Equal(reqInfo, delivered[0]);
            // The link acks with an RR carrying the new N(R)=1: ctrl 0x01 | (1<<5) = 0x21.
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x21, 0x00, 0x66 }, sent[0]);

            sent.Clear();

            // 3) Next in-order I-frame N(S)=1, N(R)=1 -> ctrl = (1<<5)|(1<<1) = 0x22.
            byte[] dataInfo = LiveTestHex.Parse("21 13 00 0E 00 66 00 64 00 00 04 00 DE");
            link.OnFrameReceived(MakeFrame(0x09, 0x22, dataInfo));

            Assert.Equal(2, link.ReceiveVariable);          // V(R): 1 -> 2
            Assert.Equal(2, delivered.Count);
            // RR now carries N(R)=2: ctrl 0x01 | (2<<5) = 0x41.
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x41, 0x00, 0x66 }, sent[0]);
        }

        /// <summary>
        /// Anti-flood: while the link is establishing (not yet synced), repeated peer SABMs must
        /// trigger our OWN SABM only ONCE. Re-emitting our SABM per received SABM is what made
        /// machine 100 restart the link on every frame ("XROUT: Link restarted" flood) — our SABM
        /// restarts the peer, the peer re-SABMs, and the two spin forever. Subsequent SABMs during
        /// the same unsynced episode get UA + RR only.
        /// </summary>
        [Fact]
        public void RepeatedSabm_DuringEstablishment_EmitsOwnSabmOnce_NoFlood()
        {
            LapbLink link = new LapbLink(ownNode: 102);
            List<byte[]> sent = new List<byte[]>();
            link.OnTransmit += delegate (byte[] body) { sent.Add(body); };

            // First peer SABM -> our SABM + UA + RR (3 frames).
            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));
            Assert.Equal(3, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x3F, 0x00, 0x66 }, sent[0]);   // our SABM (once)

            sent.Clear();

            // Second peer SABM in the SAME unsynced episode -> UA + RR ONLY, no second SABM.
            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));
            Assert.Equal(2, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x73, 0x00, 0x66 }, sent[0]);   // UA
            Assert.Equal(new byte[] { 0x09, 0x01, 0x00, 0x66 }, sent[1]);   // RR
            for (int i = 0; i < sent.Count; i++)
            {
                Assert.NotEqual(ControlSabmByte, sent[i][1]);              // NO SABM re-emitted
            }
        }

        /// <summary>
        /// Resync detection: a SABM that arrives AFTER the link is synced (I-frames flowed) is a
        /// genuine re-establishment (the peer's HDLC controller was reset). It must be honoured —
        /// V(S)/V(R) reset and our own SABM re-issued — so the establishment SABM budget is
        /// re-armed and we send SABM + UA + RR again.
        /// </summary>
        [Fact]
        public void SabmAfterSync_IsTreatedAsResync_ReissuesOwnSabm()
        {
            LapbLink link = new LapbLink(ownNode: 102);
            List<byte[]> sent = new List<byte[]>();
            link.OnTransmit += delegate (byte[] body) { sent.Add(body); };

            // Establish and then sync by delivering one in-order I-frame.
            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));
            link.OnFrameReceived(MakeFrame(0x09, 0x00, new byte[] { 0x21, 0x13 }));
            Assert.Equal(1, link.ReceiveVariable);   // synced: V(R) advanced

            sent.Clear();

            // Now a fresh SABM -> genuine resync: our SABM re-issued, V(R) reset to 0.
            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));
            Assert.Equal(0, link.ReceiveVariable);   // V(R) reset by the resync
            Assert.Equal(3, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x3F, 0x00, 0x66 }, sent[0]);   // our SABM re-issued
            Assert.Equal(new byte[] { 0x01, 0x73, 0x00, 0x66 }, sent[1]);   // UA
            Assert.Equal(new byte[] { 0x09, 0x01, 0x00, 0x66 }, sent[2]);   // RR N(R)=0
        }

        /// <summary>SABM control byte (0x3F), used to assert we do NOT re-emit a SABM.</summary>
        private const byte ControlSabmByte = 0x3F;

        /// <summary>
        /// A duplicate/out-of-order I-frame (wrong N(S)) is not delivered but is still
        /// answered with an RR carrying the unchanged expected N(R).
        /// </summary>
        [Fact]
        public void OutOfOrderInformation_NotDelivered_ButAcked()
        {
            LapbLink link = new LapbLink(ownNode: 102);
            List<byte[]> delivered = new List<byte[]>();
            link.OnInformation += delegate (ReadOnlyMemory<byte> info) { delivered.Add(info.ToArray()); };

            link.OnFrameReceived(MakeFrame(0x01, 0x3F, new byte[] { 0x00, 0x64 }));   // SABM -> Connected

            // First I-frame after connect adopts the peer's sequence (N(S)=0 here) and is
            // delivered, so V(R) advances to 1.
            link.OnFrameReceived(MakeFrame(0x09, 0x00, new byte[] { 0x21, 0x13 }));
            Assert.Single(delivered);
            Assert.Equal(1, link.ReceiveVariable);
            delivered.Clear();

            // Now the expected N(S) is 1; feed N(S)=3 instead — out of order, not delivered,
            // V(R) unchanged.
            link.OnFrameReceived(MakeFrame(0x09, (byte)((0 << 5) | (3 << 1)), new byte[] { 0x21, 0x13 }));
            Assert.Empty(delivered);
            Assert.Equal(1, link.ReceiveVariable);
        }

        /// <summary>
        /// Builds a de-framable <see cref="LapbFrame"/> from an address, control byte and
        /// info field. The trailing FCS bytes are placeholders (LAPB parsing ignores them).
        /// </summary>
        /// <param name="address">
        /// The LAPB address byte.
        /// </param>
        /// <param name="control">
        /// The LAPB control byte.
        /// </param>
        /// <param name="info">
        /// The information field.
        /// </param>
        /// <returns>
        /// A parsed <see cref="LapbFrame"/>.
        /// </returns>
        private static LapbFrame MakeFrame(byte address, byte control, byte[] info)
        {
            byte[] frameBytes = new byte[2 + info.Length + 2];
            frameBytes[0] = address;
            frameBytes[1] = control;
            Array.Copy(info, 0, frameBytes, 2, info.Length);
            // Two placeholder FCS bytes; LapbFrame does not validate the FCS.
            return new LapbFrame(default, frameBytes);
        }
    }
}
