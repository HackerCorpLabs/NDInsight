using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Live;

using Xunit;

using static NDInsight.Sintran.Xmsg.Live.Tests.LapbTestKit;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// Conformance proof for the ND LAPB state machine (spec sections 3-6): balanced bring-up,
    /// hard reset on SABM (no adoption), cumulative acknowledgement with V(A), the transmit window
    /// with go-back-N, RNR flow control, the P/F exchange, per-node stamping and neighbour capture,
    /// and independence of multiple links. Every emitted frame is asserted byte-for-byte and every
    /// sequence variable is checked after each event; expected bytes are derived from the spec's
    /// encode rules, not from the implementation.
    /// </summary>
    public sealed class LapbLayerTests
    {
        /// <summary>
        /// Connect emits a SABM (P=1) carrying our node number and enters SABM_SENT (spec 3.1).
        /// </summary>
        [Fact]
        public void Connect_EmitsSabmP1WithNode_EntersSabmSent()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewLink(100, sent);

            link.Connect(currentTicks: 0);

            Assert.Equal(LapbLayerState.SabmSent, link.State);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x01, 0x3F, Node100Hi, Node100Lo }, sent[0]);   // SABM P=1 + node 100
        }

        /// <summary>
        /// A UA answering our SABM completes the link, hard-zeroes the variables, learns the peer id,
        /// and pushes an RR so the peer advances to RUN (spec 3.1 / 3.2 / 3.4).
        /// </summary>
        [Fact]
        public void RxUa_Connects_ResetsVariables_EmitsRr_LearnsPeer()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewLink(100, sent);
            link.Connect(currentTicks: 0);
            sent.Clear();

            Deliver(link, 0x01, 0x73, Node100Hi, Node100Lo);   // peer UA + its node (here also 100 for the vector)

            Assert.Equal(LapbLayerState.Connected, link.State);
            Assert.Equal(0, link.SendVariable);
            Assert.Equal(0, link.ReceiveVariable);
            Assert.Equal(0, link.AcknowledgeVariable);
            Assert.Equal(0x0064, link.PeerNode);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x01, Node100Hi, Node100Lo }, sent[0]);   // RR N(R)=0 + our node
        }

        /// <summary>
        /// A passive station answers a received SABM with UA then RR (no reflexive SABM), resets, and
        /// learns the peer id (spec 6.3 rx-SABM row; our own SABM comes only from Connect).
        /// </summary>
        [Fact]
        public void RxSabm_AnswersUaThenRr_NoReflexiveSabm_LearnsPeer()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewLink(102, sent);

            Deliver(link, 0x01, 0x3F, Node100Hi, Node100Lo);   // peer (node 100) SABM

            Assert.Equal(LapbLayerState.Connected, link.State);
            Assert.Equal(0, link.ReceiveVariable);
            Assert.Equal(0x0064, link.PeerNode);
            Assert.Equal(2, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x73, Node102Hi, Node102Lo }, sent[0]);   // UA F=1 + our node 102
            Assert.Equal(new byte[] { 0x09, 0x01, Node102Hi, Node102Lo }, sent[1]);   // RR N(R)=0 + our node 102
            for (int i = 0; i < sent.Count; i++)
            {
                Assert.NotEqual(0x3F, sent[i][1]);   // NO reflexive SABM
            }
        }

        /// <summary>
        /// Two in-sequence I-frames are delivered and each acknowledged with an RR carrying the new
        /// V(R); V(R) advances 0 -&gt; 1 -&gt; 2 (spec 4.4).
        /// </summary>
        [Fact]
        public void InSequenceInformation_Delivered_AdvancesVr_AcksRr()
        {
            List<byte[]> sent = new List<byte[]>();
            List<byte[]> got = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, got);
            sent.Clear();

            byte[] info0 = new byte[] { 0x21, 0x13, 0xAA };
            Deliver(link, 0x09, 0x00, info0);            // I N(S)=0 N(R)=0
            Assert.Equal(1, link.ReceiveVariable);
            Assert.Single(got);
            Assert.Equal(info0, got[0]);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x21, Node102Hi, Node102Lo }, sent[0]);   // RR N(R)=1
            sent.Clear();

            byte[] info1 = new byte[] { 0x21, 0x13, 0xBB };
            Deliver(link, 0x09, 0x02, info1);            // I N(S)=1 N(R)=0
            Assert.Equal(2, link.ReceiveVariable);
            Assert.Equal(2, got.Count);
            Assert.Equal(info1, got[1]);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x41, Node102Hi, Node102Lo }, sent[0]);   // RR N(R)=2
        }

        /// <summary>
        /// A retransmitted already-accepted I-frame is not re-delivered; it is re-acknowledged with an
        /// RR carrying the unchanged V(R) (spec 4.5 duplicate).
        /// </summary>
        [Fact]
        public void DuplicateInformation_NotDelivered_ReAcked()
        {
            List<byte[]> sent = new List<byte[]>();
            List<byte[]> got = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, got);
            Deliver(link, 0x09, 0x00, new byte[] { 0x21, 0x13 });   // deliver N(S)=0 -> V(R)=1
            Assert.Single(got);
            sent.Clear();

            Deliver(link, 0x09, 0x00, new byte[] { 0x21, 0x13 });   // N(S)=0 again: duplicate (behind V(R)=1)

            Assert.Single(got);                                     // not re-delivered
            Assert.Equal(1, link.ReceiveVariable);                  // V(R) unchanged
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x21, Node102Hi, Node102Lo }, sent[0]);   // RR N(R)=1
        }

        /// <summary>
        /// A gap yields exactly one REJ and latches the reject condition; a further ahead frame is
        /// dropped silently; the awaited in-sequence frame then clears the condition (spec 4.5, S5).
        /// </summary>
        [Fact]
        public void Gap_EmitsSingleRej_NoStorm_ThenClears()
        {
            List<byte[]> sent = new List<byte[]>();
            List<byte[]> got = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, got);   // V(R)=0
            sent.Clear();

            Deliver(link, 0x09, 0x02, new byte[] { 0x21, 0x13 });   // I N(S)=1 (ahead of V(R)=0): gap
            Assert.True(link.RejectCondition);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x09, Node102Hi, Node102Lo }, sent[0]);   // REJ N(R)=0 (nibble 0x9)
            sent.Clear();

            Deliver(link, 0x09, 0x04, new byte[] { 0x21, 0x13 });   // I N(S)=2: still ahead -> silent
            Assert.Empty(sent);
            Assert.Empty(got);

            Deliver(link, 0x09, 0x00, new byte[] { 0x21, 0x13 });   // I N(S)=0: the awaited frame
            Assert.False(link.RejectCondition);
            Assert.Single(got);
            Assert.Equal(1, link.ReceiveVariable);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x21, Node102Hi, Node102Lo }, sent[0]);   // RR N(R)=1
        }

        /// <summary>
        /// Sending an information field emits an I-frame, advances V(S), and marks it outstanding; a
        /// later RR acknowledges it and advances V(A) (spec 4.2 / 4.3).
        /// </summary>
        [Fact]
        public void SendInformation_EmitsIFrame_AdvancesVs_ThenRrAdvancesVa()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null);
            sent.Clear();

            byte[] payload = new byte[] { 0x21, 0x13, 0xCC };
            link.SendInformation(payload, currentTicks: 0);

            Assert.Equal(1, link.SendVariable);
            Assert.Equal(0, link.AcknowledgeVariable);
            Assert.Equal(1, link.Outstanding);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x00, 0x21, 0x13, 0xCC }, sent[0]);   // I N(S)=0 N(R)=0 + payload
            sent.Clear();

            Deliver(link, 0x09, 0x21, Node100Hi, Node100Lo);   // RR N(R)=1 from peer
            Assert.Equal(1, link.AcknowledgeVariable);
            Assert.Equal(0, link.Outstanding);
        }

        /// <summary>
        /// A REJ triggers go-back-N: both outstanding I-frames are retransmitted in order, byte-identical
        /// to the originals (spec 4.6, S2).
        /// </summary>
        [Fact]
        public void Rej_GoBackN_RetransmitsAllOutstanding()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null);
            sent.Clear();

            link.SendInformation(new byte[] { 0xD0 }, currentTicks: 0);   // I N(S)=0
            link.SendInformation(new byte[] { 0xD1 }, currentTicks: 0);   // I N(S)=1
            Assert.Equal(2, link.Outstanding);
            byte[] i0 = new byte[] { 0x09, 0x00, 0xD0 };
            byte[] i1 = new byte[] { 0x09, 0x02, 0xD1 };
            Assert.Equal(i0, sent[0]);
            Assert.Equal(i1, sent[1]);
            sent.Clear();

            Deliver(link, 0x09, 0x09, Node100Hi, Node100Lo);   // REJ N(R)=0

            Assert.Equal(2, sent.Count);
            Assert.Equal(i0, sent[0]);   // resent N(S)=0
            Assert.Equal(i1, sent[1]);   // resent N(S)=1
            Assert.Equal(2, link.SendVariable);
        }

        /// <summary>
        /// RNR sets peer-busy and holds new I-frames; a following RR clears busy and releases the held
        /// frame (spec 4.7, S3).
        /// </summary>
        [Fact]
        public void Rnr_HoldsSends_ThenRrResumes()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null);
            sent.Clear();

            link.SendInformation(new byte[] { 0xE0 }, currentTicks: 0);   // I N(S)=0
            Assert.Single(sent);
            sent.Clear();

            Deliver(link, 0x09, 0x25, Node100Hi, Node100Lo);   // RNR N(R)=1 (nibble 0x5)
            Assert.True(link.PeerBusy);
            Assert.Equal(1, link.AcknowledgeVariable);

            link.SendInformation(new byte[] { 0xE1 }, currentTicks: 0);   // must be held
            Assert.Empty(sent);

            Deliver(link, 0x09, 0x21, Node100Hi, Node100Lo);   // RR N(R)=1 clears busy
            Assert.False(link.PeerBusy);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x02, 0xE1 }, sent[0]);   // held I N(S)=1 now sent
        }

        /// <summary>
        /// The transmit window blocks a (k+1)-th send until an ack opens a slot (spec 4.2), here with
        /// k = 2.
        /// </summary>
        [Fact]
        public void Window_BlocksBeyondK_ResumesOnAck()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null, new LapbOptions(windowSize: 2));
            sent.Clear();

            link.SendInformation(new byte[] { 0xA0 }, currentTicks: 0);   // N(S)=0
            link.SendInformation(new byte[] { 0xA1 }, currentTicks: 0);   // N(S)=1
            link.SendInformation(new byte[] { 0xA2 }, currentTicks: 0);   // blocked by window (k=2)
            Assert.Equal(2, sent.Count);
            Assert.Equal(2, link.SendVariable);

            Deliver(link, 0x09, 0x21, Node100Hi, Node100Lo);   // RR N(R)=1 frees a slot

            Assert.Equal(3, sent.Count);
            Assert.Equal(new byte[] { 0x09, 0x04, 0xA2 }, sent[2]);   // N(S)=2 now sent
            Assert.Equal(3, link.SendVariable);
        }

        /// <summary>
        /// A received P=1 is answered promptly with an RR carrying F=1 and the current N(R) (spec 4.8, S6).
        /// </summary>
        [Fact]
        public void ReceivedPoll_AnsweredWithFinal()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null);
            sent.Clear();

            Deliver(link, 0x09, 0x11, Node100Hi, Node100Lo);   // RR P=1 N(R)=0 (0x01 | 0x10)

            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x11, Node102Hi, Node102Lo }, sent[0]);   // RR F=1 N(R)=0
        }

        /// <summary>
        /// A SABM received mid-session resets the variables and queue but stays CONNECTED (UA + reset),
        /// and the next I-frame N(S)=0 is delivered afresh (spec 3.2 highest-risk deviation, S7).
        /// </summary>
        [Fact]
        public void MidSessionSabm_ResetsButStaysConnected()
        {
            List<byte[]> sent = new List<byte[]>();
            List<byte[]> got = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, got);

            Deliver(link, 0x09, 0x00, new byte[] { 0x21, 0x13 });   // advance V(R) to 1
            link.SendInformation(new byte[] { 0x55 }, currentTicks: 0); // advance V(S) to 1
            Assert.Equal(1, link.SendVariable);
            Assert.Equal(1, link.ReceiveVariable);
            sent.Clear();
            got.Clear();

            Deliver(link, 0x01, 0x3F, Node100Hi, Node100Lo);   // mid-session SABM

            Assert.Equal(LapbLayerState.Connected, link.State);   // stayed up
            Assert.Equal(0, link.SendVariable);
            Assert.Equal(0, link.ReceiveVariable);
            Assert.Equal(0, link.AcknowledgeVariable);
            Assert.Equal(2, sent.Count);
            Assert.Equal(new byte[] { 0x01, 0x73, Node102Hi, Node102Lo }, sent[0]);   // UA
            Assert.Equal(new byte[] { 0x09, 0x01, Node102Hi, Node102Lo }, sent[1]);   // RR N(R)=0

            Deliver(link, 0x09, 0x00, new byte[] { 0x21, 0x13 });   // next I N(S)=0 delivered afresh
            Assert.Single(got);
            Assert.Equal(1, link.ReceiveVariable);
        }

        /// <summary>
        /// A DISC received while connected is answered with UA and the link goes DISCONNECTED (spec 4.9).
        /// </summary>
        [Fact]
        public void RxDisc_WhenConnected_AnswersUa_Disconnects()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null);
            sent.Clear();

            Deliver(link, 0x01, 0x53, Node100Hi, Node100Lo);   // DISC P=1

            Assert.Equal(LapbLayerState.Disconnected, link.State);
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x01, 0x73, Node102Hi, Node102Lo }, sent[0]);   // UA (F=1 mirrors P)
        }

        /// <summary>
        /// Two independent links keep separate sequence state, node stamping and neighbour ids; driving
        /// one never affects the other (spec 1.2 one-machine-per-link).
        /// </summary>
        [Fact]
        public void MultipleTrunks_KeepIndependentSequenceAndIdentity()
        {
            List<byte[]> sentA = new List<byte[]>();
            List<byte[]> sentB = new List<byte[]>();
            LapbLayer linkA = NewConnected(102, sentA, null);   // neighbour 100 by NewConnected
            LapbLayer linkB = NewConnected(103, sentB, null);
            sentA.Clear();
            sentB.Clear();

            // Drive A twice (V(R) 0 -> 2), B once (V(R) 0 -> 1). No cross-talk.
            Deliver(linkA, 0x09, 0x00, new byte[] { 0x21, 0x13 });
            Deliver(linkA, 0x09, 0x02, new byte[] { 0x21, 0x13 });
            Deliver(linkB, 0x09, 0x00, new byte[] { 0x21, 0x13 });

            Assert.Equal(2, linkA.ReceiveVariable);
            Assert.Equal(1, linkB.ReceiveVariable);
            Assert.Equal(0x0064, linkA.PeerNode);
            Assert.Equal(0x0064, linkB.PeerNode);

            // Each stamps its OWN node number on its RR acks.
            Assert.Equal(new byte[] { 0x09, 0x41, Node102Hi, Node102Lo }, sentA[sentA.Count - 1]);   // node 102
            Assert.Equal(new byte[] { 0x09, 0x21, 0x00, 0x67 }, sentB[sentB.Count - 1]);             // node 103
        }
    }
}
