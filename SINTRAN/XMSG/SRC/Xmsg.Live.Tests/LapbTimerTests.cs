using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Live;

using Xunit;

using static NDInsight.Sintran.Xmsg.Live.Tests.LapbTestKit;

namespace NDInsight.Sintran.Xmsg.Live.Tests
{
    /// <summary>
    /// Timer proof for the LAPB state machine (spec section 5): T1 retransmission/poll, N2 exhaustion
    /// with auto re-establish, and the T3 idle keepalive, all driven by injected time. Also covers
    /// <see cref="LapbOptions"/> validation and that custom timer values change the timing.
    /// </summary>
    public sealed class LapbTimerTests
    {
        // Fast timers so the injected clock stays readable: T1 = 100, T3 = 1000, N2 = 3.
        private static LapbOptions Fast()
        {
            return new LapbOptions(t1: 100, t3: 1000, n2: 3);
        }

        /// <summary>
        /// T1 does not fire before its deadline and, on expiry with a frame outstanding, polls the peer
        /// with RR (P=1) (spec 5.1 poll-first).
        /// </summary>
        [Fact]
        public void T1_ExpiryWithOutstanding_PollsRrP1()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null, Fast());
            link.SendInformation(new byte[] { 0x55 }, currentTicks: 0);   // arms T1 at deadline 100
            sent.Clear();

            Assert.False(link.Tick(99));    // before the deadline: nothing
            Assert.Empty(sent);

            Assert.True(link.Tick(100));    // at the deadline: poll
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x11, Node102Hi, Node102Lo }, sent[0]);   // RR P=1 N(R)=0
        }

        /// <summary>
        /// T1 expiry while in SABM_SENT retransmits the SABM (P=1) (spec 5.1).
        /// </summary>
        [Fact]
        public void T1_ExpiryInSabmSent_RetransmitsSabm()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewLink(102, sent, Fast());
            link.Connect(currentTicks: 0);   // SABM, arms T1 at 100
            sent.Clear();

            Assert.True(link.Tick(100));
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x01, 0x3F, Node102Hi, Node102Lo }, sent[0]);   // resent SABM P=1
        }

        /// <summary>
        /// Exceeding N2 retransmissions declares the link failed and auto re-establishes from CONNECTED
        /// with a fresh SABM (spec 5.1 step 1, S4).
        /// </summary>
        [Fact]
        public void N2_Exhaustion_AutoReEstablishes()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null, Fast());   // N2 = 3
            link.SendInformation(new byte[] { 0x55 }, currentTicks: 0);
            sent.Clear();

            // Each expiry increments the retry counter and restarts T1 (deadline now + 100).
            Assert.True(link.Tick(100));   // retry 1
            Assert.True(link.Tick(200));   // retry 2
            Assert.True(link.Tick(300));   // retry 3
            sent.Clear();
            Assert.True(link.Tick(400));   // retry 4 > N2 -> re-establish

            Assert.Equal(LapbLayerState.SabmSent, link.State);
            Assert.Equal(0, link.SendVariable);   // Reset cleared outstanding
            Assert.Contains(new byte[] { 0x01, 0x3F, Node102Hi, Node102Lo }, sent);   // fresh SABM P=1
        }

        /// <summary>
        /// T3 expiry while connected and idle sends an RR (P=1) keepalive poll (spec 5.2).
        /// </summary>
        [Fact]
        public void T3_ExpiryWhenIdle_SendsRrP1()
        {
            List<byte[]> sent = new List<byte[]>();
            LapbLayer link = NewConnected(102, sent, null, Fast());   // T3 armed at 1000 on connect
            sent.Clear();

            Assert.False(link.Tick(999));   // before T3
            Assert.Empty(sent);

            Assert.True(link.Tick(1000));   // T3 fires
            Assert.Single(sent);
            Assert.Equal(new byte[] { 0x09, 0x11, Node102Hi, Node102Lo }, sent[0]);   // RR P=1
        }

        /// <summary>
        /// Custom T1 changes the timing: a link built with the default T1 (3000) does not fire at 100,
        /// while a fast link does.
        /// </summary>
        [Fact]
        public void CustomOptions_ChangeTiming()
        {
            List<byte[]> sentFast = new List<byte[]>();
            LapbLayer fast = NewConnected(102, sentFast, null, Fast());
            fast.SendInformation(new byte[] { 0x55 }, currentTicks: 0);
            sentFast.Clear();
            Assert.True(fast.Tick(100));    // fast T1 fires
            Assert.NotEmpty(sentFast);

            List<byte[]> sentSlow = new List<byte[]>();
            LapbLayer slow = NewConnected(102, sentSlow, null);   // default T1 = 3000
            slow.SendInformation(new byte[] { 0x55 }, currentTicks: 0);
            sentSlow.Clear();
            Assert.False(slow.Tick(100));   // default T1 has not elapsed
            Assert.Empty(sentSlow);
        }

        /// <summary>
        /// <see cref="LapbOptions"/> rejects non-positive timers, a sub-1 N2, and a window outside 1..7.
        /// </summary>
        [Fact]
        public void Options_ValidateRanges()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new LapbOptions(t1: 0));
            Assert.Throws<ArgumentOutOfRangeException>(() => new LapbOptions(t3: -1));
            Assert.Throws<ArgumentOutOfRangeException>(() => new LapbOptions(n2: 0));
            Assert.Throws<ArgumentOutOfRangeException>(() => new LapbOptions(windowSize: 0));
            Assert.Throws<ArgumentOutOfRangeException>(() => new LapbOptions(windowSize: 8));
        }

        /// <summary>
        /// The default options carry the spec section 5 values.
        /// </summary>
        [Fact]
        public void Default_CarriesSpecValues()
        {
            LapbOptions d = LapbOptions.Default;
            Assert.Equal(3000, d.T1);
            Assert.Equal(30000, d.T3);
            Assert.Equal(10, d.N2);
            Assert.Equal(7, d.WindowSize);
        }
    }
}
