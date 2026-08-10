using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Api;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Api.Tests
{
    /// <summary>
    /// <c>XFGSM</c>, checked against ND's own worked example.
    /// </summary>
    /// <remarks>
    /// <para><b>Where the expected values come from</b></para>
    /// ND wrote the example, not us. <c>X-MESSAGE 210373L</c> section 6.3 sets up a task that opens
    /// ports 6, 9 and 3 in that order, queues one message on each, and states the answer:
    /// <c>A=7, D=2, X=1</c>. Using their arithmetic rather than ours is the whole point - the bit
    /// order is REVERSED against open order, which is the one thing about this function that is
    /// easy to get backwards and impossible to notice.
    /// <para>
    /// This function was recorded for months as "blocked on evidence" because Appendix A of the
    /// COSMOS Programmer Guide has no section for it. The release description does.
    /// </para>
    /// </remarks>
    public sealed class XmsgQueueSnapshotTests
    {
        /// <summary>
        /// Builds a kernel.
        /// </summary>
        /// <returns>
        /// A kernel with an arbitrary system number.
        /// </returns>
        private static XmsgKernel NewKernel()
        {
            return new XmsgKernel(102, 0x1234, null);
        }

        /// <summary>
        /// Queues one message of a given type on a port.
        /// </summary>
        /// <param name="kernel">
        /// The kernel.
        /// </param>
        /// <param name="target">
        /// The port to queue on.
        /// </param>
        /// <param name="sender">
        /// The port the message is sent from.
        /// </param>
        /// <param name="flags">
        /// Send flags; <c>Route</c> produces an <c>XMROU</c> message.
        /// </param>
        private static void QueueOne(
            XmsgKernel kernel, XmsgPortNumber target, XmsgPortNumber sender, XmsgSendFlags flags)
        {
            XmsgMagicNumber targetMagic;
            kernel.ConvertPortToMagic(target, out targetMagic);

            XmsgMessageIdentifier message;
            kernel.ReserveBuffer(8, XmsgBufferOptions.None, out message);

            Assert.False(kernel.Send(targetMagic, sender, flags).IsError);
        }

        /// <summary>
        /// With no ports open, every mask is empty rather than undefined.
        /// </summary>
        [Fact]
        public void NoPortsGivesEmptyMasks()
        {
            XmsgQueueSnapshot snapshot = NewKernel().GetGeneralStatusMultiple();

            Assert.Equal(0, snapshot.Queued);
            Assert.Equal(0, snapshot.RouterFirst);
            Assert.Equal(0, snapshot.ReturnedFirst);
            Assert.False(snapshot.HasMessage(0));
        }

        /// <summary>
        /// A port with nothing queued does not appear, even though it is open.
        /// </summary>
        [Fact]
        public void AnOpenButQuietPortIsNotFlagged()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber quiet;
            kernel.OpenPort(out quiet);

            Assert.Equal(0, kernel.GetGeneralStatusMultiple().Queued);
        }

        /// <summary>
        /// Bit 0 is the port opened LAST, which is the reverse of open order.
        /// </summary>
        /// <remarks>
        /// Asserted on its own, before the full example, because it is the single fact that makes
        /// every other expected value in this file what it is.
        /// </remarks>
        [Fact]
        public void BitZeroIsTheMostRecentlyOpenedPort()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber first;
            XmsgPortNumber second;
            XmsgPortNumber sender;
            kernel.OpenPort(out first);
            kernel.OpenPort(out second);
            kernel.OpenPort(out sender);

            // Queue on the FIRST-opened port only.
            QueueOne(kernel, first, sender, XmsgSendFlags.None);

            XmsgQueueSnapshot snapshot = kernel.GetGeneralStatusMultiple();

            // Three ports open, so the first-opened is bit 2 - not bit 0.
            Assert.True(snapshot.HasMessage(2));
            Assert.False(snapshot.HasMessage(0));
            Assert.Equal(1 << 2, snapshot.Queued);
        }

        /// <summary>
        /// ND's worked example, in the shape this kernel can actually produce.
        /// </summary>
        /// <remarks>
        /// <para>
        /// ND's own example uses three ports holding an ordinary message, a router message and a
        /// returned one. Two of those cannot be made through this kernel's public surface, and
        /// finding that out is worth more than the example:
        /// </para>
        ///  - <c>XMROU</c> is what XROUT sends. A local <c>Send</c> never produces one whatever
        ///    flags are passed, so there is no way to stage that arm here.
        ///  - <c>XMTRE</c> comes from closing a port, which returns its secure messages - see
        ///    <see cref="ReturnedMessageShowsAsXmtre"/>.
        /// <para>
        /// So this test keeps the part that CAN be staged honestly - reverse bit order, several
        /// ports, ordinary messages reporting as neither type - and the returned case is covered
        /// separately by the path that genuinely produces one.
        /// </para>
        /// </remarks>
        [Fact]
        public void SeveralPortsMapToReversedBitsWithOrdinaryMessages()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber openedFirst;
            XmsgPortNumber openedSecond;
            XmsgPortNumber sender;

            kernel.OpenPort(out openedFirst);
            kernel.OpenPort(out openedSecond);
            kernel.OpenPort(out sender);

            QueueOne(kernel, openedFirst, sender, XmsgSendFlags.None);
            QueueOne(kernel, openedSecond, sender, XmsgSendFlags.None);

            XmsgQueueSnapshot snapshot = kernel.GetGeneralStatusMultiple();

            // Three ports open: first-opened is bit 2, second bit 1, sender bit 0 and quiet.
            Assert.True(snapshot.HasMessage(2));
            Assert.True(snapshot.HasMessage(1));
            Assert.False(snapshot.HasMessage(0));
            Assert.Equal((1 << 2) | (1 << 1), snapshot.Queued);

            // ND's rule: XMTNO, XMHIP and XMBNC all leave BOTH type bits clear.
            Assert.Equal(0, snapshot.RouterFirst);
            Assert.Equal(0, snapshot.ReturnedFirst);
            Assert.Equal(XmsgMessageType.XMTNO, snapshot.FirstMessageType(2));
        }

        /// <summary>
        /// A message returned by closing its port shows up in the returned mask.
        /// </summary>
        /// <remarks>
        /// Closing a port returns its secure messages to their senders as <c>XMTRE</c>, which is the
        /// one path in this kernel that genuinely produces the type ND's X-register reports.
        /// </remarks>
        [Fact]
        public void ReturnedMessageShowsAsXmtre()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber sender;
            XmsgPortNumber target;
            kernel.OpenPort(out sender);
            kernel.OpenPort(out target);

            QueueOne(kernel, target, sender, XmsgSendFlags.Secure);

            // Closing the target hands its secure message back to the sender, typed XMTRE.
            kernel.ClosePort(target);

            XmsgQueueSnapshot snapshot = kernel.GetGeneralStatusMultiple();

            // Only the sender remains open, so it is bit 0.
            Assert.True(snapshot.HasMessage(0));
            Assert.Equal(XmsgMessageType.XMTRE, snapshot.FirstMessageType(0));
            Assert.Equal(1, snapshot.ReturnedFirst);
            Assert.Equal(0, snapshot.RouterFirst);
        }

        /// <summary>
        /// Only the FIRST queued message sets the type bits.
        /// </summary>
        /// <remarks>
        /// ND's example makes this explicit: once the special message at the head is received, the
        /// type bit clears on the next call even though the port still has traffic. A reading that
        /// ORed every queued message together would pass the simple cases and fail here.
        /// </remarks>
        [Fact]
        public void OnlyTheFirstQueuedMessageSetsTheTypeBits()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber sender;
            XmsgPortNumber target;
            kernel.OpenPort(out sender);
            kernel.OpenPort(out target);

            // A secure message that will come back as XMTRE, and an ordinary one behind it.
            QueueOne(kernel, target, sender, XmsgSendFlags.Secure);
            kernel.ClosePort(target);
            QueueOne(kernel, sender, sender, XmsgSendFlags.None);

            // sender is the only port left, so bit 0. Its head is the returned message.
            Assert.Equal(
                XmsgMessageType.XMTRE, kernel.GetGeneralStatusMultiple().FirstMessageType(0));

            // Take the returned message off; the ordinary one is now the head and the bit clears.
            Assert.True(kernel.Receive(sender, XmsgWaitOptions.None).Received);

            XmsgQueueSnapshot after = kernel.GetGeneralStatusMultiple();
            Assert.True(after.HasMessage(0));
            Assert.Equal(XmsgMessageType.XMTNO, after.FirstMessageType(0));
            Assert.Equal(0, after.ReturnedFirst);
        }

        /// <summary>
        /// XFRTN's returned message does NOT carry <c>XMTRE</c> today. Recorded, not asserted as
        /// correct.
        /// </summary>
        /// <remarks>
        /// <para><b>An open question, pinned so it cannot drift unnoticed</b></para>
        /// <c>ReturnMessage</c> sets the type to <c>XMTRE</c> and then calls <c>Deliver</c>, which
        /// unconditionally overwrites it with <c>XMTNO</c> or <c>XMTHI</c>. So a task that receives
        /// a message returned by <c>XFRTN</c> sees an ordinary message, while one returned by a port
        /// CLOSING sees <c>XMTRE</c>.
        /// <para>
        /// Whether that is a defect is genuinely unsettled. <c>XFRTN</c> is how a server replies to
        /// a request, and a reply is arguably ordinary traffic; but the function is named "return
        /// message" and the type is named "returned". No capture in this project shows a real
        /// machine's answer, and the COSMOS guide's section on XFRTN does not name a message type.
        /// </para>
        /// <para>
        /// So the behaviour is pinned as it stands rather than changed on a reading. If a capture
        /// later shows <c>XMTRE</c> on an XFRTN reply, this test fails and says exactly what to fix.
        /// </para>
        /// </remarks>
        [Fact]
        public void XfrtnReplyIsNotTypedReturned_Unverified()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber sender;
            XmsgPortNumber server;
            kernel.OpenPort(out sender);
            kernel.OpenPort(out server);

            QueueOne(kernel, server, sender, XmsgSendFlags.None);

            XmsgReceiveResult got = kernel.Receive(server, XmsgWaitOptions.None);
            Assert.True(got.Received);
            Assert.False(kernel.ReturnMessage(got.Message, 0, server, XmsgSendFlags.None).IsError);

            // sender was opened first of two, so bit 1.
            XmsgQueueSnapshot snapshot = kernel.GetGeneralStatusMultiple();
            Assert.True(snapshot.HasMessage(1));

            // MEASURED, not endorsed - see the remarks.
            Assert.Equal(XmsgMessageType.XMTNO, snapshot.FirstMessageType(1));
        }

        /// <summary>
        /// A bit position outside the 16 the masks carry is answered false, not indexed.
        /// </summary>
        [Theory]
        [InlineData(-1)]
        [InlineData(16)]
        [InlineData(99)]
        public void BitsOutsideTheMaskAreRefused(int bit)
        {
            XmsgQueueSnapshot snapshot = new XmsgQueueSnapshot(0xFFFF, 0xFFFF, 0xFFFF);

            Assert.False(snapshot.HasMessage(bit));
            Assert.Equal(XmsgMessageType.XMTNO, snapshot.FirstMessageType(bit));
        }
    }
}
