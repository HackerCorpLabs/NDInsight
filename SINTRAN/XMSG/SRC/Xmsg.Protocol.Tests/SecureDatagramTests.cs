using NDInsight.Sintran.Xmsg;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Tests
{
    /// <summary>
    /// Tests for the XFSEC secure-datagram delivery state machine (send + receive sides).
    /// </summary>
    public sealed class SecureDatagramTests
    {
        [Fact]
        public void Send_Timeout_ResendThreeTimes_ThenReturned()
        {
            // timeout = 10 ticks (XTUs), default 3 resends.
            SecureDatagramSession session = new SecureDatagramSession(timeoutTicks: 10);

            int resendCalls = 0;
            int returnedCalls = 0;
            XmsgError returnedReason = XmsgError.XENOT;
            session.OnResend += delegate (ushort seq, int attempt) { resendCalls++; };
            session.OnReturned += delegate (ushort seq, XmsgError reason)
            {
                returnedCalls++;
                returnedReason = reason;
            };

            session.Send(0x0007, currentTicks: 0);
            Assert.Equal(SecureDatagramState.AwaitingAck, session.State);

            // Before the timeout elapses nothing happens.
            Assert.False(session.Tick(5));
            Assert.Equal(SecureDatagramState.AwaitingAck, session.State);

            // Resend #1, #2, #3 at successive timeout boundaries.
            Assert.True(session.Tick(10));
            Assert.Equal(1, session.ResendCount);
            Assert.True(session.Tick(20));
            Assert.Equal(2, session.ResendCount);
            Assert.True(session.Tick(30));
            Assert.Equal(3, session.ResendCount);
            Assert.Equal(SecureDatagramState.AwaitingAck, session.State);

            // Fourth timeout with no resends left -> Returned.
            Assert.True(session.Tick(40));
            Assert.Equal(SecureDatagramState.Returned, session.State);
            Assert.True(session.IsTerminal);

            Assert.Equal(3, resendCalls);
            Assert.Equal(1, returnedCalls);
            Assert.Equal(XmsgError.XENTO, returnedReason);
            Assert.Equal(XmsgError.XENTO, session.ReturnReason);
            Assert.Equal(XmsgMessageType.XMTRE, session.ReturnMessageType);
        }

        [Fact]
        public void Send_AckMatchingSequence_Delivered()
        {
            SecureDatagramSession session = new SecureDatagramSession(timeoutTicks: 10);

            bool deliveredRaised = false;
            session.OnDelivered += delegate (ushort seq) { deliveredRaised = true; };

            session.Send(0x0041, currentTicks: 0);

            // A non-matching ACK leaves it outstanding.
            Assert.False(session.AckReceived(0x0040));
            Assert.Equal(SecureDatagramState.AwaitingAck, session.State);

            // The matching ACK delivers.
            Assert.True(session.AckReceived(0x0041));
            Assert.Equal(SecureDatagramState.Delivered, session.State);
            Assert.True(deliveredRaised);

            // Further timeouts do nothing once terminal.
            Assert.False(session.Tick(1000));
        }

        [Fact]
        public void Nack_RequestsRetransmit_ThenReturnsOnExhaustion()
        {
            SecureDatagramSession session = new SecureDatagramSession(timeoutTicks: 10, maxResends: 2);

            session.Send(0x0009, currentTicks: 0);

            Assert.True(session.Nack(1));   // resend #1
            Assert.Equal(1, session.ResendCount);
            Assert.True(session.Nack(2));   // resend #2
            Assert.Equal(2, session.ResendCount);

            // Budget exhausted: XEREJ return.
            Assert.False(session.Nack(3));
            Assert.Equal(SecureDatagramState.Returned, session.State);
            Assert.Equal(XmsgError.XEREJ, session.ReturnReason);
        }

        [Fact]
        public void Receiver_DeliversAndEmitsAck_EchoingFlags1()
        {
            // A data frame arrives 100 -> 102 with datagram sequence 7.
            XmsgFrame data = new XmsgFrameBuilder()
                .Between(102, 100)
                .WithDatagramSequence(7)
                .WithProtocol(SintranProtocolId.Routing)
                .WithEndpoints(102, 0x02C1, 100, 0x02AD)
                .WithControlService(0x0100014B)
                .WithBody(new XroutMessageBuilder().WithSerial(1).WithService(XroutService.XSNUL).Build())
                .Build();

            SecureDatagramReceiver receiver = new SecureDatagramReceiver(initialCounter: 0x2D);

            ushort deliveredSeq = 0;
            bool received = false;
            receiver.OnReceived += delegate (ushort seq) { received = true; deliveredSeq = seq; };

            XmsgFrame ack = receiver.ReceiveDataFrame(data);

            Assert.True(received);
            Assert.Equal(7, deliveredSeq);

            // ACK properties per XMSG-PROTOCOL.md section 6.
            Assert.Equal(SintranPacketSubtype.Ack, ack.Header.Subtype);
            Assert.Equal(7, ack.Header.Flags1);                 // echoes the datagram sequence
            Assert.Equal(0x0001, ack.Header.Flags2);
            Assert.Equal(100, ack.Header.DestinationNode);      // opposite direction
            Assert.Equal(102, ack.Header.SourceNode);
            Assert.Equal(SintranProtocolId.Routing, ack.Header.ProtocolId);

            // Trailing byte is the receiver's per-direction counter, which decrements.
            Assert.Single(ack.TrailingBytes);
            Assert.Equal(0x2D, ack.TrailingBytes[0]);
            Assert.Equal(0x2C, receiver.Counter);
        }

        [Fact]
        public void Receiver_AckChannel_StepsDownWhenCounterWraps()
        {
            // CAPTURE-VERIFIED (multiple-connect-100-to102 session 2): the session ACK channel is
            // connect+4 (here 0xDE) but steps DOWN by one each time the decrementing ACK counter wraps
            // past 0x00 - counter 0x00 is still on 0xDE, counter 0xFF is already on 0xDD. Emitting the
            // wrapped ACK on the un-stepped 0xDE is what 24B/XXPER-crashed 100 at a long-session
            // teardown; this test locks the step in.
            XmsgFrame data = new XmsgFrameBuilder()
                .Between(102, 100)
                .WithDatagramSequence(0x1F)
                .WithProtocol(SintranProtocolId.Dc)
                .WithEndpoints(102, 0x0211, 100, 0x02E3)
                .WithControlService(0x01080000)
                .WithBody(new XroutMessageBuilder().WithSerial(1).WithService(XroutService.XSNUL).Build())
                .Build();

            // Seed low so the counter reaches 0x00 after three ACKs, then wraps.
            SecureDatagramReceiver receiver = new SecureDatagramReceiver(initialCounter: 0x02);

            // counter 0x02, 0x01, 0x00 - all still on the base channel 0xDE.
            for (int i = 0; i < 3; i++)
            {
                XmsgFrame ack = receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
                Assert.Equal(SintranProtocolId.Routing, ack.Header.ProtocolId);   // 0xDE
            }

            // The counter has now wrapped 0x00 -> 0xFF: this and the next ACK ride 0xDD (0xDE - 1).
            XmsgFrame wrapped = receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
            Assert.Equal(0xFF, wrapped.TrailingBytes[0]);
            Assert.Equal(SintranProtocolId.Tad, wrapped.Header.ProtocolId);       // 0xDD

            XmsgFrame afterWrap = receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
            Assert.Equal(0xFE, afterWrap.TrailingBytes[0]);
            Assert.Equal(SintranProtocolId.Tad, afterWrap.Header.ProtocolId);     // 0xDD
        }

        [Fact]
        public void Receiver_SeedCounter_ResetsWrapStep()
        {
            // A new session re-seeds the counter (connect+0x0A) and MUST reset the wrap step, so a
            // prior session's wraps never carry over and mis-step this session's ACK channel.
            XmsgFrame data = new XmsgFrameBuilder()
                .Between(102, 100)
                .WithDatagramSequence(0x05)
                .WithProtocol(SintranProtocolId.Dc)
                .WithEndpoints(102, 0x0211, 100, 0x02E3)
                .WithControlService(0x01080000)
                .WithBody(new XroutMessageBuilder().WithSerial(1).WithService(XroutService.XSNUL).Build())
                .Build();

            SecureDatagramReceiver receiver = new SecureDatagramReceiver(initialCounter: 0x00);

            // First ACK (counter 0x00) is on 0xDE and wraps; the second would be 0xDD.
            receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
            XmsgFrame wrapped = receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
            Assert.Equal(SintranProtocolId.Tad, wrapped.Header.ProtocolId);       // 0xDD after wrap

            // Re-seed: the next ACK must be back on the base 0xDE, wrap step cleared.
            receiver.SeedCounter(0x0E);
            XmsgFrame fresh = receiver.ReceiveDataFrame(data, SintranProtocolId.Routing);
            Assert.Equal(0x0E, fresh.TrailingBytes[0]);
            Assert.Equal(SintranProtocolId.Routing, fresh.Header.ProtocolId);     // 0xDE again
        }
    }
}
