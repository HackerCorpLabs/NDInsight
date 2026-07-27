using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Api;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Api.Tests
{
    /// <summary>
    /// Proves the kernel obeys the rules appendix A of the COSMOS Programmer Guide lays down for
    /// ports, message buffers, currency and queueing.
    /// </summary>
    public sealed class XmsgKernelTests
    {
        /// <summary>
        /// A newly opened port becomes the default port, so the port list is newest-first.
        /// </summary>
        /// <remarks>
        /// The manual's own worked example: open 15, 4, 6, 19 and the list is 19-6-4-15, with 19
        /// the default.
        /// </remarks>
        [Fact]
        public void OpenPort_NewestBecomesTheDefault()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber first;
            XmsgPortNumber second;
            kernel.OpenPort(out first);
            kernel.OpenPort(out second);

            XmsgMagicNumber defaultMagic;
            XmsgMagicNumber secondMagic;
            kernel.ConvertPortToMagic(XmsgPortNumber.Default, out defaultMagic);
            kernel.ConvertPortToMagic(second, out secondMagic);

            Assert.Equal(secondMagic, defaultMagic);
            Assert.NotEqual(first.Value, second.Value);
        }

        /// <summary>
        /// Minted ports carry this system's number and a nine-bit one-based port number.
        /// </summary>
        [Fact]
        public void OpenPort_MintsAWellFormedMagicNumber()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber port;
            kernel.OpenPort(out port);

            XmsgMagicNumber magic;
            Assert.False(kernel.ConvertPortToMagic(port, out magic).IsError);
            Assert.Equal(102, magic.SystemNumber);
            Assert.Equal(1, magic.PortNumber);
            Assert.True(magic.HasMintableRandom);
        }

        /// <summary>
        /// A message sent between two ports of the same kernel is transferred, not copied, and
        /// arrives with the sender's magic number attached.
        /// </summary>
        [Fact]
        public void Send_LocalDelivery_TransfersTheBufferAndTheSender()
        {
            XmsgKernel kernel = NewKernel();

            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            XmsgMagicNumber clientMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);
            kernel.ConvertPortToMagic(client, out clientMagic);

            XmsgMessageIdentifier message;
            kernel.ReserveBuffer(32, XmsgBufferOptions.None, out message);
            int written;
            kernel.Write(message, new byte[] { 1, 2, 3, 4 }, 0, false, out written);

            Assert.False(kernel.Send(serverMagic, client, XmsgSendFlags.None).IsError);

            XmsgReceiveResult received = kernel.Receive(server, XmsgWaitOptions.None);
            Assert.True(received.Received);
            Assert.Equal(XmsgMessageType.XMTNO, received.MessageType);
            Assert.Equal(4, received.Length);

            XmsgMessageStatus status = kernel.GetMessageStatus(received.Message);
            Assert.Equal(clientMagic, status.Sender);

            byte[] readBack = new byte[4];
            int read;
            kernel.Read(received.Message, readBack, 0, out read);
            Assert.Equal(new byte[] { 1, 2, 3, 4 }, readBack);
        }

        /// <summary>
        /// Receiving on an empty port is not an error - it is the not-terminated status.
        /// </summary>
        [Fact]
        public void Receive_EmptyQueue_IsNotTerminated()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber port;
            kernel.OpenPort(out port);

            XmsgReceiveResult result = kernel.Receive(port, XmsgWaitOptions.None);

            Assert.False(result.Received);
            Assert.True(result.Status.IsNotTerminated);
            Assert.False(result.Status.IsError);
        }

        /// <summary>
        /// A high-priority message is queued ahead of normal ones.
        /// </summary>
        [Fact]
        public void Send_HighPriority_JumpsTheQueue()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);

            SendBytes(kernel, client, serverMagic, new byte[] { 0x11 }, XmsgSendFlags.None);
            SendBytes(kernel, client, serverMagic, new byte[] { 0x22 }, XmsgSendFlags.HighPriority);

            XmsgReceiveResult first = kernel.Receive(server, XmsgWaitOptions.None);
            Assert.Equal(XmsgMessageType.XMTHI, first.MessageType);

            byte[] one = new byte[1];
            int read;
            kernel.Read(first.Message, one, 0, out read);
            Assert.Equal(0x22, one[0]);
        }

        /// <summary>
        /// The high-priority option on a status call reports only high-priority messages.
        /// </summary>
        [Fact]
        public void GetPortStatus_HighPriorityOption_IgnoresNormalMessages()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);
            SendBytes(kernel, client, serverMagic, new byte[] { 1 }, XmsgSendFlags.None);

            XmsgPortStatus any = kernel.GetPortStatus(server, XmsgWaitOptions.None);
            Assert.True(any.HasMessage);
            Assert.Equal(1, any.QueueLength);

            XmsgPortStatus high = kernel.GetPortStatus(server, XmsgWaitOptions.HighPriority);
            Assert.False(high.HasMessage);
            Assert.Equal(1, high.QueueLength);
        }

        /// <summary>
        /// An undeliverable SECURE message comes back to the sender as a returned message carrying
        /// the reason; a non-secure one is discarded silently.
        /// </summary>
        [Fact]
        public void Send_Undeliverable_ReturnsOnlyWhenSecure()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber client;
            kernel.OpenPort(out client);

            XmsgMagicNumber nowhere = XmsgMagicNumber.Create(102, 400, 42);

            XmsgStatus nonSecure = SendBytes(kernel, client, nowhere, new byte[] { 1 }, XmsgSendFlags.None);
            Assert.True(nonSecure.IsError);
            Assert.True(kernel.Receive(client, XmsgWaitOptions.None).Status.IsNotTerminated);

            XmsgStatus secure = SendBytes(kernel, client, nowhere, new byte[] { 1 }, XmsgSendFlags.Secure);
            Assert.False(secure.IsError);

            XmsgReceiveResult back = kernel.Receive(client, XmsgWaitOptions.None);
            Assert.Equal(XmsgMessageType.XMTRE, back.MessageType);
            Assert.Equal(XmsgError.XEIMA, back.ReturnReason);
            Assert.Null(back.Length);
        }

        /// <summary>
        /// Closing a port returns its queued secure messages to their senders and releases the
        /// non-secure ones.
        /// </summary>
        [Fact]
        public void ClosePort_ReturnsSecureMessagesAndDropsTheRest()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);

            SendBytes(kernel, client, serverMagic, new byte[] { 1 }, XmsgSendFlags.None);
            SendBytes(kernel, client, serverMagic, new byte[] { 2 }, XmsgSendFlags.Secure);

            kernel.ClosePort(server);

            XmsgReceiveResult returned = kernel.Receive(client, XmsgWaitOptions.None);
            Assert.Equal(XmsgMessageType.XMTRE, returned.MessageType);
            Assert.Equal(XmsgError.XEPCL, returned.ReturnReason);

            // Only the secure one came back.
            Assert.True(kernel.Receive(client, XmsgWaitOptions.None).Status.IsNotTerminated);
        }

        /// <summary>
        /// The general-status scan starts after the named port and wraps, which is what makes
        /// round-robin service work.
        /// </summary>
        [Fact]
        public void GetGeneralStatus_ScansRoundRobin()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber a;
            XmsgPortNumber b;
            XmsgPortNumber sender;
            kernel.OpenPort(out a);
            kernel.OpenPort(out b);
            kernel.OpenPort(out sender);

            XmsgMagicNumber magicA;
            XmsgMagicNumber magicB;
            kernel.ConvertPortToMagic(a, out magicA);
            kernel.ConvertPortToMagic(b, out magicB);
            SendBytes(kernel, sender, magicA, new byte[] { 1 }, XmsgSendFlags.None);
            SendBytes(kernel, sender, magicB, new byte[] { 2 }, XmsgSendFlags.None);

            XmsgPortNumber found;
            Assert.False(kernel.GetGeneralStatus(XmsgPortNumber.Default, XmsgWaitOptions.None, out found).IsError);
            Assert.True(found.Value == a.Value || found.Value == b.Value);

            // Servicing that one and asking again from it must land on the other.
            kernel.Receive(found, XmsgWaitOptions.None);
            XmsgPortNumber next;
            kernel.GetGeneralStatus(found, XmsgWaitOptions.None, out next);
            Assert.NotEqual(found.Value, next.Value);
        }

        /// <summary>
        /// A returned message writes two status bytes over the front and goes back to its sender.
        /// </summary>
        [Fact]
        public void ReturnMessage_WritesStatusBytesAndGoesBack()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);
            SendBytes(kernel, client, serverMagic, new byte[] { 0xAA, 0xBB, 0xCC, 0xDD }, XmsgSendFlags.None);

            XmsgReceiveResult request = kernel.Receive(server, XmsgWaitOptions.None);
            Assert.True(request.Received);

            Assert.False(kernel.ReturnMessage(request.Message, 0x1234, server, XmsgSendFlags.None).IsError);

            XmsgReceiveResult reply = kernel.Receive(client, XmsgWaitOptions.None);
            Assert.True(reply.Received);

            byte[] head = new byte[4];
            int read;
            kernel.Read(reply.Message, head, 0, out read);
            Assert.Equal(new byte[] { 0x12, 0x34, 0xCC, 0xDD }, head);
        }

        /// <summary>
        /// A send to another system goes out through the sink rather than being queued locally.
        /// </summary>
        [Fact]
        public void Send_RemoteSystem_GoesToTheSink()
        {
            RecordingSink sink = new RecordingSink();
            XmsgKernel kernel = new XmsgKernel(102, 0x1234, sink);

            XmsgPortNumber client;
            kernel.OpenPort(out client);

            XmsgMagicNumber remote = XmsgMagicNumber.Create(100, 5, 43);
            Assert.False(SendBytes(kernel, client, remote, new byte[] { 9, 9 }, XmsgSendFlags.None).IsError);

            Assert.Single(sink.Sent);
            Assert.Equal(remote, sink.Sent[0].Destination);
            Assert.Equal(new byte[] { 9, 9 }, sink.Sent[0].Data);
        }

        /// <summary>
        /// Without a sink, an out-of-system send fails rather than pretending to succeed.
        /// </summary>
        [Fact]
        public void Send_RemoteSystem_WithoutSink_Fails()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber client;
            kernel.OpenPort(out client);

            XmsgStatus status = SendBytes(
                kernel, client, XmsgMagicNumber.Create(100, 5, 43), new byte[] { 1 }, XmsgSendFlags.None);

            Assert.True(status.IsError);
            Assert.Equal(XmsgError.XENOS, status.Error);
        }

        /// <summary>
        /// An inbound datagram is queued on the addressed port and carries its sender.
        /// </summary>
        [Fact]
        public void Deliver_InboundDatagram_ReachesThePort()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber port;
            kernel.OpenPort(out port);

            XmsgMagicNumber mine;
            kernel.ConvertPortToMagic(port, out mine);
            XmsgMagicNumber theirs = XmsgMagicNumber.Create(100, 5, 43);

            Assert.False(kernel.Deliver(mine, theirs, new byte[] { 7, 7, 7 }, XmsgSendFlags.None).IsError);

            XmsgReceiveResult received = kernel.Receive(port, XmsgWaitOptions.None);
            Assert.True(received.Received);
            Assert.Equal(3, received.Length);
            Assert.Equal(theirs, kernel.GetMessageStatus(received.Message).Sender);
        }

        /// <summary>
        /// Receive-and-read hands back the data in one call; receive-and-read-header hands back the
        /// first two user bytes instead of the length.
        /// </summary>
        [Fact]
        public void ReceiveVariants_ReportTheirOwnExtraValue()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber server;
            XmsgPortNumber client;
            kernel.OpenPort(out server);
            kernel.OpenPort(out client);

            XmsgMagicNumber serverMagic;
            kernel.ConvertPortToMagic(server, out serverMagic);
            SendBytes(kernel, client, serverMagic, new byte[] { 0xDE, 0xAD, 0xBE }, XmsgSendFlags.None);

            XmsgReceiveResult header = kernel.ReceiveAndReadHeader(server, XmsgWaitOptions.None);
            Assert.True(header.Received);
            Assert.Equal(0xDEAD, header.Extra);

            SendBytes(kernel, client, serverMagic, new byte[] { 1, 2, 3 }, XmsgSendFlags.None);
            byte[] into = new byte[3];
            int read;
            XmsgReceiveResult full = kernel.ReceiveAndRead(server, into, XmsgWaitOptions.None, out read);
            Assert.True(full.Received);
            Assert.Equal(3, read);
            Assert.Equal(new byte[] { 1, 2, 3 }, into);
        }

        /// <summary>
        /// Disconnect releases every port and buffer the task held.
        /// </summary>
        [Fact]
        public void Disconnect_ReleasesEverything()
        {
            XmsgKernel kernel = NewKernel();
            XmsgPortNumber port;
            kernel.OpenPort(out port);
            XmsgMessageIdentifier message;
            kernel.ReserveBuffer(16, XmsgBufferOptions.None, out message);

            Assert.Equal(1, kernel.OpenPortCount);
            Assert.Equal(1, kernel.OwnedBufferCount);

            kernel.Disconnect();

            Assert.Equal(0, kernel.OpenPortCount);
            Assert.Equal(0, kernel.OwnedBufferCount);
        }

        /// <summary>
        /// Converting a magic number back reports the port, the system, and whether it is local.
        /// </summary>
        [Fact]
        public void ConvertMagicToPort_ClassifiesTheMagicNumber()
        {
            XmsgKernel kernel = NewKernel();
            kernel.MakePrivileged(0);

            XmsgPortNumber port;
            kernel.OpenPort(out port);
            XmsgMagicNumber mine;
            kernel.ConvertPortToMagic(port, out mine);

            XmsgPortNumber decoded;
            int system;
            XmsgStatus local = kernel.ConvertMagicToPort(mine, out decoded, out system);
            Assert.Equal(2, local.Value);           // local port, privileged owner
            Assert.Equal(port.Value, decoded.Value);
            Assert.Equal(102, system);

            XmsgStatus remote = kernel.ConvertMagicToPort(
                XmsgMagicNumber.Create(100, 5, 43), out decoded, out system);
            Assert.Equal(1, remote.Value);          // remote port
            Assert.Equal(100, system);
        }

        private static XmsgKernel NewKernel()
        {
            return new XmsgKernel(102, 0x1234, null);
        }

        private static XmsgStatus SendBytes(
            XmsgKernel kernel,
            XmsgPortNumber from,
            XmsgMagicNumber to,
            byte[] payload,
            XmsgSendFlags flags)
        {
            XmsgMessageIdentifier message;
            kernel.ReserveBuffer(payload.Length == 0 ? 1 : payload.Length, XmsgBufferOptions.None, out message);
            int written;
            kernel.Write(message, payload, 0, false, out written);
            return kernel.Send(to, from, flags);
        }

        private sealed class RecordingSink : IXmsgDatagramSink
        {
            internal RecordingSink()
            {
                Sent = new List<Record>();
            }

            internal List<Record> Sent { get; }

            public XmsgStatus Send(
                XmsgMagicNumber destination,
                XmsgMagicNumber sender,
                ReadOnlySpan<byte> userData,
                XmsgSendFlags flags)
            {
                Sent.Add(new Record(destination, sender, userData.ToArray(), flags));
                return XmsgStatus.Completed;
            }

            internal sealed class Record
            {
                internal Record(XmsgMagicNumber destination, XmsgMagicNumber sender, byte[] data, XmsgSendFlags flags)
                {
                    Destination = destination;
                    Sender = sender;
                    Data = data;
                    Flags = flags;
                }

                internal XmsgMagicNumber Destination { get; }

                internal XmsgMagicNumber Sender { get; }

                internal byte[] Data { get; }

                internal XmsgSendFlags Flags { get; }
            }
        }
        /// <summary>
        /// The registry table records the same names on different ports on different systems,
        /// which is the evidence that a registry port is load-order dependent, not well-known.
        /// </summary>
        [Fact]
        public void KnownServers_RecordSameNameOnDifferentPorts()
        {
            AssertNameSeenOnSeveralPorts("*XM-FIDO");
            AssertNameSeenOnSeveralPorts("*TADADM");

            // Port 4 is a DIFFERENT server depending on what was started - the sharpest form of
            // the same evidence.
            List<string> onPortFour = new List<string>();
            IReadOnlyList<XmsgServerName> all = XmsgKnownServers.All();
            for (int i = 0; i < all.Count; i++)
            {
                if (all[i].ObservedPort == 4 && !onPortFour.Contains(all[i].Name))
                {
                    onPortFour.Add(all[i].Name);
                }
            }

            Assert.True(onPortFour.Count > 1, "port 4 should have been seen carrying more than one name");
        }

        /// <summary>
        /// Only names checked against captured wire bytes are marked as confirmed.
        /// </summary>
        [Fact]
        public void KnownServers_MarkWireConfirmationHonestly()
        {
            IReadOnlyList<XmsgServerName> tad = XmsgKnownServers.FindAll("*TADADM");

            int confirmed = 0;
            for (int i = 0; i < tad.Count; i++)
            {
                if (tad[i].PortConfirmedOnWire)
                {
                    confirmed++;
                    Assert.Equal(2, tad[i].ObservedPort);
                }
            }

            // Exactly one observation - the captures' own machine - is wire-confirmed.
            Assert.Equal(1, confirmed);
        }

        /// <summary>
        /// A connection port reports the same free-connection count wherever it runs, which is
        /// what makes capacity - unlike the port number - a property of the server.
        /// </summary>
        [Fact]
        public void KnownServers_CapacityIsStableAcrossSystems()
        {
            AssertCapacityAgrees("*XFTRA", 1);
            AssertCapacityAgrees("*FA-FSA", 2);
            AssertCapacityAgrees("*FA-SERVER", 30);
        }

        private static void AssertCapacityAgrees(string name, int expected)
        {
            IReadOnlyList<XmsgServerName> seen = XmsgKnownServers.FindAll(name);
            Assert.True(seen.Count > 1, name + " should have been seen on more than one system");

            for (int i = 0; i < seen.Count; i++)
            {
                Assert.Equal(expected, seen[i].FreeConnections);
            }
        }

        private static void AssertNameSeenOnSeveralPorts(string name)
        {
            IReadOnlyList<XmsgServerName> seen = XmsgKnownServers.FindAll(name);
            Assert.True(seen.Count > 1, name + " should have more than one observation");

            List<int> ports = new List<int>();
            for (int i = 0; i < seen.Count; i++)
            {
                if (!ports.Contains(seen[i].ObservedPort))
                {
                    ports.Add(seen[i].ObservedPort);
                }
            }

            Assert.True(ports.Count > 1, name + " should have been seen on more than one port");
        }

    }
}