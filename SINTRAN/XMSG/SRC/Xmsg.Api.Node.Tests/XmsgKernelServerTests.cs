using System;
using System.Collections.Generic;
using System.Text;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Api;
using NDInsight.Sintran.Xmsg.Api.Node;
using NDInsight.Sintran.Xmsg.Node.Services;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Api.Node.Tests
{
    /// <summary>
    /// Proves the bridge carries traffic both ways between the manual-shaped API and the node's
    /// frame-shaped world.
    /// </summary>
    public sealed class XmsgKernelServerTests
    {
        /// <summary>
        /// A datagram arriving from the node becomes a message on the addressed kernel port, with
        /// the sender's magic number recoverable exactly as the manual says.
        /// </summary>
        [Fact]
        public void Inbound_DatagramBecomesAMessageOnThePort()
        {
            XmsgKernel kernel = new XmsgKernel(102, 0x1111, null);
            XmsgKernelServer bridge = new XmsgKernelServer(kernel, "*DEMO", 4);

            XmsgPortNumber port;
            kernel.OpenPort(out port);
            XmsgMagicNumber mine;
            kernel.ConvertPortToMagic(port, out mine);
            bridge.RegisterPort(mine.PortWord);

            XmsgMagicNumber theirs = XmsgMagicNumber.Create(100, 5, 43);
            XmsgFrame incoming = BuildFrame(theirs, mine, Encoding.ASCII.GetBytes("HELLO"));

            IReadOnlyList<XmsgFrame> replies = bridge.Handle(incoming, new FakeTransport());

            Assert.Empty(replies);

            XmsgReceiveResult received = kernel.Receive(port, XmsgWaitOptions.None);
            Assert.True(received.Received);
            Assert.Equal(5, received.Length);
            Assert.Equal(theirs, kernel.GetMessageStatus(received.Message).Sender);

            byte[] body = new byte[5];
            int read;
            kernel.Read(received.Message, body, 0, out read);
            Assert.Equal("HELLO", Encoding.ASCII.GetString(body));
        }

        /// <summary>
        /// A kernel send to another system leaves as a datagram addressed to that system's node,
        /// carrying our port as the source.
        /// </summary>
        [Fact]
        public void Outbound_KernelSendBecomesADatagram()
        {
            // Create wires the kernel and the bridge to each other in the one order that works.
            XmsgKernelServer wiredBridge = XmsgKernelServer.Create(102, 0x1111, "*DEMO", 4);
            XmsgKernel wired = wiredBridge.Kernel;

            XmsgPortNumber port;
            wired.OpenPort(out port);
            XmsgMagicNumber mine;
            wired.ConvertPortToMagic(port, out mine);

            FakeTransport transport = new FakeTransport();

            // A datagram must arrive first so the bridge has a transport to build replies with.
            XmsgMagicNumber remote = XmsgMagicNumber.Create(100, 5, 43);
            wiredBridge.RegisterPort(mine.PortWord);
            wiredBridge.Handle(BuildFrame(remote, mine, Array.Empty<byte>()), transport);
            wired.Receive(port, XmsgWaitOptions.None);

            XmsgMessageIdentifier message;
            wired.ReserveBuffer(4, XmsgBufferOptions.None, out message);
            int written;
            wired.Write(message, Encoding.ASCII.GetBytes("PONG"), 0, false, out written);

            Assert.False(wired.Send(remote, port, XmsgSendFlags.None).IsError);

            IReadOnlyList<XmsgFrame> pending = wiredBridge.DrainPending(transport);
            Assert.Single(pending);
            Assert.Equal(100, transport.LastRemoteNode);
            Assert.Equal(remote.PortWord, transport.LastClientPort);
            Assert.Equal(mine.PortWord, transport.LastSourcePort);
            Assert.Equal("PONG", Encoding.ASCII.GetString(transport.LastPayload));
        }

        /// <summary>
        /// Only registered ports are routed here, so the node does not hand this server traffic
        /// belonging to another one.
        /// </summary>
        [Fact]
        public void OwnsPort_OnlyReportsRegisteredPorts()
        {
            XmsgKernel kernel = new XmsgKernel(102, 0x1111, null);
            XmsgKernelServer bridge = new XmsgKernelServer(kernel, "*DEMO", 4);

            Assert.False(bridge.OwnsPort(1218));

            bridge.RegisterPort(1218);
            Assert.True(bridge.OwnsPort(1218));

            bridge.UnregisterPort(1218);
            Assert.False(bridge.OwnsPort(1218));
        }

        /// <summary>
        /// Without a transport there is no way off this system, and the send says so rather than
        /// silently dropping the message.
        /// </summary>
        [Fact]
        public void Outbound_BeforeAnyTransport_Fails()
        {
            XmsgKernelServer bridge = new XmsgKernelServer(
                new XmsgKernel(102, 0x1111, null), "*DEMO", 4);

            XmsgStatus status = bridge.Send(
                XmsgMagicNumber.Create(100, 5, 43),
                XmsgMagicNumber.Create(102, 1, 7),
                Encoding.ASCII.GetBytes("X"),
                XmsgSendFlags.None);

            Assert.True(status.IsError);
            Assert.Equal(XmsgError.XENRU, status.Error);
        }

        /// <summary>
        /// The server's registry identity is what the node advertises.
        /// </summary>
        [Fact]
        public void Identity_ReportsNameAndPorts()
        {
            XmsgKernelServer bridge = new XmsgKernelServer(
                new XmsgKernel(102, 0x1111, null), "*DEMO", 4);

            Assert.Equal("*DEMO", bridge.Name);
            Assert.Equal(4, bridge.LogicalPort);
            Assert.Equal(4 << 7, bridge.WirePort);
            Assert.False(bridge.AdvancesOutputOnAck);
        }

        private static XmsgFrame BuildFrame(XmsgMagicNumber from, XmsgMagicNumber to, byte[] payload)
        {
            XmsgFrame frame = new XmsgFrameBuilder()
                .Between(to.SystemNumber, from.SystemNumber)
                .WithDatagramSequence(1)
                .WithProtocol(SintranProtocolId.Tad)
                .WithFlags2(0x0108)
                .WithSubHeaderControl(0x10, 0x86, 0x84)
                .WithEndpoints(to.SystemNumber, to.PortWord, from.SystemNumber, from.PortWord)
                .WithControlService(0x01080000)
                .Build();

            // The builder has no trailer method - the payload is assigned on the built frame.
            frame.TrailingBytes = payload;
            return frame;
        }

        private sealed class FakeTransport : IXmsgServerTransport
        {
            private ushort _nextPort = 0x0211;
            private int _nextSession = 1;

            internal ushort LastRemoteNode { get; private set; }

            internal ushort LastClientPort { get; private set; }

            internal ushort LastSourcePort { get; private set; }

            internal byte[] LastPayload { get; private set; } = Array.Empty<byte>();

            public ushort NodeNumber
            {
                get { return 102; }
            }

            public ushort AllocateSessionPort()
            {
                return _nextPort++;
            }

            public int AllocateSessionNumber()
            {
                return _nextSession++;
            }

            public XmsgFrame BuildDatagram(
                ushort remoteNode,
                ushort clientSystem,
                ushort clientPort,
                ushort sourcePort,
                uint controlService,
                byte frameFlags,
                byte role,
                byte[] payload)
            {
                LastRemoteNode = remoteNode;
                LastClientPort = clientPort;
                LastSourcePort = sourcePort;
                LastPayload = payload;

                XmsgFrame built = new XmsgFrameBuilder()
                    .Between(remoteNode, NodeNumber)
                    .WithDatagramSequence(1)
                    .WithProtocol(SintranProtocolId.Tad)
                    .WithFlags2((ushort)(controlService >> 16))
                    .WithSubHeaderControl(0x10, frameFlags, role)
                    .WithEndpoints(clientSystem, clientPort, NodeNumber, sourcePort)
                    .WithControlService(controlService)
                    .Build();

                built.TrailingBytes = payload;
                return built;
            }
        }
    }
}
