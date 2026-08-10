using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Ethernet;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Ethernet.Tests
{
    /// <summary>
    /// Tests the ND link layer: peer learning, the acknowledgement rule, sequencing, and the
    /// self-frame guard.
    /// </summary>
    public sealed class NdLinkLayerTests
    {
        private const ushort LocalSystem = 9999;
        private const ushort PeerSystem = 100;
        private const ushort LocalLinkId = 0x4242;
        private const ushort PeerLinkId = 0x59C1;

        /// <summary>
        /// Collects frames a layer hands to its transport.
        /// </summary>
        private sealed class FrameSink
        {
            /// <summary>
            /// Every frame sent, each copied out of the shared buffer.
            /// </summary>
            public List<byte[]> Frames { get; } = new List<byte[]>();

            /// <summary>
            /// Records one frame.
            /// </summary>
            /// <param name="data">
            /// The buffer holding the frame.
            /// </param>
            /// <param name="length">
            /// The number of valid bytes.
            /// </param>
            public void Send(byte[] data, int length)
            {
                byte[] copy = new byte[length];
                Array.Copy(data, copy, length);
                Frames.Add(copy);
            }
        }

        /// <summary>
        /// Builds a data frame as the peer would send it.
        /// </summary>
        /// <param name="sequence">
        /// The sequence number to use.
        /// </param>
        /// <param name="datagram">
        /// The datagram to carry.
        /// </param>
        /// <returns>
        /// A complete Ethernet frame from the peer to the local node.
        /// </returns>
        private static byte[] BuildPeerDataFrame(byte sequence, byte[] datagram)
        {
            byte[] llcPayload = new byte[NdLinkHeader.Length + datagram.Length];
            // This frame comes FROM the peer, so the first id field is OUR reference (the
            // destination) and the second is the peer's own. That order is not what the builder's
            // parameter names suggest; it is what D100's own NPDU trace shows on the wire, where a
            // connection request carries zero in the first field and the sender's reference in the
            // second. The fixture used to fill them the other way round, which meant these tests
            // agreed with the parser only because both were wrong in the same direction.
            NdLinkHeader.Data(sequence, LocalLinkId, PeerLinkId, (ushort)datagram.Length).Write(llcPayload);
            Array.Copy(datagram, 0, llcPayload, NdLinkHeader.Length, datagram.Length);

            byte[] buffer = new byte[Ieee8023Frame.PayloadOffset + llcPayload.Length + Ieee8023Frame.MinimumFrameLength];
            int written = Ieee8023Frame.Build(
                NdMacAddress.FromSystemNumber(LocalSystem),
                NdMacAddress.FromSystemNumber(PeerSystem),
                llcPayload,
                buffer);

            byte[] frame = new byte[written];
            Array.Copy(buffer, frame, written);
            return frame;
        }

        /// <summary>
        /// A received data frame is acknowledged with the received sequence plus one, and the
        /// acknowledgement carries no payload.
        /// </summary>
        [Fact]
        public void ReceivedDataFrameIsAcknowledgedWithSequencePlusOne()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte[] datagram = new byte[] { 0x21, 0x13, 0x00, 0x0E };
            Assert.True(layer.HandleFrame(BuildPeerDataFrame(0x43, datagram), BuildPeerDataFrame(0x43, datagram).Length));

            Assert.Single(sink.Frames);
            byte[] ack = sink.Frames[0];

            Assert.True(NdLinkHeader.TryParse(ack.AsSpan(Ieee8023Frame.PayloadOffset), out NdLinkHeader header));
            Assert.True(header.IsAcknowledge);
            Assert.Equal(0x44, header.Sequence);
            Assert.Equal(0, header.PayloadLength);
            // First id field is the DESTINATION's reference, second is ours - the property names
            // are back to front, see the note in the fixture's frame builder.
            Assert.Equal(PeerLinkId, header.SenderLinkId);
            Assert.Equal(LocalLinkId, header.ReceiverLinkId);
        }

        /// <summary>
        /// The sequence wraps through zero rather than overflowing.
        /// </summary>
        [Fact]
        public void AcknowledgementSequenceWrapsAtByteBoundary()
        {
            NdLinkHeader ack = NdLinkHeader.AcknowledgeFor(0xFF, LocalLinkId, PeerLinkId);
            Assert.Equal(0x00, ack.Sequence);
        }

        /// <summary>
        /// The peer's link id and address are learned from the first frame, not derived.
        /// </summary>
        [Fact]
        public void PeerIdentityIsLearnedFromTheFirstFrame()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            Assert.False(layer.HasLearnedPeer);
            Assert.Equal(NdLinkLayer.UnknownPeerLinkId, layer.PeerLinkId);

            byte[] frame = BuildPeerDataFrame(0x10, new byte[] { 0x01 });
            layer.HandleFrame(frame, frame.Length);

            Assert.True(layer.HasLearnedPeer);
            Assert.Equal(PeerLinkId, layer.PeerLinkId);
            Assert.True(layer.PeerMac.TryGetSystemNumber(out ushort peerSystem));
            Assert.Equal(PeerSystem, peerSystem);
        }

        /// <summary>
        /// Nothing is sent before the peer is known, because a frame addressed to nobody is not
        /// worth putting on the segment.
        /// </summary>
        [Fact]
        public void SendIsRefusedUntilThePeerIsKnown()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            Assert.False(layer.SendDatagram(new byte[] { 0x21, 0x13 }));
            Assert.Empty(sink.Frames);
        }

        /// <summary>
        /// Outgoing data frames carry an increasing sequence and the learned peer's link id.
        /// </summary>
        [Fact]
        public void OutgoingDataFramesIncrementTheSequence()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte[] incoming = BuildPeerDataFrame(0x10, new byte[] { 0x01 });
            layer.HandleFrame(incoming, incoming.Length);
            sink.Frames.Clear();

            byte[] datagram = new byte[] { 0x21, 0x13, 0x00, 0x0E, 0x00, 0x64 };
            Assert.True(layer.SendDatagram(datagram));
            Assert.True(layer.SendDatagram(datagram));

            Assert.Equal(2, sink.Frames.Count);

            Assert.True(NdLinkHeader.TryParse(sink.Frames[0].AsSpan(Ieee8023Frame.PayloadOffset), out NdLinkHeader first));
            Assert.True(NdLinkHeader.TryParse(sink.Frames[1].AsSpan(Ieee8023Frame.PayloadOffset), out NdLinkHeader second));

            Assert.True(first.IsData);
            Assert.Equal(datagram.Length, first.PayloadLength);
            // We are the sender here, so OUR reference is in the second field and the peer's in the
            // first - see the note in the fixture's frame builder.
            Assert.Equal(LocalLinkId, first.ReceiverLinkId);
            Assert.Equal(PeerLinkId, first.SenderLinkId);
            Assert.Equal(unchecked((byte)(first.Sequence + 1)), second.Sequence);
        }

        /// <summary>
        /// The datagram is delivered up intact.
        /// </summary>
        [Fact]
        public void DatagramIsDeliveredUpIntact()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte[] received = Array.Empty<byte>();
            layer.PayloadReceived += (payload, length) =>
            {
                received = new byte[length];
                Array.Copy(payload, received, length);
            };

            byte[] datagram = new byte[] { 0x21, 0x13, 0x00, 0x0E, 0x00, 0x64, 0x00, 0x66 };
            byte[] frame = BuildPeerDataFrame(0x20, datagram);
            layer.HandleFrame(frame, frame.Length);

            Assert.Equal(datagram, received);
        }

        /// <summary>
        /// A frame from our own address is ignored.
        /// </summary>
        /// <remarks>
        /// On a multicast segment a node hears its own transmissions. Processing them would
        /// acknowledge our own data and corrupt the sequence, so the guard matters in practice and
        /// not only in theory.
        /// </remarks>
        [Fact]
        public void OwnLoopedBackFrameIsIgnored()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte[] llcPayload = new byte[NdLinkHeader.Length];
            NdLinkHeader.Data(0x01, LocalLinkId, PeerLinkId, 0).Write(llcPayload);

            byte[] buffer = new byte[Ieee8023Frame.MinimumFrameLength];
            int written = Ieee8023Frame.Build(
                NdMacAddress.FromSystemNumber(PeerSystem),
                NdMacAddress.FromSystemNumber(LocalSystem),   // source = US
                llcPayload,
                buffer);

            Assert.False(layer.HandleFrame(buffer, written));
            Assert.Empty(sink.Frames);
            Assert.False(layer.HasLearnedPeer);
        }

        /// <summary>
        /// An acknowledgement is counted but never itself acknowledged.
        /// </summary>
        [Fact]
        public void AcknowledgementIsNotAcknowledged()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte[] llcPayload = new byte[NdLinkHeader.Length];
            NdLinkHeader.AcknowledgeFor(0x30, PeerLinkId, LocalLinkId).Write(llcPayload);

            byte[] buffer = new byte[Ieee8023Frame.MinimumFrameLength];
            int written = Ieee8023Frame.Build(
                NdMacAddress.FromSystemNumber(LocalSystem),
                NdMacAddress.FromSystemNumber(PeerSystem),
                llcPayload,
                buffer);

            Assert.True(layer.HandleFrame(buffer, written));
            Assert.Empty(sink.Frames);
            Assert.Equal(1, layer.AcknowledgementsReceived);
            Assert.Equal(0, layer.DataFramesReceived);
        }

        /// <summary>
        /// An unrecognised frame kind is surfaced rather than dropped or thrown.
        /// </summary>
        [Fact]
        public void UnknownFrameKindIsSurfaced()
        {
            FrameSink sink = new FrameSink();
            NdLinkLayer layer = new NdLinkLayer(LocalSystem, LocalLinkId, sink.Send);

            byte reported = 0;
            layer.OnUnknownFrameKindReceived += kind => reported = kind;

            byte[] frame = BuildPeerDataFrame(0x10, new byte[] { 0x01 });
            frame[Ieee8023Frame.PayloadOffset + 2] = 0x7E;

            Assert.True(layer.HandleFrame(frame, frame.Length));
            Assert.Equal(0x7E, reported);
        }

        /// <summary>
        /// Two link layers on one in-process segment exchange a datagram and acknowledge it.
        /// </summary>
        /// <remarks>
        /// End-to-end over the segment abstraction: proves the pieces fit together, not just that
        /// each parses in isolation.
        /// </remarks>
        [Fact]
        public void TwoNodesOnASegmentExchangeADatagram()
        {
            InProcessEthernetSegment segment = new InProcessEthernetSegment();
            IEthernetBackend portA = segment.CreatePort("a");
            IEthernetBackend portB = segment.CreatePort("b");
            portA.Start();
            portB.Start();

            NdLinkLayer nodeA = new NdLinkLayer(9999, 0x4242, (data, length) => portA.SendPacket(data, 0, length));
            NdLinkLayer nodeB = new NdLinkLayer(100, 0x59C1, (data, length) => portB.SendPacket(data, 0, length));

            portA.OnPacketReceived += (data, length) => nodeA.HandleFrame(data, length);
            portB.OnPacketReceived += (data, length) => nodeB.HandleFrame(data, length);

            byte[] receivedByB = Array.Empty<byte>();
            nodeB.PayloadReceived += (payload, length) =>
            {
                receivedByB = new byte[length];
                Array.Copy(payload, receivedByB, length);
            };

            // B must speak first so A learns who it is - A cannot invent B's link id.
            byte[] hello = new byte[] { 0x21, 0x13, 0x00, 0x19 };
            byte[] llcPayload = new byte[NdLinkHeader.Length + hello.Length];
            // B does not know A's reference yet, so the destination field is zero and B's own
            // reference goes second - exactly how a real connection request looks on the wire.
            NdLinkHeader.Data(0x01, 0x0000, 0x59C1, (ushort)hello.Length).Write(llcPayload);
            Array.Copy(hello, 0, llcPayload, NdLinkHeader.Length, hello.Length);

            byte[] buffer = new byte[Ieee8023Frame.MinimumFrameLength + llcPayload.Length];
            int written = Ieee8023Frame.Build(
                NdMacAddress.FromSystemNumber(9999),
                NdMacAddress.FromSystemNumber(100),
                llcPayload,
                buffer);
            portB.SendPacket(buffer, 0, written);

            Assert.True(nodeA.HasLearnedPeer);
            Assert.Equal(0x59C1, nodeA.PeerLinkId);

            // Now A can answer, and B should receive it and acknowledge.
            byte[] datagram = new byte[] { 0x21, 0x13, 0x00, 0x0E, 0x00, 0x64, 0x27, 0x0F };
            Assert.True(nodeA.SendDatagram(datagram));

            Assert.Equal(datagram, receivedByB);
            Assert.True(nodeA.AcknowledgementsReceived > 0);
        }
    }
}
