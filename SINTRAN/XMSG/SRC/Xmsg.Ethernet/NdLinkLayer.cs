using System;

namespace NDInsight.Sintran.Xmsg.Ethernet
{
    /// <summary>
    /// Receives one SINTRAN datagram taken out of an ND link data frame.
    /// </summary>
    /// <param name="payload">
    /// The buffer holding the datagram. It may be reused after the handler returns, so a handler
    /// that retains it MUST copy.
    /// </param>
    /// <param name="length">
    /// The number of valid bytes in <paramref name="payload"/>.
    /// </param>
    public delegate void NdLinkPayloadReceived(byte[] payload, int length);

    /// <summary>
    /// The ND link layer for one peer on an Ethernet segment: sequences outgoing data frames,
    /// acknowledges incoming ones, and hands the SINTRAN datagram up.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is the Ethernet counterpart of LAPB on HDLC. The framing it drives is documented on
    /// <see cref="NdLinkHeader"/>; this type owns the state that framing needs.
    /// </para>
    /// <para><b>Learned, not assumed</b></para>
    /// <para>
    /// The peer's link id is UNKNOWN in origin - it is neither the node number nor the system
    /// number in the MAC - so it is <b>learned from the first frame received</b> rather than
    /// derived. Deriving it from the node number would be inventing a constant that merely happens
    /// to work, which is the failure mode this project has been bitten by before. Until a frame
    /// arrives, <see cref="HasLearnedPeer"/> is false and outgoing frames use
    /// <see cref="UnknownPeerLinkId"/>.
    /// </para>
    /// <para><b>Acknowledgement rule</b></para>
    /// <para>
    /// Every received data frame is answered with an acknowledgement carrying the received sequence
    /// PLUS ONE - the next expected value. Acknowledgements are never themselves acknowledged.
    /// </para>
    /// <para><b>What is deliberately not implemented</b></para>
    /// <para>
    /// No retransmission, no window, no reject handling. None of those were exercised by any
    /// capture: no loss occurred, no frame kind other than <c>0x20</c> and <c>0x3F</c> was seen,
    /// and the window size is unknown. Guessing at them would produce confident, untested and
    /// probably wrong behaviour. An unrecognised frame kind is surfaced through
    /// <see cref="UnknownFrameKindReceived"/> so it reaches a log rather than being silently
    /// dropped or throwing.
    /// </para>
    /// </remarks>
    public sealed class NdLinkLayer
    {
        /// <summary>
        /// Link id used for the peer before its real one has been learned.
        /// </summary>
        public const ushort UnknownPeerLinkId = 0x0000;

        /// <summary>
        /// First sequence number sent. The captured links were already running, so the true
        /// starting value is UNKNOWN; any value works because the peer follows what it receives.
        /// </summary>
        public const byte InitialSequence = 0x01;

        private readonly ushort _localLinkId;
        private readonly NdMacAddress _localMac;
        private readonly Action<byte[], int> _sendFrame;

        private byte[] _frameBuffer = new byte[1600];
        private byte _nextSequence = InitialSequence;

        /// <summary>
        /// Initialises the link layer.
        /// </summary>
        /// <param name="localSystemNumber">
        /// This node's ND system number, used to build its station address.
        /// </param>
        /// <param name="localLinkId">
        /// This node's link id, placed in the sender field of outgoing frames.
        /// </param>
        /// <param name="sendFrame">
        /// Sends a complete Ethernet frame; called with the buffer and its length.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="sendFrame"/> is null.
        /// </exception>
        public NdLinkLayer(ushort localSystemNumber, ushort localLinkId, Action<byte[], int> sendFrame)
        {
            _localMac = NdMacAddress.FromSystemNumber(localSystemNumber);
            _localLinkId = localLinkId;
            _sendFrame = sendFrame ?? throw new ArgumentNullException(nameof(sendFrame));
            LocalSystemNumber = localSystemNumber;
        }

        /// <summary>
        /// Occurs when a data frame's SINTRAN datagram has been extracted.
        /// </summary>
        public event NdLinkPayloadReceived? PayloadReceived;

        /// <summary>
        /// Occurs when a frame carries a kind other than data or acknowledgement.
        /// </summary>
        public event Action<byte>? UnknownFrameKindReceived;

        /// <summary>
        /// Gets this node's ND system number.
        /// </summary>
        public ushort LocalSystemNumber { get; }

        /// <summary>
        /// Gets this node's station address.
        /// </summary>
        public NdMacAddress LocalMac => _localMac;

        /// <summary>
        /// Gets the peer's link id, once learned.
        /// </summary>
        public ushort PeerLinkId { get; private set; } = UnknownPeerLinkId;

        /// <summary>
        /// Gets the peer's station address, once a frame has arrived from it.
        /// </summary>
        public NdMacAddress PeerMac { get; private set; }

        /// <summary>
        /// Gets a value indicating whether a frame has been received and the peer's identity learned.
        /// </summary>
        public bool HasLearnedPeer { get; private set; }

        /// <summary>
        /// Gets the sequence number that will be used by the next data frame sent.
        /// </summary>
        public byte NextSequence => _nextSequence;

        /// <summary>
        /// Gets the number of data frames received.
        /// </summary>
        public long DataFramesReceived { get; private set; }

        /// <summary>
        /// Gets the number of acknowledgements received.
        /// </summary>
        public long AcknowledgementsReceived { get; private set; }

        /// <summary>
        /// Sends a SINTRAN datagram as a data frame.
        /// </summary>
        /// <param name="payload">
        /// The datagram bytes.
        /// </param>
        /// <returns>
        /// True when a frame was built and handed to the transport.
        /// </returns>
        /// <remarks>
        /// Returns false when the peer is not yet known, because a frame addressed to nobody is not
        /// worth putting on the segment. A node that must speak first has to be given the peer's
        /// address another way.
        /// </remarks>
        public bool SendDatagram(ReadOnlySpan<byte> payload)
        {
            if (!HasLearnedPeer || payload.Length == 0)
            {
                return false;
            }

            int required = Ieee8023Frame.PayloadOffset + NdLinkHeader.Length + payload.Length;
            EnsureBuffer(required);

            Span<byte> llcPayload = stackalloc byte[NdLinkHeader.Length];
            NdLinkHeader.Data(_nextSequence, _localLinkId, PeerLinkId, (ushort)payload.Length).Write(llcPayload);

            int written = BuildFrame(PeerMac, llcPayload, payload);
            _nextSequence = unchecked((byte)(_nextSequence + 1));
            _sendFrame(_frameBuffer, written);
            return true;
        }

        /// <summary>
        /// Processes one received Ethernet frame.
        /// </summary>
        /// <param name="frame">
        /// The frame bytes, starting at the destination MAC.
        /// </param>
        /// <param name="length">
        /// The number of valid bytes.
        /// </param>
        /// <returns>
        /// True when the frame was a well-formed ND/COSMOS frame and was processed.
        /// </returns>
        /// <remarks>
        /// A frame sourced from this node's own address is ignored: on a multicast segment a node
        /// hears its own transmissions, and processing them would acknowledge our own data and
        /// corrupt the sequence.
        /// </remarks>
        public bool HandleFrame(byte[] frame, int length)
        {
            if (frame == null || length <= 0)
            {
                return false;
            }

            ReadOnlySpan<byte> span = new ReadOnlySpan<byte>(frame, 0, length);
            if (!Ieee8023Frame.TryParse(span, out NdMacAddress destination, out NdMacAddress source, out int payloadOffset, out int payloadLength))
            {
                return false;
            }

            // Our own frame looped back by the segment.
            if (source.Equals(_localMac))
            {
                return false;
            }

            // Not addressed to us. Broadcast/multicast destinations are not filtered out, because
            // COSMOS reachability traffic uses them.
            if (destination.HasNdVendorPrefix && !destination.Equals(_localMac))
            {
                return false;
            }

            if (payloadLength < NdLinkHeader.Length)
            {
                return false;
            }

            if (!NdLinkHeader.TryParse(span.Slice(payloadOffset, payloadLength), out NdLinkHeader header))
            {
                return false;
            }

            LearnPeer(source, header.SenderLinkId);

            if (header.IsAcknowledge)
            {
                AcknowledgementsReceived++;
                return true;
            }

            if (!header.IsData)
            {
                UnknownFrameKindReceived?.Invoke(header.Kind);
                return true;
            }

            DataFramesReceived++;

            int datagramOffset = payloadOffset + NdLinkHeader.Length;
            int available = payloadLength - NdLinkHeader.Length;
            int datagramLength = header.PayloadLength <= available ? header.PayloadLength : available;

            SendAcknowledgement(header.Sequence);

            if (datagramLength > 0)
            {
                byte[] datagram = new byte[datagramLength];
                Array.Copy(frame, datagramOffset, datagram, 0, datagramLength);
                PayloadReceived?.Invoke(datagram, datagramLength);
            }

            return true;
        }

        /// <summary>
        /// Records the peer's address and link id from a received frame.
        /// </summary>
        /// <param name="source">
        /// The peer's station address.
        /// </param>
        /// <param name="senderLinkId">
        /// The link id the peer put in the sender field.
        /// </param>
        private void LearnPeer(NdMacAddress source, ushort senderLinkId)
        {
            PeerMac = source;
            PeerLinkId = senderLinkId;
            HasLearnedPeer = true;
        }

        /// <summary>
        /// Sends the acknowledgement for a received data frame.
        /// </summary>
        /// <param name="receivedSequence">
        /// The sequence number being acknowledged.
        /// </param>
        private void SendAcknowledgement(byte receivedSequence)
        {
            EnsureBuffer(Ieee8023Frame.MinimumFrameLength);

            Span<byte> llcPayload = stackalloc byte[NdLinkHeader.Length];
            NdLinkHeader.AcknowledgeFor(receivedSequence, _localLinkId, PeerLinkId).Write(llcPayload);

            int written = BuildFrame(PeerMac, llcPayload, ReadOnlySpan<byte>.Empty);
            _sendFrame(_frameBuffer, written);
        }

        /// <summary>
        /// Builds an Ethernet frame from a link header and its datagram into the shared buffer.
        /// </summary>
        /// <param name="destination">
        /// The destination station address.
        /// </param>
        /// <param name="linkHeader">
        /// The already-written 11-byte link header.
        /// </param>
        /// <param name="datagram">
        /// The datagram to place after it; may be empty.
        /// </param>
        /// <returns>
        /// The number of bytes written to the buffer.
        /// </returns>
        private int BuildFrame(NdMacAddress destination, ReadOnlySpan<byte> linkHeader, ReadOnlySpan<byte> datagram)
        {
            // The LLC payload is the link header followed by the datagram; assemble it in place at
            // the frame's payload offset to avoid a second copy.
            int payloadLength = linkHeader.Length + datagram.Length;
            Span<byte> scratch = payloadLength <= 256 ? stackalloc byte[payloadLength] : new byte[payloadLength];
            linkHeader.CopyTo(scratch);
            if (datagram.Length > 0)
            {
                datagram.CopyTo(scratch.Slice(linkHeader.Length));
            }

            return Ieee8023Frame.Build(destination, _localMac, scratch, _frameBuffer);
        }

        /// <summary>
        /// Grows the shared frame buffer when a larger frame is needed.
        /// </summary>
        /// <param name="required">
        /// The number of bytes needed.
        /// </param>
        private void EnsureBuffer(int required)
        {
            if (_frameBuffer.Length < required)
            {
                _frameBuffer = new byte[required];
            }
        }
    }
}
