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
        /// First sequence number sent on a freshly opened connection.
        /// </summary>
        /// <remarks>
        /// <para>
        /// MEASURED 2026-08-04 against a live SINTRAN (D100 -> our node 19999). This was 0x01, on
        /// the reasoning that the captured links were already running so the true starting value was
        /// unknown and "any value works because the peer follows what it receives". <b>That
        /// reasoning was wrong.</b> D100 does not follow what it receives: on every fresh connection
        /// its own first data frame is sequence <c>0</c>, and when we opened at <c>1</c> it accepted
        /// the connection, then ignored our reply and re-sent its ReachabilityRequest - consistent
        /// with a receiver holding a frame that arrived one ahead of the sequence it was waiting for.
        /// </para>
        /// <para>
        /// The old value was never contradicted by the captures because every capture began
        /// mid-conversation, where the starting value is invisible. Only opening a connection from
        /// scratch could show it.
        /// </para>
        /// </remarks>
        public const byte InitialSequence = 0x00;

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
        public event UnknownFrameKindReceived? OnUnknownFrameKindReceived;

        /// <summary>
        /// Occurs when the peer asks to tear the link down. Carries the raw kind byte, because
        /// there are two disconnect-request types (by user and by network service) and only the
        /// by-network-service one has ever been seen on the wire.
        /// </summary>
        public event DisconnectRequested? OnDisconnectRequested;

        /// <summary>
        /// Gets the number of connection requests received from the peer.
        /// </summary>
        public int ConnectionRequestsReceived { get; private set; }

        /// <summary>
        /// Gets the number of connection confirms this node has sent.
        /// </summary>
        /// <remarks>
        /// If this keeps climbing while the peer goes on repeating its request, the confirm is
        /// being rejected or ignored - which is the signal that
        /// <see cref="ConnectionConfirmKindUnverified"/> or the field layout is wrong.
        /// </remarks>
        public int ConnectionConfirmsSent { get; private set; }

        /// <summary>
        /// Gets the number of disconnect requests received from the peer.
        /// </summary>
        public int DisconnectRequestsReceived { get; private set; }

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
        /// Occurs the first time the peer becomes known, RAISED BEFORE the frame that taught us is
        /// delivered upward.
        /// </summary>
        /// <remarks>
        /// The ordering is the whole point. A caller that waits for <c>HandleFrame</c> to RETURN
        /// before treating the link as usable is too late: the datagram inside that same frame has
        /// already been delivered up and its reply already attempted and refused, because the link
        /// still looked unusable. MEASURED against a live SINTRAN 2026-08-04 - the reply to the
        /// first datagram of a reused connection was dropped exactly this way, which the remote saw
        /// as no answer at all.
        /// </remarks>
        public event PeerLearned? OnPeerLearned;

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
            // First id field is the DESTINATION's reference, second is our own - see the note on
            // SendConnectionConfirm. The builder's parameters are named the other way round.
            NdLinkHeader.Data(_nextSequence, PeerLinkId, _localLinkId, (ushort)payload.Length).Write(llcPayload);

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

            // Not addressed to us. Only frames whose destination carries the ND vendor prefix are
            // filtered here, so a non-ND destination (including broadcast) still passes.
            //
            // An earlier version of this comment justified that by saying "COSMOS reachability
            // traffic uses broadcast". That is UNSUPPORTED and is not repeated: no captured COSMOS
            // frame has a broadcast destination, and the ENCOS receive path has no broadcast case
            // at all - a broadcast reaches the host only if FF:FF:FF:FF:FF:FF has been registered
            // in the group filter at 0x542C (ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md
            // section 5). Whether COSMOS ever sends a non-unicast frame is simply UNKNOWN, so the
            // pass-through is left as-is rather than tightened on a guess in either direction.
            //
            // LAYER TRAP, and the likely origin of that wrong comment: an emulated segment run as
            // "--net=udp" is carried over IP multicast 239.3.9.4:3094, so EVERY frame travels by
            // multicast whatever its destination MAC. That is the TRANSPORT being multicast and
            // says nothing about what COSMOS puts in the destination address. Do not read one as
            // evidence for the other.
            //
            // NOTE this does NOT make one instance safe on a shared segment: a frame unicast to us
            // by a DIFFERENT station is accepted here and re-learns the peer below. Binding to one
            // peer is the caller's job - see EthernetLink.
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

            // The peer's own reference is in bytes 7-8, which TryParse puts in ReceiverLinkId - the
            // property names are the wrong way round, see the note on SendConnectionConfirm. Reading
            // SenderLinkId here learned OUR OWN reference as the peer's, so every reply addressed
            // D100 as "1" and it answered "DR BY NS reason 1" even after the link was up.
            //
            // A connection request carries zero for the destination and cannot teach us anything, so
            // only learn from frames that actually carry the peer's reference.
            if (header.ReceiverLinkId != 0)
            {
                LearnPeer(source, header.ReceiverLinkId);
            }
            else
            {
                bool firstContact = !HasLearnedPeer;
                PeerMac = source;
                HasLearnedPeer = true;
                if (firstContact)
                {
                    OnPeerLearned?.Invoke();
                }
            }

            if (header.IsAcknowledge)
            {
                AcknowledgementsReceived++;
                return true;
            }

            if (header.IsConnectionRequest)
            {
                ConnectionRequestsReceived++;
                SendConnectionConfirm(header);
                return true;
            }

            if (header.IsDisconnectRequest)
            {
                DisconnectRequestsReceived++;
                OnDisconnectRequested?.Invoke(header.Kind);
                return true;
            }

            if (!header.IsData)
            {
                OnUnknownFrameKindReceived?.Invoke(header.Kind);
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
            bool first = !HasLearnedPeer;
            PeerMac = source;
            PeerLinkId = senderLinkId;
            HasLearnedPeer = true;
            if (first)
            {
                OnPeerLearned?.Invoke();
            }
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
            // Destination reference first, ours second - same correction as the data path.
            NdLinkHeader.AcknowledgeFor(receivedSequence, PeerLinkId, _localLinkId).Write(llcPayload);

            int written = BuildFrame(PeerMac, llcPayload, ReadOnlySpan<byte>.Empty);
            _sendFrame(_frameBuffer, written);
        }

        /// <summary>
        /// The wire byte this node sends for a connection confirm. UNVERIFIED - see the remarks.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The high nibble is the NPDU type and CC is type 1, so the top half is solid. The LOW
        /// nibble is a GUESS: every control frame captured so far ends in <c>F</c> - CR
        /// <c>0x0F</c>, AK <c>0x3F</c>, DR <c>0x6F</c> - and only the data frame <c>0x20</c> ends
        /// in <c>0</c>. <c>0x1F</c> follows that pattern. No CC has ever been captured, so this is
        /// a testable proposal, not a fact.
        /// </para>
        /// <para>
        /// How to tell whether it is right: after we answer a CR with this byte, a peer that
        /// accepts it should stop repeating the CR and move on to sending DT. If it keeps
        /// repeating, the byte or the field layout is wrong.
        /// </para>
        /// </remarks>
        public const byte ConnectionConfirmKindUnverified = 0x1F;

        /// <summary>
        /// Answers a connection request so the peer can finish opening the link.
        /// </summary>
        /// <param name="request">
        /// The connection request being answered.
        /// </param>
        /// <remarks>
        /// EVERY field here except the sender link id is UNVERIFIED, because no connection confirm
        /// has been captured. What the captured CR frames DO establish, from both the D100-to-D102
        /// and the D100-to-D19999 traces:
        ///  - The sender link id is 0, because the requester has no link yet.
        ///  - The trailing field carries the SENDER'S OWN system number, not a payload length
        ///    (D100's CR carries <c>0x0064</c> = 100), so we answer with ours.
        ///  - The receiver field is NOT a link id. It steps in lockstep with the sequence number
        ///    (seq 49 with 11553, seq 50 with 11554), so it behaves as a counter or token. We echo
        ///    it back unchanged, on the assumption the requester uses it to match the answer to its
        ///    request.
        /// We put our own link id in the sender field so the peer can learn it, which is the one
        /// part of this frame the protocol clearly needs.
        /// </remarks>
        private void SendConnectionConfirm(NdLinkHeader request)
        {
            EnsureBuffer(Ieee8023Frame.MinimumFrameLength);

            // Field order, from D100's own trace decode of a connection request:
            //     "NPDU out to ND19999 loc 11557 rem 0  CR from ND 100"
            // D100 knows its OWN reference and not ours, and the frame carries 0 in the FIRST id
            // field with 11557 in the SECOND. So the first field is the DESTINATION's reference and
            // the second is the SENDER'S OWN - the opposite of what the property names suggest.
            //
            // The first attempt filled them the other way round and D100 answered every confirm
            // with "DR BY NS reason 1", ten retries then FAILED TRANSMIT. So the confirm must echo
            // the requester's reference back as the destination and carry ours as the source.
            NdLinkHeader confirm = new NdLinkHeader(
                ConnectionConfirmKindUnverified,
                request.Sequence,
                request.ReceiverLinkId,
                _localLinkId,
                LocalSystemNumber);

            Span<byte> llcPayload = stackalloc byte[NdLinkHeader.Length];
            confirm.Write(llcPayload);

            int written = BuildFrame(PeerMac, llcPayload, ReadOnlySpan<byte>.Empty);
            _sendFrame(_frameBuffer, written);
            ConnectionConfirmsSent++;
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
