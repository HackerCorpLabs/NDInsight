using System;

namespace NDInsight.Sintran.Xmsg.Ethernet
{
    /// <summary>
    /// The kind of ND link frame, from byte 2 of <see cref="NdLinkHeader"/>.
    /// </summary>
    /// <remarks>
    /// Only these two values were observed in the 2026-08-01 captures. A parser must ACCEPT an
    /// unrecognised value and surface it rather than throw - see <see cref="NdLinkHeader.Kind"/>.
    /// </remarks>
    public enum NdLinkFrameKind
    {
        /// <summary>
        /// <c>0x20</c> - carries a payload (the SINTRAN datagram).
        /// </summary>
        Data = 0x20,

        /// <summary>
        /// <c>0x3F</c> - an acknowledgement. Always carries
        /// <see cref="NdLinkHeader.PayloadLength"/> = 0.
        /// </summary>
        Acknowledge = 0x3F
    }

    /// <summary>
    /// The 11-byte ND link header that sits between the LLC header and the SINTRAN header on
    /// COSMOS Ethernet frames.
    /// </summary>
    /// <remarks>
    /// <para><b>There IS a link layer on Ethernet</b></para>
    /// <para>
    /// COSMOS does not rely on LLC1's connectionless service. It carries its own sequenced,
    /// acknowledged link protocol inside the LLC payload - the Ethernet counterpart of LAPB on
    /// HDLC. An earlier revision of the transport document claimed Ethernet had no link layer;
    /// that was drawn from a 2026-07-24 capture of a link that never came up, which contained only
    /// retransmissions and therefore no acknowledgements to notice.
    /// </para>
    /// <para><b>Layout</b> (VERIFIED, 96 frames, both directions)</para>
    /// Byte offsets within the header:
    ///  - <c>+0</c>, <c>+1</c>: <c>0x0B</c>, <c>0x02</c> - constant on every frame observed.
    ///  - <c>+2</c>: the frame kind, see <see cref="NdLinkFrameKind"/>.
    ///  - <c>+3</c>: <c>0x00</c> - constant on every frame observed.
    ///  - <c>+4</c>: the send sequence number.
    ///  - <c>+5</c>..<c>+6</c>: the SENDER's link id, big-endian.
    ///  - <c>+7</c>..<c>+8</c>: the RECEIVER's link id, big-endian.
    ///  - <c>+9</c>..<c>+10</c>: the payload length in bytes, big-endian.
    /// <para><b>The length relation</b></para>
    /// <para>
    /// On every captured frame, in both directions and for both kinds:
    /// <c>802.3 length = 3 (LLC) + 11 (this header) + PayloadLength</c>. So
    /// <see cref="PayloadLength"/> is authoritative; the frame is padded out to the 60-byte
    /// Ethernet minimum and the padding is NOT part of the message.
    /// </para>
    /// <para><b>Sequencing</b></para>
    /// <para>
    /// Each direction runs its own send sequence (observed stepping <c>0x43, 0x44, 0x45, ...</c>).
    /// On receiving a <see cref="NdLinkFrameKind.Data"/> frame the peer replies with an
    /// <see cref="NdLinkFrameKind.Acknowledge"/> frame whose sequence is <b>the received sequence
    /// plus one</b> - the next expected value - and whose payload length is zero.
    /// </para>
    /// <para><b>Link ids</b></para>
    /// <para>
    /// UNKNOWN where they come from. Node 102 used <c>0x5062</c> and node 100 <c>0x59C1</c>; these
    /// are neither the node number nor the system number embedded in the MAC. They are stable for
    /// the life of the link and swap with direction. A node must LEARN the peer's id from the first
    /// frame received and take its own from configuration - synthesising one from the node number
    /// would be a fabricated constant.
    /// </para>
    /// <para>
    /// Doc: <c>SINTRAN/XMSG/DOC/COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md</c> section 2a.
    /// </para>
    /// </remarks>
    public readonly struct NdLinkHeader
    {
        /// <summary>
        /// Length of the ND link header in bytes.
        /// </summary>
        public const int Length = 11;

        /// <summary>
        /// Byte 0, constant <c>0x0B</c> on every observed frame.
        /// </summary>
        public const byte Signature0 = 0x0B;

        /// <summary>
        /// Byte 1, constant <c>0x02</c> on every observed frame.
        /// </summary>
        public const byte Signature1 = 0x02;

        /// <summary>
        /// Initialises a header.
        /// </summary>
        /// <param name="kind">
        /// The raw frame-kind byte (offset 2).
        /// </param>
        /// <param name="sequence">
        /// The send sequence number (offset 4).
        /// </param>
        /// <param name="senderLinkId">
        /// The sender's link id (offsets 5-6).
        /// </param>
        /// <param name="receiverLinkId">
        /// The receiver's link id (offsets 7-8).
        /// </param>
        /// <param name="payloadLength">
        /// The number of payload bytes following the header (offsets 9-10).
        /// </param>
        public NdLinkHeader(byte kind, byte sequence, ushort senderLinkId, ushort receiverLinkId, ushort payloadLength)
        {
            Kind = kind;
            Sequence = sequence;
            SenderLinkId = senderLinkId;
            ReceiverLinkId = receiverLinkId;
            PayloadLength = payloadLength;
        }

        /// <summary>
        /// Gets the raw frame-kind byte at offset 2.
        /// </summary>
        /// <remarks>
        /// Deliberately a <see cref="byte"/> and not <see cref="NdLinkFrameKind"/>: only
        /// <c>0x20</c> and <c>0x3F</c> have been observed, and an unexpected value must reach a log
        /// rather than take the link down. Use <see cref="IsData"/> / <see cref="IsAcknowledge"/>
        /// for the known cases.
        /// </remarks>
        public byte Kind { get; }

        /// <summary>
        /// Gets the send sequence number at offset 4.
        /// </summary>
        public byte Sequence { get; }

        /// <summary>
        /// Gets the sender's link id (offsets 5-6, big-endian).
        /// </summary>
        public ushort SenderLinkId { get; }

        /// <summary>
        /// Gets the receiver's link id (offsets 7-8, big-endian).
        /// </summary>
        public ushort ReceiverLinkId { get; }

        /// <summary>
        /// Gets the payload length in bytes (offsets 9-10, big-endian).
        /// </summary>
        public ushort PayloadLength { get; }

        /// <summary>
        /// Gets a value indicating whether this frame carries a payload.
        /// </summary>
        public bool IsData => Kind == (byte)NdLinkFrameKind.Data;

        /// <summary>
        /// Gets a value indicating whether this frame is an acknowledgement.
        /// </summary>
        public bool IsAcknowledge => Kind == (byte)NdLinkFrameKind.Acknowledge;

        /// <summary>
        /// Builds a data header.
        /// </summary>
        /// <param name="sequence">
        /// The send sequence number.
        /// </param>
        /// <param name="senderLinkId">
        /// This node's link id.
        /// </param>
        /// <param name="receiverLinkId">
        /// The peer's link id.
        /// </param>
        /// <param name="payloadLength">
        /// The number of payload bytes that will follow.
        /// </param>
        /// <returns>
        /// A header with <see cref="Kind"/> set to <see cref="NdLinkFrameKind.Data"/>.
        /// </returns>
        public static NdLinkHeader Data(byte sequence, ushort senderLinkId, ushort receiverLinkId, ushort payloadLength)
        {
            return new NdLinkHeader((byte)NdLinkFrameKind.Data, sequence, senderLinkId, receiverLinkId, payloadLength);
        }

        /// <summary>
        /// Builds the acknowledgement for a received data frame.
        /// </summary>
        /// <param name="receivedSequence">
        /// The sequence number of the data frame being acknowledged.
        /// </param>
        /// <param name="senderLinkId">
        /// This node's link id.
        /// </param>
        /// <param name="receiverLinkId">
        /// The peer's link id.
        /// </param>
        /// <returns>
        /// A header with <see cref="Kind"/> set to <see cref="NdLinkFrameKind.Acknowledge"/>,
        /// sequence <paramref name="receivedSequence"/> + 1, and zero payload length.
        /// </returns>
        /// <remarks>
        /// The plus one is the observed convention - the acknowledgement carries the NEXT EXPECTED
        /// sequence, not the one being acknowledged. Verified on every data frame in the capture.
        /// </remarks>
        public static NdLinkHeader AcknowledgeFor(byte receivedSequence, ushort senderLinkId, ushort receiverLinkId)
        {
            byte next = unchecked((byte)(receivedSequence + 1));
            return new NdLinkHeader((byte)NdLinkFrameKind.Acknowledge, next, senderLinkId, receiverLinkId, 0);
        }

        /// <summary>
        /// Parses a header from the start of a buffer.
        /// </summary>
        /// <param name="source">
        /// The bytes to parse, starting at the header.
        /// </param>
        /// <param name="header">
        /// Receives the parsed header, or the default value when parsing fails.
        /// </param>
        /// <returns>
        /// True when the buffer is long enough and carries the expected signature bytes.
        /// </returns>
        public static bool TryParse(ReadOnlySpan<byte> source, out NdLinkHeader header)
        {
            if (source.Length < Length || source[0] != Signature0 || source[1] != Signature1)
            {
                header = default;
                return false;
            }

            header = new NdLinkHeader(
                source[2],
                source[4],
                (ushort)((source[5] << 8) | source[6]),
                (ushort)((source[7] << 8) | source[8]),
                (ushort)((source[9] << 8) | source[10]));
            return true;
        }

        /// <summary>
        /// Writes the header in wire order.
        /// </summary>
        /// <param name="destination">
        /// The buffer to write into; must be at least <see cref="Length"/> bytes.
        /// </param>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="destination"/> is shorter than <see cref="Length"/>.
        /// </exception>
        public void Write(Span<byte> destination)
        {
            if (destination.Length < Length)
            {
                throw new ArgumentException($"Need {Length} bytes to write an ND link header.", nameof(destination));
            }

            destination[0] = Signature0;
            destination[1] = Signature1;
            destination[2] = Kind;
            destination[3] = 0x00;
            destination[4] = Sequence;
            destination[5] = (byte)(SenderLinkId >> 8);
            destination[6] = (byte)(SenderLinkId & 0xFF);
            destination[7] = (byte)(ReceiverLinkId >> 8);
            destination[8] = (byte)(ReceiverLinkId & 0xFF);
            destination[9] = (byte)(PayloadLength >> 8);
            destination[10] = (byte)(PayloadLength & 0xFF);
        }
    }
}
