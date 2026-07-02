using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// The fixed 13-byte SINTRAN header that prefixes every LAPB information field.
    /// </summary>
    /// <remarks>
    /// <para><b>Layout</b></para>
    /// Byte offsets (see XMSG-PROTOCOL.md section 4):
    ///  - <c>0</c> Marker 1, always <c>0x21</c>.
    ///  - <c>1</c> Marker 2, <c>0x13</c> normal or <c>0x12</c> relay.
    ///  - <c>2</c> Packet Type, <c>0x00</c> in all observed XMSG traffic.
    ///  - <c>3</c> Packet Subtype (message kind, not a length).
    ///  - <c>4</c>..<c>5</c> Destination node number (big-endian).
    ///  - <c>6</c>..<c>7</c> Source node number (big-endian).
    ///  - <c>8</c>..<c>9</c> Flags 1 (datagram sequence / broadcast marker).
    ///  - <c>10</c>..<c>11</c> Flags 2 (frame-class word).
    ///  - <c>12</c> Protocol ID (sub-protocol selector).
    /// </remarks>
    public sealed class SintranHeader
    {
        /// <summary>
        /// Serialised size of the SINTRAN header in bytes.
        /// </summary>
        public const int Size = 13;

        /// <summary>
        /// Marker 1 constant that opens every SINTRAN header.
        /// </summary>
        public const byte Marker1Value = 0x21;

        /// <summary>
        /// Marker 2 value for a normal (non-relayed) frame.
        /// </summary>
        public const byte Marker2Normal = 0x13;

        /// <summary>
        /// Marker 2 value for a relay frame (a node forwarding between two others).
        /// </summary>
        public const byte Marker2Relay = 0x12;

        /// <summary>
        /// Gets or sets the Marker 1 byte (offset 0). Normally <see cref="Marker1Value"/>.
        /// </summary>
        public byte Marker1 { get; set; } = Marker1Value;

        /// <summary>
        /// Gets or sets the Marker 2 byte (offset 1). Normally <see cref="Marker2Normal"/>.
        /// </summary>
        public byte Marker2 { get; set; } = Marker2Normal;

        /// <summary>
        /// Gets or sets the Packet Type byte (offset 2). <c>0x00</c> in observed traffic.
        /// </summary>
        public byte PacketType { get; set; }

        /// <summary>
        /// Gets or sets the Packet Subtype (offset 3) identifying the message kind.
        /// </summary>
        public SintranPacketSubtype Subtype { get; set; }

        /// <summary>
        /// Gets or sets the destination node number (offsets 4-5, big-endian).
        /// </summary>
        public ushort DestinationNode { get; set; }

        /// <summary>
        /// Gets or sets the source node number (offsets 6-7, big-endian).
        /// </summary>
        public ushort SourceNode { get; set; }

        /// <summary>
        /// Gets or sets Flags 1 (offsets 8-9): the datagram sequence number, or
        /// <c>0xFFFF</c> on broadcast/reachability frames.
        /// </summary>
        public ushort Flags1 { get; set; }

        /// <summary>
        /// Gets or sets Flags 2 (offsets 10-11): the frame-class word.
        /// </summary>
        public ushort Flags2 { get; set; }

        /// <summary>
        /// Gets or sets the Protocol ID (offset 12): the sub-protocol selector.
        /// </summary>
        public SintranProtocolId ProtocolId { get; set; }

        /// <summary>
        /// Parses a SINTRAN header from the first <see cref="Size"/> bytes of a span.
        /// </summary>
        /// <param name="source">
        /// The frame bytes; must contain at least <see cref="Size"/> bytes.
        /// </param>
        /// <returns>
        /// The parsed <see cref="SintranHeader"/>.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="source"/> is shorter than <see cref="Size"/> bytes.
        /// </exception>
        public static SintranHeader Parse(ReadOnlySpan<byte> source)
        {
            if (source.Length < Size)
            {
                throw new ArgumentException("SINTRAN header requires at least 13 bytes.", nameof(source));
            }

            SintranHeader header = new SintranHeader();
            header.Marker1 = source[0];
            header.Marker2 = source[1];
            header.PacketType = source[2];
            header.Subtype = (SintranPacketSubtype)source[3];
            header.DestinationNode = BigEndian.ReadUInt16(source.Slice(4, 2));
            header.SourceNode = BigEndian.ReadUInt16(source.Slice(6, 2));
            header.Flags1 = BigEndian.ReadUInt16(source.Slice(8, 2));
            header.Flags2 = BigEndian.ReadUInt16(source.Slice(10, 2));
            header.ProtocolId = (SintranProtocolId)source[12];
            return header;
        }

        /// <summary>
        /// Serialises this header into the first <see cref="Size"/> bytes of a span.
        /// </summary>
        /// <param name="destination">
        /// The destination span; must have room for at least <see cref="Size"/> bytes.
        /// </param>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="destination"/> is shorter than <see cref="Size"/> bytes.
        /// </exception>
        public void Serialize(Span<byte> destination)
        {
            if (destination.Length < Size)
            {
                throw new ArgumentException("SINTRAN header requires at least 13 bytes.", nameof(destination));
            }

            destination[0] = Marker1;
            destination[1] = Marker2;
            destination[2] = PacketType;
            destination[3] = (byte)Subtype;
            BigEndian.WriteUInt16(destination.Slice(4, 2), DestinationNode);
            BigEndian.WriteUInt16(destination.Slice(6, 2), SourceNode);
            BigEndian.WriteUInt16(destination.Slice(8, 2), Flags1);
            BigEndian.WriteUInt16(destination.Slice(10, 2), Flags2);
            destination[12] = (byte)ProtocolId;
        }

        /// <summary>
        /// Serialises this header into a freshly allocated 13-byte array.
        /// </summary>
        /// <returns>
        /// A new array containing the serialised header.
        /// </returns>
        public byte[] ToArray()
        {
            byte[] buffer = new byte[Size];
            Serialize(buffer);
            return buffer;
        }
    }
}
