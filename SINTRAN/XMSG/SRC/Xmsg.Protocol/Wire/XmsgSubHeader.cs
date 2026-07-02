using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// The XMSG (DC/XMSG) sub-header carried by a data message (subtype
    /// <see cref="SintranPacketSubtype.Data"/>) immediately after the SINTRAN header.
    /// </summary>
    /// <remarks>
    /// <para><b>Layout</b></para>
    /// Offsets relative to the start of the payload (SINTRAN offset 13 onward);
    /// see XMSG-PROTOCOL.md section 5:
    ///  - <c>0</c> Counter (per-direction, decrements).
    ///  - <c>1</c>..<c>2</c> Marker, always <c>0x21 0x00</c>.
    ///  - <c>3</c> Frame Flags.
    ///  - <c>4</c> Role (asker/responder hint).
    ///  - <c>5</c>..<c>6</c> XMDSY destination system (big-endian).
    ///  - <c>7</c>..<c>8</c> XMDPT destination port (big-endian).
    ///  - <c>9</c>..<c>10</c> XMSSY source system (big-endian).
    ///  - <c>11</c>..<c>12</c> XMSPT source port (big-endian).
    ///  - <c>13</c>..<c>16</c> XMCSM control / service word.
    ///  - <c>17</c> Pad, <c>0x00</c>.
    ///  - <c>18</c> XMLEN user-data length (low byte).
    /// </remarks>
    public sealed class XmsgSubHeader
    {
        /// <summary>
        /// Serialised size of the full XMSG sub-header in bytes.
        /// </summary>
        public const int Size = 19;

        /// <summary>
        /// First marker byte of the constant <c>0x21 0x00</c> sub-header marker.
        /// </summary>
        public const byte MarkerByte0 = 0x21;

        /// <summary>
        /// Second marker byte of the constant <c>0x21 0x00</c> sub-header marker.
        /// </summary>
        public const byte MarkerByte1 = 0x00;

        /// <summary>
        /// Gets or sets the per-direction counter (offset 0).
        /// </summary>
        public byte Counter { get; set; }

        /// <summary>
        /// Gets or sets the frame-flags byte (offset 3).
        /// </summary>
        public byte FrameFlags { get; set; }

        /// <summary>
        /// Gets or sets the role byte (offset 4): low nibble 4 = asker, 0 = responder.
        /// </summary>
        public byte Role { get; set; }

        /// <summary>
        /// Gets or sets the XMDSY destination system number (offsets 5-6, big-endian).
        /// </summary>
        public ushort DestinationSystem { get; set; }

        /// <summary>
        /// Gets or sets the XMDPT destination port (offsets 7-8, big-endian).
        /// </summary>
        public ushort DestinationPort { get; set; }

        /// <summary>
        /// Gets or sets the XMSSY source system number (offsets 9-10, big-endian).
        /// </summary>
        public ushort SourceSystem { get; set; }

        /// <summary>
        /// Gets or sets the XMSPT source port (offsets 11-12, big-endian).
        /// </summary>
        public ushort SourcePort { get; set; }

        /// <summary>
        /// Gets or sets the XMCSM control / service word (offsets 13-16), the dispatch
        /// key for ROUTING and TAD services.
        /// </summary>
        public uint ControlService { get; set; }

        /// <summary>
        /// Gets or sets the pad byte (offset 17), normally <c>0x00</c>.
        /// </summary>
        public byte Pad { get; set; }

        /// <summary>
        /// Gets or sets the XMLEN user-data length low byte (offset 18).
        /// </summary>
        public byte UserDataLength { get; set; }

        /// <summary>
        /// Parses a full XMSG sub-header from the first <see cref="Size"/> bytes of a span.
        /// </summary>
        /// <param name="source">
        /// The payload bytes (SINTRAN offset 13 onward); must contain at least
        /// <see cref="Size"/> bytes.
        /// </param>
        /// <returns>
        /// The parsed <see cref="XmsgSubHeader"/>.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="source"/> is shorter than <see cref="Size"/> bytes.
        /// </exception>
        public static XmsgSubHeader Parse(ReadOnlySpan<byte> source)
        {
            if (source.Length < Size)
            {
                throw new ArgumentException("XMSG sub-header requires at least 19 bytes.", nameof(source));
            }

            XmsgSubHeader sub = new XmsgSubHeader();
            sub.Counter = source[0];
            // source[1..2] is the fixed 0x21 0x00 marker - retained implicitly on serialise.
            sub.FrameFlags = source[3];
            sub.Role = source[4];
            sub.DestinationSystem = BigEndian.ReadUInt16(source.Slice(5, 2));
            sub.DestinationPort = BigEndian.ReadUInt16(source.Slice(7, 2));
            sub.SourceSystem = BigEndian.ReadUInt16(source.Slice(9, 2));
            sub.SourcePort = BigEndian.ReadUInt16(source.Slice(11, 2));
            sub.ControlService = BigEndian.ReadUInt32(source.Slice(13, 4));
            sub.Pad = source[17];
            sub.UserDataLength = source[18];
            return sub;
        }

        /// <summary>
        /// Serialises this sub-header into the first <see cref="Size"/> bytes of a span.
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
                throw new ArgumentException("XMSG sub-header requires at least 19 bytes.", nameof(destination));
            }

            destination[0] = Counter;
            destination[1] = MarkerByte0;
            destination[2] = MarkerByte1;
            destination[3] = FrameFlags;
            destination[4] = Role;
            BigEndian.WriteUInt16(destination.Slice(5, 2), DestinationSystem);
            BigEndian.WriteUInt16(destination.Slice(7, 2), DestinationPort);
            BigEndian.WriteUInt16(destination.Slice(9, 2), SourceSystem);
            BigEndian.WriteUInt16(destination.Slice(11, 2), SourcePort);
            BigEndian.WriteUInt32(destination.Slice(13, 4), ControlService);
            destination[17] = Pad;
            destination[18] = UserDataLength;
        }
    }
}
