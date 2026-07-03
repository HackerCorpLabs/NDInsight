using System;

using NDInsight.Sintran.Xmsg;

namespace NDInsight.Sintran.Xmsg.ListRouting
{
    /// <summary>
    /// Low-level wire helpers shared by <see cref="ListRoutingClient"/> and
    /// <see cref="ListRoutingServer"/> for assembling and reading the XSGSY
    /// information field (SINTRAN header + XMSG sub-header + raw parameter-block trailer).
    /// </summary>
    /// <remarks>
    /// VERIFIED trailer format (XMSG-PROTOCOL.md section 9.1): the XSGSY trailer is a
    /// sequence of 4-byte records <c>[param-number][length=2][value-hi][value-lo]</c>
    /// with a 16-bit big-endian value. Unlike a standard XROUT "letter" body it has
    /// NO serial/service/length header — the service is identified by the sub-header
    /// XMCSM word — so the trailer is assembled and read directly here rather than
    /// through <see cref="XroutMessage"/>.
    /// </remarks>
    internal static class XsgsyWire
    {
        /// <summary>
        /// Size in bytes of one XSGSY parameter block (<c>[num][len=2][hi][lo]</c>).
        /// </summary>
        internal const int ParamBlockSize = 4;

        /// <summary>
        /// Byte offset of the parameter-block trailer inside a data-frame information
        /// field (SINTRAN header + XMSG sub-header).
        /// </summary>
        internal const int TrailerOffset = SintranHeader.Size + XmsgSubHeader.Size;

        /// <summary>
        /// Assembles a full XSGSY information field from a header, sub-header, and trailer.
        /// </summary>
        /// <param name="header">
        /// The SINTRAN header to serialise first.
        /// </param>
        /// <param name="subHeader">
        /// The XMSG sub-header; its user-data length is set to the trailer length.
        /// </param>
        /// <param name="trailer">
        /// The raw parameter-block trailer bytes.
        /// </param>
        /// <returns>
        /// A new array holding the serialised information field.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="header"/> or <paramref name="subHeader"/> is null.
        /// </exception>
        internal static byte[] BuildInfoField(SintranHeader header, XmsgSubHeader subHeader, ReadOnlySpan<byte> trailer)
        {
            if (header == null)
            {
                throw new ArgumentNullException(nameof(header));
            }

            if (subHeader == null)
            {
                throw new ArgumentNullException(nameof(subHeader));
            }

            // VERIFIED (captures): XMLEN carries the trailer byte-length (4 for a request,
            // 16 for a four-parameter response). XMLEN is the low byte of the length field.
            subHeader.UserDataLength = (byte)trailer.Length;

            byte[] buffer = new byte[SintranHeader.Size + XmsgSubHeader.Size + trailer.Length];
            header.Serialize(buffer);
            subHeader.Serialize(buffer.AsSpan(SintranHeader.Size, XmsgSubHeader.Size));
            trailer.CopyTo(buffer.AsSpan(TrailerOffset));
            return buffer;
        }

        /// <summary>
        /// Writes one XSGSY parameter block into a span.
        /// </summary>
        /// <param name="destination">
        /// The destination span; must have room for at least <see cref="ParamBlockSize"/> bytes.
        /// </param>
        /// <param name="parameterNumber">
        /// The one-based parameter number (byte 0 of the block).
        /// </param>
        /// <param name="value">
        /// The 16-bit big-endian value (bytes 2-3 of the block).
        /// </param>
        internal static void WriteParamBlock(Span<byte> destination, byte parameterNumber, ushort value)
        {
            destination[0] = parameterNumber;
            destination[1] = 0x02; // VERIFIED (captures): every XSGSY value is a 2-byte integer.
            BigEndian.WriteUInt16(destination.Slice(2, 2), value);
        }

        /// <summary>
        /// Reads the 16-bit value of a named parameter from an XSGSY trailer.
        /// </summary>
        /// <param name="trailer">
        /// The raw parameter-block trailer bytes.
        /// </param>
        /// <param name="parameterNumber">
        /// The one-based parameter number to find.
        /// </param>
        /// <param name="value">
        /// When this method returns <c>true</c>, the parameter's 16-bit value;
        /// otherwise <c>0</c>.
        /// </param>
        /// <returns>
        /// <c>true</c> if the parameter was present; otherwise <c>false</c>.
        /// </returns>
        internal static bool TryReadParam(ReadOnlySpan<byte> trailer, byte parameterNumber, out ushort value)
        {
            int pos = 0;
            while (pos + 2 <= trailer.Length)
            {
                byte number = trailer[pos];
                int length = trailer[pos + 1];
                int dataStart = pos + 2;
                if (dataStart + length > trailer.Length)
                {
                    break; // truncated block - stop scanning.
                }

                if (number == parameterNumber && length >= 2)
                {
                    value = BigEndian.ReadUInt16(trailer.Slice(dataStart, 2));
                    return true;
                }

                pos = dataStart + length;
            }

            value = 0;
            return false;
        }

        /// <summary>
        /// Extracts the parameter-block trailer from a decoded data frame.
        /// </summary>
        /// <param name="frame">
        /// The frame whose trailer is wanted.
        /// </param>
        /// <returns>
        /// A new array holding the trailer bytes (empty if the frame has none).
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frame"/> is null.
        /// </exception>
        internal static byte[] ExtractTrailer(XmsgFrame frame)
        {
            if (frame == null)
            {
                throw new ArgumentNullException(nameof(frame));
            }

            // Serialise the frame back to its information field; when the frame was
            // decoded from a capture, ToArray reproduces the original bytes verbatim
            // (RawBytes is authoritative), so the trailer is read from real wire bytes.
            byte[] info = frame.ToArray();
            if (info.Length <= TrailerOffset)
            {
                return Array.Empty<byte>();
            }

            int trailerLength = info.Length - TrailerOffset;
            byte[] trailer = new byte[trailerLength];
            Array.Copy(info, TrailerOffset, trailer, 0, trailerLength);
            return trailer;
        }
    }
}
