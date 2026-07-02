using System;

namespace NDInsight.Sintran.Xmsg
{
    /// <summary>
    /// Big-endian read/write helpers for the XMSG wire format.
    /// </summary>
    /// <remarks>
    /// All multi-byte integers on the SINTRAN/XMSG wire are big-endian (most
    /// significant byte first). See XMSG-PROTOCOL.md section 1.
    /// </remarks>
    internal static class BigEndian
    {
        /// <summary>
        /// Reads a big-endian 16-bit unsigned integer from the start of a span.
        /// </summary>
        /// <param name="source">
        /// The span to read from; must contain at least two bytes.
        /// </param>
        /// <returns>
        /// The 16-bit value formed from <c>source[0]</c> (high) and <c>source[1]</c> (low).
        /// </returns>
        public static ushort ReadUInt16(ReadOnlySpan<byte> source)
        {
            // High byte first, then low byte - big-endian per XMSG-PROTOCOL.md section 1.
            return (ushort)((source[0] << 8) | source[1]);
        }

        /// <summary>
        /// Reads a big-endian 32-bit unsigned integer from the start of a span.
        /// </summary>
        /// <param name="source">
        /// The span to read from; must contain at least four bytes.
        /// </param>
        /// <returns>
        /// The 32-bit value formed from the first four bytes, most significant first.
        /// </returns>
        public static uint ReadUInt32(ReadOnlySpan<byte> source)
        {
            return ((uint)source[0] << 24)
                 | ((uint)source[1] << 16)
                 | ((uint)source[2] << 8)
                 | source[3];
        }

        /// <summary>
        /// Writes a big-endian 16-bit unsigned integer to the start of a span.
        /// </summary>
        /// <param name="destination">
        /// The span to write to; must have room for at least two bytes.
        /// </param>
        /// <param name="value">
        /// The 16-bit value to serialise.
        /// </param>
        public static void WriteUInt16(Span<byte> destination, ushort value)
        {
            destination[0] = (byte)(value >> 8);
            destination[1] = (byte)value;
        }

        /// <summary>
        /// Writes a big-endian 32-bit unsigned integer to the start of a span.
        /// </summary>
        /// <param name="destination">
        /// The span to write to; must have room for at least four bytes.
        /// </param>
        /// <param name="value">
        /// The 32-bit value to serialise.
        /// </param>
        public static void WriteUInt32(Span<byte> destination, uint value)
        {
            destination[0] = (byte)(value >> 24);
            destination[1] = (byte)(value >> 16);
            destination[2] = (byte)(value >> 8);
            destination[3] = (byte)value;
        }
    }
}
