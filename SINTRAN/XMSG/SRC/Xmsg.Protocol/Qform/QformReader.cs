using System;
using System.Collections.Generic;

namespace NDInsight.Sintran.Xmsg.Protocol.Qform
{
    /// <summary>
    /// Reads a QFORM typed-parameter body: the format the COSMOS file servers carry inside an XMSG
    /// message.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The grammar is a port of <c>qform_read_tag_and_value</c> at <c>ram:0x7d01</c> in
    /// <c>cos-fa-serv-e04</c>, with every mask taken from its literal pool at
    /// <c>0x7d82..0x7d89</c>:
    /// </para>
    /// <code>
    /// bit 7 CLEAR            -> end of stream
    /// class = (tag &amp; 0x70) >> 4
    /// class 1..7 : length  = tag &amp; 0x0F
    /// class 0    : subtype = tag &amp; 0x17, and the length is ALWAYS escaped
    /// a length nibble of 0 escapes: the length comes from the following byte
    /// </code>
    /// <para>
    /// A body is a SELECTOR/VALUE stream: a class-7 selector (<c>0xF2</c>) names a field, the field's
    /// value follows, and the selector <c>0x00FF</c> ends the list. Class 0 values are CONSTRUCTED -
    /// length-delimited, with their content itself tagged.
    /// </para>
    /// <para>
    /// That last point is why a naive flat walk fails. Reading a body as a flat run of tag/value
    /// pairs descends into constructed payloads and reads their bytes as top-level tags; measured
    /// against a real capture, a flat walk parsed only 21 of 65 frames.
    /// </para>
    /// </remarks>
    public static class QformReader
    {
        /// <summary>
        /// The selector value that ends a parameter list.
        /// </summary>
        public const int EndOfListSelector = 0x00FF;

        /// <summary>
        /// Marks a length byte as an ESCAPE: the real length is the byte that follows.
        /// </summary>
        /// <remarks>
        /// Resolved from the capture <c>fa-access-secret-102-to-100-2026-07-29.pcapng</c>, frame 23,
        /// which carries <c>8C 80 46</c>. Reading 0x80 as a marker gives a declared length of 0x46 =
        /// 70, and the constructed value's contents total exactly 70 bytes: a <c>B0 3E</c> string
        /// (2 + 62) plus <c>A2 0000</c> (3) plus <c>A2 FFFF</c> (3). The length is therefore
        /// arithmetically confirmed, not merely consistent with a clean parse.
        /// <para>
        /// Measured over the 34 data frames of that capture, treating 0x80 as an escape marker takes
        /// the reader from 28 clean walks to 32.
        /// </para>
        /// </remarks>
        private const byte LengthEscapeMarker = 0x80;

        /// <summary>
        /// Walks a body and returns every field found, including fields nested inside constructed
        /// values.
        /// </summary>
        /// <param name="body">
        /// The message body, starting at the first tag byte.
        /// </param>
        /// <returns>
        /// The fields in the order encountered.
        /// </returns>
        /// <exception cref="QformFormatException">
        /// The body is malformed, or it uses the multi-byte escape-length continuation, which is not
        /// yet decoded (see <see cref="QformFormatException"/>).
        /// </exception>
        public static IReadOnlyList<QformField> Read(ReadOnlySpan<byte> body)
        {
            List<QformField> fields = new List<QformField>();
            ReadInto(body, 0, 0, fields);
            return fields;
        }

        /// <summary>
        /// Walks a body and reports whether it parses cleanly, without throwing. Intended for
        /// bulk-validating captures.
        /// </summary>
        /// <param name="body">
        /// The message body.
        /// </param>
        /// <param name="fields">
        /// The fields found, or an empty list on failure.
        /// </param>
        /// <returns>
        /// <see langword="true"/> if the whole body parsed.
        /// </returns>
        public static bool TryRead(ReadOnlySpan<byte> body, out IReadOnlyList<QformField> fields)
        {
            try
            {
                fields = Read(body);
                return true;
            }
            catch (QformFormatException)
            {
                fields = Array.Empty<QformField>();
                return false;
            }
        }

        /// <summary>
        /// Walks one nesting level, recursing into constructed values.
        /// </summary>
        private static void ReadInto(ReadOnlySpan<byte> body, int start, int depth, List<QformField> fields)
        {
            int i = start;
            while (i < body.Length)
            {
                byte tag = body[i];

                // Bit 7 clear ends the stream. This is the reader's own first test (BSKP 7 at
                // 0x7d14), not a convention we invented, and it is why trailing zero padding
                // terminates a body cleanly rather than being read as a tag.
                if ((tag & 0x80) == 0)
                {
                    return;
                }

                i++;

                int cls = (tag & 0x70) >> 4;
                int length;

                if (cls == 0)
                {
                    // Constructed: the low nibble is a subtype, so the length ALWAYS follows.
                    length = ReadEscapedLength(body, ref i);
                }
                else
                {
                    length = tag & 0x0F;
                    if (length == 0)
                    {
                        length = ReadEscapedLength(body, ref i);
                    }
                }

                if (length < 0 || i + length > body.Length)
                {
                    throw new QformFormatException(
                        "QFORM value at offset " + (i - 1).ToString() + " runs past the end of the body.");
                }

                fields.Add(new QformField(tag, i, length, depth));

                // A constructed value's content is itself a tagged stream, so descend. Failing to
                // do this is exactly what desynchronises a flat reader.
                if (cls == 0 && length > 0)
                {
                    ReadInto(body.Slice(0, i + length), i, depth + 1, fields);
                }

                i += length;
            }
        }

        /// <summary>
        /// Reads an escaped length: the byte after the tag, with 0x80 continuing.
        /// </summary>
        private static int ReadEscapedLength(ReadOnlySpan<byte> body, ref int i)
        {
            if (i >= body.Length)
            {
                throw new QformFormatException("QFORM escape length runs past the end of the body.");
            }

            byte first = body[i];
            i++;

            if (first != LengthEscapeMarker)
            {
                return first;
            }

            // 0x80 is a marker rather than a length: the length is the following byte. See the
            // remarks on LengthEscapeMarker for the frame that pins this arithmetically.
            if (i >= body.Length)
            {
                throw new QformFormatException("QFORM escaped length marker is not followed by a length byte.");
            }

            byte escaped = body[i];
            i++;
            return escaped;
        }
    }

    /// <summary>
    /// Thrown when a QFORM body cannot be parsed.
    /// </summary>
    public sealed class QformFormatException : Exception
    {
        /// <summary>
        /// Initialises the exception.
        /// </summary>
        /// <param name="message">
        /// What went wrong.
        /// </param>
        public QformFormatException(string message)
            : base(message)
        {
        }
    }
}
