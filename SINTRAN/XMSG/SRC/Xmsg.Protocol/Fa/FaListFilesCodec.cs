using System;
using System.Collections.Generic;
using NDInsight.Sintran.Xmsg.Protocol.Qform;

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// Builds and reads the QFORM bodies of a remote <c>LIST-FILES</c>: the request that asks for one
    /// directory entry, and the reply that returns it.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A remote directory listing is <b>not</b> one bulk reply. It is one request/reply round trip per
    /// entry, driven by a cursor. Captured from node 102 to node 100 in
    /// <c>claude-list-files-d100-system-2026-07-29.pcapng</c>, ten entries in a row:
    /// </para>
    /// <code>
    /// request  92 000C  92 &lt;serial&gt;  F2 0001 92 0078  F2 0002 8C 80 46 &lt;block&gt;  F2 00FF
    /// reply    92 000C  92 &lt;serial&gt;                    F2 0002 8C 4B &lt;record&gt;     F2 00FF
    /// </code>
    /// <para>
    /// The request body is byte-identical from one entry to the next except for the serial and the
    /// LAST of the two trailing <c>A2</c> values inside the constructed block. That value is the
    /// cursor: <c>0xFFFF</c> asks for the first entry, and thereafter it is the index of the entry
    /// wanted. The reply echoes the request's serial.
    /// </para>
    /// <para>
    /// <b>The 64-byte record is deliberately opaque here.</b> It is the SINTRAN on-disk object entry
    /// shipped verbatim - the file server does not reformat it - so its layout belongs to the file
    /// system, not to this wire codec. Decode it with
    /// <c>RetroFS.NDFS.Elements.ObjectEntry.FromBytes</c>; the adapter in <c>Xmsg.Ndfs</c> does
    /// exactly that. Duplicating the layout here would mean two copies of the same structure.
    /// </para>
    /// <para>
    /// The 62-byte block that precedes the cursor carries the directory and user spec - captured as
    /// <c>"(SYSTEM)'..."</c> - and is <b>not decoded</b>. It is handled as opaque bytes so a captured
    /// listing can be replayed exactly. Do not synthesise one.
    /// </para>
    /// </remarks>
    public static class FaListFilesCodec
    {
        /// <summary>The cursor value that asks for the first entry in the directory.</summary>
        public const ushort FirstEntryCursor = 0xFFFF;

        /// <summary>The length of the opaque directory/user block in a request.</summary>
        public const int SpecBlockLength = 62;

        /// <summary>
        /// The length of the directory entry record carried in a reply.
        /// </summary>
        /// <remarks>
        /// This is the SINTRAN object entry size, and it matches
        /// <c>RetroFS.NDFS.Elements.ObjectEntry</c>, which reads and writes 64 bytes.
        /// </remarks>
        public const int EntryRecordLength = 64;

        /// <summary>The leading constant seen on every body, request and reply alike.</summary>
        /// <remarks>Meaning UNKNOWN. It was 0x000C on every captured frame.</remarks>
        public const ushort LeadingConstant = 0x000C;

        /// <summary>The value carried under selector 1 in a request.</summary>
        /// <remarks>Meaning UNKNOWN. It was 0x0078 on every captured request.</remarks>
        public const ushort RequestSelector1Value = 0x0078;

        /// <summary>Selector naming the field that carries the request block or the entry.</summary>
        public const ushort PayloadSelector = 0x0002;

        /// <summary>
        /// Builds the body of a request for one directory entry.
        /// </summary>
        /// <param name="serial">The transaction serial; the reply echoes it.</param>
        /// <param name="cursor">
        /// <see cref="FirstEntryCursor"/> for the first entry, otherwise the index wanted.
        /// </param>
        /// <param name="specBlock">
        /// The opaque <see cref="SpecBlockLength"/>-byte directory and user block, taken from a
        /// capture. See the remarks on the class.
        /// </param>
        /// <returns>The QFORM body, ready to place in an XMSG message.</returns>
        /// <exception cref="ArgumentException">The block is the wrong length.</exception>
        public static byte[] BuildRequest(ushort serial, ushort cursor, ReadOnlySpan<byte> specBlock)
        {
            if (specBlock.Length != SpecBlockLength)
            {
                throw new ArgumentException(
                    "The directory spec block is " + SpecBlockLength + " bytes, got " + specBlock.Length.ToString() + ".",
                    nameof(specBlock));
            }

            // 92 000C | 92 serial | F2 0001 92 0078 | F2 0002 8C 80 46 <70> | F2 00FF
            byte[] body = new byte[3 + 3 + 3 + 3 + 3 + 3 + 70 + 3];
            int at = 0;

            at = WriteTagged(body, at, 0x92, LeadingConstant);
            at = WriteTagged(body, at, 0x92, serial);
            at = WriteTagged(body, at, 0xF2, 0x0001);
            at = WriteTagged(body, at, 0x92, RequestSelector1Value);
            at = WriteTagged(body, at, 0xF2, PayloadSelector);

            // The constructed value: tag, the 0x80 escape marker, then the real length.
            body[at++] = 0x8C;
            body[at++] = 0x80;
            body[at++] = 70;

            body[at++] = 0xB0;
            body[at++] = SpecBlockLength;
            for (int i = 0; i < SpecBlockLength; i++)
            {
                body[at++] = specBlock[i];
            }

            at = WriteTagged(body, at, 0xA2, 0x0000);
            at = WriteTagged(body, at, 0xA2, cursor);

            at = WriteTagged(body, at, 0xF2, QformReader.EndOfListSelector);

            if (at != body.Length)
            {
                throw new InvalidOperationException("Request body length mismatch: wrote " + at + " of " + body.Length + ".");
            }

            return body;
        }

        /// <summary>
        /// Builds the body of a reply carrying one directory entry.
        /// </summary>
        /// <param name="serial">The serial from the request being answered.</param>
        /// <param name="entryRecord">
        /// The <see cref="EntryRecordLength"/>-byte object entry to return, already in on-disk form.
        /// </param>
        /// <param name="leadingA">The first <c>A2</c> value inside the constructed block.</param>
        /// <param name="leadingB">The second <c>A2</c> value inside the constructed block.</param>
        /// <returns>The QFORM body.</returns>
        /// <remarks>
        /// The two leading values are passed in rather than assumed: captured replies carried
        /// <c>0x0000</c> and <c>0x0007</c> in the first slot on different sessions, and their meaning
        /// is UNKNOWN. The third value was <c>0x0001</c> on every capture and is written as such.
        /// </remarks>
        /// <exception cref="ArgumentException">The record is the wrong length.</exception>
        public static byte[] BuildReply(
            ushort serial,
            ReadOnlySpan<byte> entryRecord,
            ushort leadingA = 0,
            ushort leadingB = 0)
        {
            if (entryRecord.Length != EntryRecordLength)
            {
                throw new ArgumentException(
                    "A directory entry record is " + EntryRecordLength + " bytes, got " + entryRecord.Length.ToString() + ".",
                    nameof(entryRecord));
            }

            // 92 000C | 92 serial | F2 0002 8C 4B <75> | F2 00FF
            byte[] body = new byte[3 + 3 + 3 + 2 + 75 + 3];
            int at = 0;

            at = WriteTagged(body, at, 0x92, LeadingConstant);
            at = WriteTagged(body, at, 0x92, serial);
            at = WriteTagged(body, at, 0xF2, PayloadSelector);

            // Constructed, 75 bytes: three A2 pairs (9) plus B0 40 and the 64-byte record (66).
            body[at++] = 0x8C;
            body[at++] = 75;

            at = WriteTagged(body, at, 0xA2, leadingA);
            at = WriteTagged(body, at, 0xA2, leadingB);
            at = WriteTagged(body, at, 0xA2, 0x0001);

            body[at++] = 0xB0;
            body[at++] = EntryRecordLength;
            for (int i = 0; i < EntryRecordLength; i++)
            {
                body[at++] = entryRecord[i];
            }

            at = WriteTagged(body, at, 0xF2, QformReader.EndOfListSelector);

            if (at != body.Length)
            {
                throw new InvalidOperationException("Reply body length mismatch: wrote " + at + " of " + body.Length + ".");
            }

            return body;
        }

        /// <summary>
        /// Reads the serial and cursor out of a request body.
        /// </summary>
        /// <param name="body">The QFORM body.</param>
        /// <param name="serial">The transaction serial.</param>
        /// <param name="cursor">The directory cursor.</param>
        /// <returns><see langword="true"/> if the body looked like a listing request.</returns>
        public static bool TryReadRequest(ReadOnlySpan<byte> body, out ushort serial, out ushort cursor)
        {
            serial = 0;
            cursor = 0;

            IReadOnlyList<QformField> fields;
            if (!QformReader.TryRead(body, out fields) || fields.Count < 2)
            {
                return false;
            }

            serial = ReadUInt16(body, fields[1]);

            // The cursor is the last nested A2 - the deepest, latest integer in the body.
            bool found = false;
            for (int i = 0; i < fields.Count; i++)
            {
                if (fields[i].Depth == 1 && fields[i].Class == QformClass.TypedInteger && fields[i].ValueLength == 2)
                {
                    cursor = ReadUInt16(body, fields[i]);
                    found = true;
                }
            }

            return found;
        }

        /// <summary>
        /// Reads the serial and the raw directory entry record out of a reply body.
        /// </summary>
        /// <param name="body">The QFORM body.</param>
        /// <param name="serial">The echoed transaction serial.</param>
        /// <param name="entryRecord">
        /// The <see cref="EntryRecordLength"/>-byte object entry, exactly as it arrived. Decode it
        /// with <c>RetroFS.NDFS.Elements.ObjectEntry.FromBytes</c>.
        /// </param>
        /// <returns><see langword="true"/> if the body carried an entry.</returns>
        public static bool TryReadReply(ReadOnlySpan<byte> body, out ushort serial, out byte[] entryRecord)
        {
            serial = 0;
            entryRecord = Array.Empty<byte>();

            IReadOnlyList<QformField> fields;
            if (!QformReader.TryRead(body, out fields) || fields.Count < 2)
            {
                return false;
            }

            serial = ReadUInt16(body, fields[1]);

            for (int i = 0; i < fields.Count; i++)
            {
                if (fields[i].Class == QformClass.ByteString && fields[i].ValueLength == EntryRecordLength)
                {
                    entryRecord = new byte[EntryRecordLength];
                    for (int j = 0; j < EntryRecordLength; j++)
                    {
                        entryRecord[j] = body[fields[i].ValueOffset + j];
                    }

                    return true;
                }
            }

            return false;
        }

        /// <summary>Writes a tag byte followed by a two-byte value.</summary>
        private static int WriteTagged(byte[] destination, int at, byte tag, int value)
        {
            destination[at] = tag;
            destination[at + 1] = (byte)(value >> 8);
            destination[at + 2] = (byte)value;
            return at + 3;
        }

        /// <summary>Reads a two-byte value out of a field.</summary>
        private static ushort ReadUInt16(ReadOnlySpan<byte> body, QformField field)
        {
            if (field.ValueLength < 2) { return 0; }
            return (ushort)((body[field.ValueOffset] << 8) | body[field.ValueOffset + 1]);
        }
    }
}
