using System;

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// Builds and reads the message bodies of a COSMOS file transfer: the data blocks that carry
    /// file content, the end-of-transfer marker, and the replies to both.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Captured from node 102 to node 100 in
    /// <c>claude-transfer-file-COMPLETE-102-to-100-2026-07-29.pcapng</c>, the first transfer ever
    /// captured carrying real file content:
    /// </para>
    /// <code>
    /// TRANSFER-FILE d100(system)."xfertest:data",DUMMY:DATA
    ///   -> Completed. Transfer rate: 3 Kbytes/sec
    /// </code>
    /// <para>
    /// <b>This class handles message BODIES only.</b> A transfer data message is 1030 bytes and does
    /// not fit in one LAPB frame, so on the wire it is segmented across two frames using packet
    /// subtypes <c>0x0A</c> and <c>0x0C</c>. That segmentation belongs to the frame layer;
    /// <see cref="SplitForTransmission"/> is provided to compute the split, but assembling the frames
    /// themselves is not this class's job.
    /// </para>
    /// <para>
    /// <b>The transfer is stop-and-wait.</b> Measured over the capture in wire order, the peak number
    /// of unacknowledged messages is exactly 1 - the sender never begins a message before the
    /// previous one is answered. A server may therefore process one message at a time and never needs
    /// to buffer a window of outstanding blocks.
    /// </para>
    /// </remarks>
    public static class FaTransferCodec
    {
        /// <summary>
        /// Function code of a data message carrying one block of file content.
        /// </summary>
        public const ushort FunctionData = 0x0042;

        /// <summary>
        /// Function code of the end-of-transfer marker.
        /// </summary>
        public const ushort FunctionEndOfTransfer = 0x0043;

        /// <summary>
        /// Size in bytes of the file-content block carried by one data message.
        /// </summary>
        public const int BlockLength = 1024;

        /// <summary>
        /// Size in bytes of the message header preceding the block: function, page, displacement.
        /// </summary>
        public const int DataHeaderLength = 6;

        /// <summary>
        /// Total length of a data message body.
        /// </summary>
        /// <remarks>
        /// 6 + 1024 = 1030, which is the length the frame's Flags2 announces.
        /// </remarks>
        public const int DataMessageLength = DataHeaderLength + BlockLength;

        /// <summary>
        /// Length of the short control bodies: end-of-transfer and every reply.
        /// </summary>
        public const int ControlMessageLength = 6;

        /// <summary>
        /// Number of body bytes carried by the FIRST of the two frames of a data message.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The remaining <c>1030 - 594 = 436</c> bytes travel in the continuation frame.
        /// </para>
        /// <para>
        /// <b>No longer a single-case observation, 2026-07-30.</b> This used to warn that only one
        /// transfer had been recorded, so 594 might be a property of that one file. A second transfer
        /// was then recorded, of <c>LOAD-MODE:BATC</c> - a file of only 167 bytes, a sixth of one
        /// block. It splits at exactly the same point.
        /// </para>
        /// <para>
        /// The reason is that a transfer always ships WHOLE PAGES. The 167-byte file produced two
        /// full 1030-byte messages, page 0 displacement 0 and page 0 displacement 512 words, padded.
        /// The message length never tracks how much of the file is real, so within a transfer this
        /// split cannot vary.
        /// </para>
        /// <para>
        /// Whether 594 would differ for a message of some OTHER length remains UNKNOWN, and probably
        /// cannot be tested: the transfer data message is the only message large enough to need
        /// splitting, and it is always 1030 bytes. Locked in by
        /// <c>TransferFragmentationTests.TransferMessage_IsAlwaysAWholeBlockNoMatterHowSmallTheFileIs</c>.
        /// </para>
        /// </remarks>
        public const int FirstFragmentBodyLength = 594;

        /// <summary>
        /// The displacement, in words, at which the second block of a page begins.
        /// </summary>
        /// <remarks>
        /// The displacement counts WORDS, not bytes: it advances 0 -> 512 within a page and resets as
        /// the page number increments, so one page holds two 1024-byte blocks.
        /// </remarks>
        public const ushort SecondBlockDisplacementWords = 512;

        /// <summary>
        /// Builds the body of a data message carrying one block of file content.
        /// </summary>
        /// <param name="page">
        /// The page number within the file.
        /// </param>
        /// <param name="displacementWords">
        /// The displacement within the page, in WORDS - 0 for the first block, 512 for the second.
        /// </param>
        /// <param name="block">
        /// Exactly <see cref="BlockLength"/> bytes of file content.
        /// </param>
        /// <returns>
        /// The <see cref="DataMessageLength"/>-byte message body.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// The block is the wrong length.
        /// </exception>
        public static byte[] BuildDataMessage(ushort page, ushort displacementWords, ReadOnlySpan<byte> block)
        {
            if (block.Length != BlockLength)
            {
                throw new ArgumentException(
                    "A transfer block is " + BlockLength + " bytes, got " + block.Length.ToString() + ".",
                    nameof(block));
            }

            byte[] body = new byte[DataMessageLength];
            WriteUInt16(body, 0, FunctionData);
            WriteUInt16(body, 2, page);
            WriteUInt16(body, 4, displacementWords);
            block.CopyTo(new Span<byte>(body, DataHeaderLength, BlockLength));
            return body;
        }

        /// <summary>
        /// Builds the end-of-transfer message body.
        /// </summary>
        /// <returns>
        /// The six-byte body <c>0043 FFFF FFFF</c>.
        /// </returns>
        public static byte[] BuildEndOfTransfer()
        {
            byte[] body = new byte[ControlMessageLength];
            WriteUInt16(body, 0, FunctionEndOfTransfer);
            WriteUInt16(body, 2, 0xFFFF);
            WriteUInt16(body, 4, 0xFFFF);
            return body;
        }

        /// <summary>
        /// Builds the reply to a data message.
        /// </summary>
        /// <returns>
        /// Six zero bytes.
        /// </returns>
        /// <remarks>
        /// The reply carries no reference to the message it answers - it does not need one, because
        /// the exchange is stop-and-wait and only one message is ever outstanding. The frame that
        /// carries it does repeat the data message's XMCSM, so its Flags2 reads 1030.
        /// </remarks>
        public static byte[] BuildDataReply()
        {
            return new byte[ControlMessageLength];
        }

        /// <summary>
        /// Builds the reply to the end-of-transfer message.
        /// </summary>
        /// <returns>
        /// The six-byte body <c>0000 0000 0001</c>.
        /// </returns>
        /// <remarks>
        /// This differs from <see cref="BuildDataReply"/> only in the final word, which is 1 rather
        /// than 0. The meaning of that word is UNKNOWN; it is reproduced because it is what the
        /// capture contains. It may be a status or a count, and one capture cannot tell them apart.
        /// </remarks>
        public static byte[] BuildEndOfTransferReply()
        {
            byte[] body = new byte[ControlMessageLength];
            WriteUInt16(body, 4, 0x0001);
            return body;
        }

        /// <summary>
        /// Reads the header of a data message body.
        /// </summary>
        /// <param name="body">
        /// The message body.
        /// </param>
        /// <param name="page">
        /// The page number within the file.
        /// </param>
        /// <param name="displacementWords">
        /// The displacement within the page, in words.
        /// </param>
        /// <param name="block">
        /// The file-content block, as a slice of <paramref name="body"/>.
        /// </param>
        /// <returns>
        /// <see langword="true"/> if the body was a well-formed data message.
        /// </returns>
        public static bool TryReadDataMessage(
            ReadOnlySpan<byte> body,
            out ushort page,
            out ushort displacementWords,
            out ReadOnlySpan<byte> block)
        {
            page = 0;
            displacementWords = 0;
            block = default;

            if (body.Length != DataMessageLength) { return false; }
            if (ReadUInt16(body, 0) != FunctionData) { return false; }

            page = ReadUInt16(body, 2);
            displacementWords = ReadUInt16(body, 4);
            block = body.Slice(DataHeaderLength, BlockLength);
            return true;
        }

        /// <summary>
        /// Determines whether a body is the end-of-transfer message.
        /// </summary>
        /// <param name="body">
        /// The message body.
        /// </param>
        /// <returns>
        /// <see langword="true"/> if this is the end-of-transfer marker.
        /// </returns>
        public static bool IsEndOfTransfer(ReadOnlySpan<byte> body)
        {
            return body.Length == ControlMessageLength && ReadUInt16(body, 0) == FunctionEndOfTransfer;
        }

        /// <summary>
        /// Computes the byte offset at which a data message body must be split across two frames.
        /// </summary>
        /// <param name="body">
        /// The message body to send.
        /// </param>
        /// <param name="first">
        /// The portion carried by the first fragment (subtype <c>0x0A</c>).
        /// </param>
        /// <param name="continuation">
        /// The portion carried by the continuation (subtype <c>0x0C</c>), or empty when the body fits
        /// in a single frame and needs no segmentation.
        /// </param>
        /// <remarks>
        /// The continuation frame's Flags2 is the offset at which it resumes, which is exactly
        /// <c>first.Length</c>.
        /// </remarks>
        public static void SplitForTransmission(
            ReadOnlySpan<byte> body,
            out ReadOnlySpan<byte> first,
            out ReadOnlySpan<byte> continuation)
        {
            if (body.Length <= FirstFragmentBodyLength)
            {
                first = body;
                continuation = default;
                return;
            }

            first = body.Slice(0, FirstFragmentBodyLength);
            continuation = body.Slice(FirstFragmentBodyLength);
        }

        /// <summary>
        /// Writes a big-endian 16-bit value.
        /// </summary>
        private static void WriteUInt16(byte[] destination, int at, ushort value)
        {
            destination[at] = (byte)(value >> 8);
            destination[at + 1] = (byte)value;
        }

        /// <summary>
        /// Reads a big-endian 16-bit value.
        /// </summary>
        private static ushort ReadUInt16(ReadOnlySpan<byte> source, int at)
        {
            return (ushort)((source[at] << 8) | source[at + 1]);
        }
    }
}
