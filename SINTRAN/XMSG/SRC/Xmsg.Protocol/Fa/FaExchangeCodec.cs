using System;

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// The envelope shared by every <c>*FA-SERVER</c> operation: the message type, the conversation
    /// number, the session header, and the operation-code / sequence pair that opens the QFORM body.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Dumping FILE-STATISTICS and DELETE-FILE side by side, in wire order and in full, showed the
    /// two operations are the SAME conversation shape. Their opening 112-byte request is
    /// byte-identical apart from the conversation number:
    /// </para>
    /// <code>
    /// FILE-STAT    07F0 0046  8000 0001  92 0002 92 0001 ...
    /// DELETE-FILE  07F0 0048  8000 0001  92 0002 92 0001 ...
    ///                   ^^^^ conversation number only
    /// </code>
    /// <para>
    /// <b>So the operation is not named anywhere in the opening exchange.</b> That is consistent with
    /// the decentralised dispatch seen in the disassembly, and it is why the 16-bit word after the
    /// message type was concluded to be a conversation counter rather than an opcode: across the
    /// captures it runs 003F, 0044, 0046, 0048 and never repeats.
    /// </para>
    /// <para><b>Body layout</b></para>
    /// <code>
    /// off 0  message type      07F0 request, 07A2 short, 0782, 07C0 / 07D2 closing
    /// off 2  conversation      chosen by the asker; the responder answers with 0002
    /// off 4  session header    80+n, 00, then 0001 on the first exchange and D761 after
    /// off 8  92 &lt;operation&gt;    the operation code for THIS exchange
    /// off 11 92 &lt;sequence&gt;     the exchange number, 1, 2, 3 ...
    /// off 14 ... QFORM fields, ending F2 00FF
    /// </code>
    /// <para>
    /// The operation code and sequence are echoed by the reply, which is what lets a reply be matched
    /// to its request.
    /// </para>
    /// </remarks>
    public static class FaExchangeCodec
    {
        /// <summary>
        /// Message type of a request carrying a QFORM body.
        /// </summary>
        public const ushort MessageTypeRequest = 0x07F0;

        /// <summary>
        /// Message type of the short eight-byte acknowledgement.
        /// </summary>
        public const ushort MessageTypeShortAck = 0x07A2;

        /// <summary>
        /// Conversation number the responder uses on every reply.
        /// </summary>
        /// <remarks>
        /// The asker picks its own conversation number, but node 100 answered with <c>0x0002</c> in
        /// every captured operation.
        /// </remarks>
        public const ushort ResponderConversation = 0x0002;

        /// <summary>
        /// Session token carried on the first exchange of a conversation.
        /// </summary>
        public const ushort SessionTokenFirst = 0x0001;

        /// <summary>
        /// Session token carried on every exchange after the first, asker side.
        /// </summary>
        public const ushort SessionTokenAsker = 0xD761;

        /// <summary>
        /// Session token carried on every exchange after the first, responder side.
        /// </summary>
        /// <remarks>
        /// <para>
        /// <b>NOT A CONSTANT - corrected 2026-07-30.</b> This value held on the FILE-STATISTICS and
        /// DELETE-FILE recordings, so it was fixed here as a constant. The CREATE-FILE recording taken
        /// later the same day uses <c>0x909E</c> on every server reply instead.
        /// </para>
        /// <para>
        /// Its meaning is UNKNOWN. It is per-conversation, not per-message: it is identical on every
        /// reply within one conversation and differs between conversations. Treat this constant as
        /// "the value seen in the two 2026-07-29 recordings" and read the real one off the first
        /// reply, rather than asserting it.
        /// </para>
        /// </remarks>
        public const ushort SessionTokenResponder = 0x9081;

        /// <summary>
        /// The responder session token seen in the CREATE-FILE recording of 2026-07-30.
        /// </summary>
        /// <remarks>
        /// Recorded only so the two known values sit side by side and neither looks like the rule. Two
        /// samples are not a pattern; do not infer one from them.
        /// </remarks>
        public const ushort SessionTokenResponderCreateFile = 0x909E;

        /// <summary>
        /// Operation code opening a conversation: the directory and user spec.
        /// </summary>
        /// <remarks>
        /// Carried by exchange 1 of both FILE-STATISTICS and DELETE-FILE, whose bodies are otherwise
        /// identical. The payload is the pack and user - captured as <c>BAK05  SYSTEM</c> - and a
        /// 56-byte constructed block holding <c>SYSTEM'</c>.
        /// </remarks>
        public const ushort OperationOpenSpec = 0x0002;

        /// <summary>
        /// Operation code of a directory or file enquiry.
        /// </summary>
        /// <remarks>
        /// Used by LIST-FILES and by exchange 2 of FILE-STATISTICS.
        /// </remarks>
        public const ushort OperationDirectoryEnquiry = 0x000C;

        /// <summary>
        /// Operation code of a delete request.
        /// </summary>
        /// <remarks>
        /// Carried by exchange 2 of DELETE-FILE, whose payload is the file name as a plain string -
        /// <c>XFERTEST:DATA</c>. This is the field that distinguishes deleting from enquiring; the
        /// opening exchange does not.
        /// </remarks>
        public const ushort OperationDelete = 0x000B;

        /// <summary>
        /// Operation code that creates a file with a reserved page count and no contents.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Recorded 2026-07-30 in
        /// <c>claude-create-file-102-to-100-2026-07-30.pcapng</c>. The request carries the file name
        /// and the page count and nothing else:
        /// </para>
        /// <code>
        /// 92 000A                       operation
        /// 92 0002                       exchange sequence
        /// F2 0002                       selector 2
        /// BE "RONNY-R2:TXT'" + pad      file name, 0x27 terminated
        /// F2 0004                       selector 4
        /// A4 0000 0001                  page count, 32-bit
        /// F2 00FF                       end of list
        /// </code>
        /// <para>
        /// The file it produces is a CONTINUOUS file - node 100's own <c>FILE-STATISTICS</c> reports
        /// <c>(CONTINUOUS FILE)</c>, 1 page, 0 bytes. So contiguity is implied by the operation and is
        /// not a field on the wire.
        /// </para>
        /// </remarks>
        public const ushort OperationCreate = 0x000A;

        /// <summary>
        /// Operation code that opens a file and returns the file number to use for it.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Recorded 2026-07-30 in <c>claude-open-close-file-102-to-100-2026-07-30.pcapng</c>, driving
        /// <c>OPEN-FILE d100(system).ronny-r2:txt,R</c> from node 102:
        /// </para>
        /// <code>
        /// request  92 0005  92 0002  F2 0002  BE "RONNY-R2:TXT'" + pad  F2 00FF
        /// reply    92 0005  92 0002  F2 0002  A2 0040                   F2 00FF
        /// </code>
        /// <para>
        /// <b>The number in the reply is the REMOTE file number, not the one the caller sees.</b>
        /// Node 100 answered <c>0x0040</c>, which is 100 octal, while node 102 printed
        /// <c>FILE NUMBER IS 000101</c> to the operator. 100 octal was already taken locally by
        /// <c>SCRATCH03:DATA</c>, visible in that machine's own <c>LIST-OPEN-FILES</c>, so the local
        /// system clearly allocates its own handle and maps it to the remote one. A client must not
        /// assume the two match.
        /// </para>
        /// <para>
        /// The access type asked for at the terminal - <c>R</c> here - does NOT appear anywhere in the
        /// request. Where it goes is UNKNOWN; only one access type has been recorded, so it may be
        /// carried somewhere already dismissed as constant, or negotiated earlier.
        /// </para>
        /// </remarks>
        public const ushort OperationOpenFile = 0x0005;

        /// <summary>
        /// Operation code seen once after an open, carrying no fields in either direction.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Both request and reply are bare: <c>92 0006 92 &lt;sequence&gt; F2 00FF</c>.
        /// </para>
        /// <para>
        /// <b>Meaning INFERRED, not established.</b> It appears between the open and the closing
        /// <see cref="OperationClose"/> exchange, and the terminal session that had the file open was
        /// disconnected at that point, so it is consistent with closing the file. But the operator
        /// also typed <c>CLOSE-FILE</c> in a later session, and only ONE conversation appears in the
        /// recording, so which action produced this exchange is not proven. Do not treat it as a
        /// close until a recording isolates it.
        /// </para>
        /// </remarks>
        public const ushort OperationSixUnknown = 0x0006;

        /// <summary>
        /// Operation code closing a conversation.
        /// </summary>
        /// <remarks>
        /// Carried by the final exchange of both captured operations, as <c>92 0003</c>.
        /// </remarks>
        public const ushort OperationClose = 0x0003;

        /// <summary>
        /// Offset of the message type within a body.
        /// </summary>
        public const int MessageTypeOffset = 0;

        /// <summary>
        /// Offset of the conversation number within a body.
        /// </summary>
        public const int ConversationOffset = 2;

        /// <summary>
        /// Offset of the four-byte session header within a body.
        /// </summary>
        public const int SessionHeaderOffset = 4;

        /// <summary>
        /// Offset at which the QFORM fields begin.
        /// </summary>
        public const int QformOffset = 8;

        /// <summary>
        /// Smallest body that can carry the envelope and one field pair.
        /// </summary>
        public const int MinimumBodyLength = 14;

        /// <summary>
        /// Reads the envelope of a file-server message body.
        /// </summary>
        /// <param name="body">
        /// The message body, starting at the message type.
        /// </param>
        /// <param name="messageType">
        /// The message type word.
        /// </param>
        /// <param name="conversation">
        /// The conversation number.
        /// </param>
        /// <param name="sequenceByte">
        /// The session header's first byte, <c>0x80 + n</c>, where <c>n</c> counts exchanges from
        /// zero.
        /// </param>
        /// <param name="sessionToken">
        /// The 16-bit session token.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the body was long enough to carry an envelope.
        /// </returns>
        public static bool TryReadEnvelope(
            ReadOnlySpan<byte> body,
            out ushort messageType,
            out ushort conversation,
            out byte sequenceByte,
            out ushort sessionToken)
        {
            messageType = 0;
            conversation = 0;
            sequenceByte = 0;
            sessionToken = 0;

            if (body.Length < QformOffset) { return false; }

            messageType = ReadUInt16(body, MessageTypeOffset);
            conversation = ReadUInt16(body, ConversationOffset);
            sequenceByte = body[SessionHeaderOffset];
            sessionToken = ReadUInt16(body, SessionHeaderOffset + 2);
            return true;
        }

        /// <summary>
        /// Reads the operation code and exchange sequence that open the QFORM body.
        /// </summary>
        /// <param name="body">
        /// The message body, starting at the message type.
        /// </param>
        /// <param name="operation">
        /// The operation code for this exchange.
        /// </param>
        /// <param name="sequence">
        /// The exchange number, counting from one.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when both fields were present and correctly tagged.
        /// </returns>
        /// <remarks>
        /// Both fields are class-1 two-byte integers, tag <c>0x92</c>. A body whose first two fields
        /// are not tagged that way is not one of these exchanges, and the method reports false rather
        /// than returning bytes read from the wrong place.
        /// </remarks>
        public static bool TryReadOperation(ReadOnlySpan<byte> body, out ushort operation, out ushort sequence)
        {
            operation = 0;
            sequence = 0;

            if (body.Length < MinimumBodyLength) { return false; }
            if (body[QformOffset] != 0x92 || body[QformOffset + 3] != 0x92) { return false; }

            operation = ReadUInt16(body, QformOffset + 1);
            sequence = ReadUInt16(body, QformOffset + 4);
            return true;
        }

        /// <summary>
        /// Determines whether a body is a reply rather than a request.
        /// </summary>
        /// <param name="body">
        /// The message body.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the conversation number is the responder's.
        /// </returns>
        public static bool IsReply(ReadOnlySpan<byte> body)
        {
            if (body.Length < QformOffset) { return false; }
            return ReadUInt16(body, ConversationOffset) == ResponderConversation;
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
