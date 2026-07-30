using System;

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// Drives the client side of a <c>*FA-SERVER</c> conversation: it holds the conversation number,
    /// the exchange sequence and the session token, and builds each request body in turn.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every captured file-server operation has the same shape - open, one or more operation
    /// exchanges, close - and differs only in the operation codes and payloads. This class owns the
    /// part that is identical across operations, so a client for a new operation only has to supply
    /// its own operation code and payload.
    /// </para>
    /// <para>
    /// <b>Payloads are opaque here, on purpose.</b> The opening exchange carries a directory and user
    /// spec whose internal layout is only partly understood, and synthesising one from a guess would
    /// produce a request the server rejects. Callers pass the QFORM field bytes they want sent -
    /// taken from a capture until the layout is decoded - and this class supplies the envelope
    /// around them.
    /// </para>
    /// <para>
    /// <b>Structural resemblance to RR-LIB.</b> The captured conversation looks like the
    /// request-response model in <c>Xmsg.Api.Rr</c>: an opening letter carrying user data that names
    /// the server, a confirmation, request/response pairs, then a disconnect. That resemblance is
    /// NOT verified - no document has been found stating that <c>*FA-SERVER</c> is an RR-LIB server,
    /// and the mapping is inferred from shape alone. It is recorded because it is a useful lead, not
    /// because it is established.
    /// </para>
    /// </remarks>
    public sealed class FaClientConversation
    {
        /// <summary>
        /// The conversation number this client uses on every request.
        /// </summary>
        private readonly ushort _conversation;

        /// <summary>
        /// How many exchanges have been built so far.
        /// </summary>
        private int _exchangesBuilt;

        /// <summary>
        /// Starts a conversation.
        /// </summary>
        /// <param name="conversation">
        /// The conversation number to use. The asker picks this; across the captures it ran 003F,
        /// 0044, 0046, 0048 and never repeated, so it behaves as a counter rather than an opcode.
        /// </param>
        public FaClientConversation(ushort conversation)
        {
            _conversation = conversation;
        }

        /// <summary>
        /// Gets the conversation number in use.
        /// </summary>
        public ushort Conversation
        {
            get { return _conversation; }
        }

        /// <summary>
        /// Gets how many exchanges have been built.
        /// </summary>
        public int ExchangesBuilt
        {
            get { return _exchangesBuilt; }
        }

        /// <summary>
        /// Builds the next request body, advancing the exchange sequence.
        /// </summary>
        /// <param name="operation">
        /// The operation code for this exchange - see the <c>Operation*</c> constants on
        /// <see cref="FaExchangeCodec"/>.
        /// </param>
        /// <param name="qformFields">
        /// The QFORM fields that follow the operation and sequence pair, including the closing
        /// <c>F2 00FF</c> end-of-list selector. Pass an empty span for an exchange that carries
        /// nothing but the pair and the terminator, and it will be added.
        /// </param>
        /// <returns>
        /// The complete message body, ready to place in an XMSG message.
        /// </returns>
        public byte[] BuildRequest(FaOperation operation, ReadOnlySpan<byte> qformFields)
        {
            // 07F0 | conversation | session header (4) | 92 operation | 92 sequence | fields
            int length = FaExchangeCodec.QformOffset + 3 + 3 + qformFields.Length;
            byte[] body = new byte[length];

            WriteUInt16(body, FaExchangeCodec.MessageTypeOffset, FaExchangeCodec.MessageTypeRequest);
            WriteUInt16(body, FaExchangeCodec.ConversationOffset, _conversation);

            // Session header: 0x80 + n, a zero byte, then the token. The token is 0001 on the very
            // first exchange and the steady-state asker value on every one after it.
            body[FaExchangeCodec.SessionHeaderOffset] = (byte)(0x80 + _exchangesBuilt);
            body[FaExchangeCodec.SessionHeaderOffset + 1] = 0x00;
            WriteUInt16(
                body,
                FaExchangeCodec.SessionHeaderOffset + 2,
                _exchangesBuilt == 0 ? FaExchangeCodec.SessionTokenFirst : FaExchangeCodec.SessionTokenAsker);

            int at = FaExchangeCodec.QformOffset;
            at = WriteTagged(body, at, 0x92, (ushort)operation);
            at = WriteTagged(body, at, 0x92, (ushort)(_exchangesBuilt + 1));

            qformFields.CopyTo(new Span<byte>(body, at, qformFields.Length));

            _exchangesBuilt++;
            return body;
        }

        /// <summary>
        /// Checks that a reply belongs to this conversation's exchange.
        /// </summary>
        /// <param name="body">
        /// The reply body.
        /// </param>
        /// <param name="expectedOperation">
        /// The operation code the request carried.
        /// </param>
        /// <param name="expectedSequence">
        /// The exchange sequence the request carried.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the reply echoes both fields.
        /// </returns>
        /// <remarks>
        /// The reply echoes the operation code and the exchange sequence, which is the only way to
        /// match a reply to its request - the conversation number is not echoed, because the
        /// responder answers with its own.
        /// </remarks>
        public static bool IsReplyTo(ReadOnlySpan<byte> body, FaOperation expectedOperation, ushort expectedSequence)
        {
            if (!FaExchangeCodec.IsReply(body)) { return false; }

            FaOperation operation;
            ushort sequence;
            if (!FaExchangeCodec.TryReadOperation(body, out operation, out sequence)) { return false; }

            return operation == expectedOperation && sequence == expectedSequence;
        }

        /// <summary>
        /// Writes a tag byte followed by a big-endian 16-bit value.
        /// </summary>
        private static int WriteTagged(byte[] destination, int at, byte tag, ushort value)
        {
            destination[at] = tag;
            destination[at + 1] = (byte)(value >> 8);
            destination[at + 2] = (byte)value;
            return at + 3;
        }

        /// <summary>
        /// Writes a big-endian 16-bit value.
        /// </summary>
        private static void WriteUInt16(byte[] destination, int at, ushort value)
        {
            destination[at] = (byte)(value >> 8);
            destination[at + 1] = (byte)value;
        }
    }
}
