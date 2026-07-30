using System;

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// Drives the server side of a <c>*FA-SERVER</c> conversation: it answers each request with a
    /// reply carrying the echoed operation and sequence, and builds the short acknowledgement,
    /// connection confirmation and close messages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The server's envelope differs from the client's in two ways, both taken from the captures
    /// rather than assumed symmetric:
    /// </para>
    ///  - It answers with its OWN conversation number - <c>0x0002</c> in every captured operation -
    ///    not the client's. The client's number appears only in the connection confirmation and the
    ///    close.
    ///  - Its session token is <c>0x9081</c> on <b>every</b> exchange including the first. The client
    ///    is different: it sends <c>0x0001</c> on the first exchange and <c>0xD761</c> thereafter.
    ///    Assuming the two sides behaved alike here would produce a wrong first reply.
    /// </remarks>
    public sealed class FaServerConversation
    {
        /// <summary>
        /// Short-acknowledgement constant sent by the responder.
        /// </summary>
        /// <remarks>
        /// The asker sends <c>0x8485</c> in the same position. Meaning UNKNOWN for both; they are
        /// reproduced because they are what the capture contains.
        /// </remarks>
        public const ushort ResponderShortAckConstant = 0x922A;

        /// <summary>
        /// Short-acknowledgement constant sent by the asker.
        /// </summary>
        public const ushort AskerShortAckConstant = 0x8485;

        /// <summary>
        /// Message type of the server's connection confirmation.
        /// </summary>
        public const ushort MessageTypeConnectionConfirm = 0x07D2;

        /// <summary>
        /// Message type of the close message, sent by both sides.
        /// </summary>
        public const ushort MessageTypeClose = 0x07C0;

        /// <summary>
        /// The conversation number the client is using.
        /// </summary>
        private readonly ushort _clientConversation;

        /// <summary>
        /// How many replies have been built so far.
        /// </summary>
        private int _repliesBuilt;

        /// <summary>
        /// Starts answering a conversation.
        /// </summary>
        /// <param name="clientConversation">
        /// The conversation number the client chose. It is echoed in the connection confirmation and
        /// the close, but NOT in ordinary replies.
        /// </param>
        public FaServerConversation(ushort clientConversation)
        {
            _clientConversation = clientConversation;
        }

        /// <summary>
        /// Gets how many replies have been built.
        /// </summary>
        public int RepliesBuilt
        {
            get { return _repliesBuilt; }
        }

        /// <summary>
        /// Builds the reply to a request, echoing its operation and sequence.
        /// </summary>
        /// <param name="operation">
        /// The operation code from the request.
        /// </param>
        /// <param name="sequence">
        /// The exchange sequence from the request.
        /// </param>
        /// <param name="qformFields">
        /// The QFORM fields following the operation and sequence pair, including the closing
        /// end-of-list selector.
        /// </param>
        /// <returns>
        /// The complete reply body.
        /// </returns>
        public byte[] BuildReply(FaOperation operation, ushort sequence, ReadOnlySpan<byte> qformFields)
        {
            int length = FaExchangeCodec.QformOffset + 3 + 3 + qformFields.Length;
            byte[] body = new byte[length];

            WriteUInt16(body, FaExchangeCodec.MessageTypeOffset, FaExchangeCodec.MessageTypeRequest);
            WriteUInt16(body, FaExchangeCodec.ConversationOffset, FaExchangeCodec.ResponderConversation);

            body[FaExchangeCodec.SessionHeaderOffset] = (byte)(0x80 + _repliesBuilt);
            body[FaExchangeCodec.SessionHeaderOffset + 1] = 0x00;

            // Always the responder token - unlike the client, there is no first-exchange special case.
            WriteUInt16(body, FaExchangeCodec.SessionHeaderOffset + 2, FaExchangeCodec.SessionTokenResponder);

            int at = FaExchangeCodec.QformOffset;
            at = WriteTagged(body, at, 0x92, (ushort)operation);
            at = WriteTagged(body, at, 0x92, sequence);

            qformFields.CopyTo(new Span<byte>(body, at, qformFields.Length));

            _repliesBuilt++;
            return body;
        }

        /// <summary>
        /// Builds the eight-byte short acknowledgement.
        /// </summary>
        /// <param name="counter">
        /// The exchange counter, which the capture carries in the HIGH byte of the word: the first
        /// acknowledgement is <c>0x0100</c>, the second <c>0x0200</c>, and so on.
        /// </param>
        /// <param name="fromResponder">
        /// <see langword="true"/> for the server's form, <see langword="false"/> for the client's.
        /// The two differ only in the trailing constant.
        /// </param>
        /// <returns>
        /// The eight-byte body.
        /// </returns>
        public byte[] BuildShortAck(byte counter, bool fromResponder)
        {
            byte[] body = new byte[8];
            WriteUInt16(body, 0, FaExchangeCodec.MessageTypeShortAck);
            WriteUInt16(
                body,
                2,
                fromResponder ? FaExchangeCodec.ResponderConversation : _clientConversation);
            body[4] = counter;
            body[5] = 0x00;
            WriteUInt16(body, 6, fromResponder ? ResponderShortAckConstant : AskerShortAckConstant);
            return body;
        }

        /// <summary>
        /// Builds the server's connection confirmation.
        /// </summary>
        /// <param name="systemNumber">
        /// The answering system's number - <c>100</c> in the captures, appearing as <c>0x6400</c>.
        /// </param>
        /// <returns>
        /// The eight-byte body.
        /// </returns>
        /// <remarks>
        /// This is the one server message that carries the CLIENT's conversation number, which is
        /// how the client recognises the confirmation as belonging to its own request.
        /// </remarks>
        public byte[] BuildConnectionConfirm(byte systemNumber)
        {
            byte[] body = new byte[8];
            WriteUInt16(body, 0, MessageTypeConnectionConfirm);
            WriteUInt16(body, 2, FaExchangeCodec.ResponderConversation);
            WriteUInt16(body, 4, _clientConversation);
            body[6] = systemNumber;
            body[7] = 0x00;
            return body;
        }

        /// <summary>
        /// Builds the close message.
        /// </summary>
        /// <returns>
        /// The eight-byte body.
        /// </returns>
        /// <remarks>
        /// Both sides send this, byte for byte identical - the capture shows node 100 and node 102
        /// each emitting <c>07C0 0002 &lt;client conversation&gt; 0000</c>.
        /// </remarks>
        public byte[] BuildClose()
        {
            byte[] body = new byte[8];
            WriteUInt16(body, 0, MessageTypeClose);
            WriteUInt16(body, 2, FaExchangeCodec.ResponderConversation);
            WriteUInt16(body, 4, _clientConversation);
            WriteUInt16(body, 6, 0x0000);
            return body;
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
