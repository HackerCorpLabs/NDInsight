//
// SPDX-License-Identifier: MIT
// Copyright (c) 1985-2026 Ronny Hansen
// HackerCorp Labs — https://github.com/HackerCorpLabs
//

namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// The first word of an FA message body, identifying what kind of message it is.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every FA message body opens with this word at offset 0, before anything else. It is what
    /// tells a reader whether the rest of the body is a QFORM structure, a bare acknowledgement,
    /// or a control message with no payload at all.
    /// </para>
    /// <para>
    /// The values were read off the wire between two live ND-100s; they are not documented in any
    /// manual we hold. Both directions use the same set - the message type says what the message
    /// is, not who sent it, so a Close carries <c>0x07C0</c> whichever side is closing.
    /// </para>
    /// <para>
    /// These were four separate <c>const ushort</c> fields split across
    /// <see cref="FaExchangeCodec"/> and <see cref="FaServerConversation"/>, which made it
    /// impossible to see the set as a set or to spot a value that belonged to neither.
    /// </para>
    /// </remarks>
    public enum FaMessageType : ushort
    {
        /// <summary>
        /// A request carrying a QFORM body.
        /// </summary>
        /// <remarks>
        /// The operation itself is a separate field further into the body - see
        /// <see cref="FaOperation"/>. This word only says "a QFORM structure follows".
        /// </remarks>
        Request = 0x07F0,

        /// <summary>
        /// The short eight-byte acknowledgement.
        /// </summary>
        /// <remarks>
        /// Carries no QFORM body at all: the eight bytes are the whole message.
        /// </remarks>
        ShortAck = 0x07A2,

        /// <summary>
        /// The server's confirmation that the conversation is open.
        /// </summary>
        /// <remarks>
        /// Sent once, in reply to the opening letter, before any operation is accepted.
        /// </remarks>
        ConnectionConfirm = 0x07D2,

        /// <summary>
        /// The conversation is being closed.
        /// </summary>
        /// <remarks>
        /// Sent by whichever side closes first; the other side answers with the same type.
        /// </remarks>
        Close = 0x07C0,
    }
}
