using System;
using System.Text;

namespace NDInsight.Sintran.Xmsg.Chat
{
    /// <summary>
    /// One chat message: a kind, a nickname and a line of text.
    /// </summary>
    /// <remarks>
    /// <para><b>Wire layout</b></para>
    /// <code>
    /// byte  0      kind
    /// byte  1      nickname length, 0..255
    /// bytes 2..    nickname, ASCII
    /// then  2      text length, big-endian
    /// then  n      text, ASCII
    /// </code>
    /// Big-endian because everything else on this machine is: the ND-100 is a big-endian,
    /// word-addressed machine, and a chat program written in PLANC on the other side would read
    /// these as words. Lengths are explicit rather than terminator-scanned so a text containing any
    /// byte value survives the trip.
    /// <para><b>Why ASCII</b></para>
    /// A SINTRAN terminal is a 7-bit device and the TAD layer strips bit 7 from keystrokes, so a
    /// nickname or line that is not ASCII could never be typed at the other end anyway.
    /// </remarks>
    public readonly struct ChatMessage
    {
        /// <summary>
        /// The largest nickname the length byte can describe.
        /// </summary>
        public const int MaximumNicknameLength = 255;

        /// <summary>
        /// The largest text the two length bytes can describe.
        /// </summary>
        /// <remarks>
        /// Well beyond anything a terminal user types; the real limit in practice is the message
        /// buffer the sender reserved.
        /// </remarks>
        public const int MaximumTextLength = 65535;

        /// <summary>
        /// Initialises a message.
        /// </summary>
        /// <param name="kind">
        /// What the message is.
        /// </param>
        /// <param name="nickname">
        /// Who it concerns - the speaker, joiner or leaver. May be empty.
        /// </param>
        /// <param name="text">
        /// The line of text. May be empty.
        /// </param>
        /// <exception cref="ArgumentException">
        /// Thrown when either string is longer than its length field can describe.
        /// </exception>
        public ChatMessage(ChatMessageKind kind, string nickname, string text)
        {
            if (nickname == null)
            {
                nickname = string.Empty;
            }

            if (text == null)
            {
                text = string.Empty;
            }

            if (nickname.Length > MaximumNicknameLength)
            {
                throw new ArgumentException(
                    "A nickname may be at most " + MaximumNicknameLength + " characters.",
                    nameof(nickname));
            }

            if (text.Length > MaximumTextLength)
            {
                throw new ArgumentException(
                    "Text may be at most " + MaximumTextLength + " characters.",
                    nameof(text));
            }

            Kind = kind;
            Nickname = nickname;
            Text = text;
        }

        /// <summary>
        /// Gets what this message is.
        /// </summary>
        public ChatMessageKind Kind { get; }

        /// <summary>
        /// Gets the nickname this message concerns.
        /// </summary>
        public string Nickname { get; }

        /// <summary>
        /// Gets the line of text.
        /// </summary>
        public string Text { get; }

        /// <summary>
        /// Gets the number of bytes <see cref="Encode"/> will write.
        /// </summary>
        public int ByteCount
        {
            get { return 1 + 1 + Nickname.Length + 2 + Text.Length; }
        }

        /// <summary>
        /// Writes this message into a buffer.
        /// </summary>
        /// <param name="destination">
        /// The buffer to write into. Must be at least <see cref="ByteCount"/> bytes.
        /// </param>
        /// <returns>
        /// The number of bytes written.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// Thrown when <paramref name="destination"/> is too small.
        /// </exception>
        public int Encode(Span<byte> destination)
        {
            int needed = ByteCount;
            if (destination.Length < needed)
            {
                throw new ArgumentException(
                    "The buffer needs " + needed + " bytes.", nameof(destination));
            }

            destination[0] = (byte)Kind;
            destination[1] = (byte)Nickname.Length;

            int at = 2;
            at += Encoding.ASCII.GetBytes(Nickname, destination.Slice(at));

            destination[at] = (byte)((Text.Length >> 8) & 0xFF);
            destination[at + 1] = (byte)(Text.Length & 0xFF);
            at += 2;

            at += Encoding.ASCII.GetBytes(Text, destination.Slice(at));
            return at;
        }

        /// <summary>
        /// Reads a message from a buffer.
        /// </summary>
        /// <param name="source">
        /// The bytes to read.
        /// </param>
        /// <param name="message">
        /// The message read, or the default when the bytes are not a well-formed message.
        /// </param>
        /// <returns>
        /// True when a message was read.
        /// </returns>
        /// <remarks>
        /// Rejects rather than throws. A message arriving on a chat port came off a wire or out of
        /// another program, so malformed input is an expected condition, not a bug: a truncated or
        /// over-long buffer is dropped and the caller keeps serving everybody else.
        /// </remarks>
        public static bool TryDecode(ReadOnlySpan<byte> source, out ChatMessage message)
        {
            message = default;

            if (source.Length < 4)
            {
                // Kind, nickname length and the two text-length bytes are the irreducible minimum.
                return false;
            }

            byte kind = source[0];
            if (kind == (byte)ChatMessageKind.None || kind > (byte)ChatMessageKind.Left)
            {
                return false;
            }

            int nicknameLength = source[1];
            int at = 2;
            if (source.Length < at + nicknameLength + 2)
            {
                return false;
            }

            string nickname = nicknameLength == 0
                ? string.Empty
                : Encoding.ASCII.GetString(source.Slice(at, nicknameLength));
            at += nicknameLength;

            int textLength = (source[at] << 8) | source[at + 1];
            at += 2;
            if (source.Length < at + textLength)
            {
                return false;
            }

            string text = textLength == 0
                ? string.Empty
                : Encoding.ASCII.GetString(source.Slice(at, textLength));

            message = new ChatMessage((ChatMessageKind)kind, nickname, text);
            return true;
        }

        /// <summary>
        /// Returns a readable form, for logs and test failures.
        /// </summary>
        /// <returns>
        /// A short description of the message.
        /// </returns>
        public override string ToString()
        {
            if (Nickname.Length == 0)
            {
                return Kind + " \"" + Text + "\"";
            }

            return Kind + " " + Nickname + " \"" + Text + "\"";
        }
    }
}
