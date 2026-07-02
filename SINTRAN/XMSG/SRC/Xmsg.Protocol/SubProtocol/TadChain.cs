using System;
using System.Collections.Generic;

namespace NDInsight.Sintran.Xmsg.SubProtocol
{
    /// <summary>
    /// A decoded chain of TAD (protocol <c>0xDD</c>) messages parsed from the payload
    /// that follows a SINTRAN header.
    /// </summary>
    /// <remarks>
    /// <para><b>Chain format</b></para>
    /// The payload is a sequence of <c>opcode(1) count(1) data(count)</c> messages
    /// (hdlc_tcp.lua <c>dissect_tad</c>):
    ///  - a <c>0x00</c> byte between messages is an alignment pad and is skipped.
    ///  - a trailing fragment too short to form a complete message is kept in
    ///    <see cref="Remainder"/> so no byte is lost from the decoded view.
    /// This is a read-only analytical view; byte-identical re-serialisation is guaranteed
    /// by the owning frame's retained raw bytes, not by this type.
    /// </remarks>
    public sealed class TadChain
    {
        private readonly List<TadMessage> _messages;

        /// <summary>
        /// Initialises an empty TAD chain.
        /// </summary>
        public TadChain()
        {
            _messages = new List<TadMessage>();
            Remainder = Array.Empty<byte>();
        }

        /// <summary>
        /// Gets the decoded messages in chain order.
        /// </summary>
        public IReadOnlyList<TadMessage> Messages
        {
            get { return _messages; }
        }

        /// <summary>
        /// Gets any trailing bytes that could not be decoded as a complete message.
        /// </summary>
        public byte[] Remainder { get; private set; }

        /// <summary>
        /// Decodes a TAD message chain from a payload span.
        /// </summary>
        /// <param name="payload">
        /// The bytes following the SINTRAN header for a TAD frame.
        /// </param>
        /// <returns>
        /// The decoded <see cref="TadChain"/>.
        /// </returns>
        public static TadChain Parse(ReadOnlySpan<byte> payload)
        {
            TadChain chain = new TadChain();

            int pos = 0;
            int len = payload.Length;
            while (pos < len)
            {
                // A lone 0x00 is a TAD alignment pad byte, not a message.
                if (payload[pos] == 0x00)
                {
                    pos++;
                    continue;
                }

                // A complete message needs at least opcode + count.
                if (pos + 2 > len)
                {
                    break;
                }

                byte opcode = payload[pos];
                byte count = payload[pos + 1];
                int dataStart = pos + 2;

                int available = count;
                if (dataStart + available > len)
                {
                    available = len - dataStart;
                }

                byte[] data = new byte[available];
                for (int i = 0; i < available; i++)
                {
                    data[i] = payload[dataStart + i];
                }

                chain._messages.Add(new TadMessage(opcode, count, data));

                // Advance by the declared length; an over-long count ends the walk.
                pos = dataStart + count;
            }

            // Keep any undecoded tail (an odd leftover byte) verbatim in the view.
            if (pos < len)
            {
                int tailLength = len - pos;
                byte[] tail = new byte[tailLength];
                for (int i = 0; i < tailLength; i++)
                {
                    tail[i] = payload[pos + i];
                }

                chain.Remainder = tail;
            }

            return chain;
        }
    }
}
