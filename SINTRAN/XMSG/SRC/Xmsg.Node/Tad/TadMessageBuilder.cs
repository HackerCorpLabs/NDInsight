using System;
using System.Collections.Generic;
using System.Text;

namespace NDInsight.Sintran.Xmsg.Node.Tad
{
    /// <summary>
    /// A typed, fluent builder for a TAD message chain: one method per message type, each encoding
    /// its fields correctly and keeping the chain word-aligned. Produces the payload that follows the
    /// XMSG sub-header. Replaces hand-built <c>byte[]</c> / hex / string-concatenated chains.
    /// </summary>
    /// <remarks>
    /// <para><b>Word alignment (spec TAD-Message-Formats.md section 1)</b></para>
    /// Each TAD message MUST start on an even (word) offset because the ND reader (<c>GETMES</c>) is
    /// word-oriented. This builder inserts a <c>0x00</c> pad automatically before a message when the
    /// chain length is odd. Omitting the pad is what made odd-length terminal output (for example the
    /// menu and the Echo reply) hang: the trailing <see cref="Rfi"/> landed on an odd byte, the host
    /// mis-read it, the input credit was lost, and the session stalled to its idle timeout.
    /// <para><b>Field encoding</b></para>
    /// Multi-byte fields are big-endian (for example the 16-bit <see cref="Sycn"/> state). Terminal
    /// text is 7-bit ASCII (host output direction). Every message is <c>opcode(1) count(1) data</c>.
    /// </remarks>
    public sealed class TadMessageBuilder
    {
        private readonly List<byte> _buffer;

        /// <summary>
        /// Initialises an empty builder.
        /// </summary>
        public TadMessageBuilder()
        {
            _buffer = new List<byte>(64);
        }

        /// <summary>
        /// Appends a BDAT terminal-data message carrying raw bytes.
        /// </summary>
        /// <param name="data">
        /// The terminal data bytes (at most 255).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Bdat(ReadOnlySpan<byte> data)
        {
            return Append(TadOp.Bdat, data);
        }

        /// <summary>
        /// Appends a BDAT terminal-data message carrying 7-bit ASCII text (host output direction).
        /// </summary>
        /// <param name="text">
        /// The terminal text to send (at most 255 bytes of ASCII).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder BdatText(string text)
        {
            byte[] ascii = Encoding.ASCII.GetBytes(text ?? string.Empty);
            return Append(TadOp.Bdat, ascii);
        }

        /// <summary>
        /// Appends an RFI (ready-for-input) flow-control credit — the message that lets the peer send
        /// the next input line. Every host frame that expects input MUST end with one.
        /// </summary>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Rfi()
        {
            return Append(TadOp.Rfi, ReadOnlySpan<byte>.Empty);
        }

        /// <summary>
        /// Appends a SYCN session-sync message carrying a 16-bit login state (big-endian).
        /// </summary>
        /// <param name="state">
        /// The login state to assert.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Sycn(SycnState state)
        {
            Span<byte> data = stackalloc byte[2];
            data[0] = (byte)((ushort)state >> 8);
            data[1] = (byte)state;
            return Append(TadOp.Sycn, data);
        }

        /// <summary>
        /// Appends a CESC (command-escape / echo-control state) message carrying a 1-byte state.
        /// </summary>
        /// <param name="state">
        /// The escape-control state (<c>0x00</c> auth-prompt, <c>0x01</c> auth-complete).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Cesc(byte state)
        {
            Span<byte> data = stackalloc byte[1];
            data[0] = state;
            return Append(TadOp.Cesc, data);
        }

        /// <summary>
        /// Appends an ECKM (echo-strategy) message carrying a 1-byte strategy code.
        /// </summary>
        /// <param name="strategy">
        /// The echo strategy (for example <c>0x01</c> echo on, <c>0xFF</c> echo off for a password).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Eckm(byte strategy)
        {
            Span<byte> data = stackalloc byte[1];
            data[0] = strategy;
            return Append(TadOp.Eckm, data);
        }

        /// <summary>
        /// Appends a BMMX (break-strategy / max-break) message: a 1-byte strategy and a 16-bit
        /// max-break level (big-endian).
        /// </summary>
        /// <param name="strategy">
        /// The break strategy code.
        /// </param>
        /// <param name="maxBreak">
        /// The maximum break level.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Bmmx(byte strategy, ushort maxBreak)
        {
            Span<byte> data = stackalloc byte[3];
            data[0] = strategy;
            data[1] = (byte)(maxBreak >> 8);
            data[2] = (byte)maxBreak;
            return Append(TadOp.Bmmx, data);
        }

        /// <summary>
        /// Appends a DUMM (dummy / filler) message.
        /// </summary>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Dumm()
        {
            return Append(TadOp.Dumm, ReadOnlySpan<byte>.Empty);
        }

        /// <summary>
        /// Appends a RESE (reset-request) message.
        /// </summary>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Rese()
        {
            return Append(TadOp.Rese, ReadOnlySpan<byte>.Empty);
        }

        /// <summary>
        /// Appends a RECO (reset-confirm) message.
        /// </summary>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Reco()
        {
            return Append(TadOp.Reco, ReadOnlySpan<byte>.Empty);
        }

        /// <summary>
        /// Appends a DCON (disconnect indication) message.
        /// </summary>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Dcon()
        {
            return Append(TadOp.Dcon, ReadOnlySpan<byte>.Empty);
        }

        /// <summary>
        /// Appends a message with an arbitrary opcode and data (escape hatch for opcodes without a
        /// dedicated typed method yet).
        /// </summary>
        /// <param name="opcode">
        /// The TAD opcode byte.
        /// </param>
        /// <param name="data">
        /// The message data (at most 255 bytes).
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        public TadMessageBuilder Raw(byte opcode, ReadOnlySpan<byte> data)
        {
            return Append(opcode, data);
        }

        /// <summary>
        /// Serialises the chain to its on-wire byte form (opcode, count, data per message, with the
        /// word-alignment pads inserted).
        /// </summary>
        /// <returns>
        /// The TAD payload bytes.
        /// </returns>
        public byte[] Build()
        {
            return _buffer.ToArray();
        }

        /// <summary>
        /// Appends one message, inserting a word-alignment pad first when the chain length is odd.
        /// </summary>
        /// <param name="opcode">
        /// The TAD opcode byte.
        /// </param>
        /// <param name="data">
        /// The message data.
        /// </param>
        /// <returns>
        /// This builder, for chaining.
        /// </returns>
        /// <exception cref="ArgumentOutOfRangeException">
        /// Thrown when <paramref name="data"/> is longer than 255 bytes (the count is a single byte).
        /// </exception>
        private TadMessageBuilder Append(byte opcode, ReadOnlySpan<byte> data)
        {
            if (data.Length > 255)
            {
                throw new ArgumentOutOfRangeException(
                    nameof(data), "A single TAD message carries at most 255 data bytes (1-byte count).");
            }

            // Word alignment (spec section 1): a message must start on an even offset. Pad when odd.
            if ((_buffer.Count & 1) == 1)
            {
                _buffer.Add(0x00);
            }

            _buffer.Add(opcode);
            _buffer.Add((byte)data.Length);
            for (int i = 0; i < data.Length; i++)
            {
                _buffer.Add(data[i]);
            }

            return this;
        }
    }
}
