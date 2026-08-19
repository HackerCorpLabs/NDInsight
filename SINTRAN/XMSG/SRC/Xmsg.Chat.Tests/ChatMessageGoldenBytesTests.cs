using System;

using NDInsight.Sintran.Xmsg.Chat;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Chat.Tests
{
    /// <summary>
    /// The exact bytes of a chat message, pinned.
    /// </summary>
    /// <remarks>
    /// <para><b>This file IS the specification, and that is not a figure of speech</b></para>
    /// <para>
    /// Every other wire format in this repository is pinned against a capture of a real ND. This
    /// one cannot be: we invented it, and no Norsk Data machine has ever sent a chat message. So
    /// there is nothing to check against except a decision written down - and this is where it is
    /// written down.
    /// </para>
    /// <para><b>Why a round trip is not enough</b></para>
    /// <para>
    /// <c>EveryKindSurvivesARoundTrip</c> encodes and decodes with the same code, so it passes
    /// even if the layout changes - a one-byte text length, or a little-endian one, would round
    /// trip perfectly. What it cannot see is the OTHER implementation: <c>SINTRAN-CHAT\CHATSV.PLNC</c>
    /// and <c>CHAT.PLNC</c> lay these bytes out by hand, in PLANC, against no shared code at all.
    /// A change here that still round trips would leave the two silently disagreeing, and the
    /// symptom would be a garbled nickname on somebody's terminal.
    /// </para>
    /// <para><b>If one of these fails</b></para>
    /// <para>
    /// Do not update the expected bytes to make it pass. Either the change was a mistake, or it is
    /// deliberate and the PLANC files have to change in the same commit.
    /// </para>
    /// </remarks>
    public sealed class ChatMessageGoldenBytesTests
    {
        /// <summary>
        /// A spoken line: kind, name length, name, a two-byte big-endian text length, the text.
        /// </summary>
        [Fact]
        public void ASaidMessageHasExactlyTheseBytes()
        {
            ChatMessage said = new ChatMessage(ChatMessageKind.Said, "ANNA", "hei");

            byte[] buffer = new byte[64];
            int written = said.Encode(buffer);

            //  05        kind = Said
            //  04        nickname length
            //  41 4E 4E 41   "ANNA"
            //  00 03     text length, HIGH byte first
            //  68 65 69  "hei"
            Assert.Equal("0504414E4E41000368 6569".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// The text length is two bytes, BIG endian - a length over 255 proves the order.
        /// </summary>
        /// <remarks>
        /// The one field where a mistake is invisible in ordinary use: every short message has a
        /// zero high byte, so a little-endian writer looks perfectly correct until somebody types
        /// more than 255 characters.
        /// </remarks>
        [Fact]
        public void TheTextLengthIsTwoBytesBigEndian()
        {
            string longText = new string('x', 300);
            ChatMessage said = new ChatMessage(ChatMessageKind.Said, "A", longText);

            byte[] buffer = new byte[512];
            said.Encode(buffer);

            // 05 kind, 01 name length, 41 "A", then 300 = 0x012C as high byte then low.
            Assert.Equal(0x05, buffer[0]);
            Assert.Equal(0x01, buffer[1]);
            Assert.Equal(0x41, buffer[2]);
            Assert.Equal(0x01, buffer[3]);   // HIGH byte of 300
            Assert.Equal(0x2C, buffer[4]);   // LOW byte
        }

        /// <summary>
        /// A rename carries the NEW name in the name field and the OLD one as the text.
        /// </summary>
        /// <remarks>
        /// Pinned as bytes because both PLANC programs build this message by hand and the order of
        /// the two names is the thing most easily got backwards. Getting it wrong renames everybody
        /// to their predecessor, which reads as a bizarre chat bug rather than a byte-order one.
        /// </remarks>
        [Fact]
        public void ARenamedMessageCarriesTheNewNameFirst()
        {
            ChatMessage renamed = new ChatMessage(ChatMessageKind.Renamed, "ANNIKA", "ANNA");

            byte[] buffer = new byte[64];
            int written = renamed.Encode(buffer);

            //  0A        kind = Renamed
            //  06        length of the NEW name
            //  414E4E494B41  "ANNIKA"
            //  00 04     length of the OLD name, as the text
            //  414E4E41  "ANNA"
            Assert.Equal("0A06414E4E494B41000441 4E4E41".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// The rename a CLIENT sends carries the new name in the name field and an empty text.
        /// </summary>
        /// <remarks>
        /// The companion to <see cref="ARenamedMessageCarriesTheNewNameFirst"/>, and the direction
        /// that was missing. Only the server-to-client Renamed was pinned, so nothing said where
        /// the new name travels on the way IN - and the PLANC client put it in the TEXT while the
        /// PLANC server read it from the NAME field.
        /// <para><b>What that cost</b></para>
        /// Nothing was printed and nothing happened. Measured on D100 on 2026-08-18: "/nick OLAV"
        /// was accepted, sent, and changed nothing, because the server compared the arriving name
        /// against the seat's current name, found them equal, and took its deliberate
        /// "already your name: not news" path. Two self-consistent sides agreeing to do nothing,
        /// which is why it survived a build that was otherwise working.
        /// <para><b>Why a golden and not a round trip</b></para>
        /// The two ends are a C# program and two PLANC programs on an ND-100. They share no code,
        /// only these bytes, so the bytes are the only place the agreement can be written down.
        /// </remarks>
        [Fact]
        public void ARenameMessageCarriesTheNewNameAndAnEmptyText()
        {
            // Exactly what ChatClient.Rename sends: the NEW nickname, and no text at all.
            ChatMessage rename = new ChatMessage(ChatMessageKind.Rename, "OLAV", string.Empty);

            byte[] buffer = new byte[64];
            int written = rename.Encode(buffer);

            //  09        kind = Rename
            //  04        length of the NEW name
            //  4F4C4156  "OLAV"
            //  00 00     an empty text, both length bytes still written
            Assert.Equal("09044F4C41560000", Convert.ToHexString(buffer, 0, written));
            Assert.Equal(8, written);
        }

        /// <summary>
        /// An empty text still writes both length bytes.
        /// </summary>
        /// <remarks>
        /// A Join carries no text, and it is the first message the PLANC client ever sends. If the
        /// zero length were omitted the server would read the next message's bytes as this one's
        /// text, and the very first exchange would fail in a way that looks like a transport fault.
        /// </remarks>
        [Fact]
        public void AnEmptyTextStillWritesBothLengthBytes()
        {
            ChatMessage join = new ChatMessage(ChatMessageKind.Join, "RONNY", string.Empty);

            byte[] buffer = new byte[64];
            int written = join.Encode(buffer);

            //  01 kind, 05 name length, "RONNY", then 00 00 - and nothing after it.
            Assert.Equal("0105524F4E4E590000", Convert.ToHexString(buffer, 0, written));
            Assert.Equal(9, written);
        }
    }
}
