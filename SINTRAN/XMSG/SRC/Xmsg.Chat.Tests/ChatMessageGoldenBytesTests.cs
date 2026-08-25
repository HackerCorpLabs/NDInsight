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
        /// The all-rooms answer: no name, and the whole picture in the text.
        /// </summary>
        /// <remarks>
        /// <para><b>Pinned against the PLANC that emits it</b></para>
        /// <c>buildAllWho</c> in <c>CHATSV.PLNC</c> writes the kind, then a ZERO name length -
        /// the SERVER is answering, not a member - then the two-byte text length and the text.
        /// The empty name field is load-bearing: a client that printed the name would print
        /// nothing, and one that expected a name there would read the length bytes as characters.
        /// <para><b>The text is ready to read and nothing parses it</b></para>
        /// Rooms two spaces apart, people one apart. A format nobody parses cannot be parsed
        /// wrongly, which is the whole reason it is laid out for a person rather than a decoder.
        /// </remarks>
        [Fact]
        public void AnAllWhoMessageHasExactlyTheseBytes()
        {
            ChatMessage all = new ChatMessage(ChatMessageKind.AllWho, string.Empty, "LOBBY: ANNA");

            byte[] buffer = new byte[64];
            int written = all.Encode(buffer);

            //  0F        kind = AllWho (15)
            //  00        name length - EMPTY, the server is answering
            //  00 0B     text length, HIGH byte first
            //  4C 4F 42 42 59 3A 20 41 4E 4E 41   "LOBBY: ANNA"
            Assert.Equal("0F00000B4C4F424259 3A20414E4E41".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// A replayed line: the same shape as a spoken one, under a different kind.
        /// </summary>
        /// <remarks>
        /// <para><b>Same bytes as Said except the first</b></para>
        /// That is deliberate and is the point of the kind being separate. The layout stays
        /// familiar so a client renders it with the same code, while the FIRST BYTE still says
        /// this is history - so a client that timestamps, beeps or counts unread can tell.
        /// <para><b>Pinned against histReplay in CHATSV.PLNC</b></para>
        /// Kind, name length, name, two-byte big-endian text length, text - the same universal
        /// prefix every kind uses.
        /// </remarks>
        [Fact]
        public void AHistoryMessageHasExactlyTheseBytes()
        {
            ChatMessage past = new ChatMessage(ChatMessageKind.History, "ANNA", "hei");

            byte[] buffer = new byte[64];
            int written = past.Encode(buffer);

            //  10        kind = History (16)
            //  04        nickname length
            //  41 4E 4E 41   "ANNA"
            //  00 03     text length, HIGH byte first
            //  68 65 69  "hei"
            Assert.Equal("1004414E4E41000368 6569".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));

            // The ONLY difference from Said is the kind byte. If that ever stops being true,
            // one of the two layouts has drifted.
            ChatMessage said = new ChatMessage(ChatMessageKind.Said, "ANNA", "hei");
            byte[] saidBuffer = new byte[64];
            int saidWritten = said.Encode(saidBuffer);

            Assert.Equal(saidWritten, written);
            for (int i = 1; i < written; i++)
            {
                Assert.Equal(saidBuffer[i], buffer[i]);
            }
        }

        /// <summary>
        /// A forwarded line between servers: the speaker unqualified, the room in front of the text.
        /// </summary>
        /// <remarks>
        /// <para><b>The room travels in the text, split at the FIRST slash</b></para>
        /// A message may contain slashes and a room name may not, so the first one is the
        /// separator. Splitting at the last would put half a sentence in the room name.
        /// <para><b>The speaker is NOT qualified here</b></para>
        /// The name is who their own machine knows them as. The receiver adds the machine, taken
        /// from the magic the letter arrived with - so a speaker cannot forge the machine they
        /// are on by putting it in the name.
        /// </remarks>
        [Fact]
        public void ATrunkSaidMessageHasExactlyTheseBytes()
        {
            ChatMessage forwarded = new ChatMessage(ChatMessageKind.TrunkSaid, "ANNA", "LOBBY/hei");

            byte[] buffer = new byte[64];
            int written = forwarded.Encode(buffer);

            //  33        kind = TrunkSaid (51 decimal)
            //  04        speaker length, UNQUALIFIED
            //  41 4E 4E 41   "ANNA"
            //  00 09     text length, HIGH byte first
            //  4C 4F 42 42 59 2F 68 65 69   "LOBBY/hei"
            Assert.Equal("3304414E4E410009 4C4F4242592F686569".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// A RELAYED line: the same shape as TrunkSaid with an origin and a hop count in front.
        /// </summary>
        /// <remarks>
        /// <para><b>Why the origin travels and cannot be inferred</b></para>
        /// The receiver's usual trick is to qualify the speaker with the system the letter arrived
        /// from. On a relayed message that names the RELAY, not the speaker's own machine, so the
        /// origin has to be carried explicitly.
        /// <para><b>Why TrunkSaid was not simply extended</b></para>
        /// Its bytes are pinned by the test above and the PLANC server reads the same layout.
        /// Adding fields there would mean every machine had to be upgraded before any trunk
        /// worked, and these machines are upgraded one at a time.
        /// </remarks>
        [Fact]
        public void ATrunkRelayMessageHasExactlyTheseBytes()
        {
            ChatMessage relayed = new ChatMessage("ANNA", "LOBBY/hei", 103, 3);

            byte[] buffer = new byte[64];
            int written = relayed.Encode(buffer);

            //  34        kind = TrunkRelay (52 decimal)
            //  00 67     origin system = 103, HIGH byte first
            //  03        hops remaining
            //  04        speaker length, UNQUALIFIED
            //  41 4E 4E 41   "ANNA"
            //  00 09     text length, HIGH byte first
            //  4C 4F 42 42 59 2F 68 65 69   "LOBBY/hei"
            Assert.Equal("34006703 04414E4E41 0009 4C4F4242592F686569".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// The other direction: those exact bytes decode back to the same message.
        /// </summary>
        /// <remarks>
        /// A golden for EACH direction is the rule for a new kind - CHAT-FEDERATION-DESIGN.md
        /// constraint 5. The /nick defect was precisely an unpinned direction: the client wrote
        /// one field and the server read another, and both ends silently did nothing.
        /// </remarks>
        [Fact]
        public void ThoseExactBytesDecodeBackToARelayedMessage()
        {
            byte[] wire = Convert.FromHexString(
                "34006703" + "04414E4E41" + "0009" + "4C4F4242592F686569");

            ChatMessage decoded;
            bool ok = ChatMessage.TryDecode(wire, out decoded);

            Assert.True(ok);
            Assert.Equal(ChatMessageKind.TrunkRelay, decoded.Kind);
            Assert.Equal((ushort)103, decoded.OriginSystem);
            Assert.Equal((byte)3, decoded.HopsRemaining);
            Assert.Equal("ANNA", decoded.Nickname);
            Assert.Equal("LOBBY/hei", decoded.Text);
        }

        /// <summary>
        /// A relay carrying the origin's line number has exactly these bytes.
        /// </summary>
        /// <remarks>
        /// <para><b>Both directions are pinned, and that is the rule for a new kind</b></para>
        /// CHAT-FEDERATION-DESIGN.md constraint 5. The /nick defect was an unpinned direction: the
        /// client wrote one field, the server read another, and both ends silently did nothing.
        /// <para><b>Why the id sits after the hop count</b></para>
        /// It keeps the first three bytes identical in layout to kind 52, which makes the two
        /// readable side by side on a trace - which is where these get diagnosed.
        /// </remarks>
        [Fact]
        public void ATrunkRelayIdMessageHasExactlyTheseBytes()
        {
            ChatMessage relayed = new ChatMessage("ANNA", "LOBBY/hei", 103, 3, 1234);

            byte[] buffer = new byte[64];
            int written = relayed.Encode(buffer);

            //  35        kind = TrunkRelayId (53 decimal)
            //  00 67     origin system = 103, HIGH byte first
            //  03        hops remaining
            //  04 D2     line id = 1234, HIGH byte first
            //  04        speaker length, UNQUALIFIED
            //  41 4E 4E 41   "ANNA"
            //  00 09     text length, HIGH byte first
            //  4C 4F 42 42 59 2F 68 65 69   "LOBBY/hei"
            Assert.Equal(
                "3500670304D2 04414E4E41 0009 4C4F4242592F686569".Replace(" ", string.Empty),
                Convert.ToHexString(buffer, 0, written));
        }

        /// <summary>
        /// Those exact bytes decode back, id and all.
        /// </summary>
        [Fact]
        public void ThoseExactBytesDecodeBackToARelayedMessageWithAnId()
        {
            byte[] wire = Convert.FromHexString(
                "3500670304D2" + "04414E4E41" + "0009" + "4C4F4242592F686569");

            ChatMessage decoded;
            bool ok = ChatMessage.TryDecode(wire, out decoded);

            Assert.True(ok);
            Assert.Equal(ChatMessageKind.TrunkRelayId, decoded.Kind);
            Assert.Equal((ushort)103, decoded.OriginSystem);
            Assert.Equal((byte)3, decoded.HopsRemaining);
            Assert.Equal((ushort)1234, decoded.LineId);
            Assert.Equal("ANNA", decoded.Nickname);
            Assert.Equal("LOBBY/hei", decoded.Text);
        }

        /// <summary>
        /// Kind 52 is untouched by the arrival of kind 53.
        /// </summary>
        /// <remarks>
        /// The same guarantee TrunkSaid was given when TrunkRelay arrived, for the same reason: a
        /// server that has not been upgraded still speaks 52, and its bytes must not have moved
        /// under it.
        /// </remarks>
        [Fact]
        public void TrunkRelayIsUnchangedByTheArrivalOfTrunkRelayId()
        {
            ChatMessage relayed = new ChatMessage("ANNA", "LOBBY/hei", 103, 3);

            byte[] buffer = new byte[64];
            int written = relayed.Encode(buffer);

            Assert.Equal("34006703" + "04414E4E41" + "0009" + "4C4F4242592F686569",
                Convert.ToHexString(buffer, 0, written));
            Assert.Equal((ushort)0, relayed.LineId);
        }

        /// <summary>
        /// A kind-53 header cut short is refused, not read as a nickname length.
        /// </summary>
        /// <remarks>
        /// Its header is two bytes longer than kind 52's, so a message long enough to satisfy the
        /// OLD check can still be too short for this one. That is exactly the case that would read
        /// the id's high byte as a name length.
        /// </remarks>
        [Fact]
        public void ATruncatedRelayIdHeaderIsRefused()
        {
            // Enough bytes for a kind-52 header, one short of a kind-53 one.
            byte[] wire = Convert.FromHexString("35006703");

            ChatMessage decoded;
            bool ok = ChatMessage.TryDecode(wire, out decoded);

            Assert.False(ok);
        }

        /// <summary>
        /// A relay header cut short is refused, not read as a nickname length.
        /// </summary>
        /// <remarks>
        /// Without the length check this decodes: 0x00 becomes a zero-length nickname, 0x67 and
        /// the next byte become a text length, and a plausible-looking message comes out of
        /// rubbish. Dropping it is the whole point of TryDecode rejecting rather than throwing.
        /// </remarks>
        [Fact]
        public void ATruncatedRelayHeaderIsRefused()
        {
            byte[] wire = Convert.FromHexString("340067");

            ChatMessage decoded;

            Assert.False(ChatMessage.TryDecode(wire, out decoded));
        }

        /// <summary>
        /// TrunkSaid still encodes exactly as it did, with no relay header.
        /// </summary>
        /// <remarks>
        /// This is the promise that made a separate kind worth having: an un-upgraded server must
        /// keep seeing the bytes it already understands while the rollout is half done.
        /// </remarks>
        [Fact]
        public void TrunkSaidIsUnchangedByTheArrivalOfTrunkRelay()
        {
            ChatMessage forwarded = new ChatMessage(ChatMessageKind.TrunkSaid, "ANNA", "LOBBY/hei");

            byte[] buffer = new byte[64];
            int written = forwarded.Encode(buffer);

            Assert.Equal("3304414E4E410009" + "4C4F4242592F686569",
                Convert.ToHexString(buffer, 0, written));
        }

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
