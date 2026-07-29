using System;
using NDInsight.Sintran.Xmsg.Ndfs;
using NDInsight.Sintran.Xmsg.Protocol.Fa;
using RetroFS.NDFS.Elements;
using Xunit;

namespace NDInsight.Sintran.Xmsg.Ndfs.Tests
{
    /// <summary>
    /// Proves that the 64-byte record the COSMOS file server puts on the wire IS the SINTRAN on-disk
    /// object entry, by decoding a captured listing through RetroFS.NDFS.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Source capture:
    /// <c>E:\Dev\Ronny\X25Emulator\pcap\claude-list-files-d100-system-2026-07-29.pcapng</c> -
    /// <c>LIST-FILES d100(system).</c> issued on node 102 against node 100. SINTRAN printed the
    /// listing to the terminal at the same time, so the expected names and types below are what the
    /// machine itself said, not what this code decided.
    /// </para>
    /// <para>
    /// The machine's clock read 29 JULY 1998 during the capture, which is what makes the date
    /// assertions meaningful.
    /// </para>
    /// </remarks>
    public sealed class FaDirectoryListingTests
    {
        private static byte[] FromHex(string hex)
        {
            string clean = hex.Replace(" ", string.Empty);
            byte[] bytes = new byte[clean.Length / 2];
            for (int i = 0; i < bytes.Length; i++)
            {
                bytes[i] = Convert.ToByte(clean.Substring(i * 2, 2), 16);
            }

            return bytes;
        }

        private const string Sintran =
            "900053494e5452414e2700000000000000004441544100000000000700200000000000000000000ac1cd0de3c1cd0ef0c1cd0ef00000003f0001dfff000078da";

        private const string Segfil0 =
            "900053454746494c30270000000000000000444154410002000200e7002000000000000200000016c1cd0de4c1f87a43c1f87a430000271000e0d7ff000078da";

        private const string Rtfil =
            "9000525446494c2700000000000000000000444154410004000400e7000800000000000400000020c1cd0e00c1f87a46c1f87a460000000200000a11400078da";

        private const string SystemOutput1 =
            "900053595354454d2d4f55545055542d312753594d420006000604f701080000000000060000000dc1cd0e0600000000c1fad7a30000000b00005509400078da";

        private const string Terminal =
            "80005445524d494e414c270000000000000027000000000500051ce7000100010000000500000000c1cd0e06000000000000000000000000ffffffff000078da";

        private const string PapertapeReader =
            "80005041504552544150452d524541444552270000000009000904f7000200020000000900000000c1cd0e0c000000000000000000000000ffffffff000078da";

        /// <summary>
        /// Every captured record decodes to the name and type SINTRAN printed for that entry.
        /// </summary>
        /// <remarks>
        /// The expectations are taken from the terminal output of the same command:
        /// <c>FILE 0 : D100.(PACK-ONE:SYSTEM)SINTRAN:DATA;1</c> and so on. A device such as
        /// TERMINAL printed as <c>TERMINAL:;1</c> - no type - which is why its type is empty here.
        /// </remarks>
        [Theory]
        [InlineData(Sintran, "SINTRAN", "DATA")]
        [InlineData(Segfil0, "SEGFIL0", "DATA")]
        [InlineData(Rtfil, "RTFIL", "DATA")]
        [InlineData(SystemOutput1, "SYSTEM-OUTPUT-1", "SYMB")]
        [InlineData("80004d41434d2d415245412700000000000044415441000100010007002000000000000100000000c1cd0de4000000000000000000000040ffffffff000078da", "MACM-AREA", "DATA")]
        [InlineData("80004d41494c424f5827000000000000000044415441000300030007000000000000000300000000c1cd0e00000000000000000000000000ffffffff000078da", "MAILBOX", "DATA")]
        [InlineData(Terminal, "TERMINAL", "")]
        [InlineData("8000464c4f5050592d31270000000000000027000000000700071ce7000202000000000700000000c1cd0e06000000000000000000000000ffffffff000078da", "FLOPPY-1", "")]
        [InlineData("80004c494e452d5052494e54455227000000270000000008000818e7000200050000000800000000c1cd0e0b000000000000000000000000ffffffff000078da", "LINE-PRINTER", "")]
        [InlineData(PapertapeReader, "PAPERTAPE-READER", "")]
        public void EveryCapturedRecord_DecodesToWhatSintranPrinted(string recordHex, string expectedName, string expectedType)
        {
            ObjectEntry? entry = FaDirectoryListing.ParseRecord(FromHex(recordHex));

            Assert.NotNull(entry);
            Assert.Equal(expectedName, entry!.ObjectName);
            Assert.Equal(expectedType, entry.Type);
        }

        /// <summary>
        /// A name that exactly fills the 16-byte field decodes whole.
        /// </summary>
        /// <remarks>
        /// PAPERTAPE-READER is the entry that fixes the field width: it is exactly 16 characters and
        /// therefore carries no 0x27 terminator, so a decoder that insisted on one would truncate it
        /// or run into the type field.
        /// </remarks>
        [Fact]
        public void FullWidthName_DecodesWhole()
        {
            ObjectEntry? entry = FaDirectoryListing.ParseRecord(FromHex(PapertapeReader));

            Assert.NotNull(entry);
            Assert.Equal("PAPERTAPE-READER", entry!.ObjectName);
            Assert.Equal(16, entry.ObjectName.Length);
        }

        /// <summary>
        /// The packed date words decode to times consistent with the machine that produced them.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This is the strongest single piece of evidence that the wire record is the on-disk object
        /// entry: the dates were not guessed at, they were run through
        /// <c>RetroFS.NDFS.Elements.NdDateTime</c> - written for the on-disk structure, with no
        /// knowledge of this protocol - and they came out as coherent history.
        /// </para>
        /// <para>
        /// SINTRAN, SEGFIL0, RTFIL and SYSTEM-OUTPUT-1 were all created within seconds of each other
        /// on 1998-07-06, which is when the pack was built.
        /// </para>
        /// </remarks>
        [Fact]
        public void CreationDates_ShowTheSystemFilesWereAllMadeTogether()
        {
            DateTime? sintran = NdDateTime.FromNdDate(FaDirectoryListing.ParseRecord(FromHex(Sintran))!.DateCreated);
            DateTime? segfil0 = NdDateTime.FromNdDate(FaDirectoryListing.ParseRecord(FromHex(Segfil0))!.DateCreated);
            DateTime? rtfil = NdDateTime.FromNdDate(FaDirectoryListing.ParseRecord(FromHex(Rtfil))!.DateCreated);

            Assert.Equal(new DateTime(1998, 7, 6, 16, 55, 35), sintran);
            Assert.Equal(new DateTime(1998, 7, 6, 16, 55, 36), segfil0);
            Assert.Equal(new DateTime(1998, 7, 6, 16, 56, 0), rtfil);
        }

        /// <summary>
        /// SYSTEM-OUTPUT-1 was written on the day of the capture, and never opened for read.
        /// </summary>
        /// <remarks>
        /// The machine's clock read 29 JULY 1998 during the capture. A spooler output file being
        /// written that day, with a zero read date, is exactly the history that file should have -
        /// and a zero decodes to null rather than to a bogus 1950 date.
        /// </remarks>
        [Fact]
        public void SpoolerOutput_WasWrittenOnTheDayOfTheCaptureAndNeverRead()
        {
            ObjectEntry entry = FaDirectoryListing.ParseRecord(FromHex(SystemOutput1))!;

            Assert.Null(NdDateTime.FromNdDate(entry.LastDateOpenedForRead));

            DateTime? written = NdDateTime.FromNdDate(entry.LastDateOpenedForWrite);
            Assert.NotNull(written);
            Assert.Equal(new DateTime(1998, 7, 29), written!.Value.Date);
        }

        /// <summary>
        /// SEGFIL0 and RTFIL were last opened at the previous boot, seconds apart.
        /// </summary>
        [Fact]
        public void SystemFiles_WereLastOpenedAtThePreviousBoot()
        {
            DateTime? segfil0 = NdDateTime.FromNdDate(FaDirectoryListing.ParseRecord(FromHex(Segfil0))!.LastDateOpenedForRead);
            DateTime? rtfil = NdDateTime.FromNdDate(FaDirectoryListing.ParseRecord(FromHex(Rtfil))!.LastDateOpenedForRead);

            Assert.Equal(new DateTime(1998, 7, 28, 7, 41, 3), segfil0);
            Assert.Equal(new DateTime(1998, 7, 28, 7, 41, 6), rtfil);
        }

        /// <summary>
        /// Every file with content reports a length that fits inside its allocation.
        /// </summary>
        /// <remarks>
        /// A SINTRAN page is 1KW, which is 2048 bytes. This is the check that would break first if
        /// the pages and bytes fields were being read as something they are not.
        /// </remarks>
        [Theory]
        [InlineData(Sintran, 63u, 122880UL)]
        [InlineData(Segfil0, 10000u, 14735360UL)]
        [InlineData(Rtfil, 2u, 2578UL)]
        [InlineData(SystemOutput1, 11u, 21770UL)]
        public void FilesWithContent_FitInsideTheirAllocation(string recordHex, uint expectedPages, ulong expectedBytes)
        {
            ObjectEntry entry = FaDirectoryListing.ParseRecord(FromHex(recordHex))!;

            Assert.Equal(expectedPages, entry.PagesInFile);
            Assert.Equal(expectedBytes, entry.BytesInFile);

            Assert.True(
                entry.BytesInFile <= (ulong)entry.PagesInFile * 2048UL,
                entry.ObjectName + " reports " + entry.BytesInFile + " bytes in " + entry.PagesInFile + " pages.");
        }

        /// <summary>A reply built from an entry reads back as the same entry.</summary>
        /// <remarks>
        /// This is the offline server-to-client loop with the real structure in the middle: the
        /// server side serialises an ObjectEntry into a reply, and the client side gets it back.
        /// </remarks>
        [Fact]
        public void ReplyBuiltFromAnEntry_ReadsBackAsThatEntry()
        {
            ObjectEntry original = FaDirectoryListing.ParseRecord(FromHex(Sintran))!;

            byte[] body = FaDirectoryListing.BuildReply(4242, original);

            ushort serial;
            ObjectEntry? parsed;
            Assert.True(FaDirectoryListing.TryReadReply(body, out serial, out parsed));

            Assert.Equal(4242, serial);
            Assert.NotNull(parsed);
            Assert.Equal(original.ObjectName, parsed!.ObjectName);
            Assert.Equal(original.Type, parsed.Type);
            Assert.Equal(original.PagesInFile, parsed.PagesInFile);
            Assert.Equal(original.BytesInFile, parsed.BytesInFile);
            Assert.Equal(original.DateCreated, parsed.DateCreated);
            Assert.Equal(original.LastDateOpenedForWrite, parsed.LastDateOpenedForWrite);
        }

        /// <summary>
        /// Every decoded field of the SINTRAN:DATA entry matches what FILE-STATISTICS printed for
        /// the same file on the same day.
        /// </summary>
        /// <remarks>
        /// <para>
        /// This is the ground-truth check the earlier work asked for and could not do. The record
        /// below came off the wire in a <c>LIST-FILES</c> reply. Separately,
        /// <c>FILE-STAT d100(system).sintran:data</c> was run from node 102 on 2026-07-29 and node
        /// 100 printed:
        /// </para>
        /// <code>
        /// FILE 0 : D100.(PACK-ONE:SYSTEM)SINTRAN:DATA;1
        ///            (ALLOCATED FILE)
        ///            PUBLIC ACCESS : NONE
        ///            FRIEND ACCESS : NONE
        ///            OWN ACCESS : READ, WRITE, APPEND
        ///            OPENED 10 TIMES
        ///            CREATED 16.55.35  JULY 6, 1998
        ///            OPENED FOR READ 16.59.48  JULY 6, 1998
        ///            OPENED FOR WRITE 16.59.48  JULY 6, 1998
        ///            63 PAGES , 122880 BYTES IN FILE
        /// </code>
        /// <para>
        /// The dates, the page count, the byte count and the open count are therefore no longer
        /// inferred from internal consistency - they are confirmed against the system's own report.
        /// </para>
        /// </remarks>
        [Fact]
        public void DecodedEntry_MatchesWhatFileStatisticsPrinted()
        {
            ObjectEntry entry = FaDirectoryListing.ParseRecord(FromHex(Sintran))!;

            Assert.Equal("SINTRAN", entry.ObjectName);
            Assert.Equal("DATA", entry.Type);

            // "CREATED 16.55.35  JULY 6, 1998"
            Assert.Equal(new DateTime(1998, 7, 6, 16, 55, 35), NdDateTime.FromNdDate(entry.DateCreated));

            // "OPENED FOR READ 16.59.48  JULY 6, 1998"
            Assert.Equal(new DateTime(1998, 7, 6, 16, 59, 48), NdDateTime.FromNdDate(entry.LastDateOpenedForRead));

            // "OPENED FOR WRITE 16.59.48  JULY 6, 1998"
            Assert.Equal(new DateTime(1998, 7, 6, 16, 59, 48), NdDateTime.FromNdDate(entry.LastDateOpenedForWrite));

            // "63 PAGES , 122880 BYTES IN FILE"
            Assert.Equal(63u, entry.PagesInFile);
            Assert.Equal(122880UL, entry.BytesInFile);

            // "OPENED 10 TIMES"
            Assert.Equal(10u, entry.TotalOpenCount);
        }

        /// <summary>
        /// Bytes 1 to 63 survive a round trip through the file system layer unchanged.
        /// </summary>
        /// <remarks>
        /// ObjectEntry keeps the verbatim on-disk bytes and re-serialises from them, so everything
        /// except byte 0 comes back identical. That is what lets our server answer with entries it
        /// did not invent. Byte 0 is excluded deliberately - see
        /// <see cref="RetroFsDropsTheHighHeaderByteOnWrite"/>.
        /// </remarks>
        [Fact]
        public void RecordBelowTheHeaderByte_SurvivesARoundTripThroughTheFileSystemLayer()
        {
            byte[] arrived = FromHex(Sintran);

            ObjectEntry entry = FaDirectoryListing.ParseRecord(arrived)!;

            byte[] rewritten = new byte[FaListFilesCodec.EntryRecordLength];
            entry.ToBytes(rewritten, 0);

            for (int i = 1; i < FaListFilesCodec.EntryRecordLength; i++)
            {
                Assert.True(arrived[i] == rewritten[i], "Byte " + i + " changed across the round trip.");
            }
        }

        /// <summary>
        /// DEFECT IN RetroFS: ObjectEntry.ToBytes discards the high byte of the header word.
        /// </summary>
        /// <remarks>
        /// <para>
        /// <c>ObjectEntry.ToBytes</c> writes <c>buffer[offset] = 0x80</c> as a literal instead of
        /// writing <c>Header</c>. The captured SINTRAN:DATA entry arrives with <c>0x90</c>, so the
        /// round trip silently clears bit 12 - "file modified", per the field documentation on
        /// <c>ObjectEntry.Header</c>. Bits 15 to 8 are all at risk, which is the used / write /
        /// reserved / modified flags and the user-vs-object entry flag.
        /// </para>
        /// <para>
        /// This is a real data-losing bug on the RetroFS write path, not a quirk of this protocol:
        /// any on-disk entry rewritten through ToBytes loses those flags. It matters here because a
        /// COSMOS server answering from RetroFS would hand clients entries with the modified flag
        /// stripped.
        /// </para>
        /// <para>
        /// This test asserts the CURRENT, WRONG behaviour on purpose, so that fixing RetroFS makes it
        /// fail and forces this note to be revisited. Fix is one line: write the header word instead
        /// of the literal.
        /// </para>
        /// </remarks>
        [Fact]
        public void RetroFsDropsTheHighHeaderByteOnWrite()
        {
            byte[] arrived = FromHex(Sintran);
            Assert.Equal(0x90, arrived[0]);

            ObjectEntry entry = FaDirectoryListing.ParseRecord(arrived)!;

            // The header was read correctly...
            Assert.Equal(0x9000u, entry.Header);

            byte[] rewritten = new byte[FaListFilesCodec.EntryRecordLength];
            entry.ToBytes(rewritten, 0);

            // ...but writing it back forces 0x80 and loses bit 12.
            Assert.Equal(0x80, rewritten[0]);
        }
    }
}
