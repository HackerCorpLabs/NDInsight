# FA listing: SOLVED - a remote LIST-FILES now prints (2026-08-05)

> **The listing works.** D100 lists our C# `*FA-SERVER` over HDLC and prints:
>
> ```
> FILE 0 : D103.(PACK-ONE:SYSTEM)HELLO:TXT;1
> FILE 1 : D103.(PACK-ONE:SYSTEM)README:TXT;1
> FILE 2 : D103.(PACK-ONE:SYSTEM)THIRD:TXT;1
> ```
>
> Five defects, found in this order. The first four were in the RECORD; the fifth was the
> DISPATCH, and it was the one that mattered:
>
> 1. the file ACCESS word was zero - nobody could read the file, not even its owner (section 3)
> 2. the FILE POINTER said contiguous while the attributes said indexed (section 5)
> 3. the CREATION DATE was zero, which no real record has ever been (section 6a)
> 4. header BIT 12 was clear while the record carried a real byte count (section 8a)
> 5. **every request was read as "give me the next file"** - so the two requests asking for the
>    DIRECTORY and the USER were answered with file records, and the client gave up (section 8d)
>
> Fixing 1 to 4 changed nothing D100 could see. Only 5 made it print.

---

# FA listing record: bytes 26-29 named, and earlier readings corrected (2026-08-05)

Closes the last open field in the 64-byte directory record - the one
[HANDOFF-FA-FILE-SERVER-2026-08-04.md](HANDOFF-FA-FILE-SERVER-2026-08-04.md) called "the next
lead". It also corrects two fields that were previously written down as MEASURED and were wrong.

**None of this needed the machine.** It came from the manuals and from the capture already in the
repository.

---

## 1. The headline

**Record bytes 26-27 are the SINTRAN file ACCESS word, and we were sending zero** - a file that
grants nobody read, write or append, not even its owner. That is the leading suspect for why D100
ran the whole listing conversation, served both files, and then discarded the result and retried.

Bytes 28-29 were never a defect at all.

---

## 2. How it was found - and the lesson

The two words were called "unexplained" for a day. They are named outright in two manuals **already
in this repository**, and a project document had already decoded both from a disc image and marked
them VERIFIED:

 - `Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`, appendix C, the byte-indexed
   object-entry table.
 - `Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md`, an annotated
   `@DUMP-OBJECT-ENTRY` listing that labels every word of the record.
 - `SINTRAN\Filesystem\on-disk-format\object-entry.md`, sections 2, 4.2 and 4.3.

The System Supervisor dump is the strongest of the three because it names the words in place:

```
002377                 ACCESS WORD
000040                 OBJBL (BITS 017-014)/TEMP/L/M/A/C/I/S/P/T
000000                 DEVICE NUMBER
```

That is the standing repository lesson again, and it has now cost time twice: **grep the manuals
first.** Nothing here required a live machine, a capture session or a carve.

---

## 3. VERIFIED - bytes 26-27, the ACCESS word

Three 5-bit tiers, bit 15 unused:

```
bit 15 | bits 14-10 PUBLIC | bits 9-5 FRIEND | bits 4-0 OWN
```

Within a tier: `D = 0x10`, `C = 0x08`, `A = 0x04`, `W = 0x02`, `R = 0x01`.

The tier split and order come from the manuals. The letter-to-bit assignment inside a tier is
**INFERRED** - it is carried over from the ndfs-c reader and flagged as inferred in
`object-entry.md` section 4.2. It is not load-bearing for anything below.

Every value in `DOC\captures\FA-READ-WRITE-2026-08-04\capture-list-files.txt`, all 49 records:

| value | count | own | friend | public | which files |
| --- | --- | --- | --- | --- | --- |
| `0x04F7` | 40 | RWA+D | RWA | R | ordinary user files |
| `0x0007` | 3 | RWA | none | none | SINTRAN, MACM-AREA, MAILBOX - system files |
| `0x00E7` | 2 | RWA | RWA | none | SEGFIL0, RTFIL |
| `0x1CE7` | 2 | RWA | RWA | RWA | TERMINAL, FLOPPY-1 |
| `0x18E7` | 1 | RWA | RWA | WA | LINE-PRINTER |
| `0x7FFF` | 1 | all | all | all | ENCOS-LOAD-0-B01:LIST |

`0x04F7` is what an ordinary user file carries, and it is what we now send. The nine that differ
are system files and peripheral files, which we do not serve.

---

## 4. VERIFIED - bytes 28-29, the ATTRIBUTE word - and NOT a defect

Bits 15-12 are the object block number; bits 8-0 are the logical file type:

```
bit 8 temporary | bit 7 library | bit 6 magnetic tape | bit 5 allocated
bit 4 contiguous | bit 3 indexed | bit 2 spooling | bit 1 peripheral | bit 0 terminal
```

The 2026-08-04 diff read our `0x0008` against a real `0x0020` and filed it as unexplained. Both are
correct - they are different KINDS of file:

| value | count | meaning |
| --- | --- | --- |
| `0x0008` | 37 | indexed - an ordinary user file, which is what we serve |
| `0x0020` | 3 | allocated - SINTRAN, MACM-AREA, SEGFIL0 |
| `0x0002` | 5 | peripheral |
| `0x0108` | 1 | indexed + temporary (SYSTEM-OUTPUT-1) |
| `0x0001`, `0x0010`, `0x0000` | 1 each | terminal; contiguous; MAILBOX |

The diff compared our indexed user file against `SINTRAN:DATA`, an allocated system file. **We were
already right.** The lesson is narrower than the last one but worth keeping: a one-record diff
cannot tell "wrong value" from "different kind of thing".

---

## 5. CORRECTED - bytes 60-63 are not constant

`NOTES-FOR-RONNY-2026-08-04.md` and the code comment both said `0x000078DA` on **all 49** records.
Only bytes 62-63 are constant. Bytes 60-61 are `0x4000` on 38 records and `0x0000` on 11:

```
attribute word 0x0008 (indexed)      ->  pointer 0x400078DA    x38
attribute word 0x0020 / 0x0002 / ... ->  pointer 0x000078DA    x11
```

The field is a block pointer: two bits of type over a 30-bit page id, with type `01` = indexed and
`00` = contiguous. So the top two bits simply repeat what the attribute word says, and they never
disagree in the capture. **We were declaring an indexed file and then handing over a contiguous
pointer** - a self-contradictory record.

The low page id `0x78DA` really is the same on all 49 records, for 49 different files. On a real
pack these differ (the System Supervisor dump shows `0x400049F5`), so **INFERRED** that the FA
server substitutes something of its own rather than handing a remote client disc addresses. What it
is remains UNKNOWN. We reproduce it because it is what the wire carries.

---

## 6. CORRECTED - bytes 22-25 are the VERSION pointers, not "the walk ordinal"

`a85ad3e` recorded bytes 22-25 as the entry's ordinal in the walk. They are the **next and previous
version pointers**, and each holds an object index; a single-version file points both at itself.
The System Supervisor dump of object entry 025 prints `000025 000025  POINTERS TO NEXT AND PREVIOUS
VERSION`.

The same correction applies to bytes 34-35, the OBJECT INDEX: it is the entry's **slot** in the
directory, not its position in the walk. The two agree only while the directory has no holes. In
the capture the first 42 entries agree and then deleted slots start being skipped:

```
walk position 45  =  LOAD-MODE:BATC     object index 0x0030 = 48
walk position 46  =  FTPULL:SYMB        object index 0x0033 = 51
```

**No behaviour changes.** We synthesise the directory from a Windows folder and never leave a hole,
so slot and walk position coincide. Only the name was wrong - but a wrong name is what makes the
next person write the wrong code when the sidecar design gives files real slot numbers.

---

## 6a. CORRECTED - a zero CREATION DATE is something no real record carries

The 2026-08-04 notes waved the missing dates away: "not a protocol fault - the capture has real
records carrying `0000` in these fields too". That is true of the last-opened fields and **false of
the creation date**. Counted per FIELD rather than per record, over all 49:

| field | record bytes | how many of 49 are zero |
| --- | --- | --- |
| `DateCreated` | 40-43 | **0 - never** |
| `LastDateOpenedForRead` | 44-47 | 17 |
| `LastDateOpenedForWrite` | 48-51 | 9 |

Every file we serve carried zero in all three, because the packed ND date holds only 1950-2013 and
every file in a Windows folder has a present-day timestamp. So the creation date was in exactly the
same position as the access word: **a field that is never zero on real hardware and always zero on
ours.**

`FaFolderEntry.ToListingDate` now folds the year by whole 64-year cycles - the format's own span -
until it is representable, keeping month, day and time of day. `2026-08-05` becomes `1962-08-05`.
The year is congruent to the real one modulo the span, so the mapping is reversible and derived
from the format rather than invented.

**This is a compromise and is recorded as one.** It is not the real year. The alternatives were a
zero the wire contradicts, or a fixed made-up date carrying no information at all. The proper fix
is the sidecar in `PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md` section 1.
`LastDateOpenedForRead` is deliberately left at zero - we have nothing to put there, and a third of
real records leave it zero too.

---

## 7. What changed in the code

| File | Change |
| --- | --- |
| `SRC\Xmsg.Ndfs\FaFolderEntry.cs` | sets `AccessBits` (was zero), sets `FileTypeFlags` explicitly, sets the file pointer to type INDEXED, sets both version pointers, folds the creation date into range instead of writing zero; the hand-written byte pokes after `ToBytes` are gone |
| `SRC\Xmsg.Ndfs.Tests\FaFolderEntryTests.cs` | access word must not be zero and must grant own RWA; the attribute word and the pointer type must agree; version pointers must match the object index; a present-day file still gets a creation date, and an in-range date is not folded |

`ObjectEntry` already models all four fields - `AccessBits`, `FileTypeFlags`,
`ObjectEntryNextVersion` / `ObjectEntryPrevVersion` and `FilePointer`. Writing bytes into the buffer
by hand after `ToBytes` is what hid that, so nothing is poked in any more.

575 tests pass.

---

## 8. LIVE RUN 2026-08-05 07:20 - the access word reached the wire, and did NOT fix the listing

Run against a freshly booted D100 over HDLC, link Active, `LIST-FILES D103(sys).`. The record we
sent for `HELLO:TXT`, lifted from the runner log:

```
8000 "HELLO'"..... "TXT'" 0000 0000 04F7 0008 0000 ...
                          ^ordinal  ^access ^attrs
... 00000001 00000012 400078DA
    pages=1  bytes-1  INDEXED pointer
```

Both fixes are confirmed on the wire: access `0x04F7`, pointer `0x400078DA`. `README:TXT` was
identical at ordinal 1.

**D100 behaved exactly as before.** It reserved the file entry, walked both files, sent `0x0782`
finished, we answered with a Close, and it opened a second connection. Nothing printed.

Two things the run DID establish:

 - **Zero XENSE, zero network errors, zero rejections** in the whole conversation. The sequencing
   model and the frame shapes are not the problem.
 - **D100 never asked for a third entry.** It stopped after exactly as many requests as we have
   files, so the end-of-directory reply - the short 24-byte one in the capture - was never
   exercised. Whether that is D100 giving up or D100 satisfied is the open fork.

So the access word was a real defect and worth removing, but it was NOT the blocker. The date fix in
section 6a was made after this run and has NOT been live-tested.

---

## 8a. CORRECTED - header bit 12, and the record is now byte-plausible everywhere

Two more live runs followed, each fixing one more measurable difference.

**Header bit 12.** The comment in `FaFolderEntry` said bit 12 "describes pack state we do not
have", so we sent a bare `0x8000`. Measured over all 49 records, bit 12 tracks the max byte pointer
exactly:

| bit 12 | max byte pointer | records |
| --- | --- | --- |
| set | a real byte count | 40 |
| clear | `0xFFFFFFFF` | 9 |
| any other pairing | - | **0** |

We were sending bit 12 CLEAR with a real byte count - a pairing that occurs nowhere in the capture,
the same class of self-contradiction as the indexed/contiguous pointer. Now `0x9000`.

**HONEST LIMIT:** this does not prove bit 12 means "the byte count is valid". `ObjectEntry` calls it
"file modified", and on this data the two readings cannot be told apart - every file with a real
length has also been written, every peripheral file has neither. It does not matter for a served
file, where both readings say the same thing. It would matter if anything tried to READ the bit.

**Where that leaves the record.** After the access word, the pointer type, the creation date and
bit 12, every FA message we send now matches the real server byte for byte:

 - the 64-byte record - every field named and agreeing with a real one of the same kind
 - the reply wrapper `8C 4B  A2 0000  A2 <ordinal>  A2 0001  B0 40` - identical, ordinal for ordinal
 - the 18-byte `ReserveFileEntry` reply - identical apart from the responder session token
   (`0x9081` against `0x90BB`), which is the server's own per-conversation value and varies across
   the recordings we hold

**And D100 still stops after exactly two entries and prints nothing.** So the fault is NOT in the
FA message bytes, and replaying a real captured record verbatim - the experiment held in reserve -
would no longer tell us anything, because our record already IS one in every checkable respect.

---

## 8b. SOLVED (very likely) - we never send the PACK entry, and D100 aborts where it should arrive

**The measurement that decides it.** Served THREE files instead of two, on a freshly restarted
machine (first inbound Flags 1 `0x0000`, so the run is valid). D100 asked for **exactly two entries
again**. Its stopping point does NOT track our file count, which kills "the walk-end signalling is
wrong" - we never reach the end of the walk at all.

**What the real server sends as the second entry.** The capture holds two record shapes:

| shape | count | wrapper | what it is |
| --- | --- | --- | --- |
| `B0 40` (64 bytes) | 49 | `8C 4B  A2 0000  A2 <ordinal>  A2 0001  B0 40 <rec>` | a file entry |
| `B0 2A` (42 bytes) | **1** | `8C 2F  B0 2A <rec>  A2 0001` | the **PACK / directory entry**, `PACK-ONE` |

Matched by session-header byte, the real walk runs:

```
request 81 (long, cursor FFFF)  -> SINTRAN:DATA   B0 40
request 82 (short, cursor 0000) -> PACK-ONE       B0 2A     <- WE NEVER SEND THIS
request 83 (short, cursor 0000) -> MACM-AREA      B0 40
```

The pack entry does not consume a walk ordinal - the file records carry `A2 0000, A2 0001,
A2 0002 ...` unbroken across it - and its reply has a **different shape**, one `A2` field instead
of three.

`XMSG-LIST-FILES-ON-THE-WIRE-2026-07-29.md` section 4.3 already had this record isolated, and
`FILE-STATISTICS` names what it is for: a file is reported as
`D100.(PACK-ONE:SYSTEM)SINTRAN:DATA;1`, so **`PACK-ONE` is the directory the listing is headed
with**. A client that cannot get it cannot print the listing header.

That is exactly where D100 stops: it takes our first file, asks again expecting the pack entry, gets
a 64-byte file record instead, and abandons the conversation without a `ReleaseFileEntry`.

**INFERRED, not yet proven:** that supplying the pack entry as the second reply is sufficient. It is
the next thing to implement and the next live run to make.

The 42-byte record, from the capture:

```
 0- 9  D0 01 02 40 00 00 05 40 00 00     preamble, five words - UNDECODED
10-25  "PACK-ONE" 27 then zeros          16-byte name, 0x27 terminated
26-29  40 00 48 FC                       block pointer, type 01 = indexed
30-33  40 00 48 FE                       block pointer, type 01 = indexed
34-37  00 00 48 24                       ?
38-41  00 00 34 7E                       ?  (a second capture reads 00 00 3F D2 here)
```

---

## 8c. The QFORM tag rule - asked 2026-08-05, and it was already carved

Whether the octal symbol tables help decode the tags: **they do not, and the answer was already in
the repo.** Searched exhaustively - `SYMBOLS.FADM` is a SINTRAN segment/address map and
`REFERENCE.FADM` an address-to-symbol cross reference, neither has anything to do with File Access;
the `.SYMB` and `.INCL` files stop at the XMSG kernel and XROUT layer. The FA message-type octals
appear nowhere. ND's manuals never document this encoding at all - and ND's own "QFORM" is a
terminal OUTPUT-FORMATTING language (SINTRAN III Communication Guide 4.6.1), not this. The project
borrowed the name.

The only authoritative specification is ND's own reader, `qform_read_tag_and_value` at `ram:0x7d01`
in `COS-FA-SERV-E04.PROG`, and it was already disassembled - the rule lives in
`SRC\Xmsg.Protocol\Qform\QformReader.cs`:

```
bit 7 CLEAR       -> END OF STREAM
class = (tag & 0x70) >> 4
class 1..7 : length = tag & 0x0F,  a nibble of 0 escapes to the next byte
class 0    : subtype = tag & 0x17  (NOT a length), length ALWAYS escaped
escape byte 0x80 -> the length is in the byte after it
```

**So `B0 40` and `B0 2A` are the SAME tag.** Class 3, byte string, low nibble 0 = escaped length -
`0x40` = 64 and `0x2A` = 42 are just sizes. The record KIND is carried by its LENGTH, nothing else,
which is why section 4.2 of the 2026-07-29 document could already say "the entry record is not a
fixed-size struct".

It is BER-like but is not BER: constructed is class 0 rather than bit 5, the short length lives in
the tag's own nibble, and "bit 7 clear ends the stream" has no BER counterpart. Treat it as ND's own
house TLV.

---

## 8d. THE ANSWER - the walk is not positional, the request says WHICH entry it wants

Section 8b guessed the pack entry belonged at walk position 2. That was close enough to make D100
take one more step - the live run went file, pack, file and then stopped - but it was the wrong
model. **The client asks for what it wants.**

Pairing every request with its reply through the session-header byte:

```
sess  request  reply  carries
 80    112 B    18 B  ReserveFileEntry
 81    100 B    98 B  FILE  SINTRAN
 82     32 B    70 B  PACK  PACK-ONE
 83     32 B    40 B  USER  SYSTEM
 84    100 B    98 B  FILE  MACM-AREA
 85    100 B    98 B  FILE  SEGFIL0
```

The request sizes sort into two groups, and the field that varies with them is the FOURTH tagged
word of the request body:

| sub-function | asks for | reply |
| --- | --- | --- |
| `92 0078` | the next FILE entry | 98 bytes, `B0 40` record |
| `92 00A4` | the DIRECTORY entry | 70 bytes, `B0 2A` record |
| `92 008C` | the USER entry | 40 bytes, `8C 12 B0 10` name string |

The two short requests arrive as a pair right after the first file, which is exactly what printing
`D103.(PACK-ONE:SYSTEM)HELLO:TXT;1` needs - the directory and the user come from their own requests,
not from the file entries.

**We answered all three with file records.** That is why D100 abandoned a listing after the second
file whatever the folder held, and why serving three files instead of two never moved the stopping
point. Modelled now as `FaListingFunction`, and neither short request touches the walk position, so
the file ordinals still count 0, 1, 2 unbroken across them.

The user reply's 16-byte field reads `"SYSTEM" 27` then `40 00 00 "PACK-O"` in the capture. The tail
is uninitialised server memory, not a field - the name ends at its `0x27`. We pad with zeros rather
than reproduce another machine's leftovers.

---

## 8e. `0x0788` - the finished message is a FAMILY

With the listing printing, one rough edge was left: D100 ended the completed conversation with
message type `0x0788`, which we answered with a BadRequest, and it reconnected once for nothing.

Three variants have now been seen at the same point, with the same ten bytes and the same layout:

 - `0x0782` captured between two real ND machines at the end of a full listing.
 - `0x0781` from D100 when it gave up part-way through.
 - `0x0788` from D100 after a listing that ran to completion.

The test is now the family `0x078x` rather than a list of exact values. **What the low nibble means
is still UNKNOWN** - it clearly varies with how the conversation ended, but three samples cannot say
how, and no manual we hold documents these message types at all. What IS established is the cost of
getting it wrong: a finished message carries no operation/sequence pair, so a server that only
parses requests answers it with an error and the client reconnects.

---

## 9. Still open

The listing works, so what follows is loose ends rather than blockers.

1. **The pack fields are SYNTHESISED.** The three block pointers and the unreserved-page count in
   the directory entry are copied from the captured `PACK-ONE`; we serve a Windows folder and have
   no pack. The NAME is the only field that carries real meaning, and it is what the header needs.
   If a client ever follows one of those pointers, this breaks.
2. **The directory and user names are a CHOICE**, `PACK-ONE` and `SYSTEM`, hard-coded in
   `FaServer`. They belong in the topology file beside the served root.
3. What the low nibble of the `0x078x` finished family means (section 8e).
4. What the low page id `0x78DA` is (section 5).
5. ~~**Only LIST-FILES works.** Read, write, open, close, create and delete are still refused.~~
   **CLOSED 2026-08-06.** Open, close, set-block-size, read, write, create and delete are all
   served. A 12690-byte file has been read off the server and written back to it byte-for-byte
   from a live D100, so COSMOS file ACCESS is done, not just LISTING. Create is served but has not
   been driven live - `COPY-FILE` does not send it. Three operations are still refused because
   their layouts have never been captured: `0x01`, `0x04` and `0x0D`.
6. The date fold means a listing shows 1962 for a 2026 file (section 6a).
