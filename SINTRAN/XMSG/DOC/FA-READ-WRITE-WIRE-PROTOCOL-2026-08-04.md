# COSMOS FA: the READ and WRITE wire protocol (2026-08-04)

Captured live from two real SINTRAN III machines, D100 and D102, sharing one emulated Ethernet
segment through `Xmsg.Hub`. A third client joined the hub as a silent observer and logged every
frame; it never transmitted, so nothing here can be an artefact of the observer.

Until now `FaOperation.ReadFile` (`0x0008`) and `FaOperation.WriteFile` (`0x0009`) both said
"Never recorded. Request and reply layouts are UNKNOWN". Both are now recorded.

Raw captures and the decoding scripts are preserved next to this document in
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\captures\FA-READ-WRITE-2026-08-04`:

| File | What it holds |
| --- | --- |
| `capture-read.txt` | Two complete file reads, D100 pulling from D102 |
| `capture-write.txt` | One complete file write, D100 pushing to D102 |
| `capture-open-error.txt` | An open that failed, for the error path |
| `capture-list-files.txt` | `li-fi d102(sys).,,`, kept as the reference conversation |
| `ethsniff.ps1` | The passive hub observer that produced them |
| `decode.py`, `reasm.py` | The decoders every hex quote below came out of |

Each capture line is `HH:MM:SS.mmm <length> <full frame hex>`.

---

## 1. How the traffic was provoked

`COPY-FILE` **does** accept a remote file, but only with a dot between the user part and the file
name. These two forms were rejected with `ILLEGAL CHARACTER IN PARAMETER`:

```
copy-file "RDTEST1:OUT",d102(system)patch-file:out
copy-file "RDTEST2:OUT","D102(SYSTEM)PATCH-FILE:OUT"
```

These three were accepted and produced everything in this document:

```
copy-file "RDTEST3:OUT",d102(system).patch-file:out      READ,  9 blocks
copy      "RDTEST4:OUT",d102(system).patch-file:out      READ,  9 blocks, different block size
copy-file d102(system)."WRTEST1:OUT",RDTEST3:OUT         WRITE, 9 blocks
copy-file "ERRTEST1:OUT",d102(system).NO-SUCH-FILE:OUT   open failure
```

The quoting rule holds exactly as recorded before: quote the file being CREATED, leave the existing
one bare. The quotes wrap the file-name part only, not the `(USER)` part.

---

## 2. VERIFIED - `SintranHeader.Size` is 14, not 13

This had to be settled first because every FA offset hangs off it. It is settled by arithmetic, not
by opinion.

The SINTRAN datagram header is **7 words = 14 bytes**, and word 6 is a ones-complement sum of
words 0 to 5 with end-around carry, complemented:

```
21 13 | 00 0E | 00 66 | 00 64 | 01 BB | 00 64 | DB F5
 w0     w1      w2      w3      w4      w5      w6

0x2113 + 0x000E + 0x0066 + 0x0064 + 0x01BB + 0x0064 = 0x240A
~0x240A = 0xDBF5   == w6
```

Run over every data datagram in all four captures:

```
datagrams            : 2245
checksum ok, 14-byte : 2245
checksum ok, 13-byte : 0
```

**2245 out of 2245 for 14 bytes, 0 out of 2245 for 13.** A checksum is the ideal test here because
it is falsifiable: a wrong boundary cannot pass it by luck 2245 times.

What the 13-byte model got wrong: it names byte 12 `ProtocolId`. Byte 12 is the HIGH byte of the
checksum, and byte 13 is its low byte. The C# model then recovers the lost byte by calling byte 13
the sub-header's `Counter`, so the two errors partly cancel and the marker `0x21 0x00` still lands
where the code expects it. That is why the mistake survived this long.

### 2b. VERIFIED - the sub-header is 14 bytes and its last word is the length

After the 14-byte SINTRAN header comes a 14-byte sub-header. Its last word is always equal to the
datagram's Flags2:

```
sub-hdr starts with 21 00 : 1069 of 1069
sub[12:14] == Flags2      : 1069 of 1069
```

So the FA body starts at absolute offset **28**, not 32. Laid out on a real frame:

```
abs 0-13   21 13 00 0E 00 66 00 64 01 BB 00 64 DB F5   SINTRAN header, checksum ok
abs 14-27  21 00 82 84 00 66 06 B6 00 64 08 35 00 64   sub-header; 0066=dest 102, 0064=src 100,
                                                        0064=100=the body length
abs 28+    07 F0 00 08 98 00 D7 61 92 00 0C ...        FA body: message type 07F0 onward
```

`XmsgSubHeader.Size` is declared 19 in the C# and `FaServer.cs` line 618 takes the body at
`13 + 19 = 32`. Measured, the body starts at `14 + 14 = 28`. **The C# is four bytes past the start
of every FA body it parses.** UNVERIFIED whether 19 is right for some other sub-protocol; every FA
datagram measured here is 14.

---

## 3. The shape of a file transfer

Both a read and a write are one FA conversation. The order below is from `capture-read.txt` and
`capture-write.txt` in wire order:

```
READ                                    WRITE
ReserveFileEntry  0x0002                ReserveFileEntry  0x0002
OpenFile          0x0005                OpenFile          0x0005
SetBlockSize      0x0007                SetBlockSize      0x0007
SiiiSpecial       0x000C sub 0x0021     ReadFile          0x0008  x 9
ReadFile          0x0008  x 9           SiiiSpecial       0x000C sub 0x003B
CloseFile         0x0006                CloseFile         0x0006
ReleaseFileEntry  0x0003                ReleaseFileEntry  0x0003
```

The `copy` command (conversation `0x0043`) omitted `SetBlockSize` and the `SiiiSpecial 0x0021`;
`copy-file` (conversation `0x0042`) sent both. That difference is what pins down the block-size
rule in section 6.

**VERIFIED - the two directions differ in when the data moves.**

 - READ: request, then the reply, THEN the data messages from the server.
 - WRITE: request, then the data messages from the client, THEN the reply.

The reply is therefore the completion of the operation in both cases, which is a tidy rule to build
a server on.

---

## 4. VERIFIED - the READ request and reply

Full frame, D100 to D102, `capture-read.txt` line 79:

```
0800266600000800266400000044 A8A803
0B02 20 00 05 2143 2D18 0036
2113 000E 0066 0064 0202 001A DBF8
2100 8284 0066 06B6 0064 0812 001A
07F0 0042 84 00 D761 92 0008 92 0005 F2 0001 A4 00000000 F2 00FF 55
```

FA body, field by field:

| Offset | Bytes | Meaning | Status |
| --- | --- | --- | --- |
| 0 | `07F0` | Message type: a request carrying a body | VERIFIED, matches `FaMessageType` |
| 2 | `0042` | Conversation number, chosen by the asker | VERIFIED |
| 4 | `84` | Exchange counter, `0x80 + n` on the asker's side | VERIFIED |
| 5 | `00` | Always zero in every FA message captured | VERIFIED as a value, meaning UNKNOWN |
| 6 | `D761` | Session token for this conversation | VERIFIED per-conversation, meaning UNKNOWN |
| 8 | `92 0008` | Operation = `ReadFile` | VERIFIED |
| 11 | `92 0005` | Exchange sequence | VERIFIED |
| 14 | `F2 0001` | Selector 1 follows | VERIFIED |
| 17 | `A4 00000000` | Selector 1 value: the read POSITION, 32-bit | VERIFIED |
| 22 | `F2 00FF` | End of fields | VERIFIED |
| 25 | `55` | Filler past the terminator | VERIFIED as junk, see section 9 |

The reply, D102 to D100:

```
07F0 0002 84 00 90BB 92 0008 92 0005 F2 00FF 8C
```

It echoes the operation and the exchange sequence and carries no other field. Conversation `0002`
is the responder's, exactly as `FaExchangeCodec.ResponderConversation` already says.

**VERIFIED - the position advances by one per request.** The nine reads of conversation `0x0042`,
in wire order, quoting only the changing part:

```
A4 00000000   A4 00000001   A4 00000002   A4 00000003   A4 00000004
A4 00000005   A4 00000006   A4 00000007   A4 00000008
```

**VERIFIED - a read may also state its own byte count.** Conversation `0x0043`, which never sent
`SetBlockSize`, added selector 3 to every request and got selector 2 back:

```
request  ... 92 0008 92 0003 F2 0001 A4 00000000 F2 0003 A2 0800 F2 00FF FF
reply    ... 92 0008 92 0003 F2 0002 A2 0800 F2 00FF A4
```

`0x0800` = 2048. Selector 3 in the request is the byte count asked for; selector 2 in the reply is
the byte count delivered. Its positions ran `0, 4, 8, 0C, 10, 14, 18, 1C, 20` - step 4, for the
same 2048 bytes per read that conversation `0x0042` got with step 1.

---

## 5. VERIFIED - the WRITE request and reply

```
request  07F0 0044 83 00 D761 92 0009 92 0004 F2 0001 A4 00000000 F2 00FF BF
   ... two 1032-byte data messages from D100 ...
reply    07F0 0002 83 00 90BB 92 0009 92 0004 F2 00FF A2
```

**The request is the READ request with the operation changed from `0x0008` to `0x0009`. Nothing
else differs.** Same selector 1, same 32-bit position under tag `A4`, same reply shape. The nine
write positions ran `0, 1, 2, 3, 4, 5, 6, 7, 8`, matching the block size of 2048 that this
conversation set with `SetBlockSize`.

That symmetry is the single most useful result here: one codec serves both.

---

## 6. VERIFIED numbers, INFERRED rule - what the position counts

Every read and every write in every conversation moved exactly 2048 bytes. The position step did
not agree:

| Conversation | `SetBlockSize` sent | Position step | Bytes moved per request |
| --- | --- | --- | --- |
| `0x0042` read | yes, `A2 0800` = 2048 | 1 | 2048 |
| `0x0043` read | no | 4 | 2048 |
| `0x0044` write | yes, `A2 0800` = 2048 | 1 | 2048 |

**VERIFIED**: the steps and the byte counts, off the captures.

**INFERRED**: the position is an index in units of the CURRENT block size, so setting the block
size to 2048 makes the step 1, and the default block size is 512 bytes, which makes 2048 bytes
four units. The only protocol difference between `0x0042` and `0x0043` is the `SetBlockSize`, so
nothing else in the capture can account for the change. It is still an inference: the default was
never stated on the wire, only deduced from `2048 / 4`.

`SetBlockSize` itself, now recorded for the first time:

```
request  07F0 0042 82 00 D761 92 0007 92 0003 F2 0001 A2 0800 F2 00FF 0A
reply    07F0 0002 82 00 90BB 92 0007 92 0003 F2 00FF A2
```

Selector 1 carries the block size in BYTES as a 16-bit value.

---

## 7. VERIFIED - how the file data is carried

A data message is its own FA message, not a field inside the request or reply.

**Its body is 1032 bytes: an 8-byte envelope and then exactly 1024 bytes of raw file content.**
There is no QFORM tagging inside it, no length, no offset, no checksum.

```
07F0 0044 04 00 D761 | <1024 bytes of the file>
```

The bytes that follow really are the file: the first read of `PATCH-FILE:OUT` starts
`8D 0A C0 28 4E 44 2D 50 41 D4 ...`, which read as ND 7-bit text (mask each byte with `0x7F`) is
`.. (ND-PAT...` - the opening of the patch file, with `0x8D` as ND's newline. It was tempting to
read that leading `8D` as a QFORM tag; it is file content.

**VERIFIED - a 2048-byte block is two data messages.** Every request in every conversation was
followed by exactly two, and 2 x 1024 = 2048 = the block size.

**VERIFIED - each 1032-byte message is split across two datagrams by the frame layer**, using
subtypes `0x0A` and `0x0C`, and the split is at 594 bytes with the continuation frame's Flags2
carrying the resume offset:

```
d102 -> d100  subtype 0A  plen 622  Flags2 0x0408 = 1032   first 594 body bytes
d102 -> d100  subtype 0C  plen 452  Flags2 0x0252 =  594   the remaining 438
594 + 438 = 1032
```

This CONFIRMS `FaTransferCodec.FirstFragmentBodyLength = 594` and the 0x0A / 0x0C subtypes on
traffic those constants were not derived from. It also shows the earlier constants were measured on
a slightly different message: `FaTransferCodec` documents a 1030-byte message with a 6-byte
function/page/displacement header. **The READ and WRITE data message is 1032 bytes with an 8-byte
envelope and no page or displacement.** They are two different message shapes; do not reuse the
1030-byte codec for `ReadFile` and `WriteFile`.

Each data message is acknowledged on its own with a short `07A2` message before the next is sent -
stop and wait, exactly as recorded for the older transfer service.

### The envelope's counter byte

For the write, the counter bytes of the eighteen data messages ran:

```
04 85 | 07 88 | 0A 8B | 0D 8E | 10 91 | 13 94 | 16 97 | 19 9A | 1C 9D
```

**VERIFIED**: the counter increments and the top bit `0x80` is set on the SECOND message of every
pair, in all eighteen, and in all thirty-six read data messages too. **INFERRED**: `0x80` marks the
last data message of a block. **UNKNOWN**: why the token word alongside it alternates between the
conversation's own token (`D761` writing, `90BB` reading) on the first message and `0001` on the
second. It is reproduced here because it is what the wire contains.

---

## 8. VERIFIED - how the transfer ends, and the partial last block

**There is no end-of-data marker in the read stream and no short block.** `PATCH-FILE:OUT` is
17904 bytes. Nine reads of 2048 = 18432 were issued, and the LAST one returned a full
`A2 0800` = 2048 bytes like all the others:

```
request  ... 92 0008 92 000B F2 0001 A4 00000020 F2 0003 A2 0800 F2 00FF FF
reply    ... 92 0008 92 000B F2 0002 A2 0800 F2 00FF A4
```

Position `0x20` = 32 units of 512 = byte 16384, which leaves only 1520 real bytes. The server sent
2048 anyway. So the caller must stop on a count it already knows, and it does: the OPEN reply gives
it the file's byte size.

Open reply for a read:

```
07F0 0002 81 00 90BB 92 0005 92 0002 F2 0002 A2 0040 F2 0003 A4 000045F1 F2 00FF 00
                                     selector 2: file number 0x40 = 64
                                     selector 3: file size 0x45F1 = 17905 bytes
```

Open reply for a write, on a file just created and therefore empty - the size field is simply
absent:

```
07F0 0002 81 00 90BB 92 0005 92 0002 F2 0002 A2 0040 F2 00FF A4
```

**VERIFIED - the writer states the true length afterwards.** The write shipped nine whole 2048-byte
blocks, then sent this before closing:

```
07F0 0044 9E 00 D761 92 000C 92 000D F2 0001 92 003B F2 0002 8C 80 05 A4 000045F0 F2 00FF
                     SiiiSpecial     sub-function 0x003B     A4 0x45F0 = 17904 bytes
```

`0x45F0` = 17904 is the byte size of the file that was written, and it is not a multiple of 2048.
So `SiiiSpecial` sub-function `0x003B` sets the file's byte length after a write. That closes the
open question in `FaOperation.SiiiSpecial`, which recorded `0x003B` as an unexplained sub-function
seen at teardown - **it is the end-of-file setter, not teardown**.

The independent cross-check: the same size appears in the `SiiiSpecial 0x0021` file-information
reply taken during the READ, alongside a block count of nine:

```
... 00 00 00 09 | 00 00 45 F0 ...
       9 blocks    17904 bytes
```

INFERRED, not verified: the inner structure of that `8C 80 05` prefix before the `A4`. `0x8C`
elsewhere is a counted field whose length byte follows, and `0x80 05` does not read as a length
here. Do not build on those three bytes.

**Teardown**, identical after read and after write:

```
CloseFile        07F0 0044 9F 00 D761 92 0006 92 000E F2 00FF 0A
ReleaseFileEntry 07F0 0044 A0 00 D761 92 0003 92 000F F2 00FF 0E
session end      0782 0044 0002 80000000
final            07C0 0002 0044 0000
```

---

## 9. VERIFIED - how an error is signalled

The one deliberate failure, opening `NO-SUCH-FILE:OUT` on D102, from `capture-open-error.txt`:

```
request  07F0 0045 81 00 D761 92 0005 92 0002 F2 0002 B0 12 4E4F2D535543482D46494C453A4F5554 2755 F2 00FF
reply    07F0 0002 81 00 90BB 92 0005 92 0002 F2 0001 A2 002E F2 00FF A4
```

`0x002E` = **46**, and SINTRAN error 46 is `NO SUCH FILE NAME` - which is exactly what the terminal
printed. That is an independent confirmation from a source that is not the capture.

**The rule, VERIFIED across every reply in all four captures:**

 - A reply that carries **selector 1** carries a SINTRAN error code and nothing else.
 - A successful reply never carries selector 1; its results start at selector 2.

Compare the two open replies side by side, which is what makes the rule readable:

```
success  ... 92 0005 92 0002 F2 0002 A2 0040 F2 0003 A4 000045F1 F2 00FF
failure  ... 92 0005 92 0002 F2 0001 A2 002E                     F2 00FF
```

After the failure D100 did NOT send `CloseFile` - it went straight to `ReleaseFileEntry` and shut
the conversation down. A server must not expect a close for a file that never opened.

**UNKNOWN**: whether a `ReadFile` or `WriteFile` can itself fail this way. No read or write error
was provoked, so this rule is verified on `OpenFile` only. It is the obvious generalisation, but it
is a generalisation.

---

## 10. The field tags, as far as the captures prove them

| Tag | Shape | Status |
| --- | --- | --- |
| `92` | two-byte integer | VERIFIED, was already known |
| `F2` | two-byte integer used as the field SELECTOR; `F2 00FF` terminates | VERIFIED, already known |
| `A2` | two-byte integer value | VERIFIED - block size, byte count, file number, error code |
| `A4` | FOUR-byte integer value | VERIFIED - read/write position, file byte size |
| `8C` | composite, length in the following byte | INFERRED |
| `B0` | character string, length in the following byte | INFERRED |
| `B1`-`BF` | character string, length in the low nibble of the tag | INFERRED |

`A4` is the new one and it matters: the read and write POSITION is 32-bit, so a file server must
not carry it in a 16-bit variable.

The `B0` family, from three samples that agree:

```
B0 10  "PATCH-FILE:OUT'T"     0x10 = 16 characters
B0 12  "NO-SUCH-FILE:OUT'U"   0x12 = 18 characters
BF     "\"WRTEST1:OUT\"'W"    0xBF - 0xB0 = 15 characters, no length byte
```

Three samples is not a proof. INFERRED.

Note the write's name keeps its quotes: the client sent `"WRTEST1:OUT"` with the quote characters
in the string, because quoting is what asks SINTRAN to create the file. The quoting rule is not
just terminal syntax - it travels on the wire.

### The filler byte

Most FA bodies carry one byte past the `F2 00FF` terminator. Counted over the read and write
captures: **78 of 86 bodies have a trailing byte, 8 end exactly at the terminator.** The values
seen are `00 0A 0C 0E 4C 55 8C A2 A4 BF FF`, with no pattern, and none is ever read back.
**VERIFIED as present, varying, and sometimes absent; INFERRED to be uninitialised buffer tail** -
that it is sometimes missing altogether is what makes a meaning unlikely. A receiver must ignore
anything after
`F2 00FF`, and a sender should not try to reproduce it.

---

## 11. What is still unknown

 1. The default block size. Deduced as 512 bytes from a step of 4 per 2048; never stated on the
    wire.
 2. Why the exchange sequence sometimes has bit 15 set - `920005`, `928006`, `920007`, `928008`.
    It alternates cleanly but nothing in the capture explains it.
 3. The `05 00 <token>` envelope of a data message, and why the token alternates between the
    conversation token and `0001` within a pair.
 4. The three bytes `8C 80 05` before the end-of-file size in the `SiiiSpecial 0x003B` message.
 5. Whether `ReadFile` or `WriteFile` returns an error the same way `OpenFile` does. Not provoked.
 6. Whether the 14-byte sub-header holds for non-FA sub-protocols. Only FA traffic was measured.
 7. What a read past the end of a file does. Never reached - the caller always stopped on the size
    it learned at open.
 8. `SiiiSpecial 0x0021`, the file-information call. Its reply plainly contains the name, the block
    count and the byte size, but the rest of its 88-byte reply was not decoded.

---

## 12. What this means for the C# code

**Status updated 2026-08-04 (later the same day): items 1 and 4 are DONE.** See
[FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md](FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md)
for the session-layer rules found while applying them - the ShortAck/reply exchange model, the
XENSE sequencing reject, the directory-walk cursor, and the `0x078x` teardown.

 1. **DONE.** `SintranHeader.Size` is now **14**, byte 12-13 is the checksum, `ProtocolId` and
    `Counter` are `[Obsolete]` derived views. `XmsgSubHeader` starts at 14, is 14 bytes, and its
    last word is `Xmcsm` - which is the **body length**, confirmed on every Data frame in all four
    captures. The body starts at absolute 28. The old "32-bit XMCSM" was
    `(XMCSM << 16) | firstBodyWord`, an artefact of reading across the boundary.
 2. `FaOperation.ReadFile` and `FaOperation.WriteFile` can have their "UNKNOWN" remarks replaced by
    sections 4, 5 and 7.
 3. `FaTransferCodec` must NOT be reused for these. Its 1030-byte page/displacement message is a
    different service. A new codec for the 1032-byte envelope-plus-1024 message is needed, and it
    can share `FirstFragmentBodyLength = 594`, now confirmed a third time.
 4. `FaOperation.SiiiSpecial` should record that sub-function `0x003B` sets the end-of-file byte
    length after a write.
 5. A reply parser should read selector 1 as a SINTRAN error code and treat its presence as
    failure.
