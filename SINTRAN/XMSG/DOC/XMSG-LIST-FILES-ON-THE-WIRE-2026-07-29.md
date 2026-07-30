# LIST-FILES on the wire, and two corrections to the QFORM framing

**Date**: 2026-07-29
**Capture**: `E:\Dev\Ronny\X25Emulator\pcap\fa-access-secret-102-to-100-2026-07-29.pcapng`
**Direction**: node 102 (TCP 45164) to node 100 (TCP 10362), HDLC LAPB over TCP
**Status**: two framing facts corrected and VERIFIED; the directory-entry record is PARTIALLY decoded

---

## 1. Summary

This capture already contained a successful remote directory listing. Decoding it
produced two corrections to how we frame a QFORM body, both confirmed by measurement,
and a first look at the record a file server returns per directory entry.

It also showed a second service name on the wire, `*FA-USER`, which had not been
recorded before.

---

## 2. CORRECTION: the body starts 38 bytes into the LAPB frame, not 34

Every earlier walk of this capture used an offset of 34. That is wrong by 4 bytes.

Worked example, frame 27 (server to client), bytes after the opening `0x7E` flag:

```
off 0  09              LAPB address
off 1  AC              LAPB control
off 2  21 13           SINTRAN protocol id
off 4  00 0E           frame type - 0x0E marks a data frame
off 6  00 66 00 64     ... envelope fields ...
...
off 30 07 F0 00 02 81 00 90 81
off 38 92 00 0C        <- FIRST QFORM TAG
```

At offset 38 the body opens `92 000C` (a class-1 two-byte integer) and every
subsequent field closes cleanly on the `F2 00FF` end-of-list selector. At offset 34
the walk starts mid-field and desynchronises.

The same offset holds for client-to-server frames (frame 23 also opens `92 000C` at 38).

**This supersedes the `80 00 00 01` "unresolved body opener" noted in
`XMSG-FA-ACCESS-PASSWORD-ON-THE-WIRE-2026-07-29.md` section 6.3.** That opener was an
artefact of starting four bytes early. The bytes at offsets 30-37 are header, not body.

---

## 3. CORRECTION: a length byte of 0x80 is an escape MARKER

`QformReader` previously threw on a length byte of `0x80`, on the grounds that the
accumulation arithmetic at `ram:0x7d48` was unresolved and no captured frame exercised
it. Both halves of that statement are now obsolete: this capture exercises it, and the
rule is simply that the real length is the byte that follows.

Frame 23 carries a constructed value tagged `8C 80 46`. Read as an escape marker, the
declared length is `0x46` = 70. The contents account for exactly 70 bytes:

| Field           | Bytes            |
| --------------- | ---------------- |
| `B0 3E` + string | 2 + 62 = 64      |
| `A2 0000`        | 1 + 2 = 3        |
| `A2 FFFF`        | 1 + 2 = 3        |
| **total**        | **70**           |

This is an arithmetic confirmation, not merely a clean parse: the three inner fields
close exactly on the declared boundary. Any other reading of `0x80` would leave the
constructed value over- or under-run.

### Measured effect

Same capture, same 34 qualifying data frames, one variable changed at a time:

| Configuration                          | Clean walks |
| -------------------------------------- | ----------- |
| offset 34, `0x80` refused (the old code) | 19 / 34    |
| offset 38, `0x80` refused                | 28 / 34    |
| offset 38, `0x80` as escape marker       | **32 / 34** |

Both changes are now in
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Qform\QformReader.cs`, with
tests in `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\QformReaderTests.cs`.

Two frames still fail and the cause is not established. Do not tune anything further
against them until it is.

---

## 4. The directory listing itself

### 4.1 The request (frame 23, client to server)

```
92 000C                       (meaning not established)
92 0002                       (meaning not established)
F2 0001  92 0078              field 1 = 120
F2 0002  8C 80 46             field 2, constructed, 70 bytes:
           B0 3E  "(SECRET)'..." padded to 62
           A2 0000
           A2 FFFF
F2 00FF                       end of list
```

### 4.2 The replies - one frame per directory entry

Frame 27, frame 46 and frame 62 each carry ONE entry. The second integer steps
`0002`, `0003`, `0004` across them, so it behaves as an entry sequence number.
**That reading is INFERRED from three consecutive values, not confirmed.**

Frame 27:

```
92 000C
92 0002                       entry sequence
F2 0002  8C 4B                field 2, constructed, 75 bytes:
           A2 0007
           A2 0000
           A2 0001
           B0 40              a 64-byte entry record (see below)
F2 00FF
```

Frame 46 has the same shape with a 47-byte constructed value holding a 42-byte record,
so **the entry record is not a fixed-size struct**.

### 4.3 Inside the entry record

Frame 27's 64-byte record, split at the readable boundaries:

```
90 00
"TXT1" 27                     name, terminated by 0x27
00 00 ... 00                  padding
"SYMB" 07 00 07 00 03 FF 00 10 00 00 00 07 07 00 00 00 00 02
C1 FA E1 F0                   \
C1 FA E2 05                    > three 32-bit values, two of them equal
C1 FA E2 05                   /
00 00 00 01
00 00 00 0C
00 00 78 DA
```

Frame 46's record, same treatment:

```
D0 01 02 40 00 00 05 40 00 00
"PACK-ONE" 27                 name, terminated by 0x27
00 00 ... 00
40 00 48 FC  40 00 48 FE  00 00 48 24  00 00 3F D2
```

**VERIFIED**: the byte `0x27` terminates a name. It follows `TXT1`, `PACK-ONE` and
`(SECRET)` consistently, and `'` is the SINTRAN string terminator.

**VERIFIED**: `TXT1` and `SYMB` correspond to a file name and a file type, matching
the SINTRAN `name:type` convention.

### CONFIRMED against FILE-STATISTICS, 2026-07-29

`FILE-STAT d100(system).sintran:data` run from node 102 printed:

```
FILE 0 : D100.(PACK-ONE:SYSTEM)SINTRAN:DATA;1
           (ALLOCATED FILE)
           PUBLIC ACCESS : NONE
           FRIEND ACCESS : NONE
           OWN ACCESS : READ, WRITE, APPEND
           OPENED 10 TIMES
           CREATED 16.55.35  JULY 6, 1998
           OPENED FOR READ 16.59.48  JULY 6, 1998
           OPENED FOR WRITE 16.59.48  JULY 6, 1998
           63 PAGES , 122880 BYTES IN FILE
```

Against the same file's record taken off the wire in a `LIST-FILES` reply:

| Offset | Decoded | FILE-STATISTICS says | |
| --- | --- | --- | --- |
| +26 | `0007` | OWN: READ, WRITE, APPEND; PUBLIC and FRIEND none | match |
| +28 | `0020` | (ALLOCATED FILE) | match |
| +38 | `000A` = 10 | OPENED 10 TIMES | match |
| +40 | 1998-07-06 16:55:35 | CREATED 16.55.35 JULY 6, 1998 | match |
| +44 | 1998-07-06 16:59:48 | OPENED FOR READ 16.59.48 | match |
| +48 | 1998-07-06 16:59:48 | OPENED FOR WRITE 16.59.48 | match |
| +52 | 63 | 63 PAGES | match |
| +56 | 122879, +1 = 122880 | 122880 BYTES IN FILE | match |

**Every field matches.** The page count, byte count, the three dates and the open count are no longer
inferred from internal consistency - they are confirmed against the system's own report, and the
`+56 = bytes - 1` convention is confirmed exactly. Locked in by
`Xmsg.Ndfs.Tests.FaDirectoryListingTests.DecodedEntry_MatchesWhatFileStatisticsPrinted`.

**NOT ESTABLISHED**: the remainder. The name does not sit at a fixed offset - it is
at +2 in frame 27 and at +10 in frame 46 - so the leading bytes are some kind of field
descriptor rather than a header of fixed width. The three equal-looking 32-bit values
in frame 27 are *consistent with* created / last-opened / last-modified timestamps, and
`0x78DA` is *consistent with* a byte count, but nothing in this capture tests either
reading. **Do not build a decoder on those guesses.**

---

## 5. A second service: `*FA-USER`

Frames 98 and 106 (client to server) name `*FA-USER`, distinct from the `*FA-SERVER`
named in frames 1, 130, 156, 190 and 220. The corresponding server frames 92 and 100
carry the literal `USER` followed by a long zero-filled region, and use a different
sub-header (`86 84`) from the file-access frames (`82 84`).

This service had not previously been recorded. Its purpose is **not established**.

Note that this does **not** by itself imply a remote user-listing capability - see
section 6.

---

## 6. LIST-USERS: there is no remote form

Checked against the manuals:

- `Reference-Manuals\ND-60.128.5 EN SINTRAN III Reference Manual.md` line 6328 gives
  `@LIST-USERS [<directory name>:]<user name>,<output file>`. The only qualifier is a
  local *directory*, never a system or node.
- `Operations\Cosmos\ND-60.163.4 EN COSMOS User Guide.md` appendix G, lines 3593-3632,
  is the authoritative list of SINTRAN commands that work on remote files. It runs from
  `@ALLOCATE-FILE` to `@WFILE` and includes `@LIST-FILES`. **`LIST-USERS` is absent**,
  as are `LIST-FRIENDS` and `USER-STATISTICS`.
- The only remote `LIST-` command in the whole command set is `@LIST-REMOTE-QUEUE`
  (same manual, line 5932), which lists a batch queue, not users.

You can list *another user's files* remotely - `ND-60.163.4` line 1108 shows
`MINOR(TOM(XYZ)).(JERRY)REPORTS:TEXT` - but you must already know the user name.

**Conclusion: there is no command to enumerate users on a remote machine.** The reason
the correct command could not be found is that it does not exist.

---

## 6a. LIST-SYSTEMS, LIST-ROUTING and LIST-NAMES are three different things

All three were asked about together. They are distinct, and the captures show it.

### LIST-SYSTEMS - probes `*TADADM` on each system

From `E:\Dev\Ronny\X25Emulator\pcap\li-syst-tad-103.pcapng`, frames 529 and 541, decoded:

```
41 00 14                      service 0x41 = XSLET, length 20
FF 07 "*TADADM" 00            string parameter 1, padded to a word
FE 04 "D100"                  string parameter 2 - the system being probed
04 02 0001                    integer parameter 4 = 1
```

Frame 541 is byte-identical except `"D102"`. So LIST-SYSTEMS walks the systems it knows about and
sends each one an opening letter addressed to that system's `*TADADM`. It is a **per-system liveness
and identity probe**, carried out one system at a time over a real server conversation.

This also confirms the general XSLET parameter tagging against a second, independent server: string
parameter *n* is tagged `256 - n` (`FF` = 1, `FE` = 2), integer parameter *n* is tagged *n*, and
strings are padded to a word boundary. That rule had only been seen on `*FA-SERVER` and `*XFTRA`.

### LIST-ROUTING - the XROUT routing table

`li-rout-103-tree.pcapng` and `li-rout-102-tree.pcapng` carry **no server names at all** - no
`*TADADM`, no `*FA-*`, no `*XFTRA`. The traffic is short XROUT frames. This is the routing layer
answering which link reaches which system; it never opens a conversation with a named server.

### LIST-NAMES - the name and port directory

A `List-names` inside the FILE-TRANSFER program returns, per system, the registered server names and
their port numbers. Run live on 2026-07-29 against system 100:

| System | Port | Free SPs | Name |
| ---: | ---: | ---: | --- |
| 100 | 0 | | `D100.` |
| 100 | 2 | | `*COSPO.` |
| 100 | 3 | 2 | `*FA-FSA.` |
| 100 | 4 | | `*TADADM.` |
| 100 | 6 | | `*XM-FIDO.` |
| 100 | 7 | 1 | `*XFTRA.` |
| 100 | 11 | 30 | `*FA-SERVER.` |
| 102 | 0 | | `D102.` |
| 103 | 0 | | `D103.` |

This is the port map a server implementation has to register into. Note `*FA-USER` is **absent**,
consistent with it being the client-side File User registering on the requesting machine rather than
a service on 100.

**Summary**: LIST-NAMES asks *what services exist and on which ports*; LIST-SYSTEMS asks *is this
system there*, by talking to its `*TADADM`; LIST-ROUTING asks *how do I reach that system*.

---

## 6b. The FILE-TRANSFER program's advanced mode, and its MON 200 debug trace

`SET-ADVANCED-MODE` at the `F-T:` prompt unlocks a second command set. It matters because two of
those commands make the original software describe its own protocol:

```
Mode <File name>,<No of times>
List-variables
Checkout <Remote system and user name>,<No of page transfers>
Decode-buffer <Input buffer (y/n)>
Get-error-message <Error value (dec)>
Debugprint-on
Debugprint-off
Define-transfer-conditions <No of buffers>,<Size in bytes>,<Secure messages>
```

### DEFINE-TRANSFER-CONDITIONS settles what p10 and p11 are

The `*XFTRA` opening letter carries integer parameters 10 = 1024 and 11 = 2, and both had been
recorded as constants of unknown meaning - a controlled-variation run on 2026-07-28 failed to move
either. The existence of a command whose only three parameters are **number of buffers, size in
bytes, secure messages**, together with the COSMOS User Guide p.146 statement that "files are
transferred using two 1024-byte buffers at a time", makes p10 the size and p11 the count.

**Still not proven.** No capture yet shows the letter changing when the setting changes. Driving
`Define-transfer-conditions` with different values and re-capturing would prove it. The third knob,
"secure messages", has not been located in the letter at all.

### DEBUGPRINT-ON traces the MON 200 calls

With debug printing on, each XMSG call is printed with its registers before and after. From a
`Checkout d102(system),2` on node 100, 2026-07-29 (identical across three consecutive runs):

```
* XMSG Function:     2. Regs (A,D,X):      200      47   54457
   Return status=     1                  161605      47   54457
* XMSG Function:    12. Regs (A,D,X):        0      47       0
   Return status=     1                       5      47       0
*** Error in accessing: DUMMY
Sintran file system error:
NO SUCH FILE NAME
* XMSG Function:     1. Regs (A,D,X):        5      47      34
   Return status=     1                       5      47      34
```

**The numbers are OCTAL.** The A register is 16 bits, and 161605 decimal does not fit; as octal it
is 58245, which does.

The function numbers decode against `Xmsg.Protocol.Enums.XmsgFunction`:

| Printed | Function | Meaning |
| ---: | --- | --- |
| 2 | `XFGET` | get a message buffer |
| 12 | `XFSND` | send |
| 1 | `XFDCT` | disconnect / release |

So the shape is **get buffer, send, release**. `D` is 47B = 39 on every call including the returns,
so it behaves as a message or port handle held across the sequence. The value `XFGET` returns in A,
161605B, has the shape of a message identifier.

**`XFSND` returned success and NOTHING crossed the wire** - the capture over both HDLC links for the
same period contains zero packets.

> **CORRECTION.** This document first explained that as "the send was a local XROUT lookup, because
> XROUT on node 100 already knows D102 from its routing table". **That explanation is wrong.**
> A later capture on the same day shows a remote server lookup DOES cross the wire: a
> `CREATE-FILE d102(system).dummy:data,2` on node 100 produced 8 frames carrying `*FA-SERVER` and
> `D102` between 100 and 102, ending in "REMOTE FILE SERVER IS NOT AVAILABLE". Resolving a named
> server on a REMOTE system is a network round trip.
>
> What the `Checkout` trace actually shows is only that **whatever that `XFSND` was, it was not a
> remote server lookup** - the local `DUMMY` open failed before any remote resolution was attempted.
> Its destination is **NOT established**.

It also corrects an assumption: `Checkout` does **not** open its network conversation before touching
the file. It does a local lookup, opens the file, and only then would the conversation follow. The
local file open is the gate - which is the same trap that made four of five TRANSFER-FILE attempts
look like network failures on 2026-07-28.

**`Checkout` needs a scratch file called `DUMMY`.** Creating `DUMMY:DATA` (2 pages) under
`(PACK-ONE:SYSTEM)` on node 100 did **not** satisfy it - the same error recurred. Where the file must
live, and with which type, is **NOT established**. A remote `DUMMY` on d102 has not been tried.

### CORRECTION to PLAN-FILE-SERVER-CAPTURE-2026-07-28.md

That document states that on E02 the commands `DEBUGPRINT-ON`, `DEBUGPRINT-OFF`, `DECODE-BUFFER`,
`CHECKOUT`, `LIST-VARIABLES` and `DEFINE-TRANSFER-CONDITIONS` all answer `** Illegal command **`, and
concludes: "So there is **no built-in buffer dump to lean on** - scratch the plan above that hoped for
one. The MON 200 trace is the instrument."

**That conclusion is wrong.** All of those commands exist on E02. They are gated behind
`SET-ADVANCED-MODE`, which promotes the prompt from `F-T:` to `F-T(Adv.):`. Every one of them was run
successfully on 2026-07-29 against `(COSMOS-BASIC)COS-FILE-TRA-E02:PROG` on node 100.

The practical cost of the error was real: it closed off the program's own decoder and variable dump
as instruments for a day.

### LIST-VARIABLES - the program's own state

```
Name buffer contents:

Input buffer address: 54216
Output buffer address: 54316
Page buffer address: 216
Function (oct): 0, page no: 0, displacement: 0
Letter status (oct): 0, remote file system status (oct): 0
Number of transfer buffers: 2 of size: 1024 bytes.
```

**This CONFIRMS p10 and p11.** The `*XFTRA` letter carries integer parameter 10 = 1024 and parameter
11 = 2; the program reports "2 buffers of size 1024 bytes". Parameter 10 is the buffer **size** and
parameter 11 the **count**. This was previously recorded as inferred - the controlled-variation run
on 2026-07-28 never moved either field - and can now be treated as established, although a capture
showing the letter change when `DEFINE-TRANSFER-CONDITIONS` changes would still be stronger.

`Function`, `page no` and `displacement` are the **bulk-transfer request fields**, held as live
program state. This is the first direct evidence of the structure that
`COSMOS-XMSG-Synthesis.md` section 8 predicted from disassembly ("the request/reply carry position +
count as typed params"). It also shows the transfer is addressed by **page number plus displacement**,
not by a byte offset alone.

Two separate status words are tracked - `Letter status` and `remote file system status` - which
matches the wire behaviour where a refusal replaces the service byte (letter level) while a file
error arrives as a tagged value in the body (file-system level).

### GET-ERROR-MESSAGE 48

```
Error code not recognised by XMSG error routine
```

Useful negative evidence. The value 48 decoded off the wire in a rejected file access is **not** an
XMSG error code, which is consistent with reading it as the SINTRAN file-system error 060 octal,
"Wrong password". It does not by itself prove that reading.

---

## 6c. A COMPLETE file transfer, captured

**Capture**: `E:\Dev\Ronny\X25Emulator\pcap\claude-transfer-file-COMPLETE-102-to-100-2026-07-29.pcapng`

This is the first capture anywhere of COSMOS file **content** crossing the wire. Every earlier
attempt caught only the opening letter.

Command, run on node 102 inside `COS-FILE-TRA-E02` with `DEBUGPRINT-ON`:

```
TRANSFER-FILE d100(system)."xfertest:data",DUMMY:DATA
```

Result: `Completed. Transfer rate: 3 Kbytes/sec`, and `XFERTEST:DATA` created on node 100.

### The destination must be QUOTED to be created

The identical command with an unquoted destination fails with:

```
*** Error in accessing: D100(SYSTEM).XFERTEST:DATA
Sintran file system error: NO SUCH FILE NAME
```

`TRANSFER-FILE` does **not** create the destination implicitly. The quotes are what the remote reads
as "create this file". This confirms, from the client side, the note already recorded against the
captured `*XFTRA` letter - that the quotes around `"XMSG-COPY:BATC"` are content the client must not
strip.

Note the remote spec needs a **dot** between the system/user part and the file name:
`d100(system).name:type`. Without it SINTRAN answers `ILLEGAL CHARACTER IN PARAMETER`.

### Wire measurements

| Measure | Value |
| --- | ---: |
| Payload frames | 45 |
| Frames of exactly 256 bytes | 16 |
| Total payload bytes 102 -> 100 | 4601 |
| Total payload bytes 100 -> 102 | 504 |

> **CORRECTION - the 256-byte figure is NOT the LAPB frame size.**
> This section first concluded "the on-the-wire unit is a 256-byte LAPB I-frame". **Wrong.** 256 is
> the TCP segment size of the bridge carrying HDLC over TCP. Checking which payloads actually begin
> with the `0x7E` flag shows frames 15, 17 and 19 do not - they are TCP continuations of the single
> LAPB frame that started at frame 13.
>
> **SECOND CORRECTION - it is not 1024 either.** The estimate "roughly 1024 bytes of payload,
> matching the 1024-byte buffer" was arithmetic on TCP segments again, not on reassembled frames.
>
> Proper reassembly through `HdlcPcap.ReadFrames` (see
> `Xmsg.Protocol.Tests/FileTransferStreamTests.cs`) gives the real figures:
>
> | Direction | Information frames | Info bytes |
> | --- | ---: | ---: |
> | 102 -> 100 | 16 | 4496 |
> | 100 -> 102 | 12 | 352 |
>
> Information-field sizes are **450 and 622 bytes, four of each**, plus smaller control-carrying
> frames - **not** a uniform 1024, and **not** 256. Total LAPB frames: 39.
>
> **The relationship between the 1024-byte application buffer and the 450/622-byte frames is NOT
> established.** Note only that 4 x 450 + 4 x 622 = 4288, against a 2-page (4096-byte) file, leaving
> 192 bytes across 8 frames - consistent with a per-frame header of about 24 bytes, but that is
> arithmetic, not a decode. Do not build on it.

### Acknowledgement: LAPB sliding window at the link layer

The LAPB control bytes are decisive. Data frames carry I-frame control values (`0xAA`, `0xEE`,
`0x26`, `0x6C` - bit 0 clear), and the reverse direction carries RR supervisory frames (`0xC1`,
`0x81`, `0xA1`, `0x01`, `0x21`, `0x61` - bits 1:0 = 01), each with a receive sequence number N(R).

Acknowledgements arrive **interleaved with** data rather than gating it: data at t=1.97, an RR at
t=2.02, more data at t=2.03. So the sender does not halt for each acknowledgement.

**ESTABLISHED - there IS application-level reply traffic, not only link acknowledgement.**
Node 100 sends **12 information frames totalling 352 bytes** back, alongside 10 supervisory RR
frames. Had the reverse direction been acknowledgement only, it would carry supervisory frames and
nothing else. So the transfer is a request/reply exchange at the XMSG layer, riding on LAPB's
acknowledged link - which matches the `XFRCV` after each buffer in the MON 200 trace.

**NOT established - and one measurement is known bad:**

### The transfer message, DECODED

Dumped by `FileTransferStreamTests.TransferStream_DumpInformationFieldHeads`.

Every information field opens with the SINTRAN header
`2113 <type> <src system> <dst system> <counter> <length> <checksum>`, with the systems appearing as
`0064` (100) and `0066` (102). Data frames then carry an XMSG sub-header, and the body follows.

Across the whole transfer the bodies are:

```
0406  0042 0000 0000   + 1024 bytes    page 0, displacement 0
0406  0042 0000 0200   + 1024 bytes    page 0, displacement 512
0406  0042 0001 0000   + 1024 bytes    page 1, displacement 0
0406  0042 0001 0200   + 1024 bytes    page 1, displacement 512
0006  0043 ffff ffff                   end of transfer
```

**The arithmetic closes exactly.** The declared length `0x0406` is 1030, and 1030 - 6 = **1024** - the
six header bytes are the function, the page number and the displacement, and the remainder is one
full buffer of file data. That 1024 is the same number `LIST-VARIABLES` reports as the buffer size
and the same number carried as integer parameter 10 of the `*XFTRA` letter.

| Field | Width | Observed |
| --- | --- | --- |
| function | 2 bytes | `0042` for a data block, `0043` for end of transfer |
| page number | 2 bytes | 0, then 1 - the file is 2 pages |
| displacement | 2 bytes | 0 and `0200` |
| data | 1024 bytes | the file contents |

**Displacement is counted in WORDS, not bytes.** `0x200` is 512, and 512 words is 1024 bytes - exactly
one buffer, so the two blocks of a page are at displacement 0 and 512. A SINTRAN page is 1KW = 2048
bytes, so each message carries half a page. Two pages x two halves = the four data messages seen.

The end marker `0043` carries `ffff ffff` and a declared length of 6, so it has a header and no data.

**This also disposes of the 450/622-byte question.** Those are not protocol blocks: LAPB fragments the
single 1030-byte message across two frames because its own frame limit is about 600 bytes. Nothing at
the XMSG layer is 450 or 622 bytes long.

The reverse direction carries a 34-byte frame per block, echoing the length `0406` followed by zeros,
and a final one carrying `0006 0000 0000 0001`. These are the application-level replies established
earlier - one per data block.

Two incidental confirmations from the same dump: the opening letter is visible as
`0141 003a ff06 2a58 4654 5241 fe04 4431 3030` - XSLET, `*XFTRA`, `D100` - and node 100's first reply
carries the string `XFERTEST'`, the name of the file it was asked to create.

**Caveat, now resolved.** The first transfer (`DUMMY:DATA`) was a freshly created file and therefore
all zeros, so nothing proved the data region was a plain byte copy. A second capture settles it.

### A content-bearing PULL

**Capture**: `E:\Dev\Ronny\X25Emulator\pcap\claude-transfer-PULL-content-100-to-102-2026-07-29.pcapng`

```
TRANSFER-FILE "ftpull:symb",d100(system).filsys-symbols:symb
  -> 21 pages, page index 0..20, Completed. Transfer rate: 4 Kbytes/sec
```

**The data region is a plain byte copy.** The payloads contain readable text - `BANK`, `SN`, octal
digit runs - which is exactly what the `:SYMB` symbol-table file holds. Nothing is compressed,
encoded or reordered.

### PULL and PUSH letters DIFFER - a correction

`XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md` recorded, from push captures, that **the source file
is not in the request at all**. That holds for a push, but is not a general rule. The pull letter is:

```
0141 003E  FF06 "*XFTRA"  FE04 "D100"  F406 "SYSTEM"  0D02 0000
           F813 "FILSYS-SYMBOLS:SYMB"
```

Parameter 8 carries the **remote** file name in both directions. On a push that is the destination;
on a pull it is the source. So parameter 8 is "the file on the other machine", and the local file
never appears in the letter either way. That is a cleaner rule than the one recorded before, and it
explains the earlier observation without contradicting it.

### The pull reply carries the source file's object entry

Node 100 answers with a record beginning `9000`, the name `FILSYS-SYMBOLS'`, the type `SYMB` - the
same 64-byte object entry structure returned by `LIST-FILES`. Two further field confirmations come
free:

- `+28` reads `0008` here against `0020` for `SINTRAN:DATA`, matching FILE-STATISTICS reporting
  "(INDEXED FILE)" for this file and "(ALLOCATED FILE)" for that one.
- `+38` reads `0003`, one more than the "OPENED 2 TIMES" that FILE-STATISTICS reported minutes
  earlier - the transfer itself being the third open.

### Acknowledgement cadence, measured

Measured by `FileTransferStreamTests.TransferStream_IsTheSendWindowActuallyUsed`.

An earlier attempt walked the capture counting frames sent but not yet acknowledged and reported 16
outstanding. **That was wrong and its numbers must not be quoted** - LAPB's modulo-8 space allows at
most 7. The cause: `HdlcPcap.ReadFrames` returns frames grouped by directional flow, not interleaved
in capture order, so no acknowledgement ever falls between two data frames.

Order *within* a flow is preserved, so the N(R) progression is reliable:

| Direction | N(R) sequence | Largest advance |
| --- | --- | ---: |
| 100 -> 102 (acknowledging the file data) | 4,4,5,6,7,7,0,1,2,2,3,4,5,5,6,7,0,0,1,2,2,3 | **1** |
| 102 -> 100 (acknowledging the replies) | 3,4,5,5,5,7,7,7,1,1,1,3,3,3,5,5,7 | **2** |

**The bulk data direction is acknowledged one frame at a time.** Every N(R) advance is exactly 1
across all 22 acknowledgements, with no exceptions. The reverse direction advances by 2 on five
occasions, so the sliding window is genuinely live on this link - it simply is not exercised for the
file data.

**What this does NOT prove.** Per-frame acknowledgement is a property of the RECEIVER. A sender with
two frames in flight could still be acknowledged individually, producing the same N(R) trace. To
establish in-flight depth the capture must be walked in true interleaved order, which
`HdlcPcap.ReadFrames` does not currently support - it would need frames tagged with the capture
ordinal of the segment they end in. So "the data direction is not pipelined" is **consistent with**
the evidence but **not established** by it.

This resolves the contradiction flagged earlier between `COSMOS-XMSG-Synthesis.md` section 8
(~0x800-byte page buffer) and `COS-FA-SERV-E04-Analysis.md` section 6.1 (1850 words = 3700 bytes):
**both describe application-side buffers, neither describes the wire unit.** Nothing on the wire
carries a 2048- or 3700-byte block.

The reverse direction carried 504 bytes in total against 4601 forward, so acknowledgements are small
and far fewer than one per data frame.

### The MON 200 call sequence of a real transfer

Per page, the pattern is:

```
10  XFOPN   A=<magic>          open
 7  XFWRI   A=216 then 1216    write - the two addresses are 1000 octal apart, one buffer
 5          A=102, X=0 / 1000  displacement 0 then 1000 octal = 512 words = 1024 bytes
1014        (XFSND + options)  send
```

then the reply side:

```
40015       (XFRCV + wait)     status 0 = nothing yet, status 1 = message present
 4          A=177777           consume
```

`Current page index:` printed `0` then `1`, matching two pages.

**Functions 3, 4, 5 and 11 are NOT identified** against `XmsgFunction`. Function 5 carries the
displacement and function 4 always takes A=177777 (-1), but neither is confirmed. The composite
values `1014`, `23014` and `40015` are octal and clearly a function number OR'd with option bits -
`40015` and `23014` both end in a low nibble matching XFRCV and XFSND respectively - but the exact
split has **not** been verified against `XmsgOption`.

---

## 6d. FILE-STATISTICS on the wire, and the `8000 0001` opener SOLVED

**Capture**: `E:\Dev\Ronny\X25Emulator\pcap\claude-file-stat-102-to-100-2026-07-29.pcapng`
(`FILE-STAT d100(system).sintran:data` from node 102).

The session opens with the documented FA-SERVER letter, byte for byte as previously recorded:

```
1B 41 0012  FF 0A "*FA-SERVER"  FE 04 "D1.."
```

### A 4-byte session header sits in front of every QFORM body

Earlier documents flagged an unexplained `80 00 00 01` at the start of a body and listed it under
"still open"; a later note in this file dismissed it as an artifact of reading from the wrong offset.
**Both were wrong.** It is real, and this capture shows what it is, because six requests occur in one
session and the field varies systematically:

```
07F0 0046   8000 0001   92 000C ...    first request
07F0 0046   8100 D761   92 000C ...    second
07F0 0046   8200 D761   92 000C ...    third
07F0 0046   8300 D761   92 000C ...    fourth
07F0 0046   8400 D761   92 000C ...    fifth
07F0 0046   8500 D761   92 000C ...    sixth
```

The four bytes before the QFORM body are a **sequence byte `0x80 + n`, a zero byte, and a 16-bit
token**. The token is `0x0001` on the opening exchange and a constant `0xD761` for the remainder of
the session, which is the shape of a value handed out by the server on first contact and echoed
thereafter. **That reading of the token is INFERRED** - nothing here shows the server choosing it.

This also explains why a body offset of 38 works: it is 34 bytes of SINTRAN and XMSG headers plus
these 4. The QFORM body proper begins after them, which is why every body appears to start `92 000C`.

The value `07F0` precedes a request carrying a QFORM body; `07A2` appears instead on short 36-byte
frames whose payload is `0100 8485`, `0200 8485`, `0300 8485` and so on - an incrementing counter
with a constant. Their role is **not established**.

Field 1 of the first request carries `A2 07D0` = 2000, matching what was recorded from the earlier
password capture.

---

## 6e. DELETE-FILE, and the message-type / conversation fields

**Capture**: `E:\Dev\Ronny\X25Emulator\pcap\claude-delete-file-102-to-100-2026-07-29.pcapng`
(`DELETE-FILE d100(system).xfertest:data` from node 102 - the file created by the earlier transfer).

It completes with no confirmation prompt and takes three QFORM exchanges:

```
07F0 0048   8000 0001   92 0002  92 0001  F2 0001  A2 07D0
07F0 0048   8100 D761   92 000B  92 0002  F2 0001  BF "XFERTEST:DATA.."
07F0 0048   8200 D761   92 0003  92 0003  F2 00FF
```

The file name arrives under selector 1 as a `BF` byte string - class 3, length 15 - and the third
exchange is a bare end-of-list. The first exchange is byte-identical to the first exchange of
FILE-STATISTICS, including `A2 07D0` = 2000, so the opening handshake is shared across operations.

### The word after the message tag is a conversation number, not an opcode

Comparing the three captured operations:

| Operation | Value |
| --- | --- |
| LIST-FILES | `003F` |
| FILE-STATISTICS | `0046` |
| DELETE-FILE | `0048` |

It increases in the order the captures were taken and never repeats, so it identifies the
conversation rather than the operation. This matches the earlier observation in
`XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md` that a trailer byte "behaves like a request counter
incrementing by 2, not a function code" - the same counter, seen in a different place.

**So the operation is NOT carried in this field.** What distinguishes DELETE-FILE from
FILE-STATISTICS on the wire is the sequence of QFORM exchanges and the selectors used, not an opcode
byte. That is consistent with `COSMOS-XMSG-Synthesis.md` section 8, which records that dispatch in
the server is decentralised with no opcode-to-handler table.

### Message-type tags

Four tags appear in front of that conversation number:

| Tag | Seen carrying |
| --- | --- |
| `07F0` | a request with a QFORM body |
| `07A2` | a short 36-byte frame, body `0100 8485`, `0200 8485`, `0300 8485` - a counter and a constant |
| `0782` | `0002 8000 0000` |
| `07C0` / `07D2` | the closing exchange; the reply carries `6400` = 100, the system number |

The `07A2` counter runs in step with the exchange number. Beyond that, **the meanings of these tags
are NOT established.**

---

## 6f. CORRECTION: a transfer message is SPLIT ACROSS TWO LAPB FRAMES

Section 6c described the transfer message as a single 1030-byte unit. On the wire it is not.
It is segmented into two LAPB information frames, and the segmentation is visible in the
SINTRAN header:

```
2113 000a 0064 0066 0178 0406 ...   len=622   first fragment
2113 000c 0064 0066 0178 0252 ...   len=450   continuation
```

**Two packet subtypes appear here that are not in `SintranPacketSubtype`**, which only knows
`0x0E` for data:

| Subtype | Role | Header before the body |
| --- | --- | --- |
| `0x0A` | first fragment of a segmented message | 28 bytes: SINTRAN header + full addressing |
| `0x0C` | continuation | **14 bytes**: SINTRAN header + the counter byte only |

A continuation does not repeat the addressing words - it carries the 13-byte SINTRAN header
plus the single counter byte and then picks the message straight back up. The two fragments are
paired by the Flags1 datagram sequence, which is identical on both.

### Flags2 on the two fragments

| Fragment | Flags2 | Meaning |
| --- | --- | --- |
| `0x0A` | `0406` = 1030 | total message length |
| `0x0C` | `0252` = 594 | byte offset at which the continuation resumes |

VERIFIED on all four messages in `claude-transfer-file-COMPLETE-102-to-100-2026-07-29.pcapng`:
594 + 436 = 1030, exactly, every time.

### CORRECTION: the message body starts at info-field offset 28, not 32

This is what made the first attempt at the arithmetic above miss by four bytes.

`XmsgSubHeader` documents a 19-byte sub-header whose offsets 13-16 are a four-byte XMCSM control
word, which would put the body at 13 + 19 = 32. Measured against the corpus, **only the HIGH half
of XMCSM is a header field - and it is simply Flags2 repeated.** That is why the long-standing rule
`Flags2 == XMCSM >> 16` has always held: they are the same field written twice.

The LOW half of XMCSM is already the first word of the message body. So:

```
info off 0   SINTRAN header                        13 bytes
info off 13  counter, 21 00 marker, flags, role,
             XMDSY / XMDPT / XMSSY / XMSPT         15 bytes
info off 28  message body starts here
```

(Add 2 for the frame-relative offsets used in section 2: body at frame offset 30, which is
exactly where section 2 shows `07 F0 00 02` beginning.)

This makes the message-type word (`07F0`, `07A2`, `0782`, `07C0`, `07D2`) and the transfer
function code (`0042`) **the same field** - the first word of every message body - rather than two
unrelated discoveries. Body layout, unified:

```
QFORM message:  <type 07F0> <conversation> <4-byte session header> <QFORM ...>
transfer:       <fn 0042>   <page>         <displacement in words> <1024 bytes>
```

6 + 1024 = 1030. The displacement advances 0 -> 512 and resets as the page advances, which is what
establishes that it counts **words** and that one page holds two 1024-byte blocks.

### For the QFORM classes, Flags2 is exactly the body length

VERIFIED across 98 data frames in three captures - FILE-STAT (28), DELETE-FILE (17) and
LIST-FILES (53), both directions - with zero mismatches:

```
Flags2 == (information field length) - 28
```

Two classes in the transfer capture do NOT follow it, and both are real message classes rather
than noise:

- **Class `0x0080`** - the two XSLET opening letters that name `*XFTRA` in plain ASCII. Here
  `0x0080` is a class code, not a length.
- **The transfer acknowledgements** - 34-byte frames whose body is six zero bytes, but which carry
  the acknowledged message's XMCSM verbatim, so Flags2 reads 1030.

So Flags2 is a frame-CLASS word, and "body length" is what that word means for the QFORM
request/reply class specifically.

### ANSWERED: the transfer is STOP-AND-WAIT at the application layer

Four data messages, four acknowledgements - but a 1:1 count says nothing about ordering, so this
was settled separately by walking both directions in the order the frames completed on the wire:

```
sent message 1 (outstanding now 1)
acknowledged 1 (outstanding now 0)
sent message 2 (outstanding now 1)
acknowledged 2 (outstanding now 0)
sent message 3 (outstanding now 1)
acknowledged 3 (outstanding now 0)
sent message 4 (outstanding now 1)
acknowledged 4 (outstanding now 0)

PEAK OUTSTANDING: 1
```

**Peak outstanding is 1.** The sender never starts a message before the previous one is
acknowledged - strict alternation, no pipelining. This is the answer to "is the transfer using a
sliding window ack, or waiting for ack": the LAPB layer underneath has a sliding window and uses it
(a 1030-byte message is two I-frames), but the file-transfer application above it waits for every
message.

That matters for a server implementation: it may answer one message at a time and never needs to
buffer a window of outstanding transfer blocks.

**How this was measured.** `HdlcPcap.ReadFrames` reassembles each TCP flow separately and returns
the flows one after another, so the two directions are never interleaved and no ordering question
can be answered from it. A new `HdlcPcap.ReadFramesInCaptureOrder` places each frame using the
capture ordinal of the TCP segment carrying its closing flag. A guard test asserts both readers
return the same 39 frames, so the ordering conclusion cannot be drawn from a stream that silently
lost or duplicated frames.

This is the third time flow-grouping has nearly produced a wrong published claim - it previously
yielded an impossible count of 16 outstanding LAPB frames. **Never answer an ordering question with
`ReadFrames`.**

### The complete transfer, message by message

Every body in the transfer, in wire order, dumped from the capture rather than reconstructed:

```
102->100  62  0141 003a ff06 *XFTRA fe04 D100 f406 SYSTEM 0d02 0000
              f80f "XFERTEST:DATA" 00 f704 SYMB 0a02 0400 0b02 0002
100->102  70  0100 0000 ff06 d000 XFERTEST' ... DATA <object entry>
102->100 1030 0042 0000 0000 <1024 bytes>      \
100->102    6 0000 0000 0000                    | four times, strictly
102->100 1030 0042 0000 0200 <1024 bytes>       | alternating
100->102    6 0000 0000 0000                   /
102->100    6 0043 ffff ffff                   end of transfer
100->102    6 0000 0000 0001                   completion
```

Points worth recording:

- **The opening letter is an XSLET letter** with the parameter tagging already known from
  `XSLET`: `FF` = string parameter 1 (`*XFTRA`, the server name), `FE` = parameter 2 (`D100`),
  `F4` = `SYSTEM`, `F8` = the quoted file name `XFERTEST:DATA`, `F7` = `SYMB`.
- **The opening reply carries a 64-byte object entry** for the file just created - the same
  structure the directory listing returns. So the transfer's opening exchange hands back the
  created file's directory entry.
- **The end-of-transfer reply is `0000 0000 0001`**, differing from the four data replies
  (`0000 0000 0000`) in the final word only. Whether that word is a status or a count is
  **UNKNOWN** - one capture cannot distinguish them.

The codec is `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Fa\FaTransferCodec.cs`, held to
these bytes by
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\FaTransferCodecTests.cs`: every message
it builds is byte-identical to the captured one, and the split point it computes matches the
segmentation the machine used.

**One caveat on the 594-byte split.** It is identical on all four messages, but only one transfer
has ever been captured. Whether 594 is fixed by the protocol or falls out of this link's frame size
is UNKNOWN. A transfer of a differently sized file would settle it.

Tests: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\TransferFragmentationTests.cs`
(6 tests). Library support added in
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Hdlc\HdlcPcap.cs` and
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Hdlc\HdlcDeframer.cs`.

---

## 6g. The operation code, and a CORRECTION to `FaListFilesCodec`

Dumping FILE-STATISTICS and DELETE-FILE side by side - in wire order, complete bodies rather than
48-byte heads - settled how an operation is identified.

**The opening request of the two operations is byte-identical apart from the conversation number.**
Asserted, not eyeballed: 112 bytes, zero differences outside offsets 2-3.

```
FILE-STAT    07F0 0046  8000 0001  92 0002 92 0001 ...
DELETE-FILE  07F0 0048  8000 0001  92 0002 92 0001 ...
                  ^^^^ conversation number only
```

So the operation is **not named in the opening exchange at all**, which confirms the earlier
conclusion that the word after the message type is a conversation counter rather than an opcode.

### Where the operation IS named

The first QFORM field of each exchange is an operation code, and the second is the exchange
sequence. Both are echoed by the reply, which is what matches a reply to its request.

| Exchange | DELETE-FILE | FILE-STATISTICS | Payload |
| --- | --- | --- | --- |
| 1 | `92 0002` | `92 0002` | pack and user - `BAK05  SYSTEM`, then a 56-byte block holding `SYSTEM'` |
| 2 | `92 000B` | `92 000C` | delete: the file name `XFERTEST:DATA`; stat: the enquiry block |
| 3 | `92 0003` | `92 0003` | close |

**`92 000B` in exchange 2 is what makes a delete a delete.** Codes so far:

| Code | Meaning |
| --- | --- |
| `0002` | open, carrying the directory and user spec |
| `000B` | delete |
| `000C` | directory or file enquiry (LIST-FILES, FILE-STATISTICS) |
| `0003` | close |

The session header also resolves cleanly: its first byte is `0x80 + n` counting exchanges from
zero, and the token is `0001` on the first exchange, then `D761` from the asker and `9081` from the
responder.

### CORRECTION to `FaListFilesCodec`

That codec declared:

```csharp
public const ushort LeadingConstant = 0x000C;   // "Meaning UNKNOWN"
```

**It is not a constant - it is the operation code**, and it only looked constant because every
frame examined at the time belonged to one operation. Renamed to `OperationDirectoryEnquiry` and
now sourced from `FaExchangeCodec`. This is the second time in this work that "constant on every
captured frame" has meant "only one case was captured"; the first was the 594-byte fragment split,
which is still flagged UNKNOWN for the same reason.

New code: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Fa\FaExchangeCodec.cs`, tested by
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\FaExchangeCodecTests.cs`. The body dump
used to derive it is
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\FaOperationDumpTests.cs`.

---

## 6h. Client and server drivers, verified by replay

Both halves of a file-server conversation are now driven by code, and both are held to the captures
by replay: the client driver rebuilds every request the real client sent, and the server driver
rebuilds every message node 100 answered with - byte for byte, in both cases.

| | `FaClientConversation` | `FaServerConversation` |
| --- | --- | --- |
| Conversation number | its own (003F, 0044, 0046, 0048 ...) | always `0002` |
| Session token | `0001` on exchange 1, then `D761` | `9081` on **every** exchange |
| Session counter | `0x80 + n` | `0x80 + n` |
| Short-ack constant | `8485` | `922A` |

**The token asymmetry is the trap.** A server written symmetric with the client would send `0001`
on its first reply; the capture says `9081` from the very first one. There is a test whose only job
is to pin that difference.

Conversation lengths differ by operation and are NOT a fixed three:

- DELETE-FILE: open (`0002`), delete (`000B`), close (`0003`) - 3 exchanges.
- FILE-STATISTICS: open, then the enquiry `000C` **four times**, then close - 6 exchanges.

The server also sends three messages that are not replies: the connection confirmation
`07D2 0002 <client conversation> 6400` - the one server message carrying the CLIENT's conversation
number, and the system number 100 - the short acknowledgements, and the close
`07C0 0002 <client conversation> 0000`, which both sides send identically.

### Structural resemblance to RR-LIB - a lead, NOT a finding

The conversation looks like the request-response model in `Xmsg.Api.Rr`: an opening letter carrying
user data that names the server, a confirmation, request/response pairs, then a disconnect. **No
document has been found stating that `*FA-SERVER` is an RR-LIB server.** The mapping is inferred
from shape alone and is recorded as a lead worth chasing, not as established fact.

Code:
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Fa\FaClientConversation.cs`,
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Fa\FaServerConversation.cs`.
Tests:
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\FaClientConversationTests.cs`,
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol.Tests\FaServerConversationTests.cs`.

---

## 7. What is still needed

To build a server that answers real clients, the following are not yet captured:

1. **`FILE-STATISTICS`** against a known file, so the entry record can be read against
   known values (size, dates) instead of guessed at.
2. **`CREATE-FILE`** with a stated page count, which should pin the create op and its
   parameters.
3. **A listing of a directory with many files of known sizes and dates**, which is what
   would actually settle the record layout in section 4.3.
4. **A bulk file transfer that completes.** No capture of file *content* on the wire
   exists at all - only the opening letter. The block size, position/count fields and
   the acknowledgement rule are known from disassembly only.
5. **`DELETE-FILE`**, and a read and a write against an open file.

---

## 6i. The spec block across two users, and a third "constant" that was not one (2026-07-30)

Two of the values the listing codec hard-codes were checked against a SECOND directory,
which needed no new capture - the two listings already on disk are against two different
users on node 100, `SYSTEM` and `SECRET`.

### What survived

| Value | First case | Second case | Verdict |
| --- | --- | --- | --- |
| Selector-1 value `0x0078` | SYSTEM, 11 requests | SECRET, 2 requests | **VERIFIED across two users** |
| Spec block length 62 | SYSTEM | SECRET | **VERIFIED across two users** |

Neither is a single-case artefact any longer. Note the one thing this pair cannot settle:
`SYSTEM` and `SECRET` are both six characters, so a length that happens to track the user
name length would look constant here too.

### What did NOT survive: "byte-identical from one entry to the next"

`FaListFilesCodec` documented the request body as byte-identical across a listing apart
from the serial and the cursor. Measured over all eleven directory enquiries in the SYSTEM
capture, that holds only from the `0xFFFF` request onward. The enquiry BEFORE it differs:

```
request 0   cursor 002d   ...4d29 2927 3704 b5dc 0000 0000 0000 48ff 0000 0000 ... 0000 0000
request 1   cursor ffff   ...4d29 2927 3704 b5dc 2704 0000 2705 48ff 0000 ffff ... 0540 b56b
requests 2-10  cursors 1..9, block byte-identical to request 1
```

Same leading filespec text, but a zero-filled tail where the walk requests carry values.
So part of the 62-byte block is state that exists only once the walk has started, not
something the caller supplies for the whole listing.

**Meaning UNKNOWN.** It is consistent with a position handle carried forward from the
previous reply - `0x2704`/`0x2705` look like terminated small values and `0xFFFF` matches
the cursor of the request it appears in - but nothing in either capture tests that reading.
The block stays opaque and must be replayed, never synthesised.

### The block's leading text, both users

```
SYSTEM   (SYSTEM)'EM).(SYSTEM)'         then 3704 b5dc ... 48ff ... zero fill
SECRET   (SECRET)'ET(SECRET)).(SECRET)' then 0000 ... 48ff ... zero fill
```

23 of the 62 bytes differ between the two users. `0x48FF` sits in the same relative
position in both. The text is truncated **on the left** - the SECRET block still shows the
password parentheses that were typed, the SYSTEM one does not - so this is a fixed-width
window over the typed specification, not a self-describing string. Where the window starts
is UNKNOWN.

### Cursor sequence, now asserted

`0xFFFF` asks for the first entry, then the cursor runs `1, 2, 3, ...` one per entry. The
SYSTEM walk is `ffff` then 1..9 for ten entries; the SECRET walk is `ffff` then 1 for two.

Locked in by `Xmsg.Protocol.Tests.FaSpecBlockCrossUserTests`
(`SpecBlock_IsSameShapeButDifferentContentForTwoUsers`, `EntryWalkRequests_ShareOneSpecBlock`).

### Methodological note, for the fourth time

This is the fourth "constant on every captured frame" in this decode that turned out to
mean "only one case was captured": `LeadingConstant` (was the operation code), the 594-byte
fragment split (one file size), and now the per-listing spec block. `RequestSelector1Value`
and the 62-byte length are the two that DID survive - and they only count as surviving
because a second case was actually run against them.

---

## 6j. Why the envelope channel rule failed on this traffic (2026-07-30)

`EnvelopeConformanceTests.AllCaptures_EnvelopeModel_ReproducesEveryDataFrameChannel` had been
red at 70 mismatches out of 970 ever since the file-server captures joined the corpus. The
cause is now identified, and it is caused by the very finding in section 6f.

### The rule and its hidden assumption

```
baseLow = (seed - (Flags2 & 0xFF)) & 0xFF
epoch   = (Flags1 - baseLow + 0xFF) >> 8
Channel = 0xDE - (XMCSM >> 24) - epoch
```

`Flags2 & 0xFF` was a **message-class marker** in every capture the rule was derived from:
`0x00` for control, `0x08` for terminal data. On COSMOS file-server traffic Flags 2 is the
message **body length** (`length - 28`). So `baseLow` becomes a function of how long the
message happened to be, and the epoch derived from it is a wrap count of nothing.

### The measurement

Split the whole corpus by that one property:

| Flags 2 low byte | Data frames | Channel mismatches |
| --- | ---: | ---: |
| a class marker (`0x00` / `0x08`) | 800 | **0** |
| a body length | 170 | **70** |

The rule is not weakly true everywhere. It is **exactly** true on 800 frames and simply does
not apply to the other class. Every one of the 70 was the channel off by 1 or 2 with the
counter correct - and the counter agreeing proves nothing, because the test learns the seed
from the same frame, which makes `LearnSeed` its own inverse.

### What was changed, and what was not

- The conformance test now asserts the domain where the rule holds and counts the rest
  without asserting on it. The whole suite is green: **439 passing, 0 failing.**
- `XmsgEnvelope` carries the scope limit next to the rule itself.
- `ChannelOffsetDiagnosticTests` locks the 800/0 versus 170/70 split in, so a future change
  that quietly widens or narrows the domain fails.

**No replacement formula was fitted.** The wire channel offsets on length-valued Flags 2 run
0, 1, 2 and 3. Three frames were worked through by hand and a rule could be made to fit them,
which is exactly the mistake section 6i is about. Until that class is captured in quantity,
the honest options are to track the channel per direction or to leave it undeclared.

**Also worth stating plainly:** the Counter line of the model survives on all 970 frames, so
only the channel derivation was ever class-dependent.

---

## 6k. Single-action recordings: which service does what (2026-07-30)

Three operations had been seen but not attributed, because every earlier recording contained several
operator commands and a disconnect. Two recordings were then made with exactly ONE command each, and
the recording stopped BEFORE the session disconnected, which is what makes the attribution sound.

### The split

| operator command | service | exchanges on the wire |
| --- | --- | --- |
| `OPEN-FILE` | `*FA-SERVER` | `0002` open spec, then `0005` open file. Nothing else. |
| `CLOSE-FILE` | **`*FA-USER`** | no `*FA-SERVER` traffic at all |
| disconnect / logout | `*FA-SERVER` | `0006`, an `000C`, then `0003` |

### What this settles

**The conversation is long-lived.** `claude-OPENONLY-102-to-100-2026-07-30.pcapng` holds exactly two
exchanges and no close of any kind. The conversation is left standing while the file is open, so it
spans the file's lifetime rather than one command. A client that tears the conversation down after
each request is wrong.

**Operation `0x0006` is not the file close.** It had been recorded as "consistent with closing the
file"; that was wrong. It only appears when the terminal session ends. What it does is still UNKNOWN,
but its trigger is now narrowed to session teardown.

**`*FA-USER` has a purpose.** Recorded on 2026-07-29 as a second service whose "purpose is not
established". `claude-CLOSEONLY-102-to-100-2026-07-30.pcapng` contains only this, in a window whose
sole operator command was `CLOSE-FILE`:

```
100->102  len=100   0144 0060 0104 0066 0587  "USER" then zero fill
102->100  len=14    0100 000a fe08 "*FA-USER"
```

So closing a remote file is handled by `*FA-USER`, not by the file-access server. Both frames still
need decoding.

**Attribution caveat.** The close window also contained the login sequence. Login on its own has
never produced file-access traffic in any earlier recording - the `*FA-SERVER` opener has always
appeared only when a file command was typed - so `CLOSE-FILE` is the reasonable attribution. It is
not proven to the standard of the open recording, where the command was the only thing in the window.

### Method note

The trick that made these work: give `dumpcap` a duration short enough that the recording ends while
the terminal session is still connected. Stopping the recorder by hand always came after the
disconnect, and the disconnect generates its own traffic, which is exactly what had been polluting
the earlier attributions.
