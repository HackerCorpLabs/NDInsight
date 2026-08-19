# Aligning our FA code with what COSMOS actually does - running notes

Full path: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-BEHAVIOUR-ALIGNMENT-2026-08-11.md`

Started 2026-08-11. This is a running note, not a finished analysis. It records what has been
MEASURED against real captures, what is only a hypothesis, and what could not be checked yet.

The prompt behind it: our FA message BODIES match the captures byte for byte, and live runs
still fail. So the mismatch is in behaviour - sequencing, repeats, lifetimes - not in field
layout. That is what this document chases.

---

## Settled: our bodies are right

Two independent checks on 2026-08-10, both against real machine-to-machine traffic.

**OpenFile.** The real client's open in `readback-10-blocks.pcapng` (message `0c24`) is

```
92 0005  92 0002  F2 0002  BD "BIGPSH3:TXT'."  F2 00FF
```

and `SRC\Xmsg.Protocol.Tests\FaReadLadderTests.cs:233` already asserts that exact tail. A live
D100 then answered our OpenFile with the file's real length, which settles it from both ends.

**ReserveFileEntry reply.** Real server, `capture-list-files.txt` at 02:20:05.663:

```
07F0 0002 8000 90BB  92 0002  92 0001  F2 00FF  00
```

Ours, from the 23:19 live log:

```
07F0 0002 8000 9081  92 0002  92 0001  F2 00FF  ..
```

Same 18 bytes, same shape, same trailing pad. Only word 3 differs, and that is not constant on
the real side either - see below.

---

## Measured: the reply port belongs to the SERVER, not the conversation

D100 answers from `0x05B9` in every capture we hold, across three days, two different client
ports and four different connection numbers - including `nd-to-nd-write.pcapng`, where a client
releases one conversation and opens another and the server confirms the second from the same
port. A different server has a different port (`D102` is `0x06B6` throughout
`capture-list-files.txt`), so the port belongs to the server.

Reproduce:

```
cd DOC\captures\ND-TO-ND-WRITE-2026-08-10
PYTHONPATH=. python ports.py nd-to-nd-write.pcapng
```

We used to allocate one per conversation and release it on close. Fixed 2026-08-10; the 23:19
live run answered nine connect letters all from `0x0211`. D100's complaint changed from
`NO ANSWER FROM REMOTE SYSTEM` to `FILE-ACCESS PROTOCOL ERROR` - from ignoring us to arguing
with us.

---

## Observed, not yet explained: word 3 tells client from server

Across the captures, word 3 of a Request/Reply splits cleanly by role:

 - clients send `0xD7xx` - `d77e`, `d79b`, `d761`.
 - servers send `0x90xx` - `9081`, `90bb`.

It is NOT constant per role: the same server uses `9081` in one session and `90bb` in another,
and the same client `d77e` then `d79b`. So it carries something per-session. We emit `0x9081` as
a server, which is at least the right side of the split.

UNKNOWN: what varies it. Do not model it as a constant on the strength of one session - that is
the mistake that produced the confirm-trailing-word defect on 2026-08-09.

---

## METHOD: the code is the oracle. Stop guessing.

Ronny, 2026-08-11: *"when we dont know, we dont keep guesing and trying - we do carve and see the
original assembly and what the cosmos done"* and *"or we carve new, but the oracle is always the
code"*.

This supersedes how the rest of this document was being written. Captures show what happened
ONCE; the code shows what happens ALWAYS, including the branches no capture we hold ever took.
Where a question cannot be answered from a capture, the answer is a carve, not another live run.

The guess this replaced, recorded so nobody re-runs it: our server burns a new
short-acknowledgement counter when it replays a reply to a retransmitted request (visible in the
23:19 log as acknowledgements 2 and 3 both at Flags 1 `0x0004`). That MIGHT be the violation. It
is not established, and it is not worth another live run - the client code says so outright.

## IDENTIFIED: what D100 actually reported, and where its code lives

The terminal message is not a COSMOS program string - no COSMOS binary contains it. It is a
SINTRAN kernel error:

```
267 octal = 183 decimal = "File-access protocol error; connection aborted"
```

Sources: `Reference-Manuals\ND-60.128.5 EN SINTRAN III Reference Manual.md` (error list and
description), `Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`,
`Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md` (which classes it a
SERIOUS EVENT, number 26).

The manual's whole description is **"Internal error in remote file access."** - so the manual is
a dead end and the code is the only oracle, exactly as Ronny said.

**Carve target:** `SINTRAN\XMSG\COS-FSART-E02.BPUN` (66074 bytes). FSART is the RT program the
COSMOS start-up runs (`@ABORT FSART` / `@RT FSART`) and the one that owns the file-access
connections - `LIST-PORTS` on D100 shows FSART holding ports 3, 9 and 14, and
`FS-ADMINISTRATOR` reports "No of FACs attached: 30". It is NOT yet carved:
`DOC\COSMOS-RE\carve\` holds `COS-FA-SERV-E04_PROG.bin`, `COS-CONN-TO-E02_PROG.bin`,
`XMSG-FIDO-L03_PROG.bin`, `XMSG-IN-L03_PROG.bin` and `XMSG-KERNEL-L03_flat.bin`, and no FSART.

Note the existing `DOC\COSMOS-RE\Analysis\COS-FA-SERV-E04-Analysis.md` is the SERVER side, with
~179 functions already named in a Ghidra database. Useful background, but it answers the wrong
half: in the failing run D100 is the CLIENT and we are the server, so the code that rejected us
is the client's.

**Tool:** `tools\sintran-segment-carver`, plus the PLANC toolkit in `tools\ghidra-planc`.

### The carve route, settled 2026-08-11

Do NOT write a BPUN parser. FSART is not loaded as a flat image - `COS-FSART-E02.MODE` shows the
RT-loader reading it into a named segment:

```
NEW-SEGMENT FSASG,,DM,,,,,
READ-BINARY (COSMOS-BASIC)COS-FSART-E02 FSASG,,,
DECLARE-PROGRAM FSART,,,
CHANGE-RT-DESCRIPTION FSART 40 FSASG 0 0,,,,
```

So the loaded code lives in segment `FSASG`, and the repo's existing, verified segment pipeline
applies unchanged: `ndtool -x SEGFIL0:DATA` then `carve.py`. (`COS-FSART-E02.BPUN` does start with
a 38-word octal-text bootstrap in parity-set ASCII followed by a binary payload, but none of that
has to be decoded to get the code.)

**Segment facts, ASKED OF THE MACHINE rather than derived** - D100, `LIST-SEGMENT FSASG`:

```
FIRST PAGE:       100B    LENGTH:      41B
SEGMENT FILE:       0B    MASS. ADR: 15674B
WPM RPM FPM
DEMAND OK
```

 - `LOGAD` = `100` octal = 64 pages. Load address is `(64 * 1024) & 0xFFFF` = **0x0000**, so
   FSART sits at the base of a bank - the same shape as the FA server, which the existing
   analysis records as BANK2.
 - `SEGLE` = `41` octal = 33 pages = 33792 words (67584 bytes). The BPUN is 66074 bytes, which
   is consistent.
 - `SEGFIL` = 0, so `SEGFIL0:DATA`.
 - `MADR` = `15674` octal = 7100. The segment is
   `SEGFIL0[7100*2048 : 7100*2048 + 33*2048]` = bytes 14540800 .. 14608384.

**Inputs.** D100 mounts `BIGDISK0-K-100.IMG` on `bd0` - read from `F:\RC\RonnyTest\HDLC1\RetroCore.ini`,
not assumed; several other images sit in that folder and are commented out. `ndtool.exe` is at
`E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build_win\ndtool.exe` (underscore in `build_win`).

**CAUTION.** D100 is running and writing to that image (its timestamp moves). COPY the image
before extracting, or extract while the machine is stopped - do not read a pack underneath a live
machine and trust what comes out.

### Carve done, and it moved the target

FSASG was carved and cross-checks: `SEGFIL0[7100*2048 : +33*2048]` begins `aa02 aa01 340a`, the
same byte run that follows the octal loader in `COS-FSART-E02.BPUN`. So the segment really is the
loaded FSART, 66.7 per cent non-zero.

**And FSART does NOT raise our error.** The word `0x00B7` (183 = 267 octal) does not occur
anywhere in the 67584-byte segment, at any word-aligned position. So FSART is not the raiser and
carving it further will not answer the question. Recording that as a result, not a failure - it
is exactly what the carve was for.

**The raiser is the SINTRAN kernel, and the K set is ALREADY CARVED.** D100's banner is
`SINTRAN III - VSX/500 K`, and `tools\sintran-segment-carver\versions\K-VSX-500\segments\` holds
35 carved segments for that exact version - including **`022-S3RFAC.bin`**, S3 Remote File
ACcess, the client side of what we are talking to. No new carve was needed.

## MEASURED: the SINTRAN error-message table, and how to read any error out of it

The message lives in `versions\K-VSX-500\segments\014-S3ERRP.bin` at file offset `0x2456`
(bytes are ND ASCII with the parity bit set - strip bit 7 to read them). Messages are stored
consecutively, each terminated by an apostrophe.

**RETRACTED 2026-08-11, same night.** What follows was written as "a general tool: any SINTRAN
error message maps to its number and back". That claim is FALSE and has been withdrawn - see
"The table model does not hold" below. What survives is only that these five messages are
CONSECUTIVE and in error-number order, which is enough for the one conclusion drawn from them
and nothing more.

Splitting that segment into apostrophe-terminated printable strings and numbering them from zero
gives a run whose index tracks the SINTRAN error number with a **local offset of 112**:

| index | message | error octal | error decimal | checked |
|---|---|---|---|---|
| 292 | NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED | 264 | 180 | yes |
| 293 | FILE-ACCESS INITIALIZATION FAILED | 265 | 181 | implied |
| 294 | UNKNOWN REMOTE SYSTEM NAME | 266 | 182 | yes |
| 295 | FILE-ACCESS PROTOCOL ERROR; CONNECTION ABORTED | 267 | 183 | yes |
| 296 | FILE-ACCESS INTERNAL ERROR; CALL NOT VALID IN CURRENT STATE | 270 | 184 | yes |

### The table model does not hold - RETRACTED

The four checks above were all made INSIDE one contiguous run of messages, so they could not tell
"a global table with origin 112" apart from "these five happen to be consecutive". Tested wider,
the offset is not constant:

| message | error | index | index - error |
|---|---|---|---|
| FILE ALREADY EXISTS | 62 | 182 | 120 |
| FILE ALREADY OPEN | 69 | 189 | 120 |
| NO ANSWER FROM REMOTE SYSTEM | 180 | 292 | 112 |
| UNKNOWN REMOTE SYSTEM NAME | 182 | 294 | 112 |
| FILE-ACCESS PROTOCOL ERROR | 183 | 295 | 112 |

Eight records go missing between error 69 and error 180. Allowing EMPTY apostrophe-terminated
records shifts both groups by two (122 and 114) and does not close the gap, so empty slots are
not the explanation either. Checking the predicted origin directly kills it outright: index 112
is `OCTOBUS ERROR CODE:` and index 114 is `JANUARY` - the segment holds month names and other
text mixed in with the error strings, so a naive string split is not a model of the table at all.

**What survives:** the messages ARE stored in error-number order, verified independently in two
separate spans (62/69 agree with each other; 180/182/183/184 agree with each other). That is
enough to support the one thing this was used for - that both errors a real D100 has thrown at us
are neighbours in the same remote-file-access family, 264 to 270 octal, and that moving from 264
to 267 means moving from "it gave up waiting" to "it read what we sent and rejected it".

**What is dead:** any fixed offset, and any claim that this maps arbitrary messages to numbers.
Do not use it that way. The real table needs the indexing CODE, which was the point of the
exercise in the first place.

**What this tells us about our problem.** Both errors a real D100 has thrown at us -
`NO ANSWER FROM REMOTE SYSTEM` (the 22:17 run, before the reply-port fix) and
`FILE-ACCESS PROTOCOL ERROR` (the 23:19 run, after it) - are NEIGHBOURS in one contiguous
remote-file-access block, 264 to 270 octal. One subsystem raises them all, and moving from 264 to
267 means we moved from "it gave up waiting" to "it read what we sent and rejected it".

### Next: find the raiser inside S3RFAC

`0x00B7` does not appear as a plain word in `022-S3RFAC.bin` either, so the number is not a
stored constant - it is built or carried some other way (an index into a common error return, a
computed base plus offset, or an instruction displacement). Finding it needs disassembly, not a
byte search:

 - Disassemble `022-S3RFAC.bin` at its own load address from `022-S3RFAC.meta.json`.
 - Find the common error-return path and what selects 264 vs 267 - those two branches are
   precisely "no answer" versus "answer rejected", which is the distinction our two live runs
   crossed.
 - Only three K segments contain `0x00B7` at all - `007-S3DMAC`, `033-S3XMSGP`, `065-S3SIPIT` -
   and `033-S3XMSGP` (the XMSG part) is the interesting one to check first.

## FALSE LEAD, chased and killed the same night - the constant alias trap

`033-S3XMSGP` holds `000267` at word offset `012236`, and the words around it read, in octal,
`000277`, `000273`, `000267`, `000266` - 191, 187, 183, 182. Four descending values landing
inside the remote-file-access error block, `000267` being our exact error, each one preceded by
what looked like a call. It looked like the error table.

**It is not. They are ordinary instructions.** Disassembled at the segment's own load address
(`033-S3XMSGP` loads at `0120000`, so the region is `0132210`):

```
132233  050272  LDT -106
132234  142060  SKP IF 0 UEQ ST
132235  124003  JMP 3          ; -> 132240
132236  000267  STZ -111       ; reached when the skip IS taken
132237  124004  JMP 4          ; -> 132243
```

`000267` = `0x00B7` = `0000 0000 1011 0111`: opcode `STZ`, X/I/B all zero so P-relative,
displacement `0267` = 183 which as a signed byte is -73. A store, not a constant. And the flow
proves it is code rather than skipped data - the branch targets are self-consistent (`JMP 3` at
`132235` lands exactly on the next `LDT`), and `132236` is reachable, with a `JMP` after it to
the common join at `132243`. Data you skip over does not have a jump on the far side of it.

So the four "error numbers" are four `STZ` displacements that happen to fall in that range.

**This is exactly the trap already recorded in the memory `nd100-dis-carving-workflow`:** *"Never
scan for constants on this architecture. `020400` (the X5THD marker) is also the encoding of
`STD 0,B`. All six hits in the kernel were ordinary instructions. Navigate by SYMBOL."* The rule
was written down, and the byte search still produced something that looked like a discovery.

**Standing conclusion:** 267 octal is not stored as a literal word in FSART, in `022-S3RFAC`, or
anywhere that survives inspection. The number is computed or carried, so it has to be found by
control flow - from the routine that prints the message backwards to its callers - and not by
searching for its value.

**The question to put to the carved code, in order:**
 1. Every site that raises 267 octal. That is the shortest path to the exact check we failed.
 2. What it does with a retransmitted request - does it expect the same short-acknowledgement
    counter back?
 3. Why it re-sends a connect letter it has already had confirmed.
 4. What makes it stop advancing a directory walk.

---

## The Ghidra database is live, and where it is and is not usable

`cos-fa-serv-e04.prog` is one of ten programs open in Ghidra (ND-100:BE:16), with the earlier
analysis intact - 120 named `fa_*` functions.

**Usable:** the entry/registry region `0x26xx-0x3fxx` - clean, well-named
(`fa_reserve_file_entry`, `fa_open_file_op`, `fa_parse_request_params`, `fa_bitmap_*`, ...).

**NOT usable by decompiler:** the message-I/O region around `0x8c5d-0x93xx`. Decompiling
`fa_recv_request_wait` produces over a hundred "Removing unreachable block" warnings and a body
that merges several routines - exactly the "fragment inflation" the analysis warns about. The
previous analyst clearly hit the same wall and left a detailed hand-written header comment
instead. **Read the comments in that region; do not trust its decompilation.**

What the comment does give, verbatim from the binary:

 - `fa_recv_request_wait` (`0x8c5d`) is a thin wrapper on `xmsg_XFRCV`, option bit 15 = XFWTF
   (wait for a letter).
 - There is a FAMILY of identically-shaped message-I/O wrappers at `0x8c5d`, `0x8c99`, `0x8cd4`,
   `0x8d26`, `0x8d74`, `0x8da7` - each builds an option word, calls one primitive, then runs the
   same status translation. **Note:** the analysis document calls `0x8c99`
   "`fa_request_engine_process`, the core engine"; the in-binary comment says `0x8c99` is one of
   these wrappers. Those two claims disagree and the disagreement is unresolved - do not build on
   either until it is settled.
 - A library-status translation set: `0x4225` and `0x423F` mean "no letter / end of stream" and
   return quietly; `0x4209`->`0x436B`, `0x4243`->`0x4338`, `0x4244`->`0x436C`; anything else is
   re-raised as `0x433F`.

**These are NOT our wire status codes and must not be aligned with them.** Our `FaServerStatus`
carries SINTRAN error numbers (0, 46, 48, 97, 129, 197, 211) per
`fa-status-codes-must-match-nd-error-table`. The `0x42xx`/`0x43xx` values are the server's
INTERNAL PLANC library statuses - the comment says outright that their symbolic names are not
known. Two different layers; mapping one onto the other would manufacture a false alignment.

## ROOT CAUSE FOUND, 2026-08-11: we had no send window, and it was one layer down

Everything in the "still unexplained" list below turned out to be one fault, and it was not in the
file-access protocol at all. **Our ND link layer sent every frame the moment it was handed one and
never looked at whether the peer had acknowledged anything.**

### How it was found

Not by theory - by counting frames in the 23:19 log. `[sniff]` lines show D100's link sequence
numbers and `[tx]` lines show ours, so the backlog can be worked out at every moment:

```
23:19:07.199  REPEAT of their seq 5    our unacknowledged backlog: 6
23:19:07.428  REPEAT of their seq 9                               7
23:19:08.001  REPEAT of their seq 12                             15
23:19:09.676  REPEAT of their seq 24                             33
```

The repeats are byte-for-byte identical frames - the seq 5 frame at 06.838 and the one at 07.199
are the same 50 bytes. D100 was not asking anything twice. It was **re-sending what it had not
seen acknowledged**, and our backlog grew and never came down.

### What a real ND does - measured, three captures

**CORRECTED the same day, by the test written to pin it.** The first measurement read
`capture-list-files.txt` alone - 444 data frames, 443 acknowledgements - and found a maximum of
TWO frames outstanding, which is the short acknowledgement and the reply sent together
(02:20:08.634 and .643, both covered by one acknowledgement at .677).

That number is too small, and a LISTING is why: it never sends a content message.
`capture-read.txt` shows a real D102 sending **four**, the whole answer to one request at once:

```
02:29:50.792  D102 -> D100  seq 44   36 bytes   short acknowledgement
02:29:50.803                seq 45   52 bytes   reply
02:29:50.810                seq 46  622 bytes   content, fragment 1
02:29:50.810                seq 47  452 bytes   content, fragment 2
02:29:50.827  D100 -> D102  acknowledges up to 44
02:29:50.844                acknowledges up to 47
```

So the window is **4**. This is the same trap `FaServerConversation.BuildReply` already records
for the session counter - a listing capture cannot show what a read capture shows - and it was
walked into again while reading the listing by hand. `NdLinkCaptureConformanceTests` caught it by
measuring all three captures, which is exactly what that test exists for.

A window below the real value is not incorrect, only slower: the queue releases frames one at a
time as acknowledgements arrive, which is why the listing still worked at 2. It serialises
exactly the content bursts that carry file data.

The acknowledgement frame carries no credit field - its trailing word is `0000` on every one from
both machines - so nothing on the wire negotiates this. There IS a separate window NPDU in the
protocol (`NdNpduType.Window`, index 4) and it has never been captured.

### The second defect the same measurement found

The frame sequence is **seven bits, not eight**. Across all three captures the highest sequence is
`0x7F` and not one frame has bit 7 set; the wrap is visible at 02:20:10.52 where D102 sends `0x7F`
and its next data frame is `0x00`. Our code wrapped at 256. The 23:19 run reached 124 and stopped
four frames short of emitting a `0x80` no real machine ever sends.

### Fixed

 - `SRC\Xmsg.Ethernet\NdLinkLayer.cs` - a send window of 4, a bounded queue behind it, the peer's
   acknowledgement sequence now read and used as the window's low edge, and a repeated or stale
   acknowledgement ignored rather than re-opening the window.
 - `SRC\Xmsg.Ethernet\NdLinkHeader.cs` - `SequenceModulus = 128` and `NextSequence`, one wrapping
   rule in one place.
 - `SRC\Xmsg.Ethernet.Tests\NdLinkLayerTests.cs` - five tests. **Proved to bite**: with
   `SendWindow` set back to 1000 and the modulus back to 256, they fail. The old
   `AcknowledgementSequenceWrapsAtByteBoundary` was replaced - it asserted `0xFF -> 0x00`, which
   passed while describing a value no ND ever puts on the wire.

### The two guards, so this class of fault announces itself next time

 - **`NdLinkCaptureConformanceTests`** reads the three shipped captures and holds `SendWindow` and
   `SequenceModulus` to what the machines do. It asserts the window EQUALS the widest real burst,
   so it cannot be quietly lowered or inflated. If the captures go missing it FAILS rather than
   skips - a conformance test that passes without its evidence reads as proof while proving
   nothing. **It earned its place immediately**, by catching the window-of-2 error above.
 - **`NdLinkLayer.DuplicateDataFramesReceived` and `OnDuplicateDataFrameReceived`.** A repeat is
   acknowledged again - the peer repeated because it did not see the first acknowledgement - but is
   no longer handed up a second time, which is what let a repeat reach the file server as a fresh
   request. The runner logs a warning naming the sequence and what it was waiting for. Note the
   re-delivery drop is marked in the code as REASONED, not measured: no capture shows an ND
   receiving a duplicate.

Whole suite: 920 pass, 0 fail.

### What this explains, and what it does not

Explained, all by the one cause:

 - the duplicate requests our server answered with fresh counters and fresh connection numbers,
 - the nine repeated connect letters,
 - the directory walk that never advanced past entry 0,
 - D100 finally raising SINTRAN error 267 octal, FILE-ACCESS PROTOCOL ERROR.

### VERIFIED LIVE, 2026-08-11 - twice, on two freshly restarted machines

`LIST-FILES D19999(SYSTEM).` on a real D100 printed all six files. Both runs followed the
[[d19999-live-test-protocol]] order: runner stopped, `restart-xmsg-cosmos.ps1 -Port 9010
-WithEthernet`, `xmsg-sequence.state` deleted, runner rebuilt and restarted.

```
FILE 0 : D19999.(PACK-ONE:SYSTEM)BIG:TXT;1     ... through ...
FILE 5 : D19999.(PACK-ONE:SYSTEM)THIRD:TXT;1
```

| | 2026-08-10 23:19 (before) | 2026-08-11 09:04 (after) |
|---|---|---|
| connect letters | 9 | 1 |
| directory walk | stuck on entry 0 | entries 0-5, all six |
| duplicate replies | yes, counters burned | none - one reply per request |
| frames D100 re-sent | dozens | **0 of 49** |
| our worst backlog | 33 and climbing | **1** |
| result | error 267 octal | the file list |

Three runs, each on a freshly restarted machine:

 - **A** - window 2, tracing off. Passed.
 - **B** - window 2, `--trace-frames` ON, the same setting the failing run used and the one the
   runner's own comment records as sitting on the hot path. Passed, so the result is not an
   artefact of quieter logging. The frame counts above are from run B.
 - **C** - window 4 after the conformance test corrected it, tracing on. Passed, with the same
   numbers: 49 frames, 0 re-sent, 0 duplicate warnings, 1 connect letter, worst backlog 1.

Logs: `SRC\Xmsg.Live.Runner\bin\Debug\net9.0\xmsg-run-A-no-trace-PASSED.log`,
`xmsg-run-B-traced-PASSED.log`, `xmsg-run-C-window4-PASSED.log`; the failing run is kept beside
them as `xmsg-d19999-2026-08-10-2319.log`.

Note the backlog never even reached 2 - a LISTING is naturally stop-and-wait, so the window is a
ceiling this traffic does not touch. It is a file read or write that needs the room, which is
also why the listing capture gave the wrong window in the first place.

NOT settled by this, and still open:

 - **What a real server does with a genuinely retransmitted request** - replay the same reply, or
   build a new one with a new counter? Ours builds a new one. With the window in place the repeats
   have stopped, so this is no longer on the failing path, but it is not answered.

### A model that survived a scare - the reply counter is NOT an echo

Chasing the duplicates, the listing capture looked like proof that a server ECHOES the request's
session-header word: request `07F0 0008 8100 D761` is answered by `07F0 0002 8100 90BB`, twenty
times unbroken. It is not proof, and `FaServerConversation.BuildReply` already says why in detail:
on a LISTING an echo and an own-counter are indistinguishable, because the server sends nothing but
replies so its counter stays in step. `capture-read.txt` separates them - request `86`, reply `8A`,
data messages taking the numbers between. **Our counter model is right.** Recorded because the
listing capture will look like an echo again to the next reader.

## Still unexplained

 - **Whether the listing ever worked on THIS path.** The code remarks record a real D100 listing
   our server and printing files on 2026-08-05; it printed nothing in either run on 2026-08-10.
   Either something regressed since, or that proof was on the HDLC path and not this Ethernet
   one. Worth settling before chasing anything else - it decides whether this is a regression
   or was never true here.

---

## Related

 - `DOC\captures\ND-TO-ND-WRITE-2026-08-10\ports.py` - the port evidence, reproducible.
 - `DOC\captures\FA-READ-WRITE-2026-08-04\capture-list-files.txt` - a real listing, decoded.
 - Memories: `fa-retransmitted-confirm-rewinds-session`, `fa-shortack-then-reply-next-exchange`,
   `test-with-real-wire-bytes-not-built-frames`.
