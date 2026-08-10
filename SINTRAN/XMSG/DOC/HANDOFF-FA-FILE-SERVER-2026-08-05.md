# Handoff: the COSMOS FA file server, 2026-08-05

**For what to do NEXT, read [PLAN-FA-FILE-SERVER-2026-08-06.md](PLAN-FA-FILE-SERVER-2026-08-06.md).**
This file is the record of what was learned on 2026-08-05 and why. It follows on from
[HANDOFF-FA-FILE-SERVER-2026-08-04.md](HANDOFF-FA-FILE-SERVER-2026-08-04.md), which is still the
place to go for how the listing was made to work.

> **Pruned 2026-08-06.** Everything that has since been validated has been removed - the three
> "decisions waiting" that framed the original document are all resolved, and their reasoning is
> now only of historical interest. What remains is the knowledge that is still load-bearing.

---

## 1. What landed

Newest last:

| Commit | What |
| --- | --- |
| `325df2a` | The working listing pinned byte for byte, before the frame layer was touched |
| `1bb612d` | Serving file contents, split across the fragment pair the wire needs |
| `1fd7254` | The file name read out of the spec block, so `FILE-STATISTICS` names one file |
| `518db99` | The last private QFORM writer gone |
| `5b23ff3` | File specifications matched by parsing, not by searching inside them |
| `86eab68` | The LAPB measurement - 452 of 3673 recorded frames exceed the old 312 limit |
| `1fd6d2e` | The fragment rules measured over the whole capture rather than one example |
| `1ccf2ac` | Header word 6 derived as the carved checksum; LAPB raised to a derived 622 |
| `605a8b9` | One codec for both directions - `FaReadFileCodec` becomes `FaFileDataCodec` |
| `00a94cb` | Rejoin a split message, so file content can reach a server at all |
| `ca77405` | Serve a `WriteFile`, and the set-length that states the real size |
| `8969943` | The closing close was sending conversation `0000` - found on the live D100 |

**Two results worth keeping in one line each.** Word 6: the fitted seed/counter model reproduced the
LOW byte by construction and anchored the HIGH byte at a constant, which only worked because every
capture it was fitted to used node numbers under 256; ours is 19999. LAPB: the 226/226 size band is
fragment pairs, occurring in the four transfer captures and nowhere else - every other traffic class
tops out at 292.

---

## 2. Lessons that still apply

### 2.1 The regression net came first, and earned its keep

`FaListingRegressionTests` drives one whole listing - connect, first file, directory, user, second
file, end of walk - and compares **every emitted frame byte for byte**. A companion test requires
each frame to be a single unfragmented data frame.

That second one is the point: serving file contents needs subtypes `0x0A`/`0x0C` on the same send
path a listing travels, and a listing must never end up on them.

**Every behavioural test passed before the listing worked, too.** They check meanings - the record's
name, the walk position, the refusal code. What actually fixed it was tag bytes, field widths, word
alignment and frame sequencing, which no meaning-level assertion touches. Update a golden
deliberately and say why in the commit; a silent update makes the file worthless.

### 2.2 Fragmentation belongs to the frame layer, not to the transfer

The 594-byte split was recorded as a property of the XFTRA transfer and possibly of the one file
size it had been seen at. It is neither. The file-access read delivery is a different protocol at a
different message length (1032, not 1030) and splits at the same byte. It now lives in
`SintranMessageFragment`; `FaTransferCodec` reads it from there.

Measured over the whole of `capture-read.txt`, 72 fragment frames:

 - 36 first fragments, 36 continuations, all 36 pairs **sharing one Flags 1**.
 - Every first fragment declares Flags 2 = 1032, the TOTAL message length.
 - Every continuation declares Flags 2 = 594, the offset it resumes at.
 - Within a delivery the two messages take consecutive Flags 1 (`0204`, `0205`); the next delivery
   opens three higher because the reply and its acknowledgement take numbers of their own.

A message needing a THIRD fragment is refused rather than chained - no capture shows one.

### 2.3 The spec block is readable enough to name a file

It was ignored entirely, so `FILE-STATISTICS` on one file walked the whole folder. Carved by
comparing a listing that names a file against two that do not:

```
FILE-STATISTICS  (SYSTEM)SINTRAN:DATA'SINTRAN:DATA'0000...
LIST-FILES       (SYSTEM)'EM).(SYSTEM)'7.........H....
LIST-FILES       (SECRET)'ET(SECRET)).(SECRET)'..H....
```

User in brackets, then the filespec TWICE, each ended by the SINTRAN terminator `0x27`. A
whole-directory listing puts the terminator straight after the bracket - **an empty filespec is how
"give me everything" is said**. What follows it there is residue from the caller's uninitialised
buffer, which is why the two `LIST-FILES` blocks differ from each other past that point.

Only the first copy is read. Why the spec appears twice is UNKNOWN, and reading the second would
mean deciding what to do when they disagree - a case no capture shows. Everything past the second
terminator stays opaque.

**This carve is not finished.** Live, a real client's `FILE-STATISTICS` matched nothing - see P1 in
the plan.

### 2.4 Two matching bugs, found by self-review

Both asked whether a served file's name **appears inside** the specification. That is true for
almost any short name - a file called `A` is a substring of nearly every specification there is. The
test files were long enough that none collided, which is exactly why it passed.

In the listing it returns the wrong file. In `LengthOf` it is worse: the store had already resolved
the open correctly, so an open of `BETA:DATA` in a folder also holding `A:SYMB` opened BETA and
reported **A's length**. The read protocol has no end marker - the length reported at open is the
only thing telling a client when to stop.

Both now split the specification and compare name and type as fields. One wrinkle worth remembering:
`FaFileName.TryParse` splits the type on a COLON, which is correct for SINTRAN, but we hand out the
names of files in a Windows folder and a client echoing one back says `PATCH.SYMB`. The dot is
accepted as a second separator **in the server that serves a folder**, not by loosening the SINTRAN
parser for everyone.

### 2.5 The close, and why it hid for so long

`FaServerSession.Conversation` is a lazy getter that builds with conversation 0. The FIRST thing it
is asked for is the answer to the connect LETTER - an XROUT letter, which carries no file-access
conversation number. The real number arrives on the first request afterwards, but `EnsureConversation`
only assigned when the builder was null, so it never landed and every close named conversation 0.

The builder must **learn** the number rather than be replaced: it counts the replies it has produced
(the `0x80 + n` session-header byte), and a fresh one would restart that count.

The test that catches it has to drive connect-letter, then request, then finish, **in that order**.
Any other order hides the bug.

---

## 3. What is verified and what is not

**Verified against a real machine:** the listing only. D100 lists our `*FA-SERVER` and prints the
files.

**Verified against captures only:** everything about reading AND writing a file - the request, the
empty reply, the 1032-byte data messages, the counter and token rule, the fragment split, the
reassembler, the set-length. **No real client has read a file from us.** Closing that gap is P1 in
the plan.

**Ours, not measured:** three of the codes in `FaServerStatus`, and the data-message counter's step
between deliveries. Both are listed under "deliberately not doing" in the plan, with the reasoning.
