# RETIRED - THE LEAK WAS ALREADY FIXED WHEN THIS WAS WRITTEN

**Do not action this brief.** It was committed at 13:46 on 2026-08-18 asking for work that had been
finished at 12:44 (`a6e8f047`) and 13:37 (`ec7f0772`). It is kept because its reasoning was right and
its remaining unanswered question is genuinely open, but nobody should start from it.

## Why it was written after the fix

It was drafted from a stale view of the repository - a working context that ended before those two
commits landed - and never re-checked against `git log` before publishing. **The lesson is the same
one as RULE #0b, one level up: I did not verify the state of the thing I was writing about.** A
`git log --oneline -5` would have cost five seconds.

Worse, the same stale view produced a WRONG correction elsewhere: task #54 was reopened as "the leak
is NOT fixed" when it was. And the seat measurements reported alongside it - "30, then 29, still
leaking" - were taken against **my own broken experimental builds**, because I had overwritten the
already-fixed `BuildCloseBody` with a wrong version, measured the leak I had just reintroduced, and
reported it as the original defect persisting.

## What the fix actually was

All three had to be right **together**, which is exactly why each of the three attempts listed below
as "refuted" was destructive or useless - every one of them changed a subset.

| | wrong | right |
|---|---|---|
| type | `07C0` Close - the SERVER's message | `0782` Release - the client's |
| operands | `LetterEchoWord` first (the number the server stamps) | sender's conversation first, then the peer's |
| flags | `0x96` / `0x00` | `0x82` / `0x84` |

`FaWriteDriver.cs`:

```csharp
NdEndian.PutBe16(body, 0, (ushort)FaMessageType.SessionFinished);  // 0782
NdEndian.PutBe16(body, 2, _serverConversation);                     // ours, first
NdEndian.PutBe16(body, 4, _target.LetterEchoWord);                  // theirs
NdEndian.PutBe16(body, 6, 0x8000);
```

`FaReadDriver.cs` had the identical bug, fixed in the same commit. **The brief's own "prime remaining
suspect - the two conversation words" was correct.**

Then `ec7f0772` found the other half: **a refused transfer leaked too**, because `NextAction` tested
the failure first and the ladder ended before `SendClose`. A refusal now owes a Release, sent outside
the ladder.

**Measured after:** 3 pushes + 2 pulls with `*FA-SERVER` steady at 29 free, then 3 refused creates +
2 refused pulls + 1 good pull, zero seats spent. Tests in `FaClientReleaseFrameTests.cs`, and
`FaWriteDriverTests` was widened - it had been naming `FaMessageType.Close` exactly, so it silently
stopped seeing the teardown.

## Question 1 IS answered: the FA server is NOT in the NPL sources

Both trees searched. `XMPINFC`, `XSGSP`, `FA-SERVER` and `QFORM` appear nowhere. `XSNSP` appears only
in symbol tables, never in a listing:

```
NPL-SOURCE/SYMBOLS/J/SYMBOL-1-LIST.SYMB.TXT:2124     XSNSP=000121
NPL-SOURCE/SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT:3005   XSNSP=000121
NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT:825 XSNSP=000121
NPL-SOURCE/SYMBOLS/L07/l07-kallsyms.txt:4776         0x51 T XSNSP
```

Nothing in `S3VS-6.SYMB`, `s3vs-4.symb` or any split NPL file. The earlier audit was right: the FA
server and QFORM are a COSMOS product, not kernel. `XSNSP` = 0o121 = 81 = 0x51 confirmed across J,
K03 and L07.

## What is still genuinely open

Questions 2 and 3 are answered **behaviourally**: the server returns the seat when it receives a
well-formed client Release naming a session it actually holds. What is NOT established is the routine
inside `FA-SERVER-TAD` that makes the `XMPINFC` call and what else it is conditional on. That needs
the Ghidra carve laid out below, and **nothing currently depends on it** - it is curiosity, not a
blocker.

---
---

# Investigation request: what makes a COSMOS file server return a connection seat?

> ## CLOSED 2026-08-18. Read this banner before the brief below it.
>
> **The leak itself was already fixed when this brief was committed.** Commits `a6e8f047` (transfer
> path) and `ec7f0772` (refusal path) landed at 12:44 and 13:37; this file was committed at 13:46 in
> a batch and never retired. Our client sent the SERVER's `Close` (`07C0`) with the two conversation
> numbers swapped and the wrong frame flags, where a client sends `Release` (`0782`), sender's
> conversation first, flags `0x82`/role `0x84`. All three had to be right together, which is why the
> three attempts tabulated below were each destructive or useless. Measured after: 3 pushes + 2 pulls,
> `*FA-SERVER` steady at 29 free.
>
> **The one question was then answered by carving, 2026-08-18 — and the answer is "it never does".**
> `cos-fa-serv-e04.prog` calls `XMPINFC`/`XSNSP` in exactly one place, on the initialisation path.
> No request handler, `Release` handler or session-teardown routine can reach it. There was never a
> message a client could send to trigger one. That is answer 4 of the four this brief said it would
> accept. Full working, call chain and search counts:
> **`DOC\COSMOS-RE\CARVE-ANSWER-FA-SEAT-RETURN-2026-08-18.md`**
>
> **Two corrections to the plan below, so nobody follows it as written:**
> - **Section 1 is answered: the FA server logic is NOT in the NPL sources.** `XMPINFC`, `FA-SERVER`
>   and `QFORM` appear nowhere in either NPL tree; `XSNSP` appears only as `XSNSP=000121` in the
>   symbol tables, never in a listing.
> - **Section 2 aims at the wrong artifact.** In the carved L-VSX-500 set, segment `073` is
>   `S3IDMWD`; the `LIST-REENTRANT` numbers are D100's and do not match that image, and there is no
>   FA-SERVER segment in the carve at all. **You do not need one** - the file server ships as
>   `Installation\Communication\COSMOS Basic\x\cos-fa-serv-e04.prog`, already in Ghidra with ~190
>   functions named (`Analysis\COS-FA-SERV-E04-Analysis.md`).
>
> Everything below is left as written, as the record of what was known at the time.

**This is a request for source analysis, not for live experiments.** Three live attempts have already
failed and two of them killed the file server on the machine. Read code, answer the question, then we
will try one narrow change.

---

## The one question

**When does `*FA-SERVER` call `XMPINFC` (or the raw `XSNSP` service) to give a connection seat back,
and what must a CLIENT send to make that happen?**

If the answer is "it never does, the seat is only released when the port closes", that is an equally
good answer and ends the hunt - say so and show what you read.

---

## Where to look, in order

### 1. NPL sources - probably NOT there, but check and say so

`SINTRAN/NPL-SOURCE/` and `SINTRAN/NPL-SOURCE-2/` hold the SINTRAN III kernel (version J SI-GEN and
others). A previous audit concluded that **the FA server and QFORM are a COSMOS product, not part of
the kernel listing**, so the file server's own logic is probably absent. But two things there ARE
worth reading first because they are cheap:

- **`SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT`** - the XMSG/XROUT symbol table.
  Already known from it:
  ```
  XSNUL=000100   XSLET=000101   XSNAM=000102   XSCRS=000120   XSNSP=000121
  ```
  These are **service numbers in OCTAL** (`XSLET=000101`=65 matches the value our chat client already
  uses). So **`XSNSP` = 0o121 = 81 decimal**. Look for neighbouring symbols that name the counter
  itself or the XT/port block field that holds it.
- **XROUT's own tables.** If the kernel NPL implements XROUT's name table and the free-connection
  counter, then the RULES for when it is incremented are there even if the FA server is not. Grep for
  the symbol names you find above, and for the port/name table structures.

**Report explicitly whether the FA server logic is present in NPL or not.** "Not there" is a result.

### 2. Ghidra - the COSMOS binaries, which is where it almost certainly lives

The subsystems are dumped into shared memory and appear in `LIST-REENTRANT` on D100:

```
   START RESTART SEGMENT   NAME
  21703B  21703B     73B   FA-SERVER-TAD      <-- THE FILE SERVER. Primary target.
   4472B   4472B     74B   FS-ADMINISTRATOR   <-- starts/stops it, and knows the FAC count
      0B      1B     76B   XMSG-COMMAND       <-- the X-C program; it PRINTS Free SPs
      0B      1B     70B   TRANSFER-FILE      <-- a native CLIENT: what does it send at the end?
      2B      2B     70B   TRANSFER
      3B      3B     70B   REMOTE-BATCH
```

**Segment numbers are octal.** The carving tooling lives in
`tools/sintran-segment-carver/versions/L-VSX-500/`, and there is a Ghidra toolkit for ND-100 code in
`tools/ghidra-planc/` plus the `nd100-ghidra` and `sintran-carving` skills.

**Priority order for Ghidra:**

1. **`FA-SERVER-TAD` (segment 73B)** - find where it handles the client's `Release` (`0x0782`) and
   what it does after: does it call `XMPINFC`/`XSNSP` there? Is the call conditional on something in
   the message? That conditional IS the answer.
2. **`TRANSFER-FILE` (segment 70B)** - a native client. Whatever it sends at the end of a transfer is
   by definition the thing that works. Compare against what we send (below).
3. **`XMSG-COMMAND` (segment 76B)** - it prints the `Free SPs` column, so it reads the counter. That
   tells you WHERE the counter lives in the port block, which makes it findable in the server.

---

## What is already established - do not re-derive these

**From the COSMOS Programmer Guide ND-60.164.3** (in `Reference-Manuals/`), quoted:

> XROUT will look at the free connection counter for portName and if this is greater than zero,
> **XROUT will decrement the counter** and forward the whole message to portName.

> After opening a connection port using XMPOPCN, **a task can later increment** (when the connections
> become available) or decrement ... the free connection counter associated with that port.

So: XROUT takes the seat on delivery; only the port's **owner** gives it back. `XMPINFC` is the
library call, `XSNSP` (0o121 = 81) the service.

**Measured on the wire:** the return never crosses the link. In `DOC/captures/ND-TO-ND-WRITE-2026-08-10/
nd-to-nd-write.pcapng` (D102 client, D100 server, no C# in the path, two complete file writes) there
is **not one `XSNSP` letter**, and **exactly one frame in the whole transfer is addressed to XROUT** -
the client's opening connect letter. That is consistent with the server calling `XMPINFC` locally to
its own XROUT, which no capture of the inter-machine link could ever show.

**The closing exchange a real client performs** (full frames, sub-header included):

```
D102 client  Release  frameFlags=0x82 role=0x84   body 0782 0043 0004 8000 0000
D100 server  Close    frameFlags=0x82 role=0x84   body 07C0 0004 0043 0000
```

Body layout is `[kind][clientConv][serverConv][...]`, and the server's Close mirrors the two
conversation numbers. The `0x8000` word in the Release is **not understood**.

**What OUR client sends instead** (`SRC/Xmsg.Servers/Fa/FaWriteDriver.cs`, `BuildCloseBody`):

```
ours         Close    frameFlags=0x96 role=0x00   body 07C0 <LetterEchoWord> <serverConv> 0000
```

We send `07C0`, **the server's own message**, where a real client sends `0782`. Our own C# `FaServer`
handles an incoming `0782` correctly - a real ND client driven at it logs
`system 100 has finished with conversation 0x0082 (0x0782); answering with a close` - so the server
side of this is understood and only the client is wrong.

---

## What has already been tried and REFUTED - do not repeat

| # | change | result |
|---|---|---|
| 1 | body `0782` with our usual flags `0x96`/`0x00` | **killed `*FA-FSA`**; seat still lost |
| 2 | body `07C0` with native flags `0x82`/`0x84` | harmless, **seat still lost** |
| 3 | both together (`0782` + `0x82`/`0x84`) | **killed `*FA-SERVER` and `*FA-FSA`** |

Attempt 3 had the right opcode **and** the right flags and still destroyed the server. So the
remaining suspect is **the two conversation words in the body**. A real client sends its own
per-session number first (`0x0082` in a captured run); we send `_target.LetterEchoWord`, which our own
code comments describe as "only the USUAL value" (`0x0002`). Telling a server to tear down a session
it is not holding would explain a dying server.

**Specifically worth answering from the binary:** what does `FA-SERVER-TAD` do with those two words
when it receives a `0782`? Which one does it use to find the session, and what happens when it does
not match?

---

## Symptom, for context

Every FA transfer consumes one `*FA-SERVER` seat and never returns it. Thirty after a file-server
start, the server answers new connect letters with **silence**, and every transfer then looks like a
network fault. Measured repeatedly: `LIST-NAMES` shows `Free SPs` 30, then 29 after one push, then 28
after the next. This is the cause of years of "random push stalls" - the discriminator is the transfer
**count**, not the file size.

Native clients do **not** appear to leak: five native `COPY-FILE`s left the count unmoved. That
comparison is weak on its own (a local copy may short-circuit), which is why `TRANSFER-FILE` in
Ghidra, or a native client driven over a real link, is the better control.

---

## What a good answer looks like

1. **Whether the FA server's seat logic is in the NPL sources** - yes with file and routine, or no.
2. **The condition under which the server returns a seat**, from the binary: the routine, the call to
   `XMPINFC`/`XSNSP`, and what has to be true for it to be reached.
3. **What a client must send** to satisfy that condition - especially the two conversation words.
4. **Or: proof that it never returns one**, in which case the seat is released only by closing the
   port, and the answer is operational rather than a code fix.

Cite what you read - file, routine, address. If something cannot be determined, say so plainly rather
than inferring; three confident guesses have already cost a working file server twice.

---

## Reference

- Full carve, all three refutations, and the controls: `DOC/CARVE-FA-SEAT-LEAK-2026-08-18.md`
- Ordered plan: `DOC/PLAN-2026-08-18.md` (this is Phase 1)
- Our client: `SRC/Xmsg.Servers/Fa/FaWriteDriver.cs`, `FaReadDriver.cs`
- Our server, which gets the protocol right: `SRC/Xmsg.Servers/Fa/FaServer.cs`
- Recovery if the file server dies during any live test:
  `ABORT FSART`, `RT FSART`, then `FS-ADMINISTRATOR` / `SELECT-FSA` / `START-SERVER 1,,,,`
  (the `ABORT`/`RT` pair FIRST if `*FA-FSA` itself is missing from `LIST-NAMES`).
