# Handoff: the ACCP <-> ND-5000 link seam, for the station split

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\HANDOFF-ACCP-LINK-SEAM-CONTRACT-2026-08-04.md`
**Date:** 2026-08-04
**To:** the LLM splitting `OctobusND5000Station` so only its ACCP part moves to the real firmware
**From:** the control-store link side
**Why you are getting this:** the link contract changed twice today, and one of the changes
invalidates a shape the station split would naturally assume.

---

## FILE OWNERSHIP - agreed 2026-08-04, to stop two agents colliding

We already collided once today: a file read mid-write by the other agent was diagnosed as "my edits
were reverted". They had not been. So we split by FILE, not by goal.

| Owner | Files (RetroCore, full paths under `E:\Dev\Repos\Ronny\RetroCore\`) |
|---|---|
| **You** | `Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs`, `Emulated.HW\ND\CPU\NDBUS\NDBusOctobus.cs`, `Emulated.Machines\ND\ND100\ND100Machine.ND5000.cs`, `Emulated.Machines\ND\ND100\ND100Memory.cs` |
| **Me** | `Nuget\HackerCorpLabs.Emulation.Machines.Accp\src\Devices\Nd5000ControlStoreLink.cs`, `...\src\Devices\Nd5000LinkWindow.cs`, `...\src\AccpMachine.cs` |

If you need a change on my side, ask rather than edit - and I will do the same.

---

## THE ONE THING THAT WILL BITE THE SPLIT

**The station models the ACCP as a COMMAND layer: one command in, one reply out.** That is right for
the octobus command library (OMD 3) and it is how `CMSYSPAR`, `RTEST`, `ALIVE`, `LPARP`/`VPARP`
behave. **It is NOT how the control-store layer works**, and a seam carved on the command shape will
not pass the control-store traffic through.

The real firmware drives the ND-5000 through a **register/shift protocol**, not commands:

| Address | Role |
|---|---|
| `0x220000` | clock/command port - BOTH a parallel command register AND a shift clock, decided by the value |
| `0x330000` | gate latch (bit 1 or bit 2 - see below) |
| `0x440000` / `0x550000` | the 32-bit data pair; `0x550000` is also the microword staging port |
| `0x660000` | status; bit 0 = "control-store operation OK" |

A single control-store access is: shift 8 x 16 bits through `0x550000` with 8 clock pairs each, then
one command word. **There is no request/reply framing anywhere in it.**

---

## THE TWO OPERATIONS - do not conflate them (this was a real defect, fixed 2026-08-04)

Carved from `octo.bin` at `0x773A`:

```
7746  jsr     0x7776               ; shift the 128-bit microword OUT
774C  move.w  #0x2018,(0x220000)   ; the operation
7754  unlk / rts
```

Compare `0x741E`:

```
741E  <word parameter in D0>
742E  jsr     0x76E6               ; address phase
7434  bset    #2,(0x001144EE)      ; -> 0x330000, GATE ON
7446  move.w  #0x0018,(0x00220000) ; the control-store command
744E  btst    #0,(0x00660000)      ; STATUS CHECK
7484  bclr    #2,(0x001144EE)      ; gate off
7496  jsr     0x775A               ; read-back verify
```

| | `0x0018` (CommandPerform) | `0x2018` (CommandOperation) |
|---|---|---|
| Gate set by the routine | YES (bit 2) | no |
| Address phase | YES | no |
| Status checked | YES (`0x660000` bit 0) | no |
| Target | **control store, at an address** | **the MIR register** |
| Boot uses it | 4 times | **284 times** |

**An operation with no address cannot write addressed storage.** `0x2018` loads the microinstruction
register - which is what the card's own `MIR test` and `Start/stop microprogram test` drive.

> **How this stayed hidden, and the lesson worth carrying into the split:** both commands were routed
> to the same commit, so every `0x2018` wrote control-store address 0, 284 times per boot. **Nothing
> failed.** The read-back verify still matched, because it read back the same slot that had just been
> overwritten. A test asserting `MicrowordsWritten >= 200` during boot PASSED - it was encoding the
> defect. The only external symptom was a wrong CLAIM ("booting the card loads 281 microwords").
> **A round trip that agrees with itself proves nothing about what the far end is.**

---

## MEASURED FACTS THE SPLIT MUST NOT RE-DERIVE

1. **The gate is bit 1 OR bit 2** of the `0x330000` latch. Boot uses bit 1 (`0x764E` path)
   exclusively; bit 2 (`0x741E`) is the console command. Recognising only bit 2 gave 1.6M clock
   pairs and ZERO microwords.
2. ~~**The address is the NINTH gated word**, after the eight halves - not a pre-gate write.~~
   **REPLACED 2026-08-04 - see "THE ADDRESS PHASE, CARVED" below.** The firmware does write the
   address ninth, but it is the address PHASE doing it, ended by an explicit command. The old
   reading only worked because the folded latch held the gate open across the whole shift.
3. ~~**The completed word must be LATCHED** - the card closes and reopens the gate between the
   shift and the perform.~~ **REFUTED 2026-08-04 - an artifact of the fold.**
4. ~~**Multiple shift+operate cycles happen inside ONE gate window** - staging must reset after
   each.~~ **REFUTED 2026-08-04 - an artifact of the fold.**

   > Audited by counting which buffer each commit actually took its words from, over a real boot:
   > **latch / staging / ring = 0 / 0 / 8.** Neither the latched-word path nor the gated staging
   > buffer is used at all any more; every microword is shifted out **ungated** and committed from
   > the ring. Both claims were compensating for the gate spuriously closing when `0x330001`
   > traffic was being read as a gate change.
   >
   > **The gate therefore does not capture the shift.** What it does select is still open - the
   > carve's phrase "BUFFERED CI-bit groups" may still be the answer, but nothing here has shown
   > it. The latch and staging code paths in `Commit` are now dead for the boot path and are
   > candidates for removal once the console path has been audited the same way.
   >
   > This is the concrete cost of the fold: two "MEASURED" facts that were really workarounds.
5. **`0x440000` must be HELD and echoed** (a `0x71F8` 32-bit round trip), not ignored.
6. **`0x2018` words can arrive GATED or UNGATED.** The boot's `0x764E` path holds the gate open
   across the whole sequence; the console path shifts ungated. Read only one and you get zero.
   [Learned by being wrong: a first attempt read only the ungated buffer and a real boot returned
   `MirLoads = 0`.]
7. Word order: `word[0]` = bits 127-112, confirmed three ways.

---

## WHAT THE LINK OFFERS YOU

`Nd5000ControlStoreLink` (my side) exposes, all earned rather than stubbed:

- `MirHi` / `MirLo` / `MirLoaded` / `MirLoads` - the microinstruction register.
- `MicrowordsWritten` - addressed control-store writes ONLY. Do not use it as a proxy for activity;
  that is the mistake above.
- `ClockPairs`, `GateOpens`, `PerformCommands` - diagnostics. A run with many clock pairs and no gate
  opens means a path is unrecognised; that is how the bit-1 gate was found.
- `Trace` / `TraceEnabled` / `TraceLimit` - operation log, clocks excluded.
- `HighBits` (via `IAccpStatusBitSource`) - contributes `0x660000` bit 0.

The ND-5000 side is the **`IControlStoreSink`** interface (`WordCount`, `WriteWord`, `TryReadWord`).
It exists so the ACCP package never depends on the CPU package - please keep that direction. A real
`ControlStore` satisfying it is proven by `Nd5000RealControlStoreTests`.

---

## WHAT IS STILL OPEN ON MY SIDE

**`Start/stop microprogram test abc failed at CSA: 00FFH`.** The card now reports a VALUE where it
used to report only a failure, and `0x00FF` has the shape of an unmodelled register reading all-ones.
The MIR is loaded correctly; what is missing is CSA (the control-store address register) and,
presumably, anything that would make the microengine actually step. That is the remaining half of
"run it", and it is mine.

**Do not model CSA on your side** - if we both do it we will disagree, and the card will believe
whichever answers first.

### First carve of the start/stop test - what it actually drives [V, ROM read 2026-08-04]

The failing test body is ROM `0xBE22`-`0xC05A` (bounded by its own two loop branches). Its calls:

| Target | Called from | What it is |
|---|---|---|
| `0x773E` | `0xBE78` | **the MIR load** (`jsr 0x7776` + `0x2018`) - so this test drives the path fixed in `c1e73a711` |
| `0x775A` | `0xBE9E` | the read-back **verify** (`0x2010` + `0x77B6`) |
| `0x474C` | `0xBF28`, `0xBFA6` | **hex-print helper, WORD** (masks `0xFFFF`, tag `0x30`) |
| `0x478C` | (same family) | **hex-print helper, BYTE** (masks `0xFF`, tag `0x30`) |
| `0x1A0A`, `0x1BF6` | several | console print helpers |

Loop structure: an inner count to **7** (`0xC050`) inside an outer count to **0x7F** (`0xC05E`), so
8 x 128 iterations. The failure message descriptor is at `0x11B00` (" failed at CSA:"), printed from
`0xC0B6`.

> **CORRECTION, same day, before anyone acted on it.** The first version of this section said
> `0x474C`/`0x478C` were a hardware service call and therefore that `00FFH` was "the signature of an
> unanswered read, NOT a wrong value - do not go looking for an off-by-one". **Both halves were
> wrong, and the advice was actively misleading.** The setup at `0xBF00` settles it:
>
> ```
> bf0a  lea     (0x00013044).l,A1      ; format-descriptor table
> bf10  move.w  (A1,...),(0x16,A0)     ; descriptor into the call frame
> bf1e  lea     (0x001144F0).l,A2      ; the 128-bit microword buffer
> bf24  move.w  (A2,D0.l),D0           ; the VALUE, read out of that buffer
> bf28  jsr     0x474C                 ; print it
> ```
>
> They are **hex-print helpers** - `0x474C` word, `0x478C` byte, both tagged `0x30`. So `00FFH` is a
> VALUE the test read back, and an off-by-one or a wrong read-back word is exactly the kind of thing
> that could produce it. **How the error happened:** the wrapper's shape (stash two words, tag with a
> byte, branch to a dispatcher) reads like a device call, and I concluded from the shape without
> looking at what the CALLER passed in. One dump of the call site refuted it.

**How the test actually works** (`0xBE6C` onward):

```
be7a  jsr     0x773E              ; MIR load - the 0x2018 path
be80..be9e                        ; CLEAR the 0x001144F0 buffer, words 0..0x0E
bea0  jsr     0x775A              ; verify = shift the microword back IN
beba  lea     (0x001144F0).l,A0
bec0  move.w  (A0,D0.l),D2        ; what came back
bec4  cmp.w   (0x20,A6,D1.l),D2   ; against the expected array in the frame
bec8  beq     ...                 ; mismatch falls through to the failure print
```

It zeroes the buffer between write and read-back **on purpose**, so a read-back that returns nothing
cannot be mistaken for a match. That is the same anti-vacuous discipline used in
`ScanAccpBitDispatchTests`, and it means the value printed is genuinely what our link supplied.

### Where the CSA value comes from - and why chasing it statically is a dead end [V, ROM read 2026-08-04]

The CSA failure print is a fragment of a chained print routine, not a subroutine. **Nothing in the
ROM does `jsr 0xC0AE`** (searched all 131072 bytes); each fragment ends `4ED5 jmp (A5)`, so A5 is a
continuation register and the fragments are strung together by whoever set it.

```
c0ae  2f0e / 2c56 / 2d4f 0008     ; PLANC prologue, re-frame to caller
c0b6  45ee 0028      lea (0x28,A6),A2      ; install the arg block at frame+0x28
c0bc  41f9 00011B00  lea 0x11B00,A0        ; " failed at CSA:"
c0c8  22d8 x3        move.l (A0)+,(A1)+    ; 12-byte descriptor into the arg block
c0ce  jsr 0x1A0A                           ; console print
c0d6  3039 001131FC  move.w (0x001131FC).l,D0   ; <-- THE NUMBER
c0e8  3179 001131FC 0014                   ; ... into arg slot 0x14
c0f0  43f9 00013044  lea 0x13044,A1        ; format-descriptor table (same one as 0xBF0A)
c0f6  3171 0800 0016                       ; descriptor into arg slot 0x16
c100  jsr 0x474C                           ; hex print, WORD
```

### MEASURED, and it refuted the reading above [V, emulator run 2026-08-04]

`Nd5000CsaFailureTraceTests` watches `0x001131FC` through a real 120-million-instruction boot.
**The cell changes exactly FOUR times, all inside the RAM walk-test, and never holds `0x00FF`:**

```
#0 @     44,793  PC 0x000BE6  0x0000 -> 0x0011
#1 @     44,798  PC 0x000BF6  0x0011 -> 0xFFEE
#2 @    124,294  PC 0x000C50  0xFFEE -> 0x0000
#3 @    150,143  PC 0x000DB6  0x0000 -> 0x0010
```

It settles at `0x0010` before the selftests even start and is never touched again, while the console
still prints `failed at CSA: 00FFH`. **So `0x001131FC` is not the printed value.**

Re-reading the fragment with that in hand, the operand roles are the other way round:

- `0x001131FC` (= `0x0010`) is a **format/width selector**. It is what gets `+0x20`-ed, doubled and
  used to index the table at `0x13044` - and `0x13044` is not a descriptor table at all, it sits
  inside a string blob (`"d not store$Max address "`), so the index reaches a descriptor further in.
- The **printed number is the frame local at `(0x20,A6)`**, loaded by `302E 0020` at `0xC0FC`
  immediately before `jsr 0x474C`.

> **This is the third wrong answer about this one message, and all three failed the same way:**
> reading a routine's shape and picking the operand that looked like the payload, instead of
> measuring which one carried it. The neighbouring `0x001131F8` / `0x001131FA` are the CPU-identity
> word and its valid flag, so `0x001131FC` is an ordinary variable in that status block - the "180
> references = shared print scratch" reading was wrong too.

### `00FFH` is NOT our absent-device fill [V, emulator run 2026-08-04]

Worth ruling out explicitly, because the same boot prints `Result : 0000FFFFH` for the neighbouring
A,MARG D,AIB test and that has the shape of an all-ones fill. Re-run with
`AbsentDeviceDataValue` and `StubReadValue` both changed to `0x5A`:

```
CSA line with fill 0x5A: failed at CSA: 00FFH
```

Unchanged. **`0x00FF` is a value the firmware computed**, not a byte this emulator handed it. The
"unanswered read" theory has now been floated twice and is measured dead - do not raise it a third
time.

### SOLVED: `00FFH` is a HARD-CODED CONSTANT, and the real test is elsewhere [V, measured + ROM 2026-08-04]

Trapping `0xC0FC` gives exactly one hit: `D0 = 0x000000FF`, `A6 = 0x11008C`, and the console prints
`failed at CSA: 00FFH`. So the printer decode is confirmed end to end, and the slot is the fixed
address `0x001100AC`. Watching *that* names the producer - `PC 0x00CDA6`, the last write before the
print (`0x0011 -> 0x00FF`).

And `0xCDA6` is this:

```
cd78  4eb9 0000775A   jsr 0x775A                  ; the read-back verify (shift the microword IN)
cd7e  700c            moveq #12,D0
cd80  41f9 001144F0   lea 0x001144F0,A0           ; the 128-bit microword buffer
cd86  0c70 0100 0800  cmpi.w #0x0100,(0x00,A0,D0.l)   ; buffer word[6] must be 0x0100
cd8c  6700 00a4       beq  0xCE32                 ; ... and that is the PASS path
cd90  2256            movea.l (A6),A1             ; failure: build the print argument block
cd92  45f9 001144F0   lea 0x001144F0,A2
cd98  234a 0014       move.l A2,(0x14,A1)         ; point it at the buffer
cd9c  42a9 0018       clr.l (0x18,A1)
cda0  7207            moveq #7,D1
cda2  2341 001c       move.l D1,(0x1C,A1)         ; 8 words to dump
cda6  337c 00ff 0020  move.w #0x00FF,(0x20,A6)    ; <-- the "CSA" value, an IMMEDIATE
cdac  6100 f300       bsr  0xC0AE                 ; the failure printer
```

**`0x00FF` is a literal in the EPROM.** It is a canned marker stored just before the failure print,
not a control-store address, not a read-back word, and not a measurement of anything. There is no
off-by-one to hunt and never was - the number carries no information at all.

**The actual pass condition is one line earlier:** after `jsr 0x775A` shifts the microword back in,
**word[6] of the buffer at `0x001144F0` must read `0x0100`.** Nothing else is compared. (Word[6] is
the field the load test already noted as incrementing from record to record in the EPROM's own
microcode blob.)

That is the concrete, checkable thing the link now has to satisfy, and it is what "run it" reduces
to on this test. The three calls immediately before the verify - `0x7A66`, `0x78B2`, `0x7A84` - are
the start/stop microprogram operations that are supposed to make word[6] come back as `0x0100`;
carving those is the next step.

### WHY word[6] never comes back as `0x0100` - three gaps in the link [V, ROM read 2026-08-04]

The three calls before the verify carve cleanly. `0x7A66` is START, `0x7A84` is STOP (they set and
clear the same flag at `0x001143AC`), and `0x78B2` is the known `0x660000` bit-4 spin. The real work
is in `0x78CA` (start) and `0x795A` (stop):

```
78ca  4e56 ffe8      link A6,#-0x18
78d2  3d40 0014      move.w D0,(0x14,A6)          ; the start ADDRESS parameter (0 from this test)
78d6  6100 fe0e      bsr 0x76E6                   ; address phase - same one the 0x0018 path uses
78da  1039 001144EF  move.b (0x001144EF).l,D0     ; shadow of a SECOND latch byte
78e2  0881 0001      bclr #1,D1
78e6  13c1 00330001  move.b D1,(0x00330001).l     ; ... which lives at 0x330001
78ec  1439 001144EE  move.b (0x001144EE).l,D2     ; shadow of the KNOWN latch
78f2  08c2 0002      bset #2,D2
78f6  13c2 00330000  move.b D2,(0x00330000).l     ; gate on, as usual
78fc  33fc 0017 0022 0000   move.w #0x0017,(0x00220000)   ; COMMAND 0x0017
7904  33fc 0015 0022 0000   move.w #0x0015,(0x00220000)   ; COMMAND 0x0015
790c  13f9 001144EE 00330000    ; gate off from the shadow
7916  0000 005c      ori.b #0x5C,D0               ; ... and 0x330001 gets bits 2,3,4,6 set
791a/7920            ; shadow and latch 0x330001 updated
7926  0c79 5400 001131F8   cmpi.w #0x5400,(identity)   ; an ND-5400 / ND-5500 only tail follows
```

Three things the link does not do:

1. **Commands `0x0017` and `0x0015` are unknown to it.** `Nd5000ControlStoreLink.WriteCommand`
   recognises `0x0018`, `0x2018`, `0x2010` and `0x2011` and nothing else. These two are the
   start-microprogram pair; the stop path at `0x795A` issues its own. **No microprogram ever runs,
   so nothing can update word[6].** This is the direct cause of the CSA failure.
### THE NINTH-GATED-WORD ADDRESS MODEL IS ENTANGLED WITH A BUG [V, measured 2026-08-04] - READ THIS

Splitting the two latch bytes exposed something bigger, and it undermines fact 2 in the MEASURED
list above. **Both gate bits provably live in `0x330000`:**

```
7434  08f9 0002 001144EE   bset #2,(0x001144EE)   ; console 0x741E - gate bit 2
765a  08f9 0001 001144EE   bset #1,(0x001144EE)   ; boot    0x764E - gate bit 1
      13f9 001144EE 00330000                      ; BOTH store that shadow to 0x330000
```

So gating on `0x330000` alone is the correct model - **and it fails.** The typed
`LOAD-CONTROL-STORE` then stages nothing at all. Measured over a boot plus one typed command:

| Byte | Writes | Settles at |
|---|---|---|
| `0x330000` | **208** | `0x00` |
| `0x330001` | **42,297** | `0x52` (bit 1 SET; flips `0x53`/`0x13`, bit 6 is the AOB strobe) |

**The odd byte holds bit 1 set almost permanently, so folding the two bytes together held the gate
open across the entire shift sequence - by accident.** Every word therefore looked "gated" and the
address looked like a ninth gated word. Split the registers correctly and those words shift out
UNGATED and never reach the staging buffer.

The ROM agrees that the model is wrong: the address phase is its own routine (`0x76E6`) called
**before** the gate opens, in both the console path and the start path. That is not "the ninth gated
word".

> **RESOLVED the same day - the fold is GONE.** Gating is now `0x330000` only, which is what the
> ROM says. What made it possible was carving the address phase; see below.

### THE ADDRESS PHASE, CARVED - and it replaces the ninth-word model [V, ROM + measured 2026-08-04]

`0x76E6` is called by EVERY control-store path (console `0x741E`, boot `0x764E`, start `0x78CA`)
**before** the gate opens:

```
76ee  3d40 0014            move.w D0,(0x14,A6)              ; the address parameter
76f2  207c 00220000        movea.l #0x00220000,A0
76f8  303c 0010            move.w #0x0010,D0                ; loop count = 16
7704  33ee 0014 00550000   move.w (0x14,A6),(0x00550000).l  ; the address, written ONCE
770c  3084 / 3085          move.w D4,(A0) / move.w D5,(A0)  ; 16 clock pairs
7714  30bc 3010            move.w #0x3010,(A0)              ; LATCH IT AS THE ADDRESS
7718  1439 001144EF ...    bclr #1 -> (0x00330001)          ; bracket the strobe
7728  30bc 0015            move.w #0x0015,(A0)              ; the strobe
772c  13f9 ...             restore 0x330001 from its shadow
```

Three corrections fall out:

1. **`0x3010` is the address-latch command.** The word most recently written to `0x550000` is the
   control-store address. Explicit, so it no longer depends on the gate being open at the right
   moment - which is exactly what let the fold be removed.
2. **`0x0015` is a generic STROBE, not a microprogram run.** It is issued here, by every access. My
   first pass named it `CommandMicroprogramRun` because it follows `0x0017` at `0x7904`; a typed
   `LOAD-CONTROL-STORE` then logged two "MICRO-RUN" entries with no `0x0017` anywhere, which
   refuted it. Renamed `CommandStrobe`.
3. **The shift ring needs NINE slots, not eight.** The address goes through the same `0x550000`
   port right after the eight halves, so with eight slots it evicted the first half and the
   microword committed one word out of step - measured as `33445566...F0010100` stored where
   `11223344...DDEEF001` was expected.

**Result, measured:** the typed command now stores `112233445566778899AABBCCDDEEF001` at `0x0100`,
and a boot performs **8** addressed control-store writes where the folded model saw 4. MIR loads
unchanged at 284.

2. **`0x330001` is a SECOND latch byte and the link folds it into the first.**
   `Nd5000LinkWindow.WriteByte` calls `_link.WriteLatch(value)` for the `Latch` role *without
   passing the address*, so writes to `0x330000` and `0x330001` land on one register. The start/stop
   sequence drives `0x330001` bits 0, 1, 2 and 3 and ORs in `0x5C`, entirely independently of the
   gate bits on `0x330000`.
   > **This is the same defect shape as the `0x2018`/`0x0018` conflation** - two distinct registers
   > routed to one handler, invisible because the paths that exercised it only ever used one of
   > them. Worth fixing on its own merits even before the start/stop work.
3. The address phase runs, so a microprogram start **has a control-store address** (0 in this test).
   Whatever models the start must take it from the same staging the `0x0018` path uses.

Also noted, not a gap: the tail at `0x7926` only executes for identity `0x5400` / `0x5500`
(ND-5400 / ND-5500). The default configuration reports ND-5800, so it is skipped - but a test that
sets a different identity word will take a different path through both start and stop.

> **Method note worth keeping.** Three hand-decodes of this message produced three wrong answers.
> Two instruments - a word watch and a PC trap, both in `AccpMachine` and both off by default -
> settled it in about twenty seconds of run time each. When a value cannot be traced statically,
> stop reading and measure.

The generic mismatch path at `0xBECC` (descriptor `0x11B1C`) remains a different site; the two were
conflated once already.

---

## STATE OF RECORD

- Commit `c1e73a711` - the `0x2018` / `0x0018` split. ACCP suite **135/135, 0 failed**.
- Commit `6dc2ad5fb` - the AFLAG seam (`AflagAtrapBit = 5`; FATAL is a trap-word payload).
- Commit `55aa3edce` - the control-store link itself.
- Companion docs: `ACCP-EMULATION-STATUS-AND-HANDOFF.md` (headline section),
  `ANSWERS-ACCP-CPU-SEAM-CONTRACT-2026-08-04.md`, `ACCP-COMPLETE-REFERENCE.md` sections 2.4l-2.4n.
