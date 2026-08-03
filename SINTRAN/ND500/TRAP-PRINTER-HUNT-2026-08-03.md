# Hunting the trap printer: what renders "At program address: 1 10533B"

**Date:** 2026-08-03
**Goal:** find what value the ND-500 trap report is handed for "At program address", because
`1 10533B` does not map to any swapper PSEG instruction boundary and six explanations are
already dead.
**Status:** printer region located and bounded; the exact field source not yet pinned.

---

## 1. Premise verified, not assumed

**The message is printed by the ND-500 monitor program (`MON-DEBUG:PROG`), not by SINTRAN.**
Searched every carved segment in `versions/L-VSX-500/segments/` and both resident images for
the literal `At program address` - **zero hits**. The only image in the tree containing it is
`nd-500-mon-j04-bank2.bin`.

This mattered because the running text arrives interleaved with SINTRAN's own output, and the
notes elsewhere say "SINTRAN prints a PROTECT VIOLATION for shadow process 5SWAP". SINTRAN
prints *that* line; the field report underneath it is the monitor's.

## 2. The string-output primitive `[V]`

**`147211`** - print a byte substring. Verified by reading the loop and by the pointer word
`bank1[154371] = 147211`:

```
147211  entry (JPL -> 177300 ENTER)
147214  LDA ,B -171 / STA ,B -167     start byte index
147216  LDA ,B -170 / STA ,B -165     end index
147224  LDX ,B -167                   loop:
147225  LDT ,B -172                     T := string base (a WORD address)
147226  LBYT                            A := byte (T,X)
147230  SAT 1                           T := 1  (output device)
147232  MON 2                           OutByte
147233  MON 65                          error -> QERMS
147235  X+1 -> B-167
147240  SKP IF DX EQL ST / JMP 147224   until end
```

Parameters are a **word base + byte start + byte end**, passed as the 3-word `F` register
(`STF ,X 6`) - the caller builds it with `SAA <len>` / `SWAP CLD SA DD` / `LDT <base>`.

**It has exactly 25 call sites, all inside `151156`-`154363`.** That bounds the whole report
printer to about 1700 words.

## 3. The numeric renderer `[V]` - this is what produces `10533B`

`154251`-`154365`. Reading it directly:

```
154261  LDX ,B -155                  field index
154262  LDA I ,B ,X -150             <- fetch field value from an ARRAY at B-150
154263  SAD SHR 20                   take the high half (shift right 16)
154265  SAA 5 / STA ,B -153          digit counter = 5 (six digits)
154267  loop:  LDD ,B -130
154272    LDD 100 / RAND             mask
154303    SBYT                       store one digit byte into the buffer at B-126
154305    SAD SHR 3                  shift right 3  <- OCTAL
154324    counter-1, repeat until 0
154330  SAA 5 / SWAP / LDT ,B -126   F := (buffer, 0, 5)
154335  JPL -> 154371 = 147211       print the six digits
154346  LDX ,B -155 / X+1
154351  SAT 7 / SKP IF DX EQL ST     <- loop over EIGHT fields
```

Two things worth having:

- **`SAD SHR 3` with a mask and `SBYT` is an octal digit emitter.** This is where the `B`-suffixed
  numbers in the report are rendered.
- **`LDA I ,B ,X -150` with the outer loop bounded by `SAT 7` means the report prints EIGHT
  numeric fields out of an array at `B-150`.** "At program address" is one entry of that array.
  Finding which index, and where the array is filled, is the remaining step - that is where the
  answer to `1 10533B` lives.

---

## 4. THREE SEARCH METHODS THAT ALL RETURNED NOISE - do not repeat them

Every one of these produced a confident-looking result that was an artefact. Recording them
because each cost real time and the third one nearly got published.

1. **Scanning bank 1 for a word equal to a bank-2 string address.** The pool lives around
   `0o050100`-`0o052100`, and `0o050xxx` **is the `LDT` opcode**. Every "pointer" found in the
   printer region disassembled as an ordinary `LDT <disp>` instruction. Earlier the same method
   over a wider range returned **1217 hits**.

2. **Scanning for a 151-word window of plausible code addresses** (looking for a dispatch
   array): 8 false candidates in bank 1, 65 in bank 2. The bank-2 ones were ASCII text whose
   word values happen to land in the code-address range.

3. **Scanning for `SAA <ascii>` immediates of the characters a template parser must handle**
   (`$ : D I O B`). All six landed within `146472`-`146673`, which reads as an overwhelming
   signal - and is **the MON 60 thunk table**. That table is a dense run of `SAA 0o000`
   .. `SAA 0o177`, so *every* character code in the ASCII range appears in it exactly once,
   adjacent, by construction. The cluster was guaranteed by the table's shape and carried no
   information at all.

**The common failure:** searching for a VALUE in a range that ordinary ND-100 instruction
encodings already occupy, or that a dense table already spans. On this architecture a value
scan is only decisive when the constant is distinctive *and* the hit count is tiny - the one
that worked all session was `0o011547`, which occurs exactly once.

**What worked instead, both times:** search for the *behaviour*, not the data. The command
dispatch fell out of the one-occurrence descriptor-array base; the printer fell out of the
`MON 2` (OutByte) call inventory - bank 1 has only 68 `MON` instructions total and only 5 are
`OutByte`, so the output path was a 5-way choice rather than a 1217-way one.

---

## 0. ANSWERED - and the manual was in the repo the whole time `[V]`

**`ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE` chapter 6 documents this exact report, with two
worked examples.** No carving was needed.

### 0.1 The address format, proven by the manual reporting ONE fault TWO ways

Page 114 prints the user-facing report:

```
*** ND-5000 HARDWARE FAULT ***
At program address:        1                   31B
From CPU in slot position:                      6D
Logical address:           1             466414B
MEMORY MANAGEMENT STATUS:                      5B
DATA POFF read request
Physical address:         13       137776414B
Physical segment:                             8D
WR:                         13771B
ACCP status:                62750B
BADAP:                        140B
```

Page 115 prints what **the same fault** writes to the error device:

```
ND-500/5000 trap number: 51B at: 1000000031B ND-500/5000 Hardware fault
```

`1` and `31B` are the two halves of **`1000000031B`**. So:

> **full VA = (first number) x `0o1000000000` + (second number, octal)**

`0o1000000000` = `0x08000000` = one segment. The first number is the **segment**, the second the
**byte offset**.

**Therefore `1 10533B` = VA `0o1000010533`** - segment 1, byte offset `0o10533`. That is exactly
the reading the swapper `.asm` uses, and it is now **verified against the manual instead of
assumed**. The standing "the frame and units of that address are genuinely UNKNOWN" caveat is
**closed**.

Corroborated by the manual's second example, a swapper page fault on page 109:
`At program address:  1  2242B` - same shape, same segment 1, for the same process we are
debugging.

### 0.2 The all-zero fields are EXPECTED, not evidence of a bug

Our trap reports logical `0:4`, MMS status `0`, physical `0`, physical segment `0`, WR `0`, and
that was written up as "what decoding an UNFILLED message buffer looks like". **The manual says
otherwise, in as many words** (page 115):

> "if a memory error occurs during the final read/write access, the MMS status will not have
> been locked, or the Logical address or WR register."

So for a fault taken on the **final access** - as opposed to during an MMS table lookup - those
registers are simply **not latched**, and reading zero from them is the documented behaviour.
Our field pattern matches that case exactly.

**Consequence:** the zeros are not a servicer defect and never were. The "phantom from a trap
message our servicer never populated" hypothesis is dead, killed by documentation rather than by
measurement.

### 0.3 THE FAULTING INSTRUCTION IS `RPHS` AT `1000010525` - and the old search looked for the wrong opcode

```
1000010525: 377 365 304    rphs   $1777777777777777777704   <- the physical read
1000010530: 010 001        h1 :=  $1
1000010532: 115 054        w set1 $54
1000010533:                                                  <- reported "At program address"
```

**`RPHS` = Read from PHysical Segment**, the swapper's swap-in page read
(`swapper-k01-deep-analysis.md:297`). It reads a *physical* segment, i.e. **a data read that
bypasses paging** - which is precisely what the trap line "DATA POFF read request" says.

**There are exactly TWO `RPHS` instructions in the entire 38161-byte PSEG**, at `1000010316` and
`1000010525`. The reported address `1000010533` is **6 bytes past the second one**; the first is
`0o215` bytes away and not a candidate. So the identification is essentially forced.

**Why this was missed for days.** The standing note reads:

> "there is **no `dmof`** between `0o2364` and `0o11013`, so the 'DATA POFF read request' cannot
> originate in that region under the current disassembly"

That is **true about `dmof` and wrong as a conclusion.** `dmof`/`dmon` (turn data paging off, then
on) is *one* way to produce a paging-off read. **`RPHS` is another, and it needs no `dmof` at
all** - it names a physical segment directly. The search was for the wrong opcode, so a region
containing the only two physical-read instructions in the program was excluded as impossible.

This also retires the routine at `1000012503` (`dmof` / `w1 * $4000` / `bmove` / `dmon`), which was
blamed by *behaviour* and never matched the reported address. It is not the faulting site.

### 0.35 Where `RPHS` gets its zero, byte-verified

Both `RPHS` sites share an identical five-instruction setup, differing only in the transfer count:

```
1000010300: w3 := b.30        1000010507: w3 := b.30
1000010302: w3 * $400         1000010511: w3 * $400
1000010306: w1 := $400        1000010515: w1 := $200     <- only difference
1000010312: w2 := b.34        1000010521: w2 := b.34
1000010314: w4 := b.24        1000010523: w4 := b.24
1000010316: rphs              1000010525: rphs
```

**The physical address is `b.30 x 0o400`.** So `b.30 = 0` gives physical address 0 - which is
exactly the "Physical address 0 / Physical segment 0" the trap reports.

**`b.30` is a parameter of the routine**, not a local it computes: the routine entry is
`1000010432 ents $504`, and although `b.30` is written at 51 sites across the PSEG, **none of them
lies between `10432` and `10507`**. Within this routine it is read-only.

So the zero is supplied by the caller (`1000011701` / `1000063066`), and traces back to the swap
page that was read as all zeros.

### 0.4 The whole chain now closes

`RPHS` is the **swap-in page read**. The swapper is reading a page in, using a page number taken
from a swap-file page that was never written (the file is virgin - measured, and the read of its
first page returns zeros). Page number zero -> physical read from physical page 0 -> protect
violation. Every measured symptom is accounted for:

| Symptom | Explanation |
|---|---|
| "DATA POFF read request" | `RPHS` is a paging-off physical read `[V]` |
| "Physical address 0 / Physical segment 0" | the page number read out of the virgin swap page is 0 `[V]` |
| Logical/MMS/WR all zero | not latched for a fault on the final access - manual p.115 `[V]` |
| PC 6 bytes past the instruction | pipeline run-ahead `[H]`, see below |

### 0.5 The 6-byte offset

`[H]` - the reported address is **not** the faulting instruction's start; it is 6 bytes (three
instructions) beyond it. The ND-5000 is pipelined and we already know instructions are staged on
the I-level ahead of execution (`ND-05.022.1` section 7.3.4, the EXUC sneak-cycle rules), so a
program address that has run ahead of the faulting operation is the expected shape.

Not proven, and it does not need to be for the identification above to hold - that rests on
`RPHS` being the only paging-off read in range. But **anyone reading a trap PC on this machine
should treat it as approximate**, which is consistent with the manual describing the field only as
"Program address | What kind of instruction" rather than as an exact fault PC.

### 0.6 What the puzzle WAS

The format is settled, so the anomaly is real and narrow: **VA `0o1000010533` is mid-instruction**
in a listing whose every length was hand-verified, in a build proven SHA256-identical to what
runs.

Two candidates remain, and the manual leans on neither:

1. A length defect in the disassembly *before* `0o10533` that the hand-verification from
   `0o10432` did not cover.
2. **The ND-5000 reports the address of the memory reference in progress, not the instruction
   start.** This is a hardware-fault trap taken on a final read/write access - the microcode may
   well latch the operand address. `[H]` - the manual's field table says "Program address | What
   kind of instruction", which reads as instruction-start and argues *against* this, so it is a
   hypothesis and not a conclusion.

---

## 4a. The chain from printed text back to the field array `[V]`

Traced end to end after the section-4 dead ends, this time by following callers rather than
scanning for values:

```
trap handler
  132304   STA ,B -146        B-146 := (B - 0o104) + 200      <- the ONLY write
  132637   JPL -> 151521      call the report printer, passing
                                param1 = (base = [B-146], count = 8)
                                plus three more 3-word params

report printer  151521 .. ~154404   (ONE routine, ~1700 words)
  25 calls to 147211 (print byte substring)
  154251..154365  numeric renderer:
      154262  LDA I ,B ,X -150     fetch field[X] through the array pointer
      154303  SBYT                 emit a digit
      154305  SAD SHR 3            octal
      154335  JPL -> 147211        print the six digits
      154351  SAT 7                loop bound = EIGHT fields
```

Four facts that make this solid rather than suggestive:

1. **The printer has exactly ONE caller** (`132637`). Not a shared utility - it is the trap
   report and nothing else.
2. **`B-146` has exactly ONE writer** (`132304`). The array location is not reassigned.
3. **The count passed by the caller (8) matches the loop bound in the callee (`SAT 7`)**, derived
   independently at each end.
4. The renderer's `SAD SHR 3` + mask + `SBYT` is unambiguously an octal digit emitter, which is
   what puts the `B` on `10533B`.

**So the eight printed numeric fields live at `(B - 0o104) + 200` in the trap handler's frame**,
and "At program address" is one of those eight words.

## 4b. What is still missing

**Which index, and what writes the array.** `132461` loads the pointer and reads `array[8]` -
one *past* the eight printed entries - so the structure is at least nine words. The fill is
reached through `132457 JPL -> bank1[132517] = 123515`, a routine that computes a count from
`B-167`/`B-170`, scales it, and passes two buffer addresses plus a double to a MON 60 thunk at
`123556`.

**That last step is NOT yet trustworthy** and is deliberately left unclaimed: `132440` is a
conditional (`JAZ`) and I have not established that `132451`-`132466` is on the trap path rather
than a sibling branch. Given three noise results already recorded in section 4, an unverified
fourth reading is worth less than an honest gap.

---

## 5. Next step

Establish which of the eight words at `(B - 0o104) + 200` is "At program address", and what
writes them.

Two routes, both bounded:

- **Static:** settle whether `132451`-`132466` is on the trap path (the `JAZ` at `132440`), then
  read `123515`'s MON 60 thunk at `123556` to see what it fetches from the ND-500.
- **Dynamic, and probably cheaper:** the printer is a single routine with a single caller and a
  known 8-word array. A breakpoint at `132637` in the boot harness dumps all eight values at the
  moment of the fault, which gives the index by inspection instead of by inference - and would
  also show whether `1` and `10533B` are two fields or one.

The dynamic route is worth preferring here: three of the four static searches in section 4
produced confident-looking noise, and this one has a live reproduction available.
