# Microcode answer: where PSTP and the program SEGMENT come from at swapper start

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\MICROCODE-ANSWER-PSTP-AND-SEGMENT-2026-07-20.md`
**Date:** 2026-07-20
**Answered by:** the ND-5000 microcode-CPU track (B30 image)
**Answers:** questions B4 (task 2.2, PSTP) and B3/A (task 2.3, segment number) of
`QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md`.

## Sources (every citation below is line-anchored to these files)

- B30 decoder table: `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`
  (16384 microwords; addresses are OCTAL as printed in the listing).
- Mailbox pseudo-C: `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`.

All microword text below is quoted verbatim from the decoder table. "@NNNNNN" is the
octal microword address as it appears in column 1 of that table.

---

## The one fact that reframes both questions: functions 20 and 21 are ILLEGAL on B30

The 3RREG (20) / 3WREG (21) register read/write functions do NOT exist as register
movers in this image. The MICFU dispatch table routes both to the reject handler:

- `MAILBOX-MICROCODE-PSEUDOCODE.md:164` -- `| 20,21 | MSG_ILLEG |`.

So on this ND-5800 B30 microcode SINTRAN CANNOT hand the CPU a live register image
through 21B; that mailbox function is rejected with 5ERANSWER. This is consistent with
the project memory note "21B = MSG_ILLEG on B30 microcode". Therefore the 21B image the
emulator inspects is NOT the structure the microcode reads at start; the real register
state (P, L, X/A/E regs, PS, DOM) is loaded from a MEMORY-RESIDENT CONTEXT BLOCK by the
23B (3START) handler. That single fact answers most of both questions.

---

## The real 3START path (MICFU 23 = MSG_START)

`MSG_START @015671` checks CPU availability then falls to `MSG_START1 @015674`, which
pushes the context loader and then jumps to the macro-execution entry:

- `MICRO-5800-B30.md:7111` -- `@015671 MSG_START ... [ADDR=CPU_AVAIL?]`
- `MICRO-5800-B30.md:7114` -- `@015674 MSG_START1 ... T,PUSH ... [ADDR=NEWCNTXT]`
- `MICRO-5800-B30.md:7115` -- `@015675 ... [ADDR=EXECUTE]`

`NEWCNTXT @014660` selects the context by the CURRENT PROCESS word SRF11, then calls
`CNTXTLOAD`:

- `MICRO-5800-B30.md:6590` -- `@014660 NEWCNTXT ALU,A A,SRF11 B,X1 D,SC12 ...`
  (SC12 := SRF11 = current process number; for the swapper this is process 0).
- `MICRO-5800-B30.md:6594` -- `@014664 NEWCNTXT1 ... [ADDR=CNTXTLOAD]`

`CNTXTLOAD @014742` scales the process index (repeated `A+B,*2`) and forms the context
block physical address DPA = index + OFFSET base, then streams the block out of memory:

- `MICRO-5800-B30.md:6640-6643` -- `@014742..@014745 CNTXTLOAD ALU,A+B,*2 A,SC12 B,SC12 D,SC12`
  (four doublings -> per-process stride).
- `MICRO-5800-B30.md:6644` -- `@014746 ... EXUC ... [ADDR=OFFSET]` -> OFFSET helper
  `@000020` returns SC13 := 0o4000 (`MICRO-5800-B30.md:30`).
- `MICRO-5800-B30.md:6645` -- `@014747 ALU,A+B A,SC12 B,SC13 D,DAC,DPA ...`
  (context-block DPA := process_index + 0o4000).
- `MICRO-5800-B30.md:6646-6669` -- `@014750..@014775` a run of `RD,POF ... D,X1/X2/.../A1../E4`:
  the whole X/A/E register file is READ from that memory block (`ADACT ... ORCON` stepping).

P and L are loaded from the block, NOT from any register mailbox:

- `MICRO-5800-B30.md:6668` -- `@014776 ... A,DATA ... D,SC3 ... RD,POF` (context word -> SC3)
- `MICRO-5800-B30.md:6653` -- `@014757 CNTXTLOAD1 ALU,XOR A,SC3 B,SC14 D,IAC,P ...`
  (**P := SC3, i.e. P comes from the context block**)
- `MICRO-5800-B30.md:6654` -- `@014760 ... D,IAC,L ...` (**L := SC4 from the block**)

---

## Q1 (B4 / task 2.2) -- Where does PSTP come from at swapper start?

### VERDICT: CONFIRMED -- PSTP is a MACHINE-GLOBAL set by microcode, NOT per-process, NOT in any register/context handed over by SINTRAN.

PSTP (`MM,PSTP`) and PUWP (`MM,PUWP`) are written by the microcode in exactly two
places, both to FIXED values, and NEITHER is inside the 23B context-block load:

1. CPU / SAMSON initialization -- `INIT_SAM_3`:
   - `MICRO-5800-B30.md:6536` -- `@014572 INIT_SAM_3 ALU,A A,SC3 B,X1 D,MM,PUWP ...`
     (PUWP := SC3)
   - `MICRO-5800-B30.md:6537` -- `@014573 ALU,A A,SC13 B,X1 D,MM,PSTP ...`
     (**PSTP := SC13**). SC13 here is the constant returned by the `PSTBASE` helper
     `@000021 PSTBASE ALU,A A,LARG LARG=00000000002 B,X1 D,SC13 T,RETURN`
     (`MICRO-5800-B30.md:31`), reached from `@014567 ... [ADDR=PSTBASE]`
     (`MICRO-5800-B30.md:6533`). So at init **PSTP := 2** (a fixed physical convention
     baked into the control store, i.e. "PST base = physical page 2 << ...").

2. The macro cold-start reset sequence (see Q2) -- `@017731`:
   - `MICRO-5800-B30.md:8167` -- `@017731 ALU,FZRO A,BM00 B,X1 D,MM,PSTP ...`
     (**PSTP := 0**)
   - `MICRO-5800-B30.md:8168` -- `@017732 ALU,A A,BM02 B,X1 D,MM,PUWP ...`
     (PUWP := BM02 = 4)

Crucially, the 3START context loader (`CNTXTLOAD`, `@014742`-`@015016`) writes P, L,
X1-X4, A1-A4, E1-E4, MOD, PS, CED, CAD -- but there is **NO `D,MM,PSTP` and no
`D,MM,PUWP` anywhere in that block**. PSTP is therefore never reloaded per process.

Answering the question's menu directly: PSTP is "set once at CPU init long before any
process starts" (from a fixed control-store constant via `PSTBASE`), and additionally
forced to a fixed value by the macro cold-start path. It is NOT in the control-store as
a per-process value, NOT in the register image, NOT in the process context block.

**Emulator consequence:** stop trying to source PSTP from the 21B image (it cannot be
there -- 21 is illegal). Seed `MM,PSTP` at CPU-init time to the fixed convention and
leave it untouched across process starts. The two candidate seed values the microcode
uses are `2` (init/PSTBASE) and `0` (macro cold-start). Which one is live at swapper
start depends on which start path SINTRAN drives (see the UNKNOWN section).

---

## Q2 (B3/A / task 2.3) -- Where does the program SEGMENT number come from at 3START?

### VERDICT: CONFIRMED -- the segment is in a SEPARATE register (PS = current program segment), loaded independently of P. P carries ONLY the offset. On 3START, PS is loaded from the context block; in the macro cold-start path PS is hardcoded to 1.

The architecture separates the two: `IAC,P` holds the offset, `MM,PS` holds the current
program segment. Instruction fetch forms the virtual address from PS (segment) plus P
(offset), which is why P = 0x00000004 with PS = 1 yields fetch VA 0x08000004
(segment 1, base 0x08000000). The microcode sets P and PS in two different instructions
from two different sources.

On the 3START context-load path, PS is loaded from a word in the process context block:

- `MICRO-5800-B30.md:6670` -- `@015000 ... A,DATA ... D,SC5 ... RD,POF` (context word -> SC5)
- `MICRO-5800-B30.md:6673` -- `@015003 ALU,XOR A,SC5 B,SC14 D,SC13 ...` (SC13 := SC5)
- `MICRO-5800-B30.md:6674` -- `@015004 ALU,XOR A,SC13 B,SC14 D,SRF13 ... [ADDR=NEW_PS_1]`
- `MICRO-5800-B30.md:6705` -- `@015043 NEW_PS_1 ALU,A TYP,HW A,SC13 B,X1 D,MM,PS ...`
  (**PS := SC13 = SC5 = a halfword from the context block**)
- `MICRO-5800-B30.md:6706` -- `@015044 ... D,MM,PHS ...` (physical-segment shadow := same)

So the microcode "derives the code segment" neither from P nor from a capability lookup
at start: it takes PS verbatim from the context block. CED/CAD (current / alternative
domain) are likewise loaded from the block right after:

- `MICRO-5800-B30.md:6679` -- `@015011 ... [ADDR=NEW_CED]`
- `MICRO-5800-B30.md:6680` -- `@015012 ... [ADDR=NEW_CAD]`

This is the direct answer to the sharpened question in the QUESTIONS doc ("SINTRAN's
21B image sends P = 0x00000004 ... yet the swapper runs at 0x08000004 ... something
supplies the segment number 1"): the segment number lives in the process CONTEXT BLOCK
that 3START reads from memory, in the PS halfword -- NOT in the 21B register image the
emulator was inspecting. The emulator is reading the wrong structure.

### The alternate anchor: the macro cold-start hardcodes exactly P=4, PS=1

There is a second, entry-vector cold-start path that produces the observed values as
literal constants. `MACRO_STARTL @000033` (an ACCP "execute microroutine" vector)
jumps to `MACRO_STL1 @017713`, whose tail sets the MMU register file to fixed constants:

- `MICRO-5800-B30.md:8167` -- `@017731  MM,PSTP := 0`
- `MICRO-5800-B30.md:8168` -- `@017732  MM,PUWP := 4` (BM02)
- `MICRO-5800-B30.md:8169` -- `@017733  ALU,A A,BM00 B,X1 D,MM,PS ...` -> **MM,PS := 1** (BM00)
- `MICRO-5800-B30.md:8170` -- `@017734  MM,DOM  := 1`
- `MICRO-5800-B30.md:8171` -- `@017735  MM,ADOM := 1`
- `MICRO-5800-B30.md:8172` -- `@017736  MM,PHS  := 1`
- `MICRO-5800-B30.md:8175` -- `@017741  ... EXUC ... [ADDR=MACRO_SETP]`
- `MICRO-5800-B30.md:45` -- `@000037 MACRO_SETP ALU,A A,LARG LARG=00000000004 B,X1 D,IAC,P`
  -> **IAC,P := 4**

That is P := 4, PS := 1, DOM := 1 as pure microcode literals -- a byte-for-byte match
with the emulator's observed swapper state (segment 1, offset 4). If SINTRAN starts the
swapper via this cold-start vector rather than via mailbox 23B, then NEITHER the segment
NOR P comes from SINTRAN at all -- both are microcode constants, and PSTP is 0 (not 2).

**Emulator consequence:** model PS as a distinct register. Do not try to extract a
segment from the 32-bit P value. Either (a) load PS from the context-block PS halfword
when honoring 3START, or (b) if you emulate the macro cold-start, hardcode PS := 1,
P := 4, DOM := ADOM := 1, PSTP := 0, PUWP := 4 exactly as the microwords above do.

---

## What remains UNKNOWN

1. **Which of the two start paths SINTRAN actually drives for the swapper on the 5800.**
   The microcode contains both:
   - mailbox MICFU 23 (`MSG_START` -> `NEWCNTXT`/`CNTXTLOAD` -> P and PS from a
     memory-resident context block), and
   - the ACCP entry-vector cold-start (`MACRO_STARTL @000033` -> P=4, PS=1, PSTP=0 as
     literals).
   I cannot prove from the microcode alone which ACCP command / mailbox function SINTRAN
   issues for the swapper on this generation -- that is a SINTRAN-side / ACCP-firmware
   carve (the octobus/ACCP track). The values match BOTH paths, so the observation does
   not disambiguate. Given that 20/21 are illegal and the console prints "> Loading
   Swapper" via the control-store/micro-clock path, the cold-start vector is plausible
   but UNPROVEN here.

2. **The exact byte layout of the 3START context block.** I confirmed the READ ORDER and
   destinations (P from the word read at `@014776`, L at `@014777`, PS from `@015000`),
   and that the block base = process_index + OFFSET(0o4000). I did NOT independently
   verify the absolute byte offset of each field against a SINTRAN symbol table; the
   ORCON displacement stepping is present in the listing but I did not tabulate every
   field-to-offset mapping. If SINTRAN builds this block, its field layout is the thing
   to cross-check.

3. **Where the context block physically lives** (ND-500 local memory vs the shared MPM
   window). `CNTXTLOAD` uses `RD,POF` at DPA = index + 0o4000; whether that DPA lands in
   the shared window or in local memory is not decidable from the microword text alone.

4. **PSTP seed disambiguation.** Init sets PSTP := 2 (via PSTBASE); the macro cold-start
   sets PSTP := 0. Which value is live when the swapper's first instruction executes
   depends on UNKNOWN 1 above.

5. **PUWP semantics.** Confirmed it is written (SC3 at init; 4 in cold-start) but its
   use by the address translator was not traced here.
