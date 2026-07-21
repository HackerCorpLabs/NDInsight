# Microcode answer: 21B/20B register order and word packing (B1 / B2, task 2.4)

**Date:** 2026-07-20
**Answered by:** the ND-5000 microcode-CPU track (B30 image)
**Question source:** `SINTRAN/ND500/QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md`, sections B1 and B2
**Rule followed:** no guessing. Every claim below is either grounded in an exact file+line
citation, or explicitly marked UNKNOWN / NOT IN MICROCODE.

---

## Headline result (read this first)

On the **B30 (ND-5800) microcode image**, mailbox functions **16, 17, 20 and 21** - i.e. the
single register EXAMINE (16), single register DEPOSIT (17), register-block READ (20 = 3RREG) and
register-block WRITE (21 = 3WREG) - **all dispatch to `MSG_ILLEG`**. The handler bodies do not
exist. The microcode therefore **never reads a register block and never packs two ND-100 halfwords
into a 32-bit register**.

Consequence: **B30 cannot answer B1 (register order) or B2 (word packing), because it does not
implement the path.** This is not "unknown because I did not find it" - it is verified-absent. Both
answers below are therefore `NOT IN MICROCODE` for B30, plus the best cross-reference I can ground.

This is fully consistent with the standing carve note "This 5800 image DISABLES classic
(21B=MSG_ILLEG on B30 microcode)".

---

## Evidence: the MICFU dispatch table (byte-verified in the raw decode)

The 64-entry jump table is at `015224` (`MSG_00`), indexed by MICFU in OCTAL, reached by `JMPREL`
from `MSG_LINK9` (`015222-23`).

Raw microcode `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`:

```
line 6815  015221  MSG_ILLEG:  ALU,A A,BM02 B,X1 D,SC10 T,JMP ... [ADDR=MSG_END]   (the reject stub)
line 6831  015241  MSG_15:  ... T,JMP ... [ADDR=MSG_ILLEG]
line 6832  015242  MSG_16:  ... T,JMP ... [ADDR=MSG_ILLEG]     <- register EXAMINE  (fn 16)
line 6833  015243  MSG_17:  ... T,JMP ... [ADDR=MSG_ILLEG]     <- register DEPOSIT  (fn 17)
line 6834  015244  MSG_20:  ... T,JMP ... [ADDR=MSG_ILLEG]     <- register READ  block (fn 20 = 3RREG)
line 6835  015245  MSG_21:  ... T,JMP ... [ADDR=MSG_ILLEG]     <- register WRITE block (fn 21 = 3WREG)
```

Cross-check in the reviewed pseudocode `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`:
- line 163: `| 15-17 | MSG_ILLEG |`
- line 164: `| 20,21 | MSG_ILLEG |`

The `[V]` (verified) tag on that dispatch-table section (`MAILBOX-MICROCODE-PSEUDOCODE.md:148`)
and the raw decode agree.

### What the vendor message spec says the (classic) function is

`E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md`:
- lines 1302-1310 (13.12 REGISTER READ, `link.06 = 20`): `link.07` = first register number,
  `link.10` = number of registers, `link.11` = **physical ND-100 address** where the registers are
  returned.
- lines 1318-1326 (13.13 REGISTER WRITE, `link.06 = 21`): `link.07` = first register number,
  `link.10` = number of registers, `link.11` = **physical ND-100 address for the registers to
  write** into the ND-500.

Key structural fact: for 20/21 the register VALUES are **not inline in the message** - they are
block-copied to/from an ND-100 physical address named by `link.11`. So on a machine that DID
implement 21, the word-packing order (B2) is decided at the moment the microcode copies those
ND-100 16-bit words into the ND-500 register file. That copy code is exactly what is absent (=
`MSG_ILLEG`) on B30.

---

## Q1 (B1): register-block ORDER for functions 20/21

**Verdict: NOT IN MICROCODE (B30).** `MSG_20`/`MSG_21` -> `MSG_ILLEG`
(`MICRO-5800-B30.md:6834-6835`). The B30 image defines no register-block ordering for the mailbox
register functions because it has no handler for them.

**Cross-reference (NOT from microcode - from the ND-500 Reference Manual), with caveats:**

`E:\Dev\Ronny\ND500-DOCS\ND-05.009.4 EN ND-500 Reference Manual.md`:
- line 11135: *"Register block layout used in store and load register block is the same as used in
  store and load context, as shown in chapter 2. Register number*4 gives displacement relative to
  the start of the save area (**Program counter is register number = 0**)."*

  -> This CONFIRMS the one concrete part of the B1 assumption: **P is at register number 0**
  (byte offset 0, 4 bytes per register). It does NOT by itself confirm the rest of the numeric map.

- line 11156: LREGBL in non-privileged mode cannot modify `ST2, PS, CED, CAD, CTE, MTE, TEMM` -
  these are the "domain information table" registers. Confirms those seven names are part of the
  block, consistent with the tail of the B1 list.

- **Figure 2 "The register block", lines 987-1022, lists the block in this visual order:**
  `P, L, B, R, TOS, LL, HL, THA,` then `I1..I4` (integer accumulators), `A1..A4 / E1..E4`
  (float accum + extension), `ST1/ST2`, `OTE, MTE, CTE, TEMM`.

**DISCREPANCY you must resolve before trusting the B1 map:** the B1 assumption places
`I1..I4` at 4..7 and `TOS` at 19, `LL/HL/THA` at 20/21/22. But **Reference-Manual Figure 2 places
`TOS, LL, HL, THA` immediately after `R` (i.e. before the integer accumulators)**, not after PS.
If the LREGBL numeric displacement order equals the Figure-2 order (line 11135 says the two layouts
are "the same as used in chapter 2"), then the B1 numbering is WRONG in the middle: TOS/LL/HL/THA
would be registers 4..7 and I1..I4 would come later. I cannot resolve this from the microcode (no
handler) and the Figure-2 rows are not printed with explicit index numbers, so the exact integer
index of each register past 0 is **UNKNOWN from these sources**. The safe, verified facts are only:
P = 0; ST2/PS/CED/CAD/CTE/MTE/TEMM are the domain-table tail; 4 bytes per register.

The claim "3WREG register N == LREGBL register N+1" (a one-slot shift) is **UNVERIFIED** - I found no
source, in microcode or manual, that establishes a +1 offset between the mailbox register number and
the LREGBL register number.

---

## Q2 (B2): word order inside one 32-bit register (hi-first vs lo-first)

**Verdict: NOT IN MICROCODE (B30), and UNKNOWN from the available resources.**

The B30 microcode never assembles a register from two ND-100 halfwords, because `MSG_21`/`MSG_20`
are `MSG_ILLEG` (`MICRO-5800-B30.md:6834-6835`). There is no packing step to inspect. I therefore
**cannot confirm or refute** `hi<<16 | lo` versus `lo<<16 | hi` from the microcode. Anyone claiming
one or the other from the B30 image is guessing.

What I can add without guessing:
- The word-packing order for functions 20/21 is a **SINTRAN-side + classic-microcode convention**,
  fixed at the `link.11` block-copy (vendor guide lines 1309, 1325). It is NOT observable in B30.
- The B30 START path does not use a halfword register image either. `MICFU 23/25` (`MSG_START`,
  3START / 3TRACO) is `NEWCNTXT(); EXECUTE();` (`MAILBOX-MICROCODE-PSEUDOCODE.md:591-593`) - it
  loads a **context block from ND-500 logical memory** (native 32-bit words), not an ND-100
  halfword-pair image. So START gives no window onto the ND-100 halfword order either.
- Therefore B2 must be settled from **the SINTRAN 5STDRIV carve** (how SINTRAN LAYS OUT the image
  in ND-100 memory) checked against a **classic-500 microcode listing that actually implements 21**
  (we do not have one in these resources). This is exactly carve target #2 in the questions doc.

Concrete implication for the live `reg[18]=PS = {0x4848, 0x0003}` ambiguity: the microcode does not
disambiguate it. Nothing in B30 tells you whether PS is `0x48480003` or `0x00034848`.

---

## Bonus (bears on B5): how the swapper is actually started on B30

Not asked in B1/B2 but directly relevant and verified while tracing the dispatch:
- `MICFU 22` `MSG_STARTP0` (`015660`) exists and is real; the standing carve identifies it as the
  **watchdog / process-0 arm (P0START)**, and it issues a soft OCB self-command, not a register
  load (`MAILBOX-MICROCODE-PSEUDOCODE.md:587-590`).
- `MICFU 23/25` `MSG_START` (`015671`) = `NEWCNTXT(); EXECUTE();`
  (`MAILBOX-MICROCODE-PSEUDOCODE.md:591-593`) - context-block load then run.
So on B30 the start hand-off is a CONTEXT BLOCK (NEWCNTXT), and the classic register-image path
(21B) it would pair with is disabled (MSG_ILLEG). Whatever supplies PS/PSTP/segment-1 at start must
come through the context block or CPU-internal/control-store state, not through a 21B register image
on this image.

---

## What remains UNKNOWN

1. **The exact integer-indexed register order for 20/21.** Not in B30 (no handler). The Reference
   Manual gives P=0 and the Figure-2 grouping, but the printed figure carries no per-row indices,
   and its ordering appears to CONFLICT with the B1 assumption for TOS/LL/HL/THA vs I1..I4. Needs
   the chapter-2 numbered table body or a classic-500 microcode listing to pin each index.
2. **The "+1" shift (3WREG N == LREGBL N+1).** No source found. UNVERIFIED.
3. **Word order hi-first vs lo-first (the whole of B2).** Not in B30. Not derivable from any
   resource I was given. Must come from the SINTRAN 5STDRIV image-build carve cross-checked against
   a classic-500 (144-bit) microcode listing that implements function 21 - neither of which exists
   in `ND5000UC` (all images there are ND-5200/5500/5700/5800, none classic).
4. **Whether a classic-500 microcode image would pack hi-first.** Cannot be answered without that
   image. No classic-500 microcode binary is present in `E:\Dev\Ronny\ND5000UC\docs\MC\`.

---

## Sources cited

- `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` lines 6815, 6831-6835 (dispatch entries ->
  MSG_ILLEG).
- `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` lines 163-164 (table), 148 ([V]
  tag), 587-593 (MSG_STARTP0 / MSG_START).
- `E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md` lines 1302-1326
  (functions 20/21 message layout; values DMA'd via link.11).
- `E:\Dev\Ronny\ND500-DOCS\ND-05.009.4 EN ND-500 Reference Manual.md` lines 987-1022 (Figure 2
  register block), 11135 (P = register 0, regnum*4), 11156 (domain-table register list).
- `E:\Dev\Ronny\ND500-DOCS\instructions\asm\lregbl.md` (LREGBL: addr + regnum*4, 4 bytes/register).
