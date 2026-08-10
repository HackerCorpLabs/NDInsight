# The 5SWAP `1 10533B` trap: root cause is our RPHS operand encoding

**Date:** 2026-08-03
**Status:** SOLVED. Root cause proven at byte level, fixed in both emulators, and **confirmed
end to end on the live harness**.

**Harness confirmation (2026-08-03).** `Nd500SwapFile_CreateAndDefine_Capture`, 21 minutes,
clean build (zero MSB copy errors - checked, because a locked DLL has produced fake results in
this project before). In **666,703 lines of output the string `10533` does not appear once.**

| | Before the fix | After the fix |
|---|---|---|
| Trap address | `1 10533B` | `26 6721B` and `0 1400000B` |
| `P1` | inside the RPHS operand | `0xB0000DD1` |
| Reason | garbage decode | `capability is ZERO - segment 1 is not in domain 0 (CED=0, PS=10)` |

The old fault is gone outright and a different, coherent fault surfaces in a different code
region (`0xB0000000` = the linker/loader area). That is the next target, and it is NOT this bug.
Note the test's `Passed` verdict proves nothing on its own - it is a capture harness that passes
either way. The disappearance of `10533` is the evidence.

**A/B PROOF, 2026-08-04.** The first comparison was against an older capture that predated these
diagnostics, so it could only show that the trap moved. Repeated properly: the SAME harness build,
run twice, with ONLY the `O_DIR` bit flipped back and forth.

| Marker | Defect restored | Fixed |
|---|---|---|
| `At program address: 1 10533B` | **6** | **0** |
| `At program address: 26 6721B` (domain start) | 0 | 3 |
| `At program address: 0 1400000B` | 0 | 3 |
| `swpfu[LNEWSWAP:...]` | 7 | 8 |
| `ansMON=` | 377B | 377B |

Putting the defect back brings the original trap back exactly, so this is **causal, not
correlation**. The domain-start state is reached only with the fix.

Honest nuance: `ansMON=377B` appears in BOTH runs, so the swapper reached its `MON 377B` either
way. The fix's effect is that it no longer dies at `10533` and instead proceeds to start the
loaded domain.

GOTCHA from that A/B: restoring the source from a backup copy carries the BACKUP's older
timestamp, so MSBuild skips the rebuild and the test runs against the stale binary. Five tests
failed on an already-correct source. Touch the file after any restore - and note it was the
table-guard test (section 6) that caught it.
**Supersedes the open question in** `5SWAP-TRAP-MEASURED-WITH-P1-2026-08-03.md`
("what reads address `0x00000004`?").

---

## 1. The answer in one line

`RPHS` was described in our instruction tables as taking **four inline literal bytes**. It does
not - it takes an ordinary operand. That made the decoder eat one byte too few, resume **inside**
the operand at `0o1000010533`, and execute garbage. `0o10533` is exactly the address SINTRAN
reported.

**The trap was never a SINTRAN fault, an MMU fault, or a bad segment number. It was our decoder.**

## 2. The bytes

From the swapper P-segment (`swapper/SWAPPER-K01.PSEG`, virtual base `0o1000000000`):

```
1000010521:  015 107                            w2 :=  b.34
1000010523:  017 105                            w4 :=  b.24
1000010525:  377 365 | 304 | 010 001 115 054    rphs   <abs 0o1000246454>
1000010534:  300 057                            go     $57
```

- `377 365` = opcode `0xFFF5` = `RPHS` (octal `177765B`), 2 bytes.
- `304` = the **address code**, 1 byte.
- `010 001 115 054` = the 32-bit absolute address `0x08014D2C`, 4 bytes.

Total **7 bytes**, `0o10525` .. `0o10533` inclusive. The next instruction starts at `0o10534`.

## 3. Why `0o304` means "32-bit absolute address follows"

Not assumed - proven from a sibling instruction **eight bytes earlier in the same routine**, one
the ND-500 disassembler already decodes correctly:

```
1000010477:  104 304 010 002 075 154            w test $1000436554
```

Same address code `0o304`, followed by exactly four bytes `010 002 075 154` = `0x08023D6C`.
Written in octal, `0x08023D6C` is:

```
0x08023D6C = 0000 1000 0000 0010 0011 1101 0110 1100
           = 0 00001000000000100011110101101100        (33 bits, for octal grouping)
           = 000 001 000 000 000 100 011 110 101 101 100
           =  0   1   0   0   0   4   3   6   5   5   4
           = 0o1000436554
```

Which is character for character what the disassembler printed. So the address code, the operand
size, and the byte order are all confirmed on a known-good decode.

The general operand form was cross-checked on the same routine: top 2 bits of the address-code
byte select the mode, low 6 bits are a displacement **in words** -
`0o105` = `b.24` (5 words = 20 bytes = `0o24`), `0o107` = `b.34`, `0o205` = `r.24`,
`0o316` = 16-bit immediate follows, `0o317` = 32-bit immediate follows.

## 4. What each tool got wrong

| | Bytes consumed | Next instruction | Verdict |
|---|---|---|---|
| ND-500 disassembler | 3 (`377 365 304`) | `0o10530` | **wrong** - prints the nonsense literal `$1777777777777777777704` |
| RetroCore / nd500x | 6 (`377 365` + 4 inline) | `0o10533` | **wrong** - lands inside the operand |
| Correct | 7 (opcode + code + address) | `0o10534` | matches `go $57` |

Both tools were wrong, in **different directions**, which is why they never contradicted each
other in a way anyone noticed.

## 5. The three independent confirmations

1. **Arithmetic.** `0o10525 + 6 = 0o10533`. The reported trap address is precisely where a
   6-byte read resumes. That is not a coincidence - it is the defect's signature.
2. **The address code.** `0o304 + 4 bytes` is proven by `w test $1000436554` above.
3. **Semantics.** `ND-05.009.4` section 16.31 writes the operand as `<domain number/r/W>` - a
   register or word operand, i.e. a value to be *fetched*. Read as a direct literal the domain
   number would be `0xC408014D`, which is not a domain number. Read correctly, the domain number
   is loaded from memory at `0o1000246454`.

## 6. The fix

Operand 0 of `RPHS` (`0xFFF5`) and `WPHS` (`0xFFF4`) is no longer flagged `O_DIR`.

**RetroCore**
- `Emulated.HW/ND/CPU/ND500/Instructionset.Init.cs` - `directOperandMask` `0x01` -> `0x00`,
  `directOperandSizes` `{4}` -> `{0}`, and `OperandFlags.O_DIR` dropped, for both instructions.
  The proof above is recorded in the comment on the `rphs` entry.
- `Emulated.HW/ND/CPU/ND500/Instructions/instructions.json` - `operandTemplates`
  `0x0003FFE8` -> `0x0001FFE8` (the `0x00020000` bit is `O_DIR`).
- `Emulated.HW/ND/CPU/ND500/Registers.cs` - the `P1` worked example rewritten; it previously
  named the `RPHS` as the faulting instruction, which was the retracted lead.

**nd500x (C port)**
- `src/cpu/instructions_gen.c`, `src/cpu/instructions.json`,
  `docs/instructions/instructions.json` - same `0x0003FFE8` -> `0x0001FFE8` change.
- `src/cpu/instructions/SYSTEM/Rphs.c` and `Wphs.c` - the proof recorded in the header comment.
- Built and run: `rphs_wphs` passes; 3 failures remain (`ote_instructions`, `mon_calls`,
  `instruction_validation`), all pre-existing `DoubleSub` float cases, untouched by this change.

## 7. What this retires

- **"What reads address `0x00000004`?"** - nothing does. It was garbage produced by decoding a
  fragment of an operand as an instruction. The question is dismissed, not answered.
- **The instruction-boundary contradiction at `0o10533`** - resolved. Neither the disassembly
  nor the trap address was lying; our operand length was.
- **The `b.30 = 0` -> `I3 = 0` suspicion** - irrelevant. The registers going into the `RPHS`
  were measured sane (`I1=0x80 I2=0x08024364 I3=0 I4=0x0A`) and they still are.

## 8. Where the machine stops NOW - and it is past the swapper

Measured 2026-08-04 with a new instruction-history ring (see section 10).

```
came from (newest first):
  0xB0000DD1 <- 0x0800823F <- 0x08008237 <- 0x08000198 <- 0x08000182 <- ...
```

**`0x0800823F` = `0o1000101077`**, and the swapper disassembly gives it exactly:

```
1000101077: 303 370 000 000 377 004 304 010 001 052 050 304 010 002 100 260 ...
  call $F80000FF,$4,$1000225050,$1000440260,$1000440264,b.24     ; MON 377B
```

The call target `0xF80000FF` is not an address: bits 31-27 are **31** (the monitor segment) and
the low byte is `0xFF` = `0o377`. **This is MON 377B = `N5SWAP`, the swapper trapping outward to
SINTRAN**, and two of its arguments (`0x080240B0`, `0x080240B4`) are the addresses that show up in
the `lastProtectViolation` line. It was serviced correctly - the run records
`ansMON=377B` with `ansP=0x08008255`, which is precisely the instruction after the call.

**The next PC, `0xB0000DD1`, is CORRECT.** `0xB0000000` is segment 22, and the domain being
started is `(210319H02:FLOPPY-USER)LINKAGE-LOAD-H02`. Extracting `LINKAGE-LOAD-H02:PSEG` from
`210319H02-XX-01D.img` and reading offset `0xDD1`:

```
0x0DD1:  DC B0 00 26 E4 CF 00 00 00 1C CE 20 00 ...
         ^^ 0xDC = 0o334 = `init`
```

`init` is the ND-500 domain entry-point instruction - the same first byte as `LINKER-B01`'s
documented entry (`B0013B41: DC B0 00 1A BC ...`) and as the swapper's own `init` at
`1000000004: 334 010 ...`. So SINTRAN started the loaded domain at its real entry point.

**The remaining fault is therefore NOT a wrong PC - it is that segment 22's program pages are not
mapped**, so the instruction fetch raises `PGF - address not present`. That is the next target.

### 8a. What is measured about the remaining fault

**The swapper's MON 377B function codes are carved.** All 15 `MON 377B` sites in
`swapper-k01-pseg.asm` pass a pointer into a 6-word table in the D-segment; the values are the
`SWPFU` codes that `SWPDECODER` switches on. Read straight out of `SWAPPER-K01.DSEG`:

| Address | File offset | Value | SWPFU | Call sites | Times it actually fired |
|---|---|---|---|---|---|
| `0o1000225040` | `0x12A20` | `0x427` | (not a code - unexplained) | 1 | - |
| `0o1000225044` | `0x12A24` | 2 | `LSWPAGE` | **8** | **1** |
| `0o1000225050` | `0x12A28` | 1 | `LNEWSWAP` | 1 (the failing site) | 8 |
| `0o1000225054` | `0x12A2C` | 4 | `LALLOPAGE` | 1 | **0** |
| `0o1000225060` | `0x12A30` | 5 | `LDATREADY` | 1 | 0 |
| `0o1000225064` | `0x12A34` | 6 | `LCLTSB` | 3 | 0 |

Note `3` (`LPRSUSPEND`) is absent from the table and `0o225040` holds `0x427`, which is not a
SWPFU code - **UNEXPLAINED**, flagged rather than guessed at. The other five line up exactly with
`SWPDECODER`'s vocabulary, so the base offset is right.

**Two different traps fire at `0xB0000DD1`:**

- `trapsPosted=1 lastTRAPN=44B` - `44B` = 36 decimal = bit 36 = **PV (protect violation)**, which
  is exactly what SINTRAN prints (`PROTECT VIOLATION / At program address: 26 6721B`). This one
  WAS posted.
- The fatal stop is **PGF** (bit 38, `0o46`) - `instruction fetch: address not present`. `3TRACO`
  never appears in the run, so the CPU never parked awaiting SINTRAN for it.

Both are consistent with one cause: **segment 22 has neither a program capability nor pages in
domain 0 when SINTRAN starts the domain.**

DELIBERATELY NOT USED as evidence: the `operand: mode=... B=... I1..I4` fields inside the
`lastProtectViolation` string. Those are captured by the LAST OPERAND DECODE, not by the faulting
instruction (section 9), and here they still show the SWAPPER's frame (`B=0x08024278`) while the
PC is in segment 22. Reading them as the faulting instruction's operands would invent a story
about `init` referencing `b.20`, which its own bytes (`DC B0 00 26 E4 ...`, all absolute) refute.

CAUTION recorded so the next reader does not repeat it: do not check this against
`LINKER-B01.DOM` (entry `0xB0013B41`, and its PSEG file offset is `VA - 0xB0000000 + 0x1000`).
That is a DIFFERENT product. `LINKAGE-LOAD-H02:PSEG` is a raw PSEG whose file offset is the
segment offset directly, with no `+0x1000`.

## 9. Method note worth keeping

A `P1` that does not land on an instruction boundary is **evidence about the decoder**, not an
anomaly to explain away. The previous session treated the off-boundary address as a puzzle about
SINTRAN and spent days on it. The right reading was: *we resumed at a byte that is not an
instruction start, therefore the preceding instruction's length is wrong.*

## 10. The instrument that found section 8

`CpuND500` now keeps a 64-entry ring of instruction start addresses (one array store per
instruction, always on), and the last 12 are appended to the fatal-trap message as
`[came from (newest first): ...]`. A trap says WHERE the machine died but never HOW it got there,
and for a control transfer into unmapped memory that is the only question that matters.

nd500x already had the equivalent - `nd500_dump_stop_ring()` in `src/cpu/cpu.c`, gated on env
`ND500X_STOPDBG=1`, and richer (it carries `CED`/`CAD` per entry). It prints OLDEST first; the
C# one prints NEWEST first. Read the labels.

Do NOT try to reconstruct the path from a `lastProtectViolation` string: its bundled `I1`-`I4`
and operand fields are captured by the LAST OPERAND DECODE, not by the faulting instruction, and
that has already sent this project down a wrong path once.

Related: `5SWAP-TRAP-MEASURED-WITH-P1-2026-08-03.md`,
`HANDOFF-RPHS-WPHS-PHYSICAL-SEGMENT-TO-ND500X-LLM-2026-08-03.md`,
`PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md`.
