# ND-500 C: compile + link path in nd500x (working notes, 2026-07-25)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\NC-C-COMPILE-AND-LINK-PATH-2026-07-25.md`

How to compile a C program to `:NRF` with the real Norsk Data **NC** compiler and link it to
`:PSEG/:DSEG/:DOM` with the real **ND Linker**, running inside the `nd500x` WSL emulator - plus the
two emulator bugs on that path (one fixed, one under active investigation). Status tags: VERIFIED
(observed live / in code / manual) vs OPEN.

## Environment (VERIFIED)

- Launcher: `~/run_500.sh` -> `~/repos/nd500x/build/bin/nd500x --monitor --config ~/ND500USERS/nd500x.ini`.
- SINTRAN root: `/home/ronny/ND500USERS`. Default user `GUEST`. Program name `NAME` resolves to
  `{root}/{USER}/{NAME}.DOM` then `{root}/SYSTEM/{NAME}.DOM`.
- Tools in `SYSTEM/`: `NC-A06.DOM` (C compiler front-end, "Norsk Data C - Version: A06 - 1989-01-10"),
  `CAT-CAT5-B06.DOM` (referenced), `LINKER-B01.DOM` (ND Linker B01), plus libraries
  `NC-LIB-A06.NRF`, `USLIB3.NRF`, and per-language auto-link jobs incl. `LINKER-AUTO-C.JOB`.
- User files live as host files: e.g. `GUEST/HELLO.C` <-> SINTRAN `(GUEST)HELLO:C`.

## The compile command sequence (VERIFIED to work through the front-end)

NC is driven interactively. `compile` then prompts `source file:` / `list file:` / `object file:`.
SINTRAN OPEN will NOT auto-create an output file (returns error `56B` "no such file"), so the
`:NRF` and `:LIST` outputs MUST be pre-created (this is CORRECT SINTRAN behaviour, not a bug - see
`mon-oracle-for-NC/`). Driving script (stdin to `--monitor`):

```
LOGIN GUEST
CREATE-FILE HELLO:NRF
CREATE-FILE HELLO:LIST
NC-A06
compile
HELLO            <- source file  (-> HELLO:C)
HELLO            <- list file    (-> HELLO:LIST)
HELLO            <- object file  (-> HELLO:NRF)
exit
```

Result today (VERIFIED): front-end completes -
`preprocessing: ok / syntax check: ok / semantic check: ok`, `HELLO:LIST` written with
`*** no errors detected ***`. Then the BACK-END code generator ("-CROSS-A") runs and CRASHES (see
Bug 2). `HELLO:NRF` stays 0 bytes. **No C file has ever produced object output** (`A.NRF`, `B.NRF`
all 0 bytes) - the back-end has never completed in nd500x.

## The link step (from prior sessions - the standing linker blocker)

Once a valid `:NRF` exists, the ND Linker (`LINKER-B01.DOM`, run as `LINKAGE-LOADER` / by name)
produces `:PSEG/:DSEG` and a domain via `SET-DOMAIN "NAME" / LOAD-SEGMENT / EXIT` (auto-link at
CLOSE). See the full command ladder in `ND500-DOMAIN-HANDLING-TEST-COMMAND-SEQUENCE.md` (Phase H).
Prior sessions hit a separate linker `error (-677:52)` on `LOAD B:NRF`
(`CARVE-ANSWER-LINKER-LOAD-ERROR52-V*.md`) - NOT yet reached again here because the compile does not
yet emit an NRF.

## Bug 1 - compiler DATA segment capped at 1 MB (FIXED 2026-07-25, VERIFIED)

Back-end wrote ~34 MB into its DSEG; the DOM loader built DATA segments in **PS_ASI** mode, capped at
1 MB (L1 must be 0), so `[MMU] TRAP: PS_ASI page fault! L1=34 must be 0! vaddr=0x02200000`.

Fix (mirrors the linker's existing MON-412B/422B PS_ADI growable path): the DOM loader now builds
DATA segments **PS_ADI (two-level, 128 MB) + demand-growable**, adopting the loader's already-loaded
pages, in a SECOND pass seeded above the loader's final allocation cursor (anti-collision), plus a
growable ALIAS for the FORTRAN/compiler segment-0 -> segment-1 DATA case. Files (nd500x):
- `src/ndlib/ndlib_dom_loader.c` - pass-1 records DATA segs, pass-2 builds PS_ADI + alias.
- `src/cpu/nd500_segment_alloc.c` - `nd500_segment_adopt_growable_data`,
  `nd500_segment_register_growable_alias`.
- `src/cpu/nd500_mmu.h` - declarations.
Verified: MMU trap gone; `dom_nc_compile_a/b`, `dom_nc_compiler` pass; the 3 pre-existing suite
failures (`ote_instructions`, `mon_calls`, `instruction_validation`) fail identically WITHOUT the
change, so not a regression.

## Bug 2 - back-end jumps to PC=0 (OPEN - under investigation)

After Bug 1, the back-end runs further then crashes:
`[STOP] Invalid instruction 0x00 at PC=0x00000000`. Pinned to a computed jump:

```
0x0801AAE1  call $0x0801D94D, $1, IND(b.148)     ; arg = 0x100001A4 (a CALLER-frame stack pointer)
0x0801D94D  ents #52                              ; callee frame, B=0x10000118
0x0801D958  w1 := b.20   -> I1 = 0x100001A4       ; callee reads its parameter DIRECTLY as a value
0x0801D95A  w1 * $5                                ; uses it as an index
0x0801DAC8  jumpg $0x08019588(r1)  r1=I1=0x100001A4
            ; ABSOLUTE_PI: EA = 0x08019588 + 4*I1 = 0x48019C18 -> MEM[]=0 -> PC=0
```

VERIFIED: the `jumpg` decode is correct (addr-code `0xE0` = ABSOLUTE_PI, index I1, scale 4). The bad
input is `I1 = 0x100001A4`, a by-reference stack pointer, where a small switch index is expected.

Localized to the **CALL/ENTS argument-passing convention**: `Call.c` passes the argument's ADDRESS
(`pending_call_arg_addresses[i] = arg_operand->effective_address`, ~line 82/92); for `IND(b.148)`
that address is `MEM[B+148] = 0x100001A4`. ENTS stores that address at `b.20`. The callee reads
`b.20` DIRECTLY (not dereferenced) and uses it as a value/index.

### Decisive findings (VERIFIED 2026-07-25)

- **`Call.c` / `Ents.c` / `classify_mode` are all CORRECT.** Verified against
  `docs/ND-05.009.4 EN ND-500 Reference Manual.md` (ch.3.2 frame layout PREVB/RETA/SP/AUX/N =
  0/4/8/12/16, arg addresses at 20+) and `docs/CALL_and_ENTRY_Points_Explained.md`: ND-500 passes
  args BY REFERENCE (addresses); a callee is meant to read a param via `IND(b.20)`; and `0x45` is
  unambiguously local-short DIRECT (there is no short-form indirect - indirect is `0xC5/0xC6/0xC7`).
  RetroCore C# (`Call.cs:115-116`) agrees. So (a) address-vs-value, (b) ENTS offset, (c) `0x45`
  mis-decode are ALL REFUTED.
- **The DOM image is FAITHFUL - refutes decode-desync / image-corruption.** The exact executed
  routine is in `NC-A06.DOM` at file offset `0x2194D`; `file_offset = VA - 0x08000000 + 0x4000`
  maps VA `0x0801D94D` there exactly, and the bytes at `+9` are `20 43 0C 45 6C 05 42 E0` - a
  byte-perfect match to the trace. The compiler GENUINELY emitted `0C 45` (DIRECT `w1 := b.20`);
  `0C C6` (the IND form) is NOT present there.

### ENTS convention - RESOLVED: it is BY-REFERENCE, the emulator is CORRECT (VERIFIED 2026-07-25)

The manual settles it definitively: ND-05.009.4 Figure 3 (frame layout, lines 1096-1120) labels the
`b.20, b.24, ...` slots **"addresses of arguments"**, and line 1120 says "argument addresses will be
put on the stack." `Ents.c` (writes arg addresses at `b.20+`) and `Call.c` (passes the arg EA) match
this exactly; RetroCore C# agrees. **So the fix is NOT in ENTS/CALL** - the by-reference convention
and its emulation are correct. Do not change them.

### The real defect: an upstream WRONG VALUE on the stack

Since by-reference is correct, `b.20 = 0x100001A4` is a faithfully-passed argument address, and the
callee's DIRECT read of it (then `*5`, then `jumpg`) means the callee expects a SMALL integer there.
So the wrong value originates upstream - some earlier computation put the wrong thing on the caller's
stack (or the arg operand chain is off). Note `0x100001A4 = caller_B + 0xA8` (address of a caller
local) and `MEM[caller_B+0x94] = 0x100001A4` is a natural "address-of-local" - so the by-reference
DATA looks self-consistent; the puzzle is why the callee treats it as an int. This is now a
value-provenance / instruction-correctness question, NOT a calling-convention one.

Decisive experiment DONE (2026-07-25): instrumented the ENTS at `0x0801D94D` (env
`ND500X_CRASHDIAG`) and ran WITH vs WITHOUT the Bug-1 fix.
- WITH fix: reaches `ents@0x0801D94D new_b=0x10000118 N=1 arg0=0x100001A4` at ic=1693450, then the
  jumpg crash.
- WITHOUT fix: hits `[MMU] TRAP ... vaddr=0x02200000` FIRST and NEVER reaches the ENTS.

**Conclusion: the jumpg->PC=0 is strictly DOWNSTREAM of the 34 MB heap access that Bug-1 unblocked.**
No C compile ever executed this far before (everything died at the 1 MB DATA cap), so this is
previously-unreached CAT-500 territory. The wrong value is either (a) a pre-existing emulator
instruction/behaviour defect never before exercised, or (b) an artifact of the demand-grown (zeroed)
heap my fix provides where CAT-500 expected different contents. Distinguishing them needs a reference
execution (RetroCore C# running the same `NC-A06.DOM` to a breakpoint at `0x0801D958`, comparing
`b.20`) to bisect the first divergent instruction, or a heap read/write-consistency audit of the
PS_ADI demand-grow. Diagnostics used were reverted; the Bug-1 fix remains and NC dom tests pass 100%.

## Two distinct toolchains - do NOT conflate

- REAL Norsk Data path (this doc): NC-A06.DOM + LINKER-B01.DOM inside nd500x -> genuine SINTRAN
  `:NRF` / `:PSEG` / `:DSEG` / `:DOM`. This is the "compile hello.c in the GUEST account" path.
- HOST PCC cross-compiler (`~/repos/nd500x/examples/04-c-math`, `bin/cc` -> `nd500-as` -> `.o`) makes
  Linux-side ND-500 object files, NOT SINTRAN segments. Different toolchain; not this path.

---
*Provenance: live nd500x runs 2026-07-25, nd500x source, instruction trace. Bug 1 fix verified;
Bug 2 root-cause localized, exact fix OPEN pending the CALL/ENTS convention analysis.*
