# BUG REPORT - nd500x: growable PS_ADI DATA loader regressed NC codegen (jumpg -> PC=0)

**File:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\BUGREPORT-nd500x-growable-DATA-regressed-NC-codegen-2026-07-26.md`
**Date:** 2026-07-26
**Repo:** `~/repos/nd500x` (WSL)  **Branch:** `fix/deabf-i1-success-and-load-investigation`
**Severity:** HIGH - the real Norsk Data NC C compiler crashes during code generation; no `:NRF` is produced.
**For:** the team that authored the DOM-loader / MMU page-table-format change.

## One-line

Commit **`951237d` "fix(dom): realign DOM-loader PTEs to hardware format + growable DATA segments"**
changed how a domain's DATA segment is mapped (from **PS_ASI + demand-fault** to **PS_ADI
growable**). The new growable **work-region** mapping delivers different data to the running program
than the old path did, so the NC compiler reads a wrong value, takes a wrong branch, and crashes.
The commit immediately before it (`aa5cd5e`) compiles cleanly.

This is NOT a config/shell issue and NOT the MMU core (PTE walk, init-page mapping, ENTS/CALL are all
correct). It is specifically the growable DATA **work-region (grown/demand pages)** behaviour.

## Repro

```
cd /home/ronny/ND500USERS
printf 'LOGIN GUEST\nCREATE-FILE B:LIST\nCREATE-FILE BOUT:NRF\nNC-A06\nCHECK B,B,B\nGENERATE-CODE B,BOUT\nEXIT\n' \
  | ~/repos/nd500x/build/bin/nd500x --monitor --config ~/ND500USERS/nd500x.ini
```
- `aa5cd5e` (and everything before it): NC runs to a **clean exit** (~2,313,500 instructions).
- `951237d`..HEAD: `[STOP] Invalid instruction 0x00 at PC=0x00000000` (~2,312,800 instructions).
- (`B.C` is a trivial `int x; main(){ x = VALUE; }`.)

## Bisect (definitive)

| Commit | DATA-mapping mechanism | Result |
|--------|------------------------|--------|
| `f7bb50e` (Jul 21) | PS_ASI + demand-fault | GOOD - clean exit |
| `aa5cd5e` (Jul 25, one before the change) | PS_ASI + 512-page reserve + demand-fault | **GOOD** - clean exit |
| `6b3d4fb`/`09e3108`/`dadb73b` | PTE refactor, DOM load broken | untestable (different early crash) |
| **`951237d`** | **PS_ADI growable DATA** | **BAD** - jumpg -> PC=0 |

Every commit through `aa5cd5e` - including all the July arithmetic / compare / flag / string
instruction fixes - is GOOD. The regression enters only with the DATA-loader rewrite.

Method: each commit was built from a clean `git archive` tree (era-matched `ndmonlib`, plus a 1-line
graft of the `ndlib_aout_set_data_base` setter that only lands in `951237d`), driven with the script
above. Verdict = clean deep exit (GOOD) vs `PC=0x00000000` (BAD).

## First divergence (instruction-exact)

A differential PC trace of `aa5cd5e` (GOOD) vs `951237d` (BAD) is **identical for 1,196,939
instructions**, then splits at ONE conditional branch inside a number-to-hex-string formatter:

```
routine 0x080294D9 ; inner loop 0x08029545..0x0802955F = "divide by 16, emit digit, repeat until 0"
  0x0802955D  w test b.24        ; b.24 = the number being formatted
  0x0802955F  if><go  $230        ; loop while b.24 != 0
```
- GOOD: `b.24` is larger -> loops more -> branch taken (keep looping)
- BAD:  `b.24` is smaller (~2 hex digits) -> loop exits early -> branch falls through

`b.24` is **not written by any nearby program store** - NC reads it from its DATA/work region. Because
`951237d` maps that region differently, NC gets a different value there, formats a shorter number, and
falls through the branch into a code path the GOOD run never enters. That wrong path is what later
reaches callee `0x0801D94D`, where a stray `&local` (`0x100001A4`) is used as a switch index:

```
0x0801AAE1  call $0x0801D94D, $1, IND(b.148)   ; arg addr = 0x100001A4
0x0801D958  w1 := b.20  -> 0x100001A4          ; callee reads it as a small int
0x0801D95A  w1 * $5
0x0801DAC8  jumpg $0x08019588(r1) r1=0x100001A4 ; EA overflows -> MEM=0 -> PC=0
```

## What was RULED OUT (with evidence)

- **DATA init-page mapping**: run-time validation - all 61 initialized DATA pages of the domain
  translate byte-exact through the live MMU at crash time (`0 mismatch`). The *initialized* data is
  fine; only the *grown/work* region differs.
- **CALL/ENTS by-reference logic**: `git diff` from the last-good era shows the arg-copy loop and
  frame offsets (PREVB/RETA/SP/AUX/N/ARG1 = 0/4/8/12/16/20) are unchanged; matches manual
  ND-05.009.4 lines 1096-1139. Only env-gated diagnostics + a MON-path pending-state clear were added.
- **The FORTRAN `DC[0] -> seg1` alias**: present in BOTH good and bad - not the cause.
- **CPU instruction semantics**: all instruction changes up to and including `aa5cd5e` are GOOD.
- **Config / `--monitor` shell / `nd500x.ini`**: identical across good and bad.

## Root cause (localized)

`951237d` replaced *PS_ASI DATA + demand-fault-on-first-access* with *PS_ADI growable DATA*. The
NC/CAT-500 back-end addresses a large work region (its ~34 MB codegen scratch, e.g. via segment-0 VAs
around `0x02200000`). The old path serviced those accesses via an MMU fault that mapped the page; the
new growable path maps them differently, so a value NC reads back from that region differs. The
initialized pages are byte-exact; the defect is in the **grown work-region** path (grow_on_fault +
the segment-0 growable alias), not in the init adoption.

## Suggested fix

Two options, in order of confidence:

1. **Revert the DATA path to `aa5cd5e`'s PS_ASI + demand-fault approach** (proven to reach a clean NC
   exit), keeping the PTE-format realignment. Lowest risk.
2. **Fix the growable PS_ADI work-region** to deliver byte-identical data to the demand-fault path.
   Use `aa5cd5e` as a regression oracle: run both builds with a read-watch on the work region and
   diff the value NC reads just before instruction ~1,196,930 (the source of `b.24`) to find the
   exact cell that reads differently.

Files in scope (nd500x): `src/ndlib/ndlib_dom_loader.c` (DATA-segment build, the seg-0 growable
alias), `src/cpu/nd500_segment_alloc.c` (`nd500_segment_adopt_growable_data`,
`nd500_segment_register_growable_alias`, `grow_on_fault`, watermark allocator), `src/cpu/nd500_mmu.c`
(the demand-map / grow-on-fault path).

## Notes

- `aa5cd5e` is a usable known-good nd500x oracle for this test - no RetroCore needed.
- Reproduced against branch HEAD `762058d` (regression still present).
- All diagnostics used were reverted; the tree is clean apart from the authoring teams' own
  in-progress work.
