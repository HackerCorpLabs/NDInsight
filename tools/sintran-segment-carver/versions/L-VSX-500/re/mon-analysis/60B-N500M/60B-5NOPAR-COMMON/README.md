# MON 60B - 5NOPAR common path (the hand-off to the ND-500 SYSTEM MONITOR)

**Every `5IFUNC` handler ends with `GO FAR 5NOPAR`** (or is `5NOPAR` directly for the ~70 no-prep
codes). This is the shared code that packages the monitor call and enters the ND-500 **system
monitor** - i.e. the boundary between "MON 60B / N500M" and the "more than MON 60" code that actually
drives the ND-500 (Phase 1/3 dependency).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## What it does (verbatim in `.npl`)
1. Compute the caller's ND-500 process number (RDIV) and map into its ND-500 data segment (`M1MEXY`).
2. For reserve/start functions (`N5RES`, `MRESSPES`, `SSTDOM`): clear the ND-500 data segment + set up an IOF exit.
3. For WISON/TSTFUNC: copy all procs' `RTRES` into `SBUFFR` on the ND-500 data segment (+ count).
4. Combine `N5FUNCTION<<8 | 5FUNCTION`; `MOVAA` saves the moncall info block (`ZPREG..5DFSIZE`) onto the ND-500 data segment (**this is the 5MPM message the system monitor reads**).
5. `TOSYMON`: IOF; `BRELEASE` (release the N500DF datafield); if STOP-ND-500 -> `CFORGET`; if background -> `ESCON`.
6. **`CALL FPT2ENTRY` - ENTER THE ND-500 SYSTEM MONITOR.** Returns `5PT2RET` (ok/error) or `SYMNLOAD` (system monitor not loaded).

## Why this matters
`FPT2ENTRY` / the ND-500 **system monitor** is the code that builds/consumes the 5MPM message and
actually talks to the ND-500 - it is NOT part of N500M. This is the concrete "more than MON 60"
carving target (parent SCOPE NOTE, Phase 1). The `SYMNLOAD` path is the "system monitor not loaded"
error - related to the control-store / swapper bring-up gate.

## Byte status
VERIFIED: this is the common tail of the byte-verified N500M dispatcher. From NPL: body. PENDING: L07
address + carving `FPT2ENTRY` / the system monitor (separate module).
