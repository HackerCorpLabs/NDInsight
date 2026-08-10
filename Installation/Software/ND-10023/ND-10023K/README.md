# ND-10023K — FORTRAN (48-bit), version K

> Status: STUB — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [INF]

| Field | Value |
|-------|-------|
| Part number | `10023K` (as printed on the floppy volume label) |
| Base product | [`ND-10023`](../README.md) |
| Version | K |
| Release date | files on disk dated 1979-07-12 (libraries) and 1979-07-26 (compiler, print utility) |
| CPU target | NORD-10, 48-bit floating-point |
| OS requirement | unknown |

## Description
FORTRAN compiler for NORD-10 (48-bit float). Single floppy, four files, no installer script and
no PD sheet — everything here is read from the mounted floppy's directory listing only.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10023K` | `FTN-2090I:BPUN` (the compiler, 32 pages) · `FTNLIBR-2091F:BRF` (library, 18 pages) · `FTNRTLIBR-2092F:BRF` (runtime library, 14 pages) · `PRINT-FILE-2324B:BPUN` (a print utility, 9 pages), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `d5799ff428b0bc95b980bd90167ead26`) and reading it with
`ndtool -t`. Note the four files each carry their own independent revision letter/number
(`2090I`, `2091F`, `2092F`, `2324B`) under the single volume revision `K` — the same
per-file-revision pattern seen in this catalog's Subsystem Package II and PLANC entries.

## Installation procedure — INFERRED, NOT CONFIRMED

No install text exists on this floppy (no `:MODE` script, no PD sheet). By pattern with every
other simple `:BPUN`-shaped product in this catalog (PLANC A/B, Backup-System B):

1. Enter the floppy directory and copy all four files to the user where you keep compiler
   binaries (commonly `SYSTEM`):
   ```
   @ENTER-DIRECTORY ND-10023K,FLOPPY-DISC-1,0,
   @COPY-FILE "FTN-2090I:BPUN",(SYSTEM)FTN:BPUN
   @COPY-FILE "FTNLIBR-2091F:BRF",(SYSTEM)FTNLIBR:BRF
   @COPY-FILE "FTNRTLIBR-2092F:BRF",(SYSTEM)FTNRTLIBR:BRF
   @COPY-FILE "PRINT-FILE-2324B:BPUN",(SYSTEM)PRINT-FILE:BPUN
   ```
2. Dump the compiler reentrant. **The start/restart addresses are NOT known** — every other PD
   sheet in this catalog states them explicitly (PLANC uses `0 1`; Subsystem Package II's MAC/FMAC
   use `-1 -3`); nothing on this floppy or in any document read so far gives FTN's values. Do not
   guess a number:
   ```
   @DUMP-REENTRANT FTN,<start-address>,<restart-address>,(SYSTEM)FTN:BPUN
   ```

**Do not treat the above as verified.** Step 1 follows the well-established copy pattern; step 2
is incomplete without a source for the actual addresses.

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: not located
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual (assumed applicable, not confirmed for this
  specific 48-bit product)

## Provenance & open items
- Source: floppy directory listing only, via `ndtool` on the downloaded image.
- **TODO (blocking the reentrant-dump step):** find the compiler's start/restart addresses — not
  present anywhere read so far.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10023` product overview)
