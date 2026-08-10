# ND-10309B — PLANC for ND-100, version B

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `10309B` (source article: `10370B`) |
| Base product | [`ND-10309`](../README.md) |
| Version | B |
| Release date | 82.06.15 (15 Jun 1982) |
| CPU target | ND-100 (also NORD-10) |
| OS requirement | SINTRAN III VS |

## Description
PLANC-100 Compiler, revision B. Adds `$OPTION ARRAY-INDEX-CHECK <ON/OFF>` (compile-/run-time
array bounds checking) and `$EJECT` (form-feed to the list device); implements exponential
`REAL8**INTEGER`; corrects 14 errors (`FOR`/set-array, predeclaration consistency warnings,
quoted macro parameters, `NEW ... IN <integer array>`, boolean-operator variable initiation,
INLINE routines, stack damage with composite out-values, and others — see the source document).
Also updates the version-A syntactic-restriction list (10 items now, vs 17 for A — several were
fixed, a few new ones documented). [PD]

## Prerequisites
Same as version A: ND-100/NORD-10, SINTRAN III VS, 126K-word terminal background segment (128K
in the actual command parameter). [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10309B` | `PLANC-100-B:BPLUN` (compiler — see type note), `PLANC-1BANK-B:BRF` (1-bank runtime), `PLANC-2BANK-B:BRF` (2-bank runtime), user `FLOPPY-USER` |

> **Type discrepancy, as printed on the PD sheet:** the Diskette Directory page lists the compiler
> as `PLANC-100-B:BPLUN`, while the "Loading/Operating Procedure" text and "Programs (Files)"
> table both call it `PLANC-100-B:BPUN`. Same pattern as version A's `:IFUN`/`:BPUN` mismatch —
> `BPUN` is almost certainly correct; not resolved against the mounted floppy.

## Installation procedure

Source: PD sheet "Loading/Operating Procedure, Use", verbatim — identical shape to version A. [PD]

1. Enter the directory on the floppy and copy all three files to user `SYSTEM`.
2. Dump the compiler as a reentrant subsystem:
   ```
   @DUMP-REENTRANT PLANC-100,0,1,<input-file>
   ```
3. Set the terminal background segment size:
   ```
   @CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>, 128
   ```

**Note:** PLANC-100 cannot be restarted with `@CONTINUE`. [PD]

## Configuration / post-install
None beyond the reentrant dump and background-segment-size change above.

## Documentation
- PD-sheet: [../../../../Reference-Manuals/ND-10309B PLANC FOR ND-100.md](../../../../Reference-Manuals/ND-10309B%20PLANC%20FOR%20ND-100.md)
- PI-sheet: [../../../Product-Info/ND-10309-A1-EN.md](../../../Product-Info/ND-10309-A1-EN.md)
- Manual(s): `ND-60.117.03` PLANC Reference Manual

## Provenance & open items
- Source: single OCR'd PD-sheet scan.
- **TODO:** resolve the `:BPLUN` vs `:BPUN` file-type discrepancy against the mounted floppy.
- **TODO:** this install has not yet been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10309` product overview)
