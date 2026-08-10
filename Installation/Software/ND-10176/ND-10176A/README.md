# ND-10176A — COBOL for ND-100, version A

> Status: IN-PROGRESS — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `10176A` (3-part floppy set) |
| Base product | [`ND-10176`](../README.md) |
| Version | A |
| Release date | files dated 1981-03-19/23 |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown |

## Description
The oldest located COBOL-100 release — compiler only, no ISAM or VTM screen-handling bundled on
these three floppies. Ships the compiler in **both** `:PROG` and `:BPUN` forms (same dual-shape
pattern seen in [ND-10191A FORTRAN](../../ND-10191/ND-10191A/README.md)).

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10176A-PART1` | `COBOL:PROG` (81 pages, pre-linked) and `COBOL:BPUN` (61 pages, raw) — the compiler, both forms |
| `ND-10176A-PART2` | `LIST-COBOL-FILE:BPUN` (50 pages) — a utility program |
| `ND-10176A-PART3` | `COBLIB:BRF` (82 pages, 1-bank runtime) and `SEPARATE-COBLIB:BRF` (63 pages — the 2-bank/"separate data" runtime; named differently from the later `COBOL-1BANK`/`COBOL-2BANK` convention, but the same concept) |

Confirmed by downloading all three images and reading with `ndtool -t`.

## Installation procedure — INFERRED, NOT CONFIRMED

No `:MODE` script and no PD sheet exist for this version. By pattern with every other
manual-copy compiler in this catalog:

```
@ENTER-DIRECTORY ND-10176A-PART1,FLOPPY-DISC-1,0,
@COPY-FILE "COBOL:PROG",(SYSTEM)COBOL-100:PROG
@RELEASE-DIRECTORY ND-10176A-PART1
@ENTER-DIRECTORY ND-10176A-PART3,FLOPPY-DISC-1,0,
@COPY-FILE "COBLIB:BRF",(SYSTEM)COBLIB:BRF
@COPY-FILE "SEPARATE-COBLIB:BRF",(SYSTEM)SEPARATE-COBLIB:BRF
```
Then `@DUMP-PROGRAM-REENTRANT COBOL-100,(SYSTEM)COBOL-100:PROG` (using the pre-linked `:PROG`) —
**not verified**.

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10176-A2-EN.md](../../../Product-Info/ND-10176-A2-EN.md), [../../../Product-Info/ND-10176-A3-EN.md](../../../Product-Info/ND-10176-A3-EN.md)
- Manual(s): `ND-60.144` COBOL Reference Manual

## Provenance & open items
- Source: floppy directory listings via `ndtool` on all three downloaded images.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10176` product overview)
