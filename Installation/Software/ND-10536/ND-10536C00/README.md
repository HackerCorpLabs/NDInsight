# ND-10536C00 — COB-GEN, version C00

> Status: IN-PROGRESS — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `10536C00` |
| Base product | [`ND-10536`](../README.md) |
| Version | C00 |
| Release date | files dated 1985-01-15 / 1985-05-10 |
| CPU target | ND-100 |
| OS requirement | COBOL compiler release F or later [PI] |

## Description
Single-floppy release: the generator program, its error-message file, and a VTM (Virtual Terminal
Manager) table file for screen drawing.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `10536C00-1D` | `COB-GEN-C00:PROG` (38 pages, pre-linked) · `UE-ERMSG-EN-B00:ERR` (41 pages, error messages) · `DDBTABLES-E00:VTM` (17 pages, VTM tables), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `674e0780a9837536110ee3775a2987bd`) and reading with
`ndtool -t`.

## Installation procedure — INFERRED, NOT CONFIRMED

No `:MODE` script and no PD sheet exist for this version:

```
@ENTER-DIRECTORY 10536C00-1D,FLOPPY-DISC-1,0,
@COPY-FILE "COB-GEN-C00:PROG",(SYSTEM)COB-GEN-C00:PROG
@COPY-FILE "UE-ERMSG-EN-B00:ERR",(SYSTEM)UE-ERMSG-EN-B00:ERR
@COPY-FILE "DDBTABLES-E00:VTM",(SYSTEM)DDBTABLES-E00:VTM
@DUMP-PROGRAM-REENTRANT COB-GEN,(SYSTEM)COB-GEN-C00:PROG
```
**Not verified.**

## Configuration / post-install
Unknown. Requires COBOL compiler release F or later already installed [PI] — see
[ND-10176](../../ND-10176/README.md).

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10536-A1-EN.md](../../../Product-Info/ND-10536-A1-EN.md)
- Manual(s): `ND-60.171` COB-GEN Reference Manual (not located in this repo)

## Provenance & open items
- Source: floppy directory listing via `ndtool` on the downloaded image.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10536` product overview)
