# ND-210191F02 — Fortran 77 for ND-100/NORD-10, version F02

> Status: IN-PROGRESS — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `210191F02` |
| Base product | [`ND-210191`](../README.md) |
| Version | F02 |
| Release date | files dated 1986-11-19/20 |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown |

## Description
Everything ND-10191 needed three floppies for, consolidated onto one double-density disk: the
compiler (pre-linked) and both floating-point runtime bank pairs.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210191F02-XX-01D` | `FORTRAN-100-F02:PROG` (137 pages, the compiler, pre-linked) · `FORT48-1BANK-F02:BRF` (54 pages), `FORT48-2BANK-F02:BRF` (56 pages) — 48-bit runtime · `FORT32-1BANK-F02:BRF` (54 pages), `FORT32-2BANK-F02:BRF` (56 pages) — 32-bit runtime, user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `5b6a3ad3e72571540755b93d1607ff09`) and reading with
`ndtool -t`. No `:MODE` install script present — same as every other FORTRAN floppy checked so
far, this is a manual copy-and-dump install, not a scripted one.

## Installation procedure — INFERRED, NOT CONFIRMED

1. Enter the floppy directory and copy the compiler plus whichever bank pair matches the target
   machine's floating-point format:
   ```
   @ENTER-DIRECTORY 210191F02-XX-01D,FLOPPY-DISC-1,0,
   @COPY-FILE "FORTRAN-100-F02:PROG",(SYSTEM)FORTRAN-100:PROG
   @COPY-FILE "FORT48-1BANK-F02:BRF",(SYSTEM)FORT48-1BANK:BRF
   @COPY-FILE "FORT48-2BANK-F02:BRF",(SYSTEM)FORT48-2BANK:BRF
   ```
2. Dump the compiler reentrant. Because this ships as `:PROG` (pre-linked), use
   `DUMP-PROGRAM-REENTRANT` — no start/restart addresses needed, unlike the raw-`:BPUN` shape:
   ```
   @DUMP-PROGRAM-REENTRANT FORTRAN-100,(SYSTEM)FORTRAN-100:PROG
   ```

**Do not treat the above as verified.**

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-210190-B1-EN.md](../../../Product-Info/ND-210190-B1-EN.md) (shared ND-FORTRAN family sheet)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual

## Provenance & open items
- Source: floppy directory listing only, via `ndtool` on the downloaded image.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210191` product overview)
