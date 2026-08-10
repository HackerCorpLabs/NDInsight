# ND-10191A — Fortran 77 for ND-100/NORD-10, version A

> Status: IN-PROGRESS — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [INF]

| Field | Value |
|-------|-------|
| Part number | `10191A` (per-part suffixes below) |
| Base product | [`ND-10191`](../README.md) |
| Version | A |
| Release date | files dated 1982-06-11 (PART1, PART2) and 1982-06-15 (PART3) |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown |

## Description
The original, complete, matched 3-part release — every part carries the same `A` revision
letter. This is the reference point for the D-revision comparison in the
[product overview](../README.md).

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10191A-PART1` | `FORTRAN-100-A:PROG` (74 pages, pre-linked) and `FORTRAN-100-A:BPUN` (65 pages, raw) — **the compiler, shipped in both forms** |
| `ND-10191A-PART2` | `FORTRAN-1BANK-A:BRF` (45 pages), `FORTRAN-2BANK-A:BRF` (46 pages) — 48-bit floating-point runtime banks |
| `ND-10191A-PART3` | `F32FORT-1BANK-A:BRF` (45 pages), `F32FORT-2BANK-A:BRF` (46 pages) — 32-bit floating-point runtime banks |

All confirmed by downloading each image and reading with `ndtool -t` (MD5s in the
[product overview](../README.md)).

PART1 shipping the compiler as **both** `:PROG` and `:BPUN` is unusual in this catalog — every
other product picks one shape. The `:PROG` is presumably for a straight copy-and-run install; the
`:BPUN` for a `@DUMP-REENTRANT` shared install (see procedure below).

## Installation procedure — INFERRED, NOT CONFIRMED

No `:MODE` install script and no PD sheet exist for this product. By pattern with the other
`:BPUN`/`:BRF`-shaped compilers in this catalog (PLANC A/B, ND-10023K):

1. Enter each floppy directory in turn and copy the files to `SYSTEM` (or wherever you keep
   compiler binaries) — pick either the compiler's `:PROG` (simplest, but not shared/reentrant) or
   its `:BPUN` (for the reentrant path below):
   ```
   @ENTER-DIRECTORY ND-10191A-PART1,FLOPPY-DISC-1,0,
   @COPY-FILE "FORTRAN-100-A:BPUN",(SYSTEM)FORTRAN-100:BPUN
   @RELEASE-DIRECTORY ND-10191A-PART1
   @ENTER-DIRECTORY ND-10191A-PART2,FLOPPY-DISC-1,0,
   @COPY-FILE "FORTRAN-1BANK-A:BRF",(SYSTEM)FORTRAN-1BANK:BRF
   @COPY-FILE "FORTRAN-2BANK-A:BRF",(SYSTEM)FORTRAN-2BANK:BRF
   @RELEASE-DIRECTORY ND-10191A-PART2
   ```
   and PART3 similarly if the target machine is 32-bit floating-point instead of 48-bit — the
   two bank pairs are format-specific alternatives, not both-required.
2. Dump the compiler reentrant. **Start/restart addresses are NOT known** — nothing read so far
   states them for this product:
   ```
   @DUMP-REENTRANT FORTRAN-100,<start-address>,<restart-address>,(SYSTEM)FORTRAN-100:BPUN
   ```
   Alternatively, if using the pre-linked `:PROG` copy instead, `@DUMP-PROGRAM-REENTRANT
   FORTRAN-100,(SYSTEM)FORTRAN-100:PROG` avoids needing the addresses at all.

**Do not treat the above as verified.**

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10191-A1-EN.md](../../../Product-Info/ND-10191-A1-EN.md)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual

## Provenance & open items
- Source: floppy directory listings only, via `ndtool` on the downloaded images.
- **TODO:** find the compiler's start/restart addresses if the `:BPUN`/`DUMP-REENTRANT` path is
  used instead of `:PROG`/`DUMP-PROGRAM-REENTRANT`.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10191` product overview)
