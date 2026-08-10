# ND-10191D — Fortran 77 for ND-100/NORD-10, version D (partial)

> Status: STUB — PART1 missing/unconfirmed, no PD sheet, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `10191D00` (per-part suffixes below) |
| Base product | [`ND-10191`](../README.md) |
| Version | D (confirmed for PART2/PART3 only — see below) |
| Release date | files dated 1984-03-26 |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown |

## Description
A later revision of the floating-point runtime banks only. **This entry does NOT have its own
PART1** — see the [product overview](../README.md) for the open question of whether a
`ND-10191D-PART1` exists at all, or whether the compiler simply stayed at revision A. **Use
[ND-10191A](../ND-10191A/README.md)'s PART1 for the compiler** until this is resolved.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| PART1 | **not found** — see product overview |
| `ND-10191D-PART2` | `FORT48-1BANK-D00:BRF` (53 pages), `FORT48-2BANK-D00:BRF` (55 pages) — 48-bit floating-point runtime banks |
| `ND-10191D-PART3` | `FORT32-1BANK-D00:BRF` (53 pages), `FORT32-2BANK-D00:BRF` (55 pages) — 32-bit floating-point runtime banks |

Confirmed by downloading both images and reading with `ndtool -t` (MD5s in the
[product overview](../README.md)). Note the file names changed convention from A's
`FORTRAN-1BANK-A`/`F32FORT-1BANK-A` to `FORT48-1BANK-D00`/`FORT32-1BANK-D00` — larger file sizes
too (53-55 pages here vs. 45-46 pages in A), consistent with a genuine content revision, not just
a relabel.

## Installation procedure — INFERRED, NOT CONFIRMED

Same shape as [ND-10191A](../ND-10191A/README.md)'s procedure, substituting these two floppies
for PART2/PART3 and A's PART1 for the compiler itself:

```
@ENTER-DIRECTORY ND-10191A-PART1,FLOPPY-DISC-1,0,
@COPY-FILE "FORTRAN-100-A:BPUN",(SYSTEM)FORTRAN-100:BPUN
@RELEASE-DIRECTORY ND-10191A-PART1
@ENTER-DIRECTORY ND-10191D-PART2,FLOPPY-DISC-1,0,
@COPY-FILE "FORT48-1BANK-D00:BRF",(SYSTEM)FORT48-1BANK:BRF
@COPY-FILE "FORT48-2BANK-D00:BRF",(SYSTEM)FORT48-2BANK:BRF
@RELEASE-DIRECTORY ND-10191D-PART2
```
(substitute PART3's `FORT32-*` files instead, if targeting a 32-bit floating-point machine)

Then dump the compiler reentrant exactly as in ND-10191A's procedure — same unresolved
start/restart-address gap applies.

**Do not treat the above as verified**, and do not treat the A/D mixing as confirmed-safe — see
the product overview's "working hypothesis, not confirmed" note.

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10191-A1-EN.md](../../../Product-Info/ND-10191-A1-EN.md)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual

## Provenance & open items
- Source: floppy directory listings only, via `ndtool` on the downloaded images.
- **TODO (blocking):** determine whether `ND-10191D-PART1` exists. Until then this cannot be
  called a complete, self-consistent version.
- **TODO:** find the compiler's start/restart addresses.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10191` product overview)
