# ND-10191 — Fortran 77 for ND-100/NORD-10

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10191` |
| Product name | FORTRAN for ND-100/NORD-10 (ANSI-77) |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-10023`/`ND-10033` earlier FORTRAN compilers (this product is upward-compatible with their extensions to ANSI-77, per the PI sheet) · `ND-210191` — later `21`-prefixed article for the same product, see [../ND-210191/README.md](../ND-210191/README.md) · `ND-10190`/`ND-210190` sibling compiler for ND-500 |

## Description
ND FORTRAN based on the ANSI-77 standard (ANSI X3.9-1978), full implementation plus extensions.
Full feature list: [../../Product-Info/ND-10191-A1-EN.md](../../Product-Info/ND-10191-A1-EN.md). [PI]

**No Program Description or Product Information install sheet has been located** for this
product — only the general marketing PI sheet, and the floppies themselves. `[OBS]`

## The 3-part floppy set, and why versions mix letters

This product ships as **three separate floppies**: PART1 (the compiler itself — does not depend
on target floating-point format), PART2 (48-bit floating-point runtime banks), PART3 (32-bit
floating-point runtime banks). Two labeled generations were found in the floppy library:

- A **complete, matched set** at revision **A**: `ND-10191A-PART1`, `ND-10191A-PART2`,
  `ND-10191A-PART3` (dated June 1982).
- A **partial, later revision** at letter **D**: `ND-10191D-PART2`, `ND-10191D-PART3` only
  (dated March 1984) — **no `ND-10191D-PART1` has been found**, in this repo's temporary floppy
  reference or otherwise.

The D-revision files were also **renamed** — `FORTRAN-1BANK-A`/`F32FORT-1BANK-A` (A) became
`FORT48-1BANK-D00`/`FORT32-1BANK-D00` (D) — a naming-convention change, not just a version-letter
bump, which is consistent with PART2/PART3 having been reworked independently of PART1.

**Working hypothesis, not confirmed:** the compiler (PART1) was never revised past A, while the
floating-point runtime banks (PART2/PART3) were later patched to D. Under this hypothesis, your
physical set — `ND-10191A-PART1` + `ND-10191D-PART2` + `ND-10191D-PART3` — is a normal, correct
combination (the latest of each part), not a mismatched/broken set. This is **not proven**: it is
equally possible a `D`-revision PART1 exists and simply wasn't found in the reference library used
here. If you find or acquire a floppy labeled `ND-10191D-PART1` (or any other PART1 letter later
than A), that would resolve the question either way.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A | [ND-10191A](ND-10191A/README.md) | IN-PROGRESS — no PD sheet, procedure inferred | full matched 3-part set: `ND-10191A-PART1/2/3` |
| D | [ND-10191D](ND-10191D/README.md) | IN-PROGRESS — PART1 missing/unconfirmed, see above | `ND-10191D-PART2/3` only; pairs with A's PART1 in practice |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-10191-A1-EN.md](../../Product-Info/ND-10191-A1-EN.md)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual — [../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading all five images from the ND floppy library and reading
them with `ndtool`: `ND-10191A-PART1` (MD5 `ddbe57cf408b3a1dd8a5fbdebd6394fc`), `-PART2`
(`f01896b2106b273a53afd9e1b03cccda`), `-PART3` (`372b44af118bd032ca7b7aae4723ce50`);
`ND-10191D-PART2` (`8abbcc51ffbe7f45e6a8c297cf3c2e99`), `-PART3` (`84c15a0ddef7a096dae59332bf0e6757`).

---
**Parent:** [../README.md](../README.md) (Software catalog)
