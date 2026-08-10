# ND-210191 — Fortran 77 for ND-100/NORD-10 (later article)

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-210191` |
| Product name | Fortran 77 for ND-100/NORD-10 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 / NORD-10 |
| OS requirement | SINTRAN III (per the ND-FORTRAN family Product-Info sheet's general requirement) |
| Related products | `ND-10191` — older article number for the same product family, 3-part floppy set, see [../ND-10191/README.md](../ND-10191/README.md) · `ND-210190` FORTRAN for ND-500 (sibling, see [../ND-210190/README.md](../ND-210190/README.md)) · `ND-210863` FORTRAN Crosscompiler for ND-100 running on ND-500 |

## Description
Same product as `ND-10191`, re-issued under the later `21`-prefixed article number — the same
pattern already seen for Subsystem Package II (`ND-10400`→`ND-210400`) and Backup-System
(`ND-10337`→`ND-210337`). Per the combined ND-FORTRAN Product-Info sheet:
[../../Product-Info/ND-210190-B1-EN.md](../../Product-Info/ND-210190-B1-EN.md) (covers
`ND-210190`/`ND-210191`/`ND-210863` together). [PI]

**No product-specific PD sheet located.**

## What changed vs. the older ND-10191

The floppy set shrank from three disks to **one**: `210191F02-XX-01D` carries the compiler
(pre-linked `:PROG`) plus **all four** runtime bank files (both 48-bit and 32-bit floating-point)
on a single double-density diskette — see [ND-210191F02](ND-210191F02/README.md).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| F02 | [ND-210191F02](ND-210191F02/README.md) | IN-PROGRESS — no PD sheet, procedure inferred | single floppy `210191F02-XX-01D` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-210190-B1-EN.md](../../Product-Info/ND-210190-B1-EN.md) (shared sheet for the ND-FORTRAN family)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual — [../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading the image (MD5 `5b6a3ad3e72571540755b93d1607ff09`) and
reading it with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
