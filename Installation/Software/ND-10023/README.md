# ND-10023 — FORTRAN (48-bit)

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10023` |
| Product name | FORTRAN (48-bit) |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | NORD-10 (48-bit floating-point machines) |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-10033` FORTRAN (32-bit, sibling product for 32-bit float machines) · `ND-10067` FORTRAN Runtime system (48-bit) · `ND-10191` Fortran 77 for ND-100/NORD-10 (later ANSI-77 successor, see [../ND-10191/README.md](../ND-10191/README.md)) |

## Description
The original ND FORTRAN compiler for NORD-10, 48-bit floating-point format. Later superseded for
ND-100/NORD-10 by the ANSI-77 [ND-10191](../ND-10191/README.md) compiler, which the ND-10191
Product-Info sheet explicitly states remains compatible with "those features of the ND FORTRAN
products ND 10023/10033 which are ... in addition to ANSI-77" — i.e. ND-10191 is a superset, not
a clean break. **No Program Description or Product Information sheet for ND-10023 has been
located** — everything below is read directly off the floppy. `[OBS]`

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| K | [ND-10023K](ND-10023K/README.md) | IN-PROGRESS — no PD sheet, procedure inferred by pattern | floppy `ND-10023K` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual — [../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)
  (per the ND-10191 Product-Info sheet's documentation reference, likely applies to this earlier
  compiler too, but not confirmed against an ND-10023-specific manual reference)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading the image from the ND floppy library (MD5
`d5799ff428b0bc95b980bd90167ead26`) and reading it with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
