# ND-10166 — SIBAS II Database System (older article number)

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10166` |
| Product name | SIBAS-II Data Base System for ND-100 |
| Functional category | Databases & File Access |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 (a parallel `ND-500`-side product exists — the F-revision PD sheet references "SIBAS-500" as a separately ordered product) |
| OS requirement | SINTRAN III/VS |
| Related products | `ND-210166` — later `21`-prefixed article for the same product, see [../ND-210166/README.md](../ND-210166/README.md) · `ND-210729` UNIQUE-II SIBAS · `ND-210340` SIBAS-II for ND-500 · `ND-210197` SIBAS Backend (COSMOS remote-database access, ordered separately) |

## What SIBAS-II is

A CODASYL-DBTG-style database management system — the first full CODASYL implementation on a
minicomputer, originally from the Central Institute for Industrial Research (Oslo), later
co-developed with A/S Shipping Research Services and Norsk Data. Same database format as the
IBM 360/370 and Univac 1100 versions. Full description:
[../../Product-Info/ND-10166-A1-EN.md](../../Product-Info/ND-10166-A1-EN.md). [PI]

**Application languages:** FORTRAN, COBOL, BASIC, NORD-PL, and Assembly — SIBAS is a "host
language" DBMS: it adds DML (Data Manipulation Language) call facilities to these languages
rather than providing its own standalone query language. [PI]

**Core concepts** (see [../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md](../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md)
for the full chapter): records, realms, CALC/SERIAL location modes, primary/secondary
indices (B-trees), CODASYL set types (single-member/involuted), run-units, before/after-image
logging, checkpoints.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| E (rev 10/00) | [ND-10166E](ND-10166E/README.md) | IN-PROGRESS — floppy contents verified; install procedure adapted from the later F-revision's real PD sheet (structurally near-identical file set) | 9-part floppy set `ND-10166E-PART1..9` |

## Documentation
- Program Description (PD-sheet): not located for the E revision specifically — see
  [ND-210166](../ND-210166/README.md) for the F-revision PD sheet used as the install reference
- Product Information (PI-sheet): [../../Product-Info/ND-10166-A1-EN.md](../../Product-Info/ND-10166-A1-EN.md)
- Manual(s): `ND-60.127.5` SIBAS-II ND User Manual — [../../../Reference-Manuals/ND-60.127.5 EN THE DATABASE SYSTEM SIBAS II ND User Manual.md](../../../Reference-Manuals/ND-60.127.5%20EN%20THE%20DATABASE%20SYSTEM%20SIBAS%20II%20ND%20User%20Manual.md)
  · `ND-30.009.3` SIBAS-II Operator's Manual — [../../../Reference-Manuals/ND-30.009.3 EN SIBAS II Operator Manual.md](../../../Reference-Manuals/ND-30.009.3%20EN%20SIBAS%20II%20Operator%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading all 9 images and reading with `ndtool`. Install
reasoning cross-referenced against the real, complete `210166F` PD sheet (see
[ND-210166](../ND-210166/README.md)).

---
**Parent:** [../README.md](../README.md) (Software catalog)
