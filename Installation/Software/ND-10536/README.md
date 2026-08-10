# ND-10536 — COB-GEN (COBOL Code Generator)

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10536` |
| Product name | COB-GEN for ND-100/ND-10 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 |
| OS requirement | Prerequisite: COBOL compiler release F or later [PI] |
| Related products | `ND-10176` COBOL for ND-100 (COB-GEN generates `PROCEDURE DIVISION` source for it) |

## Description
Generates COBOL `PROCEDURE DIVISION` source code for screen input/output, input validation, and
reports — draw a `PICTURE`/`REPORT` layout with a `@PICTURE`/`@REPORT` description file, run
COB-GEN, `COPY` the generated sections into your program. Full feature description with a worked
example: [../../Product-Info/ND-10536-A1-EN.md](../../Product-Info/ND-10536-A1-EN.md). [PI]

**No install PD sheet located.**

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| C00 | [ND-10536C00](ND-10536C00/README.md) | IN-PROGRESS — no PD sheet, procedure inferred | floppy `10536C00-1D` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-10536-A1-EN.md](../../Product-Info/ND-10536-A1-EN.md)
- Manual(s): `ND-60.171` COB-GEN Reference Manual (not located in this repo) · `ND-60.144` COBOL Reference Manual
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading the image (MD5 `674e0780a9837536110ee3775a2987bd`,
labeled on disk as `10536C00-1D` despite the source folder's own filename saying
`10535c00-xx-1d` — the mounted volume label is authoritative) and reading with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
