# ND-210166 — SIBAS II for ND-100 (later article number)

> Status: VERIFIED (complete real PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-210166` (also referenced as `ND 210340` for the ND-500 side on the PI sheet) |
| Product name | SIBAS-II Database System for ND-100/ND-500 |
| Functional category | Databases & File Access |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 (this article); `ND-210340` is the ND-500 sibling |
| OS requirement | SINTRAN III version I or newer (K if more than 12 SIBAS processes are wanted) |
| Related products | `ND-10166` — older article for the same product, see [../ND-10166/README.md](../ND-10166/README.md) · `ND-210340` SIBAS-II for ND-500 · `ND-210197` SIBAS Backend (COSMOS remote access, ordered separately) · `ND-210729` UNIQUE-II SIBAS |

## Description
Same product as `ND-10166`, later article. Full marketing description (CODASYL concepts, DDL/DML
separation, distributed access via COSMOS, dictionary/DIALOGUE integration):
[../../Product-Info/ND-210166-A1-EN.md](../../Product-Info/ND-210166-A1-EN.md). [PI] See
[../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md](../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md)
for the full programming/DDL/DML chapter.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| F (rev 02) | [ND-210166F02](ND-210166F02/README.md) | **VERIFIED — complete real PD sheet, install + operations fully transcribed** | dated 86.09.25; floppy `210166F02-XX-01D`/`-02D` (or a 10-disk single-density set) |

## Documentation
- Program Description (PD-sheet): [../../../Reference-Manuals/210166F SIBAS II for ND-100.md](../../../Reference-Manuals/210166F%20SIBAS%20II%20for%20ND-100.md)
- Product Information (PI-sheet): [../../Product-Info/ND-210166-A1-EN.md](../../Product-Info/ND-210166-A1-EN.md)
- Manual(s): `ND-60.127.5` SIBAS-II ND User Manual — [../../../Reference-Manuals/ND-60.127.5 EN THE DATABASE SYSTEM SIBAS II ND User Manual.md](../../../Reference-Manuals/ND-60.127.5%20EN%20THE%20DATABASE%20SYSTEM%20SIBAS%20II%20ND%20User%20Manual.md)
  · `ND-30.009.3` SIBAS-II Operator's Manual — [../../../Reference-Manuals/ND-30.009.3 EN SIBAS II Operator Manual.md](../../../Reference-Manuals/ND-30.009.3%20EN%20SIBAS%20II%20Operator%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Complete 17-page PD sheet found filed under `Reference-Manuals/` (the same filing quirk already
seen for PLANC's A/B sheets and ND-10335B), covering metadata, full errors-corrected /
modifications history, the complete installer Q&A dialogue, manual mode-file editing, cold-start
persistence, and database-conversion procedure all in one document. Floppy contents confirmed
against the floppy-image reference catalog (temporary working reference, not committed to this
repo).

---
**Parent:** [../README.md](../README.md) (Software catalog)
