# ND-210887 — AUTOMAKE for ND-500

> Status: VERIFIED (complete real PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-210887` (source article: `ND-250094`) |
| Product name | AUTOMAKE for ND-500 |
| Functional category | Build tool (make-equivalent) |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | SINTRAN III VSX >= H |
| Related products | `ND-210886` AUTOMAKE for ND-100 (companion, see [../ND-210886/README.md](../ND-210886/README.md)) |

## Description
Same tool as `ND-210886`, ND-500 build — ships as a proper domain (`:LINK`/`:DSEG`/`:PSEG` +
`DESCRIPTION-FILE`) instead of a plain `:PROG`, loaded via the ND-500 Linkage-Loader's
`COPY-DOMAIN` rather than a plain file copy.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B (rev 00) | [ND-210887B00](ND-210887B00/README.md) | verified — first official release | dated 86.08.18; floppy `210887B00-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-210887-2-EN.md](../../Installation-Description/ND-210887-2-EN.md)
- Product Information (PI-sheet): [../../Product-Info/ND-210886-A1-EN.md](../../Product-Info/ND-210886-A1-EN.md) (shared with the ND-100 sibling)
- Manual(s): `ND-60.232.03` AUTOMAKE User Guide (not located in this repo)

## Provenance
Single-page PD sheet (metadata + install procedure) plus diskette listing.

---
**Parent:** [../README.md](../README.md) (Software catalog)
