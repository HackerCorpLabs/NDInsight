# ND-211049 — SQL for ND-500

> Status: VERIFIED (complete real 6-page PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-211049` (source article: `ND-250195`) |
| Product name | SQL for ND-500 |
| Functional category | Database / query tool (SIBAS query-language exception — a genuine query-language compiler, unlike plain application products) |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | SINTRAN III >= K |
| Related products | `ND-210340` SIBAS-II for ND-500 (hard prerequisite) · `ND-210319` ND-500 Linkage-Loader, version >= H (hard prerequisite) — see [../ND-210166/README.md](../ND-210166/README.md) for SIBAS and [../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md) for NLL |

## Description
An interactive relational SQL front-end for querying **existing** SIBAS-II databases. **This "A"
version only implements the DML part of SQL** — you still define/create new databases with
`SIB2-DRL` (SIBAS's own DDL — see
[SIBAS-DATABASE-PROGRAMMING.md](../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md)), not
with SQL `CREATE TABLE`. [PD] This is the closest thing this repo has found to an SQL-style query
layer over SIBAS.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A (rev 02) | [ND-211049A02](ND-211049A02/README.md) | verified (PD sheet transcribed) | dated 87.02.10; 2 double-density or 7 single-density floppies |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211049-1-EN.md](../../Installation-Description/ND-211049-1-EN.md)
- Product Information (PI-sheet): [../../Product-Info/ND-211049-A1-EN.md](../../Product-Info/ND-211049-A1-EN.md), [../../Product-Info/ND-211049-A3-EN.md](../../Product-Info/ND-211049-A3-EN.md)
- Manual(s): `ND-60.258.1` SQL User Manual (not located in this repo)

## Provenance
Complete 8-page PD sheet: metadata, install procedure, SIBAS-version patching, starting/
regenerating the domain, and the diskette listing.

---
**Parent:** [../README.md](../README.md) (Software catalog)
