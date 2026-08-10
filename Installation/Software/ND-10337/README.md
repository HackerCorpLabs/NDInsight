# ND-10337 — Backup-System (older article number)

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10337` |
| Product name | Backup-System |
| Functional category | Backup / Storage / Disk Utilities |
| ND doc-category tag | 80 Utility software [curated] |
| CPU target | ND-100 (also NORD-10 per the B PD sheet's "10" computer checkbox) |
| OS requirement | SINTRAN III VS (B, 1981) |
| Related products | `ND-210337` — later `21`-prefixed article number for the same product, with a fuller installer; see [ND-210337](../ND-210337/README.md) |

## Description
Earlier releases of the same Backup-System product later re-issued as `ND-210337`. Two versions
identified so far span a visible jump in install complexity: version **B** (1981) is a single
`:BPUN` file with a two-line manual copy+dump procedure; version **F** ships as a single already-
linked `:PROG` file with no PD sheet at all. Compare against the much more elaborate 5-module
`:PROG` installer used by the later I04/I05 releases under `ND-210337`.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B | [ND-10337B](ND-10337B/README.md) | verified (PD sheet transcribed) | dated 81.11.16; floppy `ND-10337B` |
| F | [ND-10337F](ND-10337F/README.md) | stub — no PD sheet, install inferred by pattern | floppy `ND-10337F` |

## Documentation
- Program Description (PD-sheet), version B: [../../Installation-Description/ND-10337-2-EN.md](../../Installation-Description/ND-10337-2-EN.md)
- Program Description, version F: not located
- Manual(s): `ND-60.151.01` SINTRAN III Utilities Manual (per the B PD sheet, "not released yet"
  as of 1981) · the later `ND-60.250 EN` BACKUP User Guide (same product family, see
  [../ND-210337/README.md](../ND-210337/README.md)) — [../../../Reference-Manuals/ND-60.250.1_EN_BACKUP_User_Guide.md](../../../Reference-Manuals/ND-60.250.1_EN_BACKUP_User_Guide.md)
- NDWIKI: not checked yet

## Provenance
Version B: `Installation-Description/ND-10337-2-EN.md`. Version F: floppy directory listing only
(temporary working reference, not committed to this repo) — a single file `BACKUP-F00:PROG`, no
PD/PI sheet found for this specific revision.

---
**Parent:** [../README.md](../README.md) (Software catalog)
