# ND-211226 — Backup Manager for ND-500/5000

> Status: IN-PROGRESS — real decoded MODE scripts, installer identified by analogy to the verified NLL installer

| Field | Value |
|-------|-------|
| ND article number | `ND-211226` |
| Product name | Backup Manager for ND-500/5000 |
| Functional category | Backup / Storage / Disk Utilities |
| ND doc-category tag | 80 Utility software [curated] |
| CPU target | ND-500 / ND-5000 |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-210337`/`ND-10337` Backup-System (the older, simpler ND-100 file-copy backup tool — this is a different, much larger scheduling/automation product for ND-500, not a newer revision of it) · SIBAS-II (optional integration, see below) |

> **A floppy labeled `ND-211226A` exists in a friend's recently-imaged batch, but its label was
> overwritten with "GAMES" and its contents match that, not this product — do not use it.** See
> [research/FLOPPY-BACKLOG-2026-08.md](../research/FLOPPY-BACKLOG-2026-08.md). The floppies used
> for this entry are a separate, verified `211226B02` set from the Gandalf library.

## Description
A screen-oriented backup-scheduling/automation tool — "Screenshots of backup manager interface"
per the PI sheet's own cover image. Ships as an installer plus four ND-500 domains
(`BM-OPERATOR`, `BM-DEFINITION`, `BM-SCHEDULER`, `BM-FILERESTORE`) and a background scheduler
server process (`BMSERV`). Optionally integrates with **SIBAS/R** — its own job database
(`BM-JOBDB-B02:DATA`) can be stored in SIBAS, via a link-time choice between two NRF modules
(`BM-OSIBAS-B02:NRF` with SIBAS, `BM-SIBDUM-B02:NRF` a dummy stub without it).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B (rev 02) | [ND-211226B02](ND-211226B02/README.md) | IN-PROGRESS — real decoded MODE scripts, installer not run live | floppies `211226B02-XX-01D` through `-04D` (English variant also exists: `-EN-01D`/`-EN-02D`) |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-211226-A1-EN.md](../../Product-Info/ND-211226-A1-EN.md)
- Manual(s): none identified

## Provenance
Floppy contents and MODE scripts confirmed by downloading all four `-XX-` disks and reading/
decoding with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
