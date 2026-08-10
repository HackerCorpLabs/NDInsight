# ND-211005 — UNIQUE Text System

> Status: VERIFIED (transcribed from a complete real PD sheet)

| Field | Value |
|-------|-------|
| ND article number | `ND-211005` |
| Product name | UNIQUE Text System |
| Functional category | 4th-generation application tools (DIALOGUE family) |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 / ND-500 |
| OS requirement | SINTRAN VS/VSX, version > I |
| Related products | `ND-210729` UNIQUE II for SIBAS/ND-100 (see [../ND-210729/README.md](../ND-210729/README.md)) and its siblings `ND-210730` (SIBAS/ND-500), `ND-210731` (ISAM/ND-100), `ND-210895` (ISAM/ND-500), plus UNIQUE UNIQUICK's own four: `ND-210871`/`ND-210872`/`ND-210896`/`ND-210897` — none of these siblings have a PD/PI sheet or floppy located in this repo yet |

## Description
The **language-dependent text/message base every UNIQUE product reads from** — "All
messages/texts used by Unique products come from the language dependent UNIQUE Text System files
under user DIALOG-SYS. The Text System must be properly installed before any UNIQUE products may
be run." [PD] Not a standalone application — install this first, before any UNIQUE II/UNIQUICK
product.

Ships in English, Norwegian, Swedish, German (Swedish/German "not yet available" as of this PD
sheet's date) — English/German/Swedish all use English example applications; only the Norwegian
package has Norwegian examples.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| C (rev 04) | [ND-211005C](ND-211005C/README.md) | verified (PD sheet transcribed) | dated 88.02.05; floppies `211005C04-<lang>-01D` (+ 3 single-density disks per language) |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211005-3-EN.md](../../Installation-Description/ND-211005-3-EN.md)
- Manual(s): `ND-60.206.3` DIALOGUE UNIQUE-II User Guide · `ND-60.210.3` DIALOGUE UNIQUE-II Application Development · `ND-60.281.1` DIALOGUE UNIQUE-II Programming Examples (none located in this repo)
- NDWIKI: not checked yet

## Provenance
Complete 7-page PD sheet, covering metadata, full file list (including per-language diskette
directory listings for both the Norwegian and English releases), and install procedure.

---
**Parent:** [../README.md](../README.md) (Software catalog)
