# ND-210190 — FORTRAN for ND-500

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-210190` |
| Product name | FORTRAN for ND-500 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | SINTRAN III |
| Related products | `ND-10190` — older article number for the same product family (not documented here — user has no floppy for it) · `ND-210191` FORTRAN for ND-100/NORD-10 (sibling, see [../ND-210191/README.md](../ND-210191/README.md)) · `ND-210863` FORTRAN Crosscompiler for ND-100 running on ND-500 |

## Description
Native ND-500 FORTRAN compiler (as opposed to `ND-210863`, which is the ND-500-hosted
cross-compiler that *generates* ND-100 code). Per the shared ND-FORTRAN Product-Info sheet:
[../../Product-Info/ND-210190-B1-EN.md](../../Product-Info/ND-210190-B1-EN.md). [PI]

**This product was missing from this catalog's own index entirely** (only the older `ND-10190`
was listed) — added now that a real floppy for it was confirmed. `[fixed 2026-08-10]`

## A genuine installer, unlike the ND-100 FORTRAN products

Unlike every ND-100 FORTRAN floppy checked so far (plain files, manual copy-and-dump, no
install script), this ND-500 floppy carries a **real installer program**
(`IN-FORT-XX-K02:PROG`) in the same family as the ND-500 Linkage-Loader installer already
documented in [../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
— domain-based (`:LINK`/`:DSEG`/`:PSEG` segment files), driven by an embedded XCOM
command-template engine. See [ND-210190K02](ND-210190K02/README.md) for what was recovered from
the binary.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| K02 | [ND-210190K02](ND-210190K02/README.md) | IN-PROGRESS — installer identified, exact dialogue not extracted | floppy `210190K02-XX-01D` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-210190-B1-EN.md](../../Product-Info/ND-210190-B1-EN.md)
- Manual(s): `ND-60.145` ND FORTRAN Reference Manual — [../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents and installer analysis: downloaded image (MD5 `7cde7e416ca1ec59c9698dc06896d4a0`),
read with `ndtool`, installer binary's embedded string constants extracted for analysis (not a
live run).

---
**Parent:** [../README.md](../README.md) (Software catalog)
