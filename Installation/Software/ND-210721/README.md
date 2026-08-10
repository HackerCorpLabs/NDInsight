# ND-210721 — BRF-Linker for ND-100

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-210721` |
| Product name | BRF-Linker for ND-100 |
| Functional category | Language Tools — Linkers / Loaders / Debuggers / Assemblers / Monitors |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-10 / ND-100 |
| OS requirement | SINTRAN III VS, version >= H |
| Related products | `ND-10336` Symbolic Debugger for ND-100 (its `STACK-INSTRUCTIONS` command can speed up the BRF-Linker — see version doc) |

## Description
Loads and links BRF-units (relocatable code produced by MAC/FMAC and other ND-100 compilers) into
program files, with a built-in BRF editor (`PREFIX-BRF` to rename symbol sets,
`APPEND-BRF`/`REPLACE-BRF`, library file preparation). This is the ND-100-side counterpart to the
ND-500's NRF-based linkers (`ND-10319`/`ND-210319` Linkage-Loader, `ND-211224` ND Linker — see
[../ND-211224/README.md](../ND-211224/README.md)).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| C (revision 01) | [ND-210721C](ND-210721C/README.md) | verified (PD sheet transcribed, cross-checked against the mounted floppy) | dated 87.09.03; floppy `210721C01-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-210721-3-EN.md](../../Installation-Description/ND-210721-3-EN.md)
- Product Information (PI-sheet): not located
- Manual(s): `ND-60.196.2` BRF-LINKER User Manual (not located in this repo's Reference-Manuals —
  note there is also `ND-60.085.01 BRF EDITOR.md`, a different/related tool, do not confuse the two)
- NDWIKI: not checked yet

## Provenance
PD sheet: `Installation-Description/ND-210721-3-EN.md`. Floppy contents confirmed by downloading
the image (MD5 `68dae73b565995151d2966b89a4c685f`) and reading with `ndtool` — matches the PD
sheet's own diskette listing exactly (one file, `BRF-LINKER-C01:PROG`, 45 pages).

---
**Parent:** [../README.md](../README.md) (Software catalog)
