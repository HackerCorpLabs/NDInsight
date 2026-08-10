# ND-211037 — PLANC for ND-110, compiling on ND-500/5000

> Status: VERIFIED (complete real PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-211037` (source article: `ND-210370`) |
| Product name | PLANC for ND-110, compiling on ND-500/5000 |
| Functional category | Programming Languages & Compilers (cross-compiler) |
| ND doc-category tag | 60 General [curated] |
| CPU target | Hosted on ND-500/5000, generates ND-110/ND-100 code |
| OS requirement | SINTRAN III >= I |
| Related products | `ND-10309`/`ND-210761` PLANC for ND-100 (native compiler, see [../ND-10309/README.md](../ND-10309/README.md)) · sibling cross-compilers per the background sweep: `ND-211038` PLANC for MC68000 compiling on ND-500/5000, `ND-250405` PLANC for Intel-386 (neither documented here) |

## Description
A PLANC cross-compiler — runs on ND-500/5000 hardware but generates code for the ND-110 (a
member of the ND-100 family). Same install/runtime shape as the native PLANC compilers: a
compiler domain plus 1-bank/2-bank runtime BRF files. This revision (I) adds significant language
features over PLANC-H: routines as record components, a new routine-heading syntax, matching
routine names after `ENDROUTINE`, unsigned integer types, underscored integer constants, string
constants, public/private record members, `(% %)` block comments, hex I/O format, record-to-bytes
conversion, operator-priority routines, `XARGS`/`MAINSTART` routine modifiers, and several new
compiler commands (`$GET-VALUE`, `$GENERATE-IMPORTS`, `$LONG-NAMES`, `$EXPAND-MACROS`,
`$VERSION-INFORMATION`) — not exhaustively transcribed here, see the PD sheet for the full
9-page changelog. [PD]

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| I (rev 01) | [ND-211037I01](ND-211037I01/README.md) | verified (PD sheet transcribed) | dated 88.07.29; floppy `211037I01-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211037-9-EN.md](../../Installation-Description/ND-211037-9-EN.md)
- Manual(s): `ND-60.117.5` PLANC Reference Manual — [../../../Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md](../../../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)

## Provenance
Complete 10-page PD sheet: metadata, an extensive errors-corrected/new-features changelog, the
install procedure (both installer-driven and manual paths), and the diskette listing.

---
**Parent:** [../README.md](../README.md) (Software catalog)
