# ND-210400 — Subsystem Package II

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-210400` |
| Product name | Subsystem Package II |
| Functional category | System Utilities & Subsystem Packages |
| ND doc-category tag | 80 Utility software [curated] |
| CPU target | ND-100 (also runs on NORD-10/12 per the PD sheet's floating-point split, see below) |
| OS requirement | SINTRAN III, version >= H |
| Related products | `ND-10400` (earlier/base article number for the same product, index-only — no PD sheet located yet); `ND-10005`/`ND-10044` "Subsystem Package" (older 32-bit/48-bit NORD-10 predecessors) |

## Description
Subsystem Package II is Norsk Data's standard developer-tools bundle for a SINTRAN III system:
the **QED** editor, the **MAC/FMAC** macro assembler (shipped in separate 48-bit and 32-bit
floating-point builds), **NORD-PL (NPL)**, and **DITAP** (a file dump/copy utility used by the
install procedure itself). The PD sheet states its purpose plainly: "Basic Utilities for
SINTRAN III." [PD]

This is usually the **first** product installed on a bare SINTRAN III system — without it there
is no editor and no assembler on the machine. It is also the install vehicle for two of this
repo's Developer language guides: [MAC-DEVELOPER-GUIDE.md](../../../Developer/Languages/System/MAC-DEVELOPER-GUIDE.md)
and [NPL-DEVELOPER-GUIDE.md](../../../Developer/Languages/System/NPL-DEVELOPER-GUIDE.md).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B | [ND-210400B](ND-210400B/README.md) | verified (PD sheet transcribed, cross-checked against 2 independent scans) | dated 87.05.20; floppy `210400B00-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../OS/SUBSYSTEM/210400B_Subsystem_Package_II_combined.md](../../OS/SUBSYSTEM/210400B_Subsystem_Package_II_combined.md) and the independent second scan [../../OS/SUBSYSTEM/ND0117.md](../../OS/SUBSYSTEM/ND0117.md)
- Product Information (PI-sheet): not located
- Manual(s) (per the PD sheet's "Documentation" table): `ND-60.096.01 EN` MAC User's Guide ·
  `ND-60.151.2A EN` SINTRAN III Utilities Manual · `ND-60.031.4C EN` QED User's Manual ·
  `ND-60.047.3A EN` NORD PL User's Guide
- NDWIKI: not checked yet

## Provenance
ND article number and category tag: `Installation/Software/README.md` catalog (curated). Product
facts and module list: the two independent OCR'd PD-sheet scans cited above, which agree on every
field checked so far except one digit in a module name (flagged in the version doc).

---
**Parent:** [../README.md](../README.md) (Software catalog)
