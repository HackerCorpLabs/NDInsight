# ND-211160 — LED for ND-500/5000 (Language Editor)

> Status: VERIFIED (complete real PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-211160` (source article: `ND-250213`) |
| Product name | LED (Language Program Editor) for ND-500/5000 |
| Functional category | Editors & Word Processing |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500/5000, CX type |
| OS requirement | SINTRAN III VS version K, work mode 312B, patch file 6034B |
| Related products | `ND-211157` LED-DEBUGGER (source-level debugger integrated into LED's window/region model, see [../ND-211157/README.md](../ND-211157/README.md)) · `ND-211159` LED-FORTRAN, `ND-211158` LED-PLANC (per-language syntax-editor variants, floppies exist, no PD sheet located) · `ND-230050` LED for OWS (no PD sheet/floppy located) |

## Description
An integrated syntax/source editor with language-aware indentation, pretty-print, and code
browsing (hide/show lower structure levels) for C and FORTRAN, windowed multi-region editing, an
auxiliary-process control mode (run a shell-like process attached to a region, `FUNC CTRL+E`),
compiler-message mapping (jump straight to warning/error lines, `SHIFT+EXECUTE`/`EXECUTE`), and a
user-programmable key system (LEDCALL services — load a program at runtime and bind it to any
key). [PD]

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B (rev 03) | [ND-211160B03](ND-211160B03/README.md) | verified (PD sheet transcribed) | dated 88.08.03; floppy `211160B03-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211160-2-EN.md](../../Installation-Description/ND-211160-2-EN.md)
- Product Information (PI-sheet): [../../Product-Info/ND-211160-A1-EN.md](../../Product-Info/ND-211160-A1-EN.md), [../../Product-Info/ND-211160-A2-EN.md](../../Product-Info/ND-211160-A2-EN.md)
- Manual(s): `ND-60.266.2` LED User Guide — [../../../Reference-Manuals/ND-60.266.2-EN LED User Guide.md](../../../Reference-Manuals/ND-60.266.2-EN%20LED%20User%20Guide.md)

## Provenance
Complete 6-page PD sheet: metadata, install procedure, and two full pages of feature
additions/error corrections, plus the diskette listing.

---
**Parent:** [../README.md](../README.md) (Software catalog)
