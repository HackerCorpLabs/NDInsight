# ND-211157 — LED-DEBUGGER for ND-500/5000

> Status: VERIFIED (complete real PD sheet transcribed)

| Field | Value |
|-------|-------|
| ND article number | `ND-211157` (source article: `ND-250213` — shared with LED itself) |
| Product name | LED-DEBUGGER (Source Symbolic Debugger) |
| Functional category | Editors & Word Processing / Language Tools |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500/5000, CX type |
| OS requirement | SINTRAN III VS version K, work mode 312B, patch file 6034B |
| Related products | `ND-211160` LED (this product is the debugger integration bolted onto LED's window/region model, see [../ND-211160/README.md](../ND-211160/README.md)) |

## Description
A source-level symbolic debugger built into LED's editor window/region model — set breakpoints
and inspect state directly against your source text rather than a separate debugger console. This
revision adds the same auxiliary-process-control, compiler-message-mapping, and LEDCALL feature
set as the same-dated LED release, plus a speed optimization for the debug-table lookup algorithm
on large systems (>1000 pages of debug info), and states explicitly: **"Preparations for support
of ADA for ND-500/500 compiler are also included."** [PD]

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B (rev 03) | [ND-211157B03](ND-211157B03/README.md) | verified (PD sheet transcribed) | dated 88.08.03; floppy `211157B03-XX-01D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211157-2-EN.md](../../Installation-Description/ND-211157-2-EN.md)
- Manual(s): `ND-60.266.2` LED User Guide, `ND-60.158.5` Symbolic Debugger User Guide — both in
  this repo, see [ND-211160](../ND-211160/README.md) and [ND-210336](../ND-210336/README.md) for
  the exact filenames

## Provenance
Complete 5-page PD sheet found in the Installation-Description archive.

---
**Parent:** [../README.md](../README.md) (Software catalog)
