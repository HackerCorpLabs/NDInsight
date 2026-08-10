# ND-10335 — ND-500 Symbolic Debugger

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10335` |
| Product name | Symbolic Debugger for ND-500, Multiuser |
| Functional category | Language Tools — Linkers / Loaders / Debuggers / Assemblers / Monitors |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | SINTRAN III VS; this version requires ND-500-MONITOR Version B or later |
| Related products | `ND-10336` Symbolic Debugger for ND-100 (companion product, same PI sheet, single user manual `ND-60.158` covers both) |

## Description
One debugger product for FORTRAN, COBOL, and PLANC (PASCAL "to be added during 1982" per the PI
sheet) — runs in its own address space, symbolic references via `DEBUG-MODE`-compiled modules,
breakpoints (line/routine/address/conditional), call-hierarchy inspection, `DISPLAY`/`SET` for
variables, `LOOK-AT` for raw memory/registers/code with a built-in assembler/disassembler. Full
feature description: [../../Product-Info/ND-10335-C1-EN.md](../../Product-Info/ND-10335-C1-EN.md). [PI]

A PD sheet was located, filed under `Reference-Manuals/` (the same filing quirk seen with
PLANC's A/B sheets, not something changed here) — see
[Documentation](#documentation) below.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| B | [ND-10335B](ND-10335B/README.md) | verified (PD sheet transcribed, cross-checked against the mounted floppy) | floppy `ND-10335B`; requires ND-500-MONITOR Version B+ |

## Documentation
- Program Description (PD-sheet): [../../../Reference-Manuals/ND-10335B ND-500 SYMBOLIC DEBUGGER.md](../../../Reference-Manuals/ND-10335B%20ND-500%20SYMBOLIC%20DEBUGGER.md)
- Product Information (PI-sheet): [../../Product-Info/ND-10335-C1-EN.md](../../Product-Info/ND-10335-C1-EN.md) (covers ND-10335 and ND-10336 together)
- Manual(s): `ND-60.158.01` Symbolic Debugger Reference Manual — [../../../Reference-Manuals/ND-60158-5-EN Symbolic Debugger - User Guide.md](../../../Reference-Manuals/ND-60158-5-EN%20Symbolic%20Debugger%20-%20User%20Guide.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading the image (MD5 `138f37dfa978bee02fd9077576133f33`) and
reading it with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
