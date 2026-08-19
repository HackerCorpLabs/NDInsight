# ND-10080 — PED Program Editor

> Status: STUB — PI sheet only; a related later floppy is reported to exist but is not in this repo's accessible archive

| Field | Value |
|-------|-------|
| ND article number | `ND-10080` |
| Product name | PED Program Editor |
| Functional category | Editors & Word Processing |
| CPU target | ND-100 (asynchronous VDU terminals) |
| Related products | `ND-210080` PED, later article number — reported in the backlog as `ND-210080J`, no source found for it either (see below) |

## What is known
A VDU page-mode program editor. Primarily for maintaining source files (FORTRAN, BASIC, COBOL,
Pascal) but usable for general text editing. Files can be compiled directly by handing control to
`PERFORM`, which activates the appropriate compiler. Reads/writes any mass-storage file or I/O
device. [PI]

Features: dynamic (instant) screen editing with no timelag, cursor movement to a specified
character position or via tab stops, movement to a specified line/character string, line delete/
move/copy, merging text from other files, adaptable to several VDU terminal types, files up to
32,000 lines (~500 A4 pages), lines up to 256 characters, string search, and search/replace within
a specified text range. [PI]

**No PD sheet has been located** for this article number — only the PI sheet below. **No install
procedure can be given.** The backlog also reports a later article, `ND-210080J`, with no source
of any kind found for it. See
[../research/FLOPPY-BACKLOG-2026-08.md](../research/FLOPPY-BACKLOG-2026-08.md), Batch 4.

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10080-A1-EN.md](../../Product-Info/ND-10080-A1-EN.md)

## Provenance
PI-sheet-only. No install procedure, no accessible floppy. If the friend's floppy images can be
supplied to this session, this entry can be completed.

---
**Parent:** [../README.md](../README.md) (Software catalog)
