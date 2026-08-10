# ND-211464 — VTM terminal tables (Type 128/129) DEC VT200

> Status: VERIFIED (transcribed from a complete real PD sheet)

| Field | Value |
|-------|-------|
| ND article number | `ND-211464` (source article: `ND-250339`) |
| Product name | VTM terminal tables (Type 128/129), DEC VT200 |
| Functional category | System Utilities / VTM terminal-type data |
| ND doc-category tag | 80 Utility [curated] |
| CPU target | ND-100 and ND-500, both "All" types/floating formats |
| OS requirement | SINTRAN III >= H |
| Related products | `ND-210455` VTM terminal tables (Standard) — no PD sheet located for that one yet |

## Description
Terminal-table data (not code) that teaches VTM how to talk to the DEC VT200 in two modes
(Multinational = type 128, National = type 129) — "Make it possible for non-standard terminals to
communicate with ND application software." [PD] See
[../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md)
for the full VTM programming/configuration chapter this product plugs into.

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A (rev 00) | [ND-211464A](ND-211464A/README.md) | verified (PD sheet transcribed) | dated 88.09.08; floppy `211464A00-XX-01D` (double-density) or a 2-disk single-density set |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211464-1-EN.md](../../Installation-Description/ND-211464-1-EN.md)
- Manual(s): `ND-60.151.02` SINTRAN III Utilities Manual

## Provenance
Complete 6-page PD sheet found in the archive, covering metadata, install procedure, the
terminal-table-editing procedure, and the diskette listing all in one document.

---
**Parent:** [../README.md](../README.md) (Software catalog)
