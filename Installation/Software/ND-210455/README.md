# ND-210455 — VTM terminal tables (Standard)

> Status: IN-PROGRESS — floppy contents confirmed, no PD/PI sheet located

| Field | Value |
|-------|-------|
| ND article number | `ND-210455` |
| Product name | VTM terminal tables (Standard) |
| Functional category | System Utilities / VTM terminal-type data |
| ND doc-category tag | 80 Utility [curated] |
| CPU target | ND-100 / ND-500 |
| OS requirement | unknown — no PD sheet |
| Related products | `ND-211464` VTM terminal tables (DEC VT200) — the sibling product with a full real PD sheet, see [../ND-211464/README.md](../ND-211464/README.md); this product is the base/standard set every other VTM terminal-type product extends |

## Description
The base set of standard-terminal `DDBnnn` descriptor tables and their compounded
`DDBTABLES-<rev>:VTM` files — see
[../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md)
for the full VTM chapter this plugs into. Unlike `ND-211464` (which adds one new terminal
model), this floppy carries the standard set itself — dozens of individual `DDBnnn-<suffix>:VTM`
files (terminal type `nnn`, format-variant suffix — `-O`, `-7S`/`-7B` 7-bit single/double,
`-8S`/`-8B` 8-bit, `-N` national — matching the C/D/E/G-format distinction already documented in
the VTM chapter).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| G (rev 04) | [ND-210455G04](ND-210455G04/README.md) | IN-PROGRESS — floppy contents confirmed, install not yet transcribed | floppy `210455G04-XX-01D` — also found: revisions G02, G03, G06 |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located
- Manual(s): `ND-60.151.xx` SINTRAN III Utilities Manual (per the `ND-211464` sibling product's
  citation)

## Provenance
Floppy contents confirmed by downloading the G04 image (MD5 `51b54d36e3f11bb834fa213ef3a54a9c`)
and reading with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
