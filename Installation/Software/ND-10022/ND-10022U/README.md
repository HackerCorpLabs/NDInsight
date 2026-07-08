# ND-10022U — SINTRAN Utility Programs, version U

> Status: STUB   ·   Install source: [WIKI] (NDWIKI loading note; no PD/PI sheet)

| Field | Value |
|-------|-------|
| Part number | `ND-10022U` |
| Base product | [`ND-10022`](../README.md) |
| Version | U |
| Release date | unknown |
| CPU target | NORD-10 / NORD-12 / ND-100 |
| OS requirement | SINTRAN III VS |

## Description
Version U of the SINTRAN Utility Programs package. NDWIKI marks the page a stub. [WIKI]

## Prerequisites
- **Hardware:** NORD-10, NORD-12, or ND-100 computer [WIKI]
- **Software / OS:** SINTRAN III VS [WIKI]
- **Dependency products:** none stated

## Distribution media
One floppy image referenced on NDWIKI (`DISK50.IMD`, hosted at Datormuseum.se). [WIKI]
File listing not yet captured — run `ndtool -t <image>` to populate.

| Floppy volume | Boot format | Key files |
|---------------|-------------|-----------|
| (TODO via ndtool) | (TODO) | MCOPY-HP, … (program names per loading note) |

## Installation procedure
Verbatim NDWIKI loading note: [WIKI]

> "Insert floppy, do `1560&` and then load the program with `LOAD <program name>`.
> Example: `LOAD MCOPY-HP`"

Interpretation (`1560&` = load from floppy unit; `LOAD <prog>` loads the named utility) is
consistent with the MACM-style `<addr>&` floppy bootstrap, but the exact mapping is **[INF]**
pending confirmation. No PD/PI sheet exists to verify a fuller procedure.

## Configuration / post-install
None documented. These are standalone utilities loaded on demand, not resident subsystems.

## Documentation
- PD-sheet: not located
- PI-sheet: not located
- Manual(s): see [product overview](../README.md)
- NDWIKI: https://www.ndwiki.org/wiki/ND-10022U

## Provenance & open items
- Source: NDWIKI ND-10022U (stub page).
- TODO: capture floppy file list with ndtool; confirm `1560&` semantics; locate any PD/PI sheet.

---
**Parent:** [../README.md](../README.md) (ND-10022 product overview)
