# L-VSX-500 (L07) - the carved SINTRAN data home

**This folder IS the carved data for SINTRAN III VSX/500 L-version (L07).** Everything cut out of the
L07 SMD disk image - the paged system segments, the memory-resident images, the reverse-engineering
tree, and the exact inputs used to produce them - lives here. It is the folder to READ from when you
need L07 bytes, and the folder to EXTEND when you carve more.

Sibling versions: `../K-VSX-500/` (K05) and `../M-VSX-500/` (M06). See `../README.md` for the
version index.

The procedure that produced everything here is documented once, authoritatively, in:
- `../../EXTRACTING-SEGMENTS.md` - the paged system segments (the `reconcile.py` -> `carve.py` pipeline).
- `../../EXTRACTING-RESIDENT-CODE.md` - the memory-resident code/data the segment carver cannot produce.
- `../../README.md` - the carver overview and the one-line pipeline.

Do NOT re-derive the page/carve model - it is settled and proven against the raw disk in those docs.

---

## What is in this folder

| Path | What it is |
|------|------------|
| `segments/` | The carved paged **system segments**: one big-endian `NNN-<NAME>.bin` + `NNN-<NAME>.meta.json` sidecar (carries the Ghidra base) per segment, plus `manifest.json` (all segments in one file) and `README.md` (the human-readable per-segment catalog: load address, pages, MADR, content type). 79 segment `.bin` files. START HERE for segment bytes. |
| `resident/` | The memory-**resident** images the segment carver skips (`madr==0`): `SINTRAN-DATA_commoncode.bin` (resident common code, Ghidra base `0x0`), `MACM-AREA-DATA_rtloader.bin` (RT-loader area, base `0x3000`), plus a disassembly. Pulled by raw NDFS block range - see `../../EXTRACTING-RESIDENT-CODE.md`. |
| `re/` | The L07 **reverse-engineering tree**: `mon-analysis/` (156 per-MON-call folders), `segments-ref/` (promoted, verified per-segment analyses), `MON-CALL-INDEX.md` (the authoritative MON index), `ND500-SYSTEM-MONITOR/`, `ND500-HANDLERS-OVERLAY.md`, disassemblies (`*.dis`), Ghidra symbol dumps (`*.ghidra-symbols.txt`), and handoffs. See `re/README.md`. |
| `inputs/` | The exact facts fed to the carver: `list-segment.txt` (live `@RT-LOADER LIST-SEGMENT`), `list-rt-programs.txt` (validation), `segment-table-bank3.bin` (the in-memory Segment Table dump giving MADR/SEGLE). Re-carving reads these. |
| `segment-facts.json` | The canonical reconciled facts (`reconcile.py` output) that `carve.py` consumes. |
| `section83-from-manual.json` | Release-manual section 8.3 ranges, parsed. |
| `json-discrepancies.txt` | Where the AI-derived segment JSON disagreed with the live/manual facts. |
| `mon-calls/` | Empty placeholder. Live MON-call analysis is under `re/mon-analysis/`. |

---

## How to EXTEND this (carve more L07 data)

1. **A missing paged segment, or a re-carve:** run the pipeline in `../../EXTRACTING-SEGMENTS.md`
   (sections 4-6) using this folder's `inputs/` and `segment-facts.json`; `carve.py` writes into
   `segments/` and refreshes `manifest.json` + the per-segment `meta.json`.
2. **Resident code/data** (not in `segments/`): pull by raw NDFS block range per
   `../../EXTRACTING-RESIDENT-CODE.md` into `resident/`. Resident DATA cells read zero on disk -
   capture them LIVE over DAP, do not disassemble them out of a carve.
3. **A disassembly / analysis:** write it into `re/` (a promoted, verified segment analysis goes to
   `re/segments-ref/NNN-<NAME>/`; MON-call work to `re/mon-analysis/<CODE>B-<NAME>/`). Record the
   `nd100-dis -b` base and byte order used; tag every claim `[V]`/`[I]`/`[OPEN]`; name which symbol
   table an address came from.
4. **Update the status-of-record doc in the SAME change** - `../../../../SINTRAN/CARVING-HANDOFF.md`
   for ND-100 work, `../../../../SINTRAN/ND500/ND500-STATUS-AND-INDEX.md` for ND-500 work - and index
   the new file there. An un-indexed carve is invisible to the next session.

**Byte-order reminder:** every `.bin` here is big-endian (native ND-100), ready for a big-endian
Ghidra processor as-is. Byte-swap ONLY for `nd100-dis` (it reads little-endian). ND-500 segments
(`030-S3SM5`, `062-S3SSM5`) are 32-bit byte-addressed - do NOT load them with the ND-100 processor.

**Parent:** `../README.md`
