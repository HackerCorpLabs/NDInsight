# Carved SINTRAN versions

**This folder holds the carved data, one subfolder per SINTRAN III version.** Each version folder is
the home for everything cut out of that version's SMD disk image - the paged system segments, the
memory-resident images, the reverse-engineering tree, and the inputs used to produce them.

| Version folder | SINTRAN version | Catalog / home |
|----------------|-----------------|----------------|
| `L-VSX-500/` | VSX/500 L-version (L07) | `L-VSX-500/README.md` - the fullest set; the worked example. |
| `K-VSX-500/` | VSX/500 K-version (K05) | `K-VSX-500/segments/README.md` |
| `M-VSX-500/` | VSX/500 M-version (M06) | `M-VSX-500/segments/README.md` |

Each version folder contains: `segments/` (carved big-endian `.bin` + `meta.json` + `manifest.json` +
catalog), `resident/` (resident common code / RT-loader images), `re/` (the RE tree), and `inputs/`
(the live listings + Segment-Table dump the carve was reconciled from) plus `segment-facts.json`.

## Adding a NEW version

Create `<VER>/inputs/` with `list-segment.txt`, `list-rt-programs.txt`, and `segment-table-bank3.bin`
(how to obtain each is in `../EXTRACTING-SEGMENTS.md` sections 3-5), then run `reconcile.py` ->
`carve.py` per `../EXTRACTING-SEGMENTS.md` section 6. Match the binary to its OWN symbol version
(K binary <-> K03 symbols, L07 <-> L07, ...); never mix revisions.

**Parent:** `../README.md`
