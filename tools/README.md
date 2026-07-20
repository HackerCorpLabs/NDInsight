# tools/

Tooling for the NDInsight SINTRAN III reverse-engineering effort.

| Subfolder | What it is |
|-----------|------------|
| `sintran-segment-carver/` | **The SINTRAN carver and all carved data.** Cuts the individual SINTRAN pieces (paged system segments + memory-resident code/data) out of a raw SINTRAN III SMD disk image into per-piece big-endian `.bin` files at their correct load addresses, for disassembly (`nd100-dis`) or Ghidra. Also holds the reverse-engineering tree (MON-call analysis, ND-500 system monitor, segment references). See `sintran-segment-carver/README.md`. |
| `boot-floppy/` | **The ND distribution diskettes, disc boot sectors, and how SINTRAN is generated onto a disc.** Works from the original ND *distribution floppies* and from *page 0 of real installed packs* — the shipped systems before installation, and the boot code an installed system writes for itself. Answers how a system is installed (large disc and SCSI), what disc geometries/sizes are supported, how the disc boot sector is authored and structured, what MACM's install dialogue does, and how SINTRAN patches work. The complementary witness to the carver: floppies = as-shipped, carver = installed-and-patched. See `boot-floppy/README.md`. |

## Where the carved data lives

The carved bytes are under `sintran-segment-carver/versions/<VER>/`, one subfolder per SINTRAN
version (`L-VSX-500/` = L07, `K-VSX-500/` = K05, `M-VSX-500/` = M06). **`L-VSX-500/` is the primary,
fullest set** and the folder to read from / extend for L07 work - see
`sintran-segment-carver/versions/L-VSX-500/README.md`.

## How the carving works / how to extend it

- `sintran-segment-carver/EXTRACTING-SEGMENTS.md` - the paged system segments (the
  `reconcile.py` -> `carve.py` pipeline; how to carve a new version or re-carve).
- `sintran-segment-carver/EXTRACTING-RESIDENT-CODE.md` - the memory-resident code/data the segment
  carver cannot produce (raw NDFS block extraction; PIT-overlay resolution; live-DAP capture).

**Everything stays under `E:\Dev\Ronny\NDInsight\`.** The status-of-record docs the carving updates
are `../SINTRAN/CARVING-HANDOFF.md` (ND-100) and `../SINTRAN/ND500/ND500-STATUS-AND-INDEX.md` (ND-500).

## The two are complementary — read them together

The carver and `boot-floppy/` are independent witnesses to the same systems.

- **`sintran-segment-carver/`** = an **installed, running, site-patched** SINTRAN,
  carved off a hard disk. Ground truth for what the kernel *is* at runtime.
- **`boot-floppy/`** = the same systems **as ND shipped them** (distribution
  floppies) plus **page 0 of real packs**. Ground truth for how a system is
  *generated onto a disc* and how it *boots*.

Where they disagree, the disagreement is information — e.g. the disc-geometry
table lives in the carved **DPIT** segment (not `S3FS`); the boot sector is
written by the running kernel at cold-start (not shipped, not by MACM); and the
disc-type numbering splits into three distinct axes (MACM `MSTYP`, kernel
`SWTYP`, page-0 `YSWTY`). Two skills, `nd-disc-boot` and `sintran-generation`,
distil the `boot-floppy/` findings; `sintran-carving` covers the carver.
