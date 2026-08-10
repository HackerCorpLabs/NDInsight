# ND-210400B — Subsystem Package II, version B

> Status: VERIFIED (transcribed from 2 independent PD-sheet scans; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210400B` |
| Base product | [`ND-210400`](../README.md) |
| Version | B |
| Release date | 87.05.20 (20 May 1987) |
| CPU target | ND-100 |
| OS requirement | SINTRAN III, version >= H |

## Description
This release fixes two QED bugs (reading a specified line interval; a 12-tab-stop bug), drops
"empty library mark" listings from MAC/FMAC output, fixes three NORD-PL compiler bugs (`IF A
NBIT`, the `A BZERO`/`A BONE` statements now correctly error, a symbol-name warning when a MAC
system symbol is reused as an entry point, better recovery from source lines > 128 chars, and a
`SYMBOL`-constant codegen bug), and changes one QED edit key (`CTRL-R` "skip word" moved to
`CTRL-G`; `CTRL-R` now retypes old/new line for further editing). [PD]

## Why install this

A freshly generated SINTRAN III system has **no editor and no assembler**. Subsystem Package II
is Norsk Data's standard bundle that provides both, plus the language most of this repo's
SINTRAN development documentation targets:

| Module | File on floppy | What it is |
|--------|-----------------|------------|
| QED | `QED-1644L:BPUN` | the standard line editor |
| MAC (48-bit) | `MAC-1415C:BPUN` | Macro Assembler, 48-bit floating-point build |
| FMAC (48-bit) | `FMAC-1408D:BPUN` | Macro Assembler, 48-bit float, "F" variant |
| MAC (32-bit) | `F32-MAC-1628C:BPUN` | Macro Assembler, 32-bit floating-point build |
| FMAC (32-bit) | `F32-FMAC-1920C:BPUN` | Macro Assembler, 32-bit float, "F" variant |
| NPL | `NPL-1896D:BPUN` | NORD-PL compiler |
| DITAP | `DITAP-1880D:BPUN` | file dump/copy utility — also used to install this package |

Install this before installing any other product whose install procedure assumes an editor or
`MAC`/`NPL` are already on the system. [PD]

## Prerequisites
- **Hardware:** ND-100 (any type per the PD sheet's "Type: Any"); floating-point format "All"
  (both the 48-bit and 32-bit assembler builds are on the floppy — pick the pair matching the
  machine's hardware floating-point format). [PD]
- **Software / OS:** SINTRAN III, version H or later. [PD]
- **Mass storage for install:** 107 pages, 7 files (both "for installation" and "permanent"
  figures on the PD sheet are the same 107 pages / 7 files — i.e. nothing is discarded after
  install). [PD]
- **Dependency products:** the install procedure below calls `@BACKUP-SYSTEM` directly — see
  [ND-210337 Backup-System](../../ND-210337/README.md), which must already be installed. Not
  stated explicitly on this product's own PD sheet, but the same dependency is documented as a
  hard, verified prerequisite for the ND-500 Linkage-Loader install.

## Release package (ND Software Library — 4 parts)

| Part | What it is | This release |
|------|-----------|--------------|
| **Program Description** (PD-sheet) | 1-page metadata + installation procedure, all in one document for this product | [../../../OS/SUBSYSTEM/ND0117.md](../../../OS/SUBSYSTEM/ND0117.md) (also [210400B_Subsystem_Package_II_combined.md](../../../OS/SUBSYSTEM/210400B_Subsystem_Package_II_combined.md) — independent second scan, same content) |
| **Installation** | folded into the PD sheet's section 3 (this product does not ship a separate Installation document) | see below |
| **Diskette** | floppy directory listing, printed as PD-sheet pages 1–2 | see "Distribution media" below |
| **Revision Log** | PD-sheet sections 1–2 ("Errors Corrected" / "Modifications") | summarized in Description above |

## Distribution media

| Floppy volume | Boot format | Key files |
|---------------|-------------|-----------|
| `210400B00-XX-01D` | data floppy (not self-booting — loaded from a running SINTRAN III system) | `DITAP-1880D`, `F32-FMAC-1920C`, `F32-MAC-1628C`, `FMAC-1408D`, `MAC-1415C`, `NPL-1896D`, `QED-1644L` (all `:BPUN`, user `FLOPPY-USER`, 107 pages / 148 reserved) |

Directory name on the floppy: `210400B00-XX-01D`. An older/smaller pressing of the same content
also exists under directory name `210400B00-XX-01S` (148 pages total vs. 610 on the `D` pressing)
— same 7 files, same sizes. [PD]

> **Note (image provenance, not yet resolved):** four physical dumps of a floppy named
> `210400B00-XX-01D` were found across the local floppy-image collections, with **four different
> MD5 hashes** (`cc27c274…`, `9cb614a4…`, `c7dd8feb…`, `5a63e100…`). One of the four
> (`5a63e100…`) sits in Ronny's own curated `UTILS\ND-210400B-Subsystem package II\` folder,
> which is the natural candidate to mount first, but the discrepancy across all four is
> unexplained (different revisions under the same label vs. bit-rot vs. mismatched labeling) and
> **has not been checked** — read each image's own directory listing/label before trusting it.

> **Module-name discrepancy, as printed on the PD sheet (not resolved, not silently fixed):** the
> Diskette Directory listing (pages 1–2, both independent scans) names the 32-bit MAC file
> `F32-MAC-1628C`. The Installation Procedure table (section 3.1, both independent scans) instead
> prints the module name as `MAC-32-1626C` while still giving the *target file name* as
> `(BPUN-FILES)F32-MAC-1628C`. Both scans agree on this exact mismatch, so it is very unlikely to
> be an OCR error introduced here — it is either a typo in the original 1987 document or a real
> distinction not explained on the sheet. **Use the file actually present on the mounted floppy**
> (`F32-MAC-1628C`) as authoritative; the `1626C` string appears to be a label-only typo.

## Installation procedure

Source: PD sheet §3 "Installation Procedure" (both scans agree verbatim). [PD]

1. **Delete old versions first.** If a previous Subsystem Package II (or an older 32-bit/48-bit
   Subsystem Package) is present, delete its old `:PROG` and `:BPUN` copies of these modules
   before installing the new ones. [PD]
2. **Mount the floppy and copy files to the BPUN home user.** Standard practice per this repo's
   generic methodology ([INSTALL-METHODOLOGY.md](../../INSTALL-METHODOLOGY.md) step 1) — enter the
   floppy directory, then use the **BACKUP-SYSTEM** to copy all 7 files to the user where BPUN
   files are kept. **Standard user is `BPUN-FILES`.** [PD]
   ```
   @ENTER-DIRECTORY
   DIRECTORY NAME:            (leave empty — taken from the floppy label)
   DEVICE NAME: FLOPPY-DISC-1
   DEVICE UNIT: 0
   ```
   then copy `DITAP-1880D`, `FMAC-1408D`, `MAC-1415C`, `F32-MAC-1628C`, `F32-FMAC-1920C`,
   `NPL-1896D`, `QED-1644L` (all `:BPUN`) into `(BPUN-FILES)`.
3. **Choose how each module will run** — the PD sheet describes two independent options, and
   both use the *same* start/restart address pairs (see table below):
   - **3.1 — Dump reentrant** (shared, memory-resident — the normal choice so every terminal can
     run the tool without its own private copy): login as `SYSTEM`, then for each module: [PD]
     ```
     @DUMP-REENTRANT
     NAME: <name>
     START ADDRESS: <start>
     RESTART ADDRESS: <restart>
     FILE NAME: (BPUN-FILES)<bpun-file>
     ```
     Worked example from the PD sheet (QED): [PD]
     ```
     @DUMP-REENTRANT
     NAME: QED
     START ADDRESS: 0
     RESTART ADDRESS: 1
     FILE NAME: (BPUN-FILES)QED-1644L
     ```
   - **3.2 — Dump as a `:PROG` file** (private, per-user copy — only if you do NOT want it
     shared reentrant): login as `SYSTEM` or the target user, same address pairs: [PD]
     ```
     @PLACE-BINARY,(BPUN-FILES)QED-1644L
     @DUMP
     FILE NAME: "QED"
     START ADDRESS: 0
     RESTART ADDRESS: 1
     ```

### Reentrant dump addresses for every module (PD sheet §3.1, both scans)

| Module | Start address | Restart address | Source `:BPUN` file |
|--------|---------------|------------------|----------------------|
| DITAP | `70` (octal) | `70` (octal) | `(BPUN-FILES)DITAP-1880D` |
| FMAC (48-bit) | `-1` (`177777` octal) | `-3` (`177775` octal) | `(BPUN-FILES)FMAC-1408D` |
| MAC (48-bit) | `-1` (`177777` octal) | `-3` (`177775` octal) | `(BPUN-FILES)MAC-1415C` |
| MAC (32-bit) | `-1` (`177777` octal) | `-3` (`177775` octal) | `(BPUN-FILES)F32-MAC-1628C` *(see module-name discrepancy note above)* |
| FMAC (32-bit) | `-1` (`177777` octal) | `-3` (`177775` octal) | `(BPUN-FILES)F32-FMAC-1920C` |
| NPL | `0` | `1` | `(BPUN-FILES)NPL-1896D` |
| QED | `0` | `1` | `(BPUN-FILES)QED-1644L` |

This exact table is also cross-referenced (independently, from the same PD sheet) in
[../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §12](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md#12-making-subsystems-reentrant--dump-reentrant-manual-3x--pd-sheets),
which additionally reviewed a real hand-built `DUMP-REENTRANT:MODE` file and found every entry
that has a PD sheet (i.e. every row above) used the correct addresses.

## How to have it loaded again after a cold start

`@DUMP-REENTRANT` only loads a module into the **current** segment file / memory image. SINTRAN
distinguishes **cold start** (a full reload from disk — segments must be rebuilt) from **warm
start** (the existing segment file, with everything already dumped reentrant, is reused as-is).
Full mechanics: [../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §1](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md#1-the-two-kinds-of-start-manual). Summary for this product:

1. **Do the 7 `@DUMP-REENTRANT` commands above once**, by hand, right after installing from the
   floppy, to prove they work.
2. **Save them into a mode file**, conventionally `(UTILITY)DUMP-REENTRANT:MODE` — one
   `@DUMP-REENTRANT` line per module, using the table above. This is the file the boot guide's §12
   already reviews (a real-world copy of exactly this file, for exactly this product).
3. **Call that mode file from `(SYSTEM)HENT-MODE:MODE`** — the cold-start script — so it re-runs
   automatically every time the segment file is rebuilt from scratch:
   ```
   @MODE (UTILITY)DUMP-REENTRANT:MODE,,,
   ```
   (placed alongside the other cold-start steps — segment-file definition, `RTENTER`, etc. — see
   the full `HENT-MODE:MODE` skeleton in
   [../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §8](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md#8-hent-modemode-for-cold-start--skeleton-for-this-machine-manual-h3--your-config)).
4. **Do NOT put it in `LOAD-MODE`.** `DUMP-REENTRANT` is cold-start-only work — the reentrant
   segments it creates persist in the segment file across every subsequent **warm** start, so
   `LOAD-MODE` (which runs on every warm start) does not need to repeat it.
5. **Cold start itself is manual, not automatic**: on a cold start, SINTRAN's stored initial
   commands do **not** run, so an operator must `@ENTER-DIRECTORY`, log in as `SYSTEM`, and run
   `@MODE (SYSTEM)HENT-MODE:MODE,,,` by hand — which then triggers step 3 above and hands off to
   `LOAD-MODE` for the rest of the warm-start chain. See the boot guide §1/§8/§9 for the full
   bring-up order.

## Configuration / post-install
None beyond the reentrant dump itself — this package has no start command, no terminal/printer
configuration, and nothing to `SET-AVAILABLE`. QED/MAC/FMAC/NPL/DITAP are simply callable by name
once dumped reentrant (or run directly if dumped as `:PROG`).

## Documentation
- PD-sheet: [../../../OS/SUBSYSTEM/ND0117.md](../../../OS/SUBSYSTEM/ND0117.md) · second independent scan: [../../../OS/SUBSYSTEM/210400B_Subsystem_Package_II_combined.md](../../../OS/SUBSYSTEM/210400B_Subsystem_Package_II_combined.md)
- PI-sheet: not located
- Manual(s): `ND-60.096.01 EN` MAC User's Guide · `ND-60.151.2A EN` SINTRAN III Utilities Manual
  · `ND-60.031.4C EN` QED User's Manual · `ND-60.047.3A EN` NORD PL User's Guide
- NDWIKI: not checked yet

## Provenance & open items
- Source: two independent OCR'd scans of the same ND Software Library Diskette document
  (`ND0117.md` and `210400B_Subsystem_Package_II_combined.md`), which agree on every field except
  the one module-name digit flagged above.
- **TODO:** resolve which of the four differently-hashed `210400B00-XX-01D` floppy image dumps is
  authoritative (see "Distribution media" note above) — not yet checked.
- **TODO:** confirm the `MAC-32-1626C` vs `F32-MAC-1628C` naming discrepancy against the actual
  mounted floppy directory listing once an image is chosen.
- **TODO:** this install has not yet been run live in the emulator to confirm the commands work
  as printed (unlike the ND-500 Linkage-Loader guide, which is a verified live session).

---
**Parent:** [../README.md](../README.md) (`ND-210400` product overview)
