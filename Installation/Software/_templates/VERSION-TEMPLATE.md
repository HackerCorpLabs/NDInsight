<!--
VERSION INSTALL TEMPLATE — copy to Installation/Software/<ND-XXXXX>/<ND-XXXXXv>/README.md
One page per product VERSION. This is the install-bearing document.

IMPORTANT: PD-sheets / PI-sheets do NOT exist for many products. Do not block on them.
Fill the Installation section from whichever source IS available, and tag it:
  [PD]   = Program Description sheet
  [PI]   = Product Information sheet
  [MODE] = extracted from the floppy's own MODE/BATCH files (via ndtool -x)
  [WIKI] = NDWIKI loading note
  [OBS]  = observed by running it in the emulator
  [INF]  = inferred / not yet verified
If no install source exists at all, say so explicitly — do not invent steps.
-->
# <ND-XXXXXv> — <Product Name>, version <v>

> Status: <STUB | IN-PROGRESS | VERIFIED>   ·   Install source: <[PD]/[PI]/[MODE]/[WIKI]/[OBS]/none>

| Field | Value |
|-------|-------|
| Part number | `<ND-XXXXXv>` |
| Base product | [`<ND-XXXXX>`](../README.md) |
| Version | <v> |
| Release date | <date or "unknown"> |
| CPU target | <ND-100 / ND-500 / ND-5000> |
| OS requirement | <SINTRAN III version> |

## Description
<What this version provides; deltas vs other versions if known.>

## Prerequisites
- **Hardware:** <CPU, memory, devices>
- **Software / OS:** <SINTRAN version, multiport, etc.>
- **Dependency products:** <other ND-xxxxx that must be installed first, e.g. XMSG before COSMOS>

## Release package (ND Software Library — 4 parts)
A Norsk Data software release is typically delivered as four documents. Record which exist:

| Part | What it is | This release |
|------|-----------|--------------|
| **Program Description** (PD-sheet) | 1-page metadata form: product, ND-number, computers/instr-set/OS checkboxes, file list | <link / "not located"> |
| **Installation** | the step-by-step procedure (often branched per SINTRAN version) | <link / source tag> |
| **Diskette** | floppy contents manifest (directory name = ND product number; file table) | <link / "not located"> |
| **Revision Log** | change history | <link / "not located"> |

## Distribution media
<Floppy volume name(s) and file listing. Auto-discoverable: a floppy whose volume name's
article number matches `<XXXXX>` belongs here (the volume-name→product rule). Capture file
list with `ndtool -t <image>`; cross-check against the Diskette manifest above.>

| Floppy volume | Boot format | Key files |
|---------------|-------------|-----------|
| <vol-name> | <FLOMON/Binary/None> | <files> |

## Installation procedure
<The actual steps. Tag each step's source per the legend above. Follow the generic pattern in
[INSTALL-METHODOLOGY.md](../INSTALL-METHODOLOGY.md) and record this product's specifics.
Reminder: a re-entrant-subsystem install **may be branched per SINTRAN version** (seen in the
Pascal J sheet and in disk MODE files) — H uses `§DUMP-REENTRANT` (+`§DITAP`), I+ uses
`§DUMP-PROGRAM-REENTRANT`. Follow the product's own sheet. Example skeleton:>

1. <Insert/attach floppy, load it> — `<command>`  <!-- [WIKI]/[MODE] -->
2. <Copy distribution files to system files with correct type> (`:BRF`, `:SYMB`, …); see the
   BPUN/PROG home convention in [HDD-IMAGE-FINDINGS.md](../../OS/research/HDD-IMAGE-FINDINGS.md) §6
3. <Load + dump via NRL> — `§NRL` → `*IMAGE-FILE` / `*SIZE` / `*LOAD …` / `*VALUE` / `*DUMP …` / `*EXIT`
4. <Dump re-entrant (version-branched)> — H: `§DUMP-REENTRANT …` · I+: `§DUMP-PROGRAM-REENTRANT …`
5. <Hook into HENT-MODE / LOAD-MODE if resident> — `@MODE (<user>)<file>:MODE,,,`  <!-- [MODE] -->

> **If no PD/PI sheet and no MODE file exist:** state "No install documentation located;
> needs reconstruction from the floppy contents or emulator testing." Leave a TODO. Do NOT
> fabricate a procedure.

## Configuration / post-install
<MODE-file hooks, terminal/printer config, start commands, SET-AVAILABLE, etc.>

## Documentation
- PD-sheet: <link or "not located">
- PI-sheet: <link or "not located">
- Manual(s): <…>
- NDWIKI: <https://www.ndwiki.org/wiki/ND-XXXXXv>

## Provenance & open items
- Source(s): <…>
- TODO: <unverified items>

---
**Parent:** [../README.md](../README.md) (<ND-XXXXX> product overview)
