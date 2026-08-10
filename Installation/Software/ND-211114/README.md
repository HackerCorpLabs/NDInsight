# ND-211114 — ADA for ND-500/5000

> Status: IN-PROGRESS — real install command, huge multi-disk distribution not fully catalogued

| Field | Value |
|-------|-------|
| ND article number | `ND-211114` |
| Product name | ADA for ND-500/5000 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 / ND-5000 |
| OS requirement | SINTRAN III >= K |
| Related products | `ND-10745` ND-Ada — an older/different Ada product per the background sweep's own findings; relationship to this article not confirmed. Depends on `ND-210319` ND-500 Linkage-Loader (version >= H) and a minimum ND-500 microcode version (see below). |

## Description
The first release of the ND-Ada compiler, "successfully validated against version 1.9 of the Ada
Compiler Validation Suite." [PD] Ships as four domains (`ADA-INTERFAC`, `ADA-COMPILER`,
`ADA-LIBRARIA`, `ADA-PREPARSE`), an RT timer-support program, and hundreds of `:NRF` compiler
source/object modules spread across **9 double-density floppies** — by far the largest
distribution documented in this catalog.

**Hard microcode-version requirement:** the ND-500 microcode must be at least `15x12` — `15112`
for ND-580, `15212` for ND-550/560/570, `15312` for ND-530. Earlier microcode "will cause the Ada
exception `NUMERIC_ERROR` to be raised for some comparisons involving real values." [PD]

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A (rev 00) | [ND-211114A00](ND-211114A00/README.md) | IN-PROGRESS — install command real, full per-disk file catalog not transcribed (69-79 files per disk × 9 disks) | dated 88.06.30; floppies `211114A00-XX-01D` through `-09D` |

## Documentation
- Program Description (PD-sheet): [../../Installation-Description/ND-211114-1-EN.md](../../Installation-Description/ND-211114-1-EN.md)
  (large — 6 pages of metadata/changelog plus ~9 pages of per-disk file listings, mostly compiler
  internals not relevant to installation)
- Manual(s): `ND-60.198.2` Ada User Guide (not located in this repo)

## Provenance
PD sheet read in full for metadata and install procedure; the per-disk file catalog (hundreds of
individual `:NRF` compiler-internals files) was not fully transcribed here — see the PD sheet
directly if that level of detail is needed.

---
**Parent:** [../README.md](../README.md) (Software catalog)
