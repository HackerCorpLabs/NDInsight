# ND-10177H00 — ND-500 COBOL, version H00

> Status: IN-PROGRESS — PART2 missing, procedure inferred from a later revision's MODE files, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `10177H00` (per-part suffixes below) |
| Base product | [`ND-10177`](../README.md) |
| Version | H00 |
| Release date | files dated 1984-09 through 1985-05 |
| CPU target | ND-500 |
| OS requirement | unknown |

## Description
A 3-disk set (`-1S`/`-2S`/`-3S` naming, single density), of which only PART1 and PART3 have been
found — see the [product overview](../README.md) for the gap analysis.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `10177H00-1S` | `DESCRIPTION-FILE:DESC` (8 pages) · `COBOL-500-H00:LINK`/`:DSEG`/`:PSEG` (6/43/85 pages) — **the compiler domain** · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders) |
| `10177H00-2S` | **not found** — see [product overview](../README.md) |
| `10177H00-3S` | `ISAMRT-I00:MODE` (3 pages) · `ISAMRT-I00:BRF` (14 pages) · `ISAM-SERVICE-I00:PROG` (65 pages) · `ISAM-INTER-I00:PROG` (50 pages) — **an ISAM (Indexed Sequential Access Method) add-on**, not part of the compiler domain itself; corresponds to the related product `ND-10343` ISAM for ND-500 |

Confirmed by downloading both images (MD5 `687fb0375b19b9badbe05ef29e49be86` for `-1S`,
`61ff11c1133dd0a81e0e75ee437d4e39` for `-3S`) and reading with `ndtool -t`. No `:MODE` install
script on either disk.

## Installation procedure — INFERRED, NOT CONFIRMED

No install script exists on either disk found, and no PD sheet exists for this revision. The
later `ND-210177J02` revision (see [../../ND-210177/ND-210177J02/README.md](../../ND-210177/ND-210177J02/README.md))
does carry real, decoded `:MODE` install files for the equivalent domain+library+ISAM
combination — the procedure below adapts that pattern to H00's file names, with the gap left
where PART2 (the presumed library) would slot in:

1. Copy the compiler domain off PART1 (mirroring the recovery-path pattern already documented for
   the ND-500 Linkage-Loader, see
   [../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md §4a-VERIFIED](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)):
   ```
   @ENTER-DIRECTORY 10177H00-1S,FLOPPY-DISC-1,0,
   @COP-FIL "DESCRIPTION-FILE:DESC",(SYSTEM)DESCRIPTION-FILE:DESC
   @COP-FIL "COBOL-500-H00:LINK",(SYSTEM)COBOL-500-H00:LINK
   @COP-FIL "COBOL-500-H00:DSEG",(SYSTEM)COBOL-500-H00:DSEG
   @COP-FIL "COBOL-500-H00:PSEG",(SYSTEM)COBOL-500-H00:PSEG
   ```
2. **PART2 would presumably load the runtime library into the domain here**, via a
   `Linkage-Loader` mode file analogous to J02's `COBOL-LIB-J02:MODE` — `Open-Segment`,
   `Total-Segment-Load <library-domain>`, `Close-Segment`. Not available for H00.
3. Load the ISAM add-on from PART3, if needed, following the same `Linkage-Loader`
   `Total-Segment-Load` pattern J02 uses for its own bundled ISAM files.

**Do not treat the above as verified** — step 2 is a placeholder for missing content, not a real
procedure.

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10177-A1-EN.md](../../../Product-Info/ND-10177-A1-EN.md)
- Manual(s): `ND-60.144.3` COBOL Reference Manual

## Provenance & open items
- Source: floppy directory listings via `ndtool` on the two downloaded images.
- **TODO (blocking):** locate `10177H00-2S` (or confirm it never existed) to complete this
  version.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10177` product overview)
