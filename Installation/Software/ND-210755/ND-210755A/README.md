# ND-210755A — BASIC for ND-500, version A (revision 02)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210755A02` (source article: `250095A`) |
| Base product | [`ND-210755`](../README.md) |
| Version | A, revision 02 |
| Release date | 86.12.10 |
| CPU target | ND-500 |
| OS requirement | SINTRAN III VSX |

## Description
Compile and execute BASIC programs on the ND-500. Note from the PD sheet: the `BASIC-500-A02`
domain includes the segment `BASIC-SLIB-A02`; in the earlier `A00` version of the compiler this
segment was instead called `BASIC-SEG-LIB` — a rename to be aware of if comparing against older
notes or floppies. [PD]

## Prerequisites
- **Hardware/OS:** ND-500, SINTRAN III VSX. [PD]
- **Mass storage for install:** 381 pages, 4 files, on user `SYSTEM`. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210755A02-XX-01D` | `DESCRIPTION-FILE:DESC` (9 pages) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders) · `BASIC-SLIB-A02:LINK`/`:DSEG`/`:PSEG` (5/28/22 pages) · `BASIC-500-A02:LINK`/`:DSEG`/`:PSEG` (16/121/136 pages, the compiler domain) · `BASIC-LIB-A02:NRF` (39 pages, runtime library), user `FLOPPY-USER` — 11 files, 381 pages |

## Installation procedure

Source: PD sheet §2 "Installation Procedure", verbatim. [PD]

1. Enter the floppy directory `210755A02-XX-01D` and copy the runtime library:
   ```
   @COPY-FILE
   DESTINATION FILE: "BASIC-LIB-A02:NRF"
   SOURCE FILE: (210755A02-XX-01D:FLOPPY-USER)BASIC-LIB-A02:NRF
   ```
2. Generate the `BASIC-500-A02` domain via the ND-500 Linkage-Loader:
   ```
   @ND-LINKAGE-LOADER
   NLL: ABORT-BATCH-ON-ERROR OFF
   NLL: DELETE-DOMAIN BASIC-500
   NLL: COPY-DOMAIN (210755A02-XX-01D:FLOPPY-USER)BASIC-500-A02,"BASIC-500-A02"
   NLL: EXIT
   ```
   (`DELETE-DOMAIN` before `COPY-DOMAIN` is the expected-error pattern already seen elsewhere in
   this catalog — it fails harmlessly on a first-time install, which is why
   `ABORT-BATCH-ON-ERROR` is turned off first.)
3. As user `SYSTEM`, make it a standard domain:
   ```
   @ND-500-MONITOR
   N500: DEFINE-STANDARD-DOMAIN BASIC-500-A02 BASIC-500-A02
   N500: EXIT
   ```
4. Start the compiler:
   ```
   @BASIC-500-A
   ```
   or
   ```
   @ND BASIC-500-A
   ```

## Configuration / post-install
`DEFINE-STANDARD-DOMAIN` (step 3) does not survive a cold start by itself — append the
`@ND-500-MONITOR` / `DEFINE-STANDARD-DOMAIN` lines to `ND500-HENT` on user `SYSTEM`, the same
cold-start persistence pattern already documented for the ND-500 Linkage-Loader in
[../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
(not independently re-verified for this product).

## Documentation
- PD-sheet: [../../../Installation-Description/ND-210755-1-EN.md](../../../Installation-Description/ND-210755-1-EN.md)
- PI-sheet: not located
- Manual(s): `ND-60.207.01` ND-500 BASIC User Manual

## Provenance & open items
- Source: single OCR'd PD-sheet scan, complete (metadata + install procedure + diskette listing
  all on one 3-page document).
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210755` product overview)
