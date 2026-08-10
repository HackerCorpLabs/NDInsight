# ND-10166E — SIBAS II for ND-100, version E (rev 10/00)

> Status: IN-PROGRESS — floppy contents real; install dialogue ADAPTED from the later F-revision's real PD sheet, not this version's own   ·   Install source: [OBS] + [INF, from a different but structurally-matching revision's real PD sheet]

| Field | Value |
|-------|-------|
| Part number | `10166E` (per-file revisions `E10`/`E00`, see below) |
| Base product | [`ND-10166`](../README.md) |
| Version | E |
| Release date | files dated 1984-09-27 (most) / 1984-10-18 (`SIB2-RTLST`) |
| CPU target | ND-100 |
| OS requirement | SINTRAN III, version >= I (per the F-revision PD sheet's stated requirement — not independently confirmed for E) |

## Description
A 9-floppy release. The file set matches the later F-revision's module list almost exactly
(installer, program/data segment MODE+BPUN pair, service program, DML libraries, DRL
redefinition tool, DBM maintenance, LOOKLOG, SIBINTER interactive) — strong evidence this is the
same product across revisions, not a different design.

One naming detail not resolved: files come in two parallel sets suffixed `MH` and `MX`
(`SIBAS-SERV-MHE00`/`SIBAS-SERV-MXE00`, `SIBLIB-1N-MH-E10`/`SIBLIB-1N-MX-E10`,
`SIBINTER-MH-E00`/`SIBINTER-MX-E00`) — almost certainly two build variants (e.g. two
floating-point/instruction-set targets, matching the "All" floating-format prerequisite pattern
seen on other ND-100 products in this catalog), but **which is which is not confirmed** from any
source read.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10166E-PART1` | `SIB2-PROG-E10:MODE` (1 page) · `SIB2-DATA-E10:MODE` (7 pages) · `SIB2-RTLST-E10:SYMB` (11 pages) · `SIB2-INSTALL-E10:PROG` (56 pages — **the installer**) |
| `ND-10166E-PART2` | `SIB2-PROG-E10:BPUN` (57 pages) · `SIB2-DATA-E10:BPUN` (57 pages) — program/data segment raw binaries |
| `ND-10166E-PART3` | `SIBAS-SERV-MHE00:PROG` (59 pages) · `SIB2-DML-B-MHE10:BRF` (16 pages) · `SIB2-DML-R-MHE10:BRF` (14 pages) · `SIBLIB-1N-MH-E10:BRF` (14 pages) · `SIBLIB-1R-MH-E10:BRF` (11 pages) · `SIBLIB-2N-MH-E10:BRF` (14 pages) — "MH" variant |
| `ND-10166E-PART4` | `MAKEDRL-E10:PROG` (20 pages) · `SIB2-DRL-1-E10:PROG` (90 pages) |
| `ND-10166E-PART5` | `SIB2-DRL-2-E10:PROG` (91 pages) |
| `ND-10166E-PART6` | `SIB2-DBM-E10:PROG` (103 pages, maintenance/verify) · `SIB2-LOOKLOG-E00:PROG` (42 pages) |
| `ND-10166E-PART7` | `SIBAS-SERV-MXE00:PROG` (59 pages) · `SIB2-DML-B-MXE10:BRF` (15 pages) · `SIB2-DML-R-MXE10:BRF` (13 pages) · `SIBLIB-1N-MX-E10:BRF` (13 pages) · `SIBLIB-1R-MX-E10:BRF` (11 pages) · `SIBLIB-2N-MX-E10:BRF` (14 pages) — "MX" variant |
| `ND-10166E-PART8` | `SIBINTER-MX-E00:PROG` (106 pages) · `UE-SIBINTER-E00:ERR` (6 pages) |
| `ND-10166E-PART9` | `SIBINTER-MH-E00:PROG` (107 pages) · `UE-SIBINTER-E00:ERR` (6 pages) |

Confirmed by downloading all 9 images and reading with `ndtool -t`.

## Installation procedure — ADAPTED from the F-revision's real, complete PD sheet

`SIB2-INSTALL-E10:PROG` is a compiled installer (like `INST-SIB-100-F` on the F revision), so its
exact dialogue cannot be extracted the way `:MODE` text files can. The F-revision PD sheet (see
[ND-210166F02](../../ND-210166/ND-210166F02/README.md) for the full verbatim transcription) gives
the complete, real install flow for the same product one revision later — reproduced here as the
best available guide, **not verified for E specifically**:

1. **Prerequisites to check first:** user `DIALOG-SYS` must exist with >= 1100 pages / 16
   filenumbers; user `SYSTEM` needs >= 50 pages / 1 filenumber; all SIBAS processes must be
   passive; the `RT-LOADER` must be free.
2. **Log in as SYSTEM**, mount the first floppy, enter its directory, and run the installer:
   ```
   %ENTER-DIRECTORY,ND-10166E-PART1
   :DISK-DIRECTORY
   %SET-DEF-DIR ND-10166E-PART1
   @(ND-10166E-PART1:FLOPPY-USER)SIB2-INSTALL-E10
   ```
3. **Answer the installer's questions** (real question text, from the F installer — expect the
   same shape): Install or Change; how many SIBAS processes (1-12); which process is first;
   which seg-file to load onto (default 0 — check free seg-files with
   `@LIST-FILES SEGFILE` from `SYSTEM` if it reports full); which directory `DIALOG-SYS` gets
   space on; which floppy station/unit; whether to copy the SIBAS library files to `SYSTEM`;
   whether to dump each file reentrant (CR lets the installer decide); whether to copy each file
   to `SYSTEM`; whether to run the `SIB2-PROG`/`SIB2-DATA` mode files immediately (say NO if you
   need to edit system locations first — see the mode-file table in
   [ND-210166F02](../../ND-210166/ND-210166F02/README.md)).
4. **Critical reminder from the source PD sheet, applies to every revision:** a copy of the DRL
   redefinition program must always reside on user `DIALOG-SYS`.

## Configuration / post-install
Add the `SIB2-PROG:MODE` load to `HENT-MODE` for cold-start persistence, and update the
`DUMP-PROGRAM-REENTRANT` entries for the SIBAS utility programs — see
[ND-210166F02](../../ND-210166/ND-210166F02/README.md) "After Installation" for the exact,
real wording this is drawn from.

## Documentation
- PD-sheet: not located for E specifically — see [ND-210166F02](../../ND-210166/ND-210166F02/README.md)
- PI-sheet: [../../../Product-Info/ND-10166-A1-EN.md](../../../Product-Info/ND-10166-A1-EN.md)
- Manual(s): `ND-60.127.5` SIBAS-II ND User Manual, `ND-30.009.3` SIBAS-II Operator's Manual (both in this repo)

## Provenance & open items
- Source: floppy directory listings via `ndtool` on all 9 downloaded images; install procedure
  adapted from the real F-revision PD sheet.
- **TODO:** confirm the `MH`/`MX` variant meaning.
- **TODO:** this install has not been run live in the emulator, and its dialogue has not been
  independently confirmed to match the F-revision installer exactly.

---
**Parent:** [../README.md](../README.md) (`ND-10166` product overview)
