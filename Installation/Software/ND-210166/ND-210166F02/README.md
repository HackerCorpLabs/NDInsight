# ND-210166F02 — SIBAS II for ND-100, version F (rev 02)

> Status: VERIFIED (transcribed from a complete, 17-page real PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210166F02` (source article: `210308F`) |
| Base product | [`ND-210166`](../README.md) |
| Version | F, revision 02 |
| Release date | 86.09.25 |
| CPU target | ND-100 |
| OS requirement | SINTRAN III version I or newer; version K if more than 12 SIBAS processes are wanted |

## Description
The F revision — mostly bug fixes and capacity increases over E (max realms per database raised
to ~100 on SIBAS-100/254 on SIBAS-500, max 2GB per realm, more concurrent users, R-log/BIM-log
size raised to 65000 pages, SIBAS libraries rewritten in PLANC and consolidated to 3 variants).
**Databases defined with SIBAS-E or earlier must be converted before use with SIBAS-F** — see
the conversion procedure below. [PD]

## Prerequisites
- **Hardware/OS:** ND-100, SINTRAN III version >= I (>= K for > 12 processes). [PD]
- **Mass storage for install:** `SYSTEM` needs 60 pages / 2 files; `DIALOG-SYS` needs 1100 pages
  / 16 files (both install and permanent — nothing shrinks after install). [PD]
- **RT-descriptions/segments:** max 13, min 2. Segment-file space:
  `64 + ((no. of SIBAS processes) × 56)` pages. [PD]
- **User `DIALOG-SYS` must exist** with exactly that name (not `DIALOGUE-SYS` or
  `DIALOG-SYSTEM` — the installer will not recognize an abbreviation). [PD]
- All SIBAS processes must be passive; the `RT-LOADER` must be free.

## Distribution media

| Floppy volume(s) | Density | Key contents |
|---|---|---|
| `210166F02-XX-01D` + `-02D` | Double (2 disks) | Installer (`INST-SIB-100-F02:PROG`), program/data segment MODE+BPUN pairs, DBM, DRL, LOOKLOG, service program, SIBLIB 1-bank/2-bank, SIBINTER, error messages, DBCONV |
| `210166F<rev>-XX-01S` through `-10S` | Single (10 disks) | Same content split across 10 disks — disk 4/5 need the `MAKEDRL` two-step copy (see below) |

Confirmed by downloading the double-density images and reading with `ndtool -t` — matches the PD
sheet's own diskette listing exactly (17 files total across the two double-density disks).

## Installation procedure — installer-driven (recommended path)

Source: PD sheet §6 "Installation Using the Installation Program", verbatim. [PD]

1. Log in as `SYSTEM`, insert the first floppy, and set it as the default directory:
   ```
   %ENTER-DIRECTORY,210166F
   :DISK-DIRECTORY
   %SET-DEF-DIR 210166F
   ```
2. Run the installer:
   ```
   @(210166F:FL-U)INST-SIB-100-F
   ```
3. Answer its questions (real text from the PD sheet):
   - **"What do you want to do, Install or Change (type I or C)?"** — Install reads all SIBAS
     files from the diskettes and defines the SIBAS-F processes; Change reloads/reconfigures an
     existing install (an Install must already have been done).
   - **"How many SIBAS processes do you want (1 to 12)?"**
   - **"Which process do you want to be the first (0 to nn)?"**
   - **"On which seg-file is SIBAS to be loaded (default: 0)?"** — CR usually works; if a later
     mode-file run reports the seg-file full, re-run the installer with "Change" and pick another
     (list defined seg-files with `@LIST-FILES SEGFILE` from `SYSTEM`).
   - **"On which directory do you want to give user DIALOG-SYS space?"** — CR if `DIALOG-SYS`
     already exists with enough free space; otherwise name a directory with enough space.
   - **"Which floppy station and unit are you going to use?"**
   - **"Do you want to copy SIBAS library-files to user SYSTEM?"** — YES copies
     `SIBLIB-1BANK-Fxx`/`SIBLIB-2BANK-Fxx` to `SYSTEM`.
   - **"Do you want to dump `<file>` reentrant (default YES/NO)?"** — CR lets the installer
     decide if unsure.
   - **"Do you want to copy `<file>` to user SYSTEM (default YES/NO)?"**
   - **"Do you want to run the mode files SIB2-PROG-F<rev>:MODE and SIB2-DATA-F<rev>:MODE?"** —
     YES installs with default system locations; NO if you need to edit system locations first
     (see "Configuration" below), then run `SIB2-PROG-Fxx:MODE` yourself from `RT` or `SYSTEM`
     afterward.
4. **Critical reminder, applies regardless of path chosen:** a copy of `SIB2-DRL-100-F:PROG`
   must always reside on user `DIALOG-SYS`.

## Installation procedure — manual path (no installer)

Source: PD sheet §7, verbatim. [PD]

1. **Checking:** `DIALOG-SYS` exists with >= 1100 free pages (create it if not); SIBAS-F may
   coexist with SIBAS-E processes; all target SIBAS processes are passive (use `SET-PASSIVE` in
   `SIBAS-SERVICE` if not); `RT-LOADER` is free; enough free segments (one data segment per SIBAS
   process, plus one shared program segment); RT-common has >= 1/2 Kword per process; seg-file
   has enough space.
2. **Copy all floppy files to `DIALOG-SYS`.** On single-density media, `SIB2-DRL-100-F<rev>` must
   instead be reconstructed via the `MAKEDRL` two-step process:
   ```
   (mount disk 4, directory 210166F<rev>-XX-04S)
   copy MAKEDRL-F<rev>:PROG to (DIALOG-SYS)
   ØMAKEDRL-F 1
   (release disk 4; mount disk 5, directory 210166F<rev>-XX-05S)
   ØMAKEDRL-F 2
   (delete MAKEDRL-F<rev>:PROG; release disk 5)
   ```
3. Edit `SIB2-DATA-F<rev>:MODE` if any system location needs changing (I/O device numbers,
   RT-common index, restart-area size, max calls/security-breaks/remembered-records limits, LAMU
   page/size, direct-transfer flag — the PD sheet gives the full symbol table with default octal
   values, e.g. `Y.IUTDV` = input device for the error device, `Y.MYSIFLG` = SIBAS process
   number). Then run `SIB2-PROG-F:MODE` from `SYSTEM` or `RT` to load the process.

## Configuration / post-install
- Add `MODE SIB2-PROG:MODE` to `(SYSTEM)HENT-MODE:MODE` so SIBAS reloads on every cold start —
  see [../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md)
  for the general cold-start pattern this plugs into.
- Update the `DUMP-PROGRAM-REENTRANT` entries for the SIBAS utility programs in the same
  cold-start chain.

## Converting an older (SIBAS-E or earlier) database to F

Source: PD sheet, verbatim. [PD]

1. Verify the database is consistent first (`SIB2-DBM-Exx`, VERIFY mode).
2. **Take a full backup.**
3. From the user owning the database (or a user with `RWC` access), run
   `(DIALOG-SYS)SIB2-DBCONV-F<rev>:PROG`, driven from a mode file so output can be checked:
   ```
   (DIALOG-SYS)SIB2-DBCONV-F
   FUNCTEST        <- name of DB owner (required)
   FUNCBASE        <- name of database (required)
   SCRATCH:SYMB    <- scratch file name; CR gives a default; must be an INDEXED file, not contiguous
   ```
   run as `MODE CONVDB:MODE,"CONVDB:OUT"`.
4. Conversion runs at roughly 20-100 MB/hour (slower with more defined sets) and **removes any
   defined R-LOG and/or BIM-LOG** — reinitialize both after conversion.
5. Re-verify with `SIB2-DBM-F<rev>` and take a fresh full backup.

## Documentation
- PD-sheet: [../../../../Reference-Manuals/210166F SIBAS II for ND-100.md](../../../../Reference-Manuals/210166F%20SIBAS%20II%20for%20ND-100.md)
- Manual(s): `ND-60.127.5` SIBAS-II ND User Manual, `ND-30.009.3` SIBAS-II Operator's Manual

## Provenance & open items
- Source: single, complete 17-page OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210166` product overview)
