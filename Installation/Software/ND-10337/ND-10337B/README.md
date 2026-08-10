# ND-10337B — Backup-System, version B

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `10337B` (ND-number for source: `10347B`) |
| Base product | [`ND-10337`](../README.md) |
| Version | B |
| Release date | 81.11.16 (16 Nov 1981) |
| CPU target | NORD-10 / ND-100 (PD sheet checks both "10" and "100") |
| OS requirement | SINTRAN III VS |

## Description
"Sub-system for file copying and backup under SINTRAN-III." [PD] This is the earliest documented
release of the product — a single `:BPUN` program with no installer, copied and dumped reentrant
by hand.

## Prerequisites
Not stated beyond the target computer/OS checkboxes on the PD sheet (NORD-10 or ND-100, SINTRAN
III VS). [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10337B` | single file: `BACKUP-SYSTEM-B:BFUN` (also referred to as `:BPUN` in the loading procedure text — see discrepancy note below), user `FLOPPY-USER` |

> **Discrepancy, as printed on the PD sheet (not resolved):** the "Loading/Operating Procedure"
> section calls the file `BACKUP-SYSTEM-B:BPUN`, but the Diskette page (page 3) lists it as
> `BACKUP-SYSTEM-B:BFUN`. `BPUN` (binary-punch relocatable format) is the type used everywhere
> else in this catalog's PD sheets for reentrant-dumpable programs, and the Program Description
> also calls the underlying file type `BPUN` in its own file table (`203160B BACKUP-SYSTEM BPUN`)
> — so `BFUN` on the diskette page is most likely a typo/OCR artifact for `BPUN`. Confirm against
> the mounted floppy before trusting either spelling.

## Installation procedure

Source: PD sheet "Loading/Operating Procedure, Use", verbatim. [PD]

> "Copy the file BACKUP-SYSTEM:BPUN from the floppy called ND-10337B (BACKUP-SYSTEM for
> SINTRAN-III) to the user where you keep the :BPUN files (SYSTEM, UTILITY, BPUN-FILES...)."

```
@ENTER-DIRECTORY ND-10337B,FLOPPY-DISK-1<,0>
@COPY-FILE "BACKUP-SYSTEM-B:BPUN" (ND-10337B:F-U)BACKUP-SYSTEM:BPUN
```

Then, logged in as user `SYSTEM`:
```
@DUMP-REENTRANT BACKUP-SYSTEM-B 0 0 (???)BACKUP-SYSTEM
```
The `(???)` is printed literally on the PD sheet — the destination user placeholder was not
filled in on the scanned original. Substitute the user you copied the file to in the first step
(e.g. `BPUN-FILES`). [PD]

## Configuration / post-install
None beyond the reentrant dump. Online help is built into the program itself (`HELP` command,
and `?` following any ambiguous command or parameter). [PD]

## Documentation
- PD-sheet: [../../../Installation-Description/ND-10337-2-EN.md](../../../Installation-Description/ND-10337-2-EN.md)
- PI-sheet: not located for this version (a later, general PI sheet exists under `ND-210337`)
- Manual(s): `ND-60.151.01` SINTRAN III Utilities Manual (marked "not released yet" on the PD
  sheet, 1981)
- NDWIKI: not checked yet

## Provenance & open items
- Source: single OCR'd PD-sheet scan.
- **TODO:** resolve the `:BPUN` vs `:BFUN` file-type discrepancy against the mounted floppy.
- **TODO:** this install has not yet been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10337` product overview)
