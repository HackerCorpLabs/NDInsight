# ND-211114A00 — ADA for ND-500/5000, version A (rev 00)

> Status: IN-PROGRESS — install command and cold-start persistence real, not yet run live   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `211114A00` |
| Base product | [`ND-211114`](../README.md) |
| Version | A, revision 00 — first release |
| Release date | 88.06.30 |
| CPU target | ND-500 / ND-5000 |
| OS requirement | SINTRAN III >= K; ND-500 microcode >= `15x12` (see product overview) |

## Prerequisites
- **Mass storage for install:** `<scratch-user>` 3970 pages/448 files; `ADA-PACKAGES` 252
  pages/37 files; `STD-PACKAGES` 380 pages/74 files; `VII-ADA-RUNTIMES` 45 pages/10 files;
  `<domain-user>` 2650 pages/21 files; `SYSTEM` 340 pages/20 files. [PD]
- **Mass storage (permanent):** slightly less on most users (e.g. `SYSTEM` drops to 280 pages/12
  files) once install-only scratch data is cleared. [PD]
- User *slots* for the above don't need to be pre-created with those exact names — the installer
  just needs enough free space/file-count headroom to create/expand them. [PD]
- **Segments:** max/min 1 RT-description, max/min 1 segment (18 pages on the segment file), max/
  min **4 Standard Domains**. [PD]

## Distribution media
Nine double-density 1.2MB floppies, `211114A00-XX-01D` through `-09D` — not available on
single-density media. Disk 1 carries the installer (`MAIN-ADA-INS-A00:PROG`), the four compiler
domains, RT timer support (`ADA-TIMER-A00:BPUN`/`:MODE`), and three runtime libraries
(`CAT-LIB-B03`, `MON-CALL-LIB-A00`, `NC-LIB-A04`, `PLANC-LIB-G00:NRF`). Disks 2-9 carry the
compiler's own source/object modules (hundreds of `:NRF` files — compiler internals, not
individually catalogued here).

## Installation procedure

Source: PD sheet §4, verbatim. [PD]

```
@ENTER-DIRECTORY,211114A00-XX-01D
DEVICE NAME: FLOPPY-DISC-1
DEVICE UNIT: <FLOPPY-UNIT>
@(211114A00-XX-01D:FLOPPY-USER)INSTALL-ADA
```
"The installation procedure has now started. It is self-explanatory, but you will be asked to
insert the appropriate diskettes as the installation proceeds until the installation is
completed." (all 9 disks are needed in sequence)

## Configuration / post-install — real, from the PD sheet

**Add to `HENT-MODE`** (cold start):
```
@mode ada-timer-a:mode,,
@nd-500-monitor
delete-standard-dom ada-compiler
delete-standard-dom ada-preparse
delete-standard-dom ada-librarian
delete-standard-dom ada
define-standard-dom ada-compiler   (<DOM_USER>)ada-compile-a
define-standard-dom ada-preparse   (<DOM_USER>)ada-prepars-a
define-standard-dom ada-librarian  (<DOM_USER>)ada-librar-a
define-standard-dom ada            (<DOM_USER>)ada-interfac-a
```
(`<DOM_USER>` = the username chosen for domain storage during install.)

**Add to `LOAD-MODE`** (warm start — starts the RT timer support process):
```
@rt adatm
```

## Documentation modifications relevant to installation
The PD sheet lists changes to the Ada User Guide's chapter 4 command set (`Page-Size`,
`Pragma-Inline`, `Closure-Check`, `Delete-Library`, `Remove-Unit`) and notes the two new packages
`SEGMENT_IO`/`INTERACTIVE_IO` both require `MONITOR_CALLS` installed first — not transcribed in
full here, see the PD sheet directly.

## Documentation
- PD-sheet: [../../../Installation-Description/ND-211114-1-EN.md](../../../Installation-Description/ND-211114-1-EN.md)
- Manual(s): `ND-60.198.2` Ada User Guide (not located in this repo)

## Provenance & open items
- Source: PD sheet metadata, install procedure, and cold/warm-start configuration all
  transcribed verbatim; the per-disk file catalog (hundreds of files across disks 2-9) was not
  fully transcribed.
- **TODO:** this install has not been run live in the emulator (and would need all 9 floppy
  images, which have not been downloaded/verified for this entry).

---
**Parent:** [../README.md](../README.md) (`ND-211114` product overview)
