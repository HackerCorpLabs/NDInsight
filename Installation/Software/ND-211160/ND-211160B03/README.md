# ND-211160B03 — LED for ND-500/5000, version B (rev 03)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `211160B03` |
| Base product | [`ND-211160`](../README.md) |
| Version | B, revision 03 |
| Release date | 88.08.03 |
| CPU target | ND-500/5000, CX type |
| OS requirement | SINTRAN III VS version K, work mode 312B, patch file 6034B |

## Description
This revision adds: auxiliary process control, compiler-message mapping, LEDCALL
user-programmable keys, structure browsing, several new commands (`Read-All-Files`,
`Rename-Region`, `Delete-Region`, `Compare-Area`, `Write-Modified-Files`), position bookmarking
(`SHIFT+F3`/`F3`), mark-size adjustment, quoted-argument search-language bypass, cancelable
read/write/search/pretty-print, and indentation/pretty-print/browsing support for C and FORTRAN.
Several commands were renamed (`Read`→`Read-File`, `Write`→`Write-File`, `Update`→`Update-File`,
`Move`→`Move-To`, `Value`→`Value-Of`, `Sort`→`Sort-Lines`, `Global-Mode`→`Global-Search`). [PD]

## Prerequisites
- **Hardware/OS:** ND-500/5000, CX type, SINTRAN III VS version K, work mode 312B, patch file
  6034B. [PD]
- **Mass storage (permanent):** 271 pages, 4 files, on any user. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `211160B03-XX-01D` | `DESCRIPTION-FILE:DESC` (9 pages) · `LED-B03:LINK`/`:DSEG`(147 pages)/`:PSEG`(111 pages) — the LED domain · `UPK-IF:DEFS` (4 pages, LEDCALL service definitions) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders), user `FLOPPY-USER` |

## Installation procedure

Source: PD sheet §2 "Installation Procedure", verbatim. [PD]

1. Log in as the target user, insert the floppy, and enter its directory:
   ```
   @ENTER-DIRECTORY,211160B-XX-01D
   DEVICE NAME: FLOPPY-DISC-<drive-no.>
   DEVICE UNIT: <floppy-unit-no.>
   ```
2. Copy the domain to the destination user via the Linkage-Loader:
   ```
   @LINKAGE-LOADER
   N1: COPY-DOMAIN
   Source-domain: <211160B:FL-US>LED-B<rev>
   Destination-domain: "<LED-B<rev>>"
   N1: EXIT
   ```
3. Register it as a standard domain:
   ```
   @ND-500-MON
   N500(0): DEFINE-STANDARD-DOMAIN LED-B<rev> <user>\<Dest.Domain>
   N500(0): EXIT
   ```

## Configuration / post-install
None beyond the standard-domain registration above (not explicitly tied to a cold-start `HENT`
step on this PD sheet, unlike several other products in this catalog — follow the generic
pattern in [../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](../../../INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
if persistence across a cold start is needed).

## Documentation
- PD-sheet: [../../../Installation-Description/ND-211160-2-EN.md](../../../Installation-Description/ND-211160-2-EN.md)
- Manual(s): `ND-60.266.2` LED User Guide

## Provenance & open items
- Source: single, complete OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-211160` product overview)
