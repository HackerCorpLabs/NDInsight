# ND-211049A02 — SQL for ND-500, version A (rev 02)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `211049A02` |
| Base product | [`ND-211049`](../README.md) |
| Version | A, revision 02 |
| Release date | 87.02.10 |
| CPU target | ND-500 |
| OS requirement | SINTRAN III >= K |

## Prerequisites
- **Dependency products:** `ND-210340` SIBAS-II for ND-500 and `ND-210319` Linkage-Loader for
  ND-500 (version >= H) must already be installed. [PD]
- **Mass storage for install:** `SYSTEM` 300 pages/6 files; `DIALOG-SYS` 1400 pages/41 files. [PD]
- **Mass storage (permanent):** `SYSTEM` 220 pages/3 files; `DIALOG-SYS` 1400 pages/41 files (same
  as install — `DIALOG-SYS` usage doesn't shrink). [PD]
- **H-version of the Linkage-Loader** must be on the system before running the installer. If
  installing the example database from the SQL User Manual, a version of `SIB2-DRL` must also
  already be on `SYSTEM`. [PD] `SIB2-DRL-D:PROG` — the SIBAS Data Retrieval Language interactive
  program this refers to — is confirmed present on part 4 of the [ND-10340](../../ND-10340/README.md)
  SIBAS-II-for-ND-500 floppy set.

## Distribution media

| Floppy set | Density |
|---|---|
| `211049A02-XX-01D` + `211049A02-XX-02D` | Double (2 disks) |
| `211049A<rev>-XX-01S` through `-07S` | Single (7 disks) |

Disk 1 carries the installer (`SQL-INSTALL-A02:PROG`/`:XCOM`), error messages (English/
Norwegian), the SIBAS-E-compatibility patch, example database schema/catalog/tutorial, and
terminal tables. Disk 2 carries 22 `SQL-nn-A<rev>:NRF` modules (the compiled SQL system itself).
You must use the same media type ("A" or "B" density) throughout the whole install.

## Installation procedure — fully interactive installer

Source: PD sheet §1, verbatim. [PD]

```
@ENTER-DIRECTORY 211049A-XX-01
DEVICE NAME: FLOPPY-DISC-<drive-no.>
DEVICE UNIT: <floppy-unit>
@(211049A:FLOPPY-USER)SQL-INSTALL-A
```
"The installation procedure has now started. It is self-explanatory." It continuously checks its
own operation and tells the operator what to do if an error occurs. When it finishes:
```
@DELETE-FILE SQL-INSTALL-A:XCOM
```

**If SIBAS is version E10 or older**, a patch must be run from user `SIB2-500` *before* using SQL:
```
@MODE SQL-PATCH-A<rev>:SYMB,,
```

## Starting SQL

The installer generates a standard domain `SQLI-A<rev>`:
```
@SQLI-A<rev>
```
or, to log in on the command line (displays the password if given this way):
```
@SQLI-A<rev> <db-name>,<password>
```

## Regenerating the domain (if destroyed, or after a new SIBAS install)

```
@ND
DELETE-STANDARD-DOMAIN SQLI-A<rev>
```
then, logged in as the SQL-owning user:
```
@MODE SQLI-A<rev>:LOAD,,,
```
then, as `SYSTEM` again:
```
@ND
DEF-STANDARD-DOM SQLI-A<rev> (<sql user>)SQLI-A<rev>
```

## Saving disk space
The `:NRF` files on the SQL-owning user may be deleted once installed — reinstalling later just
means re-running the installer. [PD]

## Read/read-write toggle — a direct patch, not a documented command

```
Read Only                    Read Write
@ND-                         @ND-
LOOK-AT-DATA 1'4B SQLI-A-     LOOK-AT-DATA 1'4B SQLI-A-
PERMIT-                       PERMIT-
0-                            1-
EXIT-                         EXIT-
EXIT-                         EXIT-
```
This is a direct in-place patch of a flag byte, not a normal SINTRAN command — the same
"undocumented `LOOK-AT-DATA` patch" pattern already seen in this catalog on
[ND-211250's install scripts](../../ND-211250/ND-211250C13/README.md). [PD]

## New databases
Make a new database available to SQL by defining it in `(SYSTEM)SQL:BASE` — see Appendix E of the
SQL User Manual (not located in this repo) for the exact format. [PD]

## Configuration / post-install
See "Starting SQL" and "Regenerating the domain" above.

## Documentation
- PD-sheet: [../../../Installation-Description/ND-211049-1-EN.md](../../../Installation-Description/ND-211049-1-EN.md)
- Manual(s): `ND-60.258.1` SQL User Manual (not located in this repo)

## Provenance & open items
- Source: single, complete 8-page OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-211049` product overview)
