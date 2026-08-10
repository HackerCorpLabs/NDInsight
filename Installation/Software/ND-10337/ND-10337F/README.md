# ND-10337F — Backup-System, version F

> Status: STUB — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [INF]

| Field | Value |
|-------|-------|
| Part number | `10337F` (as printed on the floppy volume label) |
| Base product | [`ND-10337`](../README.md) |
| Version | F |
| Release date | unknown |
| CPU target | ND-100 (assumed, consistent with sibling versions) |
| OS requirement | unknown |

## Description
A later revision than B, but **no PD or PI sheet has been located for F specifically.** Unlike
B (ships as a `:BPUN` file needing a separate `@DUMP-REENTRANT` step) and I04/I05 (ship as a
`:PROG` installer with a 3-question dialogue), F ships as a single **already-linked `:PROG`**
file with no installer program at all — the simplest of the three shapes seen for this product.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10337F` | single file: `BACKUP-F00:PROG` (106 pages), user `FLOPPY-USER` |

Confirmed via directory listing only (temporary working reference, not committed to this repo).

## Installation procedure — INFERRED, NOT CONFIRMED

No install text exists on this floppy to read (there is no `:MODE` script and no PD sheet). The
following is inferred **by pattern** from the two sibling versions and should be treated as a
starting point to verify live, not a confirmed procedure:

1. Enter the floppy directory and copy `BACKUP-F00:PROG` to the user where you want the program
   to live (mirroring B's "copy to the user where you keep your files" step):
   ```
   @ENTER-DIRECTORY ND-10337F,FLOPPY-DISK-1,0
   @COPY-FILE "BACKUP-F00:PROG" (ND-10337F:FLOPPY-USER)BACKUP-SYSTEM:PROG
   ```
2. Dump it reentrant so it is shared/callable by name, using **`DUMP-PROGRAM-REENTRANT`** (the
   `:PROG`-file form, since this file is already linked — not `DUMP-REENTRANT`, which is for raw
   `:BPUN` binaries like version B). This mirrors the I04/I05 installer's own final action
   (`@DUMP-PROGRAM-REENTRANT BACKUP-SYSTEM-I, (UTILITY)BACKUP-SYS-I02:PROG`):
   ```
   @DUMP-PROGRAM-REENTRANT BACKUP-SYSTEM-F,(BPUN-FILES)BACKUP-SYSTEM:PROG
   ```
3. Persist across a cold start by appending the `DUMP-PROGRAM-REENTRANT` line to the
   `DUMP-REENTRANT:MODE` file called from `HENT-MODE`, per the generic pattern in
   [../../INSTALL-METHODOLOGY.md](../../INSTALL-METHODOLOGY.md) §4 step 5 and
   [../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md §12](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md).

**Do not treat the above as verified.** It has not been run, and no source document confirms the
exact command names/addresses for this specific revision.

## Configuration / post-install
Unknown — inferred to be none beyond the reentrant dump, consistent with the rest of this
product's version history.

## Documentation
- PD-sheet: not located
- PI-sheet: not located for this version (a later, general PI sheet exists under `ND-210337`)
- Manual(s): presumed `ND-60.151.xx` SINTRAN III Utilities Manual, unconfirmed for this version
- NDWIKI: not checked yet

## Provenance & open items
- Source: floppy directory listing only.
- **TODO (blocking):** mount `ND-10337F` and confirm whether `BACKUP-F00:PROG` needs any
  companion install steps not visible from the directory listing (e.g. an embedded self-install
  routine) before trusting the inferred procedure above.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10337` product overview)
