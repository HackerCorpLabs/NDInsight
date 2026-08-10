# ND-10309F — PLANC for ND-100, version F

> Status: STUB — no PD sheet, procedure inferred by pattern, NOT verified   ·   Install source: [INF]

| Field | Value |
|-------|-------|
| Part number | `10309F00` (as printed on the floppy volume label) |
| Base product | [`ND-10309`](../README.md) |
| Version | F |
| Release date | unknown |
| CPU target | ND-100 |
| OS requirement | unknown (assumed SINTRAN III VS, consistent with A/B) |

## Description
A later revision than A/B, but **no PD sheet has been located for F.** The floppy's file shape
has changed from A/B in one important way: the compiler ships as an already-linked **`:PROG`**
(`PLANC-100-F00:PROG`), not a raw `:BPUN` needing a manual start/restart address. This is the
same shape change seen in this catalog's CC-100 (ND-10760) and Backup-System F (ND-10337F)
entries — later revisions of several ND-100 products moved from shipping raw `:BPUN` to shipping
pre-linked `:PROG`.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `10309F00-1S` | `PLANC-100-F00:PROG` (120 pages — the compiler, already linked) · `PLANC-1BANK-F00:BRF` (15 pages, 1-bank runtime) · `PLANC-2BANK-F00:BRF` (16 pages, 2-bank runtime), user `FLOPPY-USER` |

Single disk, no companion floppy found under the same `10309F00` prefix (unlike CC-500's 3-disk
single-density set) — confirmed via directory listing only (temporary working reference, not
committed to this repo). No `:MODE` install script is present on this floppy to extract.

## Installation procedure — INFERRED, NOT CONFIRMED

No install text exists on this floppy (no `:MODE` script, no PD sheet). The following adapts the
verified A/B procedure to this floppy's `:PROG` shape:

1. Enter the directory on the floppy and copy all three files to user `SYSTEM` (same as A/B):
   ```
   @ENTER-DIRECTORY 10309F00-1S,FLOPPY-DISC-1,0,
   @COPY-FILE "PLANC-100-F00:PROG",(SYSTEM)PLANC-100-F00:PROG
   @COPY-FILE "PLANC-1BANK-F00:BRF",(SYSTEM)PLANC-1BANK-F00:BRF
   @COPY-FILE "PLANC-2BANK-F00:BRF",(SYSTEM)PLANC-2BANK-F00:BRF
   ```
2. Dump the compiler as a reentrant subsystem — but using **`DUMP-PROGRAM-REENTRANT`**, the
   `:PROG`-file form, not A/B's raw-`:BPUN` `DUMP-REENTRANT ...,0,1,...` form (mirroring the
   generic pattern in [../../INSTALL-METHODOLOGY.md](../../INSTALL-METHODOLOGY.md) §4 step 3 —
   SINTRAN I+ uses `DUMP-PROGRAM-REENTRANT` for already-linked files):
   ```
   @DUMP-PROGRAM-REENTRANT PLANC-100,(SYSTEM)PLANC-100-F00:PROG
   ```
3. Set the terminal background segment size, unchanged from A/B (no reason to assume this
   requirement went away — PLANC-100's memory model did not change between these releases as far
   as anything read here shows):
   ```
   @CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>, 128
   ```

**Do not treat the above as verified.** Steps 1 and 3 are carried over from the confirmed A/B
procedure; step 2's command choice (`DUMP-PROGRAM-REENTRANT` vs `DUMP-REENTRANT`) is inferred
from the file being `:PROG` rather than `:BPUN`, following this catalog's generic methodology,
not from a document specific to this revision.

## Configuration / post-install
Presumed same as A/B — none beyond the reentrant dump and background-segment-size change.
Unconfirmed for F.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10309-A1-EN.md](../../../Product-Info/ND-10309-A1-EN.md) (general product sheet, not version-specific)
- Manual(s): `ND-60.117.03`/`ND-60.117.5` PLANC Reference Manual

## Provenance & open items
- Source: floppy directory listing only.
- **TODO (blocking):** mount `10309F00-1S` (e.g. download by MD5 `fd9a9296290fb76318e4ca1fe2a0eb63`
  from the ND floppy library and read with `ndtool`) and confirm there is genuinely no install
  script hiding in a file type not shown by the directory-listing summary.
- **TODO:** confirm whether `DUMP-PROGRAM-REENTRANT` is really the right command for this file,
  and whether the background-segment-size requirement still applies to F.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10309` product overview)
