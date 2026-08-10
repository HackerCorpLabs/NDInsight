# ND-10176H00 — COBOL for ND-100, version H00 (latest complete set)

> Status: IN-PROGRESS — no PD sheet, but two real `:MODE` scripts decoded and verified   ·   Install source: [OBS] + [MODE] + [INF]

| Field | Value |
|-------|-------|
| Part number | `10176H00` |
| Base product | [`ND-10176`](../README.md) |
| Version | H00 |
| Release date | files dated 1984-09 through 1985-05-10 |
| CPU target | ND-100 / NORD-10 |
| OS requirement | unknown |

## Description
The most complete COBOL-100 release found: compiler (pre-linked, plus both 1-bank and 2-bank
runtime), ISAM (bundled as `ND-10189`'s own component — see below), and a VTM screen-handling
"bridge" patch — all on one floppy, `10176H00-D`. Two smaller, earlier-dated companion floppies
(`10176H00-3S` = ISAM only, `10176H00-4S` = VTM bridge only) carry byte-identical copies of the
same files, confirming `10176H00-D` is a later consolidation of both onto one disk — the same
pattern already seen for `ND-210191F02` FORTRAN and `ND-210177J02` COBOL-500.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `10176H00-D` | `COBOL-100-H00:PROG` (105 pages, pre-linked compiler) · `COBOL-1BANK-H00:BRF` (72 pages) · `COBOL-2BANK-H00:BRF` (74 pages) · `COBOL-ERRORS-H00:DATA` (5 pages, error-message text) · `ISAMRT-I00:MODE` + `:BRF` (14 pages) · `ISAM-SERVICE-I00:PROG` (65 pages) · `ISAM-INTER-I00:PROG` (50 pages) · `VTM-BRIDGE-1-H00:BRF`/`:MODE` (35 pages) · `VTM-BRIDGE-2-H00:BRF`/`:MODE` (36 pages), user `FLOPPY-USER` |
| `10176H00-3S` (ISAM only, superseded by `-D` above) | identical `ISAMRT-I00`/`ISAM-SERVICE-I00`/`ISAM-INTER-I00` files, same dates |
| `10176H00-4S` (VTM only, superseded by `-D` above) | identical `VTM-BRIDGE-1/2-H00` files, same dates |

Confirmed by downloading all three images and reading/extracting with `ndtool`.

## Installation procedure

### Compiler — INFERRED (no `:MODE` script for this part)
```
@ENTER-DIRECTORY 10176H00-D,FLOPPY-DISC-1,0,
@COPY-FILE "COBOL-100-H00:PROG",(SYSTEM)COBOL-100-H00:PROG
@COPY-FILE "COBOL-1BANK-H00:BRF",(SYSTEM)COBOL-1BANK-H00:BRF
@COPY-FILE "COBOL-2BANK-H00:BRF",(SYSTEM)COBOL-2BANK-H00:BRF
@COPY-FILE "COBOL-ERRORS-H00:DATA",(SYSTEM)COBOL-ERRORS-H00:DATA
@DUMP-PROGRAM-REENTRANT COBOL-100,(SYSTEM)COBOL-100-H00:PROG
```
Link user programs against `COBOL-1BANK-H00:BRF` (default, 128Kbyte) or `COBOL-2BANK-H00:BRF`
(after compiling with `1-BANK` given — see the product overview's two-bank note; this compiler
defaults to 2-bank, so `COBOL-2BANK-H00:BRF` is the default link target, not `COBOL-1BANK-H00`).
**Not run live — inferred from the generic pattern.**

### ISAM runtime — REAL, decoded `ISAMRT-I00:MODE` [MODE]

This is a genuine RT-LOADER script, not a simple copy — it loads the ISAM runtime process
(`ISAMRT1`) onto a free RT segment as a background real-time process:

```
@CREATE-FILE ISAM-WORK:BPUN,0
@SCHEDULE 503
@RT-LOADER
YES
DELETE-PROGRAM ISAMRT1
CLEAR-SEGMENT <SEGNO>
YES
NEW-SEGMENT <SEGNO>,,,,,,,
LOAD ISAMRT-I:BRF,,,,,,
YES
WRITE-LOAD-ADDRESS,,,,
WRITE-REFERENCES,,,,
END-LOAD
BINARY-DUMP ISAM-WORK,<SEGNO>,,,,,
Y
Y      Load ISAMRT1 onto segment <SEGNO>
Y
CLEAR-SEGMENT <SEGNO>
YES
NEW-SEGMENT <SEGNO>,,,,,,,,
READ-BINARY ISAM-WORK,,,,,
Y
Y      The following patch will insert internal device number (octal)
Y
CHANGE-LOCATION ,,,,,,
2/<DEVNO>
.
END-LOAD
DECLARE-PROGRAM ISAMRT1,,,,,
END-LOAD
CHANGE-RT-DESCRIPTION ISAMRT1,26,<SEGNO>,,0,,,,,,
END-LOAD
EXIT
@DELETE-FILE ISAM-WORK:BPUN
```

Read the way the script's own header comments explain it:
1. Before running, find a free RT segment number with `*LIST-FREE-SEGMENTS` inside `@RT-LOADER`,
   and substitute that number for every `<SEGNO>` above.
2. Pick an internal device number, octal `200` by default, and substitute it for `<DEVNO>` — but
   **`ISAMRT` will then use `DEVNO` and `DEVNO+1`** together, so if `200`/`201` aren't both free,
   pick a different pair and also change the symbol `ISLUN` in the ISAM library to match (patched
   at application-link time with NRL's `DEPOSIT` command — see the ISAM manual for the permanent
   version of this change).
3. Prerequisite: `ISAMRT-I:BRF` must already be copied to user `SYSTEM`.
4. After running, start the ISAM runtime process with `@RT ISAMRT1`; stop it with
   `@ABORT ISAMRT1`.
5. Running more than one ISAM runtime simultaneously means re-running this whole script with
   `ISAMRT1` renamed to `ISAMRT2`, a different `<SEGNO>`, and a different `<DEVNO>` pair.
6. Performance tip from the script's own trailing comment: after starting the process, run
   `@FIX <SEGNO>` to reduce swapping.

Also copy the two ISAM interactive tools as plain files (no script needed):
```
@COPY-FILE "ISAM-SERVICE-I00:PROG",(SYSTEM)ISAM-SERVICE-I00:PROG
@COPY-FILE "ISAM-INTER-I00:PROG",(SYSTEM)ISAM-INTER-I00:PROG
```

### VTM screen-handling bridge — REAL, decoded `VTM-BRIDGE-1-H00:MODE` / `-2-H00:MODE` [MODE]

These are **BRF-editor patch scripts**, not simple copies — they strip specific units out of the
COBOL runtime BRF files and append a "bridge" module in their place (wiring COBOL's screen
handling to VTM, the Virtual Terminal Manager, per the PI sheet's screen-handling feature):

```
@CREATE-FILE     WORK-1BANK-H00:BRF,88
@COPY-FILE       WORK-1BANK-H00:BRF,COBOL-1BANK-H00:BRF
@SET-FILE-ACCESS COBOL-1BANK-H00:BRF,D,D,D
@DELETE-FILE     COBOL-1BANK-H00:BRF
@RENAME-FILE     WORK-1BANK-H00:BRF,COBOL-1BANK-H00:BRF
@BRF-EDITOR
DELETE-UNITS COBOL-1BANK-H00 MVGTTY  MVSETD
DELETE-UNITS COBOL-1BANK-H00 MV_I_NU MV_BRKT
DELETE-UNITS COBOL-1BANK-H00 MV_EC7T MV_BREA
DELETE-UNITS COBOL-1BANK-H00 5BLACK  FREE_P
APPEND-FILE VTM-BRIDGE-1-H00 COBOL-1BANK-H00
EXIT
@SET-FILE-ACCESS COBOL-1BANK-H00:BRF R R R
```
(`-2-H00:MODE` is the identical script for the 2-bank BRF instead.) Run these **after** the
COBOL runtime BRF files are already copied to `SYSTEM` — they modify `COBOL-1BANK-H00:BRF` and
`COBOL-2BANK-H00:BRF` in place (via a work-copy-rename dance to avoid editing the live file
directly), removing several video/terminal-control units (`MVGTTY`, `MV_I_NU`, `MV_EC7T`,
`5BLACK`, and their paired units) and appending the VTM bridge module instead.

## Configuration / post-install
- ISAM: re-run `ISAMRT-I00:MODE` (with new segment/device numbers) for every additional concurrent
  ISAM runtime process needed; persist the `@RT ISAMRT1` start command and `@FIX <SEGNO>` across a
  cold start by adding them to the site's boot mode files (not independently confirmed for this
  product — follow the generic pattern in
  [../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md)).
- VTM bridge: one-time BRF patch, nothing to persist.

## Documentation
- PD-sheet: not located
- PI-sheet: [../../../Product-Info/ND-10176-B1-EN.md](../../../Product-Info/ND-10176-B1-EN.md)
- Manual(s): `ND-60.144` COBOL Reference Manual; ISAM Reference Manual `ND-60.108.5` (not located
  in this repo — needed for the ISAM library's actual call syntax, see
  [../../ND-210073/README.md](../../ND-210073/README.md))

## Provenance & open items
- Source: `ndtool -t`/`-x` on all three downloaded images; `ISAMRT-I00:MODE` and both
  `VTM-BRIDGE-*:MODE` files decoded with the `byte & 0x7F` technique, clean output.
- **TODO:** this install has not been run live in the emulator.
- **TODO:** confirm the compiler copy/reentrant-dump steps (no `:MODE` script covers them on this
  floppy) — inferred only.

---
**Parent:** [../README.md](../README.md) (`ND-10176` product overview)
