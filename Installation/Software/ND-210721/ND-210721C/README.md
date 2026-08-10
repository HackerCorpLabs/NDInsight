# ND-210721C — BRF-Linker for ND-100, version C (revision 01)

> Status: VERIFIED (transcribed from PD sheet, cross-checked against the mounted floppy; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210721C01` (revision `<rev>` in the PD sheet's own notation) |
| Base product | [`ND-210721`](../README.md) |
| Version | C |
| Release date | 87.09.03 (3 Sep 1987) |
| CPU target | ND-10 / ND-100 |
| OS requirement | SINTRAN III VS, version >= H (>= I for the simpler reentrant-dump path, see below) |

## Description
This revision corrects six errors (a `LOAD file-name,,,,` trailing-comma bug, scratch-file use in
`APPEND-BRF`, `REPLACE-BRF` on files with `COMMON` units, a `PREPARE-BRF-LIBRARY-FILE` load bug,
proper termination on "Program/Data space exceeded", and short/long S-group mixing) and adds:
`DEBUG-MODE EXTENDED` (needs Symbolic Debugger F01+, fixes a "Debug table full" error),
`SEGMENT-ENTRY` fixed symbols `*DATA*`/`*PROG*`/`*REF*`, `IGNORE-ENTRY *SELECT*` (inverts
ignore-list semantics to a select-list), and the new `DATA-FILE` command for multisegment systems
(puts a program file's data into the main program file). [PD]

## Prerequisites
- **Hardware/OS:** ND-10 or ND-100, SINTRAN III VS, version >= H. [PD]
- **Mass storage:** 44 pages, 1 file, on user `SYSTEM` (both install and permanent — nothing
  discarded after install). [PD]
- **Terminal background segment:** 128K words (see procedure below). [PD]
- **Optional:** ND-100 Symbolic Debugger version D or later, if using the speed-up trick below —
  requires a commercial-instruction-set ND-100 (not ND-10). [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210721C01-XX-01D` | `BRF-LINKER-C01:PROG` (45 pages), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `68dae73b565995151d2966b89a4c685f`) and reading with
`ndtool -t` — matches the PD sheet's diskette listing exactly.

## Installation procedure

Source: PD sheet §3 "Installation Procedure", verbatim. [PD]

1. Enter the floppy directory and copy the file to user `SYSTEM`:
   ```
   @COPY-FILE
   DESTINATION FILE: "BRF-LINKER-C01:PROG"
   SOURCE FILE: (210721C01:FLOPPY-USER)BRF-LINKER-C01:PROG
   ```
2. **Optional speed-up** (commercial-instruction-set ND-100 only, not ND-10) — do this **before**
   dumping the linker reentrant:
   ```
   @DEBUGGER
   PLACE BRF-LINKER-C01,W
   STACK-INSTRUCTIONS
   EXIT
   ```
   This requires ND-100 Symbolic Debugger version D or later (see
   [ND-10335](../../ND-10335/README.md) for that product's own install doc — note ND-10335B is an
   ND-500 debugger; the ND-100 debugger is the sibling product `ND-10336`, not yet documented in
   this catalog).
3. Dump reentrant. **Two paths depending on SINTRAN version:**
   - **SINTRAN I or later** (simpler):
     ```
     @DUMP-PROGRAM-REENTRANT BRF-LINKER-C01,BRF-LINKER-C01:PROG
     ```
   - **Earlier SINTRAN III versions** — make a `:BPUN` first via `DITAP`, then dump with explicit
     addresses:
     ```
     @DITAP "BRF-LINKER-C01:BPUN",BRF-LINKER-C01:PROG
     @DUMP-REENTRANT BRF-LINKER-C01,27226,27226,BRF-LINKER-C01:BPUN
     ```
     **Note:** if you create the `:BPUN` file, do **not** delete `BRF-LINKER-C01:PROG` from user
     `SYSTEM` after the reentrant subsystem is generated. [PD]
4. Set the terminal background segment size to 128K words:
   ```
   @CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>,128
   ```

## Configuration / post-install
None beyond the reentrant dump and background-segment-size change above.

## Documentation
- PD-sheet: [../../../Installation-Description/ND-210721-3-EN.md](../../../Installation-Description/ND-210721-3-EN.md)
- PI-sheet: not located
- Manual(s): `ND-60.196.2` BRF-LINKER User Manual (not located in this repo)

## Provenance & open items
- Source: single OCR'd PD-sheet scan, cross-checked against the actual downloaded floppy image.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210721` product overview)
