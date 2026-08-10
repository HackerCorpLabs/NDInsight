# ND-10309A — PLANC for ND-100, version A

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `10309A` (source article: `10370A`) |
| Base product | [`ND-10309`](../README.md) |
| Version | A |
| Release date | 82.02.19 (19 Feb 1982) |
| CPU target | ND-100 (also NORD-10) |
| OS requirement | SINTRAN III VS |

## Description
PLANC-100 Compiler. This PD sheet also documents 17 known syntactical restrictions/limitations of
the PLANC-100-A compiler (FOR-statement/SET ARRAY interaction, MACRO/INLINE/`$INCLUDE` line
termination, `IND`/`ADDR`/`MININDEX`/`MAXINDEX`/`IN` standard-routine restrictions, and others) —
see the source document for the full list. [PD]

## Prerequisites
- **Hardware/OS:** ND-100 or NORD-10, SINTRAN III VS. [PD]
- **Terminal background segment:** the compiler needs 126K words for its terminal background
  segment (changeable per-terminal, see procedure below). [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `ND-10309A` | `PLANC-100-A:IFUN` (compiler, listed as `:IFUN`/`:BPUN` — see note), `PLANC-1BANK-A:BRF` (1-bank runtime), `PLANC-2BANK-A:BRF` (2-bank runtime), user `FLOPPY-USER` |

> The Diskette Directory page shows the compiler's type as `:IFUN` — this is an OCR misread of
> `:BPUN`, confirmed. Every other reference on the sheet (the "Loading/Operating Procedure" text,
> the "Programs (Files)" table, and the SA/RA start/restart-address annotation) reads `:BPUN`,
> which is the correct file type.

## Installation procedure

Source: PD sheet "Loading/Operating Procedure, Use", verbatim. [PD]

1. Enter the directory on the floppy and copy all three files to user `SYSTEM`.
2. Dump the compiler as a reentrant subsystem:
   ```
   @DUMP-REENTRANT PLANC-100,0,1,<input-file>
   ```
   (start address `0`, restart address `1`, matching the "SA=0, RA=1" annotation on
   `PLANC-100-A:BPUN` above). `<input-file>` is the compiler file copied in step 1.
3. Set the terminal background segment size to 128K words (the PD sheet states the requirement
   as 126K but gives this exact command with 128 as the parameter):
   ```
   @CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>, 128
   ```

**Note:** PLANC-100 cannot be restarted with `@CONTINUE`. [PD]

## Configuration / post-install
None beyond the reentrant dump and the background-segment-size change above.

## Documentation
- PD-sheet: [../../../../Reference-Manuals/ND-10309A PLANC FOR ND-100.md](../../../../Reference-Manuals/ND-10309A%20PLANC%20FOR%20ND-100.md)
- PI-sheet: [../../../Product-Info/ND-10309-A1-EN.md](../../../Product-Info/ND-10309-A1-EN.md)
- Manual(s): `ND-60.117.03` PLANC Reference Manual

## Provenance & open items
- Source: single OCR'd PD-sheet scan.
- **TODO:** this install has not yet been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-10309` product overview)
