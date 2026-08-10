# ND-210886B00 — AUTOMAKE for ND-100, version B (rev 00)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210886B00` |
| Base product | [`ND-210886`](../README.md) |
| Version | B, revision 00 — first official version |
| Release date | 86.08.18 |
| CPU target | ND-100 |
| OS requirement | SINTRAN III VSX >= H |

## Prerequisites
- **Mass storage (permanent):** 90 pages, 2 files, on user `SYSTEM`. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210886B00-XX-01D` | `AUTOMAKE-100-B00:PROG` (84 pages, pre-linked) · `AUTO-RULES-1-B00:MAKE` (6 pages, default rule file), user `FLOPPY-USER` |

## Installation procedure

Source: PD sheet §1, verbatim — plain file copy, no linker/reentrant step needed. [PD]

```
@ENTER-DIRECTORY
   DIRECTORY NAME: 210886B
   DEVICE NAME: FLOPPY-DISC-<drive-no.> <floppy-unit>

@DELETE-FILE AUTOMAKE-100:PROG
@COPY-FILE
   DESTINATION FILE: "AUTOMAKE-100-B<rev>:PROG"
   SOURCE FILE: (210886B:FLOPPY-USER)AUTOMAKE-100:PROG

@DELETE-FILE AUTO-RULES-1:MAKE
@COPY-FILE
   DESTINATION FILE: "AUTO-RULES-1-B<rev>:MAKE"
   SOURCE FILE: (210886B:FLOPPY-USER)AUTO-RULES-1:MAKE

@RELEASE-DIRECTORY 210886B
```

## Documentation
- PD-sheet: [../../../Installation-Description/ND-210886-2-EN.md](../../../Installation-Description/ND-210886-2-EN.md)
- Manual(s): `ND-60.232.03` AUTOMAKE User Guide (not located in this repo)

## Provenance & open items
- Source: single, complete OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210886` product overview)
