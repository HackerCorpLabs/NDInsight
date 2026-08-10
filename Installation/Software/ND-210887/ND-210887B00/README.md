# ND-210887B00 — AUTOMAKE for ND-500, version B (rev 00)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `210887B00` |
| Base product | [`ND-210887`](../README.md) |
| Version | B, revision 00 — first official version |
| Release date | 86.08.18 |
| CPU target | ND-500 |
| OS requirement | SINTRAN III VSX >= H |

## Prerequisites
- **Mass storage (permanent):** 83 pages, 4 files, on user `SYSTEM`. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210887B00-XX-01D` | `DESCRIPTION-FILE:DESC` (12 pages) · `AUTOMAKE-500-B00:LINK`(3p)/`:DSEG`(47p)/`:PSEG`(26p) — the domain · `AUTO-RULES-5-B00:MAKE` (7 pages, default rule file) · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders), user `FLOPPY-USER` |

## Installation procedure

Source: PD sheet §1, verbatim. [PD]

```
@ENTER-DIRECTORY
DIRECTORY NAME: 210887B
DEVICE NAME: FLOPPY-DISC-<drive-no.>,<floppy-unit>

@LINKAGE-LOADER
ABORT-BATCH-ON-ERROR OFF
DELETE-DOMAIN AUTOMAKE-500
COPY-DOMAIN
SOURCE-DOMAIN: (210887B:FLOPPY-USER)AUTOMAKE-500
DESTINATION-DOMAIN: "AUTOMAKE-500-B<rev>"
EXIT

@DELETE-FILE AUTO-RULES-5:MAKE
@COPY-FILE
DESTINATION FILE: "AUTO-RULES-5-B<rev>:MAKE"
SOURCE FILE: (210887B:F-U)AUTO-RULES-5:MAKE
@RELEASE-DIRECTORY 210887B
```

## Documentation
- PD-sheet: [../../../Installation-Description/ND-210887-2-EN.md](../../../Installation-Description/ND-210887-2-EN.md)
- Manual(s): `ND-60.232.03` AUTOMAKE User Guide (not located in this repo)

## Provenance & open items
- Source: single, complete OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210887` product overview)
