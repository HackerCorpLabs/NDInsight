# ND-211157B03 — LED-DEBUGGER for ND-500/5000, version B (rev 03)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `211157B03` |
| Base product | [`ND-211157`](../README.md) |
| Version | B, revision 03 |
| Release date | 88.08.03 (same date as [ND-211160B03 LED](../../ND-211160/ND-211160B03/README.md) — a matched release pair) |
| CPU target | ND-500/5000, CX type |
| OS requirement | SINTRAN III VS version K, work mode 312B, patch file 6034B |

## Prerequisites
- **Mass storage (permanent):** 377 pages, 3 files, on user `SYSTEM`. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `211157B03-XX-01D` | `DEBUGGER-LED-B03:PSEG` (173 pages), `DEBUGGER-LED-B03:DSEG` (200 pages) · `UPK-IF:DEFS` (4 pages, shared with LED itself), user `FLOPPY-USER` |

## Installation procedure

Source: PD sheet §2 "Installation Procedure", verbatim — notably **simpler** than LED itself (a
plain `COPY-FILE`, no Linkage-Loader domain copy or standard-domain registration): [PD]

```
@COPY-FILE "DEBUGGER-LED-B<rev>:PSEG" (211157:F-U)DEBUGGER-LED-B:PSEG
@COPY-FILE "DEBUGGER-LED-B<rev>:DSEG" (211157:F-U)DEBUGGER-LED-B:DSEG
```
(copies from the floppy directly to user `SYSTEM` — no separate domain/link file is shipped,
since `PSEG`/`DSEG` here integrate directly into the already-installed LED domain rather than
standing alone.)

## Configuration / post-install
None beyond the copy above. Requires [ND-211160 LED](../../ND-211160/ND-211160B03/README.md)
already installed.

## Documentation
- PD-sheet: [../../../Installation-Description/ND-211157-2-EN.md](../../../Installation-Description/ND-211157-2-EN.md)
- Manual(s): `ND-60.266.2` LED User Guide, `ND-60.158.5` Symbolic Debugger User Guide

## Provenance & open items
- Source: single, complete OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-211157` product overview)
