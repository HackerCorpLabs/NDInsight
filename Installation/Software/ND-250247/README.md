# ND-250247 — ND-5000 Test Microprograms

> Status: DIRECTORY LISTING ONLY — no floppy image located

| Field | Value |
|-------|-------|
| ND article number | `ND-250247` |
| Directory name | `250247A00-XX-01D` (revision A00, one diskette) |
| Product name | ND-5000 Test Microprograms |
| Functional category | Hardware test / diagnostics for the ND-5000 (SAMSON) CPU |

## What is known

The diskette's own directory listing survives — see
[ND-250247A-Software-Library-Diskette-Listing.md](./ND-250247A-Software-Library-Diskette-Listing.md).
It holds **20 files using 263 of 610 reserved pages**, owned by `FLOPPY-USER`:

- `TPF-MON-100-A04` (`PROG`) — the test program monitor that runs on the ND-100 side
- `SEMICS-1-A00` (`TEST`) plus `SEMICS-2-A00` / `SEMICS-3-A00` (`NEXT` continuation files)
- Per-block `EXT` tests, which map one-to-one onto the SAMSON block diagram:
  `ALU-VERIFY`, `ALU-CARD`, `MIC-REGISTER`, `MIC-SEQUENCE`, `CACHE-TEST`,
  `IAC-TEST`, `DAC-TEST`, `IDU-REGISTER`, `IDA-VERIFY`,
  `DMM-REGISTER`, `IMM-REGISTER`, `MM-VERIFY`,
  `AAP1-2-TEST`, `AAP2-VERIFY`
- Two `MODE` install files: `INST-SEMICS-A00`, `INST-EXT-A00`

The block abbreviations (IAC, DAC, DMM, IMM, MIC, AAP, ...) are the ones defined in the SAMSON
design document — see
[../../../Reference-Manuals/500/ND-SAMSON-1-EN SAMSON Expected Behaviour.md](../../../Reference-Manuals/500/ND-SAMSON-1-EN%20SAMSON%20Expected%20Behaviour.md).

## What is NOT known

- No floppy image for this article number has been located, so the programs themselves are absent.
- The install procedure is not documented here; it lives in the two `MODE` files on the diskette.

## Provenance

Scanned PDF `ND-250247A-DDHF-ND-5000-Test-Microprograms-30001682.pdf` (DDHF /
Datamuseum.dk, item 30001682), OCR'd 2026-08-24. 9 pages.

---
**Parent:** [../README.md](../README.md) (Software catalog)
