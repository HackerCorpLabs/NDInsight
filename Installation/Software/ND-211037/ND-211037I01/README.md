# ND-211037I01 — PLANC for ND-110 compiling on ND-500/5000, version I (rev 01)

> Status: VERIFIED (transcribed from PD sheet; not yet run live)   ·   Install source: [PD]

| Field | Value |
|-------|-------|
| Part number | `211037I01` |
| Base product | [`ND-211037`](../README.md) |
| Version | I, revision 01 |
| Release date | 88.07.29 |
| CPU target | Hosted on ND-500/5000, generates ND-110 code |
| OS requirement | SINTRAN III >= I |

## Prerequisites
- **Mass storage (permanent):** `SYSTEM` 10 pages/1 file; `<ANY>` 145 pages/3 files; `<ANY>`
  (second user) 31 pages/2 files. [PD]

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `211037I01-XX-01D` | `DESCRIPTION-FILE:DESC` (9 pages) · `PLANC-100-I01:LINK`(9p)/`:DSEG`(51p)/`:PSEG`(85p) — the compiler domain · `PLANC-1BANK-I01:BRF`(15p)/`PLANC-2BANK-I01:BRF`(16p) — runtime · `PLANC-I:HELP` (10 pages, version info) · `IN-PLANC-I01:PROG`(66p)/`:XCOM`(15p)/`:INIT`(11p) — the installer · `SCRATCH-SEG-01:LINK`/`:DSEG`/`:PSEG` (empty placeholders), user `FLOPPY-USER` |

`IN-PLANC-I01:PROG`/`:XCOM`/`:INIT` is the same three-file installer shape already seen on the
ND-500 Linkage-Loader and ND LINKER installers elsewhere in this catalog.

## Installation procedure

Source: PD sheet §4, verbatim — installer-driven and manual paths both given. [PD]

### Installer-driven
```
@(211037I<rev>-XX-01:FLOPPY-USER)IN-PLANC-I<rev>
@RELEASE-DIRECTORY 211037I<rev>
```

### Manual
```
@COPY-FILE "PLANC-1BANK-I<rev>:BRF"
SOURCE FILE: {211037I<rev>-XX-01:FLOPPY-USER}PLANC-1BANK-I<rev>:BRF

@COPY-FILE "PLANC-2BANK-I<rev>:BRF"
SOURCE FILE: {211037I<rev>-XX-01:FLOPPY-USER}PLANC-2BANK-I<rev>:BRF

@LINKAGE-LOADER
N11:ABORT-BATCH-ON-ERROR OFF
N11:DELETE-DOMAIN PLANC-100
N11:COPY-DOMAIN
SOURCE DOMAIN: {211037I<rev>-XX-01:FLOPPY-USER}PLANC-100-I<rev>
DESTINATION DOMAIN: "PLANC-100-I<rev>"
N11:EXIT

@CC Now, you must be user SYSTEM!
N500:DEFINE-STANDARD-DOMAIN PLANC-100-I<rev>,PLANC-100-I<rev>
N500:EXIT

@COPY-FILE "PLANC-I:HELP"
SOURCE FILE: {211037I<rev>-XX-01:FLOPPY-USER}PLANC-I:HELP
```

## Starting the compiler
```
@PLANC-100-I
```
or
```
@ND PLANC-100-I
```

## Configuration / post-install
None beyond the standard-domain registration above.

## Documentation
- PD-sheet: [../../../Installation-Description/ND-211037-9-EN.md](../../../Installation-Description/ND-211037-9-EN.md)
- Manual(s): `ND-60.117.5` PLANC Reference Manual

## Provenance & open items
- Source: single, complete 10-page OCR'd PD-sheet scan.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-211037` product overview)
