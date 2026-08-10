# ND-210073 — ISAM for ND-100/ND-500

> Status: IN-PROGRESS — install mechanics verified from a real decoded script; programming API NOT documented (source manual not in this repo)

| Field | Value |
|-------|-------|
| ND article number | `ND-210073` (unified ND-100+ND-500 product; older `ND-10343` was ND-500-only, max 6 keys — see below) |
| Product name | ISAM for ND-100/ND-500 |
| Functional category | Database / file-access library — used *from* FORTRAN, PLANC, Pascal, and COBOL, not a standalone application |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 and ND-500 (file-compatible between the two) |
| OS requirement | unknown — no install PD sheet located |
| Related products | `ND-10343` older ND-500-only ISAM (max 6 keys vs. this product's max 64) · `ND-10176`/`ND-10177`/`ND-210177` COBOL, which bundles ISAM directly on its own floppies (see below) — no standalone ISAM floppy has been located separately from a COBOL disk |

## What ISAM is

Indexed Sequential Access Method — disk storage/retrieval of records by index (key) or in
sequential order, call-oriented (you call routines from the ISAM library). COBOL's own indexed
file organization uses it directly; FORTRAN, PLANC, and Pascal call it explicitly. An ISAM file is
normally two SINTRAN files (index part + data part), optionally one. Full feature list:
[../../Product-Info/ND-210073-A1-EN.md](../../Product-Info/ND-210073-A1-EN.md). [PI]

**Key numbers, per the PI sheet:** up to 64 keys per file (all may have duplicates, may overlap,
group keys allowed), up to 2^24 records, max record length 32 Kbytes, max key length 240 bytes,
fixed or variable record length, ND-500 version "at least 4 times faster" than ND-100. [PI]

## The four modules — what's actually on disk

Every mention of ISAM found on a real floppy in this catalog uses the same four-module shape the
PI sheet describes:

| Module | What it is | Confirmed on disk as |
|---|---|---|
| ISAM library | all ISAM routines, linked into your program | `ISAMRT-I00:BRF` (ND-100) — loaded as a background RT process, not linked directly into each program; see the install procedure below |
| ISAM service program | interactive: convert a SINTRAN file to ISAM, modify structure (create/delete keys), reset error flag, verify/repair consistency, unload/load, rebuild index from data, estimate index size | `ISAM-SERVICE-I00:PROG` (ND-100), `ISAM-SERVICE-K01:PROG`/`ISAM-SERVICE-K02:PROG` (ND-500, on the COBOL-500 floppies) |
| ISAM interactive | interactive store/retrieve/modify/delete without writing a program | `ISAM-INTER-I00:PROG` (ND-100), `ISAM-INTER-K01:PROG` (ND-500) |
| ISAM multiuser supervisor | resolves concurrency between programs sharing the same ISAM files | `IS-MULTI-K00:NRF`/`:MODE` (found only on the ND-500 COBOL floppies, e.g. [ND-210177J02](../ND-210177/ND-210177J02/README.md) — no ND-100-side multiuser supervisor file has been found) |

**No standalone ISAM distribution floppy has been located** — every occurrence found so far is
bundled onto a COBOL floppy: [ND-10176H00](../ND-10176/ND-10176H00/README.md) (ND-100, real
decoded install script) and [ND-210177J02](../ND-210177/ND-210177J02/README.md) /
[ND-210177K01](../ND-210177/ND-210177K01/README.md) (ND-500).

## Installing the ND-100 ISAM runtime — REAL, decoded script

See [ND-10176H00's own writeup](../ND-10176/ND-10176H00/README.md#isam-runtime--real-decoded-isamrt-i00mode-mode)
for the full verbatim `ISAMRT-I00:MODE` script and its explanation — in short: it's an
`@RT-LOADER` script that loads the ISAM library onto a free RT segment as a background process
(`ISAMRT1`), patches in a chosen internal device-number pair, and is started with `@RT ISAMRT1`.
Running more than one ISAM runtime at once means re-running the script with a new process name,
segment number, and device-number pair each time.

## Installing the ND-500 ISAM multiuser supervisor — REAL, decoded script

See [ND-210177J02's own writeup](../ND-210177/ND-210177J02/README.md#is-multi-k00mode--decoded-verbatim)
for the full verbatim `IS-MULTI-K00:MODE` script — a `Linkage-Loader` script that creates a shared
common data segment (`IsPool`/`IsParam`/`IsHeadFil`/`IsHeadRec`/`IsRecArr`) which the COBOL
runtime library then attaches to for multiuser ISAM coordination.

## Programming against ISAM — NOT DOCUMENTED HERE, by design

**This document does not cover ISAM's actual call syntax** (routine names, parameters, how to
`OPEN`/`CREATE`/`READ`/`WRITE`/define keys from FORTRAN, PLANC, Pascal, or COBOL's `SELECT`
clause) — the ISAM Reference Manual (`ND-60.108.5 Rev. A EN`) is **not in this repo**. Do not
invent this content. If deeper programming detail is needed:
1. Source/OCR the ISAM Reference Manual (`ND-60.108.5`) and add it to `Reference-Manuals/`.
2. In the meantime, COBOL's own indexed-file `SELECT`/`ORGANIZATION IS INDEXED` clauses (general
   ANSI-74 COBOL, documented in `ND-60.144` already in this repo) are the closest available
   in-repo reference for the COBOL side specifically — not verified to map 1:1 onto this ISAM
   product's own extensions (multi-user modes, the 64-key limit, etc.).

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-210073-A1-EN.md](../../Product-Info/ND-210073-A1-EN.md)
  (unified ND-100/500 product) · older ND-500-only version: [../../Product-Info/ND-10343-A1-EN.md](../../Product-Info/ND-10343-A1-EN.md)
- Manual(s): ISAM Reference Manual `ND-60.108.5 Rev. A EN` (**not located in this repo** — the
  single biggest gap for actually programming against ISAM)
- NDWIKI: not checked yet

## Provenance
Install scripts: real, decoded from the ND-10176H00 and ND-210177J02 floppies (see those docs for
MD5s and extraction method). Product/feature description: PI sheets only, no PD sheet located.

---
**Parent:** [../README.md](../README.md) (Software catalog)
