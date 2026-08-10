# ND-10177 — ND-500 COBOL

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10177` |
| Product name | ND-500 COBOL |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-210177` — later article, "COBOL-85 for ND-500/5000" (ANSI-85 successor, see [../ND-210177/README.md](../ND-210177/README.md)) · `ND-10343` ISAM for ND-500 (indexed-file support, bundled onto these same floppies — see below) |

## Description
The original ND-500 COBOL compiler, shipped as an ND-500 **domain** (`:LINK`/`:DSEG`/`:PSEG`
segment triple), the same distribution shape as `ND-210190` FORTRAN-for-ND-500 and unlike the
plain-file ND-100 language products in this catalog. **No Program Description sheet has been
located** — only the general marketing PI sheet
([../../Product-Info/ND-10177-A1-EN.md](../../Product-Info/ND-10177-A1-EN.md) /
[../../Product-Info/ND-10177-A2-EN.md](../../Product-Info/ND-10177-A2-EN.md)). [PI]

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| H00 | [ND-10177H00](ND-10177H00/README.md) | IN-PROGRESS — one part of a 3-disk set is missing | floppies `10177H00-1S`, `10177H00-3S` — **`10177H00-2S` not found anywhere**, see below |

## The missing `10177H00-2S`

Only `10177H00-1S` (the compiler domain) and `10177H00-3S` (an ISAM add-on) were found — both in
your physical set and in the reference floppy-image library consulted for this catalog. Neither
this repo nor the library used to source floppy images has a `10177H00-2S`. Given what PART1 and
PART3 actually contain (see [ND-10177H00](ND-10177H00/README.md)), and given that the later
`ND-210177` revisions (J02, K01) each bundle a `COBOL-*-LIB-*:NRF` runtime library alongside the
domain, **the most likely content of the missing PART2 is that runtime library** — but this is
inference from the gap's shape, not confirmation that the file ever existed under that name, and
not proof it is truly lost rather than mislabeled. If you locate a real `10177H00-2S`, that
settles it either way.

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): [../../Product-Info/ND-10177-A1-EN.md](../../Product-Info/ND-10177-A1-EN.md), [../../Product-Info/ND-10177-A2-EN.md](../../Product-Info/ND-10177-A2-EN.md)
- Manual(s): `ND-60.144.3` COBOL Reference Manual — [../../../Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md](../../../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents confirmed by downloading both images (MD5s in the version doc) and reading with
`ndtool`. The gap analysis above is reasoning from what is and isn't on the two disks found, not
an external source confirming the missing part's identity.

---
**Parent:** [../README.md](../README.md) (Software catalog)
