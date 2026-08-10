# ND-210177 — COBOL-85 for ND-500/5000

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-210177` |
| Product name | COBOL-85 for ND-500/5000 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500 / ND-5000 |
| OS requirement | unknown — no PD sheet located |
| Related products | `ND-10177` — older article for the ANSI-74-era compiler, see [../ND-10177/README.md](../ND-10177/README.md) · `ND-10343` ISAM for ND-500 (bundled onto these floppies, see below) |

## Description
The ANSI-85 COBOL compiler for ND-500/5000, same domain-based distribution shape as `ND-10177`.
Two revisions confirmed, both bundling the compiler domain, the runtime library, and an ISAM
add-on onto a **single** floppy each — a consolidation similar to what happened with
`ND-210191` FORTRAN (3 floppies → 1). **No Program Description sheet has been located.**

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| J02 | [ND-210177J02](ND-210177J02/README.md) | **VERIFIED procedure** — real `:MODE` install files decoded from the floppy | floppy `210177J02-XX-01D` |
| K01 | [ND-210177K01](ND-210177K01/README.md) | IN-PROGRESS — no `:MODE` script on this disk, procedure adapted from J02 | floppy `210177K01-XX-01D` |

An earlier revision, `210177H03-XX-01D`, is known to exist (its volume name was found in the
floppy reference library) but was **not** available to download when checked and is not
documented here.

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located (the `ND-10177-A1/A2` sheets predate this article number and were not re-checked for `ND-210177` specifically)
- Manual(s): `ND-60.144.3` COBOL Reference Manual — [../../../Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md](../../../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md)
- NDWIKI: not checked yet

## Provenance
Floppy contents and the real install-script text confirmed by downloading both images and reading
with `ndtool` — including decoding `COBOL-LIB-J02:MODE` and `IS-MULTI-K00:MODE` with the same
bit-7-mask technique used for the CC-100 install scripts.

---
**Parent:** [../README.md](../README.md) (Software catalog)
