# ND-211250 — UNIQUE DOCUMENTATION (for ND-500/SIBAS-500)

> Status: VERIFIED identity (from real decoded install scripts naming the domain `UNIQUE-DOC-S5-C`); no PD/PI sheet located

| Field | Value |
|-------|-------|
| ND article number | `ND-211250` (identity confirmed by decoding the floppy's own install scripts — no PD/PI sheet names this number directly) |
| Product name | UNIQUE DOCUMENTATION |
| Functional category | 4th-generation application tools (DIALOGUE-1/UNIQUE family) |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-500, against a SIBAS-500 database (`-S5-` in every internal file/domain name) |
| OS requirement | unknown — no PD sheet |
| Related products | `ND-211202`/`ND-211203` DIALOGUE-1 (the umbrella package UNIQUE DOCUMENTATION is bundled/available with, per that PI sheet) — see [../ND-210729/README.md](../ND-210729/README.md) for the full family map; `ND-211005` UNIQUE Text System (near-certain runtime dependency, same pattern as every other UNIQUE product) |

## What this is

Per the `ND-211202` "DIALOGUE-1" PI sheet's description of the UNIQUE DOCUMENTATION function (no
PD sheet exists for this specific article, so this is the closest sourced description): "produces
system documentation and user documentation, i.e. user handbook, database documentation,
cross-reference lists of fields for maintenance etc." — a tool that generates documentation
*about* a UNIQUE/SIBAS application, not a document-authoring tool itself.

## How the identity was confirmed

No PD or PI sheet in this repo's archive names `ND-211250` directly. Its identity was recovered
by mounting the floppy and decoding its three real `:MODE` install scripts (`byte & 0x7F`,
verified clean) — every one of them builds a domain explicitly named **`UNIQUE-DOC-S5-C`**:
```
SET-DOMAIN "UNIQUE-DOC-S5-C"
...
DEFINE-STANDARD-DOMAIN UNIQUE-DOC-S5-C,(DIALOG-SYS)UNIQUE-DOC-S5-C
```
`-S5-` matches the same "SIBAS-500" naming convention seen elsewhere in this catalog (e.g.
`SIB2-500` user, `SIBR-LIBRARY-A05` — the SIBAS-500 runtime library this product links against).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| C13 | [ND-211250C13](ND-211250C13/README.md) | verified identity, install scripts real/decoded | floppy `211250C13-XX-01D` |

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located under this number — see
  [`ND-211202-A1-EN.md`](../../Product-Info/ND-211202-A1-EN.md) for the closest description
  (as a DIALOGUE-1 bundled function, not its own sheet)
- Manual(s): unknown — likely covered within `ND-60.260` UNIQUE Application Development or
  `ND-60.240` UNIQUE UNIQUICK User Guide (both referenced by the ND-211202 sheet, neither located
  in this repo)

## Provenance
Floppy contents and install scripts confirmed by downloading the image (MD5
`9a4808530a4c66021778517f6787f18b`) and reading/decoding with `ndtool`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
