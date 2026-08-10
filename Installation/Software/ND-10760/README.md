# ND-10760 — C-Compiler for ND-100 (CC-100)

> Status: IN-PROGRESS — install procedure recovered from the real floppies; product identity (article number) still UNVERIFIED (see Provenance)

| Field | Value |
|-------|-------|
| ND article number | `ND-10760` — **assumed** from the floppy volume label (`10760A00-1` / `10760A00-2`); no PD/PI sheet carrying this number has been located, so the article number itself is not independently confirmed. |
| Product name | C-Compiler for ND-100 (CC-100) |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 |
| OS requirement | unknown — not documented |
| Related products | `ND-210761` "CC-500" — the ND-500 C compiler (separate product, separate floppy set, PD sheet located) — see [ND-210761](../ND-210761/README.md) |

## Description
The C compiler for the ND-100, companion to CC-500. Ships as a two-floppy set. User-facing docs
are shared with CC-500 in `ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual`. **No Program
Description or Product Information sheet for this product has been found** in this repo's
`Installation-Description/` or `Product-Info/` archives — but the install procedure itself has
been recovered directly from the floppies (downloaded by MD5 hash and read with `ndtool`), so it
no longer depends on a PD sheet existing. `[MODE]`

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A | [ND-10760A](ND-10760A/README.md) | IN-PROGRESS — install procedure verified from the real `INSTALL-1:MODE`/`INSTALL-2:MODE` | floppy set `10760A00-1` + `10760A00-2` |

## Documentation
- Program Description (PD-sheet): **not located**
- Product Information (PI-sheet): **not located**
- Manual(s): `ND-60.214.01` CC-100 and CC-500 C-Compiler User Manual — [../../../Reference-Manuals/ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual.md](../../../Reference-Manuals/ND-60.214.01%20CC-100%20and%20CC-500%20C-Compiler%20User%20Manual.md)
- Developer guide: [../../../Developer/Languages/Application/C-DEVELOPER-GUIDE.md](../../../Developer/Languages/Application/C-DEVELOPER-GUIDE.md)
- NDWIKI: not checked yet

## Provenance
Floppy volume names (`10760A00-1`, `10760A00-2`) and per-file directory listings only — read from
a local floppy-image reference catalog (899 dumped ND floppy images with directory listings,
consulted as a temporary working reference, not committed to this repo). The article number
`ND-10760` is an assumption based on the floppy label's leading digits, following the same
volume-name -> article-number convention documented in
[../../ND-NUMBERING-REFERENCE.md](../../ND-NUMBERING-REFERENCE.md) and used by
[../README.md](../README.md) "How products map to floppies" — but for this product it is
**unconfirmed**, since no PD sheet exists to cross-check it against.

---
**Parent:** [../README.md](../README.md) (Software catalog)
