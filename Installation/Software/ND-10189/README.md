# ND-10189 — COBOL runtime System

> Status: STUB — only a partial floppy set located

| Field | Value |
|-------|-------|
| ND article number | `ND-10189` |
| Product name | COBOL runtime System |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 |
| OS requirement | unknown |
| Related products | `ND-10176` COBOL for ND-100 (the one located floppy for this product is **byte-identical** to a component already bundled in [ND-10176H00](../ND-10176/ND-10176H00/README.md) — see below) |

## Description
Only one floppy for this article number has been located: `10189H00-2S`. Its four files
(`ISAMRT-I00:MODE`, `ISAMRT-I00:BRF`, `ISAM-SERVICE-I00:PROG`, `ISAM-INTER-I00:PROG`) are
**identical in name, size, and timestamp** to the ISAM component already documented as part of
[ND-10176H00](../ND-10176/ND-10176H00/README.md) (via its `10176H00-3S` companion floppy). This
strongly suggests `ND-10189` is the ISAM/runtime portion of the COBOL-100 release, distributed
both bundled into `ND-10176` (as `10176H00-D`) and separately under its own article number —
**not confirmed**, since no PD/PI sheet for `ND-10189` has been located to state the relationship
explicitly.

Given the naming ("`-2S`", implying a multi-part set) and that only one part was found, **this
entry is known to be incomplete** — there is likely a `10189H00-1S` (and possibly more parts)
not yet located.

## Versions

No version subfolder created — see [ND-10176H00](../ND-10176/ND-10176H00/README.md) for the
fully-documented, verified-identical content (ISAM runtime install via `ISAMRT-I00:MODE`, a real
decoded RT-LOADER script).

## Documentation
- Program Description (PD-sheet): not located
- Product Information (PI-sheet): not located
- Manual(s): presumed ISAM Reference Manual `ND-60.108.5` (not located in this repo), given the
  file overlap with the ISAM component

## Provenance
Floppy contents confirmed by downloading the image (MD5 `7a1c0074d3325a484ae5f6ec7cc5ce18`) and
reading with `ndtool` — cross-checked byte-for-byte (name/size/timestamp) against
`10176H00-3S`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
