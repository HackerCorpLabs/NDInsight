# ND-10309 — PLANC for ND-100

> Status: IN-PROGRESS

| Field | Value |
|-------|-------|
| ND article number | `ND-10309` |
| Product name | PLANC for ND-100 |
| Functional category | Programming Languages & Compilers |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 (also NORD-10 per the A/B PD sheets' "10" checkbox) |
| OS requirement | SINTRAN III VS |
| Related products | `ND-10310` PLANC for ND-500 (sibling product, documented together on the same Product-Info sheet — not yet catalogued here); source article `ND-10370` (compiler's own source code registration, printed on every PD sheet as "ND-NUMBER FOR SOURCE") |

## Description
PLANC (Programming Language for ND Computers) — a high-level, machine-independent systems
programming language in the ALGOL/PASCAL family, used mainly for writing operating systems and
compilers. Full feature description:
[../../Product-Info/ND-10309-A1-EN.md](../../Product-Info/ND-10309-A1-EN.md). [PI]

This is the compiler most of this repo's system-level documentation refers to — see
[../../../Developer/Languages/Application/PLANC-DEVELOPER-GUIDE.md](../../../Developer/Languages/Application/PLANC-DEVELOPER-GUIDE.md).

## Versions

| Version | Subfolder | Status | Notes |
|---------|-----------|--------|-------|
| A | [ND-10309A](ND-10309A/README.md) | verified (PD sheet transcribed) | dated 82.02.19; ships as `:BPUN` |
| B | [ND-10309B](ND-10309B/README.md) | verified (PD sheet transcribed) | dated 82.06.15; ships as `:BPUN`; adds `$OPTION ARRAY-INDEX-CHECK` and `$EJECT` |
| F | [ND-10309F](ND-10309F/README.md) | IN-PROGRESS — no PD sheet, procedure adapted from A/B | floppy `10309F00-1S`; ships pre-linked as `:PROG` (a shape change from A/B) |

## Documentation
- Program Description (PD-sheet), version A: [../../../Reference-Manuals/ND-10309A PLANC FOR ND-100.md](../../../Reference-Manuals/ND-10309A%20PLANC%20FOR%20ND-100.md)
- Program Description, version B: [../../../Reference-Manuals/ND-10309B PLANC FOR ND-100.md](../../../Reference-Manuals/ND-10309B%20PLANC%20FOR%20ND-100.md)
- Program Description, version F: not located
- Product Information (PI-sheet): [../../Product-Info/ND-10309-A1-EN.md](../../Product-Info/ND-10309-A1-EN.md) (covers ND-10309 and ND-10310 together)
- Manual(s): `ND-60.117.03`/`ND-60.117.5` PLANC Reference Manual — [../../../Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md](../../../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)
  · [../../../Reference-Manuals/ND-860117-6-EN PLANC - User Guide and Reference Manual.md](../../../Reference-Manuals/ND-860117-6-EN%20PLANC%20-%20User%20Guide%20and%20Reference%20Manual.md)
  · [../../../Reference-Manuals/ND-20034-1-EN ND-Specific Programming & Advanced PLANC.md](../../../Reference-Manuals/ND-20034-1-EN%20ND-Specific%20Programming%20%26%20Advanced%20PLANC.md)
- NDWIKI: not checked yet

## Provenance
Versions A/B: two OCR'd PD-sheet scans, filed under `Reference-Manuals/` (not
`Installation-Description/` — an existing naming/filing quirk, not something changed here).
Version F: floppy directory listing only (temporary working reference, not committed to this
repo).

---
**Parent:** [../README.md](../README.md) (Software catalog)
