# ND-210865 — MERCUR for ND-500 (English)

> Status: IN-PROGRESS — real floppy decoded, runtime module inventory recovered

| Field | Value |
|-------|-------|
| ND article number | `ND-210865` |
| Product name | MERCUR for ND-500, English version |
| Functional category | Programming Languages & Compilers (application-specific, financial modelling) |
| CPU target | ND-500 |
| Related products | `ND-10145` MERCUR (see [../ND-10145/README.md](../ND-10145/README.md)) — PI-sheet-only entry for the general MERCUR product/language; this is the real ND-500 runtime. |

## What is known — real floppy, decoded

Floppy `210865B00-EN-01D` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen; reported
"errors" by the imager, but it mounted and read cleanly in this session) contains seven relocatable
modules:

```
MRC-MAIN1-EN-B00:NRF   through   MRC-MAIN7-EN-B00:NRF
```

Sizes range from ~1.5 KB (`MAIN2`) up to ~256 KB (`MAIN4`, by far the largest — likely the core
interpreter). No `:MODE`/`:BATC` install script, no `DESCRIPTION-FILE`, and no installer `:PROG`
were found — a bare runtime module set only.

## Documentation
- No PD sheet, no PI sheet located specific to the ND-500 version (see [ND-10145](../ND-10145/README.md)
  for the general MERCUR PI sheet).

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x`).
- **TODO:** no install procedure could be recovered.

---
**Parent:** [../README.md](../README.md) (Software catalog)
