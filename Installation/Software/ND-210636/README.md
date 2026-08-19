# ND-210636 — NORTEXT-100 AD System, AD Order Entry w/Editor Extension

> Status: IN-PROGRESS — real floppy set decoded, real installer present (compiled, not decoded)

| Field | Value |
|-------|-------|
| ND article number | `ND-210636` |
| Product name | NORTEXT-100 AD System, AD Order Entry with Editor Extension |
| Functional category | Office — NORTEXT (typesetting/publishing) |
| CPU target | ND-100 |
| Related products | `ND-210733` NORTEXT-100 AD System, AD Order Entry Module — see [../ND-210733/README.md](../ND-210733/README.md), likely the base module this extends with editing capability. |

## What is known — real floppy set, decoded

Two floppies, `210636C00-EN-1D`/`-XX-2D` (downloaded via NDwiki, imaged by Torfinn "Tingo"
Ingolfsen):

```
210636C00-IN-EN:PROG       real installer (compiled, not decoded)
ORDER-EN-C00:TXT/:ERR/:FORM/:HELP/:INFO   text, error, form, help, info files for the order system
OR-MAIN-EN-C00:PROG        main program
OR-ADUTILITY-C00:PROG      utility program
OR-EDITOR-C00:PROG         editor program
OR-UTILITY-C00:PROG        further utility program
```

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: two real floppy images, downloaded via NDwiki and decoded in this session (`ndfs -t`).
- **TODO:** installer is compiled, not decoded; `:FORM`/`:HELP`/`:INFO`/`:TXT` content files not opened.

---
**Parent:** [../README.md](../README.md) (Software catalog)
