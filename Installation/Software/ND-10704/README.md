# ND-10704 — FORTRAN Interface (T2000 patch)

> Status: IN-PROGRESS — real floppy decoded; a narrow patch, not a standalone product

| Field | Value |
|-------|-------|
| ND article number | `ND-10704` |
| Product name | FORTRAN interface (patch for T2000 HSTI Software) |
| Functional category | Patches / Language Tools |
| Related products | T2000 HSTI Software — flagged low-priority in the floppy backlog (`ND-210907B`, "errors"), not separately documented in this catalog. |

## What is known — real floppy, decoded

Floppy `10704E00-1D` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts cleanly.
Twelve `T2BA01:SYMB` through `T2BA12:SYMB` source files, one `T2-BEN-BEN:NRF` object, an installer
`INSTALL-10704-1:XCOM`, and a real, decoded patch script `PATCH-UPD-BEN:MODE`:

```
@QED (T2000-PROG)T2-RAS-LINK:MODE
S:L//CC **FORTRAN** /
W (T2000-PROG)T2-RAS-LINK:MODE
EX
@QED (T2000-SAVE)T2-RAS-LINK:MODE
S:L//CC **FORTRAN** /
W (T2000-SAVE)T2-RAS-LINK:MODE
EX
@QED (T2000-PROG)T2-GMA-LINK:MODE
S:L//CC **FORTRAN** /
W (T2000-PROG)T2-GMA-LINK:MODE
EX
@QED (T2000-SAVE)T2-GMA-LINK:MODE
S:L//CC **FORTRAN** /
W (T2000-SAVE)T2-GMA-LINK:MODE
EX
@CC
@CC PATCHING DONE
@CC
```

**Reading it**: this is not an installer for a standalone product — it's a `QED`-scripted patch
that inserts a `**FORTRAN**` marker comment line into four existing files
(`T2-RAS-LINK:MODE`/`T2-GMA-LINK:MODE`, each present under both a `T2000-PROG` and a `T2000-SAVE`
user) belonging to the T2000 HSTI Software product. `S:L//CC **FORTRAN** /` is a real QED
substitute command: search for a blank line (`L` = the pattern before `//`, here empty, matching
line-start) and insert the comment text before it.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for the `:MODE` script, in full).
- **TODO:** what the patch actually enables (presumably a FORTRAN-callable code path in T2000
  HSTI) was not determined — the twelve `T2BAnn:SYMB` source files were not opened.

---
**Parent:** [../README.md](../README.md) (Software catalog)
