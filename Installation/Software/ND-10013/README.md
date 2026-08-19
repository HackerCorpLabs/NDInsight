# ND-10013 — NSHS (NORD Screen Handling System)

> Status: IN-PROGRESS — real floppy (parts 2-3 of 3) decoded, runtime file inventory recovered; part 1 (installer/description) not found

| Field | Value |
|-------|-------|
| ND article number | `ND-10013` |
| Product name | NSHS — NORD Screen Handling System |
| Functional category | Language Tools / UI building |
| Callable from | FORTRAN, BASIC, COBOL, RPG II |
| Related products | A second, real concrete answer to "how do I build a UI on ND hardware" alongside VTM and COBOL's screen-handling statements — see [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md) |

## What is known
Two modules: the **NORD Screen Picture Maintenance Program** (an interactive program for
creating/modifying/testing screen "pictures" — leading text plus input-field descriptions, with
per-field editing formats, storage codes, and input-control functions), and the **NORD Screen
Picture Handling Library** (routines your program calls to read/write records through a saved
picture). Pictures are stored as disk files, retrieved by user programs, and can drive both
terminal I/O and direct updates to local databases; a hardcopy of a picture can be printed. [PI]

Field-level features: standard editing for numeric/decimal/alphabetic/alphanumeric/bank-account/
social-security-number fields, user-defined field editing, automatic storage-format conversion
(integer/byte/packed-decimal), content checking (legal/illegal value ranges, default values,
check-digit verification mod 10/11, date controls, field accumulation, user-supplied control
procedures), and simple keystroke-based field editing (copy previous value, insert/delete
characters). [PI]

**No PD sheet has been located** for this article number — only the PI sheet.

## What is known — real floppy (parts 2-3), decoded

Two floppies, `ND-10013K-PART2`/`-PART3` (downloaded via NDwiki, imaged by Torfinn "Tingo"
Ingolfsen — part 1, which likely carried the installer and `DESCRIPTION-FILE`, was not imaged/
found). Both mount cleanly:

| File | Interpretation |
|---|---|
| `SCREEN-1BANK-K:BRF` | 1-bank runtime library |
| `SCREEN-2BANK-K:BRF` | 2-bank runtime library (see [TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)) |
| `SCREEN-1REEN-K:BRF` | a third, "reentrant" runtime variant — 1-bank, but built for reentrant/shared loading (see [LINKING-GUIDE.md §6.4](../../../Developer/Workflow/LINKING-GUIDE.md) on the PROG-vs-reentrant-BPUN distinction) |
| `SCREEN-COPY-K:BPUN` | a utility program (likely for copying/managing picture files) |
| `SCREEN-DEMO-K:SYMB` | demo source |
| `SCREEN-UCONT-K:SYMB` | source (control-related, name not further decoded) |
| `SCREEN-SYMB-K:SYMB` | symbol table |

No `:MODE`/`:BATC` install script and no `DESCRIPTION-FILE` were found on these two disks — the
installer, if any, was likely on the missing part 1.

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10013-A2-EN.md](../../Product-Info/ND-10013-A2-EN.md)

## Provenance & open items
- Source: two real floppy images (of a 3-part set), downloaded via NDwiki and decoded in this
  session (`ndfs -t`/`ndtool -x`).
- **TODO:** part 1 of this floppy set has not been located — likely carries the installer and/or
  `DESCRIPTION-FILE`.

---
**Parent:** [../README.md](../README.md) (Software catalog)
