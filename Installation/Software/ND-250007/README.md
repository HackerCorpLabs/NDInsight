# ND-250007 — Mini-Line Editor (MLE) for ND-100/500

> Status: IN-PROGRESS — real floppy decoded, full runtime file inventory recovered; no install script found

| Field | Value |
|-------|-------|
| ND article number | `ND-250007` |
| Product name | Mini-Line editor (MLE) for ND-100/500 |
| Functional category | Editors & Word Processing |
| CPU target | ND-100 / ND-500 |

## What is known — real floppy, decoded

Floppy `250007A00-XX-01S` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts
cleanly. Seven runtime modules, each shipped as a 1-bank/2-bank `:BRF` pair (see
[TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)), plus one source module:

| Module | 1-bank | 2-bank |
|---|---|---|
| Line editing core | `MLE-LINE-1B-A00:BRF` | `MLE-LINE-2B-A00:BRF` |
| Editor | `MLE-EDIT-1B-A00:BRF` | `MLE-EDIT-2B-A00:BRF` |
| Service | `MLE-SERVI-1B-A00:BRF` | `MLE-SERVI-2B-A00:BRF` |
| Library | `MLE-LIB-1B-A00:BRF` | `MLE-LIB-2B-A00:BRF` |
| Data | `MLE-DATA-1B-A00:BRF` | `MLE-DATA-2B-A00:BRF` |
| Screen handling | `MLE-SCREE-1B-A00:BRF` | `MLE-SCREE-2B-A00:BRF` |
| Utilities | `MLE-UTILI-1B-A00:BRF` | `MLE-UTILI-2B-A00:BRF` |

Plus `MLE-A00:NRF` (a relocatable object — likely the top-level entry point/main module).

No `:MODE`/`:BATC` install script, no `DESCRIPTION-FILE`, and no installer `:PROG` were found —
this floppy is the raw runtime module set only.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x`).
- **TODO:** no install procedure could be recovered — the module names suggest a standard
  `COPY-DOMAIN`/`COPY-FILE`-style manual install matching this catalog's other ND-500 runtime
  products, but that is an inference, not evidenced here.

---
**Parent:** [../README.md](../README.md) (Software catalog)
