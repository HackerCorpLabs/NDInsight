# ND-10188 — FOCUS Level 1 Screen Handling System

> Status: IN-PROGRESS — real 4-disk floppy set decoded, no PD sheet located (only a real PI sheet)

| Field | Value |
|-------|-------|
| ND article number | `ND-10188` (later article: `ND-210188`) |
| Product name | FOCUS Level 1, Screen Handling System For Transaction Processing |
| Functional category | Graphics, Plotting & Screen Handling |
| Callable from | FORTRAN, BASIC, COBOL |
| Manual | `ND-60.137` (per the PI sheet's own citation; not present in this repo) |
| Related products | A **fourth** real, concrete answer to "how do I build a UI on ND hardware" alongside VTM, PLANC-SCREEN-H, and NSHS — see the full comparison in [PLANC-VTM-UI-CATALOG.md](../../../Developer/Workflow/PLANC-VTM-UI-CATALOG.md). Confirmed built in PLANC and sitting on VTM (see below) — resolves the earlier "no manual found for FOCUS" note in that catalog: a real PI sheet exists in this repo, it was just not yet cross-referenced. |

## What is known — from the real PI sheet

Source: [Installation/Product-Info/ND-10188-A2-EN.md](../../Product-Info/ND-10188-A2-EN.md), verbatim.

FOCUS lets a user define screen forms interactively at display terminals with cursor control, and
use those forms for data input/output from programs. Two modules, in the PI sheet's own words —
almost identical phrasing to NSHS's PI sheet (`ND-10013-A2-EN`), see the open question this raises
in the UI catalog:

- **ND FORMS Maintenance Program** — interactive picture/form creation and modification
  (leading texts + data-entry fields, editing formats, fill characters). New form modifications
  create a new version; old versions remain until explicitly deleted.
- **ND FORMS Handling Library** (called "FOCUS Runtime Library" elsewhere in the same sheet) —
  callable from **FORTRAN, BASIC, or COBOL**. Routines exist to retrieve forms, display leading
  text/fields, read operator-entered data, write to a field, edit a single field, and write
  messages to a specific screen line. **Both reentrant and non-reentrant versions exist** — "when
  many active programs are using the system, the reentrant version should be used."
- A third listed component: **FOCUS Auxiliary Subroutines**.

"The program can be used from different types of terminals to input screen layouts and commands
without user specification of terminal type" — consistent with sitting on VTM, and directly
confirmed by the real floppy contents below.

## What is known — real floppy, decoded (4 disks)

`ND-10188E-PART1` through `PART4` mount cleanly:

```
PART1: FOCUS-DEF-ENG-E:PROG      FOCUS-ENVIRON-EN:FORM   FOCUS-ENVIRON-NO:FORM   UE-ERMSG-ENG-A01:ERR
PART2: FOCUS-COMPILE-E:PROG      FOCUS-CONVERT:PROG
PART3: FC-MAIN-1CODE:BRF   FC-MAIN-1DATA:BRF   FC-MVTM-1CODE:BRF   FC-MVTM-1DATA:BRF
       VTM-R-D:BRF   VTM-DATA-D:BRF   VTM-CPOS-D:BRF   VTM-CPAR-D:BRF
       PLANC-1BANK-E:BRF
       FOCUS-1B-N:BUIL   FOCUS-1B-R:BUIL   MFOCUS-1B-N:BUIL   MFOCUS-1B-R:BUIL   FOCUS-TPS:BUIL
       VTM-1B-ARRAY-D-C:BRF   DDBTABLES-D-C:VTM
PART4: FC-MAIN-2CODE:BRF   FC-MAIN-2DATA:BRF   FC-MVTM-2CODE:BRF   FC-MVTM-2DATA:BRF
       VTM-2B-R-D:BRF   VTM-2B-DATA-D:BRF   VTM-2B-CPOS-D:BRF   VTM-2B-CPAR-D:BRF
       PLANC-2BANK-E:BRF
       FOCUS-2B-N:BUIL   FOCUS-2B-R:BUIL   MFOCUS-2B-N:BUIL   MFOCUS-2B-R:BUIL
       VTM-2B-ARRAY-D-C:BRF
```

**Three real findings not in the PI sheet:**

1. **FOCUS is itself built in PLANC** — both `PART3` and `PART4` bundle
   `PLANC-1BANK-E:BRF`/`PLANC-2BANK-E:BRF`, the PLANC runtime, as a build dependency.
2. **FOCUS carries its own VTM bridge**, `FC-MVTM-1CODE:BRF`/`FC-MVTM-2CODE:BRF` (`+DATA`
   counterparts) — the same "compiled bridge module spliced into the runtime" pattern already
   documented for COBOL's `VTM-BRIDGE-*:MODE` scripts on `ND-10176H00`, but here shipped
   pre-compiled rather than as a patch script.
3. **FOCUS ships its own private VTM terminal-table set** (`VTM-1B-ARRAY-D-C:BRF`,
   `DDBTABLES-D-C:VTM`) rather than depending purely on the system-wide `DDBTABLES:VTM` — matching
   the "load the compounded tables together with the application itself" option documented in
   [VTM-TERMINAL-INTERFACES.md §3](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md#3-building-and-extending-terminal-tables--the-real-vtm-compound-procedure).

`FOCUS-COMPILE-E:PROG` (Part 2) is a real form/picture compiler, name suggesting it plays the same
role as PLANC-SCREEN-H's `PLANC-GEN-A00:PROG` — turning a form definition into something the
runtime library loads — but its input/output file format has not been decoded.
`FOCUS-1B-N`/`FOCUS-1B-R`/`MFOCUS-1B-N`/`MFOCUS-1B-R` (`:BUIL` files, likely build/link scripts)
were not opened; `N`/`R` most likely mean non-reentrant/reentrant per the PI sheet's own
terminology, not confirmed against file content.

## Installation procedure — NOT established

No PD sheet located. The `:BUIL` files are plausibly build scripts but were not opened/decoded to
confirm an install sequence.

## Documentation
- Product Information (PI-sheet): [../../Product-Info/ND-10188-A2-EN.md](../../Product-Info/ND-10188-A2-EN.md)
- Program Description (PD-sheet): not located
- Manual: `ND-60.137` FOCUS Screen Handling System — not present in this repo

## Provenance & open items
- Source: four real floppy images in the archive
  (`nd-10188-e-d1` through `-d4`), confirmed by catalog listing.
- **TODO:** `FOCUS-DEF-ENG-E:PROG`, `FOCUS-COMPILE-E:PROG`, `FOCUS-CONVERT:PROG`, and the
  `:BUIL` files are compiled/unopened — the actual form-definition syntax and build sequence are
  not decoded, only inferred by analogy to PLANC-SCREEN-H and NSHS.
- **Open**: relationship to NSHS (`ND-10013`) — near-identical PI-sheet wording ("Maintenance
  Program" + "Handling Library", same caller-language set minus RPG II) but confirmed as a
  distinct product number. See [PLANC-VTM-UI-CATALOG.md §8](../../../Developer/Workflow/PLANC-VTM-UI-CATALOG.md#8-what-is-still-open-across-this-whole-catalog).

---
**Parent:** [../README.md](../README.md) (Software catalog)
