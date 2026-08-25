# ND-10459 — VTM terminal tables (DEC VT100)

> Status: IN-PROGRESS — real floppy confirmed, no PD/PI sheet located

| Field | Value |
|-------|-------|
| ND article number | `ND-10459` (later article: `ND-210459`, per the archive's own sibling-product mapping) |
| Product name | VTM terminal tables, DEC VT100 (terminal type 6) |
| Functional category | System Utilities / VTM terminal-type data |
| CPU target | ND-100 / ND-500 |
| Related products | Same shape as [ND-211464](../ND-211464/README.md) (VT200) and [ND-10465](../ND-10465/README.md) (VT52) — a single-terminal-type VTM add-on, versus [ND-210455](../ND-210455/README.md)'s full standard set. See [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md) for the chapter this plugs into. |

## What is known — real floppy, confirmed

Floppy `ND-10459A` mounts cleanly:

```
DDB999-006-A-A:VTM        Version-A standard-file terminal descriptor, type 006 (DEC-VT100)
DDB006-A-A:VTM            per-type descriptor file, type 006, Version A
DDBARRAYS006-B-A:VTM      Version-B compounded array
VTM-1B-006-B-A:BRF        loadable array, ND-100 1-bank
VTM-2B-006-B-A:BRF        loadable array, ND-100 2-bank
VTM-500-006-B-A:NRF       loadable array, ND-500
```

Terminal type **6 = DEC-VT100 (80 column)** — matches the standard terminal-type table already
documented in [VTM-TERMINAL-INTERFACES.md §2](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md#the-standard-terminal-type-table-excerpt--table-18-of-the-manual).
File-naming shape is identical to [ND-211464's real PD-sheet procedure](../ND-211464/ND-211464A/README.md)
(`DDBnnn-<ver>:VTM` per-type file, `DDBARRAYS`/`DDBTABLES` compounded file, `VTM-1B`/`VTM-2B`/
`VTM-500` loadable arrays) — strong evidence the same `VTM-COMPOUND` install procedure applies
here, but this has **not been independently confirmed** for this specific floppy.

## Installation procedure

Not independently verified for this product. By direct analogy with
[ND-211464's real PD-sheet procedure](../ND-211464/ND-211464A/README.md): copy files to `SYSTEM`,
then use `VTM-COMPOUND` to add terminal type 6 to the system's compounded `DDBTABLES-n:VTM` (or
`DDB999:VTM` on Version A), and optionally generate a loadable array for linking directly into a
program. See [VTM-TERMINAL-INTERFACES.md §3](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md#3-building-and-extending-terminal-tables--the-real-vtm-compound-procedure)
for the verbatim command sequence (sourced from the sibling product, not this one).

## Documentation
- No PD sheet, no PI sheet located for this article number.

## Provenance & open items
- Source: real floppy image in the archive (`nd-10459-a-d1-f3c4b253`), confirmed by catalog
  listing.
- **TODO:** install not run live; no PD sheet found to verify against.

---
**Parent:** [../README.md](../README.md) (Software catalog)
