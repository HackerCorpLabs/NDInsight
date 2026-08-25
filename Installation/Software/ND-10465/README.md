# ND-10465 — VTM terminal tables (DEC VT52)

> Status: IN-PROGRESS — real floppy confirmed, no PD/PI sheet located

| Field | Value |
|-------|-------|
| ND article number | `ND-10465` |
| Product name | VTM terminal tables, DEC VT52 (terminal type 29) |
| Functional category | System Utilities / VTM terminal-type data |
| CPU target | ND-100 / ND-500 |
| Related products | Same shape as [ND-211464](../ND-211464/README.md) (VT200) and [ND-10459](../ND-10459/README.md) (VT100) — a single-terminal-type VTM add-on, versus [ND-210455](../ND-210455/README.md)'s full standard set. See [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md) for the chapter this plugs into. |

## What is known — real floppy, confirmed

Floppy `ND-10465A` mounts cleanly:

```
DDB999-029-A-A:VTM        Version-A standard-file terminal descriptor, type 029 (DEC-VT52)
DDB029-A-A:VTM            per-type descriptor file, type 029, Version A
DDBARRAYS029-B-A:VTM      Version-B compounded array
VTM-1B-029-B-A:BRF        loadable array, ND-100 1-bank
VTM-2B-029-B-A:BRF        loadable array, ND-100 2-bank
VTM-500-029-B-A:NRF       loadable array, ND-500
```

Terminal type **29 = DEC-VT52** — matches the standard terminal-type table already documented in
[VTM-TERMINAL-INTERFACES.md §2](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md#the-standard-terminal-type-table-excerpt--table-18-of-the-manual).
Identical file-naming shape to [ND-10459](../ND-10459/README.md) (its VT100 sibling) — same
generation, same six-file layout, just a different terminal type number substituted throughout.

## Installation procedure

Not independently verified for this product. By direct analogy with
[ND-211464's real PD-sheet procedure](../ND-211464/ND-211464A/README.md): copy files to `SYSTEM`,
then use `VTM-COMPOUND` to add terminal type 29 to the system's compounded `DDBTABLES-n:VTM` (or
`DDB999:VTM` on Version A). See
[VTM-TERMINAL-INTERFACES.md §3](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md#3-building-and-extending-terminal-tables--the-real-vtm-compound-procedure)
for the verbatim command sequence (sourced from the sibling product, not this one).

## Documentation
- No PD sheet, no PI sheet located for this article number.

## Provenance & open items
- Source: real floppy image in the archive (`nd-10465-a-d1-b9c9485f`), confirmed by catalog
  listing.
- **TODO:** install not run live; no PD sheet found to verify against.

---
**Parent:** [../README.md](../README.md) (Software catalog)
