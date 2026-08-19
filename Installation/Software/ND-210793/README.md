# ND-210793 — NOTIS-BG for ND-500 (English)

> Status: IN-PROGRESS — real floppy decoded, real VTM-based install confirmed

| Field | Value |
|-------|-------|
| ND article number | `ND-210793` |
| Product name | NOTIS-BG for ND-500 (Business Graphics), English version |
| Functional category | Office — NOTIS suite |
| CPU target | ND-500 |
| Related products | Same `DDBTABLES:VTM` + `VTM-COMPOUND:PROG` pattern documented in [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md) |

## What is known — real floppy, decoded

Floppy `210793C01-EN-02D` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts
cleanly:

```
DDBTABLES-E-E04:VTM        VTM terminal-type configuration file
DDB078:VTM                 a second, smaller VTM table
VTM-COMPOUND:PROG           real VTM-COMPOUND installer/table-builder — confirms the pattern already
                            documented generically in VTM-TERMINAL-INTERFACES.md §3 with a real
                            in-the-wild product using it
BG-PRINTERS-C01:SYMB       printer configuration source
BG-EX01-EN-C01:CDBG        real example chart file 1 (binary format, not decoded)
BG-EX02-EN-C01:DABG        real example chart-data file (binary format, not decoded)
BG-EX03-EN-C01:CDBG        real example chart file 3 (binary format, not decoded)
```

`CDBG` and `DABG` are NOTIS-BG's own binary chart-definition and chart-data file types — opened
with `ndtool -x` and confirmed to be compiled/binary (not text), so their content was not
transcribed, only their existence and file-type extensions.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x`).
- **TODO:** `VTM-COMPOUND:PROG`'s interactive session was not run/decoded — see
  [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md) for the
  generic procedure this program follows.

---
**Parent:** [../README.md](../README.md) (Software catalog)
