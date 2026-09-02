# 5000 Compact cabinet (COMSON)

*A Compact-sized box with two backplanes: a whole 1xx machine and an ND-5000 beside it.*

|  |  |
|---|---|
| **Used from** | 1987 |
| **Type** | Cabinet |
| **Family** | [Coprocessor systems](../systems/) |
| **Takes which CPU** | One [1xx CPU](../cpu/nd-1xx/) as I/O processor, **plus** [ND-5000 CPU](../cpu/nd-5xx/ND-5000.md) |
| **Coprocessor?** | **Yes** |

## The short version

From the outside it looks much like the [1xx Compact](Compact.md). Inside it is
a different machine: it carries **two backplanes** - one holding the 1xx CPU and
its I/O boards, exactly as a standalone Compact would, and a second holding the
ND-5000 CPU and its support boards.

ND's own drawings keep the two apart: `ND-100 COMPACT` (B2C6, B2C7) and
`ND-100/5000 COMPACT (COMSON)` (B2C8).

**COMSON is not a product code name.** It is the *name of a machine*. Norsk Data
ran two ND-5000 Compacts in Oslo as disc test machines and called them **COMSON-A**
and **COMSON-B** (serial numbers 21175 and 21176), with a third, plain **COMSON**,
listed as a test machine (21645). The drawing set was titled after those machines.

ND named its machines throughout - the same list holds KOLOSS, THOR, ODIN, BALDER,
MAGDA and Hylen, and the company's own operations machines were Obelix, Hades, Rosa
and Luring.

## Specification

| Item | Value |
|---|---|
| Format | Compact-sized, similar external appearance to the [1xx Compact](Compact.md) |
| Backplanes | **Two** - one ND-100 bus, one ND-5000 |
| I/O processor | One 1xx CPU board plus I/O boards on the ND-100 backplane |
| Compute CPU | ND-5000 CPU plus support boards on the second backplane |
| COMSON | **A machine name, not a product name** - ND's own test machines in Oslo |
| Drawings | ND Book 2: B2C8 (1987 and 1988 editions), and B4BC12 for the drive rack |
| Sold as | ND-5200 Compact, ND-5400 Compact, ND-5500 Compact, ND-5700 Compact |

## What goes in it

| Slot / backplane | Holds |
|---|---|
| ND-100 bus backplane | 1xx CPU, memory, I/O controllers |
| ND-5000 backplane | ND-5000 CPU boards and support boards |

## Notes

- **Do not confuse with the [1xx Compact](Compact.md)** - similar box, different insides.
- Earlier drafts of this document treated COMSON as a product code name. That was wrong; corrected 2026-08-28 from the archive's own systems list, where it appears in a column headed `Name` alongside other machine names.

## Sources

- **Primary**: cabinet drawings B2C8, B2C8-1987, B4BC12 in the mirror - **not yet imported**
- **Primary**: `Reference-Manuals/500/ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md` Table 1 (Compact configurations)

---

*Index: [../README.md](../../README.md). Full context: [../../MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
