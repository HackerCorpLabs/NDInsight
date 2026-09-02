# Large cabinet (1xx)

*The full-size 19-inch rack machine. The standard ND-100-line installation.*

|  |  |
|---|---|
| **Used from** | 1979 |
| **Type** | Cabinet |
| **Family** | [ND-1xx standalone](../cpu/nd-1xx/) |
| **Takes which CPU** | Any one [ND-1xx CPU board](../cpu/nd-1xx/) |
| **Coprocessor?** | No |

## The short version

The full-size floor-standing cabinet, and what most people picture as an
ND-100. It holds one CPU board, the memory management card beside it, memory and
I/O cards, with power supplies and fans. Because the CPU is a board, the same
cabinet carried an ND-100 in 1979 and an ND-120/CX in 1987 - you upgraded by
swapping a card, not by buying a machine.

## Specification

| Item | Value |
|---|---|
| Format | 19-inch rack, floor standing |
| Backplanes | **One** - the ND-100 bus |
| CPU | One 1xx CPU board |
| Memory management | Separate MMS card on the ND-100; combined onto the CPU board from the ND-110 on |
| Upgrade path | Swap the CPU board. ND explicitly offered this in the 1986 refresh |

## What goes in it

| Slot / backplane | Holds |
|---|---|
| Slot 1 | **Tracer** - hardware debugger (reserved) |
| Slot 2 | CPU board |
| Slot 3 | Memory management (MMS) - freed on the ND-110 and later |
| Remaining slots | Memory and I/O controllers |

## Notes

- The slot numbering is from the ND-100 era; the ND-110 put CPU and MMS on one board and plugged it into the MMS slot.
- Distinct from the [5xx large cabinet](Large-5xx.md), which is a different design.

## Sources

- **Primary**: `Reference-Manuals/ND-06.015.02 ND-100 Functional Description.md`
- **Secondary**: [ndwiki ND-100](../../sources/ndwiki-nd-100.md)

---

*Index: [../README.md](../../README.md). Full context: [../../MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
