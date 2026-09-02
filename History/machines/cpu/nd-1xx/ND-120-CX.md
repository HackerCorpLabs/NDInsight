# ND-120/CX

*The ND-100 line reduced to a single chip, code-named Delilah.*

|  |  |
|---|---|
| **Introduced** | 1985-87 **[?]** |
| **Type** | Base machine |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | ND-110/CX |
| **Replaced by** | ND-125/CX (~1994) |
| **Survivors** | Not recorded |

## The short version

A complete redesign of the ND-110 onto one large LSI gate array called
**Delilah**. About 1.9 times the speed of an ND-110/CX, with minor microcode
changes and no change at all to the instruction set. It was nearly sold as the
**ND-1000**, to mark the technology change the way the ND-500 to ND-5000 rename
did. The name is a joke: the 32-bit machine was **Samson**, so the 16-bit chip that
fed it was Delilah - and ND's internal documentation for the chip carries a drawing
of a grinning woman with hair in her clenched fist.

## Specification

| Item | Value |
|---|---|
| CPU | One LSI gate array (Delilah) |
| Speed | About 1.9 times an ND-110/CX |
| Instruction set | Unchanged. Minor microcode changes only |
| On-board memory | 2 MB to 6 MB |
| Second role | I/O processor for the ND-5700 and ND-5800 |
| Nearly named | ND-1000 |

## What was new

- The whole CPU on one chip.
- The last real step in the 16-bit line's evolution.

## Configurations

| Machine | What differs |
|---|---|
| [ND-125/CX](ND-125-CX.md) | Same CPU, faster and larger on-board memory |

## Sources

- **Primary**: ND-350002-N1 "ND120 CPU, MM & M" (1991) exists in the mirror - **not yet imported**; `Reference-Manuals/500/ND-05.020.01` for the I/O processor role
- **Secondary**: [ndwiki ND-100](../../../sources/ndwiki-nd-100.md), [English Wikipedia](../../../sources/en-wikipedia-nord-100.md)

---

*Full context: [MACHINE-TIMELINE.md](../../../MACHINE-TIMELINE.md).*
