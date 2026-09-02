# ND-100/CX

*An ND-100 with a bigger microcode chip, adding commercial and system instructions.*

|  |  |
|---|---|
| **Introduced** | 1982 |
| **Type** | Configuration of [ND-100](ND-100.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | ND-100/CE |
| **Replaced by** | ND-110/CX (1986) |
| **Survivors** | Not separately recorded |

## The short version

Not new hardware. The ND-100's instruction set lives in a microcode chip, and the
CX option is simply a larger one - 4K by 64 bits instead of 2K. That bigger chip
holds the improved commercial instructions from the earlier CE option plus four
additions the operating system wanted. Everything else about the machine is the
same.

## Specification

| Item | Value |
|---|---|
| CPU | ND-100 CPU, unchanged |
| Control store | **4K x 64 bits** microcode PROM (the base ND-100 has 2K x 64) |
| Added instructions | MOVEW (move a block of words, 1431xx), TSET (test and set, 140123), RDUS (read without using cache, 140127), and the segment-change instructions |
| Also includes | The CE option's decimal arithmetic and stack instructions |
| Everything else | As [ND-100](ND-100.md) |

## What was new

- The instruction set grew without any hardware change - a bigger PROM, nothing else.
- `TSET`, test-and-set, is the instruction you need to lock something safely when
  more than one thing is running.
- `RDUS` lets a program read memory while deliberately bypassing the cache.
- The segment-change instructions were added for SINTRAN III specifically.

## A note on a common error

English Wikipedia assigns MOVEW, TSET and RDUS to the **CE** option. That is
wrong. The ND-100 Reference Manual says: *"The CX-option consists of improved
CE-instructions (Commercial Extended) plus the following instructions (**CX
only**): MOVEW ... TSET ... RDUS ... segment-change instructions."* **[P]**

## Sources

- **Primary**: `Reference-Manuals/ND-06.014.2A EN ND-100 Reference Manual.md`;
  `Reference-Manuals/ND-06.015.02 ND-100 Functional Description.md`
- **Secondary**: [ndwiki ND-100](../../../sources/ndwiki-nd-100.md),
  [English Wikipedia](../../../sources/en-wikipedia-nord-100.md) - see the note above

---

*Full context: [MACHINE-TIMELINE.md](../../../MACHINE-TIMELINE.md).*
