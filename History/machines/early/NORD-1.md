# NORD-1

*Norsk Data's first machine, and the first computer built commercially in Norway.*

|  |  |
|---|---|
| **Introduced** | 1968 |
| **Type** | Base machine |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | - |
| **Replaced by** | NORD-10 (1973) |
| **Survivors** | At least 11, including serials 2, 4, 5, 37 and 47 |

## The short version

A 16-bit minicomputer built from the then brand-new 7400-series TTL chips, at a
time when most machines were still made of discrete transistors. It had hardware
floating point as standard and, from 1969, optional hardware paging - so a program
could be written for more memory than the machine physically had. Roughly 142 were
built. The first one sold went into a ship's anti-collision system.

## Specification

| Item | Value |
|---|---|
| CPU | Two racks, 64 cards, over 1,300 TTL chips. No microcode - control logic is hardwired |
| Memory | Ferrite core, 4K to 64K words. Fastest usable cycle 1 microsecond |
| Address space | 64K words. Optional paging gives 64K virtual in 256-word pages |
| Control store | None - hardwired |
| Cache | None |
| Interrupts | 16 levels (0-15). Registers saved and restored through core: max 38 cycles, 45 microseconds |
| Floating point | Standard. 48-bit, in three memory words. Accumulator is T:A:D |
| Registers | 8 general (R, A, D, T, L, X, B, P) plus 6 status flip-flops |
| Operating system | SINTRAN II (real-time) or NORD-TSS (timesharing), from 1971 |
| Physical | Cabinet with power supply, memory, CPU, I/O, power panel and fan box |

## What was new

- One of the first computers built from integrated circuits rather than transistors.
- Hardware floating point as standard, which was unusual for a minicomputer.
- Hardware demand paging, documented by February 1970 - a real virtual memory system.
- Memory protection with a 16-bit register, one flag per memory block.
- Two CPUs could share one memory block.

## Configurations

| Machine | What differs |
|---|---|
| [NORD-2B](NORD-2B.md) | Simplified and cheaper version of the same design |
| [NORD-5](NORD-5.md) | 32-bit compute module that attaches to a NORD-1 |

## Sources

- **Primary**: NORD-1 Reference Manual (Feb 1970); ND-01.004.01 Hardware Manual
  vols I and II; ND-01.005.01 Connectors - all in `Reference-Manuals/1/`
- **Secondary**: [ndwiki NORD-1](../../sources/ndwiki-nord-1.md),
  [CPU detail](../../sources/ndwiki-nord-1-cpu-detail.md),
  [boards](../../sources/ndwiki-nord-1-boards.md),
  [serial 47](../../sources/ndwiki-nord-1-serial-47.md)
- **Long form**: [../NORD-1.md](../../NORD-1.md) - the full chapter, with open questions

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
