# NORD-5

*A 32-bit compute engine that bolts onto a NORD-1. ND's first 32-bit machine.*

|  |  |
|---|---|
| **Introduced** | 1972 (development from 1970) |
| **Type** | **Coprocessor** |
| **Word length** | 32-bit |
| **Needs a host?** | **Yes - needs a NORD-1** |
| **Replaced** | - |
| **Replaced by** | NORD-50 (1973) |
| **Survivors** | At least one, Norwegian Telecommunication Museum |

## The short version

Not a computer you could buy on its own. The NORD-5 is a fast arithmetic
engine attached to a NORD-1, which runs the operating system, compiles the code
and does all the input and output for it. Heavy calculation jobs are handed over
while the host gets on with other work. It is often called the world's first
32-bit minicomputer, beating the VAX by six years - true in the sense that the
arithmetic is 32-bit, but it was an attached processor, not a standalone machine.

## Specification

| Item | Value |
|---|---|
| Role | Attached compute module; runs one program at a time |
| Memory | Its own core. Several NORD-5s could share a common memory pool |
| Floating add / subtract | 950 ns |
| Shifts and bit operations | 950 ns, regardless of shift count |
| Floating multiply / divide | 4 microseconds for 64-bit, or 950 ns with the optional fast multiplier **[?]** |
| Technology | Logic arrays for the floating and shift units; Schottky TTL in multiply and divide |
| Operating system | None of its own - the NORD-1 holds it |

## What was new

- ND's first 32-bit arithmetic, six years before the VAX.
- Arithmetic units connect asynchronously, so different speed grades were possible.
- First delivered to the Norwegian Meteorological Institute, March 1972.

## Sources

- **Primary**: `Reference-Manuals/Assembler_for_NORD-5_April_1972.md` (not yet read)
- **Secondary**: [ndwiki NORD-5](../../sources/ndwiki-nord-5.md), citing ND-NYTT No 5, September 1972
- **Note**: sources disagree on the 64-bit multiply time - see the source file

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
