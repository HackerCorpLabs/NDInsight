# NORD-10

*The machine that made Norsk Data. Microprogrammed, paged, and built for real-time work.*

|  |  |
|---|---|
| **Introduced** | 1973 |
| **Type** | Base machine |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | NORD-1 |
| **Replaced by** | ND-100 (1979) |
| **Survivors** | Several, including one at CERN in 1974 photographs |

## The short version

The NORD-1's successor and ND's international breakthrough - this is the
machine CERN bought. Where the NORD-1's control logic was wired in, the NORD-10's
instructions are carried out by a microprogram in a small memory, which made the
design cleaner and let ND add instructions without changing hardware. It gave
each of its 16 interrupt levels a complete set of registers, so switching between
tasks became a change of selection rather than a copy through memory.

## Specification

| Item | Value |
|---|---|
| CPU | 24 printed circuit boards; last 8 rack slots for program-controlled I/O |
| Registers | 160 total - 8 general plus 2 microcode scratch on each of 16 levels; 128 visible to programs |
| Memory | 8K-word modules, up to eight per 19-inch rack |
| Address space | 64K words virtual, extended to 256K words physical by paging |
| Paging | 1024-word pages; 16-bit virtual mapped to 18-bit physical; four page tables of 64 words in fast registers |
| Protection | Per-page read/write/fetch bits **and** four rings (0 user, 1 compilers, 2 operating system, 3 kernel). Both must pass |
| Control store | 1K x 32 bits ROM. Four microinstruction types: ARITHMETIC, INTERBLOCK, JUMP, LOOP |
| Also in the ROM | Operator panel driver, MOPC operator communication, bootstrap loader, memory check |
| Interrupts | 16 levels, each with its own registers. Context switch 1.5 microseconds |
| Floating point | Standard, 48-bit. 32-bit format available as alternate microcode |
| Operating system | SINTRAN III, and NORD-TSS for timesharing |

## What was new

- Microprogrammed control - ND said the NORD-1's 'large and complicated Time Counter/Cycle Counter' was hard to structure and understand.
- Registers per interrupt level, so a context switch stops copying anything.
- Hardware paging with a four-ring protection model on top of per-page bits.
- Customers could have their own instructions microprogrammed in.

## Configurations

| Machine | What differs |
|---|---|
| [NORD-10/S](NORD-10-S.md) | Adds cache and improved paging (1975) |
| [NORD-12](NORD-12.md) | Compact, cheaper, same instruction set and ROM |
| [NORD-42](NORD-42.md) | OEM version of the NORD-12 for Norcontrol |
| [NORD-50](NORD-50.md) | 32-bit array processor that attaches to a NORD-10/S |

## Sources

- **Primary**: `Reference-Manuals/10/` - ND-06.008.01 Reference Manual, ND-06.010.01 Microprogram, NORD-10-Design-Goals
- **Secondary**: [ndwiki NORD-10](../../sources/ndwiki-nord-10.md)

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
