# NORD-12

*A compact NORD-10 for less money. Same instruction set, same microcode, smaller box.*

|  |  |
|---|---|
| **Introduced** | 1974-75 **[?]** |
| **Type** | Configuration of [NORD-10](NORD-10.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | - |
| **Replaced by** | ND-100 (1979) |
| **Survivors** | Serial 85, Telemuseum (marked 8004) |

## The short version

Program-compatible with the NORD-10 because it uses the same instruction set
**and the same microcode ROM**. What it gives up is memory: 64K words maximum, and
no memory management option. What it keeps is the whole peripheral range - every
NORD-10 interface works on it, and moving up to a NORD-10 meant carrying your
peripherals across.

## Specification

| Item | Value |
|---|---|
| CPU | Microprogrammed; 490-500 ns per microinstruction (NORD-10: 300 ns) |
| Memory | MOS, 4K to 64K words in 4K steps |
| Memory width | 18 bits with the parity option - 16 data plus one parity bit per byte |
| Address space | 64K words. **No memory management option** - this is the main limit versus the NORD-10 |
| Control store | 65 ns ROM, shared with the NORD-10. Up to 1024 customer instructions in an added PROM |
| Interrupts | 16 program levels with their own registers; context switch 2.0 microseconds. 2048 vectored I/O interrupts |
| Floating point | Standard hardware. Results are truncated (the NORD-10 rounds) |
| Options | Power fail with auto restart, and 30 minutes of memory standby power |
| Operating system | SINTRAN III/12 |

## What was new

- Same ROM as the NORD-10, which is what guarantees compatibility.
- MOS memory instead of core.
- Peripherals and interfaces interchangeable with the NORD-10 - an upgrade path, not a dead end.

## Configurations

| Machine | What differs |
|---|---|
| [NORD-42](NORD-42.md) | The same machine, badged for Norcontrol |

## Sources

- **Primary**: the NORD-12 Reference Manual is **not held** - it exists in the mirror, see [../OCR-WANTED.md](../../OCR-WANTED.md)
- **Secondary**: [ndwiki NORD-12](../../sources/ndwiki-nord-12.md), largely transcribed from that manual

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
