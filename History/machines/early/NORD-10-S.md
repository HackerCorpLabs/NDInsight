# NORD-10/S

*The NORD-10 with cache added. The mature 16-bit machine of the 1970s.*

|  |  |
|---|---|
| **Introduced** | 1975 |
| **Type** | Configuration of [NORD-10](NORD-10.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | NORD-10 |
| **Replaced by** | ND-100 (1979) |
| **Survivors** | Not separately recorded |

## The short version

A follow-up to the NORD-10 adding cache memory and improved paging. This is
the version that hosts the NORD-50 array processor, and the one most NORD-10-era
installations actually ran. Its context switch is faster than the base machine's -
1 microsecond against 1.5.

## Specification

| Item | Value |
|---|---|
| CPU | As NORD-10, plus cache |
| Memory | 1K to 256K words, read-only or read/write. Modules 8K x 18 bits at 300 ns, 32K x 18 bits at 300-350 ns |
| Memory width | 18 bits = 16 data + one parity bit per byte. 21-bit modules available for error correction |
| Address space | 128 Kbytes virtual, 512 Kbytes physical |
| Interrupts | 16 levels, 8 registers each. Context switch **1 microsecond** |
| I/O | 2048 priority vectored interrupts, plus 10 internal hardware status interrupts |
| Bootstrap | Loaders for mass storage and character devices; octal, binary and mass-storage load formats |
| Operating system | SINTRAN III; version H was the last to support the NORD-10 line **[?]** |

## What was new

- Cache memory, which the NORD-10 did not have.
- Context switch down from 1.5 to 1 microsecond.
- Improved paging.

## Sources

- **Primary**: `Reference-Manuals/10/ND-06.008.01 NORD-10-S Reference Manual.md`
- **Secondary**: [ndwiki NORD-10/S](../../sources/ndwiki-nord-10-s.md)

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
