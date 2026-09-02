# ND-110/PCX

*An ND-110 on two PC expansion cards.*

|  |  |
|---|---|
| **Introduced** | 1987 |
| **Type** | Configuration of [ND-110](ND-110.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No - but it lives inside a PC |
| **Replaced** | - |
| **Replaced by** | - |
| **Survivors** | Not recorded |

## The short version

The ND-110/CX design rebuilt onto **two full-length ISA cards** so it could be
dropped into an IBM PC/AT compatible. It is the machine inside the
[Butterfly-110](../../Butterfly.md) workstation. The PC boots MS-DOS first, and DOS
then starts SINTRAN III/VSX on the ND-110/PCX beside it.

## Specification

| Item | Value |
|---|---|
| Form | Two full-length ISA cards |
| Based on | The same design as the ND-110/CX |
| Memory | 1 MB, of which **128 KB is given to the PC** so DOS sees the desirable 640 KB |
| Boots | MS-DOS 3.1 on the PC starts SINTRAN III/VSX on the PCX |
| Runs | DOS software and SINTRAN applications at the same time |

## What was new

- A full ND minicomputer as an expansion card in a personal computer.

## Sources

- **Primary**: ND-06.025.3 Butterfly-110 Technical Reference Manual and ND-06.028.1 Butterfly PC Technical Reference Manual exist in the mirror - **not yet imported**
- **Secondary**: [English Wikipedia](../../../sources/en-wikipedia-nord-100.md) - currently the only source

---

*Full context: [MACHINE-TIMELINE.md](../../../MACHINE-TIMELINE.md).*
