# Butterfly-110

*A PC with a Norsk Data minicomputer inside it. The strangest machine ND built.*

|  |  |
|---|---|
| **Introduced** | 1987 |
| **Type** | Base machine (hybrid) |
| **Word length** | 16-bit ND plus 16-bit PC |
| **Needs a host?** | No, runs on its own |
| **Replaced** | - |
| **Replaced by** | - |
| **Survivors** | At least one known - a Butterfly TS in a private collection |

## The short version

An IBM PC/AT compatible made by **Ericsson**, with an
[ND-110/PCX](cpu/nd-1xx/ND-110-PCX.md) on two expansion cards inside it. The PC boots MS-DOS,
and MS-DOS then boots SINTRAN III/VSX on the ND cards. From then on the machine
runs DOS software and SINTRAN applications side by side, and can export the
SINTRAN side to terminal users over its serial ports. It existed largely so people
could keep running the NOTIS office software while also having a PC.

## Specification

| Item | Value |
|---|---|
| Host PC | IBM PC/AT compatible built by Ericsson. Intel 80286, 512 KB RAM, EGA graphics, floppy and hard disk |
| ND side | ND-110/PCX on two full-length ISA cards, 1 MB RAM |
| Memory trick | 128 KB of the ND card's memory is given to the PC, to make DOS's 640 KB |
| Boot order | MS-DOS 3.1 first; DOS then boots SINTRAN III/VSX on the cards |
| Runs | DOS and SINTRAN concurrently; SINTRAN exported to terminals on the serial ports |
| Documentation | ND-06.025.3 Technical Reference Manual, 548 pages; ND-06.028.1 PC Technical Reference Manual, 374 pages |

## What was new

- Two architectures in one box, running at the same time.
- A migration path: keep NOTIS and SINTRAN, gain a PC.

## Configurations

| Machine | What differs |
|---|---|
| [Butterfly Teamstation](Butterfly.md) | The same machine with terminals attached |
| Butterfly 10, 11, 12 | Low-end models with **no ND-110** at all - Windows and Norsk Data Desk Top Manager only |

## Sources

- **Primary**: ND-06.025.3 and ND-06.028.1 exist in the mirror - **not yet imported**, and would be the best source by far
- **Secondary**: [English Wikipedia](../sources/en-wikipedia-nord-100.md) - currently the only source

---

*Full context: [MACHINE-TIMELINE.md](../MACHINE-TIMELINE.md).*
