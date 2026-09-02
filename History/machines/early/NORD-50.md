# NORD-50

*A 32-bit array processor that is a total slave to a NORD-10/S.*

|  |  |
|---|---|
| **Introduced** | 1973 (completed 1975 **[?]**) |
| **Type** | **Coprocessor** |
| **Word length** | 32-bit |
| **Needs a host?** | **Yes - needs a NORD-10/S** |
| **Replaced** | NORD-5 |
| **Replaced by** | ND-500 (1981) |
| **Survivors** | None recorded |

## The short version

A special-purpose number cruncher for convolution, vector sums, scaling and
multiplexing - built for seismic surveying and scientific work. It has **no input
or output system and no interrupt system of its own**. The NORD-10/S runs the
operating system, compiles its programs, feeds it work in batches and does all its
I/O. To the NORD-10 it simply looks like a device, driven by ordinary IOX
instructions.

Several sources call it a general-purpose supermini of 1975. ND's own documents do
not: it is an array processor, introduced 1973.

## Specification

| Item | Value |
|---|---|
| Role | Attached compute unit. Runs application programs only - nothing else |
| Operations | Convolution and vector element sums, scaling, multiplexing, demultiplexing |
| Performance | Over one million floating multiplications plus over one million floating additions per second |
| I/O | **None.** All input and output goes through the NORD-10/S |
| Interrupts | **None of its own.** It interrupts the NORD-10/S when it finishes |
| Memory | Private memory including fast static memory, plus the shared multiport memory |
| Host link | IOX instructions. Registers are 32-bit, so each transfer takes **two** IOX instructions |
| Multi-unit | Several NORD-50s could run under one NORD-10/S |
| Sold in | ND 1100/S, ND 1200/S, ND 1300/S and ND 1400/S systems |

## What was new

- The host-and-slave split stated explicitly by ND for the first time.
- A debugging feature called **simulated memory**: a program on the NORD-10 acts as the NORD-50's memory controller, so the CPU can be tested with no memory attached at all.

## Sources

- **Primary**: `Reference-Manuals/10/ND-06.005.01 NORD-10 - NORD-50 Communication System.md` (Aug 1975); ND-60.116.01 Operator's Guide (not yet read); `NORD-10-Design-Goals.md`
- **Secondary**: [ndwiki NORD-50](../../sources/ndwiki-nord-50.md)
- **Not held**: five NORD-50 manuals sit in the mirror - see [../OCR-WANTED.md](../../OCR-WANTED.md)

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
