# Source: ndwiki article "NORD-50"

- **Live page**: <https://www.ndwiki.org/wiki/NORD-50>
- **Copy used here**: Wayback Machine snapshot, fetched 2026-08-27 by Ronny's
  request. The live ndwiki is behind an Anubis proof-of-work gate.

**Status: SECONDARY - and it SETTLES the biggest conflict in this research.**

Three secondary sources (the norsk-data.com timeline, and both Wikipedia
articles) call the NORD-50 a "second generation 32-bit supermini" of 1975. The
**primary** ND document `Reference-Manuals/10/NORD-10-Design-Goals.md` calls it a
special-purpose array processor introduced in 1973.

**This page agrees with the primary document, quotes it by name, and goes
further.** It is not a supermini in any ordinary sense:

> "The NORD-50 CPU is designed to be a fast floating point processor which is a
> **total slave to the NORD-10/S**."

> "NORD-50 does not have an I/O system or interrupt system by itself, all I/O is
> done through the NORD-10/S which submits jobs to NORD-50 for execution in
> batches. The NORD-10 acts as a system supervisor running the operating system
> SINTRAN III/VS and the NORD-50 monitor."

> "To the NORD-10 the NORD-50 looks like any else device and is controlled via
> standard IOX instructions."

So the three sources calling it a standalone supermini are repeating an error.
**Treat that claim as dead** unless a primary document revives it.

## The host-and-slave division of labour, spelled out

The NORD-10/S does: supervision and synchronisation of the NORD-50s; running
SINTRAN III; generating NORD-50 machine code via the NORD-50 assembler or the
FORTRAN compiler; and being the entire I/O system for the NORD-50s. The NORD-50s
"do not perform any tasks except for running the application programs". The
NORD-10/S activates a NORD-50, which then runs in parallel and interrupts the
NORD-10/S when it finishes.

This is the clearest statement anywhere in our sources of the pattern that runs
through all four ND generations - and Ronny confirmed it independently: NORD-5
needs a NORD-1, NORD-50 needs a NORD-10, and the ND-500 and ND-5000 need an
ND-100, ND-110 or ND-120.

## A product naming discovery

**"The NORD-50 CPU was used in the ND 1100/S, ND 1200/S, ND 1300/S and ND 1400/S
systems."** An ND-1x00/S system range appears in no other source we hold. It
looks like the system-level product name for a NORD-10/S plus NORD-50
combination, which would mean the paired machines were sold under their own model
numbers.

---

## Verbatim extract

The NORD-50 computer system is a completely integrated system of NORD-10/S and NORD-50 CPUs. The I/O system, file system and operating system are common to both processors.

NORD-50 array processor

The NORD-50, which was introduced in 1973, is a special purpose compute unit which performs convolution vector element sum operations, scaling, multiplexing and demultiplexing operations. The NORD-50 array processor will be used in seismic surveying and performs more than one million floating multiplications plus more than one million floating point additions in one second.[1]

The NORD-50 CPU was used in the ND 1100/S, ND 1200/S, ND 1300/S and ND 1400/S systems.

The NORD-50 CPU is designed to be a fast floating point processor which is a total slave to the NORD-10/S.[2]

The Norsk Data document NORD-10 Design Goals [3] describes the NORD-50 as the 'NORD-50 array processor'), a special purpose compute unit with the following features:

- Convolution vector element sum operations

- Scaling

- Multiplexing

- Demultiplexing

- Performs more than one milling floating point multiplications plus more than one million floating point additions in one second

NORD-50 does not have an I/O system or interrupt system by itself, all I/O is done through the NORD-10/S which submits jobs to NORD-50 for execution in batches. The NORD-10 acts as a system supervisor running the operating system SINTRAN III/VS and the NORD-50 monitor.

In a NORD-50 computer system, the NORD-10/S has the following functions:

- Supervision and synchronization of the NORD-50s in the system.

- Running the operating system, SINTRAN III.

- Generating NORD-50 executable machine code via NORD-50 assembler or FORTRAN compiler.

- Being the I/O system for the NORD-50s performing I/O transfer to/from the NORD-50 memory.

The NORD-50s in the system perform execution of instructions with data supplied through the NORD-10/S I/O system from byte-oriented devices such as terminals, general DMA devices such as disks and magnetic tapes, the universal DMA interface, and special high-speed DMA channels connected directly to ports in the multiport memory system.

The system architecture is such that NORD-50s can have their private memories including a high-speed static memory in addition to the multiport memory system.

The NORD-50s themselves do not perform any tasks except for running the application programs.

The slave processor NORD-50 is activated by the NORD-10/S and NORD-50 executes programs in parallel with the activities in NORD-10/S. The NORD-10/S is interrupted when the NORD-50 comes to an end of its tasks.

To the NORD-10 the NORD-50 looks like any else device and is controlled via standard IOX instructions.

### Contents

- 1 CPU

- 2 Address space

- 3 Circuit Boards

- 4 Operators panel

- 5 Performance

- 6 Remaining machines

- 6.1 Lost machines

- 7 Related

- 8 References and sources

### CPU

The NORD-50 CPU has a word length of 32 bits.

There are a number of registers in the NORD-50 CPU.[4]

- 64 general data registers with register 0 always containing zero.

- The 64 general registers can be combined in pairs as 32 64 bit floating point registers.

- The lower 16 registers can be used as base or index registers for addressing.

- There are 65 instructions in the NORD-50 CPU.

### Address space

The NORD-50 has a 4 megabytes physical address space but the maximum memory it can be equipped with was 1 Mbyte (1976).

### Circuit Boards

(Main article?: NORD-50 boards)

Main registers and arithmetic, communication registers NORD-10/NORD-50, memory address and data lines, line drivers for external arithmetic are organized on three different boards, each handling four bits:

- 1501 Address Arithmetic

- 1502 Register

- 1503 Arithmetic Buffer

The 32 bit CPU uses eight of each board, making a total of 24 boards located in the middle crate (crate B). The top crate (crate A) contains hardware for multiplication and division, while the bottom crate contains arithmetic circuitry and NORD-10 bus connectors (crate C).

The timing and control section of the CPU uses eight different boards:

- 1500 NORD-50 I/O Control

- 1504 NORD-50 Controller

- 1505 Register Address

- 1506 Cycle Counter

- 1507 Arithmetic Control

- 1508 Chip Select

- 1510 Instruction Control

- 1519 Timing Control

### Operators panel

The front panel.

The operators panel on the NORD-50 is simpler than the panel on the NORD-10. It is more of a status display than a operators panel. It contains?:

- Power lock switch (with a key) and a start switch

- 16 lamps showing bit 0-15 of the active address block.

- Parity check, showing parity errors

- 4 status lamps

- Parity error

- Instruction read

- Indirect reference

- Data reference

- 4-byte lamps showing which byte triggered the parity error

- 2 control switches

- Display PC

- Display dataref

- 8 status lamps

- Memory examine

- Memory deposit

- Simulate instruction

- Simulate data

- Status break

- External stop

- Stop

- Run

The operators panel is connected directly to the NORD-50 I/O CONTROL board 1500 in position B32.

### Performance

Whetstone results [5]

ROW| 
MWIPS | 
MWIPS double precision | 
Language | 
Date
 | 

ROW| 
0.531 | 
0.451 | 
Fortran | 
1975
 | 

### Remaining machines

This is the first shot at a list of remaining machines. Also machines that are known to be lost will be listed to make it easier to locate remaining ones. We don't have any production numbers yet.

- NORD-50 with unknown serial is in the collection of Troeim, in Telemuseums storage in Fetsund (2016 inventory).

- NORD-50 with unknown serial is part of the collection of Datormuseum.se[6]

### Lost machines

- NORD-50 snr 11 was bought by Boliden in Sweden. Later donated to Forskarf?reningen Umeaa Naturvetare, also known as FUN. The machine was scrapped shortly before the society was disbanded. Only a few manuals remain.

### Related

NORD-50 (command)

NORD-50 monitor

### References and sources

- ^ Norsk Data library, Software Nord-10 Design Goals (TSS-02)

- ^ 30.001.01A NORD-10/NORD-50, Operator's Guide

- ^ sintran.com NORD-10 design goals

- ^ Norsk Data Document ND-05.004 NORD-50 CPU HARDWARE MANUAL I 

- ^ Benchmark History and Results

- ^ The collection of Datormuseum.se
