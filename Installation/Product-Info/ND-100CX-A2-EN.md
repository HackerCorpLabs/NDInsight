## Page 1

# ND-100/CX Commercial Extended Computer System

## Features

- Single board CPU utilizing state-of-the-art bit slicer technology. CPU board contains also a real-time clock, a current loop interface and a power fail detect and restart mechanism.
- 2-3 times more multi-user throughput than ND-100, 40% more than ND-100/CE.
- 16 positions for I/O and memory expandable in steps of 18 modules.
- Up to 32 Mbytes physical memory.
- Up to 2,400 Mbytes disk storage.

## Hardware

- Software directed architecture.
  - Critical parts of operating system implemented in microcode.
  - Instructions for COBOL and FORTRAN.
  - Subroutine calls in microcode.
- Bit, byte, single-, double-, triple-word and register instructions.
- Fixed point.
- BCD.
- 16 bit microprogrammed processor.
  - 8 memory addressing modes.
  - Instruction Prefetch.
  - 150 ns CPU cycle time.
  - Hardware paging and memory protection.

```
[Photo: Computer system with monitors, CPU unit, and peripheral devices]
```

---

## Page 2

# Technical Specifications

- 128 Kbyte data space per user.
- 128 Kbyte program space per user.
- 128, 256, and 512 Kbyte ECC memory modules.
- 2 Kbyte cache memory system.
- 16 level priority interrupt system, each level with own central register set.
- 2048 vectored interrupts.

# Software

All ND Software including:

- SINTRAN III/VSE Operating System.
  - Simultaneous multilingual time-sharing, real-time, communications and local remote batch processing.
  - The development of the 100/CX CPU and its operating system was carried out in parallel. The result is that an ND-100/CX system running SINTRAN III/VSE can accept a considerably higher multi-user load than the ND-100 and the ND-100/CE computers.
- Compilers include FORTRAN, COBOL, BASIC, RPG II, SIMULA, PASCAL and PLANC.
- ND File System.
- ISAM Index Sequential File System.
- SIBAS Data Base System.
- SORT/MERGE.
- PED full page program editor.
- COSMOS communications system.
- RJE emulators and terminal concentrators for most mainframes.
- NOTIS-WP word processor.
- NOTIS-IR Information Storage and Retrieval System.
- ACCESS, «Query by Example» table handling database query and update system.

# Product Description

## Introduction

The ND-100/CX is a 16 bit general purpose computer. The ND-100/CX makes full use of the latest advances in hardware technology. The maximum physical address space is 32 Mbyte. The Memory Management System offers an efficient paging system including extensive memory protection through a Page Protect System and a Ring Protect System.

## Instruction Set

The ND-100/CX has a comprehensive instruction set which includes bit, byte, word, double word, triple word, subroutine call and BCD instructions. Integer arithmetic operations include single precision memory-to-register operations.

The ND-100/CX instruction set includes instructions for the execution of critical parts of the SINTRAN III/VSE Operating System, for example segment switching.

The 48 bit Floating Point Instructions (add, subtract, multiply and divide), use a 32 bit mantissa and a 16 bit exponent (2 bits for sign of exponent and mantissa). The optional 32 bit Floating Point Instructions use a 23 bit mantissa and a 9 bit exponent.

For efficient system control, specially tailored privileged instructions, such as loading and storing of the complete central register block and interprogram level read/write operations, are included.

The ND-100/CX is microprogrammed, and all instruction execution is in firmware using a 2K by 64 bit Read Only Memory - ROM.

## Addressing Modes

A variety of addressing modes may be used:

- Program counter relative addressing
- Indirect addressing
- Pre-indexed addressing
- Post-indexed addressing
- Combinations of these modes

## Register Block

The CPU has 16 program levels; each level has 8 registers. One accumulator with 2 extensions for handling double and triple words, an index register, a base register, a link register and a status register.

## The Interrupt System

The ND-100/CX has a 16 level priority interrupt system. A complete set of all the central registers is assigned to each level. Context switching is reduced to the selection of the working set of central registers. The time required for this operation is 5 μs.

All program levels may be activated by software. In addition, each of the levels 10, 11, 12, and 13 may be activated by 512 vectored I/O interrupts. An IDENT instruction is used to identify the interrupting device. Program level 14 is used by the Internal Interrupt System, which monitors error conditions or traps in the CPU. Program level 15 may only have one I/O interrupt source.

(Program level 15 is not used by standard ND equipment or software, but is available for users who need urgent access to the CPU.)

## CACHE Memory

The 2 Kb high speed CACHE memory reduces the average memory access time significantly. The CACHE holds the most actual data and instructions to be processed.

### CACHE Memory Architecture

The CACHE Memory is organized as a 1K by 32 bit look-up table. A word in CACHE is identified with the main memory word, of which it is a copy, and by its main memory physical address – the physical page number.

The Cache Memory is homogeneous, i.e. the CACHE Memory does not discriminate between data words, instructions or indirect addresses stored in main memory.

### CACHE Inhibit Area

The CACHE Memory System contains two limit registers which define contiguous area in memory which will not be copied into CACHE when accessed.

The inhibit area features are intended for use on memory areas which are operated upon by high frequency DMA transfers and/or parallel processors.

## Main Memory

Maximum physical memory size is 32 Mbytes. 22 bits are used to represent each 16 bit data word (i.e. 6 ECC bits).

- A single bit error occurring in a word will be corrected and the error recorded
- All double bit errors and most multiple bit errors will be reported to the Internal Interrupt System which interrupts the CPU

## Memory Management System

The Memory Management System includes two major subsystems:

- The Paging System
- The Memory Protection System

The Paging System maps a 16 bit virtual address into a 24 bit physical address, extending the physical address space from 128 Kbytes to 32 Mbytes. Four page index tables of 64 words each, located in high-speed registers, reduce paging overhead to practically zero. Data and instruction pages may be allocated anywhere in memory without restriction. The page size is 2048 words.

---

## Page 3

# Memory Protection System

The Memory Protection System may be divided into two subsystems:

- **The Page Protection System**
- **The Ring Protection System**

The Page Protection System may protect each page from read, write or instruction fetch accesses or any combination of these.

The Ring Protection System places each page on one of four priority rings. A page of memory that is placed on one specific ring may not be accessed by a program that resides in a page on a ring of lower priority. This system is used to protect operating system programs from user programs, the Operating System from its subsystems, and the system kernel from the Operating System.

# The Input/Output System

## General

The ND-100/CX uses the advanced high speed ND-100 bus to provide communication between programmed input/output devices, DMA controllers, memory modules and the CPU. The ND-100 bus supports a physical address space of 32 Mbytes for DMA devices.

The ND-100 bus is controlled by the Bus Controller Driver which is an integrated part of the CPU.

## Programmed Input/Output – PIO

Program controlled input/output operates via the A-register, which implies that each word of input/output has to be programmed via this register. The PIO interfaces are always controlled by the CPU.

## Direct Memory Access – DMA

A Direct Memory Access — DMA — channel is used to obtain high transfer rates to and from main memory. Processing in the CPU and DMA transfers may thus take place simultaneously. The DMA controllers transfer information to main memory via the ND-100 bus on a cycle steal basis. More than one DMA device may be active at the same time, sharing the total bandwidth of the ND-100 bus.

# Bootstrap Loading

Bootstrap loading is under microprogram control and makes available the following facilities:

- Binary load, from character oriented devices, i.e., from Floppy Disk or communication line
- System loading from block oriented devices, i.e., disk

# PHYSICAL SPECIFICATIONS

The ND-100/CX is delivered in a tall cabinet (11 module cabinet). It includes power supply, rack-mounted floppy, 20 positions rack, operator's panel and display panel.

The rack has 20 positions, of which 2 are occupied by the ND-100/CX processor and its Memory Management System.

In standard ND-100/CX systems, one further position is occupied by the floppy disk controller, and 2 are used by the disk controller. The remaining positions in the rack may be used for I/O and memory boards.

## Expansion

If the rack becomes full, the ND-3304 expansion system (including expander modules, rack, internal wiring) may be used to add another 18 positions.

# TECHNICAL SPECIFICATIONS

| Parameter               | Specification                           |
|-------------------------|-----------------------------------------|
| Cabinet height          | 1.69 m                                  |
| Cabinet depth           | 0.91 m                                  |
| Cabinet width           | 0.60 m                                  |
| Full weight (average)   | 130–200 kg                              |
| Power                   | 1600 Watt max. ±15–25 Watt              |
|                         | 230 V AC                                |
|                         | 50 Hz ±1%                               |
| Operating temperature   | +10°C – +35°C                           |
|                         | Maximum temperature gradient            |
|                         | per hour: 3°C                           |
| Start Peak Current      | 70 ampere ~30 milliseconds              |
| Main Fuse               | 16 ampere «G» characteristic            |

# DOCUMENTATION

- ND-100 Reference Manual ......................... ND-60.014
- ND-100 I/O System ............................... ND-06.016
- ND-100 Functional Description ................... ND-06.015

---

## Page 4

# Norsk Data

| Location     | Contact Details                                    |
|--------------|----------------------------------------------------|
| Oslo         | Tel.: 02-309030, Tlx: 18661 nd n                   |
| Bergen       | Tel.: 05-229009                                    |
| Sandnes      | Tel.: 04-876598                                    |
| Tromsø       | Tel.: 083-75776                                    |
| Stockholm    | Tel.: 0760-42000, Tlx: 15255 nordata s             |
| Gothenburg   | Tel.: 031-496500                                   |
| Malmø        | Tel.: 040-70510                                    |
| Copenhagen   | Tel.: 02-425505, Tlx. 37725 nd dk                  |
| Århus        | Tel.: 06-210051                                    |
| Wiesbaden    | Tel.: 0611-72441, Tlx. 4186730 noda n              |
| Firenze/Volterra | Tel.: 050-690576, Tlx. 382563 nordata fervu    |
| Paris        | Tel.: 1-602396, Tlx. 201100 nd parsi               |
| Lausanne     | Tel.: 021-320840, Tlx. 26218 nd ch                 |
| Lyon         | Tel.: 787 43717, T.837 5957                        |
| Newbury      | Tel.: 0635-374563, Tlx. 849819 norskd g            |
| Utrecht      | Tel.: 03408-88743                                  |
| Boston       | Tel.: (617) 237-7945, Tlx. 921740 norskd well      |

### Notes

- **Trondheim**: Tel. 075-16520, Tlx. 55580 comtc n
- **Stockholm**: Tel. 0760-840100, Tlx. 15255 nordata s
- **Odense**: Tel. 09-157440, Tlx. 59860 comtec dk
- **Düsseldorf**: Tel. 0211-666838, Tlx. 858727 comtc d

### Address
- **Oslo**: Olav Helsets vei 5, Boks 25 Bogerud, Oslo 6
- **Jerkivoeien 20**: Boks 4 Linderberg gård, Oslo 10

### Communication
- **Tel**: 02-309030
- **Tlx**: 18661 nd n
- **Telefax**: 02-309247

## Notice
NOTE: NORSK DATA reserves the right to change specifications without notice.

---

