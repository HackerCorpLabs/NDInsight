## Page 1

# ND-100/CE Computer System

## Features

- Single board CPU utilizing state-of-the-art bit slicer technology. CPU board contains also a real-time clock, a current loop interface and a power fail detect and restart mechanism
- Over 80% more powerful (average) than the standard ND-100 for COBOL programs, over 60% more powerful for FORTRAN programs
- 20 position rack, upgradable by 18 positions

## Hardware

- 16 bit parallel microprogrammed processor
- Bit, byte, single-, double-, triple-word and register instructions
- Fixed and floating point instructions
- BCD instructions
- Subroutine call instructions
- 8 memory addressing modes
- Instruction prefetch

---

[Photo: ND-100/CE Computer System]

---

## Page 2

# Product Description

## Introduction

The ND-100/CE is a 16 bit general purpose single board computer. The ND-100/CE makes full use of the latest advances in hardware technology. The maximum physical address space is 32 Mbyte. The Memory Management System offers an efficient paging system including extensive memory protection through a Page Protect System and a Ring Protect System.

## Instruction Set

The ND-100/CE has a comprehensive instruction set which includes bit, byte, double word, triple word, subroutine call and BCD instructions. Integer arithmetic operations include single precision memory-to-register operations.

The 48 bit Floating Point Instructions (add, subtract, multiply and divide), use a 32 bit mantissa and a 16 bit exponent (2 bits for sign of exponent and mantissa). The optional 32 bit Floating Point Instructions use a 23 bit mantissa and a 9 bit exponent.

For efficient system control, specially tailored privileged instructions, such as loading and storing of the complete central register block and interprogram level read/write operations, are included.

The ND-100/CE is microprogrammed, and all instruction execution is in firmware using a 2K by 64 bit Read Only Memory – ROM.

## Features

- 150 ns internal CPU cycle time
- Hardware paging and memory protection system
- 128 Kbyte data and 128 Kbyte instruction space for each user
- Up to 32 Mb physical memory
- Standard Error Checking and Correcting memory system
- 64 or 128 Kbyte memory modules: 2 bits – 16 data + 6 error checking bits
- 2 Kbyte CACHE memory for increased memory access speed
- 16 level priority interrupt system – each level with a set of the 8 central registers
- 5 μs context switching time
- 2048 vectored interrupts
- Bootstrap loading in firmware
- Built-in diagnostics in firmware

## Software

- SINTRAN III/VSE Operating System
  - Simultaneous multilingual time-sharing, real-time, communications and local remote batch processing
- Compilers include FORTRAN, COBOL, BASIC, RPG II, SIMULA, PASCAL and PLANC
- ND File System
- ISAM Index Sequential File System
- SIBAS Data Base System
- SORT/MERGE
- NOTIS Word Processing System
- PED full page program editor
- ND-NET communications system for ND computers
- RJE emulators and terminal concentrators for most mainframes

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

The ND-100/CE has a 16 level priority interrupt system. A complete set of all the central registers is assigned to each level. Context switching is reduced to the selection of the working set of central registers. The time required for this operation is 5 μs.

All program levels may be activated by software. In addition, each of the levels 10, 11, 12, and 13 may be activated by 512 vectored I/O interrupts. An IDENT instruction is used to identify the interrupting device. Program level 14 is used by the Internal Interrupt System, which monitors error conditions or traps in the CPU. Program level 15 may only have one I/O interrupt source.

Program level 15 is not used by standard ND equipment or software, but is available for users who need urgent access to the CPU.

## CACHE Memory

The 2 Kb high speed CACHE memory reduces the average memory access time significantly. The CACHE holds the most actual data and instructions to be processed.

### CACHE Memory Architecture

The CACHE Memory is organized as a 1K by 31 bit lookup table. A word in CACHE is identified with the main memory word, of which it is a copy, and by its main memory physical address – the physical page number.

The CACHE Memory is homogeneous, i.e. the CACHE Memory does not discriminate between data words, instructions or indirect address stored in main memory.

### CACHE Inhibit Area

The CACHE Memory System contains two limit registers which define a contiguous area in memory which will not be copied into CACHE when accessed.

The inhibit area features are intended for use on memory areas which are operated upon by high frequency DMA transfers and/or parallel processors.

## Main Memory

Maximum physical memory size is 32 Mbytes. 22 bits are used to represent each 16 bit data word (i.e. 6 ECC bits).

- A single bit error occurring in a word will be corrected and the error recorded
- All double bit errors and most multiple bit errors will be reported to the Internal Interrupt System which interrupts the CPU

---

## Page 3

# Memory Management System

The Memory Management System includes two major subsystems:

- ● The Paging System
- ● The Memory Protection System

The Paging System maps a 16 bit virtual address into a 24 bit physical address, extending the physical address space from 128 Kbytes to 32 Mbytes. Four page index tables of 64 words each, located in high-speed registers, reduce paging overhead to practically zero. Data and instruction pages may be allocated anywhere in memory without restriction. The page size is 1024 words.

The Memory Protection System may be divided into two subsystems:

- ● The Page Protection System
- ● The Ring Protection System

The Page Protection System may protect each page from read, write or instruction fetch accesses or any combination of these.

The Ring Protection System places each page on one of four priority rings. A page of memory that is placed on one specific ring may not be accessed by a program that resides in a page on a ring of lower priority. This system is used to protect system programs from user programs, the Operating System from its subsystems, and the system kernel from the Operating System.

# The Input/Output System

## General

The ND-100/CE uses the advanced high speed ND-100 bus to provide communication between programmed input/output devices, DMA controllers, memory modules and the CPU. The ND-100 bus supports a physical address space of 32 Mbytes for DMA devices.

The ND-100 bus is controlled by the Bus Controller Driver which is an integrated part of the CPU.

## Programmed Input/Output – PIO

Program controlled input/output operates via the A-register, which implies that each word of input/output has to be programmed via this register. The PIO interfaces are always controlled by the CPU.

## Direct Memory Access – DMA

A Direct Memory Access – DMA – channel is used to obtain high transfer rates to and from main memory. Processing in the CPU and DMA transfers may thus take place simultaneously. The DMA controllers transfer information to main memory via the ND-100 bus on a cycle steal basis. More than one DMA device may be active at the same time, sharing the total bandwidth of the ND-100 bus.

## Bootstrap Loading

Bootstrap loading is under microprogram control and makes available the following facilities:

- ● Binary load, from character oriented devices, i.e., from Floppy Disk or communication line
- ● System loading from block oriented devices, i.e., disk

# PHYSICAL SPECIFICATIONS

The ND-100/CE is delivered in a tall cabinet (11 module cabinet). It includes power supply, rack-mounted floppy, 20 positions rack, operator's panel and display panel.

The rack has 20 positions, of which 2 are occupied by the ND-100/CE processor and its Memory Management Systems.

In standard ND-100/CE systems, one further position is occupied by the floppy disk controller and 4 terminal interfaces, and 2 are used by the disk controller. The remaining positions in the rack may be used for I/O and memory boards.

## Expansion

If the rack becomes full, the ND-3304 expansion system (including expander modules, rack, internal wiring) may be used to add another 18 positions.

# TECHNICAL SPECIFICATIONS

| Specification         | Value                        |
|-----------------------|------------------------------|
| Cabinet height        | 1.69 m                       |
| Cabinet depth         | 0.91 m                       |
| Cabinet width         | 0.60 m                       |
| Full weight (average) | 130–200 kg                   |
| Power                 | 1600 Watt max. + 15–25 Watt  |
|                       | 230 V A/C                    |
|                       | 50 Hz ± 1%                   |
| Operating temperature | + 10°C – + 35°C              |
|                       | Maximum temperature gradient |
|                       | per hour: 3°C                |
| Start Peak Current    | 70 ampere – 30 milliseconds  |
| Main Fuse             | 16 ampere «G» characteristic |

# DOCUMENTATION

- ND-100 Reference Manual ............ ND-06.014
- ND-100 I/O System .................. ND-06.016
- ND-100 Functional Description ...... ND-06.015

---

## Page 4

```
# Contact Information

| Location              | Telephone            | Telex                           |
|-----------------------|----------------------|---------------------------------|
| **Bergen**            | tel. 05-292090       |                                 |
| **Sandnes**           | tel. 04-89554        |                                 |
| **Tromsø**            | tel. 083-77546       |                                 |
| **Stockholm**         | tel. 087-29590, ttx. 13528 nordata s |                   |
| **Gothenburg**        | tel. 031-29590       |                                 |
| **Malmo**             | tel. 040-755        |                                 |
| **Copenhagen**        | tel. 02-19550, ttx. 37725 nd dk |                      |
| **Wiesbaden**         | tel. 06124-7054, ttx. 418670 norddata       |           |
| **Versoix/Vaud**      | tel. 050-471168, ttx. 33563 nordata fera   |            |
| **Paris**             | tel. 01-43224660, ttx. 204159 nd paris |                 |
| **Lyon**              | tel. 07-837317      |                                 |
| **Newbury (Berkshire)** | tel. 0635-31445, ttx. 848919 norsk d |                |
| **Boston**            | tel. 061-237-7943, ttx. 921705 norsk wel    |           |

# COMTEC

| Location              | Telephone            | Telex                           |
|-----------------------|----------------------|---------------------------------|
| **Trondheim**         | tel. 075-16520, ttx. 55580 comtec n |                   |
| **Stockholm (Upplands Väsby)** | tel. 066-07550, ttx. 13528 nordata s |        |
| **Stockholm (Sollent)** | tel. 08-78555, ttx. 13756 wocom s |                   |
| **Odense**            | tel. 09-51404, ttx. 596590 comtec dk |                 |
| **Ballerup/Copenhagen** | tel. 02-85050      |                                 |
| **Dusseldorf**        | tel. 0211-608538, ttx. 858727 comt d |                 |

# Location

Jernkoveien 20  
Boks 4 Lindeberg gård  
Oslo 10  
Tel: 02-390030  
Tlx: 18661 nd n  

NOTE: NORSK DATA reserves the right to change specifications without given notice!
```

---

