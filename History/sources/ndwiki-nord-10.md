# Source: ndwiki article "NORD-10"

- **Live page**: <https://www.ndwiki.org/wiki/NORD-10>
- **Copy used here**: Wayback Machine snapshot, fetched 2026-08-27 by Ronny's
  request. The live ndwiki is behind an Anubis proof-of-work gate.

**Status: SECONDARY, and unusually careful.** The page warns you itself: "Much of
the following information is taken from a document written by Norsk Data
introducing the NORD-10. Some information, particularly about the memory system,
may not be accurate for the later NORD-10/S." That document is very likely
`Reference-Manuals/10/NORD-10-Design-Goals.md`, which we hold - so much of this
should be checked there rather than taken from here.

Introduced 1973; the NORD-10/S followed in 1975 with cache, paging and other
improvements.

## What it gives

- **A terminology warning worth keeping.** The CPU had "a microprocessor, which
  was defined in the manual as a portmanteau of **microcode processor** - not to be
  confused with the then nascent microprocessor". ND's own word, not the modern one.
- **24 printed circuit boards** for the CPU, with the last 8 rack positions used
  for program-controlled I/O - console Teletype, paper tape, card reader and
  punch, line printer, display, operator's panel and the real-time clock.
- **160 registers**, 128 of them available to programs - 8 on each of the 16
  program levels. Six general registers per level, one program counter, one status.
- Floating point standard. Five operand formats: bit, 8-bit byte, 16-bit word,
  32-bit double word, 48-bit floating point - the same 48-bit float as the NORD-1.
- **1K ROM** holding the microprogram, plus operator communication, bootstrap
  loaders and hardware test programs. Customer-specified instructions could be
  built into the microprogram.
- **Memory and paging**: 8K 16-bit modules, up to eight per 19-inch rack;
  physical space extendable past 64K to 256K words; the paging system translates a
  16-bit virtual address into an **18-bit physical** address, in 1K pages, with the
  four page index tables held in a 256-word very fast memory block, adding no
  appreciable delay.
- **Two independent protection systems**, and this is the interesting part. Each
  page could be protected against read, write or instruction fetch. On top of that
  sat **four rings, 0 to 3**, shown on the operator's panel: ring 0 USER, ring 1
  PROTECTED USER, ring 2 SYSTEM, ring 3 PROTECTED SYSTEM. A lower ring could never
  touch a higher ring's pages; rings 2 and 3 got the full instruction set, rings 0
  and 1 a limited one. Compilers and assemblers were expected to run in ring 1, the
  bulk of the operating system in ring 2 and the kernel in ring 3. A violation
  raised a hardware status interrupt on **program level 14** - the same level the
  NORD-1 used for protection violations.

---

## Verbatim extract

NORD-10 at CERN in March 1974.

NORD-10 was a medium-sized general-purpose 16-bit minicomputer designed for multilingual time-sharing applications and for real-time multiprogram systems, produced by Norsk Data. It was introduced in 1973. The later follow up model, NORD-10/S, introduced in 1975, introduced cache, paging, and other miscellaneous improvements.

The CPU had a microprocessor, which was defined in the manual as a portmanteau of "microcode processor" - not to be confused with the then nascent microprocessor. The CPU additionally contained instructions, operator communication, bootstrap loaders, and hardware test programs, that were implemented in a 1K ROM.

The microprocessor also allowed for customer specified instructions to be built in. NORD-10 had a memory management system with hardware paging extending the memory size from 64 to 256K 16-bit words and two independent protecting systems, one acting on each page and one on the mode of instructions. The interrupt system had 16 program levels in hardware, each with its own set of general-purpose registers. 

Note: Much of the following information is taken from a document written by Norsk Data introducing the NORD-10. Some information, particularly about the memory system, may not be accurate for the later NORD-10/S.

### Contents

- 1 The CPU

- 2 The memory

- 3 I/O system and Bus Architecture

- 4 The Interrupt System

- 5 System Software

- 6 Known remaining systems

- 7 Sources

### The CPU

The CPU consisted of a total 24 printed circuit boards. The last 8 positions in the rack were used for I/O devices operated by program control, such as the console Teletype, punched paper tape and card reader and punch, line printer, display, operator's panel, and the real time clock.

The NORD-10 had 160 registers, of which 128 were available to programs, 8 on each of the 16 program levels. 6 of those registers were general registers, one was the program counter, and the other contained status information. Floating point operations were standard. The instructions could operate on 5 different formats, a bit, an 8-bit byte, 16-bit words, 32-bit double words, and 48-bit floating point words.

### The memory

The memory system of the first NORD-10s were built up of 8K 16-bit modules housed in a special memory rack. One 19-inch rack could take up to eight 8K modules. It was possible to extend the NORD-10's physical address space beyond 64K up to a maximum of 256K 16-bit words. The paging system translated a 16-bit virtual address into an 18-bit physical address.

The hardware paging system made it possible for one user to write programs up to 64K (virtual memory), and only parts of the program to be present in physical memory at any time (using dynamic memory allocation). The paging system divided memory into 1K pages. The 4 page index tables were found in a 256 word extremely fast memory block. The calculation of a physical address resulted in no appreciable delay in the effective memory cycle time.

The NORD-10 had two independent protection systems. Each individual page could be protected against being read from, written into (type data or type instructions), or against reading of instructions. In addition, there was a system which divided the pages into four different categories, called rings. The rings had a priority from 0 to 3. A program on a lower ring was never allowed to access the pages on a higher ring. Programs which ran on rings 2 and 3 could use the whole NORD-10 instruction set, while programs on rings 0 and 1 only had a limited instruction set available. The different rings were displayed on the operator's panel. For example, ring 0 (USER) may have held a user program, while compilers and assemblers ran in ring 1 (PROTECTED USER). The bulk of the operating system could run in ring 2 (SYSTEM), and the kernel in ring 3 (PROTECTED SYSTEM). If one attempted to execute privileged instructions in ring 0 or 1, or attempts were made to accessed a protected page, a hardware status interrupt would automatically be generated on program level 14 indicating the error.

### I/O system and Bus Architecture

The NORD-10 was equipped with a common bus system for all external devices. The bus system was divided into groups, and a great deal of effort had been made to ensure that no device would be able to jam the bus system in the case of malfunction. Each group had its own controller which in addition to functioning as an electronic switch for the bus system, could also change priority for the whole group. All interconnections between the cards were done with multilayer printed circuit backwiring boards, and all I/O interface had the same standard form. The system could therefore be extended or reconfigured by plugging in new or shifting around the existing interface cards. The position of the device interface in the card rack determined the interrupt priority of the device. In DMA transfers the device would send a "REQUEST". The CPU would answer with a "GRANT" signal, which would be passed from device to device until it came to the device which initiated the "REQUEST", and transfer to the memory could take place. When two or more devices issued a DMA request simultaneously the device closest to the CPU thus had the highest priority. One memory cycle later the next DMA along the chain would be allowed to send data, and so on, until a higher priority device again sent a REQUEST. This meant that many DMA devices could use the same bus system at the full data transfer rate. It was not necessary to establish a "master-slave" connection. The transfer was one 16-bit word/850 nanoseconds, or 2.2MB/s.

The printed backplane of the I/O bus was modular in groups of 8 interface slots. Interfaces for mass storages as disk, drum, magtape, etc., were built with one interface card to be plugged at the appropriate place in the bus system, the remaining control cards (6-7) were placed in one of the backplane modules.

### The Interrupt System

The NORD-10 had a multiprogram system with 16 priority program levels. Each program level had its own set of registers, including a program counter and a status word. The levels running could be shown on the front panel by pressing the button ACTIVE LEVELS. Levels 0 through 9 were used for programs. Internal hardware status interrupts were assigned to level 14, whilst level 15 was reserved for extremely fast user interrupts (this was colloquially called the "Synchrotron level", since the only program ever to have used it was the program controlling the synchrotron at CERN)

Levels 10, 11, 12, and 13 were reserved for external devices. Each device had its own unique identification vector. In all 2048 such vectors were available. The "IDENT" instruction determined which device was giving an interrupt. The identification of an interrupt took 1.7 microseconds, including the time taken to enable and disable the registers.

### System Software

The NORD-10 was delivered with a time-shared system, NORD-TSS, and a real-time multitasking operating system, SINTRAN III. The minimum configuration for SINTRAN III included a standard NORD-10 with 8K of core. 

With NORD-TSS all users could simultaneously run any of the systems FORTRAN IV, BASIC, MAC Assembler, NODAL, NORD PL, or QED.

### Known remaining systems

Two adjacent NORD-10/S in the collection of NTNU.

There are several known NORD-10 and NORD-10/S system known to remain, many of which are in near-operational condition, and several are in the care of NODAF. Restorations of systems are planned in both Oslo by NODAF[1] and Trondheim by Norwegian University of Science and Technology (NTNU).

Its predecessor was the NORD-1 and its successor the NORD-100.

### Sources

- ^ NODAF:NORD-10.5 progress log

- "Inside NORD-10", by Cand. Real. Jan Aske Boerresen for A/S Norsk Data-Elektronikk, ND-nytt

- This article was originally a copy of the English Wikipedia article NORD-10 in 17th Oct 2008.
