## Page 1

# ND 540 32-Bit Computer Systems

## INTRODUCTION

The ND 540 systems are each based upon the ND 500 32 bit Central Processor.

The full 32 bit architecture of the ND 500 CPU accommodates exceedingly large programs and data areas. It has a very efficient instruction repertoire, including high speed floating point operations on both single and double precision operands. An advanced 32 Kilobyte Cache Memory System together with a Prefetch Processor handle the fetching of data from the multiport memory system and the instruction pipelining. Together, these features make the ND 540 one of the most powerful and advanced 32 bit super-minis available.

The basic ND 540 Computer Systems consist of an ND 500 CPU, an ND 100 CPU, the SINTRAN III/VSE-500E multi-user multi-mode operating system, which performs input/output, job scheduling and resource allocation, a floppy disc unit, a disc system, a system console and a memory of 3/4 Megabyte.

## FEATURES

- **High Execution Speed**  
  The basic cycle time of 200 ns executes the majority of the ND 500's machine instructions, providing a formidable processing power.

- **Cache Memory System**  
  The ND 540's 32 Kilobyte Cache Memory System employs a prefetch mechanism in accessing data from main memory. Each memory access is a full 4 byte word. There are two independent but identical cache systems, one for instructions and one for data.

```
[Photo: Tall computer cabinet with visible branding and components]
```

---

## Page 2

# Prefetch

A Prefetch Processor handles the pre-decoding and assembling of instructions in the pipeline, as well as the initiation of data fetch cycles for memory reference instructions. The instruction and data pipelines are kept full to ensure minimum idle time and hence maximum processor speed.

# Floating Point Arithmetic Performance

Arithmetic instructions are performed by hardware on parallel arrays. This results in extremely fast execution times. For example, double precision (64 bit operands) addition and subtraction use only 400 nanoseconds, multiplication 500 ns and division 2900 ns.

# Accuracy

The hardware arithmetic provides 32 bit single precision floating point operations with a 23 bit mantissa and a 9 bit exponent. For 64 bit operations, the mantissa is 55 bits long.

# Advanced Instruction Repertoire

Instructions are byte-oriented and tailored for high-level program execution efficiency, such as FORTRAN DO-loops and COBOL string handling.

# Large Program Size

The ND 500's memory management system allows each user an address space of up to 4.3 Gigabytes for programs.

# Large Data Space

In addition to the 4.3 Gigabytes available for program, each user may use up to 4.3 Gigabytes for data. This means that large files may be accessed as if they were arrays, thus allowing the hardware memory management system to access data, rather than the more resource consuming file system.

# Multiprogramming

Through the use of its efficient virtual memory system and its SINTRAN III/VSE-500E operating system, the ND 500 CPU may be shared by many programs. Context switching routines are implemented in the microcode.

# Multiport Memory System

The main memory system is of the multiport type. This allows sharing and direct access for the ND 500 CPU, the ND 100 CPU and DMA devices such as disc and magnetic tape and high performance communication links (MEGALINK, HDLC).

# SINTRAN III/VSE-500E Operating System

This is Norsk Data's interactive, terminal oriented operating system. It allows many programs written in many languages to be executed simultaneously. Real Time tasks, local and remote batch jobs and other forms for data communication may also be run concurrently with the timesharing users.

# Software

- Each ND 540 system is supplied together with:

  - SINTRAN III/VSE-500 Operating System
  - Including ND 500 monitor and link-loader
  - FORTRAN for ND 500
  - FORTRAN for ND 100
  - Accounting System
  - ND Backup System
  - Test and Verification programs
  - SINTRAN III Utility programs
  - Subsystem Package

## Other Software

- **Communications**
  - ND-NET for communication between ND systems
  - X.25 Packet and Link Level for PSS, TRANSPAC, DATAPAC, DATEX-P and other public networks
  - X.21 for accessing circuit switched networks e.g. the Nordic Public Data Network

- **Remote Job Entry Terminal Emulators and Terminal Concentrators**
  - IBM HASP-II
  - IBM 2780/3780
  - IBM 3270
  - HONEYWELL GRTS-II
  - HONEYWELL VIP-II
  - CDC 200 User Terminal - Multidrop
  - UNIVAC DCT-2000
  - UNIVAC NTR
  - UNIVAC UTS-400

- **Compilers**
  - PASCAL
  - SIMULA
  - COBOL
  - BASIC (for ND 100)
  - RPG-II (for ND 100)

- **Databases**
  - SIBAS Codasyl Database System
  - ISAM Indexed Sequential File Access System

- **Editors, Word Processing, Information Processing**
  - PED Fullscreen Program Editor
  - NOTIS-W Word Processor software
  - NOTIS-IR Information Storage and Retrieval System
  - ACCESS Query-by-example database query and update system
  - FOCUS-1 Screen Handling System

# ND 540 Systems

(Disc sizes quoted are space available after formatting)

- **ND 5400 System**
  - Supplied with a 30 MB (ND 585) disc. Two 15 MB directories, 1 removable and 1 fixed.

- **ND 5401 System**
  - Supplied with a 60 MB (ND 586) disc. Four 15 MB directories, 1 removable and 3 fixed.

- **ND 5402 System**
  - Supplied with a 90 MB (ND 587) disc. Six 15 MB directories, 1 removable and 5 fixed.

- **ND 5403 System**
  - Supplied with a 75 MB (ND 514) disc. 1.75 MB removable directory. In addition 5403 systems must have a magnetic-tape unit or at least one additional disc, ND 514, ND 572 or ND 574. If ND 574 (288 MB) is used, it is handled as three 75 MB directories, i.e. its capacity becomes 225 MB.

- **ND 5404 System**
  - Supplied with a 150 MB (ND 572) disc. Two 75 MB fixed directories. In addition, ND 5404 systems must have a magnetic-tape unit or at least one additional ND 514 (75 MB) disc.

---

## Page 3

# Basic Configurations

The ND 500 and ND 100 CPUs and their I/O system are housed in a single cabinet.

The systems include a disc, with controller and interface, a floppy disc unit, with controller and interface, a console hardcopy terminal, with interface and the operating system SINTRAN III/VSE-500E.

There are 6 free slots in the basic system for peripheral controllers and interfaces, i.e. terminal ports, magnetic tape, communications ports etc. Any ND 100 peripherals may be attached.

For example, the ND 272 Terminal Interface contains 8 serial ports, each single port may be either 20 mA current loop or RS 232-C. The ND 271 provides 4 such ports. The ND 557 Magnetic Tape Controller may handle up to two magnetic tape formatters for 800 bpi, 1600 bpi and 800/1600 bpi Pertec Tape units. The ND 730 HDLC DMA interface may be used for communication between ND systems, as may the ND 734 MEGALINK interface.

The memory supplied with a basic ND 540 system is 3/4 Megabytes. 1/4 Megabyte is local for the ND 100 CPU and the operating system. All memory above the first 1/4 Megabyte is common to both the ND 500 and the ND 100 CPU's and is contained in the Multiport IV memory system.

# Upgrading

## Memory

The maximum memory size for single cabinet ND 540 systems is 2 1/4 Megabytes. Up to this limit, memory may be expanded by simply adding modules. The increment for memory expansion is 1/4 MB.

If memory size is to be increased to more than 2 1/4 Megabytes, an ND 5001 Expansion Cabinet and an ND 5002 Memory Expansion System for 7 Megabytes may be added. This allows the system a total of 9 1/4 Megabytes.

Another ND 5002 Memory Expansion System may be added to this cabinet, giving a maximum memory of 16 1/4 Megabytes.

### Input/Output Slots

Additional I/O Slots may be added by installing an ND 5001 Expansion Cabinet and an ND 5003 I/O Expansion System. The number of slots is increased by 18.

# ND Numbers for Upgrading

| System                         | Details                                               |
|-------------------------------|-------------------------------------------------------|
| **ND 5001** Expansion Cabinet | This may contain ND 5002 and ND 5003 systems, maximum 2 in one cabinet. (ND 500 Cabinet including power supply.) |
| **ND 5002**                   | Memory Expansion System for maximum 7 Megabytes memory. (MPM IV rack with 2 banks, ports, controllers, cables etc.) |
| **ND 5003**                   | I/O Expansion System for 18 additional I/O cards. (I/O Rack, Bus expander cards, cables etc.) |

# Technical Specifications

The following dimensions are for the CPU cabinet and its contents only, i.e. do not include peripherals except the floppy disc unit.

| Specification           | Measurement           |
|-------------------------|-----------------------|
| Number of Cabinets      | 1                     |
| Cabinet height          | 1.69 metres           |
| Cabinet depth           | 0.91 metres           |
| Cabinet width           | 0.60 metres           |
| Gross weight            | 180-250 kg            |
| Power Consumption       | 3500 Watt max         |
|                         | 230 Volts A/C, 50 Hertz ± 1% |
| Operating Temperature   | + 10 °C to + 35 °C    |

# Documentation

- **ND-500 Central Processing Unit**
  - Datasheet: 060-C1-6000-0481
- **ND-500 Reference Manual**
  - ND-05.009

---

## Page 4

```
ND                ND
Norsk Data        COMTEC

Bergen, tel. 05-202420, tkn. 55550 comtec n  
Sandnes, tel. 064-26554  
Trondheim, tel. 075-16520, tkn. 55550 comtec n  
Tromsø, tel. 046-76654  
Stockholm, tel. 08/96-4600, tkn. 15255 nordata s  
Stockholm (Upplands Väsby), tel. 08/96-36100, tkn. 15255 nordata s
Stockholm (Solna), tel. 08/728382, tkn. 13705 swecom s  
Odense, tel. 09-15440, tkn. 99600 comtec dk  
Glostrup, tel. 03-961670  
Ballerup (Copenhagen), tel. 02-675001  
Malmø, tel. 040-785170  
Düsseldorf, tel. 0211-665838, tkn. 8887277 comt d  

Completeren, tel. 02-325856, tkn. 37275 nod dk  
Wiesbaden, tel. 0611-24541, tkn. 412720 noda n  
Ferray-Voltron, tel. 0595-57816, tkn. 385863 nordata ferray  
Paris, tel. 01-60742330, tkn. 0110 nd park  
Avon, tel. 07-397147  
Newbury, Berkshire, tel. 0635-31465, tkn. 948919 norsk d  
Boston, tel. 617-237-5945, tkn. 921740 nd works well   
                   

Jerkvicveien 20  
Boks 4 Linderberg gård  
Oslo 10  
Tel.: 02-390030  
Tlx.: 18661 nd n  

Jerkvicveien 20  
Boks 4 Linderberg gård  
Oslo 10  
Tel.: 02-390030  
Tlx.: 18661 nd n  

NOTE: NORSK DATA reserves the right to change specifications without notice  
```

---

