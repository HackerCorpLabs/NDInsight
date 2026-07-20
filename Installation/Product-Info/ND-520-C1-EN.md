## Page 1

# ND 520 32-Bit Computer Systems

## INTRODUCTION

The ND 520 systems are each based upon the ND 500 32 bit Central Processor.

The full 32 bit architecture of the ND 500 CPU accommodates extremely large programs and data areas. It has an efficient instruction repertoire, including high-speed floating point operations on both single and double precision operands. These factors make the ND 520 the most powerful of the «mini» super-minis, its performance in fact surpassing that of most competitors' top-line super-minis.

The basic ND 520 Computer Systems consist of an ND 500 CPU, an ND 100 CPU, the SINTRAN III/VSE-500E multi-user multi-mode operating system, which performs input/output, job scheduling and resource allocation, a floppy disc unit, a disc system, a system console, and a memory of 3/4 Megabyte.

## FEATURES

- **High Execution Speed**
  The basic cycle time of 200 ns executes the majority of the ND 500's machine instructions, providing a formidable processing power.

- **Floating Point Arithmetic Performance**
  Arithmetic instructions are performed by hardware on parallel arrays. This results in extremely fast execution times. For example, double precision (64 bit operands) addition and subtraction use only 400 nanoseconds, multiplication 500 ns and division 2900 ns.

- **Accuracy**
  The hardware arithmetic provides 32 bit single precision floating point operations with a 23 bit mantissa and a 9 bit exponent. For 64 bit operations, the mantissa is 55 bits long.

---

```
[Photo: ND Computer System]
```

520-CI-6000-0182

---

## Page 2

# ND 520 Systems

- **Advanced instruction repertoire**  
  Instructions are byte-oriented and tailored for high-level program execution efficiency, such as FORTRAN DO-loops and COBOL string handling.

- **Large Program Size**  
  The ND 500's memory management system allows each user an address space of up to 4.3 Gigabytes for programs.

- **Large Data Space**  
  In addition to the 4.3 Gigabytes available for programs, each user may use up to 4.3 Gigabytes for data. This allows arrays to be accessed as if they were primary memory, minimizing the need for resource-consuming file systems.

- **Multiprogramming**  
  The memory system supports efficient virtual memory, allowing the ND 500 CPU to be shared by many programs.

- **Multiport Memory System**  
  Direct access is enabled for the ND 500 CPU, ND 100 CPU, and DMA devices.

- **SINTRAN III/VSE-500E Operating System**  
  It supports multiple programming languages simultaneously, with real-time, local, and remote batch processing.

## Software

- Each ND 520 system is supplied with:

  - SINTRAN III/VSE-500 Operating System
    - including ND 500 monitor and link-loader
  - FORTRAN for ND 500
  - FORTRAN for ND 100
  - Accounting System
  - ND Backup System
  - Test and Verification programs
  - SINTRAN III Utility programs
  - Subsystem Package

### Other Software

- **Communications**  
  ND-NET for communication between ND systems  
  X.25 Packet and Link Level for PSS, TRANSPAC, DATAPAC, DATEX-P and other public networks  
  X.21 for accessing circuit switched networks  

- **Remote Job Entry Terminal emulators and Terminal Concentrators**  
  IBM HASP-II  
  IBM 2780/3780  
  IBM 3270  
  HONEYWELL GRTS-II  
  HONEYWELL VIP-II  

| **Compilers** | **Databases** |
|---------------|---------------|
| PASCAL | SIBAS Codasyl Database System |
| SIMULA | ISAM Indexed Sequential File Access System |
| COBOL |  |
| BASIC (for ND 100) |  |
| RPG-II (for ND 100) |  |

| **Editors, Word Processing, Information Processing** |
|-----------------------------------------------------|
| PED Fullscreen Program Editor                        |
| NOTIS-WP Word Processor software                     |
| NOTIS-IR Information Storage and Retrieval System    |
| ACCESS Query-by-example database query and update system |
| FOCUS-1 Screen Handling System                       |

## ND 520 Systems

(Disc sizes quoted are space available after formatting)

- **ND 5200 System**  
  Supplied with a 30 MB (ND 585) disc. Two 15 MB directories, 1 removable and 1 fixed.

- **ND 5201 System**  
  Supplied with a 60 MB (ND 586) disc. Four 15 MB directories, 1 removable and 3 fixed.

- **ND 5202 System**  
  Supplied with a 90 MB (ND 587) disc. Six 15 MB directories, 1 removable and 5 fixed.

- **ND 5203 System**  
  Supplied with a 75 MB (ND 514) disc. 1 75 MB removable directory. Requires additional disc or magnetic-tape unit for extended capacity.

- **ND 5204 System**  
  Supplied with a 150 MB (ND 572) disc. Two 75 MB fixed directories. Requires an additional disc or magnetic-tape unit.

- **ND 5205 System**  
  Supplied with a 288 MB (ND 574) disc. 1 288 MB removable directory. Requires a magnetic-tape unit or an additional disc for extended capacity.

## Basic Configurations

The ND 500 and ND 100 CPUs and their I/O systems are housed in a single cabinet, including:

- A disc, with controller and interface
- A floppy disc unit, console and interface
- Hardcopy terminal, console hardware terminal
- System SINTRAN III/VSE-500E

---

## Page 3

# Upgrading

## Memory

The maximum memory size for single cabinet ND 520 systems is 2 1/4 Megabytes. Up to this limit, memory may be expanded by simply adding modules. The increment for memory expansion is 1/4 MB.

If memory size is to be increased to more than 2 1/4 Megabytes, an ND 5001 Expansion Cabinet and an ND 5002 Memory Expansion System for 7 Megabytes may be added. This allows a total of 9 1/4 Megabytes. Another ND 5002 Memory Expansion System may be added to this cabinet, giving a maximum memory of 16 1/4 Megabytes.

## Upgrading to ND 540x Systems

This is a simple field upgrade resulting in an ND 540x system which has a CPU performance of nearly double that of the ND 520 system.

## Input/Output Slots

Additional I/O Slots may be added by installing an ND 5001 Expansion Cabinet and an ND 5003 I/O Expansion System. The number of slots is increased by 18.

# ND Numbers for Upgrading

| Model  | Description                                                           |
|--------|-----------------------------------------------------------------------|
| ND 062 | Upgrading of an ND 520 system to an ND 540x system. (32 KBytes cache memory) |
| ND 5001 | Expansion Cabinet. This may contain ND 5002 and ND 5003 systems, maximum 2 in one cabinet. (ND 500 Cabinet including power supply.) |
| ND 5002 | Memory Expansion System for maximum 7 Megabytes memory. (MPM IV rack with 2 banks, ports, controllers, cables etc.) |
| ND 5003 | I/O Expansion System for 18 additional I/O cards. (I/O Rack, Bus expander cards, cables etc.) |

# Technical Specifications

The following dimensions are for the CPU cabinet and its contents only, i.e. do not include peripherals except the floppy disc unit.

| Specification          | Details                     |
|------------------------|-----------------------------|
| Number of Cabinets     | 1                           |
| Cabinet height         | 1.69 metres                 |
| Cabinet depth          | 0.91 metres                 |
| Cabinet width          | 0.60 metres                 |
| Gross weight           | 180-250 kg                  |
| Power Consumption      | 3500 Watt max               |
|                        | 230 Volts A/C, 50 Hertz ± 1%|
| Operating Temperature  | + 10°C to + 35°C            |

# Documentation

- ND-500 Central Processing Unit
  - Datasheet: 060-C1-6000-0481
- ND-500 Reference Manual
  - ND-05.009

---

## Page 4

# Contact Information

## Norsk Data

```
ND               ND                  ND
Norsk Data       COMTEC
                                                                               
Boks 4 Linderud gård  Jernkroken 20                                           
Oslo 10                                                                        
Tel.: 02-309030                                                                
Tlx.: 18664 nd n                                                               
```

- **Bergen**, tel: 05-232020
- **Sandnes**, tel: 065-56544
- **Tromsø**, tel: 063-75754
- **Stockholm**, tel: 700-416500, tlx: 15255 nordata s
- **Gothenburg**, tel: 031-876667
- **Malmö**, tel: 040-96570

### Copenhagen

- tel: 02-52-6655, tlx: 37725 nd dk

### Wenham

- tel: 021-27461, tlx: 418763 nodna n

### Ferney-Voltaire

- tel: 065.40-0158, tlx: 38955 nordata fermv

### Paris

- tel: 45-587310, tlx: 21018 nd parts

### Lyon

- tel: 87-437441

### Newbury (Berkshire)

- tel: 0635-31465, tlx: 849819 norsk g

### Boston

- tel: 617-237-3955, tlx: 721470 norsk well

## Additional Contacts

- **Trondheim**, tel: 075-16260, tlx: 55580 comtec n
- **Stockholm Upplands Väsby**, tel: 760-94110, tlx: 15255 nordata s
- **Stockholm (Solna)**, tel: 7857785, tlx: 13706 swecom s
- **Odense**, tel: 09-15740, tlx: 20580 comtec dk
- **Ballerup (Copenhagen)**, tel: 02-675550
- **Düsseldorf**, tel: 0211-664386, tlx: 8587727 comt d

**Note:** NORSK DATA reserves the right to change specifications without notice.

---

