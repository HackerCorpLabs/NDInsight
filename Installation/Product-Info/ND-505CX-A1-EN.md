## Page 1

# ND 505/CX COMPUTER SYSTEM

for Commercial and Technical/Scientific Applications

## INTRODUCTION

The ND-505/CX system is based upon the ND-500/2 Central Processor. It has 29 bits addressing space available to the user. The architecture of the ND-500/2 CPU accommodates exceedingly large programs and data areas. A Prefetch Processor handles the fetching of data from the multiport memory system and the instruction pipelining. Together, these features make the ND-505/CX one of the most advanced supermini computer systems available.

The basic ND-505/CX Computer System consists of an ND-500/2 CPU, an ND-100 Front-End Processor, a Controller, a System Console and a Main Memory of 2 1/4 Megabyte. It also includes the SINTRAN III multituser, multimode operating system, which performs input/output, job scheduling and resource allocation.

## FEATURES

- **Processor Technology**  
  Implementation in FAST TTL and LSI logic.

- **BCD Arithmetic**  
  A full BCD Arithmetic and conversion processor gives high speed COBOL processing.

- **Advanced Instruction Repertoire**  
  Instructions are byte-oriented and tailored for high-level program execution efficiency, such as FORTRAN DO-loops and COBOL string-handling.

- **Large Program Size**  
  The ND-505/CX memory management system allows each user an address space of up to 1/4 Gigabytes for programs.

- **Large Data Space**  
  Each user may use up to 1/4 Gigabyte for data. This means that files may be accessed as if they were arrays, thus allowing the hardware memory management system to access data, rather than the more resource-consuming file system.

- **Multiprogramming**  
  Through the use of its efficient virtual memory system and its SINTRAN III Operating System, the ND-500/2 CPU may be shared by many programs. Context-switching routines are implemented in the microcode.

- **Multiport Memory System**  
  The main memory system is of the multiport type. This allows sharing and direct access for the ND-500/2 CPU, the ND-100 Front-End Processing Unit, and DMA devices such as disk and magnetic tape, and high performance communication links.

- **SINTRAN III Operating System**  
  This is Norsk Data's interactive, terminal-oriented operating system. It allows many programs written in many languages to be executed simultaneously. Real Time tasks, local and remote batch jobs and other types of data communication may also be run concurrently with the timesharing users.

## PRODUCT DESCRIPTION

### SOFTWARE

- Each ND-505/CX system is supplied with the following software:
  - SINTRAN III Operating System including ND-500 monitor, linkage-loader and symbolic debugger.
  - Accounting System
  - ND Backup System
  - Test and Verification programs
  - SINTRAN III Utility programs
  - Subsystem Package
  - ND Spooling System
  - Exception Handling
  - User Environment
  - Other Software Available:

    - **Communications**
      - COSMOS for communication between ND-systems
    
    - **Remote Job entry Terminal emulators and Terminal Concentrators.**
      - IBM HASP-II
      - CDC 200 User Terminal - Multidrop
      - IBM 2780/3780

```
  |          |          |
  | Software |          |  
  |----------|----------|
  | SINTRAN  | ND-500   |
  | III      | Monitor  |
```

---

## Page 2

# Technical Information

## System Features

### Computers
- UNIVAC DCT-2000
- IBM 3270
- UNIVAC NT 101
- HONEYWELL GRTS-II
- UNIVAC UTS-400
- HONEYWELL VIP-II

### Compilers, Programming Languages
- C
- SIMULA
- COBOL
- PASCAL
- FORTRAN ANSI 77

### Databases
- SIBAS Codasyl Database System
- ISAM Indexed Sequential File Access System

### Editors, Word Processing, Information Processing
- PED Fullscreen Program Editor
- NOTIS-WP Word Processor Software
- NOTIS-IR Information Storage and Retrieval System
- NOTIS-RG Report Generator
- NOTIS-CALC Spread Sheet System
- NOTIS-QL Query-by-example database query and update system
- FOCUS Screen Handling System
- NOTIS-BG Business Graphics System
- NOTIS-ID Electronic Mail

### Administrative Data Processing Tools
- FOCUS Screen Handling System
- UNIQUE Very High Level Appl. Specification Tool
- ABM Applications Building and Maintenance for COBOL/FORTRAN
- MERCUR Financial Modelling

## Basic Configuration

The ND-505/CX Computer System is housed in one cabinet. The system includes the ND-500/2 CPU, the Multiport V Memory System, the ND-100 Front-End Processor and the I/O system.

The system includes a disk controller and interface, a floppydisk unit with controller and interface, a console hardcopy terminal with interface, and the SINTRAN III Operating System. Any disk system from 70 MB and upwards may be added according to the configuration rules.

There are 12 free I/O slots in the basic system for peripheral controllers and interfaces, i.e., terminal ports, magnetic tape, communications ports, etc. Any ND-100 peripherals may be attached.

The memory supplied with a basic ND-505/CX system is 2-1/4 Megabytes. 1/4 Megabyte is local for the ND-100 Front-End Processing Unit and the operating system, with the remainder being common to both the ND-500/2 and the ND-100 Front-End Processing Units. The maximum shared memory size for the ND-505/CX system is 8 Megabytes (using 4 MB Modules).

## Technical Specifications

The following dimensions are for the CPU cabinet and its contents:

| Component                  | Dimension/Specification    |
|----------------------------|----------------------------|
| Cabinet height             | 1.69 metres                |
| Cabinet depth              | 0.91 metres                |
| Cabinet width              | 0.60 metres                |
| Gross weight               | ca. 180-250 kg             |
| Operating temperature      | -10°C to +35°C             |
| Power consumption          | 3500 Watt max              |
|                            | 230 Volts ac.              |
|                            | 50 Hertz ± 1%              |

## Documentation

| Document                              | Document Number |
|---------------------------------------|-----------------|
| ND-500 Reference Manual               | ND-05.009       |
| ND-500 Hardware Description           | ND-05.011       |
| ND-500 Micro Test Program Description | ND-30.013       |
| ND-500 Maintenance Manual             | ND-30.014       |
| ND-500 Loader/Monitor                 | ND-60.136       |

```ascii
                                 __
  ______ _____ _____ _____   O  |  | | 
 |  ____|  _  |  _  |  __ \ /_\ |  |_|
 | |__  | |_) | |_) | |__| / _ \| (_) |
 |_____|_____// \__/ |___/ |_| |_| | |

 CORPORATE
 HEADQUARTERS
 P.O. Box 65
 0rv Boks 0. 6 N-0601
 Newbury.  Tel: +44-63 3564

 WEST GERMANY
 Thomasstrasse 10-12
 8300 Bad Homnburg u.d.H.
 West Germany

 UNITED KINGDOM
 Norsk Data Ltd.
 Berkshire House
 London:  TeL+44-81 993-9964
 Edinburgh    Tel:+44-31 668-1561
```

---

