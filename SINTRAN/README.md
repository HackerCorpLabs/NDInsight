# SINTRAN III Documentation

**Complete Documentation for SINTRAN III Operating System**

---

## Overview

This folder contains comprehensive documentation for the SINTRAN III/VS operating system used on NORD-10/ND-100 computer systems, including ND-500 multiprocessor integration.

### Purpose

SINTRAN III was a sophisticated real-time operating system providing:
- Real-time multitasking with priority-based scheduling  
- Demand paging virtual memory  
- Multi-level interrupt handling (16 levels)
- Multi-CPU support (ND-100 + ND-500)
- Device-independent I/O system

---

## Folder Structure

| Folder | Contents | Files |
|--------|----------|-------|
| [Devices/](Devices/) | Hardware device documentation (HDLC, SCSI) | 95+ files |
| [Emulator/](Emulator/) | C# emulator implementation guides | 5 files |
| [ND500/](ND500/) | ND-500 processor documentation | TBD |
| [NPL-SOURCE/](NPL-SOURCE/) | **SINTRAN III NPL source code & symbols** | **45 NPL + 7 symbol files** |
| [OS/](OS/) | Core operating system architecture (00-19) | 34 files |
| [Release-Documentation/](Release-Documentation/) | SINTRAN III release information (versions J-N) | 7 files |
| [SINTRAN Structures/](SINTRAN%20Structures/) | System structures and data analysis | 6 files |
| [TAD/](TAD/) | TAD protocol analysis | 7 files |

---

## Quick Start

### Understanding SINTRAN III

**Start here:**
1. [OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md](OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md) - System overview
2. [OS/01-BOOT-SEQUENCE.md](OS/01-BOOT-SEQUENCE.md) - How the system boots
3. [OS/02-QUEUE-STRUCTURES-DETAILED.md](OS/02-QUEUE-STRUCTURES-DETAILED.md) - Task scheduling

**For specific topics:**
- **Memory management** → [OS/04-MMU-CONTEXT-SWITCHING.md](OS/04-MMU-CONTEXT-SWITCHING.md), [OS/16-PAGE-FAULT-HANDLER.md](OS/16-PAGE-FAULT-HANDLER.md)
- **Interrupt handling** → [OS/13-INT14-HANDLER-DETAILED.md](OS/13-INT14-HANDLER-DETAILED.md)
- **Device drivers** → [OS/18-DEVICE-DRIVER-FRAMEWORK.md](OS/18-DEVICE-DRIVER-FRAMEWORK.md)
- **ND-500 integration** → [OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md)

### Building an Emulator

**Emulator developers:**
1. Read [Emulator/KERNEL-ACCESS-EMULATOR.md](Emulator/KERNEL-ACCESS-EMULATOR.md) for C# kernel access
2. Read [Emulator/ND500-INTEGRATION-GUIDE.md](Emulator/ND500-INTEGRATION-GUIDE.md) for ND-500 support
3. Use [Emulator/ND500-QUICK-REFERENCE.md](Emulator/ND500-QUICK-REFERENCE.md) as quick reference

### Hardware Specifics

**HDLC communication:**
- [Devices/HDLC/01-HDLC-Hardware-Reference.md](Devices/HDLC/01-HDLC-Hardware-Reference.md)
- [Devices/HDLC/Quick-Reference-Card.md](Devices/HDLC/Quick-Reference-Card.md)

**SCSI disk controllers:**
- [Devices/SCSI/SCSI-Master-Index.md](Devices/SCSI/SCSI-Master-Index.md)
- [Devices/SCSI/SCSI-C#-Implementation-Guide.md](Devices/SCSI/SCSI-C%23-Implementation-Guide.md)

---

## SINTRAN III Source Code

### NPL Source Code and Symbols

**Location:** [NPL-SOURCE/](NPL-SOURCE/)

This folder contains **actual SINTRAN III operating system source code** written in NPL (Norsk Data Programming Language), extracted from the s3vs-4.symb build job output.

#### What's Included

**NPL Source Files (45 files):**
- Kernel/Monitor code (MON60)
- SCSI disk drivers (IP-P2-SCSI-*.NPL)
- HDLC communication drivers (MP-P2-HDLC-DRIV.NPL)
- ND-500 interface code (CC-P2-N500.NPL, MP-P2-N500.NPL)
- Disk I/O subsystem (IP-P2-DISK-*.NPL, MP-P2-DISK-*.NPL)
- Segment administration (IP-P2-SEGADM.NPL)
- HASP protocol implementation (MP-P2-HASP-ETC.NPL)
- Terminal and communication handling (TP-P2-*.NPL)

**Symbol Tables (7 files, SINTRAN L07):**
- **FILSYS-SYMBOLS.SYMB.TXT** (61 KB) - File system symbols
- **N500-SYMBOLS.SYMB.TXT** (122 KB) - ND-500 interface symbols
- **XMSG-SYMBOL-LIST.SYMB.TXT** (30 KB) - XMSG message system symbols
- **SYMBOL-1-LIST.SYMB.TXT** (102 KB) - Primary kernel symbols
- **SYMBOL-2-LIST.SYMB.TXT** (69 KB) - Secondary kernel symbols
- **RTLO-SYMBOLS.SYMB.TXT** (56 KB) - Runtime library symbols
- **LIBRARY-MARKS.SYMB.TXT** (14 KB) - Library entry points

#### Why This is Important

1. **Authentic Implementation** - See how SINTRAN III was actually implemented
2. **Emulator Verification** - Validate emulator behavior against real OS code
3. **Symbol Tables** - Map memory addresses to symbolic names for debugging
4. **File System Insight** - While we lack file system source, symbols provide structure information
5. **Complete Device Drivers** - Full SCSI and HDLC driver implementations

#### Using Source Code with Documentation

The NPL source code should be read alongside the OS documentation:

| Source Files | Corresponding Documentation |
|--------------|---------------------------|
| IP-P2-SCSI-*.NPL | [Devices/SCSI/](Devices/SCSI/) + [OS/15-DISK-IO-SUBSYSTEM.md](OS/15-DISK-IO-SUBSYSTEM.md) |
| MP-P2-HDLC-DRIV.NPL | [Devices/HDLC/](Devices/HDLC/) |
| CC-P2-N500.NPL, MP-P2-N500.NPL | [ND500/](ND500/) + [OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md) |
| IP-P2-SEGADM.NPL | [OS/16-PAGE-FAULT-HANDLER.md](OS/16-PAGE-FAULT-HANDLER.md) |
| 5P-P2-MON60.NPL | [OS/14-MONITOR-KERNEL-MONCALLS.md](OS/14-MONITOR-KERNEL-MONCALLS.md) |

**Full details:** [NPL-SOURCE/README.md](NPL-SOURCE/README.md)

---

## SINTRAN III Release Information

### Historical Release Documentation

**Location:** [Release-Documentation/](Release-Documentation/)

This folder contains official SINTRAN III release notes documenting system evolution across major versions:

**Release Documents (7 files):**
- **ND-60.230.01** - SINTRAN III J-version (January 1985)
- **ND-60230-5-EN** - SINTRAN III K-version Release Information
- **ND-860230-6-EN** - SINTRAN III L-version Release Information
- **ND-860230-7A-EN** - SINTRAN III M-version Release Information
- **ND-860230-8-EN** - SINTRAN III N-version Release Information
- **SINTRAN-III-Release-History.md** - Comprehensive version timeline
- **ndfs-extensions.md** - NDFS file system extensions

#### Why This is Important

1. **Feature Evolution** - Track when features were added/changed across versions
2. **Bug Fix History** - Understand what issues were resolved in each release
3. **Compatibility** - Determine version-specific behavior for emulation
4. **System Requirements** - Hardware/software requirements per version
5. **Migration Guides** - How to upgrade between versions

---

## SINTRAN System Structures

### Internal Data Structure Analysis

**Location:** [SINTRAN Structures/](SINTRAN%20Structures/)

This folder contains detailed analysis of SINTRAN III internal data structures extracted from symbol tables and source code:

**Structure Files (6 files):**
- **SINTRAN-STRUCTURES.md** (224 KB) - Master reference for all kernel structures
- **LOGICAL-DEVICE-NUMBERS.md** (24 KB) - Device number assignments
- **logical-device-numbers.json** (193 KB) - Machine-readable device mapping
- **sintran-rt-programs.json** (60 KB) - RT program metadata
- **sintran-system-segments.json** (28 KB) - System segment definitions
- **SINTRAN_PCCS_ARRAY_ANALYSIS.md** (21 KB) - PCCS array structure analysis

#### Using Structure Documentation

The structure files provide essential reference for:
- **Emulator Development** - Correctly implement kernel data structures
- **Memory Debugging** - Interpret raw memory dumps
- **Symbol Table Cross-Reference** - Link addresses to data structure fields
- **Device Driver Analysis** - Understand device number assignments

**Cross-Reference Example:**
- Read [SINTRAN Structures/SINTRAN-STRUCTURES.md](SINTRAN%20Structures/SINTRAN-STRUCTURES.md) for structure layouts
- Cross-reference with [NPL-SOURCE/](NPL-SOURCE/) symbol tables for addresses
- Use [OS/19-MEMORY-MAP-REFERENCE.md](OS/19-MEMORY-MAP-REFERENCE.md) for memory locations

---

## XMSG (message system, protocol, API and code)

All XMSG material now lives in one hub: **[XMSG/](XMSG/)** — start at
[XMSG/README.md](XMSG/README.md).

| Area | Location |
|------|----------|
| Wire-format reference (HDLC / SINTRAN header / sub-protocols) | [XMSG/DOC/XMSG-PROTOCOL.md](XMSG/DOC/XMSG-PROTOCOL.md) |
| Programming / API reference (MON 200B, XROUT letter, constants) | [XMSG/DOC/XMSG-API.md](XMSG/DOC/XMSG-API.md) |
| Operator utility (XMSG-COMMAND) | [XMSG/DOC/XMSG-COMMAND-REFERENCE.md](XMSG/DOC/XMSG-COMMAND-REFERENCE.md) |
| Official ND constants + machine-readable JSON | [XMSG/XMSG-PL-VALUES-M.INCL](XMSG/XMSG-PL-VALUES-M.INCL), [XMSG/xmsg-constants.json](XMSG/xmsg-constants.json) |
| C# protocol library + tests | [XMSG/SRC/](XMSG/SRC/) (see [XMSG/SRC/README.md](XMSG/SRC/README.md)) |
| Superseded / historical notes | [XMSG/OLD/](XMSG/OLD/) |

The HDLC hardware/framing layer beneath XMSG is documented separately in
[HDLC-Frame-Format-Reference.md](Devices/HDLC/HDLC-Frame-Format-Reference.md).

**Cross-Reference:** See also [NPL-SOURCE/XMSG-SYMBOL-LIST.SYMB.TXT](NPL-SOURCE/) for XMSG message system symbols.

---

## Documentation by Topic

### Core OS Architecture

Located in [OS/](OS/) folder:

| Chapter | Document | Topic |
|---------|----------|-------|
| 00 | SINTRAN-ARCHITECTURE-OVERVIEW.md | System components, interrupts, memory |
| 01 | BOOT-SEQUENCE.md | Boot process and initialization |
| 02 | QUEUE-STRUCTURES-DETAILED.md | Execution, time, waiting queues |
| 03 | CPU-DETECTION-AND-INITIALIZATION.md | Hardware detection |
| 04 | MMU-CONTEXT-SWITCHING.md | Memory management unit |
| 05 | ND500-DMA-KERNEL.md | ND-500 DMA operations |
| 05 | ND500-PROGRAMS-SPECIAL.md | Special ND-500 programs |
| 06 | MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md | Shared memory architecture |
| 06 | MULTIPORT-MEMORY-PART2.md | Additional 5MPM details |
| 07 | ND500-IO-AND-USER-INTERACTION.md | ND-500 user interaction |
| 08 | MESSAGE-PASSING-DETAILED.md | Inter-CPU messaging |
| 09 | ND500-CODE-LOADING.md | Loading code into ND-500 |
| 10 | ND500-STANDALONE-EMULATOR.md | Standalone ND-500 emulation |
| 11 | RT-SEGMENTS-AND-SEGFIL.md | Segment management |
| 12 | ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md | ND-500 domain configuration |
| 13 | INT14-HANDLER-DETAILED.md | Interrupt handler (Level 14) |
| 14 | MONITOR-KERNEL-MONCALLS.md | Monitor kernel and system calls |
| 15 | DISK-IO-SUBSYSTEM.md | Disk I/O and drivers |
| 16 | PAGE-FAULT-HANDLER.md | Page fault handling |
| 17 | SCHEDULER-AND-PRIORITIES.md | Task scheduler |
| 18 | DEVICE-DRIVER-FRAMEWORK.md | Device driver architecture |
| 19 | MEMORY-MAP-REFERENCE.md | Complete memory layout |

### Hardware Documentation

**MPM5 Multiport Memory:**
- [OS/MPM5-KEY-FINDINGS.md](OS/MPM5-KEY-FINDINGS.md) - Hardware details from official manual
- [OS/MPM5-DOCUMENTATION-UPDATE-SUMMARY.md](OS/MPM5-DOCUMENTATION-UPDATE-SUMMARY.md) - Documentation updates

**Device Drivers:**
- [Devices/HDLC/](Devices/HDLC/) - HDLC communication controller
- [Devices/SCSI/](Devices/SCSI/) - SCSI disk controllers

**Protocols:**
- [TAD/](TAD/) - TAD protocol analysis (X.25, HDLC encapsulation)

### Emulator Implementation

Located in [Emulator/](Emulator/) folder:

| Document | Purpose |
|----------|---------|
| KERNEL-ACCESS-EMULATOR.md | Read SINTRAN kernel structures from C# |
| ND500-EMULATION-COMPLETE.cs | Complete C# ND-500 emulation code |
| ND500-INTEGRATION-GUIDE.md | Integrate ND-500 into your emulator |
| ND500-QUICK-REFERENCE.md | Quick reference for development |
| ND500-MESSAGE-STRUCTURE-VERIFIED.md | Verified message structure |

---

## Key Concepts

### SINTRAN III Architecture

**Interrupt-Driven Design:**
- 16 interrupt levels with complete register sets
- Level 14: Internal interrupts (monitor calls, page faults)
- Level 13: Real-time clock
- Levels 10-12: Device I/O
- Level 3: Monitor kernel
- Level 1: User programs

**Virtual Memory:**
- 4 Page Index Tables (PITs) mapping 64K address space
- Demand paging (pages loaded on access)
- Ring protection (4 privilege levels)
- POF area accessible without MMU

**Queue-Driven Scheduler:**
- Execution queue: Ready-to-run programs (priority-ordered)
- Time queue: Scheduled programs (time-ordered)
- Waiting queues: Per-resource (priority-ordered)
- Monitor queue: Pending monitor activations (FIFO)

### ND-500 Integration

**Dual-CPU Architecture:**
- ND-100: 16-bit word-addressed CPU (control processor)
- ND-500: Byte-addressed CPU (computation processor)
- 5MPM (Multiport Memory): Shared physical RAM

**Communication:**
- Message passing through 5MPM
- Process descriptors in 5MPM
- TAG-IN/TAG-OUT signaling
- Interrupt-driven protocol

**Key Point:** ND-500 is **byte-oriented**, not 32-bit word! The "32-bit" refers to memory **bus width** for bandwidth optimization.

---

## Document Statistics

| Category | Files | Size |
|----------|-------|------|
| **NPL Source Code** | **45** | **~3.9MB** |
| **Symbol Tables (L07)** | **7** | **~450KB** |
| OS Architecture (00-19) | 34 | ~550KB |
| Release Documentation (J-N) | 7 | ~1.2MB |
| SINTRAN Structures | 6 | ~570KB |
| HDLC Documentation | 30+ | ~350KB |
| SCSI Documentation | 10+ | ~125KB |
| Emulator Guides | 5 | ~115KB |
| TAD Protocol | 7 | ~75KB |
| XMSG Reference | 1 | ~40KB |
| **Total** | **160+** | **~7.4MB** |

---

## Navigation

### By Experience Level

**Beginner (Understanding SINTRAN):**
1. [OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md](OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md)
2. [OS/02-QUEUE-STRUCTURES-DETAILED.md](OS/02-QUEUE-STRUCTURES-DETAILED.md)
3. [OS/17-SCHEDULER-AND-PRIORITIES.md](OS/17-SCHEDULER-AND-PRIORITIES.md)

**Intermediate (System Details):**
1. [OS/13-INT14-HANDLER-DETAILED.md](OS/13-INT14-HANDLER-DETAILED.md)
2. [OS/14-MONITOR-KERNEL-MONCALLS.md](OS/14-MONITOR-KERNEL-MONCALLS.md)
3. [OS/15-DISK-IO-SUBSYSTEM.md](OS/15-DISK-IO-SUBSYSTEM.md)
4. [OS/18-DEVICE-DRIVER-FRAMEWORK.md](OS/18-DEVICE-DRIVER-FRAMEWORK.md)

**Advanced (ND-500 & Emulation):**
1. [OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md)
2. [OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md](OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md)
3. [Emulator/ND500-INTEGRATION-GUIDE.md](Emulator/ND500-INTEGRATION-GUIDE.md)

### By Task

**Reading SINTRAN III Source Code:**
→ Start in [NPL-SOURCE/](NPL-SOURCE/) folder - actual kernel implementation

**Building an Emulator:**
→ Start in [Emulator/](Emulator/) folder, then cross-reference [NPL-SOURCE/](NPL-SOURCE/) and [SINTRAN Structures/](SINTRAN%20Structures/)

**Understanding Devices:**
→ See [Devices/](Devices/) folder, then check corresponding NPL files

**OS Internals:**
→ See [OS/](OS/) folder, chapters 00-19, verified against [NPL-SOURCE/](NPL-SOURCE/)

**Version-Specific Features:**
→ See [Release-Documentation/](Release-Documentation/) for feature evolution across versions J-N

**Data Structure Reference:**
→ See [SINTRAN Structures/](SINTRAN%20Structures/) for kernel structures, device numbers, and memory layouts

**Network Management:**
→ See [XMSG/](XMSG/) for the full XMSG hub (protocol, API, C# library), and
[XMSG/DOC/XMSG-COMMAND-REFERENCE.md](XMSG/DOC/XMSG-COMMAND-REFERENCE.md) for the COSMOS/XMSG operator utility

**Protocol Analysis:**
→ See [TAD/](TAD/) folder

---

## Related Documentation

### Developer Guides

Located in `Developer/` folder (parent directory):
- NPL-DEVELOPER-GUIDE.md - NORD PL language
- MAC-DEVELOPER-GUIDE.md - Macro assembler
- C-DEVELOPER-GUIDE.md - C compiler
- PLANC-DEVELOPER-GUIDE.md - PLAN C compiler

### External References

**Official Manuals:**
- SINTRAN III System Manuals
- ND-100 Architecture Manual
- ND-500 Architecture Manual
- MPM5 Technical Description (ND-10.004.01)

**Online Resources:**
- SINTRAN Preservation Project
- NDInsight GitHub Repository

---

## Contributing

When adding documentation:

1. **Follow existing structure** - Use numbered chapters for OS docs
2. **Use Mermaid diagrams** - Follow `../MERMAID_COLOR_STANDARDS.md`
3. **Cross-reference** - Link related documents
4. **Use relative paths** - No absolute paths (E:\, C:\, /)
5. **Add to appropriate README** - Update this file and folder READMEs

---

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-14 | 1.1 | Added Release-Documentation, SINTRAN Structures, XMSG reference |
| 2025-10-17 | 1.0 | Initial comprehensive SINTRAN documentation structure |

---

**For project overview, see:** [../README.md](../README.md)  
**For developer guides, see:** [../Developer/](../Developer/)

---

*This documentation preserves knowledge of the SINTRAN III operating system for emulation, study, and historical preservation.*

