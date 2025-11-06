# NDInsight - NORD Computer Systems Analysis & Documentation

**Comprehensive technical analysis and documentation for Norsk Data NORD computer systems**

---

## 📖 Overview

**NDInsight** is a centralized repository of deep technical analysis, reverse-engineered documentation, and implementation guides for the **Norsk Data NORD computer systems**, focusing on the **SINTRAN III operating system** and its hardware architecture.

The analysis is made from original SINTRAN III source code, Norsk Data technical manuals, and with heavy use of AI (Claude AI, Cursor AI, ChatGPT AI). While comprehensive, there might be errors and hallucinations - use with appropriate verification.

### What's Inside

- **SINTRAN III NPL source code** - 45 files of authentic operating system source code
- **SINTRAN L07 symbol tables** - 7 files mapping memory addresses to kernel structures
- **Kernel-level documentation** extracted from SINTRAN III source code
- **Hardware analysis** of communication protocols (HDLC, X.21, TAD)
- **ND-500 coprocessor** integration and communication
- **Device driver** implementation details (with complete source code)
- **SCSI subsystem** analysis (with complete source code)
- **Emulator implementation** guides and C# code
- **Complete reference manuals** for all NORD/SINTRAN components and languages

---

## 🗂️ Repository Structure

```
NDInsight/
├── README.md                    ← You are here
├── MERMAID_COLOR_STANDARDS.md   ← Diagram color standards
├── Developer/                   ← Developer guides and language references
├── Reference-Manuals/           ← Complete NORD/SINTRAN reference manuals
└── SINTRAN/                     ← SINTRAN III Operating System
    ├── Devices/                 ← Hardware device documentation
    ├── Emulator/                ← C# emulator implementation guides
    ├── ND500/                   ← ND-500 coprocessor documentation
    ├── NPL-SOURCE/              ← ⭐ SINTRAN III source code & symbols
    ├── OS/                      ← Core OS kernel documentation
    └── TAD/                     ← TAD protocol analysis
```

---

## 🚀 Quick Start

### New to SINTRAN III?

**Start with these documents:**
1. [SINTRAN/OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md](SINTRAN/OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md) - System overview
2. [SINTRAN/OS/01-BOOT-SEQUENCE.md](SINTRAN/OS/01-BOOT-SEQUENCE.md) - How the system boots
3. [SINTRAN/README.md](SINTRAN/README.md) - Complete SINTRAN documentation guide

### Building an Emulator?

**Critical resources:**
1. [SINTRAN/NPL-SOURCE/README.md](SINTRAN/NPL-SOURCE/README.md) - Access to actual source code
2. [SINTRAN/Emulator/README.md](SINTRAN/Emulator/README.md) - C# implementation guides
3. [SINTRAN/OS/19-MEMORY-MAP-REFERENCE.md](SINTRAN/OS/19-MEMORY-MAP-REFERENCE.md) - Memory layout

### Developing Software?

**Language guides:**
1. [Developer/README.md](Developer/README.md) - Developer documentation index
2. [Reference-Manuals/README.md](Reference-Manuals/README.md) - Complete language references

---

## 📂 Main Sections

### [Developer/](Developer/) - Development Guides

Complete guides for developing software on SINTRAN III systems.

**Contents:**
- **Languages/** - NPL, MAC, PLANC, PASCAL, FORTRAN, COBOL, BASIC, C
- **Editors/** - QED, PED, LED editor guides
- **Workflow/** - Compiler, linker, tools, scripts

📖 **[See Developer/README.md for full details](Developer/README.md)**

---

### [Reference-Manuals/](Reference-Manuals/) - Official Documentation

Authoritative reference manuals for all NORD/SINTRAN components (25 complete manuals, ~2.2MB).

**Categories:**
- System & CPU Architecture (ND-500, SINTRAN Commands)
- Assemblers (NORD-500 Assembler, MAC, MACM)
- System Programming (NPL)
- Application Languages (PLANC, PASCAL, FORTRAN, COBOL, BASIC)
- Editors (QED, PED)
- System Tools (Relocating Loader)

📖 **[See Reference-Manuals/README.md for full index](Reference-Manuals/README.md)**

---

### [SINTRAN/](SINTRAN/) - Operating System Documentation

Comprehensive documentation for SINTRAN III operating system (140+ files, ~5.6MB).

#### Key Subdirectories

| Directory | Description | Details |
|-----------|-------------|---------|
| **[NPL-SOURCE/](SINTRAN/NPL-SOURCE/)** ⭐ | **SINTRAN III source code** | 45 NPL files + 7 symbol tables (4.4MB) |
| **[OS/](SINTRAN/OS/)** | Core kernel documentation | 31 files covering chapters 00-19 (518KB) |
| **[Devices/](SINTRAN/Devices/)** | Hardware device drivers | HDLC (30+ docs) + SCSI (10+ docs) |
| **[Emulator/](SINTRAN/Emulator/)** | C# emulator implementation | 5 files with production code |
| **[ND500/](SINTRAN/ND500/)** | ND-500 coprocessor | Integration and communication |
| **[TAD/](SINTRAN/TAD/)** | TAD protocol | Terminal access protocol analysis |

📖 **[See SINTRAN/README.md for complete overview](SINTRAN/README.md)**

---

### [SINTRAN/NPL-SOURCE/](SINTRAN/NPL-SOURCE/) - Source Code ⭐

**Authentic SINTRAN III operating system source code** from the s3vs-4 build job.

**What's Included:**
- **45 NPL source files** - Kernel, device drivers, ND-500 interface, disk I/O
- **7 symbol tables (SINTRAN L07)** - Memory addresses and kernel structures
- **s3vs-4.symb** - Original 3.9MB build output

**Key Components:**
- 5P-P2-MON60.NPL - Monitor/kernel core
- IP-P2-SCSI-*.NPL - SCSI drivers (complete implementation)
- MP-P2-HDLC-DRIV.NPL - HDLC communication driver
- CC-P2-N500.NPL, MP-P2-N500.NPL - ND-500 interface code

📖 **[See SINTRAN/NPL-SOURCE/README.md for details](SINTRAN/NPL-SOURCE/README.md)**

---

### [SINTRAN/OS/](SINTRAN/OS/) - Kernel Documentation

Complete kernel documentation extracted from SINTRAN III source code (31 files, 518KB).

**Chapters 00-19:**
- 00: Architecture Overview
- 01-12: Boot, queues, MMU, ND-500 integration, memory
- 13: Interrupt handler (Level 14)
- 14: Monitor kernel and system calls
- 15: Disk I/O subsystem
- 16: Page fault handler
- 17: Scheduler and priorities
- 18: Device driver framework
- 19: Memory map reference

📖 **[See SINTRAN/OS/README.md for complete index](SINTRAN/OS/README.md)**

---

### [SINTRAN/Devices/](SINTRAN/Devices/) - Hardware Devices

Device driver documentation with complete source code analysis.

**[HDLC/](SINTRAN/Devices/HDLC/)** - High-Level Data Link Control
- COM5025 chip specification
- X.21 interface protocol
- LAPB/X.25 protocol implementation
- C# emulator guide

**[SCSI/](SINTRAN/Devices/SCSI/)** - SCSI Disk Controllers
- NCR 5386 protocol controller
- 27 SCSI commands documented
- Disk, optical, tape drivers
- C# implementation guide

📖 **[See SINTRAN/Devices/README.md for details](SINTRAN/Devices/README.md)**

---

### [SINTRAN/Emulator/](SINTRAN/Emulator/) - Emulator Implementation

C# emulator implementation guides with production code.

**Contents:**
- KERNEL-ACCESS-EMULATOR.md - Reading kernel structures from C#
- ND500-EMULATION-COMPLETE.cs - Complete ND-500 emulation (677 lines)
- ND500-INTEGRATION-GUIDE.md - Step-by-step integration guide
- ND500-QUICK-REFERENCE.md - Quick reference card

📖 **[See SINTRAN/Emulator/README.md for details](SINTRAN/Emulator/README.md)**

---

## 📊 Repository Statistics

### Documentation by Category

| Category | Files | Size | Status |
|----------|-------|------|--------|
| **NPL Source Code** | **45** | **3.9MB** | ✅ Complete (s3vs-4) |
| **Symbol Tables (L07)** | **7** | **450KB** | ✅ Complete |
| **Reference Manuals** | 25 | ~2.2MB | ✅ Complete Collection |
| **OS Kernel Docs** | 31 | 518KB | ✅ Phase 1 Complete |
| **HDLC Analysis** | 30+ | ~350KB | ✅ Consolidated |
| **SCSI Analysis** | 10+ | ~125KB | ✅ Complete |
| **Developer Guides** | 20+ | ~500KB | ✅ Multiple Languages |
| **Emulator Code** | 6 | ~100KB | ✅ Production Ready |
| **ND-500 Docs** | 6+ | ~200KB | ✅ Complete |
| **TAD Protocol** | 7 | ~150KB | ✅ Complete |
| **Total** | **187+** | **~8.2MB** | - |

### Source Code Coverage

**What We Have (NPL Source):**
- ✅ Kernel/Monitor core (MON60)
- ✅ SCSI device drivers (disk, optical, tape)
- ✅ HDLC communication driver
- ✅ ND-500 interface code
- ✅ Disk I/O subsystem
- ✅ Segment administration
- ✅ HASP protocol
- ✅ Terminal handlers (partial)

**What's Missing:**
- ❌ File system implementation (but symbols available)
- ❌ XMSG message system (but symbols available)
- ❌ Complete terminal handlers
- ❌ Network subsystems (beyond HDLC)
- ❌ Batch processing components

### Documentation Quality Metrics

| Metric | Value |
|--------|-------|
| **Total Words** | ~100,000+ words |
| **Code Examples** | 200+ NPL/C# examples |
| **Mermaid Diagrams** | 40+ diagrams |
| **Reference Tables** | 150+ tables |
| **Cross-References** | 300+ internal links |
| **Production C# Code** | ~2,700 lines |

---

## 🛠️ Technologies Documented

### Hardware
- **NORD-10/100** - 16-bit minicomputer (1970s-1980s)
- **ND-500** - 32-bit coprocessor
- **MPM5** - Multiport memory (shared RAM)
- **COM5025** - HDLC controller chip
- **NCR 5386** - SCSI protocol controller

### Software
- **SINTRAN III** - Real-time operating system
- **NPL** - NORD Programming Language (system programming)
- **MAC** - Macro assembler
- **PLANC, PASCAL, FORTRAN, COBOL, BASIC, C** - Application languages

### Protocols
- **HDLC** - High-Level Data Link Control
- **X.21** - Serial interface standard
- **X.25** - Packet switching protocol
- **LAPB** - Link Access Procedure Balanced
- **TAD** - Terminal Access Device protocol

---

## 🎓 Documentation Methodology

### Sources
1. **SINTRAN III Source Code** - Direct NPL source analysis (s3vs-4 build)
2. **Hardware Manuals** - Official Norsk Data documentation
3. **Protocol Specifications** - HDLC, X.21, X.25, TAD standards
4. **Reverse Engineering** - Bit-level analysis when needed

### Quality Standards
- ✅ **No guesswork** - Only verified facts from source/manuals
- ✅ **Source citations** - Every claim traced to source
- ✅ **Cross-references** - Extensive linking between documents
- ✅ **Visual aids** - Mermaid diagrams for complex flows
- ✅ **Production code** - Working C# implementations included
- ✅ **WCAG 2.1 AA** - Accessible diagram color standards

---

## 📂 Complete Folder Navigation

Each major folder has its own README for detailed navigation:

| Folder | README | Contents |
|--------|--------|----------|
| **Reference-Manuals/** | [README.md](Reference-Manuals/README.md) | 15 complete NORD/SINTRAN reference manuals |
| **Developer/** | [README.md](Developer/README.md) | Development guides, quick-starts, language references |
| **Developer/Languages/** | [README.md](Developer/Languages/README.md) | System & Application language guides |
| **Developer/Workflow/** | [README.md](Developer/Workflow/README.md) | Compiler commands, linking, tools |
| **SINTRAN/** | [README.md](SINTRAN/README.md) | SINTRAN III complete documentation overview |
| **SINTRAN/NPL-SOURCE/** | [README.md](SINTRAN/NPL-SOURCE/README.md) | Source code & symbol tables |
| **SINTRAN/OS/** | [README.md](SINTRAN/OS/README.md) | Operating system kernel (chapters 00-19) |
| **SINTRAN/Devices/** | [README.md](SINTRAN/Devices/README.md) | Hardware device documentation |
| **SINTRAN/Devices/HDLC/** | [README.md](SINTRAN/Devices/HDLC/README.md) | HDLC communication controller |
| **SINTRAN/Devices/SCSI/** | [README.md](SINTRAN/Devices/SCSI/README.md) | SCSI disk controllers |
| **SINTRAN/Emulator/** | [README.md](SINTRAN/Emulator/README.md) | C# emulator implementation |
| **SINTRAN/ND500/** | [README.md](SINTRAN/ND500/README.md) | ND-500 processor documentation |
| **SINTRAN/TAD/** | [README.md](SINTRAN/TAD/README.md) | TAD protocol analysis |

---

## 🚀 Use Cases

### For Emulator Developers
→ Start at [SINTRAN/Emulator/README.md](SINTRAN/Emulator/README.md)
- Understand SINTRAN kernel internals
- Implement accurate hardware emulation
- Access production C# code
- Verify against actual NPL source

### For System Archaeologists
→ Start at [SINTRAN/OS/README.md](SINTRAN/OS/README.md)
- Study 1970s-1980s OS design
- Analyze interrupt-driven architecture
- Learn paging and virtual memory
- Understand multi-CPU coordination

### For Software Developers
→ Start at [Developer/README.md](Developer/README.md)
- Learn SINTRAN development
- Choose your language (8 languages supported)
- Access complete reference manuals
- Follow practical examples

### For Hardware Analysts
→ Start at [SINTRAN/Devices/README.md](SINTRAN/Devices/README.md)
- Understand device driver architecture
- Analyze communication protocols
- Study HDLC and SCSI implementations
- Access chip-level specifications

---

## 🗺️ Project Roadmap

### ✅ Completed (Phase 1)
- Core kernel documentation (Chapters 00-19)
- HDLC protocol analysis (consolidated)
- SCSI subsystem analysis
- ND-500 integration guide
- TAD protocol documentation
- NPL source code organization
- Symbol table documentation
- C# emulator code

### 🚧 Current Focus
- Developer guide improvements
- Cross-reference validation
- Documentation accessibility

### 📋 Planned (Phase 2)
- Monitor calls comprehensive guide
- XMSG development guide
- File system analysis (from symbols)
- Additional device drivers
- Build system analysis (MODE files)

---

## 🤝 Contributing

This repository is a work in progress. Areas for contribution:
- Additional SINTRAN subsystem analysis
- Emulator bug fixes and improvements
- Protocol trace analysis
- Hardware documentation
- Historical information and context

---

## 📜 License

**Documentation**: Creative Commons Attribution 4.0 International (CC BY 4.0)
**Code**: MIT License

---

## 🙏 Acknowledgments

- **Norsk Data AS** - Original SINTRAN III development (1970s-1980s)
- **SINTRAN Source Code** - Foundation for this analysis
- **Hardware Manuals** - Official Norsk Data documentation
- **Emulator Community** - Keeping vintage computing alive

---

**Last Updated**: 2025-11-06
**Repository Version**: 1.0
**Documentation Status**: Phase 1 Complete ✅

---

*Preserving the legacy of Norsk Data computing through comprehensive technical analysis and documentation.*
