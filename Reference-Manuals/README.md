# Reference Manuals - Complete NORD/SINTRAN Documentation

**Authoritative reference manuals for all NORD/SINTRAN system components, programming languages, and development tools**

---

## 📖 Overview

This folder contains **52 complete reference manuals** spanning system architecture, ND-100/ND-110 hardware, the SINTRAN III operating system, database systems, programming languages, assemblers, editors, compilers, and development tools. These manuals serve as the primary source documentation for all NORD/SINTRAN development work.

### Purpose

- **Comprehensive Reference** - Complete technical documentation for all system components
- **Developer Resource** - Source material for creating developer guides and tutorials
- **Historical Archive** - Preservation of original NORD/SINTRAN documentation (1970s-1980s)
- **Cross-Reference** - Detailed specifications for advanced users and system programmers

### Total Collection

| Category | Manuals | Total Size |
|----------|---------|------------|
| **System & Architecture** | 4 | ~1.2MB |
| **ND-100 / ND-110 Hardware & Architecture** | 6 | ~2.1MB |
| **SINTRAN III System & OS** | 14 | ~2.6MB |
| **Database Systems (SIBAS)** | 4 | ~700KB |
| **Hardware** | 1 | 85KB |
| **Programming Languages** | 13 | ~450KB |
| **Assemblers** | 3 | ~200KB |
| **Compilers & Linkers** | 2 | ~150KB |
| **Editors** | 4 | ~100KB |
| **System Tools** | 1 | ~50KB |
| **Total** | **52** | **~7.6MB** |

---

## 🗂️ Complete Manual Index

### System & CPU Architecture

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **ND-500 Reference Manual** | ND-05.009.4 EN | 16,324 | Complete ND-500 CPU architecture, instruction set, domains, memory management |
| **ND-500 Addressing Modes** | - | 949 | **NEW** Complete addressing modes reference with binary encoding for disassembly |
| **SINTRAN III Monitor Calls** | ND-860228-2-EN | 23,478 | Complete system call reference - monitor routines, kernel interface, I/O operations |
| **SINTRAN Commands Reference** | - | 11,657 | All SINTRAN III commands - batch processing, file management, MODE files |
| **ND-500 Loader Monitor** | ND-60.136.04A | - | ND-500 program loading and monitor interface |

**Key Topics:** CPU architecture, instruction sets, addressing modes, system calls, command reference, monitor interface

---

### Hardware Documentation

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **MPM 5 Technical Description** | ND-10.004.01 | - | Multiport Memory (5MPM) hardware specifications - critical for ND-500 integration |

**Key Topics:** 5MPM architecture, memory mapping, ND-100/ND-500 shared memory, hardware interface

**Related:** See [SINTRAN/OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../SINTRAN/OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md)

---

### ND-100 / ND-110 Hardware & Architecture

The ND-100 and ND-110 processor hardware manual set - CPU architecture, functional
descriptions, the I/O system, the big multiport memory, and the ND-110 instruction set.
Primary source material for emulator development and low-level hardware work.

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **ND-100 Functional Description** | ND-06.015.02 | 9,020 | Complete ND-100 hardware functional description - CPU, buses, memory, control logic |
| **ND-110 Functional Description** | ND-06.026-1 EN | 6,375 | Complete ND-110 hardware functional description - CPU, buses, memory, control logic |
| **ND-100 Reference Manual** | ND-06.014.2A EN | 6,231 | ND-100 hardware reference - registers, timing, hardware interface |
| **NORD-100 Input/Output System** | ND-06.016.01 | 5,713 | NORD-100 I/O system - device interfaces, IOX, DMA, interrupt system |
| **ND-110 Instruction Set** | ND-06.029.1 EN | 5,512 | Complete ND-110 instruction set - opcodes, addressing, encoding |
| **BIG MULTIPORT MEMORY SYSTEM** | ND-06.007.01 | 3,072 | Big multiport memory hardware - shared memory, port arbitration |

**Key Topics:** ND-100/ND-110 CPU architecture, hardware functional description, instruction set encoding, I/O system, interrupts, DMA, multiport memory

**Related:**
- [SINTRAN/OS/README.md](../SINTRAN/OS/README.md) - Operating system internals
- [SINTRAN/Emulator/](../SINTRAN/Emulator/) - C# emulator implementation

---

### SINTRAN III System & OS Manuals

The core SINTRAN III operating-system manual set. (The **SINTRAN III Monitor Calls** and
**SINTRAN Commands Reference** are listed above under System & CPU Architecture.)

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **SINTRAN III Reference Manual** | ND-60.128.5 EN | 21,694 | Master SINTRAN III reference - commands, subsystems, system behaviour |
| **SINTRAN III Real Time Guide** | ND-60.133.02A | 14,445 | Real-time programming under SINTRAN III |
| **SINTRAN III Users Guide** | ND-60.050.06 | 11,589 | General user guide to SINTRAN III |
| **SINTRAN III Timesharing / Batch Guide** | ND-60.132.03 | 6,616 | Timesharing and batch processing guide |
| **SINTRAN III Communication Guide** | ND-60.134.2 EN | 5,740 | Communications / networking under SINTRAN III |
| **SINTRAN III System Documentation, Appendix A - Data Fields** | ND-60.112.01 | 4,455 | System data-field definitions (system documentation appendix) |
| **SINTRAN III - Real Time Loader** | ND-60.051.8 EN | 3,430 | Real Time Loader (RT loader) manual |
| **SINTRAN III Introduction** | ND-60.125.04 | 3,245 | Introduction / getting started with SINTRAN III |
| **SINTRAN III Utilities Manual** | ND-60.151.3 EN | 2,257 | SINTRAN III utility programs manual |
| **SINTRAN III Real Time Loader - System Documentation** | ND-60.072.02 | 1,620 | RT loader internal / system documentation |
| **SINTRAN Utility Programs** | ND-10022S | 1,158 | SINTRAN utility programs package (version S) |
| **SINTRAN III Accounting System** | ND-10315B | 584 | Accounting system for SINTRAN III |
| **SINTRAN III Quick Reference Card** | ND-60.174.Q01 | 537 | Quick reference card |
| **SINTRAN III Configuration Program** | 211024C | 205 | System configuration program note |

**Key Topics:** SINTRAN III commands, real-time programming, RT loader, timesharing/batch, communications, accounting, utilities, system data fields

**Related:**
- [SINTRAN/OS/README.md](../SINTRAN/OS/README.md) - Operating system internals
- [SINTRAN/SINTRAN Structures/SINTRAN-STRUCTURES.md](../SINTRAN/SINTRAN%20Structures/SINTRAN-STRUCTURES.md) - Kernel data structures

---

### Database Systems (SIBAS)

SIBAS - the Norsk Data CODASYL / DBTG-style (network model) database system, accessed
from a host language (COBOL, FORTRAN).

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **THE DATABASE SYSTEM SIBAS II - ND User Manual** | ND-60.127.5 EN | 13,080 | Primary SIBAS II user manual - schema, realms, DML, host-language access (replaces ND-60.057) |
| **SIBAS II Operator Manual** | ND-30.009.3 EN | 3,548 | SIBAS II database administration / operator procedures |
| **SIBAS II for ND-100** | 210166F | 1,196 | SIBAS II product documentation for the ND-100 |
| **The Data Base System SIBAS I - Users Manual, Appendix A** | ND-60.057.03 | 1,063 | SIBAS I users manual (Appendix A) - earlier generation |

**Key Topics:** CODASYL/DBTG network database, realms, schema, DML, host-language (COBOL/FORTRAN) access, operator administration

**Related:** [Developer/Languages/Application/SIBAS-DEVELOPER-GUIDE.md](../Developer/Languages/Application/SIBAS-DEVELOPER-GUIDE.md)

---

### Assemblers

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **MAC Interactive Assembly and Debugging** | ND-60.096.01 | 6,020 | MAC assembler and debugger - interactive assembly, BRF format, debugging |
| **MACM Mac Mass Storage Assembler** | ND-60.009.02 | - | MACM macro assembler - assembly, macros, mass storage linking |
| **NORD-500 Assembler Reference** | ND-60.113.02 EN | 4,140 | Complete ND-500 assembly - NRF format, structured programming, domains |

**Key Topics:** Assembly language, macro programming, debugging, object file formats (BRF, NRF)

**Related:** See [Developer/Languages/System/MAC-DEVELOPER-GUIDE.md](../Developer/Languages/System/MAC-DEVELOPER-GUIDE.md)

---

### System Programming Language

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **NORD PL User's Guide** | ND-60.047.03 | - | NPL (NORD Programming Language) complete reference - system programming, NPL syntax, OS development |

**Key Topics:** NPL language, system programming, kernel development, MAC code generation

**Related:**
- [Developer/Languages/System/NPL-DEVELOPER-GUIDE.md](../Developer/Languages/System/NPL-DEVELOPER-GUIDE.md)
- [SINTRAN/NPL-SOURCE/](../SINTRAN/NPL-SOURCE/) - Actual SINTRAN III NPL source code

---

### Application Languages

#### C Language

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **CC-100 and CC-500 C-Compiler User Manual** | ND-60.214.01 | - | Complete C compiler reference for ND-100 and ND-500 systems |

**Key Topics:** C language, compiler usage, ND-100/ND-500 differences, optimization

**Related:** [Developer/Languages/Application/C-DEVELOPER-GUIDE.md](../Developer/Languages/Application/C-DEVELOPER-GUIDE.md)

#### PLANC

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **PLANC Reference Manual** | ND-60.117.5 EN | - | Complete PLANC language reference - structured programming, modules |
| **PLANC FOR ND-100** | ND-10309A | 163 | PLANC compiler for ND-100 - setup, restrictions, runtime system |

**Key Topics:** PLANC syntax, structured programming, modular design

**Note:** ND-60.117.5 has one missing page (documented in source)

#### PASCAL

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **ND-PASCAL User's Guide** | ND-60.124.05 | - | ND-PASCAL language reference - PASCAL syntax, compiler, runtime |

**Key Topics:** PASCAL language, structured programming, data types

#### COBOL

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **COBOL Reference Manual** | ND-60.144.3 EN | - | COBOL language reference - business programming, file handling |

**Key Topics:** COBOL syntax, business data processing, file I/O

#### FORTRAN

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **NORD Standard FORTRAN Reference** | ND-60.011.04 | - | NORD Standard FORTRAN (1974) - FORTRAN IV, scientific computing |
| **ND FORTRAN Reference Manual** | ND-60.145.7A EN | 25,000+ | ND FORTRAN (ANSI 77, Version 7A 1986) - Extended FORTRAN, optimization |
| **Fortran for ND-100 / NORD-10** | ND-10191A | 135 | ND-100 / NORD-10 FORTRAN compiler product note |
| **FORTRAN for ND-500** | ND-10190D | 280 | FORTRAN targeting the ND-500 processor |
| **FORTRAN 32 Bits Floating Format** | ND-10033K | 377 | 32-bit floating-point format used by FORTRAN |

**Key Topics:** FORTRAN language, scientific computing, numerical analysis, floating-point format, ND-100/ND-500 targets

#### BASIC

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **NORD BASIC Reference Manual** | ND-60.040.02 | - | NORD BASIC (1975) - Interactive BASIC programming |
| **NORD-10 BASIC Compiler Reference** | ND-60.071.01D | 8,513 | NORD-10 BASIC Compiler - Compiled BASIC, optimization |

**Key Topics:** BASIC language, interactive programming, compilation

---

### Compilers & Linkers

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **ND Linker User Guide and Reference** | ND-860289-2-EN | - | Complete linker reference - object file linking, library management, symbol resolution |
| **ND Relocating Loader** | ND-60.066.04 | - | Loader documentation - program loading, relocation, memory allocation |

**Key Topics:** Linking, loading, relocation, symbol resolution, library management

**Related:**
- [Developer/Workflow/LINKING-GUIDE-100-DEEP-DIVE.md](../Developer/Workflow/LINKING-GUIDE-100-DEEP-DIVE.md)
- [Developer/Workflow/LINKING-GUIDE-500-DEEP-DIVE.md](../Developer/Workflow/LINKING-GUIDE-500-DEEP-DIVE.md)

---

### Editors

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **QED User Manual** | ND-60.031.04 EN | - | QED editor reference - line-oriented text editing |
| **PED User's Guide** | ND-60.121.4 | - | PED editor reference (English) - screen-oriented text editing |
| **PED Bruker-Veiledning** | ND-60.148.01 | - | PED editor guide (Norwegian) - Norwegian language version |
| **BRF EDITOR** | ND-60.085.01 | - | BRF file editor - binary relocatable format editing |

**Key Topics:** Text editing, file editing, screen editors, line editors

**Related:** [Developer/Editors/](../Developer/Editors/)

---

## 🔗 Cross-Reference to Developer Guides

### System Languages → [Developer/Languages/System/](../Developer/Languages/System/)

| Language | Reference Manuals | Developer Guide | Status |
|----------|-------------------|-----------------|--------|
| **NPL** | ND-60.047.03 | NPL-DEVELOPER-GUIDE.md | ✅ Complete |
| **MAC** | ND-60.096.01, ND-60.009.02 | MAC-DEVELOPER-GUIDE.md | ✅ Complete |
| **NORD-500 Asm** | ND-60.113.02, ND-05.009.4 | NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md | ✅ Complete |

### Application Languages → [Developer/Languages/Application/](../Developer/Languages/Application/)

| Language | Reference Manuals | Developer Guide | Status |
|----------|-------------------|-----------------|--------|
| **C** | ND-60.214.01 | C-DEVELOPER-GUIDE.md | ✅ Complete |
| **PLANC** | ND-60.117.5, ND-10309A | PLANC-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **PASCAL** | ND-60.124.05 | PASCAL-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **COBOL** | ND-60.144.3 | COBOL-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **FORTRAN** | ND-60.011.04, ND-60.145.7A, ND-10191A, ND-10190D, ND-10033K | FORTRAN-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **BASIC** | ND-60.040.02, ND-60.071.01D | BASIC-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **SIBAS** (database) | ND-60.127.5, ND-30.009.3, 210166F, ND-60.057.03 | SIBAS-DEVELOPER-GUIDE.md | 🚧 Reference index |

### Development Tools → [Developer/Workflow/](../Developer/Workflow/)

| Tool | Reference Manuals | Workflow Guide | Status |
|------|-------------------|----------------|--------|
| **Linker** | ND-860289-2-EN | LINKING-GUIDE-100/500-DEEP-DIVE.md | ✅ Complete |
| **Loader** | ND-60.066.04 | (Covered in linking guides) | ✅ Complete |
| **Editors** | ND-60.031.04, ND-60.121.4 | [Developer/Editors/](../Developer/Editors/) | ✅ Complete |

---

## 🚀 How to Use These Manuals

### For Learning SINTRAN Development

**Step 1: Choose Your Language**
- System programming → Start with NPL (ND-60.047.03)
- Application development → Choose PLANC, PASCAL, C, FORTRAN, COBOL, or BASIC
- Low-level programming → Start with MAC (ND-60.096.01) or ND-500 Assembler (ND-60.113.02)

**Step 2: Understand the System**
- Read SINTRAN Commands Reference for system interaction
- Read SINTRAN III Monitor Calls for system call interface
- Read ND-500 Reference Manual for CPU architecture (if using ND-500)

**Step 3: Set Up Development Environment**
- QED or PED User Manual for text editing
- Linker User Guide for linking programs
- Workflow guides in [Developer/Workflow/](../Developer/Workflow/)

### For Building an Emulator

**Critical Manuals:**
1. **ND-05.009.4** - ND-500 CPU instruction set and architecture
2. **ND-10.004.01** - MPM5 hardware (5MPM shared memory)
3. **ND-860228-2-EN** - SINTRAN III Monitor Calls (system call interface)
4. **ND-60.047.03** - NPL language (to read SINTRAN source code)

**Workflow:**
1. Read hardware manuals for CPU and memory architecture
2. Cross-reference with [SINTRAN/NPL-SOURCE/](../SINTRAN/NPL-SOURCE/) actual source code
3. Use [SINTRAN/Emulator/](../SINTRAN/Emulator/) C# implementation guides
4. Validate against [SINTRAN/OS/](../SINTRAN/OS/) kernel documentation

### For System Programming

**Essential Manuals:**
- **NPL User's Guide** (ND-60.047.03) - Language reference
- **Monitor Calls** (ND-860228-2-EN) - System call interface
- **MAC Assembler** (ND-60.096.01) - Understanding NPL output
- **Linker Guide** (ND-860289-2-EN) - Linking and libraries

**Workflow:**
1. Write code in NPL
2. Compile to MAC assembly
3. Assemble to object files
4. Link with system libraries
5. Test and debug

### For Application Development

**Essential Manuals:**
- **Your chosen language manual** (PLANC, PASCAL, C, FORTRAN, COBOL, BASIC)
- **SINTRAN Commands Reference** - File I/O and system interaction
- **QED or PED User Manual** - Text editing
- **Linker Guide** - Linking applications

**Workflow:**
1. Read language manual for syntax
2. Check [Developer/Languages/Application/](../Developer/Languages/Application/) for quick start
3. Follow [Developer/Workflow/](../Developer/Workflow/) for compilation
4. Use SINTRAN Commands Reference for system operations

---

## 📋 Manual Quality & Versions

### OCR Sources

All manuals are high-quality OCR-scanned versions from original NORD/SINTRAN documentation:

- **Gandalf-OCR** - Primary OCR processing (high quality)
- **Tingo-OCR** - Alternative OCR processing
- **Web versions** - Some manuals have web-optimized versions (e.g., SINTRAN III Monitor Calls-WEB.md)

### Known Issues

| Issue | Affected Manuals | Notes |
|-------|------------------|-------|
| **Missing page** | ND-60.117.5 (PLANC) | One page missing, documented in source |
| **OCR artifacts** | Various | Minor formatting issues, generally readable |
| **Page numbers** | All | Original page numbers preserved |
| **Cross-references** | All | References use original document numbers |
| **Formatting variance** | All | Each manual has unique formatting from original |

### Version Notes

- **Multiple SINTRAN Monitor Calls versions** - Both standard and web versions available
- **Five FORTRAN manuals** - ND-60.011.04 (1974 standard), ND-60.145.7A (ANSI 77 extended), plus product/format notes ND-10191A (ND-100/NORD-10), ND-10190D (ND-500), ND-10033K (32-bit floating format)
- **Two BASIC manuals** - ND-60.040.02 (interactive) and ND-60.071.01D (compiled)
- **Two PED manuals** - English (ND-60.121.4) and Norwegian (ND-60.148.01)
- **Two SIBAS generations** - SIBAS I (ND-60.057.03) superseded by SIBAS II (ND-60.127.5, which replaces ND-60.057)

---

## 📊 Statistics

### By Category

| Category | Manuals | Total Lines | Average Lines/Manual |
|----------|---------|-------------|---------------------|
| Languages | 13 | ~55,000+ | ~4,200 |
| SINTRAN III System & OS | 14 | ~77,500+ | ~5,500 |
| ND-100 / ND-110 Hardware | 6 | ~35,900+ | ~6,000 |
| Database (SIBAS) | 4 | ~18,900+ | ~4,700 |
| System/Architecture | 4 | ~52,000+ | ~13,000 |
| Assemblers | 3 | ~10,000+ | ~3,300 |
| Editors | 4 | ~5,000+ | ~1,250 |
| Tools | 4 | ~8,000+ | ~2,000 |
| **Total** | **52** | **~238,000+** | **~4,600** |

### By Era

| Era | Manuals | Key Documents |
|-----|---------|---------------|
| **Early (1974-1975)** | 2 | NORD Standard FORTRAN, NORD BASIC |
| **Mid (1976-1980)** | 15 | Most language manuals, assemblers |
| **Late (1981-1985)** | 8 | SINTRAN III manuals, C compiler |

---

## 🔍 Quick Document Number Reference

| Doc Number | Title | Category | Lines |
|------------|-------|----------|-------|
| ND-05.009.4 | ND-500 Reference Manual | System/CPU | 16,324 |
| ND-06.007.01 | BIG MULTIPORT MEMORY SYSTEM | ND-100/110 HW | 3,072 |
| ND-06.014.2A | ND-100 Reference Manual | ND-100/110 HW | 6,231 |
| ND-06.015.02 | ND-100 Functional Description | ND-100/110 HW | 9,020 |
| ND-06.016.01 | NORD-100 Input/Output System | ND-100/110 HW | 5,713 |
| ND-06.026-1 | ND-110 Functional Description | ND-100/110 HW | 6,375 |
| ND-06.029.1 | ND-110 Instruction Set | ND-100/110 HW | 5,512 |
| ND-10.004.01 | MPM 5 Technical Description | Hardware | - |
| ND-10309A | PLANC FOR ND-100 | Language | 163 |
| ND-60.009.02 | MACM Assembler | Assembler | - |
| ND-60.011.04 | NORD Standard FORTRAN | Language | - |
| ND-60.031.04 | QED User Manual | Editor | - |
| ND-60.040.02 | NORD BASIC | Language | - |
| ND-60.047.03 | NORD PL User's Guide | System Language | - |
| ND-60.066.04 | ND Relocating Loader | Tools | - |
| ND-60.071.01D | NORD-10 BASIC Compiler | Language | 8,513 |
| ND-60.085.01 | BRF EDITOR | Editor | - |
| ND-60.096.01 | MAC Interactive Assembly | Assembler | 6,020 |
| ND-60.113.02 | NORD-500 Assembler | Assembler | 4,140 |
| ND-60.117.5 | PLANC Reference Manual | Language | - |
| ND-60.121.4 | PED User's Guide (EN) | Editor | - |
| ND-60.124.05 | ND-PASCAL User's Guide | Language | - |
| ND-60.136.04A | ND-500 Loader Monitor | System/ND-500 | - |
| ND-60.144.3 | COBOL Reference | Language | - |
| ND-60.145.7A | ND FORTRAN Reference | Language | - |
| ND-60.148.01 | PED Bruker-Veiledning (NO) | Editor | - |
| ND-60.214.01 | CC-100/500 C-Compiler | Compiler | - |
| ND-860228-2 | SINTRAN III Monitor Calls | System | 23,478 |
| ND-860289-2 | ND Linker User Guide | Tools | - |
| ND-10033K | FORTRAN 32 Bits Floating Format | Language | 377 |
| ND-10190D | FORTRAN for ND-500 | Language | 280 |
| ND-10191A | Fortran for ND-100 / NORD-10 | Language | 135 |
| ND-60.050.06 | SINTRAN III Users Guide | SINTRAN III | 11,589 |
| ND-60.051.8 | SINTRAN III - Real Time Loader | SINTRAN III | 3,430 |
| ND-60.072.02 | SINTRAN III RT Loader - System Doc | SINTRAN III | 1,620 |
| ND-60.112.01 | SINTRAN III System Doc, Appendix A - Data Fields | SINTRAN III | 4,455 |
| ND-60.125.04 | SINTRAN III Introduction | SINTRAN III | 3,245 |
| ND-60.128.5 | SINTRAN III Reference Manual | SINTRAN III | 21,694 |
| ND-60.132.03 | SINTRAN III Timesharing / Batch Guide | SINTRAN III | 6,616 |
| ND-60.133.02A | SINTRAN III Real Time Guide | SINTRAN III | 14,445 |
| ND-60.134.2 | SINTRAN III Communication Guide | SINTRAN III | 5,740 |
| ND-60.151.3 | SINTRAN III Utilities Manual | SINTRAN III | 2,257 |
| ND-60.174.Q01 | SINTRAN III Quick Reference Card | SINTRAN III | 537 |
| ND-10022S | SINTRAN Utility Programs | SINTRAN III | 1,158 |
| ND-10315B | SINTRAN III Accounting System | SINTRAN III | 584 |
| 211024C | SINTRAN III Configuration Program | SINTRAN III | 205 |
| ND-60.127.5 | SIBAS II - ND User Manual | Database | 13,080 |
| ND-30.009.3 | SIBAS II Operator Manual | Database | 3,548 |
| 210166F | SIBAS II for ND-100 | Database | 1,196 |
| ND-60.057.03 | SIBAS I Users Manual, Appendix A | Database | 1,063 |

---

## 🔗 Related Documentation

### Developer Resources
- **[Developer/README.md](../Developer/README.md)** - Main developer documentation entry point
- **[Developer/Languages/README.md](../Developer/Languages/README.md)** - Language-specific guides
- **[Developer/Workflow/README.md](../Developer/Workflow/README.md)** - Compilation and linking workflows
- **[Developer/Editors/README.md](../Developer/Editors/README.md)** - Editor documentation

### SINTRAN System
- **[SINTRAN/README.md](../SINTRAN/README.md)** - SINTRAN III system documentation
- **[SINTRAN/OS/README.md](../SINTRAN/OS/README.md)** - Operating system internals
- **[SINTRAN/NPL-SOURCE/README.md](../SINTRAN/NPL-SOURCE/README.md)** - Actual SINTRAN source code

### Quick Start
- **[Developer/SINTRAN-DEVELOPER-GUIDE.md](../Developer/SINTRAN-DEVELOPER-GUIDE.md)** - Comprehensive developer guide
- **[Developer/QUICK-START-EXAMPLES.md](../Developer/QUICK-START-EXAMPLES.md)** - Hello World examples

---

## 🤝 Contributing

When adding new reference manuals:

1. **Naming Convention**: `ND-XX.XXX.XX [Title].md`
2. **Remove OCR Suffixes**: Clean up `-Gandalf-OCR_combined` from filenames
3. **Check Duplicates**: Compare line counts and content quality before adding
4. **Update This README**: Add manual to appropriate category with description
5. **Cross-Reference**: Link to relevant developer guides
6. **Document Quality**: Note any missing pages or OCR issues

---

**Last Updated**: 2026-07-05
**Total Manuals**: 52
**Total Documentation Lines**: ~238,000+
**Status**: ✅ Complete Collection

---

*These manuals represent the complete technical documentation for the NORD/SINTRAN computing platform from the 1970s-1980s era.*
