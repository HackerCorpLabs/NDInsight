# Reference Manuals - Complete NORD/SINTRAN Documentation

**Authoritative reference manuals for all NORD/SINTRAN system components, programming languages, and development tools**

---

## 📖 Overview

This folder contains **25 complete reference manuals** spanning system architecture, programming languages, assemblers, editors, compilers, and development tools. These manuals serve as the primary source documentation for all NORD/SINTRAN development work.

### Purpose

- **Comprehensive Reference** - Complete technical documentation for all system components
- **Developer Resource** - Source material for creating developer guides and tutorials
- **Historical Archive** - Preservation of original NORD/SINTRAN documentation (1970s-1980s)
- **Cross-Reference** - Detailed specifications for advanced users and system programmers

### Total Collection

| Category | Manuals | Total Size |
|----------|---------|------------|
| **System & Architecture** | 4 | ~1.2MB |
| **Hardware** | 1 | 85KB |
| **Programming Languages** | 10 | ~400KB |
| **Assemblers** | 3 | ~200KB |
| **Compilers & Linkers** | 2 | ~150KB |
| **Editors** | 4 | ~100KB |
| **System Tools** | 1 | ~50KB |
| **Total** | **25** | **~2.2MB** |

---

## 🗂️ Complete Manual Index

### System & CPU Architecture

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **ND-500 Reference Manual** | ND-05.009.4 EN | 16,324 | Complete ND-500 CPU architecture, instruction set, domains, memory management |
| **SINTRAN III Monitor Calls** | ND-860228-2-EN | 23,478 | Complete system call reference - monitor routines, kernel interface, I/O operations |
| **SINTRAN Commands Reference** | - | 11,657 | All SINTRAN III commands - batch processing, file management, MODE files |
| **ND-500 Loader Monitor** | ND-60.136.04A | - | ND-500 program loading and monitor interface |

**Key Topics:** CPU architecture, instruction sets, system calls, command reference, monitor interface

---

### Hardware Documentation

| Manual | Document # | Lines | Description |
|--------|-----------|-------|-------------|
| **MPM 5 Technical Description** | ND-10.004.01 | - | Multiport Memory (5MPM) hardware specifications - critical for ND-500 integration |

**Key Topics:** 5MPM architecture, memory mapping, ND-100/ND-500 shared memory, hardware interface

**Related:** See [SINTRAN/OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../SINTRAN/OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md)

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
| **ND FORTRAN Reference Manual** | ND-60.145.7A EN | - | ND FORTRAN (later version) - Extended FORTRAN, optimization |

**Key Topics:** FORTRAN language, scientific computing, numerical analysis

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
| **FORTRAN** | ND-60.011.04, ND-60.145.7A | FORTRAN-DEVELOPER-GUIDE.md | 🚧 Placeholder |
| **BASIC** | ND-60.040.02, ND-60.071.01D | BASIC-DEVELOPER-GUIDE.md | 🚧 Placeholder |

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
- **Two FORTRAN manuals** - ND-60.011.04 (1974 standard) and ND-60.145.7A (extended version)
- **Two BASIC manuals** - ND-60.040.02 (interactive) and ND-60.071.01D (compiled)
- **Two PED manuals** - English (ND-60.121.4) and Norwegian (ND-60.148.01)

---

## 📊 Statistics

### By Category

| Category | Manuals | Total Lines | Average Lines/Manual |
|----------|---------|-------------|---------------------|
| Languages | 10 | ~30,000+ | ~3,000 |
| System/Architecture | 4 | ~52,000+ | ~13,000 |
| Assemblers | 3 | ~10,000+ | ~3,300 |
| Editors | 4 | ~5,000+ | ~1,250 |
| Tools | 4 | ~8,000+ | ~2,000 |
| **Total** | **25** | **~105,000+** | **~4,200** |

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

**Last Updated**: 2025-11-06
**Total Manuals**: 25
**Total Documentation Lines**: ~105,000+
**Status**: ✅ Complete Collection

---

*These manuals represent the complete technical documentation for the NORD/SINTRAN computing platform from the 1970s-1980s era.*
