# SINTRAN III Developer Guide

**Complete Development Reference - Master Guide**

**Version:** 1.0  
**Date:** October 17, 2025  
**Status:** Complete

---

## Welcome to SINTRAN III Development

This is the **master guide** that ties together all SINTRAN III development documentation. Whether you're developing in NPL, C, assembler, or any other supported language, this guide will direct you to the right resources.

---

## 📚 Documentation Structure

### Core Documentation (Complete) ✅

| Document | Size | Description |
|----------|------|-------------|
| **[QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)** | 12KB | Hello World for all languages |
| **[NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md)** | 70KB | Complete NPL language guide |
| **[MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md)** | 21KB | MAC assembler reference |
| **[NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md)** | 30KB | NORD-500 assembly language |
| **[LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)** | 17KB | NRL, BRF, BPUN, PROG files |
| **[SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)** | 15KB | MODE files and automation |
| **[EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md)** | 15KB | Text editor selection and use |

### Language-Specific Guides

**Application Languages (Languages/Application/):**

| Language | Guide |
|----------|-------|
| **C** | [C-DEVELOPER-GUIDE.md](Languages/Application/C-DEVELOPER-GUIDE.md) |
| **PLANC** | [PLANC-DEVELOPER-GUIDE.md](Languages/Application/PLANC-DEVELOPER-GUIDE.md) |
| **PASCAL** | [PASCAL-DEVELOPER-GUIDE.md](Languages/Application/PASCAL-DEVELOPER-GUIDE.md) |
| **FORTRAN** | [FORTRAN-DEVELOPER-GUIDE.md](Languages/Application/FORTRAN-DEVELOPER-GUIDE.md) |
| **COBOL** | [COBOL-DEVELOPER-GUIDE.md](Languages/Application/COBOL-DEVELOPER-GUIDE.md) |
| **BASIC** | [BASIC-DEVELOPER-GUIDE.md](Languages/Application/BASIC-DEVELOPER-GUIDE.md) |

**System Languages (Languages/System/):**

| Language | Guide |
|----------|-------|
| **NPL** | [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) |
| **MAC** | [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) |
| **NORD-500 Assembler** | [NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md) |
| **NORD-500 Expert** | [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-EXPERT-GUIDE.md) |

---

## 🚀 Quick Start

### First Time Here?

**Start with these in order:**

1. **[QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)** - Get something running immediately
2. **[NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md)** Chapter 12 - Practical workflow
3. **[EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md)** - Choose your editor
4. **[SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)** - Automate your builds
5. **[LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)** - Understand the build process

**Total reading time:** ~2 hours for basics

### By Role

#### Application Developer

**Focus on:**
- [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) - Basic examples
- Your language guide (NPL, C, etc.)
- [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) - Automation

**Start with:**
- High-level languages (C, PLANC, FORTRAN)
- Standard development workflow

#### System Programmer

**Focus on:**
- [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) - System language
- [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) - Assembly language
- [NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md) - ND-500 assembly
- Low-level programming and optimization
- See `SINTRAN\OS\` for advanced system programming details

#### Emulator Developer

**Focus on:**
- System documentation in `SINTRAN\OS\` and `SINTRAN\Emulator\`
- Hardware specifications and architecture
- See repository README for complete emulator documentation

---

## 🛠️ Development Workflow

### Standard Build Process

```
┌─────────────────────────────────────────┐
│ 1. EDIT                                 │
│    @ED or @PED or @QED                  │
│    Create SOURCE:NPL (or .C, .MAC, etc) │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│ 2. COMPILE                              │
│    @NPL SOURCE:NPL → SOURCE:MAC         │
│    @CC-100 SOURCE:C → SOURCE:BRF        │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│ 3. ASSEMBLE (if NPL/MAC source)         │
│    @MAC SOURCE:MAC → SOURCE:BRF         │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│ 4. LINK                                 │
│    @NRL + commands → SOURCE:PROG/BPUN   │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│ 5. RUN                                  │
│    @SOURCE                              │
└─────────────────────────────────────────┘
```

### Automated Build (Recommended)

**Create: BUILD:MODE**
```mode
@NPL SOURCE:NPL
@MAC SOURCE:MAC
@NRL
PROG-FILE "SOURCE"
LOAD SOURCE
EXIT
```

**Run:**
```bash
@MODE BUILD:MODE
```

**See:** [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) for complete automation guide

---

## 📖 Language-Specific Guides

### NPL - NORD Programming Language

**Status:** ✅ Complete  
**Guide:** [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md)  
**Size:** 70KB, 3000+ lines

**When to use:**
- Operating system development
- Device drivers
- Performance-critical code
- System utilities

**Quick Start:**
```npl
SUBR HELLO, START
INTEGER ARRAY MSG:='HELLO NPL!', 15, 12
START:
    A:=43; T:="MSG"; *MONITOR 43
    A:=3; *MONITOR 3
RBUS
```

**Build:**
```bash
@NPL HELLO:NPL
@MAC HELLO:MAC
@NRL
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO
```

### MAC - Macro Assembler

**Status:** ✅ Complete  
**Guide:** [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md)  
**Size:** 21KB

**When to use:**
- Hand-optimized code
- Boot loaders
- Interrupt handlers
- Learning architecture

**Quick Start:**
```mac
START,  LDA =43
        LDT I (MSG)
        MONITOR 43
        LDA =3
        MONITOR 3
MSG,    'HELLO MAC!'
        15, 12
        )ENTR START
```

**Build:**
```bash
@MAC HELLO:MAC
@NRL
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO
```

### C - CC-100/CC-500

**Status:** ✅ Placeholder  
**Guide:** [C-DEVELOPER-GUIDE.md](Languages/Application/C-DEVELOPER-GUIDE.md)  
**Reference:** [ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual](../Reference-Manuals/ND-60.214.01%20CC-100%20and%20CC-500%20C-Compiler%20User%20Manual.md)

**When to use:**
- Portable applications
- Standard libraries
- Team development

**Quick Start:**
```c
#include <stdio.h>
main() {
    printf("HELLO C!\n");
    return 0;
}
```

**Build:**
```bash
@CC-100 HELLO:C
@NRL
*IMAGE 100
*PROG-FILE "HELLO"
*LOAD CC-2HEADER
*LOAD HELLO
*LOAD CC-2BANK
*LOAD CC-2TRAILER
*EXIT
@HELLO
```

### PLANC

**Status:** ✅ Placeholder  
**Guide:** [PLANC-DEVELOPER-GUIDE.md](Languages/Application/PLANC-DEVELOPER-GUIDE.md)  
**Reference:** [ND-60.117.5 EN PLANC Reference Manual](../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)

**When to use:**
- Structured programming
- Pascal-like syntax
- Business applications

**Quick Start:**
```planc
PROGRAM HELLO;
BEGIN
    WRITE('HELLO PLANC!');
    WRITELN;
END.
```

### Other Languages

**FORTRAN:** [FORTRAN-DEVELOPER-GUIDE.md](Languages/Application/FORTRAN-DEVELOPER-GUIDE.md)  
**PASCAL:** [PASCAL-DEVELOPER-GUIDE.md](Languages/Application/PASCAL-DEVELOPER-GUIDE.md)  
**COBOL:** [COBOL-DEVELOPER-GUIDE.md](Languages/Application/COBOL-DEVELOPER-GUIDE.md)  
**BASIC:** [BASIC-DEVELOPER-GUIDE.md](Languages/Application/BASIC-DEVELOPER-GUIDE.md)

See [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) for Hello World examples.

---

## 🔗 Linking and Binary Management

**Guide:** [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)

### Key Concepts

| Term | Meaning | Created By | Used By |
|------|---------|------------|---------|
| **BRF** | Binary Relocatable Format | MAC assembler | NRL linker |
| **PROG** | Executable program | NRL | SINTRAN (run directly) |
| **BPUN** | Binary punched | NRL | DUMP-REENTRANT |
| **Reentrant** | Shared program in memory | DUMP-REENTRANT | Multiple users |

### NRL Commands Quick Reference

```
@NRL
*IMAGE 100                   % Target ND-100
*PROG-FILE "NAME"            % Output file
*LOAD MODULE                 % Load BRF
*LIBRARY LIB                 % Load library
*MAP                         % Memory map
*EXIT                        % Done
```

### Reentrant Programs

**Create:**
```bash
@NRL
*BPUN-FILE "EDITOR"
*LOAD EDITOR
*EXIT
@DUMP-REENTRANT EDITOR:BPUN
```

**List:**
```bash
@LIST-REENTRANT
```

**Delete:**
```bash
@DELETE-REENTRANT EDITOR
```

---

## 🤖 Automation with MODE Files

**Guide:** [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)

### Basic MODE File

```mode
% BUILD:MODE - Automated build
@NPL SOURCE:NPL
@MAC SOURCE:MAC
@NRL
PROG-FILE "PROGRAM"
LOAD SOURCE
EXIT
@CC Build complete!
```

### Advanced Features

- **Output redirection:** `OUTPUT FILE: @LOG:TXT`
- **Input redirection:** `INPUT FILE: @COMMANDS:TXT`
- **Nested MODE files:** `@MODE SUBSCRIPT:MODE`
- **User interaction:** `?` (wait for input)
- **Comments:** `%` at start of line (not displayed)
- **Display messages:** `@CC message` (Comment Command - displayed during execution)

### Real-World Example

```mode
% Multi-module project build
OUTPUT FILE: @BUILD-LOG:TXT
@NPL MODULE1:NPL
@NPL MODULE2:NPL
@NPL MODULE3:NPL
@MAC MODULE1:MAC
@MAC MODULE2:MAC
@MAC MODULE3:MAC
OUTPUT FILE: @

@NRL
IMAGE 100
PROG-FILE "PROJECT"
LOAD MODULE1
LOAD MODULE2
LOAD MODULE3
LIBRARY SYSLIB
MAP
EXIT

@CC Project built successfully!
```

---

## 🐛 Debugging

### Tools

| Tool | Purpose | Command |
|------|---------|---------|
| **MAC Debugger** | Assembly-level debugging | `@MAC` with debug |
| **Symbolic Debugger** | High-level debugging | `@DEBUGGER` |
| **PERFORM** | Performance monitoring | `@PERFORM` |

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| **UNDEFINED SYMBOL** | External reference | Check `)EXTR` declarations |
| **PHASE ERROR** | Forward reference | Rearrange code |
| **MEMORY OVERFLOW** | Program too large | Use segments |
| **RUNTIME ERROR** | Program logic | Use debugger |

---

## 📊 File Extensions Reference

| Extension | Type | Created By | Purpose |
|-----------|------|------------|---------|
| `.NPL` | Source | Editor | NPL source code |
| `.C` | Source | Editor | C source code |
| `.MAC` | Assembly | NPL compiler | MAC assembly source |
| `.BRF` | Object | MAC assembler | Binary relocatable |
| `.PROG` | Executable | NRL | Program file |
| `.BPUN` | Executable | NRL | Binary punched |
| `.LST` | Listing | Compiler/Assembler | Human-readable listing |
| `.MODE` | Script | Editor | Batch script |
| `.LIB` | Library | BRF-EDITOR | Object library |

---

## 🎓 Learning Path

### Beginner (Week 1)

1. **Day 1-2:** Read [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md), run Hello World examples
2. **Day 3-4:** [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) Chapters 1-6
3. **Day 5:** [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) - Understand build process
4. **Day 6:** [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) - Automate builds
5. **Day 7:** Build a simple utility program

### Intermediate (Week 2-3)

1. [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) Chapters 7-14 - Advanced NPL
2. [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) - Assembly language
3. Build multi-module project
4. Create reentrant program
5. Try different application languages (C, PLANC, etc.)

### Advanced (Week 4+)

1. Master all language compilers
2. Device driver development
3. Interrupt handlers
4. System programming
5. Contribute to emulator

---

## 🔍 Quick Problem Solving

### "My program won't compile"

1. Check syntax in language guide
2. Verify file exists: `@LI`
3. Check for typos in identifiers
4. Look at error messages carefully

### "Link errors"

1. Check all modules compiled: `@LI *.BRF`
2. Verify external references: `)EXTR` declarations
3. Check for multiply defined symbols
4. See [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) Section 3

### "Program won't run"

1. Check if PROG file exists: `@LI *.PROG`
2. Verify entry point defined: `)ENTR`
3. Check memory requirements
4. Try with debugger: `@DEBUGGER`

### "Want to automate"

1. Create MODE file - See [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)
2. Test commands manually first
3. Add to MODE file one at a time
4. Use logging: `OUTPUT FILE: @LOG:TXT`

---

## 📚 Complete Documentation Index

### Local Documentation (Developer Directory)

**Core Guides:**
- **README.md** - Master index and navigation
- **SINTRAN-DEVELOPER-GUIDE.md** (This file) - Master technical guide
- **QUICK-START-EXAMPLES.md** (12KB) - Hello World all languages

**Languages/System/:**
- **NPL-DEVELOPER-GUIDE.md** (70KB) - Complete NPL guide
- **MAC-DEVELOPER-GUIDE.md** (21KB) - MAC assembler
- **NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md** (30KB) - NORD-500 assembly (Intro)
- **NORD-500-ASSEMBLER-EXPERT-GUIDE.md** (50KB) - NORD-500 assembly (Expert)

**Languages/Application/:**
- **PLANC-DEVELOPER-GUIDE.md** - PLANC guide
- **C-DEVELOPER-GUIDE.md** - C compiler guide
- **COBOL-DEVELOPER-GUIDE.md** - COBOL guide
- **FORTRAN-DEVELOPER-GUIDE.md** - FORTRAN guide
- **PASCAL-DEVELOPER-GUIDE.md** - PASCAL guide
- **BASIC-DEVELOPER-GUIDE.md** - BASIC guide

**Workflow/:**
- **LINKING-GUIDE.md** (17KB) - NRL linking
- **SCRIPT-GUIDE.md** (15KB) - MODE files
- **COMPILER-COMMANDS-REFERENCE.md** (18KB) - All commands
- **TOOLS-REFERENCE.md** (22KB) - Complete tools reference

**Editors/:**
- **EDITORS-GUIDE.md** (15KB) - Editor selection and usage
- **QED-QUICK-REFERENCE.md** - QED command reference
- **PED-QUICK-REFERENCE.md** - PED command reference

### OS Internals (Advanced - Optional)

**Location:** `SINTRAN\OS\`  
For system programming and emulator development - See repository README for details.

### Reference Manuals

See [Reference-Manuals/README.md](../Reference-Manuals/README.md) for complete list of all manuals:
- [NPL User's Guide](../Reference-Manuals/ND-60.047.03%20NORD%20PL%20User's%20Guide.md)
- [MAC Manual](../Reference-Manuals/ND-60.096.01%20MAC%20Interactive%20Assembly%20and%20Debugging%20System%20User's%20Guide.md)
- [CC-100/500 C Compiler](../Reference-Manuals/ND-60.214.01%20CC-100%20and%20CC-500%20C-Compiler%20User%20Manual.md)
- [PLANC Reference](../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md)
- [NRL Linker](../Reference-Manuals/ND-60.066.04%20ND%20Relocating%20Loader.md)

---

## 🎯 Next Steps

**After completing this guide:**

1. **Practice:** Build the examples in [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)
2. **Automate:** Create your first MODE file
3. **Explore:** Try different programming languages
4. **Create:** Build your own application
5. **Contribute:** Share your knowledge and improvements

---

## 📞 Support

**Documentation Issues:**
- Check GitHub repository for updates
- Cross-reference with official manuals
- Review related documents

**Getting Help:**
- Start with QUICK-START-EXAMPLES.md
- Check relevant language guide
- Review compilation and linking guides
- Use MODE files for reproducible builds

---

## 📈 Documentation Statistics

### Developer Guides (Complete)
- **Language Guides:** 9 (NPL, MAC, NORD-500 Asm, C, PLANC, PASCAL, FORTRAN, COBOL, BASIC)
- **Workflow Guides:** 4 (Compiler Commands, Linking, Scripts, Tools)
- **Editor Guides:** 3 (Editors Overview, QED, PED)
- **Quick Start:** Complete examples for all languages
- **Status:** ✅ Ready for development

### Total Documentation
- **Total Files:** 46+
- **Total Size:** 723KB+
- **Total Investment:** Comprehensive system documentation

---

## 🌟 Key Takeaways

1. **Start Simple:** Use QUICK-START-EXAMPLES.md to get running quickly
2. **Automate Early:** MODE files save time and reduce errors
3. **Understand Linking:** NRL and file formats are critical
4. **Know Your Tools:** NPL, MAC, NRL, MODE files work together
5. **Reference Manuals:** Use complete specifications when needed
6. **Practice:** Build real projects to learn effectively

---

**Version:** 1.0  
**Last Updated:** October 18, 2025  
**Status:** Complete

**Documentation Repository:** https://github.com/HackerCorpLabs/NDInsight  
**Developer Guides:** `Developer/` | **Reference Manuals:** `Reference-Manuals/`

---

*This guide ties together all SINTRAN III development documentation. Start with QUICK-START-EXAMPLES.md and work your way through based on your needs.*

