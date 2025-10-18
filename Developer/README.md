# SINTRAN III Development Documentation

**Complete developer documentation for SINTRAN III operating system**

**Version:** 3.0  
**Date:** October 18, 2025  
**Status:** Active Development

---

## 🚀 Quick Start (5 Minutes)

### New to SINTRAN III?

**Start here:**
1. Read **[SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
2. Try **[QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)** - Run Hello World
3. Explore your language guide in `Languages/`

### Build Your First Program

```bash
# 1. Create file HELLO:NPL with this content:
SUBR HELLO, START
INTEGER ARRAY MSG:='HELLO NPL!', 15, 12
START:
    A:=43; T:="MSG"; *MONITOR 43
    A:=3; *MONITOR 3
RBUS

# 2. Build and run:
@NPL HELLO:NPL
@MAC HELLO:MAC
@NRL
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO
```

**Result:** Your first SINTRAN program! 🎉

---

## 📁 Documentation Structure

```
Developer/
├── README.md (This file - Entry point)
├── SINTRAN-DEVELOPER-GUIDE.md (Master guide - READ THIS!)
├── QUICK-START-EXAMPLES.md (Hello World examples)
│
├── Languages/
│   ├── Application/ (Intro-level guides)
│   │   ├── PLANC-DEVELOPER-GUIDE.md
│   │   ├── C-DEVELOPER-GUIDE.md
│   │   ├── COBOL-DEVELOPER-GUIDE.md
│   │   ├── FORTRAN-DEVELOPER-GUIDE.md
│   │   ├── PASCAL-DEVELOPER-GUIDE.md
│   │   └── BASIC-DEVELOPER-GUIDE.md
│   │
│   └── System/ (Intro + Expert guides)
│       ├── NPL-DEVELOPER-GUIDE.md (Intro - 70KB)
│       ├── NPL-EXPERT-GUIDE.md (Future)
│       ├── MAC-DEVELOPER-GUIDE.md (Intro - 21KB)
│       ├── MAC-EXPERT-GUIDE.md (Future)
│       ├── NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md (Intro)
│       └── NORD-500-ASSEMBLER-EXPERT-GUIDE.md (Expert)
│
├── Workflow/ (Build and automation)
│   ├── COMPILER-COMMANDS-REFERENCE.md
│   ├── LINKING-GUIDE.md
│   ├── SCRIPT-GUIDE.md
│   └── TOOLS-REFERENCE.md
│
├── Editors/ (Text editor guides)
│   ├── EDITORS-GUIDE.md
│   ├── QED-QUICK-REFERENCE.md
│   └── PED-QUICK-REFERENCE.md
│
└── System-Development/ (Future - High Priority)
    ├── MONITOR-CALLS-GUIDE.md (Future)
    └── XMSG-DEVELOPMENT-GUIDE.md (Future)
```

---

## 📚 Core Documentation

### Master Guide

| Document | Purpose | Start Here If... |
|----------|---------|------------------|
| **[SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)** | Complete technical reference | You need detailed information |
| **[QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)** | Hello World all languages | You want to code NOW |

### Language Guides

#### Application Languages (Intro Level)

| Language | Guide | Status | When to Use |
|----------|-------|--------|-------------|
| **PLANC** | [PLANC-DEVELOPER-GUIDE.md](Languages/Application/PLANC-DEVELOPER-GUIDE.md) | ✅ | Pascal-like, structured programming |
| **C** | [C-DEVELOPER-GUIDE.md](Languages/Application/C-DEVELOPER-GUIDE.md) | ✅ | Portable applications |
| **COBOL** | [COBOL-DEVELOPER-GUIDE.md](Languages/Application/COBOL-DEVELOPER-GUIDE.md) | ✅ | Business applications |
| **FORTRAN** | [FORTRAN-DEVELOPER-GUIDE.md](Languages/Application/FORTRAN-DEVELOPER-GUIDE.md) | 🔄 | Scientific computing |
| **PASCAL** | [PASCAL-DEVELOPER-GUIDE.md](Languages/Application/PASCAL-DEVELOPER-GUIDE.md) | 🔄 | Education, structured programming |
| **BASIC** | [BASIC-DEVELOPER-GUIDE.md](Languages/Application/BASIC-DEVELOPER-GUIDE.md) | 🔄 | Beginner-friendly scripting |

#### System Programming Languages (Intro + Expert)

| Language | Intro Guide | Expert Guide | When to Use |
|----------|-------------|--------------|-------------|
| **NPL** | [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) (70KB) | *Future* | OS development, drivers |
| **MAC** | [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) (21KB) | *Future* | Assembly, optimization |
| **NORD-500 ASM** | [NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md) | [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](Languages/System/NORD-500-ASSEMBLER-EXPERT-GUIDE.md) | ND-500 CPU programming |

### Workflow & Tools

| Guide | Purpose | Size |
|-------|---------|------|
| **[COMPILER-COMMANDS-REFERENCE.md](Workflow/COMPILER-COMMANDS-REFERENCE.md)** | All compiler commands | 18KB |
| **[LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)** | NRL linker, BRF, PROG files | 17KB |
| **[SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)** | MODE files, automation | 15KB |
| **[TOOLS-REFERENCE.md](Workflow/TOOLS-REFERENCE.md)** | Complete tools reference | 22KB |

### Editors

| Editor | Quick Reference | Full Guide |
|--------|----------------|------------|
| **Overview** | - | [EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md) |
| **QED** | [QED-QUICK-REFERENCE.md](Editors/QED-QUICK-REFERENCE.md) | See EDITORS-GUIDE |
| **PED** | [PED-QUICK-REFERENCE.md](Editors/PED-QUICK-REFERENCE.md) | See EDITORS-GUIDE |
| **LED** | [NDWiki](https://www.ndwiki.org/wiki/LED_(Editor)) | ND-500 only |
| **TED** | [NDWiki](https://www.ndwiki.org/wiki/TED) | Limited documentation |

---

## 🎯 Learning Paths

### Application Developer

**Week 1:**
1. Run examples: [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)
2. Choose your language in `Languages/Application/`
3. Learn editors: [EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md)
4. Automate builds: [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)

**Focus on:**
- Your chosen language guide
- [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)
- [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)

### System Programmer

**Week 1-2:**
1. Master NPL: [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md)
2. Learn MAC: [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md)
3. Understand kernel: `SINTRAN/OS/` (GitHub repo)

**Week 3+:**
1. Device drivers: See kernel documentation
2. Interrupt handlers
3. NORD-500 programming (if applicable)

### Emulator Developer

**Focus on:**
- Complete kernel documentation: `SINTRAN/OS/`
- Memory map, scheduler, MMU
- C# implementation examples
- ~2,000 lines of reference code

---

## 🛠️ Development Workflow

```
┌─────────────────────────────────────┐
│ 1. EDIT SOURCE                       │
│    QED, PED, or LED                  │
│    See: Editors/EDITORS-GUIDE.md    │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ 2. COMPILE                           │
│    @NPL, @CC-100, @FORTRAN, etc     │
│    See: Workflow/COMPILER-COMMANDS  │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ 3. ASSEMBLE (if needed)             │
│    @MAC for NPL output              │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ 4. LINK                              │
│    @NRL → .PROG file                │
│    See: Workflow/LINKING-GUIDE.md   │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│ 5. RUN                               │
│    @PROGRAM                          │
└──────────────────────────────────────┘
```

**Or automate:** See [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)

---

## 🔍 Find What You Need

### By Task

| I want to... | Read this... |
|--------------|--------------|
| **Get started quickly** | [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) |
| **Learn a language** | `Languages/Application/` or `Languages/System/` |
| **Choose an editor** | [Editors/EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md) |
| **Link programs** | [Workflow/LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) |
| **Automate builds** | [Workflow/SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) |
| **Find commands** | [Workflow/COMPILER-COMMANDS-REFERENCE.md](Workflow/COMPILER-COMMANDS-REFERENCE.md) |
| **Understand tools** | [Workflow/TOOLS-REFERENCE.md](Workflow/TOOLS-REFERENCE.md) |
| **Deep dive** | [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md) |
| **Understand OS** | Kernel docs: `SINTRAN/OS/` (GitHub) |

### By Problem

| Problem | Solution |
|---------|----------|
| **Syntax errors** | Check your language guide |
| **Link errors** | [Workflow/LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) Section 3 |
| **Build automation** | [Workflow/SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) |
| **Command syntax** | [Workflow/COMPILER-COMMANDS-REFERENCE.md](Workflow/COMPILER-COMMANDS-REFERENCE.md) |
| **Editor help** | [Editors/EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md) |

### By File Extension

| Extension | Type | See Guide |
|-----------|------|-----------|
| `.NPL` | NPL source | [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) |
| `.MAC` | Assembly source | [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) |
| `.C` | C source | [C-DEVELOPER-GUIDE.md](Languages/Application/C-DEVELOPER-GUIDE.md) |
| `.BRF` | Object code | [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) |
| `.PROG` | Executable | [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) |
| `.MODE` | Build script | [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) |

---

## 📊 Documentation Statistics

### Local Documentation (This Directory)

- **Core Guides:** 3 files (README, SINTRAN-DEV-GUIDE, QUICK-START)
- **Language Guides:** 9+ files (Application + System)
- **Workflow:** 4 files (Compiler, Linker, Script, Tools)
- **Editors:** 3 files (Guide + 2 Quick References)
- **Total Size:** ~250KB+
- **Status:** ✅ Core complete, expanding

### Kernel Documentation (GitHub Repository)

- **Location:** `SINTRAN/OS/`
- **Files:** 31 comprehensive documents
- **Size:** 518KB
- **C# Code:** ~2,000 lines
- **Status:** ✅ Complete

### Combined Total

- **Files:** 50+
- **Size:** 768KB+
- **Coverage:** Complete SINTRAN III development
- **Languages:** 8 (NPL, MAC, NORD-500, C, PLANC, COBOL, FORTRAN, PASCAL, BASIC)

---

## 🎓 External Reference Manuals

| Manual | Location |
|--------|----------|
| **NPL User's Guide** | [Reference-Manuals/ND-60.047.03 NORD PL User's Guide.md](../Reference-Manuals/ND-60.047.03%20NORD%20PL%20User's%20Guide.md) |
| **MAC Reference** | [Reference-Manuals/ND-60.096.01 MAC Interactive Assembly User's Guide.md](../Reference-Manuals/ND-60.096.01%20MAC%20Interactive%20Assembly%20and%20Debugging%20System%20User's%20Guide.md) |
| **NORD-500 Assembler** | [Reference-Manuals/ND-60.113.02 EN Assembler Reference Manual.md](../Reference-Manuals/ND-60.113.02%20EN%20Assembler%20Reference%20Manual.md) |
| **CC-100/500 C Compiler** | [Reference-Manuals/ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual.md](../Reference-Manuals/ND-60.214.01%20CC-100%20and%20CC-500%20C-Compiler%20User%20Manual.md) |
| **PLANC Reference** | [Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md](../Reference-Manuals/ND-60.117.5%20EN%20PLANC%20Reference%20Manual.md) |
| **PASCAL User's Guide** | [Reference-Manuals/ND-60.124.05 ND-PASCAL User's Guide.md](../Reference-Manuals/ND-60.124.05%20ND-PASCAL%20User's%20Guide.md) |
| **COBOL Reference** | [Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md](../Reference-Manuals/ND-60.144.3%20EN%20COBOL%20Reference%20Manual.md) |
| **FORTRAN Reference** | [Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md) |
| **BASIC Reference** | [Reference-Manuals/ND-60.040.02 NORD BASIC Reference Manual.md](../Reference-Manuals/ND-60.040.02%20NORD%20BASIC%20Reference%20Manual.md) |
| **NRL Linker** | [Reference-Manuals/ND-60.066.04 ND Relocating Loader.md](../Reference-Manuals/ND-60.066.04%20ND%20Relocating%20Loader.md) |

---

## 🚧 Planned Documentation (Future)

### Expert-Level System Programming

- **NPL-EXPERT-GUIDE.md** - Deep dive into NPL
- **MAC-EXPERT-GUIDE.md** - Advanced assembly techniques
- Both will split from current comprehensive intro guides

### Critical System Development Topics

- **MONITOR-CALLS-GUIDE.md** - Complete MON call documentation (**High Priority**)
- **XMSG-DEVELOPMENT-GUIDE.md** - XMSG system development (**High Priority**)

These topics are essential for system-level development and will be comprehensively documented.

---

## ✅ Getting Started Checklist

Before you begin development:

- [ ] Read [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)
- [ ] Run at least one example from [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)
- [ ] Choose and learn your editor: [EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md)
- [ ] Understand the build process: [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md)
- [ ] Create your first MODE file: [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md)

If you can check all boxes, you're ready to develop! 🚀

---

## 🎯 Next Steps

**Choose your path:**

1. **New to SINTRAN?**  
   → [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)

2. **Want to code immediately?**  
   → [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)

3. **Need an editor?**  
   → [Editors/EDITORS-GUIDE.md](Editors/EDITORS-GUIDE.md)

4. **Ready for your language?**  
   → `Languages/Application/` or `Languages/System/`

5. **System programming?**  
   → Kernel docs in `SINTRAN/OS/` (GitHub)

---

## 📞 Support & Resources

**Documentation Issues:**
- Check GitHub repository for updates
- Cross-reference with official manuals in `D:\OCR\ai\`
- Review [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)

**Getting Help:**
1. Start with [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)
2. Check relevant language guide in `Languages/`
3. Review workflow guides in `Workflow/`
4. Consult kernel documentation if system-level issue

---

## 🌟 Key Features

1. ✅ **Organized Structure** - Clear categorization by purpose
2. ✅ **Complete Language Coverage** - 8 languages documented
3. ✅ **Practical Examples** - Working code you can use
4. ✅ **Editor Support** - Multiple editor options with guides
5. ✅ **Build Automation** - MODE files for efficiency
6. ✅ **System-Level Access** - Complete kernel documentation
7. ✅ **Expert Paths** - Intro and expert-level content

---

**Version:** 3.0  
**Last Updated:** October 18, 2025  
**Status:** Active Development  
**Total Documentation:** 768KB+ across 50+ files

**GitHub Repository:** https://github.com/[your-username]/NDInsight  
**Kernel Documentation:** `SINTRAN/OS/`

---

*Start your SINTRAN III development journey with [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md) or dive right into [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)!* 🚀
