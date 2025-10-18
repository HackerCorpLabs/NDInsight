# Programming Languages Documentation

This folder contains developer guides for all NORD/SINTRAN programming languages, organized by purpose.

---

## 📂 Organization

### [System/](System/) - System Programming Languages
Low-level languages for operating system development, device drivers, and system utilities.

| Language | Purpose | Guide |
|----------|---------|-------|
| **NPL** | NORD Programming Language - OS development | [NPL-DEVELOPER-GUIDE.md](System/NPL-DEVELOPER-GUIDE.md) |
| **MAC** | Macro Assembler - Low-level programming | [MAC-DEVELOPER-GUIDE.md](System/MAC-DEVELOPER-GUIDE.md) |
| **NORD-500 Assembler** | ND-500 assembly language | [NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](System/NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md) |

### [Application/](Application/) - Application Programming Languages
High-level languages for business, scientific, and general-purpose application development.

| Language | Purpose | Guide |
|----------|---------|-------|
| **C** | General-purpose programming | [C-DEVELOPER-GUIDE.md](Application/C-DEVELOPER-GUIDE.md) |
| **PLANC** | Structured programming (PLAN-oriented) | [PLANC-DEVELOPER-GUIDE.md](Application/PLANC-DEVELOPER-GUIDE.md) |
| **PASCAL** | Structured programming, education | [PASCAL-DEVELOPER-GUIDE.md](Application/PASCAL-DEVELOPER-GUIDE.md) |
| **FORTRAN** | Scientific and numerical computing | [FORTRAN-DEVELOPER-GUIDE.md](Application/FORTRAN-DEVELOPER-GUIDE.md) |
| **COBOL** | Business applications | [COBOL-DEVELOPER-GUIDE.md](Application/COBOL-DEVELOPER-GUIDE.md) |
| **BASIC** | Interactive programming, learning | [BASIC-DEVELOPER-GUIDE.md](Application/BASIC-DEVELOPER-GUIDE.md) |

---

## 🎯 Choosing a Language

### System Programming
**Choose NPL or MAC when:**
- Writing device drivers
- Developing OS components
- Need direct hardware access
- Require maximum performance
- Working with SINTRAN internals

**Choose NORD-500 Assembler when:**
- Programming the ND-500 coprocessor
- Need 32-bit operations
- Implementing specialized algorithms
- Working with ND-500 domains

### Application Development

**Choose C when:**
- Need portability
- Writing system utilities
- Require pointer manipulation
- Want modern programming constructs

**Choose PLANC when:**
- Structured business logic
- Need strong typing
- Want readable, maintainable code
- Working with modules

**Choose PASCAL when:**
- Educational projects
- Structured algorithms
- Type-safe development
- Academic computing

**Choose FORTRAN when:**
- Scientific computing
- Numerical analysis
- Matrix operations
- Engineering applications

**Choose COBOL when:**
- Business data processing
- File management systems
- Record-oriented applications
- Legacy system maintenance

**Choose BASIC when:**
- Quick prototyping
- Interactive applications
- Learning programming
- Simple calculations

---

## 📚 Reference Manuals

All developer guides are based on complete reference manuals located in `../../Reference-Manuals/`:

### System Languages
- **NPL**: `ND-60.047.03 NORD PL User's Guide.md`
- **MAC**: `ND-60.096.01 MAC Interactive Assembly and Debugging System User's Guide.md`
- **NORD-500 Asm**: `ND-60.113.02 EN Assembler Reference Manual.md`

### Application Languages
- **C**: `ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual.md`
- **PLANC**: `ND-60.117.5 EN PLANC Reference Manual.md`
- **PASCAL**: `ND-60.124.05 ND-PASCAL User's Guide.md`
- **FORTRAN**: `ND-60.145.7A EN ND FORTRAN Reference Manual.md`
- **COBOL**: `ND-60.144.3 EN COBOL Reference Manual.md`
- **BASIC**: `ND-60.071.01D NORD-10 BASIC Compiler Reference Manual.md`

---

## 🔄 Development Workflow

After choosing your language, follow the complete development workflow:

1. **Write** - Use QED or PED editor ([../Editors/](../Editors/))
2. **Compile** - Follow language-specific compilation ([../Workflow/COMPILER-COMMANDS-REFERENCE.md](../Workflow/COMPILER-COMMANDS-REFERENCE.md))
3. **Link** - Create executable programs ([../Workflow/LINKING-GUIDE.md](../Workflow/LINKING-GUIDE.md))
4. **Run** - Execute and debug your program
5. **Automate** - Use MODE scripts ([../Workflow/SCRIPT-GUIDE.md](../Workflow/SCRIPT-GUIDE.md))

---

## 📖 Documentation Structure

Each language guide follows a consistent structure:

- **Overview** - Language purpose and characteristics
- **Quick Start** - First program and basic concepts
- **Core Concepts** - Language-specific features
- **Compilation** - How to compile and run
- **Advanced Topics** - Expert-level features
- **Examples** - Practical code samples
- **Reference** - Links to complete manuals

---

## 🔗 Related Documentation

- **Main Developer Guide**: [../README.md](../README.md)
- **Editor Documentation**: [../Editors/](../Editors/)
- **Workflow Guides**: [../Workflow/](../Workflow/)
- **Reference Manuals**: [../../Reference-Manuals/](../../Reference-Manuals/)
- **SINTRAN OS**: [../../SINTRAN/OS/](../../SINTRAN/OS/)

---

**Last Updated**: October 18, 2025  
**Total Languages**: 9 (3 system + 6 application)  
**Documentation Status**: ✅ Complete structure, guides in progress

