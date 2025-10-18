# Reference Manuals

This folder contains complete reference manuals for NORD/SINTRAN system components, assemblers, and programming languages. These manuals serve as the authoritative source documentation for the developer guides in the `Developer/` folder.

## Purpose

- **Comprehensive Reference**: Complete technical documentation for all system components
- **Developer Resource**: Source material for creating developer guides and tutorials
- **Historical Archive**: Preservation of original NORD/SINTRAN documentation
- **Cross-Reference**: Detailed specifications for advanced users and system programmers

---

## Reference Manual Collection

### System & CPU Architecture

| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-05.009.4 EN ND-500 Reference Manual.md` | ND-500 CPU architecture and instruction set | 16,324 | CPU architecture, instruction set, domains, memory management |
| `SINTRAN-COMMANDS-REFERENCE.md` | Complete SINTRAN III command reference | 11,657 | System commands, batch processing, file management, MODE files |

### Assemblers

| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.009.02 MACM Mac Mass Storage Assembler.md` | MACM macro assembler for mass storage | - | Assembly, macros, linking |
| `ND-60.096.01 MAC Interactive Assembly and Debugging System User's Guide.md` | MAC assembler and debugger | 6,020 | Interactive assembly, debugging, BRF format |
| `ND-60.113.02 EN Assembler Reference Manual.md` | NORD-500 Assembler complete reference | 4,140 | ND-500 assembly, NRF format, structured programming, domains |

### System Programming Languages

| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.047.03 NORD PL User's Guide.md` | NORD Programming Language (NPL) reference | - | System programming, NPL syntax, MAC output, OS development |

### Application Languages

#### PLANC
| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.117.5 EN PLANC Reference Manual.md` | PLANC language reference manual | - | PLANC syntax, structured programming, modules |
| `ND-10309A PLANC FOR ND-100.md` | PLANC for ND-100 program description | 163 | Compiler setup, restrictions, runtime system |

#### PASCAL
| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.124.05 ND-PASCAL User's Guide.md` | ND-PASCAL language reference | - | PASCAL syntax, compiler, runtime |

#### COBOL
| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.144.3 EN COBOL Reference Manual.md` | COBOL language reference | - | COBOL syntax, business programming |

#### FORTRAN
| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.011.04 NORD Standard FORTRAN Reference Manual.md` | NORD Standard FORTRAN (1974) | - | FORTRAN IV, scientific computing |
| `ND-60.145.7A EN ND FORTRAN Reference Manual.md` | ND FORTRAN (later version) | - | Extended FORTRAN, optimization |

#### BASIC
| Manual | Description | Lines | Key Topics |
|--------|-------------|-------|------------|
| `ND-60.040.02 NORD BASIC Reference Manual.md` | NORD BASIC Reference Manual (1975) | - | BASIC language, interactive programming |
| `ND-60.071.01D NORD-10 BASIC Compiler Reference Manual.md` | NORD-10 BASIC Compiler | 8,513 | Compiled BASIC, optimization |

---

## Cross-Reference to Developer Guides

### System Languages (Developer Guides in `Developer/Languages/System/`)

| Language | Reference Manual(s) | Developer Guide | Expert Guide |
|----------|-------------------|-----------------|--------------|
| **NPL** | ND-60.047.03 NORD PL User's Guide | `NPL-DEVELOPER-GUIDE.md` | `NPL-EXPERT-GUIDE.md` (planned) |
| **MAC** | ND-60.096.01 MAC Interactive Assembly<br>ND-60.009.02 MACM | `MAC-DEVELOPER-GUIDE.md` | `MAC-EXPERT-GUIDE.md` (planned) |
| **NORD-500 Assembler** | ND-60.113.02 EN Assembler Reference<br>ND-05.009.4 EN ND-500 Reference | `NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md` | `NORD-500-ASSEMBLER-EXPERT-GUIDE.md` (planned) |

### Application Languages (Developer Guides in `Developer/Languages/Application/`)

| Language | Reference Manual(s) | Developer Guide |
|----------|-------------------|-----------------|
| **PLANC** | ND-60.117.5 EN PLANC Reference<br>ND-10309A PLANC FOR ND-100 | `PLANC-DEVELOPER-GUIDE.md` (placeholder) |
| **PASCAL** | ND-60.124.05 ND-PASCAL User's Guide | `PASCAL-DEVELOPER-GUIDE.md` (placeholder) |
| **COBOL** | ND-60.144.3 EN COBOL Reference | `COBOL-DEVELOPER-GUIDE.md` (placeholder) |
| **FORTRAN** | ND-60.011.04 / ND-60.145.7A | `FORTRAN-DEVELOPER-GUIDE.md` (placeholder) |
| **BASIC** | ND-60.040.02 / ND-60.071.01D | `BASIC-DEVELOPER-GUIDE.md` (placeholder) |

---

## How to Use These Manuals

### For Learning
1. **Start with Developer Guides** (`Developer/Languages/`) for introductory material
2. **Reference these manuals** for detailed specifications and edge cases
3. **Use the SINTRAN Commands Reference** when writing MODE files or system scripts

### For Development
1. **Keep manuals open** while coding for quick reference
2. **Search for specific topics** using your editor's search function
3. **Cross-reference with working examples** in the Developer guides

### For System Programming
1. **ND-500 Reference Manual** - Essential for understanding CPU architecture
2. **NORD PL User's Guide** - Complete NPL language specification
3. **MAC Assembler Guide** - Understanding the assembly output from NPL
4. **SINTRAN Commands** - System-level operations and batch processing

### For Application Development
1. **Choose your language manual** (PLANC, PASCAL, COBOL, FORTRAN, BASIC)
2. **Review SINTRAN Commands** for file I/O and system interaction
3. **Check Workflow guides** (`Developer/Workflow/`) for compilation and linking

---

## Manual Quality & Versions

All manuals in this collection are OCR-scanned versions of original NORD/SINTRAN documentation:

- **Gandalf-OCR**: High-quality OCR processing
- **Tingo-OCR**: Alternative OCR processing
- **Multiple versions**: Where available, the most complete version is kept

### Known Issues
- Some manuals may have OCR artifacts
- Page numbers are preserved from original documents
- Formatting may vary between manuals
- Cross-references to other manuals may use original document numbers
- **PLANC Reference Manual** (`ND-60.117.5`) has a missing page (noted in source)

---

## Document Numbers Quick Reference

| Doc Number | Title | Category |
|------------|-------|----------|
| ND-05.009.4 | ND-500 Reference Manual | System/CPU |
| ND-10309A | PLANC FOR ND-100 | Language |
| ND-60.009.02 | MACM Mac Mass Storage Assembler | Assembler |
| ND-60.011.04 | NORD Standard FORTRAN Reference Manual | Language |
| ND-60.040.02 | NORD BASIC Reference Manual | Language |
| ND-60.047.03 | NORD PL User's Guide | System Language |
| ND-60.071.01D | NORD-10 BASIC Compiler Reference | Language |
| ND-60.096.01 | MAC Interactive Assembly User's Guide | Assembler |
| ND-60.113.02 | NORD-500 Assembler Reference Manual | Assembler |
| ND-60.117.5 | PLANC Reference Manual | Language |
| ND-60.124.05 | ND-PASCAL User's Guide | Language |
| ND-60.144.3 | COBOL Reference Manual | Language |
| ND-60.145.7A | ND FORTRAN Reference Manual | Language |

---

## Contributing

When adding new reference manuals to this collection:

1. **Use consistent naming**: `ND-XX.XXX.XX [Title].md`
2. **Remove OCR suffixes**: Clean up `-Gandalf-OCR_combined` from filenames
3. **Check for duplicates**: Compare line counts and content quality
4. **Update this README**: Add the manual to the appropriate category
5. **Cross-reference**: Link to relevant developer guides

---

## Related Documentation

- **Developer Guides**: `../Developer/README.md` - Main entry point for all developer documentation
- **Workflow Guides**: `../Developer/Workflow/` - Compilation, linking, and build automation
- **Editors**: `../Developer/Editors/` - QED, PED, LED editor documentation
- **SINTRAN OS**: `../SINTRAN/OS/` - Operating system internals and kernel documentation
- **Quick Start**: `../Developer/QUICK-START-EXAMPLES.md` - Get started quickly with examples

---

**Last Updated**: October 18, 2025  
**Total Manuals**: 14  
**Total Documentation Lines**: ~50,000+

