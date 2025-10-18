# Development Workflow & Tools

Complete guides for compiling, linking, building, and automating NORD/SINTRAN development workflows.

---

## 📖 Workflow Documentation

### [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md)
**Complete Compiler Command Reference**

Comprehensive reference for all NORD/SINTRAN compilers.

**Contents:**
- Compiler invocation syntax
- Command-line options
- Compiler switches and flags
- Output file formats
- Error messages
- Optimization options

**Covered Compilers:**
- **@NPL** - NORD Programming Language
- **@MAC** - Macro Assembler
- **@N500** - NORD-500 Assembler
- **@C** / **@CC** - C Compiler
- **@PLANC** - PLANC Compiler
- **@PASCAL** - Pascal Compiler
- **@FORTRAN** - FORTRAN Compiler
- **@COBOL** - COBOL Compiler
- **@BASIC** - BASIC Compiler

---

### [LINKING-GUIDE.md](LINKING-GUIDE.md)
**Complete Linking and Loading Guide**

Master the NORD relocating loader and build process.

**Contents:**
- **@LOAD** command syntax
- Object file formats (BRF, NRF)
- Library management
- Symbol resolution
- Memory layout
- Entry points
- Overlay structures
- Common linking errors

**Topics:**
- Creating executables
- Linking multiple modules
- Using system libraries
- Creating libraries
- Advanced linking techniques

---

### [SCRIPT-GUIDE.md](SCRIPT-GUIDE.md)
**MODE Scripts and Build Automation**

Automate your development workflow with MODE scripts.

**Contents:**
- MODE script syntax
- Script structure
- Variables and parameters
- Conditional execution
- Loops and iteration
- Error handling
- Build automation
- Multi-file projects

**Topics:**
- Creating build scripts
- Automating compilation
- Managing dependencies
- Testing automation
- Deployment scripts

---

### [TOOLS-REFERENCE.md](TOOLS-REFERENCE.md)
**Complete Development Tools Reference**

All SINTRAN development tools and utilities.

**Contents:**
- Debuggers
- Profilers
- Code analyzers
- File utilities
- System tools
- Cross-reference generators
- Documentation tools

**Covered Tools:**
- **@MAC-DEBUG** - Interactive debugger
- **@SIBAS** - System debugger
- **@DUMP** - Object file dumper
- **@XREF** - Cross-reference generator
- **@COMPARE** - File comparison
- **@PATCH** - Binary patching
- And many more...

---

## 🔄 Complete Development Workflow

### Phase 1: Write Code
```
@QED myprogram.npl      - Edit source file
```
See [../Editors/](../Editors/) for editor documentation.

### Phase 2: Compile
```
@NPL myprogram.npl      - Compile NPL to MAC
@MAC myprogram          - Assemble to object file
```
See [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md) for all compilers.

### Phase 3: Link
```
@LOAD myprogram         - Create executable
```
See [LINKING-GUIDE.md](LINKING-GUIDE.md) for linking details.

### Phase 4: Test
```
@myprogram              - Run your program
@MAC-DEBUG myprogram    - Debug if needed
```
See [TOOLS-REFERENCE.md](TOOLS-REFERENCE.md) for debugging tools.

### Phase 5: Automate
```
@myprogram:MODE         - Run build script
```
See [SCRIPT-GUIDE.md](SCRIPT-GUIDE.md) for automation.

---

## 🎯 Common Workflows

### Single-File Program (NPL)
```
@QED hello.npl          - Write code
@NPL hello.npl          - Compile
@MAC hello              - Assemble
@LOAD hello             - Link
@hello                  - Run
```

### Multi-File Project
```
@QED main.npl           - Write main module
@QED utils.npl          - Write utility module
@NPL main.npl           - Compile main
@NPL utils.npl          - Compile utils
@MAC main               - Assemble main
@MAC utils              - Assemble utils
@LOAD main,utils        - Link together
@main                   - Run
```

### Automated Build (MODE script)
```
! build:MODE - Build script
@NPL main.npl
@NPL utils.npl
@MAC main
@MAC utils
@LOAD main,utils
@MSG "Build complete!"
```

Run with: `@build:MODE`

---

## 📚 Language-Specific Workflows

### NPL Development
1. Write NPL source (`.npl`)
2. Compile with `@NPL` → produces MAC source
3. Assemble with `@MAC` → produces BRF object
4. Link with `@LOAD` → produces executable

### C Development
1. Write C source (`.c`)
2. Compile with `@C` → produces object file
3. Link with `@LOAD` → includes C runtime library
4. Run executable

### PLANC Development
1. Write PLANC source (`.planc`)
2. Compile with `@PLANC` → produces object file
3. Link with `@LOAD` → includes PLANC runtime
4. Run executable

### FORTRAN Development
1. Write FORTRAN source (`.for`)
2. Compile with `@FORTRAN` → produces object file
3. Link with `@LOAD` → includes FORTRAN library
4. Run executable

---

## 🛠️ Essential Tools

### Compilation Tools
- **@NPL** - NPL compiler
- **@MAC** - MAC assembler
- **@C** - C compiler
- **@PLANC** - PLANC compiler
- See [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md)

### Build Tools
- **@LOAD** - Relocating loader
- **@LIB** - Library manager
- **@MAKE** - Dependency tracking
- See [LINKING-GUIDE.md](LINKING-GUIDE.md)

### Debugging Tools
- **@MAC-DEBUG** - Interactive debugger
- **@SIBAS** - System debugger
- **@DUMP** - Object file analyzer
- See [TOOLS-REFERENCE.md](TOOLS-REFERENCE.md)

### Analysis Tools
- **@XREF** - Cross-reference
- **@PROFILE** - Performance profiler
- **@COMPARE** - File comparison
- See [TOOLS-REFERENCE.md](TOOLS-REFERENCE.md)

---

## 💡 Best Practices

### Organization
- ✅ Use separate directories for source, objects, and executables
- ✅ Name files consistently (e.g., `module.npl`, `module.mac`, `module.brf`)
- ✅ Group related files together

### Build Scripts
- ✅ Create MODE scripts for multi-file projects
- ✅ Include error checking in scripts
- ✅ Document script parameters and usage
- ✅ Version control your build scripts

### Compilation
- ✅ Enable all compiler warnings
- ✅ Fix warnings, don't ignore them
- ✅ Use optimization for production builds
- ✅ Keep debug symbols for development

### Linking
- ✅ Resolve all undefined symbols
- ✅ Use libraries for common code
- ✅ Check memory layout
- ✅ Verify entry points

### Testing
- ✅ Test after every compilation
- ✅ Use debugger for complex issues
- ✅ Create test scripts for regression testing
- ✅ Document known issues

---

## 🔗 Related Documentation

### Language Guides
- **System Languages**: [../Languages/System/](../Languages/System/)
- **Application Languages**: [../Languages/Application/](../Languages/Application/)

### Editor Documentation
- **Editors**: [../Editors/](../Editors/)

### Reference Manuals
- **SINTRAN Commands**: [../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md](../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md)
- **Loader Manual**: [../../Reference-Manuals/ND-60.066.04 ND Relocating Loader.md](../../Reference-Manuals/ND-60.066.04 ND Relocating Loader.md)
- **All Manuals**: [../../Reference-Manuals/](../../Reference-Manuals/)

### System Documentation
- **SINTRAN OS**: [../../SINTRAN/OS/](../../SINTRAN/OS/)

---

## 📋 Quick Reference

### Common Commands
```bash
@QED file               - Edit file
@NPL file.npl           - Compile NPL
@MAC file               - Assemble
@LOAD file              - Link
@file                   - Run
@DUMP file              - Examine object
@XREF file              - Generate cross-ref
@MAC-DEBUG file         - Debug program
```

### File Extensions
| Extension | Description |
|-----------|-------------|
| `.npl` | NPL source code |
| `.mac` | MAC assembly source |
| `.brf` | Binary Relocatable Format (object) |
| `.nrf` | NORD-500 Relocatable Format |
| `.c` | C source code |
| `.planc` | PLANC source code |
| `.for` | FORTRAN source code |
| `.cob` | COBOL source code |
| `:MODE` | MODE script |

---

## 🚀 Getting Started

1. **Choose a language** - See [../Languages/](../Languages/)
2. **Learn your editor** - See [../Editors/](../Editors/)
3. **Write a simple program** - Start with Hello World
4. **Follow the workflow** - Compile → Link → Run
5. **Create build scripts** - Automate repetitive tasks
6. **Use debugging tools** - Fix issues efficiently

---

**Last Updated**: October 18, 2025  
**Documentation Files**: 4 complete guides  
**Documentation Status**: ✅ Complete workflow coverage

