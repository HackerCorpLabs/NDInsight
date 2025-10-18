# Application Programming Languages

High-level programming languages for business, scientific, and general-purpose application development on NORD/SINTRAN systems.

---

## 📝 Application Languages

### [C-DEVELOPER-GUIDE.md](C-DEVELOPER-GUIDE.md)
**C Programming Language**

Modern, portable system and application programming language.

**Key Features:**
- Pointer manipulation
- Structured programming
- Standard library
- Portable code
- System utilities

**Use Cases:**
- Cross-platform applications
- System utilities
- Data structures
- Algorithm implementation
- Modern programming

**Reference Manual:** `../../../Reference-Manuals/ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual.md`

---

### [PLANC-DEVELOPER-GUIDE.md](PLANC-DEVELOPER-GUIDE.md)
**PLANC (PLAN-oriented C)**

Structured programming language with strong typing and modularity.

**Key Features:**
- Module system
- Strong type checking
- Structured control flow
- Readable syntax
- Business logic

**Use Cases:**
- Business applications
- Modular systems
- Maintainable codebases
- Team development
- Structured projects

**Reference Manuals:**
- `../../../Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md`
- `../../../Reference-Manuals/ND-10309A PLANC FOR ND-100.md`

---

### [PASCAL-DEVELOPER-GUIDE.md](PASCAL-DEVELOPER-GUIDE.md)
**ND-PASCAL**

Educational and structured programming language.

**Key Features:**
- Strong typing
- Structured programming
- Clear syntax
- Educational design
- Algorithm focus

**Use Cases:**
- Educational programs
- Algorithm development
- Structured applications
- Academic computing
- Learning programming

**Reference Manual:** `../../../Reference-Manuals/ND-60.124.05 ND-PASCAL User's Guide.md`

---

### [FORTRAN-DEVELOPER-GUIDE.md](FORTRAN-DEVELOPER-GUIDE.md)
**NORD FORTRAN**

Scientific and numerical computing language.

**Key Features:**
- Array operations
- Mathematical functions
- Floating-point arithmetic
- Scientific notation
- Numerical libraries

**Use Cases:**
- Scientific computing
- Engineering calculations
- Numerical analysis
- Matrix operations
- Statistical analysis

**Reference Manuals:**
- `../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md`
- `../../../Reference-Manuals/ND-60.011.04 NORD Standard FORTRAN Reference Manual.md`

---

### [COBOL-DEVELOPER-GUIDE.md](COBOL-DEVELOPER-GUIDE.md)
**NORD COBOL**

Business-oriented programming language.

**Key Features:**
- English-like syntax
- Record processing
- File management
- Business calculations
- Report generation

**Use Cases:**
- Business data processing
- Financial systems
- Inventory management
- Report generation
- Legacy system maintenance

**Reference Manual:** `../../../Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md`

---

### [BASIC-DEVELOPER-GUIDE.md](BASIC-DEVELOPER-GUIDE.md)
**NORD BASIC**

Interactive, easy-to-learn programming language.

**Key Features:**
- Interactive development
- Simple syntax
- Line-oriented editing
- Quick prototyping
- Immediate execution

**Use Cases:**
- Quick scripts
- Interactive programs
- Learning programming
- Simple calculations
- Prototyping

**Reference Manuals:**
- `../../../Reference-Manuals/ND-60.071.01D NORD-10 BASIC Compiler Reference Manual.md`
- `../../../Reference-Manuals/ND-60.040.02 NORD BASIC Reference Manual.md`

---

## 🎯 Choosing the Right Language

### For General-Purpose Programming
**C** - Best for modern, portable applications with system-level access

### For Business Applications
- **COBOL** - Traditional business data processing
- **PLANC** - Modern structured business logic

### For Scientific Computing
**FORTRAN** - Numerical analysis, engineering, mathematics

### For Educational Projects
- **PASCAL** - Learning structured programming
- **BASIC** - Learning programming basics

### For Maintainable Systems
**PLANC** or **PASCAL** - Strong typing and clear structure

---

## 📊 Language Comparison

| Feature | C | PLANC | PASCAL | FORTRAN | COBOL | BASIC |
|---------|---|-------|--------|---------|-------|-------|
| **Typing** | Weak | Strong | Strong | Weak | Weak | Weak |
| **Speed** | Fast | Fast | Medium | Fast | Medium | Slow |
| **Learning Curve** | Medium | Medium | Easy | Medium | Medium | Easy |
| **Portability** | High | Low | Medium | High | High | Low |
| **Modern Features** | Yes | Yes | Yes | No | No | No |
| **Best For** | Systems | Business | Education | Science | Business | Learning |

---

## 🔄 Development Workflow

### 1. Choose Your Language
Select based on project requirements and team expertise.

### 2. Set Up Development Environment
```
@QED myprogram.c        - For C
@QED myprogram.planc    - For PLANC
@QED myprogram.pas      - For PASCAL
@QED myprogram.for      - For FORTRAN
@QED myprogram.cob      - For COBOL
@BASIC                  - For BASIC (interactive)
```

### 3. Write Your Program
Use QED or PED editor ([../../Editors/](../../Editors/))

### 4. Compile
Follow language-specific compilation ([../../Workflow/COMPILER-COMMANDS-REFERENCE.md](../../Workflow/COMPILER-COMMANDS-REFERENCE.md))

### 5. Link and Run
Create executables ([../../Workflow/LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md))

### 6. Automate
Use MODE scripts ([../../Workflow/SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md))

---

## 🚀 Quick Start Examples

### C Hello World
```c
#include <stdio.h>

int main() {
    printf("Hello, NORD!\n");
    return 0;
}
```

### PLANC Hello World
```planc
PROGRAM HelloWorld;
BEGIN
    WRITELN('Hello, NORD!');
END.
```

### PASCAL Hello World
```pascal
program HelloWorld;
begin
    writeln('Hello, NORD!');
end.
```

### FORTRAN Hello World
```fortran
      PROGRAM HELLO
      WRITE(*,*) 'Hello, NORD!'
      STOP
      END
```

### COBOL Hello World
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLOWORLD.
PROCEDURE DIVISION.
    DISPLAY 'Hello, NORD!'.
    STOP RUN.
```

### BASIC Hello World
```basic
10 PRINT "Hello, NORD!"
20 END
```

---

## 📚 Learning Resources

Each developer guide includes:
- **Quick Start** - First program and basic concepts
- **Language Syntax** - Complete syntax reference
- **Standard Library** - Available functions and procedures
- **File I/O** - Reading and writing files
- **Compilation** - How to build programs
- **Examples** - Practical code samples
- **Best Practices** - Writing maintainable code

---

## 🔗 Related Documentation

### Development Workflow
- **Editors**: [../../Editors/](../../Editors/)
- **Compilation**: [../../Workflow/COMPILER-COMMANDS-REFERENCE.md](../../Workflow/COMPILER-COMMANDS-REFERENCE.md)
- **Linking**: [../../Workflow/LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)
- **Scripts**: [../../Workflow/SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md)
- **Tools**: [../../Workflow/TOOLS-REFERENCE.md](../../Workflow/TOOLS-REFERENCE.md)

### Reference Manuals
- **All Manuals**: [../../../Reference-Manuals/](../../../Reference-Manuals/)

### System Documentation
- **SINTRAN Commands**: [../../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md](../../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md)

---

## 💡 Tips for Success

1. **Start Simple** - Begin with Hello World, then gradually add complexity
2. **Use Examples** - Study example code in each guide
3. **Follow Conventions** - Each language has its own style guidelines
4. **Test Frequently** - Compile and test often during development
5. **Read Errors** - Compiler messages are your friend
6. **Document Code** - Add comments explaining your logic
7. **Use Version Control** - Keep backups of working versions

---

**Last Updated**: October 18, 2025  
**Languages**: 6 (C, PLANC, PASCAL, FORTRAN, COBOL, BASIC)  
**Documentation Status**: ✅ Complete guides available

