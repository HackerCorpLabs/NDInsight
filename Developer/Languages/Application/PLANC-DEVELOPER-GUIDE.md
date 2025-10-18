# PLANC Developer Guide

**PLANC Programming Language for SINTRAN III**

**Version:** 1.0 (Placeholder)  
**Date:** October 17, 2025  
**Status:** Reference Guide

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) Section 4 for complete Hello World example.**

### Minimal Example

```planc
PROGRAM HELLO;

BEGIN
    WRITE('HELLO FROM PLANC!');
    WRITELN;
END.
```

### Build Process

```bash
@PLANC-100-C                 # Start PLANC compiler
COMPILE HELLO-PLANC:PLANC
EXIT
@NRL                         # Start linker
*PROG-FILE "HELLO-PLANC"
*LOAD HELLO-PLANC
*EXIT
@HELLO-PLANC                 # Run
```

---

## Reference Manual

**Complete PLANC language documentation:**

**Location:** `D:\OCR\ai\ND-60.117.5 EN PLANC Reference Manual-missing page-Gandalf-OCR\`

**Manual:** ND-60.117.5 EN PLANC Reference Manual

---

## Key Features

1. **Pascal-like Syntax:** Structured programming
2. **Strong Typing:** Type-safe development
3. **SINTRAN Integration:** Full system access
4. **Compiler:** `@PLANC-100-C` for ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](LINKING-GUIDE.md)** - Linking PLANC programs
- **[SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** `D:\OCR\ai\ND-60.117.5`

---

**For comprehensive PLANC development, refer to the reference manual and QUICK-START-EXAMPLES.md.**

