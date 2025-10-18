# FORTRAN Developer Guide

**FORTRAN Programming Language for SINTRAN III**

**Version:** 1.0 (Placeholder)  
**Date:** October 18, 2025  
**Status:** Reference Guide

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 5 for complete Hello World example.**

### Minimal Example

```fortran
      PROGRAM HELLO
      WRITE(6,10)
   10 FORMAT(' HELLO FROM FORTRAN!')
      STOP
      END
```

### Build Process

```bash
@FORTRAN HELLO:FOR          # Compile FORTRAN to BRF
@NRL                        # Start linker
*PROG-FILE "HELLO"
*LOAD HELLO
*LIBRARY FORLIB             # FORTRAN runtime library
*EXIT
@HELLO                      # Run
```

---

## Reference Manual

**Complete FORTRAN language documentation:**

**Location:** `D:\OCR\ai\ND-60.145.7A EN ND FORTRAN Reference Manual-Gandalf-OCR\ND-60.145.7A EN ND FORTRAN Reference Manual-Gandalf-OCR_combined.md`

**Manual:** ND-60.145.7A EN ND FORTRAN Reference Manual

---

## Key Features

1. **FORTRAN 77 Standard:** ANSI FORTRAN 77 compatible
2. **Scientific Computing:** Designed for numerical and scientific applications
3. **SINTRAN Integration:** Full access to SINTRAN III facilities
4. **Compiler:** `@FORTRAN` for ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking FORTRAN programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** `D:\OCR\ai\ND-60.145.7A`

---

**For comprehensive FORTRAN development, refer to the reference manual and QUICK-START-EXAMPLES.md.**


