# COBOL Developer Guide

**COBOL Programming Language for SINTRAN III**

**Version:** 1.0 (Placeholder)  
**Date:** October 18, 2025  
**Status:** Reference Guide

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 6 for complete Hello World example.**

### Minimal Example

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.

PROCEDURE DIVISION.
    DISPLAY 'HELLO FROM COBOL!'.
    STOP RUN.
```

### Build Process

```bash
@COBOL HELLO:COB            # Compile COBOL to BRF
@NRL                        # Start linker
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO                      # Run
```

---

## Reference Manual

**Complete COBOL language documentation:**

**Location:** `D:\OCR\ai\ND-60.144.3 EN COBOL Reference Manual-Gandalf-OCR_combined.md`

**Manual:** ND-60.144.3 EN COBOL Reference Manual

---

## Key Features

1. **ANSI COBOL Standard:** ANSI-74 COBOL compatible
2. **Business Applications:** Designed for business data processing
3. **SINTRAN Integration:** Full access to SINTRAN III facilities
4. **Compiler:** `@COBOL` for ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking COBOL programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** `D:\OCR\ai\ND-60.144.3`

---

**For comprehensive COBOL development, refer to the reference manual and QUICK-START-EXAMPLES.md.**


