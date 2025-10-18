# C Compiler Guide - CC-100/CC-500

**NORD C Compiler for ND-100 and ND-500**

**Version:** 1.0 (Placeholder)  
**Date:** October 17, 2025  
**Status:** Reference Guide

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 3 for complete Hello World example.**

### Minimal Example

```c
#include <stdio.h>

main()
{
    printf("HELLO FROM C!\n");
    return 0;
}
```

### Build Process

```bash
@CC-100 HELLO:C              # Compile C to BRF
@NRL                         # Start linker
*IMAGE 100                   # ND-100 target
*PROG-FILE "HELLO"
*LOAD CC-2HEADER             # C runtime header
*LOAD HELLO                  # Your program
*LOAD CC-2BANK               # C runtime library
*LOAD CC-2TRAILER            # C runtime trailer
*EXIT
@HELLO                       # Run
```

---

## Reference Manual

**Complete C compiler documentation:**

**Location:** `D:\OCR\ai\ND-60.214.01 CC-100 and CC-500 C-Compiler ND-100 500 User Manual-Gandalf-OCR\`

**Manual:** ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual

---

## Key Differences from Standard C

1. **Runtime Library Required:** Always link CC-2HEADER, CC-2BANK, CC-2TRAILER
2. **Memory Model:** 16-bit word-addressable on ND-100
3. **I/O Functions:** Standard stdio.h available
4. **Compiler Invocation:** `@CC-100` for ND-100, `@CC-500` for ND-500

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking C programs with runtime
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** `D:\OCR\ai\ND-60.214.01`

---

**For comprehensive C development, refer to the reference manual and QUICK-START-EXAMPLES.md.**

