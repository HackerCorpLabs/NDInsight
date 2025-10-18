# PASCAL Developer Guide

**PASCAL Programming Language for SINTRAN III**

**Version:** 1.0 (Placeholder)  
**Date:** October 18, 2025  
**Status:** Reference Guide

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 6 for complete Hello World example.**

### Minimal Example

```pascal
PROGRAM HELLO(OUTPUT);
BEGIN
    WRITELN('HELLO FROM PASCAL!');
END.
```

### Build Process

```bash
@PASCAL HELLO:PAS           # Compile PASCAL to BRF
@NRL                        # Start linker
*PROG-FILE "HELLO"
*LOAD HELLO
*LIBRARY PASLIB             # PASCAL runtime library
*EXIT
@HELLO                      # Run
```

---

## Reference Manual

**Complete PASCAL language documentation:**

**Location:** [Reference-Manuals/ND-60.124.05 ND-PASCAL User's Guide.md](../../../Reference-Manuals/ND-60.124.05%20ND-PASCAL%20User's%20Guide.md)

**Manual:** ND-60.124.05 ND-PASCAL User's Guide

---

## Key Features

1. **Standard Pascal:** ISO Standard Pascal compatible
2. **Structured Programming:** Strong type system and structured control flow
3. **SINTRAN Integration:** Full access to SINTRAN III facilities
4. **Compiler:** `@PASCAL` for ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking PASCAL programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **Reference Manual:** [ND-60.124.05 ND-PASCAL User's Guide](../../../Reference-Manuals/ND-60.124.05%20ND-PASCAL%20User's%20Guide.md)

---

**For comprehensive PASCAL development, refer to the reference manual and QUICK-START-EXAMPLES.md.**


