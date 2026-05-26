# FORTRAN Developer Guide

**ND FORTRAN (ANSI 77) for SINTRAN III**

**Version:** 2.0  
**Date:** May 26, 2026  
**Status:** Verified on ND-100 hardware (compiler 203053F02)

---

## Quick Start

**See [QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md) Section 5 for complete Hello World example.**

### Minimal Example

**File:** `HELLO:SYMB` (must have CRLF line endings and even parity)

Source files use `:SYMB` extension (default) or `:FORT`.

```fortran
      PROGRAM HELLO
      WRITE(1, 10)
   10 FORMAT(1X, 'HELLO FROM FORTRAN!')
      END
```

**FORTRAN syntax notes:**
- Fixed-form columns: 1 = comment (C), 1-5 = label, 6 = continuation, 7-72 = code
- Unit 1 = user's terminal on ND-100
- `1X` in FORMAT = one blank for carriage control (advance one line)
- All SINTRAN text files require CRLF line endings and even parity (bit 7)

### Build Process

**Interactive build (verified on ND-100, compiler 203053F02):**

```
@FORT
SEP OFF
COMP HELLO,,"HELLO"
EXIT
@BRF-LINKER
PROG-FILE "HELLO"
LOAD HELLO
LOAD FORT-1B
EXIT
@HELLO
```

**Step by step:**
1. `@FORT` -- launches the ANSI 77 FORTRAN compiler
2. `SEP OFF` -- disables separate data space (1-bank mode)
3. `COMP HELLO,,"HELLO"` -- compiles `HELLO:SYMB`, no listing, creates `HELLO:BRF` (quoted = create)
4. `EXIT` -- leaves the compiler
5. `@BRF-LINKER` -- launches the BRF linker
6. `PROG-FILE "HELLO"` -- sets output to `HELLO:PROG` (quoted = create)
7. `LOAD HELLO` -- loads the compiled module
8. `LOAD FORT-1B` -- loads FORTRAN 1-bank runtime (must be last)
9. `EXIT` -- writes the PROG file and exits
10. `@HELLO` -- runs the program

**Automated build (MODE file):**

File: `DO-BUILD:MODE`

```mode
@DELETE-FILE HELLO:PROG
@DELETE-FILE HELLO:BRF
@FORT
SEP OFF
COMP HELLO,,"HELLO"
EXIT
@BRF-LINKER
PROG-FILE "HELLO"
LOAD HELLO
LOAD FORT-1B
EXIT
@HELLO
```

Run with: `@MODE DO-BUILD:MODE,,`

### Build Output

- `HELLO:BRF` -- binary relocatable file (from FORT compiler)
- `HELLO:PROG` -- executable program (from BRF-LINKER)

### Quoting Rules

- Files being **created** (output) need `"quotes"`: `"HELLO"` in COMP and PROG-FILE
- Files being **read** (input/source) are unquoted: `HELLO` in COMP source and LOAD

---

## Two Compilers

The ND-100 may have two FORTRAN compilers installed:

| Compiler | Command | Version String | Notes |
|----------|---------|----------------|-------|
| **ND FORTRAN (ANSI 77)** | `@FORT` | `203053F02` | Recommended. Full ANSI 77 + ND extensions |
| **NORD Standard FORTRAN** | `@FTN` | `FTN-2090I` | Older (1979 era). Limited commands |

The ANSI 77 compiler (`@FORT`) supports `SEPARATE-DATA`, `COMP`, `PROG-FILE`, `$INCLUDE`, conditional compilation, and `ERRCODE` for monitor calls. The older compiler does not.

---

## Reference Manual

**Complete FORTRAN language documentation:**

**Location:** [Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)

**Manual:** ND-60.145.7A EN ND FORTRAN Reference Manual (September 1986, Version 7A)

---

## Key Features

1. **ANSI 77 Standard:** Full ANSI X3.9-1978 with ND extensions
2. **Monitor Calls:** ERRCODE variable and MONITOR CALL statement for SINTRAN access
3. **Extended Types:** INTEGER*2/4, REAL*4/6/8, DOUBLE COMPLEX, LOGICAL*2/4
4. **ND Extensions:** DO WHILE/ENDDO, semicolons, inline comments, 31-char names, bit intrinsics
5. **Compiler:** `@FORT` on ND-100

---

## See Also

- **[QUICK-START-EXAMPLES.md](../../QUICK-START-EXAMPLES.md)** - Complete working example
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Linking FORTRAN programs
- **[SINTRAN-DEVELOPER-GUIDE.md](../../SINTRAN-DEVELOPER-GUIDE.md)** - Master guide
- **[SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md)** - MODE file automation
- **Reference Manual:** [ND-60.145.7A EN ND FORTRAN Reference Manual](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md)

---

**Last Updated:** May 26, 2026  
**Version:** 2.0
