# FORTRAN Developer Guide

**ND FORTRAN (ANSI 77) for SINTRAN III**

**Version:** 2.0  
**Date:** May 26, 2026  
**Status:** Verified on ND-100 hardware (compiler 203053F02)

---

## Installing FORTRAN

Four separate products, none with a located PD sheet — all documented from the floppies
themselves and marked accordingly. Pick the one matching your target machine and floppy set:

- **ND-100/NORD-10, older article** — [ND-10191](../../../Installation/Software/ND-10191/README.md)
  (product `ND-10191`), a 3-part floppy set. Two revisions found: a complete matched
  [ND-10191A](../../../Installation/Software/ND-10191/ND-10191A/README.md) set, and a partial
  [ND-10191D](../../../Installation/Software/ND-10191/ND-10191D/README.md) (PART2/PART3 only —
  see the product page for the open question about its missing PART1).
- **ND-100/NORD-10, later article** — [ND-210191](../../../Installation/Software/ND-210191/README.md)
  (product `ND-210191`), consolidated onto one floppy,
  [ND-210191F02](../../../Installation/Software/ND-210191/ND-210191F02/README.md).
- **ND-500** — [ND-210190](../../../Installation/Software/ND-210190/README.md) (product
  `ND-210190`), version
  [ND-210190K02](../../../Installation/Software/ND-210190/ND-210190K02/README.md) — ships a real
  ND-500 domain installer, unlike the plain-file ND-100 products; its exact dialogue has not been
  captured live yet.
- **NORD-10, original 48-bit compiler** (predecessor to ND-10191) —
  [ND-10023](../../../Installation/Software/ND-10023/README.md), version
  [ND-10023K](../../../Installation/Software/ND-10023/ND-10023K/README.md).

None of these installs have been run live in the emulator yet — every procedure is inferred from
floppy directory listings, not verified.

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

## Reference Manuals

**Complete FORTRAN language and runtime documentation** (all under [Reference-Manuals/](../../../Reference-Manuals/)):

| Manual | Document # | Covers |
|--------|-----------|--------|
| [ND FORTRAN Reference Manual](../../../Reference-Manuals/ND-60.145.7A%20EN%20ND%20FORTRAN%20Reference%20Manual.md) | ND-60.145.7A EN | ANSI 77 language reference (Sept 1986, Version 7A) - primary manual |
| [NORD Standard FORTRAN Reference Manual](../../../Reference-Manuals/ND-60.011.04%20NORD%20Standard%20FORTRAN%20Reference%20Manual.md) | ND-60.011.04 | Original NORD Standard FORTRAN (FORTRAN IV, 1974) |
| [Fortran for ND-100 / NORD-10](../../../Reference-Manuals/ND-10191A%20Fortran%20for%20ND-100-NORD-10.md) | ND-10191A | ND-100 / NORD-10 FORTRAN compiler product note |
| [FORTRAN for ND-500](../../../Reference-Manuals/ND-10190D%20FORTRAN%20FOR%20ND-500.md) | ND-10190D | FORTRAN targeting the ND-500 processor |
| [FORTRAN 32 Bits Floating Format](../../../Reference-Manuals/ND-10033K%20FORTRAN%2032%20BITS%20FLOATING%20FORMAT.md) | ND-10033K | 32-bit floating-point format used by FORTRAN |

**Primary manual:** ND-60.145.7A EN ND FORTRAN Reference Manual (September 1986, Version 7A)

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
- **Reference Manuals:** see the [Reference Manuals](#reference-manuals) table above (ND-60.145.7A, ND-60.011.04, ND-10191A, ND-10190D, ND-10033K)

---

**Last Updated:** May 26, 2026  
**Version:** 2.0
