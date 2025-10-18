# SINTRAN III Development - Quick Start Examples

**Purpose:** Immediate validation examples for each supported language  
**Date:** October 17, 2025  
**Status:** Complete

---

## Overview

This document provides minimal "Hello World" examples for each programming language supported on SINTRAN III. These examples are designed for immediate testing and validation.

**Languages covered:**
- NPL (NORD Programming Language)
- MAC (Assembler)
- C (CC-100/CC-500)
- PLANC
- FORTRAN
- PASCAL
- COBOL
- BASIC

---

## 1. NPL - NORD Programming Language

### 1.1 Hello World (NPL)

**File:** `HELLO-NPL:NPL`

```npl
% Simple Hello World in NPL

SUBR HELLO, START

INTEGER ARRAY MSG:='HELLO FROM NPL!', 15, 12

START:
    A:=43                    % WRTSW monitor call
    T:="MSG"
    *MONITOR 43
    A:=3                     % EXIT monitor call
    *MONITOR 3

RBUS
```

### 1.2 Build and Run

```bash
@NPL HELLO-NPL:NPL           # Compile to MAC
@MAC HELLO-NPL:MAC           # Assemble to BRF
@NRL                         # Start linker
*PROG-FILE "HELLO-NPL"
*LOAD HELLO-NPL
*EXIT
@HELLO-NPL                   # Run program
```

**Expected output:**
```
HELLO FROM NPL!
```

---

## 2. MAC - Assembler

### 2.1 Hello World (MAC)

**File:** `HELLO-MAC:MAC`

```mac
        % Simple Hello World in MAC Assembler
        
START,  LDA     =43          % WRTSW monitor call
        LDT     I (MSG)      % Message address
        MONITOR 43           % Write string
        LDA     =3           % EXIT monitor call
        MONITOR 3            % Exit program
        
MSG,    'HELLO FROM MAC!'
        15, 12               % CR, LF
        
        )ENTR START
```

### 2.2 Build and Run

```bash
@MAC HELLO-MAC:MAC           # Assemble to BRF
@NRL                         # Start linker
*PROG-FILE "HELLO-MAC"
*LOAD HELLO-MAC
*EXIT
@HELLO-MAC                   # Run program
```

**Expected output:**
```
HELLO FROM MAC!
```

---

## 3. C - CC-100/CC-500

### 3.1 Hello World (C)

**File:** `HELLO-C:C`

```c
/* Simple Hello World in C */

#include <stdio.h>

main()
{
    printf("HELLO FROM C!\n");
    return 0;
}
```

### 3.2 Build and Run

```bash
@CC-100 HELLO-C:C            # Compile C to BRF
@NRL                         # Start linker
*IMAGE 100                   # ND-100 image
*PROG-FILE "HELLO-C"
*LOAD CC-2HEADER             # C runtime header
*LOAD HELLO-C                # Your program
*LOAD CC-2BANK               # C runtime library
*LOAD CC-2TRAILER            # C runtime trailer
*EXIT
@HELLO-C                     # Run program
```

**Expected output:**
```
HELLO FROM C!
```

---

## 4. PLANC

### 4.1 Hello World (PLANC)

**File:** `HELLO-PLANC:PLANC`

```planc
PROGRAM HELLO;

BEGIN
    WRITE('HELLO FROM PLANC!');
    WRITELN;
END.
```

### 4.2 Build and Run

```bash
@PLANC-100-C                 # Start PLANC compiler
COMPILE HELLO-PLANC:PLANC
EXIT
@NRL                         # Start linker
*PROG-FILE "HELLO-PLANC"
*LOAD HELLO-PLANC
*EXIT
@HELLO-PLANC                 # Run program
```

**Expected output:**
```
HELLO FROM PLANC!
```

---

## 5. FORTRAN

### 5.1 Hello World (FORTRAN)

**File:** `HELLO-FTN:FTN`

```fortran
C     Simple Hello World in FORTRAN
      
      PROGRAM HELLO
      WRITE(6,100)
  100 FORMAT(' HELLO FROM FORTRAN!')
      STOP
      END
```

### 5.2 Build and Run

```bash
@FTN                         # Start FORTRAN compiler
HELLO-FTN:FTN
EXIT
@NRL                         # Start linker
*PROG-FILE "HELLO-FTN"
*LOAD HELLO-FTN
*LOAD FTNLIB                 # FORTRAN runtime library
*EXIT
@HELLO-FTN                   # Run program
```

**Expected output:**
```
 HELLO FROM FORTRAN!
```

---

## 6. PASCAL

### 6.1 Hello World (PASCAL)

**File:** `HELLO-PAS:PAS`

```pascal
PROGRAM HELLO(OUTPUT);

BEGIN
    WRITELN('HELLO FROM PASCAL!')
END.
```

### 6.2 Build and Run

```bash
@PASCAL                      # Start Pascal compiler
COMPILE HELLO-PAS:PAS
EXIT
@NRL                         # Start linker
*PROG-FILE "HELLO-PAS"
*LOAD HELLO-PAS
*LOAD PASCALLIB              # Pascal runtime library
*EXIT
@HELLO-PAS                   # Run program
```

**Expected output:**
```
HELLO FROM PASCAL!
```

---

## 7. COBOL

### 7.1 Hello World (COBOL)

**File:** `HELLO-COB:COB`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HELLO FROM COBOL!".
           STOP RUN.
```

### 7.2 Build and Run

```bash
@COBOL                       # Start COBOL compiler
HELLO-COB:COB
EXIT
@NRL                         # Start linker
*PROG-FILE "HELLO-COB"
*LOAD HELLO-COB
*LOAD COBOLLIB               # COBOL runtime library
*EXIT
@HELLO-COB                   # Run program
```

**Expected output:**
```
HELLO FROM COBOL!
```

---

## 8. BASIC

### 8.1 Hello World (BASIC)

**File:** `HELLO-BAS:BAS`

```basic
10 PRINT "HELLO FROM BASIC!"
20 END
```

### 8.2 Build and Run

```bash
@BASIC                       # Start BASIC
RUN HELLO-BAS:BAS
EXIT
```

**Expected output:**
```
HELLO FROM BASIC!
```

**Note:** BASIC is typically interpreted, not compiled.

---

## Quick Reference Table

| Language | Source Ext | Compiler | Output | Linker Needed | Runtime Lib |
|----------|-----------|----------|--------|---------------|-------------|
| **NPL** | `:NPL` | `@NPL` | `:MAC` → `:BRF` | Yes | No |
| **MAC** | `:MAC` | `@MAC` | `:BRF` | Yes | No |
| **C** | `:C` | `@CC-100` | `:BRF` | Yes | Yes (CC-2BANK) |
| **PLANC** | `:PLANC` | `@PLANC-100-C` | `:BRF` | Yes | Maybe |
| **FORTRAN** | `:FTN` | `@FTN` | `:BRF` | Yes | Yes (FTNLIB) |
| **PASCAL** | `:PAS` | `@PASCAL` | `:BRF` | Yes | Yes (PASCALLIB) |
| **COBOL** | `:COB` | `@COBOL` | `:BRF` | Yes | Yes (COBOLLIB) |
| **BASIC** | `:BAS` | `@BASIC` | Interpreted | No | Built-in |

---

## Common Build Patterns

### Pattern 1: Simple Single-File (NPL, MAC)

```bash
# Compile
@<COMPILER> SOURCE:EXT

# Assemble (if NPL)
@MAC SOURCE:MAC

# Link
@NRL
*PROG-FILE "PROGRAM"
*LOAD SOURCE
*EXIT

# Run
@PROGRAM
```

### Pattern 2: With Runtime Library (C, FORTRAN, PASCAL, COBOL)

```bash
# Compile
@<COMPILER> SOURCE:EXT

# Link with runtime
@NRL
*IMAGE 100
*PROG-FILE "PROGRAM"
*LOAD <RUNTIME-HEADER>    # If needed
*LOAD SOURCE
*LOAD <RUNTIME-LIB>
*LOAD <RUNTIME-TRAILER>   # If needed
*EXIT

# Run
@PROGRAM
```

### Pattern 3: Using MODE Files (Automation)

**File:** `BUILD:MODE`

```mode
@NPL SOURCE:NPL
@MAC SOURCE:MAC
@NRL
PROG-FILE "PROGRAM"
LOAD SOURCE
EXIT
```

**Run:**
```bash
@MODE BUILD:MODE
```

---

## Troubleshooting

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| **FILE NOT FOUND** | Source file missing | Check filename and extension |
| **SYNTAX ERROR** | Code syntax wrong | Check language syntax |
| **UNDEFINED SYMBOL** | External reference | Add library or define symbol |
| **OUT OF MEMORY** | Program too large | Reduce size or use segments |
| **RUNTIME ERROR** | Program logic error | Use debugger to trace |

### Verification Steps

1. **Compile successful?** - No errors reported
2. **Link successful?** - PROG file created
3. **File exists?** - Check with `@LI` (list files)
4. **Executable?** - Check with `@TY PROGRAM:PROG` (type file)
5. **Runs?** - Execute with `@PROGRAM`

---

## Next Steps

After validating these examples:

1. **NPL:** See [NPL-DEVELOPER-GUIDE.md](Languages/System/NPL-DEVELOPER-GUIDE.md) for complete language guide
2. **MAC:** See [MAC-DEVELOPER-GUIDE.md](Languages/System/MAC-DEVELOPER-GUIDE.md) for assembler details
3. **C:** See [C-DEVELOPER-GUIDE.md](Languages/Application/C-DEVELOPER-GUIDE.md) for C compiler guide
4. **PLANC:** See [PLANC-DEVELOPER-GUIDE.md](Languages/Application/PLANC-DEVELOPER-GUIDE.md) for PLANC guide
5. **Linking:** See [LINKING-GUIDE.md](Workflow/LINKING-GUIDE.md) for advanced linking
6. **Automation:** See [SCRIPT-GUIDE.md](Workflow/SCRIPT-GUIDE.md) for MODE files

---

## File Locations

**Quick Start Examples:** Current directory  
**Kernel Documentation:** `SINTRAN\OS\`  
**Developer Guides:** Will be created in Phase 2

---

**Last Updated:** October 17, 2025  
**Version:** 1.0  
**Status:** Complete

