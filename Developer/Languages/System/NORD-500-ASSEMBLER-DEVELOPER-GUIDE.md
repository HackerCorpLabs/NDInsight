# NORD-500 Assembler Developer Guide

**Introduction to NORD-500 Assembly Language Programming**

**Version:** 1.0  
**Date:** October 18, 2025  
**Status:** Complete - Introduction Level

**For Expert-Level Content:** See [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](NORD-500-ASSEMBLER-EXPERT-GUIDE.md)

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [NORD-500 System Architecture](#2-nord-500-system-architecture)
3. [When to Use NORD-500 Assembly](#3-when-to-use-nord-500-assembly)
4. [Development Environment](#4-development-environment)
5. [Language Fundamentals](#5-language-fundamentals)
6. [Module Structure](#6-module-structure)
7. [Data Types and Storage](#7-data-types-and-storage)
8. [Instructions and Addressing Modes](#8-instructions-and-addressing-modes)
9. [Practical Development Workflow](#9-practical-development-workflow)
10. [Example Programs](#10-example-programs)
11. [Common Patterns](#11-common-patterns)
12. [Differences from MAC Assembler](#12-differences-from-mac-assembler)
13. [Next Steps](#13-next-steps)

---

## 1. Introduction

### 1.1 What is the NORD-500 Assembler?

The **NORD-500 Assembler** is a structured, high-level assembly language designed for programming the NORD-500 CPU. Unlike traditional flat assemblers, it provides:

- **Structured programming features** (MODULE, ROUTINE, STACK, RECORD)
- **Advanced data types** (8-bit, 16-bit, 32-bit, 64-bit, floating point)
- **Sophisticated addressing modes** (10 different modes)
- **Macro processing** and conditional assembly
- **Cross-assembler architecture** (runs on NORD-100, targets NORD-500)

**Key characteristics:**
- Two-pass assembler
- Produces NORD Relocatable Format (NRF) object code
- Runs under SINTRAN III on NORD-100 CPU
- Generates code for NORD-500 CPU

### 1.2 Design Philosophy

> **The NORD-500 Assembler bridges the gap between high-level languages and machine code, providing structure without sacrificing control.**

**Core principles:**
1. **Structured** - Module and routine organization
2. **Type-aware** - Explicit data type specifications
3. **Readable** - Self-documenting syntax
4. **Powerful** - Full hardware access
5. **Linkable** - Modular program development

---

## 2. NORD-500 System Architecture

### 2.1 Dual-CPU Configuration

```
┌─────────────┐          ┌─────────────┐
│  NORD-100   │          │  NORD-500   │
│    CPU      │◄────────►│    CPU      │
│             │          │             │
│  SINTRAN III│          │             │
│  Assembler  │          │             │
└──────┬──────┘          └──────┬──────┘
       │                        │
       └────────┬───────────────┘
                │
        ┌───────▼────────┐
        │ Shared Memory  │
        │   (5MPM)       │
        └────────────────┘
```

**Components:**
- **NORD-100 CPU:** Runs SINTRAN III OS and cross-assembler
- **NORD-500 CPU:** High-performance processor executing compiled code
- **Shared Memory (5MPM):** Communication between processors

### 2.2 NORD-500 CPU Features

| Feature | Description |
|---------|-------------|
| **Architecture** | 32-bit RISC-style processor |
| **Registers** | Integer accumulators (W1-W4), Float accumulators (F1-F4, D1-D4) |
| **Addressing** | B-register (base), R-register (record), X-register (index) |
| **Data Types** | Bit, Byte, Half-word, Word, Float, Double |
| **Memory** | Separate program and data segments |

---

## 3. When to Use NORD-500 Assembly

### 3.1 Ideal Use Cases

**Use NORD-500 Assembly for:**
- High-performance numerical computing
- ND-500 system programming
- Real-time control systems
- CPU-intensive algorithms
- Direct hardware control on ND-500

**Examples:**
- Scientific computation kernels
- Graphics and signal processing
- Database engines
- Network protocol processors
- Operating system components (ND-500 side)

### 3.2 When to Use Alternatives

| Use Case | Recommended Language |
|----------|---------------------|
| ND-100 system programming | NPL |
| ND-100 device drivers | NPL or MAC |
| ND-100 applications | NPL, C, PLANC |
| Portability required | C, FORTRAN, PASCAL |
| Business applications | COBOL |
| Quick scripts | BASIC |

---

## 4. Development Environment

### 4.1 Assembly Workflow

```
┌────────────────────────────────────┐
│ 1. EDIT SOURCE                      │
│    @PED SOURCE:SYMB                 │
│    (Use QED, PED, or LED)          │
└──────────────┬─────────────────────┘
               │
┌──────────────▼─────────────────────┐
│ 2. ASSEMBLE (on NORD-100)          │
│    @NORD-500-ASSEMBLER              │
│    SOURCE:SYMB → SOURCE:NRF         │
└──────────────┬─────────────────────┘
               │
┌──────────────▼─────────────────────┐
│ 3. LINK (NORD-500 Loader)          │
│    NRF → Executable                 │
│    Creates PSEG, DSEG, LINK files   │
└──────────────┬─────────────────────┘
               │
┌──────────────▼─────────────────────┐
│ 4. EXECUTE (on NORD-500)           │
│    Program runs on ND-500 CPU       │
└────────────────────────────────────┘
```

### 4.2 File Extensions

| Extension | Description | Created By |
|-----------|-------------|------------|
| `.SYMB` | Source code | Editor |
| `:NRF` | Object code (NORD Relocatable Format) | Assembler |
| `:PSEG` | Program segment | Loader |
| `:DSEG` | Data segment | Loader |
| `:LINK` | Link information | Loader |
| `:LST` | Assembly listing | Assembler |

---

## 5. Language Fundamentals

### 5.1 Source File Format

**Basic rules:**
- ASCII character set
- Case insensitive (converted to uppercase)
- `%` starts a comment (to end of line)
- `&` continues statement on next line
- `;` or newline terminates statement
- Blank lines allowed

**Example:**
```asm
MODULE EXAMPLE              % Module name
    W DATA 100, 200         % Data definition
    F DATA 3.14159          % Float constant
    MAIN START              % Main entry point
ENDMODULE
```

### 5.2 Identifiers

**Rules:**
- Start with letter, `?`, or `#`
- Contain letters, digits, `#`, `_`
- First 16 characters significant
- No double underscores (`__`)
- `?` prefix makes symbol invisible (not in symbol table)

**Examples:**
```asm
VALID_NAME          % Standard identifier
BUFFER_SIZE         % With underscore
?TEMP_VAR           % Invisible symbol
#PCLC               % Intrinsic function
```

### 5.3 Constants

**Integer constants:**
```asm
1234                % Decimal (default)
1234D               % Decimal explicit
1234B               % Octal
0FFH                % Hexadecimal (must start with digit)
1010X               % Binary
```

**Real constants:**
```asm
3.14159             % Float
1.23E-4             % Scientific notation
```

**String constants:**
```asm
'HELLO WORLD'       % String
'CAN''T'            % Embedded quote (doubled)
```

### 5.4 Data Type Specifiers

| Specifier | Size | Description |
|-----------|------|-------------|
| `BI` | 1 bit | Bit |
| `BY` | 8 bits | Byte |
| `H` | 16 bits | Half-word |
| `W` | 32 bits | Word (integer) |
| `F` | 32 bits | Single precision float |
| `D` | 64 bits | Double precision float |

---

## 6. Module Structure

### 6.1 Basic Module

```asm
MODULE module-name [, priority [, language-code]]

% Declarations and definitions
IMPORT-P external-routines
IMPORT-D external-data
EXPORT public-symbols

% Data definitions
W DATA 100, 200, 300

% Routines
ROUTINE ENTRY1, ENTRY2
    % Code here
ENDROUTINE

ENDMODULE [module-name]
```

### 6.2 Module Components

**MODULE declaration:**
```asm
MODULE EXAMPLE              % Simple
MODULE MATHLIB, 50          % With priority
MODULE GRAPHICS, 100, 0     % Priority + language code
```

**Parameters:**
- **priority:** 0-255, default 0 (used by loader)
- **language-code:** 0=Assembly, 1=FORTRAN, 2=PLANC

**IMPORT/EXPORT:**
```asm
IMPORT-P SQRT, PRINTF       % Import procedures
IMPORT-D GLOBAL_DATA        % Import data

EXPORT INIT, PROCESS        % Export procedures
EXPORT BUFFER, STATUS       % Export data
```

**MAIN entry point:**
```asm
MAIN START                  % Specify main entry
```

### 6.3 Routine Structure

```asm
ROUTINE ROUTINE_NAME, ENTRY2, ENTRY3

% Local data (STACK or RECORD)
STACK
    LOCAL1: W BLOCK 1
    LOCAL2: W BLOCK 1
ENDSTACK

% Code
ROUTINE_NAME:
    ENTS #SCLC              % Enter stack subroutine
    % ... code ...
    RET                     % Return

ENTRY2:
    % Alternative entry point
    RET

ENDROUTINE
```

---

## 7. Data Types and Storage

### 7.1 Simple Data Definition

**DATA directive:**
```asm
VALUE: W DATA 12345         % Single word
TABLE: W DATA 100, 200, 300 % Multiple values
PI:    F DATA 3.14159       % Float
NAME:  BY DATA 'JOHN'       % String as bytes
```

**BLOCK directive (reserve space):**
```asm
BUFFER: W BLOCK 100         % Reserve 100 words
TEMP:   BY BLOCK 256        % Reserve 256 bytes
```

### 7.2 STACK (Local Variables)

**Fixed stack (static allocation):**
```asm
LOCALS: STACK FIXED
    COUNT:  W DATA 0        % Initialized to 0
    TOTAL:  W DATA 100      % Initialized to 100
    BUFFER: W BLOCK 50      % 50 words reserved
ENDSTACK

ROUTINE_NAME:
    ENTF LOCALS, 0          % Enter with fixed stack
    W MOVE B.COUNT, W1      % Access via B-register
    RET
```

**Dynamic stack (runtime allocation):**
```asm
STACK
    PARAM1: W BLOCK 1       % Parameter 1 address
    PARAM2: W BLOCK 1       % Parameter 2 address
    LOCAL1: W BLOCK 1       % Local variable
ENDSTACK

ROUTINE_NAME:
    ENTS #SCLC              % Enter stack (#SCLC = stack size)
    W MOVE IND(B.PARAM1), W1
    RET
```

### 7.3 RECORD (Data Structures)

```asm
% Define record structure
RECORD
    NEXT:   W BLOCK 1       % Offset 0 (Next pointer)
    PREV:   W BLOCK 1       % Offset 4 (Previous pointer)
    DATA1:  W BLOCK 1       % Offset 8
    DATA2:  W BLOCK 1       % Offset 12
ENDRECORD

% Use record structure
NODE: RECORD FIXED
    NEXT:   W DATA 0
    PREV:   W DATA 0
    DATA1:  W DATA 100
    DATA2:  W DATA 200
ENDRECORD

% Access record fields
R := ADDR(NODE)
W1 := R.NEXT                % Load NEXT field
W2 := R.DATA1               % Load DATA1 field
```

### 7.4 Arrays

```asm
% Array declaration
TABLE: W BLOCK 100          % 100-word array

% Access array elements
W1 := TABLE(10)             % Element 10
W1 := TABLE(W2)             % Element at index in W2

% String array
TEXT: BY DATA 'HELLO WORLD'

% Descriptor-based array
DESC_ARRAY: DESC 100, BASE_ADDR
```

---

## 8. Instructions and Addressing Modes

### 8.1 Instruction Format

```asm
[label:] [data-type][register] instruction [operands]
```

**Examples:**
```asm
START:   W1 := 100                  % Load constant to W1
         W2 := IND(B.VAR1)          % Load from memory
         W ADD2 W1, W2               % Add W1 and W2
         F1 := R.X                   % Load float from record
```

### 8.2 Basic Addressing Modes

**1. Register:**
```asm
W1 := W2                    % Register to register
```

**2. Constant:**
```asm
W1 := 12345                 % Immediate constant
F1 := 3.14159               % Float constant
```

**3. Local (B-relative):**
```asm
W1 := B.OFFSET              % Load from stack/local
W1 := B.12                  % Explicit offset
```

**4. Absolute:**
```asm
W1 := GLOBAL_VAR            % Absolute address
```

**5. Indirect:**
```asm
W1 := IND(B.PTR)            % Indirect via pointer
```

**6. Indexed:**
```asm
W1 := TABLE(W2)             % Array indexing
W1 := B.ARRAY(W2)           % Local array with index
```

**7. Record (R-relative):**
```asm
W1 := R.FIELD               % Access record field
```

**8. Pre-indexed:**
```asm
W1 := W2.OFFSET             % Index register + offset
```

### 8.3 Common Instructions

**Data Movement:**
```asm
W1 := 100                   % Load constant
W1 := W2                    % Copy register
W1 := B.VAR                 % Load from memory
W1 := B.VAR                 % Store to memory
```

**Arithmetic:**
```asm
W ADD2 W1, W2               % W1 = W1 + W2
W SUB2 W1, W2               % W1 = W1 - W2
W MUL2 W1, W2               % W1 = W1 * W2
W DIV2 W1, W2               % W1 = W1 / W2
W INCR W1                   % W1++
W DECR W1                   % W1--
```

**Comparison and Branching:**
```asm
W COMP2 W1, W2              % Compare W1 with W2
IF = GO LABEL               % If equal, goto LABEL
IF < GO LABEL               % If less than
IF > GO LABEL               % If greater than
IF >= GO LABEL              % If greater or equal
GO LABEL                    % Unconditional jump
```

**Subroutine Calls:**
```asm
CALL ROUTINE, 2, ARG1, ARG2 % Call with 2 arguments
CALLG ROUTINE, 0            % Call with no arguments
RET                         % Return
```

---

## 9. Practical Development Workflow

### 9.1 Creating Source File

**Using PED:**
```
@PED PROGRAM:SYMB
<type your program>
PED: W "PROGRAM:SYMB"
PED: E
```

**See:** [Editors Guide](../../Editors/EDITORS-GUIDE.md) for editor selection

### 9.2 Assembling

**Start assembler:**
```
@NORD-500-ASSEMBLER
```

**Assembler commands:**
```
ASSEMBLE PROGRAM:SYMB       % Assemble source
LIST                        % Enable listing
NO-LIST                     % Disable listing  
LINES 60                    % Set lines per page
HELP                        % Show commands
EXIT                        % Exit assembler
```

**Output:**
- `PROGRAM:NRF` - Object code
- `PROGRAM:LST` - Assembly listing (if LIST enabled)

### 9.3 Linking

The linking process is handled by the NORD-500 loader (separate from SINTRAN's NRL):

```
@NORD-500-LOADER
LOAD PROGRAM:NRF
PSEG PROGRAM:PSEG
DSEG PROGRAM:DSEG
LINK PROGRAM:LINK
EXIT
```

**Loader creates:**
- `PROGRAM:PSEG` - Program segment (executable code)
- `PROGRAM:DSEG` - Data segment (initialized data)
- `PROGRAM:LINK` - Link information (symbols, debug info)
- Entry in `DESCRIPTION-FILE:DESC` - Domain metadata (old format)

**See:** [LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md) Section 2.4 for NRF format details

### 9.3.1 Domain Formats

**Old Domain Format (Original):**
- Domain information stored in shared `DESCRIPTION-FILE:DESC`
- Segments in separate files: `:PSEG`, `:DSEG`, `:LINK`
- All domains for a user in one DESC file

**New Domain Format (:DOM files):**
- Self-contained `:DOM` file per domain
- Domain metadata stored in file header
- Private segments embedded in :DOM file
- Shared segments in separate `:SEG` files
- Can copy domain with `@COPY-FILE`

**Converting to New Format:**
```
@ND CONVERT-DOMAIN destination source
```

**Why use :DOM format?**
- Portability: Single file to copy
- Independence: No shared DESC file
- Flexibility: Easier backup and distribution

**Note:** Old format will be phased out. Most new development should target :DOM format, but some legacy RT programs only recognize old format.

### 9.4 Automating Builds with MODE Files

**MODE files automate the entire build process.** See [SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md) for complete MODE file documentation.

**Complete build script: BUILD-N500:MODE**

```mode
% BUILD-N500:MODE - Complete NORD-500 build automation
% Usage: @MODE BUILD-N500:MODE

% Enable output logging
OUTPUT FILE: @BUILD-LOG:TXT

% Display build start
@CC =========================================
@CC NORD-500 ASSEMBLER BUILD SCRIPT
@CC =========================================
@CC

@cc Step 1: Assemble NORD-500 source code
@cc Assembling PROGRAM:SYMB...
@NORD-500-ASSEMBLER
ASSEMBLE PROGRAM:SYMB
LIST
LINES 60
EXIT

@cc Assembly completed successfully.
@cc

@cc Step 2: Link with NORD-500 Loader
@cc Linking PROGRAM:NRF...
@NORD-500-LOADER
LOAD PROGRAM:NRF
PSEG PROGRAM:PSEG
DSEG PROGRAM:DSEG
LINK PROGRAM:LINK
MAP
EXIT

@cc Linking completed successfully.
@cc

@cc =========================================
@cc BUILD SUCCESSFUL!
@cc Output files:
@cc   PROGRAM:PSEG - Program segment
@cc   PROGRAM:DSEG - Data segment
@cc   PROGRAM:LINK - Link information
@cc =========================================
OUTPUT FILE: @
```

**Run the build:**
```
@MODE BUILD-N500:MODE
```

**Multi-module build: BUILD-MULTI:MODE**

```mode
@cc BUILD-MULTI:MODE - Build multiple NORD-500 modules
@cc Usage: @MODE BUILD-MULTI:MODE

OUTPUT FILE: @BUILD-LOG:TXT

@cc Building multi-module NORD-500 project...
@cc

@cc Assemble all modules
@cc Assembling MODULE1:SYMB...
@NORD-500-ASSEMBLER
ASSEMBLE MODULE1:SYMB
LIST
EXIT

@cc Assembling MODULE2:SYMB...
@NORD-500-ASSEMBLER
ASSEMBLE MODULE2:SYMB
LIST
EXIT

@cc Assembling MODULE3:SYMB...
@NORD-500-ASSEMBLER
ASSEMBLE MODULE3:SYMB
LIST
EXIT

@cc Link all modules together
@cc Linking all modules...
@NORD-500-LOADER
LOAD MODULE1:NRF
LOAD MODULE2:NRF
LOAD MODULE3:NRF
LIBRARY N500LIB:NRF
PSEG PROJECT:PSEG
DSEG PROJECT:DSEG
LINK PROJECT:LINK
MAP
EXIT

@cc
@cc Multi-module build successful!
OUTPUT FILE: @
```

**Development cycle: DEV-CYCLE:MODE**

```mode
% DEV-CYCLE:MODE - Edit, assemble, test cycle
% Usage: @MODE DEV-CYCLE:MODE

@EDIT:
% Edit source file
@PED PROGRAM:SYMB
% After editing and saving...

% Assemble
@NORD-500-ASSEMBLER
ASSEMBLE PROGRAM:SYMB
NO-LIST
EXIT

@IF-ERROR @GOTO EDIT

% Link
@NORD-500-LOADER
LOAD PROGRAM:NRF
PSEG PROGRAM:PSEG
DSEG PROGRAM:DSEG
EXIT

@IF-ERROR @GOTO EDIT

% Run on NORD-500 (if test harness available)
@CC Build successful - Ready to test on NORD-500

% Loop back for another edit?
@CC
@CC Edit again? (Y/N)
?
% If Y typed, goes to EDIT, otherwise falls through
```

**Quick rebuild: QUICK-BUILD:MODE**

```mode
% QUICK-BUILD:MODE - Fast rebuild without logging
@NORD-500-ASSEMBLER
ASSEMBLE PROGRAM:SYMB
NO-LIST
EXIT
@IF-ERROR @GOTO FAIL

@NORD-500-LOADER
LOAD PROGRAM:NRF
PSEG PROGRAM:PSEG
DSEG PROGRAM:DSEG
EXIT
@IF-ERROR @GOTO FAIL

@CC Quick build OK
@GOTO END

@FAIL:
@CC Build failed!

@END:
```

**Key MODE file features for NORD-500:**
- `OUTPUT FILE: @filename` - Log build output
- `@IF-ERROR @GOTO label` - Error handling
- `@CC message` - Status messages (Comment Command)
- `NO-LIST` - Fast assembly (no listing)
- `MAP` - Generate memory map

**See Also:**
- [SCRIPT-GUIDE.md](../../Workflow/SCRIPT-GUIDE.md) - Complete MODE file reference
- [LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md) - Linker details

### 9.5 Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| **UNDEFINED SYMBOL** | Symbol not declared | Add declaration or IMPORT |
| **MULTIPLY DEFINED** | Symbol defined twice | Remove duplicate |
| **ILLEGAL ADDRESSING** | Wrong addressing mode | Check instruction requirements |
| **TYPE MISMATCH** | Wrong data type | Use correct type specifier |
| **PHASE ERROR** | Forward reference issue | Rearrange code |

---

## 10. Example Programs

### 10.1 Hello World

```asm
MODULE HELLO

% External output routine
IMPORT-P WRITELN

% Main entry point
MAIN START

% Message data
MSG: BY DATA 'HELLO FROM NORD-500!'
MSGLEN: W DATA #NCHR(MSG)

% Main program
START:
    W1 := ADDR(MSG)         % Message address
    W2 := IND(MSGLEN)       % Message length
    CALL WRITELN, 2, W1, W2 % Call output
    RET                     % Return (stop)

ENDMODULE
```

### 10.2 Simple Arithmetic

```asm
MODULE MATH

MAIN COMPUTE

% Data
A: W DATA 10
B: W DATA 20
RESULT: W BLOCK 1

% Computation
COMPUTE:
    W1 := IND(A)            % Load A
    W2 := IND(B)            % Load B
    W ADD2 W1, W2           % W1 = W1 + W2
    W1 := IND(RESULT)       % Store result
    RET

ENDMODULE
```

### 10.3 Routine with Parameters

```asm
MODULE EXAMPLE

ROUTINE MULTIPLY
    
% Stack frame for parameters
STACK
    PARAM_A: W BLOCK 1      % First parameter address
    PARAM_B: W BLOCK 1      % Second parameter address
    RESULT:  W BLOCK 1      % Result storage
ENDSTACK

MULTIPLY:
    ENTS #SCLC              % Enter with stack
    
    % Load parameters
    W1 := IND(B.PARAM_A)    % Load value of A
    W2 := IND(B.PARAM_B)    % Load value of B
    
    % Multiply
    W MUL2 W1, W2           % W1 = W1 * W2
    
    % Store result
    W1 := B.RESULT          % Store in local
    
    RET                     % Return

ENDROUTINE

ENDMODULE
```

### 10.4 Array Processing

```asm
MODULE ARRAY_SUM

MAIN SUM_ARRAY

% Data
ARRAY: W DATA 10, 20, 30, 40, 50
COUNT: W DATA 5
TOTAL: W BLOCK 1

% Sum array elements
SUM_ARRAY:
    W1 := 0                 % Initialize sum
    W2 := 0                 % Initialize index
    W3 := IND(COUNT)        % Load count
    
LOOP:
    W COMP2 W2, W3          % Compare index with count
    IF >= GO DONE           % If done, exit loop
    
    W4 := ARRAY(W2)         % Load array element
    W ADD2 W1, W4           % Add to sum
    W INCR W2               % Increment index
    GO LOOP                 % Continue loop
    
DONE:
    W1 := IND(TOTAL)        % Store total
    RET

ENDMODULE
```

---

## 11. Common Patterns

### 11.1 Parameter Passing

**By address (pointer):**
```asm
ROUTINE PROCESS
STACK
    DATA_PTR: W BLOCK 1     % Pointer to data
ENDSTACK

PROCESS:
    ENTS #SCLC
    W1 := IND(B.DATA_PTR)   % Dereference pointer
    % Process data in W1
    RET
ENDROUTINE

% Call:
% CALL PROCESS, 1, ADDR(MY_DATA)
```

**By value:**
```asm
ROUTINE COMPUTE
STACK
    VALUE: W BLOCK 1        % Value parameter
ENDSTACK

COMPUTE:
    ENTS #SCLC
    W1 := B.VALUE           % Load value directly
    % Process value
    RET
ENDROUTINE

% Call:
% W1 := 100
% CALL COMPUTE, 1, W1
```

### 11.2 Loop Constructs

**Counted loop:**
```asm
    W2 := 0                 % Initialize counter
    W3 := 10                % Loop limit

LOOP:
    % Loop body
    
    W INCR W2               % Increment counter
    W COMP2 W2, W3          % Compare
    IF < GO LOOP            % Continue if less
```

**While loop:**
```asm
WHILE_START:
    % Test condition
    W COMP2 W1, W2
    IF >= GO WHILE_END
    
    % Loop body
    
    GO WHILE_START

WHILE_END:
```

### 11.3 Data Structure Access

**Linked list traversal:**
```asm
RECORD
    NEXT: W BLOCK 1         % Offset 0
    DATA: W BLOCK 1         % Offset 4
ENDRECORD

TRAVERSE:
    R := IND(LIST_HEAD)     % R = first node

NEXT_NODE:
    W COMP R, #ZEROD        % Check for null
    IF = GO DONE
    
    W1 := R.DATA            % Access data
    % Process W1
    
    R := R.NEXT             % R = next node
    GO NEXT_NODE

DONE:
    RET
```

---

## 12. Differences from MAC Assembler

| Feature | MAC (ND-100) | NORD-500 ASM |
|---------|--------------|--------------|
| **Structure** | Flat, unstructured | Modular (MODULE/ROUTINE) |
| **Data Types** | Limited (word-based) | Rich (BI/BY/H/W/F/D) |
| **Addressing** | Simple, register-oriented | 10 sophisticated modes |
| **Registers** | A, B, D, L, T, X, P | W1-W4, F1-F4, D1-D4, B, R |
| **Stack Support** | Manual | Built-in STACK directive |
| **Records** | Manual structures | Built-in RECORD directive |
| **Symbols** | 5 chars significant | 16 chars significant |
| **Target** | ND-100 CPU | ND-500 CPU |
| **Philosophy** | Low-level, minimal | Structured, high-level |

**When to use each:**
- **MAC:** ND-100 system programming, device drivers
- **NORD-500 ASM:** ND-500 applications, high-performance computing

---

## 13. Next Steps

### 13.1 Continue Learning

**Recommended path:**
1. ✅ You've completed the intro guide
2. → Try the example programs above
3. → Study [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](NORD-500-ASSEMBLER-EXPERT-GUIDE.md)
4. → Read the reference manual for complete instruction set

**Expert Guide covers:**
- Complete instruction set reference
- All addressing modes in detail
- Advanced stack and record techniques
- Macro programming
- Conditional assembly
- Integration with NORD-100
- Performance optimization
- Real-world examples

### 13.2 Reference Materials

**Essential documentation:**
- **Reference Manual:** `D:\OCR\ai\ND-60.113.02 NORD-500 ASSEMBLER Reference Manual`
- **Expert Guide:** [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](NORD-500-ASSEMBLER-EXPERT-GUIDE.md)
- **CPU Manual:** NORD-500 CPU Reference Manual (for instruction details)

### 13.3 Related Guides

- **[NPL-DEVELOPER-GUIDE.md](NPL-DEVELOPER-GUIDE.md)** - Similar structured approach for ND-100
- **[MAC-DEVELOPER-GUIDE.md](MAC-DEVELOPER-GUIDE.md)** - ND-100 assembly language
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** - Understanding object files and linking
- **[EDITORS-GUIDE.md](../../Editors/EDITORS-GUIDE.md)** - Choosing and using editors

---

## Quick Reference

### Intrinsic Functions

| Function | Description |
|----------|-------------|
| `#PCLC` | Program location counter |
| `#DCLC` | Data location counter |
| `#SCLC` | Stack location counter (size) |
| `#RCLC` | Record location counter (size) |
| `#NCHR(string)` | String length |
| `#NARG` | Number of macro arguments |
| `#DATE` | Current date/time |
| `#LOG2(n)` | Log base 2 of n |

### Stack Header Fields

| Name | Offset | Description |
|------|--------|-------------|
| `PREVB` | 0 | Saved B-register |
| `RETA` | 4 | Return address |
| `SP` | 8 | Stack pointer |
| `AUX` | 12 | System cell |
| `NARG` | 16 | Number of arguments |

### Data Type Sizes

| Type | Size | Description |
|------|------|-------------|
| `BI` | 1 bit | Bit |
| `BY` | 8 bits | Byte |
| `H` | 16 bits | Half-word |
| `W` | 32 bits | Word |
| `F` | 32 bits | Single float |
| `D` | 64 bits | Double float |

---

**Version:** 1.0  
**Last Updated:** October 18, 2025  
**Status:** Complete - Introduction Level

**For advanced topics, see:** [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](NORD-500-ASSEMBLER-EXPERT-GUIDE.md)

**Reference Manual:** `D:\OCR\ai\ND-60.113.02 NORD-500 ASSEMBLER Reference Manual-Gandalf-OCR_combined.md`

---

*You've completed the NORD-500 Assembler introduction! Ready for more? Check out the Expert Guide for advanced techniques and complete reference material.*

