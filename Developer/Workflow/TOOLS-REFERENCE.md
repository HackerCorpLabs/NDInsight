# SINTRAN III Development Tools Reference

**Complete reference for all development tools**

**Version:** 1.0  
**Date:** October 17, 2025  
**Status:** Complete

---

## Table of Contents

1. [Editors](#editors)
2. [Compilers & Assemblers](#compilers--assemblers)
3. [Linkers & Loaders](#linkers--loaders)
4. [Debuggers](#debuggers)
5. [Utilities](#utilities)
6. [System Tools](#system-tools)
7. [Quick Reference](#quick-reference)

---

## Editors

### ED - Line Editor

**Command:** `@ED`

**Purpose:** Basic line-oriented text editor

**Common Commands:**
```
INPUT       - Enter input mode
CHANGE      - Change text
DELETE      - Delete lines
LIST        - List lines
FIND        - Search
SAVE        - Save file
QUIT        - Exit
```

**Usage Pattern:**
```bash
@ED
*INPUT HELLO:NPL
[enter text]
*SAVE
*QUIT
```

**When to Use:** Quick edits, batch processing, remote terminals

---

### PED - Program Editor

**Command:** `@PED-ENG-J`

**Purpose:** Full-featured program editor with syntax awareness

**Manual:** ND-60.121.4 PED User's Guide  
**Location:** `D:\OCR\ai\ND-60.121.4 PED User's Guide-Gandalf-OCR\`

**Features:**
- Syntax-aware editing
- Block operations
- Search and replace
- Macro recording
- Multiple buffers

**Basic Commands:**
```
EDIT filename   - Open file
INSERT          - Insert mode
DELETE          - Delete text
FIND            - Search
REPLACE         - Search and replace
SAVE            - Save changes
EXIT            - Quit editor
```

**Common Usage:**
```bash
@PED-ENG-J
*EDIT MYPROG:NPL
[editing...]
*SAVE
*EXIT
```

**When to Use:** Large programs, structured editing, NPL/MAC development

---

### QED - Queue Editor

**Command:** `@QED-1644L`

**Purpose:** Context-based text editor

**Manual:** ND-60.031.04 EN QED User Manual  
**Location:** `D:\OCR\ai\ND-60.031.04 EN QED User Manual-Sintran-OCR\`

**Features:**
- Context editing
- Pattern matching
- Batch operations
- Scripting support

**Basic Commands:**
```
OPEN filename   - Open file
APPEND          - Append text
CHANGE          - Modify text
DELETE          - Delete text
FIND            - Search
WRITE           - Save file
QUIT            - Exit
```

**Common Usage:**
```bash
@QED-1644L
*OPEN SOURCE:NPL
[editing...]
*WRITE
*QUIT
```

**When to Use:** Pattern-based editing, automated changes, scripting

---

## Compilers & Assemblers

### NPL - NORD Programming Language Compiler

**Command:** `@NPL source:NPL [,list,list]`

**Purpose:** Compile NPL source to MAC assembly

**Manual:** ND-60.047.03 NORD PL User's Guide  
**Input:** `.NPL` files  
**Output:** `.MAC` files

**Options:**
```bash
@NPL prog:NPL              # Compile to prog:MAC
@NPL prog,list,list        # With listing
```

**Exit Status:**
```
END OF COMPILATION - 0 ERRORS DETECTED   # Success
END OF COMPILATION - X ERRORS DETECTED   # X errors
```

**See:** [NPL-DEVELOPER-GUIDE.md](../Languages/System/NPL-DEVELOPER-GUIDE.md)

---

### MAC - Macro Assembler

**Command:** `@MAC source:MAC`

**Purpose:** Assemble MAC source to BRF object files

**Manual:** ND-60.096.01 MAC User's Guide  
**Input:** `.MAC` files  
**Output:** `.BRF` files

**Interactive Mode:**
```bash
@MAC
*commands
*EXIT
```

**Batch Mode:**
```bash
@MAC prog:MAC
```

**See:** [MAC-DEVELOPER-GUIDE.md](../Languages/System/MAC-DEVELOPER-GUIDE.md)

---

### CC-100 - C Compiler for ND-100

**Command:** `@CC-100 source:C`

**Purpose:** Compile C source to BRF object files

**Manual:** ND-60.214.01 CC-100 and CC-500 C-Compiler User Manual  
**Input:** `.C` files  
**Output:** `.BRF` files

**Usage:**
```bash
@CC-100 HELLO:C
```

**Output Messages:**
```
ND-100 C-Compiler Version A
Errors: none
Code: 130w Data: 107w
```

**Requires:** Link with CC-2HEADER, CC-2BANK, CC-2TRAILER

**See:** [C-DEVELOPER-GUIDE.md](../Languages/Application/C-DEVELOPER-GUIDE.md)

---

### PLANC-100-C - PLANC Compiler

**Command:** `@PLANC-100-C`

**Purpose:** Compile PLANC programs (Pascal-like language)

**Manual:** ND-60.117.5 EN PLANC Reference Manual  
**Input:** `.PLANC` files  
**Output:** `.BRF` files

**Interactive Mode:**
```bash
@PLANC-100-C
COMPILE filename:PLANC
EXIT
```

**See:** [PLANC-DEVELOPER-GUIDE.md](../Languages/Application/PLANC-DEVELOPER-GUIDE.md)

---

### FTN - FORTRAN Compiler

**Command:** `@FTN`

**Purpose:** Compile FORTRAN programs

**Input:** `.FTN` or `.FOR` files  
**Output:** `.BRF` files

**Usage:**
```bash
@FTN
*INPUT source:FTN
*COMPILE
*EXIT
```

**See:** [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) Section 5

---

### ASSEMBLER-500 - ND-500 Assembler

**Command:** `@ASSEMBLER-500`

**Purpose:** Assemble ND-500 programs

**Input:** ND-500 assembly source  
**Output:** ND-500 object files

**Usage:**
```bash
@ASSEMBLER-500
[commands]
```

**Note:** Used for ND-500 CPU programs only

---

## Linkers & Loaders

### NRL - ND Relocating Loader

**Command:** `@NRL-1935J`

**Purpose:** Link BRF object files into executable programs

**Manual:** ND-60.066.04 ND Relocating Loader  
**Input:** `.BRF` files  
**Output:** `.PROG` or `.BPUN` files

**Interactive Commands:**
```
*IMAGE cpu         - Set target (100 or 500)
*PROG-FILE "name"  - Output filename
*LOAD filename     - Load object file
*START symbol      - Set entry point
*MAP               - Show memory map
*EXIT              - Link and exit
```

**Common Usage:**
```bash
@NRL
*IMAGE 100
*PROG-FILE "MYPROG"
*LOAD module1
*LOAD module2
*EXIT
```

**See:** [LINKING-GUIDE.md](LINKING-GUIDE.md)

---

### BRF-EDITOR - Binary Relocatable Format Editor

**Command:** `@BRF-EDITOR-1858F`

**Purpose:** Edit symbols and create libraries from BRF files

**Manual:** ND-60.085.01 BRF EDITOR  
**Location:** `D:\OCR\ai\ND-60.085.01 BRF EDITOR-Gandalf-OCR\`

**Commands:**
```
RENAME-SYMBOL old new     - Rename symbol
CHANGE-FILE filename      - Select file to edit
MAKE-LIBRARY-UNITS file   - Create library
EXIT                      - Save and exit
```

**Usage Pattern:**
```bash
@BRF-EDITOR
RENAME-SYMBOL RI1F RIDB1F
RENAME-SYMBOL RINF RIDBNF
CHANGE-FILE module:BRF
MAKE-LIBRARY-UNITS module:BRF
EXIT
```

**When to Use:**
- Avoid symbol conflicts
- Create reusable libraries
- Modify object files

**See:** [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md)

---

### BINDER - System Linker

**Purpose:** Link multiple modules into system images

**Note:** BINDER is typically invoked as part of system build, not for application development. Use NRL for normal linking.

---

## Debuggers

### MAC Interactive Debugger

**Invocation:** Start MAC in debug mode

**Manual:** ND-60.096.01 MAC Interactive Assembly and Debugging System  
**Location:** `D:\OCR\ai\ND-60.096.01_MAC_Interactive_Assembly_and_Debugging_System_Users_Guide_March_1978_ocr\`

**Purpose:** Debug assembly programs at instruction level

**Features:**
- Set breakpoints
- Single-step execution
- Examine/modify registers
- Examine/modify memory
- Symbol table access

**Common Commands:**
```
B address       - Set breakpoint
G               - Go (run)
S               - Single step
D address       - Display memory
M address value - Modify memory
R               - Display registers
Q               - Quit
```

**When to Use:** Low-level debugging, MAC programs, system-level issues

---

### DEBUGGER - Symbolic Debugger

**Command:** `@DEBUGGER`

**Manual:** ND-60.158-5-EN Symbolic Debugger - User Guide  
**Purpose:** High-level symbolic debugging

**Features:**
- Source-level debugging
- Symbolic breakpoints
- Variable inspection
- Stack traces
- Expression evaluation

**Common Commands:**
```
BREAK procedure     - Set breakpoint
RUN                 - Start execution
STEP                - Single step
CONTINUE            - Continue execution
PRINT variable      - Show variable value
WHERE               - Stack trace
QUIT                - Exit debugger
```

**Usage:**
```bash
@DEBUGGER
*LOAD MYPROG
*BREAK MAIN
*RUN
[debugging...]
*QUIT
```

**When to Use:** High-level debugging, NPL/PLANC/C programs

---

## Utilities

### DUMP - File Dump Utility

**Purpose:** Display file contents in various formats

**Usage:**
```bash
@DUMP filename
```

**Formats:**
- Octal dump
- Hexadecimal dump
- ASCII dump
- Symbolic dump (with symbol table)

**When to Use:** Inspect binary files, debug file formats

---

### LOOK-FILE - File Inspector

**Command:** `@LOOK-FILE-2244E`

**Purpose:** Examine file contents and structure

**Usage:**
```bash
@LOOK-FILE-2244E
*filename
```

**Shows:**
- File type
- File size
- Creation date
- File attributes

---

### FIL-EXTR - File Extract Utility

**Command:** `@FIL-EXTR-2232B`

**Purpose:** Extract portions of files

**Usage:**
```bash
@FIL-EXTR-2232B
[commands]
```

**When to Use:** Extract data from large files, file manipulation

---

### GPM - General Purpose Monitor

**Command:** `@GPM-2365B`

**Purpose:** System monitoring and statistics

**Usage:**
```bash
@GPM-2365B
[monitoring commands]
```

**Features:**
- CPU usage
- Memory usage
- I/O statistics
- Process information

---

### PERFORM - Performance Monitor

**Command:** `@PERFORM-E`

**Purpose:** Performance profiling and analysis

**Usage:**
```bash
@PERFORM-E
[profiling commands]
```

**Features:**
- Code profiling
- Hot spot identification
- Timing analysis
- Call graphs

---

### ACCOUNTS - Account Management

**Command:** `@ACCOUNTS-2183A`

**Purpose:** User account and resource accounting

**Usage:**
```bash
@ACCOUNTS-2183A
[account commands]
```

---

### FILSYS-INV - File System Inventory

**Command:** `@FILSYS-INV-2135H`

**Purpose:** File system analysis and verification

**Usage:**
```bash
@FILSYS-INV-2135H
[analysis commands]
```

**When to Use:** File system maintenance, disk space analysis

---

## System Tools

### SINTRAN-SERVICE-PROGRAM - System Configuration

**Purpose:** Configure SINTRAN III system parameters

**Usage:** Invoked during system setup

**Functions:**
- Define RTCOMMON size
- Configure memory layout
- Set system parameters
- Device configuration

**Note:** Typically used by system administrators, not application developers

**MODE File Usage:**
```
@SINTRAN-SERVICE
[commands to service program]
EXIT
[back to MODE file]
```

---

### S3-CONFIGURATION - System Configuration

**Command:** `@S3-CONFIGURATION`

**Purpose:** Configure SINTRAN III system

**Usage:**
```bash
@S3-CONFIGURATION
[configuration commands]
```

---

### BACKUP-SYSTEM-B - System Backup

**Command:** `@BACKUP-SYSTEM-B`

**Purpose:** Backup system and user files

**Usage:**
```bash
@BACKUP-SYSTEM-B
[backup commands]
```

---

### DITAP - Tape Utility

**Command:** `@DITAP`

**Purpose:** Magnetic tape operations

**Usage:**
```bash
@DITAP
[tape commands]
```

**Functions:**
- Read tapes
- Write tapes
- Copy tapes
- Verify tapes

---

## Quick Reference

### Tool Categories

#### Editors
| Tool | Command | Purpose |
|------|---------|---------|
| **ED** | `@ED` | Line editor |
| **PED** | `@PED-ENG-J` | Program editor |
| **QED** | `@QED-1644L` | Queue editor |

#### Compilers
| Tool | Command | Input | Output |
|------|---------|-------|--------|
| **NPL** | `@NPL file:NPL` | `.NPL` | `.MAC` |
| **MAC** | `@MAC file:MAC` | `.MAC` | `.BRF` |
| **CC-100** | `@CC-100 file:C` | `.C` | `.BRF` |
| **PLANC** | `@PLANC-100-C` | `.PLANC` | `.BRF` |
| **FTN** | `@FTN` | `.FTN` | `.BRF` |

#### Linkers
| Tool | Command | Input | Output |
|------|---------|-------|--------|
| **NRL** | `@NRL` | `.BRF` | `.PROG` |
| **BRF-EDITOR** | `@BRF-EDITOR` | `.BRF` | `.BRF` |

#### Debuggers
| Tool | Command | Purpose |
|------|---------|---------|
| **MAC Debugger** | `@MAC` (debug mode) | Low-level debugging |
| **DEBUGGER** | `@DEBUGGER` | Symbolic debugging |

#### Utilities
| Tool | Command | Purpose |
|------|---------|---------|
| **DUMP** | `@DUMP` | File dump |
| **LOOK-FILE** | `@LOOK-FILE` | File inspection |
| **GPM** | `@GPM` | System monitoring |
| **PERFORM** | `@PERFORM` | Performance profiling |

---

## Tool Selection Guide

### "Which editor should I use?"

| Scenario | Recommended Tool |
|----------|------------------|
| Quick file edit | **ED** |
| Large program development | **PED** |
| Pattern-based edits | **QED** |
| Batch processing | **ED** or **QED** |

### "Which compiler for my language?"

| Language | Tool | Notes |
|----------|------|-------|
| NPL | **@NPL** | System language |
| Assembly | **@MAC** | Low-level control |
| C | **@CC-100** | Portable code |
| PLANC | **@PLANC-100-C** | Pascal-like |
| FORTRAN | **@FTN** | Scientific computing |

### "How do I debug?"

| Problem Type | Tool | Why |
|--------------|------|-----|
| Crash | **DEBUGGER** | Symbolic debugging |
| Memory corruption | **MAC Debugger** | Low-level inspection |
| Performance | **PERFORM** | Profiling |
| Logic error | **DEBUGGER** | Source-level stepping |

---

## Common Workflows

### Edit → Compile → Debug

```bash
# 1. Edit
@PED-ENG-J
*EDIT MYPROG:NPL
[editing...]
*SAVE
*EXIT

# 2. Compile
@NPL MYPROG:NPL
@MAC MYPROG:MAC

# 3. Link
@NRL
*PROG-FILE "MYPROG"
*LOAD MYPROG
*EXIT

# 4. Debug (if needed)
@DEBUGGER
*LOAD MYPROG
*RUN
```

### Profile → Optimize → Verify

```bash
# 1. Profile
@PERFORM
*PROFILE MYPROG

# 2. Analyze results
[identify hot spots]

# 3. Edit and recompile
@PED-ENG-J
[optimize code]

# 4. Verify improvement
@PERFORM
*PROFILE MYPROG
```

---

## File Associations

### By Extension

| Extension | Type | Tools |
|-----------|------|-------|
| `.NPL` | NPL source | **PED**, **@NPL** |
| `.MAC` | Assembly source | **PED**, **@MAC** |
| `.C` | C source | **PED**, **@CC-100** |
| `.PLANC` | PLANC source | **PED**, **@PLANC-100-C** |
| `.BRF` | Object file | **BRF-EDITOR**, **@NRL** |
| `.PROG` | Executable | Run with `@filename` |
| `.BPUN` | Binary program | **DUMP-REENTRANT** or run |

---

## Tool Locations

### System Directory

All tools are typically in `(PACK-ONE:SYSTEM)` or `(PACK-ONE:BPUN-FILES)`.

### Common Files

```
(PACK-ONE:SYSTEM)
├── NPL                    # NPL compiler
├── MAC-1415C:BPUN        # MAC assembler
├── NRL-1935J:BPUN        # NRL linker
├── CC-100-A:PROG         # C compiler
├── PLANC-100-C:PROG      # PLANC compiler
├── BRF-EDITOR:PROG       # BRF editor
├── DEBUGGER:PROG         # Symbolic debugger
├── PED-ENG-J:BPUN        # PED editor
└── QED-1644L:BPUN        # QED editor
```

---

## Documentation References

### Manuals Available

| Tool | Manual | Location |
|------|--------|----------|
| **NPL** | ND-60.047.03 | `D:\OCR\ai\ND-60.047.03` |
| **MAC** | ND-60.096.01 | `D:\OCR\ai\ND-60.096.01` |
| **CC-100** | ND-60.214.01 | `D:\OCR\ai\ND-60.214.01` |
| **PLANC** | ND-60.117.5 | `D:\OCR\ai\ND-60.117.5` |
| **NRL** | ND-60.066.04 | `D:\OCR\ai\ND-60.066.04` |
| **BRF-EDITOR** | ND-60.085.01 | `D:\OCR\ai\ND-60.085.01` |
| **PED** | ND-60.121.4 | `D:\OCR\ai\ND-60.121.4` |
| **QED** | ND-60.031.04 | `D:\OCR\ai\ND-60.031.04` |
| **Debugger** | ND-60.158-5 | Coming |

### Local Guides

| Guide | File |
|-------|------|
| **Master Guide** | [SINTRAN-DEVELOPER-GUIDE.md](../SINTRAN-DEVELOPER-GUIDE.md) |
| **NPL Guide** | [NPL-DEVELOPER-GUIDE.md](../Languages/System/NPL-DEVELOPER-GUIDE.md) |
| **MAC Guide** | [MAC-DEVELOPER-GUIDE.md](../Languages/System/MAC-DEVELOPER-GUIDE.md) |
| **Linking Guide** | [LINKING-GUIDE.md](LINKING-GUIDE.md) |
| **Commands** | [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md) |
| **Quick Start** | [QUICK-START-EXAMPLES.md](../QUICK-START-EXAMPLES.md) |

---

## Tips & Best Practices

### Tool Selection
- **Use PED** for serious development
- **Use ED** for quick edits and scripts
- **Use MAC Debugger** for low-level issues
- **Use Symbolic Debugger** for application debugging

### Performance
- **Profile before optimizing** with PERFORM
- **Use GPM** for system-wide monitoring
- **Check memory usage** before scaling up

### Maintenance
- **Use BRF-EDITOR** to avoid symbol conflicts
- **Create libraries** for reusable code
- **Keep tool versions consistent** across builds

---

**Version:** 1.0  
**Last Updated:** October 17, 2025  
**Status:** Complete  

**See Also:**
- [SINTRAN-DEVELOPER-GUIDE.md](../SINTRAN-DEVELOPER-GUIDE.md) - Master guide
- [COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md) - Command reference
- [QUICK-START-EXAMPLES.md](../QUICK-START-EXAMPLES.md) - Working examples
- [README.md](../README.md) - Documentation overview

