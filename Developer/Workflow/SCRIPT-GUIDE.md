# SINTRAN III Scripting and Automation Guide

**MODE Files and Build Automation**

**Version:** 1.0  
**Date:** October 17, 2025  
**Status:** Complete

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [MODE File Basics](#2-mode-file-basics)
3. [Command Execution](#3-command-execution)
4. [Input/Output Redirection](#4-inputoutput-redirection)
5. [Calling Other MODE Files](#5-calling-other-mode-files)
6. [Build Automation](#6-build-automation)
7. [Real-World Examples](#7-real-world-examples)
8. [Best Practices](#8-best-practices)

---

## 1. Introduction

### 1.1 What are MODE Files?

**MODE files** are SINTRAN III batch script files that automate sequences of commands. They are the equivalent of shell scripts in Unix or batch files in DOS.

**Key features:**
- Execute SINTRAN commands sequentially
- Run programs and pass input
- Redirect output to files
- Call other MODE files
- Automate complex build processes

### 1.2 When to Use MODE Files

**Use MODE files for:**
- Build automation (compile, assemble, link)
- Repetitive tasks
- Multi-step processes
- System initialization
- Testing and validation

**Example use cases:**
- Compile all modules in a project
- Build and test in one command
- Deploy system updates
- Generate documentation

---

## 2. MODE File Basics

### 2.1 File Format

**Extension:** `:MODE` (e.g., `BUILD:MODE`)

**Content:** Plain text file with SINTRAN commands, one per line

**Example:**
```mode
@cc Building program...
@NPL SOURCE:NPL
@MAC SOURCE:MAC
@NRL
PROG-FILE "PROGRAM"
LOAD SOURCE
EXIT
@cc Build complete!
```

### 2.2 Running MODE Files

```bash
@MODE FILENAME:MODE
```

or simply:

```bash
@MODE FILENAME
```

**Example:**
```bash
@MODE BUILD:MODE
```

### 2.3 Command Syntax - The `@` Symbol Rule

**CRITICAL CONCEPT:** The `@` symbol determines whether a line is a SINTRAN command or program input.

#### Lines Starting with `@` - SINTRAN Commands

```mode
@NPL SOURCE:NPL              % SINTRAN: Start NPL compiler
@MAC SOURCE:MAC              % SINTRAN: Start MAC assembler
@NRL                         % SINTRAN: Start linker
@CC This is a message        % SINTRAN: Display message
@DELETE FILE:BRF             % SINTRAN: Delete file
```

**These are SINTRAN system commands** executed by the operating system.

#### Lines WITHOUT `@` - Program Input

```mode
PROG-FILE "HELLO"            % Input TO NRL
LOAD MODULE                  % Input TO NRL
EXIT                         % Input TO NRL (exits NRL)
```

**These lines are sent as input** to whatever program is currently running (started by the last `@` command).

#### Complete Example Showing Both

```mode
% Start the NRL linker (SINTRAN command - has @)
@NRL

% The following lines are INPUT to NRL (no @)
IMAGE 100
PROG-FILE "HELLO"
LOAD HELLO
EXIT                         % Exits NRL, returns to SINTRAN

% Back at SINTRAN level, run the program (SINTRAN command - has @)
@HELLO
```

#### Another Example: NORD-500 Assembly

```mode
@CC Starting assembly...     % SINTRAN: Display message

% Start the assembler (SINTRAN command - has @)
@NORD-500-ASSEMBLER

% Input to the assembler (no @)
ASSEMBLE PROGRAM:SYMB
LIST
LINES 60
EXIT                         % Exits assembler

@CC Assembly complete!       % SINTRAN: Display message
```

#### Key Rules

| Line Type | Starts with `@`? | Who Executes It? | Example |
|-----------|------------------|------------------|---------|
| **SINTRAN Command** | ✅ Yes | Operating System | `@NPL SOURCE:NPL` |
| **Program Input** | ❌ No | Currently Running Program | `PROG-FILE "TEST"` |
| **Comment/Message** | ✅ Yes (`@cc` or `@CC`) | Displayed during execution | `@cc Building...` |

**Remember:** After starting a program with `@PROGRAM-NAME`, all following lines without `@` are sent as input to that program until it exits (usually with `EXIT`).

**Note:** MODE files run automatically without user interaction - they are batch scripts.

---

## 3. Command Execution

### 3.1 Basic Commands

```mode
@NPL SOURCE:NPL              % Compile NPL
@MAC SOURCE:MAC              % Assemble
@LIST-FILES                  % List directory
@DELETE FILE:BRF             % Delete file
```

### 3.2 Running Programs

**Start program:**
```mode
@PROGRAM                     % Run program
```

**Program with parameters:**
```mode
@PROGRAM PARAM1 PARAM2       % Pass parameters
```

### 3.3 Providing Input to Programs

```mode
@NRL                         % Start NRL
PROG-FILE "OUTPUT"           % Input to NRL
LOAD MODULE                  % Input to NRL
EXIT                         % Input to NRL
```

**How it works:**
- `@NRL` starts the NRL program
- Next lines are fed as input to NRL
- Program processes input until it exits

### 3.4 Displaying Messages and Comments with @cc

```mode
@cc This message will be displayed during execution
@cc =========================================
@cc NORD-500 Build Process
@cc =========================================
```

**`@cc` or `@CC` (Comment Command):** Displays text during MODE file execution. This is the ONLY way to add comments or status messages in MODE files.

**Format:** `@cc <text>` or `@CC <text>`

**Rules:**
- There should be a space between the second C and the text
- Case doesn't matter: `@cc` and `@CC` both work
- The text is displayed during execution
- Useful for progress messages, status updates, section headers, and documentation

**Examples:**
```mode
@cc ================================================
@cc Multi-Module Build Script
@cc ================================================

@cc Step 1: Compiling modules...
@NPL SOURCE:NPL

@cc Step 2: Assembling...
@MAC SOURCE:MAC

@cc Step 3: Linking...
@NRL
PROG-FILE "PROGRAM"
LOAD SOURCE
EXIT

@cc Build complete!
```

**Note:** MODE files have no other comment syntax. If you want to document your MODE file, use `@cc` (it will be displayed when the MODE file runs).

---

## 4. Input/Output Redirection

### 4.1 Output Redirection

**Redirect program output to file:**

```mode
OUTPUT FILE: @FILENAME
```

**Example:**
```mode
OUTPUT FILE: @BUILD-LOG:TXT
@NPL SOURCE:NPL
@MAC SOURCE:MAC
OUTPUT FILE: @                  % Back to terminal
```

### 4.2 Input from File

```mode
INPUT FILE: @COMMANDS:TXT
@PROGRAM
INPUT FILE: @                   % Back to keyboard
```

### 4.3 Combining Redirection

```mode
OUTPUT FILE: @LOG:TXT
INPUT FILE: @COMMANDS:TXT
@PROGRAM
INPUT FILE: @
OUTPUT FILE: @
```

---

## 5. Calling Other MODE Files

### 5.1 Nested MODE Files

**Main MODE file can call other MODE files:**

```mode
% MASTER:MODE
@MODE COMPILE:MODE           % Call compile script
@MODE LINK:MODE              % Call link script
@MODE TEST:MODE              % Call test script
```

### 5.2 Modular Build Scripts

**COMPILE:MODE:**
```mode
@NPL MODULE1:NPL
@NPL MODULE2:NPL
@NPL MODULE3:NPL
@MAC MODULE1:MAC
@MAC MODULE2:MAC
@MAC MODULE3:MAC
```

**LINK:MODE:**
```mode
@NRL
IMAGE 100
PROG-FILE "APP"
LOAD MODULE1
LOAD MODULE2
LOAD MODULE3
EXIT
```

**BUILD:MODE:**
```mode
@MODE COMPILE:MODE
@MODE LINK:MODE
@cc Build complete
```

---

## 6. Build Automation

### 6.1 Simple NPL Build

**File: BUILD-NPL:MODE**

```mode
@cc Compile, assemble, and link NPL program

OUTPUT FILE: @BUILD-LOG:TXT

@cc Compile
@NPL %1:NPL

@cc Assemble
@MAC %1:MAC

@cc Link
@NRL
PROG-FILE "%1"
LOAD %1
EXIT

OUTPUT FILE: @

@cc Build complete: %1:PROG
```

**Usage:**
```bash
@MODE BUILD-NPL MYPROGRAM
```

**Result:** Creates `MYPROGRAM:PROG`

### 6.2 C Program Build

**File: BUILD-C:MODE**

```mode
@cc Build C program with runtime

@CC-100 %1:C

@NRL
IMAGE 100
PROG-FILE "%1"
LOAD CC-2HEADER
LOAD %1
LOAD CC-2BANK
LOAD CC-2TRAILER
EXIT

@cc C build complete
```

### 6.3 Multi-Module Project

**File: BUILD-PROJECT:MODE**

```mode
@cc Build complete project

@cc Compiling modules...

@cc Compile all NPL modules
@NPL MAIN:NPL
@NPL UTILS:NPL
@NPL DATABASE:NPL
@NPL NETWORK:NPL

@cc Assembling...

@cc Assemble all
@MAC MAIN:MAC
@MAC UTILS:MAC
@MAC DATABASE:MAC
@MAC NETWORK:MAC

@cc Linking...

@cc Link all modules
@NRL
IMAGE 100
PROG-FILE "PROJECT"
LOAD MAIN
LOAD UTILS
LOAD DATABASE
LOAD NETWORK
LIBRARY SYSLIB
MAP
EXIT

@cc Build successful!
@cc Run with: @PROJECT
```

### 6.4 Clean and Rebuild

**File: REBUILD:MODE**

```mode
@cc Clean and rebuild project

@cc Cleaning old files...

@DELETE MAIN:MAC
@DELETE MAIN:BRF
@DELETE UTILS:MAC
@DELETE UTILS:BRF
@DELETE PROJECT:PROG

@cc Starting build...

@MODE BUILD-PROJECT:MODE

@cc Rebuild complete!
```

---

## 7. Real-World Examples

### 7.1 SINTRAN Build Script

**From actual SINTRAN source:**

```mode
@NPL NORD PL NOVEMBER 1979
@DEV ABSTR,1,1
% ... (input file loaded here) ...

@MAC
)9SLPL
)9ASSM 100,LIST-FILE,(OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
?)9TSS

@BRF-EDITOR
RENAME-SYMBOL RI1F RIDB1F
RENAME-SYMBOL RINF RIDBNF
CHANGE-FILE (OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
MAKE-LIBRARY-UNITS (OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
EXIT
```

**This shows:**
- Batch compilation
- MAC assembly with special directives
- BRF-EDITOR post-processing
- Symbol renaming
- Library unit creation

### 7.2 Multi-Stage Build

**File: XMSG-START:MODE (from SINTRAN)**

```mode
OUTPUT FILE: @BUILD-OUTPUT

@CC-100 CAT:C

@NRL
IMAGE 100
PROG-FILE "CAT"
LOAD CC-2HEADER
LOAD CAT
LOAD CC-2BANK
LOAD CC-2TRAILER
EXIT

OUTPUT FILE: @
```

### 7.3 Test Automation

**File: TEST:MODE**

```mode
@cc Running test suite...

@cc Build test program
@NPL TEST-SUITE:NPL
@MAC TEST-SUITE:MAC
@NRL
PROG-FILE "TEST-SUITE"
LOAD TEST-SUITE
EXIT

@cc Run tests
OUTPUT FILE: @TEST-RESULTS:TXT
@TEST-SUITE
OUTPUT FILE: @

@cc Check results
@cc Tests complete. Results in TEST-RESULTS:TXT
```

---

## 8. Best Practices

### 8.1 Structure

**Good structure:**
```mode
@cc ================================================
@cc Build Main Application
@cc Author: Name
@cc Date: 2025-10-18
@cc ================================================

@cc Clean phase
@cc Cleaning...
@DELETE OLD-FILES

@cc Build phase
@cc Building...
@NPL SOURCE:NPL
@MAC SOURCE:MAC

@cc Link phase
@cc Linking...
@NRL
PROG-FILE "APP"
LOAD SOURCE
EXIT

@cc Done
@cc Complete!
```

### 8.2 Error Handling

**Check for errors:**
```mode
@NPL SOURCE:NPL
% If compilation fails, MODE file continues
% Add manual checks if needed

@CC Check for errors above
?                            % Wait for user to verify
```

### 8.3 Logging

**Always log builds:**
```mode
OUTPUT FILE: @BUILD-LOG:TXT
@CC Build started
@NPL SOURCE:NPL
@MAC SOURCE:MAC
@CC Build completed
OUTPUT FILE: @
```

### 8.4 Parameterization

**Use parameters:**
```mode
% BUILD:MODE - Generic build script
% Usage: @MODE BUILD PROGRAM-NAME

@NPL %1:NPL
@MAC %1:MAC
@NRL
PROG-FILE "%1"
LOAD %1
EXIT
```

### 8.5 Modular Scripts

**Break into modules:**
```
BUILD:MODE           → Main build script
  ├─ COMPILE:MODE    → Compile phase
  ├─ ASSEMBLE:MODE   → Assembly phase
  ├─ LINK:MODE       → Link phase
  └─ TEST:MODE       → Test phase
```

### 8.6 Comments

**Always comment:**
```mode
% ============================================
% PROJECT BUILD SCRIPT
% ============================================
% Purpose: Build entire project from source
% Dependencies: NPL, MAC, NRL, SYSLIB
% Output: PROJECT:PROG
% ============================================

@ECHO "Starting build..."

% Compile phase
% Compiles all NPL modules to MAC
@NPL MODULE1:NPL      % Main module
@NPL MODULE2:NPL      % Utilities
@NPL MODULE3:NPL      % I/O handlers

% ... etc ...
```

---

## Quick Reference

### Essential MODE Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `@COMMAND` | Run SINTRAN command | `@NPL SOURCE:NPL` |
| `?` | Wait for user | `?` |
| `%` | Comment | `% This is a comment` |
| `OUTPUT FILE:` | Redirect output | `OUTPUT FILE: @LOG:TXT` |
| `INPUT FILE:` | Redirect input | `INPUT FILE: @DATA:TXT` |
| `@MODE` | Call other MODE | `@MODE SUBSCRIPT:MODE` |

### Common Patterns

**Simple build:**
```mode
@NPL %1:NPL
@MAC %1:MAC
@NRL
PROG-FILE "%1"
LOAD %1
EXIT
```

**Build with logging:**
```mode
OUTPUT FILE: @BUILD-LOG:TXT
@NPL SOURCE:NPL
@MAC SOURCE:MAC
OUTPUT FILE: @
```

**Multi-module:**
```mode
@MODE COMPILE-ALL:MODE
@MODE LINK-ALL:MODE
@MODE TEST-ALL:MODE
```

---

## Common Pitfalls

### Pitfall 1: Context Switching - Mixing `@` and non-`@`

**Problem:**
```mode
@NRL                         % @ = SINTRAN: Start NRL
PROG-FILE "TEST"             % No @ = Input to NRL
@PROGRAM                     % Wrong! @ = SINTRAN command, but NRL still running!
```

**Why this fails:** The `@PROGRAM` line has `@`, so SINTRAN tries to execute it as a system command. But NRL is still running and hasn't exited yet, causing confusion.

**Solution:**
```mode
@NRL                         % @ = SINTRAN: Start NRL
PROG-FILE "TEST"             % No @ = Input to NRL
LOAD SOURCE                  % No @ = Input to NRL
EXIT                         % No @ = Input to NRL (exits NRL)
@PROGRAM                     % @ = SINTRAN: Now at SINTRAN level
```

**Remember:** All lines without `@` go to the currently running program. Use `EXIT` (no `@`) to exit the program first, then you can use `@` commands again.

### Pitfall 2: Missing EXIT

**Problem:**
```mode
@NRL
PROG-FILE "TEST"
LOAD SOURCE
% Missing EXIT - MODE file hangs
```

**Solution:**
```mode
@NRL
PROG-FILE "TEST"
LOAD SOURCE
EXIT                         % Always exit programs
```

### Pitfall 3: Forgetting Output Redirect

**Problem:**
```mode
OUTPUT FILE: @LOG:TXT
@NPL SOURCE:NPL
% Long script continues...
% Everything goes to LOG:TXT (invisible)
```

**Solution:**
```mode
OUTPUT FILE: @LOG:TXT
@NPL SOURCE:NPL
OUTPUT FILE: @               % Reset to terminal
@CC Compilation done
```

---

## See Also

- **[SINTRAN-COMMANDS-REFERENCE.md](../SINTRAN-COMMANDS-REFERENCE.md)** - Complete SINTRAN III command reference including @CC
- **[COMPILER-COMMANDS-REFERENCE.md](COMPILER-COMMANDS-REFERENCE.md)** - Compiler and linker commands
- **[LINKING-GUIDE.md](LINKING-GUIDE.md)** - Understanding the build process

---

## See Also

- **[QUICK-START-EXAMPLES.md](../QUICK-START-EXAMPLES.md)** - Basic examples
- **[LINKING-GUIDE.md](LINKING-GUIDE.md)** - NRL linking
- **[NPL-DEVELOPER-GUIDE.md](../Languages/System/NPL-DEVELOPER-GUIDE.md)** - NPL language
- **Kernel Documentation:** `SINTRAN\OS\`

---

**Last Updated:** October 17, 2025  
**Version:** 1.0  
**Status:** Complete

