# SINTRAN III Linking and Binary Management Guide

**Complete reference for NRL, BRF, BPUN, and PROG files**

**Version:** 1.0  
**Date:** October 17, 2025  
**Status:** Complete

---

## Table of Contents

1. [Overview](#1-overview)
2. [File Formats](#2-file-formats)
3. [NRL - NORD Relocating Loader](#3-nrl---nord-relocating-loader)
4. [Creating Executables](#4-creating-executables)
5. [Reentrant Programs](#5-reentrant-programs)
6. [Binary Management Commands](#6-binary-management-commands)
7. [Practical Examples](#7-practical-examples)

---

## 1. Overview

### 1.1 The Linking Process

```
Source → Compiler → Assembler → Linker → Executable
  .NPL      →      .MAC       →   .BRF    →   .PROG/.BPUN
  .C                            NRL
```

**Key concepts:**
- **BRF:** Binary Relocatable Format - object code from assembler
- **NRL:** NORD Relocating Loader - links BRF files
- **PROG:** Executable program file - ready to run
- **BPUN:** Binary Punched format - alternative executable
- **Reentrant:** Shared code in memory

---

## 2. File Formats

### 2.1 BRF - Binary Relocatable Format

**Purpose:** Object code output from MAC assembler

**Contents:**
- Machine code (relocatable)
- Symbol table (exported/imported symbols)
- Relocation information
- Entry points

**Creation:**
```
@MAC SOURCE:MAC    →    SOURCE:BRF
```

### 2.2 PROG - Program File

**Purpose:** Executable program ready to load and run

**Contents:**
- Absolute machine code
- Entry point address
- Memory requirements
- Load address

**Characteristics:**
- Self-contained executable
- Can be run directly with `@PROGRAM`
- Non-reentrant (one instance)

### 2.3 BPUN - Binary Punched File

**Purpose:** Alternative executable format

**Contents:**
- Similar to PROG
- Additional metadata
- Can be dumped to reentrant

**Characteristics:**
- More flexible than PROG
- Can be converted to reentrant
- Used for system programs

**Why use BPUN?**
- System programs that need reentrant capability
- Programs that may be shared
- Programs loaded via DUMP-REENTRANT

### 2.4 Format Comparison

| Feature | BRF | PROG | BPUN |
|---------|-----|------|------|
| **Type** | Object | Executable | Executable |
| **Relocatable** | Yes | No | No |
| **Can Link** | Yes | No | No |
| **Can Run** | No | Yes | Yes |
| **Can Dump** | No | No | Yes |
| **Reentrant** | N/A | No | Can be |

---

## 3. NRL - NORD Relocating Loader

### 3.1 Starting NRL

```
@NRL
*                    % NRL command prompt
```

### 3.2 NRL Commands

| Command | Purpose | Example |
|---------|---------|---------|
| **IMAGE** | Set target CPU | `*IMAGE 100` or `*IMAGE 500` |
| **PROG-FILE** | Set output PROG file | `*PROG-FILE "MYPROG"` |
| **BPUN-FILE** | Set output BPUN file | `*BPUN-FILE "MYPROG"` |
| **LOAD** | Load BRF file | `*LOAD MODULE1` |
| **LIBRARY** | Load from library | `*LIBRARY SYSLIB` |
| **MAP** | Show memory map | `*MAP` |
| **XREF** | Cross-reference | `*XREF` |
| **EXIT** | Exit NRL | `*EXIT` |

### 3.3 Basic Linking Session

```
@NRL
*IMAGE 100                   % Target ND-100
*PROG-FILE "HELLO"           % Output file
*LOAD HELLO                  % Load HELLO:BRF
*EXIT                        % Exit NRL

% Creates HELLO:PROG
```

### 3.4 Multi-Module Linking

```
@NRL
*IMAGE 100
*PROG-FILE "MYAPP"
*LOAD MODULE1                % Main module
*LOAD MODULE2                % Support module
*LOAD MODULE3                % Utility module
*LIBRARY STDLIB              % Standard library
*MAP                         % Show memory layout
*EXIT
```

### 3.5 Symbol Resolution

**NRL resolves:**
- External references (`)EXTR` declarations)
- Entry points (`)ENTR` declarations)
- Common blocks
- Library references

**Example:**

**Module1.MAC:**
```mac
        )EXTR FUNC2          % Reference to Module2
        
START,  LDA =100
        JSR FUNC2            % Call external function
        EXIT
        
        )ENTR START
```

**Module2.MAC:**
```mac
FUNC2,  LDA =200
        EXIT
        
        )ENTR FUNC2          % Export FUNC2
```

**Linking:**
```
*LOAD MODULE1                % Needs FUNC2
*LOAD MODULE2                % Provides FUNC2
```

NRL resolves `FUNC2` reference automatically.

---

## 4. Creating Executables

### 4.1 Simple Program (PROG)

**For:** Single-use programs, utilities

```
@NPL PROG:NPL               % Compile
@MAC PROG:MAC               % Assemble
@NRL                        % Link
*IMAGE 100
*PROG-FILE "PROG"
*LOAD PROG
*EXIT

@PROG                       % Run
```

### 4.2 C Program with Runtime

**For:** C programs requiring runtime library

```
@CC-100 PROG:C              % Compile
@NRL
*IMAGE 100
*PROG-FILE "PROG"
*LOAD CC-2HEADER            % C runtime header
*LOAD PROG                  % Your program
*LOAD CC-2BANK              % C runtime library
*LOAD CC-2TRAILER           % C runtime trailer
*EXIT

@PROG                       % Run
```

### 4.3 System Program (BPUN)

**For:** System programs, reentrant candidates

```
@NPL SYSPROG:NPL
@MAC SYSPROG:MAC
@NRL
*IMAGE 100
*BPUN-FILE "SYSPROG"
*LOAD SYSPROG
*EXIT

% Creates SYSPROG:BPUN
```

---

## 5. Reentrant Programs

### 5.1 What is Reentrant?

**Reentrant program:**
- Loaded once into memory
- Shared by multiple users/tasks
- Single code copy, multiple instances
- Memory efficient

**Benefits:**
- Saves memory
- Faster loading (already in memory)
- System programs (editors, compilers)

### 5.2 Creating Reentrant from BPUN

**Step 1: Create BPUN file**
```
@NRL
*BPUN-FILE "EDITOR"
*LOAD EDITOR
*EXIT
```

**Step 2: Dump to reentrant**
```
@DUMP-REENTRANT EDITOR:BPUN
```

**Now `EDITOR` is reentrant and can be shared**

### 5.3 Reentrant Management Commands

| Command | Purpose | Example |
|---------|---------|---------|
| **DUMP-REENTRANT** | Load BPUN as reentrant | `@DUMP-REENTRANT PROG:BPUN` |
| **LIST-REENTRANT** | List reentrant programs | `@LIST-REENTRANT` |
| **DELETE-REENTRANT** | Remove reentrant | `@DELETE-REENTRANT PROG` |
| **DEFINE-REENTRANT-PROGRAM** | Define reentrant | (System use) |
| **LOAD-REENTRANT-SEGMENT** | Load segment | (System use) |
| **CLEAR-REENTRANT-SEGMENT** | Clear segment | (System use) |

### 5.4 LIST-REENTRANT Output

```
@LIST-REENTRANT

START RESTART SEGMENT NAME
0B    1B      130B     NRL
0B    0B      131B     BACKUP-SYSTEM-B
70B   70B     132B     DITAP
177777B 177775B 133B   FMAC
177777B 177775B 134B   MAC
0B    1B      135B     QED
0B    1B      136B     NPL
```

**Fields:**
- **START:** Start address (octal)
- **RESTART:** Restart address (octal)
- **SEGMENT:** Segment number (octal)
- **NAME:** Program name

---

## 6. Binary Management Commands

### 6.1 LOAD-BINARY

**Purpose:** Load binary file into memory

```
@LOAD-BINARY ADDRESS, FILE:BIN
```

**Example:**
```
@LOAD-BINARY 10000, BOOTLOADER:BIN
```

### 6.2 PLACE-BINARY

**Purpose:** Place binary at specific address

```
@PLACE-BINARY ADDRESS, SIZE, FILE:BIN
```

**Used for:**
- Boot loaders
- Device firmware
- Memory-mapped code

### 6.3 PROG vs BPUN Decision

**Use PROG when:**
- Simple utility program
- Single-user application
- Not shared across users
- No reentrant needs

**Use BPUN when:**
- System program
- May need to be shared
- Potential reentrant use
- Complex application

### 6.4 Converting BPUN to PROG

**Cannot directly convert**, but can:

1. Relink from BRF files:
```
@NRL
*PROG-FILE "NEWPROG"
*LOAD SOURCE            % Load original BRF
*EXIT
```

2. Or use BPUN directly:
```
@BPUNFILE:BPUN          % Run BPUN like PROG
```

---

## 7. Practical Examples

### 7.1 Complete NPL Build

**Source: MYAPP:NPL**

```bash
# Compile
@NPL MYAPP:NPL
NPL COMPILER VERSION 3.5
...
COMPILATION COMPLETE

# Assemble
@MAC MYAPP:MAC
MAC ASSEMBLER VERSION 4.2
...
ASSEMBLY COMPLETE

# Link
@NRL
*IMAGE 100
*PROG-FILE "MYAPP"
*LOAD MYAPP
*MAP
ENTRY POINT: START
CODE: 100-500
DATA: 600-1000
*EXIT

# Run
@MYAPP
```

### 7.2 Multi-Module Project

**Files: MAIN:NPL, UTILS:NPL, IO:NPL**

```bash
# Compile all
@NPL MAIN:NPL
@NPL UTILS:NPL
@NPL IO:NPL

# Assemble all
@MAC MAIN:MAC
@MAC UTILS:MAC
@MAC IO:MAC

# Link
@NRL
*IMAGE 100
*PROG-FILE "PROJECT"
*LOAD MAIN              % Main module
*LOAD UTILS             % Utilities
*LOAD IO                % I/O routines
*LIBRARY SYSLIB         % System library
*MAP
*XREF                   % Cross-reference
*EXIT

# Run
@PROJECT
```

### 7.3 Creating System Command

**Goal:** Create reentrant system command

```bash
# Step 1: Compile and assemble
@NPL MYCMD:NPL
@MAC MYCMD:MAC

# Step 2: Create BPUN
@NRL
*BPUN-FILE "MYCMD"
*LOAD MYCMD
*EXIT

# Step 3: Make reentrant
@DUMP-REENTRANT MYCMD:BPUN

# Step 4: Verify
@LIST-REENTRANT
...
0B    1B      150B     MYCMD

# Step 5: Use command
@MYCMD
```

### 7.4 Library Creation

**Create reusable library:**

```bash
# Compile library modules
@NPL LIB-MATH:NPL
@NPL LIB-STRING:NPL
@NPL LIB-IO:NPL

# Assemble
@MAC LIB-MATH:MAC
@MAC LIB-STRING:MAC
@MAC LIB-IO:MAC

# Create library file (using BRF-EDITOR)
@BRF-EDITOR
MAKE-LIBRARY-UNITS LIB-MATH:BRF
MAKE-LIBRARY-UNITS LIB-STRING:BRF
MAKE-LIBRARY-UNITS LIB-IO:BRF
CHANGE-FILE MYLIB:LIB
EXIT

# Use library
@NRL
*LIBRARY MYLIB          % Load library
*LOAD MYAPP             % Load app
*EXIT
```

---

## Quick Reference

### NRL Essential Commands

| Command | Purpose |
|---------|---------|
| `*IMAGE 100` | Target ND-100 |
| `*IMAGE 500` | Target ND-500 |
| `*PROG-FILE "NAME"` | Create PROG file |
| `*BPUN-FILE "NAME"` | Create BPUN file |
| `*LOAD MODULE` | Load BRF file |
| `*LIBRARY LIB` | Load library |
| `*MAP` | Memory map |
| `*XREF` | Cross-reference |
| `*EXIT` | Exit NRL |

### File Extension Summary

| Extension | Type | Created By | Used By |
|-----------|------|------------|---------|
| `.NPL` | Source | Editor | NPL compiler |
| `.MAC` | Assembly | NPL compiler | MAC assembler |
| `.BRF` | Object | MAC assembler | NRL linker |
| `.PROG` | Executable | NRL | SINTRAN |
| `.BPUN` | Executable | NRL | SINTRAN/DUMP-REENTRANT |
| `.LST` | Listing | Compiler/Assembler | Human |

### Build Process Summary

```
1. Edit:    @ED                    → PROG:NPL
2. Compile: @NPL PROG:NPL          → PROG:MAC
3. Assemble: @MAC PROG:MAC         → PROG:BRF
4. Link:    @NRL + LOAD + EXIT     → PROG:PROG or PROG:BPUN
5. Run:     @PROG                  → Execute
```

---

## See Also

- **[NPL-DEVELOPER-GUIDE.md](KERNEL/NPL-DEVELOPER-GUIDE.md)** - NPL language
- **[MAC-DEVELOPER-GUIDE.md](MAC-DEVELOPER-GUIDE.md)** - MAC assembler
- **[SCRIPT-GUIDE.md](SCRIPT-GUIDE.md)** - Automation with MODE files
- **Kernel Documentation:** `SINTRAN\OS\`

---

**Last Updated:** October 17, 2025  
**Version:** 1.0  
**Status:** Complete

