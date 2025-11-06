# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Repository Context

This directory contains **authentic SINTRAN III operating system source code** from the s3vs-4 generation job. This is production NPL (Norsk Data Programming Language) code that ran on ND-100 computers in the 1980s.

**Important**: This is historical preservation and documentation work. The code is read-only for analysis, emulator development, and cross-referencing with documentation.

---

## Source Code Organization

### File Naming Convention

NPL files follow the pattern: `[Prefix]-P2-[Component].NPL`

**Prefixes indicate compilation phase:**
- **0** - System generation/initialization
- **5P** - Pass 5 (monitor/kernel core)
- **CC** - Common compiler components
- **DP** - Data processing
- **IP** - Input processing (device drivers, I/O)
- **MP** - Main processing (memory, peripherals)
- **RP** - Runtime processing (RT-level coordination)
- **TP** - Terminal processing
- **PH** - Phase handlers (startup, restart)
- **XC** - External communication

**P2 designation** = Pass 2 of SINTRAN build (code generation and linking phase)

### Critical Source Files

| File | Component | Documentation Reference |
|------|-----------|------------------------|
| **5P-P2-MON60.NPL** | Monitor/kernel core | ../OS/14-MONITOR-KERNEL-MONCALLS.md |
| **IP-P2-SCSI-DISK.NPL** | SCSI disk driver | ../Devices/SCSI/ + ../OS/15-DISK-IO-SUBSYSTEM.md |
| **IP-P2-SCSI-DRIV.NPL** | SCSI protocol driver | ../Devices/SCSI/SCSI-Commands-Analysis.md |
| **MP-P2-HDLC-DRIV.NPL** | HDLC communication | ../Devices/HDLC/ |
| **CC-P2-N500.NPL** | ND-500 command processor | ../ND500/ + ../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md |
| **MP-P2-N500.NPL** | ND-500 monitor routines | ../ND500/ND500-INTEGRATION-GUIDE.md |
| **RP-P2-N500.NPL** | ND-500 RT coordination | ../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md |
| **IP-P2-SEGADM.NPL** | Segment administration | ../OS/16-PAGE-FAULT-HANDLER.md |
| **IP-P2-DISK-START.NPL** | Disk startup/initialization | ../OS/15-DISK-IO-SUBSYSTEM.md |
| **MP-P2-TAD.NPL** | TAD protocol | ../TAD/ |
| **RP-P2-MONCALLS.NPL** | Monitor call handlers | ../OS/14-MONITOR-KERNEL-MONCALLS.md |

---

## NPL Language Characteristics

### Syntax Essentials

**Declarations:**
```npl
INTEGER variable
REF(TYPE) pointer
INTEGER ARRAY arrayname=?
SUBR subroutineName
```

**Comments:**
```npl
! Single line comment
%========================================
% Block comment separator
%========================================
COMMENT This is a block comment;
```

**Control Flow:**
```npl
IF condition THEN statement
IF condition THEN statement ELSE statement
WHILE condition DO statement
FOR variable := start STEP increment UNTIL end DO statement
```

**Assembly Blocks:**
```npl
*"8N500    ! Marker for ND-500 assembly code
% ... assembly instructions ...
RBUS      ! Return from subroutine
```

**Predeclarations:**
```npl
INTEGER VARIABLE=?    ! Forward declaration
SUBR PROCEDURE        ! Predeclare subroutine
```

### Common Patterns in SINTRAN Source

**Error Codes (SYMBOL):**
```npl
SYMBOL ENAUTHORISED= 25     ! Error code constant
SYMBOL EC174=       174     ! Illegal parameter
SYMBOL N5TIMOUT=   2000     ! ND-500 timeout
```

**Device Register Access:**
```npl
! Register addresses typically at HDEV+offset
! Example: HDEV+0, HDEV+1, HDEV+17
```

**Bit Manipulation:**
```npl
! NPL uses octal constants
! Example: 177777 (octal) = 0xFFFF (hex)
```

---

## Symbol Tables (SYMBOLS/ folder)

### Format

Symbol files map NPL variables/procedures to octal memory addresses:
```
SYMBOLNAME=OCTAL_ADDRESS
```

### Symbol Files Reference

| File | Use Case |
|------|----------|
| **FILSYS-SYMBOLS.SYMB.TXT** | File system structures (source code missing) |
| **N500-SYMBOLS.SYMB.TXT** | ND-500 interface memory layout |
| **XMSG-SYMBOL-LIST.SYMB.TXT** | XMSG message system (source code missing) |
| **SYMBOL-1-LIST.SYMB.TXT** | Primary kernel variables/procedures |
| **SYMBOL-2-LIST.SYMB.TXT** | Secondary kernel symbols |
| **RTLO-SYMBOLS.SYMB.TXT** | Runtime library entry points |
| **LIBRARY-MARKS.SYMB.TXT** | Library markers |

### Finding Symbols

**Search for variable names:**
```bash
grep "VARIABLENAME" SYMBOLS/*.TXT
```

**Find address range:**
```bash
grep "^SYMB.*=04" SYMBOLS/SYMBOL-1-LIST.SYMB.TXT  # Addresses starting with 04 (octal)
```

**Cross-reference with NPL code:**
1. Find symbol in SYMBOLS/ files
2. Note octal address
3. Search for symbol name in NPL/*.NPL files
4. Verify usage context

---

## Working with NPL Source Code

### Reading Source Code

**Step 1: Identify the component**
- Check file prefix (IP, MP, RP, etc.) to understand functional area
- Refer to README.md for component descriptions

**Step 2: Understand dependencies**
- Look for PREDECLARATION sections (variables/subroutines declared elsewhere)
- Variables with `=?` are forward declarations

**Step 3: Cross-reference documentation**
- Every major source file has corresponding documentation in ../OS/, ../Devices/, or ../ND500/
- Use the mapping table above to find relevant docs

**Step 4: Symbol resolution**
- Unknown variables → search SYMBOLS/ files
- Convert octal addresses to understand memory layout
- Cross-check with ../OS/19-MEMORY-MAP-REFERENCE.md

### Analyzing Device Drivers

**SCSI Drivers (IP-P2-SCSI-*.NPL):**
1. Read ../Devices/SCSI/SCSI-Master-Index.md first
2. Understand NCR 5386 controller (../Devices/SCSI/SCSI-Commands-Analysis.md)
3. Cross-reference with IP-P2-SCSI-DRIV.NPL (protocol) and IP-P2-SCSI-DISK.NPL (disk driver)
4. Check SYMBOL-1-LIST.SYMB.TXT for memory addresses

**HDLC Driver (MP-P2-HDLC-DRIV.NPL):**
1. Read ../Devices/HDLC/README.md for overview
2. Understand COM5025 chip (../Devices/HDLC/01-HDLC-Hardware-Reference.md)
3. Review register map (../Devices/HDLC/02-HDLC-Register-Reference.md)
4. Analyze interrupt handlers (../Devices/HDLC/04-HDLC-Interrupt-Handlers.md)

**ND-500 Interface (CC-P2-N500.NPL, MP-P2-N500.NPL, RP-P2-N500.NPL):**
1. Start with ../ND500/ND500-INTEGRATION-GUIDE.md
2. Understand 5MPM shared memory (../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md)
3. Check N500-SYMBOLS.SYMB.TXT for message structure addresses
4. Cross-reference with ../Emulator/ND500-MESSAGE-STRUCTURE-VERIFIED.md

### Emulator Development

**When implementing SINTRAN behavior in C#:**

1. **Read NPL source first** - Understand exact algorithm
2. **Verify with documentation** - Cross-check understanding
3. **Use symbol tables** - Match memory layout exactly
4. **Preserve semantics** - Keep SINTRAN behavior identical, even if inefficient
5. **Reference emulator code** - See ../Emulator/ND500-EMULATION-COMPLETE.cs for examples

**Memory Layout:**
- Use ../OS/19-MEMORY-MAP-REFERENCE.md for complete memory map
- Symbol tables provide octal addresses
- Convert octal to hex/decimal for C# implementation

**Critical Resources:**
- ../Emulator/KERNEL-ACCESS-EMULATOR.md - Reading kernel structures from C#
- ../Emulator/ND500-INTEGRATION-GUIDE.md - ND-500 emulation guide
- ../Emulator/ND500-QUICK-REFERENCE.md - Quick reference card

---

## Source Code Limitations

### Missing Components

The following SINTRAN III components do **not** have NPL source code available:

- **File system implementation** (Level 11/23) - Only symbols in FILSYS-SYMBOLS.SYMB.TXT
- **XMSG message system** - Only symbols in XMSG-SYMBOL-LIST.SYMB.TXT
- **Complete terminal handlers** - Partial code only
- **Network subsystems** (beyond HDLC)
- **Batch processing**
- **System utilities/commands**

**When encountering missing source:**
1. Check if symbols are available in SYMBOLS/ folder
2. Refer to corresponding documentation in ../OS/ or ../Devices/
3. Use symbols to understand data structure layout
4. Document assumptions clearly when inferring behavior

---

## Documentation Cross-References

### Always Consult These Documents

**Before analyzing any NPL file:**
1. Read corresponding documentation first (see mapping table above)
2. Check ../OS/README.md for kernel architecture overview
3. Review ../OS/19-MEMORY-MAP-REFERENCE.md for memory layout

**For specific subsystems:**
- **Interrupts** → ../OS/13-INT14-HANDLER-DETAILED.md
- **Monitor Calls** → ../OS/14-MONITOR-KERNEL-MONCALLS.md
- **Disk I/O** → ../OS/15-DISK-IO-SUBSYSTEM.md
- **Page Faults** → ../OS/16-PAGE-FAULT-HANDLER.md
- **Scheduling** → ../OS/17-SCHEDULER-AND-PRIORITIES.md
- **Device Drivers** → ../OS/18-DEVICE-DRIVER-FRAMEWORK.md

### NPL Language Reference

- ../../Reference-Manuals/ND-60.128.1 EN NPL Programmers Reference Manual.md (if available)
- ../../Developer/Languages/System/NPL-REFERENCE.md (if available)

---

## Key Architectural Concepts

### SINTRAN Build Process

This source code represents **Pass 2 (P2)** output:
1. **Pass 1** - Syntax analysis and symbol table generation
2. **Pass 2** - Code generation and linking (these files)
3. **Pass 3** - Final assembly and memory layout

The **s3vs-4.symb** file is the complete build output containing all Pass 2 compilation units.

### Component Interaction

**Layered Architecture:**
```
User Programs (Level 1)
    ↓
Monitor Kernel (Level 3) - RP-P2-*.NPL
    ↓
Device Drivers (Levels 10-12) - IP-P2-*.NPL, MP-P2-*.NPL
    ↓
Internal Interrupts (Level 14) - 5P-P2-MON60.NPL
    ↓
Hardware
```

**ND-500 Integration:**
```
ND-100 (Control Processor)
    ↕ 5MPM (Multiport Memory)
ND-500 (Computation Processor)

Files involved:
- CC-P2-N500.NPL (command processing)
- MP-P2-N500.NPL (monitor-level routines)
- RP-P2-N500.NPL (RT-level coordination)
```

### Memory Addressing

**Critical**: NPL source uses **octal** notation for addresses and constants.

**Conversion:**
- Octal 177777 = Hex 0xFFFF = Decimal 65535
- Octal 000100 = Hex 0x0040 = Decimal 64

**Symbol Table Addresses:**
- All addresses in SYMBOLS/*.SYMB.TXT are octal
- Must convert to hex/decimal for C# emulator work

---

## Historical Context

**SINTRAN Version**: SINTRAN III Version 4 (s3vs-4)
**SINTRAN Level**: L07 (symbol tables)
**Build Job**: s3vs-4
**Target Hardware**: ND-100 (16-bit) + ND-500 (32-bit byte-addressed)
**Era**: 1980s production operating system

**This is authentic historical source code** - preserve accuracy when documenting or implementing emulator behavior.

---

## Quick Reference

### Finding Information

| I need to... | Action |
|--------------|--------|
| Understand a device driver | Check README.md mapping table → Read ../Devices/ docs → Read NPL source |
| Find a variable's address | `grep "VARNAME" SYMBOLS/*.TXT` |
| Understand ND-500 interface | Read ../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md then CC-P2-N500.NPL |
| Implement in emulator | Read NPL source → Check ../Emulator/ guides → Verify with symbols |
| Understand memory layout | ../OS/19-MEMORY-MAP-REFERENCE.md + SYMBOLS/ files |
| Find missing source info | Check SYMBOLS/ for structure addresses → Refer to ../OS/ documentation |

### File Count Reference

- **NPL Source Files**: 45
- **Symbol Table Files**: 7
- **Original Build Output**: s3vs-4.symb (3.9 MB)
- **Total Size**: ~4.4 MB

---

**Last Updated**: 2025-11-06
**Source**: SINTRAN III s3vs-4 build job
**Purpose**: Historical preservation, emulator development, documentation verification
