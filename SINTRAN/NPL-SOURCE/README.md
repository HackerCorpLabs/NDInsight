# SINTRAN III NPL Source Code

This directory contains **SINTRAN III operating system source code** written in **NPL (Norsk Data Programming Language)**.

---

## Contents

### Source Files

- **`s3vs-4.symb`** (3.9 MB) - Original SINTRAN generation job output file containing the complete source code listing
- **`NPL/`** - Directory containing 45 individual NPL source files extracted from the SYMB file
- **`SYMBOLS/`** - Directory containing SINTRAN L07 symbol tables for reference

### NPL Subfolder Structure

The `NPL/` subfolder contains the split-out source files from `s3vs-4.symb`, organized by component:

- **0.SIN-GEN.NPL** - SINTRAN generator/initialization
- **5P-P2-MON60.NPL** - Monitor/kernel code
- **CC-P2-*.NPL** - Common compiler components
- **DP-P2-*.NPL** - Data processing components
- **IP-P2-*.NPL** - Input/output and device drivers (DISK, SCSI, SEGADM)
- **MP-P2-*.NPL** - Memory and peripheral management (HDLC, ND-500, HASP)
- **TP-P2-*.NPL** - Terminal and communication handling

### SYMBOLS Subfolder - SINTRAN L07 Symbol Tables

The `SYMBOLS/` subfolder contains **symbol tables for SINTRAN L07** - these are reference files mapping variable names, procedure entry points, and constants to their memory addresses in octal. These symbols are critical for:

- **Understanding memory layout** - Where kernel structures reside
- **Cross-referencing NPL code** - Matching symbols to source code
- **Emulator development** - Implementing correct memory addressing
- **Debugging** - Resolving addresses to symbolic names

#### Symbol Files Available

| File | Size | Description |
|------|------|-------------|
| **FILSYS-SYMBOLS.SYMB.TXT** | 61 KB | File system symbols (Level 11/23 filesystem) |
| **N500-SYMBOLS.SYMB.TXT** | 122 KB | ND-500 coprocessor interface symbols |
| **RTLO-SYMBOLS.SYMB.TXT** | 56 KB | Runtime library symbols |
| **XMSG-SYMBOL-LIST.SYMB.TXT** | 30 KB | XMSG message system symbols |
| **SYMBOL-1-LIST.SYMB.TXT** | 102 KB | Primary kernel symbol table |
| **SYMBOL-2-LIST.SYMB.TXT** | 69 KB | Secondary kernel symbol table |
| **LIBRARY-MARKS.SYMB.TXT** | 14 KB | Library entry point markers |

**Note**: While we don't have the complete file system source code, the **FILSYS-SYMBOLS.SYMB.TXT** file provides crucial reference information about file system data structures and entry points in SINTRAN L07.

#### Symbol File Format

Symbol files contain entries in the format:
```
SYMBOLNAME=OCTAL_ADDRESS
```

Example from FILSYS-SYMBOLS.SYMB.TXT:
```
UINDP=000003    ! User index pointer
DPAGA=153555    ! Data page address
UNLCQ=034062    ! Unlock queue
DATA=054400     ! Data segment base
```

#### Using Symbol Tables

1. **Find kernel variables** - Search symbol files for variable names mentioned in documentation
2. **Resolve addresses** - Convert octal addresses to understand memory layout
3. **Map to source code** - Cross-reference symbols with NPL source files
4. **Emulator verification** - Validate emulator memory structures match SINTRAN layout

---

## What is s3vs-4.symb?

The **s3vs-4.symb** file is the output of a **SINTRAN generation job** - a complete build listing that includes:

1. Source code for kernel components
2. Symbol tables and cross-references
3. Compilation metadata
4. Assembly listings

This SYMB file has been **split into individual NPL source files** for easier analysis and navigation. Each NPL file in the `NPL/` subfolder represents one compilation unit from the original SINTRAN III system.

---

## Coverage and Limitations

### What's Included

This source code covers several critical SINTRAN III subsystems:

- **Kernel/Monitor** (MON60) - Core OS services
- **Device Drivers** - SCSI disk, HDLC communication, magnetic tape
- **Memory Management** (SEGADM) - Segment administration
- **ND-500 Integration** - 32-bit coprocessor interface
- **Disk I/O Subsystem** - Disk startup and logging
- **HASP Protocol** - Remote job entry

### What's Missing

**This is an incomplete source code collection.** Major missing components include:

- **File system implementation** (Level 11, directory management, file access)
- **Level 23 file system** (symbolic files, directories)
- **Complete terminal handlers**
- **Network subsystems** (beyond HDLC drivers)
- **Batch processing components**
- **Backup/restore utilities**
- **System utilities and commands**

The file system code is particularly significant because it represents a large portion of the SINTRAN III kernel that is not available in this collection.

---

## NPL Language

**NPL (Norsk Data Programming Language)** is a systems programming language similar to ALGOL/Pascal, designed for:

- Operating system development
- Real-time systems
- Low-level hardware access
- Bit manipulation and memory control

### Key NPL Characteristics

- Strong typing with runtime checks
- Direct memory addressing
- Interrupt handling primitives
- Hardware register access
- Procedure-oriented with nested scopes
- Assembler-like control over memory layout

### NPL Resources

For NPL language reference and development guides, see:

- **[Developer/Languages/System/NPL-REFERENCE.md](../../Developer/Languages/System/NPL-REFERENCE.md)** - Complete NPL language reference
- **[Developer/SINTRAN-DEVELOPER-GUIDE.md](../../Developer/SINTRAN-DEVELOPER-GUIDE.md)** - System programming guide
- **[Reference-Manuals/ND-60.128.1 EN NPL Programmers Reference Manual.md](../../Reference-Manuals/ND-60.128.1%20EN%20NPL%20Programmers%20Reference%20Manual.md)** - Official NPL manual

---

## How to Use This Source Code

### 1. Reading NPL Source Files

NPL files can be opened in any text editor. They contain:

- **Procedure declarations** - `PROCEDURE name;`
- **Variable declarations** - `INTEGER var;`, `REF(type) ptr;`
- **Control structures** - `IF...THEN...ELSE`, `WHILE...DO`, `FOR...DO`
- **Assembly blocks** - `%ASSEMBLY_CODE%` for direct machine code
- **Comments** - `!` for line comments, `COMMENT ...;` for block comments

### 2. Cross-Referencing with Documentation

This source code should be read alongside the kernel documentation:

- **[SINTRAN/OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md](../OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md)** - System architecture
- **[SINTRAN/OS/13-INT14-HANDLER-DETAILED.md](../OS/13-INT14-HANDLER-DETAILED.md)** - Interrupt handling
- **[SINTRAN/OS/14-MONITOR-KERNEL-MONCALLS.md](../OS/14-MONITOR-KERNEL-MONCALLS.md)** - Monitor calls implementation
- **[SINTRAN/OS/15-DISK-IO-SUBSYSTEM.md](../OS/15-DISK-IO-SUBSYSTEM.md)** - Disk I/O code
- **[SINTRAN/OS/19-MEMORY-MAP-REFERENCE.md](../OS/19-MEMORY-MAP-REFERENCE.md)** - Memory layout

### 3. Analyzing Specific Components

| Component | NPL Files | Symbol Tables | Documentation |
|-----------|-----------|---------------|---------------|
| **SCSI Subsystem** | IP-P2-SCSI-*.NPL | SYMBOL-1-LIST.SYMB.TXT | [SINTRAN/Devices/SCSI/](../Devices/SCSI/) |
| **HDLC Driver** | MP-P2-HDLC-DRIV.NPL | SYMBOL-1-LIST.SYMB.TXT | [SINTRAN/Devices/HDLC/](../Devices/HDLC/) |
| **ND-500 Interface** | CC-P2-N500.NPL, MP-P2-N500.NPL | N500-SYMBOLS.SYMB.TXT | [SINTRAN/ND500/](../ND500/) |
| **Disk Management** | IP-P2-DISK-*.NPL, MP-P2-DISK-*.NPL | SYMBOL-1-LIST.SYMB.TXT | [SINTRAN/OS/15-DISK-IO-SUBSYSTEM.md](../OS/15-DISK-IO-SUBSYSTEM.md) |
| **Segment Admin** | IP-P2-SEGADM.NPL | SYMBOL-1-LIST.SYMB.TXT | [SINTRAN/OS/16-PAGE-FAULT-HANDLER.md](../OS/16-PAGE-FAULT-HANDLER.md) |
| **File System** | *(source not available)* | FILSYS-SYMBOLS.SYMB.TXT | [SINTRAN/OS/](../OS/) |
| **XMSG System** | *(source not available)* | XMSG-SYMBOL-LIST.SYMB.TXT | [SINTRAN/OS/14-MONITOR-KERNEL-MONCALLS.md](../OS/14-MONITOR-KERNEL-MONCALLS.md) |

---

## File Naming Convention

NPL source files follow the pattern: `[Prefix]-P2-[Component].NPL`

### Prefix Meanings

- **0** - System generation/initialization
- **5P** - Pass 5 (monitor/kernel)
- **CC** - Common compiler components
- **DP** - Data processing
- **IP** - Input processing
- **MP** - Main processing
- **TP** - Terminal processing

### P2 Designation

**P2** indicates **"Pass 2"** of the SINTRAN build process - this is the code generation and linking phase that produces executable kernel code.

---

## Extraction History

The NPL files in the `NPL/` subfolder were extracted from `s3vs-4.symb` by:

1. Identifying file boundaries in the SYMB listing
2. Extracting individual compilation units
3. Preserving original file names from the build job
4. Maintaining source code formatting and comments

**Original SYMB file**: Preserved as-is for reference and verification
**Split files**: Individual NPL files for easier navigation and analysis

---

## Related Documentation

### Kernel Architecture
- [SINTRAN/OS/ - Complete OS Documentation](../OS/)
- [SINTRAN/OS/README.md - Kernel Documentation Index](../OS/README.md)

### Device Drivers
- [SINTRAN/Devices/HDLC/ - HDLC Communication Controller](../Devices/HDLC/)
- [SINTRAN/Devices/SCSI/ - SCSI Disk Controllers](../Devices/SCSI/)

### ND-500 Coprocessor
- [SINTRAN/ND500/ - ND-500 Integration](../ND500/)
- [SINTRAN/ND500/ND500-INTEGRATION-GUIDE.md](../ND500/ND500-INTEGRATION-GUIDE.md)

### Reference Manuals
- [Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md](../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md)
- [Reference-Manuals/ND-60.128.1 EN NPL Programmers Reference Manual.md](../../Reference-Manuals/ND-60.128.1%20EN%20NPL%20Programmers%20Reference%20Manual.md)

---

## Notes for Developers

### Emulator Implementation

When implementing SINTRAN III behavior in the C# emulator:

1. **Read the NPL source first** - Understand exact algorithm and data structures
2. **Cross-reference documentation** - Verify understanding with kernel docs
3. **Match memory layouts** - Use [SINTRAN/OS/19-MEMORY-MAP-REFERENCE.md](../OS/19-MEMORY-MAP-REFERENCE.md)
4. **Preserve semantics** - Keep SINTRAN behavior identical, even if inefficient

### Source Code Authenticity

This source code is from a **real SINTRAN III build job** (s3vs-4). It represents production operating system code that ran on Norsk Data ND-100 computers in the 1980s.

**Verification**: All code can be traced to the original `s3vs-4.symb` file.

---

## Status

**Source Code Availability**: Partial (kernel components only)
**File System Code**: Missing (but symbols available in SYMBOLS/FILSYS-SYMBOLS.SYMB.TXT)
**Total NPL Files**: 45
**Total Symbol Tables**: 7 files (~450 KB)
**Total Size**: ~4.4 MB (SYMB file + symbols)

---

**Last Updated**: 2025-11-06
**SINTRAN Version**: SINTRAN III Version 4
**SINTRAN Level**: L07 (Level 7 - symbol tables)
**Build Job**: s3vs-4
