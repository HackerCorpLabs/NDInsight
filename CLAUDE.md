# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Project Overview

**NDInsight** is a comprehensive documentation and analysis repository for **Norsk Data NORD computer systems**, focusing on the **SINTRAN III operating system** (1970s-1980s real-time OS), hardware architecture (NORD-10, ND-100, ND-500 processors), and historical computer preservation through C# emulator implementation.

**Project Type**: Documentation repository with production C# emulator code
**Primary Language**: Markdown (210+ files)
**Programming**: Python scripts (13), PowerShell scripts (4), C# (1 production file)

---

## Critical Technical Knowledge

### ND-100 Memory Architecture

**Hardware:**
- 24-bit address bus (A23-A0)
- 16-bit data width - ALL memory and I/O access is in **WORDS**
- 16-bit CPU registers

**Address Structure:**
- Bits 0-15: Offset within bank (64KW addressable by 16-bit register)
- Bits 16-23: Bank/page selector (8 bits = 256 banks)

**Memory Notation Convention**: Always use WORDS as primary unit with bytes in parentheses:
- 1 word = 2 bytes (16 bits)
- 64KW (64K words) = 128KB
- 1 page (MMU) = 1024 words = 1KW (2KB)

**Bank Definition:**
- Each bank = **64KW (128KB)** - what a 16-bit register can address
- Total: 256 banks × 64KW = **16MW (32MB)**

**MMU Page Calculations:**
- ENDPAGE = 3777₈ (multiport) = 2048 pages × 1KW = 2MW (4MB)
- ENDPAGE = 37777₈ (no multiport) = 16384 pages × 1KW = 16MW (32MB)

---

## Development Commands

### Documentation Link Validation
```powershell
python scripts/find-unlinked-entries.py "Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md"
```

### OCR Cleanup and Processing
```powershell
python scripts/find-typos-simple.py
python scripts/apply-ocr-fixes.py
python scripts/extract-headers.py
.\scripts\extract-headers.ps1
```

### Content Processing
```powershell
.\scripts\extract-content.ps1
.\scripts\analyze-and-split.ps1
.\scripts\split-monitor-calls.ps1
```

---

## Architecture Overview

### Repository Structure

1. **Developer/**: Entry point for SINTRAN development (guides for 8 languages)
2. **Operations/**: Operator and user guides (COSMOS network, SINTRAN system administration)
3. **Reference-Manuals/**: Authoritative technical manuals (25 complete ND/SINTRAN manuals)
4. **SINTRAN/OS/**: Kernel subsystem documentation (numbered 00-19, 34 files)
5. **SINTRAN/NPL-SOURCE/**: SINTRAN III source code (45 NPL files + 7 symbol tables)
6. **SINTRAN/Release-Documentation/**: SINTRAN III release notes (versions J-N, 7 files)
7. **SINTRAN/SINTRAN Structures/**: System structures and data analysis (6 files)
8. **SINTRAN/Devices/**: Hardware drivers (HDLC, SCSI, Octobus)
9. **SINTRAN/Emulator/**: C# emulation implementation
10. **SINTRAN/ND500/**: ND-500 32-bit coprocessor analysis
11. **SINTRAN/TAD/**: TAD protocol analysis
12. **scripts/**: Python/PowerShell documentation maintenance tools (13 scripts)

### Documentation Patterns

**Pattern 1: Numbered Chapters** - `SINTRAN/OS/00-*.md` through `20-*.md`
**Pattern 2: Hierarchical Analysis** - `learning/`, `deep-dives/`, `reference/`, `implementation/`
**Pattern 3: Language-Specific** - `Developer/Languages/Application/` and `System/`

### Key Files

- `SINTRAN/NPL-SOURCE/README.md` - SINTRAN III source code index (45 NPL files)
- `SINTRAN/OS/20-MPM-VS-LOCAL-MEMORY-DETECTION.md` - MPM/MFbus/Octobus hardware configuration
- `SINTRAN/SINTRAN Structures/SINTRAN-STRUCTURES.md` - Complete kernel data structures reference
- `SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md` - Version evolution timeline
- `SINTRAN/XMSG/` - XMSG message system hub: `DOC/` (wire protocol, MON 200B API, operator commands), `SRC/` (C# protocol library + tests), official constants + `xmsg-constants.json`
- `SINTRAN/Emulator/ND500-EMULATION-COMPLETE.cs` - Production C# emulator code
- `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md` - Monitor calls reference
- `MERMAID_COLOR_STANDARDS.md` - WCAG 2.1 AA compliant diagram standards

---

## Important Constraints

### NEVER Make Assumptions

- If you don't know something, say "I don't know"
- Clearly separate verified facts from speculation
- When debugging: "What I can verify" vs "What I'm guessing"
- Ask for clarification rather than guessing

### Path Handling

- **NEVER use absolute paths** in markdown files (E:\, C:\, /)
- **ALWAYS use relative paths** for internal links
- Example: `[Link](../SINTRAN/OS/README.md)` NOT `[Link](E:\Dev\...\README.md)`

### Windows Environment

This is Windows. Use PowerShell or cmd, never Bash for system commands.
- Cannot `cd E:\path` without changing drive first - use `E:` then `cd \path`, or use PowerShell
- All Python scripts assume UTF-8 encoding
- Use backslashes for Windows paths in scripts

### Memory Documentation

When documenting memory sizes:
- Primary unit: **WORDS** (KW, MW)
- Secondary: (bytes in parentheses)
- Example: "64KW (128KB)" NOT just "128KB"

### Documentation Quality

- Only verified facts from SINTRAN III source code or official manuals
- Mark assumptions clearly: "ASSUMPTION:" or "UNVERIFIED:"
- Source citations for every claim
- Use Mermaid diagrams following `MERMAID_COLOR_STANDARDS.md`

### C# Code Standards

- No LINQ - forbidden
- No FluentAssertions
- Use Span/ArrayPool for performance, avoid allocations
- Prefer for loops over foreach
- Always validate that code compiles before reporting success
- Never create standalone test programs - use proper unit tests

### Git Workflow

- NEVER mention Claude in commit messages
- NEVER skip hooks (no --no-verify)
- NEVER force push to main/master
- Create NEW commits after hook failures (don't amend)

---

## Mermaid Diagram Colors

**Approved WCAG 2.1 AA Palette**:
- Sky Blue `#2196F3` - Frontend/Pass 1/Parsing
- Indigo `#3F51B5` - Grammar/Syntax Trees
- Purple `#9C27B0` - Template Matching
- Magenta `#E91E63` - Backend/Pass 2/Code Gen
- Red `#F44336` - Errors/Critical Paths
- Amber `#FFA726` - Preprocessor/Warnings
- Green `#4CAF50` - Success/Optimization
- Teal `#009688` - Register Allocation

---

## Navigation

Each major folder has a README.md. Key entry points:
- `README.md` - Project overview
- `CLAUDE.md` - This file (AI assistant guidance)
- `Developer/README.md` - Developer documentation index
- `Reference-Manuals/README.md` - Complete manual index (25 manuals)
- `SINTRAN/README.md` - SINTRAN III documentation overview (160+ files)
- `SINTRAN/NPL-SOURCE/README.md` - Source code index (45 NPL files + 7 symbol tables)
- `SINTRAN/OS/README.md` - Operating system architecture (00-19, 34 files)
- `SINTRAN/Release-Documentation/` - SINTRAN III release notes (versions J-N)
- `SINTRAN/SINTRAN Structures/` - System structures and data analysis
- `Operations/` - COSMOS and SINTRAN operator guides

---

**Last Updated**: 2026-02-15
**Repository Version**: 1.1
**Status**: Phase 1 Complete
