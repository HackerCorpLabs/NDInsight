# SINTRAN III Text Editors Guide

**Guide to Choosing and Using Text Editors for SINTRAN III Development**

**Version:** 1.0  
**Date:** October 18, 2025  
**Status:** Complete

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Editor Comparison](#2-editor-comparison)
3. [QED - Quick Editor](#3-qed---quick-editor)
4. [PED - Program Editor](#4-ped---program-editor)
5. [LED - Language-sensitive Editor](#5-led---language-sensitive-editor)
6. [TED - Text Editor](#6-ted---text-editor)
7. [Choosing the Right Editor](#7-choosing-the-right-editor)
8. [Editor Integration with Development Workflow](#8-editor-integration-with-development-workflow)

---

## 1. Introduction

SINTRAN III provides several text editors designed for different use cases and user preferences. This guide helps you understand the capabilities and appropriate use of each editor to enhance your development productivity.

### Available Editors

- **QED** - Quick EDitor: Line-oriented, compact, fast
- **PED** - Program EDitor: Full-screen, powerful, feature-rich
- **LED** - Language-sensitive EDitor: ND-500 only, syntax-aware
- **TED** - Text EDitor: Third-party editor (limited documentation)

---

## 2. Editor Comparison

| Feature | QED | PED | LED | TED |
|---------|-----|-----|-----|-----|
| **Mode** | Line-based | Full-screen | Full-screen | (Unknown) |
| **Platform** | ND-100 | ND-100 | ND-500 only | ND-100 |
| **Terminal** | Any (teletype) | Video terminal | NOTIS preferred | (Unknown) |
| **Language Awareness** | No | No | Yes (Fortran, Pascal) | No |
| **Learning Curve** | Easy | Moderate | Moderate | (Unknown) |
| **Memory Usage** | Low | Moderate | Higher | (Unknown) |
| **Best For** | Quick edits, batch | Program development | ND-500 dev | (Unknown) |
| **Syntax Highlighting** | No | No | Yes | No |
| **Multiple Windows** | No | Yes | Yes | No |
| **Integrated Debugger** | No | No | Yes | No |

---

## 3. QED - Quick Editor

### Overview

QED (Quick EDitor) is a powerful and compact text editor designed for rapid file editing. It operates in a line-oriented mode, making it suitable for both interactive terminal use and batch processing.

**Key Features:**
- Line-based editing interface
- Works on any terminal type (teletype compatible)
- QED-compatible command syntax
- Low memory footprint
- Fast startup time
- Excellent for quick edits and scripting

### When to Use QED

**Ideal for:**
- Quick file modifications
- Remote editing over slow connections
- Batch file processing
- Users comfortable with ed/vi-style editors
- Simple search and replace operations
- Editing on systems with limited resources

**Not ideal for:**
- Large file editing requiring visual overview
- Complex multi-window editing tasks
- Users who prefer visual, cursor-based editing

### Getting Started with QED

```sintran
@QED filename              # Start QED editing file
```

### Example: Creating a New File from Scratch

**Method 1: Create file when starting QED (Simpler)**

```sintran
@QED "HELLO:NPL"           # Start with filename in quotes - creates the file

QED 4.3
0 WORDS READ               # Confirms new file created

*A                         # Append mode - start adding lines
)PROGRAM HELLO;            # Type line 1 and press ENTER
DEFINE                     # Type line 2 and press ENTER
  OUTTEXT(1);              # Type line 3 and press ENTER
BEGIN                      # Type line 4 and press ENTER
  OUTTEXT("Hello, SINTRAN III!");  # Type line 5 and press ENTER
  OUTNEWLINE;              # Type line 6 and press ENTER
END                        # Type line 7 and press ENTER
*                          # Press CTRL-L (or . alone) to exit append mode
*W HELLO:NPL               # Save WITHOUT quotes - file already exists!
51 WORDS WRITTEN           # QED confirms the write
*EX                        # Exit QED
```

**Method 2: Create file when saving**

```sintran
@QED                       # Start QED without specifying a filename

QED 4.3

*A                         # Append mode - start adding lines
)PROGRAM HELLO;
DEFINE
  OUTTEXT(1);
BEGIN
  OUTTEXT("Hello, SINTRAN III!");
  OUTNEWLINE;
END
*                          # Exit append mode
*W "HELLO:NPL"             # Write with quotes - creates the file
64 WORDS WRITTEN
*EX                        # Exit QED
```

**Key Points:**
- **Method 1:** Start with `@QED "filename"` → Save with `W filename` (NO quotes)
- **Method 2:** Start with `@QED` → Save with `W "filename"` (WITH quotes)
- **Rule:** Quotes are needed when CREATING the file
  - Create on start → Quotes at start, no quotes when saving
  - Create on save → No quotes at start, quotes when saving
- `A` command enters append mode
- Press CTRL-L (or type `.` alone) to exit append mode
- Both methods work equally well - choose what feels natural!

### Editing an Existing File

Here's how to edit an existing file in QED:

```sintran
@QED HELLO:NPL             # Open existing file

QED 4.3
64 WORDS READ              # QED confirms file was read

*L1,$                      # List all lines (1 to end)
)PROGRAM HELLO;
DEFINE
  OUTTEXT(1);
BEGIN
  OUTTEXT("Hello, SINTRAN III!");
  OUTNEWLINE;
END

*C5                        # Change line 5 (the OUTTEXT line)
  OUTTEXT("Hello, WORLD!");  # Type the new content
                           # Press ENTER to confirm

*L1,$                      # List to verify the change
)PROGRAM HELLO;
DEFINE
  OUTTEXT(1);
BEGIN
  OUTTEXT("Hello, WORLD!");  # Line 5 is now changed
  OUTNEWLINE;
END

*W HELLO:NPL               # Write (save) - NO quotes needed for existing files!
64 WORDS WRITTEN

*EX                        # Exit QED
```

**Key Points for Editing Existing Files:**
- Open with `@QED filename` (no quotes needed)
- `C` followed by line number changes that line (e.g., `C5` = change line 5)
- Type the new content and press ENTER
- `W filename` saves changes - **NO quotes** needed for existing files!
- Use `L1,$` to list all lines and verify changes

**Quick Reference:**  
See **[QED-QUICK-REFERENCE.md](QED-QUICK-REFERENCE.md)** for complete command reference.

---

## 4. PED - Program Editor

### Overview

PED (Program EDitor) is SINTRAN III's advanced full-screen text editor, designed specifically for program and data file editing. It provides cursor-controlled editing with support for multiple windows, regions, and comprehensive text manipulation.

**Key Features:**
- Full-screen visual editing
- Multiple windows and regions (up to 40 regions)
- Cursor-controlled with function keys
- Support for NOTIS and FACIT 4420 terminals
- Page mode (full-screen) and Line mode (QED-compatible)
- Region-based editing
- Extensive search and replace capabilities
- Up to 255 columns per line

### When to Use PED

**Ideal for:**
- Program development (NPL, MAC, FORTRAN, etc.)
- Large file editing with visual overview
- Multi-file editing (regions)
- Users with video terminals
- Complex editing tasks requiring multiple views
- Professional development work

**Not ideal for:**
- Teletype terminals
- Very limited display capabilities
- Quick one-line edits (use QED)

### Getting Started with PED

```sintran
@PED filename              # Start PED editing file
```

**Basic workflow:**
```
@PED PROGRAM:NPL           # Open file in Page Mode
<Use cursor keys to navigate>
HOME                       # Go to command line
PED: W "PROGRAM:NPL"       # Write file
PED: E                     # Exit PED
```

**Common Operations:**
- **Navigate:** Arrow keys, SCROLL-UP/DOWN
- **Edit:** Insert mode (INS LED), cursor positioning
- **Mark area:** MARK key or FUNC <V>
- **Delete:** DELETE or FUNC D
- **Copy:** FUNC C
- **Execute SINTRAN:** @ command

**Quick Reference:**  
See **[PED-QUICK-REFERENCE.md](PED-QUICK-REFERENCE.md)** for complete command reference.

---

## 5. LED - Language-sensitive Editor

### Overview

LED (Language-sensitive programmers' EDitor) is a powerful screen-based integrated text editor and debugger specifically designed for the ND-500 architecture. It provides syntax-aware editing and integrated debugging capabilities.

**Key Features:**
- Language-sensitive editing (syntax awareness)
- Integrated debugger
- ND-500 architecture only
- Optimized for ND-NOTIS terminals
- VTM support (VT-100 compatible)
- Initial support for Fortran, later versions added Pascal and other languages

**When to Use LED**

**Ideal for:**
- ND-500 application development
- Fortran or Pascal programming on ND-500
- Developers needing integrated debugging
- NOTIS terminal users
- Syntax-aware editing and validation

**Not ideal for:**
- ND-100 development (LED is ND-500 only)
- Assembly language editing
- Non-supported languages

### Product Information

- **Product Number:** ND-211465
- **Availability:** SINTRAN III on ND-500 only (no ND-100 version)
- **Early Version:** LED-FORTRAN-A01 (Fortran only)
- **Later Versions:** Multi-language support

### More Information

For detailed information about LED, visit:  
**NDWiki:** https://www.ndwiki.org/wiki/LED_(Editor)

---

## 6. TED - Text Editor

### Overview

TED (Text EDitor) is a third-party text editor that originated from Rolf Terje Kvam, later KVAM Data. Limited documentation is currently available for TED.

**Known Information:**
- Developer: Rolf Terje Kvam / KVAM Data
- Platform: Likely ND-100
- Status: Limited documentation available

### More Information

For available information about TED, visit:  
**NDWiki:** https://www.ndwiki.org/wiki/TED

---

## 7. Choosing the Right Editor

### Decision Guide

Use this flowchart to select the appropriate editor:

```
START
  |
  ├─> Working on ND-500?
  │   └─> YES ──> Using Fortran/Pascal?
  │       └─> YES ──> Use LED
  │       └─> NO ──> Use PED
  │
  └─> NO (ND-100)
      |
      ├─> Need full-screen editing?
      │   └─> YES ──> Use PED
      │
      ├─> Quick edit or scripting?
      │   └─> YES ──> Use QED
      │
      └─> Teletype terminal only?
          └─> YES ──> Use QED (Line Mode)
```

### Recommendation by Task

| Task | Recommended Editor | Alternative |
|------|-------------------|-------------|
| **NPL Development** | PED | QED |
| **MAC Assembly** | PED | QED |
| **FORTRAN (ND-100)** | PED | QED |
| **FORTRAN (ND-500)** | LED | PED |
| **PASCAL (ND-500)** | LED | PED |
| **C Development** | PED | QED |
| **PLANC Development** | PED | QED |
| **Quick file edits** | QED | PED |
| **Batch processing** | QED | - |
| **Multi-file projects** | PED (regions) | - |
| **Remote editing** | QED | PED |
| **Script files (MODE)** | QED | PED |

---

## 8. Editor Integration with Development Workflow

### Typical Development Workflow

#### Application Development (FORTRAN, Pascal, PLANC, C)

```
1. CREATE SOURCE
   @PED PROGRAM:FORTRAN      # Or @LED for ND-500 Fortran
   <write code>
   PED: W "PROGRAM:FORTRAN"
   PED: E

2. COMPILE
   @FORTRAN PROGRAM:FORTRAN

3. LINK
   @NRL
   *PROG-FILE "PROGRAM"
   *LOAD PROGRAM
   *EXIT

4. TEST
   @PROGRAM

5. DEBUG AND REPEAT
   @PED PROGRAM:FORTRAN      # Edit source
   <fix bugs>
   @FORTRAN PROGRAM:FORTRAN  # Recompile
   @PROGRAM                  # Test again
```

#### System Programming (NPL, MAC)

```
1. CREATE SOURCE
   @PED DRIVER:NPL           # Full-screen editing
   <write code>
   PED: W "DRIVER:NPL"
   PED: E

2. COMPILE NPL
   @NPL DRIVER:NPL           # NPL -> MAC

3. ASSEMBLE
   @MAC DRIVER:MAC           # MAC -> BRF

4. LINK
   @NRL
   *PROG-FILE "DRIVER"
   *LOAD DRIVER
   *EXIT

5. TEST
   @DRIVER

6. DEBUG
   @PED DRIVER:NPL           # Edit source
   <analyze and fix>
   <repeat compile/test>
```

#### Quick Script Editing (QED)

```
1. EDIT SCRIPT
   @QED BUILD:COM
   *A$                       # Append at end
   @FORTRAN PROGRAM:FORTRAN
   @NRL
   *PROG-FILE "PROGRAM"
   *LOAD PROGRAM
   *EXIT
   <CTRL-L>
   *W BUILD:COM
   *EX

2. RUN SCRIPT
   @DO BUILD:COM
```

### Editor Combination Strategies

Many developers use multiple editors based on context:

**Strategy 1: PED for Development, QED for Quick Fixes**
- Use PED for main development work
- Switch to QED for quick one-line fixes
- Use QED for batch editing tasks

**Strategy 2: LED on ND-500, PED on ND-100**
- Use LED for ND-500 Fortran/Pascal development
- Use PED for ND-100 development and general editing
- Use QED for scripting and automation

**Strategy 3: Context-Dependent**
- Remote access → QED
- Office development → PED/LED
- Batch operations → QED scripts

### Editor Performance Tips

**PED:**
- Use regions to organize multi-file projects
- Set appropriate borders for language (B 5.72 for Fortran)
- Use FUNC keys for faster operations
- Save frequently with UPDATE command

**QED:**
- Use substitution command for bulk changes
- Set mode parameters appropriately
- Use line ranges effectively
- Keep commands short for speed

**LED:**
- Take advantage of syntax checking
- Use integrated debugger features
- Optimize for NOTIS terminal capabilities

---

## See Also

- **[QED-QUICK-REFERENCE.md](QED-QUICK-REFERENCE.md)** - Complete QED command reference
- **[PED-QUICK-REFERENCE.md](PED-QUICK-REFERENCE.md)** - Complete PED command reference
- **[QUICK-START-EXAMPLES.md](../QUICK-START-EXAMPLES.md)** - Example development workflows
- **[SINTRAN-DEVELOPER-GUIDE.md](../SINTRAN-DEVELOPER-GUIDE.md)** - Master developer guide

### External Resources

- **LED Editor:** https://www.ndwiki.org/wiki/LED_(Editor)
- **TED Editor:** https://www.ndwiki.org/wiki/TED

---

**Last Updated:** October 18, 2025  
**Version:** 1.0  
**Status:** Complete

