# Compiler Commands Reference

**Complete command reference for NPL and MAC**

**Version:** 1.0  
**Date:** October 17, 2025  
**Source:** Official manuals and source code analysis

---

## Table of Contents

1. [NPL Compiler Commands](#npl-compiler-commands)
2. [MAC Assembler Commands](#mac-assembler-commands)
3. [NRL Linker Commands](#nrl-linker-commands)
4. [BRF-EDITOR Commands](#brf-editor-commands)
5. [Quick Reference Tables](#quick-reference-tables)

---

## NPL Compiler Commands

### Starting the NPL Compiler

```bash
@NPL [source-file][:NPL] [,list-file] [,list-file]
```

**Parameters:**
- `source-file` - Input NPL source file
- `:NPL` - Optional extension (default)
- `list-file` - Optional listing output

**Examples:**
```bash
@NPL HELLO:NPL              # Compile HELLO:NPL to HELLO:MAC
@NPL MYPROG,LIST,LIST       # Compile with listing to LIST:LIST
@NPL SOURCE:NPL             # Simple compilation
```

### NPL Compiler Directives (Inside Source)

These directives appear **inside** NPL source files:

#### @DEV - Device/File Specification (Batch Mode)

**Syntax:**
```npl
@DEV device-name,line-count,line-count
```

**Purpose:** Redirect output to different device/file (used in master source files for batch compilation)

**Example:**
```npl
@DEV ABSTR,1,1     % Output to ABSTR:MAC
```

**Note:** Used in SINTRAN master source files when compiling multiple modules from one source.

---

## MAC Assembler Commands

### Starting the MAC Assembler

```bash
@MAC [source-file][:MAC]
```

**Interactive Mode:**
```bash
@MAC
*[commands]
*EXIT
```

**Examples:**
```bash
@MAC HELLO:MAC              # Assemble HELLO:MAC to HELLO:BRF
@MAC MYPROG                 # Assemble MYPROG:MAC
```

### MAC Assembler Directives

All MAC directives start with `)` and appear in MAC source files.

#### )FILL - Fill Memory

**Syntax:**
```assembly
)FILL [count]
```

**Purpose:** Reserve space, fill to boundary

**Examples:**
```assembly
)FILL          % Fill to next location
)FILL 10       % Reserve 10 words
RBUS           % Reserve bus word (same as )FILL)
```

#### )KILL - Remove Symbols

**Syntax:**
```assembly
)KILL symbol1 symbol2 ...
)KILL symbol1,symbol2,...
```

**Purpose:** Remove symbols from symbol table (clean up after use)

**Examples:**
```assembly
)KILL TEMP1 TEMP2      % Remove TEMP1 and TEMP2
)KILL L1,L2,L3         % Remove multiple labels
)KILL BRG L2           % Clean up temporary symbols
```

**Common Pattern:**
```assembly
L2=*
    LDA I (HSTAT
    ...
)KILL L2               % Remove after use
```

#### )ZERO - Zero Fill

**Syntax:**
```assembly
)ZERO count
```

**Purpose:** Fill with zero values

**Example:**
```assembly
)ZERO 100      % Reserve 100 words of zeros
```

#### )PCL - Set Location Counter (Print Control Line)

**Syntax:**
```assembly
)PCL octal-address
```

**Purpose:** Set assembly location counter

**Example:**
```assembly
)PCL 1000      % Set location to octal 1000
```

#### )LINE - Include Source Line

**Syntax:**
```assembly
)LINE
```

**Purpose:** Include a line in the listing (for debugging/documentation)

**Example:**
```assembly
)LINE          % This line will appear in listing
```

#### )9SLPL - Set Listing Options (9-track Symbolic List Print Line)

**Syntax:**
```assembly
)9SLPL
```

**Purpose:** Control listing format for symbolic output

**Example:**
```assembly
)9SLPL         % Enable symbolic listing
```

#### )9ASSM - Assemble with Options

**Syntax:**
```assembly
)9ASSM line-count,list-file,output-file
```

**Purpose:** Assemble with specific options

**Example:**
```assembly
)9ASSM 100,LIST-FILE,(OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
```

#### )9TSS - TSS System Directive

**Syntax:**
```assembly
)9TSS
```

**Purpose:** Indicate TSS (Timesharing System) compatibility mode

**Example:**
```assembly
)9TSS          % TSS mode
```

### Conditional Assembly

#### Quote Blocks - Conditional Code

**Syntax:**
```assembly
" condition
    [code if true]
"
```

**Purpose:** Conditional assembly based on symbol definition

**Example:**
```assembly
" ND100         % If ND100 is defined
    LDA 100
" ND500         % If ND500 is defined  
    LDWB R1,100
"               % End conditional
```

---

## NRL Linker Commands

### Starting NRL

```bash
@NRL
```

### NRL Commands (Interactive)

#### IMAGE - Specify Target CPU

**Syntax:**
```
*IMAGE cpu-type
```

**Parameters:**
- `100` - ND-100 target
- `500` - ND-500 target

**Example:**
```
*IMAGE 100
```

#### PROG-FILE - Specify Output File

**Syntax:**
```
*PROG-FILE "filename"
```

**Purpose:** Specify name of output executable

**Example:**
```
*PROG-FILE "HELLO"
*PROG-FILE "MYPROG"
```

#### LOAD - Load Object File

**Syntax:**
```
*LOAD filename
```

**Purpose:** Load BRF (object) file into link

**Examples:**
```
*LOAD HELLO           % Load HELLO:BRF
*LOAD CC-2HEADER      % Load C runtime header
*LOAD MYPROG          % Load MYPROG:BRF
```

#### START - Set Entry Point

**Syntax:**
```
*START symbol
*START address
```

**Purpose:** Specify program entry point

**Example:**
```
*START MAIN           % Start at MAIN symbol
*START 0              % Start at address 0
```

#### MAP - Generate Memory Map

**Syntax:**
```
*MAP
```

**Purpose:** Display memory allocation map

**Example:**
```
*MAP                  % Show memory layout
```

#### EXIT - Exit Linker

**Syntax:**
```
*EXIT
```

**Purpose:** Exit NRL and write output file

**Example:**
```
*EXIT                 % Save and exit
```

### NRL Output Messages

```
FREE: 000706-177777 ....        # Free memory range
FREE DATA AREA: 000202-177777   # Free data area
```

---

## BRF-EDITOR Commands

### Starting BRF-EDITOR

```bash
@BRF-EDITOR
```

### BRF-EDITOR Commands (Interactive)

#### RENAME-SYMBOL - Rename Symbol

**Syntax:**
```
RENAME-SYMBOL old-name new-name
```

**Purpose:** Rename a symbol in BRF file

**Example:**
```
RENAME-SYMBOL RI1F RIDB1F
RENAME-SYMBOL RINF RIDBNF
```

**Common Use:** Avoid symbol conflicts when linking multiple modules

#### CHANGE-FILE - Select File

**Syntax:**
```
CHANGE-FILE filename
```

**Purpose:** Select which BRF file to edit

**Example:**
```
CHANGE-FILE (OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
```

#### MAKE-LIBRARY-UNITS - Create Library

**Syntax:**
```
MAKE-LIBRARY-UNITS filename
```

**Purpose:** Convert BRF to library format

**Example:**
```
MAKE-LIBRARY-UNITS (OTS-CRS-BRF-1)IDB-VIA-MONC-OPR
```

**Use Case:** Create reusable library from object files

#### EXIT - Exit BRF-EDITOR

**Syntax:**
```
EXIT
```

**Purpose:** Save changes and exit

**Example:**
```
EXIT              % Save and quit
```

---

## Quick Reference Tables

### NPL Compiler Quick Reference

| Command | Purpose | Example |
|---------|---------|---------|
| `@NPL file:NPL` | Compile NPL to MAC | `@NPL HELLO:NPL` |
| `@NPL file,list,list` | Compile with listing | `@NPL PROG,OUT,OUT` |

### MAC Assembler Quick Reference

| Directive | Purpose | Example |
|-----------|---------|---------|
| `)FILL` | Reserve space | `)FILL 10` |
| `)KILL` | Remove symbols | `)KILL TEMP` |
| `)ZERO` | Zero fill | `)ZERO 100` |
| `)PCL` | Set location | `)PCL 1000` |
| `)LINE` | Include line | `)LINE` |
| `)9SLPL` | Listing options | `)9SLPL` |
| `)9ASSM` | Assemble options | `)9ASSM 100,LIST,OUT` |
| `)9TSS` | TSS mode | `)9TSS` |
| `"` | Conditional block | `" ND100` ... `"` |
| `RBUS` | Reserve bus word | `RBUS` |

### NRL Linker Quick Reference

| Command | Purpose | Example |
|---------|---------|---------|
| `*IMAGE` | Set target CPU | `*IMAGE 100` |
| `*PROG-FILE` | Output filename | `*PROG-FILE "HELLO"` |
| `*LOAD` | Load object file | `*LOAD HELLO` |
| `*START` | Set entry point | `*START MAIN` |
| `*MAP` | Show memory map | `*MAP` |
| `*EXIT` | Save and exit | `*EXIT` |

### BRF-EDITOR Quick Reference

| Command | Purpose | Example |
|---------|---------|---------|
| `RENAME-SYMBOL` | Rename symbol | `RENAME-SYMBOL OLD NEW` |
| `CHANGE-FILE` | Select file | `CHANGE-FILE file` |
| `MAKE-LIBRARY-UNITS` | Create library | `MAKE-LIBRARY-UNITS file` |
| `EXIT` | Save and exit | `EXIT` |

---

## Common Command Sequences

### Build NPL Program

```bash
@NPL HELLO:NPL              # 1. Compile
@MAC HELLO:MAC              # 2. Assemble
@NRL                        # 3. Link
*IMAGE 100
*PROG-FILE "HELLO"
*LOAD HELLO
*EXIT
@HELLO                      # 4. Run
```

### Build C Program

```bash
@CC-100 HELLO:C             # 1. Compile C to BRF
@NRL                        # 2. Link with runtime
*IMAGE 100
*PROG-FILE "HELLO"
*LOAD CC-2HEADER            # C runtime header
*LOAD HELLO                 # Your program
*LOAD CC-2BANK              # C library
*LOAD CC-2TRAILER           # C runtime trailer
*EXIT
@HELLO                      # 3. Run
```

### Rename Symbols for Library

```bash
@BRF-EDITOR
RENAME-SYMBOL RI1F RIDB1F
RENAME-SYMBOL RINF RIDBNF
RENAME-SYMBOL WI1F WIDB1F
RENAME-SYMBOL WINF WIDBNF
CHANGE-FILE module:BRF
MAKE-LIBRARY-UNITS module:BRF
EXIT
```

---

## Command Categories

### Compilation Phase

| Tool | Input | Output | Command |
|------|-------|--------|---------|
| **NPL** | `.NPL` | `.MAC` | `@NPL source:NPL` |
| **CC-100** | `.C` | `.BRF` | `@CC-100 source:C` |
| **PLANC** | `.PLANC` | `.BRF` | `@PLANC-100-C` |

### Assembly Phase

| Tool | Input | Output | Command |
|------|-------|--------|---------|
| **MAC** | `.MAC` | `.BRF` | `@MAC source:MAC` |

### Linking Phase

| Tool | Input | Output | Command |
|------|-------|--------|---------|
| **NRL** | `.BRF` | `.PROG` | `@NRL` + commands |

### Post-Processing

| Tool | Input | Output | Command |
|------|-------|--------|---------|
| **BRF-EDITOR** | `.BRF` | `.BRF` | `@BRF-EDITOR` + commands |

---

## Error Messages

### NPL Compiler Errors

```
END OF COMPILATION - X ERRORS DETECTED
```

**Meaning:** X syntax/semantic errors found during compilation

**Action:** Check listing for error details

### MAC Assembler Errors

```
PHASE ERROR
```

**Meaning:** Symbol value changed between assembly passes

**Action:** Check forward references, ensure symbols defined before use

```
UNDEFINED SYMBOL: symbolname
```

**Meaning:** Symbol used but not defined

**Action:** Define symbol or check spelling

### NRL Linker Errors

```
UNDEFINED SYMBOL: symbolname
```

**Meaning:** External symbol not found in loaded modules

**Action:** Load missing module containing symbol

```
FILE ALREADY EXISTS
```

**Meaning:** Output PROG file already exists

**Action:** Delete old file or use different name

---

## Command-Line Patterns

### Single-File Build

```bash
@NPL prog:NPL && @MAC prog:MAC && @NRL
```

### Multi-Module Build

```bash
@NPL mod1:NPL
@MAC mod1:MAC
@NPL mod2:NPL
@MAC mod2:MAC
@NRL
*LOAD mod1
*LOAD mod2
*PROG-FILE "combined"
*EXIT
```

### Build with Libraries

```bash
@NRL
*LOAD main
*LOAD mylib
*LOAD systemlib
*PROG-FILE "app"
*EXIT
```

---

## Special Symbols

### MAC Assembler Special Symbols

| Symbol | Meaning | Example |
|--------|---------|---------|
| `*` | Location counter | `LABEL=*` |
| `I` | Indirect addressing | `LDA I (ADDR` |
| `,X` | Indexed addressing | `LDA ,X OFFSET` |
| `,0` | Label reference | `JMP ,label` |

---

## Documentation References

### NPL
- **Manual:** ND-60.047.03 NORD PL User's Guide
- **Guide:** [NPL-DEVELOPER-GUIDE.md](NPL-DEVELOPER-GUIDE.md)

### MAC
- **Manual:** ND-60.096.01 MAC User's Guide
- **Guide:** [MAC-DEVELOPER-GUIDE.md](MAC-DEVELOPER-GUIDE.md)

### NRL
- **Manual:** ND-60.066.04 ND Relocating Loader
- **Guide:** [LINKING-GUIDE.md](LINKING-GUIDE.md)

### BRF-EDITOR
- **Manual:** ND-60.085.01 BRF EDITOR
- **Guide:** See [TOOLS-REFERENCE.md](TOOLS-REFERENCE.md)

---

## Notes

### Command Syntax
- `@` - SINTRAN command prefix
- `*` - NRL/interactive tool prompt
- `:` - File extension separator
- `,` - Parameter separator
- `%` - NPL/MAC comment

### File Extensions
- `.NPL` - NPL source
- `.MAC` - MAC assembly source
- `.BRF` - Binary Relocatable Format (object)
- `.PROG` - Executable program
- `.C` - C source
- `.PLANC` - PLANC source

### Case Sensitivity
- Commands are case-insensitive
- Filenames follow SINTRAN conventions
- Symbols are case-sensitive in some contexts

---

**Version:** 1.0  
**Last Updated:** October 17, 2025  
**Status:** Complete  
**Source:** Official manuals, source code analysis, verified examples

**See Also:**
- [QUICK-START-EXAMPLES.md](QUICK-START-EXAMPLES.md) - Working examples
- [MAC-DEVELOPER-GUIDE.md](MAC-DEVELOPER-GUIDE.md) - Complete MAC guide
- [LINKING-GUIDE.md](LINKING-GUIDE.md) - Complete linking guide
- [SINTRAN-DEVELOPER-GUIDE.md](SINTRAN-DEVELOPER-GUIDE.md) - Master guide

