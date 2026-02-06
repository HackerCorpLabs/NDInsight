# MON 60 (N500M) - ND-500 Monitor Function Reference

**Monitor Call:** 60B (octal)
**Name:** N500M / ND500Function
**Purpose:** Inter-subsystem communication for ND-500 control
**Source:** ND-60.136.04A ND-500 Loader Monitor, Chapter 10

---

## Introduction for Newcomers

### What is MON 60?

If you're new to Norsk Data systems, here's what you need to know:

**MON 60** is a SINTRAN III "monitor call" - essentially a system call that lets programs running on the **ND-100** (the main 16-bit computer) control the **ND-500** (a powerful 32-bit coprocessor).

Think of it like this:
- The **ND-500** is a fast computation engine, but it can't do anything by itself
- The **ND-100** is the "brain" that runs the operating system and handles all I/O
- **MON 60** is how the ND-100 tells the ND-500 what to do

### Why Does This Exist?

The ND-500 was designed in the 1980s as an add-on to boost computing power. Rather than replacing the ND-100, Norsk Data made them work together:

```
┌─────────────────┐                    ┌─────────────────┐
│     ND-100      │   "Do this math"   │     ND-500      │
│  ─────────────  │ ──────────────────►│  ─────────────  │
│  Operating      │                    │  Fast 32-bit    │
│  System         │◄────────────────── │  computation    │
│  All I/O        │   "Here's result"  │  No I/O         │
└─────────────────┘                    └─────────────────┘
```

### How is MON 60 Used?

When you run `@ND-500-MONITOR` at the SINTRAN prompt, you're using a program that internally uses MON 60 to:
- Load programs into ND-500 memory
- Start and stop ND-500 execution
- Read/write ND-500 registers
- Debug ND-500 programs

### Related Documentation

- **[60B_N500M_Hardware_Mapping.md](60B_N500M_Hardware_Mapping.md)** - How MON 60 talks to hardware (TAG registers, IOX commands)
- **[60B_N500M.yaml](60B_N500M.yaml)** - Structured data version of this document

---

## Technical Overview

MON 60 (N500M) is the primary interface between ND-100 programs and the ND-500 coprocessor. It provides 67 subfunctions (0B through 142B octal) for:

- Register and memory access
- Process management
- Control store operations
- Memory configuration
- Performance monitoring (histograms)
- Logging and debugging
- Domain management

### Architecture

The ND-500 Monitor runs entirely on the ND-100 in three parts:

```
+------------------------------------------------------------------+
|                         ND-100                                    |
+------------------------------------------------------------------+
|  PAGE TABLE 2 (User Space)                                        |
|  +--------------------------------------------------------------+ |
|  |  ND-500-MONITOR Subsystem                                    | |
|  |  - User interface and command parsing                        | |
|  |  - High-level operations                                     | |
|  +--------------------------------------------------------------+ |
|                              |                                    |
|                        MON 60 (N500M)                             |
|                        Function code + parameters                 |
|                              v                                    |
+------------------------------------------------------------------+
|  PAGE TABLE 0 (Kernel)                                            |
|  +--------------------------------------------------------------+ |
|  |  N500M Handler                                               | |
|  |  - Function dispatch (67 subfunctions)                       | |
|  |  - Process management                                        | |
|  |  - Memory allocation                                         | |
|  |  - 5MPM shared memory access                                 | |
|  +--------------------------------------------------------------+ |
|                              |                                    |
+------------------------------------------------------------------+
|  RESIDENT (Paging Off)                                            |
|  +--------------------------------------------------------------+ |
|  |  ND-500 Driver                                               | |
|  |  - 3022 interface control (IOX instructions)                 | |
|  |  - Level 12 interrupt handling                               | |
|  +--------------------------------------------------------------+ |
+------------------------------------------------------------------+
                               |
                          3022 / 5MPM
                               v
+------------------------------------------------------------------+
|                         ND-500                                    |
|  - Executes domains/programs                                      |
|  - No direct I/O capability                                       |
+------------------------------------------------------------------+
```

### Parameter Passing Convention

MON 60 uses **Fortran-style parameter passing**:

1. **A-register** points to a parameter address list
2. **First word** is the 16-bit function code
3. **Remaining words** are function-specific parameters (32-bit words or arrays)
4. **Skip return** = successful completion
5. **Direct return** = error occurred (error code in A-register)

```
; MAC assembly example
        LDA    (PARAMS        ; A-register -> parameter list
        MON    60             ; Call N500M
        JMP    ERROR          ; Direct return = error
        ...                   ; Skip return = success

PARAMS, <function-code>       ; First word: function code (16-bit)
        <param1>              ; Second word: first parameter
        <param2>              ; Third word: second parameter
        ...
```

---

## Function Reference

### Category 1: System Services (0B - 14B)

#### 0B - RRREG (Read Register)

**Purpose:** Read a single ND-500 register

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| reg.no | INT | I | Register number to read |
| value | INT4 | O | Register value returned |

---

#### 1B - WRREG (Write Register)

**Purpose:** Write a single ND-500 register

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| reg.no | INT | I | Register number to write |
| value | INT4 | I | Value to write |

---

#### 2B - RPROG (Read Program Memory)

**Purpose:** Read from ND-500 program address space

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no.of bytes | INT4 | I | Number of bytes to read |
| ND-500 addr | INT4 | I | Source address in ND-500 program memory |
| data area | ARR | O | Buffer to receive data |
| bytes returned | INT4 | O | Actual bytes read |

---

#### 3B - RDATA (Read Data Memory)

**Purpose:** Read from ND-500 data address space

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no.of bytes | INT4 | I | Number of bytes to read |
| ND-500 addr | INT4 | I | Source address in ND-500 data memory |
| data area | ARR | O | Buffer to receive data |
| bytes returned | INT4 | O | Actual bytes read |

---

#### 4B - WPROG (Write Program Memory)

**Purpose:** Write to ND-500 program address space

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no.of bytes | INT4 | I | Number of bytes to write |
| ND-500 addr | INT4 | I | Destination address in program memory |
| data area | ARR | I | Data to write |

---

#### 5B - WDATA (Write Data Memory)

**Purpose:** Write to ND-500 data address space

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no.of bytes | INT4 | I | Number of bytes to write |
| ND-500 addr | INT4 | I | Destination address in data memory |
| data area | ARR | I | Data to write |

---

#### 6B - PLACE (Place Segment)

**Purpose:** Load a segment file into ND-500 memory

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| file name | STRING | I | Segment file to load |
| segment base | INT4 | I | Base address for segment |
| size in bytes | INT4 | I | Size of segment |
| segment type | INT | I | Type code (program/data) |

---

#### 7B - SWLOD (Load Swapper)

**Purpose:** Load the swapper segment

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| swapper segment name | STRING | I | Name of swapper segment file |

**Notes:** The swapper is actually an ND-100 RT program, not ND-500 code.

---

#### 10B - RRREG_BLOCK (Read Registers Block)

**Purpose:** Read multiple ND-500 registers as a block

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| register block | ARR | O | Array to receive register values |

---

#### 11B - WRREG_BLOCK (Write Registers Block)

**Purpose:** Write multiple ND-500 registers as a block

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| register block | ARR | I | Array of register values to write |

---

#### 12B - RUNN (Start Program)

**Purpose:** Start execution of an ND-500 domain/program

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| stop reason | INT4 | O | Reason execution stopped (see below) |
| returned trap info | INT4 | O | Trap information if applicable |
| clear time used | INT | I | Flag to clear time accounting |

**Stop Reason Values:**
- 65 (101B) - Normal completion via MON 407B (TPSTRA)
- Other values indicate traps or errors

**Notes:** Seven parameters are transferred back to ND-100 on return.

---

#### 13B - CNCFI (Connect File)

**Purpose:** Connect a file for ND-500 process use

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| file name | STRING | I | File to connect |
| access code | INT | I | Access mode (read/write/etc.) |
| default type | INT | I | Default file type |
| connect no. | INT | I | Requested connection number (0 = any) |
| returned connect no. | INT | O | Actual connection number assigned |

---

#### 14B - CLSFI (Close File)

**Purpose:** Close a connected file

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| file no. | INT | I | Connection number to close |

---

### Category 2: Process Management (15B - 37B)

#### 15B - RESRV (Reserve ND-500 Process)

**Purpose:** Allocate an ND-500 process from an ND-100 RT program

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| start addr. after escape | INT4 | I | Restart address if user escapes |
| version string of PTO | STRING | I | Page Table Owner version string |

**Notes:**
- Creates a process that will be terminated via RELIS (16B)
- Requires detailed knowledge of monitor operation
- Used by RT programs that manage ND-500 processes

**Example (MAC):**
```
        LDA    (RESRVP
        MON    60
        JMP    ERROR
        ; Process reserved successfully

RESRVP, 15                    ; Function code RESRV
        STADDR                ; Start address after escape
        VERSTR                ; Version string
```

---

#### 16B - RELIS (Release ND-500 Process)

**Purpose:** Release a previously reserved ND-500 process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

**Notes:** Releases all resources associated with the process reserved by RESRV.

---

#### 17B - LISOP (List Open Files)

**Purpose:** List all files currently open by the ND-500 process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Output goes to terminal |

---

#### 20B - TIMUS (Time Used)

**Purpose:** Get CPU time used by current process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Time returned in standard format |

---

#### 21B - WHO (Who Is On)

**Purpose:** List active ND-500 processes

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Output goes to terminal |

---

#### 22B - ERRFL (Set Error Flag)

**Purpose:** Set the error flag value

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| value | INT | I | Error flag value |

---

#### 23B - REACS (Read Control Store)

**Purpose:** Read from ND-500 control store (microcode memory)

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| CS addr. | INT4 | I | Control store address |
| no of 16 bit words | INT | I | Number of words to read |
| data-area | ARR | O | Buffer to receive data |

**Notes:** Control store is 144 bits wide, transferred as 16-bit words.

---

#### 24B - WRICS (Write Control Store)

**Purpose:** Write to ND-500 control store

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| CS addr. | INT4 | I | Control store address |
| no of 16 bit words | INT | I | Number of words to write |
| data-area | ARR | I | Data to write |

**Notes:** Used for microcode patching or custom microprogram loading.

---

#### 25B - MICST (Start Microprogram)

**Purpose:** Start microprogram execution at specified address

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| micro program start address | INT4 | I | Control store address to begin execution |

---

#### 26B - DMEXAM (Data Memory Examine)

**Purpose:** Examine (read) single data memory location

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| addr. | INT4 | I | Address to examine |
| value | INT4 | O | Value at address |

---

#### 27B - DMDEP (Data Memory Deposit)

**Purpose:** Deposit (write) to single data memory location

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| addr. | INT4 | I | Address to modify |
| value | INT4 | I | Value to deposit |

---

#### 30B - PMEXAM (Program Memory Examine)

**Purpose:** Examine single program memory location

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| addr. | INT4 | I | Address to examine |
| value | INT4 | O | Value at address |

---

#### 31B - PMDEP (Program Memory Deposit)

**Purpose:** Deposit to single program memory location

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| addr. | INT4 | I | Address to modify |
| value | INT4 | I | Value to deposit |

---

#### 32B - ABSMR (Absolute Memory Read)

**Purpose:** Read from absolute physical memory

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no. of bytes | INT4 | I | Number of bytes to read |
| ND-500 addr. | INT4 | I | Physical address |
| data area | ARR | O | Buffer to receive data |
| bytes returned | INT4 | O | Actual bytes read |

**Notes:** Bypasses memory management - direct physical access.

---

#### 33B - ABSMW (Absolute Memory Write)

**Purpose:** Write to absolute physical memory

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no. of bytes | INT4 | I | Number of bytes to write |
| ND-500 addr. | INT4 | I | Physical address |
| data area | ARR | I | Data to write |

---

#### 34B - MSTOP (Stop Microprogram)

**Purpose:** Stop microprogram execution

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 35B - MSTCL (Master Clear)

**Purpose:** Master clear the ND-500

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

**Notes:** Resets the ND-500 hardware via 3022 interface MCLR5 command.

---

#### 37B - LDCS (Load Control Store)

**Purpose:** Load control store from file

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| CS addr | INT4 | I | Starting address in control store |
| no of words | INT4 | I | Number of words to load |
| file name | STRING | I | Microcode file (typically CONTROL-STORE:DATA) |

---

### Category 3: Memory Management (40B - 61B)

#### 40B - DEFM (Define Memory Configuration)

**Purpose:** Define ND-500 memory configuration

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| start page | INT4 | I | First page number |
| no. of memory parts | INT | I | Number of memory regions |
| part array | ARR | I | Array describing each memory region |

---

#### 41B - RSTAT (Read Communication Status)

**Purpose:** Read 3022/5015 interface status

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| status | INT4 | O | Status bits (16:31=ND-500, 0:15=ND-100) |
| MAR | INT4 | O | Memory Address Register value |

**Status Bits (ND-100 side, bits 0:15):**
- Bit 5: Interface locked
- Bit 6: DMA error
- Bit 7: Power fail
- Bit 8: Power was off
- Bit 9: Clock stopped

---

#### 43B - SPRES (Reserve for Special Use)

**Purpose:** Reserve ND-500 for exclusive/special use

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 44B - SPREL (Release After Special Use)

**Purpose:** Release ND-500 from exclusive use

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 45B - DEFSW (Define Swap File)

**Purpose:** Define a swap file for paging

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| file name | STRING | I | Swap file name |

---

#### 47B - DELSW (Delete Swap File)

**Purpose:** Delete a swap file

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| file name | STRING | I | Swap file to delete |

---

#### 50B - TESTF (Test Function)

**Purpose:** Internal test function

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| I1 | INT4 | I | Test parameter 1 |
| I2 | INT4 | I | Test parameter 2 |
| I3 | INT4 | I | Test parameter 3 |
| I4 | INT4 | I | Test parameter 4 |

---

#### 51B - RIFRG (Read Interface Register)

**Purpose:** Read 3022 interface register

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| register value | INT4 | O | Interface register value |

---

#### 52B - G500P (Give ND-500 Pages)

**Purpose:** Allocate memory pages to ND-500

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| number of pages | INT4 | I | Pages to allocate |

---

#### 53B - T500P (Take ND-500 Pages)

**Purpose:** Reclaim memory pages from ND-500

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| number of pages | INT4 | I | Pages to reclaim |

---

#### 54B - STSWP (Start Swapper)

**Purpose:** Start the swapper RT program

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 55B - SPLAC (Start Place)

**Purpose:** Start segment placement operation

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 56B - EPLAC (End Place)

**Purpose:** End segment placement operation

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 57B - MPVER (Microprogram Version)

**Purpose:** Get microprogram version number

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| version number | INT4 | O | Microprogram version |

---

#### 60B - LIMEM (List Memory Configuration)

**Purpose:** Get current memory configuration

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| array | ARR | O | Memory configuration data |

---

#### 61B - RESER (Reserve ND-500 and Memory)

**Purpose:** Reserve ND-500 with specified memory

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no. of pages | INT4 | I | Number of pages to reserve |
| first page no. | INT4 | O | First allocated page number |

**Notes:** Use RELMEM (123B) to release.

---

### Category 4: Histogram & Logging (62B - 72B)

#### 62B - HIDEF (Define Histogram)

**Purpose:** Define a performance histogram

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| start address | INT4 | I | Start of address range to monitor |
| interval size | INT4 | I | Size of each histogram bucket |
| no. of intervals | INT | I | Number of histogram buckets |

---

#### 63B - HISTA (Start Histogram)

**Purpose:** Start histogram collection

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 64B - HISTP (Stop Histogram)

**Purpose:** Stop histogram collection

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 65B - HISTN (Read Histogram)

**Purpose:** Read collected histogram data

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| array | ARR | O | Histogram data |

---

#### 66B - HIREL (Release Histogram)

**Purpose:** Release histogram resources

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 67B - SPRTE (Search for Process Entry)

**Purpose:** Search for a process by name

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process name | STRING | I | Name to search for |
| record | ARR | O | Process entry data |

---

#### 70B - GPRTE (Get Process Entry)

**Purpose:** Get process entry by number

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process | INT | I | Process number |
| record | ARR | O | Process entry data |

---

#### 71B - SSGTE (Search for Physical Segment)

**Purpose:** Search for a physical segment by name

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| name of phys. segment | STRING | I | Segment name |
| array | ARR | O | Segment data |

---

#### 72B - GSGTE (Get Physical Segment)

**Purpose:** Get physical segment by number

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| phys. segment no. | INT | I | Segment number |
| array | ARR | O | Segment data |

---

### Category 5: Process Control (73B - 110B)

#### 73B - RPHSG (Read Physical Segment)

**Purpose:** Read data from a physical segment

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| phys.segment no. | INT | I | Segment number |
| address | INT4 | I | Offset within segment |
| no. of bytes | INT4 | I | Bytes to read |
| array | ARR | O | Data buffer |

---

#### 74B - SPRNM (Set Process Name)

**Purpose:** Set the current process name

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process name | STRING | I | New process name |

---

#### 75B - USYST (User SYSTEM Test)

**Purpose:** Test if user is SYSTEM

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Skip return if SYSTEM |

---

#### 76B - TOSWP (Send Message to Swapper)

**Purpose:** Send a message to the swapper RT program

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| record | ARR | I | Message record |

---

#### 77B - RPROC (Read Last Message)

**Purpose:** Read last message for a process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process number |
| record | ARR | O | Message record |

---

#### 100B - RFLAG (Read Process Flag)

**Purpose:** Read 32-bit process communication flag

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process number |
| flag | INT4 | O | 32-bit flag value |

**Notes:** Equivalent to SINTRAN MON 402B (Read input flag) for ND-500 processes.

**Example (MAC):**
```
        LDA    (RFLAGP
        MON    60
        JMP    ERROR
        ; Flag value now at FLAGVAL

RFLAGP, 100                   ; Function code RFLAG
        PROCNO                ; Process number
FLAGVAL,0                     ; 32-bit flag returned here
        0                     ; (high word)
```

---

#### 101B - SPFLAG (Set Process Flag)

**Purpose:** Set 32-bit process communication flag

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process number |
| flag | INT4 | I | 32-bit flag value to set |

**Notes:** Equivalent to SINTRAN MON 403B (Write output flag) for ND-500 processes. Also known as SFLAG in some documentation.

---

#### 102B - GPSGE (Release ND-500 System)

**Purpose:** Release ND-500 system resources

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 103B - RSYSP (Read System Parameters)

**Purpose:** Read system parameter array

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| parameter array | ARR | O | System parameters |

---

#### 104B - WSYSP (Write System Parameters)

**Purpose:** Write system parameter array

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| parameter array | ARR | I | System parameters |

---

#### 105B - SPRIO (Set Priority)

**Purpose:** Set process priority and CPU limits

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| ND-100 mon call priority | INT | I | Priority for ND-100 monitor calls |
| max percent CPU time | INT | I | Maximum CPU percentage allowed |

---

#### 106B - LNKPR (Link to Process)

**Purpose:** Link current process to another

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process to link to |

---

#### 110B - WPHSG (Write Physical Segment)

**Purpose:** Write data to a physical segment

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| segm no. | INT | I | Segment number |
| ND-500 address | INT4 | I | Offset within segment |
| no. of bytes | INT4 | I | Bytes to write |
| data area | ARR | I | Data to write |

---

### Category 6: Process Logging (111B - 126B)

#### 111B - SLOG1 (Start Process Log One)

**Purpose:** Start logging for one process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process to log |

---

#### 112B - STOPLOG (Stop Logging)

**Purpose:** Stop process logging

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 113B - RLOG (Read Log Info)

**Purpose:** Read logged information

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| data area | ARR | O | Log data |

---

#### 114B - RELLOG (Release Log Facility)

**Purpose:** Release logging resources

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 115B - SLOGA (Start Log All)

**Purpose:** Start logging all active processes

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 117B - ABORT (Abort Process)

**Purpose:** Abort an ND-500 process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no. | INT | I | Process to abort |

---

#### 120B - SETOUT (Set Output Device)

**Purpose:** Set output device for process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| unit | INT | I | Device unit number |

---

#### 121B - RDSWP (Read from Swapper)

**Purpose:** Read data from swapper memory

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| no. of bytes | INT4 | I | Bytes to read |
| ND-500 address | INT4 | I | Source address |
| data area | ARR | O | Data buffer |
| bytes read | INT4 | O | Actual bytes read |

---

#### 122B - LOGOUT (Logout Process)

**Purpose:** Log out an ND-500 process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| process no | INT | I | Process to log out |

---

#### 123B - RELMEM (Release Memory)

**Purpose:** Release memory reserved by RESER (61B)

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 124B - SMONLOG (Start Moncall Log)

**Purpose:** Start monitor call logging

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 125B - PMONLOG (Print Moncall Log)

**Purpose:** Print collected monitor call log

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| array | ARR | O | Array of 1K 16-bit words |

---

#### 126B - XMONLOG (Stop/Release Moncall Log)

**Purpose:** Stop and release monitor call logging

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

### Category 7: Domain Management (127B - 142B)

#### 127B - DEFDOM (Define Standard Domain)

**Purpose:** Define a standard domain configuration

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| array | ARR | I | Domain definition |

---

#### 130B - PLADOM (Place Standard Domain)

**Purpose:** Place (load) a standard domain

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| name | STRING | I | Domain name |

---

#### 131B - DELDOM (Delete Standard Domain)

**Purpose:** Delete a standard domain definition

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| name | STRING | I | Domain name to delete |

---

#### 132B - LSTDOM (List Standard Domain)

**Purpose:** List defined standard domains

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Output to terminal |

---

#### 133B - LSTEXQ (List Execution Queue)

**Purpose:** List processes in execution queue

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | Output to terminal |

---

#### 134B - PLADBG (Place Debugger)

**Purpose:** Place the ND-500 debugger

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 135B - LOGABT (Logout and Abort)

**Purpose:** Log out process and abort corresponding RT-programs

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 136B - ACTIV (Activate)

**Purpose:** Activate a stopped process

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 137B - UNUSED

**Purpose:** Not used (reserved)

---

#### 140B - SRESPL (Start Residual Place)

**Purpose:** Start residual placement operation

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (none) | - | - | - |

---

#### 141B - SETBLK (Set Block Size)

**Purpose:** Set block size of a file

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (parameters not documented) | - | - | - |

---

#### 142B - DEFINF (Redefine Default Infant File)

**Purpose:** Redefine the default infant file

| Parameter | Type | I/O | Description |
|-----------|------|-----|-------------|
| (parameters not documented) | - | - | - |

---

## Error Handling

### Return Convention

- **Skip return:** Function completed successfully
- **Direct return:** Error occurred, error code in A-register

### Common Error Codes

| Code | Description |
|------|-------------|
| 1 | Illegal function code |
| 2 | ND-500 not present |
| 3 | ND-500 not available |
| 4 | Process not reserved |
| 5 | Invalid parameter |
| 6 | Memory allocation failed |
| 7 | File not found |
| 8 | Access denied |

### Error Message Example

"Illegal Function Code in MON 60" - verify function code is in range 0B-142B.

---

## Programming Examples

### MAC Assembly - Reserve and Release Process

```
; Reserve an ND-500 process
        LDA    (RESRVP
        MON    60
        JMP    RESERR         ; Error reserving
        ; Process reserved successfully

        ; ... do work with ND-500 ...

; Release the process
        LDA    (RELISP
        MON    60
        JMP    RELERR         ; Error releasing
        ; Process released
        JMP    DONE

RESRVP, 15                    ; Function RESRV
        0                     ; Start address after escape
        VERSTR                ; Version string pointer

RELISP, 16                    ; Function RELIS (no parameters)

VERSTR, 'PTO V1.0           ' ; 16-character version string

RESERR, ...                   ; Handle reservation error
RELERR, ...                   ; Handle release error
DONE,   ...
```

### MAC Assembly - Read/Write Process Flags

```
; Read flag from process 3
        LDA    (RFLAGP
        MON    60
        JMP    FLGERR
        ; Flag value now at FLAGVAL

; Set flag for process 3
        LDA    (SFLAGP
        MON    60
        JMP    FLGERR
        JMP    DONE

RFLAGP, 100                   ; Function RFLAG
        3                     ; Process number
FLAGVAL,0                     ; Flag value (low word)
        0                     ; Flag value (high word)

SFLAGP, 101                   ; Function SPFLAG
        3                     ; Process number
        177777                ; New flag value (low word)
        177777                ; New flag value (high word)

FLGERR, ...                   ; Handle flag error
DONE,   ...
```

### FORTRAN - Read Control Store

```fortran
      INTEGER*2 FUNCCODE
      INTEGER*4 CSADDR, WORDS
      INTEGER*2 CSBUF(100)

C     Read 20 words from control store address 1000
      FUNCCODE = 23           ! Function 23B = REACS
      CSADDR = 1000
      WORDS = 20

      CALL MONITOR_CALL('N500M', FUNCCODE, CSADDR, WORDS, CSBUF)
      IF (ERRCODE .NE. 0) THEN
          WRITE(*,*) 'Error reading control store:', ERRCODE
          STOP
      ENDIF

C     CSBUF now contains control store data
```

### PLANC - Start Domain Execution

```planc
INTEGER : FUNCCODE
INTEGER4 : STOPREASON, TRAPINFO
INTEGER : CLEARTIME

ON ROUTINEERROR DO
    IF ErrCode > 0 THEN
        % Handle error
    ENDON
ENDON

% Execute domain
FUNCCODE := 12B               % RUNN
CLEARTIME := 1                % Clear time accounting
Monitor_Call('N500M', FUNCCODE, STOPREASON, TRAPINFO, CLEARTIME)

% Check stop reason
IF STOPREASON = 65 THEN
    % Normal completion
ELSE
    % Abnormal termination
ENDIF
```

---

## Related Documentation

| Document | Description |
|----------|-------------|
| [ND-60.136.04A ND-500 Loader Monitor](../../Reference-Manuals/ND-60.136.04A%20ND-500%20Loader%20Monitor.md) | Official MON 60 reference (Chapter 10) |
| [ND-860228-2-EN SINTRAN III Monitor Calls](../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md) | Monitor calls reference manual |
| [ND500-MONITOR-CALL-MECHANISM.md](../../SINTRAN/ND500/ND500-MONITOR-CALL-MECHANISM.md) | Analysis of inter-processor calls |
| [ND500-MONITOR-CALL-PARAMETER-PASSING.md](../../SINTRAN/ND500/ND500-MONITOR-CALL-PARAMETER-PASSING.md) | Parameter passing details |
| [ND500-SWAPPER-ANALYSIS.md](../../SINTRAN/ND500/ND500-SWAPPER-ANALYSIS.md) | Swapper operation details |

---

## Revision History

| Date | Version | Author | Changes |
|------|---------|--------|---------|
| 2025-02-05 | 1.0 | Claude | Initial comprehensive documentation |

---

*Source: ND-60.136.04A ND-500 Loader Monitor, Chapter 10, pp. 192-195*
*Scanned by Jonny Oddene for Sintran Data*
