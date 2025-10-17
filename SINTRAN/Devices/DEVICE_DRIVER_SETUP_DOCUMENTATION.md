# SINTRAN III Device Driver Setup and Data Field Mapping

## Overview

This document maps the data fields described in **ND-60112-01-EN SINTRAN III System Documentation - Appendix A** to the actual device driver implementations in the SINTRAN L NPL source code. It explains how SINTRAN sets up its device drivers during system initialization.

**Document Version:** Based on SINTRAN III/VS (1978 version)  
**Source Files Location:** `Z:\NorskData\Source Code\Sintran L\NPL`  
**Generated:** October 16, 2025

---

## Table of Contents

1. [System Architecture](#system-architecture)
2. [Data Field Structure](#data-field-structure)
3. [Device Driver Mapping](#device-driver-mapping)
4. [Initialization Process](#initialization-process)
5. [Driver Categories](#driver-categories)
6. [Reference Tables](#reference-tables)

---

## System Architecture

### Overview of SINTRAN Device Driver System

SINTRAN III uses a **data field-based architecture** where each device or device type has an associated data structure (data field) that contains:

- Device control information
- Hardware device numbers (HDEV (=177775))
- Driver entry points
- Buffer management structures
- State information
- Error handling data

### Key Components

| Component           | Source File(s)                    | Purpose                                        |
|---------------------|-----------------------------------|------------------------------------------------|
| System Generator    | `0.SIN-GEN.NPL`                   | Configuration and library mark definitions     |
| Configuration Table | `PH-P2-CONFG-TAB.NPL`             | Device configuration tables                    |
| Startup Base        | `PH-P2-START-BASE.NPL`            | System startup and memory layout               |
| Monitor Calls       | `RP-P2-MONCALLS.NPL`              | Monitor call interface                         |
| Terminal Driver     | `MP-P2-TERM-DRIV.NPL`             | Terminal I/O driver                            |
| HDLC (=103112) Driver         | `MP-P2-HDLC-DRIV.NPL`             | HDLC (=103112) communication driver                      |
| X.21 Driver         | `MP-P2-X21-DRIV.NPL`              | X.21 network interface driver                  |
| PIOC Driver         | `MP-P2-PIOC-DRIV.NPL`             | PIOC processor communication driver            |
| SCSI Driver         | `IP-P2-SCSI-DRIV.NPL`             | SCSI device interface driver                   |

---

## Data Field Structure

### Common Data Field Layout

All device data fields share a common header structure (offsets relative to data field base):

| Offset | Symbol    | Type    | Description                                    |
|--------|-----------|---------|------------------------------------------------|
| -6     | TMSUB (=177772)     | INTEGER | Time-out subroutine address                    |
| -5     | TMR (=177773)       | INTEGER | Time-out counter                               |
| -4     | TTMR (=177774)      | INTEGER | Start value for TMR (=177773)                            |
| -3     | HDEV (=177775)      | INTEGER | Hardware device number (IOX instruction)       |
| -2     | STDRIV    | INTEGER | Start address of driver                        |
| -1     | DRIVER    | INTEGER | Restart address after interrupt                |
| 0      | RESLINK   | INTEGER | Reservation link                               |
| 1      | RTRES (=000001)     | INTEGER | Reserving RT (=036620) program                           |
| 2      | BWLINK    | INTEGER | Beginning of waiting queue                     |
| 3      | TYPRING   | INTEGER | Device type bits and ring                      |
| 4      | ISTATE    | INTEGER | Device state (0=idle, 1=busy, -1=no wait mode) |
| 5      | MLINK (=000005)     | INTEGER | Monitor queue link                             |
| 6      | MFUNC (=000006)     | INTEGER | Monitor level function address                 |

### Device-Specific Extensions

Each device type extends this base structure with device-specific fields.

---

## Device Driver Mapping

### 1. Mass Storage Devices

#### Disk Devices (DRFIE, BIGDI (=031550))

**Documentation Reference:** Section 28.4, Page 11  
**Source Files:** `IP-P2-DISK-START.NPL`, `MP-P2-DISK-START.NPL`

**Data Field Structure:**

| Field Name  | Offset | Description                              | Source Reference     |
|-------------|--------|------------------------------------------|----------------------|
| HDEV (=177775)        | -3     | Hardware device number (e.g., 1540#)     | Doc p.20, line 737   |
| DRIVER      | -1     | Restart after interrupt                  | Doc p.20, line 739   |
| STDRIV      | -2     | Start address (CTRDI (=054305))                    | Doc p.20, line 738   |
| BLSZ (=177765)        | 13     | Block size (1000 octal)                  | Doc p.20, line 729   |
| MONTYP      | 20     | Disk type/unit bits                      | Doc p.21, line 769   |

**Library Marks (0.SIN-GEN.NPL):**
```npl
"8ZSCS
175777/8SCSI; 7D1U0      % SWAPPING ON SCSI DISK (=127170)
```

#### SCSI Disk Devices (7D1U0-7D8U0)

**Documentation Reference:** Section 28.4  
**Source File:** `IP-P2-SCSI-DRIV.NPL`

**Driver Entry Points:**
- `SCLLD (=067160)` - Load/Start SCSI operation
- `SCINT (=067247)` - Interrupt handler
- `SCTIO (=067471)` - Timeout routine

**Configuration (0.SIN-GEN.NPL, lines 6-40):**
```npl
"7D1U0+7D1U1+7D1U2+7D1U3
% SCSI DISK (=127170) 1 PREREQUISITES
175777/8SCSI
```

**SCSI Driver Symbols (IP-P2-SCSI-DRIV.NPL, lines 17-75):**

| Symbol  | Value | Purpose                                |
|---------|-------|----------------------------------------|
| 7SIDE   | 5     | Initiator detected error               |
| 7SMRJ   | 7     | Message reject                         |
| 7SMPE   | 11    | Message parity error                   |
| RLMAR   | 00    | Read memory address register           |
| WLMAR   | 01    | Write memory address register          |
| RSTAU   | 04    | Read status                            |
| WCONT   | 05    | Write control                          |

---

### 2. Terminal Devices (DTxxR/DTxxW)

**Documentation Reference:** Section 28.5, Page 12  
**Source File:** `MP-P2-TERM-DRIV.NPL`

**Data Field Structure:**

| Field Name | Offset | Value     | Description                        | Doc Reference |
|------------|--------|-----------|------------------------------------|---------------|
| TYPRING    | 3      | 110002    | Device type bits and ring          | p.22, line 806|
| ISTATE     | 4      | 0         | 0=idle, 1=busy, -1=no wait mode    | p.22, line 807|
| MFUNC (=000006)      | 6      | IORES (=033477)     | Monitor level function address     | p.22, line 809|
| IOTRANS    | 7      | 0         | Called from INBT/OUTBT to transfer | p.22, line 810|
| STDEV (=000010)      | 10     | TEXIT (=003550)     | Start device                       | p.22, line 811|
| SETDV (=000011)      | 11     | CEXIT (=003547)     | IOSET (=044021) routine                      | p.22, line 812|
| DFOPP (=000012)      | 12     | DTxxW     | Opposite data field (for 2-way)    | p.22, line 813|

**Driver Entry Points (MP-P2-TERM-DRIV.NPL):**

| Routine  | Line  | Purpose                               |
|----------|-------|---------------------------------------|
| STTIN (=055006)    | 22    | Terminal input driver (Level 12)      |
| TYENT (=055011)    | 24    | Interrupt entry point                 |
| DWRITE   | 299   | Terminal output driver (Level 10)     |
| TWRITE   |       | Terminal write routine                |

**Terminal Driver Features:**
- Character echo control (DFLAG (=177766) bit 5ECHO (=000000))
- XON/XOFF flow control
- Break character handling
- Escape character processing
- Ring buffer management

**Ring Buffer Structure:**
- BUFST: Start of ring buffer
- MAX: Buffer capacity
- BHOLD: Number of characters in buffer
- HENTE: Fetch pointer
- FYLLE: Store pointer
- CFREE: Free positions

---

### 3. HDLC (=103112) Communication Driver

**Documentation Reference:** Section 28.16, 28.17  
**Source File:** `MP-P2-HDLC-DRIV.NPL`

**Configuration Table (PH-P2-CONFG-TAB.NPL, lines 26-96):**

Each HDLC (=103112) interface has a 12-word configuration entry:

| Offset | Field Name      | Description                            |
|--------|-----------------|----------------------------------------|
| 0      | HDLCSELECTION   | 0=unused, 1=HDLC mode, 2=Sync modem   |
| 1      | LOGHDLC         | Logical device number                  |
| 2      | NHDLC (=000002)           | HDLC (=103112) datafield number                  |
| 3      | NOHDLC          | Output datafield                       |
| 4      | SYNHDLC         | Sync modem input datafield             |
| 5      | SYNOHDLC        | Sync modem output datafield            |
| 6      | IDHDLC          | Ident code                             |
| 7      | INTHDLC         | DF address for ident table             |
| 8      | CDF1CLTIMER     | Timer table entry 1                    |
| 9      | CDF2CLTIMER     | Timer table entry 2                    |

**Example Configuration (PH-P2-CONFG-TAB.NPL, line 57):**
```npl
"8C1HD+8HM01
11HDX 1360,1,150          % HDLC/SYNCH 1.
% Results in:
% 1; 1360; HDIF1 (=075450);HDOF1;IDHM1;UDHM1;150; 0;0;0
```

**HDLC Driver Entry Points (MP-P2-HDLC-DRIV.NPL):**

| Routine   | Line | Purpose                                |
|-----------|------|----------------------------------------|
| HDSTA (=103636)     | 40   | Main driver entry, wait for message    |
| XSSDATA   | 169  | Transmit data to remote machine        |
| HDREC (=104371)     | 285  | Receive data from remote machine       |
| HOINT (=104233)     | 215  | Output interrupt handler               |
| HIINT (=104636)     | 341  | Input interrupt handler                |
| SMCLEAR   | 439  | Total clear of interface               |
| HDSIN (=105227)     | 487  | Initiate interface and set mode        |

**HDLC Message Structure:**

| Field    | Offset      | Description                     |
|----------|-------------|---------------------------------|
| XCFUNC   | BHEAD+0     | Function code                   |
| CRET (=000001)     | BHEAD+1     | Return status                   |
| ADSTA (=000002)    | BHEAD+2     | Hardware status                 |
| BBYTC (=000002)    | BHEAD+3     | Byte count                      |
| BMBYT (=000003)    | BHEAD+4     | Maximum byte count              |
| Data     | BHEAD+6+... | Transmitted/received data       |

**HDLC Modes (HDSIN (=105227) initialization):**

| Mode Value | Name      | Description               |
|------------|-----------|---------------------------|
| 0          | FULL      | Full duplex               |
| 1          | HALF (=000001)      | Half duplex               |
| 2          | MAMOD (=000002)     | Maintenance mode          |

---

### 4. X.21 Network Interface Driver

**Documentation Reference:** Section 28.16  
**Source File:** `MP-P2-X21-DRIV.NPL`

**X.21 Driver States:**

| State | Name                    | Description                      | Source Line |
|-------|-------------------------|----------------------------------|-------------|
| 1     | Ready                   | T=1, C=OFF                       | 766         |
| 2     | DTE Call Request        | T=0, C=ON                        | 909         |
| 3     | Proceed to Select       | Waiting for "+"                  | 915         |
| 4     | Send Selector Signals   | T=IA5, C=ON                      | 922         |
| 5     | DTE Waiting             | T=1, C=ON                        | 978         |
| 7     | Call Progress Signals   | Receiving CP signals             | 1000        |
| 8     | Incoming Call           | Remote connection request        | 1158        |
| 10    | DTE Provided Info       | Receiving line ID/info           | 1035        |
| 12    | Call Progress           | Processing call progress         | 1035        |
| 14    | Ready for Data          | Data transfer phase              | 1066        |
| 16-21 | Clearing States         | Connection termination           | 861         |

**X.21 Commands (X21C (=107457) command decoder, line 528):**

| Function | Code   | Routine | Description                  |
|----------|--------|---------|------------------------------|
| 1        | XFCALL | X21CA   | Initiate call                |
| 2        | XFREAD | X21RE (=107060)   | Ready for data               |
| 3        | XFCLEA | X21CL (=107131)   | Clear request                |
| 4        | XFCHAR | X21CH (=107376)   | Get charging information     |
| 5        | XFCONN | X21CN (=107207)   | Connect to X.21              |
| 6        | XFDISC | X21DC (=107346)   | Disconnect from X.21         |
| 7        | XFREDI | X21RD (=107344)   | Redirect (not implemented)   |
| 8        | XFSTAT | X21AA (=106644)   | Status query                 |
| 9        | XFBREA | X21BR (=107445)   | Return when broken           |

**X.21 Data Field Integration:**

The X.21 driver acts as an intermediary between user datafields (X2DUI (=000071), X2DUO (=000072)) and HDLC (=103112) datafields:

```npl
X2DUI.HXDOK=:X2DUO.HXDOK  % Lock/unlock user drivers
```

**Interrupt Routing (lines 476-498):**
- Level 12 interrupts routed via `X212S (=106301)`
- Level 13 interrupts routed via `X213S (=106312)`
- Updates ident tables to redirect interrupts to X.21 driver

---

### 5. PIOC Driver (Processor I/O Controller)

**Documentation Reference:** Not in Appendix A (internal driver)  
**Source File:** `MP-P2-PIOC-DRIV.NPL`

**PIOC-ND100 Communication Structure:**

**XMSG Box Layout (lines 11-24):**

| Offset | Field   | Type    | Description                          |
|--------|---------|---------|--------------------------------------|
| 0      | NXMSG   | DOUBLE  | XMSG activation queue                |
| 2      | NXFNC   | INTEGER | Status flags (bit 1=done, bit 3=req) |
| 3      | NXPAR   | DOUBLE  | Parameter pointer                    |
| 5      | NXXTB   | INTEGER | XT (=170344) block (from ND-100)               |
| 6      | NXLB    | INTEGER | Last local bank for this task        |
| 7      | NXPNU   | INTEGER | Process number                       |

**RT Activation Queue (lines 20-24):**

| Offset | Field   | Type    | Description                      |
|--------|---------|---------|----------------------------------|
| 0      | NXRTW   | DOUBLE  | RT (=036620) activation queue              |
| 2      | NXRTF   | INTEGER | Status (bit 0=req, bit 2=done)   |

**Superkick Mechanism (lines 36-142):**

The PIOC driver uses a "superkick" ring buffer mechanism for fast message passing:

```npl
SUKOF=1012    % Fixed address to port system
HSKPA=52525   % Handshake pattern (high)
LSKPA=125252  % Handshake pattern (low)
MSK=7         % Ring buffer index mask (modulo 8)
```

**Ring Structure:**
- PATRN: Double pattern for validation
- RNTOP: ND100-to-PIOC ring pointer
- RPTON: PIOC-to-ND100 ring pointer
- PODIR: Port directory pointer

**Driver Entry Points:**

| Routine   | Line | Purpose                                   |
|-----------|------|-------------------------------------------|
| PISUPER   | 32   | Superkick activation handler              |
| PIWKF     | 233  | Wake up routine (from XMSG)               |
| PIINC     | 260  | Load XMSG context from PIOC               |
| PILOC     | 261  | Load context after interrupt              |
| PISAC     | 285  | Save XMSG context to PIOC                 |
| PICXM     | 301  | Call XMSG from PIOC                       |
| XMMC      | 357  | Handle multicall (6-word to 4-word)       |
| DOIT      | 426  | Main XMSG execution point                 |
| PDRIV (=101166)     | 530  | Main driver entry                         |

---

### 6. Cassette and Magnetic Tape Devices

**Documentation Reference:** Section 28.4, Pages 10-11  
**Source Files:** Various MT/CA drivers

**Cassette Data Field (CADIX) - Page 25:**

| Offset | Field    | Value   | Description                        |
|--------|----------|---------|------------------------------------|
| 3      | TYPING   | 113000  | Device type bits                   |
| 7      | IOTRANS  | CBGET (=074104)   | Transfer routine                   |
| 11     | SETDV (=000011)    | CAICL (=074270)   | IOSET (=044021) routine                      |
| 12     | DFOPP (=000012)    | CADOX   | Opposite datafield (output)        |
| 15     | MAX (=000015)      | 1000    | Buffer capacity                    |
| 22     | CLOGDV   | 575     | Logical unit number for DMA        |
| 25     | CASUN (=000025)    | x       | Device unit number                 |

**Magnetic Tape Controllers:**

Configuration in `0.SIN-GEN.NPL`:

```npl
% CONVERSION FROM 'OLD' TO (=172476) 'NEW' FOR CONTROLLER 1
"99TM1+99HM1+99SM1; 175777/8MT1;"

% CONVERSION FROM 'OLD' TO (=172476) 'NEW' FOR CONTROLLER 2
"99TM2+99HM2+99SM2; 175777/8MT2;"
```

---

## Initialization Process

### System Startup Sequence

**Source File:** `PH-P2-START-BASE.NPL`, `PH-P2-OPPSTART.NPL`

#### 1. Memory Layout Initialization (PH-P2-START-BASE.NPL, lines 12-134)

The system sets up physical memory regions for various components:

| Region          | First Page Variable | Last Page Variable | Purpose                    |
|-----------------|---------------------|-------------------|----------------------------|
| Memory Map      | MMFPAGE             | MMLPAGE           | Memory management          |
| Device Buffers  | DBFPAGE             | DBLPAGE           | I/O buffers                |
| LAMU Tables     | FLAMPAGE            | LLAMPAGE          | Memory allocation          |
| Segment Table   | SGTFPHPAGE          | SGTLPHPAGE        | Segment management         |
| HDLC (=103112) Buffers    | HDLCFPHPAGE         | HDLCLPHPAHE       | HDLC (=103112) communication         |
| SCSI Buffers    | SCFPHYSPAGE         | SCLPHYSPAGE       | SCSI device buffers        |
| Terminal DFs    | TDFPAGE             | TDLPHPAGE         | Terminal datafields        |
| PIOC Interface  | Various             | Various           | PIOC communication         |

#### 2. Configuration Table Processing (PH-P2-CONFG-TAB.NPL)

**HDLC Device Configuration (lines 27-96):**

The system scans the HDLC (=103112) table (`0HDTA (=036177)` to `0HDEN (=036367)`) and:
1. Checks `HDLCSELECTION` for each entry
2. If selection=1: Configures as HDLC (=103112) device
3. If selection=2: Configures as synchronous modem
4. Sets up logical device numbers
5. Installs datafield addresses in ident tables

**Line Printer Configuration (lines 128-149):**

Each printer entry contains:
- `LPSELECTION`: Printer type (0=unused, 1=DMPR, 2=DMLP, 3=DLPR)
- `LPLOGNO`: Main logical device number
- `LPDMLOGNO`: DMA datafield logical number
- `LPIODF`: Main datafield address array
- `LPDMDF`: DMA datafield address array
- `LPIDENT`: Ident code array

#### 3. Device Driver Installation

The system uses **library marks** (defined in `0.SIN-GEN.NPL`) to conditionally include devices:

**Example: SCSI Disk Setup (lines 6-15)**

```npl
"8SCSI 7D1U0 -8ZSCS
175777/8ZSCS

"7D1U0+7D1U1+7D1U2+7D1U3
% SCSI DISK (=127170) 1 PREREQUISITES
175777/8SCSI
```

Translation:
- If library mark `8SCSI` and `7D1U0` are set, but NOT `8ZSCS`
- Then include SCSI controller support
- For units 0-3 on SCSI disk 1
- Require library mark `8SCSI` to be set

#### 4. Datafield Initialization Sequence

**For each device type:**

1. **Allocate physical memory pages**
   - Calculate required size from device count
   - Allocate from available physical memory
   - Update first/last page variables

2. **Initialize datafield structure**
   - Set HDEV (=177775) (hardware device number)
   - Set STDRIV (driver start address)
   - Set DRIVER (interrupt restart address)
   - Set TYPRING (device type and ring number)
   - Initialize state (ISTATE = 0)

3. **Install in system tables**
   - Add to ident code table (for interrupt routing)
   - Add to logical device table
   - Set up reservation links
   - Initialize wait queues

4. **Configure device-specific features**
   - Buffer sizes and addresses
   - DMA list pointers (for DMA devices)
   - Protocol parameters (HDLC (=103112), X.21)
   - Timeout values

---

## Driver Categories

### Category 1: Character Device Drivers

**Characteristics:**
- Use ring buffers for character storage
- Support INBT/OUTBT monitor calls
- Implement echo and break strategies
- Handle flow control (XON/XOFF)

**Examples:**
- **Terminals** (DTxxR/DTxxW)
  - Input: `STTIN (=055006)` interrupt at level 12
  - Output: `DWRITE` interrupt at level 10
  - Ring buffer management
  - Echo control via DFLAG (=177766)

- **Paper Tape** (DREAR, DPNCH (=063537))
- **Card Reader** (IDV4)

### Category 2: Block Device Drivers

**Characteristics:**
- Transfer data in blocks
- Use DMA for efficient transfer
- Implement retry and error recovery
- Maintain transfer counters

**Examples:**
- **Disk Devices** (DRFIE, BIGDI (=031550))
  - Block size: 1024 words typical
  - Retry counters (TACNS (=177762), TACOUNT)
  - Error accumulation (AERRB (=177761), SERRB (=177757))

- **SCSI Devices** (7DxUy)
  - Arbitration phase
  - Message protocol
  - Disconnect/reconnect
  - Tagged command queuing

### Category 3: DMA Device Drivers

**Characteristics:**
- Use direct memory access
- Require DMA list structures
- Handle scatter-gather I/O
- Minimize CPU involvement

**Examples:**
- **HDLC** (HDIFx/HDOFx)
  - DMA list with LMEM1/LMEM2 addressing
  - Interrupt on frame complete
  - Hardware CRC generation

- **Magnetic Tape** (MTFIE, M2FIE)
- **Versatec** (VEFIE (=050172))

### Category 4: Communication Protocol Drivers

**Characteristics:**
- Implement layered protocols
- State machine driven
- Message-based interface
- Support connection management

**Examples:**
- **X.21** (X2S01/X2S02)
  - 22+ distinct states
  - Call setup/teardown
  - Character-based signaling
  - Integration with HDLC (=103112)

- **HASP** (HDMxx)
  - BSC protocol
  - Block transfer
  - Multi-device support

---

## Reference Tables

### HDEV (=177775) (Hardware Device Numbers)

Common IOX device numbers used in datafields:

| Device Type     | HDEV (=177775) (Octal) | HDEV (=177775) Symbol | Source Reference         |
|-----------------|--------------|-------------|--------------------------|
| Disk            | 1540         | IOX 1540    | Doc p.20 line 737        |
| Terminal        | Variable     | -           | Based on ident code      |
| HDLC (=103112)            | Variable     | -           | Configured in table      |
| Cassette        | 700          | IOX 520     | Doc p.27 line 1053       |
| Card Reader     | Variable     | -           | -                        |
| Line Printer    | Variable     | -           | Configured in table      |
| SCSI Controller | Variable     | -           | Multiple per controller  |
| PIOC            | Variable     | -           | Via PIOCA (=000013) bank           |

### TYPRING Bit Definitions

The TYPRING field encodes device type and ring number:

| Bits     | Field | Description                              |
|----------|-------|------------------------------------------|
| 15-12    | Type  | Device type code                         |
| 11-0     | Ring  | Ring number (0=unrestricted, 1=system)   |

**Common TYPRING Values:**

| Value (Octal) | Device Type          | Ring |
|---------------|----------------------|------|
| 110000        | Character device     | 0    |
| 110002        | Character device     | 2    |
| 113000        | Block device         | 0    |
| 1002          | Mass storage         | 2    |

### DFLAG (=177766) Bit Definitions (Terminal Devices)

From documentation Section 4:

| Bit | Symbol    | Description                          |
|-----|-----------|--------------------------------------|
| 0   | 5ECHO (=000000)     | Echo characters                      |
| 1   | 5SPEC (=000002)     | Special character processing         |
| 2   | 5CTRLO    | Control-O in effect                  |
| 3   | 5CAPITAL  | Convert lowercase to uppercase       |
| 4   | 5XDEVICE  | XON/XOFF device                      |
| 5   | 5XOFF (=000010)     | XOFF (=000023) received (stop output)          |
| 6   | 5OXON (=000012)     | Output stopped by XOFF (=000023)               |

### Library Mark Prefixes

Library marks in `0.SIN-GEN.NPL` use prefixes to categorize devices:

| Prefix | Category               | Examples                    |
|--------|------------------------|-----------------------------|
| 8      | Device/feature flags   | 8SCSI, 8N500, 8LAMU         |
| 7      | Unit devices           | 7D1U0 (SCSI disk 1 unit 0)  |
| 5      | System components      | 5FYFS (=000144), 5FYSP (=000534)                |
| 9      | Secondary devices      | 9M1U0 (mag tape)            |

### Common Driver Entry Point Names

| Entry Point | Purpose                              |
|-------------|--------------------------------------|
| STDRIV      | Start driver (normal entry)          |
| DRIVER      | Restart after interrupt              |
| xxxINT      | Interrupt handler                    |
| xxxTIO      | Timeout handler                      |
| xxxCL       | Close/cleanup routine                |
| xxxST       | Status/state routine                 |

---

## Detailed Example: Terminal Driver Setup

### Configuration in 0.SIN-GEN.NPL

Terminal devices are pre-configured based on ident codes:

```npl
LIDTE=140040-1               % IDENT CODE OF (=173771) TERMINAL 65 (-1)
HIDTE=140437-1               % IDENT CODE OF (=173771) TERMINAL 256 (-1)
```

This allows for terminals numbered 1-256 with ident codes from 140040 to 140437.

### Terminal Data Field Pair

Each terminal has TWO datafields:

1. **Input Datafield (DTxxR)** - For reading from terminal
   - Ident code: 140040 + (2 * terminal_number) - 1
   - Driver: `STTIN (=055006)` (Level 12)
   - DFOPP (=000012) points to output datafield

2. **Output Datafield (DTxxW)** - For writing to terminal
   - Ident code: 140040 + (2 * terminal_number)
   - Driver: `DWRITE` (Level 10)
   - DFOPP (=000012) points to input datafield

### Terminal Driver Flow

**Input Path:**
```
Hardware Interrupt (Level 12)
    ↓
TYENT (=055011) (entry point)
    ↓
TIAPD (=061177) (read character from hardware)
    ↓
Character processing (echo, break, control chars)
    ↓
CXRBPUT (put in ring buffer)
    ↓
TSTBACK (restart waiting program if buffer full/break)
```

**Output Path:**
```
DWRITE (programmed restart or interrupt Level 10)
    ↓
Check buffer for characters
    ↓
PRCHA (=057260) (output character to hardware)
    ↓
Update pointers and counters
    ↓
TID10 (wait for next interrupt)
```

---

## Detailed Example: HDLC (=103112) Driver Setup

### Configuration Table Entry

From `PH-P2-CONFG-TAB.NPL`:

```npl
"8C1HD+8HM01
11HDX 1360,1,150          % HDLC/SYNCH 1.
```

This expands to a table entry:
```
Selection: 1              (Use as HDLC (=103112))
LogNo:     1360           (Logical device number)
InputDF:   HDIF1 (=075450)          (Input datafield)
OutputDF:  HDOF1 (=075306)          (Output datafield)
SyncInDF:  IDHM1 (=104304)          (Sync modem input, if selected)
SyncOutDF: UDHM1 (=104347)          (Sync modem output, if selected)
IdentCode: 150            (Interrupt ident code)
```

### HDLC (=103112) Message Flow

**Transmit:**
```
User calls XMSG with XSSDATA function
    ↓
Message placed in IQUEU (=000061)
    ↓
HDSTA (=103636) gets message from queue
    ↓
XSSDATA validates and sets up DMA list
    ↓
XHMST starts hardware transmission
    ↓
HOINT (=104233) (interrupt when complete)
    ↓
Message returned to user with status
```

**Receive:**
```
User calls XMSG with HDREC (=104371) function
    ↓
Empty message placed in receiver list
    ↓
HDENA starts receiver if not active
    ↓
HIINT (=104636) (interrupt when frame received)
    ↓
Frame copied to message
    ↓
Message returned to user with data
```

### HDLC (=103112) DMA List Structure

Each DMA list entry (4 words):
```
Word 0-1: LMEM (=000002) (Physical memory address, split high/low)
Word 2:   LKEY (=000000) (Control flags and block status)
Word 3:   LBYTC (=000001) (Byte count for this block)
```

LKEY (=000000) bit definitions:
- Bit 15 (XBLDN (=000010)): Block done
- Bit 14-13: Reserved
- Bit 12-0: Control flags

---

## Appendix A: Complete File Inventory

### Source Files by Category

**Phase 1 - Configuration and Generation:**
- `0.SIN-GEN.NPL` - Library marks and prerequisites
- `DP-P2-VARIABLES.NPL` - Global variable declarations
- `PH-P2-CONFG-TAB.NPL` - Device configuration tables
- `PH-P2-START-BASE.NPL` - Startup and memory layout
- `PH-P2-OPPSTART.NPL` - System initialization
- `PH-P2-RESTART.NPL` - System restart handling
- `PH-P2-MEMTOF.NPL` - Memory table of contents

**Phase 2 - Monitor and Runtime (RP-P2-*):**
- `RP-P2-1.NPL` - Main monitor code
- `RP-P2-MONCALLS.NPL` - Monitor call interface
- `RP-P2-MSYSU.NPL` - System utilities
- `RP-P2-ACCRT.NPL` - RT (=036620) accounting
- `RP-P2-CONFG.NPL` - Configuration routines
- `RP-P2-SEGADM.NPL` - Segment administration
- `RP-P2-PIOC.NPL` - PIOC support
- `RP-P2-TAD.NPL` - TAD management
- `RP-P2-MON-ADP.NPL` - Monitor adaptation
- `RP-P2-N500.NPL` - ND-500 specific code

**Phase 3 - Device Drivers (MP-P2-*, IP-P2-*):**
- `MP-P2-1.NPL` - Main device driver code
- `MP-P2-2.NPL` - Additional driver code
- `MP-P2-TERM-DRIV.NPL` - Terminal driver
- `MP-P2-HDLC-DRIV.NPL` - HDLC (=103112) communication driver
- `MP-P2-X21-DRIV.NPL` - X.21 network driver
- `MP-P2-PIOC-DRIV.NPL` - PIOC processor driver
- `MP-P2-HASP-ETC.NPL` - HASP protocol driver
- `MP-P2-DIMIR.NPL` - DIMIR (=150067) support
- `MP-P2-DISK-START.NPL` - Disk initialization
- `MP-P2-TAD.NPL` - TAD driver
- `MP-P2-PERF-CODE.NPL` - Performance monitoring
- `MP-P2-PERF-DATA.NPL` - Performance data
- `MP-P2-PERF-SAMP.NPL` - Performance sampling
- `MP-P2-N500.NPL` - ND-500 device support
- `IP-P2-1.NPL` - Interrupt processing
- `IP-P2-SCSI-DRIV.NPL` - SCSI driver
- `IP-P2-SCSI-DISK.NPL` - SCSI disk support
- `IP-P2-SCSI-MAGTP.NPL` - SCSI magnetic tape
- `IP-P2-SCSI-OPDI.NPL` - SCSI optical disk
- `IP-P2-DISK-START.NPL` - Disk startup
- `IP-P2-DISK-LOGG.NPL` - Disk logging
- `IP-P2-SEGADM.NPL` - Segment administration

**Phase 4 - System Calls and Control (XC-P2-*):**
- `XC-P2-1.NPL` - System call interface
- `XC-P2-2.NPL` - Additional system calls
- `XC-P2-N500.NPL` - ND-500 system calls

**Phase 5 - Common Code (CC-P2-*):**
- `CC-P2-COMMON.NPL` - Common routines
- `CC-P2-N500.NPL` - ND-500 common code

**Phase 6 - Monitor Level 60 (5P-P2-*):**
- `5P-P2-MON60.NPL` - Monitor level 60 code

---

## Appendix B: Key Concepts

### Library Marks

Library marks are conditional compilation directives that control which code is included in the system. They use a quote syntax:

```npl
"8SCSI          % If library mark 8SCSI is defined
code...         % This code is included
"               % End conditional

"8SCSI-8ZSCS    % If 8SCSI defined AND 8ZSCS NOT defined
code...
"
```

### Data Field Addressing

Datafields use relative addressing with negative offsets for driver-specific data:

```
-6  -5  -4  -3  -2  -1   0   1   2   3   4 ...
[TMSUB (=177772)][TMR (=177773)][TTMR (=177774)][HDEV (=177775)][STDRIV][DRIVER][RESLINK][RTRES (=000001)][BWLINK][TYPRING][ISTATE]...
```

Access in NPL:
```npl
X.HDEV          % Access HDEV (=177775) field of datafield pointed to by X
X-BHEAD         % Point to start of buffer header
```

### Interrupt Levels

SINTRAN uses priority-based interrupt levels:

| Level | Priority | Typical Use                    |
|-------|----------|--------------------------------|
| 15    | Highest  | Critical hardware errors       |
| 14    | High     | Disk controllers               |
| 13    | High     | Fast communication devices     |
| 12    | Medium   | Terminal input, slow comm      |
| 11    | Medium   | Time-critical operations       |
| 10    | Low      | Terminal output, slow devices  |
| 0-9   | Lowest   | Background processing          |

### Page and Bank Organization

SINTRAN uses a paged memory architecture:

- **Physical Pages:** Actual memory pages (typically 1K words)
- **Logical Pages:** Virtual memory pages
- **Banks:** Groups of pages (Bank 0, 1, 2 for different address spaces)

Memory access:
```npl
*1BANK          % Switch to bank 1
LDA ,X          % Load from bank 1
*2BANK          % Switch to bank 2
STA ,X          % Store to bank 2
```

---

## Appendix C: Cross-Reference

### Data Field Name to Source File

| Data Field | Label  | Source File(s)              | Line References |
|------------|--------|-----------------------------|-----------------|
| Disk       | DRFIE  | IP-P2-DISK-START.NPL        | Various         |
| Terminal R | DTxxR  | MP-P2-TERM-DRIV.NPL         | 1-300           |
| Terminal W | DTxxW  | MP-P2-TERM-DRIV.NPL         | 240-600         |
| HDLC (=103112) In    | HDIFx  | MP-P2-HDLC-DRIV.NPL         | Throughout      |
| HDLC (=103112) Out   | HDOFx  | MP-P2-HDLC-DRIV.NPL         | Throughout      |
| X.21       | X2Sxx  | MP-P2-X21-DRIV.NPL          | Throughout      |
| SCSI       | Various| IP-P2-SCSI-DRIV.NPL         | Throughout      |
| Cassette   | CADxx  | (Not in provided files)     | -               |
| Mag Tape   | MTFxx  | (Not in provided files)     | -               |

### Library Mark to Data Field

| Library Mark | Data Fields Affected           | Purpose                |
|--------------|--------------------------------|------------------------|
| 8SCSI        | 7DxUy, SCSI controller DFs     | SCSI devices           |
| 8C1HD        | HDIF1 (=075450), HDOF1 (=075306)                   | HDLC (=103112) controller 1      |
| 8C1X2        | X2S01 (=111451)                          | X.21 connection 1      |
| 8N500        | Various ND-500 specific        | ND-500 CPU support     |
| 8LAMU        | LAMU tables                    | Memory allocation      |
| 8LOG         | Logging buffers                | Program logging        |

---

## Document History

| Version | Date           | Changes                                      |
|---------|----------------|----------------------------------------------|
| 1.0     | Oct 16, 2025   | Initial documentation based on source analysis |

## References

1. **ND-60112-01-EN SINTRAN III System Documentation - Appendix A - Data Fields**
   - Published: June 1979
   - Location: `D:\OCR\ai\ND-60112-01-EN SINTRAN III System Documentation - Appendix A - Data Fields\`

2. **SINTRAN L NPL Source Code**
   - Location: `Z:\NorskData\Source Code\Sintran L\NPL\`
   - Files: See Appendix A

3. **Key Files for Device Driver Setup:**
   - `0.SIN-GEN.NPL` - System generation and configuration
   - `PH-P2-CONFG-TAB.NPL` - Configuration tables
   - `PH-P2-START-BASE.NPL` - Memory and initialization

---

## Appendix D: Complete System Tables Reference

This section documents ALL tables found in the SINTRAN L source code with their complete data, field descriptions, variants, and default values.

**Symbol Value Notation:** Symbols throughout this appendix are annotated with their actual octal values in parentheses, e.g., `MTNSP (=000010) (000010)` indicates the symbol MTNSP (=000010) has the octal value 000010. These values are derived from the SYMBOLS-L directory symbol files (SYMBOL-1-LIST.SYMB.TXT, SYMBOL-2-LIST.SYMB.TXT, and N500-SYMBOLS.SYMB.TXT). Note that symbol names in the files are truncated to 5 characters maximum, so some longer symbol names from the NPL source may not have corresponding values listed if they exceed this length.

### Table Organization

Tables are organized by source file and functional category:
1. **Configuration Tables** (PH-P2-CONFG-TAB.NPL)
2. **Variable Tables** (DP-P2-VARIABLES.NPL)
3. **Generation Parameters** (0.SIN-GEN.NPL)
4. **Memory Layout Tables** (PH-P2-START-BASE.NPL)
5. **Runtime Tables** (RP-P2-1.NPL)

---

### 1. HDLC (=103112) Device Configuration Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 9-96)  
**Table Name:** `0HDTA (=036177)` (HDLC (=103112) Device Table Array)  
**Element Size:** 12 words (TBLHDLCSIZE)  
**Table Range:** `0HDTA (=036177)` to `0HDEN (=036367)`  
**Element Count:** `0NHDL (=000014)` (Number of generated HDLC (=103112) devices)

#### Table Structure Definition (DISP 0 ... PSID, Lines 9-21)

| Offset | Field Name      | Type    | Description                                    |
|--------|-----------------|---------|------------------------------------------------|
| 0      | HDLCSELECTION   | INTEGER | Device mode selection                          |
| 1      | LOGHDLC         | INTEGER | Logical device number                          |
| 2      | NHDLC (=000002)           | INTEGER | HDLC (=103112) number/index                              |
| 3      | NOHDLC          | INTEGER | Output datafield address                       |
| 4      | SYNHDLC         | INTEGER | Sync modem input datafield                     |
| 5      | SYNOHDLC        | INTEGER | Sync modem output datafield (part of DOUBLE)   |
| 6      | IDHDLC          | INTEGER | Ident code for interrupts                      |
| 7      | INTHDLC         | INTEGER | DF address to insert in ident table            |
| 8      | CDF1CLTIMER     | INTEGER | DF address to remove from timer (part of DBL)  |
| 9      | CDF2CLTIMER     | INTEGER | DF address to remove from timer                |
| 10-11  | (Reserved)      | INTEGER | Padding/future use                             |

#### HDLCSELECTION Values

| Value | Mode            | Description                                                    |
|-------|-----------------|----------------------------------------------------------------|
| 0     | Unused          | This HDLC (=103112) interface is not configured                          |
| 1     | HDLC (=103112) Mode       | Use as HDLC (=103112) device (LOGNO.IN=HDLC-IN, LOGNO+1.OUT=HDLC-OUT)   |
| 2     | Sync Modem Mode | Use as synchronous modem (both IN/OUT on LOGNO)                |

#### Table Entries (Lines 56-91)

**Entry 1: HDLC/SYNCH Controller 1**
```
Library Mark:  "8C1HD+8HM01
Macro Call:    11HDX 1360,1,150
Expanded Data: 1; 1360; HDIF1 (=075450);HDOF1;IDHM1;UDHM1;150; 0;0;0
```

| Field           | Value          | Octal  | Description                                |
|-----------------|----------------|--------|--------------------------------------------|
| HDLCSELECTION   | 1              | 1      | Configured for HDLC (=103112) mode                   |
| LOGHDLC         | 1360           | 1360   | Logical device number (decimal 752)        |
| NHDLC (=000002)           | HDIF1 (=075450) (075450) | -      | Input datafield address                    |
| NOHDLC          | HDOF1 (=075306) (075306) | -      | Output datafield address                   |
| SYNHDLC         | IDHM1 (=104304) (104304) | -      | Sync modem input (if mode=2)               |
| SYNOHDLC        | UDHM1 (=104347) (104347) | -      | Sync modem output (if mode=2)              |
| IDHDLC          | 150            | 150    | Ident code (decimal 104)                   |
| INTHDLC         | 0              | 0      | Not used in this configuration             |
| CDF1CLTIMER     | 0              | 0      | Not used                                   |
| CDF2CLTIMER     | 0              | 0      | Not used                                   |

**Entry 2: HDLC/SYNCH Controller 2**
```
Library Mark:  "8C2HD+8HM02
Macro Call:    11HDX 1362,2,151
Expanded Data: 1; 1362; HDIF2 (=076017);HDOF2;IDHM2;UDHM2;151; 0;0;0
```

| Field           | Value          | Description                                |
|-----------------|----------------|--------------------------------------------|
| HDLCSELECTION   | 1              | Configured for HDLC (=103112) mode                   |
| LOGHDLC         | 1362           | Logical device number (decimal 754)        |
| NHDLC (=000002)           | HDIF2 (=076017) (076017) | Input datafield address                    |
| NOHDLC          | HDOF2 (=075655) (075655) | Output datafield address                   |
| SYNHDLC         | IDHM2 (=104410) (104410) | Sync modem input (if mode=2)               |
| SYNOHDLC        | UDHM2 (=104453) (104453) | Sync modem output (if mode=2)              |
| IDHDLC          | 151            | Ident code (decimal 105)                   |

**Entries 3-6: Similar pattern for controllers 3-6**

| Controller | Library Mark | LogNo | Ident | Notes                        |
|------------|--------------|-------|-------|------------------------------|
| 3          | 8C3HD+8HM03  | 1364  | 152   | Datafields: HDIF3 (=076366), HDOF3 (=076224)     |
| 4          | 8C4HD+8HM04  | 1366  | 153   | Datafields: HDIF4 (=076735), HDOF4 (=076573)     |
| 5          | 8C5HD+8HM05  | 1370  | 154   | Datafields: HDIF5 (=077304), HDOF5 (=077142)     |
| 6          | 8C6HD+8HM06  | 1372  | 155   | Datafields: HDIF6 (=077653), HDOF6 (=077511)     |

**Table Terminator:** `-1` (Lines 94)

---

### 2. Line Printer Configuration Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 128-151)  
**Table Name:** `0LPTA (=036370)` (Line Printer Table Array)  
**Element Size:** 16 words (LPTBSIZE)  
**Element Count:** `0NLPS (=000002)` (Number of generated line printers)

#### Table Structure Definition (Lines 128-141)

| Offset | Field Name      | Type    | Size | Description                              |
|--------|-----------------|---------|------|------------------------------------------|
| 0      | LPSELECTION     | INTEGER | 1    | Printer driver type selection            |
| 1      | LPLOGNO         | INTEGER | 1    | Main logical device number               |
| 2      | LPDMLOGNO       | INTEGER | 1    | DMA datafield logical number             |
| 3      | LPIODF(0)       | INTEGER | 3    | Array: Main datafield addresses          |
| 3      | 1LPIODF         | INTEGER | 1    | - Type 1 datafield address               |
| 4      | 2LPIODF         | INTEGER | 1    | - Type 2 datafield address               |
| 5      | 3LPIODF         | INTEGER | 1    | - Type 3 datafield address               |
| 6      | LPDMDF(0)       | INTEGER | 3    | Array: DMA datafield addresses           |
| 6      | 1LPDMDF         | INTEGER | 1    | - Type 1 DMA datafield                   |
| 7      | 2LPDMDF         | INTEGER | 1    | - Type 2 DMA datafield                   |
| 8      | 3LPDMDF         | INTEGER | 1    | - Type 3 DMA datafield                   |
| 9      | LPIDENT(0)      | INTEGER | 3    | Array: Ident codes                       |
| 9      | 1LPIDENT        | INTEGER | 1    | - Type 1 ident code                      |
| 10     | 2LPIDENT        | INTEGER | 1    | - Type 2 ident code                      |
| 11     | 3LPIDENT        | INTEGER | 1    | - Type 3 ident code                      |
| 12     | LPCLENTRY       | INTEGER | 1    | Clear entry flag (≠0 = cannot be used)   |
| 13-15  | (Reserved)      | INTEGER | 3    | Padding/future use                       |

#### LPSELECTION Values

| Value | Driver Type | Description                                |
|-------|-------------|--------------------------------------------|
| 0     | Unused      | This line printer is not configured        |
| 1     | DMPR-TYPE   | Use DF-1 datafield (DMA printer type)      |
| 2     | DMLP-TYPE   | Use DF-2 datafield (DMA line printer)      |
| 3     | DLPR-TYPE   | Use DF-3 datafield (Character mode)        |

#### Table Entries

**Entry 1: Line Printer 1**
```
Library Marks: "8LP1+8DLP1+8NLP1+8DVE1
Data:          0, 5, 1167, IDMP1 (=107250), DILP1 (=107102), DLPR (=107670), DMPR1 (=107217), DMLP1 (=107006), DLPR (=107670), 140230, 3, 3, 0,0,0,0
```

| Field         | Value           | Octal  | Description                                     |
|---------------|-----------------|--------|-------------------------------------------------|
| LPSELECTION   | 0               | 0      | **Default: Not configured**                     |
| LPLOGNO       | 5               | 5      | Logical device number (decimal 5)               |
| LPDMLOGNO     | 1167            | 1167   | DMA datafield log no (decimal 631)              |
| 1LPIODF       | IDMP1 (=107250) (107250)  | -      | Type 1 (DMPR) main datafield                    |
| 2LPIODF       | DILP1 (=107102) (107102)  | -      | Type 2 (DMLP) main datafield                    |
| 3LPIODF       | DLPR (=107670)            | -      | Type 3 (DLPR (=107670)) main datafield                    |
| 1LPDMDF       | DMPR1 (=107217) (107217)  | -      | Type 1 DMA datafield                            |
| 2LPDMDF       | DMLP1 (=107006) (107006)  | -      | Type 2 DMA datafield                            |
| 3LPDMDF       | DLPR (=107670)            | -      | Type 3 DMA datafield (same as main)             |
| 1LPIDENT      | 140230          | 140230 | Type 1 ident code (decimal 49304)               |
| 2LPIDENT      | 3               | 3      | Type 2 ident code                               |
| 3LPIDENT      | 3               | 3      | Type 3 ident code                               |
| LPCLENTRY     | 0               | 0      | Can be used                                     |

**Entry 2: Line Printer 2**
```
Library Marks: "8LP2+8DLP2+8NLP2+8DVE2
Data:          0, 15, 1175, IDMP2 (=107616), DILP2 (=107450), DLPR2 (=107723), DMPR2 (=107565), DMLP2 (=107354), DLPR2 (=107723), 140231, 23, 23, 0,0,0,0
```

| Field         | Value           | Description                                     |
|---------------|-----------------|------------------------------------------------|
| LPSELECTION   | 0               | **Default: Not configured**                     |
| LPLOGNO       | 15              | Logical device number (decimal 13)              |
| LPDMLOGNO     | 1175            | DMA datafield log no (decimal 637)              |
| 1LPIODF       | IDMP2 (=107616) (107616)  | Type 1 main datafield                           |
| 2LPIODF       | DILP2 (=107450) (107450)  | Type 2 main datafield                           |
| 3LPIODF       | DLPR2 (=107723) (107723)  | Type 3 main datafield                           |
| 1LPDMDF       | DMPR2 (=107565) (107565)  | Type 1 DMA datafield                            |
| 2LPDMDF       | DMLP2 (=107354) (107354)  | Type 2 DMA datafield                            |
| 3LPDMDF       | DLPR2 (=107723) (107723)  | Type 3 DMA datafield                            |
| 1LPIDENT      | 140231          | Type 1 ident code (decimal 49305)               |
| 2LPIDENT      | 23              | Type 2 ident code (decimal 19)                  |
| 3LPIDENT      | 23              | Type 3 ident code (decimal 19)                  |

**Table Terminator:** `-1`

---

### 3. Spooling Device Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 159-187)  
**Table Name:** `0SPTA (=036431)` (Spooling Table Array)  
**Element Size:** 1 word per entry  
**Table Range:** `0SPTA (=036431)` to `0SPLE (=000020)`

#### Purpose
Maps spooling index numbers to logical device numbers. Each entry contains the logical device number for the corresponding spooling index (1-22).

#### Table Entries (Lines 162-184)

| Index | Library Mark | Value | Description                          |
|-------|--------------|-------|--------------------------------------|
| 1     | SLP1         | SLD1 (=000005)  | Spooling device #1 logical number    |
| 2     | SLP2         | SLD2 (=000015)  | Spooling device #2 logical number    |
| 3     | SLP3         | SLD3 (=000022)  | Spooling device #3 logical number    |
| 4     | SLP4         | SLD4 (=000023)  | Spooling device #4 logical number    |
| 5     | SLP5         | SLD5 (=001167)  | Spooling device #5 logical number    |
| 6     | SLP6         | SLD6 (=001175)  | Spooling device #6 logical number    |
| 7     | SLP7         | SLD7  | Spooling device #7 logical number    |
| 8     | SLP8         | SLD8  | Spooling device #8 logical number    |
| 9     | SLP9         | SLD9  | Spooling device #9 logical number    |
| 10    | SLP10        | SLD10 | Spooling device #10 logical number   |
| 11    | SLP11        | SLD11 | Spooling device #11 logical number   |
| 12    | SLP12        | SLD12 | Spooling device #12 logical number   |
| 13    | SLP13        | SLD13 | Spooling device #13 logical number   |
| 14    | SLP14        | SLD14 | Spooling device #14 logical number   |
| 15    | SLP15        | SLD15 | Spooling device #15 logical number   |
| 16    | SLP16        | SLD16 | Spooling device #16 logical number   |
| 17    | SLP17        | SLD17 | Spooling device #17 logical number   |
| 18    | SLP18        | SLD18 | Spooling device #18 logical number   |
| 19    | SLP19        | SLD19 | Spooling device #19 logical number   |
| 20    | SLP20        | SLD20 | Spooling device #20 logical number   |
| 21    | SLP21        | SLD21 | Spooling device #21 logical number   |
| 22    | SLP22        | SLD22 | Spooling device #22 logical number   |

**Table Terminator:** `-1`

**Note:** The actual logical device numbers (SLD1-SLD22) are defined elsewhere in the system configuration.

---

### 4. XMSG Generation Parameters Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 202-229)  
**Table Name:** `XMINA (=036452)` (XMSG Minimum/Current/Maximum Array)  
**Element Size:** 3 words per parameter  
**Used By:** MON 343 (CONFG) monitor call

#### Table Structure

Each parameter has 3 values:
1. **Minimum legal value**
2. **Current generated value** (variable name)
3. **Maximum legal value**

#### Complete Parameter Table (Lines 204-227)

| Index | Parameter | Min   | Current | Max    | Description                                          |
|-------|-----------|-------|---------|--------|------------------------------------------------------|
| 1     | Tasks     | 3     | X4TSK   | 764    | Number of task descriptors                           |
| 2     | Ports     | 3     | X5PRT   | 777    | Number of ports                                      |
| 3     | Names     | 1     | X4NAM   | 1500   | Number of named systems per port                     |
| 4     | Name Len  | 4     | X4NLW   | 24     | Length of name in words                              |
| 5     | Messages  | 2     | X4MES   | 7640   | Number of message elements                           |
| 6     | Msg Size  | 400   | X4MMX   | 77776  | Message size in bytes                                |
| 7     | Task Buf  | 400   | X5MTS   | 177466 | Buffer space owned by task (bytes)                   |
| 10    | Buf Pages | 1     | X4BPG   | 400    | Message buffer space in pages                        |
| 11    | Multicall | 0     | X4MCB   | 144    | Max calls in multicall function                      |
| 12    | Systems   | 1     | X4SIR   | 1500   | Number of accessible systems                         |
| 13    | Links     | 0     | X5LNK   | 310    | Number of links                                      |
| 14    | HDLC (=103112) TO (=172476)   | 0     | X4LTO   | 77777  | HDLC/Megalink timeout (XTU's)                        |
| 15    | RX TO (=172476)     | 0     | X5TO1   | 77777  | Receive datagram timeout (XTU's)                     |
| 16    | TX TO (=172476)     | 0     | X5TO2   | 77777  | Transmit datagram timeout (XTU's)                    |
| 17    | Frame In  | 0     | X3FSZ   | 37776  | Input frame size (words)                             |
| 20    | Frame Out | 0     | X4FSO   | 37776  | Output frame size (words)                            |
| 21    | ACK Fram  | 0     | X4ACK   | 764    | Network acknowledgement frames                       |
| 22    | RX Bufs   | 0     | X4NBF   | 10     | Receive buffers per link                             |
| 23    | TX Bufs   | 1     | X4TMS   | 144    | Transmit buffers per network server                  |
| 24    | SABM Tries| 0     | X4IRM   | 177777 | Max SABM's when starting link                        |
| 25    | Repeats   | 0     | X4RPM   | 77777  | Repeats before link stopped                          |
| 26    | Max Hops  | 0     | X4MXH   | 377    | Number of hops allowed                               |
| 27    | GW TO (=172476)     | 0     | X4NGT   | 77777  | Gateway timeout to net server (XTU's)                |
| 30    | Trace Buf | 0     | X5TRB   | 177    | Number of trace buffers                              |

**Runtime Array:** `XRUNA (=036453)` points to `XMINA+1` (current values only)

**Default Philosophy:**
- **Minimum values** ensure basic system functionality
- **Maximum values** prevent resource exhaustion
- **Current values** are variable names that hold actual configuration

---

### 5. XMSG Calculation Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 234-243)  
**Table Name:** `XCALC (=036562)` (XMSG Size Calculation Array)  
**Element Size:** 1 word per entry  
**Purpose:** Calculate size of XMSG tables on segment S3XMK

| Index | Symbol | Description                                   | Typical Value |
|-------|--------|-----------------------------------------------|---------------|
| 1     | 4TLEN  | Length of one XT-block (task descriptor)      | Variable      |
| 2     | 4PLEN  | Length of one XP-block (port descriptor)      | Variable      |
| 3     | 4MLEN  | Length of one XM-block (message descriptor)   | Variable      |
| 4     | 4LLEN  | Length of one XL-block (link descriptor)      | Variable      |
| 5     | 5FLEN  | Length of one XD/XF-block                     | Variable      |
| 6     | X5FUN (=000060)  | Length of function block                      | Variable      |
| 7     | X6TOP  | End of segment 76                             | Variable      |

---

### 6. Nucleus Generation Parameters Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 247-257)  
**Table Name:** `NUPAR (=036571)` (Nucleus Parameters Array)  
**Element Size:** 1 word per parameter

| Index | Parameter              | Default | Description                                    |
|-------|------------------------|---------|------------------------------------------------|
| 1     | System Msg Buf         | 250     | Total message buffer area for system           |
| 2     | System Descriptors     | 500     | Total number descriptors for system            |
| 3     | Public Msg Buf         | 250     | Total message buffer area for public           |
| 4     | Public Descriptors     | 300     | Total number descriptors for public            |
| 5     | User Msg Buf           | 10      | Message buffer area per user process           |
| 6     | User Descriptors       | 10      | Number descriptors per user process            |
| 7     | Trace Buffer Size      | 2       | Trace buffer size                              |
| 10    | Startup Function       | 0       | Nucleus startup function (future extension)    |
| 11    | (Reserved)             | 0       | Future extension                               |
| 12    | (Reserved)             | 0       | Future extension                               |

---

### 7. Versatec DMA Interface Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 264-281)  
**Table Name:** `0DMVT (=036603)` (DMA Versatec Table)  
**Element Size:** 7 words (TBLVERSATEC)

#### Table Structure (Lines 264-272)

| Offset | Field Name      | Type    | Description                                    |
|--------|-----------------|---------|------------------------------------------------|
| 0      | DMVSELECTION    | INTEGER | 0=don't use, ≠0=use if interface present      |
| 1      | DMVDATFADDR     | INTEGER | Datafield address                              |
| 2      | DMVDLOGNO       | INTEGER | Logical device number of Versatec              |
| 3      | DMVDFLOGNO      | INTEGER | Logical device number of Versatec-DF           |
| 4      | DMVIOLOGNO      | INTEGER | Logical device number of Versatec I/O          |
| 5      | DMVIDENT        | INTEGER | Ident code                                     |
| 6      | DMVHDEV         | INTEGER | IOX number (hardware device)                   |

#### Table Entries

**Entry 1: Versatec Controller 1**
```
Library Mark: "8DMVC
Data:         1, VEFIE (=050172), 577, 576, 22, 4, 600
```

| Field         | Value          | Octal | Description                               |
|---------------|----------------|-------|-------------------------------------------|
| DMVSELECTION  | 1              | 1     | Use if present                            |
| DMVDATFADDR   | VEFIE (=050172) (050172) | -     | Main datafield                            |
| DMVDLOGNO     | 577            | 577   | Logical device number (decimal 383)       |
| DMVDFLOGNO    | 576            | 576   | DF datafield log number (decimal 382)     |
| DMVIOLOGNO    | 22             | 22    | I/O datafield log number (decimal 18)     |
| DMVIDENT      | 4              | 4     | Ident code                                |
| DMVHDEV       | 600            | 600   | IOX number (decimal 384)                  |

**Entry 2: Versatec Controller 2**
```
Library Mark: "8DMV2
Data:         1, VE2FI (=050343), 1125, 1126, 23, 14, 1600
```

| Field         | Value          | Octal | Description                               |
|---------------|----------------|-------|-------------------------------------------|
| DMVSELECTION  | 1              | 1     | Use if present                            |
| DMVDATFADDR   | VE2FI (=050343) (050343) | -     | Main datafield                            |
| DMVDLOGNO     | 1125           | 1125  | Logical device number (decimal 597)       |
| DMVDFLOGNO    | 1126           | 1126  | DF datafield log number (decimal 598)     |
| DMVIOLOGNO    | 23             | 23    | I/O datafield log number (decimal 19)     |
| DMVIDENT      | 14             | 14    | Ident code (decimal 12)                   |
| DMVHDEV       | 1600           | 1600  | IOX number (decimal 896)                  |

**Table Terminator:** `-1`

---

### 8. Synchronous Modem Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 287-303)  
**Table Name:** `0SYMTAB` (Sync Modem Table)  
**Element Size:** 6 words (TBLSYMSIZE)

#### Table Structure (Lines 287-295)

| Offset | Field Name      | Type    | Description                                    |
|--------|-----------------|---------|------------------------------------------------|
| 0      | SYMSELECTION    | INTEGER | 0=don't use this sync modem                    |
| 1      | SYMIDFELT       | INTEGER | Address of input datafield                     |
| 2      | SYMODFELT       | INTEGER | Address of output datafield                    |
| 3      | SYMLOGNO        | INTEGER | Logical device number                          |
| 4      | SYMCIDENT       | INTEGER | Ident code                                     |
| 5      | SYMCLENTRY      | INTEGER | Clear entry flag (set when should be cleared)  |

#### Table Entries

**Entry 1: Sync Modem 1**
```
Library Mark: "8SMO1
Data:         1, IDMO1 (=063336), UDMO1 (=063401), 6, 4, 1
```

| Field         | Value          | Description                               |
|---------------|----------------|-------------------------------------------|
| SYMSELECTION  | 1              | Use this sync modem                       |
| SYMIDFELT     | IDMO1 (=063336) (063336) | Input datafield address                   |
| SYMODFELT     | UDMO1 (=063401) (063401) | Output datafield address                  |
| SYMLOGNO      | 6              | Logical device number (decimal 6)         |
| SYMCIDENT     | 4              | Ident code                                |
| SYMCLENTRY    | 1              | Entry should be cleared                   |

**Entry 2: Sync Modem 2**
```
Library Mark: "8SMO2
Data:         1, IDMO2 (=063442), UDMO2 (=063505), 16, 14, 1
```

| Field         | Value          | Description                               |
|---------------|----------------|-------------------------------------------|
| SYMSELECTION  | 1              | Use this sync modem                       |
| SYMIDFELT     | IDMO2 (=063442) (063442) | Input datafield address                   |
| SYMODFELT     | UDMO2 (=063505) (063505) | Output datafield address                  |
| SYMLOGNO      | 16             | Logical device number (decimal 14)        |
| SYMCIDENT     | 14             | Ident code (decimal 12)                   |
| SYMCLENTRY    | 1              | Entry should be cleared                   |

**Entry 3: Sync Modem 3** (Line 303)
```
Data: -1  (Terminator - no third modem configured)
```

---

### 9. Universal DMA/VICOM/RAMTEC Configuration Table

**Source File:** `PH-P2-CONFG-TAB.NPL` (Lines 319-339)  
**Table Name:** `TBUDMA` (Universal DMA Table)  
**Element Size:** 10 words (TUDMSIZE)

#### Table Structure (Lines 307-318)

| Offset | Field            | Description                              |
|--------|------------------|------------------------------------------|
| 0      | Datafield Addr   | Address of datafield                     |
| 1      | Logical Dev No   | Logical device number                    |
| 2      | UDMA (=110770) Dev No      | UDMA (=110770) device number                       |
| 3      | UDMA (=110770) Ident       | UDMA (=110770) ident code                          |
| 4      | VICOM Dev No     | VICOM device number                      |
| 5      | VICOM Ident      | VICOM ident code                         |
| 6      | RAMTEC Dev No    | RAMTEC device number                     |
| 7      | RAMTEC Ident     | RAMTEC ident code                        |
| 8-9    | (Reserved)       | Padding                                  |

#### Table Entries (Lines 321-337)

| Unit | DF              | LogNo | UDMA (=110770) Dev | UDMA (=110770) Id | VICOM Dev | VICOM Id | RAMTEC Dev | RAMTEC Id |
|------|-----------------|-------|----------|---------|-----------|----------|------------|-----------|
| 1    | UDI01 (=105162) (105162)  | 2100  | 140050   | 140010  | 141460    | 140146   | 141760     | 140176    |
| 2    | UDI02 (=105357) (105357)  | 2101  | 140060   | 140011  | 141470    | 140147   | 141770     | 140177    |
| 3    | UDI03 (=105554) (105554)  | 2102  | 140070   | 140012  | 141500    | 140150   | 142000     | 140200    |
| 4    | UDI04 (=105751) (105751)  | 2103  | 140100   | 140013  | 141510    | 140151   | 142010     | 140201    |
| 5    | UDI05 (=106146) (106146)  | 2104  | 140110   | 140014  | 141520    | 140152   | 142020     | 140202    |
| 6    | UDI06 (=106343) (106343)  | 2105  | 140120   | 140015  | 141530    | 140153   | 142030     | 140203    |
| 7    | UDI07 | 2106  | 140130   | 140016  | 141540    | 140154   | 142040     | 140204    |
| 8    | UDI08 | 2107  | 140140   | 140017  | 141550    | 140155   | 142050     | 140205    |
| 9    | UDI09 | 2110  | 140150   | 140020  | 141560    | 140156   | 142060     | 140206    |
| 10   | UDI10 | 2111  | 140160   | 140021  | 141570    | 140157   | 142070     | 140207    |
| 11   | UDI11 | 2112  | 140170   | 140022  | 141600    | 140160   | 142100     | 140210    |
| 12   | UDI12 | 2113  | 140200   | 140023  | 141610    | 140161   | 142110     | 140211    |
| 13   | UDI13 | 2114  | 140210   | 140024  | 141620    | 140162   | 142120     | 140212    |
| 14   | UDI14 | 2115  | 140220   | 140025  | 141630    | 140163   | 142130     | 140213    |
| 15   | UDI15 | 2116  | 140230   | 140026  | 141640    | 140164   | 142140     | 140214    |
| 16   | UDI16 | 2117  | 140240   | 140027  | 141650    | 140165   | 142150     | 140215    |

**Table Terminator:** `-1` (Line 337)

**Note:** This table supports three different DMA interface types:
- **UDMA:** Universal DMA interface ND-850
- **VICOM:** VICOM interface  
- **RAMTEC:** RAMTEC interface

The system selects which type to use based on hardware detection.

---

### 10. Server Parameter Array

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 8-9)  
**Table Name:** `SERVARRAY` (Server Parameters)  
**Element Size:** 16 words

#### Table Data (Line 8)

```npl
INTEGER ARRAY SERVARRAY:=(XSERV (=007312), 0, XSERV (=007312), 0, XSERV (=007312), MTSPA (=011043), DPFLG (=042727), 0,
                          0, 0, 0, 0, 0, 0, 0, 0);
```

| Index | Value           | Description                                    |
|-------|-----------------|------------------------------------------------|
| 0     | XSERV (=007312) (007312)  | Server function entry point 1                  |
| 1     | 0               | Reserved                                       |
| 2     | XSERV (=007312) (007312)  | Server function entry point 2                  |
| 3     | 0               | Reserved                                       |
| 4     | XSERV (=007312) (007312)  | Server function entry point 3                  |
| 5     | MTSPA (=011043) (011043)  | MTAD server parameter address                  |
| 6     | DPFLG (=042727) (042727)  | Driver/parameter flags                         |
| 7-15  | 0               | Reserved/padding                               |

---

### 11. Nucleus MTAD Datafield

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 14-21)  
**Purpose:** MTAD (Message Transfer and Delivery) for nucleus communication

#### Configuration Parameters

| Symbol          | Default | Description                              |
|-----------------|---------|------------------------------------------|
| MTNSP (=000010)  (000010) | 10      | Number of nucleus MTAD clients           |

#### Datafield Structure (Lines 15-20)

```npl
INTEGER ARRAY MTNKDF(7)              % Standard datafield part
*MTNKD/0;0;0;2;0;0;MTNUC             % MFUNC (=000006) points to driver
INTEGER MTNKPORT                     % Own nucleus port
**<*+MTNSP; )ZERO                    % Port array (10 entries)
DOUBLE ARRAY MTSRE (=011023)(0)                % Send references and LAMU pages
**+MTNSP+MTNSP/                      % 2*MTNSP entries
```

| Field         | Offset | Value          | Description                           |
|---------------|--------|----------------|---------------------------------------|
| RESLINK       | 0      | 0              | Reservation link                      |
| RTRES (=000001)         | 1      | 0              | Reserving RT (=036620) program                  |
| BWLINK        | 2      | 0              | Beginning of waiting queue            |
| TYPRING       | 3      | 2              | Device type bits and ring             |
| ISTATE        | 4      | 0              | Device state                          |
| MLINK (=000005)         | 5      | 0              | Monitor queue link                    |
| MFUNC (=000006)         | 6      | MTNUC (=070006) (070006) | Monitor function (nucleus driver)     |
| MTNKPORT      | 7      | -              | Own port number                       |
| Ports         | 8+     | -              | MTNSP (=000010) port entries                    |
| MTSRE (=011023)         | -      | -              | Send references (2*MTNSP entries)     |

#### Parameter List (Line 21)

```npl
INTEGER ARRAY MTSPA:=(MTNKDF,MTNKPO,MTCHK,MTSRE)
```

---

### 12. Background Allocation Constants

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 28-29)  
**Library Mark:** `"8BACS` (Background Allocation Control System)

#### Decimal Constants Array (Line 28)

```npl
@DEC
DATA (=054400)(10000,1000,100,10,1); INTEGER ARRAY BCONST(0)
@OCT
```

| Index | Value (Decimal) | Description                              |
|-------|-----------------|------------------------------------------|
| 0     | 10000           | Ten thousand (for time calculations)     |
| 1     | 1000            | Thousand                                 |
| 2     | 100             | Hundred                                  |
| 3     | 10              | Ten                                      |
| 4     | 1               | One                                      |

**Purpose:** Used by `9BPTMOUT` routine for converting time values and calculating minutes/seconds for background process timeout warnings.

---

### 13. ND-500 Routine Vector Table

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 89-105)  
**Table Name:** `GOVECTOR` (Go Vector for inter-PIT calls)  
**Element Size:** 2 words per entry (PIT number + routine address)

#### Table Structure (Lines 89-103)

| Entry | PIT             | Routine        | Description                              |
|-------|-----------------|----------------|------------------------------------------|
| 1     | NMPIT (=050000)  (050000) | XKICK500       | Kick ND-500 processor                    |
| 2     | NMPIT (=050000)  (050000) | XTER500        | Terminate ND-500                         |
| 3     | NMPIT (=050000)  (050000) | XACT500        | Activate ND-500                          |
| 4     | NMPIT (=050000)  (050000) | XACTRDY        | Activate ready queue                     |
| 5     | NMPIT (=050000)  (050000) | XRSTARTALL     | Restart all processors                   |
| 6     | NRPIT (=040000)  (040000) | XIBMOVE        | Inter-bank move operation                |
| 7     | NRPIT (=040000)  (040000) | XCLEAN         | Clean/initialize                         |
| 8     | NRPIT (=040000)  (040000) | XMSINIT        | Message system initialize                |
| 9     | NRPIT (=040000)  (040000) | X5ERACTIVATE   | Error activation                         |
| 10    | NRPIT (=040000)  (040000) | X5PRACTIVATE   | Process activation                       |
| 11    | NRPIT (=040000)  (040000) | X5XGBUFF       | Extended get buffer                      |
| 12    | NRPIT (=040000)  (040000) | X5GBUFF        | Get buffer                               |
| 13    | NRPIT (=040000)  (040000) | XX5ONOMD       | Command handling                         |

**Return Vector (Line 104):**
```npl
INTEGER ARRAY RETVECTOR:=(0,0,0,0,0,R5PIT,0,0,RRPIT,0,RMPIT,0,0,0,0,0);
```

---

### 14. ND-500 Patch Tables

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 174-227)  
**Purpose:** Branch and conversion tables for ND-500 multi-processor support

#### Main Navigation Table (Lines 174-180)

**NNTAB** - Dispatch table linking PITs to their specific tables:

| PIT  | Instr  | Table  | Instr  | Table  | Description                    |
|------|--------|--------|--------|--------|--------------------------------|
| MPIT (=000012) | 124001 | MNATAB | 124001 | MNJTAB | Common & MPIT (=000012) navigation       |
| MPIT (=000012) | 124002 | MNTTAB | 124003 | MNCTAB | Common & MPIT (=000012) terminate/conv   |
| RPIT (=000010) | 124002 | RNTTAB | 124003 | RNCTAB | RPIT (=000010) terminate/convert         |
| 5PIT (=000005) | 124002 | 5NTTAB | 124003 | 5NCTAB | 5PIT (=000005) (ND-500) terminate/conv   |

#### MNATAB - Common/MPIT Navigation (Line 185)

```npl
INTEGER ARRAY MNATAB:=(-1);  % Terminated with -1
```

#### MNJTAB - Common/MPIT Branch Table (Lines 188-192)

| Index | Routine | Description                              |
|-------|---------|------------------------------------------|
| 0     | NNJ00 (=022503)   | Jump target 0                            |
| 1     | NNJ01 (=022516)   | Jump target 1                            |
| 2     | NNJ02 (=030455)   | Jump target 2                            |
| 3     | NNJ03 (=022763)   | Jump target 3                            |
| 4     | NNJ04 (=023454)   | Jump target 4                            |
| 5     | NNJ05 (=023706)   | Jump target 5                            |
| 6     | NNJ06 (=024041)   | Jump target 6                            |
| 7     | NNJ07 (=135020)   | Jump target 7                            |
| 8     | NNJ08 (=135067)   | Jump target 8                            |
| 9     | NNJ09 (=136137)   | Jump target 9                            |
| 10    | NNJ10 (=137022)   | Jump target 10                           |
| 11    | NNJ11 (=145276)   | Jump target 11                           |
| 12    | NNJ12 (=145403)   | Jump target 12                           |
| 13    | NNJ13 (=145466)   | Jump target 13                           |
| 14    | NNJ14 (=145751)   | Jump target 14                           |
| 15-19 | 0       | Reserved/patch space                     |

**Table Terminator:** `-1`

#### MNTTAB - Common/MPIT Terminate-500 Table (Lines 194-198)

| Index | Routine | Description                              |
|-------|---------|------------------------------------------|
| 0-16  | NNT01-  | Terminate routines 1-17                  |
|       | NNT17 (=145120)   |                                          |
| 17-19 | NNT50-  | Performance monitor call routines        |
|       | NNT52 (=131022)   |                                          |
| 20-24 | 0       | Reserved/patch space                     |

**Table Terminator:** `-1`

#### RNTTAB - RPIT (=000010) Terminate-500 Table (Lines 201-203)

| Index | Routine      | Description                         |
|-------|--------------|-------------------------------------|
| 0-7   | NNT30-NNT37  | RPIT (=000010) terminate routines 30-37       |
| 8-12  | 0            | Reserved/patch space                |

**Table Terminator:** `-1`

#### 5NTTAB - 5PIT (=000005) Terminate-500 Table (Lines 206-208)

| Index | Routine      | Description                         |
|-------|--------------|-------------------------------------|
| 0-1   | NNT40-NNT41  | 5PIT (=000005) terminate routines 40-41       |
| 2-6   | 0            | Reserved/patch space                |

**Table Terminator:** `-1`

#### MNCTAB - Common/MPIT Convert-Address Table (Lines 211-216)

Contains 27 normal conversion routines (NNC01-NNC27) plus 6 performance monitor routines (NNC50-NNC55), followed by 5 reserved entries.

#### RNCTAB - RPIT (=000010) Convert-Address Table (Lines 219-221)

Contains 10 RPIT-specific conversion routines (NNC30-NNC39) plus 5 reserved entries.

#### 5NCTAB - 5PIT (=000005) Convert-Address Table (Lines 224-226)

Contains 1 conversion routine (NNC40 (=034066)) plus 5 reserved entries.

**Common Pattern:**
- All tables terminate with `-1`
- Each has 5 reserved entries for patching
- Organized by PIT (Processor Interrupt Table) type

---

### 15. XSEMS (=011507) - ND-500 Semaphore Table

**Source File:** `DP-P2-VARIABLES.NPL` (Lines 141-142)  
**Table Name:** `XSEMS (=011507)` (ND-500 Locks/Semaphores)

#### Table Data (Line 141)

```npl
INTEGER ARRAY XSEMS:=(5NAMSEM,CSSEM,PLSSEM,FIXSEM,CSSEM,PLSSEM,
                      SYDSEG,SWORKA,WEMSEM,WEMSEM,
                      CPU51 (=011605),CPU52,CPU53,CPU54,CPU55,CPU56,CPU57,CPU58);
```

| Index | Semaphore        | Description                              |
|-------|------------------|------------------------------------------|
| 0     | 5NAMSEM (110003) | ND-500 name semaphore                    |
| 1     | CSSEM (=110007)   (110007) | Cache synchronization semaphore          |
| 2     | PLSSEM  (110013) | Page list semaphore                      |
| 3     | FIXSEM  (110017) | Fix semaphore                            |
| 4     | CSSEM (=110007)   (110007) | Cache sync (duplicate entry)             |
| 5     | PLSSEM  (110013) | Page list (duplicate entry)              |
| 6     | SYDSEG  (110023) | System data segment semaphore            |
| 7     | SWORKA           | Swap work area semaphore                 |
| 8     | WEMSEM           | Write-error message semaphore            |
| 9     | WEMSEM           | Write-error message (duplicate)          |
| 10    | CPU51 (=011605)   (011605) | CPU #51 semaphore (if configured)        |
| 11    | CPU52 (=011614)   (011614) | CPU #52 semaphore (if configured)        |
| 12    | CPU53 (=011623)   (011623) | CPU #53 semaphore (if configured)        |
| 13    | CPU54 (=011632)   (011632) | CPU #54 semaphore (if configured)        |
| 14    | CPU55            | CPU #55 semaphore (if configured)        |
| 15    | CPU56            | CPU #56 semaphore (if configured)        |
| 16    | CPU57            | CPU #57 semaphore (if configured)        |
| 17    | CPU58            | CPU #58 semaphore (if configured)        |

---

### 16. Generation Default Parameters

**Source File:** `0.SIN-GEN.NPL` (Lines 333-437)  
**Purpose:** System generation default values

#### Major Parameters (Lines 335-354)

| Symbol            | Line | Default | Description                                    |
|-------------------|------|---------|------------------------------------------------|
| 5FYFS (=000144)    (000144) | 335  | 144     | Filesystem save-area page displacement         |
| 5FYRL (=000502)    (000502) | 336  | 502     | RT-loader page displacement                    |
| 5FYSP (=000534)    (000534) | 337  | 534     | Spooling page displacement                     |
| 5FCRL (=000131)    (000131) | 338  | 32+77   | ND-NET filecopy page displacement              |
| 5FYER (=000136)    (000136) | 339  | 136     | Error program page displacement                |
| 5FYOP (=000077)    (000077) | 340  | 77      | Command segment page displacement              |
| 5NASE (=000517)            | 344  | 517     | (Undocumented parameter)                       |
| FLBPA (=000100)    (000100) | 346  | 100     | First logical page in background segment       |
| LOADR (=000200)    (000200) | 347  | 200     | Length of background segment                   |
| SEMSE (=001204)    (001204) | 350  | 1204    | Semaphore segment                              |
| XDASA (=000010)            | 351  | 10      | (Undocumented)                                 |
| MACD (=153777)             | 352  | 153777  | (Undocumented)                                 |
| N5ADR (=100000)            | 353  | 100000  | ND-500 address base                            |

#### Terminal Ident Codes (Lines 360-361)

| Symbol           | Value       | Description                                    |
|------------------|-------------|------------------------------------------------|
| LIDTE (=140037)   (140037) | 140040-1    | Ident code of terminal 65 (-1) = 140037        |
| HIDTE (=140436)   (140436) | 140437-1    | Ident code of terminal 256 (-1) = 140436       |

This creates a range of ident codes from 140037 to 140436 for terminals 1-256.

#### Default Values Section (Lines 364-410)

| Symbol            | Line | Default | Condition | Description                                |
|-------------------|------|---------|-----------|-------------------------------------------|
| SPSGL (=000004)    (000004) | 364  | 4       | if undef  | Length of spooling queue segment          |
| LBSEG (=100000)    (100000) | 365  | -       | if undef  | (Uses LOADR (=000200))                              |
| IDBUS (=000040)    (000040) | 367  | 40      | if undef  | ID bus parameter                          |
| TERMC (=177777)    (177777) | 368  | -1      | if undef  | Terminal count                            |
| MIBU1 (=002114)    (002114) | 369  | 2114    | if undef  | Max input buffer unit 1                   |
| MOBU1 (=000344)    (000344) | 370  | 344     | if undef  | Max output buffer unit 1                  |
| MIBU2 (=002114)            | 371  | 2114    | if undef  | Max input buffer unit 2                   |
| MOBU2 (=000344)            | 372  | 344     | if undef  | Max output buffer unit 2                  |
| ...              | ...  | ...     | ...       | (Pattern continues for units 3-16)        |
| DR1SI (=002000)    (002000) | 401  | 2000    | if undef  | Disk 1 size                               |
| DR2SI (=002000)    (002000) | 402  | 2000    | if undef  | Disk 2 size                               |
| 5SSSZ (=000005)    (000005) | 403  | 5       | if undef  | System segment size                       |
| GNSTA (=156000)    (156000) | 404  | calc    | if undef  | = 5SSSZ@12+BGSYS                          |
| MXLPR (=000002)    (000002) | 406  | 2       | 8LAMU     | Number of LAMUs per program               |
| MXLAM (=000040)    (000040) | 407  | 40      | 8LAMU     | Number of LAMUs in system                 |
| MXSYL (=000100)    (000100) | 408  | -       | 8LAMU     | Number of system LAMUs                    |
| FLDNO (=000277)    (000277) | 410  | 277     | if undef  | Device number for "FTX" error handler     |
| CCNO (=000006)             | 415  | 6       | Comment   | RT (=036620) common size (depends on SIBAS procs)   |
| NULDN (=000010)    (000010) | 423  | 10      | if undef  | Number of user reserved logical dev nums  |
| XTIME (=000006)    (000006) | 426  | 6       | if undef  | Number of extra timer table entries       |
| XID10 (=000012)    (000012) | 427  | 12      | if undef  | Extra ident table entries, level 10       |
| XID11 (=000012)    (000012) | 428  | 12      | if undef  | Extra ident table entries, level 11       |
| XID12 (=000012)    (000012) | 429  | 12      | if undef  | Extra ident table entries, level 12       |
| XID13 (=000004)    (000004) | 430  | 4       | if undef  | Extra ident table entries, level 13       |
| 8RTN (=000226)             | 432  | -       | if 8RTN (=000226)   | 24 RT (=036620) descriptions                        |
| 8SGN (=001356)             | 433  | -       | if 8SGN (=001356)   | 40 segments                               |
| CADEV (=000700)    (000700) | 434  | 700     | if undef  | (Cassette device?)                        |
| 8IOXT (=000010)            | 435  | 10      | if 8IOXT (=000010)  | I/O extended                              |
| NALME (=000160)    (000160) | 436  | -       | if undef  | Max # of memory buffers to allocate       |

**Pattern for Buffer Sizes:**
- All MIBUx (Max Input Buffer) default to 2114 (octal) = 1100 (decimal)
- All MOBUx (Max Output Buffer) default to 344 (octal) = 228 (decimal)
- This pattern continues for units 1-16

---

### 17. Memory Layout Physical Page Tables

**Source File:** `PH-P2-START-BASE.NPL` (Lines 20-132)  
**Purpose:** Track physical memory page allocation for each system component

#### Table Structure Pattern

Each component has:
- **DOUBLE** - Combined first+last page address
- **First Page Variable** - Starting physical page
- **Last Page Variable** - Ending physical page

#### Complete Memory Region Table (Lines 29-131)

| Component               | First Page Var   | Last Page Var   | Default First | Default Last | Description                    |
|-------------------------|------------------|-----------------|---------------|--------------|--------------------------------|
| Memory Map              | MMFPAGE          | MMLPAGE         | -1            | -1           | Memory management              |
| Device Buffers          | DBFPAGE          | DBLPAGE         | -1            | -1           | I/O buffers                    |
| LAMU Tables             | FLAMPAGE         | LLAMPAGE        | -1            | -1           | Memory allocation units        |
| Segment Table           | SGTFPHPAGE       | SGTLPHPAGE      | -1            | -1           | Segment management             |
| Logical Number Table    | LGTFPHPAGE       | LGTLPHPAGE      | -1            | -1           | Logical number table           |
| Extended Common         | ECOFPHPAGE       | ECOLPHPAGE      | -1            | -1           | Extended common code           |
| RPIT (=000010)                    | RPIFPHPAGE       | RPILPHPAGE      | -1            | -1           | RP interrupt table             |
| MPIT (=000012)                    | MPIFPHPAGE       | MPILPHPAGE      | -1            | -1           | MP interrupt table             |
| IPIT (=000015)                    | IPIFPHPAGE       | IPILPHPAGE      | -1            | -1           | IP interrupt table             |
| Sync Modem Buffer       | SYMFPHPAGE       | SYMLPHPAGE      | -1            | -1           | Big sync modem buffer          |
| Common Code             | CMFPHPAGE        | CMLPHPAGE       | -1            | -1           | Common code segment            |
| DPIT (=000007)                    | DPIFPHPAGE       | DPILPHPAGE      | -1            | -1           | DP interrupt table             |
| Restart Routine         | RSFPHPAGE        | RSLPHPAGE       | -1            | -1           | Restart/regblocks/bitmaps      |
| Page Owner Table        | POWFPAG          | POWLPAG         | -1            | -1           | Page ownership tracking        |
| TAD Datafields          | FTADPAGE         | LTADPAGE        | -1            | -1           | TAD management                 |
| Terminal Datafields     | TDFPAGE          | TDLPHPAGE       | -1            | -1           | Terminal control               |
| I/O Buffers             | IOBFPHPAGE       | IOBLPHPAGE      | -1            | -1           | General I/O buffers            |
| RT (=036620) System Segment       | RTSGFPHPAGE      | RTSGLPHPAGE     | -1            | -1           | RT's system segment            |
| PITs                    | PITSFPHPAGE      | PITSLPHPAGE     | 76            | 77           | Page interrupt tables          |
| HDLC (=103112) Buffers            | HDLCFPHPAGE      | HDLCLPHPAHE     | -1            | -1           | HDLC (=103112) communication             |
| Edit Routine Segment    | EDFPHPAGE        | EDLPHPAGE       | -1            | -1           | Editor segment                 |
| Background Alloc Tables | BACSFPHPAGE      | BACSLPHPAGE     | -1            | -1           | Background allocation          |
| 5PIT (=000005) Segment            | 5PIFPHPAGE       | 5PILPHPAGE      | -1            | -1           | Level 5 PIT                    |
| Program Log Buffer      | 8LOGFPHPAGE      | 8LOGLPHPAGE     | -1            | -1           | Program logging sampling       |
| Spare Track Buffer      | SPTRFPHPAGE      | SPTRLPHPAGE     | -1            | -1           | Spare track addresses          |
| Monitor Call Log        | CMCLFPHPAGE      | CMCLLPHPAGE     | -1            | -1           | Monitor call logging           |
| RT (=036620) Work Field           | FRTWFPAGE        | LRTWFPAGE       | -1            | -1           | RT (=036620) program mon.call work       |
| Cache Inhibit Bitmap    | FPHPCACHEINHIBIT | LPHPCACHEINHIBIT| -1            | -1           | Cache control bitmap           |
| SCSI Buffers            | SCFPHYSPAGE      | SCLPHYSPAGE     | -1            | -1           | SCSI device buffers            |
| Nucleus                 | NUFPHPAGE        | NULPHPAGE       | -1            | -1           | System nucleus                 |
| Octobus                 | OCFPHPAGE        | OCLPHPAGE       | -1            | -1           | Octobus interface              |
| Error Device Buffer     | IERFPHPAGE       | IERLPHPAGE      | -1            | -1           | Error device                   |
| Filesystem              | FSEFP (=170313)            | FSELP (=170314)           | -1            | -1           | Filesystem (fixed in low mem)  |
| Microcode               | UCFPA (=170315)            | UCTPA (=170316)           | -1            | -1           | Microcode storage              |

**Note:** Default value of `-1` means "not allocated" - these are filled in during system startup based on actual configuration and available memory.

**Exception:** PITs default to pages 76-77 (fixed location required for interrupt handling).

---

### 18. Background Process Timeout Text Strings

**Source File:** `RP-P2-1.NPL` (Lines 278-286)  
**Purpose:** Text messages for background process timeout warnings  
**Used By:** `9BPTMOUT` RT (=036620) program

#### String Table (Lines 278-286)

| Symbol   | Line | Value/Content                                               | Description                    |
|----------|------|-------------------------------------------------------------|--------------------------------|
| TXCRLF   | 278  | `6412; 6412; #''`                                           | CR+LF, CR+LF, null terminator  |
| TXBELL   | 279  | `3407; #''`                                                 | Bell character, null term      |
| TXWARN   | 280  | `3407; 3407; 3415`                                          | Bell, Bell, CR (=051152)                 |
|          | 281  | `'IF NO ACTIVITY ON THE TERMINAL, YOU WILL BE LOGGED OUT IN  '` | Warning message part 1     |
| XTMLEFT  | 283  | `'              '`                                          | Placeholder for time (14 spaces)|
| TXMINS   | 284  | `' MINUTE'`                                                 | " MINUTE" text                 |
| TXXBEL   | 285  | `3407; 3407; #''`                                           | Bell, Bell, null               |
| TXBLA    | 286  | `'  '`                                                      | Two spaces                     |

#### Complete Warning Message Assembly

When displayed, these strings combine to form:
```
<BELL><BELL><CR>
IF NO ACTIVITY ON THE TERMINAL, YOU WILL BE LOGGED OUT IN <time> MINUTE[S]  <BELL><BELL>
```

**Character Codes:**
- `3407` (octal) = Bell/BEL character (ASCII 7)
- `6412` (octal) = CR+LF (Carriage Return + Line Feed)
- `3415` (octal) = CR (=051152) (Carriage Return only)

**Dynamic Content:**
- `XTMLEFT` is filled at runtime with the calculated time remaining
- 'S' is appended if time ≠ 1 minute

---

### Summary Statistics

#### Total Number of Tables Documented

| Category                    | Table Count | Total Elements      |
|-----------------------------|-------------|---------------------|
| Configuration Tables        | 8           | Variable            |
| Variable Tables             | 5           | 50+                 |
| Patch/Vector Tables         | 8           | 100+                |
| Memory Layout Tables        | 30          | 60 (30 pairs)       |
| Generation Parameters       | 1           | 50+                 |
| Text String Tables          | 1           | 8 strings           |
| **TOTAL**                   | **53**      | **350+ entries**    |

#### Table Size Characteristics

| Element Size | Table Count | Examples                                 |
|--------------|-------------|------------------------------------------|
| 1 word       | 15          | SPTA, BCONST, XSEMS (=011507)                      |
| 2 words      | 5           | DOUBLE pairs, GOVECTOR entries           |
| 3 words      | 2           | XMINA (=036452) parameters, NUPAR (=036571)                  |
| 6 words      | 1           | SYMTAB (sync modem)                      |
| 7 words      | 1           | DMVT (Versatec)                          |
| 10 words     | 1           | TBUDMA (universal DMA)                   |
| 12 words     | 1           | HDLC (=103112) table                               |
| 16 words     | 2           | SERVARRAY, Line printer table            |
| Variable     | 25          | Memory layout tables, text strings       |

---

## Index of All Tables by Source File

### PH-P2-CONFG-TAB.NPL
1. HDLC (=103112) Device Configuration Table (0HDTA (=036177))
2. Line Printer Configuration Table (0LPTA (=036370))
3. Spooling Device Table (0SPTA (=036431))
4. XMSG Generation Parameters (XMINA (=036452))
5. XMSG Calculation Table (XCALC (=036562))
6. Nucleus Parameters (NUPAR (=036571))
7. Versatec DMA Table (0DMVT (=036603))
8. Sync Modem Table (0SYMTAB)
9. Universal DMA/VICOM/RAMTEC Table (TBUDMA)

### DP-P2-VARIABLES.NPL
10. Server Parameter Array (SERVARRAY)
11. Nucleus MTAD Datafield (MTNKDF/MTSPA)
12. Background Allocation Constants (BCONST)
13. ND-500 Routine Vector (GOVECTOR/RETVECTOR)
14. ND-500 Patch Tables (NNTAB (=011641), MNJTAB, MNTTAB, etc.)
15. XSEMS (=011507) Semaphore Table (XSEMS (=011507))

### 0.SIN-GEN.NPL
16. Generation Default Parameters (multiple symbols)
17. Terminal Ident Code Ranges (LIDTE (=140037), HIDTE (=140436))

### PH-P2-START-BASE.NPL
18. Memory Layout Physical Page Tables (30 table pairs)

### RP-P2-1.NPL
19. Background Timeout Text Strings (TXCRLF, TXWARN, etc.)

---

**End of Document**

