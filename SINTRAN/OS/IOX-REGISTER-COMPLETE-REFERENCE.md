# Complete IOX Register Reference

**All IOX Device Registers and Bit Definitions**

**Version:** 1.0  
**Date:** 2025-01-XX  
**Status:** Complete  
**Source:** Analysis of SINTRAN III source code and hardware documentation

---

## Table of Contents

1. [Overview](#1-overview)
2. [Memory Devices](#2-memory-devices)
   - [BIG MPM ERROR LOG (MPM3)](#21-big-mpm-error-log-mpm3)
   - [BUS EXPANDER](#22-bus-expander)
   - [ECCR (Error Correction Register)](#23-eccr-error-correction-register)
   - [BUSC Devices (MPM4)](#24-busc-devices-mpm4)
3. [Communication Devices](#3-communication-devices)
   - [HDLC Controllers (COM5025)](#31-hdlc-controllers-com5025)
   - [Octobus Interface](#32-octobus-interface)
4. [ND-500 Interface](#4-nd-500-interface)
   - [3022 Interface (PCB 3022)](#41-3022-interface-pcb-3022)
5. [Device Address Summary](#5-device-address-summary)
6. [Register Access Patterns](#6-register-access-patterns)

---

## 1. Overview

**IOX Instructions:**

SINTRAN uses **IOX** (I/O eXecute) instructions to access hardware device registers. There are two main forms:

- **`IOX address`** - Direct I/O instruction to fixed address
- **`IOXT`** - I/O Transfer instruction (T register contains address)
- **`*EXR ST`** - Execute I/O instruction (T register contains address, A register contains data)

**Register Access Methods:**

```npl
% Method 1: Direct IOX
*IOX 750; TRA IIC    % Read from address 750₈

% Method 2: IOXT (I/O Transfer)
T:=100000; *IOXT; TRA IIC    % Read from address in T register

% Method 3: EXR ST (Execute Register Store)
T:=HDEV+RRTS; *EXR ST    % Read register, result in A
A:=value; T:=HDEV+WRTC; *EXR ST    % Write value to register
```

**Result Interpretation:**

- **`TRA IIC`** - Transfer to Internal Interrupt Code register
- **A=0:** Device present/operation successful
- **A≠0:** I/O error (device absent or operation failed)

---

## 2. Memory Devices

### 2.1 BIG MPM ERROR LOG (MPM3)

**Base Address:** 750₈ (488 decimal, 0x1E8 hex)  
**Address Range:** 750₈ - 753₈ (4 registers)  
**Device Number:** 5₈  
**Purpose:** MPM3 (Multiport Memory Module 3) controller status and error logging

#### 2.1.1 Register Map

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Purpose |
|--------|----------------|-------------------|----------|--------|---------|
| +0 | 750₈ | 488 / 0x1E8 | **Status/Test** | Read | Device presence test |
| +1 | 751₈ | 489 / 0x1E9 | **Page Test** | Read/Write | Page-level MPM3 memory test |
| +2 | 752₈ | 490 / 0x1EA | **Error Log 1** | Read | Error log register 1 |
| +3 | 753₈ | 491 / 0x1EB | **Error Log 2** | Read | Error log register 2 |

#### 2.1.2 Status/Test Register (750₈)

**Detection Usage:**
```npl
% From PH-P2-OPPSTART.NPL, lines 328-333, 2413-2414
A:=200; *TRR IIE; TRA IIC; IOX 750; TRA IIC
IF A=0 THEN
    % MPM3 controller present
    A:=3777; A=:ENDPAGE    % Set 2MB limit
    MEMTYPE BONE BMPM3=:MEMTYPE
ELSE
    % No MPM3 controller
    A:=37777; A=:ENDPAGE   % Set 16MB limit
FI
```

**Register Definition:**
- **Read Operation:** `IOX 750`
- **Result:**
  - **A=0:** MPM3 controller present
  - **A≠0:** I/O error (controller absent)

**Bit Usage:** Not detailed in source code - simple presence test only

#### 2.1.3 Page Test Register (751₈)

**Usage:**
```npl
% From PH-P2-OPPSTART.NPL, lines 3857-3860
A:=140751; *IOX 751        % Write test pattern 1
0=:X.S0; A:=140764; *IOX 751; TRR 10    % Write test pattern 2, read register 10
X.S0; *TRA IIC
IF A=10 THEN
    % Page is MPM3 memory
    T:=KMPM3; A:=CURRPAGE; CALL SMEMTYPE
FI
```

**Register Definition:**
- **Write Operation:** Write test patterns to test page
- **Read Operation:** Read result from register 10
- **Test Patterns:**
  - Pattern 1: `140751₈` (octal)
  - Pattern 2: `140764₈` (octal)
- **Result:** A=10₈ if page is MPM3 memory

**Bit Usage:** Not detailed in source code - hardware-specific test sequence

#### 2.1.4 Error Log Registers (752₈, 753₈)

**Purpose:** Error logging for MPM3 memory errors

**Error Log 1 (752₈) - Bit Map:**

Based on MPM5 documentation (similar hardware):

| Bit | Name | Description |
|-----|------|-------------|
| 0 | BA18 | Bus address bit 18 |
| 1 | BA19 | Bus address bit 19 |
| 2 | BA20 | Bus address bit 20 |
| 3 | BA21 | Bus address bit 21 |
| 4 | BA22 | Bus address bit 22 |
| 8 | BDREQERROR | Bus data request error, timeout |
| 9 | BDRYERROR | Bus data ready error, timeout |
| 10 | BARYERROR | Bus address ready error, timeout |
| 11 | BREF | Bus refresh |
| 12-15 | Reserved | Not used |

**Error Log 2 (753₈) - Bit Map:**

| Bit | Name | Description |
|-----|------|-------------|
| 0-4 | BMCOD | Bus master code bits |
| 5 | BLOCAL | Bus local access |
| 6 | BGLOBAL | Bus global access |
| 7 | LOGALL | Log all errors on bus |
| 8-12 | BSCOD | Bus slave code bits |
| 13-15 | BCYCLE | Bus cycle type (000=unused, 001=memory, 010=I/O) |

**Note:** Error log registers are locked when errors occur and must be read to unlock.

---

### 2.2 BUS EXPANDER

**Base Address:** 100000₈ (32768 decimal, 0x8000 hex)  
**Address Range:** 100000₈ - 100003₈ (4 registers)  
**Device Number:** 10₈  
**Purpose:** Memory expansion controller indicator

#### 2.2.1 Register Map

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Purpose |
|--------|----------------|-------------------|----------|--------|---------|
| +0 | 100000₈ | 32768 / 0x8000 | **Status/Test** | Read | Device presence test |
| +1 | 100001₈ | 32769 / 0x8001 | **Reserved** | - | Not used in boot code |
| +2 | 100002₈ | 32770 / 0x8002 | **Reserved** | - | Not used in boot code |
| +3 | 100003₈ | 32771 / 0x8003 | **Reserved** | - | Not used in boot code |

#### 2.2.2 Status/Test Register (100000₈)

**Detection Usage:**
```npl
% From PH-P2-OPPSTART.NPL, lines 2409-2411
*"8BEX1
T:=100000; *IOXT; TRA IIC
IF A=0 THEN MEMTYPE BONE BBEXPANDER=:MEMTYPE FI
```

**Register Definition:**
- **Read Operation:** `IOXT` to 100000₈
- **Result:**
  - **A=0:** BUS EXPANDER present
  - **A≠0:** I/O error (device absent)

**Bit Usage:** Not detailed in source code - simple presence test only

**Note:** BUS EXPANDER indicates memory expansion capability and may indicate MPM4 presence.

---

### 2.3 ECCR (Error Correction Register)

**Base Address:** 100115₈ (32781 decimal, 0x800D hex)  
**Address Range:** 100115₈ (single register, accessed via TRR instruction)  
**Device Number:** Not applicable (CPU-internal register)  
**Purpose:** Error Checking and Correction Register for ND-120 OnCpu memory

#### 2.3.1 Register Access

**Note:** ECCR is accessed via **`TRR ECCR`** instruction, not IOX. It's a CPU-internal register.

**Detection Usage:**
```npl
% From PH-P2-OPPSTART.NPL, lines 2415-2416
A:=4; T:=100115; *IOXT; TRA IIC    % Test ECCR device presence
IF A=0 THEN MEMTYPE BONE BMECCR=:MEMTYPE FI
```

**Page-Level Test Usage:**
```npl
% From PH-P2-OPPSTART.NPL, lines 3852-3855
A:=11; *TRR ECCR                   % Write 11₈ to ECCR
0=:X.S0; A:=4; *TRR ECCR; TRR 10  % Write 4₈, then read register 10
X.S0; *TRA IIC
IF A=10 THEN
    % Page has ECCR capability (Local/OnCpu memory)
    T:=KMECCR; A:=CURRPAGE; CALL SMEMTYPE
FI
```

#### 2.3.2 ECCR Register Bit Definitions

**ECCR Register Bits** (from ND-06.026.1 EN, Page 128):

| Bit | Name | Meaning |
|-----|------|---------|
| 0 | **0TS** | Simulate memory error in bit 0 |
| 1 | **15T** | Simulate memory error in bit 15 |
| 2 | **ANY** | Enable parity interrupt on all errors |
| 3 | **DIS** | Disable ECC System and parity interrupt |
| 4 | **6TS** | Simulate memory error in bit 6 |

**Note:** ECCR is accessed via `TRR ECCR` instruction, but since ECCR is on the ND-100 bus, the microprogram performs the equivalent of an `IOXT` to address 100115₈.

#### 2.3.3 ECCR Register Operations

**TRR ECCR Instructions:**

| Operation | Instruction | Value | Binary | Bits Set | Purpose |
|-----------|-------------|-------|--------|----------|---------|
| Write | `A:=11; *TRR ECCR` | 11₈ | `1001` | Bit 0 (0TS), Bit 3 (DIS) | Setup ECCR test: Disable ECC, simulate error in bit 0 |
| Write | `A:=4; *TRR ECCR` | 4₈ | `100` | Bit 2 (ANY) | Trigger ECCR test: Enable parity interrupt on all errors |
| Cache Clear | `TRR 10` | 10₈ | - | - | Clear cache (CCL register) |
| Read IIC | `*TRA IIC` | - | - | - | Read IIC register (10₈ = memory parity error) |

**Test Pattern Sequence:**

1. **Write 11₈ to ECCR:**
   - Bit 0 (0TS): Simulate memory error in bit 0
   - Bit 3 (DIS): Disable ECC System and parity interrupt
   - **Purpose:** Setup test pattern (disable ECC, simulate error)

2. **Write 4₈ to ECCR:**
   - Bit 2 (ANY): Enable parity interrupt on all errors
   - **Purpose:** Enable parity interrupt (triggers ECCR test)

3. **TRR 10 (Cache Clear):**
   - Clear cache
   - **Side Effect:** ECCR hardware processes the test pattern

4. **TRA IIC (Read IIC Register):**
   - Read IIC register into A register
   - **Result:** A=10₈ if ECCR detected parity error (page has ECCR capability)

**Bit Usage:** Not detailed in source code - hardware-specific test sequence

**ECCR Functions:**
- **Error Detection:** Detects single-bit and multi-bit memory errors
- **Error Correction:** Automatically corrects single-bit errors
- **Error Logging:** Records error addresses and status
- **Memory Identification:** Used to identify pages with ECC capability

---

### 2.4 BUSC Devices (MPM4)

**Base Address:** 100200₈ (32768 decimal, 0x8040 hex)  
**Address Formula:** `100200₈ + (NBUSCN × 4)` for NBUSCN 0-17  
**Device Count:** Up to 18 devices  
**Device Numbers:** 20₈ - 37₈  
**Purpose:** MPM4 (Multiport Memory Module 4) controllers

#### 2.4.1 Register Map (Per BUSC Device)

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Purpose |
|--------|----------------|-------------------|----------|--------|---------|
| +0 | Base+0 | Base+0 | **Status/Test** | Read | Device presence test, Read memory limits |
| +1 | Base+1 | Base+1 | **Reserved** | - | Not used in boot code |
| +2 | Base+2 | Base+2 | **Reserved** | - | Not used in boot code |
| +3 | Base+3 | Base+3 | **Control** | Write | Enable read limits |

#### 2.4.2 Status/Test Register (Base+0)

**Device Presence Test:**
```npl
% From PH-P2-OPPSTART.NPL, line 2420
A:=NBUSCN*4+100200=:T; *IOXT; TRA IIC
IF A=0 THEN
    % BUSC device present
ELSE
    % BUSC device absent
FI
```

**Memory Limits Read:**
```npl
% From PH-P2-OPPSTART.NPL, lines 2426-2427
T+3; A:=100; *IOXT      % Enable read limits (write to Base+3)
T-3; *IOXT              % Read limits (read from Base+0)
% Result: A = lower limit (bits 0-7), D = combined limits
```

**Register Definition:**
- **Presence Test:** Read from Base+0, A=0 if present
- **Memory Limits:** Read from Base+0 after enabling Base+3
- **Result Format:** Combined limits in D register (see Section 2.4.4)

**Bit Usage:** Not detailed in source code - returns combined limit values

#### 2.4.3 Control Register (Base+3)

**Enable Read Limits:**
```npl
% From PH-P2-OPPSTART.NPL, line 2426
T+3; A:=100; *IOXT    % Write 100₈ to control register
```

**Register Definition:**
- **Write Operation:** Write `100₈` (64 decimal, 0x40 hex) to enable read limits mode
- **Effect:** Enables reading memory limits from Status/Test register (Base+0)
- **Note:** Must be written before reading limits

**Bit Usage:** Not detailed in source code - control value only

#### 2.4.4 Memory Limit Format

**Limit Register Content (D register after read):**

```
Bits:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
      ┌─────────────────────────────────────────────────────────────┐
      │ Upper Limit (bits 10-15) │ Lower Limit (bits 0-7) │ 0 0 0 0 │
      └─────────────────────────────────────────────────────────────┘
```

**Parsing Algorithm:**
```npl
% From PH-P2-OPPSTART.NPL, line 2428
A=:D/\377 SH 6:=:D SHZ -10 SH 6:=:D
```

**Step-by-Step:**
1. **Save A:** `A=:D` - Save accumulator (contains lower limit bits 0-7)
2. **Extract Lower Limit:** `D/\377 SH 6` - Mask bits 0-7, shift left 6 bits
   - Result: Lower limit in page units (page = 1024 words = 2048 bytes)
3. **Extract Upper Limit:** `D SHZ -10 SH 6` - Shift right 10 bits, then left 6 bits
   - Result: Upper limit in page units

**Example:**
```
If D register = 0x1234 (hex) = 0001100110100 (binary):
- Lower limit bits (0-7): 00110100 = 64₈ = 52 decimal
- Upper limit bits (10-15): 000110 = 6₈ = 6 decimal

After parsing:
- Lower limit: 64₈ << 6 = 6400₈ pages
- Upper limit: 6₈ << 6 = 600₈ pages
```

**Empty Port Detection:**
```npl
IF A><D THEN D-1 ELSE A:=0; D:=0 FI
```
- **If A ≠ D:** Valid memory range exists → Store range A to D-1
- **If A = D:** Empty port (no memory) → Set A=0, D=0

---

## 3. Communication Devices

### 3.1 HDLC Controllers (COM5025)

**Base Address:** HDEV (hardware-configured, varies per device)  
**Register Count:** 18 registers (HDEV+0 through HDEV+17)  
**Purpose:** HDLC/SDLC communication controllers

#### 3.1.1 Complete Register Map

| Offset | Address | Register | Read/Write | Full Name | Purpose |
|:------:|:-------:|:--------|:----------:|-----------|---------|
| +0 | HDEV+0 | **RRDR** | Read | Read Receiver Data Register | Read received character data |
| +1 | HDEV+1 | **WPCR** | Write | Write Parameter Control Register | Configure parameters |
| +2 | HDEV+2 | **RRS** | Read | Read Receiver Status | Receiver status flags |
| +3 | HDEV+3 | **WSAR** | Write | Write Sync/Address Register | Set SYNC/address byte |
| +4 | HDEV+4 | **WCHL** | Write | Write Character Length | Set bits per character (5-8) |
| +5 | HDEV+5 | **WTDR** | Write | Write Transmitter Data Register | Write character to transmit |
| +6 | HDEV+6 | **RTSR** | Read | Read Transmitter Status Register | Transmitter status flags |
| +7 | HDEV+7 | **WTCR** | Write | Write Transmitter Control Register | Transmitter control |
| +10 | HDEV+10 | **RRTS** | Read | Read Receiver Transfer Status | **DMA receiver status** |
| +11 | HDEV+11 | **WRTC** | Write | Write Receiver Transfer Control | **DMA receiver control** |
| +12 | HDEV+12 | **RTTS** | Read | Read Transmitter Transfer Status | **DMA transmitter status** |
| +13 | HDEV+13 | **WTTC** | Write | Write Transmitter Transfer Control | **DMA transmitter control** |
| +14 | HDEV+14 | **RDMA** | Read | Read DMA Address (Least) | Read current DMA address |
| +15 | HDEV+15 | **WDMA** | Write | Write DMA Address (Least) | Set DMA descriptor address |
| +16 | HDEV+16 | **RDCR** | Read | Read DMA Command Register | Read DMA status |
| +17 | HDEV+17 | **WDCR** | Write | Write DMA Command Register + Trigger | Start DMA operation |

#### 3.1.2 RRTS - Read Receiver Transfer Status (IOX+10)

**Critical Register:** Primary status register for DMA receive operations and X.21 interface.

**Bit Map:**

| Bit | Name | Type | Interrupt | Clear Behavior | Description |
|:---:|:-----|:----:|:---------:|:---------------|:------------|
| 0 | **RXD** | Status | Level 13 | Auto | Data Available - character ready |
| 1 | **RXSA** | Status | Level 13 | Auto | Status Available - status info ready |
| 2 | **RXA** | Status | - | Auto | Receiver Active - within frame |
| 3 | **SFR** | Status | - | Read/Clear | Sync/Flag Received |
| 4 | **DMAR** | Trigger | Level 13 | **Instant** | DMA Module Request (always reads as 0) |
| 5 | **SD** | Status | Level 13 | - | Signal Detector (CCITT 109) |
| 6 | **DSR** | Status | Level 13 | - | Data Set Ready (CCITT 107/X.21 I) |
| 7 | **RI** | Status | Level 13 | - | Ring Indicator (CCITT 125) |
| 8 | **BE** | DMA | Level 13 | On Read | Block End - DMA block complete |
| 9 | **FE** | DMA | Level 13 | On Read | Frame End - DMA frame complete |
| 10 | **LE** | DMA | Level 13 | On Read | List End - DMA list complete |
| 11 | **EMTY** | DMA | Level 13 | On Read | List Empty - no buffers available |
| 12-13 | Reserved | - | On Read | Reserved (cleared on read) |
| 13 | **X21D** | Error | Level 13 | **Persistent** | X.21 Data Error |
| 14 | **X21S** | Error | Level 13 | **Persistent** | X.21 Clear Indication |
| 15 | **OR** | Error | Level 13 | **Persistent** | Receiver Overrun |

**SINTRAN Bit Constants:**

| Constant | Value (Octal) | Value (Hex) | Bits | Purpose |
|----------|--------------|-------------|------|---------|
| **EMTY** | 000400 | 0x0800 | 11 | List Empty flag |
| **HX21M** | 060000 | 0x6000 | 13-14 | X.21 Error Mask |
| **HX21S** | 000016 | 0x000E | 1-3 | Receiver State (not X.21!) |

**Critical Processing Logic:**

```npl
% SINTRAN Reception Check
HIINT: T:=HDEV+RRTS; *EXR ST     % Read RRTS
       A=:HASTAT                 % Store in HASTAT
       
       % Check 1: X.21 protocol errors
       IF A/\ HX21M >< 0 THEN    % Test bits 13-14
          IF A BIT HX21S THEN    % Test bits 1-3 (receiver state)
             HASTAT BONE BLDON=:HASTAT  % Terminate cleanly
          FI
       FI
       
       % Check 2: Buffer availability
       IF HASTAT/\"EMTY" >< 0 THEN   % Test bit 11
          0=:ACTSW                   % Stop device
       FI
       
       % Check 3: Data valid
       IF A NBIT 0 OR A/\60000><0 THEN  % No data OR X.21 error
          return; % Drop packet
       FI
```

**Hardware Auto-Clear Behavior:**

- **Cleared IMMEDIATELY (before read completes):**
  - Bit 4 (DMAR): DMA Module Request

- **Cleared AFTER read completes:**
  - Bits 8-12: DMA status (BE, FE, LE, EMTY, Reserved)

- **Persistent (require WRTC device clear):**
  - Bit 13 (X21D): X.21 Data Error
  - Bit 14 (X21S): X.21 Clear Indication
  - Bit 15 (OR): Receiver Overrun

#### 3.1.3 RTTS - Read Transmitter Transfer Status (IOX+12)

**Critical Register:** Primary status register for DMA transmit operations.

**Bit Map:**

| Bit | Name | Type | Interrupt | Clear Behavior | Description |
|:---:|:-----|:----:|:---------:|:---------------|:------------|
| 0 | **TXBE** | Status | Level 12 | Auto | Transmit Buffer Empty |
| 1 | **TXU** | Error | Level 12 | **Persistent** | Transmitter Underrun |
| 2 | **TXA** | Status | - | Auto | Transmitter Active |
| 3 | Reserved | - | - | - | Reserved |
| 4 | **DMAR** | Trigger | Level 12 | **Instant** | DMA Module Request (always reads as 0) |
| 5 | Reserved | - | - | - | Reserved |
| 6 | **RFS** | Status | Level 12 | - | Ready for Sending (CCITT 106) |
| 7 | Reserved | - | - | - | Reserved |
| 8 | **BE** | DMA | Level 12 | On Read | Block End - DMA block complete |
| 9 | **FE** | DMA | Level 12 | On Read | Frame End - DMA frame complete |
| 10 | **LE** | DMA | Level 12 | On Read | List End - DMA list complete |
| 11 | **TRFIN** | DMA | Level 12 | On Read | Transmission Finished |
| 12-14 | Reserved | - | On Read | Reserved (cleared on read) |
| 15 | **ER** | Error | Level 12 | **Persistent** | Illegal Key/Format Error |

**SINTRAN Bit Constants:**

| Constant | Value (Octal) | Value (Hex) | Bits | Purpose |
|----------|--------------|-------------|------|---------|
| **TXUND** | 000002 | 0x0002 | 1 | Transmitter Underrun |
| **SILFO** | 100000 | 0x8000 | 15 | Illegal Format/Key Error |

**Success Detection:**

```npl
% SINTRAN Success Logic
HOINT: T:=HDEV+RTTS; *EXR ST     % Read RTTS
       A=:HASTAT                 % Store in HASTAT
       
       % SUCCESS CHECK - CRITICAL
       IF A/\ "SILFO+TXUND" = 0 THEN    % (status & 0x8002) == 0
          % SUCCESS: Neither illegal format nor underrun
          XRETRY=:RTDYN; A:=0; CALL SADTS  % Clear retry, log success
          0=:ACTSW                         % Mark inactive
          CALL NEXTS                       % Next frame
       ELSE
          % TRANSMISSION ERROR
          XRETRY+1=:XRETRY                 % Increment retry
          IF XRETRY > MAXRETRY THEN
             A:=237; CALL DRERR            % Report error
          ELSE
             CALL RETRANSMIT               % Retry
          FI
       FI
```

**Success Condition:** `(rtts & 0x8002) == 0` (both error bits clear)

#### 3.1.4 WRTC - Write Receiver Transfer Control (IOX+11)

**Purpose:** Control DMA receiver operations and device initialization.

**Control Values:**

| Value (Octal) | Value (Hex) | Purpose |
|:-------------:|:-----------:|:--------|
| 000 | 0x00 | **Device Clear** - reset receiver to idle state |
| 040 | 0x20 | **Maintenance Mode** - diagnostic mode |
| 100 | 0x40 | **Basic Receiver Enable** - normal operation |
| 140 | 0x60 | **Maintenance + Clear** - thorough reset |
| 1734 | 0x3DC | **DMA Mode Enable** - full DMA operation |

**Device Clear Sequence:**

```npl
% SINTRAN X.21 Error Clearing
X21SH: A:=0; T:=X2DHD+XWRTC; *EXR ST     % DEVICE CLEAR
       A:=40; *EXR ST                     % MAINTENANCE MODE
       *AAT 6; EXR ST                     % CLEAR DMA
       A:=0; T:=X2DHD+XWRTC; *EXR ST      % NORMAL MODE
```

**Bit Usage:** Control values are written as whole values, not individual bits

#### 3.1.5 WTTC - Write Transmitter Transfer Control (IOX+13)

**Purpose:** Control DMA transmitter operations.

**Control Values:**

| Value (Octal) | Value (Hex) | Purpose |
|:-------------:|:-----------:|:--------|
| 0 | 0x00 | **Transmitter Off** - disable transmission |
| 1134+CMODI | 0x25C+mode | **DMA Transmit Enable** - start transmission |

**Note:** CMODI is a mode flag (typically 0 or 040 octal).

**Transmitter Start Sequence:**

```npl
% SINTRAN Transmission Start
XHMST: LIINT+DPITPHYS;                   % Calculate DMA address
       T:=HDEV+WDMA; *IOF; EXR ST        % Write DMA address
       A:=2000\/D; T+"WDCR-WDMA"; *EXR ST % Write DMA command (0x400)
       T+"RDCR-WDCR"; X:=-20;*EXR ST     % Verify status
       CALL LTOUT; *JAF *-2; ION         % Timeout check
       1134+CMODI; T:=HDEV+WTTC; *EXR ST % Enable transmission
       1 =: ACTSW                        % Mark active
```

#### 3.1.6 WDMA/WDCR - DMA Control Registers

**WDMA (IOX+15) - Write DMA Address:**

- **Purpose:** Set DMA descriptor list start address (least significant word)
- **Usage:** Write physical address low 16 bits

**WDCR (IOX+17) - Write DMA Command + Trigger:**

**Command Values:**

| Command | Octal | Hex | Purpose |
|---------|:-----:|:---:|:--------|
| **Start Transmitter** | 2000 | 0x0400 | Initiate DMA transmission |
| **Start Receiver** | 1001 | 0x0201 | Initiate DMA reception |
| **Initialize** | 401 | 0x0101 | Reset DMA controller |

**RDCR (IOX+16) - Read DMA Command/Status:**

- **Purpose:** Read current DMA operation status
- **Status Bits:** Bit 15 = DMA Active, Bit 14 = DMA Error

#### 3.1.7 Other HDLC Registers

**RRDR (IOX+0)** - Read Receiver Data:
- Returns last received character
- Used in non-DMA mode

**WTDR (IOX+5)** - Write Transmitter Data:
- Send character to transmit
- Used in non-DMA mode

**RRS (IOX+2)** - Read Receiver Status:
- Low-level receiver status
- Frame sync, parity, etc.

**RTSR (IOX+6)** - Read Transmitter Status:
- Low-level transmitter status
- Buffer state, etc.

**WPCR (IOX+1)** - Write Parameter Control:
- Configure protocol mode
- Set HDLC/SDLC/BiSync mode

**WSAR (IOX+3)** - Write Sync/Address:
- Set synchronization byte
- Set station address

**WCHL (IOX+4)** - Write Character Length:
- Set bits per character (5-8)
- Typically 8 for HDLC

**WTCR (IOX+7)** - Write Transmitter Control:
- Low-level transmitter control
- Enable/disable, mode settings

---

### 3.2 Octobus Interface

**Base Address:** 100400₈ (32768 decimal, 0x8060 hex)  
**Address Range:** 100400₈ - 100407₈ (8 registers)  
**Purpose:** Octobus communication interface (ND-5000 communication)

#### 3.2.1 Register Map

**Input Controller (Base 100400):**

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Description |
|--------|----------------|-------------------|----------|--------|-------------|
| +0 | 100400₈ | 32768 / 0x8060 | **InputReadData** | Read | Read received data from FIFO |
| +1 | 100401₈ | 32769 / 0x8061 | **InputWriteData** | Write | Write data (rarely used for input) |
| +2 | 100402₈ | 32770 / 0x8062 | **InputReadStatus** | Read | Read receiver status |
| +3 | 100403₈ | 32771 / 0x8063 | **InputWriteControl** | Write | Control register (InterruptEnable, Reset) |

**Output Controller (Base 100404):**

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Description |
|--------|----------------|-------------------|----------|--------|-------------|
| +4 | 100404₈ | 32772 / 0x8064 | **OutputReadData** | Read | Read transmitted data (loopback) |
| +5 | 100405₈ | 32773 / 0x8065 | **OutputWriteData** | Write | Write data to transmit FIFO |
| +6 | 100406₈ | 32774 / 0x8066 | **OutputReadStatus** | Read | Read transmitter status |
| +7 | 100407₈ | 32775 / 0x8067 | **OutputWriteControl** | Write | Control register (InterruptEnable, Reset) |

#### 3.2.2 Control Registers (100403₈, 100407₈)

**Reset Operation:**
```npl
% From PH-P2-OPPSTART.NPL, lines 4054-4055
T:=HDEV+DCONT; 20; *IOXT    % CLEAR INTERFACE (DCONT=3, writes to +3)
T+4; *IOXT                   % (writes to +7 with same value 20 = octal for Reset)
```

**Control Values:**
- **20₈ (16 decimal, 0x10 hex):** Reset/Clear interface

**Bit Usage:** Not detailed in source code - control value only

#### 3.2.3 Frame Bit Structure

**Control/Data Frame Format:**

| Bit | Symbol | Name | Description |
|-----|--------|------|-------------|
| 15 | CBIT | Control Bit | 1=Control frame, 0=Data frame |
| 14-8 | - | Reserved/Data | Upper data bits or reserved |
| 7 | EBIT | Enable Bit | 1=Enable processing, 0=Disabled |
| 6-0 | - | Command/Data | Command code or data bits |

**Symbol Definitions:**
- `CBIT = 000017` (octal) = bit number 15
- `EBIT = 000007` (octal) = bit number 7

---

## 4. ND-500 Interface

### 4.1 3022 Interface (PCB 3022)

**Base Address:** HDEV (hardware-configured, typically 500₈ or 600₈)  
**Register Count:** 16 registers (HDEV+0 through HDEV+17)  
**Purpose:** ND-500 bus interface controller

#### 4.1.1 Complete Register Map

| Offset | Address | Register | Read/Write | Full Name | Purpose |
|:------:|:-------:|:--------|:----------:|-----------|---------|
| +0 | HDEV+0 | **RMAR5** | Read | Read Memory Address Register | Read current memory address |
| +1 | HDEV+1 | **LMAR5** | Write | Load Memory Address Register | Set 5MPM base address |
| +2 | HDEV+2 | **RSTA5** | Read | Read Status Register | **Status register (detection!)** |
| +3 | HDEV+3 | **LSTA5** | Write | Load Status Register | Set status bits |
| +4 | HDEV+4 | **RCON5** | Read | Read Control Register | Read control register |
| +5 | HDEV+5 | **LCON5** | Write | Load Control Register | Load control register (enable interrupts) |
| +6 | HDEV+6 | **MCLR5** | Write | Master Clear | Reset interface |
| +7 | HDEV+7 | **TERM5** | Write | Terminate | Terminate ND-500 process |
| +10 | HDEV+10 | **RTAG5** | Read | Read Tag | Read TAG-IN register |
| +11 | HDEV+11 | **LTAG5** | Write | Load Tag | Write TAG-OUT register |
| +12 | HDEV+12 | **RLOW5** | Read | Read Lower Limit | Read lower limit register |
| +13 | HDEV+13 | **LDAT5/LLOW5** | Write | Load Data/Lower Limit | Write data/lower limit |
| +14 | HDEV+14 | **SLOC5** | Read | Status Lock | Read lock status |
| +15 | HDEV+15 | **BITM5/CLXD5** | Write | Bitmask/Clock DATA | Write bitmask or clock data |
| +16 | HDEV+16 | **UNLC5** | Write | Unlock | Unlock operation |
| +17 | HDEV+17 | **RETG5** | Write | Return Gate | Return/end gate |

#### 4.1.2 RSTA5 - Read Status Register (IOX+2)

**Critical Register:** Status register for ND-500 interface detection and status.

**Bit Map:**

```
Bit:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     |C15|        STOPREASON     |CLO|POF|PFA|DMA|ILK|PAG|FIN|BSY| - |INT|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```

| Bit | SINTRAN Symbol | Octal Mask | Hex Mask | Decimal | Meaning | Source |
|-----|----------------|------------|----------|---------|---------|--------|
| 0 | INTE | 000001 | 0x0001 | 1 | Interrupt enabled | NEC-01 3.2 |
| 1 | - | 000002 | 0x0002 | 2 | Not used | NEC-01 3.2 |
| 2 | BUSY | 000004 | 0x0004 | 4 | ND-500 busy | NEC-01 3.2 |
| 3 | FIN | 000010 | 0x0008 | 8 | ND-500 finished | NEC-01 3.2 |
| 4 | 5PAGF | 000020 | 0x0010 | 16 | Inclusive OR of errors | XC-P2-N500.NPL:41 |
| 5 | 5ILOCK | 000040 | 0x0020 | 32 | Interface locked | MP-P2-N500.NPL:2935 |
| 6 | 5DMAER | 000100 | 0x0040 | 64 | DMA/communication error | XC-P2-N500.NPL:42 |
| 7 | 5PFAIL | 000200 | 0x0080 | 128 | Power fault (microprogram) | XC-P2-N500.NPL:43 |
| 8 | 5POWOF | 000400 | 0x0100 | 256 | Power has been off | XC-P2-N500.NPL:44 |
| 9 | 5CLOST | 001000 | 0x0200 | 512 | Microclock stopped | XC-P2-N500.NPL:45 |
| 10-14 | STOPREASON | 037000 | 0x3E00 | 15872 | Stop reason (5 bits) | NEC-01 3.2 |
| 15 | CNTRL15 | 100000 | 0x8000 | 32768 | Control reg bit 15 | NEC-01 3.2 |

**Stop Reason Values (Bits 10-14):**

| Octal | Decimal | Symbol | Full Name | Meaning | When Set |
|-------|---------|--------|-----------|---------|----------|
| 000001 | 1 | MOCAL | MOCALL | Monitor call | ND-500 executed MON instruction |
| 000002 | 2 | TRAPC | TRAPCODE | Trap occurred | Hardware trap (page fault, etc.) |
| 000003 | 3 | 5FMOC | 5FMOCALL | File transfer MON | File I/O monitor call |
| 000101 | 65 | - | (TPSTRA) | N500M RUNN return | MON 407B return from RUNN |

**Status Register Masks:**

| Purpose | Octal | Hex | Binary | Effect |
|---------|-------|-----|--------|--------|
| Clear 5POWOF | 177377 | 0xFEFF | 1111 1110 1111 1111 | ANDs out bit 8 |
| Clear 5POWOF+5PFAIL | 177177 | 0xFE7F | 1111 1110 0111 1111 | ANDs out bits 7-8 |

#### 4.1.3 LCON5 - Load Control Register (IOX+5)

**Purpose:** Control ND-500 interface operations.

**Bit Map:**

```
Bit:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     | - |      OPERATION CODE       |CHN|DMA|TAG|CLR|TST|ACT| - |INT|
     +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```

| Bit | NEC-01 Symbol | Octal Mask | Hex Mask | Decimal | Meaning | Source |
|-----|---------------|------------|----------|---------|---------|--------|
| 0 | INTE | 000001 | 0x0001 | 1 | Enable interrupt from ND-500 | NEC-01 3.1 |
| 1 | - | 000002 | 0x0002 | 2 | Not used | NEC-01 3.1 |
| 2 | ACTV | 000004 | 0x0004 | 4 | Activate ND-500 (locks comm) | NEC-01 3.1 |
| 3 | TEST | 000010 | 0x0008 | 8 | Test mode | NEC-01 3.1 |
| 4 | PCLY | 000020 | 0x0010 | 16 | ND-500 programmed clear | NEC-01 3.1 |
| 5 | DTAG | 000040 | 0x0020 | 32 | Disable TAG-IN when locked | NEC-01 3.1 |
| 6 | DMAERR | 000100 | 0x0040 | 64 | DMA error | NEC-01 3.1 |
| 7 | CMDCH | 000200 | 0x0080 | 128 | Command chaining | NEC-01 3.1 |
| 8-14 | NDOP | 077600 | 0x7F00 | 32512 | Operation code (7 bits) | NEC-01 3.1 |
| 15 | - | 100000 | 0x8000 | 32768 | Not used | NEC-01 3.1 |

**LCON5 Values Written by SINTRAN:**

| Value (Octal) | Value (Hex) | Value (Dec) | Bits Set | NPL Source | Purpose |
|---------------|-------------|-------------|----------|------------|---------|
| 0 | 0x00 | 0 | None | XC-P2-N500.NPL:58 | Clear control |
| 1 | 0x01 | 1 | bit 0 | MP-P2-N500.NPL:3091 | Enable interrupt only |
| 5 | 0x05 | 5 | bits 0,2 | MP-P2-N500.NPL:3086 | **ACTIVATE** |
| 10 | 0x08 | 8 | bit 3 | MP-P2-N500.NPL:3089 | Test mode |
| 40 | 0x20 | 32 | bit 5 | CC-P2-N500.NPL:215 | Disable TAG-IN |

---

## 5. Device Address Summary

### 5.1 Fixed Address Devices

| Device Name | Base Address (Octal) | Base Address (Dec/Hex) | Register Count | Device # | Purpose |
|-------------|---------------------|------------------------|----------------|----------|---------|
| **BIG MPM ERROR LOG** | 750₈ | 488 / 0x1E8 | 4 | 5₈ | MPM3 controller |
| **BUS EXPANDER #1** | 100000₈ | 32768 / 0x8000 | 4 | 10₈ | Memory expansion |
| **ECCR** | 100115₈ | 32781 / 0x800D | 1 | N/A | Error correction |
| **BUSC #0** | 100200₈ | 32768 / 0x8040 | 4 | 20₈ | MPM4 controller |
| **BUSC #1-17** | 100204₈-100304₈ | 32772-32836 / 0x8044-0x8084 | 4 each | 21₈-37₈ | MPM4 controllers |
| **Octobus Input** | 100400₈ | 32768 / 0x8060 | 4 | N/A | Octobus input |
| **Octobus Output** | 100404₈ | 32772 / 0x8064 | 4 | N/A | Octobus output |

### 5.2 Variable Address Devices

| Device Type | Base Address | Register Count | Purpose |
|-------------|--------------|----------------|---------|
| **HDLC Controllers** | HDEV (configurable) | 18 | HDLC/SDLC communication |
| **ND-500 Interface** | HDEV (typically 500₈ or 600₈) | 16 | ND-500 bus interface |
| **PIOC Devices** | Device-specific | Varies | Programmed I/O Controllers |

---

## 6. Register Access Patterns

### 6.1 Device Detection Pattern

```npl
% Standard device detection pattern
A:=200; *TRR IIE; TRA IIC    % Enable IOX error interrupt
T:=device_address; *IOXT; TRA IIC    % Test device
IF A=0 THEN
    % Device present
ELSE
    % Device absent (I/O error)
FI
A:=0; *TRR IIE    % Disable IOX error interrupt
```

### 6.2 Register Read Pattern

```npl
% Read register value
T:=base_address+offset; *EXR ST    % Read register
A=:result_variable    % Store result
```

### 6.3 Register Write Pattern

```npl
% Write register value
A:=value_to_write
T:=base_address+offset; *EXR ST    % Write register
```

### 6.4 Multi-Step Operations

**Example: BUSC Memory Limit Reading:**
```npl
% Step 1: Calculate device address
A:=NBUSCN*4+100200=:T

% Step 2: Test device presence
*IOXT; TRA IIC
IF A=0 THEN
    % Step 3: Enable read limits
    T+3; A:=100; *IOXT
    
    % Step 4: Read limits
    T-3; *IOXT
    
    % Step 5: Parse results
    A=:D/\377 SH 6:=:D SHZ -10 SH 6:=:D
FI
```

---

## 7. Quick Reference Tables

### 7.1 IOX Instruction Summary

| Instruction | Syntax | Purpose |
|-------------|--------|---------|
| **IOX** | `*IOX address` | Direct I/O to fixed address |
| **IOXT** | `T:=address; *IOXT` | I/O Transfer (address in T register) |
| **EXR ST** | `T:=address; *EXR ST` | Execute Register Store (read/write via T and A) |

### 7.2 Result Interpretation

| Result | Meaning | Action |
|--------|---------|--------|
| **A=0** | Device present / Operation successful | Continue processing |
| **A≠0** | I/O error / Device absent | Skip device or handle error |

### 7.3 Interrupt Enable

**Before IOX Operations:**
```npl
A:=200; *TRR IIE; TRA IIC    % Enable IOX error interrupt (200₈)
```

**After IOX Operations:**
```npl
A:=0; *TRR IIE    % Disable IOX error interrupt
```

---

## 8. Related Documentation

- **BUS-EXPANDER-BUSC-REGISTER-REFERENCE.md:** Detailed BUS EXPANDER and BUSC register documentation
- **HDLC Register Reference:** Complete HDLC register bit definitions
- **ND500-IF-USAGE-DEEP-ANALYSIS.md:** ND-500 interface register details
- **MEMORY-TYPE-DETECTION.md:** Memory device detection usage

---

**End of Document**
