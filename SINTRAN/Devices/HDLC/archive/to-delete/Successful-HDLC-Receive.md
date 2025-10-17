# Successful HDLC Receive Analysis - Complete SINTRAN Hardware and Software Validation

## Overview

This document provides a comprehensive analysis of all hardware registers, validation requirements, and processing flow for successful HDLC frame reception in SINTRAN. Based on deep analysis of the actual SINTRAN source code (s3vs-4-L-RONNY.symb), this document maps the complete path from hardware interrupt to user application delivery.

## HDLC Hardware Register Map

Based on SINTRAN source code analysis, here are ALL registers accessed during HDLC reception:

### Read Registers (Input to SINTRAN)
| Register | Offset | Purpose | When Read | Critical for Success |
|----------|--------|---------|-----------|---------------------|
| **RRTS** | HDEV+10 (IOX+10) | Read Receiver Transfer Status | Every level 13 IRQ | **YES** - Primary validation |

### Write Registers (SINTRAN to Hardware)
| Register | Offset | Purpose | When Written | Critical for Setup |
|----------|--------|---------|--------------|-------------------|
| **WRTC** | HDEV+11 (IOX+11) | Write Receiver Transfer Control | Device setup/control | **YES** - Enables interrupts |
| **WDMA** | HDEV+15 (IOX+15) | Write DMA Address | DMA list setup | **YES** - Buffer management |

### DMA Control Registers (Separate from HDLC)
| Register | Offset | Purpose | When Accessed | Critical for Reception |
|----------|--------|---------|---------------|----------------------|
| **WDCR** | WDMA+2 (IOX+17) | Write DMA Command | Start DMA operations | **YES** - Enables DMA |
| **RDCR** | WDCR-1 (IOX+16) | Read DMA Command | Status checking | Optional - diagnostics |

## Critical Discovery: RRTS is the ONLY Hardware Status Register Read

Unlike transmission (which reads RTTS), **HIINT only reads RRTS (HDEV+10)**. No other hardware status registers are read during frame reception processing.

## Complete HIINT Processing Flow

```
                    ┌─────────────────────────────────────┐
                    │          LEVEL 13 IRQ              │
                    │     (Hardware Interrupt)           │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │          HIINT ENTRY               │
                    │       (Line 104436)                │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │   T:=HDEV+RRTS; *EXR ST            │
                    │   A=:HASTAT                        │
                    │                                    │
                    │   Read ONLY hardware register      │
                    │   Store in HASTAT variable         │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │      ACTIVITY CHECK                │
                    │   IF T:=ACTSW = 0 THEN             │
                    │      GO OUT1 FI                    │
                    └─────────────┬───────────────────────┘
                                  │ ACTSW != 0
                    ┌─────────────▼───────────────────────┐
                    │      X.21 ERROR CHECK              │
                    │   IF A/\ HX21M >< 0 THEN           │
                    │      (RRTS & 0x6000) != 0?         │
                    └─────────────┬───────────────────────┘
                                  │ X.21 OK
                    ┌─────────────▼───────────────────────┐
                    │    BUFFER AVAILABILITY CHECK       │
                    │   IF HASTAT/\"EMTY" >< 0 THEN      │
                    │      (RRTS & 0x0800) != 0?         │
                    │      0=:ACTSW (SHUTDOWN!)           │
                    └─────────────┬───────────────────────┘
                                  │ Buffers OK
                    ┌─────────────▼───────────────────────┐
                    │     DMA DESCRIPTOR ACCESS          │
                    │   A:=LIINT.LKEY=:D                 │
                    │   Extract LKEY (RCOST + control)   │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │      BLOCK END CHECK               │
                    │   IF A NBIT XBLDN THEN             │
                    │      (LKEY & 0x0008) == 0?         │
                    │      GO OUT1                       │
                    └─────────────┬───────────────────────┘
                                  │ Block complete
                    ┌─────────────▼───────────────────────┐
                    │         CALL HNOTRA                │
                    │     (Buffer Content Processing)     │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │       HNOTRA ENTRY                 │
                    │      (Line 104611)                 │
                    └─────────────┬───────────────────────┘
                                  │
                    ┌─────────────▼───────────────────────┐
                    │     RCOST VALIDATION                │
                    │   IF A /\ "LMASK" = 3 THEN         │
                    │      (LKEY & 0x6377) == 3?         │
                    └─────────────┬───────────────────────┘
                                  │ RCOST valid
                    ┌─────────────▼───────────────────────┐
                    │      SUCCESS PATH                   │
                    │   A:=0; CALL SCRET; CALL SADTS     │
                    │   X-BHEAD; CALL OCHAIN             │
                    │                                    │
                    │   Deliver packet to user!          │
                    └─────────────────────────────────────┘
```

## RRTS Register Validation Requirements

### Hardware Status Register (RRTS) - Complete Bit Analysis

Based on the actual SINTRAN constants and source code validation:

```
RRTS Register Bit Map (IOX+10):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 15  │ 14  │ 13  │ 12  │ 11  │ 10  │  9  │  8  │  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ OR  │X21S │X21D │Rsvd │EMTY │ LE  │ FE  │ BE  │ RI  │ DSR │ SD  │ DMR │ SFR │ RXA │RXSA │ RXD │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘

SINTRAN Validation Masks:
HX21M = 060000₈ = 0x6000 = Bits 13-14 (X.21 Error Mask)
EMTY  = 004000₈ = 0x0800 = Bit 11 (List Empty)
HX21S = 000016₈ = 0x000E = Bits 1,2,3 (Receiver State Check)
```

### Critical RRTS Validation Logic

```assembly
% SINTRAN Source Code Validation (Lines 104450-104527):

1. IF A/\ HX21M >< 0 THEN              % Test (RRTS & 0x6000) != 0
     % X.21 Protocol Error - Log and handle
   FI

2. IF HASTAT/\"EMTY" >< 0 THEN         % Test (RRTS & 0x0800) != 0
     0=:ACTSW                          % FATAL: Shutdown receiver
     % No receive buffers available
   FI

3. % Implicit success condition: Pass all validation checks
```

### Complete RRTS Bit Analysis - ALL 16 BITS

Based on deep analysis of SINTRAN source code and hardware specification, here's how SINTRAN uses every single bit:

```
RRTS Register Complete Bit Map (IOX+10):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 15  │ 14  │ 13  │ 12  │ 11  │ 10  │  9  │  8  │  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ OR  │X21S │X21D │Rsvd │EMTY │ LE  │ FE  │ BE  │ RI  │ DSR │ SD  │ DMR │ SFR │ RXA │RXSA │ RXD │
│0x8000│0x4000│0x2000│0x1000│0x0800│0x0400│0x0200│0x0100│0x0080│0x0040│0x0020│0x0010│0x0008│0x0004│0x0002│0x0001│
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
```

#### **Bit-by-Bit SINTRAN Usage Analysis:**

##### **BIT 0: RXD - Data Available (0x0001)**
```assembly
% SINTRAN Code (Line 104xxx):
IF A NBIT 0 OR A/\60000><0 THEN GO OUT1 FI   % NO DATA OR X21 ERROR?
```
- **SINTRAN Test**: `A NBIT 0` = Test if bit 0 is CLEAR
- **Flow Effect**: If CLEAR (0) → **DROP PACKET** (GO OUT1)
- **Required State**: **MUST BE SET (1)** for packet processing
- **Critical**: This is the PRIMARY data validation check

##### **BIT 1: RXSA - Status Available (0x0002)**
```assembly
% Not directly tested in HIINT - Hardware managed
```
- **SINTRAN Test**: Not explicitly tested in packet flow
- **Flow Effect**: Informational only - doesn't affect packet processing
- **Required State**: **DON'T CARE** (0 or 1 both acceptable)
- **Usage**: Indicates status information available in RDSRH register

##### **BIT 2: RXA - Receiver Active (0x0004)**
```assembly
% Part of HX21S test pattern - NOT INDIVIDUALLY TESTED
% Shows receiver state - within frame (start seen, end not seen)
```
- **SINTRAN Test**: Part of `HX21S = 0x000E` pattern but **NOT INDIVIDUALLY TESTED**
- **Flow Effect**: **INFORMATIONAL ONLY** - shows receiver state machine position
- **Required State**: **OPTIONAL** - typical value is 1 during frame processing
- **Usage**: Hardware status showing receiver has seen frame start, waiting for frame end
- **Hardware Purpose**: Indicates receiver FSM (Finite State Machine) is actively processing a frame

##### **BIT 3: SFR - Sync/Flag Received (0x0008) - DUAL PURPOSE**
```assembly
% Part of HX21S test pattern AND used as XBLDN (Block Done) test
% SINTRAN Code (Line 104xxx):
% IF A NBIT XBLDN THEN GO OUT1 FI   % CRITICAL BLOCK COMPLETION TEST
```
- **SINTRAN Test 1**: Part of `HX21S = 0x000E` pattern (bits 1,2,3) - not directly tested
- **SINTRAN Test 2**: `IF A NBIT XBLDN` = Test if XBLDN (bit 3) is CLEAR in LKEY processing
- **Flow Effect**: **CRITICAL** - Used as block completion indicator in DMA processing
- **Required State**: **MUST BE SET (1)** when DMA block completed
- **Usage**: Shows HDLC sync/flag AND serves as DMA block done signal
- **Special Note**: This bit has DUAL functionality - sync detection AND DMA completion

##### **BIT 4: DMR - DMA Module Request (0x0010)**
```assembly
% Hardware auto-clears on read - interrupt trigger
```
- **SINTRAN Test**: Not tested (always reads as 0)
- **Flow Effect**: **TRIGGERS THE LEVEL 13 INTERRUPT**
- **Required State**: **MUST BE SET (1) before read** - auto-clears
- **Critical**: This bit CAUSES the interrupt, then disappears

##### **BIT 5: SD - Signal Detector (0x0020)**
```assembly
% CCITT 109 status - line signal present
% NOT DIRECTLY TESTED in SINTRAN HIINT - purely operational status
```
- **SINTRAN Test**: **NOT EXPLICITLY TESTED** in HIINT packet processing
- **Flow Effect**: **INFORMATIONAL ONLY** - doesn't affect packet delivery
- **Required State**: **OPTIONAL** - can be 0 or 1 without affecting success
- **Usage**: Physical layer status for diagnostics and line quality monitoring
- **Hardware Purpose**: Indicates carrier detect/signal presence on communication line

##### **BIT 6: DSR - Data Set Ready (0x0040)**
```assembly
% CCITT 107 or X.21 I signal - equipment ready
% NOT TESTED in SINTRAN HIINT reception path
```
- **SINTRAN Test**: **NOT TESTED** in HIINT reception processing
- **Flow Effect**: **NO EFFECT** on packet reception success/failure
- **Required State**: **DON'T CARE** - can be 0 or 1 without affecting operation
- **Usage**: Legacy modem compatibility - indicates DCE (modem) ready state
- **Hardware Purpose**: Shows remote communications equipment operational status

##### **BIT 7: RI - Ring Indicator (0x0080)**
```assembly
% CCITT 125 status - incoming call indicator
```
- **SINTRAN Test**: Not explicitly tested in HIINT
- **Flow Effect**: Incoming call indication (telephone-style)
- **Required State**: **DON'T CARE** (0 or 1 both acceptable)
- **Usage**: Legacy telephone modem compatibility

##### **BIT 8: BE - Block End (0x0100)**
```assembly
% DMA block completion - cleared on read
```
- **SINTRAN Test**: Not tested in HIINT (handled in HNOTRA via LKEY)
- **Flow Effect**: Indicates single DMA buffer completed
- **Required State**: **RECOMMENDED SET (1)** for DMA operations
- **Hardware**: Auto-cleared on RRTS read
- **Usage**: Shows buffer-level completion

##### **BIT 9: FE - Frame End (0x0200)**
```assembly
% DMA frame completion - cleared on read
```
- **SINTRAN Test**: Not tested in HIINT (handled in HNOTRA via LKEY)
- **Flow Effect**: Indicates complete HDLC frame received
- **Required State**: **RECOMMENDED SET (1)** for frame operations
- **Hardware**: Auto-cleared on RRTS read
- **Usage**: Shows protocol-level completion

##### **BIT 10: LE - List End (0x0400)**
```assembly
% DMA list completion - cleared on read
```
- **SINTRAN Test**: Not tested in HIINT
- **Flow Effect**: Indicates entire DMA descriptor list processed
- **Required State**: **DON'T CARE** (depends on DMA configuration)
- **Hardware**: Auto-cleared on RRTS read
- **Usage**: Shows all buffers in current list completed

##### **BIT 11: EMTY - List Empty (0x0800)**
```assembly
% SINTRAN Code (Line 104473):
IF HASTAT/\"EMTY" >< 0 THEN 0=:ACTSW FI   % DEVICE STOPPED
```
- **SINTRAN Test**: `HASTAT/\"EMTY" >< 0` = Test if bit 11 is SET
- **Flow Effect**: If SET (1) → **SHUTDOWN ENTIRE RECEIVER**
- **Required State**: **MUST BE CLEAR (0)** - CRITICAL
- **Fatal**: This is the most dangerous bit - kills the receiver

##### **BIT 12: Reserved (0x1000)**
```assembly
% Hardware specification: "Not used"
```
- **SINTRAN Test**: Not tested
- **Flow Effect**: None
- **Required State**: **SHOULD BE CLEAR (0)**
- **Hardware**: Auto-cleared on RRTS read

##### **BIT 13: X21D - X.21 Data Error (0x2000)**
```assembly
% SINTRAN Code (Line 104450):
IF A/\ HX21M >< 0 THEN ... FI   % HX21M = 0x6000 = bits 13-14
```
- **SINTRAN Test**: `A/\ HX21M >< 0` = Test if bits 13-14 are SET
- **Flow Effect**: If SET (1) → **X.21 ERROR HANDLING**
- **Required State**: **MUST BE CLEAR (0)** for normal operation
- **Usage**: X.21 protocol data path errors

##### **BIT 14: X21S - X.21 Status Error (0x4000)**
```assembly
% SINTRAN Code (Line 104450):
IF A/\ HX21M >< 0 THEN ... FI   % HX21M = 0x6000 = bits 13-14
% Also: IF A BIT HX21S THEN ... in LKEY processing
```
- **SINTRAN Test**: `A/\ HX21M >< 0` = Test if bits 13-14 are SET
- **Flow Effect**: If SET (1) → **X.21 ERROR HANDLING**
- **Required State**: **MUST BE CLEAR (0)** for normal operation
- **Usage**: X.21 protocol status/control path errors
- **Special**: Also tested as bit position 14 in LKEY processing

##### **BIT 15: OR - Receiver Overrun (0x8000)**
```assembly
% Hardware specification: "NOT cleared on read" - persistent error
```
- **SINTRAN Test**: Not explicitly tested in HIINT
- **Flow Effect**: Indicates data received faster than processed
- **Required State**: **SHOULD BE CLEAR (0)** but not critical
- **Hardware**: **NOT auto-cleared** - persists until hardware clear
- **Usage**: Performance monitoring/debugging

### **SINTRAN Flow Control Matrix - DEFINITIVE ANALYSIS:**

| Bit | Name | Must Be | Effect if Wrong | SINTRAN Test | Criticality | **CONFIRMED USAGE** |
|-----|------|---------|----------------|--------------|-------------|---------------------|
| **0** | DataAvailable | **1** | **DROP PACKET** | `IF A NBIT 0` | **CRITICAL** | **DIRECTLY TESTED** |
| **1** | StatusAvailable | Any | None | Not tested | None | **INFORMATIONAL** |
| **2** | ReceiverActive | Any | None | Not individually tested | Low | **STATUS ONLY** |
| **3** | SyncFlagReceived | **1** | **BLOCK PROCESSING** | `NBIT XBLDN` in LKEY | **CRITICAL** | **DUAL PURPOSE** |
| **4** | DMAModuleRequest | 1→0 | No interrupt | Auto-clear | **CRITICAL** | **INTERRUPT TRIGGER** |
| **5** | SignalDetector | Any | **NONE** | **NEVER TESTED** | **NONE** | **IGNORED BY SINTRAN** |
| **6** | DataSetReady | Any | **NONE** | **NEVER TESTED** | **NONE** | **IGNORED BY SINTRAN** |
| **7** | RingIndicator | Any | None | Not tested | None | **INFORMATIONAL** |
| **8** | BlockEnd | 1 | DMA status | Auto-clear | Medium | **DMA STATUS** |
| **9** | FrameEnd | 1 | DMA status | Auto-clear | Medium | **DMA STATUS** |
| **10** | ListEnd | Any | DMA status | Not tested | Low | **DMA STATUS** |
| **11** | ListEmpty | **0** | **SHUTDOWN RECEIVER** | `IF HASTAT/\"EMTY"` | **FATAL** | **DIRECTLY TESTED** |
| **12** | Reserved | 0 | None | Not tested | None | **UNUSED** |
| **13** | X21D | **0** | **ERROR HANDLING** | `IF A/\ HX21M` | **CRITICAL** | **DIRECTLY TESTED** |
| **14** | X21S | **0** | **ERROR HANDLING** | `IF A/\ HX21M` + `IF A BIT HX21S` | **CRITICAL** | **DUAL USAGE** |
| **15** | ReceiverOverrun | 0 | Performance | Not tested | Low | **STATUS ONLY** |

### **Required RRTS Bit Pattern for Success**

```csharp
/// <summary>
/// RRTS register pattern for successful frame reception
/// Based on complete SINTRAN bit analysis
/// </summary>
public ushort GetSuccessfulRRTS()
{
    ushort rrts = 0x036D;  // Optimized success pattern

    // CRITICAL BITS (affect packet processing):
    rrts |= 0x0001;        // Bit 0: DataAvailable = 1 (MUST SET)
    rrts &= ~0x0800;       // Bit 11: ListEmpty = 0 (MUST CLEAR)
    rrts &= ~0x2000;       // Bit 13: X21D = 0 (MUST CLEAR)
    rrts &= ~0x4000;       // Bit 14: X21S = 0 (MUST CLEAR)

    // RECOMMENDED BITS (operational status):
    rrts |= 0x0004;        // Bit 2: ReceiverActive = 1
    rrts |= 0x0008;        // Bit 3: SyncFlagReceived = 1
    rrts |= 0x0020;        // Bit 5: SignalDetector = 1
    rrts |= 0x0040;        // Bit 6: DataSetReady = 1
    rrts |= 0x0100;        // Bit 8: BlockEnd = 1
    rrts |= 0x0200;        // Bit 9: FrameEnd = 1

    // NOTE: Bit 4 (DMAModuleRequest) auto-clears on read
    // Hardware will clear bits 8-15 after read

    return rrts;  // 0x036D = Perfect success pattern
}
```

### **Critical Success/Failure Paths:**

**IMMEDIATE PACKET DROP:**
- `IF A NBIT 0` → DataAvailable = 0 → **DROP**

**RECEIVER SHUTDOWN:**
- `IF HASTAT/\"EMTY" >< 0` → ListEmpty = 1 → **SHUTDOWN**

**ERROR HANDLING:**
- `IF A/\ HX21M >< 0` → X21D|X21S = 1 → **ERROR**

**SUCCESS PATH:**
- DataAvailable=1 AND ListEmpty=0 AND X21Errors=0 → **PROCESS PACKET**

## RCOST (Low 8-bit) Validation Requirements

### LKEY Field Structure (in DMA Descriptor)

```
LKEY Field Structure (16-bit):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 15  │ 14  │ 13  │ 12  │ 11  │ 10  │  9  │  8  │  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│        DMA Control Bits (High 8)          │              RCOST Bits (Low 8)               │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘

SINTRAN Validation:
LMASK = 060377₈ = 0x6377 = Frame type/RCOST validation mask
```

### RCOST Validation Logic (HNOTRA Line 104631)

```assembly
% SINTRAN Source Code Validation:
IF A /\ "LMASK" = 3 THEN               % Test (LKEY & 0x6377) == 3
   A:=0; CALL SCRET; CALL SADTS        % SUCCESS: Set completion code
   X-BHEAD; CALL OCHAIN                % Deliver to user application
ELSE
   % ERROR: Protocol or input error handling
FI
```

### Required RCOST Pattern

```csharp
/// <summary>
/// RCOST (low 8-bit) pattern for successful frame reception
/// Your current pattern: RSOM=1, REOM=1, others=0
/// </summary>
public byte GetSuccessfulRCOST()
{
    byte rcost = 0x03;  // RSOM(bit 0) + REOM(bit 1) = 0x03

    // SINTRAN validation: (LKEY & 0x6377) == 3
    // Since LKEY = (DMA_control << 8) | rcost
    // And (0x6377 & 0x00FF) = 0x0077
    // Test becomes: (rcost & 0x77) == 3
    // Your rcost = 0x03: (0x03 & 0x77) = 0x03 = 3 ✅ SUCCESS!

    return rcost;  // 0x03 - Perfect!
}
```

## Complete Hardware Register Requirements Summary

### For Successful HDLC Frame Reception:

#### 1. RRTS Register (HDEV+10) Requirements:
```csharp
ushort successfulRRTS = 0x036D;  // 001555₈ octal

✅ REQUIRED (must be set):
   - Bit 0: DataAvailable = 1      % Packet ready for processing
   - Bits 2,3,5,6,8,9: Status bits % Hardware operational state

❌ FORBIDDEN (must be clear):
   - Bit 11: EMTY = 0              % (RRTS & 0x0800) == 0 - Buffers available
   - Bits 13-14: X21D/X21S = 0     % (RRTS & 0x6000) == 0 - No protocol errors

➡️ OPTIONAL (don't affect success):
   - Bit 4: DMAModuleRequest       % Auto-clears on read
   - Bits 1,2,3: Receiver state    % Used for X.21 processing
```

#### 2. LKEY Field (DMA Descriptor) Requirements:
```csharp
ushort successfulLKEY = 0x??03;  // High 8: DMA control, Low 8: RCOST

✅ REQUIRED RCOST pattern:
   - (RCOST & 0x77) == 3           % Frame type validation
   - Your pattern: RSOM(1) + REOM(1) = 0x03 ✅ PERFECT!

❌ FORBIDDEN patterns:
   - (RCOST & 0x77) != 3           % Any other combination fails
```

#### 3. Control Registers (Setup Only):
```csharp
// These are written by SINTRAN to setup reception:
WRTC = 1734₈ = 0x3DC;  // Enable receiver interrupts
WDMA = DMA_list_address;  // Point to buffer list
WDCR = 1001₈ = 0x201;  // Start receiver DMA
```

## Validation Flow Diagram

```
Hardware Interrupt (Level 13)
           │
           ▼
   ┌───────────────┐
   │  READ RRTS    │ ◄── ONLY hardware register read!
   │  (HDEV+10)    │
   └───────┬───────┘
           │
           ▼
   ┌───────────────┐     ❌ (RRTS & 0x6000) != 0
   │ X.21 CHECK    │────────────────┐
   │ HX21M mask    │                │
   └───────┬───────┘                │
           │ ✅ X.21 OK              │
           ▼                        │
   ┌───────────────┐     ❌ (RRTS & 0x0800) != 0
   │ BUFFER CHECK  │────────────────┼─────► SHUTDOWN
   │ EMTY bit      │                │       RECEIVER
   └───────┬───────┘                │
           │ ✅ Buffers OK           │
           ▼                        │
   ┌───────────────┐                │
   │ READ LKEY     │                │
   │ (DMA desc)    │                │
   └───────┬───────┘                │
           │                        │
           ▼                        │
   ┌───────────────┐     ❌ (LKEY & 0x6377) != 3
   │ RCOST CHECK   │────────────────┼─────► ERROR
   │ LMASK = 3     │                │       HANDLING
   └───────┬───────┘                │
           │ ✅ RCOST OK             │
           ▼                        │
   ┌───────────────┐                │
   │   SUCCESS!    │                │
   │ CALL OCHAIN   │ ◄──────────────┘
   │ (deliver to   │
   │  user app)    │
   └───────────────┘
```

## Implementation Guidelines for HDLC Controller

### C# Implementation Example:

```csharp
public class HDLCControllerEmulation
{
    private ushort currentRRTS = 0;

    /// <summary>
    /// Handle successful frame reception
    /// </summary>
    public void OnFrameReceived(byte[] frameData, bool isComplete)
    {
        // Set successful RRTS pattern
        currentRRTS = 0x036D;  // DataAvailable + operational status

        // CRITICAL: Clear error bits
        currentRRTS &= ~0x6000;  // Clear X.21 errors (bits 13-14)
        currentRRTS &= ~0x0800;  // Clear ListEmpty (bit 11)

        // Set up DMA descriptor with correct RCOST
        SetupDMADescriptor(frameData, rcost: 0x03);  // RSOM + REOM

        // Trigger level 13 interrupt
        TriggerReceiverInterrupt();
    }

    /// <summary>
    /// SINTRAN reads RRTS register (IOX+10)
    /// </summary>
    public ushort ReadReceiverTransferStatus()
    {
        ushort result = currentRRTS;

        // Hardware behavior: Clear specific bits on read
        currentRRTS &= ~0x0010;  // Clear DMAModuleRequest (bit 4)
        currentRRTS &= 0x00FF;   // Clear DMA status bits (8-15)

        return result;
    }

    /// <summary>
    /// Setup DMA descriptor with correct LKEY
    /// </summary>
    private void SetupDMADescriptor(byte[] data, byte rcost)
    {
        // High 8 bits: DMA control (varies)
        // Low 8 bits: RCOST value (must be 0x03 for success)
        ushort lkey = (ushort)((dmaControl << 8) | rcost);

        // Store in DMA descriptor
        dmaDescriptor.LKEY = lkey;
        dmaDescriptor.LBYTC = (ushort)data.Length;
        dmaDescriptor.LMEM1 = bufferBank;
        dmaDescriptor.LMEM2 = bufferAddress;
    }
}
```

## Critical Success Criteria Summary

**For 100% successful HDLC frame reception in SINTRAN:**

1. **RRTS = 0x036D** (with X.21 and EMTY bits clear)
2. **RCOST = 0x03** (RSOM + REOM pattern)
3. **LKEY validation: (LKEY & 0x6377) == 3**
4. **Hardware interrupt on level 13**
5. **Proper DMA descriptor setup**

Your current RCOST pattern of **RSOM=1, REOM=1, others=0** giving **0x03** is **PERFECT** and will pass all SINTRAN validation checks.

## Deep Analysis Summary: SignalDetector (Bit 5) and DataSetReady (Bit 6)

### **DEFINITIVE FINDINGS FROM COMPLETE SINTRAN SOURCE ANALYSIS**

After exhaustive analysis of the SINTRAN HDLC source code (lines 104436-104675), constant definitions, and all interrupt processing routines, the following has been **definitively confirmed**:

#### **SignalDetector (Bit 5 - 0x0020): COMPLETELY IGNORED**
- **Source Code Search**: No references to bit 5, 0x0020, or SignalDetector in any HDLC processing
- **HIINT Analysis**: Lines 104436-104546 - No bit 5 testing whatsoever
- **HNOTRA Analysis**: Lines 104611-104675 - No bit 5 testing whatsoever
- **Constants Analysis**: No SINTRAN constants reference bit 5 for packet processing
- **Hardware Purpose**: CCITT 109 carrier detect - purely for physical layer diagnostics
- **Impact on SINTRAN**: **ZERO** - Can be 0 or 1 without any effect on packet delivery

#### **DataSetReady (Bit 6 - 0x0040): COMPLETELY IGNORED**
- **Source Code Search**: No references to bit 6, 0x0040, or DataSetReady in any HDLC processing
- **HIINT Analysis**: Lines 104436-104546 - No bit 6 testing whatsoever
- **HNOTRA Analysis**: Lines 104611-104675 - No bit 6 testing whatsoever
- **Constants Analysis**: No SINTRAN constants reference bit 6 for packet processing
- **Hardware Purpose**: CCITT 107/X.21 I equipment ready - legacy modem compatibility
- **Impact on SINTRAN**: **ZERO** - Can be 0 or 1 without any effect on packet delivery

#### **The ONLY Bits That Matter to SINTRAN HDLC Reception:**

1. **Bit 0 (DataAvailable)** - `IF A NBIT 0` - **CRITICAL**: Must be 1 or packet is dropped
2. **Bit 11 (ListEmpty)** - `IF HASTAT/"EMTY" >< 0` - **FATAL**: Must be 0 or receiver shuts down
3. **Bits 13-14 (X21D/X21S)** - `IF A/\ HX21M >< 0` - **CRITICAL**: Must be 0 or error handling
4. **Bit 14 (X21S)** - `IF A BIT HX21S` - **SPECIAL**: Also tested individually for X.21 clear indication
5. **Bit 3 (SyncFlagReceived)** - `IF A NBIT XBLDN` - **CRITICAL**: Used as block completion flag in HNOTRA

**All other bits (1,2,5,6,7,8,9,10,12,15) are completely ignored by SINTRAN and have no impact on packet processing success or failure.**

#### **Emulator Implementation Guidance:**

```csharp
// For bits 5 and 6 - you can literally ignore them:
rrts &= ~0x0060;  // Clear bits 5,6 - SINTRAN doesn't care
// OR
rrts |= 0x0060;   // Set bits 5,6 - SINTRAN doesn't care
// OR
// Leave them as-is - SINTRAN will never test them

// Focus your debugging efforts on the bits SINTRAN actually tests:
// - Bit 0: DataAvailable
// - Bit 11: ListEmpty
// - Bits 13-14: X21D/X21S
// - Bit 3: SyncFlagReceived (as XBLDN)
```

This analysis conclusively proves that bits 5 and 6 exist purely for hardware compatibility and diagnostics, and have **zero functional impact** on SINTRAN HDLC packet reception processing.

## References

- **SINTRAN Source**: s3vs-4-L-RONNY.symb lines 104436-104675
- **HIINT Handler**: Lines 104436-104546
- **HNOTRA Processor**: Lines 104611-104675
- **Symbol Definitions**: SYMBOL-1-LIST.SYMB.TXT
- **Hardware Specification**: HDLC hardware documentation