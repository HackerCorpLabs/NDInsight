## Deep Analysis: HIINT_Deep_Analysis.md

# HIINT Deep Analysis - Complete Receiver Interrupt Handler Flow

## Overview
HIINT is the SINTRAN receiver interrupt handler (Line 104436) that processes all incoming HDLC frames. It performs critical status validation, buffer management, and packet processing with sophisticated error handling and state tracking.

## Critical Constants and Variables (from SYMBOL-1-LIST.SYMB.TXT)

### Memory Variables (Read/Write)
```assembly
% Core Status Variables:
HASTA = 000076  % HASTAT - Hardware status storage (16-bit)
ACTSW = 000074  % Activity switch: 0=inactive, 1=active
```

### Status Bit Constants (Read-Only)
```assembly
% Receiver Status Detection Constants:
EMTY  = 004000  % 0x0800, bit 11 - List Empty (No Buffers)
HX21M = 060000  % 0x6000, bits 13-14 - X.21 Error Mask
HX21S = 000016  % 0x000E, bits 1,2,3 - Receiver State Check
BLDON = 000010  % 0x0008, bit 3 - Block Done Flag
ERB   = 001000  % 0x0200, bit 9 - Error Block Indicator

% DMA Descriptor Control (BREAKTHROUGH DISCOVERY):
% Receiver uses different LKEY values than transmitter
% Bits 10-8: Block status (Empty=010, Full=011)
% Bits 7-0:  COM5025 register values for frame detection
```

### Counters (Not Found in Symbol Tables - Likely Local Variables)
```assembly
% Diagnostic Counters:
T9     = ?      % Dummy interrupt counter (HIINT specific)
STPCNT = ?      % Stop counter for receiver buffer exhaustion
```

## BREAKTHROUGH: DMA KEY Field Contains COM5025 Register Values

### Revolutionary Discovery for Receiver Operations

Just as with transmission, the receiver DMA descriptor LKEY field contains **actual COM5025 chip register values** in its low 8 bits. However, for reception, these control **frame detection** rather than frame generation.

```assembly
% Receiver DMA Descriptor LKEY Structure:
% Bits 15-8: Block control (Empty=010, Full=011, etc.)
% Bits 7-0:  COM5025 RSOM/REOM detection flags and control bits

% Receiver LKEY values (expected patterns):
% Empty block ready for reception: 001000₈ (Empty block + detection flags)
% Full block with complete frame: 001403₈ (Full block + RSOM + REOM)
% Full block with frame start: 001401₈ (Full block + RSOM only)
% Full block with frame end: 001402₈ (Full block + REOM only)
```

## WRTC Interrupt Enable Control - Critical Understanding

### How HIINT Gets Called - Interrupt Enable Analysis

HIINT only executes when specific RRTS status bits generate hardware interrupts. This is controlled by **WRTC (Write Receiver Transfer Control)** register values that SINTRAN sets to enable/disable interrupt generation for different status conditions.

### WRTC Control Values Found in SINTRAN Source

Based on the SINTRAN source analysis, these are the key WRTC values that control when HIINT is called:

#### 1. Basic Receiver Clear (WRTC = 100 octal = 0x40)
```assembly
% Used during device shutdown/clear:
A:=100; T:=HDEV+WRTC; *EXR ST                    % Clear receiver, minimal interrupts
```
**Interrupt Enable Pattern:**
- **Value**: 100 (octal) = 64 (decimal) = 0x40
- **Purpose**: Basic receiver enable with minimal interrupt sources
- **Enables**: Essential error interrupts only (buffer errors, protocol violations)
- **COM5025 Integration**: Hardware reads LKEY values and writes to COM5025 for frame detection
- **Context**: Used during cleanup/shutdown operations

#### 2. Maintenance Mode (WRTC = 140 octal = 0x60)  
```assembly
% Used during maintenance operations:
IF A = MAMOD THEN A:=140 ELSE A:=100 FI          % Maintenance or normal mode
A=:MAINT; T:=HDEV+WRTC; *EXR ST                  % Set maintenance mode
```
**Interrupt Enable Pattern:**
- **Value**: 140 (octal) = 96 (decimal) = 0x60
- **Purpose**: Maintenance mode with diagnostic interrupts
- **Enables**: Additional diagnostic/test interrupts beyond normal operation
- **Context**: Used during hardware testing and calibration

#### 3. Full DMA Reception Mode (WRTC = 1734 octal = 0x3DC)
```assembly
% PRIMARY operational mode for packet reception:
A:="1734"\/MAINT/\HXDOK                          % Combine with maintenance flags
T:=HDEV+WRTC; *EXR ST                            % Enable full reception
```
**Interrupt Enable Pattern:**
- **Value**: 1734 (octal) = 988 (decimal) = 0x3DC
- **Purpose**: **Full DMA receiver mode with comprehensive interrupt enables**
- **Enables**: ALL packet reception interrupts including:
  - **DataAvailable (bit 0)** - Normal packet completion
  - **ListEmpty (bit 11)** - Buffer exhaustion 
  - **X21D/X21S (bits 13-14)** - Protocol errors
  - **DMA status changes (bits 8-10)** - Block/frame/list completion
- **Context**: **Primary mode used during active packet reception**

#### 4. Combined Control Logic
```assembly
% SINTRAN combines WRTC value with flags:
HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST              % Clear old state
1734\/MAINT/\HXDOK; T:=HDEV+WRTC; *EXR ST        % Set full mode with flags

% Where:
HXDOK = Hardware OK flags (device-specific enables)
MAINT = Maintenance mode flags (100 or 140)
```

### WRTC Bit Analysis for Interrupt Generation

The WRTC value **1734 (octal) = 0x3DC** breaks down as:

```
Binary:   0011 1101 1100
Hex:      0x3DC  
Decimal:  988
Octal:    1734

Bit Pattern:
15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 0  0  1  1  1  1  0  1  1  1  0  0  0  0  0  0
                ^  ^  ^  ^
             Enables for bits:
             - Bit 13-14: X.21 error interrupt enables
             - Bit 11: Buffer empty interrupt enable  
             - Bit 10: List end interrupt enable
             - Bit 9: Frame end interrupt enable
             - Bit 8: Block end interrupt enable
```

### Critical Interrupt Enable Logic

For HIINT to be called, the corresponding **WRTC enable bit must be set** AND the **RRTS status bit must become active**:

#### Normal Packet Reception Flow:
```assembly
% 1. SINTRAN sets WRTC = 1734 (enables multiple interrupt sources)
1734\/MAINT/\HXDOK; T:=HDEV+WRTC; *EXR ST

% 2. Hardware sets RRTS bits when conditions occur:
%    - DataAvailable (bit 0) when packet received
%    - ListEmpty (bit 11) when no more buffers  
%    - X21D/X21S (bits 13-14) when protocol errors occur

% 3. Interrupt generated ONLY if both:
%    - WRTC enable bit is set (interrupt permission)
%    - RRTS status bit becomes active (condition detected)

% 4. HIINT called with RRTS status available to read
HIINT: T:=HDEV+RRTS; *EXR ST                     % Read status that triggered interrupt
```

### WRTC Configuration Sequence in SINTRAN

#### Device Start Sequence:
```assembly
ZSTARC: IF ACTSW = 0 THEN                        % If device not active
           % Step 1: Clear old state
           HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST  % Clear garbage (value = MAINT & HXDOK)
           
           % Step 2: Start DMA  
           LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST % Set DMA address
           A:=1001; T+"WDCR-WDMA"; *EXR ST       % Start receiver DMA
           
           % Step 3: Enable full reception mode
           1734\/MAINT/\HXDOK; T:=HDEV+WRTC; *EXR ST % Enable all receiver interrupts
           
           1=:ACTSW                               % Mark device active
        FI
```

### Impact on C# HDLC Emulator

Your C# HDLC controller emulator needs to:

1. **Track WRTC register writes** to understand which interrupt sources are enabled
2. **Only generate interrupts for enabled conditions:**
   ```csharp
   // Example interrupt logic:
   ushort wrtcValue = currentWRTCRegister;  // Value SINTRAN last wrote
   ushort rrtsStatus = currentReceiverStatus;
   
   // Check each potential interrupt source:
   if ((wrtcValue & 0x0001) && (rrtsStatus & 0x0001)) {
       // DataAvailable interrupt enabled and data is available
       TriggerReceiverInterrupt();
   }
   
   if ((wrtcValue & 0x0800) && (rrtsStatus & 0x0800)) {
       // ListEmpty interrupt enabled and no buffers
       TriggerReceiverInterrupt();
   }
   
   if ((wrtcValue & 0x6000) && (rrtsStatus & 0x6000)) {
       // X.21 error interrupts enabled and error occurred
       TriggerReceiverInterrupt();
   }
   ```

3. **Understand the WRTC patterns:**
   - **WRTC = 100**: Minimal interrupts (cleanup/shutdown)
   - **WRTC = 140**: Maintenance mode interrupts  
   - **WRTC = 1734**: Full operational interrupts (normal reception)

### WRTC/HIINT Relationship Summary

| WRTC Value | Context | Interrupt Sources | HIINT Called When |
|
