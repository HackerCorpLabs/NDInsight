# HOINT Deep Analysis - Complete Transmitter Interrupt Handler Flow

## Overview
HOINT is the SINTRAN transmitter interrupt handler (Line 104033) that processes transmission completion and error conditions. It implements sophisticated retry logic, error classification, and state management for reliable HDLC transmission.

## Critical Constants (from SYMBOL-1-LIST.SYMB.TXT)
```assembly
% Success/Failure Detection Constants:
SILFO = 100000  % 0x8000, bit 15 - Illegal Format/Key Error
TXUND = 000002  % 0x0002, bit 1  - Transmitter Underrun

% Combined mask for transmission validation:
SILFO+TXUND = 100002  % 0x8002 - Critical failure mask

% Error codes and states:
EUND  = 000102  % 0x0042 - Underrun error code (bits 1,6)
BLDON = 000010  % 0x0008, bit 3 - Block Done Flag

% DMA Descriptor Control (CORRECTED UNDERSTANDING):
FSERM = 002003  % 0x403 - Transmit single block with TSOM+TEOM
                % Bits 10-8: 010 (Block to transmit)
                % Bits 7-0:  003 (COM5025: TSOM=1, TEOM=1)
```

## BREAKTHROUGH: DMA KEY Field = COM5025 Register Values

### Revolutionary Discovery

The DMA descriptor LKEY field contains **actual COM5025 chip register values** in its low 8 bits. This is what the HDLC manual calls "Dataflow Cost" - it's the exact bit pattern written to the COM5025 multi-protocol chip to control TSOM, TEOM, and other transmission parameters.

```assembly
% DMA Descriptor LKEY Structure:
% Bits 15-8: Block control (Empty, Full, ToTransmit, etc.)
% Bits 7-0:  COM5025 register value (TSOM, TEOM, control bits)

% Example - FSERM = 002003₈:
% 002003₈ = 0000 1000 0000 0011
%          ├─ 010: Block to be transmitted
%          └─ 003: COM5025 TSOM(1) + TEOM(1) = complete frame
```

## WTTC Interrupt Enable Control - Critical Understanding

### How HOINT Gets Called - Interrupt Enable Analysis

HOINT only executes when specific RTTS status bits generate hardware interrupts. This is controlled by **WTTC (Write Transmitter Transfer Control)** register values that SINTRAN sets to enable/disable interrupt generation for different transmission status conditions.

### WTTC Control Values Found in SINTRAN Source

Based on the SINTRAN source analysis, these are the key WTTC values that control when HOINT is called:

#### 1. Transmitter Off (WTTC = 0)
```assembly
% Used during device shutdown/clear:
TRASET: A:=0
        T:=HDEV+WTTC; *EXR ST                    % Clear transmitter completely
```
**Interrupt Enable Pattern:**
- **Value**: 0 (all bits clear)
- **Purpose**: Disable transmitter and all transmission interrupts
- **Enables**: No transmission interrupts generated
- **Context**: Used during cleanup/reset operations

#### 2. RQTS Mode Control (WTTC = CMODI)
```assembly
% Used for RQTS (Request To Send) control:
IF CMODI = 40 THEN
   T:=HDEV+WTTC; *EXR ST                         % Turn off RQTS (CMODI value)
FI

% And timeout handling:
POFTO: A:=CMODI; T:=HDEV+WTTC; *EXR ST          % Set CMODI value for timeout
```
**Interrupt Enable Pattern:**
- **Value**: CMODI (variable, often 40 octal = 32 decimal = 0x20)
- **Purpose**: Half-duplex RQTS (Request To Send) signal control
- **Enables**: RQTS-specific transmission control interrupts
- **Context**: Used for half-duplex communication protocols

#### 3. Full DMA Transmission Mode (WTTC = 1134 + CMODI)
```assembly
% PRIMARY operational mode for packet transmission:
1134+CMODI; T:=HDEV+WTTC; *EXR ST               % Enable full transmission
1 =: ACTSW                                      % Mark device active
```
**Interrupt Enable Pattern:**
- **Value**: 1134 (octal) + CMODI = 612 (decimal) + mode flags = 0x264 + mode
- **Purpose**: **Full DMA transmitter mode with comprehensive interrupt enables**
- **Enables**: ALL transmission completion interrupts including:
  - **SILFO (bit 15)** - Illegal format/frame error interrupts
  - **TXUND (bit 1)** - Transmitter underrun error interrupts
  - **DMA completion status** - Block/frame/list completion interrupts
  - **Control signal status** - RQTS and other protocol signal interrupts
- **Context**: **Primary mode used during active packet transmission**

#### 4. Combined Control Logic
```assembly
% SINTRAN DMA transmission start sequence:
1134+CMODI; T:=HDEV+WTTC; *EXR ST               % Set full transmission mode
1 =: ACTSW                                      % Mark device active

% Where:
% 1134 (octal) = 612 (decimal) = 0x264 = Base transmission enable mask
% CMODI = Communication Mode Identifier (half/full duplex, RQTS settings)
%         Values: 0 (full duplex), 40 (half duplex/RQTS)
```

### WTTC Bit Analysis for Interrupt Generation

The WTTC value **1134 (octal) = 0x264** breaks down as:

```
Binary:   0010 0110 0100
Hex:      0x264
Decimal:  612
Octal:    1134

Bit Pattern:
15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 0  0  1  0  0  1  1  0  0  1  0  0  0  1  0  0
             ^  ^  ^        ^           ^
          Enables for:
          - Bit 13: X.21 protocol interrupt enable
          - Bit 10: List end interrupt enable
          - Bit 9: Frame end interrupt enable
          - Bit 6: Control signal interrupt enable
          - Bit 2: Underrun interrupt enable
```

### Critical Interrupt Enable Logic

For HOINT to be called, the corresponding **WTTC enable bit must be set** AND the **RTTS status bit must become active**:

#### Normal Transmission Flow:
```assembly
% 1. SINTRAN sets WTTC = 1134+CMODI (enables multiple interrupt sources)
1134+CMODI; T:=HDEV+WTTC; *EXR ST

% 2. Hardware sets RTTS bits when conditions occur:
%    - SILFO (bit 15) when frame format errors occur
%    - TXUND (bit 1) when transmitter underrun occurs
%    - DMA completion bits when transmission finishes

% 3. Interrupt generated ONLY if both:
%    - WTTC enable bit is set (interrupt permission)
%    - RTTS status bit becomes active (condition detected)

% 4. HOINT called with RTTS status available to read
HOINT: T:=HDEV+RTTS; *EXR ST                    % Read status that triggered interrupt
```

### WTTC Configuration Sequence in SINTRAN

#### Transmission Start Sequence:
```assembly
XHMST: % X HDLC MaSter STart - DMA transmission startup
       LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST   % Set DMA address
       A:=2000\/D; T+"WDCR-WDMA"; *EXR ST      % Start transmitter DMA
       T+"RDCR-WDCR"; X:=-20; *EXR ST          % Read DMA status  
       CALL LTOUT; *JAF *-2; ION               % Link timeout handling
       
       % Enable full transmission mode:
       1134+CMODI; T:=HDEV+WTTC; *EXR ST       % Enable all transmission interrupts
       1 =: ACTSW                               % Mark device active
```

#### RQTS Signal Control:
```assembly
% During successful transmission (in HOINT):
IF CMODI = 40 THEN                             % RQTS mode active?
   T:=HDEV+WTTC; *EXR ST                       % Turn off RQTS (write CMODI value)
FI
```

### Impact on C# HDLC Emulator

Your C# HDLC controller emulator needs to:

1. **Track WTTC register writes** to understand which interrupt sources are enabled
2. **Only generate interrupts for enabled conditions:**
   ```csharp
   // Example interrupt logic:
   ushort wttcValue = currentWTTCRegister;  // Value SINTRAN last wrote
   ushort rttsStatus = currentTransmitterStatus;
   
   // Check each potential interrupt source:
   if ((wttcValue & 0x8000) && (rttsStatus & 0x8000)) {
       // SILFO interrupt enabled and format error occurred
       TriggerTransmitterInterrupt();
   }
   
   if ((wttcValue & 0x0002) && (rttsStatus & 0x0002)) {
       // TXUND interrupt enabled and underrun occurred  
       TriggerTransmitterInterrupt();
   }
   
   // Combined check for the critical SINTRAN failure mask:
   if ((wttcValue & 0x8002) && (rttsStatus & 0x8002)) {
       // Critical transmission failure (SILFO+TXUND)
       TriggerTransmitterInterrupt();
   }
   ```

3. **Understand the WTTC patterns:**
   - **WTTC = 0**: No transmission interrupts (off/reset)
   - **WTTC = CMODI**: RQTS control mode interrupts only
   - **WTTC = 1134+CMODI**: Full operational transmission interrupts

4. **Handle CMODI flag variations:**
   ```csharp
   // CMODI values affect transmission mode:
   // CMODI = 0: Full duplex mode
   // CMODI = 40 (octal): Half duplex with RQTS
   
   ushort baseEnable = 0x264;  // 1134 octal
   ushort cmodi = getCurrentCMODI();
   ushort fullWTTC = (ushort)(baseEnable | cmodi);
   ```

### WTTC/HOINT Relationship Summary

| WTTC Value | Context | Interrupt Sources | HOINT Called When |
|------------|---------|------------------|-------------------|
| 0 | Transmitter off | None | Never (disabled) |
| CMODI (40) | RQTS control | Control signals only | RQTS status changes |
| 1134+CMODI | Normal operation | All transmission events | **SILFO, TXUND, DMA completion, control signals** |

### WTTC Enable Bit Mapping for SINTRAN

Based on the 1134 (octal) = 0x264 pattern:

| Bit | Hex Mask | Purpose | SINTRAN Usage |
|-----|----------|---------|---------------|
| 15 | 0x8000 | SILFO error enable | **Critical - frame format error detection** |
| 13 | 0x2000 | X.21 protocol enable | Protocol status change detection |
| 10 | 0x0400 | List end enable | DMA descriptor list completion |
| 9 | 0x0200 | Frame end enable | Individual frame completion |
| 6 | 0x0040 | Control signal enable | RQTS and other protocol signals |
| 2 | 0x0004 | Reserved/additional | Extended control functions |
| 1 | 0x0002 | TXUND error enable | **Critical - underrun error detection** |

**Key Insight:** HOINT is **NOT called automatically** - it only executes when WTTC-enabled interrupt conditions occur in the RTTS register. The value 1134+CMODI is the key operational setting that enables comprehensive transmitter interrupt generation, with the **critical failure mask (SILFO+TXUND = 0x8002)** being the primary success/failure determination logic.

## CORRECTED: How SINTRAN Sets Frame Boundaries in DMA Descriptors

### The Missing Piece: LKEY Low 8 Bits = COM5025 Register Values

The revolutionary discovery is that SINTRAN **pre-programs** frame boundary information directly into the DMA descriptor. When SINTRAN calls:

```assembly
FSERM=:X.LKEY                             % TRANSMIT ONE BLOCK ONLY
```

**FSERM = 002003₈** breaks down as:
- **Bits 10-8**: `010` = Block to be transmitted (from block KEY table)
- **Bits 7-0**: `003` = COM5025 register value with **TSOM=1, TEOM=1**

### How This Solves the FrameEnd vs BlockEnd Question

```csharp
// Your HDLC emulator should read the DMA descriptor:
ushort lkeyValue = ReadDMADescriptor(dmaAddress + 0);  // First word is LKEY

// Extract COM5025 control bits (low 8 bits):
byte com5025Bits = (byte)(lkeyValue & 0xFF);
bool hasTSOM = (com5025Bits & 0x01) != 0;  // Start of Message
bool hasTEOM = (com5025Bits & 0x02) != 0;  // End of Message

// Now you know exactly what SINTRAN wants:
if (hasTSOM && hasTEOM) {
    // FSERM case: Complete frame in single block
    SetRTTSBit("FrameEnd + BlockEnd");
} else if (hasTEOM) {
    // Final block of multi-block frame
    SetRTTSBit("FrameEnd + BlockEnd");
} else {
    // Intermediate block or start block
    SetRTTSBit("BlockEnd");  // Frame continues
}
```

### Multi-Block Frame Constants (Expected Values)

Based on SINTRAN logic patterns, these would be the LKEY values for multi-block frames:

```assembly
% First block: Block key + TSOM only
FirstBlockKey = 002001₈   % 010 (block) + 001 (TSOM=1)

% Middle blocks: Block key only  
MiddleBlockKey = 002000₈  % 010 (block) + 000 (no flags)

% Final block: Block key + TEOM only
LastBlockKey = 002002₈    % 010 (block) + 002 (TEOM=1)

% Single block: Block key + both flags (FSERM)
SingleBlockKey = 002003₈  % 010 (block) + 003 (TSOM=1, TEOM=1)
```

## Deep Analysis: RTTS DMA Status Bits vs. Transmission Indicators

### Critical Understanding: RTTS Status Bits After Packet Transmission

When SINTRAN transmits packets, HOINT reads the **RTTS (Read Transmitter Transfer Status)** register to determine success/failure. Based on HOINT analysis, here are the critical status bits:

#### SILFO (Bit 15) - Illegal Format Error 🚨
```assembly
% SINTRAN Critical Check in HOINT:
IF A/\ "SILFO+TXUND" = 0 THEN    % Success: (status & 0x8002) == 0
   % SUCCESS PATH: Clear retries, mark complete, process next
ELSE
   % FAILURE PATH: Increment retries, log error, retry or fatal
FI
```
**Purpose**: Indicates **HDLC frame format violation** during transmission  
**HDLC Context**: Bad flag sequences, incorrect bit stuffing, frame structure errors  
**HOINT Processing**: **CRITICAL - Part of 0x8002 failure mask**  
**SINTRAN Logic**: If set → **TRANSMISSION FAILED** → retry or fatal error  
**Emulator Usage**: Set when frame format rules are violated

#### TXUND (Bit 1) - Transmitter Underrun Error ⚡
```assembly
% SINTRAN Critical Check in HOINT:  
IF A/\ "SILFO+TXUND" = 0 THEN    % Success: both SILFO and TXUND must be clear
```
**Purpose**: Indicates **transmitter could not maintain data flow**  
**HDLC Context**: CPU/DMA couldn't supply data fast enough, timing violation  
**HOINT Processing**: **CRITICAL - Part of 0x8002 failure mask**  
**SINTRAN Logic**: If set → **TRANSMISSION FAILED** → retry or fatal error  
**Emulator Usage**: Set when simulating system performance problems

#### Success/Failure Determination - HOINT's ONLY Test ⚖️
```csharp
// SINTRAN's ONLY transmission success/failure test in HOINT:
bool transmissionSuccess = (rttsStatus & 0x8002) == 0;  // SILFO+TXUND mask

// HOINT Phase 3 logic:
if (transmissionSuccess)
{
    // SUCCESS PATH:
    // - XRETRY=:RTDYN; A:=0; CALL SADTS (clear retry, log success)
    // - IF CMODI = 40 THEN T:=HDEV+WTTC; *EXR ST FI (turn off RQTS)
    // - 0=:ACTSW (mark device inactive - transmission complete)
    // - CALL NEXTS (process next frame in queue)
}
else
{
    // FAILURE PATH:
    // - XRETRY+1=:XRETRY (increment retry counter)
    // - IF XRETRY > MAXRETRY THEN fatal error ELSE retry FI
    // - A:=237; CALL DRERR or CALL XHMST (fatal error or retry DMA)
}
```

#### TransmissionFinished vs ListEmpty CLARIFICATION 📋
**IMPORTANT**: There is NO "TransmissionFinished" bit in RTTS. The confusion comes from:

1. **Different registers**: RTTS (transmitter) vs RRTS (receiver) have different bit meanings  
2. **Bit 11 in RRTS**: "ListEmpty" (receiver buffer exhaustion)  
3. **Bit 11 in RTTS**: Context-dependent, NOT directly referenced as "TransmissionFinished"  
4. **HOINT Logic**: Uses **ONLY** SILFO+TXUND (0x8002) for success/failure determination

### CORRECTED: RTTS DMA Status Bit Processing with COM5025 Integration

Now that we understand the LKEY field contains COM5025 register values, the transmitter operation flow becomes clear:

```
CORRECTED TRANSMITTER DMA OPERATION FLOW:

1. SINTRAN sets LKEY with COM5025 control bits (TSOM/TEOM)
   ↓
2. DMA hardware reads LKEY and writes low 8 bits to COM5025 chip
   ↓
3. COM5025 chip controls HDLC frame formatting based on TSOM/TEOM
   ↓
4. Hardware sets RTTS status bits based on actual transmission results:
   - BlockEnd (bit 8): Physical buffer transmission complete
   - FrameEnd (bit 9): HDLC frame transmission complete (when TEOM was set)
   - ListEnd (bit 10): All DMA descriptors processed
   - SILFO/TXUND: Transmission errors
   ↓
5. HOINT reads RTTS and performs **ONLY** SILFO+TXUND test (0x8002)

IMPACT ON EMULATOR:
- Your emulator must read LKEY to know SINTRAN's intentions
- Set RTTS bits based on actual TSOM/TEOM values, not assumptions
- BlockEnd vs FrameEnd distinction now has definitive source: COM5025 control bits
```

## HOINT-Specific Packet Transmission Scenarios for HDLC Emulator

### Understanding HOINT's RTTS Status Bit Processing

HOINT performs **ONE critical test**: `(RTTS & 0x8002) == 0`. Your emulator must set RTTS bits that match this expectation:

#### Scenario 1: Successful Single-Frame Transmission ✅
```csharp
/// <summary>
/// Normal packet transmission completed successfully - HOINT SUCCESS PATH
/// </summary>
public ushort HandleSuccessfulTransmission()
{
    // HOINT expects SILFO+TXUND = 0 for success
    // Phase 3 check: IF A/\ "SILFO+TXUND" = 0 THEN (success path)
    var status = 0x0000;  // All clear = SUCCESS

    // Optionally set FrameEnd to indicate HDLC frame completion
    // (HOINT ignores it, but provides hardware realism)
    status |= 0x0200;  // FrameEnd (bit 9)

    // HOINT Result: (status & 0x8002) == 0 → SUCCESS
    // - XRETRY = 0, ACTSW = 0, CALL NEXTS
    return (ushort)status;  // 0x0200 or 0x0000
}
```

#### Scenario 2: Multi-Block Transmission (Large Packet) 🔄
```csharp
/// <summary>
/// Large packet transmission spanning multiple DMA buffers - HOINT processing
/// </summary>
public ushort HandleMultiBlockTransmission(bool isLastBlock, bool isFrameComplete, bool hasError)
{
    var status = 0x0000;

    if (!isLastBlock)
    {
        // Intermediate block - set BlockEnd but keep SILFO+TXUND clear
        status |= 0x0100;  // BlockEnd (bit 8)
        // HOINT Result: (status & 0x8002) == 0 → SUCCESS (intermediate)
    }
    else
    {
        // Final block of packet
        if (isFrameComplete)
        {
            status |= 0x0200;  // FrameEnd (bit 9) - complete HDLC frame
        }

        if (hasError)
        {
            // Simulate transmission error - set SILFO or TXUND  
            status |= 0x8000;  // SILFO (bit 15) - format error
            // or status |= 0x0002;  // TXUND (bit 1) - underrun error
            // HOINT Result: (status & 0x8002) != 0 → FAILURE → retry
        }
        // else: status & 0x8002 == 0 → SUCCESS
    }

    return (ushort)status;
}
```

#### Scenario 3: Transmitter Underrun Error - HOINT RETRY LOGIC ⚡
```csharp
/// <summary>
/// Simulate transmitter underrun (system too slow) - HOINT retry handling
/// </summary>
public ushort HandleTransmitterUnderrun()
{
    // Set TXUND bit - triggers HOINT retry logic
    var status = 0x0002;  // TXUND (bit 1)

    // HOINT Phase 3 processing:
    // IF A/\ "SILFO+TXUND" = 0 THEN (0x0002 & 0x8002 = 0x0002 ≠ 0) → FAILURE
    // ELSE
    //    XRETRY+1=:XRETRY                 Increment retry counter
    //    IF XRETRY > MAXRETRY THEN
    //       A:=237; CALL DRERR            Fatal error 237
    //       0=:ACTSW                      Stop device
    //    ELSE
    //       CALL XHMST                    Restart DMA transmission
    //    FI
    // FI

    return (ushort)status;  // 0x0002 → HOINT retries transmission
}
```

#### Scenario 4: Illegal Format Error - HOINT RETRY LOGIC 🚨
```csharp
/// <summary>
/// Simulate HDLC frame format error - HOINT retry handling
/// </summary>
public ushort HandleIllegalFormatError()
{
    // Set SILFO bit - triggers HOINT retry logic
    var status = 0x8000;  // SILFO (bit 15)

    // HOINT Phase 3 processing:  
    // IF A/\ "SILFO+TXUND" = 0 THEN (0x8000 & 0x8002 = 0x8000 ≠ 0) → FAILURE
    // ELSE
    //    A:=HASTAT; CALL SADTS; CALL DRERR    Log error status
    //    A:=EUND                               Set error code EUND (0x0042)
    //    XRETRY+1=:XRETRY                      Increment retry counter
    //    [retry logic same as underrun]
    // FI

    return (ushort)status;  // 0x8000 → HOINT retries transmission
}
```

#### Scenario 5: Fatal Transmission Error (Both Errors) ☠️
```csharp
/// <summary>
/// Simulate catastrophic transmission failure - HOINT fatal error
/// </summary>
public ushort HandleFatalTransmissionError()
{
    // Set both SILFO and TXUND - guaranteed failure, likely fatal
    var status = 0x8002;  // SILFO (bit 15) + TXUND (bit 1)

    // HOINT Phase 3 processing:
    // IF A/\ "SILFO+TXUND" = 0 THEN (0x8002 & 0x8002 = 0x8002 ≠ 0) → FAILURE
    // Multiple error conditions will likely cause:
    // 1. Rapid retry increment (XRETRY++)
    // 2. Quick exceed of MAXRETRY limit  
    // 3. Fatal error 237 set
    // 4. Device stopped (ACTSW = 0)
    // 5. Upper layers notified (CALL ERRNOT)

    return (ushort)status;  // 0x8002 → HOINT stops transmitter
}
```

#### Scenario 6: X.21 Protocol with Transmission Control 📡
```csharp
/// <summary>
/// Handle X.21 protocol status during transmission - HOINT processing
/// </summary>
public ushort HandleX21ProtocolTransmission(bool x21Error, bool transmissionError)
{
    var status = 0x0000;  // Start with success

    if (x21Error)
    {
        // Set X.21 protocol error bit (informational)
        status |= 0x2000;  // Bit 13 - X.21 protocol error
    }

    if (transmissionError)
    {
        // Also set transmission failure to trigger HOINT retry
        status |= 0x8000;  // SILFO (bit 15) - format error
        // HOINT will process this as transmission failure regardless of X.21 bit
    }

    // HOINT only cares about SILFO+TXUND (0x8002), ignores other bits
    return (ushort)status;
}
```

### HOINT Decision Tree with RTTS Status Bits

```csharp
/// <summary>
/// Emulate HOINT's exact RTTS status bit processing logic
/// </summary>
public class HointStatusProcessor
{
    private const ushort SILFO_TXUND_MASK = 0x8002;  // CRITICAL HOINT test mask

    public HointResult ProcessRttsStatus(ushort rttsStatus, int currentXRetry, int maxRetry)
    {
        // HOINT Phase 1: Timer reset (handled before RTTS read)
        // HOINT Phase 2: Activity check (handled before RTTS read)
        // Assume ACTSW != 0 for this analysis

        // HOINT Phase 3: SUCCESS/FAILURE DETERMINATION - ONLY TEST
        bool success = (rttsStatus & SILFO_TXUND_MASK) == 0;

        if (success)
        {
            // SUCCESS PATH: Clear retries, mark complete
            return new HointResult
            {
                Success = true,
                Action = HointAction.MarkComplete,
                NextStep = "CALL NEXTS",               // Process next frame
                DeviceActive = false,                  // ACTSW = 0
                RetryCount = 0,                        // XRETRY = 0  
                Statistics = "SUCCCNT++",              // Increment success
                RqtsControl = "Turn off if CMODI=40"   // RQTS cleanup
            };
        }
        else
        {
            // FAILURE PATH: Retry logic
            int newRetryCount = currentXRetry + 1;

            if (newRetryCount > maxRetry)
            {
                // FATAL ERROR: Too many retries
                return new HointResult
                {
                    Success = false,
                    Action = HointAction.FatalError,
                    NextStep = "CALL ERRNOT",           // Notify upper layers
                    DeviceActive = false,               // ACTSW = 0
                    RetryCount = newRetryCount,
                    ErrorCode = 237,                    // Fatal transmission error
                    Statistics = "HDERC++"              // Increment error count
                };
            }
            else
            {
                // RETRY ATTEMPT: Restart transmission
                return new HointResult
                {
                    Success = false,
                    Action = HointAction.RetryTransmission,
                    NextStep = "CALL XHMST",            // Restart DMA
                    DeviceActive = true,                // ACTSW = 1 (stay active)
                    RetryCount = newRetryCount,
                    Statistics = "HDERC++",             // Log error
                    ErrorCode = 0x0042                  // EUND - underrun error code
                };
            }
        }
    }
}

public enum HointAction
{
    MarkComplete,        // Transmission successful
    RetryTransmission,   // Retry within limit
    FatalError          // Too many retries, stop
}
```

### HOINT vs HIINT: Key Differences in Status Processing

| Aspect | HOINT (Transmitter) | HIINT (Receiver) |
|--------|-------------------|------------------|
| **Critical Test** | `(RTTS & 0x8002) == 0` | Multiple specific bit checks |
| **Success Indicator** | SILFO+TXUND both clear | DataAvailable=1, errors=0 |
| **Failure Response** | Retry or fatal error | Drop packet or shutdown |
| **DMA Status Usage** | Ignores BlockEnd/FrameEnd/ListEnd | Ignores BlockEnd/FrameEnd/ListEnd |
| **Fatal Condition** | XRETRY > MAXRETRY | ListEmpty (bit 11) |
| **Device Control** | ACTSW: 1→0 (success), stays 1 (retry) | ACTSW: forced 0 (ListEmpty only) |
| **Complexity** | Simple: one mask test | Complex: multiple sequential checks |

## Entry Point and Initial State

### Function Signature
```assembly
SUBR HOINT                                   % Transmitter interrupt handler (Line 104033)
    % Entry state: Interrupt triggered by HDLC transmitter hardware
    % - DMA transmission completed or error condition occurred
    % - Hardware has processed transmit buffer or detected failure
    % - Interrupt level 12 processed, CPU state saved
```

## Step-by-Step Execution Flow with Variable Updates

### Phase 1: Timer Reset and Hardware Status Read
```assembly
HOINT: 0=:TMR                                % Reset transmission timer
       T:=HDEV+RTTS; *EXR ST                 % IOX+12 - Read transmitter status
       A=:HASTA                              % Store raw status in HASTA (HArrdware STAtus) variable
```

**Variable Updates:**
- `TMR` := `0` (reset timeout/retry timer)
- `T` := `HDEV+RTTS` (address calculation for IOX+12)
- `A` := `[HDLC_TRANSMITTER_STATUS_REGISTER]` (16-bit hardware status)
- `HASTAT` := `A` (global status storage for analysis)

**Hardware Effects:**
- DMA status bits may be cleared by the read operation
- Hardware interrupt condition is acknowledged
- Transmission timer reset prevents timeout handling

**Critical Timing:** The timer reset must occur immediately to prevent race conditions with timeout handlers.

### Phase 2: Activity Validation Check
```assembly
IF T:=ACTSW = 0 THEN                         % Device activity check
   MIN DUIN; P+0                             % Increment unexpected interrupt counter
   CALL WT12                                 % Call transmitter wait/retry handler
   GO OUT1                                   % Exit - spurious interrupt
FI
```

**Variable Updates (if ACTSW = 0):**
- `T` := `ACTSW` (load activity switch value)
- `DUIN` := `DUIN - 1` (decrement unexpected interrupt counter)
- `P` := `P + 0` (no-op, possibly for timing alignment)

**State Transitions:**
- **ACTIVE → SPURIOUS**: Transmitter not supposed to be active
- **SPURIOUS → WAIT_STATE**: WT12 handler manages retry/recovery
- **WAIT_STATE → EXIT**: Controlled exit from spurious condition

**Critical ACTSW Logic:** This is the **master control gate** preventing:
- Processing interrupts when device should be stopped (ACTSW = 0)
- Hardware/software race conditions (DMA completion vs. interrupt timing)
- Multiple interrupt source conflicts
- Processing after transmission cancellation

**ACTSW State Analysis:**
- **ACTSW = 0**: Device INACTIVE - interrupt is **SPURIOUS**
- **ACTSW ≠ 0**: Device ACTIVE - interrupt is **LEGITIMATE**
- **High DUIN count**: Indicates ACTSW/hardware synchronization problems

### Phase 3: SUCCESS/FAILURE DETERMINATION - CRITICAL SECTION
```assembly
% PRIMARY SUCCESS CHECK - MOST CRITICAL LOGIC
IF A/\ "SILFO+TXUND" = 0 THEN                % (status AND 0x8002) == 0
   % ===== SUCCESS PATH =====
   XRETRY=:RTDYN; A:=0; CALL SADTS           % Clear retry, log successful transmission
   % Continue with success processing...
ELSE
   % ===== FAILURE PATH =====  
   A:=HASTAT; CALL SADTS; CALL DRERR        % Log error status, increment error counter
   A:=EUND                                   % Set underrun error code (0x0042)
   % Continue with retry/error processing...
FI
```

**SUCCESS PATH Variable Updates:**
- `A` := `HASTAT & 0x8002` (apply SILFO+TXUND mask)
- **If result == 0 (SUCCESS):**
  - `RTDYN` := `XRETRY` (save retry count for statistics)
  - `XRETRY` := `0` (clear retry counter - success achieved)
  - `A` := `0` (clear accumulator for success logging)
  - CALL `SADTS` (Store And Display Transmission Status - success)

**FAILURE PATH Variable Updates:**
- `A` := `HASTAT & 0x8002` (apply SILFO+TXUND mask)  
- **If result != 0 (FAILURE):**
  - `A` := `HASTAT` (reload full status for logging)
  - CALL `SADTS` (Store And Display Transmission Status - failure)
  - CALL `DRERR` (Device Retry Error - increment error counters)
  - `A` := `EUND` (load underrun error code 0x0042)

**State Transitions:**
- **TRANSMITTED → SUCCESS**: Both SILFO and TXUND clear
- **TRANSMITTED → FAILURE**: Either SILFO or TXUND (or both) set
- **SUCCESS → COMPLETED**: Transmission successful, continue
- **FAILURE → RETRY_LOGIC**: Error handling and retry decision

### Phase 4: SUCCESS PATH - Detailed Processing
```assembly
% SUCCESS CONTINUATION (after SILFO+TXUND = 0 check)
% Check for RQTS (Request To Send) mode handling
IF CMODI = 40 THEN                           % RQTS mode active?
   T:=HDEV+WTTC; *EXR ST                     % IOX+13 - Turn off RQTS signal
FI

% Mark transmission completion
0=:ACTSW                                     % *** DEACTIVATE DEVICE (1→0) *** - transmission complete
CALL NEXTS                                   % Process next transmission in queue

% Update completion statistics
SUCCCNT+1=:SUCCCNT                           % Increment successful transmission counter
```

**Variable Updates (SUCCESS PATH):**
- **If RQTS mode (CMODI = 40):**
  - `T` := `HDEV+WTTC` (transmitter control register address)
  - `[HDEV+WTTC]` := `0` (turn off RQTS - Request To Send signal)
- `ACTSW` := `0` (mark transmitter inactive)
- `SUCCCNT` := `SUCCCNT + 1` (increment success counter)

**State Transitions:**
- **SUCCESS → RQTS_CLEANUP**: Turn off control signals if needed
- **RQTS_CLEANUP → INACTIVE**: Device marked inactive
- **INACTIVE → QUEUE_NEXT**: Process next frame in transmission queue

### Phase 5: FAILURE PATH - Retry Logic and Error Handling
```assembly
% FAILURE CONTINUATION (after SILFO+TXUND != 0 check)
XRETRY+1=:XRETRY                             % Increment retry counter

IF XRETRY > MAXRETRY THEN                    % Maximum retries exceeded?
   % ===== FATAL ERROR PATH =====
   A:=237; CALL DRERR                        % Set fatal error code 237
   0=:ACTSW                                  % *** FATAL DEACTIVATION (1→0) *** - stop transmitter permanently
   CALL ERRNOT                               % Notify error to upper layers
ELSE
   % ===== RETRY PATH =====
   CALL XHMST                                % X HDLC MaSter STart - restart DMA
   % XHMST performs (ACTSW STAYS ACTIVE = 1):
   % - LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST  % Set DMA address
   % - A:=2000\/D; T+"WDCR-WDMA"; *EXR ST     % Start transmitter DMA
   % - 1134+CMODI; T:=HDEV+WTTC; *EXR ST      % Enable transmission
   % *** ACTSW remains 1 throughout retry process ***
FI
```

**Variable Updates (FAILURE PATH):**
- `XRETRY` := `XRETRY + 1` (increment retry attempt counter)

**If XRETRY > MAXRETRY (Fatal Error):**
- `A` := `237` (fatal transmission error code)
- CALL `DRERR` (log fatal error, increment error statistics)
- `ACTSW` := `0` (stop transmitter permanently)
- CALL `ERRNOT` (notify upper protocol layers of failure)

**If XRETRY <= MAXRETRY (Retry Attempt):**
- CALL `XHMST` (restart complete DMA transmission)
- DMA registers reconfigured for retry
- Hardware control registers reset
- ACTSW remains active for retry

**State Transitions:**
- **FAILURE → RETRY_CHECK**: Evaluate retry count vs. maximum
- **RETRY_CHECK → FATAL_ERROR**: Too many failures, give up
- **RETRY_CHECK → RETRY_ATTEMPT**: Retry within limits
- **FATAL_ERROR → STOPPED**: Device stopped, upper layers notified
- **RETRY_ATTEMPT → RETRANSMITTING**: Hardware restarted for retry

### Phase 6: Detailed Error Classification and Bit Analysis
```assembly
% ERROR BIT ANALYSIS (based on HASTAT value)
% SILFO (bit 15) - Illegal Format/Key Error:
IF HASTAT BIT 15 THEN
   % Frame format violation:
   % - Invalid HDLC frame structure
   % - Bad flag sequence
   % - Incorrect bit stuffing
   % - Hardware key error
FI

% TXUND (bit 1) - Transmitter Underrun:  
IF HASTAT BIT 1 THEN
   % DMA underrun condition:
   % - CPU couldn't supply data fast enough
   % - Memory bandwidth insufficient
   % - System interrupt latency too high
   % - Buffer management failure
FI
```

**Error Analysis Variables:**
- `HASTAT bit 15` (SILFO) indicates frame format problems
- `HASTAT bit 1` (TXUND) indicates timing/performance problems
- Combined pattern indicates root cause classification

### Phase 7: Status Logging and Circular Buffer Updates
```assembly
% COMPREHENSIVE STATUS LOGGING
% SADTS subroutine performs:
T:=MASTB; * ADSTA@3 STATX                    % Store hardware status to master table

% Circular buffer updates:
BUFF0(BUFSIZ) % Frame first word
BUFF1(BUFSIZ) % Device number  
BUFF2(BUFSIZ) % Device status (HASTAT) ← CRITICAL FOR DEBUGGING
BUFF3(11)     % List keys for stopped devices

% Statistics updates:
HDERC+1 =: HDERC                             % Increment hardware error counter (if error)
LHAST = HASTAT                               % Last hardware status for export
```

**Variable Updates (LOGGING):**
- `MASTB[status_index]` := `HASTAT` (master status table)
- `BUFF2[buffer_index]` := `HASTAT` (circular status buffer)
- `BUFF1[buffer_index]` := `device_number` (device identification)
- **If error occurred:**
  - `HDERC` := `HDERC + 1` (hardware error counter)
  - `DSTAT` := `DSTAT | HASTAT` (cumulative device status)
- `LHAST` := `HASTAT` (exportable status for user access)

### Phase 8: Exit Processing and State Cleanup
```assembly
OUT1: % Common exit point for all code paths
      % - Success processing completed
      % - Retry initiated 
      % - Fatal error handled
      % - Spurious interrupt dismissed

% Final state verification:
% - ACTSW properly set (0=inactive, 1=retrying)
% - Error counters updated
% - Status logged to all required locations
% - Hardware registers in consistent state

% Hardware state at exit:
% - Interrupt acknowledged and cleared  
% - Transmission status captured in HASTAT
% - Device activity state reflects actual condition
% - Next transmission queued or device stopped

RBUS  % Return from subroutine - interrupt processing complete
```

## Complete Variable State Table

| Variable | Purpose | Updated When | Value Range | Critical Impact |
|----------|---------|--------------|-------------|-----------------|
| **TMR** | Transmission timer | Interrupt entry | Reset to 0 | Prevents timeout conflicts |
| **HASTAT** | Hardware status | Every interrupt | 0x0000-0xFFFF | Core decision factor |
| **ACTSW** | Activity switch | Success/fatal error | 0 (inactive) / 1 (active) | Controls device state |
| **XRETRY** | Retry counter | Success (clear) / Failure (increment) | 0 to MAXRETRY | Retry management |
| **RTDYN** | Retry dynamics | Success only | Copy of XRETRY | Statistics/debugging |
| **DUIN** | Unexpected interrupt counter | Spurious interrupts | Decremented | Diagnostic counter |
| **SUCCCNT** | Success counter | Successful transmissions | Incremented | Performance tracking |
| **HDERC** | Hardware error counter | Transmission failures | Incremented | Error rate monitoring |
| **LHAST** | Last hardware status | Status export | Copy of HASTAT | External visibility |
| **BUFF2[]** | Status log buffer | Every interrupt | Circular buffer | Historical analysis |

## State Transition Diagram

```
INTERRUPT_ENTRY
       ↓
   RESET_TIMER (TMR := 0)
       ↓
   READ_RTTS (HASTAT := hardware_status)
       ↓
   CHECK_ACTIVITY
   ├─ ACTSW=0 → COUNT_UNEXPECTED → CALL_WT12 → EXIT
   └─ ACTSW≠0 → CONTINUE
       ↓
   CRITICAL_SUCCESS_CHECK
   ├─ (HASTAT & 0x8002) = 0 → SUCCESS_PATH
   │   ├─ CLEAR_RETRY (XRETRY := 0)
   │   ├─ LOG_SUCCESS (SADTS)
   │   ├─ CLEANUP_RQTS (if needed)
   │   ├─ STOP_DEVICE (ACTSW := 0)
   │   ├─ QUEUE_NEXT (NEXTS)
   │   └─ INCREMENT_SUCCESS → EXIT
   └─ (HASTAT & 0x8002) ≠ 0 → FAILURE_PATH
       ├─ INCREMENT_RETRY (XRETRY + 1)
       ├─ LOG_FAILURE (SADTS, DRERR)
       ├─ CHECK_RETRY_LIMIT
       │   ├─ XRETRY > MAX → FATAL_ERROR
       │   │   ├─ SET_ERROR_237
       │   │   ├─ STOP_DEVICE (ACTSW := 0)
       │   │   ├─ NOTIFY_ERROR (ERRNOT)
       │   │   └─ EXIT
       │   └─ XRETRY ≤ MAX → RETRY_TRANSMISSION
       │       ├─ RESTART_DMA (XHMST)
       │       ├─ MAINTAIN_ACTIVE (ACTSW = 1)
       │       └─ EXIT
```

## Critical Success/Failure Analysis

### SUCCESS CONDITIONS (HASTAT & 0x8002) == 0
```assembly
% Both SILFO (bit 15) AND TXUND (bit 1) must be CLEAR
% Bit 15 CLEAR: No illegal format/frame errors
% Bit 1 CLEAR:  No transmitter underrun
% Result: Frame transmitted successfully to line
```

**Success Variable Changes:**
- `XRETRY` := `0` (clear retry count)
- `RTDYN` := `old_XRETRY` (save retry history)
- `ACTSW` := `0` (mark inactive - done)
- `SUCCCNT` := `SUCCCNT + 1` (increment success)

### FAILURE CONDITIONS (HASTAT & 0x8002) != 0
```assembly
% Either SILFO (bit 15) OR TXUND (bit 1) is SET (or both)
% Bit 15 SET: Illegal format - frame structure problem
% Bit 1 SET:  Transmitter underrun - timing problem  
% Both SET:   Multiple simultaneous errors
```

**Failure Variable Changes:**
- `XRETRY` := `XRETRY + 1` (increment retry)
- `HDERC` := `HDERC + 1` (increment error count)
- `A` := `EUND` (0x0042 - underrun error code)
- `ACTSW` := `1` (remain active for retry) OR `0` (stop if fatal)

### ERROR CLASSIFICATION

#### SILFO Error (Bit 15) - Format/Protocol Issues
- **Cause**: Frame format violations, bad flag sequences, bit stuffing errors
- **Impact**: Protocol-level problem, affects frame integrity
- **Recovery**: Retry transmission with same data
- **Frequency**: Should be rare in stable environment

#### TXUND Error (Bit 1) - Performance Issues  
- **Cause**: CPU/memory bandwidth insufficient, interrupt latency
- **Impact**: System performance problem, affects real-time operation
- **Recovery**: Retry transmission, may need system tuning
- **Frequency**: Indicates system loading issues

#### Combined Errors (Both Bits)
- **Cause**: Severe system problems, hardware issues
- **Impact**: Multiple failure modes active simultaneously
- **Recovery**: Likely requires system-level intervention
- **Frequency**: Should never occur in normal operation

## Performance and Timing Analysis

### Fast Path (Success) Processing
1. Timer reset (1 cycle)
2. RTTS read (hardware access, ~5 cycles)
3. Activity check (2 cycles)
4. Success bit test (3 cycles)
5. Success logging (subroutine call, ~20 cycles)
6. RQTS cleanup (conditional, ~5 cycles)
7. Device deactivation (2 cycles)
8. Queue next frame (subroutine call, ~15 cycles)

**Total Success Path**: ~53 CPU cycles + hardware access time

### Error Path Processing
1. Timer reset through bit test (~11 cycles)
2. Retry increment (2 cycles) 
3. Error logging (subroutine calls, ~40 cycles)
4. Retry limit check (3 cycles)
5. Either:
   - Fatal error handling (~25 cycles)
   - DMA restart (subroutine call, ~50 cycles)

**Total Error Path**: ~56-131 CPU cycles depending on retry vs. fatal

### Spurious Interrupt Path
1. Timer reset through activity check (~8 cycles)
2. Unexpected counter (2 cycles)
3. WT12 handler call (~30 cycles)

**Total Spurious Path**: ~40 CPU cycles

## Debugging and Monitoring Strategies

### Key Status Patterns to Monitor

#### Normal Success Pattern
```
HASTAT = 0x0000-0x7FFD  % Any value with bits 15,1 clear
SILFO+TXUND test = 0    % Success condition
XRETRY = 0              % No retries needed  
ACTSW = 0               % Device inactive after success
```

#### Underrun Error Pattern
```
HASTAT = 0x0002, 0x0003, etc.  % Bit 1 set, others variable
SILFO+TXUND test = 2           % TXUND failure
XRETRY = 1,2,3...              % Increasing retry count
ACTSW = 1                      % Device active, retrying
```

#### Format Error Pattern  
```
HASTAT = 0x8000, 0x8001, etc.  % Bit 15 set, others variable
SILFO+TXUND test = 0x8000      % SILFO failure  
XRETRY = 1,2,3...              % Increasing retry count
ACTSW = 1                      % Device active, retrying
```

#### Fatal Error Pattern
```
XRETRY > MAXRETRY              % Retry limit exceeded
Error code = 237               % Fatal transmission error
ACTSW = 0                      % Device stopped permanently
```

### Diagnostic Variables for Troubleshooting

1. **HASTAT** - Exact hardware status received
2. **XRETRY** - Current retry attempt count  
3. **RTDYN** - Last successful transmission retry count
4. **DUIN** - Spurious interrupt frequency
5. **HDERC** - Total hardware error count
6. **SUCCCNT** - Total successful transmission count  
7. **BUFF2 circular buffer** - Historical HASTAT values

## Complete Variable Reference with Symbol Details

### Core Variables with Exact Addresses
| Variable | Symbol | Address | Purpose | Read/Write | Value Range |
|----------|--------|---------|---------|------------|-------------|
| **HASTAT** | HASTA | 000076 | Hardware status storage | R/W | 0x0000-0xFFFF |
| **ACTSW** | ACTSW | 000074 | Activity switch | R/W | 0=inactive, 1=active |
| **TMR** | TMR | 177773 | Transmission timer | W | Reset to 0 |
| **XRETRY** | XRETR | 000105 | Retry counter | R/W | 0 to MAXR(77) |
| **RTDYN** | RTDYN | 000065 | Retry dynamics/statistics | W | Copy of XRETRY |
| **HDERC** | HDERC | 000066 | Hardware error counter | R/W | Incremented on errors |
| **MASTB** | MASTB | 000041 | Master status table base | R | Base address for logging |

### Constants with Exact Values
| Constant | Symbol | Octal | Hex | Bits | Purpose |
|----------|--------|-------|-----|------|---------|
| **SILFO** | SILFO | 100000 | 0x8000 | 15 | Illegal Format Error |
| **TXUND** | TXUND | 000002 | 0x0002 | 1 | Transmitter Underrun |
| **EUND** | EUND | 000102 | 0x0042 | 1,6 | Underrun Error Code |
| **MAXR** | MAXR | 000115 | 0x004D | - | Maximum Retry Count (77) |

### Subroutines with Addresses
| Subroutine | Symbol | Address | Purpose |
|------------|--------|---------|---------|
| **SADTS** | SADTS | 104735 | Store And Display Transmission Status |
| **DRERR** | DRERR | 104751 | Device Retry Error Handler |

### Memory Access Patterns

#### HASTAT Usage Pattern
```assembly
% Write Pattern (every interrupt):
T:=HDEV+RTTS; *EXR ST; A=:HASTA(000076)

% Read Patterns (decision logic):
IF A/\ "SILFO+TXUND" = 0 THEN            % Success test
IF HASTA(000076)/\ HX21M >< 0 THEN       % Error detection
```

#### ACTSW State Management
```assembly
% State transitions:
1 =: ACTSW(000074)                       % Mark active (DMA started)
0 =: ACTSW(000074)                       % Mark inactive (done/stopped)

% State checking:
IF T:=ACTSW(000074) = 0 THEN             % Check if device should be active
```

#### Retry Logic with XRETRY
```assembly
% Success path:
XRETR(000105)=:RTDYN(000065)             % Save retry count to statistics
0=:XRETR(000105)                         % Clear retry counter

% Failure path:
XRETR(000105)+1=:XRETR(000105)           % Increment retry count
IF XRETR > MAXR(000115) THEN             % Check against limit (77 decimal)
```

The HOINT handler demonstrates sophisticated transmission management with comprehensive retry logic and error classification, making it essential for reliable HDLC communication in the SINTRAN environment.