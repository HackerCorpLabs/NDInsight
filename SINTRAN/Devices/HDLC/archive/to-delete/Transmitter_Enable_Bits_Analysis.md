# Transmitter Enable Bits (WTTC) Analysis - DMA Send Configuration

## Overview: WTTC Register (IOX+13)

The **Write Transmitter Transfer Control (WTTC)** register configures how the HDLC transmitter operates, especially for DMA mode. SINTRAN writes specific bit patterns to this register to set up transmission parameters.

## SINTRAN Usage Patterns from Source Code

### Key WTTC Values Used by SINTRAN

From the source code analysis, SINTRAN uses these specific values:

| Value | Octal | Binary | Usage Context | Purpose |
|-------|--------|---------|--------------|---------|
| **105** | 151 | 01101001 | Standard DMA setup | Enable DMA transmission |
| **107** | 153 | 01101011 | Loop transmission | Enable DMA + additional control |
| **134** | 206 | 10000110 | X.21 DMA mode | Enable DMA + RQTS + X.21 control |
| **140** | 214 | 10001100 | X.21 disable mode | Disable transmission |
| **0** | 000 | 00000000 | Disable/clear | Turn off all transmission |

## Detailed Analysis of Each SINTRAN Usage

### Value 105 (Octal 151) - Standard DMA Transmission
**Source locations: Lines 076161, 103057, 103126, 106261**

```assembly
105; T+"BWTTC-BWTCR"; *EXR ST   % ENABLE OUTPUT
```

**Bit breakdown (105 = 0x45 = 01101001):**
```csharp
TransmitterEnableBits enableBits = 
    TransmitterEnableBits.TransmitBufferEmptyIE |    // Bit 0 = 1
    TransmitterEnableBits.TransmitterEnabled |       // Bit 2 = 1  
    TransmitterEnableBits.EnableTransmitterDMA |     // Bit 3 = 1
    TransmitterEnableBits.RequestToSend |            // Bit 6 = 1
    // Total: 0x45 = 69 decimal = 105 octal
```

**Purpose**: Standard HDLC DMA transmission setup
- Enable DMA operation (bit 3)
- Turn on transmitter (bit 2)
- Enable RQTS signal to modem (bit 6)
- Enable buffer empty interrupt (bit 0)

### Value 107 (Octal 153) - Enhanced DMA Setup
**Source location: Line 103107**

```assembly
T:=107; T=:A; T:=HDEV+BWTTC; *EXR ST;ION
```

**Bit breakdown (107 = 0x47 = 01101011):**
```csharp
TransmitterEnableBits enableBits = 
    TransmitterEnableBits.TransmitBufferEmptyIE |    // Bit 0 = 1
    TransmitterEnableBits.TransmitterUnderrunIE |    // Bit 1 = 1
    TransmitterEnableBits.TransmitterEnabled |       // Bit 2 = 1
    TransmitterEnableBits.EnableTransmitterDMA |     // Bit 3 = 1
    TransmitterEnableBits.RequestToSend |            // Bit 6 = 1
    // Total: 0x47 = 71 decimal = 107 octal
```

**Purpose**: DMA with enhanced error detection
- All features of 105, plus:
- Enable underrun interrupt (bit 1) for error detection

### Value 134 (Octal 206) - X.21 DMA Mode
**Source location: Line 111032**

```assembly
A:=134; T:=X2DHD+XWTTC; *EXR ST    % ENABLE DMA & XMIT, RQTS ON
```

**Bit breakdown (134 = 0x5C = 01011100):**
```csharp
TransmitterEnableBits enableBits = 
    TransmitterEnableBits.TransmitterEnabled |       // Bit 2 = 1
    TransmitterEnableBits.EnableTransmitterDMA |     // Bit 3 = 1
    TransmitterEnableBits.DMAModuleIE |              // Bit 4 = 1
    TransmitterEnableBits.RequestToSend |            // Bit 6 = 1
    // Total: 0x5C = 92 decimal = 134 octal
```

**Purpose**: X.21 protocol DMA transmission
- Enable DMA operation (bit 3)
- Enable DMA interrupts (bit 4) 
- Turn on transmitter (bit 2)
- Enable RQTS for X.21 handshaking (bit 6)

### Value 140 (Octal 214) - X.21 Disable
**Source locations: Lines 110273, 110342, 110371**

```assembly
140; T:=X2DHD+XWTTC; *EXR ST      % T = 0
```

**Bit breakdown (140 = 0x60 = 01100000):**
```csharp
TransmitterEnableBits enableBits = 
    TransmitterEnableBits.HalfDuplex |               // Bit 5 = 1
    TransmitterEnableBits.RequestToSend |            // Bit 6 = 1
    // Total: 0x60 = 96 decimal = 140 octal
```

**Purpose**: X.21 control mode without transmission
- Half-duplex mode (bit 5)
- RQTS on for line control (bit 6)
- No DMA or transmitter enabled

## Critical DMA Enable Bits

### For Normal DMA Operation, SINTRAN Sets:

1. **EnableTransmitterDMA (bit 3) = MANDATORY**
   - Without this, TXBE doesn't trigger DMA requests
   - DMA controller won't process transmit buffers

2. **TransmitterEnabled (bit 2) = MANDATORY** 
   - Physical transmitter must be enabled
   - Without this, no data goes to line

3. **RequestToSend (bit 6) = USUALLY SET**
   - Signals modem that data is coming
   - Required for proper modem handshaking

4. **DMAModuleIE (bit 4) = CONDITIONAL**
   - Only set when DMA interrupts needed
   - Used in X.21 mode (value 134)

### DMA Interrupt Enable Bits (8-10) - NOT Used by SINTRAN

**Interesting Discovery**: SINTRAN **never sets bits 8-10** (BlockEndIE, FrameEndIE, ListEndIE) in the analyzed code.

**This means:**
- SINTRAN doesn't enable individual DMA completion interrupts
- All DMA interrupts come through **DMAModuleIE (bit 4)**
- Hardware generates BlockEnd/FrameEnd/ListEnd status bits
- But only triggers interrupt when **DMAModuleRequest (bit 4)** is set in status

## Emulator Implementation Requirements

### For DMA Transmission Setup
```csharp
public void ConfigureTransmitterForDMA(bool enableUnderrunDetection, bool x21Mode)
{
    TransmitterEnableBits config = 
        TransmitterEnableBits.EnableTransmitterDMA |     // Bit 3 - MANDATORY
        TransmitterEnableBits.TransmitterEnabled |       // Bit 2 - MANDATORY  
        TransmitterEnableBits.RequestToSend |            // Bit 6 - For modem
        TransmitterEnableBits.TransmitBufferEmptyIE;     // Bit 0 - For interrupts
        
    if (enableUnderrunDetection)
    {
        config |= TransmitterEnableBits.TransmitterUnderrunIE;  // Bit 1
    }
    
    if (x21Mode)
    {
        config |= TransmitterEnableBits.DMAModuleIE;     // Bit 4 - For X.21 DMA IRQs
    }
    
    WriteWTTC(config);
}
```

### SINTRAN Pattern Recognition
```csharp
public void HandleWTTCWrite(ushort value)
{
    switch (value)
    {
        case 105:  // Standard DMA
            SetupStandardDMA();
            break;
            
        case 107:  // DMA with underrun detection  
            SetupStandardDMA();
            EnableUnderrunDetection(true);
            break;
            
        case 134:  // X.21 DMA mode
            SetupX21DMA();
            break;
            
        case 140:  // X.21 disable
            DisableTransmission();
            SetX21ControlMode();
            break;
            
        case 0:    // Complete disable
            DisableAllTransmission();
            break;
    }
}
```

### Critical Behavior Requirements

1. **EnableTransmitterDMA (bit 3)**:
   - When set: TXBE triggers DMA requests
   - When clear: TXBE only triggers CPU interrupts (character mode)

2. **DMAModuleIE (bit 4)**:
   - When set: DMA status changes trigger CPU interrupts
   - When clear: DMA operates silently (polling mode)

3. **Interrupt Generation**:
   - DMA events only generate interrupts if **both** status bit AND corresponding enable bit are set
   - Exception: **DMAModuleRequest** always triggers if **DMAModuleIE** is set

## Summary

SINTRAN uses **very specific bit patterns** for WTTC configuration:
- **105** = Standard DMA transmission
- **107** = DMA with underrun detection  
- **134** = X.21 DMA mode
- **140** = X.21 control (no transmission)
- **0** = Complete disable

**Your emulator must recognize these patterns** and configure DMA operation accordingly. The **EnableTransmitterDMA (bit 3)** is the **critical bit** - without it, no DMA operation occurs regardless of other settings.

## Deep Analysis: Transmitter DMA Status Bits vs. Packet Indicators

### Critical Understanding: RTTS Status Bits After Packet Transmission

When SINTRAN transmits packets, it reads the **RTTS (Read Transmitter Transfer Status)** register to determine success/failure. Based on the HOINT analysis, here are the critical status bits:

#### SILFO (Bit 15) - Illegal Format Error
```assembly
% SINTRAN Critical Check in HOINT:
IF A/\ "SILFO+TXUND" = 0 THEN    % Success: (status & 0x8002) == 0
```
**Purpose**: Indicates **HDLC frame format violation** during transmission
**HDLC Context**: Bad flag sequences, incorrect bit stuffing, frame structure errors  
**SINTRAN Logic**: If set → **TRANSMISSION FAILED** → retry
**Emulator Usage**: Set when frame format rules are violated

#### TXUND (Bit 1) - Transmitter Underrun Error
```assembly
% SINTRAN Critical Check in HOINT:  
IF A/\ "SILFO+TXUND" = 0 THEN    % Success: both SILFO and TXUND must be clear
```
**Purpose**: Indicates **transmitter could not maintain data flow**
**HDLC Context**: CPU/DMA couldn't supply data fast enough, timing violation
**SINTRAN Logic**: If set → **TRANSMISSION FAILED** → retry  
**Emulator Usage**: Set when simulating system performance problems

#### Success/Failure Determination (CRITICAL)
```csharp
// SINTRAN's ONLY transmission success/failure test:
bool transmissionSuccess = (rttsStatus & 0x8002) == 0;  // SILFO+TXUND mask

// If success:  XRETRY = 0, ACTSW = 0, process next packet
// If failure:  XRETRY++, retry transmission (or fatal error if max retries exceeded)
```

#### CORRECTED: TransmissionFinished (Bit 11) in RTTS
**CORRECTION BASED ON HARDWARE SPECIFICATION**: RTTS **DOES** have a **TransmissionFinished** bit at **bit 11**:

1. **RTTS bit 11**: **TransmissionFinished (TRFIN)** - DMA module transmission complete status
2. **RRTS bit 11**: **ListEmpty (LE)** - Receiver has no more receive buffers available  
3. **Critical Hardware Behavior**: **"TransmissionFinished (bit 11) always gives a DMA Module Request (bit 4)"**

**Hardware Specification Details**:
- **TransmissionFinished (bit 11)**: Transmission Finished status bit from the DMA module
- **DMAModuleRequest (bit 4)**: Always activated when TransmissionFinished is set
- **Purpose**: Signals complete transmission sequence and triggers interrupt level 12
- **Relationship**: Hardware automatically sets bit 4 when bit 11 is set

### DMA Status Bit Hierarchy for Transmitter (CORRECTED)

```
TRANSMITTER DMA OPERATION LEVELS:

BlockEnd (bit 8)          ─── Single transmit buffer emptied
    │
    ├─ FrameEnd (bit 9)        ─── HDLC frame transmission complete  
    │
    ├─ ListEnd (bit 10)        ─── All DMA descriptors processed
    │
    └─ TransmissionFinished    ─── Complete transmission sequence done
        (bit 11)                   │
             │                     ├─ Automatically sets DMAModuleRequest (bit 4)
             └─────────────────────└─ Triggers interrupt level 12
    
NORMAL TRANSMISSION PROGRESSION:
1. BlockEnd: Buffer transmitted → next buffer
2. FrameEnd: Frame complete → frame transmitted successfully
3. ListEnd: All buffers sent → transmission complete
4. SILFO+TXUND check: Determine overall success/failure
```

## HDLC Transmitter Emulation Strategy - Complete Guide

### Understanding WTTC Interrupt-Enabled Bit Setting

Based on the WTTC interrupt enable analysis, your HDLC controller must consider **which interrupts are enabled** before setting RTTS status bits:

#### WTTC Enable Pattern Analysis (1134 + CMODI)
```csharp
// From SINTRAN analysis: WTTC = 1134 + CMODI enables these interrupts:
//
// Base 1134 (octal) = 0x264:
// 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0  
//  0  0  1  0  0  1  1  0  0  1  0  0  0  1  0  0
//             ^     ^  ^     ^           ^     ^
// Enabled:   │     │  │     │           │     └─ Bit 1: TXUND interrupt enable
//            │     │  │     │           └─────── Bit 2: Additional control  
//            │     │  │     └─────────────────── Bit 6: Control signal enables
//            │     │  └───────────────────────── Bit 9: FrameEnd interrupt enable
//            │     └─────────────────────────── Bit 10: ListEnd interrupt enable
//            └───────────────────────────────── Bit 13: X.21 protocol interrupt enable
//
// PLUS CMODI flags (typically 0 or 40 octal for half/full duplex)
```

### Packet Transmission Scenarios for HDLC Emulator

#### Scenario 1: Successful Single-Frame Transmission
```csharp
/// <summary>
/// Normal packet transmission completed successfully
/// </summary>
public ushort HandleSuccessfulTransmission()
{
    // SINTRAN expects SILFO+TXUND = 0 for success
    // Do NOT set any error bits
    var status = 0x0000;  // All clear = SUCCESS
    
    // Optionally set FrameEnd to indicate HDLC frame completion
    // (Not required by SINTRAN but provides hardware realism)
    status |= 0x0200;  // FrameEnd (bit 9)
    
    return (ushort)status;  // 0x0200 or just 0x0000
}
```

#### Scenario 2: Multi-Block Transmission (Large Packet)
```csharp
/// <summary>
/// Large packet transmission spanning multiple DMA buffers
/// </summary>
public ushort HandleMultiBlockTransmission(bool isLastBlock, bool isFrameComplete, bool hasError)
{
    var status = 0x0000;
    
    if (!isLastBlock)
    {
        // Intermediate block - just indicate buffer transmitted
        status |= 0x0100;  // BlockEnd (bit 8)
    }
    else
    {
        // Final block of packet
        if (isFrameComplete)
        {
            status |= 0x0200;  // FrameEnd (bit 9) - HDLC frame complete
        }
        
        if (!hasError)
        {
            // Success: SILFO+TXUND both clear (bits 15,1 = 0)
            // Status already 0x0000 for success bits
        }
        else
        {
            // Simulate transmission error
            status |= 0x8000;  // SILFO (bit 15) - format error
            // or status |= 0x0002;  // TXUND (bit 1) - underrun error
        }
    }
    
    return (ushort)status;
}
```

#### Scenario 3: Transmitter Underrun Error
```csharp
/// <summary>
/// Simulate transmitter underrun (system too slow)
/// </summary>
public ushort HandleTransmitterUnderrun()
{
    // Set TXUND bit - this will trigger SINTRAN retry logic
    var status = 0x0002;  // TXUND (bit 1)
    
    // This causes SINTRAN to:
    // 1. Increment XRETRY counter
    // 2. Call DRERR (error logging)
    // 3. Either retry transmission or give up if max retries exceeded
    // 4. ACTSW remains 1 during retries, becomes 0 on fatal error
    
    return (ushort)status;  // 0x0002
}
```

#### Scenario 4: Illegal Format Error
```csharp
/// <summary>
/// Simulate HDLC frame format error  
/// </summary>
public ushort HandleIllegalFormatError()
{
    // Set SILFO bit - this will trigger SINTRAN retry logic
    var status = 0x8000;  // SILFO (bit 15)
    
    // This causes SINTRAN to:
    // 1. Increment XRETRY counter  
    // 2. Call DRERR (error logging)
    // 3. Set error code EUND (0x0042)
    // 4. Either retry transmission or fatal error
    
    return (ushort)status;  // 0x8000
}
```

#### Scenario 5: Fatal Transmission Error (Both Errors)
```csharp
/// <summary>
/// Simulate catastrophic transmission failure
/// </summary>
public ushort HandleFatalTransmissionError()
{
    // Set both SILFO and TXUND - guaranteed failure
    var status = 0x8002;  // SILFO (bit 15) + TXUND (bit 1)
    
    // This causes SINTRAN to:
    // 1. Immediate retry increment
    // 2. Likely exceed MAXRETRY quickly
    // 3. Set fatal error 237
    // 4. Stop device (ACTSW = 0)
    // 5. Notify upper layers (ERRNOT)
    
    return (ushort)status;  // 0x8002
}
```

#### Scenario 6: X.21 Protocol Control
```csharp
/// <summary>
/// Handle X.21 protocol status during transmission
/// </summary>
public ushort HandleX21ProtocolStatus(bool x21Error)
{
    var status = 0x0000;  // Start with success
    
    if (x21Error)
    {
        // Set X.21 protocol error bit
        status |= 0x2000;  // Bit 13 - X.21 protocol error
        
        // Could also set SILFO to make it a transmission failure
        status |= 0x8000;  // SILFO (bit 15)
    }
    
    return (ushort)status;
}
```

### Advanced Transmitter DMA Status Bit Usage

#### When to Set BlockEnd (Bit 8)
```csharp
/// <summary>
/// Set BlockEnd when individual DMA transmit buffer is emptied
/// </summary>
public bool ShouldSetBlockEnd(DmaTransmitBuffer buffer)
{
    // Set BlockEnd when:
    // 1. DMA buffer completely transmitted to line
    // 2. Not the final buffer of the frame
    // 3. More buffers needed to complete transmission
    
    return buffer.IsEmpty && !buffer.IsLastInFrame;
}
```

#### When to Set FrameEnd (Bit 9)
```csharp
/// <summary>
/// Set FrameEnd when complete HDLC frame has been transmitted
/// </summary>
public bool ShouldSetFrameEnd(HdlcTransmitFrame frame)
{
    // Set FrameEnd when:
    // 1. All frame data transmitted to line
    // 2. HDLC closing flag transmitted
    // 3. Frame Check Sequence transmitted
    // 4. Complete HDLC frame sent
    
    return frame.IsCompletelyTransmitted && frame.ClosingFlagSent;
}
```

#### When to Set ListEnd (Bit 10)
```csharp
/// <summary>
/// Set ListEnd when entire DMA descriptor list has been processed
/// </summary>
public bool ShouldSetListEnd(DmaTransmitDescriptorList descriptorList)
{
    // Set ListEnd when:
    // 1. All DMA descriptors in list processed
    // 2. No more data to transmit from current list
    // 3. May need to start new descriptor list
    
    return descriptorList.AllDescriptorsProcessed;
}
```

### Transmitter Interrupt Generation Logic

```csharp
/// <summary>
/// Complete transmitter interrupt generation logic based on WTTC enables
/// </summary>
public void CheckAndGenerateTransmitterInterrupt(ushort wttcValue, ushort rttsStatus)
{
    // Only generate interrupt if corresponding enable bit is set
    
    // Check TXUND interrupt (bit 1) - CRITICAL ERROR
    if ((wttcValue & 0x0002) && (rttsStatus & 0x0002))
    {
        TriggerTransmitterInterrupt("TransmitterUnderrun - ERROR");
    }
    
    // Check BlockEnd interrupt (bit 8)
    if ((wttcValue & 0x0100) && (rttsStatus & 0x0100))
    {
        TriggerTransmitterInterrupt("BlockEnd");
    }
    
    // Check FrameEnd interrupt (bit 9) 
    if ((wttcValue & 0x0200) && (rttsStatus & 0x0200))
    {
        TriggerTransmitterInterrupt("FrameEnd");
    }
    
    // Check ListEnd interrupt (bit 10)
    if ((wttcValue & 0x0400) && (rttsStatus & 0x0400))
    {
        TriggerTransmitterInterrupt("ListEnd");
    }
    
    // Check X.21 protocol interrupt (bit 13)
    if ((wttcValue & 0x2000) && (rttsStatus & 0x2000))
    {
        TriggerTransmitterInterrupt("X21 Protocol Status");
    }
    
    // Check SILFO error interrupt (bit 15) - CRITICAL ERROR
    if ((wttcValue & 0x8000) && (rttsStatus & 0x8000))
    {
        TriggerTransmitterInterrupt("IllegalFormat - ERROR");
    }
    
    // CRITICAL: Check combined failure condition
    if ((wttcValue & 0x8002) && (rttsStatus & 0x8002))
    {
        TriggerTransmitterInterrupt("TRANSMISSION FAILED - RETRY REQUIRED");
    }
}
```

### SINTRAN Transmission Success/Failure Logic

```csharp
/// <summary>
/// Emulate SINTRAN's exact transmission validation logic
/// </summary>
public class SintranTransmissionValidator
{
    private const ushort SILFO_TXUND_MASK = 0x8002;  // SILFO (bit 15) + TXUND (bit 1)
    
    public TransmissionResult ValidateTransmission(ushort rttsStatus)
    {
        // SINTRAN's exact logic from HOINT:
        // IF A/\ "SILFO+TXUND" = 0 THEN (success path) ELSE (failure path) FI
        
        bool success = (rttsStatus & SILFO_TXUND_MASK) == 0;
        
        if (success)
        {
            return new TransmissionResult
            {
                Success = true,
                Action = TransmissionAction.MarkComplete,  // ACTSW = 0
                NextStep = "CALL NEXTS",  // Process next frame
                RetryAction = "XRETRY = 0",  // Clear retry counter
                Statistics = "SUCCCNT++"  // Increment success counter
            };
        }
        else
        {
            return new TransmissionResult  
            {
                Success = false,
                Action = TransmissionAction.RetryOrFatal,  // Depends on XRETRY count
                NextStep = "CALL XHMST or CALL ERRNOT",  // Retry or notify error
                RetryAction = "XRETRY++",  // Increment retry counter
                ErrorCode = 237,  // Fatal transmission error (if max retries exceeded)
                Statistics = "HDERC++"  // Increment error counter
            };
        }
    }
}

public enum TransmissionAction
{
    MarkComplete,     // ACTSW = 0, transmission successful
    RetryOrFatal      // Either retry (ACTSW = 1) or fatal stop (ACTSW = 0)
}
```

## DETAILED FLOWCHARTS - DMA Send Configuration

### 1. WTTC Register Configuration Flow

```
┌═══════════════════════════════┐
║    SINTRAN HDLC TRANSMISSION  ║
║         SETUP REQUEST         ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│   DETERMINE TRANSMISSION    │
│         MODE TYPE           │
│                             │
│  - Standard DMA (105)       │
│  - Enhanced DMA (107)       │  
│  - X.21 DMA (134)          │
│  - X.21 Disable (140)      │
│  - Complete Disable (0)     │
└──────────┬──────────────────┘
           │
           ▼
    ┌──────┴──────┐
   ┌┴┐   ┌┴┐   ┌┴┐   ┌┴┐   ┌┴┐
  105  107  134  140   0
   │    │    │    │    │
   ▼    ▼    ▼    ▼    ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐   ┌─────────────────────────────┐   ┌─────────────────────────────┐   ┌─────────────────────────────┐
│   STANDARD DMA SETUP        │   │   ENHANCED DMA SETUP        │   │      X.21 DMA SETUP         │   │     X.21 DISABLE MODE       │   │    COMPLETE DISABLE         │
│   Value: 105 (0x45)         │   │   Value: 107 (0x47)         │   │     Value: 134 (0x5C)       │   │     Value: 140 (0x60)       │   │       Value: 0              │
│                             │   │                             │   │                             │   │                             │   │                             │
│ ┌─ TransmitBufferEmptyIE    │   │ ┌─ TransmitBufferEmptyIE    │   │ ┌─ TransmitterEnabled        │   │ ┌─ HalfDuplex                │   │ ┌─ All bits cleared         │
│ ├─ TransmitterEnabled       │   │ ├─ TransmitterUnderrunIE   │   │ ├─ EnableTransmitterDMA     │   │ └─ RequestToSend             │   │                             │
│ ├─ EnableTransmitterDMA     │   │ ├─ TransmitterEnabled       │   │ ├─ DMAModuleIE              │   │                             │   │                             │
│ └─ RequestToSend            │   │ ├─ EnableTransmitterDMA     │   │ └─ RequestToSend            │   │                             │   │                             │
│                             │   │ └─ RequestToSend            │   │                             │   │                             │   │                             │
└──────────┬──────────────────┘   └──────────┬──────────────────┘   └──────────┬──────────────────┘   └──────────┬──────────────────┘   └──────────┬──────────────────┘
           │                                 │                                 │                                 │                                 │
           └─────────────┬───────────────────┼─────────────────────────────────┼─────────────────────────────────┼─────────────────────────────────┘
                         │                   │                                 │                                 │
                         ▼                   ▼                                 ▼                                 ▼
┌══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════┐
║                                                    WRITE TO WTTC REGISTER                                                                                   ║  
║                                                  T:=value; T:=HDEV+WTTC; *EXR ST                                                                           ║
└══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════┘
                                                                             │
                                                                             ▼
┌─────────────────────────────┐
│    HARDWARE CONFIGURATION   │
│         ACTIVATED           │
│                             │
│ - DMA mode set if bit 3=1   │
│ - Transmitter on if bit 2=1 │
│ - Interrupts per enable bits│
│ - RQTS signal per bit 6     │
└─────────────────────────────┘
```

### 2. DMA Transmission Status Processing Flow

```
┌═══════════════════════════════┐
║   HDLC TRANSMISSION COMPLETE  ║
║   Hardware generates IRQ 12   ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│      HOINT INTERRUPT        │
│        HANDLER              │
│                             │
│ T:=HDEV+RTTS; *EXR ST       │
│ A=:HASTAT                   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   READ RTTS STATUS BITS     │
│   (Transmitter Status)      │
│                             │
│ ┌─ TransmissionFinished(11) │
│ ├─ ListEnd(10)              │
│ ├─ FrameEnd(9)              │
│ ├─ BlockEnd(8)              │
│ ├─ RequestToSend(6)         │
│ ├─ DMAModuleRequest(4)      │
│ ├─ TransmitterActive(2)     │
│ ├─ TransmitterUnderrun(1)   │
│ ├─ TransmitBufferEmpty(0)   │  
│ └─ Illegal(15)              │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║   CRITICAL SUCCESS/FAILURE    ║
║         EVALUATION            ║
║                               ║
║ IF A/\"SILFO+TXUND" = 0 THEN ║
└═══════════════════════════════┘
           │
           ▼
    ┌──────┴──────┐
   SUCCESS      FAILURE
    │              │
    ▼              ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│   TRANSMISSION SUCCESS      │   │   TRANSMISSION FAILURE      │
│                             │   │                             │
│ (RTTS & 0x8002) == 0        │   │ (RTTS & 0x8002) != 0        │
│                             │   │                             │
│ ┌─ SILFO (bit 15) = 0       │   │ ┌─ SILFO (bit 15) = 1  OR   │
│ └─ TXUND (bit 1) = 0        │   │ └─ TXUND (bit 1) = 1        │
└──────────┬──────────────────┘   └──────────┬──────────────────┘
           │                                 │
           ▼                                 ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│      SUCCESS ACTIONS        │   │      FAILURE ACTIONS        │
│                             │   │                             │
│ XRETRY=:RTDYN               │   │ A:=HASTAT; CALL SADTS       │
│ A:=0; CALL SADTS            │   │ CALL DRERR                  │
│ (Reset retry counter)       │   │ A:=EUND                     │
│                             │   │ (Error processing)          │
└──────────┬──────────────────┘   └──────────┬──────────────────┘
           │                                 │
           ▼                                 ▼
┌─────────────────────────────┐   ┌═══════════════════════════════┐
│   TRANSMISSION COMPLETE     │   ║      DRERR RETRY LOGIC      ║
│                             │   ║                             ║
│ 0=:DCBX                     │   ║ A\/DSTAT =: DSTAT           ║
│ GO FAR BACKX                │   ║ HDERC+1 =: HDERC            ║
│ (Return to user)            │   ║ MIN RTDYN; GO SRDAT         ║
└─────────────────────────────┘   ║ XRETRY=:RTDYN; EXIT         ║
                                  └═══════════════════════════════┘
                                              │
                                              ▼
                                       ┌──────┴──────┐
                                   RETRY OK      RETRIES
                                      │         EXHAUSTED
                                      ▼              │
                                  ┌─────────────────────────────┐   │
                                  │    RETRANSMISSION           │   │
                                  │                             │   │
                                  │ GO SRDAT (XSSND)            │   │ 
                                  │ GO FAR XXHMST               │   │
                                  │ (Skip normal validation)    │   │
                                  └──────────┬──────────────────┘   │
                                            │                      │
                                            ▼                      ▼
                                  ┌─────────────────────────────┐   ┌─────────────────────────────┐
                                  │   HARDWARE RESTART          │   │      FATAL ERROR           │
                                  │                             │   │                             │
                                  │ DMA restarted               │   │ XRETRY exceeded             │
                                  │ COM5025 retransmits         │   │ 0=:DCBX                     │
                                  │ Wait for completion IRQ     │   │ GO FAR BACKX                │
                                  └─────────────────────────────┘   │ (Return error to user)      │
                                                                    └─────────────────────────────┘
```

### 3. Multi-Block Frame Transmission Flow

```
┌═══════════════════════════════┐
║     LARGE PACKET REQUIRING    ║
║   MULTIPLE DMA DESCRIPTORS    ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│   SETUP DESCRIPTOR CHAIN    │
│                             │
│ First:  LKEY = 002001₈      │
│         (TSOM=1, TEOM=0)    │
│                             │  
│ Middle: LKEY = 002000₈      │
│         (TSOM=0, TEOM=0)    │
│                             │
│ Last:   LKEY = 002002₈      │  
│         (TSOM=0, TEOM=1)    │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║    START DMA TRANSMISSION     ║
║                               ║
║ LIINT+DPITPHYS;               ║
║ T:=HDEV+WDMA; *IOF; EXR ST    ║
║ A:=2000\/D; T+"WDCR-WDMA"     ║
║ 1134+CMODI; T:=HDEV+WTTC      ║
└═══════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   PROCESS FIRST BLOCK       │
│                             │
│ COM5025 sees TSOM=1         │
│ - Generate opening FLAG     │
│ - Insert Address/Control    │
│ - Transmit first data chunk │
│ - NO closing FLAG yet       │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   FIRST BLOCK INTERRUPT     │
│                             │
│ RTTS Status:                │
│ ┌─ BlockEnd = 1             │
│ ├─ FrameEnd = 0             │
│ ├─ TransmissionFinished = 0 │
│ └─ DMAModuleRequest = 1     │
│                             │
│ Frame continues...          │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│  PROCESS MIDDLE BLOCKS      │
│                             │
│ COM5025 sees TSOM=0, TEOM=0 │
│ - Continue data stream      │
│ - Raw data transmission     │
│ - Frame still open          │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│  MIDDLE BLOCK INTERRUPTS    │
│                             │
│ RTTS Status:                │
│ ┌─ BlockEnd = 1             │
│ ├─ FrameEnd = 0             │
│ ├─ TransmissionFinished = 0 │
│ └─ DMAModuleRequest = 1     │
│                             │
│ Frame continues...          │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   PROCESS FINAL BLOCK       │
│                             │
│ COM5025 sees TEOM=1         │
│ - Transmit final data       │
│ - Calculate FCS             │
│ - Append FCS bytes          │
│ - Generate closing FLAG     │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║   FINAL BLOCK INTERRUPT       ║
║                               ║
║ RTTS Status:                  ║
║ ┌─ BlockEnd = 1               ║
║ ├─ FrameEnd = 1               ║
║ ├─ TransmissionFinished = 1   ║
║ └─ DMAModuleRequest = 1       ║
║                               ║
║ COMPLETE FRAME TRANSMITTED    ║
└═══════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│    SUCCESS/FAILURE CHECK    │
│                             │
│ IF (RTTS & 0x8002) == 0:    │
│   SUCCESS - frame complete  │
│ ELSE:                       │
│   ERROR - retry entire frame│
└─────────────────────────────┘
```

### 4. TransmissionFinished → DMAModuleRequest Relationship Flow

```
┌═══════════════════════════════┐
║   DMA TRANSMISSION SEQUENCE   ║
║       NEARING COMPLETION      ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│  HARDWARE PROCESSES LAST    │
│      DMA DESCRIPTOR         │
│                             │
│ - All buffers transmitted   │
│ - All frame data sent       │
│ - COM5025 operations done   │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║  HARDWARE SETS BIT 11         ║
║  TransmissionFinished = 1     ║  
║                               ║
║  "Complete transmission       ║
║   sequence done"              ║
└═══════════════════════════════┘
                │
                ▼
┌═══════════════════════════════┐
║   AUTOMATIC HARDWARE RULE:    ║
║                               ║
║ "TransmissionFinished (bit 11)║
║  always gives a DMA Module    ║
║  Request (bit 4)"             ║
║                               ║
║  Hardware AUTOMATICALLY sets  ║
║  DMAModuleRequest = 1         ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│    RTTS REGISTER STATE      │
│                             │
│ Bit 11: TransmissionFinished│
│         = 1                 │
│ Bit 4:  DMAModuleRequest    │  
│         = 1 (auto-set)      │
│                             │
│ Other status bits as needed │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║    INTERRUPT GENERATION       ║
║                               ║
║ DMAModuleRequest (bit 4) = 1  ║
║ + DMA interrupts enabled      ║
║ = TRIGGER LEVEL 12 INTERRUPT  ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│      SINTRAN HOINT          │
│    INTERRUPT HANDLER        │
│                             │
│ T:=HDEV+RTTS; *EXR ST       │
│ A=:HASTAT                   │
│                             │
│ Processes transmission      │
│ completion                  │
└─────────────────────────────┘
```

### 5. WTTC Interrupt Enable Configuration Flow

```
┌═══════════════════════════════┐
║   SINTRAN CALCULATES WTTC     ║
║       1134 + CMODI            ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│    BASE VALUE: 1134₈        │
│    (0x264 = 0010 0110 0100) │
│                             │
│ Bit 13: X.21 interrupt      │
│ Bit 10: ListEnd interrupt   │
│ Bit 9:  FrameEnd interrupt  │
│ Bit 6:  Control signals     │
│ Bit 2:  Additional control  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│     ADD CMODI FLAGS         │
│                             │
│ CMODI = 0:  Full duplex     │
│ CMODI = 40₈: Half duplex    │
│                             │
│ Final WTTC = 1134 + CMODI   │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║    WRITE TO WTTC REGISTER     ║
║                               ║
║ 1134+CMODI; T:=HDEV+WTTC;     ║
║ *EXR ST                       ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│  HARDWARE INTERRUPT ENABLES │
│         CONFIGURED          │
│                             │
│ ┌─ TXUND interrupts (bit 1) │
│ ├─ Control signal IRQs      │
│ ├─ FrameEnd interrupts      │
│ ├─ ListEnd interrupts       │
│ └─ X.21 protocol interrupts │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   TRANSMISSION OPERATIONS   │
│     WITH IRQ MONITORING     │
│                             │
│ Hardware will generate      │
│ interrupts when:            │
│ - Status bit = 1 AND        │
│ - Corresponding enable = 1  │
└─────────────────────────────┘
```

## Updated Summary with Comprehensive Transmitter Analysis

**Combined DMA Operation Requirements:**

### Transmitter DMA Setup (WTTC):
- **1134 + CMODI**: Full operational transmission mode (SINTRAN standard)
- **CMODI = 0**: Full duplex mode  
- **CMODI = 40**: Half duplex with RQTS
- **EnableTransmitterDMA (bit 3)** is mandatory for DMA operation
- **Critical failure mask: 0x8002** (SILFO + TXUND)

### Transmitter Status Reporting (RTTS):
- **Success Pattern**: `(status & 0x8002) == 0` - both SILFO and TXUND clear
- **Failure Pattern**: `(status & 0x8002) != 0` - either SILFO or TXUND set  
- **DMA Status Bits**: BlockEnd(8), FrameEnd(9), ListEnd(10), TransmissionFinished(11)
- **Error Bits**: SILFO(15), TXUND(1) for critical failure detection
- **Hardware Rule**: TransmissionFinished(11) automatically sets DMAModuleRequest(4)

### Receiver DMA Setup (WRTC):
- **1734**: Full operational reception mode (SINTRAN standard)
- **Success Pattern**: DataAvailable(0) set, X.21 errors(13-14) clear, ListEmpty(11) clear
- **EnableReceiverDMA (bit 3)** is mandatory

### Key Implementation Points:
1. **SINTRAN uses simple success/failure logic**: Check specific bit masks, not individual status meanings
2. **DMA status bits provide operational detail** but don't affect core success/failure decisions  
3. **Interrupt generation requires both enable bit AND status bit** to be set
4. **Emulator must track WTTC/WRTC values** to know which interrupts are enabled
5. **Critical masks**: SILFO+TXUND(0x8002) for transmit, DataAvailable+X.21(varies) for receive
6. **Hardware automatically links TransmissionFinished to DMAModuleRequest** for interrupt generation