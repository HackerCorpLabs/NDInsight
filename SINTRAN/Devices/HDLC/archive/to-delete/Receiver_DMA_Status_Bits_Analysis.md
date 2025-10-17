# Receiver DMA Status Bits Analysis - SINTRAN Source Code Findings

## REGISTER CLARIFICATION - RRTS (Receiver Transfer Status)

You're referring to **RRTS (HDEV+10)** = **R**ead **R**eceiver **T**ransfer **S**tatus

This is the register that SINTRAN reads in the **HIINT** receiver interrupt handler:

```assembly
HIINT: T:=HDEV+RRTS; *EXR ST     % IOX+10 - Read RRTS register
       A=:HASTA                  % Store receiver status
```

Your C# enum is for **RRTS** (DMA receiver transfer status) based on the bit descriptions mentioning DMA module bits 8-15 being cleared on read.

## Critical Finding: What Indicates a Valid Received Packet

Based on the SINTRAN source code analysis, here's what your HDLC emulator should set for a **successful packet reception**:

### SUCCESSFUL PACKET RECEPTION - Required Bit Pattern

```csharp
// MINIMAL pattern for successful packet processing:
ReceiverStatusBits rrts = ReceiverStatusBits.DataAvailable;  // ONLY bit 0 set

// SINTRAN validation logic:
// 1. DataAvailable (bit 0) = 1 ✅ REQUIRED
// 2. X.21 errors (bits 13-14) = 0 ✅ REQUIRED  
// 3. ListEmpty (bit 11) = 0 ✅ REQUIRED
// 4. All other bits = 0 ✅ (DMA status bits 8-10 cleared after read)

// Result: Packet will be processed by SINTRAN
```

## Updated C# Enum with SINTRAN Meanings

Based on the SINTRAN source code analysis, here are the corrected comments:

```csharp
/***** DMA MODULE *****/
// Note: Bits 8-15 are cleared when reading the Receiver Transfer Status

/// <summary>
/// Block End Status bit from DMA module (bit 8).
/// SINTRAN Usage: Tested as XBLDN for "External Block Done"
/// Meaning: Indicates DMA block processing complete
/// Impact: Controls block-level processing, not packet validation
/// </summary>
BlockEnd = 1 << 8,

/// <summary>
/// Frame End Status bit from DMA module (bit 9).
/// SINTRAN Usage: NOT directly referenced in packet processing
/// Meaning: Indicates end of HDLC frame 
/// Impact: DMA status only, not used for packet validation
/// </summary>
FrameEnd = 1 << 9,

/// <summary>
/// List End Status bit from DMA module (bit 10).
/// SINTRAN Usage: NOT directly referenced in receiver processing
/// (Used in transmitter with BSKP ONE 10 test for skip operations)
/// Meaning: Indicates end of DMA descriptor list
/// Impact: DMA status only, not used for receiver packet validation
/// </summary>
ListEnd = 1 << 10,

/// <summary>
/// List Empty Status bit from DMA module (bit 11).
/// SINTRAN Usage: CRITICAL - EMTY test in HIINT causes receiver shutdown
/// Logic: IF HASTA/\"EMTY" >< 0 THEN 0=:ACTSW (force device inactive)
/// Meaning: NO receive buffers available in DMA list
/// Impact: FATAL - Forces receiver shutdown, stops all packet processing
/// Note: This is a SYSTEM FAILURE condition, not a packet indicator
/// </summary>
ListEmpty = 1 << 11,

// Bit 12: Reserved/unused in SINTRAN receiver processing

/// <summary>
/// X21D - X.21 Data Indication Error (bit 13)
/// SINTRAN Usage: Part of HX21M mask (0x6000) for X.21 error detection  
/// Logic: IF A/\ HX21M >< 0 THEN (triggers X.21 error handling)
/// Meaning: X.21 protocol data indication problem
/// Impact: Triggers protocol error handling, may terminate connection
/// </summary>
X21D = 1 << 13,

/// <summary>
/// X21S - X.21 Call Setup/Clear Indication (bit 14)  
/// SINTRAN Usage: Part of HX21M mask (0x6000) for X.21 error detection
/// Logic: IF A/\ HX21M >< 0 THEN (triggers X.21 error handling)  
/// Meaning: X.21 protocol call setup/clear indication
/// Impact: Triggers connection termination procedures
/// NOTE: This is NOT "X.21 Clear" - that meaning was incorrect
/// </summary>
X21S = 1 << 14,

/// <summary>
/// Receiver Overrun (bit 15)
/// SINTRAN Usage: Not directly referenced in receiver interrupt processing
/// Meaning: Receiver buffer overrun condition
/// Impact: Not checked in main packet validation logic
/// </summary>  
ReceiverOverrun = 1 << 15
```

## SINTRAN Packet Validation Logic - Complete Analysis

### Step 1: Data Available Check (CRITICAL)
```assembly
% From HIINT Phase 5:
IF A NBIT 0 OR A/\60000><0 THEN              % No data OR X.21 error recheck
   GO OUT1                                   % Drop packet
FI
```

**Translation**: 
- `A NBIT 0` = DataAvailable (bit 0) is CLEAR → **DROP PACKET**
- `A/\60000><0` = X.21 error bits (13-14) are SET → **DROP PACKET**  
- Only proceed to `PROCPKT` if **both conditions pass**

### Step 2: Buffer Availability Check (FATAL)
```assembly
% From HIINT Phase 4:  
IF HASTA/\"EMTY" >< 0 THEN                   % List empty check (bit 11)
   0=:ACTSW                                  % *** STOP RECEIVER ***
   STPCNT+1=:STPCNT                          % Count stop events
   GO OUT1                                   % Exit - no buffers
FI
```

**Translation**: If ListEmpty (bit 11) is set → **SHUTDOWN RECEIVER** (fatal condition)

### Step 3: X.21 Protocol Check
```assembly
% From HIINT Phase 3:
IF A/\ HX21M >< 0 THEN                       % X.21 error check (bits 13-14)
   CALL X21ERR                               % Handle protocol error  
   GO OUT1                                   % Exit
FI
```

**Translation**: If X21D (bit 13) OR X21S (bit 14) are set → **PROTOCOL ERROR**

## What Your Emulator Should Do

### For NORMAL Packet Reception:
```csharp
// Set ONLY the DataAvailable bit:
ReceiverStatusBits status = ReceiverStatusBits.DataAvailable;  // 0x0001

// Do NOT set any of these:
// - BlockEnd (bit 8) - Not required for packet indication  
// - FrameEnd (bit 9) - Not required for packet indication
// - ListEnd (bit 10) - Not required for packet indication
// - ListEmpty (bit 11) - Would cause receiver shutdown!
// - X21D (bit 13) - Would trigger protocol error
// - X21S (bit 14) - Would trigger protocol error
// - ReceiverOverrun (bit 15) - Not checked anyway

return (ushort)status;  // Returns 0x0001
```

### For BUFFER EXHAUSTION (System Problem):
```csharp
// Set DataAvailable + ListEmpty to trigger receiver shutdown:
ReceiverStatusBits status = ReceiverStatusBits.DataAvailable | 
                           ReceiverStatusBits.ListEmpty;

return (ushort)status;  // Returns 0x0801 - causes receiver shutdown
```

### For X.21 PROTOCOL ERROR:
```csharp
// Set DataAvailable + X.21 error bit:
ReceiverStatusBits status = ReceiverStatusBits.DataAvailable | 
                           ReceiverStatusBits.X21D;  // or X21S

return (ushort)status;  // Returns 0x2001 or 0x4001 - triggers protocol error
```

## Key Insights

### 1. DMA Status Bits (8-10) Are NOT Packet Indicators
- **BlockEnd (8)**, **FrameEnd (9)**, **ListEnd (10)** are **DMA operational status**
- SINTRAN does **NOT** use these bits to determine packet validity
- These bits are **cleared on read** and represent DMA module state
- **Do not set these for normal packet reception**

### 2. DataAvailable (bit 0) Is the PRIMARY Packet Indicator  
- **ONLY bit that MUST be set** for packet processing
- **SINTRAN drops packets** if this bit is clear
- **This is your "packet ready" signal**

### 3. ListEmpty (bit 11) Is a FATAL System Condition
- **NOT a packet indicator** - it's a **system failure flag**
- Setting this bit **shuts down the entire receiver**
- **Only set if emulating system buffer exhaustion**

### 4. X.21 Bits (13-14) Are Protocol Error Indicators
- **NOT success indicators** - they trigger **error handling**
- **Do not set for normal packet reception**
- **Only set if emulating X.21 protocol problems**

## Deep Analysis: DMA Status Bits vs. Packet Indicators

### Critical Understanding: BlockEnd vs FrameEnd vs ListEnd vs ListEmpty

Based on SINTRAN source analysis, these DMA status bits have **very different meanings** and usage patterns:

#### BlockEnd (Bit 8) - DMA Block Completion
```assembly
% SINTRAN Usage Pattern:
IF A BIT BLDON THEN                              % Block done check
   % Process completed DMA block
FI
```
**Purpose**: Indicates a **single DMA buffer/block** has been processed
**Scope**: One block in a multi-block transfer
**HDLC Context**: One receive buffer filled, but packet may span multiple blocks
**Emulator Usage**: Set when individual receive buffer is filled, regardless of packet completeness

#### FrameEnd (Bit 9) - HDLC Frame Completion  
```assembly
% SINTRAN Usage: Limited direct usage in packet processing
% More relevant for hardware frame boundary detection
```
**Purpose**: Indicates **HDLC frame boundary** detected by hardware
**Scope**: Single HDLC frame (flag-to-flag)
**HDLC Context**: Hardware detected end-of-frame flag sequence
**Emulator Usage**: Set when HDLC frame is complete (FCS validated, closing flag detected)

#### ListEnd (Bit 10) - DMA Descriptor List Exhausted
```assembly
% SINTRAN Usage Pattern (more common in transmitter):
IF BSKP ONE 10 THEN                             % List end check
   % Handle end of DMA descriptor list
FI
```
**Purpose**: Indicates **entire DMA descriptor list** has been processed
**Scope**: Multiple blocks/buffers in linked list structure
**HDLC Context**: All allocated receive buffers have been used
**Emulator Usage**: Set when running out of pre-allocated DMA descriptors

#### ListEmpty (Bit 11) - FATAL System Condition
```assembly
% SINTRAN Usage Pattern (CRITICAL):
IF HASTA/\"EMTY" >< 0 THEN                      % List empty check
   0=:ACTSW                                     % *** STOP RECEIVER ***
   STPCNT+1=:STPCNT                             % Count stop events
FI
```
**Purpose**: Indicates **NO receive buffers available** (system failure)
**Scope**: System-wide buffer exhaustion
**HDLC Context**: Cannot receive any more packets - catastrophic condition
**Emulator Usage**: Set ONLY to simulate system buffer starvation

### DMA Status Bit Hierarchy and Relationships

```
DMA OPERATION LEVELS (from smallest to largest scope):

BlockEnd (bit 8)     ─── Single buffer/block completed
    │
    ├─ FrameEnd (bit 9)   ─── HDLC frame boundary detected  
    │
    ├─ ListEnd (bit 10)   ─── All DMA descriptors processed
    │
    └─ ListEmpty (bit 11) ─── NO buffers available (FATAL)

NORMAL PROGRESSION:
1. BlockEnd: Buffer full → next buffer
2. FrameEnd: Frame complete → packet processing  
3. ListEnd: All buffers used → allocate more
4. ListEmpty: No buffers left → STOP RECEIVER
```

## HDLC Controller Emulation Strategy - Complete Guide

### Understanding Interrupt-Enabled Bit Setting

Based on the WRTC interrupt enable analysis, your HDLC controller must consider **which interrupts are enabled** before setting status bits:

#### WRTC Enable Pattern Analysis
```csharp
// From SINTRAN analysis: WRTC = 1734 (octal) = 0x3DC enables these interrupts:
//
// Bit Pattern of 1734 (octal) = 0x3DC:
// 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
//  0  0  1  1  1  1  0  1  1  1  0  0  0  0  0  0
//              ^  ^  ^  ^     ^
// Enabled:    │  │  │  │     └─ Bit 8: BlockEnd interrupt enable
//             │  │  │  └────── Bit 9: FrameEnd interrupt enable  
//             │  │  └───────── Bit 10: ListEnd interrupt enable
//             │  └──────────── Bit 11: ListEmpty interrupt enable
//             └─────────────── Bits 13-14: X.21 error interrupt enables
```

### Packet Reception Scenarios for HDLC Emulator

#### Scenario 1: Normal Single-Block Packet Reception
```csharp
/// <summary>
/// Normal packet that fits in one DMA buffer
/// </summary>
public ushort HandleSingleBlockPacketReceived()
{
    // SINTRAN expects ONLY DataAvailable for normal packets
    var status = ReceiverStatusBits.DataAvailable;  // Bit 0
    
    // DO NOT set BlockEnd, FrameEnd, ListEnd - these are DMA operational status
    // that SINTRAN doesn't use for packet validation
    
    return (ushort)status;  // 0x0001
}
```

#### Scenario 2: Multi-Block Packet Reception (Large Packet)
```csharp
/// <summary>
/// Large packet spanning multiple DMA buffers
/// </summary>
public ushort HandleMultiBlockPacketReceived(bool isLastBlock, bool isFrameComplete)
{
    var status = ReceiverStatusBits.None;
    
    if (!isLastBlock)
    {
        // Intermediate block - just indicate buffer filled
        // Set BlockEnd to show this buffer is complete
        status |= ReceiverStatusBits.BlockEnd;     // Bit 8
    }
    else
    {
        // Final block of packet
        status |= ReceiverStatusBits.DataAvailable; // Bit 0 - CRITICAL for SINTRAN
        
        if (isFrameComplete)
        {
            // Optionally set FrameEnd to indicate HDLC frame boundary
            // (Not required by SINTRAN but provides hardware detail)
            status |= ReceiverStatusBits.FrameEnd;  // Bit 9
        }
    }
    
    return (ushort)status;
}
```

#### Scenario 3: Buffer List Exhaustion Simulation
```csharp
/// <summary>
/// Simulate system running out of receive buffers
/// </summary>
public ushort HandleBufferExhaustion()
{
    // Set DataAvailable + ListEmpty to trigger controlled shutdown
    var status = ReceiverStatusBits.DataAvailable |  // Bit 0 - packet data
                 ReceiverStatusBits.ListEmpty;        // Bit 11 - no more buffers
    
    // This causes SINTRAN to:
    // 1. Process the current packet (DataAvailable = 1)
    // 2. Shutdown receiver (ListEmpty = 1 → ACTSW = 0)
    // 3. Increment STPCNT counter
    
    return (ushort)status;  // 0x0801
}
```

#### Scenario 4: X.21 Protocol Error Simulation
```csharp
/// <summary>
/// Simulate X.21 protocol errors
/// </summary>
public ushort HandleX21ProtocolError(X21ErrorType errorType)
{
    var status = ReceiverStatusBits.DataAvailable;  // Still have data to process
    
    switch (errorType)
    {
        case X21ErrorType.DataIndication:
            status |= ReceiverStatusBits.X21D;       // Bit 13
            break;
            
        case X21ErrorType.CallSetupClear:
            status |= ReceiverStatusBits.X21S;       // Bit 14
            break;
            
        case X21ErrorType.Both:
            status |= ReceiverStatusBits.X21D | ReceiverStatusBits.X21S;
            break;
    }
    
    // This causes SINTRAN to:
    // 1. Call X21ERR error handler
    // 2. Possibly terminate connection
    // 3. NOT process packet data
    
    return (ushort)status;  // 0x2001, 0x4001, or 0x6001
}
```

#### Scenario 5: Receiver Overrun Simulation
```csharp
/// <summary>
/// Simulate receiver hardware overrun condition
/// </summary>
public ushort HandleReceiverOverrun()
{
    // Set ReceiverOverrun but still indicate data available
    var status = ReceiverStatusBits.DataAvailable |    // Bit 0
                 ReceiverStatusBits.ReceiverOverrun;   // Bit 15
                 
    // Note: SINTRAN doesn't explicitly check bit 15 in HIINT,
    // but it may be logged for diagnostic purposes
    
    return (ushort)status;  // 0x8001
}
```

### Advanced DMA Status Bit Usage

#### When to Set BlockEnd (Bit 8)
```csharp
/// <summary>
/// Set BlockEnd when individual DMA buffer is filled
/// </summary>
public bool ShouldSetBlockEnd(DmaBuffer buffer)
{
    // Set BlockEnd when:
    // 1. DMA buffer is completely filled
    // 2. Not the final buffer of the packet
    // 3. More buffers needed to complete packet
    
    return buffer.IsFull && !buffer.IsLastInPacket;
}
```

#### When to Set FrameEnd (Bit 9)
```csharp
/// <summary>
/// Set FrameEnd when HDLC frame boundary is detected
/// </summary>
public bool ShouldSetFrameEnd(HdlcFrame frame)
{
    // Set FrameEnd when:
    // 1. HDLC closing flag detected
    // 2. Frame Check Sequence validated
    // 3. Complete HDLC frame received
    
    return frame.HasClosingFlag && frame.FcsValid;
}
```

#### When to Set ListEnd (Bit 10)  
```csharp
/// <summary>
/// Set ListEnd when DMA descriptor list is exhausted
/// </summary>
public bool ShouldSetListEnd(DmaDescriptorList descriptorList)
{
    // Set ListEnd when:
    // 1. All pre-allocated DMA descriptors used
    // 2. Need to allocate additional descriptor list
    // 3. Not a fatal condition (unlike ListEmpty)
    
    return descriptorList.AllDescriptorsUsed && descriptorList.CanAllocateMore;
}
```

### Interrupt Generation Logic for Emulator

```csharp
/// <summary>
/// Complete interrupt generation logic based on WRTC enables
/// </summary>
public void CheckAndGenerateReceiverInterrupt(ushort wrtcValue, ushort rrtsStatus)
{
    // Only generate interrupt if corresponding enable bit is set
    
    // Check DataAvailable interrupt (bit 0)
    if ((wrtcValue & 0x0001) && (rrtsStatus & 0x0001))
    {
        TriggerReceiverInterrupt("DataAvailable");
    }
    
    // Check BlockEnd interrupt (bit 8) 
    if ((wrtcValue & 0x0100) && (rrtsStatus & 0x0100))
    {
        TriggerReceiverInterrupt("BlockEnd");
    }
    
    // Check FrameEnd interrupt (bit 9)
    if ((wrtcValue & 0x0200) && (rrtsStatus & 0x0200))
    {
        TriggerReceiverInterrupt("FrameEnd");
    }
    
    // Check ListEnd interrupt (bit 10)
    if ((wrtcValue & 0x0400) && (rrtsStatus & 0x0400))
    {
        TriggerReceiverInterrupt("ListEnd");
    }
    
    // Check ListEmpty interrupt (bit 11) - CRITICAL
    if ((wrtcValue & 0x0800) && (rrtsStatus & 0x0800))
    {
        TriggerReceiverInterrupt("ListEmpty - FATAL");
    }
    
    // Check X.21 error interrupts (bits 13-14)
    if ((wrtcValue & 0x6000) && (rrtsStatus & 0x6000))
    {
        TriggerReceiverInterrupt("X21 Protocol Error");
    }
    
    // Check ReceiverOverrun interrupt (bit 15)
    if ((wrtcValue & 0x8000) && (rrtsStatus & 0x8000))
    {
        TriggerReceiverInterrupt("ReceiverOverrun");
    }
}
```

## Conclusion - Updated Understanding

**For successful packet reception, your emulator should:**

1. **Primary Pattern**: Return `0x0001` (DataAvailable only) for normal packets
2. **Consider DMA Status**: Set BlockEnd/FrameEnd/ListEnd based on actual DMA operation state
3. **Respect Interrupt Enables**: Only generate interrupts for WRTC-enabled bits  
4. **Fatal Conditions**: Use ListEmpty (0x0800) only for system buffer exhaustion
5. **Error Conditions**: Use X.21 bits (0x6000) only for protocol error simulation

The key insight is that **DataAvailable (bit 0) is the packet indicator**, while **DMA status bits (8-11) represent the operational state** of the DMA system. SINTRAN's packet validation logic focuses on the packet indicator and error bits, not the DMA operational status.