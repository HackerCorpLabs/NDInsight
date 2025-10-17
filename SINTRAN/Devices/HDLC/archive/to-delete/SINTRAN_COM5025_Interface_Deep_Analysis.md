# SINTRAN to COM5025 Interface Deep Analysis

## Overview

This document analyzes how SINTRAN communicates with the COM5025 Multi-Protocol Communications Controller chip through the DMA descriptor system. The breakthrough discovery is that SINTRAN directly embeds COM5025 register values in DMA descriptors, providing explicit hardware control.

## Architecture: SINTRAN → DMA → COM5025

```
SINTRAN OS
    ↓ (Sets up DMA descriptor with LKEY)
DMA Hardware
    ↓ (Reads LKEY bits 7-0 and writes to COM5025)
COM5025 Chip
    ↓ (Controls HDLC framing based on register values)
Physical Line
```

## DMA Descriptor LKEY Field Structure

### Complete LKEY Layout:
```
Bits 15-14: Extended control
Bits 13-11: Reserved/additional control
Bit  10:    Legal Key flag (must be 1)
Bits 9-8:   Block status (Empty=01, Full=11, ToTransmit=10)
Bits 7-0:   COM5025 register value (direct chip programming)
```

### Block Status Encoding (Bits 10-8):
```
Key Bits   Octal   Meaning
010        1000₈   Empty Receiver Block
011        1400₈   Full Receiver Block  
100        2000₈   Block to be Transmitted
101        2400₈   Already Transmitted Block
110        3000₈   New List Pointer
```

### COM5025 Control Bits (Bits 7-0):
```
Bit 0: TSOM (Transmit Start of Message) - Generate opening FLAG
Bit 1: TEOM (Transmit End of Message) - Generate closing FLAG + CRC
Bit 2: TABORT (Transmit Abort) - Send ABORT character
Bit 3: TGA (Transmit Go Ahead) - Send GA character
Bits 4-7: Additional COM5025 control functions
```

## SINTRAN DMA Descriptor Setup Examples

### 1. Single Frame Transmission (FSERM):
```assembly
FSERM = 002003₈  % Used at line 103675: FSERM=:X.LKEY

Breakdown:
002003₈ = 0000 1000 0000 0011
          ├─ 010: Block to be transmitted
          └─ 003: COM5025 TSOM(1) + TEOM(1) = complete frame
```

**Hardware Action:**
1. DMA reads descriptor, sees 010 = transmit block
2. DMA writes 0x03 to COM5025 control register
3. COM5025 generates opening FLAG (TSOM=1)
4. COM5025 transmits data from buffer
5. COM5025 generates closing FLAG + CRC (TEOM=1)

### 2. Multi-Block Frame Transmission (Expected):
```assembly
% First block:
FirstBlock = 002001₈
            ├─ 010: Block to be transmitted  
            └─ 001: COM5025 TSOM(1) only

% Middle blocks:
MiddleBlock = 002000₈
             ├─ 010: Block to be transmitted
             └─ 000: COM5025 no special flags

% Final block:
LastBlock = 002002₈
           ├─ 010: Block to be transmitted
           └─ 002: COM5025 TEOM(1) only
```

### 3. Receiver Buffer Setup (Expected):
```assembly
% Empty receive buffer:
EmptyRX = 001000₈
         ├─ 010: Empty receiver block
         └─ 000: COM5025 receive mode flags

% After reception with complete frame:
FullRX = 001403₈
        ├─ 011: Full receiver block
        └─ 403: COM5025 status - RSOM + REOM detected
```

## COM5025 Register Integration

### Transmission Control Register Mapping:
```
COM5025 Transmitter Control/Status Register:
Bit 0: TSOM - Transmit Start of Message
Bit 1: TEOM - Transmit End of Message  
Bit 2: TABORT - Transmit Abort
Bit 3: TGA - Transmit Go Ahead
Bit 4: Reserved
Bit 5: Reserved
Bit 6: Reserved  
Bit 7: Reserved
```

### Reception Status Register Mapping:
```
COM5025 Receiver Status Register:
Bit 0: RSOM - Received Start of Message (FLAG detected)
Bit 1: REOM - Received End of Message (closing FLAG detected)
Bit 2: Reserved
Bit 3: Reserved
Bits 4-7: Frame status and error indicators
```

## Hardware Status Bit Generation

### How RTTS/RRTS Gets Set:

```
COM5025 Operation → Hardware Status Logic → RTTS/RRTS Register

Examples:
1. COM5025 completes buffer transmission → Set BlockEnd (bit 8)
2. COM5025 completes TEOM sequence → Set FrameEnd (bit 9)
3. COM5025 detects transmission error → Set TXUND (bit 1) or SILFO (bit 15)
4. COM5025 buffer underrun → Set TXUND (bit 1)
5. COM5025 receives complete frame → Set DataAvailable (bit 0) in RRTS
```

## SINTRAN Integration Points

### 1. DMA Descriptor Creation:
```assembly
% At transmission setup (line 103675):
FSERM=:X.LKEY                    % Set COM5025 control bits directly
A:=OMSG+CHEAD=:X.LMEM2          % Set buffer address
T:=MASTB=:X.LMEM1               % Set memory bank
A-DISP1=:X.LBYTC                % Set byte count
```

### 2. DMA Initiation:
```assembly
% Start transmitter DMA (XHMST):
LIINT+DPITPHYS;                  % Calculate descriptor address
T:=HDEV+WDMA; *IOF; EXR ST      % Write DMA address to hardware
A:=2000\D; T+"WDCR-WDMA"; *EXR ST % Start DMA with command 2000₈
```

### 3. Interrupt Processing:
```assembly
% In HOINT (transmitter interrupt):
T:=HDEV+RTTS; *EXR ST           % Read final transmission status
A=:HASTAT                       % Store for analysis
IF A/\ "SILFO+TXUND" = 0 THEN   % Check COM5025 error flags
   % Success path
ELSE
   % Retry/error path  
FI
```

## HDLC Emulator Implementation

### Reading SINTRAN's Intentions:
```csharp
public class SintranDMADescriptorReader
{
    public struct DMADescriptor 
    {
        public ushort LKEY;      // Control + COM5025 bits
        public ushort LBYTC;     // Byte count
        public ushort LMEM1;     // Address high
        public ushort LMEM2;     // Address low
    }

    public FrameControlInfo ReadFrameControl(uint dmaAddress)
    {
        var desc = ReadDMADescriptor(dmaAddress);
        
        // Extract block control (bits 10-8)
        int blockControl = (desc.LKEY >> 8) & 0x07;
        
        // Extract COM5025 control bits (bits 7-0)
        byte com5025Bits = (byte)(desc.LKEY & 0xFF);
        
        return new FrameControlInfo
        {
            BlockType = (BlockType)blockControl,
            HasTSOM = (com5025Bits & 0x01) != 0,
            HasTEOM = (com5025Bits & 0x02) != 0,
            HasAbort = (com5025Bits & 0x04) != 0,
            HasGoAhead = (com5025Bits & 0x08) != 0
        };
    }

    public ushort GenerateRTTSStatus(FrameControlInfo control, bool success)
    {
        ushort status = 0x0000;
        
        // Always set BlockEnd when block completes
        status |= 0x0100;  // BlockEnd (bit 8)
        
        // Set FrameEnd only when TEOM was specified
        if (control.HasTEOM) {
            status |= 0x0200;  // FrameEnd (bit 9)
        }
        
        // Set error flags if transmission failed
        if (!success) {
            status |= 0x8000;  // SILFO (bit 15) - format error
            // or status |= 0x0002;  // TXUND (bit 1) - underrun
        }
        
        return status;
    }
}
```

### Multi-Block Frame Handling:
```csharp
public void ProcessTransmissionSequence(uint[] dmaAddresses)
{
    foreach (uint addr in dmaAddresses)
    {
        var control = ReadFrameControl(addr);
        
        if (control.HasTSOM) {
            // Start new frame transmission
            StartHDLCFrame();
        }
        
        // Transmit block data
        TransmitBlockData(addr);
        
        if (control.HasTEOM) {
            // End frame transmission  
            EndHDLCFrame();
            SetRTTSBit("FrameEnd + BlockEnd");
        } else {
            // More blocks to follow
            SetRTTSBit("BlockEnd");
        }
    }
}
```

## Implications for Debugging

### Root Cause Resolution:
1. **Frame boundaries are explicit** - no guessing needed
2. **X.21 protocol handling** is correctly isolated to bit 14
3. **COM5025 integration** provides hardware realism
4. **Multi-block frame support** is built into the architecture

### Testing Strategy:
```csharp
// Test single frame:
AssertDescriptor(0x2003, hasFrameEnd: true);   // TSOM+TEOM

// Test multi-block sequence:
AssertDescriptor(0x2001, hasFrameEnd: false);  // TSOM only (first)
AssertDescriptor(0x2000, hasFrameEnd: false);  // Neither (middle)  
AssertDescriptor(0x2002, hasFrameEnd: true);   // TEOM only (last)
```

This architecture explains why SINTRAN's HDLC implementation is robust - it explicitly controls every aspect of COM5025 operation through the DMA descriptor system, eliminating ambiguity about frame boundaries and hardware states.