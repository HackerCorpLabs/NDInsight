# CRITICAL CORRECTIONS: HX21S Bit Testing and KEY Field Interpretation

## BREAKTHROUGH DISCOVERIES

### 1. DMA LKEY Field = COM5025 Register Values

**Revolutionary Understanding:**
The DMA descriptor LKEY field's **low 8 bits contain actual COM5025 chip register values** that get written directly to the hardware. This is what the HDLC manual calls "Dataflow Cost."

```assembly
% DMA Descriptor LKEY Structure:
% Bits 15-8: Block control (Empty=010, Full=011, ToTransmit=100, etc.)
% Bits 7-0:  COM5025 register value (TSOM, TEOM, RSOM, REOM detection bits)

% Example - FSERM = 002003₈:
FSERM = 002003₈ = 0000 1000 0000 0011
        │        └─ 003: COM5025 TSOM(1) + TEOM(1) = complete frame
        └─ 010: Block to be transmitted
```

### 2. MASSIVE ERROR CORRECTED: HX21S Bit Testing Logic

**The Critical Misunderstanding:**
```assembly
HX21S = 000016₈  % This is 14 DECIMAL (bit number), not a bitmask!
```

**WRONG Interpretation (Previous):**
```assembly
IF A BIT HX21S THEN    % Incorrectly thought: "test bits 1,2,3"
% Actually means: "test bit 14"
```

**CORRECT Interpretation:**
```assembly
HX21S = 000016₈ = 14 decimal = bit position 14
IF A BIT HX21S THEN    % Means: "IF bit 14 in register A is 1 THEN"
```

## Impact on All Analysis Documents

### Before (WRONG):
- "HX21S tests receiver state bits 1,2,3"
- "HX21S = 0x000E is a bitmask"
- "Tests StatusAvailable, ReceiverActive, SyncFlagReceived"

### After (CORRECT):
- "HX21S tests X.21 Clear Indication bit 14"
- "HX21S = 14 (decimal) is a bit position number"
- "Tests X21S (Clear Indication) - exactly what the name suggests"

## Corrected SINTRAN Logic Flows

### Receiver Processing (HIINT):
```assembly
% CORRECTED X.21 error handling:
IF A/\ HX21M >< 0 THEN           % X.21 error detected (bits 13-14)
   IF A BIT HX21S THEN           % IF bit 14 (X21S Clear) is set
      HASTAT BONE BLDON=:HASTAT  % Terminate connection cleanly
   FI
FI
```

### C# Implementation (CORRECTED):
```csharp
// OLD (WRONG):
if ((rrts & 0x000E) != 0)  // Wrong - tested bits 1,2,3

// NEW (CORRECT):
if ((rrts & 0x4000) != 0)  // Correct - tests bit 14 (X21S Clear)
{
    // X.21 Clear Indication detected - terminate connection
    setBlockDone();
}
```

## Key Constants Corrected

| Constant | Value | Previous (Wrong) | Correct Interpretation |
|----------|-------|------------------|----------------------|
| HX21S | 000016₈ | Bitmask 0x000E (bits 1,2,3) | **Bit position 14** |
| BLDON | 000010₈ | Bitmask 0x0008 (bit 3) | **Bit position 8** |
| XBLDN | 000010₈ | Bitmask 0x0008 (bit 3) | **Bit position 8** |

**Pattern Recognition:**
SINTRAN constants ending in single digits are **bit position numbers**, not bitmasks!

## Frame Boundary Detection (SOLVED)

### How SINTRAN Controls FrameEnd vs BlockEnd:

```csharp
// Read DMA descriptor LKEY field:
ushort lkeyValue = ReadDMADescriptor(dmaAddress);

// Extract COM5025 control bits (low 8 bits):
byte com5025Bits = (byte)(lkeyValue & 0xFF);
bool hasTSOM = (com5025Bits & 0x01) != 0;  // Transmit Start of Message
bool hasTEOM = (com5025Bits & 0x02) != 0;  // Transmit End of Message

// Now you know exactly what to set:
if (hasTSOM && hasTEOM) {
    // FSERM case: Complete frame in single block
    SetRTTSBit("FrameEnd + BlockEnd");
} else if (hasTEOM) {
    // Final block of multi-block frame
    SetRTTSBit("FrameEnd + BlockEnd");
} else {
    // Intermediate block
    SetRTTSBit("BlockEnd");  // Frame continues
}
```

## Expected Multi-Block Frame Constants

Based on corrected understanding:
```assembly
% Single block (FSERM): Block + TSOM + TEOM
SingleFrame = 002003₈    % 010 (block) + 003 (TSOM=1, TEOM=1)

% Multi-block frame sequence:
FirstBlock = 002001₈     % 010 (block) + 001 (TSOM=1 only)
MiddleBlock = 002000₈    % 010 (block) + 000 (no flags)
LastBlock = 002002₈      % 010 (block) + 002 (TEOM=1 only)
```

## Root Cause Analysis Complete

### Why Reception Was Failing:
1. **Wrong HX21S interpretation** caused normal packets to trigger X.21 Clear termination
2. **DataAvailable + StatusAvailable + SyncFlagReceived** (bits 0,1,3) are NORMAL reception flags
3. **Testing these as "X.21 Clear"** terminated valid connections

### Why Transmission Issues Occurred:
1. **Unknown frame boundaries** - couldn't distinguish BlockEnd from FrameEnd
2. **COM5025 integration not understood** - LKEY contains the actual control bits
3. **Emulator assumptions** instead of reading SINTRAN's explicit instructions

## Impact on Debugging

**All previous analysis assuming HX21S = 0x000E is INVALID**

The correct logic means:
- X.21 Clear Indication is properly isolated to bit 14
- Normal reception bits (0,1,3) are not confused with protocol termination
- Frame boundary control is explicit in DMA descriptors
- HDLC emulator can read SINTRAN's exact intentions from LKEY field

This resolves the fundamental misunderstanding about both bit testing syntax and hardware integration!