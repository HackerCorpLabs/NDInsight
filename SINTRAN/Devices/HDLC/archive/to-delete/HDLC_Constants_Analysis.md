# HDLC Critical Constants Analysis

## Constants Found in SYMBOL-1-LIST.SYMB.TXT

| Constant | Octal Value | Hex Value | Binary | Bit Position | Description |
|----------|-------------|-----------|---------|--------------|-------------|
| TXUND | 000002 | 0x0002 | 0000000000000010 | Bit 1 | Transmitter Underrun |
| EMTY | 004000 | 0x0800 | 0000100000000000 | Bit 11 | Empty Buffer/List |
| ERB | 001000 | 0x0200 | 0000001000000000 | Bit 9 | Error Block |
| EUND | 000102 | 0x0042 | 0000000001000010 | Bits 1,6 | Underrun Error Code |
| BLDON | 000010 | 0x0008 | 0000000000001000 | Bit 3 | Block Done |
| SILFO | 100000 | 0x8000 | 1000000000000000 | Bit 15 | Signal Flag Output |
| HX21M | 060000 | 0x6000 | 0110000000000000 | Bits 13-14 | X.21 Error Mask |
| HX21S | 000016 | 0x000E | 0000000000001110 | Bits 1-3 | X.21 Status |
| XBLDN | 000010 | 0x0008 | 0000000000001000 | Bit 3 | External Block Done |

## Critical Analysis - The Retransmission Bug!

### SILFO+TXUND Mask Analysis
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN
```

**SILFO+TXUND = SILFO | TXUND = 0x8000 | 0x0002 = 0x8002**

This means the condition tests:
- **SILFO (Bit 15) = 0x8000** - Signal Flag Output 
- **TXUND (Bit 1) = 0x0002** - Transmitter Underrun

### 🚨 MAJOR DISCOVERY - Logic Error Found!

```csharp
// Current SINTRAN logic:
if ((hastat & 0x8002) == 0) 
{
    // SUCCESS PATH - both SILFO and TXUND are CLEAR
    clearRetryState();
    logSuccess();
}
else 
{
    // ERROR PATH - either SILFO or TXUND is SET
    // This causes RETRANSMISSION!
    logError();
    retransmitPacket();
}
```

**The Problem:**
- **SILFO (Bit 15)** appears to be a status flag, not an error flag
- **TXUND (Bit 1)** matches **TransmitterUnderrun** from the C# enum
- The logic treats **ANY** set bit as an error condition

### Comparison with C# Enum:
- **TXUND (0x0002)** = **TransmitterUnderrun (Bit 1)** ✅ Correct
- **SILFO (0x8000)** = **Illegal (Bit 15)** from TransmitterStatusBits ✅ This IS an error!

## Receiver Constants Analysis

### EMTY (Empty Buffer)
- **EMTY = 0x0800 = Bit 11** matches **ListEmpty** from ReceiverStatusBits ✅
- Used in: `IF HASTAT/\"EMTY" >< 0 THEN` - stops receiver if no buffers

### X.21 Protocol Constants  
- **HX21M = 0x6000 = Bits 13-14** matches **X21D | X21S** ✅
- **HX21S = 0x000E = Bits 1-3** ❌ **MISMATCH!**

**X.21 Issue:**
```csharp
// From C# enum: X21S should be bit 14 = 0x4000
// From constants: HX21S = 0x000E = bits 1-3
// This is a MAJOR discrepancy!
```

### Block Control Constants
- **BLDON = 0x0008 = Bit 3** - Block done flag
- **XBLDN = 0x0008 = Bit 3** - Same as BLDON (likely different context)

## Root Cause Analysis

### 1. Transmission Success Logic ✅ CORRECT
The `SILFO+TXUND` logic is actually **CORRECT**:
```csharp
// SILFO (0x8000) = Illegal format/key error (Bit 15)
// TXUND (0x0002) = Transmitter underrun (Bit 1)
// Both are ERROR conditions

if ((rtts & 0x8002) == 0) 
{
    // SUCCESS: No illegal format AND no underrun
    success();
}
else 
{
    // ERROR: Either illegal format OR underrun
    retransmit();
}
```

### 2. X.21 Status Mismatch ❌ POTENTIAL BUG
```csharp
// Expected: HX21S = X21S (bit 14) = 0x4000
// Actual:   HX21S = 0x000E (bits 1-3)

// Current logic:
if ((rrts & 0x000E) != 0) // Tests bits 1-3
{
    setBlockDone(); // Wrong bits being tested!
}

// Should be:
if ((rrts & 0x4000) != 0) // Test bit 14 (X21S)
{
    setBlockDone();
}
```

## Updated Bit Usage Analysis

### RTTS (Transmitter) - CORRECT Logic
```csharp
ushort rtts = ReadRTTS();

// Check for errors (CORRECT)
if ((rtts & 0x8002) != 0) // SILFO (Illegal) OR TXUND (Underrun)
{
    retransmitPacket(); // Correct response to errors
}
else
{
    transmissionSuccess(); // No errors detected
}
```

### RRTS (Receiver) - X.21 Logic Issue  
```csharp
ushort rrts = ReadRRTS();

// X.21 error check (POTENTIALLY WRONG)
if ((rrts & 0x6000) != 0) // HX21M - checks bits 13-14 ✅ CORRECT
{
    handleX21Error();
    
    // X.21 clear indication check (WRONG BITS)
    if ((rrts & 0x000E) != 0) // HX21S - should be 0x4000 (bit 14)
    {
        setBlockDone(); // May trigger incorrectly
    }
}
```

## Recommendations

1. **Verify X.21 Clear Indication Logic** - HX21S constant seems wrong
2. **SILFO+TXUND logic is actually CORRECT** - both are error conditions  
3. **Check if HX21S should be 0x4000 instead of 0x000E**
4. **The retransmission issue may be elsewhere** - not in the SILFO+TXUND logic

The transmission logic appears correct based on these constants. The issue might be in the **X.21 status interpretation** or **timing of status reads**.