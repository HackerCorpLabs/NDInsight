# Final HDLC Bug Analysis - With Actual Constants

## Critical Constants (Octal → Hex → Binary)

| Constant | Octal | Hex | Binary | Bit(s) | Meaning |
|----------|--------|-----|---------|---------|----------|
| TXUND | 000002 | 0x0002 | bit 1 | TransmitterUnderrun |
| SILFO | 100000 | 0x8000 | bit 15 | Illegal Format/Key Error |
| EMTY | 004000 | 0x0800 | bit 11 | List Empty |
| HX21M | 060000 | 0x6000 | bits 13-14 | X.21 Error Mask |
| HX21S | 000016 | 0x000E | bits 1,2,3 | X.21 Status (NOT bit 14!) |
| BLDON | 000010 | 0x0008 | bit 3 | Block Done |
| XBLDN | 000010 | 0x0008 | bit 3 | External Block Done |
| ERB | 001000 | 0x0200 | bit 9 | Error Block |
| EUND | 000102 | 0x0042 | bits 1,6 | Underrun Error Code |

## Transmission Logic Analysis - CORRECT!

### SILFO+TXUND Check (Line 104046)
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN    % IF (A AND 0x8002) == 0
   XRETRY=:RTDYN; A:=0; CALL SADTS  % SUCCESS PATH
ELSE
   A:=HASTAT; CALL SADTS; CALL DRERR  % ERROR PATH  
   A:=EUND
FI
```

**Analysis:**
```csharp
// SILFO+TXUND = 0x8000 | 0x0002 = 0x8002
ushort rtts = ReadRTTS();

if ((rtts & 0x8002) == 0)  // No illegal format AND no underrun
{
    // SUCCESS - transmission completed successfully
    clearRetryState();
    logSuccess();
}
else
{
    // ERROR - either illegal format OR underrun occurred
    logError();
    incrementErrorCounter();
    retransmitPacket();
}
```

**This logic is CORRECT!** 
- SILFO (0x8000) = Illegal format error (should cause retransmission)
- TXUND (0x0002) = Transmitter underrun (should cause retransmission)
- Only when BOTH are clear (0) is transmission successful

## Reception Logic Analysis - MAJOR ISSUE FOUND!

### X.21 Error Detection (Line 104450)
```assembly
IF A/\ HX21M >< 0 THEN          % IF (A AND 0x6000) != 0
   % Handle X.21 error
   IF A BIT HX21S THEN          % IF A has ANY of bits 1,2,3 set
      HASTAT BONE BLDON=:HASTAT % Set block done flag
      LIINT.LKEY BONE XBLDN=:X.LKEY
   FI
FI
```

**🚨 CRITICAL BUG FOUND:**
```csharp
// X.21 error mask check - CORRECT
if ((rrts & 0x6000) != 0)  // Check bits 13-14 for X.21 errors
{
    handleX21Error();
    
    // X.21 clear indication check - WRONG!
    if ((rrts & 0x000E) != 0)  // HX21S tests bits 1,2,3 (NOT bit 14!)
    {
        setBlockDone();  // Terminates connection
        return;
    }
}
```

**The Problem:**
- **HX21S = 0x000E** tests bits 1, 2, and 3
- These bits are **DataAvailable**, **StatusAvailable**, and **ReceiverActive**
- These are NORMAL operation bits, NOT X.21 clear indication!
- **Real X.21 Clear should be bit 14 (0x4000)**

**This means:**
- When normal data is available (bit 0 set), the X.21 clear logic triggers
- Connection gets terminated inappropriately
- Packets get dropped due to false X.21 clear indication

### List Empty Check (Line 104463)
```assembly
IF HASTAT/\"EMTY" >< 0 THEN     % IF (HASTAT AND 0x0800) != 0
   0=:ACTSW                     % Stop device
   MIN STPCNT                   % Increment stop counter
```

**Analysis:**
```csharp
if ((rrts & 0x0800) != 0)  // List Empty bit set
{
    stopDevice();           // Stops all reception
    incrementStopCounter();
    return;  // All packets dropped until restart
}
```

**This logic is CORRECT** - when no receive buffers available, stop processing.

### Data Available Check (Line 51083)
```assembly
IF A NBIT 0 OR A/\60000><0 THEN % IF bit 0 clear OR (A AND 0x6000) != 0
```

**Analysis:**
```csharp
if ((rrts & 0x0001) == 0 ||     // No data available
    (rrts & 0x6000) != 0)       // X.21 error (bits 13-14)
{
    return;  // Drop packet
}
```

**This logic is CORRECT** - only process when data available and no X.21 errors.

## ROOT CAUSE IDENTIFIED

### The Real Bug: HX21S Constant Wrong Value

**Expected X.21 Logic:**
```csharp
// Should be:
const ushort HX21S_CORRECT = 0x4000;  // Bit 14 - X.21 Clear Indication

if ((rrts & 0x6000) != 0)  // X.21 error detected
{
    if ((rrts & 0x4000) != 0)  // Actual X.21 clear indication
    {
        terminateConnection();  // Only when DCE sends clear
    }
}
```

**Actual (Buggy) Logic:**
```csharp
// Currently:
const ushort HX21S_WRONG = 0x000E;   // Bits 1,2,3 - Normal operation flags!

if ((rrts & 0x6000) != 0)  // X.21 error detected  
{
    if ((rrts & 0x000E) != 0)  // Tests DataAvailable, StatusAvailable, ReceiverActive
    {
        terminateConnection();  // Terminates on NORMAL data!
    }
}
```

## Impact on Your Issues

### Reception Problems:
1. **False X.21 Clear Triggering:** When data arrives (bit 0 set), HX21S test (0x000E) triggers connection termination
2. **Packets Dropped:** Connection terminated inappropriately, causing packet loss

### Transmission Problems:
- **Transmission logic is actually CORRECT** - the SILFO+TXUND check properly detects errors
- **Retransmission issue might be elsewhere** - possibly in timing or multiple status reads

## Recommended Fixes

1. **Fix HX21S Constant:**
   ```assembly
   HX21S=004000  % Should be bit 14, not bits 1-3
   ```

2. **Verify X.21 Clear Logic:**
   ```csharp
   if ((rrts & 0x6000) != 0)  // X.21 error
   {
       if ((rrts & 0x4000) != 0)  // Real X.21 clear (bit 14)
       {
           terminateConnection();
       }
   }
   ```

3. **Check Status Read Timing:**
   - Ensure status isn't read multiple times (clearing DMA bits)
   - Verify interrupt timing vs status register access

The **HX21S constant being 0x000E instead of 0x4000** is likely the primary cause of your packet reception issues!