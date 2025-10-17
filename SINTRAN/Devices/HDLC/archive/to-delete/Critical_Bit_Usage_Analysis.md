# Critical HDLC Bit Usage Analysis - Packet Loss & Retransmission Issues

## Problem Statement
- **Sending packets are being retransmitted** because OS gets wrong bits from RTTS
- **Received packets are not processed** because bits are not correctly set in RRTS
- Need to understand exactly how OS uses these bits to make send/receive decisions

## Critical RTTS (Transmitter) Bit Decisions

### 1. Transmission Success/Failure Detection
**Location: Line 104046 in symb file**
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN    % Check completion + underrun
   XRETRY=:RTDYN; A:=0; CALL SADTS    % SUCCESS: Clear retry, log success
ELSE
   A:=HASTAT; CALL SADTS; CALL DRERR  % FAILURE: Log error, increment counter
   A:=EUND                             % Set underrun error code
FI
```

**Critical Analysis:**
```csharp
// SILFO_TXUND = TransmissionFinished (bit 11) | TransmitterUnderrun (bit 1)
// = 0x0800 | 0x0002 = 0x0802

if ((rttsStatus & 0x0802) == 0)
{
    // SUCCESS CONDITION: Neither transmission finished NOR underrun
    // This seems WRONG - should check if transmission IS finished AND no underrun
    clearRetryState();
    logSuccess();
}
else
{
    // FAILURE CONDITION: Either transmission finished OR underrun occurred  
    // This causes RETRANSMISSION when transmission actually succeeded!
    logError();
    incrementErrorCounter();
    setUnderrunError();
}
```

**🚨 POTENTIAL BUG:** The logic appears inverted. When `TransmissionFinished` bit is set (indicating success), the OS treats it as an error and retransmits!

### 2. DMA List Completion Check
**Location: Line 51397**
```assembly
BSKP ONE 10 DA    % Skip next instruction if bit 10 is SET
```

```csharp
// Tests ListEnd (bit 10) = 0x0400
if ((rttsStatus & TransmitterStatusBits.ListEnd) != 0)
{
    // DMA list completed - skip next operation
    // If this bit interpretation is wrong, OS may not advance to next packet
}
```

### 3. Ready For Sending Check
**Multiple locations test bit 6**
```csharp
// Tests ReadyForSending (bit 6) = 0x0040  
if ((rttsStatus & TransmitterStatusBits.ReadyForSending) == 0)
{
    // DCE not ready - should wait before transmitting
    // Wrong interpretation could cause premature transmissions
    handleDCENotReady();
}
```

## Critical RRTS (Receiver) Bit Decisions

### 1. Data Availability Check
**Location: Line 51083**
```assembly
IF A NBIT 0 OR A/\60000><0 THEN    % NO DATA OR X21 ERROR?
    % Don't process packet
```

```csharp
// NBIT 0 tests DataAvailable (bit 0) = 0x0001
// A/\60000 tests upper bits including X21 errors (bits 13-14) = 0x6000

if ((rrtsStatus & ReceiverStatusBits.DataAvailable) == 0 || 
    (rrtsStatus & 0x6000) != 0)
{
    // CRITICAL: No data available OR X.21 error
    // Packet will NOT be processed
    return; // Drops the packet!
}
```

**🚨 POTENTIAL BUG:** If `DataAvailable` bit is incorrectly interpreted as clear when data is actually available, packets get dropped.

### 2. Buffer Empty Detection  
**Location: Line 104463**
```assembly
IF HASTAT/\"EMTY" >< 0 THEN      % LIST EMPTY?
   0=:ACTSW                      % DEVICE STOPPED  
   MIN STPCNT                    % INCREMENT STOP COUNTER
```

```csharp
// Tests ListEmpty (bit 11) = 0x0800
if ((rrtsStatus & ReceiverStatusBits.ListEmpty) != 0)
{
    // Stop receiver - no more buffers available
    stopDevice();
    incrementStopCounter();
    // All subsequent packets will be dropped until restart!
}
```

### 3. X.21 Protocol Error Handling
**Location: Line 104450**
```assembly
IF A/\ HX21M >< 0 THEN           % X21-ERROR?
   % Handle protocol error
   IF A BIT HX21S THEN           % X21 CLEAR INDICATION?
      HASTAT BONE BLDON=:HASTAT  % SET BLOCK DONE TO TERMINATE
   FI
FI
```

```csharp
// HX21M = X21D (bit 13) | X21S (bit 14) = 0x6000
if ((rrtsStatus & 0x6000) != 0)
{
    // X.21 protocol error detected
    handleX21Error();
    
    if ((rrtsStatus & ReceiverStatusBits.X21S) != 0)
    {
        // X.21 Clear Indication - terminate connection
        setBlockDone();
        return; // Stops all further packet processing!
    }
}
```

## Bit Interpretation Issues That Could Cause Your Problems

### RTTS Retransmission Issues:

1. **Inverted Success Logic**
   ```csharp
   // Current (possibly wrong):
   if ((status & 0x0802) == 0) { success(); } else { retransmit(); }
   
   // Should probably be:
   if ((status & TransmissionFinished) != 0 && (status & TransmitterUnderrun) == 0) 
   { 
       success(); 
   } 
   else { retransmit(); }
   ```

2. **DMA Request Bit Confusion**
   ```csharp
   // DMAModuleRequest (bit 4) is always read as 0
   // If OS expects this bit to indicate completion, it will always fail
   ```

3. **List End Misinterpretation**
   ```csharp
   // If ListEnd bit is inverted or misunderstood, 
   // OS may think more data to send when list is actually complete
   ```

### RRTS Packet Drop Issues:

1. **Data Available Bit Error**
   ```csharp
   // If DataAvailable (bit 0) is stuck at 0 or misinterpreted:
   if ((status & 0x0001) == 0) { dropPacket(); } // Drops all packets!
   ```

2. **List Empty False Positive**
   ```csharp
   // If ListEmpty (bit 11) incorrectly shows as set:
   if ((status & 0x0800) != 0) { stopReceiver(); } // Stops all reception!
   ```

3. **X.21 False Errors**
   ```csharp
   // If X21D/X21S bits (13-14) show false errors:
   if ((status & 0x6000) != 0) { handleError(); dropPacket(); }
   ```

## Debugging Strategy

### For RTTS (Transmission Issues):
1. **Log the exact RTTS value** when retransmission decision is made
2. **Check bit 11 (TransmissionFinished)** - should be SET after successful transmission
3. **Check bit 1 (TransmitterUnderrun)** - should be CLEAR for success
4. **Verify the mask 0x0802** is being applied correctly

### For RRTS (Reception Issues):  
1. **Log the exact RRTS value** when packets are dropped
2. **Check bit 0 (DataAvailable)** - should be SET when packet ready
3. **Check bits 13-14 (X21D/X21S)** - should be CLEAR for normal operation
4. **Check bit 11 (ListEmpty)** - if SET, no receive buffers available

## Hardware Register Read Timing

**Critical Timing Issue:**
```csharp
// From documentation: "DMA bits are cleared when reading the status"
// This means:
ReceiverStatusBits status1 = ReadRRTS();  // Gets full status
ReceiverStatusBits status2 = ReadRRTS();  // DMA bits now cleared!

// If OS reads status multiple times, second read will show different values!
```

## Recommended Fix Approach

1. **Add comprehensive status logging** before each send/receive decision
2. **Verify bit mask values** match hardware documentation exactly  
3. **Check for multiple status reads** that might clear important bits
4. **Test inverted logic** on the transmission success condition
5. **Monitor X.21 error bits** for false error conditions

The core issue is likely that the OS logic for interpreting these bits doesn't match the actual hardware behavior, causing successful transmissions to be seen as failures (triggering retransmits) and available data to be seen as unavailable (causing packet drops).