# HASTAT Bit Processing Analysis - RRTS/RTTS Storage and Usage

## HASTAT Variable Usage

HASTAT is the central status variable that stores both RRTS and RTTS values after hardware reads:

```assembly
% Receiver interrupt:
HIINT: T:=HDEV+RRTS; *EXR ST     % READ RECEIVER STATUS
       A=:HASTAT                 % SAVE RRTS STATUS IN HASTAT

% Transmitter interrupt:  
HOINT: T:=HDEV+RTTS; *EXR ST     % READ TRANSMITTER STATUS
       A=:HASTAT                 % SAVE RTTS STATUS IN HASTAT
```

**Key Point:** HASTAT contains the exact bit pattern from either RRTS or RTTS register, and all subsequent bit analysis operates on this stored value.

## HASTAT Processing After RRTS Read (Receiver Path)

### 1. X.21 Error Check (Line 104450)
```assembly
IF A/\ HX21M >< 0 THEN          % Check HASTAT for X.21 errors
```

**Bit Analysis:**
```csharp
// HASTAT now contains RRTS value (ReceiverStatusBits)
// HX21M = mask for bits 13-14 = 0x6000

ushort hastat = (ushort)ReadRRTS();  // HASTAT = RRTS value

if ((hastat & 0x6000) != 0)         // IF A/\ HX21M >< 0 THEN
{
    // X.21 protocol error in HASTAT
    // Bit 13 (X21D) = 0x2000 - X.21 data error  
    // Bit 14 (X21S) = 0x4000 - X.21 clear indication
    
    HandleX21Error();
    
    if ((hastat & 0x4000) != 0)     // IF A BIT HX21S THEN
    {
        // X.21 Clear Indication detected in HASTAT
        // HASTAT BONE BLDON=:HASTAT  % Set BLOCK DONE in HASTAT
        hastat |= BLDON_FLAG;        // Modify HASTAT directly
        SetBlockDone();
        return; // Stop packet processing
    }
}
```

### 2. Buffer Empty Check (Line 104463)  
```assembly
IF HASTAT/\"EMTY" >< 0 THEN     % Check HASTAT for list empty
```

**Bit Analysis:**
```csharp
// "EMTY" = ListEmpty bit = bit 11 = 0x0800

if ((hastat & 0x0800) != 0)         // IF HASTAT/\"EMTY" >< 0 THEN
{
    // List Empty bit SET in HASTAT - no receive buffers available
    ACTSW = false;                   // 0=:ACTSW - stop device
    STPCNT++;                        // MIN STPCNT - increment stop counter
    
    // 🚨 CRITICAL: All subsequent packets will be dropped!
    return;
}
```

### 3. Data Availability Check (Line 51083)
```assembly  
IF A NBIT 0 OR A/\60000><0 THEN  % Check HASTAT for data/errors
```

**Bit Analysis:**
```csharp
// A = HASTAT (contains RRTS)
// NBIT 0 = test DataAvailable bit = bit 0 = 0x0001  
// A/\60000 = mask upper bits = 0x6000 (X.21 errors)

if ((hastat & 0x0001) == 0 ||       // IF A NBIT 0 (no data available)
    (hastat & 0x6000) != 0)         // OR A/\60000><0 (X.21 errors)
{
    // 🚨 CRITICAL: Packet will be dropped!
    // Either no data ready OR X.21 protocol error in HASTAT
    return; // Don't process packet
}
```

### 4. Block Processing Check (Line 104483)
```assembly
IF A NBIT XBLDN THEN            % Check HASTAT for block completion
```

**Bit Analysis:**
```csharp
// XBLDN = BlockEnd bit = bit 8 = 0x0100

if ((hastat & 0x0100) == 0)         // IF A NBIT XBLDN THEN
{
    // No more blocks available in HASTAT
    if (hastat == 0xERB)             // IF A = "ERB" THEN
    {
        EnableReceiver();             // GO FAR ZSTARC
    }
    return; // GO FAR OUT1
}
```

## HASTAT Processing After RTTS Read (Transmitter Path)

### 1. Transmission Success/Error Check (Line 104046)
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN   % Check HASTAT for completion/underrun
```

**Bit Analysis:**
```csharp
// HASTAT now contains RTTS value (TransmitterStatusBits)  
// "SILFO+TXUND" = TransmissionFinished (bit 11) | TransmitterUnderrun (bit 1)
// = 0x0800 | 0x0002 = 0x0802

ushort hastat = (ushort)ReadRTTS();  // HASTAT = RTTS value

if ((hastat & 0x0802) == 0)         // IF A/\ "SILFO+TXUND" = 0 THEN
{
    // SUCCESS PATH: Neither transmission finished NOR underrun in HASTAT
    // 🚨 POTENTIAL BUG: This logic seems inverted!
    // When TransmissionFinished (bit 11) is SET, it should indicate SUCCESS
    
    SetRetryState();                 // XRETRY=:RTDYN
    LogSuccess();                    // A:=0; CALL SADTS
}
else
{
    // ERROR PATH: Either finished OR underrun detected in HASTAT
    // 🚨 CAUSES RETRANSMISSION when transmission actually succeeded!
    
    LogError(hastat);                // A:=HASTAT; CALL SADTS; CALL DRERR  
    IncrementErrorCounter();         // CALL DRERR
    SetUnderrunError();              // A:=EUND
}
```

### 2. List Completion Check (Line 51397)
```assembly
BSKP ONE 10 DA                  % Skip if HASTAT bit 10 is SET
```

**Bit Analysis:**
```csharp
// Bit 10 = ListEnd = 0x0400

if ((hastat & 0x0400) != 0)         // BSKP ONE 10 DA
{
    // DMA list completed in HASTAT - skip next operation
    SkipNextOperation();
}
```

### 3. Ready For Sending Check  
```assembly
IF A BIT 6 THEN                 % Check HASTAT bit 6
```

**Bit Analysis:**
```csharp
// Bit 6 = ReadyForSending = 0x0040

if ((hastat & 0x0040) != 0)         // IF A BIT 6 THEN
{
    // DCE ready for sending (from HASTAT)
    ProceedWithTransmission();
}
else
{
    // DCE not ready - wait or error
    HandleDCENotReady();
}
```

## Critical HASTAT Bit Patterns Causing Issues

### Receiver Issues (HASTAT from RRTS):

1. **DataAvailable (bit 0) = 0** in HASTAT
   ```csharp
   // If bit 0 is clear when data is actually available:
   if ((hastat & 0x0001) == 0) 
   {
       return; // Drops packet even though data is ready!
   }
   ```

2. **ListEmpty (bit 11) = 1** in HASTAT
   ```csharp
   // If bit 11 is set incorrectly:
   if ((hastat & 0x0800) != 0) 
   {
       ACTSW = false; // Stops all reception permanently!
   }
   ```

3. **X21D/X21S (bits 13-14) ≠ 0** in HASTAT
   ```csharp
   // If X.21 error bits are set incorrectly:
   if ((hastat & 0x6000) != 0) 
   {
       return; // Drops packet due to false X.21 error!
   }
   ```

### Transmitter Issues (HASTAT from RTTS):

1. **TransmissionFinished (bit 11) = 1** in HASTAT (Success Condition)
   ```csharp
   // Current logic treats this as ERROR:
   if ((hastat & 0x0802) != 0) // Includes bit 11 
   {
       retransmitPacket(); // RETRANSMITS successful transmission!
   }
   ```

2. **TransmitterUnderrun (bit 1) = 1** in HASTAT (Error Condition)  
   ```csharp
   // This should trigger retransmission:
   if ((hastat & 0x0002) != 0) 
   {
       retransmitPacket(); // Correct behavior
   }
   ```

## HASTAT Diagnostic Logging

After each HASTAT update, the value is logged to circular buffers:

```assembly  
CALL SADTS                      % Store HASTAT in diagnostic buffers
```

**Buffer Structure:**
- **BUFF2[index] = HASTAT** - Exact RRTS/RTTS bit pattern stored here
- This provides history of all status values for debugging

## Key Debugging Points

1. **Log HASTAT immediately after each read:**
   ```csharp
   ushort hastat = ReadRRTS(); // or ReadRTTS()
   Console.WriteLine($"HASTAT = 0x{hastat:X4}");
   // Then analyze each critical bit
   ```

2. **Check specific bit interpretations:**
   ```csharp
   // For RRTS in HASTAT:
   bool dataAvailable = (hastat & 0x0001) != 0;    // Should be TRUE for processing
   bool listEmpty = (hastat & 0x0800) != 0;        // Should be FALSE for processing  
   bool x21Error = (hastat & 0x6000) != 0;         // Should be FALSE for processing
   
   // For RTTS in HASTAT:
   bool txFinished = (hastat & 0x0800) != 0;       // Should be TRUE for success
   bool txUnderrun = (hastat & 0x0002) != 0;       // Should be FALSE for success
   ```

The root issue is likely that the bit interpretation logic for HASTAT doesn't match the actual hardware register values, causing the OS to make wrong decisions about packet processing and transmission success.