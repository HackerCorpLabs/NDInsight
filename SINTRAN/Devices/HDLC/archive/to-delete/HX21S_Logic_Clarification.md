# HX21S Logic Clarification - The Real Meaning

## HX21S Constant Analysis

**HX21S = 0x000E (octal 000016) = bits 1, 2, 3**

From the C# ReceiverStatusBits enum:
- **Bit 1**: StatusAvailable - Status information available in Receiver Status Register
- **Bit 2**: ReceiverActive - Receiver is active within a frame  
- **Bit 3**: SyncFlagReceived - At least one SYNC character or FLAG received

## SINTRAN Logic Re-Analysis

### Context 1: Inside X.21 Error Handler (Line 104462)
```assembly
IF A/\ HX21M >< 0 THEN                    % IF X.21 error detected (bits 13-14)
   % ... handle X.21 error ...
   IF A BIT HX21S THEN                    % IF any of bits 1,2,3 are set
      HASTAT BONE BLDON=:HASTAT           % Set BLOCK DONE  
      LIINT.LKEY BONE XBLDN=:X.LKEY       % Terminate processing
   FI
FI
```

### Context 2: Frame Processing Logic (Line 104641)  
```assembly
IF A /\ "LMASK" = 3 THEN                  % If frame type/length = 3
   A:=0; CALL SCRET; CALL SADTS           % Clear and return
ELSE
   IF A BIT HX21S THEN EX21 ELSE EINP FI  % If bits 1,2,3 set: EX21, else EINP
   CALL SCRET
   % ... continue processing ...
FI
```

## The Real Logic

### HX21S is NOT X.21 Clear Indication Bit!

**HX21S = 0x000E** tests for **active receiver state bits**:
- **StatusAvailable (Bit 1)**: Status info ready
- **ReceiverActive (Bit 2)**: Currently receiving frame
- **SyncFlagReceived (Bit 3)**: SYNC/FLAG detected

### When HX21S Test Triggers:

1. **Inside X.21 Error Handler:** 
   ```csharp
   if ((rrts & 0x6000) != 0)  // X.21 error detected (bits 13-14)
   {
       if ((rrts & 0x000E) != 0)  // If receiver is active (bits 1,2,3)
       {
           setBlockDone();  // Terminate current frame processing
       }
   }
   ```
   **Meaning**: If X.21 error occurs AND receiver is active, terminate the current frame.

2. **In Frame Processing:**
   ```csharp
   if ((frameType & LMASK) == 3)  // Special frame type
   {
       clearAndReturn();
   }
   else
   {
       if ((rrts & 0x000E) != 0)  // If receiver active/status available
       {
           callEX21();  // Handle active receiver
       }
       else
       {
           callEINP();  // Handle inactive receiver  
       }
   }
   ```
   **Meaning**: Route processing based on receiver activity state.

## Correct Interpretation for Your HDLC Emulation

### For Normal LAPB Packet Processing:

**You should set these bits in RRTS when packet arrives:**

1. **Bit 0 (DataAvailable) = 1** - Character/packet ready for read
2. **Bit 1 (StatusAvailable) = 1** - Status information available  
3. **Bit 2 (ReceiverActive) = 0** - Not currently within frame (packet complete)
4. **Bit 3 (SyncFlagReceived) = 1** - SYNC/FLAG detected (frame boundaries)
5. **Bits 13-14 (X21D/X21S) = 0** - No X.21 protocol errors

**HX21S Check Result:**
```csharp
// HX21S = 0x000E tests bits 1,2,3
// With above settings: StatusAvailable=1, ReceiverActive=0, SyncFlagReceived=1  
// Result: (rrts & 0x000E) = 0x000A (bits 1,3 set) != 0

if (x21ErrorDetected)  // Only if bits 13-14 set
{
    if ((rrts & 0x000E) != 0)  // Receiver has activity
    {
        terminateCurrentFrame();  // Clean termination
    }
}

// For normal processing (no X.21 errors):
if ((rrts & 0x000E) != 0)  // Receiver active/status available
{
    processActiveReceiver();   // EX21 path - normal packet handling
}
else  
{
    processInactiveReceiver(); // EINP path - error/idle handling
}
```

## Recommendations for Your HDLC Emulation

### Normal Packet Reception RRTS Value:
```csharp
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |      // Bit 0 = 1 (packet ready)
    ReceiverStatusBits.StatusAvailable |    // Bit 1 = 1 (status ready) 
    ReceiverStatusBits.SyncFlagReceived;    // Bit 3 = 1 (frame sync)
    // Bit 2 (ReceiverActive) = 0 (frame complete)
    // Bits 13-14 (X21D/X21S) = 0 (no X.21 errors)

// Result: 0x000B = bits 0,1,3 set
// HX21S test: (0x000B & 0x000E) = 0x000A != 0 (will take EX21 path)
```

### Error Condition RRTS Value:
```csharp
ReceiverStatusBits rrts = 0x0001;  // Only DataAvailable, no status/sync
// HX21S test: (0x0001 & 0x000E) = 0x0000 == 0 (will take EINP path)
```

**The HX21S logic is correct - it routes between active receiver processing (EX21) and error/inactive processing (EINP) based on receiver state bits, not X.21 clear indication!**