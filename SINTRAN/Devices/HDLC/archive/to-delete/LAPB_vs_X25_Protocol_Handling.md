# LAPB vs X.25 Protocol Handling in SINTRAN

## Protocol Layer Analysis

### LAPB (Link Access Procedure Balanced) - Layer 2
- **Pure HDLC data link protocol**
- **No X.21 interface requirements** 
- **4-byte frames** with HDLC flag/address/control/FCS
- **Direct hardware communication** over synchronous lines

### X.25 - Layer 3 Network Protocol  
- **Runs over LAPB** as the data link layer
- **Requires X.21 interface** for call setup/clearing
- **Uses X.21 signaling** for connection management
- **Packet-switched network protocol**

## SINTRAN HDLC Implementation Analysis

Looking at the code structure, SINTRAN appears to support **both modes**:

### 1. Pure HDLC/LAPB Mode (Your Current Use)
```assembly
% For simple HDLC frames - bypass X.21 logic
IF A NBIT 0 OR A/\60000><0 THEN    % Only check DataAvailable + X.21 errors
   return; % Drop if no data or X.21 errors
```

**For LAPB packets:**
- **Bit 0 (DataAvailable) = 1** - Frame ready
- **Bits 13-14 (X21D/X21S) = 0** - No X.21 errors  
- **HX21S bits (1,2,3) = don't matter** - not used in LAPB mode

### 2. X.25 Network Mode (Future Use)
```assembly
% X.25 requires full X.21 interface handling
IF A/\ HX21M >< 0 THEN             % Check X.21 protocol status
   IF A BIT HX21S THEN             % Check receiver state for X.21
      % Handle X.21 connection management
   FI
FI
```

**For X.25 packets:**
- **X.21 signaling required** for call setup/clearing
- **HX21S bits (1,2,3) become critical** for protocol state
- **Full connection management** needed

## Current LAPB Processing Path

### Your Working 4-byte LAPB Packets:
```csharp
// Simple HDLC frame processing
ushort rrts = 0x0001;  // Only DataAvailable set

// Primary check (Line 51083):
if ((rrts & 0x0001) == 0 ||    // DataAvailable = 0? Drop
    (rrts & 0x6000) != 0)      // X.21 error? Drop  
{
    return; // Packet dropped
}

// For LAPB: This passes, packet gets processed
// HX21S logic is skipped for pure HDLC/LAPB frames
```

### Why Your LAPB Works:
1. **DataAvailable (bit 0) = 1** → Packet accepted
2. **X.21 errors (bits 13-14) = 0** → No protocol errors
3. **HX21S logic only triggers inside X.21 error handler** → Skipped for LAPB
4. **Packet processed normally** through HDLC frame handling

## X.25 Mode Requirements (Future)

### When You Add X.25 Support:
```csharp
// X.25 requires proper receiver state signaling
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |      // Bit 0: Packet ready
    ReceiverStatusBits.StatusAvailable |    // Bit 1: Status info available
    ReceiverStatusBits.SyncFlagReceived |   // Bit 3: Frame boundaries
    // Bit 2 (ReceiverActive): Set during frame reception, clear when complete
    // Bit 14 (X21S): Set when DCE sends clear indication
    // Bits 13-14: Set for X.21 protocol errors
```

### X.25 Call Clearing:
```csharp
// When DCE wants to clear X.25 call:
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |      // Data ready
    ReceiverStatusBits.X21S;                // Bit 14: X.21 clear indication

// This would trigger connection termination in X.25 mode
```

## Protocol Detection Logic

The SINTRAN code appears to have **mode detection**:

### Frame Type Check (Line 104631):
```assembly
IF A /\ "LMASK" = 3 THEN           % Frame type = 3?
   A:=0; CALL SCRET; CALL SADTS    % Simple processing
ELSE
   IF A BIT HX21S THEN EX21 ELSE EINP FI  % Route based on receiver state
```

**Interpretation:**
- **LMASK = 3**: Simple HDLC/LAPB frames (your current use)
- **Other types**: Full X.21/X.25 processing with HX21S routing

## Summary for Your Implementation

### Current LAPB Mode (Working):
```csharp
// Minimal RRTS for 4-byte LAPB packets:
ReceiverStatusBits rrts = ReceiverStatusBits.DataAvailable;  // 0x0001
// Result: Packet processed successfully, X.21 logic bypassed
```

### Future X.25 Mode:
```csharp
// Full RRTS for X.25 packet-switched calls:
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |      // Packet data ready
    ReceiverStatusBits.StatusAvailable |    // Status available
    ReceiverStatusBits.SyncFlagReceived;    // Frame sync detected
    
// For connection clearing:
if (dceWantsToClearCall)
{
    rrts |= ReceiverStatusBits.X21S;  // Set bit 14 for clear indication
}
```

**Your current LAPB packets work because they use the simple HDLC path that bypasses the complex X.21/X.25 connection management logic. The HX21S bits only become important when you implement full X.25 packet-switched networking.**