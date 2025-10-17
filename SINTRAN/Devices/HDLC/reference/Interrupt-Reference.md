# HDLC Interrupt Handlers

**HIINT (Receive) and HOINT (Transmit) interrupt handler analysis**

---

## Interrupt Levels

| Level | Handler | Register | Purpose |
|:-----:|:-------:|:--------:|:--------|
| **12** | **HOINT** | RTTS (IOX+12) | Transmit completion and errors |
| **13** | **HIINT** | RRTS (IOX+10) | Receive completion and errors |

## HOINT - Transmit Interrupt Handler

**Entry Point**: Line 104033  
**Trigger**: Level 12 interrupt from transmitter

### Processing Flow

```
1. Read RTTS register (IOX+12)
2. Store status in HASTAT variable
3. Check if device is active (ACTSW)
4. Test for transmission success: (HASTAT & 0x8002) == 0
   - If success: Clear retry count, mark inactive, process next frame
   - If error: Increment retry, retransmit or report error
5. Return from interrupt
```

### Success Check Logic

```assembly
HOINT: T:=HDEV+RTTS; *EXR ST     % Read RTTS
       A=:HASTAT                 % Store status
       
       IF A/\ "SILFO+TXUND" = 0 THEN    % Test bits 15,1
          % SUCCESS PATH
          XRETRY=:RTDYN                 % Clear retry count
          0=:ACTSW                      % Mark inactive
          CALL NEXTS                    % Next frame
       ELSE
          % ERROR PATH
          XRETRY+1=:XRETRY              % Increment retry
          CALL RETRANSMIT               % Retry transmission
       FI
```

### C# Implementation

```csharp
public void HandleTransmitInterrupt()
{
    ushort rtts = ReadRegister(RTTS);
    
    if (!deviceActive)
    {
        // Spurious interrupt - log and ignore
        return;
    }
    
    if ((rtts & 0x8002) == 0)  // SILFO + TXUND both clear
    {
        // SUCCESS
        retryCount = 0;
        deviceActive = false;
        ProcessNextFrame();
    }
    else
    {
        // ERROR
        retryCount++;
        if (retryCount > MAX_RETRY)
            ReportError(237);  // Transmission failure
        else
            RetransmitFrame();
    }
}
```

## HIINT - Receive Interrupt Handler

**Entry Point**: Line 104436  
**Trigger**: Level 13 interrupt from receiver

### Processing Flow

```
1. Read RRTS register (IOX+10)
2. Store status in HASTAT variable
3. Check for X.21 protocol errors (bits 13-14)
4. Check for buffer exhaustion (bit 11)
5. Validate data available (bit 0) and no X.21 errors
6. Process received packet
7. Return from interrupt
```

### Reception Logic

```assembly
HIINT: T:=HDEV+RRTS; *EXR ST     % Read RRTS
       A=:HASTAT                  % Store status
       
       % Check 1: X.21 errors
       IF A/\ HX21M >< 0 THEN     % Test bits 13-14
          IF A BIT HX21S THEN     % Test receiver state
             HASTAT BONE BLDON=:HASTAT  % Terminate frame
          FI
       FI
       
       % Check 2: Buffer availability
       IF HASTAT/\"EMTY" >< 0 THEN  % Test bit 11
          0=:ACTSW                  % Stop device
       FI
       
       % Check 3: Data valid
       IF A NBIT 0 OR A/\60000><0 THEN  % No data OR X.21 error
          return  % Drop packet
       FI
       
       % Process packet
       CALL PROCESS_PACKET
```

### C# Implementation

```csharp
public void HandleReceiveInterrupt()
{
    ushort rrts = ReadRegister(RRTS);
    
    // Check 1: X.21 protocol errors
    if ((rrts & 0x6000) != 0)  // HX21M mask
    {
        HandleX21Error(rrts);
        if ((rrts & 0x000E) != 0)  // Receiver active
            TerminateCurrentFrame();
    }
    
    // Check 2: Buffer exhaustion
    if ((rrts & 0x0800) != 0)  // EMTY bit
    {
        deviceActive = false;
        ReportBufferExhaustion();
        return;
    }
    
    // Check 3: Data available and valid
    if ((rrts & 0x0001) == 0)  // No data available
        return;
    
    if ((rrts & 0x6000) != 0)  // X.21 errors present
        return;
    
    // Process received packet
    ProcessReceivedPacket();
}
```

## State Machine (ACTSW)

**ACTSW Variable**: Active Switch - tracks device state

| Value | State | Meaning |
|:-----:|:------|:--------|
| 0 | **Inactive** | No operation in progress |
| 1 | **Active** | Transmission/reception in progress |

### State Transitions

```
Transmission:
IDLE (0) → [Start Transmit] → ACTIVE (1) → [Interrupt Success] → IDLE (0)
                                      ↓ [Interrupt Error]
                                      └→ RETRY → ACTIVE (1) or ERROR

Reception:
IDLE (0) → [Start Receive] → ACTIVE (1) → [Interrupt Data] → PROCESS → ACTIVE (1)
                                     ↓ [Buffer Empty]
                                     └→ IDLE (0)
```

## Timing Considerations

**Interrupt Latency**: ~10-20 CPU cycles  
**Handler Execution**: Variable, typically 50-200 cycles  
**DMA Transfer**: Parallel to CPU, no wait states

### Race Conditions

**Critical Section**: Reading status and clearing ACTSW must be atomic

```csharp
// Ensure atomic status check and state update
lock (deviceLock)
{
    ushort status = ReadRegister(RTTS);
    if ((status & 0x8002) == 0)
        deviceActive = false;  // Atomic state transition
}
```

---

**See Also**:
- [02-HDLC-Register-Reference.md](02-HDLC-Register-Reference.md) - Status register bits
- [05-HDLC-Protocol-Implementation.md](05-HDLC-Protocol-Implementation.md) - Protocol state machines
- [06-HDLC-Emulator-Guide.md](06-HDLC-Emulator-Guide.md) - Complete implementation

**Document Version**: 2.0 (Consolidated)  
**Source Files**: HIINT_Deep_Analysis.md, HOINT_Deep_Analysis.md, HASTAT_Bit_Processing_Analysis.md, ACTSW_State_Analysis.md, SINTRAN_Timer_Analysis.md

