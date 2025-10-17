# SCSI C# Implementation Guide: Fixing Interrupt-Driven Operation

**File:** `Z:\NorskData\Source Code\Sintran L\NPL\SCSI-C#-Implementation-Guide.md`

**Target File:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`

---

## 1. Problem Statement

### Current Behavior
The C# SCSI emulator works in **polling mode** but fails with **interrupt-driven operation** (which is what SINTRAN expects).

### Why It Fails
- When SINTRAN writes `5` to WCONT (Enable Interrupt + Activate), the C# driver executes the entire DMA operation synchronously
- The NCR 5386 chip should generate interrupts on EACH phase change during SCSI protocol execution
- SINTRAN's interrupt handler (SCINT) expects to process each phase separately
- Current implementation completes the operation before SINTRAN can respond to phase changes

### What Needs to Change
The C# driver must:
1. Generate interrupts at each SCSI phase transition (not just at completion)
2. Pause operation when interrupt is triggered
3. Wait for SINTRAN to acknowledge (write `0` to WCONT)
4. Resume when SINTRAN writes `5` to WCONT again
5. Repeat for every phase change until operation completes

---

## 2. What SINTRAN Expects - Step by Step

### When WCONT Receives Value 5 (bit 0 = Enable Interrupt, bit 2 = Activate):

**Step 1:** Hardware starts DMA operation
- SINTRAN has configured:
  - Memory Address Register (MAR): where to transfer data
  - Transfer Counter: how many bytes
  - NCR Command Register: what operation to perform (e.g., `0x94` = DMA mode + Transfer Info)

**Step 2:** NCR 5386 chip begins SCSI protocol execution
- Chip performs arbitration
- Chip performs selection
- Chip enters first SCSI phase

**Step 3:** On EACH phase change, hardware MUST:
- **Set STATUS bit 11** (NCR interrupt bit)
- **Trigger Level 11 CPU interrupt** to ND-100
- **PAUSE the DMA operation** (stop transferring data)
- Wait for software to acknowledge

**Step 4:** SINTRAN's SCINT handler runs (line 123 in IP-P2-SCSI-DRIV.NPL)
```
Line 123: Read RSTAU (status register)
Line 133: Check bit 11 (NCRIT - interrupt from NCR)
Line 134: Write 0 to WCONT (PAUSE - disable interrupt temporarily)
Line 135-138: Read NCR status registers (RAUXS, RITRG)
```

**Step 5:** Handler processes the phase
- SCINT dispatches to appropriate phase handler (NEWPH, line 659)
- Phase handler examines SCSI bus phase bits
- Handler updates data pointers, byte counts
- Handler may setup new DMA parameters for next phase

**Step 6:** Handler writes 5 to WCONT (RESUME)
```
Line 187: Write 5 to WCONT (re-enable interrupt + activate)
```

**Step 7:** Hardware resumes SCSI operation
- NCR continues protocol execution
- When next phase change occurs, return to Step 3
- Repeat until operation complete

### Critical Timing in SCINT (from IP-P2-SCSI-DRIV-ANALYSIS.md)

```
Line 133: IF A =: SCSSR BIT 11 THEN        % Check NCR interrupt bit
Line 134:    "0"; T := HDEV+WCONT; *IOXT   % **WRITE 0 TO WCONT** (pause)
Line 135:    T+"RAUXS-WCONT"; *IOXT        % Read auxiliary status
Line 137:    T+"RITRG-RAUXS"; *IOXT        % Read interrupt register
Line 139:    A := D =: SCNIS; 0 =: SCCCW   % Save status
...
Line 187: 5\/SCCCW; T := HDEV+WCONT; *IOXT % **WRITE 5 TO WCONT** (resume)
Line 188: GO SCWTI                          % Exit interrupt handler
```

### SCSI Phase Sequence Example (READ operation)

1. **ARBITRATION** → Interrupt → SCINT reads status → writes 0 → processes → writes 5
2. **SELECTION** → Interrupt → SCINT reads status → writes 0 → processes → writes 5
3. **COMMAND PHASE** → Interrupt → SCINT reads status → writes 0 → sends command → writes 5
4. **DATA IN PHASE** → Interrupt → SCINT reads status → writes 0 → sets up DMA → writes 5
   - (DMA transfer may happen here, but still needs interrupt at end)
5. **STATUS PHASE** → Interrupt → SCINT reads status → writes 0 → reads status byte → writes 5
6. **MESSAGE IN PHASE** → Interrupt → SCINT reads status → writes 0 → reads message → writes 5
7. **BUS FREE** → Final interrupt → Operation complete

---

## 3. Required C# Changes

### 3.1 Add Phase Change Detection State Machine

**Location:** After line 196 (inside NDBusDiscControllerSCSI class)

**Add new fields:**

```csharp
// State machine for interrupt-driven SCSI operation
private enum SCSIOperationState
{
    Idle,                    // No operation in progress
    WaitingForResume,        // Interrupt fired, waiting for SINTRAN to resume
    Executing                // Operation in progress
}

private SCSIOperationState operationState = SCSIOperationState.Idle;
private byte lastPhase = 0xFF;  // Track previous SCSI phase
private bool phaseChangeOccurred = false;
```

### 3.2 Modify Write() for WCONT Register

**Location:** Replace lines 1053-1165 (case Register.WCONT)

**Problem with current code:**
```csharp
// Line 1151-1156: Current implementation
if (regs.active)
{
    regs.readyForTransfer = false;
    ExecuteGo();  // <-- Executes ENTIRE operation synchronously!
}
```

**Fixed implementation:**

```csharp
case Register.WCONT:  // Write CONTROL WORD (on 3201 controller)
    /*
    Bits
        0  Enable Interrupt
        2  Activate
        3  Test
        4  Clear Device
        5  ND-100 DMA enable
        6  Write ND-100 Memory
        10 Reset SCSI to bus
    */

#if DEBUG_DETAIL_PLUS_DESCRIPTION
    string controlDescription = "";
    if ((value & 1 << 0) != 0) controlDescription += "[Enabled Interrupt] ";
    if ((value & 1 << 2) != 0) controlDescription += "[Active] ";
    if ((value & 1 << 3) != 0) controlDescription += "[Test mode] ";
    if ((value & 1 << 4) != 0) controlDescription += "[Device clear] ";
    if ((value & 1 << 5) != 0) controlDescription += "[ND-100 DMA enable] ";
    if ((value & 1 << 6) != 0) controlDescription += "[Write ND-100 Memory] ";
    if ((value & 1 << 10) != 0) controlDescription += "[Reset SCSI to bus] ";
    Log($"Writing ControlWord 0x{value:X4}=> {controlDescription} [State: {operationState}]");
#endif

    bool wasInterruptEnabled = regs.interruptEnabled;
    bool wasActive = regs.active;

    regs.interruptEnabled = (value & 1 << 0) != 0;
    regs.active = (value & 1 << 2) != 0;
    regs.testMode = (value & 1 << 3) != 0;
    regs.DMAEnable = (value & 1 << 5) != 0;
    regs.WriteNDMemory = (value & 1 << 6) != 0;

    // Handle test mode (odd byte transfer)
    if (regs.testMode)
    {
        uint dma_address = (uint)(regs.MemoryAddressMSB << 16 | regs.MemoryAddressLSB);
        if (regs.WriteNDMemory)
        {
            ushort data = regs.ReadWriteData;
            Log($"DMA Write to {Numeric.Num2Str(dma_address)} value {Numeric.Num2Str(data)}");
            DMAWrite(dma_address, data);
        }
        else
        {
            regs.ReadWriteData = (ushort)DMARead(dma_address);
            Log($"DMA Read from {Numeric.Num2Str(dma_address)} value {Numeric.Num2Str(regs.ReadWriteData)}");
        }
    }

    // Clear device
    if ((value & 1 << 4) != 0)
    {
        regs.MemoryAddressLSB = 0;
        regs.MemoryAddressMSB = 0;
        bufferPointer = 0;
        readbufferPointer = 0;
        ncr5386.DeviceReset();
        regs.readyForTransfer = true;
        operationState = SCSIOperationState.Idle;
    }

    // Reset SCSI bus
    regs.resetOnSCSIBus = ((value & 1 << 10) != 0);
    if (regs.resetOnSCSIBus)
    {
        ncr5386.InitiateResetSCSIBus();
        regs.readyForTransfer = true;
    }

    // *** KEY CHANGE: State machine for interrupt-driven operation ***

    // Case 1: Writing 0 to WCONT (PAUSE) - SINTRAN acknowledges interrupt
    if (!regs.active && !regs.interruptEnabled && operationState == SCSIOperationState.WaitingForResume)
    {
#if DEBUG_HIGH_LEVEL
        Log("SINTRAN PAUSED operation (wrote 0 to WCONT)");
#endif
        // Clear the interrupt - SINTRAN has acknowledged it
        SetInterruptBit(false);
        // Stay in WaitingForResume state until SINTRAN writes 5
    }

    // Case 2: Writing 5 to WCONT (RESUME or START)
    else if (regs.active && regs.interruptEnabled)
    {
        if (operationState == SCSIOperationState.WaitingForResume)
        {
            // RESUME after interrupt
#if DEBUG_HIGH_LEVEL
            Log("SINTRAN RESUMED operation (wrote 5 to WCONT)");
#endif
            operationState = SCSIOperationState.Executing;
            regs.readyForTransfer = false;

            // Resume SCSI operation - continue from where we paused
            ResumeOperation();
        }
        else if (operationState == SCSIOperationState.Idle)
        {
            // START new operation
#if DEBUG_HIGH_LEVEL
            Log("SINTRAN STARTED new operation (wrote 5 to WCONT)");
#endif
            operationState = SCSIOperationState.Executing;
            regs.readyForTransfer = false;
            lastPhase = 0xFF;  // Reset phase tracking

            // Start new SCSI operation
            ExecuteGo();
        }
    }

    // Case 3: Writing 0 when idle (clear interrupt without resume)
    else if (!regs.active && operationState == SCSIOperationState.Idle)
    {
        // Just clearing interrupt status
        SetInterruptBit(false);
    }

    break;
```

### 3.3 Add ProcessNCRInterrupt() Method

**Location:** Add after StepGoState() method (after line 1311)

```csharp
/// <summary>
/// Check if NCR 5386 has changed phase and needs to interrupt
/// Called from StepGoState() during operation execution
/// </summary>
private void ProcessNCRInterrupt()
{
    if (operationState != SCSIOperationState.Executing)
    {
        return;  // Only check during active execution
    }

    // Read current NCR interrupt register
    byte interruptRegister = ncr5386.Read((byte)SCSIRegisters.InterruptRegister);
    byte auxiliaryStatus = ncr5386.Read((byte)SCSIRegisters.AuxilaryStatus);

    // Check for any interrupt condition from NCR chip
    bool hasInterrupt = (interruptRegister != 0);

    if (hasInterrupt)
    {
#if DEBUG_HIGH_LEVEL
        Log($"NCR INTERRUPT: IntReg=0x{interruptRegister:X2}, AuxStatus=0x{auxiliaryStatus:X2}");
#endif

        // Extract current SCSI phase from auxiliary status (bits 3-5)
        byte currentPhase = (byte)((auxiliaryStatus >> 3) & 0x07);

        // Check for phase change
        if (currentPhase != lastPhase && lastPhase != 0xFF)
        {
#if DEBUG_HIGH_LEVEL
            string[] phaseNames = { "DATA_OUT", "DATA_IN", "COMMAND", "STATUS",
                                   "ILLEGAL_OUT", "ILLEGAL_IN", "MESSAGE_OUT", "MESSAGE_IN" };
            Log($"PHASE CHANGE: {phaseNames[lastPhase]} -> {phaseNames[currentPhase]}");
#endif
            phaseChangeOccurred = true;
        }

        lastPhase = currentPhase;

        // Set interrupt status in controller
        regs.InterruptFromNCR5386 = true;

        // Transition to waiting state
        operationState = SCSIOperationState.WaitingForResume;
        regs.active = false;
        regs.readyForTransfer = true;

        // Trigger Level 11 interrupt to ND-100 CPU
        if (regs.interruptEnabled)
        {
#if DEBUG_HIGH_LEVEL
            Log("Triggering Level 11 interrupt to CPU");
#endif
            SetInterruptBit(true);
        }
    }
}
```

### 3.4 Add ResumeOperation() Method

**Location:** Add after ProcessNCRInterrupt()

```csharp
/// <summary>
/// Resume SCSI operation after SINTRAN has processed interrupt
/// Called when SINTRAN writes 5 to WCONT after handling phase
/// </summary>
private void ResumeOperation()
{
#if DEBUG_HIGH_LEVEL
    Log("Resuming SCSI operation");
#endif

    // Clear interrupt flag
    regs.InterruptFromNCR5386 = false;
    phaseChangeOccurred = false;

    // SINTRAN may have updated NCR registers (new DMA address, count, etc.)
    // The NCR chip will continue with these new parameters

    // Resume execution - StepGoState() will continue processing
    regs.active = true;
    regs.readyForTransfer = false;
}
```

### 3.5 Add SetOperationComplete() Method

**Location:** Add after ResumeOperation()

```csharp
/// <summary>
/// Mark operation as complete and trigger final interrupt
/// </summary>
private void SetOperationComplete()
{
#if DEBUG_HIGH_LEVEL
    Log("SCSI operation COMPLETE");
#endif

    operationState = SCSIOperationState.Idle;
    regs.active = false;
    regs.readyForTransfer = true;
    regs.InterruptFromNCR5386 = true;

    // Trigger final interrupt if enabled
    if (regs.interruptEnabled)
    {
        SetInterruptBit(true);
    }
}
```

### 3.6 Modify StepGoState() to Call ProcessNCRInterrupt()

**Location:** Modify StepGoState() method (lines 1231-1311)

**Find this section (around line 1262):**
```csharp
if (regs.active)
{
    if (regs.InterruptFromNCR5386)
    {
        regs.active = false;
        regs.readyForTransfer = true;
        if (regs.interruptEnabled)
        {
            SetInterruptBit(true);
        }
    }
```

**Replace with:**
```csharp
if (regs.active && operationState == SCSIOperationState.Executing)
{
    // Check for NCR interrupts (phase changes) FIRST
    ProcessNCRInterrupt();

    // If still executing (not interrupted), continue DMA
    if (operationState == SCSIOperationState.Executing)
    {
        if (regs.DMAEnable)
        {
            if (regs.DataRequestFromNCR5386)
            {
                regs.GoStateTimeout = IODELAY_SCSI_TIMEOUT;

                if (regs.WriteNDMemory)
                {
                    byte data = ncr5386.dma_read();
                    WriteNextByteDMA(data);
                }
                else
                {
                    byte data = ReadNextByteDMA();
                    ncr5386.dma_write((byte)data);
                }
            }
        }
    }
```

### 3.7 Handle Each SCSI Phase in NCR5386SCSI.cs

**Important:** The NCR 5386 emulation (ncr5386 object) must also be modified to:

1. **Track phase changes** - When SCSI bus phase changes, set interrupt register
2. **Pause on interrupt** - Stop processing until next command
3. **Generate proper interrupt codes** - Set bits in interrupt register:
   - Bit 11 (BUSSI) - Bus Service Interrupt (phase change)
   - Bit 10 (FUCOM) - Function Complete
   - Bit 13 (CONEC) - Connect
   - Bit 12 (DISCO) - Disconnect

**Example modification needed in NCR5386SCSI.cs:**

```csharp
// In NCR5386SCSI class - add phase tracking
private byte currentPhase = 0xFF;

// When phase changes in SCSI protocol execution:
private void OnPhaseChange(byte newPhase)
{
    if (newPhase != currentPhase)
    {
        currentPhase = newPhase;

        // Set Bus Service Interrupt bit (bit 11 in interrupt register)
        interruptRegister |= 0x08;  // BUSSI bit

        // Pause operation - wait for next command from host
        pauseExecution = true;
    }
}
```

---

## 4. Complete Code Example

### Modified WCONT Write Handler (Complete)

```csharp
case Register.WCONT:
    regs.interruptEnabled = (value & 1 << 0) != 0;
    regs.active = (value & 1 << 2) != 0;
    regs.testMode = (value & 1 << 3) != 0;
    regs.DMAEnable = (value & 1 << 5) != 0;
    regs.WriteNDMemory = (value & 1 << 6) != 0;

    // Handle clear device
    if ((value & 1 << 4) != 0)
    {
        regs.MemoryAddressLSB = 0;
        regs.MemoryAddressMSB = 0;
        bufferPointer = 0;
        readbufferPointer = 0;
        ncr5386.DeviceReset();
        regs.readyForTransfer = true;
        operationState = SCSIOperationState.Idle;
    }

    // Handle SCSI bus reset
    if ((value & 1 << 10) != 0)
    {
        ncr5386.InitiateResetSCSIBus();
        regs.readyForTransfer = true;
    }

    // State machine for interrupt-driven operation
    if (!regs.active && !regs.interruptEnabled)
    {
        // SINTRAN wrote 0 - PAUSE or CLEAR
        if (operationState == SCSIOperationState.WaitingForResume)
        {
            Log("Operation PAUSED by SINTRAN");
            SetInterruptBit(false);
        }
        else
        {
            SetInterruptBit(false);
        }
    }
    else if (regs.active && regs.interruptEnabled)
    {
        // SINTRAN wrote 5 - START or RESUME
        if (operationState == SCSIOperationState.WaitingForResume)
        {
            Log("Operation RESUMED by SINTRAN");
            operationState = SCSIOperationState.Executing;
            regs.readyForTransfer = false;
            ResumeOperation();
        }
        else if (operationState == SCSIOperationState.Idle)
        {
            Log("Operation STARTED by SINTRAN");
            operationState = SCSIOperationState.Executing;
            regs.readyForTransfer = false;
            lastPhase = 0xFF;
            dma_bytes_read = 0;
            dma_bytes_written = 0;
            ExecuteGo();
        }
    }
    break;
```

### Modified StepGoState() (Complete)

```csharp
void StepGoState()
{
    // Check timeout
    if (regs.GoStateTimeout > 0)
    {
        regs.GoStateTimeout--;
        if (regs.GoStateTimeout == 0)
        {
            Log("********************* ExecuteGo TIMEOUT *************************");
            SetOperationComplete();
            if (regs.interruptEnabled)
            {
                SetInterruptBit(true);
            }
            return;
        }
    }

    if (regs.active && operationState == SCSIOperationState.Executing)
    {
        // Check for NCR interrupts (phase changes)
        ProcessNCRInterrupt();

        // If still executing, continue DMA
        if (operationState == SCSIOperationState.Executing && regs.DMAEnable)
        {
            if (regs.DataRequestFromNCR5386)
            {
                regs.GoStateTimeout = IODELAY_SCSI_TIMEOUT;

                if (regs.WriteNDMemory)
                {
                    byte data = ncr5386.dma_read();
                    WriteNextByteDMA(data);
                }
                else
                {
                    byte data = ReadNextByteDMA();
                    ncr5386.dma_write((byte)data);
                }
            }
        }
    }
}
```

---

## 5. Testing Checklist

### Step 1: Verify Interrupt Generation
- [ ] Add logging to ProcessNCRInterrupt()
- [ ] Verify interrupt fires on EACH phase change (not just at end)
- [ ] Check that STATUS bit 11 (NCR interrupt) is set correctly
- [ ] Confirm Level 11 CPU interrupt is triggered

### Step 2: Verify SINTRAN Interaction
- [ ] Trace SINTRAN writes to WCONT:
  - First write: `5` (start operation)
  - Second write: `0` (pause after interrupt)
  - Third write: `5` (resume)
  - Repeat for each phase
- [ ] Verify SCINT handler runs (use SINTRAN debugger or trace)
- [ ] Check that SINTRAN reads RAUXS and RITRG after interrupt

### Step 3: Verify Phase Handling
- [ ] Log each SCSI phase transition:
  - ARBITRATION
  - SELECTION
  - COMMAND
  - DATA IN/OUT
  - STATUS
  - MESSAGE IN
  - BUS FREE
- [ ] Verify operation pauses at EACH phase
- [ ] Confirm SINTRAN processes each phase before resuming

### Step 4: Verify DMA Operation
- [ ] Check data transfers correctly during DATA phases
- [ ] Verify byte counts are correct
- [ ] Confirm memory addresses increment properly
- [ ] Test both READ and WRITE operations

### Step 5: Test Complete Operations
- [ ] Boot SINTRAN from SCSI disk
- [ ] Read file from disk
- [ ] Write file to disk
- [ ] Run SINTRAN disk utilities (FORMAT, BACKUP)
- [ ] Verify no data corruption

### Step 6: Test Error Conditions
- [ ] Test timeout handling
- [ ] Test SCSI bus reset
- [ ] Test disconnect/reconnect
- [ ] Test parity errors (if implemented)

---

## 6. Common Pitfalls

### Pitfall 1: Completing Operation Too Fast
**Problem:** ExecuteGo() completes entire SCSI transaction before SINTRAN can respond

**Solution:**
- Do NOT call ncr5386 methods that execute complete transactions
- Let StepGoState() drive operation incrementally
- Pause at EVERY phase change

### Pitfall 2: Not Clearing Interrupt Properly
**Problem:** Interrupt stays asserted, causing interrupt storm

**Solution:**
- Clear interrupt when SINTRAN writes 0 to WCONT
- Only set interrupt again when next phase change occurs

### Pitfall 3: Wrong Interrupt Timing
**Problem:** Interrupt fires too early or too late

**Solution:**
- Fire interrupt IMMEDIATELY when phase changes
- Do NOT wait for DMA to complete in that phase
- SINTRAN will set up DMA parameters before resuming

### Pitfall 4: Not Tracking State Correctly
**Problem:** State machine gets confused about Idle/Executing/Waiting states

**Solution:**
- Use clear state transitions: Idle -> Executing -> WaitingForResume -> Executing -> ...
- Log every state change during debugging
- Never skip a state

### Pitfall 5: NCR Emulation Too High-Level
**Problem:** NCR 5386 emulation executes complete SCSI commands without phase tracking

**Solution:**
- Modify NCR emulation to track phase changes
- Make NCR emulation pauseable between phases
- Expose phase information to controller

### Pitfall 6: Forgetting SINTRAN Updates Registers
**Problem:** Resuming operation with stale NCR register values

**Solution:**
- SINTRAN may write new values to NCR registers while paused
- Do NOT cache NCR register values across pause/resume
- Always read current NCR state when resuming

### Pitfall 7: Not Testing Phase Sequences
**Problem:** Only testing complete operations, missing per-phase bugs

**Solution:**
- Log EVERY phase transition
- Verify interrupt fires for EACH phase
- Check SINTRAN handler runs for EACH phase
- Test unusual phase sequences (disconnect/reconnect)

### Pitfall 8: Mixing Polling and Interrupt Modes
**Problem:** Code tries to support both modes simultaneously

**Solution:**
- Interrupt mode is the ONLY mode SINTRAN uses
- Remove or disable polling mode code paths
- Keep it simple: always pause on phase change

---

## 7. Debugging Tips

### Enable Maximum Logging
```csharp
#define DEBUG_DETAIL
#define DEBUG_HIGH_LEVEL
#define DEBUG_DETAIL_PLUS_DESCRIPTION
```

### Log Key Events
- WCONT writes (value and state)
- Phase changes (old -> new)
- Interrupt assertion/clearing
- SINTRAN register reads/writes
- State transitions

### Use SINTRAN Debugger
```
// In SINTRAN, set breakpoint in SCINT handler
LOAD @PMON:IP-P2-SCSI-DRIV
DEBUG SCINT
```

### Watch NCR Registers
Monitor these registers during operation:
- Interrupt Register (RITRG) - should show interrupt reason
- Auxiliary Status (RAUXS) - shows current phase
- Command Register (RNCOM) - shows last command
- Transfer Counter (RTCL/RTC2/RTCM) - shows bytes remaining

### Verify Interrupt Flow
Correct sequence should be:
1. C# sets STATUS bit 11
2. C# triggers Level 11 interrupt
3. SINTRAN SCINT runs
4. SINTRAN reads STATUS (RSTAU)
5. SINTRAN writes 0 to WCONT
6. C# clears interrupt
7. SINTRAN processes phase
8. SINTRAN writes 5 to WCONT
9. C# resumes operation
10. Repeat from step 1 on next phase

---

## 8. Summary

The key to fixing the C# SCSI driver is understanding that **SINTRAN expects to handle EACH SCSI phase separately**.

The hardware must:
1. **Generate interrupt on EVERY phase change**
2. **Pause operation until SINTRAN resumes**
3. **Allow SINTRAN to update NCR registers between phases**
4. **Resume operation when SINTRAN writes 5 to WCONT**

The current C# implementation executes the entire SCSI transaction synchronously, which prevents SINTRAN from controlling each phase. The fixes above add a state machine that properly pauses/resumes on phase boundaries, matching what the real hardware does.

**Most Critical Changes:**
1. Add state machine (Idle/Executing/WaitingForResume)
2. Call ProcessNCRInterrupt() in StepGoState()
3. Pause on phase change
4. Resume only when SINTRAN writes 5 to WCONT

**What NOT to Do:**
- Do NOT complete entire operation in ExecuteGo()
- Do NOT skip phase-change interrupts
- Do NOT cache NCR register values across pause/resume
- Do NOT ignore WCONT writes of 0

**Testing Strategy:**
- Start with extensive logging
- Verify interrupt fires on EACH phase
- Trace SINTRAN handler execution
- Confirm operation pauses/resumes correctly
- Test with actual disk operations

---

**Document Created:** 2025-10-13
**Based on Analysis of:** `Z:\NorskData\Source Code\Sintran L\NPL\IP-P2-SCSI-DRIV-ANALYSIS.md`
**Target Implementation:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
