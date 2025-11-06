# Integration Guide: ND-500 Segment 31 "Other CPU" Trap Mechanism

**Complete integration instructions for the monitor call system**

## Overview

This guide explains how to integrate the segment 31 "Other CPU" trap mechanism into your ND-100/ND-500 emulator. This implements the core communication channel between ND-500 and ND-100 for monitor calls, file I/O, device access, and page faults.

## Architecture Decision: Integrated 3022/5015 Interface

**RECOMMENDED APPROACH:** Use the integrated `Interface3022_5015` class that simulates both sides.

**Why?**
- Simpler: One class handles both ND-100 and ND-500 sides
- More efficient: Direct method calls instead of simulating hardware protocol
- Easier debugging: All communication in one place
- Realistic: Hardware delay/timing not needed for emulation

**Alternative (NOT recommended for now):**
- Separate `Interface3022` and `Interface5015` classes
- More realistic but adds complexity
- Only needed if you want cycle-accurate timing

## Files Created

1. **ND500-SEGMENT31-TRAP-HANDLER.cs**
   - `ND500TrapHandler` - Handles "Other CPU" traps
   - `ND500TrapState` - Saves/restores CPU state
   - `MICFUCode` enum - Monitor call function codes

2. **Interface3022-5015.cs**
   - `Interface3022_5015` - Unified interface (both sides)
   - IOX register handling for ND-100
   - TAG register communication
   - Interrupt signaling

3. **ND100-Interrupt-Level-12-Handler.cs**
   - `ND100InterruptLevel12Handler` - Processes monitor calls
   - All MICFU code handlers (DVIO, file I/O, etc.)
   - Result writing and signaling

4. **This guide** (integration instructions)

## Step-by-Step Integration

### Step 1: Initialize Components at Boot

```csharp
public class NDSystemEmulator
{
    // Core components
    private MultiportMemory _mpm;
    private Interface3022_5015 _interface;
    private ND500CPU _nd500;
    private ND100CPU _nd100;
    private ND500TrapHandler _nd500TrapHandler;
    private ND100InterruptLevel12Handler _nd100IntHandler;

    public void Initialize()
    {
        // 1. Create multiport memory (5MPM)
        _mpm = new MultiportMemory(
            nd100BaseAddress: 0x00040000,  // ND-100 sees it here
            nd500BaseAddress: 0x80000000,  // ND-500 sees it here
            sizeBytes: 128 * 1024          // 128KB typical
        );

        // 2. Create CPUs
        _nd100 = new ND100CPU();
        _nd500 = new ND500CPU();
        _nd500.MPM5 = _mpm;

        // 3. Create 3022/5015 interface
        _interface = new Interface3022_5015(
            nd100: _nd100,
            nd500: _nd500,
            mpm: _mpm,
            deviceNumber: 0x40  // 100₈ octal
        );

        _nd500.Interface = _interface;

        // 4. Create trap handler for ND-500
        _nd500TrapHandler = new ND500TrapHandler(_mpm, _interface);
        _nd500.TrapHandler = _nd500TrapHandler;

        // 5. Create interrupt handler for ND-100
        var deviceManager = new ND100DeviceManager();
        var fileSystem = new ND100FileSystem();
        _nd100IntHandler = new ND100InterruptLevel12Handler(
            _mpm,
            _interface,
            deviceManager,
            fileSystem
        );

        Console.WriteLine("[System] ND-100/ND-500 system initialized");
    }
}
```

### Step 2: Setup Segment 31 Capabilities During PLACE-DOMAIN

When placing an ND-500 domain, set up segment 31 with "Other CPU" bit:

```csharp
public void PlaceDomain(string domainName, byte processNumber)
{
    Console.WriteLine($"[PLACE-DOMAIN] Placing {domainName} as process {processNumber}");

    // ... load domain segments, etc ...

    // CRITICAL: Setup segment 31 for monitor calls
    // Bit 15 (0x8000) = Indirect
    // Bit 14 (0x4000) = Other CPU
    ushort segment31Cap = 0x8000 | 0x4000;  // I=1, O=1

    _nd500.ProgramCapabilities[31] = segment31Cap;
    _nd500.DataCapabilities[31] = segment31Cap;

    // Write capabilities to 5MPM process descriptor
    uint descriptorAddr = (uint)(processNumber * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
    // ... write other descriptor fields ...
    _mpm.WriteWord(descriptorAddr + (31 * 2), segment31Cap);      // Program cap
    _mpm.WriteWord(descriptorAddr + (31 * 2) + 1, segment31Cap);  // Data cap

    Console.WriteLine($"[PLACE-DOMAIN] Segment 31 configured: 0x{segment31Cap:X4}");
}
```

### Step 3: Integrate Trap Detection in ND-500 CPU Loop

In your ND-500 instruction execution loop:

```csharp
public void ND500ExecuteInstruction()
{
    // Don't execute if waiting for ND-100
    if (_nd500.IsWaiting)
    {
        // Check if ND-100 has responded
        // (Interface will call TrapHandler.ResumeAfterMonitorCall when ready)
        return;
    }

    // Fetch instruction
    uint instruction = FetchInstruction(_nd500.PC);

    // Decode instruction type
    if (IsCALLGInstruction(instruction))
    {
        uint targetAddress = GetCALLGTarget(instruction);
        byte segment = (byte)((targetAddress >> 24) & 0x1F);

        // Check if this is segment 31
        if (segment == 0x1F)
        {
            ushort progCap = _nd500.ProgramCapabilities[31];

            // Check for "Other CPU" bit
            if ((progCap & 0x4000) != 0)  // Bit 14
            {
                Console.WriteLine($"[ND500] CALLG to segment 31 at PC=0x{_nd500.PC:X8}");
                Console.WriteLine($"[ND500] Triggering 'Other CPU' trap!");

                // TRIGGER TRAP!
                _nd500TrapHandler.HandleOtherCPUTrap(_nd500, targetAddress);
                return;  // Don't execute CALLG normally
            }
        }
    }

    // Execute other instructions normally...
    ExecuteNormalInstruction(instruction);
}
```

### Step 4: Integrate IOX Handling in ND-100

In your ND-100 IOX instruction handler:

```csharp
public void ND100ExecuteIOXT()
{
    // T register format: High byte = device, Low byte = offset
    ushort device = (ushort)(_nd100.T >> 8);
    ushort offset = (ushort)(_nd100.T & 0xFF);

    // Check if this is 3022 interface (ND-500 device)
    if (device == 0x40)  // 100₈ octal
    {
        // Check if this is a read or write
        bool isWrite = /* check your IOX semantics */;

        if (isWrite)
        {
            _nd100.A = _interface.HandleIOX(offset, _nd100.A);
        }
        else
        {
            _nd100.A = _interface.HandleIOX(offset);
        }
        return;
    }

    // Handle other IOX devices...
}
```

### Step 5: Process ND-100 Interrupts

In your ND-100 interrupt processing loop:

```csharp
public void ND100ProcessInterrupts()
{
    // Check for pending interrupts
    if (_nd100.HasPendingInterrupt(12))
    {
        ND100InterruptData data = _nd100.GetInterruptData(12);

        Console.WriteLine($"[ND100] Processing interrupt level 12");

        // Call interrupt handler
        _nd100IntHandler.HandleInterrupt(data);
    }

    // Handle other interrupt levels...
}
```

## Complete Example: Monitor Call Flow

Here's what happens when ND-500 user code writes to terminal:

```
1. ND-500 USER CODE:
   LDWS    R0, #1               ; Device 1 (terminal)
   LDAQ    buffer_addr          ; Buffer address
   LDWS    R2, #80              ; 80 bytes
   CALLG   DVIO                 ; Call DVIO library routine

2. DVIO LIBRARY (ND-500):
   ; Fill message buffer
   STWS    [5MPM+MESSBUFF+3], #0x01  ; MICFU = DVIO_OUT
   STWS    [5MPM+MESSBUFF+14], R0    ; Device number
   ; ... fill other parameters ...

   ; Call segment 31 to invoke ND-100
   CALLG   #0x1F000000          ; ← TRIGGER TRAP HERE

3. ND-500 CPU DETECTS TRAP:
   - Segment 31 has O=1 bit (Other CPU)
   - Triggers ND500TrapHandler.HandleOtherCPUTrap()

4. ND500TrapHandler:
   - Saves ND-500 state (PC, registers)
   - Fills message buffer with parameters
   - Sets ITMQUEUE flag
   - Calls Interface3022_5015.SignalND100MonitorCall()
   - Sets _nd500.IsWaiting = true

5. Interface3022_5015:
   - Sets TAG-OUT = MONITOR_CALL_REQUEST
   - Triggers ND-100 interrupt level 12
   - Queues interrupt in ND-100

6. ND-100 INTERRUPT HANDLER:
   - Reads TAG-IN register via IOX
   - Sees MONITOR_CALL_REQUEST
   - Reads message buffer from 5MPM
   - Calls ND100InterruptLevel12Handler.HandleInterrupt()

7. ND100InterruptLevel12Handler:
   - Reads MICFU = 0x01 (DVIO_OUT)
   - Calls HandleDVIOOut()
   - Reads data from ND-500 address (via 5MPM)
   - Writes to actual terminal device
   - Writes error code 0 (success) to message buffer
   - Clears ITMQUEUE flag
   - Calls SignalOperationComplete()

8. SignalOperationComplete():
   - Writes TAG-IN = OPERATION_COMPLETE via IOX
   - Calls Interface3022_5015.ProcessTagFromND100()
   - Calls WakeUpND500Process()

9. WakeUpND500Process():
   - Calls ND500TrapHandler.ResumeAfterMonitorCall()

10. ResumeAfterMonitorCall():
    - Restores ND-500 saved state
    - Reads error code from message buffer
    - Puts error code in R0
    - Advances PC past CALLG
    - Sets _nd500.IsWaiting = false

11. ND-500 RESUMES:
    - DVIO library routine returns
    - User code continues
    - Terminal output has been written!
```

## Debugging

### Enable Debug Output

All classes have extensive `Console.WriteLine()` logging. Example output:

```
[ND500] CALLG to segment 31 at PC=0x00010234
[ND500] Triggering 'Other CPU' trap!
[ND500-TRAP] Process 2 triggered 'Other CPU' trap
[ND500-TRAP] Saved state: PC=0x00010234, R0=0x00000001
[ND500-TRAP] MICFU code: 0x0001 (DVIO_OUT)
[ND500-TRAP] DVIO_OUT: device=1, addr=0x80001000, bytes=80
[ND500-TRAP] Message buffer filled, ITMQUEUE flag set
[5015] Process 2 requesting monitor call, message at 0x400
[3022] Triggering ND-100 interrupt level 12
[ND100-INT12] Interrupt from ND-500 process 2
[ND100-INT12] MICFU=0x0001 (DVIO_OUT)
[DeviceManager] Write to device 1: 80 bytes
[ND100-INT12] Monitor call completed with result: 0
[5015] ND-100 signaled operation complete
[5015] Waking up process 2
[ND500-TRAP] Resuming process 2 after monitor call
[ND500-TRAP] Process 2 resumed at PC=0x00010238
```

### Dump Interface State

```csharp
_interface.DumpState();
```

Output:
```
=== 3022/5015 Interface State ===
Device Number: 0x40
Status Register: 0x0008
  5ALIVE:  True
  5FAULT:  False
  5PFAIL:  False
  5DMAER:  False
Control Register: 0x0008
MAR: 0x00000400
TAG-OUT (ND-500→ND-100): 0x0001 (MONITOR_CALL_REQUEST)
TAG-IN (ND-100→ND-500): 0x0002 (OPERATION_COMPLETE)
================================
```

## Testing

### Test 1: Simple DVIO OUT

```csharp
// Setup test domain with segment 31
PlaceDomain("TEST-PROGRAM", processNumber: 2);

// Simulate ND-500 executing CALLG #0x1F000000
_nd500.CurrentProcess = 2;
_nd500.PC = 0x00010000;
_nd500.Registers[0] = 1;        // Device 1 (terminal)
_nd500.Registers[1] = 0x80001000; // Buffer address
_nd500.Registers[2] = 80;       // Byte count

// Pre-fill MICFU in message buffer
uint messageAddr = 0x400;  // Process 2's message buffer
_mpm.WriteWord(messageAddr + 6, 0x0001);  // MICFU = DVIO_OUT

// Execute CALLG - should trigger trap
_nd500.ExecuteInstruction();

// Verify ND-500 is waiting
Assert.IsTrue(_nd500.IsWaiting);

// Process ND-100 interrupt
_nd100.ProcessInterrupts();

// Verify ND-500 resumed
Assert.IsFalse(_nd500.IsWaiting);
Assert.AreEqual(0x00010004, _nd500.PC);  // Advanced past CALLG
Assert.AreEqual(0, _nd500.Registers[0]); // R0 = error code 0 (success)
```

## Common Issues

### Issue 1: ND-500 Stays Waiting Forever

**Symptoms:** `_nd500.IsWaiting` never clears

**Causes:**
1. ND-100 interrupt not triggered
2. TAG-IN not written by ND-100
3. `ResumeAfterMonitorCall()` not called

**Fix:** Check interrupt queueing and TAG register handling

### Issue 2: Wrong MICFU Code

**Symptoms:** Unknown function error in `DispatchMonitorCall()`

**Causes:**
1. MICFU not written to message buffer by user code
2. Wrong offset when reading MICFU

**Fix:** Verify message buffer layout matches documentation (MICFU at offset +3 words)

### Issue 3: Data Not Transferred

**Symptoms:** Monitor call succeeds but data is wrong

**Causes:**
1. Address translation wrong (ND-500 vs 5MPM addresses)
2. Byte order (endianness) issues

**Fix:** Ensure ND-500 addresses in 5MPM range are translated correctly

## Next Steps

1. ✅ Integrate these files into your emulator
2. Test with simple DVIO operations
3. Add more MICFU handlers as needed
4. Implement swapper (process 0) for page faults
5. Add ND-500 XMSG kernel (trap handler on ND-500 side)

## Summary

You now have:
- ✅ Complete segment 31 "Other CPU" trap detection
- ✅ ND-500 trap handler with state save/restore
- ✅ 3022/5015 interface (integrated approach)
- ✅ ND-100 interrupt level 12 handler
- ✅ All major MICFU code handlers
- ✅ Complete monitor call flow

This implements the core ND-500 ↔ ND-100 communication mechanism!
