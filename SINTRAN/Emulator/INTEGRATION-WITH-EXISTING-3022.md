# Integration Guide: Segment 31 Trap with Existing 3022 Interface

**How to add monitor call support to your existing NDBusND500IF class**

## Overview

Your existing `NDBusND500IF` class already has:
- ✅ TAG-IN/TAG-OUT registers
- ✅ DMA read/write (ReadND100Memory, WriteND100Memory)
- ✅ Interrupt triggering (SetInterruptBit)
- ✅ CPU attachment (AttachCpu)

We need to add:
- ❌ Segment 31 "Other CPU" trap detection (in ND-500 CPU)
- ❌ Message buffer handling (5MPM)
- ❌ Monitor call processing (ND-100 side)

## Step 1: Add Multiport Memory (5MPM) to NDBusND500IF

Add a reference to 5MPM in your NDBusND500IF class:

```csharp
public class NDBusND500IF : NDBusDeviceBase
{
    // ... existing fields ...

    // ADD THIS:
    private MultiportMemory _multiportMemory;

    /// <summary>
    /// Initialize multiport memory (5MPM) for message passing.
    /// Call this during system initialization.
    /// </summary>
    public void Initialize5MPM(uint nd100BaseAddr, uint nd500BaseAddr, uint sizeBytes)
    {
        _multiportMemory = new MultiportMemory(nd100BaseAddr, nd500BaseAddr, sizeBytes);
        Log($"5MPM initialized: ND-100=0x{nd100BaseAddr:X8}, ND-500=0x{nd500BaseAddr:X8}, Size={sizeBytes}");
    }

    /// <summary>
    /// Get 5MPM for external access.
    /// </summary>
    public MultiportMemory MultiportMemory => _multiportMemory;
}
```

## Step 2: Extend TAG Codes for Monitor Calls

Add new TAG codes for monitor call communication:

```csharp
// ADD THESE to your TagOutCodes enum:
public enum TagOutCodes : byte
{
    // ... existing codes ...

    /// <summary>
    /// 8 - Monitor call request from ND-500
    /// </summary>
    MonitorCallRequest = 8,

    /// <summary>
    /// 9 - Page fault from ND-500
    /// </summary>
    PageFaultRequest = 9
}

// ADD THESE to your TagInCodes enum:
public enum TagInCodes : byte
{
    // ... existing codes ...

    /// <summary>
    /// 16 - Operation complete (ND-100 to ND-500)
    /// </summary>
    OperationComplete = 16,

    /// <summary>
    /// 17 - Activate process (ND-100 to ND-500)
    /// </summary>
    ActivateProcess = 17
}
```

## Step 3: Add Monitor Call Processing to ProcessTagOut

Extend your `ProcessTagOut` method to handle monitor calls:

```csharp
private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

    #if DEBUG_DETAIL
    Log($"TAG-OUT command: {code}");
    #endif

    switch (code)
    {
        // ... existing cases ...

        // ADD THIS CASE:
        case TagOutCodes.MonitorCallRequest:
            // ND-500 is requesting a monitor call
            HandleMonitorCallRequest(tagValue);
            break;

        // ADD THIS CASE:
        case TagOutCodes.PageFaultRequest:
            // ND-500 has a page fault
            HandlePageFaultRequest(tagValue);
            break;
    }
}
```

## Step 4: Add Monitor Call Handler

Add these methods to NDBusND500IF:

```csharp
/// <summary>
/// Handle monitor call request from ND-500.
/// Called when ND-500 traps to segment 31.
/// </summary>
private void HandleMonitorCallRequest(ushort tagValue)
{
    // Extract process number from TAG-OUT bits 8-11
    byte processNum = (byte)((tagValue >> 8) & 0x0F);

    Log($"Monitor call request from process {processNum}");

    // Message buffer address in 5MPM
    uint descriptorAddr = (uint)(processNum * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
    ushort messageBufOffset = _multiportMemory.ReadWord(descriptorAddr + 2);
    uint messageAddr = messageBufOffset;

    // Set interface as busy
    isBusy = true;
    isLocked = true;

    // Store message address in MAR
    mar = messageAddr;

    // Trigger interrupt level 12 on ND-100
    SetInterruptBit(12, true);

    Log($"Interrupt level 12 triggered for monitor call, message at 0x{messageAddr:X}");
}

/// <summary>
/// Handle page fault request from ND-500.
/// </summary>
private void HandlePageFaultRequest(ushort tagValue)
{
    byte processNum = (byte)((tagValue >> 8) & 0x0F);

    Log($"Page fault request from process {processNum}");

    // Similar to monitor call, but different handling
    // (Usually goes to swapper process 0)
    HandleMonitorCallRequest(tagValue);  // For now, same path
}

/// <summary>
/// Signal ND-500 that monitor call is complete.
/// Called by ND-100 interrupt handler after processing.
/// </summary>
public void SignalMonitorCallComplete()
{
    Log($"Signaling monitor call complete to ND-500");

    // Clear busy/locked flags
    isBusy = false;
    isLocked = false;
    isFinished = true;

    // Send TAG-IN to ND-500
    tagInRegister = (ushort)TagInCodes.OperationComplete;

    // In real hardware, this would trigger ND-500 interrupt
    // For emulation, ND-500 checks tagInRegister in its main loop
}
```

## Step 5: Add Segment 31 Detection to ND-500 CPU

In your ND-500 CPU emulator (IND500Cpu implementation), add trap detection:

```csharp
public class CpuND500 : IND500Cpu
{
    // ... existing fields ...

    private NDBusND500IF _busInterface;  // Reference to 3022 interface
    private ushort[] _programCapabilities = new ushort[32];
    private ushort[] _dataCapabilities = new ushort[32];

    public void AttachBusInterface(NDBusND500IF busInterface)
    {
        _busInterface = busInterface;
    }

    // In your instruction execution loop:
    public void ExecuteInstruction()
    {
        // Fetch instruction
        uint instruction = FetchInstruction(PC);

        // Check if this is CALLG instruction
        if (IsCALLGInstruction(instruction))
        {
            uint targetAddress = GetCALLGTarget(instruction);
            byte segment = (byte)((targetAddress >> 24) & 0x1F);

            // Check if calling segment 31 (0x1F = 37 octal)
            if (segment == 0x1F)
            {
                ushort progCap = _programCapabilities[31];

                // Check for "Other CPU" bit (bit 14 = 0x4000)
                if ((progCap & 0x4000) != 0)
                {
                    // SEGMENT 31 TRAP!
                    HandleSegment31Trap(targetAddress);
                    return;  // Don't execute CALLG normally
                }
            }
        }

        // Execute other instructions normally
        // ... your existing instruction execution ...
    }

    private void HandleSegment31Trap(uint targetAddress)
    {
        Console.WriteLine($"[ND500] Segment 31 'Other CPU' trap at PC=0x{PC:X8}");

        // Save ND-500 state (you'd save all registers in real implementation)
        uint savedPC = PC;

        // Get current process number (you need to track this)
        byte processNum = CurrentProcessNumber;

        // Get message buffer address from 5MPM
        uint descriptorAddr = (uint)(processNum * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
        ushort messageBufOffset = _busInterface.MultiportMemory.ReadWord(descriptorAddr + 2);
        uint messageAddr = messageBufOffset;

        // Read MICFU code from message buffer (should already be filled by user code)
        ushort micfu = _busInterface.MultiportMemory.ReadWord(messageAddr + 6);

        Console.WriteLine($"[ND500] Monitor call MICFU=0x{micfu:X4}");

        // Fill message buffer with parameters from registers
        // For DVIO_OUT (0x01):
        //   R0/I1 = Device number
        //   R1/I2 = Buffer address
        //   R2/I3 = Byte count

        if (micfu == 0x01)  // DVIO_OUT
        {
            Registers regs = CpuRegisters as Registers;
            _busInterface.MultiportMemory.WriteWord(messageAddr + 28, (ushort)regs.I1);  // Device
            _busInterface.MultiportMemory.WriteWord(messageAddr + 18, (ushort)(regs.I2 >> 16)); // Addr high
            _busInterface.MultiportMemory.WriteWord(messageAddr + 20, (ushort)(regs.I2 & 0xFFFF)); // Addr low
            _busInterface.MultiportMemory.WriteWord(messageAddr + 14, (ushort)(regs.I3 >> 16)); // Count high
            _busInterface.MultiportMemory.WriteWord(messageAddr + 16, (ushort)(regs.I3 & 0xFFFF)); // Count low
        }

        // Set ITMQUEUE flag in message buffer
        ushort flags = _busInterface.MultiportMemory.ReadWord(messageAddr + 2);
        flags |= 0x0001;  // FLAG_IN_QUEUE
        _busInterface.MultiportMemory.WriteWord(messageAddr + 2, flags);

        // Signal ND-100 via TAG-OUT
        // Bits 0-7: code (8 = MonitorCallRequest)
        // Bits 8-11: process number
        ushort tagOut = (ushort)(8 | (processNum << 8));

        // Use existing TAG-OUT mechanism
        _busInterface.Write((int)NDBusND500IF.Register.WriteTagOut, tagOut);

        // Put ND-500 in WAIT state
        IsWaitingForND100 = true;

        Console.WriteLine($"[ND500] Waiting for ND-100 to process monitor call...");
    }

    // In your main CPU loop, check for completion:
    public void CheckForMonitorCallCompletion()
    {
        if (!IsWaitingForND100)
            return;

        // Check TAG-IN register
        ushort tagIn = _busInterface.Read((int)NDBusND500IF.Register.ReadTagIn);

        if ((tagIn & 0x0F) == 16)  // OperationComplete
        {
            Console.WriteLine($"[ND500] Monitor call completed by ND-100");

            // Read result from message buffer
            byte processNum = CurrentProcessNumber;
            uint descriptorAddr = (uint)(processNum * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
            ushort messageBufOffset = _busInterface.MultiportMemory.ReadWord(descriptorAddr + 2);
            uint messageAddr = messageBufOffset;

            ushort errorCode = _busInterface.MultiportMemory.ReadWord(messageAddr + 8);

            // Put error code in I1/R0
            Registers regs = CpuRegisters as Registers;
            regs.I1 = errorCode;

            // Advance PC past CALLG (typically 4 bytes)
            PC += 4;

            // Clear WAIT state
            IsWaitingForND100 = false;

            Console.WriteLine($"[ND500] Resumed at PC=0x{PC:X8}, error code={errorCode}");
        }
    }
}
```

## Step 6: Add ND-100 Interrupt Handler

Create a new interrupt handler class or add to your existing ND-100 interrupt system:

```csharp
public class ND100MonitorCallHandler
{
    private NDBusND500IF _interface;

    public ND100MonitorCallHandler(NDBusND500IF interface)
    {
        _interface = interface;
    }

    /// <summary>
    /// Called by ND-100 when interrupt level 12 fires.
    /// </summary>
    public void HandleInterrupt()
    {
        Console.WriteLine($"[ND100] Interrupt level 12 - Monitor call request");

        // Get message buffer address from MAR
        uint messageAddr = _interface.mar;

        // Read message buffer from 5MPM
        ushort micfu = _interface.MultiportMemory.ReadWord(messageAddr + 6);

        Console.WriteLine($"[ND100] MICFU code: 0x{micfu:X4}");

        // Dispatch based on MICFU
        ushort errorCode = 0;

        switch (micfu)
        {
            case 0x01:  // DVIO_OUT
                errorCode = HandleDVIO_OUT(messageAddr);
                break;

            case 0x02:  // DVIO_IN
                errorCode = HandleDVIO_IN(messageAddr);
                break;

            // Add more MICFU handlers here...

            default:
                Console.WriteLine($"[ND100] Unknown MICFU: 0x{micfu:X4}");
                errorCode = 1;  // Error
                break;
        }

        // Write result back to message buffer
        _interface.MultiportMemory.WriteWord(messageAddr + 8, errorCode);

        // Clear ITMQUEUE flag
        ushort flags = _interface.MultiportMemory.ReadWord(messageAddr + 2);
        flags &= 0xFFFE;  // Clear bit 0
        _interface.MultiportMemory.WriteWord(messageAddr + 2, flags);

        // Signal completion to ND-500
        _interface.SignalMonitorCallComplete();

        Console.WriteLine($"[ND100] Monitor call completed with error code {errorCode}");
    }

    private ushort HandleDVIO_OUT(uint messageAddr)
    {
        // Read parameters from message buffer
        ushort device = _interface.MultiportMemory.ReadWord(messageAddr + 28);
        uint nd500Addr = _interface.MultiportMemory.ReadDoubleWord(messageAddr + 18);
        uint byteCount = _interface.MultiportMemory.ReadDoubleWord(messageAddr + 14);

        Console.WriteLine($"[ND100] DVIO_OUT: device={device}, addr=0x{nd500Addr:X8}, bytes={byteCount}");

        // Read data from ND-500 memory (via 5MPM)
        byte[] data = new byte[byteCount];
        for (uint i = 0; i < byteCount; i++)
        {
            data[i] = _interface.MultiportMemory.ReadByte(nd500Addr + i);
        }

        // Write to actual device
        // ... your device write code here ...

        return 0;  // Success
    }

    private ushort HandleDVIO_IN(uint messageAddr)
    {
        // Similar to DVIO_OUT but reversed
        // ... implement device read ...
        return 0;
    }
}
```

## Step 7: Wire Everything Together

In your system initialization:

```csharp
public class NDSystemEmulator
{
    private NDBusND500IF _nd500Interface;
    private CpuND500 _nd500Cpu;
    private ND100MonitorCallHandler _monitorCallHandler;

    public void Initialize()
    {
        // Create 3022 interface
        _nd500Interface = new NDBusND500IF(thumbwheel: 0);

        // Initialize 5MPM
        _nd500Interface.Initialize5MPM(
            nd100BaseAddr: 0x00040000,
            nd500BaseAddr: 0x80000000,
            sizeBytes: 128 * 1024
        );

        // Create ND-500 CPU
        _nd500Cpu = new CpuND500();

        // Attach CPU to interface
        _nd500Interface.AttachCpu(_nd500Cpu);
        _nd500Cpu.AttachBusInterface(_nd500Interface);

        // Setup segment 31 capabilities during PLACE-DOMAIN
        _nd500Cpu._programCapabilities[31] = 0xC000;  // I=1, O=1
        _nd500Cpu._dataCapabilities[31] = 0xC000;

        // Create monitor call handler
        _monitorCallHandler = new ND100MonitorCallHandler(_nd500Interface);

        Console.WriteLine("[System] ND-100/ND-500 system initialized with segment 31 support");
    }

    // In your ND-100 interrupt dispatch:
    public void ProcessND100Interrupt(int level)
    {
        if (level == 12)
        {
            // Check if this is from ND-500
            if (_nd500Interface.isBusy)
            {
                _monitorCallHandler.HandleInterrupt();
            }
        }
    }

    // In your ND-500 CPU loop:
    public void ND500CpuLoop()
    {
        while (running)
        {
            // Check for monitor call completion
            _nd500Cpu.CheckForMonitorCallCompletion();

            // Execute instruction (which may trigger segment 31 trap)
            _nd500Cpu.ExecuteInstruction();
        }
    }
}
```

## Summary

**You keep your existing code** and add:

1. ✅ Multiport memory reference in NDBusND500IF
2. ✅ New TAG codes for monitor calls
3. ✅ HandleMonitorCallRequest() in ProcessTagOut
4. ✅ Segment 31 trap detection in ND-500 CPU
5. ✅ ND-100 interrupt handler for MICFU dispatch

**Key integration points:**
- Use your existing TAG-OUT to signal requests
- Use your existing TAG-IN for completion
- Use your existing SetInterruptBit(12) for ND-100 interrupt
- Use your existing DMA for reading/writing 5MPM

This integrates smoothly with your current architecture!
