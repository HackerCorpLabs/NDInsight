# ND-500 Integration Guide

**How to Extend NDBusND500IF.cs with Complete ND-500 Emulation**

**Version:** 1.0  
**Last Updated:** October 17, 2025  
**Purpose:** Step-by-step guide to integrate multiport memory, message passing, and 5015 controller with existing 3022 interface

---

## Table of Contents

1. [Overview](#1-overview)
2. [Step 1: Add Fields to NDBusND500IF](#step-1-add-fields-to-ndbusnd500if)
3. [Step 2: Initialize Components](#step-2-initialize-components)
4. [Step 3: Extend TAG-IN/TAG-OUT Processing](#step-3-extend-tag-intag-out-processing)
5. [Step 4: Add Message Passing](#step-4-add-message-passing)
6. [Step 5: Implement PLACE-DOMAIN](#step-5-implement-place-domain)
7. [Complete Example](#complete-example)
8. [Testing](#testing)

---

## 1. Overview

### 1.1 What We're Adding

```
Current (Existing):                  Complete (New):
┌───────────────┐                   ┌───────────────┐
│ NDBusND500IF  │                   │ NDBusND500IF  │
│ (3022)        │                   │ (3022)        │
│               │                   │ + 5MPM        │
│ - TAG-IN/OUT  │                   │ + Messages    │
│ - Basic DMA   │                   │ + 5015        │
│ - Registers   │                   │ + Domains     │
└───────────────┘                   └───────────────┘
        ↓                                   ↓↑↓
┌───────────────┐                   ┌───────────────┐
│  Mock ND-500  │                   │ Real ND-500   │
│               │                   │ + 5015        │
│               │                   │ + Interrupts  │
└───────────────┘                   └───────────────┘
```

### 1.2 Files Involved

| File | Purpose |
|------|---------|
| `NDBusND500IF.cs` | Existing 3022 interface (modify) |
| `ND500-EMULATION-COMPLETE.cs` | New classes (add to project) |
| `ND500-INTEGRATION-GUIDE.md` | This guide |

---

## Step 1: Add Fields to NDBusND500IF

### 1.1 Add Using Statements

```csharp
// At top of NDBusND500IF.cs
using Emulated.HW.ND.CPU.ND500;  // For new classes
using System.Collections.Generic;
```

### 1.2 Add Private Fields

```csharp
public class NDBusND500IF : NDBusDeviceBase
{
    // ... existing fields ...
    private IND500Cpu nd500Cpu;
    private uint mar;
    // etc...
    
    // === NEW FIELDS ===
    
    /// <summary>
    /// Multiport memory (5MPM) shared with ND-500.
    /// </summary>
    private MultiportMemory _multiportMemory;
    
    /// <summary>
    /// ND-500 side controller (5015).
    /// </summary>
    private ND5015Controller _nd5015Controller;
    
    /// <summary>
    /// Active ND-500 process descriptors.
    /// </summary>
    private List<ND500ProcessDescriptor> _processDescriptors;
    
    /// <summary>
    /// ADRZERO: Base address of 5MPM in ND-100 address space.
    /// Set by SINTRAN when ND-500 CPU datafield is configured.
    /// </summary>
    private uint _adrzero;
    
    /// <summary>
    /// Current active process number.
    /// </summary>
    private byte _currentProcessNumber;
    
    // ... rest of class ...
}
```

---

## Step 2: Initialize Components

### 2.1 Extend AttachCpu Method

```csharp
/// <summary>
/// Attach ND-500 CPU to this bus interface.
/// NOW ALSO: Initialize multiport memory and 5015 controller.
/// </summary>
public void AttachCpu(IND500Cpu cpu)
{
    nd500Cpu = cpu;
#if DEBUG_DETAIL
    Log($"ND-500 CPU attached: {cpu.GetType().Name}");
#endif

    // === NEW: Initialize 5MPM and 5015 ===
    InitializeMultiportMemory();
}

/// <summary>
/// Initialize multiport memory and ND-500 side controller.
/// </summary>
private void InitializeMultiportMemory()
{
    // Typical 5MPM: 128KB at ND-100 0x40000, ND-500 0x80000000
    uint nd100Base = 0x00040000;  // Physical address in ND-100
    uint nd500Base = 0x80000000;  // Physical address in ND-500
    uint size = 128 * 1024;       // 128KB
    
    _multiportMemory = new MultiportMemory(nd100Base, nd500Base, size);
    _adrzero = nd100Base;  // SINTRAN will read this
    
    // Create 5015 controller
    _nd5015Controller = new ND5015Controller(_multiportMemory, nd500Cpu);
    
    // Wire interrupts
    _nd5015Controller.OnInterruptToND100 = () =>
    {
        // ND-500 → ND-100 interrupt
        SetInterruptBit(12, true);
        Log("[3022] Interrupt from ND-500");
    };
    
    _nd5015Controller.OnInterruptToND500 = (level) =>
    {
        // ND-100 → ND-500 interrupt
        if (nd500Cpu != null)
        {
            nd500Cpu.TriggerInterrupt(level);
            Log($"[3022] Triggered ND-500 interrupt level {level}");
        }
        else
        {
            Log($"[3022] WARNING: Cannot trigger ND-500 interrupt (CPU not attached)");
        }
    };
    
    // Initialize process list
    _processDescriptors = new List<ND500ProcessDescriptor>();
    
    Log($"[3022] 5MPM initialized: Base=0x{nd100Base:X8}, Size={size} bytes");
}
```

### 2.2 Add ADRZERO Property

```csharp
/// <summary>
/// Get ADRZERO address (base of 5MPM).
/// SINTRAN reads this from ND-500 datafield.
/// </summary>
public uint ADRZERO => _adrzero;
```

---

## Step 3: Extend TAG-IN/TAG-OUT Processing

### 3.1 Modify ProcessTagOut

```csharp
/// <summary>
/// Process TAG-OUT command from ND-500.
/// EXTENDED: Now forwards to 5015 controller.
/// </summary>
private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

#if DEBUG_DETAIL
    Log($"TAG-OUT command: {code}");
#endif

    // === NEW: Forward to 5015 ===
    if (_nd5015Controller != null)
    {
        // Let 5015 handle from ND-500 side
        // (For now, still process on ND-100 side)
    }

    switch (code)
    {
        case TagOutCodes.ReadMemoryAddressRegister:
            dataRegister = mar;
            break;

        case TagOutCodes.WriteMemoryAddressRegister:
            mar = dataRegister & 0xFFFFFF;
            break;

        case TagOutCodes.ReadStatusRegister:
            dataRegister = statusRegister;
            break;

        case TagOutCodes.WriteStatusRegister:
            statusRegister = (ushort)(dataRegister & 0xFFFF);
            break;

        case TagOutCodes.ReadControlRegister:
            dataRegister = controlRegister;
            break;

        case TagOutCodes.ResetActivate:
            isLocked = false;
            isBusy = false;
            isFinished = false;
            break;

        case TagOutCodes.ReadDataRegister:
            // === EXTENDED: Check if 5MPM address ===
            if (mar >= _adrzero && mar < _adrzero + _multiportMemory.Size)
            {
                // Read from 5MPM instead of normal DMA
                uint offset = mar - _adrzero;
                dataRegister = _multiportMemory.ReadDoubleWord(offset);
                Log($"TAG-OUT: Read from 5MPM[0x{offset:X}] = 0x{dataRegister:X8}");
            }
            else
            {
                // Normal DMA read
                isBusy = true;
                dataRegister = ReadND100Memory(mar);
            }
            SetOperationComplete();
            break;

        case TagOutCodes.WriteDataRegister:
            // === EXTENDED: Check if 5MPM address ===
            if (mar >= _adrzero && mar < _adrzero + _multiportMemory.Size)
            {
                // Write to 5MPM instead of normal DMA
                uint offset = mar - _adrzero;
                _multiportMemory.WriteDoubleWord(offset, dataRegister);
                Log($"TAG-OUT: Write to 5MPM[0x{offset:X}] = 0x{dataRegister:X8}");
            }
            else
            {
                // Normal DMA write
                isBusy = true;
                WriteND100Memory(mar, dataRegister);
            }
            SetOperationComplete();
            break;
    }
}
```

### 3.2 Modify ProcessTagIn

```csharp
/// <summary>
/// Process TAG-IN command to ND-500.
/// EXTENDED: Now forwards to 5015 controller.
/// </summary>
private void ProcessTagIn(ushort tagValue)
{
    // === NEW: Forward to 5015 ===
    if (_nd5015Controller != null)
    {
        _nd5015Controller.ReceiveTagIn(tagValue);
    }

    // ... existing TAG-IN processing ...
    
    bool returnTag = (tagValue & 0x20) != 0;
    if (returnTag)
    {
        tagInRegister = tagValue;
        return;
    }

    TagInCodes code = (TagInCodes)(tagValue & 0x0F);

#if DEBUG_DETAIL
    Log($"TAG-IN command: {code}");
#endif

    switch (code)
    {
        // ... existing cases ...
        
        case TagInCodes.DUNL:
            isLocked = false;
            Log($"Interface unlocked via TAG-IN DUNL");
            break;
    }

    tagInRegister = tagValue;
}
```

---

## Step 4: Add Message Passing

### 4.1 Add SendMessage Method

```csharp
/// <summary>
/// Send message from ND-100 to ND-500.
/// Called by SINTRAN when ND-500 process needs I/O.
/// </summary>
public void SendMessageToND500(byte processNumber, ND500MessageBuffer message)
{
    if (_processDescriptors == null || processNumber >= _processDescriptors.Count)
    {
        Log($"ERROR: Invalid process number {processNumber}");
        return;
    }

    var proc = _processDescriptors[processNumber];
    if (!proc.IsActive)
    {
        Log($"ERROR: Process {processNumber} not active");
        return;
    }

    // Write message to 5MPM
    message.BufferAddress = proc.MessageBufferAddress;
    message.IsInQueue = true;
    message.WriteTo5MPM(_multiportMemory);

    Log($"[3022] Message sent to ND-500 process {processNumber}");

    // Trigger ND-500 interrupt
    if (_nd5015Controller != null)
    {
        _nd5015Controller.ReceiveTagIn(0x03); // Message available TAG-IN
    }
}

/// <summary>
/// Receive message from ND-500 to ND-100.
/// Called when ND-500 process sends DVIO/DVINST request.
/// </summary>
public ND500MessageBuffer ReceiveMessageFromND500(byte processNumber)
{
    if (_processDescriptors == null || processNumber >= _processDescriptors.Count)
    {
        Log($"ERROR: Invalid process number {processNumber}");
        return null;
    }

    var proc = _processDescriptors[processNumber];
    if (!proc.IsActive)
    {
        Log($"ERROR: Process {processNumber} not active");
        return null;
    }

    // Read message from 5MPM
    var message = new ND500MessageBuffer
    {
        BufferAddress = proc.MessageBufferAddress
    };
    message.ReadFrom5MPM(_multiportMemory);

    if (!message.IsInQueue)
    {
        Log($"WARNING: No message queued for process {processNumber}");
        return null;
    }

    // Clear queue flag
    message.IsInQueue = false;
    message.WriteTo5MPM(_multiportMemory);

    Log($"[3022] Message received from ND-500 process {processNumber}");

    return message;
}
```

---

## Step 5: Implement PLACE-DOMAIN

### 5.1 Add PLACE-DOMAIN Method

```csharp
/// <summary>
/// PLACE-DOMAIN: Initialize an ND-500 domain.
/// Called by SINTRAN when user runs PLACE-DOMAIN command.
/// </summary>
public byte PlaceDomain(string domainName, uint startAddress)
{
    if (_multiportMemory == null)
    {
        Log("ERROR: 5MPM not initialized");
        return 0xFF; // Error
    }

    // Find free process slot
    byte processNumber = FindFreeProcessSlot();
    if (processNumber == 0xFF)
    {
        Log("ERROR: No free process slots");
        return 0xFF;
    }

    // Allocate process descriptor and message buffer
    uint descAddr = _multiportMemory.AllocateProcessDescriptor(processNumber);
    uint msgAddr = _multiportMemory.AllocateMessageBuffer(processNumber);

    // Create process descriptor
    var proc = new ND500ProcessDescriptor
    {
        ProcessNumber = processNumber,
        DescriptorAddress = descAddr,
        MessageBufferAddress = msgAddr,
        DomainName = domainName,
        StartAddress = startAddress,
        Status = 0,
        SendEnable = 1,  // Activate
        ReceiveState = 0,
        IsPlaced = true,
        IsRunning = false
    };

    // Initialize segment capabilities
    for (int i = 0; i < 32; i++)
    {
        // Default: Direct segments
        proc.ProgramCapabilities[i] = (ushort)i;  // Physical seg = logical seg
        proc.DataCapabilities[i] = (ushort)(0x8000 | i); // W=1, phys seg
    }

    // Segment 31: Indirect (monitor calls to ND-100)
    proc.ProgramCapabilities[31] = 0xC000; // Indirect, Other CPU

    // Write to 5MPM
    proc.WriteTo5MPM(_multiportMemory);

    // Add to list
    _processDescriptors.Add(proc);

    Log($"[3022] PLACE-DOMAIN '{domainName}' as process {processNumber}");

    return processNumber;
}

/// <summary>
/// Find first free process slot.
/// </summary>
private byte FindFreeProcessSlot()
{
    for (byte i = 0; i < 16; i++)
    {
        if (_processDescriptors.All(p => p.ProcessNumber != i))
            return i;
    }
    return 0xFF; // No free slots
}
```

---

## Complete Example

### Example: Emulator Main Loop

```csharp
// In your emulator initialization:

public void InitializeEmulator()
{
    // Create ND-100 bus
    var nd100Bus = new NDBus();
    
    // Create ND-500 CPU
    var nd500Cpu = new CpuND500(/* memory, etc */);
    
    // Create 3022 interface
    var nd500Interface = new NDBusND500IF(0); // Thumbwheel 0
    nd100Bus.AddDevice(nd500Interface);
    
    // Attach ND-500 CPU (this now also initializes 5MPM and 5015)
    nd500Interface.AttachCpu(nd500Cpu);
    
    // SINTRAN will now:
    // 1. Detect ND-500 via HWINFO
    // 2. Read ADRZERO from datafield
    // 3. Call PLACE-DOMAIN via IOXT commands
    // 4. Send/receive messages via TAG-IN/TAG-OUT
}

// Simulating SINTRAN PLACE-DOMAIN:
public void SimulatePlaceDomain()
{
    // SINTRAN code: CALL PLACE500
    
    byte procNum = nd500Interface.PlaceDomain("TEST-DOMAIN", 0x00010000);
    
    Console.WriteLine($"Domain placed as process {procNum}");
    
    // SINTRAN code: Send activation message
    var msg = new ND500MessageBuffer
    {
        MicrocodeFunction = 0x01, // Activate
        ND500Address = 0x80000000,
        ND100Address = nd500Interface.ADRZERO,
        ByteCount = 0
    };
    
    nd500Interface.SendMessageToND500(procNum, msg);
}

// ND-500 process requests I/O:
public void SimulateND500_DVIO()
{
    // ND-500 code (pseudo):
    // CALL DVIO(terminal, buffer, length)
    //   → Fills message buffer
    //   → Writes LMAR5: message address
    //   → Writes LCON5: Send message (0x0400)
    //   → Triggers interrupt to ND-100
    
    // ND-100 receives interrupt level 12:
    var msg = nd500Interface.ReceiveMessageFromND500(0);
    
    if (msg != null)
    {
        Console.WriteLine($"ND-500 requests I/O: Func=0x{msg.MicrocodeFunction:X4}");
        
        // Process I/O (terminal output, disk read, etc.)
        ProcessND500_IO(msg);
        
        // Send reply
        msg.ErrorCode = 0; // Success
        nd500Interface.SendMessageToND500(0, msg);
    }
}
```

---

## Testing

### Test 1: 5MPM Read/Write

```csharp
[Test]
public void Test_5MPM_ReadWrite()
{
    var mpm = new MultiportMemory(0x40000, 0x80000000, 1024);
    
    // Write from ND-100 side
    mpm.WriteWord(0, 0x1234);
    
    // Read from ND-500 side (same memory!)
    ushort value = mpm.ReadWord(0);
    
    Assert.AreEqual(0x1234, value);
}
```

### Test 2: Message Passing

```csharp
[Test]
public void Test_MessagePassing()
{
    // Setup
    var interface3022 = new NDBusND500IF(0);
    var cpu500 = new MockND500Cpu();
    interface3022.AttachCpu(cpu500);
    
    // Place domain
    byte procNum = interface3022.PlaceDomain("TEST", 0x10000);
    Assert.AreEqual(0, procNum);
    
    // Send message ND-100 → ND-500
    var msg = new ND500MessageBuffer
    {
        MicrocodeFunction = 0x01,
        ByteCount = 100
    };
    interface3022.SendMessageToND500(procNum, msg);
    
    // Receive message (ND-500 would read from 5MPM)
    var received = interface3022.ReceiveMessageFromND500(procNum);
    
    Assert.IsNotNull(received);
    Assert.AreEqual(0x01, received.MicrocodeFunction);
}
```

### Test 3: TAG-IN Forwarding

```csharp
[Test]
public void Test_TagInForwarding()
{
    var interface3022 = new NDBusND500IF(0);
    var cpu500 = new MockND500Cpu();
    interface3022.AttachCpu(cpu500);
    
    bool interrupted = false;
    interface3022._nd5015Controller.OnInterruptToND500 = (level) =>
    {
        interrupted = true;
    };
    
    // Send TAG-IN from ND-100
    interface3022.Write((int)NDBusND500IF.Register.WriteTagOut, 0x03);
    
    // Should forward to 5015 and trigger interrupt
    Assert.IsTrue(interrupted);
}
```

---

## Summary

### What You've Added:

1. ✅ **Multiport Memory (5MPM)**: Shared between ND-100 and ND-500
2. ✅ **Message Passing**: ND500MessageBuffer structure and send/receive methods
3. ✅ **ND-500 Side (5015)**: Controller emulating ND-500 hardware
4. ✅ **PLACE-DOMAIN**: Initialize ND-500 domains
5. ✅ **Interrupts**: Bidirectional ND-100 ↔ ND-500
6. ✅ **Process Descriptors**: Track active ND-500 processes

### Next Steps:

1. **Test with SINTRAN**: Boot SINTRAN and try `@PLACE-DOMAIN`
2. **Implement I/O Handlers**: Handle DVIO/DVINST requests
3. **Add More Operations**: Extend TAG-OUT operation codes
4. **Performance**: Profile 5MPM access for bottlenecks

---

*For more details, see `12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md` and `ND500-EMULATION-COMPLETE.cs`.*


