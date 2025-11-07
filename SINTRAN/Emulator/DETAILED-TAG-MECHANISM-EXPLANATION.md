# DETAILED EXPLANATION: What "TAG-OUT/TAG-IN Does the Heavy Lifting" Means

## Overview

When I said **"Your existing TAG-OUT/TAG-IN mechanism already does most of the heavy lifting"**, here's EXACTLY what I meant:

Your existing NDBusND500IF class already implements the **hardware communication mechanism** between ND-500 and ND-100. You just need to add **new message types** for monitor calls.

---

## What You ALREADY HAVE (The Heavy Lifting)

### 1. TAG-OUT Register (ND-500 → ND-100 Communication)

**What it is:**
- A hardware register on the 3022 interface card
- ND-500 writes to this register to signal the ND-100
- Contains a command code + optional parameters

**Your existing code structure:**
```csharp
public enum TagOutCodes : byte
{
    // You already have codes like:
    DMARead = 1,
    DMAWrite = 2,
    ClearInterrupt = 3,
    // etc...
}

private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

    switch (code)
    {
        case TagOutCodes.DMARead:
            HandleDMARead(tagValue);
            break;
        case TagOutCodes.DMAWrite:
            HandleDMAWrite(tagValue);
            break;
        // etc...
    }
}
```

**What this ALREADY does for you:**
- ✅ Receives signals from ND-500
- ✅ Decodes command codes from bits 0-7
- ✅ Extracts parameters from bits 8-15
- ✅ Dispatches to handler methods via switch statement

**What you need to ADD:**
- ❌ Just add ONE new case: `MonitorCallRequest = 8`
- ❌ Add handler method `HandleMonitorCallRequest()`

---

### 2. TAG-IN Register (ND-100 → ND-500 Communication)

**What it is:**
- A hardware register on the 3022 interface card
- ND-100 writes to this register to signal the ND-500
- Contains status codes and completion signals

**Your existing code structure:**
```csharp
public enum TagInCodes : byte
{
    // You already have codes like:
    DMAComplete = 1,
    InterruptAck = 2,
    // etc...
}

private ushort tagInRegister;

// ND-100 calls this to signal ND-500
public void SetTagIn(ushort value)
{
    tagInRegister = value;
    // ND-500 reads this via IOX instruction
}

// ND-500 reads this via IOX
public ushort ReadTagIn()
{
    return tagInRegister;
}
```

**What this ALREADY does for you:**
- ✅ Sends completion signals to ND-500
- ✅ ND-500 can read it via IOX instruction
- ✅ Status codes are already defined

**What you need to ADD:**
- ❌ Just add ONE new code: `OperationComplete = 16`
- ❌ Call `SetTagIn(16)` when monitor call finishes

---

### 3. Interrupt Triggering (ND-100 Interrupt System)

**What it is:**
- 3022 interface triggers interrupts on the ND-100
- You already have interrupt level management

**Your existing code:**
```csharp
private bool[] interruptLevels = new bool[16];

public void SetInterruptBit(int level, bool value)
{
    interruptLevels[level] = value;

    if (value)
    {
        // Trigger ND-100 interrupt handler
        nd100Cpu.TriggerInterrupt(level);
    }
}
```

**What this ALREADY does for you:**
- ✅ Can trigger any interrupt level (0-15)
- ✅ ND-100 interrupt dispatch is already implemented
- ✅ Interrupt handlers can be registered

**What you need to ADD:**
- ❌ Just call `SetInterruptBit(12, true)` when monitor call arrives
- ❌ Register a handler for level 12 (if not already there)

---

### 4. DMA Read/Write (Memory Access)

**What it is:**
- 3022 interface can read/write ND-100 memory
- Used for transferring data between machines

**Your existing code:**
```csharp
public ushort ReadND100Memory(uint address)
{
    return nd100Memory.ReadWord(address);
}

public void WriteND100Memory(uint address, ushort value)
{
    nd100Memory.WriteWord(address, value);
}
```

**What this ALREADY does for you:**
- ✅ Can access ND-100 memory from 3022 interface
- ✅ Can transfer data for DMA operations

**What you need to ADD:**
- ❌ Add access to **5MPM (multiport memory)** - same pattern!
- ❌ Just reference 5MPM instead of main ND-100 memory

---

## What You NEED TO ADD (The Easy Part)

Because you already have all the **mechanisms**, you just need to add **new message types** and **one new memory region**.

### Addition #1: Multiport Memory (5MPM)

**What it is:**
- Shared memory between ND-500 and ND-100
- Message buffers live here
- Already used by your existing DMA!

**What to add:**
```csharp
public class NDBusND500IF
{
    // ADD THIS FIELD:
    private MultiportMemory _multiportMemory;

    // ADD THIS METHOD:
    public void Initialize5MPM(uint nd100Addr, uint nd500Addr, uint size)
    {
        _multiportMemory = new MultiportMemory(nd100Addr, nd500Addr, size);
    }

    // ADD THIS PROPERTY:
    public MultiportMemory MultiportMemory => _multiportMemory;
}
```

**Why this is easy:**
- It's just another memory region
- Same read/write pattern as existing DMA memory
- You already have `ReadND100Memory()` - this is identical!

---

### Addition #2: New TAG Code for Monitor Calls

**What to add to your existing enum:**

```csharp
public enum TagOutCodes : byte
{
    // ... your existing codes ...
    DMARead = 1,
    DMAWrite = 2,
    ClearInterrupt = 3,

    // ADD THIS:
    MonitorCallRequest = 8,  // ⬅️ NEW
}

public enum TagInCodes : byte
{
    // ... your existing codes ...
    DMAComplete = 1,
    InterruptAck = 2,

    // ADD THIS:
    OperationComplete = 16,  // ⬅️ NEW
}
```

**Why this is easy:**
- Just two new enum values
- Same pattern as existing codes

---

### Addition #3: Monitor Call Handler in ProcessTagOut

**Your EXISTING ProcessTagOut:**

```csharp
private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

    switch (code)
    {
        case TagOutCodes.DMARead:
            HandleDMARead(tagValue);
            break;

        case TagOutCodes.DMAWrite:
            HandleDMAWrite(tagValue);
            break;

        case TagOutCodes.ClearInterrupt:
            HandleClearInterrupt(tagValue);
            break;

        // ... more cases ...
    }
}
```

**ADD ONE CASE:**

```csharp
private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

    switch (code)
    {
        // ... existing cases ...

        // ⬇️ ADD THIS CASE:
        case TagOutCodes.MonitorCallRequest:
            HandleMonitorCallRequest(tagValue);
            break;
    }
}

// ⬇️ ADD THIS METHOD:
private void HandleMonitorCallRequest(ushort tagValue)
{
    // Extract process number from bits 8-11
    byte processNum = (byte)((tagValue >> 8) & 0x0F);

    Log($"Monitor call from process {processNum}");

    // Trigger interrupt level 12 (ALREADY HAVE THIS METHOD!)
    SetInterruptBit(12, true);
}
```

**Why this is easy:**
- Exactly same pattern as `HandleDMARead()`
- Uses your existing `SetInterruptBit()` method
- Just one new case in existing switch

---

## The Complete Flow (Using YOUR Existing Code)

Let me show you how monitor calls work using ONLY your existing mechanisms:

### Step 1: ND-500 User Program Calls DVIO

```
ND-500 user code:
    LOAD I1, #0x10        ; Device number = 16
    LOAD I2, #0x20000000  ; Buffer address
    LOAD I3, #512         ; Byte count
    CALLG #0x1F000000     ; Call segment 31 (monitor)
```

### Step 2: ND-500 CPU Detects Segment 31 Trap

```csharp
// In your ND-500 CPU ExecuteInstruction():

if (IsCALLGInstruction(instruction))
{
    uint target = GetCALLGTarget(instruction);
    byte segment = (byte)((target >> 24) & 0x1F);

    if (segment == 0x1F)  // Segment 31
    {
        // Check "Other CPU" bit in capability
        if ((_programCapabilities[31] & 0x4000) != 0)
        {
            // TRAP! Don't execute CALLG
            HandleSegment31Trap();
            return;
        }
    }
}
```

### Step 3: ND-500 Fills Message Buffer

```csharp
private void HandleSegment31Trap()
{
    byte processNum = CurrentProcessNumber;  // e.g., 1

    // Get message buffer address in 5MPM
    uint messageAddr = processNum * 256;  // Each process gets 256 bytes

    // Fill message buffer with parameters FROM REGISTERS
    _busInterface.MultiportMemory.WriteWord(messageAddr + 6, 0x01);     // MICFU = DVIO_OUT
    _busInterface.MultiportMemory.WriteWord(messageAddr + 28, I1);      // Device = 16
    _busInterface.MultiportMemory.WriteDoubleWord(messageAddr + 18, I2);// Address = 0x20000000
    _busInterface.MultiportMemory.WriteDoubleWord(messageAddr + 14, I3);// Count = 512

    // Set ITMQUEUE flag (bit 0 of 5MSFL at offset +2)
    ushort flags = _busInterface.MultiportMemory.ReadWord(messageAddr + 2);
    flags |= 0x0001;  // Set bit 0
    _busInterface.MultiportMemory.WriteWord(messageAddr + 2, flags);

    // NOW USE YOUR EXISTING TAG-OUT! ⬇️
    ushort tagOut = (ushort)(8 | (processNum << 8));  // Code 8 + process in bits 8-11
    _busInterface.Write((int)NDBusND500IF.Register.WriteTagOut, tagOut);

    // Wait
    IsWaitingForND100 = true;
}
```

### Step 4: Your EXISTING ProcessTagOut Receives It

```csharp
// In NDBusND500IF:

private void ProcessTagOut(ushort tagValue)
{
    TagOutCodes code = (TagOutCodes)(tagValue & 0x07);

    switch (code)
    {
        case TagOutCodes.MonitorCallRequest:  // Code 8
            HandleMonitorCallRequest(tagValue);
            break;
    }
}

private void HandleMonitorCallRequest(ushort tagValue)
{
    byte processNum = (byte)((tagValue >> 8) & 0x0F);  // Extract bits 8-11

    Log($"Monitor call from process {processNum}");

    // Set busy flag (you already have this!)
    isBusy = true;

    // YOUR EXISTING METHOD! ⬇️
    SetInterruptBit(12, true);
}
```

### Step 5: ND-100 Interrupt Handler Processes Request

```csharp
// In your ND-100 interrupt dispatch (YOU ALREADY HAVE THIS STRUCTURE):

public void ProcessInterrupt(int level)
{
    if (level == 12)
    {
        HandleMonitorCall();
    }
}

private void HandleMonitorCall()
{
    // Read message buffer from 5MPM
    uint messageAddr = 256;  // Process 1's buffer

    ushort micfu = _interface.MultiportMemory.ReadWord(messageAddr + 6);  // 0x01 = DVIO_OUT

    if (micfu == 0x01)  // DVIO_OUT
    {
        // Read parameters
        ushort device = _interface.MultiportMemory.ReadWord(messageAddr + 28);     // 16
        uint nd500Addr = _interface.MultiportMemory.ReadDoubleWord(messageAddr + 18); // 0x20000000
        uint count = _interface.MultiportMemory.ReadDoubleWord(messageAddr + 14);     // 512

        // Read data from ND-500 memory (via 5MPM)
        byte[] data = new byte[count];
        for (uint i = 0; i < count; i++)
        {
            data[i] = _interface.MultiportMemory.ReadByte(nd500Addr + i);
        }

        // Write to actual device (YOUR EXISTING DEVICE MANAGER!)
        deviceManager.WriteToDevice(device, data);

        // Write success to message buffer
        _interface.MultiportMemory.WriteWord(messageAddr + 8, 0);  // Error code = 0
    }

    // Clear ITMQUEUE flag
    ushort flags = _interface.MultiportMemory.ReadWord(messageAddr + 2);
    flags &= 0xFFFE;  // Clear bit 0
    _interface.MultiportMemory.WriteWord(messageAddr + 2, flags);

    // Signal completion (YOUR EXISTING MECHANISM!)
    _interface.SetTagIn(16);  // OperationComplete

    // Clear busy flag
    _interface.isBusy = false;
}
```

### Step 6: ND-500 Resumes Execution

```csharp
// In your ND-500 CPU main loop:

public void CpuLoop()
{
    while (true)
    {
        if (IsWaitingForND100)
        {
            // Check TAG-IN (YOUR EXISTING READ METHOD!)
            ushort tagIn = _busInterface.Read((int)NDBusND500IF.Register.ReadTagIn);

            if ((tagIn & 0x0F) == 16)  // OperationComplete
            {
                // Read result
                uint messageAddr = CurrentProcessNumber * 256;
                ushort errorCode = _busInterface.MultiportMemory.ReadWord(messageAddr + 8);

                // Put in I1 register
                I1 = errorCode;

                // Resume
                PC += 4;  // Past CALLG instruction
                IsWaitingForND100 = false;
            }
        }
        else
        {
            ExecuteInstruction();  // Normal execution
        }
    }
}
```

---

## Summary: What You Already Have vs What You Need

### ✅ YOU ALREADY HAVE (Working):

| Mechanism | Method | Purpose |
|-----------|--------|---------|
| TAG-OUT | `ProcessTagOut()` | Receive signals from ND-500 |
| TAG-IN | `SetTagIn()` / `ReadTagIn()` | Send signals to ND-500 |
| Interrupts | `SetInterruptBit(level)` | Trigger ND-100 interrupts |
| DMA | `ReadND100Memory()` / `WriteND100Memory()` | Access memory |
| Busy flags | `isBusy`, `isLocked` | Track interface state |
| Command dispatch | `switch (code)` | Route different commands |

### ❌ YOU NEED TO ADD (Simple):

| Addition | Effort | Reason |
|----------|--------|--------|
| `MonitorCallRequest = 8` in enum | 1 line | New TAG code |
| `OperationComplete = 16` in enum | 1 line | New TAG code |
| Case in `ProcessTagOut()` | 3 lines | New case in existing switch |
| `HandleMonitorCallRequest()` method | 5 lines | Calls `SetInterruptBit(12)` |
| `MultiportMemory _multiportMemory` field | 1 line | New memory region |
| `Initialize5MPM()` method | 3 lines | Initialize 5MPM |
| Segment 31 trap in ND-500 CPU | 20 lines | Detect trap, fill buffer, call TAG-OUT |
| ND-100 interrupt handler | 30 lines | Read buffer, dispatch MICFU, write result |

**Total new code: ~60 lines**

**Total code you already have doing the work: ~500 lines**

That's what I meant by **"your existing TAG-OUT/TAG-IN mechanism does the heavy lifting"** - you already have the entire communication infrastructure. You're just adding new message types!

---

## Key Insight

Think of it like email:

**YOU ALREADY HAVE:**
- ✅ Email client (TAG-OUT/TAG-IN)
- ✅ Send button (ProcessTagOut)
- ✅ Inbox notification (SetInterruptBit)
- ✅ Attachment support (DMA)

**YOU NEED TO ADD:**
- ❌ New folder called "Monitor Calls" (one enum value)
- ❌ Filter rule: "If subject = Monitor Call, move to that folder" (one case statement)
- ❌ Auto-reply template (one handler method)

The whole email system is already built. You're just adding a filter rule!

---

## What "7 Steps" Means in Practice

Let me map the 7 steps to ACTUAL code changes:

### Step 1: Add Multiport Memory
**File:** `NDBusND500IF.cs`
**Change:** Add 3 lines
```csharp
private MultiportMemory _multiportMemory;
public void Initialize5MPM(uint nd100, uint nd500, uint size)
    { _multiportMemory = new MultiportMemory(nd100, nd500, size); }
public MultiportMemory MultiportMemory => _multiportMemory;
```

### Step 2: Extend TAG Codes
**File:** `NDBusND500IF.cs`
**Change:** Add 2 lines to existing enums
```csharp
MonitorCallRequest = 8,    // In TagOutCodes
OperationComplete = 16,    // In TagInCodes
```

### Step 3: Add Case to ProcessTagOut
**File:** `NDBusND500IF.cs`
**Change:** Add 3 lines to existing switch
```csharp
case TagOutCodes.MonitorCallRequest:
    HandleMonitorCallRequest(tagValue);
    break;
```

### Step 4: Add Monitor Call Handler
**File:** `NDBusND500IF.cs`
**Change:** Add new method (5 lines)
```csharp
private void HandleMonitorCallRequest(ushort tagValue)
{
    byte processNum = (byte)((tagValue >> 8) & 0x0F);
    isBusy = true;
    SetInterruptBit(12, true);  // YOUR EXISTING METHOD!
}
```

### Step 5: Add Segment 31 Detection
**File:** `CpuND500.cs` (your ND-500 CPU)
**Change:** Add check in ExecuteInstruction (20 lines)
```csharp
if (segment == 0x1F && (_programCapabilities[31] & 0x4000) != 0)
{
    HandleSegment31Trap();
}
```

### Step 6: Add ND-100 Interrupt Handler
**File:** New file `ND100MonitorCallHandler.cs` OR add to existing interrupt handler
**Change:** 30 lines - read buffer, dispatch, write result

### Step 7: Wire Everything Together
**File:** `SystemInitialization.cs` (your main setup)
**Change:** 5 lines
```csharp
_interface.Initialize5MPM(0x40000, 0x80000000, 128*1024);
_cpu.AttachBusInterface(_interface);
_cpu._programCapabilities[31] = 0xC000;
_monHandler = new ND100MonitorCallHandler(_interface);
RegisterInterruptHandler(12, _monHandler.HandleInterrupt);
```

**That's it!** The 7 steps are really just **adding extensions to systems you already have**.

---

## The "Aha!" Moment

Your existing code looks like this:

```csharp
// You ALREADY have this!
switch (tagOutCode)
{
    case 1: HandleDMARead(); break;
    case 2: HandleDMAWrite(); break;
    case 3: HandleClearInterrupt(); break;
}
```

All you're doing is adding:

```csharp
// Add THIS:
switch (tagOutCode)
{
    case 1: HandleDMARead(); break;
    case 2: HandleDMAWrite(); break;
    case 3: HandleClearInterrupt(); break;
    case 8: HandleMonitorCall(); break;  // ⬅️ ONE LINE!
}
```

**That's the heavy lifting analogy!** The switch statement, the TAG register reading, the interrupt triggering - all already built. You're just adding case 8!

🎯
