# ND-500 Quick Reference Card

**Fast lookup for ND-500 emulation development**

> ## ⚠ DEPRECATED 2026-07-20 — TAG/IOX TABLES IN THIS CARD ARE FABRICATED
>
> The "TAG-OUT codes" (0x01 READ_5MPM …), the "MicrocodeFunction" enum and the IOX
> offset table below are **not real** — they came from the invented emulator protocol
> disproven against ND-30.013.02 and the SINTRAN NPL sources (real offsets are OCTAL:
> RMAR5 0, LMAR5 1, RSTA5 2, LCON5 5, MCLR5 6, TERM5 7, RTAG5 10, LTAG5 11, …).
> **Do not use this card.** Use instead:
> `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md`
> (§3.2 register decode, §10 real TAG semantics, §14 emulation checklist).
> Kept only as a record of the poisoned prior.

---

## 🎯 Core Concepts

### Multiport Memory (5MPM)
```
ND-100: 0x00040000 ←→ ND-500: 0x80000000
Size: 128KB (typical)
Thread-safe: Required!
Contiguous: Required!
```

### Communication Flow
```
ND-500 → Fill message → Set flag → Interrupt ND-100 (level 12)
ND-100 → Process → Write result → Interrupt ND-500 (level 14)
```

### Hardware Interfaces
```
ND-100 Side: 3022 Bus Interface (your NDBusND500IF.cs)
ND-500 Side: 5015 Controller (new ND5015Controller class)
```

---

## 📋 Data Structures

### Process Descriptor (32 words in 5MPM)
| Offset | Field | Size | Description |
|--------|-------|------|-------------|
| 0 | XADPROC | 16 bits | Self address |
| 2 | MESSBUFF | 16 bits | Message buffer address |
| 4 | Status | 16 bits | Process status |
| 6 | SendEnable | 16 bits | Active if > 0 |
| 8 | ReceiveState | 16 bits | Receive state |

### Message Buffer (128 words in 5MPM)
| Offset | Field | Size | Description |
|--------|-------|------|-------------|
| 0 | PLINK | 16 bits | Process link |
| 2 | 5MSFL | 16 bits | Flags (bit 0: ITMQUEUE) |
| 4 | 5PRIO | 16 bits | Priority |
| 6 | MICFU | 16 bits | Microcode function |
| 8 | 5ERRC | 16 bits | Error code |
| 10 | TODF | 32 bits | To datafield |
| 14 | NRBYT | 32 bits | Byte count |
| 18 | N500A | 32 bits | ND-500 address |
| 22 | N100A | 32 bits | ND-100 address |
| 26 | XMICF | 16 bits | Extended function |
| 28 | 5DITN | 16 bits | DIT number |
| 30 | 5CPUN | 16 bits | CPU number |
| 32+ | Data | Variable | Data area |

### Segment Capabilities (16 bits)
```
Program Capability:
┌───┬──────┬────────────────┐
│ I │ Rsvd │Physical Seg(12)│
└───┴──────┴────────────────┘
 15  14-12  11-0

I=1: Indirect (segment 31 for monitor calls)

Data Capability:
┌───┬───┬───┬───┬────────────────┐
│ W │ P │ S │ 0 │Physical Seg(12)│
└───┴───┴───┴───┴────────────────┘
 15  14  13  12  11-0

W=1: Write allowed
P=1: Parameter access
S=1: Shared (MUST set for 5MPM!)
```

---

## 🔧 Operation Codes

### LCON5 Operations (ND-100 → ND-500)
| Code | Name | Use |
|------|------|-----|
| 0x01 | READ_5MPM | Read word from 5MPM |
| 0x02 | WRITE_5MPM | Write word to 5MPM |
| 0x03 | READ_MSG | Read message |
| 0x04 | WRITE_MSG | Write message and set flag |
| 0x05 | INTERRUPT_ND100 | Trigger ND-100 interrupt |
| 0x06 | START_PROCESS | Start ND-500 process |
| 0x10 | ACTIVATE_DOMAIN | Activate domain |

### TAG-OUT Codes (ND-500 → ND-100)
| Code | Name | Use |
|------|------|-----|
| 0 | READ_MAR | Read Memory Address Register |
| 1 | WRITE_MAR | Write Memory Address Register |
| 2 | READ_STATUS | Read Status |
| 3 | WRITE_STATUS | Write Status |
| 6 | READ_DATA | Read from ND-100 memory |
| 7 | WRITE_DATA | Write to ND-100 memory |

### 5015 Registers (ND-500 side via IOXT)
| Addr | Register | Access | Purpose |
|------|----------|--------|---------|
| 0x00 | LSTA5 | R | Status register |
| 0x01 | LCON5 | R/W | Control register |
| 0x02 | LDAT5 | R/W | Data (low 16 bits) |
| 0x03 | LDAX5 | R/W | Data (high 16 bits) |
| 0x04 | LMAR5 | W | Memory address (low) |
| 0x05 | LMAR5 | W | Memory address (high) |
| 0x06 | RTAG5 | R | TAG-IN from ND-100 |
| 0x07 | UNLC5 | W | Unlock interface |

---

## 💻 C# Snippets

### Initialize 5MPM and 5015
```csharp
// In AttachCpu():
_multiportMemory = new MultiportMemory(
    nd100BaseAddress: 0x00040000,
    nd500BaseAddress: 0x80000000,
    sizeBytes: 128 * 1024
);

_nd5015Controller = new ND5015Controller(_multiportMemory, nd500Cpu);

_nd5015Controller.OnInterruptToND100 = () => SetInterruptBit(12, true);
_nd5015Controller.OnInterruptToND500 = (level) => /* Trigger ND-500 interrupt */;
```

### Check if Address in 5MPM
```csharp
// In TAG-OUT ReadData/WriteData:
if (mar >= _adrzero && mar < _adrzero + _multiportMemory.Size)
{
    uint offset = mar - _adrzero;
    dataRegister = _multiportMemory.ReadDoubleWord(offset);
}
```

### Send Message ND-100 → ND-500
```csharp
var msg = new ND500MessageBuffer
{
    BufferAddress = proc.MessageBufferAddress,
    MicrocodeFunction = 0x01,
    ND500Address = 0x80001000,
    ByteCount = 100,
    IsInQueue = true
};
msg.WriteTo5MPM(_multiportMemory);

// Trigger interrupt
_nd5015Controller.OnInterruptToND500?.Invoke(14);
```

### Receive Message ND-500 → ND-100
```csharp
// On interrupt level 12:
var msg = new ND500MessageBuffer { BufferAddress = proc.MessageBufferAddress };
msg.ReadFrom5MPM(_multiportMemory);

if (msg.IsInQueue)
{
    // Process message
    ProcessND500_IO(msg);
    
    // Clear flag and reply
    msg.IsInQueue = false;
    msg.ErrorCode = 0; // Success
    msg.WriteTo5MPM(_multiportMemory);
}
```

### PLACE-DOMAIN
```csharp
byte procNum = PlaceDomain("MY-DOMAIN", 0x00010000);

// Allocates:
// - Process descriptor at 5MPM start + (procNum * 64)
// - Message buffer after process descriptors
// - Sets program/data capabilities
// - Writes to 5MPM
```

---

## 🐛 Common Issues

### Message Not Received
```
Check:
1. msg.IsInQueue == true?
2. proc.SendEnable > 0?
3. Interrupt enabled in LCON5 (bit 0)?
4. Correct message buffer address?
```

### 5MPM Access Violation
```
Check:
1. Address in range [_adrzero, _adrzero + size)?
2. Data capability bit 13 (S) set?
3. 5MPM contiguous?
```

### ND-500 Hangs
```
Check:
1. ND-100 cleared ITMQUEUE flag?
2. ND-100 sent interrupt back to ND-500?
3. ND-500 interrupt handler working?
```

---

## 🎓 Integration Checklist

### Step 1: Add to NDBusND500IF
```csharp
✓ private MultiportMemory _multiportMemory;
✓ private ND5015Controller _nd5015Controller;
✓ private List<ND500ProcessDescriptor> _processDescriptors;
✓ private uint _adrzero;
```

### Step 2: Initialize
```csharp
✓ InitializeMultiportMemory() in AttachCpu()
✓ Wire interrupt callbacks
✓ Initialize _processDescriptors list
```

### Step 3: Extend TAG-OUT
```csharp
✓ Check for 5MPM addresses
✓ Use _multiportMemory instead of DMA
✓ Forward to _nd5015Controller
```

### Step 4: Add Methods
```csharp
✓ PlaceDomain(name, startAddr)
✓ StartND500Process(procNum)
✓ SendMessageToND500(procNum, msg)
✓ ReceiveMessageFromND500(procNum)
✓ PerformDMATransfer(...)
```

### Step 5: Test
```csharp
✓ 5MPM read/write
✓ Message passing
✓ TAG-IN forwarding
✓ Complete terminal I/O scenario
```

---

## 📊 Memory Map

```
ND-100 Physical:                ND-500 Physical:
┌────────────────┐ 0x000000    ┌────────────────┐ 0x00000000
│ Normal RAM     │              │ Domain 0       │
│                │              │  :PSEG code    │
├────────────────┤ 0x040000    │  :DSEG data    │
│ 5MPM (128KB)   │◄────────────┤────────────────┤ 0x80000000
│  Proc Desc     │     Same    │ 5MPM (128KB)   │
│  Msg Buffers   │   Physical  │  Same memory!  │
│  XMSG Kernel   │    Memory   │                │
├────────────────┤ 0x060000    ├────────────────┤ 0x80020000
│ More RAM       │              │ More RAM       │
└────────────────┘              └────────────────┘
```

---

## 🚀 Typical Flow

```
1. Boot
   └─ DetectND500() → InitializeMultiportMemory()

2. PLACE-DOMAIN
   └─ AllocateProcessDescriptor() → AllocateMessageBuffer()
      → SetupCapabilities() → WriteToamp() → Return procNum

3. Start Process
   └─ SetPC(startAddr) → SetupSegmentCaps() → SetupTrapHandlers()
      → InterruptND500() → Process runs

4. ND-500 calls DVIO
   └─ FillMessageBuffer() → SetITMQUEUE() → WriteLCON5()
      → InterruptND100(level 12)

5. ND-100 processes
   └─ ReadMessage() → Decode → ProcessIO() → WriteResult()
      → ClearITMQUEUE() → InterruptND500(level 14)

6. ND-500 continues
   └─ ReadResult() → CheckError() → Return from DVIO
```

---

## 📚 Document References

| Topic | Document |
|-------|----------|
| Complete theory | `12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md` |
| C# implementation | `ND500-EMULATION-COMPLETE.cs` |
| Integration guide | `ND500-INTEGRATION-GUIDE.md` |
| Segment files | `11-RT-SEGMENTS-AND-SEGFIL.md` |
| Code loading | `09-ND500-CODE-LOADING.md` |
| Message passing | `08-MESSAGE-PASSING-DETAILED.md` |

---

## 🔑 Key Values

| Constant | Value | Purpose |
|----------|-------|---------|
| ADRZERO (ND-100) | 0x00040000 | 5MPM base in ND-100 space |
| ADRZERO (ND-500) | 0x80000000 | 5MPM base in ND-500 space |
| 5MPM Size | 128KB | Typical multiport memory size |
| Process Desc Size | 64 bytes | 32 words |
| Message Buffer Size | 256 bytes | 128 words |
| Max Processes | 16 | Typical limit |
| ND-100 Interrupt | Level 12 | From ND-500 |
| ND-500 Interrupt | Level 12-14 | From ND-100 |
| Monitor Segment | 31 | Indirect segment for ND-100 calls |

---

**Print this page and keep it handy while coding!** 📄


