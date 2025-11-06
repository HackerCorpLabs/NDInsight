# Where is 5MPM (Multiport Memory) Physically Located?

**ANSWER: 5MPM is a SEPARATE HARDWARE MODULE, not in the ND-100, not in the ND-500, and not on the interface cards!**

---

## The Actual Hardware Configuration

### Physical Reality

```
┌─────────────────────────────────────────────────────────┐
│                    ND-100 Computer                      │
│  ┌─────────────┐         ┌──────────────┐             │
│  │   ND-100    │         │ 3022 Card    │             │
│  │     CPU     │◄───────►│ (Interface)  │             │
│  └─────────────┘         └──────┬───────┘             │
│                                  │                      │
│  ┌─────────────────────────┐    │                      │
│  │ ND-100 Local RAM        │    │                      │
│  │ (Private memory)        │    │                      │
│  └─────────────────────────┘    │                      │
└─────────────────────────────────┼──────────────────────┘
                                   │
                                   │ Bus Connection
                                   │
        ┌──────────────────────────▼──────────────────────────┐
        │                                                      │
        │        MPM5 HARDWARE MODULE (Separate Box!)         │
        │                                                      │
        │  ┌────────────────────────────────────────────┐    │
        │  │     Dynamic RAM Modules                    │    │
        │  │     (256KB to 2MB physical RAM chips)      │    │
        │  └────────────────────────────────────────────┘    │
        │                 ↑                    ↑               │
        │                 │                    │               │
        │  ┌──────────────┴─────┐   ┌─────────┴────────────┐ │
        │  │  Port Module 0     │   │  Port Module 1       │ │
        │  │  (PCB 5152/5155)   │   │  (PCB 5152/5155)     │ │
        │  │                    │   │                      │ │
        │  │  - Address Window  │   │  - Address Window    │ │
        │  │  - BASE register   │   │  - BASE register     │ │
        │  │  - 16-bit channel  │   │  - 32-bit channel    │ │
        │  └──────────┬─────────┘   └──────────┬───────────┘ │
        │             │                         │              │
        └─────────────┼─────────────────────────┼──────────────┘
                      │                         │
                      │                         │ Bus Connection
                      │                         │
┌─────────────────────┼─────────────────────────┼──────────────┐
│                     │                         │              │
│  ┌──────────────────┴─┐         ┌────────────▼────────┐    │
│  │  3022 Card         │         │  5015 Card          │    │
│  │  (ND-100 side)     │         │  (ND-500 side)      │    │
│  └────────────────────┘         └─────────────────────┘    │
│                                                             │
│                   ND-500 Computer                          │
│  ┌─────────────┐                                          │
│  │   ND-500    │                                          │
│  │     CPU     │                                          │
│  └─────────────┘                                          │
│                                                             │
│  ┌─────────────────────────┐                              │
│  │ ND-500 Local RAM        │                              │
│  │ (Private memory)        │                              │
│  └─────────────────────────┘                              │
└─────────────────────────────────────────────────────────────┘
```

---

## MPM5 Hardware Components

### From Official MPM5 Manual (ND-10.004.01)

**MPM5 consists of THREE module types:**

1. **Twin 16-Bit Port Module (PCB 5152 or 5155)**
   - The "entrance" to memory for each CPU
   - Contains address decoding logic
   - Contains BASE register for address translation
   - Contains address windows (defines accessible memory range)
   - Configured for either 16-bit (ND-100) or 32-bit (ND-500) data width

2. **Dynamic RAM Module**
   - The actual physical storage (256KB to 2MB per module)
   - Standard DRAM chips
   - Multiple modules can be installed
   - Accessed by BOTH ports simultaneously

3. **Line Driver Module**
   - Drives signals between memory banks
   - Handles timing and buffering
   - Supports interleaving for bandwidth

---

## How It Works

### Connection Path

**ND-100 Access:**
```
ND-100 CPU
    ↓
3022 Interface Card (in ND-100 chassis)
    ↓
Bus connection to MPM5
    ↓
MPM5 Port Module 0 (in MPM5 hardware)
    ↓
Address translation (BASE register)
    ↓
Dynamic RAM Module (in MPM5 hardware)
```

**ND-500 Access:**
```
ND-500 CPU
    ↓
5015 Interface Card (in ND-500 chassis)
    ↓
Bus connection to MPM5
    ↓
MPM5 Port Module 1 (in MPM5 hardware)
    ↓
Address translation (BASE register)
    ↓
Dynamic RAM Module (in MPM5 hardware)
    ↑
    └── SAME physical RAM as ND-100!
```

### Address Translation Example

**The BASE register in each port module translates addresses:**

```
ND-100 Channel Address:    0x00040000
    ↓ (Port 0 BASE register)
Physical RAM Address:      0x00010000
    ↑ (Port 1 BASE register)
ND-500 Channel Address:    0x80000000

Result: Both CPUs access SAME physical RAM location!
```

---

## Key Insights

### 1. 5MPM is NOT in ND-100 RAM

**WRONG Understanding:**
> "SINTRAN allocates 5MPM from ND-100's physical RAM"

**CORRECT Understanding:**
> "SINTRAN configures address windows and BASE registers in the MPM5 port modules to make the **separate MPM5 hardware** accessible to both CPUs. The 'ADRZERO' value tells the ND-100 at what address it will see the MPM5 memory."

### 2. 5MPM is NOT on Interface Cards

**WRONG Understanding:**
> "The 3022 or 5015 cards have RAM that both CPUs access"

**CORRECT Understanding:**
> "The 3022 and 5015 cards are INTERFACES that connect the CPUs to the separate MPM5 hardware module. They don't contain the actual shared RAM."

### 3. 5MPM is Separate Hardware

**CORRECT Understanding:**
> "MPM5 is a standalone hardware module (or set of modules) that sits on the system bus. It has its own enclosure with:
> - Dynamic RAM modules (the actual storage)
> - Port modules (access control for each CPU)
> - Line drivers (signal buffering)
>
> Both CPUs connect to it via their interface cards (3022 for ND-100, 5015 for ND-500)."

---

## Why This Matters for Emulation

### Physical vs Logical View

**In real hardware:**
- MPM5 is separate physical modules
- Connected via system bus
- Each CPU accesses through different ports
- Address translation happens in hardware (BASE registers)

**In emulation:**
- You can simulate it as a shared memory region
- But you MUST implement:
  - Address translation (BASE register simulation)
  - Address windows (access control)
  - Different channel widths (16-bit vs 32-bit)
  - Arbitration between simultaneous accesses

### Emulator Implementation

```csharp
class MPM5Module
{
    // The actual physical RAM (separate from ND-100 and ND-500 RAM!)
    private byte[] _physicalRAM;

    // Port 0 (ND-100 side)
    private MPM5Port _port0;

    // Port 1 (ND-500 side)
    private MPM5Port _port1;

    public MPM5Module(uint sizeBytes)
    {
        _physicalRAM = new byte[sizeBytes];

        // Configure Port 0 for ND-100
        _port0 = new MPM5Port
        {
            ChannelWidth = 16,  // 16-bit words
            BaseRegister = 0,   // Address offset
            LowerLimit = 0x40000,
            UpperLimit = 0x60000
        };

        // Configure Port 1 for ND-500
        _port1 = new MPM5Port
        {
            ChannelWidth = 32,  // 32-bit (4 bytes)
            BaseRegister = 0x7FFC0000, // 2's complement offset
            LowerLimit = 0x80000000,
            UpperLimit = 0x80020000
        };
    }

    /// <summary>
    /// ND-100 accesses MPM5 through Port 0
    /// </summary>
    public ushort ReadWordND100(uint channelAddress)
    {
        // Check if address is within Port 0's window
        if (!_port0.IsInWindow(channelAddress))
            throw new MemoryAccessException("Address outside Port 0 window");

        // Translate channel address to physical address
        uint physicalAddr = _port0.TranslateAddress(channelAddress);

        // Read from physical RAM
        return ReadPhysicalWord(physicalAddr);
    }

    /// <summary>
    /// ND-500 accesses MPM5 through Port 1
    /// </summary>
    public uint ReadDWordND500(uint channelAddress)
    {
        // Check if address is within Port 1's window
        if (!_port1.IsInWindow(channelAddress))
            throw new MemoryAccessException("Address outside Port 1 window");

        // Translate channel address to physical address
        uint physicalAddr = _port1.TranslateAddress(channelAddress);

        // Read 32 bits (4 bytes) from physical RAM
        return ReadPhysicalDWord(physicalAddr);
    }

    private ushort ReadPhysicalWord(uint physicalAddr)
    {
        // Access actual shared RAM
        return (ushort)((_physicalRAM[physicalAddr] << 8) |
                        _physicalRAM[physicalAddr + 1]);
    }

    private uint ReadPhysicalDWord(uint physicalAddr)
    {
        // Access actual shared RAM (4 bytes)
        return (uint)((_physicalRAM[physicalAddr] << 24) |
                     (_physicalRAM[physicalAddr + 1] << 16) |
                     (_physicalRAM[physicalAddr + 2] << 8) |
                     _physicalRAM[physicalAddr + 3]);
    }
}

class MPM5Port
{
    public int ChannelWidth { get; set; }      // 16 or 32 bits
    public uint BaseRegister { get; set; }     // Address translation
    public uint LowerLimit { get; set; }       // Window start
    public uint UpperLimit { get; set; }       // Window end

    public bool IsInWindow(uint channelAddr)
    {
        return channelAddr >= LowerLimit && channelAddr < UpperLimit;
    }

    public uint TranslateAddress(uint channelAddr)
    {
        // Apply BASE register translation
        // BASE = 2's complement of (Lower - Base)
        uint offset = channelAddr - LowerLimit;
        return offset; // Simplified - real formula uses BASE register
    }
}
```

---

## SINTRAN's Role

### What SINTRAN Actually Does

When SINTRAN "allocates 5MPM", it's NOT allocating ND-100 RAM. Instead:

1. **Configures MPM5 Port Modules** (via register writes to 3022 card):
   - Sets address windows (which part of MPM5 each CPU can see)
   - Sets BASE registers (address translation)
   - Enables ports

2. **Records Addresses** in software:
   - `ADRZERO` = Channel address where ND-100 sees MPM5
   - `5MBBANK` = Bank number for ND-100 paging system
   - These tell ND-100 software how to ACCESS the MPM5, not WHERE it is physically

3. **Initializes MPM5 Contents**:
   - Writes process descriptors to MPM5 (via Port 0)
   - Writes message buffers to MPM5
   - Loads XMSG kernel to MPM5

**The MPM5 hardware itself is already installed and powered on - SINTRAN just configures it!**

---

## Summary

| Question | Answer |
|----------|--------|
| Where is 5MPM physically? | **Separate MPM5 hardware module** (not in ND-100 or ND-500) |
| Where is the RAM? | In the **MPM5 Dynamic RAM Modules** |
| Where are interface cards? | 3022 in ND-100 chassis, 5015 in ND-500 chassis |
| What do interface cards do? | Connect CPUs to MPM5 hardware via bus |
| Where is address translation? | In **MPM5 Port Modules** (BASE registers) |
| Where does SINTRAN allocate from? | SINTRAN **configures** MPM5, doesn't allocate from ND-100 RAM |

**Bottom line: 5MPM is a THIRD entity - separate from both ND-100 and ND-500, accessed by both through dedicated hardware!**

---

## Reference

- **ND-10.004.01**: MPM 5 Technical Description (June 1984)
- See: `/home/user/NDInsight/SINTRAN/OS/MPM5-KEY-FINDINGS.md` for detailed hardware documentation
