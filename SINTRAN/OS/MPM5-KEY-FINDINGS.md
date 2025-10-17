# MPM5 Multiport Memory - Key Findings

**Source:** ND-10.004.01 MPM 5 Technical Description  
**Date:** October 17, 2025  
**Purpose:** Document accurate technical details from official MPM5 hardware manual

---

## Critical Findings from MPM5 Documentation

### 1. Hardware Architecture

**MPM5 consists of three module types:**
1. **Twin 16-Bit Port Module** (PCB 5152 or 5155) - The channel's entrance to memory
2. **Dynamic RAM Module** - The physical storage
3. **Line Driver Module** - Drives signals between memory banks

### 2. Port Module Versions

**Two versions exist:**
- **5152**: Older version with 4K RAM for address windows
- **5155**: Newer version with 16K RAM, separate LOCAL/GLOBAL access decoding

**Key difference for ND-500:**
- **5155 has ALLOW bit** (Master Control Register bit 4):
  - `ALLOW = 1`: ND-500 with **32-bit wide cache** (cache line width, not CPU word size)
  - `ALLOW = 0`: ND-500 with **64-bit wide cache** (cache line width)
  
  *Note: This refers to the **cache line width** for the ND-500's internal cache, not the CPU architecture. The ND-500 is byte-oriented regardless of cache width.*

### 3. Data Channel Width Configuration

**Port Control Register bit 6:**
- `Bit 6 = 0`: **32-bit wide data channel (ND-500)**
- `Bit 6 = 1`: **16-bit wide data channel (ND-100)**

**CRITICAL CLARIFICATION:**

This is about **memory bus width**, NOT CPU architecture:

| CPU | Architecture | Memory Bus Width | Addressing | Notes |
|-----|--------------|------------------|------------|-------|
| ND-100 | 16-bit word-oriented | **16 bits** (1 word) | Word addresses | Fetches 1 word (2 bytes) per access |
| ND-500 | **Byte-oriented** | **32 bits** (4 bytes) | **Byte addresses** | Fetches 4 bytes per access for bandwidth |

**From MPM5 manual (page 19):**
> "During a memory read operation, 32 bits are read from memory. 16 bits (+ 2 parity bits) are forwarded to the 16-bit sources. **The ND-500 gets 32 (+ 4) bits at a time** from the twin port module."

**What this means:**
- ND-100 accesses memory as 16-bit words (word-addressable)
- ND-500 accesses memory as bytes (byte-addressable) but **fetches 4 bytes at once** for efficiency
- The "32-bit channel" is a **bandwidth optimization**, not the CPU word size
- **Both access the SAME physical RAM modules**, just with different bus widths

### 4. Address Windows

**Purpose:** Define which portions of the physical memory bank each port can access.

**Resolution:**
- **128 Kbyte** resolution for 32-bit words (ND-500)
- **64 Kbyte** resolution for 16-bit words (ND-100)

**From documentation (page 21):**
```
Address windows define the address range that the source sees in a memory bank.
The port tests the address against its address windows and responds ONLY if 
the channel address is within the address range of the port.
```

### 5. Address Conversion (BASE Register)

**Critical for shared memory:**

The MPM5 uses a **BASE register** to convert between:
- **Channel address** (what the CPU sees)
- **Physical bank address** (actual RAM location)

**Formula (from page 25):**
```
Value in BASE register = 2's complement of (Lower limit - Base)

Example:
  Lower limit = 00 004 000 000 (octal)
  Base        = 00 000 400 000 (octal)
  
  Using 16 bits:
  Lower limit = 000020 (octal)
  Base        = 000002 (octal)
  
  Lower limit - Base = 000016 (octal)
  2's complement     = 377762 (octal) ← Value in BASE register
```

**This means:**
- ND-100 sees the memory at one channel address
- ND-500 sees the same physical memory at a different channel address
- **BASE register performs the translation**

### 6. ND-500 Byte-Oriented Architecture vs Memory Bus Width

**CRITICAL: Do not confuse CPU architecture with memory bus width!**

**ND-500 CPU:**
- **Byte-oriented** (8-bit bytes)
- **Byte-addressable** memory model
- Can access individual bytes
- Has 16-bit and 32-bit operations on bytes/words
- Similar to modern CPUs: byte-addressable but with multi-byte operations

**ND-500 Memory Bus:**
- **32-bit wide data path** to MPM5 (bandwidth optimization)
- Fetches **4 bytes at once** from memory
- CPU can then extract individual bytes as needed
- Similar to cache lines in modern CPUs

**Analogy:**
```
Modern x86-64 CPU:
  - Byte-addressable (can read byte at 0x1000)
  - But fetches 64-byte cache lines for efficiency
  
ND-500:
  - Byte-addressable (can read byte at any address)
  - But fetches 4-byte chunks from MPM5 for efficiency
```

### 7. Interleaving

**Purpose:** Improve bandwidth by placing subsequent addresses in different hardware parts.

**Interleave types:**
- 0 way: No interleaving
- 2 way: 1 address bit shifted
- 4 way: 2 address bits shifted
- 8 way: 3 address bits shifted

**For ND-100/ND-500 shared memory:**
- ND-100 typically uses: 16-bit wide channel, 1-bank or 2-bank interleave
- ND-500 typically uses: 32-bit wide channel, 1-bank or 2-bank interleave
  - (Wide channel = fetch 4 bytes at once, not 32-bit CPU!)

### 8. Speed-Up Settings

**From Port Control Register bits 4-5 (page 33):**

| Bits 5-4 | Delay (ns) | Address stable (ns) | Typical Source |
|----------|------------|---------------------|----------------|
| 0 0      | 10         | 30                  | ND-100 without DMA |
| 0 1      | 30         | 10                  | **ND-100, ND-500** |
| 1 0      | 40         | 0                   | **ND-100, ND-500** |
| 1 1      | 60         | -20                 | MPM-5 Line Driver |

**This confirms both ND-100 and ND-500 can access the same port.**

### 9. How Shared Memory Actually Works

**Physical reality:**
```
┌──────────────────────────────────────┐
│ MPM5 Memory Bank (Physical RAM)      │
│                                      │
│  Dynamic RAM Module                  │
│  (e.g. 256KB, 512KB, 1MB, etc.)     │
└──────────────────────────────────────┘
        ↑                    ↑
        │                    │
┌───────┴────────┐   ┌───────┴────────┐
│ Port 0 (5152)  │   │ Port 1 (5152)  │
│ Twin 16-Bit    │   │ Twin 16-Bit    │
│                │   │                │
│ Address Window │   │ Address Window │
│ BASE register  │   │ BASE register  │
└────────────────┘   └────────────────┘
        ↑                    ↑
        │                    │
    ND-100               ND-500
  (16-bit)             (32-bit)
```

**Key insight:**
- **NOT** separate 16-bit and 32-bit memory regions
- **ONE** physical RAM accessed through **TWO** different ports
- Each port has its own:
  - Address windows (which parts of RAM it can access)
  - BASE register (address translation)
  - Data width configuration (16-bit or 32-bit)
  - Interleave settings

### 10. Cache Coherency - The Real Story

**CRITICAL: The MPM5 documentation does NOT mention:**
- Any special "Shared bit" in segment capabilities
- Cache bypass mechanisms
- Software-controlled cache coherency

**What the manual DOES say:**
- ND-500 can have 32-bit or 64-bit wide cache (ALLOW bit)
- Speed-up settings differ for different CPU types
- Write operations use buffered writes (unless WAIT bit set)

**Implication:**
- **Cache coherency is handled at the CPU level**, not by MPM5
- ND-500 **cache design** must handle multiport memory access
- This is likely **why** ND-500 segment capabilities have the S (Shared) flag
  - S flag tells ND-500 CPU: "This memory is shared, bypass cache"
  - It's a **CPU-level** mechanism, not MPM5-level

### 11. Parity and Error Checking

**Master Control Register bit 6:**
- `Bit 6 = 1`: **No parity check on write** (inverted)
- `Bit 6 = 0`: Parity check enabled

**RAM Module has:**
- Error memory address register
- Error memory data register
- Suppress error table
- Error correction capabilities

---

## Corrections to Previous Documentation

### ❌ INCORRECT Previous Assumption:
"Bit 13 (S flag) in ND-500 data capability tells MPM5 to bypass cache"

### ✅ CORRECT Understanding:
"Bit 13 (S flag) in ND-500 data capability tells the **ND-500 CPU** to bypass its internal cache when accessing this segment. The MPM5 hardware itself has no knowledge of this flag - it's a CPU-level mechanism."

### ❌ INCORRECT Previous Assumption:
"5MPM is a special type of memory with different addressing"

### ✅ CORRECT Understanding:
"MPM5 is standard Dynamic RAM accessed through special port modules that provide address translation, window filtering, and multi-channel access. The '5' in MPM5 refers to the generation (Multiport Memory 5), not a special memory type."

### ❌ INCORRECT Previous Assumption:
"ND-500 is a 32-bit CPU"

### ✅ CORRECT Understanding:
"ND-500 is a **byte-oriented CPU** with byte-addressable memory. The '32-bit' refers to the **memory bus width** (bandwidth optimization - fetches 4 bytes at once), NOT the CPU architecture. Similar to how modern CPUs are byte-addressable but have wide cache lines."

### ❌ INCORRECT Previous Assumption:
"ND-100 and ND-500 see the same physical address"

### ✅ CORRECT Understanding:
"ND-100 and ND-500 see DIFFERENT channel addresses due to BASE register address translation, but both addresses map to the SAME physical RAM location. Example:
- ND-100 might see address 0x00040000
- ND-500 might see address 0x80000000
- Both addresses map to physical RAM address 0x00010000 (after BASE conversion)"

---

## Summary for Documentation Updates

### What to Update:

1. **Remove references** to MPM5 having special cache bypass logic
2. **Clarify** that S flag is a CPU-level mechanism (ND-500 internal)
3. **Update** memory layout diagrams to show address translation via BASE
4. **Explain** that "multiport" means multiple ports to same RAM, not special RAM type
5. **Document** that ND-100 uses 16-bit **channel width**, ND-500 uses 32-bit **channel width** (bus bandwidth)
6. **Correct** any implications that physical addresses are identical for both CPUs
7. **EMPHASIZE** that ND-500 is **byte-oriented**, not 32-bit word-oriented
8. **Clarify** the distinction between CPU architecture vs memory bus width

### What to Emphasize:

1. **Address windows** control which parts of physical RAM each port can access
2. **BASE register** performs address translation (channel addr → physical addr)
3. **Interleaving** improves bandwidth by distributing accesses
4. **Port configuration** (bit 6) determines 16-bit vs 32-bit access
5. **Cache coherency** is handled by ND-500 CPU's S flag, not by MPM5 hardware

---

## References

- **ND-10.004.01**: MPM 5 Technical Description (June 1984)
- **ND-10.006**: Multiport Memory Channel Specifications
- **ND-10.005**: Multiport Memory 5 Bus Description

---

**This document supersedes previous assumptions about MPM5/5MPM operation.**

