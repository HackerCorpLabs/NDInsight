# ND-500 Processor Documentation

**ND-500 CPU Architecture and SINTRAN Integration**

---

## Overview

Documentation for the ND-500 processor, a byte-oriented CPU used alongside the ND-100 in dual-CPU SINTRAN III systems.

---

## Documents

| Document | Source | Purpose |
|----------|--------|---------|
| [ND-500-INTERFACE.md](ND-500-INTERFACE.md) | - | ND-500 interface overview |
| [CC-P2-N500.md](CC-P2-N500.md) | CC-P2-N500.NPL | Compiler/code generation |
| [MP-P2-N500.md](MP-P2-N500.md) | MP-P2-N500.NPL | Monitor program analysis |
| [MP-P2-N500_API_Documentation.md](MP-P2-N500_API_Documentation.md) | MP-P2-N500.NPL | API documentation |
| [RP-P2-N500.md](RP-P2-N500.md) | RP-P2-N500.NPL | Runtime program analysis |
| [XC-P2-N500.md](XC-P2-N500.md) | XC-P2-N500.NPL | Executive/control program |

---

## ND-500 Architecture

### Key Characteristics

**CPU Type:** Byte-oriented (NOT 32-bit word CPU!)
- Byte-addressable memory
- 8-bit bytes as basic unit
- 16-bit and 32-bit operations supported
- 32-bit memory bus for bandwidth optimization

**Memory:**
- Byte addressing (unlike ND-100's word addressing)
- Virtual memory with segmentation
- Shared multiport memory (5MPM) with ND-100

**See:** [../OS/MPM5-KEY-FINDINGS.md](../OS/MPM5-KEY-FINDINGS.md) for hardware details

### Integration with ND-100

```
┌──────────┐         ┌──────────┐
│  ND-100  │  5MPM   │  ND-500  │
│ (Control)│◄───────►│ (Compute)│
│ Word CPU │         │ Byte CPU │
└──────────┘         └──────────┘
     │                     │
     │  Message Passing    │
     └─────────────────────┘
```

**Communication:**
- Shared multiport memory (5MPM)
- Message passing protocol
- Process descriptors in 5MPM
- TAG-IN/TAG-OUT signaling
- Interrupt-driven coordination

---

## Topics Covered

### ND-500 Programming (NPL Sources)

**CC-P2-N500.NPL:**
- Code generation for ND-500
- Compiler integration
- Object code format

**MP-P2-N500.NPL:**
- Monitor program
- System services
- Monitor calls

**RP-P2-N500.NPL:**
- Runtime support
- Program execution
- Resource management

**XC-P2-N500.NPL:**
- Executive control
- Task scheduling
- Inter-CPU coordination

### Integration with SINTRAN

**Domains:**
- ND-500 runs in "domains" (process spaces)
- Each domain has program/data segments
- Segment capabilities control access
- Monitor segment (31) for ND-100 calls

**Message Passing:**
- ND-500 requests I/O via messages
- ND-100 processes requests
- Results returned via message buffers
- DVIO/DVINST system calls

**See:** [../OS/08-MESSAGE-PASSING-DETAILED.md](../OS/08-MESSAGE-PASSING-DETAILED.md)

---

## Quick Start

### Understanding ND-500

1. **Architecture:** [ND-500-INTERFACE.md](ND-500-INTERFACE.md)
2. **5MPM Hardware:** [../OS/MPM5-KEY-FINDINGS.md](../OS/MPM5-KEY-FINDINGS.md)
3. **Domain Setup:** [../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md](../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md)

### ND-500 Programming

1. **API Reference:** [MP-P2-N500_API_Documentation.md](MP-P2-N500_API_Documentation.md)
2. **Runtime:** [RP-P2-N500.md](RP-P2-N500.md)
3. **Monitor:** [MP-P2-N500.md](MP-P2-N500.md)

### For Emulator Developers

1. **Quick Reference:** [../Emulator/ND500-QUICK-REFERENCE.md](../Emulator/ND500-QUICK-REFERENCE.md)
2. **Integration Guide:** [../Emulator/ND500-INTEGRATION-GUIDE.md](../Emulator/ND500-INTEGRATION-GUIDE.md)
3. **C# Implementation:** [../Emulator/ND500-EMULATION-COMPLETE.cs](../Emulator/ND500-EMULATION-COMPLETE.cs)

---

## Related Documentation

### OS Integration (../OS/)

| Document | Topic |
|----------|-------|
| [05-ND500-DMA-KERNEL.md](../OS/05-ND500-DMA-KERNEL.md) | DMA operations |
| [06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md) | 5MPM architecture |
| [07-ND500-IO-AND-USER-INTERACTION.md](../OS/07-ND500-IO-AND-USER-INTERACTION.md) | User interaction |
| [08-MESSAGE-PASSING-DETAILED.md](../OS/08-MESSAGE-PASSING-DETAILED.md) | Message protocol |
| [09-ND500-CODE-LOADING.md](../OS/09-ND500-CODE-LOADING.md) | Code loading |
| [10-ND500-STANDALONE-EMULATOR.md](../OS/10-ND500-STANDALONE-EMULATOR.md) | Standalone emulation |
| [12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md](../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md) | Domain configuration |

### Hardware Documentation

| Document | Topic |
|----------|-------|
| [../OS/MPM5-KEY-FINDINGS.md](../OS/MPM5-KEY-FINDINGS.md) | MPM5 multiport memory hardware |
| [../OS/MPM5-DOCUMENTATION-UPDATE-SUMMARY.md](../OS/MPM5-DOCUMENTATION-UPDATE-SUMMARY.md) | MPM5 updates |

### Emulator Implementation (../Emulator/)

| Document | Purpose |
|----------|---------|
| [KERNEL-ACCESS-EMULATOR.md](../Emulator/KERNEL-ACCESS-EMULATOR.md) | Read SINTRAN kernel |
| [ND500-EMULATION-COMPLETE.cs](../Emulator/ND500-EMULATION-COMPLETE.cs) | Complete C# code |
| [ND500-INTEGRATION-GUIDE.md](../Emulator/ND500-INTEGRATION-GUIDE.md) | Integration guide |
| [ND500-QUICK-REFERENCE.md](../Emulator/ND500-QUICK-REFERENCE.md) | Quick reference |
| [ND500-MESSAGE-STRUCTURE-VERIFIED.md](../Emulator/ND500-MESSAGE-STRUCTURE-VERIFIED.md) | Message structure |

---

## Key Concepts

### Byte-Oriented vs Word-Oriented

**ND-100 (Word CPU):**
- 16-bit words
- Word addresses (address 0x1000 = word 0x1000)
- 16-bit memory bus

**ND-500 (Byte CPU):**
- 8-bit bytes
- Byte addresses (address 0x1000 = byte 0x1000)
- 32-bit memory bus (fetches 4 bytes for efficiency)

**CRITICAL:** "32-bit" refers to **memory bus width**, NOT CPU word size!

### Multiport Memory (5MPM)

**Shared RAM:**
- Physical RAM accessible by both CPUs
- ND-100 sees it as word memory
- ND-500 sees it as byte memory
- Thread-safe access required
- Contains process descriptors, message buffers

**Address Translation:**
- ND-100: Typically 0x00040000 physical
- ND-500: Typically 0x80000000 physical
- BASE register translates addresses
- Same physical RAM, different channel addresses

### Domains

**Domain = ND-500 Process:**
- Own program/data segments
- Segment capabilities for access control
- Message buffer in 5MPM
- Process descriptor in 5MPM
- Created via PLACE-DOMAIN command

**Segment Capability:**
- 16-bit descriptor
- Contains physical segment number
- Permission bits (W, P, S)
- **S bit (bit 13):** SHARED - bypass cache for 5MPM!

---

## Common Misconceptions

### ❌ WRONG: "ND-500 is a 32-bit CPU"
### ✅ CORRECT: "ND-500 is byte-oriented with 32-bit memory bus"

The 32-bit refers to bandwidth optimization (fetch 4 bytes at once), not CPU architecture.

### ❌ WRONG: "5MPM is special memory hardware"
### ✅ CORRECT: "5MPM is standard RAM with special port modules"

MPM5 = Multiport Memory 5th generation. Standard Dynamic RAM accessed through twin 16-bit port modules.

### ❌ WRONG: "Both CPUs see same addresses"
### ✅ CORRECT: "Address translation via BASE register"

ND-100 and ND-500 see different channel addresses that map to same physical RAM.

---

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2025-10-17 | 1.0 | Initial ND-500 documentation structure |

---

**Parent:** [../README.md](../README.md) - SINTRAN Documentation  
**Related:** [../Emulator/](../Emulator/) - Emulator Implementation

