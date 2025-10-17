# SINTRAN Emulator Implementation

**C# Implementation Guides for SINTRAN III and ND-500 Emulation**

---

## Overview

This folder contains implementation guides, reference code, and integration documentation for building emulators of the SINTRAN III operating system and ND-500 processor.

### Purpose

These documents bridge the gap between theoretical OS architecture (in `SINTRAN/OS/`) and practical C# implementation, providing:

- Complete C# code for accessing SINTRAN kernel structures
- ND-500 hardware emulation with multiport memory
- Integration guides for existing emulator codebases
- Quick reference cards for development

---

## Documents in This Folder

| Document | Lines | Description |
|----------|-------|-------------|
| [KERNEL-ACCESS-EMULATOR.md](KERNEL-ACCESS-EMULATOR.md) | 2767 | Complete C# implementation for reading SINTRAN III kernel data structures |
| [ND500-EMULATION-COMPLETE.cs](ND500-EMULATION-COMPLETE.cs) | 677 | C# source code for ND-500 multiport memory, message passing, and 5015 controller |
| [ND500-INTEGRATION-GUIDE.md](ND500-INTEGRATION-GUIDE.md) | 667 | Step-by-step guide to extend NDBusND500IF.cs with complete ND-500 support |
| [ND500-QUICK-REFERENCE.md](ND500-QUICK-REFERENCE.md) | 344 | Quick lookup reference for ND-500 development |
| [ND500-MESSAGE-STRUCTURE-VERIFIED.md](ND500-MESSAGE-STRUCTURE-VERIFIED.md) | 236 | Verified ND-500 message buffer structure from NPL source analysis |

**Total:** 5 documents, ~4,691 lines

---

## Quick Start

### For SINTRAN III Kernel Access

Start with **KERNEL-ACCESS-EMULATOR.md** to:
1. Understand ND-100 data type mappings (16-bit words, 32-bit doubles)
2. Implement `IMemoryAccess` interface
3. Read RT-descriptions (program control blocks)
4. Access execution queues, time queues, and waiting queues
5. Query system state for debugging

**Key Classes:**
- `RtDescription` - RT program control block (26 words)
- `IoDatafield` - Device control block
- `SintranQueueReader` - Queue traversal
- `SintranSystemReader` - Complete system state snapshot

### For ND-500 Integration

Start with **ND500-INTEGRATION-GUIDE.md** to:
1. Add multiport memory (5MPM) to your NDBusND500IF.cs
2. Implement 5015 controller (ND-500 side)
3. Add message passing between ND-100 and ND-500
4. Implement PLACE-DOMAIN functionality
5. Handle TAG-IN/TAG-OUT with 5MPM awareness

**Key Classes:**
- `MultiportMemory` - Thread-safe shared memory
- `ND5015Controller` - ND-500 side hardware
- `ND500ProcessDescriptor` - Process/domain descriptor
- `ND500MessageBuffer` - Message structure

---

## Document Relationships

```
┌─────────────────────────────────────────────────┐
│ KERNEL-ACCESS-EMULATOR.md                       │
│ - Read SINTRAN kernel from C# emulator          │
│ - Access queues, RT-descriptions, datafields    │
└────────────────┬────────────────────────────────┘
                 │
                 │ Uses concepts from
                 ↓
┌─────────────────────────────────────────────────┐
│ ../OS/ (Architecture Documentation)             │
│ - 02-QUEUE-STRUCTURES-DETAILED.md               │
│ - 17-SCHEDULER-AND-PRIORITIES.md                │
│ - 18-DEVICE-DRIVER-FRAMEWORK.md                 │
└─────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────┐
│ ND500-EMULATION-COMPLETE.cs                     │
│ - MultiportMemory class (5MPM)                  │
│ - ND5015Controller class                        │
│ - Message/process descriptor classes            │
└────────────────┬────────────────────────────────┘
                 │
                 │ Integrated via
                 ↓
┌─────────────────────────────────────────────────┐
│ ND500-INTEGRATION-GUIDE.md                      │
│ - Step-by-step NDBusND500IF.cs extension        │
│ - AttachCpu() modification                      │
│ - TAG-OUT/TAG-IN 5MPM handling                  │
│ - PLACE-DOMAIN implementation                   │
└────────────────┬────────────────────────────────┘
                 │
                 │ Quick reference
                 ↓
┌─────────────────────────────────────────────────┐
│ ND500-QUICK-REFERENCE.md                        │
│ - Data structures (one-page view)               │
│ - Operation codes                               │
│ - C# snippets                                   │
│ - Memory map                                    │
└─────────────────────────────────────────────────┘
```

---

## Related Documentation

### Architecture (Theory)
- [../OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md](../OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md) - System overview
- [../OS/02-QUEUE-STRUCTURES-DETAILED.md](../OS/02-QUEUE-STRUCTURES-DETAILED.md) - Queue mechanisms
- [../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md) - 5MPM architecture
- [../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md](../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md) - ND-500 domains
- [../OS/19-MEMORY-MAP-REFERENCE.md](../OS/19-MEMORY-MAP-REFERENCE.md) - Memory layout

### Hardware Details
- [../OS/MPM5-KEY-FINDINGS.md](../OS/MPM5-KEY-FINDINGS.md) - MPM5 multiport memory hardware
- [../OS/05-ND500-DMA-KERNEL.md](../OS/05-ND500-DMA-KERNEL.md) - DMA operations

---

## Development Workflow

### 1. Understanding SINTRAN Kernel (ND-100)

```
Read:
1. ../OS/00-SINTRAN-ARCHITECTURE-OVERVIEW.md
2. KERNEL-ACCESS-EMULATOR.md
3. Implement IMemoryAccess for your emulator

Test:
- Read RT-descriptions from memory
- Traverse execution queue
- Display system state
```

### 2. Adding ND-500 Support

```
Read:
1. ../OS/12-ND500-DOMAIN-SETUP-AND-MEMORY-MAPPING.md
2. ../OS/MPM5-KEY-FINDINGS.md (hardware details)
3. ND500-INTEGRATION-GUIDE.md

Implement:
1. MultiportMemory (from ND500-EMULATION-COMPLETE.cs)
2. ND5015Controller
3. Extend NDBusND500IF.cs
4. Test with ND500-QUICK-REFERENCE.md snippets

Verify:
- Message passing works
- PLACE-DOMAIN allocates processes
- TAG-IN/TAG-OUT access 5MPM
```

### 3. Debugging

Use **KERNEL-ACCESS-EMULATOR.md** classes to:
- Monitor execution queue changes
- Track program state transitions
- Detect deadlocks (circular waiting)
- Validate queue integrity
- Export system state as JSON

---

## Key Concepts

### SINTRAN III (ND-100)

**Word-Addressed Memory:**
- All addresses are 16-bit word addresses (not bytes)
- Physical addressing: 24-bit (up to 16MB)
- Data types: WORD (16-bit), DOUBLE (32-bit)

**Queue-Driven Scheduler:**
- Execution queue: Priority-sorted, ready-to-run programs
- Time queue: Time-sorted, scheduled programs
- Waiting queues: Per-resource, priority-sorted waiters
- Monitor queue: FIFO, pending monitor activations

**RT-Description (26 words):**
- Program control block for each RT program
- Contains saved registers, state, queue links
- Physically located in kernel data area

### ND-500

**Byte-Oriented CPU:**
- Byte-addressable memory (NOT 32-bit word CPU!)
- 32-bit memory bus = bandwidth optimization
- Multiport memory (5MPM) shared with ND-100

**Message Passing:**
- ND-500 fills message buffer in 5MPM
- Sets ITMQUEUE flag
- Interrupts ND-100 (level 12)
- ND-100 processes, clears flag, interrupts back

**Process Descriptor (32 words):**
- One per ND-500 domain
- Contains message buffer pointer, status, capabilities
- Located in 5MPM

---

## Common Pitfalls

### ❌ Wrong: Byte Addressing on ND-100
```csharp
// WRONG - ND-100 is word-addressed!
uint byteOffset = address * 2;
```

### ✅ Correct: Word Addressing
```csharp
// CORRECT - address IS the word address
ushort value = memory.ReadWord(address);
```

### ❌ Wrong: Forgetting Endianness
```csharp
// WRONG - wrong byte order
ushort value = (ushort)(bytes[0] | (bytes[1] << 8));
```

### ✅ Correct: Big-Endian
```csharp
// CORRECT - ND standard is big-endian (MSB first)
ushort value = (ushort)((bytes[0] << 8) | bytes[1]);
```

### ❌ Wrong: Not Thread-Safe 5MPM Access
```csharp
// WRONG - both CPUs access simultaneously!
public ushort ReadWord(uint offset) {
    return (ushort)((memory[offset] << 8) | memory[offset + 1]);
}
```

### ✅ Correct: Locked Access
```csharp
// CORRECT - lock for thread safety
public ushort ReadWord(uint offset) {
    lock (_accessLock) {
        return (ushort)((memory[offset] << 8) | memory[offset + 1]);
    }
}
```

---

## Testing Checklist

### SINTRAN Kernel Access
- [ ] Can read RT-descriptions from memory
- [ ] Execution queue traversal works
- [ ] Time queue ordered correctly
- [ ] Waiting queues link properly
- [ ] System state snapshot complete
- [ ] No infinite loops in queue traversal

### ND-500 Integration
- [ ] 5MPM created and accessible
- [ ] Both CPUs can read/write same location
- [ ] Message buffer write/read works
- [ ] Process descriptor allocated correctly
- [ ] TAG-IN forwarded to 5015
- [ ] Interrupt ND-100 ↔ ND-500 works

### Edge Cases
- [ ] Empty queues handled
- [ ] Circular queue detection
- [ ] Page fault during queue read
- [ ] Message buffer overflow
- [ ] Invalid process number

---

## Performance Considerations

### Queue Scanning
- Execution queue: Typically 1-10 entries (fast)
- Time queue: Can be 100+ entries (scan early, exit fast)
- Use HashSet to detect loops (prevents infinite loops)

### 5MPM Access
- Lock contention: Minimize lock duration
- Read-heavy: Consider reader-writer locks
- Batch operations when possible

### Memory Reads
- Cache PIT entries (don't re-read every access)
- Batch datafield reads when scanning
- Use async for long operations (boot, disk I/O)

---

## Support and References

### Official Documentation
- ND-100 Architecture manuals (in `/docs/` if available)
- SINTRAN III System manuals
- MPM5 Technical Description (ND-10.004.01)

### Code References
- NDBusND500IF.cs - Existing 3022 interface
- CpuND500.cs - ND-500 CPU (if available)
- NDBus.cs - ND-100 bus system

### Community
- NDInsight project repository
- SINTRAN preservation group

---

**Last Updated:** October 17, 2025  
**Status:** Production-ready implementation guides

