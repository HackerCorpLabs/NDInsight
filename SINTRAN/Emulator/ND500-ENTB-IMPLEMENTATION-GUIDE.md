# ND-500 ENTB (Enter Block) - Complete Implementation Guide

## Table of Contents

- [Quick Reference](#quick-reference)
- [1. Heap Structure and Memory Layout](#1-heap-structure-and-memory-layout)
  - [Heap Variables (pointed to by TOS register)](#heap-variables-pointed-to-by-tos-register)
  - [Free Block Format](#free-block-format)
- [2. ENTB Algorithm (Step by Step)](#2-entb-algorithm-step-by-step)
  - [Step 1: Read Heap Variables](#step-1-read-heap-variables)
  - [Step 2: Validate Log Size](#step-2-validate-log-size)
  - [Step 3: Search Free Lists for Available Block](#step-3-search-free-lists-for-available-block)
  - [Step 4: Split Block if Necessary (Buddy System)](#step-4-split-block-if-necessary-buddy-system)
  - [Step 5: Initialize Stack Frame (Like ENTS)](#step-5-initialize-stack-frame-like-ents)
  - [Step 6: Update CPU Registers](#step-6-update-cpu-registers)
- [3. RETB / RETBK - Return and Free Block](#3-retb--retbk---return-and-free-block)
  - [RETB Algorithm](#retb-algorithm)
  - [Free Block Algorithm (FREEB)](#free-block-algorithm-freeb)
- [4. Complete C# Implementation](#4-complete-c-implementation)
  - [EntbInstruction.cs](#entbinstructioncs)
  - [RetbInstruction.cs](#retbinstructioncs)
  - [GetbInstruction.cs](#getbinstructioncs)
  - [FreebInstruction.cs](#freebinstructioncs)
- [5. Heap Initialization Example](#5-heap-initialization-example)
- [6. Test Example](#6-test-example)
- [7. Key Implementation Notes](#7-key-implementation-notes)
- [8. Troubleshooting](#8-troubleshooting)
- [Summary](#summary)

## Quick Reference

**ENTB** allocates a local data area from the heap using buddy allocation and enters a subroutine.

**Instruction Format:**
```
ENTB <log size/r/BY>
Hex: 0x0BD | Octal: 275
```

**What it does:**
1. Allocates block of size 2^logSize words from heap
2. Sets B register to allocated block address
3. Initializes stack frame (like ENTS)
4. Stores log size in B.LOG
5. Triggers STO trap if no memory available

---

## 1. Heap Structure and Memory Layout

### Heap Variables (pointed to by TOS register)

The TOS register points to a structure containing heap management variables:

```
Offset  | Field        | Description
--------|--------------|------------------------------------------
+0      | MAXL         | Maximum log size of allocatable blocks
+1      | STAH         | Start address of heap (not used by CPU)
+2      | ENDH         | End address of heap (not used by CPU)
+3      | FLOG0        | Free list for 2^0 = 1 word blocks
+4      | FLOG1        | Free list for 2^1 = 2 word blocks
+5      | FLOG2        | Free list for 2^2 = 4 word blocks
+6      | FLOG3        | Free list for 2^3 = 8 word blocks
...     | ...          | ...
+3+n    | FLOG[n]      | Free list for 2^n word blocks
...     | ...          | ...
+3+MAXL | FLOG[MAXL]   | Free list for largest blocks
```

**Important Notes:**
- STAH and ENDH are reserved for user trap handlers (CPU doesn't use them)
- Each FLOG[n] is a pointer to the head of the free list for that size
- Value of 0 means the list is empty

### Free Block Format

Each free block in the heap has this structure:

```
Block Address → [NEXT]  // Address of next free block (0 = end of list)
                [...]   // Rest of block (unused)
```

**Key Points:**
- Only the FIRST word contains the next pointer
- Rest of the block is unused (available for allocation)
- When allocated, entire block is given to program

---

## 2. ENTB Algorithm (Step by Step)

### Step 1: Read Heap Variables

```csharp
uint tosAddr = cpu.TOS;
ushort maxl = memory.ReadWord(tosAddr + 0);      // MAXL
ushort stah = memory.ReadWord(tosAddr + 1);      // STAH (optional)
ushort endh = memory.ReadWord(tosAddr + 2);      // ENDH (optional)
```

### Step 2: Validate Log Size

```csharp
byte logSize = GetLogSizeOperand(); // from instruction operand

// Check if requested size exceeds maximum
if (logSize > maxl)
{
    cpu.RaiseTrap(TrapType.StackOverflow); // STO trap
    return;
}
```

### Step 3: Search Free Lists for Available Block

Try to find a block of the requested size or larger:

```csharp
uint allocatedBlock = 0;
byte actualLogSize = logSize;

// Search from requested size up to MAXL
for (byte searchLog = logSize; searchLog <= maxl; searchLog++)
{
    uint flogAddr = tosAddr + 3 + searchLog;
    uint blockAddr = memory.ReadWord(flogAddr);

    if (blockAddr != 0)
    {
        // Found a block!
        allocatedBlock = blockAddr;
        actualLogSize = searchLog;

        // Unlink from free list
        uint nextBlock = memory.ReadWord(blockAddr); // Read NEXT pointer
        memory.WriteWord(flogAddr, (ushort)nextBlock); // Update list head
        break;
    }
}

// If no block found, raise STO trap
if (allocatedBlock == 0)
{
    cpu.RaiseTrap(TrapType.StackOverflow);
    return;
}
```

### Step 4: Split Block if Necessary (Buddy System)

If we found a larger block than needed, split it:

```csharp
// Split larger blocks down to requested size
while (actualLogSize > logSize)
{
    actualLogSize--; // Reduce by one power of 2

    uint blockSize = (uint)(1 << actualLogSize); // 2^actualLogSize
    uint buddyAddr = allocatedBlock + blockSize; // Second half

    // Add buddy to appropriate free list
    uint flogAddr = tosAddr + 3 + actualLogSize;
    uint currentHead = memory.ReadWord(flogAddr);

    memory.WriteWord(buddyAddr, (ushort)currentHead); // buddy.NEXT = old head
    memory.WriteWord(flogAddr, (ushort)buddyAddr);    // FLOG[n] = buddy
}

// Now allocatedBlock is exactly 2^logSize words
```

### Step 5: Initialize Stack Frame (Like ENTS)

```csharp
uint blockAddr = allocatedBlock;
uint oldB = cpu.B;

// B.PREVB (offset +0) - previous B register
memory.WriteWord(blockAddr + 0, (ushort)oldB);

// B.RETA (offset +1) - return address from L register
memory.WriteWord(blockAddr + 1, (ushort)cpu.L);

// B.SP (offset +2) - copy from old stack frame
if (oldB != 0)
{
    ushort oldSP = memory.ReadWord(oldB + 2);
    memory.WriteWord(blockAddr + 2, oldSP);
}
else
{
    memory.WriteWord(blockAddr + 2, 0);
}

// B.N (offset +10) - number of arguments (from descriptor if any)
// This would come from CALL instruction
ushort numArgs = 0; // Get from call context
memory.WriteWord(blockAddr + 10, numArgs);

// B.LOG (offset +11) - store LOG SIZE (not block size!)
memory.WriteWord(blockAddr + 11, logSize);

// B.ARG (offset +12 onwards) - copy argument addresses
// Copy from call instruction context
```

### Step 6: Update CPU Registers

```csharp
// Set B register to allocated block
cpu.B = blockAddr;

// Update L register (RETA was already copied above)
cpu.L = cpu.P; // Current instruction as return address
cpu.P = targetAddress; // Jump to subroutine (from CALL/operand)
```

---

## 3. RETB / RETBK - Return and Free Block

When returning from an ENTB subroutine, the block must be freed:

### RETB Algorithm

```csharp
public void ExecuteRETB(bool setK)
{
    uint blockAddr = cpu.B;

    // Read log size from B.LOG
    byte logSize = (byte)memory.ReadWord(blockAddr + 11);

    // Read return information
    uint prevB = memory.ReadWord(blockAddr + 0); // B.PREVB
    uint retAddr = memory.ReadWord(blockAddr + 1); // B.RETA

    // Free the block back to heap
    FreeBlock(blockAddr, logSize);

    // Restore registers
    cpu.B = prevB;
    cpu.P = retAddr;
    cpu.L = retAddr;

    // Set/clear K flag
    if (setK)
        cpu.STATUS.K = true;
    else
        cpu.STATUS.K = false;
}
```

### Free Block Algorithm (FREEB)

```csharp
public void FreeBlock(uint blockAddr, byte logSize)
{
    uint tosAddr = cpu.TOS;
    ushort maxl = memory.ReadWord(tosAddr + 0);

    // Validate log size
    if (logSize > maxl)
    {
        throw new InvalidOperationException($"Invalid log size {logSize}");
    }

    // Get current head of free list
    uint flogAddr = tosAddr + 3 + logSize;
    uint currentHead = memory.ReadWord(flogAddr);

    // Link block to head of free list
    memory.WriteWord(blockAddr, (ushort)currentHead); // block.NEXT = old head
    memory.WriteWord(flogAddr, (ushort)blockAddr);     // FLOG[n] = block

    // Note: Blocks are NOT combined (buddy merging)
    // That's done by user trap handler for STO trap
}
```

---

## 4. Complete C# Implementation

### EntbInstruction.cs

```csharp
using System;

namespace ND500.CPU.Instructions
{
    /// <summary>
    /// ENTB - Enter subroutine with buddy allocation
    /// Allocates local data area from heap using buddy system
    /// Opcode: 0x0BD (275 octal)
    /// </summary>
    public class EntbInstruction : IInstruction
    {
        private readonly ND500CPU _cpu;
        private readonly IMemory _memory;

        public EntbInstruction(ND500CPU cpu, IMemory memory)
        {
            _cpu = cpu;
            _memory = memory;
        }

        public void Execute(InstructionContext context)
        {
            // Get log size operand
            byte logSize = context.GetByteOperand();

            // Get target address for subroutine
            uint targetAddress = context.TargetAddress;

            // Allocate block from heap
            uint blockAddr = AllocateBlock(logSize);

            if (blockAddr == 0)
            {
                // No memory available - STO trap
                _cpu.RaiseTrap(TrapType.StackOverflow);
                return;
            }

            // Initialize stack frame
            InitializeStackFrame(blockAddr, logSize, targetAddress, context);

            // Update CPU registers
            _cpu.B = blockAddr;
            _cpu.P = targetAddress;
        }

        private uint AllocateBlock(byte logSize)
        {
            uint tosAddr = _cpu.TOS;

            // Read MAXL
            ushort maxl = _memory.ReadWord(tosAddr + 0);

            // Validate log size
            if (logSize > maxl)
            {
                return 0; // Will trigger STO trap
            }

            // Search for available block (requested size or larger)
            uint allocatedBlock = 0;
            byte actualLogSize = logSize;

            for (byte searchLog = logSize; searchLog <= maxl; searchLog++)
            {
                uint flogAddr = tosAddr + 3 + searchLog;
                uint blockAddr = _memory.ReadWord(flogAddr);

                if (blockAddr != 0)
                {
                    // Found a block - unlink from free list
                    uint nextBlock = _memory.ReadWord(blockAddr);
                    _memory.WriteWord(flogAddr, (ushort)nextBlock);

                    allocatedBlock = blockAddr;
                    actualLogSize = searchLog;
                    break;
                }
            }

            if (allocatedBlock == 0)
            {
                return 0; // No block found
            }

            // Split block if necessary (buddy system)
            while (actualLogSize > logSize)
            {
                actualLogSize--;

                uint blockSize = (uint)(1 << actualLogSize); // 2^actualLogSize
                uint buddyAddr = allocatedBlock + blockSize;

                // Add buddy to free list
                uint flogAddr = tosAddr + 3 + actualLogSize;
                uint currentHead = _memory.ReadWord(flogAddr);

                _memory.WriteWord(buddyAddr, (ushort)currentHead);
                _memory.WriteWord(flogAddr, (ushort)buddyAddr);
            }

            return allocatedBlock;
        }

        private void InitializeStackFrame(uint blockAddr, byte logSize,
                                          uint targetAddress, InstructionContext context)
        {
            uint oldB = _cpu.B;

            // B.PREVB (offset +0) - previous B register
            _memory.WriteWord(blockAddr + 0, (ushort)oldB);

            // B.RETA (offset +1) - return address
            _memory.WriteWord(blockAddr + 1, (ushort)_cpu.L);

            // B.SP (offset +2) - stack pointer from old frame
            if (oldB != 0)
            {
                ushort oldSP = _memory.ReadWord(oldB + 2);
                _memory.WriteWord(blockAddr + 2, oldSP);
            }
            else
            {
                _memory.WriteWord(blockAddr + 2, 0);
            }

            // B.N (offset +10) - number of arguments
            ushort numArgs = context.NumArguments;
            _memory.WriteWord(blockAddr + 10, numArgs);

            // B.LOG (offset +11) - CRITICAL: Store log size, not block size!
            _memory.WriteWord(blockAddr + 11, logSize);

            // B.ARG (offset +12 onwards) - copy argument pointers
            for (int i = 0; i < numArgs; i++)
            {
                uint argAddr = context.GetArgumentAddress(i);
                _memory.WriteWord(blockAddr + 12 + (uint)i, (ushort)argAddr);
            }
        }
    }
}
```

### RetbInstruction.cs

```csharp
using System;

namespace ND500.CPU.Instructions
{
    /// <summary>
    /// RETB - Return from buddy subroutine
    /// Opcode: 0xFE1C (177034 octal)
    ///
    /// RETBK - Return from buddy subroutine with K flag set
    /// Opcode: 0xFE1D (177035 octal)
    /// </summary>
    public class RetbInstruction : IInstruction
    {
        private readonly ND500CPU _cpu;
        private readonly IMemory _memory;
        private readonly bool _setK;

        public RetbInstruction(ND500CPU cpu, IMemory memory, bool setK)
        {
            _cpu = cpu;
            _memory = memory;
            _setK = setK;
        }

        public void Execute(InstructionContext context)
        {
            uint blockAddr = _cpu.B;

            if (blockAddr == 0)
            {
                _cpu.RaiseTrap(TrapType.StackUnderflow);
                return;
            }

            // Read log size from B.LOG (offset +11)
            byte logSize = (byte)_memory.ReadWord(blockAddr + 11);

            // Read return information
            uint prevB = _memory.ReadWord(blockAddr + 0); // B.PREVB
            uint retAddr = _memory.ReadWord(blockAddr + 1); // B.RETA

            // Free the block back to heap
            FreeBlock(blockAddr, logSize);

            // Restore registers
            _cpu.B = prevB;
            _cpu.P = retAddr;
            _cpu.L = retAddr;

            // Set/clear K flag
            _cpu.STATUS.K = _setK;
        }

        private void FreeBlock(uint blockAddr, byte logSize)
        {
            uint tosAddr = _cpu.TOS;

            // Read MAXL
            ushort maxl = _memory.ReadWord(tosAddr + 0);

            // Validate log size
            if (logSize > maxl)
            {
                throw new InvalidOperationException(
                    $"Invalid log size {logSize} in RETB (MAXL={maxl})");
            }

            // Get free list head for this size
            uint flogAddr = tosAddr + 3 + logSize;
            uint currentHead = _memory.ReadWord(flogAddr);

            // Link block to head of free list
            _memory.WriteWord(blockAddr, (ushort)currentHead); // block.NEXT = old head
            _memory.WriteWord(flogAddr, (ushort)blockAddr);     // FLOG[n] = block

            // Note: Blocks are NOT automatically combined
            // Buddy merging is done by user trap handler for STO trap
        }
    }
}
```

### GetbInstruction.cs

```csharp
using System;

namespace ND500.CPU.Instructions
{
    /// <summary>
    /// GETB - Get buddy element from heap
    /// Format: Wn GETB <log size/r/BY>
    /// Opcode: 0xFE4C+(n-1) (177114B+(n-1) octal)
    /// </summary>
    public class GetbInstruction : IInstruction
    {
        private readonly ND500CPU _cpu;
        private readonly IMemory _memory;
        private readonly byte _destRegister; // W1-W7

        public GetbInstruction(ND500CPU cpu, IMemory memory, byte destRegister)
        {
            _cpu = cpu;
            _memory = memory;
            _destRegister = destRegister;
        }

        public void Execute(InstructionContext context)
        {
            // Get log size operand
            byte logSize = context.GetByteOperand();

            // Allocate block (same algorithm as ENTB)
            uint blockAddr = AllocateBlock(logSize);

            if (blockAddr == 0)
            {
                _cpu.RaiseTrap(TrapType.StackOverflow);
                return;
            }

            // Store address in destination register
            _cpu.W[_destRegister] = (ushort)blockAddr;
        }

        private uint AllocateBlock(byte logSize)
        {
            uint tosAddr = _cpu.TOS;
            ushort maxl = _memory.ReadWord(tosAddr + 0);

            if (logSize > maxl)
                return 0;

            // Search for available block
            uint allocatedBlock = 0;
            byte actualLogSize = logSize;

            for (byte searchLog = logSize; searchLog <= maxl; searchLog++)
            {
                uint flogAddr = tosAddr + 3 + searchLog;
                uint blockAddr = _memory.ReadWord(flogAddr);

                if (blockAddr != 0)
                {
                    uint nextBlock = _memory.ReadWord(blockAddr);
                    _memory.WriteWord(flogAddr, (ushort)nextBlock);

                    allocatedBlock = blockAddr;
                    actualLogSize = searchLog;
                    break;
                }
            }

            if (allocatedBlock == 0)
                return 0;

            // Split block if necessary
            while (actualLogSize > logSize)
            {
                actualLogSize--;
                uint blockSize = (uint)(1 << actualLogSize);
                uint buddyAddr = allocatedBlock + blockSize;

                uint flogAddr = tosAddr + 3 + actualLogSize;
                uint currentHead = _memory.ReadWord(flogAddr);

                _memory.WriteWord(buddyAddr, (ushort)currentHead);
                _memory.WriteWord(flogAddr, (ushort)buddyAddr);
            }

            return allocatedBlock;
        }
    }
}
```

### FreebInstruction.cs

```csharp
using System;

namespace ND500.CPU.Instructions
{
    /// <summary>
    /// FREEB - Free buddy element back to heap
    /// Format: FREEB <log size/r/BY>,<element/s/W>
    /// Opcode: 0xFDB6 (176666 octal)
    /// </summary>
    public class FreebInstruction : IInstruction
    {
        private readonly ND500CPU _cpu;
        private readonly IMemory _memory;

        public FreebInstruction(ND500CPU cpu, IMemory memory)
        {
            _cpu = cpu;
            _memory = memory;
        }

        public void Execute(InstructionContext context)
        {
            // Get operands
            byte logSize = context.GetByteOperand();
            uint elementAddr = context.GetWordOperand();

            // Free the block
            FreeBlock(elementAddr, logSize);
        }

        private void FreeBlock(uint blockAddr, byte logSize)
        {
            uint tosAddr = _cpu.TOS;
            ushort maxl = _memory.ReadWord(tosAddr + 0);

            if (logSize > maxl)
            {
                throw new InvalidOperationException(
                    $"Invalid log size {logSize} in FREEB");
            }

            // Get free list head
            uint flogAddr = tosAddr + 3 + logSize;
            uint currentHead = _memory.ReadWord(flogAddr);

            // Link block to head of list
            _memory.WriteWord(blockAddr, (ushort)currentHead);
            _memory.WriteWord(flogAddr, (ushort)blockAddr);
        }
    }
}
```

---

## 5. Heap Initialization Example

Before using ENTB, the heap must be initialized by user code:

```csharp
public void InitializeHeap(uint tosAddr, uint heapStart, uint heapEnd, byte maxLogSize)
{
    // Set MAXL
    _memory.WriteWord(tosAddr + 0, maxLogSize);

    // Set STAH and ENDH (for user trap handlers)
    _memory.WriteWord(tosAddr + 1, (ushort)heapStart);
    _memory.WriteWord(tosAddr + 2, (ushort)heapEnd);

    // Initialize all free lists to empty
    for (byte i = 0; i <= maxLogSize; i++)
    {
        _memory.WriteWord(tosAddr + 3 + i, 0);
    }

    // Add entire heap as one large block (if possible)
    uint heapSize = heapEnd - heapStart;
    byte actualLog = (byte)Math.Floor(Math.Log2(heapSize));

    if (actualLog <= maxLogSize)
    {
        // Add heap as single block to largest possible free list
        uint flogAddr = tosAddr + 3 + actualLog;
        _memory.WriteWord(heapStart, 0); // No next block
        _memory.WriteWord(flogAddr, (ushort)heapStart);
    }
}
```

---

## 6. Test Example

### Test Case: Allocate 64-word block

```csharp
[Test]
public void TestENTB_Allocate64WordBlock()
{
    // Setup heap: TOS at 0x1000, heap from 0x2000-0x3000
    uint tosAddr = 0x1000;
    uint heapStart = 0x2000;
    uint heapEnd = 0x3000;
    byte maxLogSize = 10; // Up to 2^10 = 1024 words

    InitializeHeap(tosAddr, heapStart, heapEnd, maxLogSize);
    _cpu.TOS = tosAddr;

    // Execute ENTB with log size 6 (2^6 = 64 words)
    var instruction = new EntbInstruction(_cpu, _memory);
    var context = new InstructionContext { LogSize = 6, TargetAddress = 0x5000 };

    instruction.Execute(context);

    // Verify B register points to allocated block
    Assert.AreEqual(heapStart, _cpu.B);

    // Verify B.LOG contains 6
    byte storedLog = (byte)_memory.ReadWord(_cpu.B + 11);
    Assert.AreEqual(6, storedLog);

    // Verify P was updated
    Assert.AreEqual(0x5000, _cpu.P);
}

[Test]
public void TestRETB_FreesBlock()
{
    // ... setup from previous test ...

    // Execute RETB
    var retInstr = new RetbInstruction(_cpu, _memory, false);
    retInstr.Execute(new InstructionContext());

    // Verify block was returned to free list
    uint flogAddr = tosAddr + 3 + 6; // FLOG6
    uint freedBlock = _memory.ReadWord(flogAddr);
    Assert.AreEqual(heapStart, freedBlock);
}
```

---

## 7. Key Implementation Notes

### ⚠️ Critical Points

1. **B.LOG stores LOG SIZE, not block size**
   - Store the logSize parameter (e.g., 6)
   - NOT the calculated block size (e.g., 64)

2. **Block splitting creates buddies**
   - If you find a 2^8 block but need 2^6:
   - Split 2^8 → two 2^7 blocks
   - Keep one 2^7, split again → two 2^6 blocks
   - Keep one 2^6 for allocation
   - Add unused 2^7 and 2^6 back to free lists

3. **Free lists are simple linked lists**
   - First word of free block = next pointer
   - No size stored in block (it's implicit from which list it's in)

4. **STAH and ENDH are not used by CPU**
   - Optional for user trap handlers
   - Can be left uninitialized

5. **Blocks are NOT automatically merged on free**
   - FREEB just adds to list
   - Buddy merging done by STO trap handler (user code)

6. **Stack frame initialization identical to ENTS**
   - Copy PREVB, RETA, SP, N, ARG fields
   - Only difference: add LOG field

---

## 8. Troubleshooting

### Problem: STO trap on ENTB with empty heap

**Cause:** Free lists not initialized

**Fix:** Call InitializeHeap() before first ENTB

### Problem: Block corruption after RETB

**Cause:** Wrong log size in B.LOG

**Fix:** Ensure ENTB stores logSize (not blockSize) in B.LOG

### Problem: Memory leak - blocks not freed

**Cause:** Using RET instead of RETB

**Fix:** ENTB routines MUST use RETB/RETBK, not RET/RETK

### Problem: Heap fragmentation

**Cause:** No buddy merging

**Fix:** Implement STO trap handler that merges adjacent buddies

---

## Summary

**ENTB Implementation Checklist:**

✅ Read heap variables from TOS
✅ Validate log size <= MAXL
✅ Search free lists from requested size upward
✅ Unlink block from free list
✅ Split larger blocks (buddy system)
✅ Initialize stack frame (PREVB, RETA, SP, N, LOG, ARG)
✅ Set B register to allocated block
✅ RETB: Free block back to appropriate free list
✅ GETB: Same allocation, store address in register
✅ FREEB: Add block to free list head

**You now have everything needed to implement ENTB!** 🎉
