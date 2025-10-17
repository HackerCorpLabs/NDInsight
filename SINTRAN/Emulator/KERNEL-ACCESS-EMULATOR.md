# SINTRAN III Kernel Access from C# Emulator

**Complete C# Implementation for Reading SINTRAN Kernel Structures**

**Version:** 1.0  
**Last Updated:** October 16, 2025  
**Purpose:** Provide complete C# code for accessing SINTRAN III kernel data structures from an ND-100 emulator

---

## Table of Contents

1. [Overview](#1-overview)
2. [Data Type Sizes and Conventions](#2-data-type-sizes-and-conventions)
3. [Memory Access Helpers](#3-memory-access-helpers)
4. [Physical Memory Map](#4-physical-memory-map)
5. [Core Data Structures](#5-core-data-structures)
6. [Queue Access](#6-queue-access)
7. [Program State Access](#7-program-state-access)
8. [MMU and Page Table Access](#8-mmu-and-page-table-access)
9. [Complete Usage Examples](#9-complete-usage-examples)
10. [Advanced Topics](#10-advanced-topics)

---

## 1. Overview

### 1.1 ND-100 Architecture Summary

The ND-100 is a **16-bit word-addressed machine**:

- **Word size**: 16 bits
- **Address space**: 64K words (128KB)
- **Physical addressing**: Can address up to 16MB with MMU (24-bit physical addresses)
- **Memory access**: Word-addressed (not byte-addressed)

### 1.2 C# Data Type Mapping

| ND-100 Type | Size (bits) | C# Type | Notes |
|-------------|-------------|---------|-------|
| **WORD** | 16 | `ushort` | Basic unit, 0-65535 |
| **INTEGER** | 16 | `short` | Signed, -32768 to 32767 |
| **DOUBLE** | 32 | `uint` | Two consecutive words |
| **POINTER** | 16 | `ushort` | Word address |
| **BIT FIELD** | 1-16 | `ushort` + mask | Extract with bitwise ops |

### 1.3 Important Notes

1. **All addresses are WORD addresses**, not byte addresses
2. **Physical addresses** can be 24-bit (0-16M words) with MMU
3. **Octal notation** is heavily used in SINTRAN (prefix with `0` for octal in comments)
4. **Endianness**: Big-endian (MSB first)
5. **Signed values**: Two's complement

---

## 2. Data Type Sizes and Conventions

### 2.1 Basic Word Layout

```
16-bit Word (ushort):
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│15 │14 │13 │12 │11 │10 │ 9 │ 8 │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
 MSB                                                           LSB

Bit 15: Sign bit (for signed integers)
Bit 0-15: Available for data/flags
```

### 2.2 Double Word Layout

```
32-bit Double Word (uint):
High Word (bits 31-16):          Low Word (bits 15-0):
┌──────────────────┐             ┌──────────────────┐
│  Word N (MSW)    │             │  Word N+1 (LSW)  │
└──────────────────┘             └──────────────────┘
Address: N                       Address: N+1

Note: High word at lower address
```

### 2.3 Packed Fields Example (RT-Description STATE/PRIORITY)

```
Word at offset 1 in RT-Description:
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│15 │14 │13 │12 │11 │10 │ 9 │ 8 │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │
├───┴───┴───┴───┴───┴───┴───┴───┼───────────────────────────────┤
│     State Flags (bits 8-15)   │   Priority (bits 0-7)         │
└───────────────────────────────┴───────────────────────────────┘

Bit 15-14: Reserved
Bit 13 (5RTOFF): RT program inhibited
Bit 12 (5RWAIT): Voluntarily waiting
Bit 11 (5ABS): Absolute time scheduling
Bit 10 (5INT): Periodic program
Bit 9 (5REP): Repeat execution requested
Bit 8 (5WAIT): Program is waiting (= octal 000017 = bit 15 in octal notation)
Bit 7-0: Priority value (0-255 decimal, 0-377 octal)
```

### 2.4 Page Index Table Entry (16 bits)

```
PIT Entry Format:
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│15 │14 │13 │12 │11 │10 │ 9 │ 8 │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │
├───┼───┼───┼───┼───┼───┴───┼───┼───┴───┴───┴───┴───┴───┴───┴───┤
│WPM│RPM│FPM│WIP│PU │ RING  │ 0 │   Physical Page (0-255)       │
└───┴───┴───┴───┴───┴───────┴───┴───────────────────────────────┘

Bit 15 (WPM): Write Permitted
Bit 14 (RPM): Read Permitted
Bit 13 (FPM): Fetch Permitted (execute)
Bit 12 (WIP): Written In Page (dirty bit)
Bit 11 (PU): Page Used (accessed bit)
Bit 10-9 (RING): Ring number (0-3)
Bit 8: Not used
Bit 7-0: Physical page number (0-255)
```

---

## 3. Memory Access Helpers

### 3.1 IMemoryAccess Interface

```csharp
/// <summary>
/// Interface for accessing ND-100 memory.
/// All addresses are WORD addresses (not byte addresses).
/// </summary>
public interface IMemoryAccess
{
    /// <summary>
    /// Read a 16-bit word from physical memory.
    /// </summary>
    /// <param name="address">Physical word address (0-16M)</param>
    /// <returns>16-bit value</returns>
    ushort ReadWord(uint address);
    
    /// <summary>
    /// Read a 32-bit double word from physical memory.
    /// High word is at 'address', low word at 'address+1'.
    /// </summary>
    /// <param name="address">Physical word address of high word</param>
    /// <returns>32-bit value (high word in upper 16 bits)</returns>
    uint ReadDoubleWord(uint address);
    
    /// <summary>
    /// Write a 16-bit word to physical memory.
    /// </summary>
    /// <param name="address">Physical word address</param>
    /// <param name="value">16-bit value to write</param>
    void WriteWord(uint address, ushort value);
    
    /// <summary>
    /// Write a 32-bit double word to physical memory.
    /// </summary>
    /// <param name="address">Physical word address for high word</param>
    /// <param name="value">32-bit value</param>
    void WriteDoubleWord(uint address, uint value);
    
    /// <summary>
    /// Translate virtual address to physical address using current page tables.
    /// </summary>
    /// <param name="virtualAddress">16-bit virtual address (0-65535)</param>
    /// <param name="pitNumber">Page Index Table number (0-3)</param>
    /// <returns>24-bit physical address, or null if page fault</returns>
    uint? TranslateAddress(ushort virtualAddress, byte pitNumber);
}
```

### 3.2 Bit Field Helper Methods

```csharp
/// <summary>
/// Helper methods for extracting bit fields from 16-bit words.
/// </summary>
public static class BitFieldHelpers
{
    /// <summary>
    /// Extract a bit field from a 16-bit word.
    /// </summary>
    /// <param name="word">Source word</param>
    /// <param name="bitPosition">LSB position (0-15, where 0 is rightmost)</param>
    /// <param name="bitCount">Number of bits to extract (1-16)</param>
    /// <returns>Extracted value</returns>
    public static ushort ExtractBits(ushort word, int bitPosition, int bitCount)
    {
        // Create mask: (1 << bitCount) - 1
        ushort mask = (ushort)((1 << bitCount) - 1);
        // Shift right and mask
        return (ushort)((word >> bitPosition) & mask);
    }
    
    /// <summary>
    /// Test if a specific bit is set.
    /// </summary>
    /// <param name="word">Source word</param>
    /// <param name="bitPosition">Bit position (0-15)</param>
    /// <returns>True if bit is set</returns>
    public static bool TestBit(ushort word, int bitPosition)
    {
        return ((word >> bitPosition) & 1) != 0;
    }
    
    /// <summary>
    /// Set a bit in a word.
    /// </summary>
    public static ushort SetBit(ushort word, int bitPosition)
    {
        return (ushort)(word | (1 << bitPosition));
    }
    
    /// <summary>
    /// Clear a bit in a word.
    /// </summary>
    public static ushort ClearBit(ushort word, int bitPosition)
    {
        return (ushort)(word & ~(1 << bitPosition));
    }
    
    /// <summary>
    /// Extract byte from word (ND-100 style: high byte or low byte).
    /// </summary>
    /// <param name="word">Source word</param>
    /// <param name="highByte">True for bits 15-8, false for bits 7-0</param>
    /// <returns>Byte value (0-255)</returns>
    public static byte ExtractByte(ushort word, bool highByte)
    {
        return highByte ? (byte)(word >> 8) : (byte)(word & 0xFF);
    }
    
    /// <summary>
    /// Combine two bytes into a word (high byte in bits 15-8, low in bits 7-0).
    /// </summary>
    public static ushort CombineBytes(byte highByte, byte lowByte)
    {
        return (ushort)((highByte << 8) | lowByte);
    }
}
```

### 3.3 Octal Conversion Helpers

```csharp
/// <summary>
/// Helpers for working with octal notation (heavily used in SINTRAN).
/// </summary>
public static class OctalHelpers
{
    /// <summary>
    /// Convert octal string to ushort.
    /// </summary>
    /// <param name="octalString">Octal string (e.g., "004136")</param>
    /// <returns>Decimal value</returns>
    public static ushort ParseOctal(string octalString)
    {
        return Convert.ToUInt16(octalString, 8);
    }
    
    /// <summary>
    /// Convert ushort to octal string.
    /// </summary>
    /// <param name="value">Value to convert</param>
    /// <param name="minDigits">Minimum digits (pad with zeros)</param>
    /// <returns>Octal string</returns>
    public static string ToOctal(ushort value, int minDigits = 6)
    {
        return Convert.ToString(value, 8).PadLeft(minDigits, '0');
    }
    
    /// <summary>
    /// Format address in octal for display.
    /// </summary>
    public static string FormatAddress(uint address)
    {
        if (address <= 0xFFFF)
            return Convert.ToString(address, 8).PadLeft(6, '0') + "₈";
        else
            return Convert.ToString(address, 8).PadLeft(8, '0') + "₈";
    }
}
```

---

## 4. Physical Memory Map

### 4.1 Known Physical Addresses (from Symbol Files)

```csharp
/// <summary>
/// Physical addresses of key SINTRAN kernel structures.
/// All addresses are in WORDS (not bytes).
/// Values from SYMBOL-1-LIST.SYMB.TXT and SYMBOL-2-LIST.SYMB.TXT.
/// </summary>
public static class SintranAddresses
{
    // ===== Queue Heads =====
    
    /// <summary>
    /// Execution queue head (BEXQU).
    /// Physical address: 004013₈ (2059 decimal).
    /// Size: 1 word (16 bits).
    /// Contains: Address of first RT-description in execution queue, or 0 if empty.
    /// </summary>
    public const ushort BEXQU = 0x0813;  // 004013₈ octal = 2059 decimal
    
    /// <summary>
    /// Time queue head (assumed, typically near other queues).
    /// Physical address: ~004xxx₈ (estimate based on layout).
    /// Size: 1 word (16 bits).
    /// Contains: Address of first RT-description in time queue, or 0 if empty.
    /// </summary>
    public const ushort BTIMQU = 0x0820;  // Estimated ~004040₈
    
    /// <summary>
    /// Monitor queue head (assumed, typically near other queues).
    /// Physical address: ~004xxx₈ (estimate based on layout).
    /// Size: 1 word (16 bits).
    /// Contains: Address of last datafield in monitor queue, or 0 if empty.
    /// </summary>
    public const ushort MQUEUE = 0x0800;  // Estimated ~004000₈
    
    // ===== Time Variables =====
    
    /// <summary>
    /// Actual time (ATIME).
    /// Physical address: 004136₈ (2142 decimal).
    /// Size: 2 words (32 bits) - DOUBLE.
    /// Contains: Current system time in basic time units.
    /// </summary>
    public const ushort ATIME = 0x085E;  // 004136₈ octal = 2142 decimal
    
    /// <summary>
    /// Monitor time (MTIME).
    /// Physical address: 004140₈ (2144 decimal).
    /// Size: 2 words (32 bits) - DOUBLE.
    /// Contains: Monitor-adjusted time in basic time units.
    /// </summary>
    public const ushort MTIME = 0x0860;  // 004140₈ octal = 2144 decimal
    
    // ===== Interrupt Levels =====
    
    /// <summary>
    /// A-level (user mode level) number.
    /// Value: 000001₈ (1 decimal).
    /// This is the interrupt level number for user mode programs.
    /// </summary>
    public const byte ALEVL = 0x01;  // 000001₈
    
    /// <summary>
    /// B-level (monitor call level) number.
    /// Value: 000004₈ (4 decimal).
    /// This is the interrupt level for monitor calls (MON).
    /// </summary>
    public const byte BLEVL = 0x04;  // 000004₈
    
    // ===== Memory Map =====
    
    /// <summary>
    /// Total memory map (TMMAP) - bitmap of installed physical memory.
    /// Physical address: 171075₈ (61997 decimal).
    /// Size: 18 words (20₈ octal).
    /// Contains: Bitmap where each bit = one 32K memory bank.
    /// </summary>
    public const ushort TMMAP = 0xF23D;  // 171075₈ octal = 61997 decimal
    
    // ===== Memory Regions (from architecture documentation) =====
    
    /// <summary>
    /// Start of kernel data area.
    /// Physical address: 0 (0 decimal).
    /// Contains: Kernel variables, queue heads, working storage.
    /// </summary>
    public const ushort KERNEL_DATA_START = 0x0000;  // 0₈
    
    /// <summary>
    /// End of kernel data area (approximate).
    /// Physical address: ~002000₈ (1024 decimal).
    /// </summary>
    public const ushort KERNEL_DATA_END = 0x0400;  // 002000₈
    
    /// <summary>
    /// Start of datafield area (device control blocks).
    /// Physical address: 020000₈ (8192 decimal).
    /// Size: Variable (typically 2000₈ words = 1024 decimal).
    /// </summary>
    public const ushort DATAFIELD_START = 0x2000;  // 020000₈
    
    /// <summary>
    /// Start of RT-description table (program control blocks).
    /// Physical address: 026000₈ (11264 decimal).
    /// Size: Variable (26 words per RT program, max programs configurable).
    /// </summary>
    public const ushort RTDESC_START = 0x2C00;  // 026000₈
    
    /// <summary>
    /// Start of POF (Paging Off) area.
    /// Physical address: 100000₈ (32768 decimal).
    /// Size: To 177377₈ (65279 decimal).
    /// Contains: Code and data accessible with paging off.
    /// </summary>
    public const ushort POF_START = 0x8000;  // 100000₈
    
    /// <summary>
    /// End of POF area.
    /// Physical address: 177377₈ (65279 decimal).
    /// </summary>
    public const ushort POF_END = 0xFEFF;  // 177377₈
    
    // ===== RT-Description Offsets (within RT-Description structure) =====
    
    /// <summary>
    /// Size of RT-Description structure.
    /// Size: 26 words (32₈ octal).
    /// </summary>
    public const ushort RTDESC_SIZE = 26;  // 032₈ octal
    
    // Offsets within RT-Description (see RtDescription class for details):
    public const byte RTDESC_TLNK = 0;       // Time queue link
    public const byte RTDESC_STATE = 1;      // State/Priority word
    public const byte RTDESC_DTIM1 = 2;      // Scheduled time (high)
    public const byte RTDESC_DTIM2 = 3;      // Scheduled time (low)
    public const byte RTDESC_DTINT1 = 4;     // Time interval (high)
    public const byte RTDESC_DTINT2 = 5;     // Time interval (low)
    public const byte RTDESC_STADR = 6;      // Start address
    public const byte RTDESC_SEGM = 7;       // Segment numbers
    public const byte RTDESC_DPREG = 8;      // Saved P register (offset 010₈)
    public const byte RTDESC_DXREG = 9;      // Saved X register (offset 011₈)
    public const byte RTDESC_DTREG = 10;     // Saved T register (offset 012₈)
    public const byte RTDESC_DAREG = 11;     // Saved A register (offset 013₈)
    public const byte RTDESC_DDREG = 12;     // Saved D register (offset 014₈)
    public const byte RTDESC_DLREG = 13;     // Saved L register (offset 015₈)
    public const byte RTDESC_DSREG = 14;     // Saved status register (offset 016₈)
    public const byte RTDESC_DBREG = 15;     // Saved B register (offset 017₈)
    public const byte RTDESC_WLNK = 16;      // Waiting/execution queue link (offset 020₈)
    public const byte RTDESC_ACTSEG1 = 17;   // Active segment 1 (offset 021₈)
    public const byte RTDESC_ACTSEG2 = 18;   // Active segment 2 (offset 022₈)
    public const byte RTDESC_ACTPRI = 19;    // Actual priority (offset 023₈)
    public const byte RTDESC_BRESLINK = 20;  // Beginning of reservation queue (offset 024₈)
    public const byte RTDESC_RSEGM = 21;     // Reserved segment (offset 025₈)
    public const byte RTDESC_BITMAP = 22;    // Segment bitmap word 0 (offset 026₈)
    public const byte RTDESC_BITM1 = 23;     // Segment bitmap word 1 (offset 027₈)
    public const byte RTDESC_BITM2 = 24;     // Segment bitmap word 2 (offset 030₈)
    public const byte RTDESC_BITM3 = 25;     // Segment bitmap word 3 (offset 031₈)
    
    // ===== I/O Datafield Offsets (within datafield structure) =====
    
    // Offsets within I/O Datafield (see IoDatafield class for details):
    public const byte DATAFIELD_RESLINK = 0;    // Reservation queue link
    public const byte DATAFIELD_RTRES = 1;      // RT program that reserved this
    public const byte DATAFIELD_BWLINK = 2;     // Beginning of waiting queue
    public const byte DATAFIELD_SEMAPHORE = 3;  // Semaphore control word
    public const byte DATAFIELD_STATUS = 4;     // Device status
    public const byte DATAFIELD_MLINK = 5;      // Monitor queue link
    public const byte DATAFIELD_MFUNC = 6;      // Monitor function address
    public const byte DATAFIELD_DEVICE_TYPE = 7; // Device type code
    public const byte DATAFIELD_HDEV = 8;       // Hardware device number (offset 010₈)
    public const byte DATAFIELD_IDENT = 9;      // Ident code (offset 011₈)
    
    // ===== State Flag Bit Positions (in RT-Description STATE word) =====
    
    /// <summary>
    /// Bit position for 5WAIT flag (program is waiting).
    /// Value from symbols: 000017₈ (15 decimal) - this is bit 15 in octal bit numbering.
    /// In binary bit numbering (0=LSB, 15=MSB), this is bit 8.
    /// </summary>
    public const int BIT_5WAIT = 8;     // Bit 8 in binary notation
    
    /// <summary>
    /// Bit position for 5REP flag (repeat execution).
    /// Estimated: Bit 9.
    /// </summary>
    public const int BIT_5REP = 9;
    
    /// <summary>
    /// Bit position for 5INT flag (periodic program).
    /// Estimated: Bit 10.
    /// </summary>
    public const int BIT_5INT = 10;
    
    /// <summary>
    /// Bit position for 5ABS flag (absolute time).
    /// Estimated: Bit 11.
    /// </summary>
    public const int BIT_5ABS = 11;
    
    /// <summary>
    /// Bit position for 5RWAIT flag (voluntarily waiting).
    /// Estimated: Bit 12.
    /// </summary>
    public const int BIT_5RWAIT = 12;
    
    /// <summary>
    /// Bit position for 5RTOFF flag (RT program inhibited).
    /// Estimated: Bit 13.
    /// </summary>
    public const int BIT_5RTOFF = 13;
    
    // ===== Page Table Constants =====
    
    /// <summary>
    /// Number of Page Index Tables (PITs).
    /// </summary>
    public const int NUM_PITS = 4;
    
    /// <summary>
    /// Number of entries per PIT.
    /// </summary>
    public const int PIT_ENTRIES = 64;
    
    /// <summary>
    /// Page size in words.
    /// </summary>
    public const int PAGE_SIZE = 1024;  // 2000₈ octal
    
    /// <summary>
    /// Page offset mask (bits 0-9 of virtual address).
    /// </summary>
    public const ushort PAGE_OFFSET_MASK = 0x03FF;  // 10 bits
    
    /// <summary>
    /// Page number shift (bits 10-15 of virtual address).
    /// </summary>
    public const int PAGE_NUMBER_SHIFT = 10;
}
```

---

## 5. Core Data Structures

### 5.1 RT-Description (Program Control Block)

```csharp
/// <summary>
/// RT-Description: Control block for an RT (Real-Time) program.
/// Size: 26 words (52 bytes).
/// All offsets are in WORDS from the base address.
/// </summary>
public class RtDescription
{
    // ===== Location Information =====
    
    /// <summary>
    /// Physical address of this RT-Description in memory.
    /// </summary>
    public ushort Address { get; set; }
    
    /// <summary>
    /// RT program number (index in RT-description table).
    /// </summary>
    public int ProgramNumber { get; set; }
    
    // ===== Queue Links (16 bits each) =====
    
    /// <summary>
    /// Time queue link (TLNK).
    /// Offset: 0 words.
    /// Size: 16 bits.
    /// Value: 0 = not in time queue
    ///        >0 = address of next RT-description in time queue
    ///        0xFFFF (-1) = last in time queue
    /// </summary>
    public ushort TLNK { get; set; }
    
    /// <summary>
    /// Waiting/Execution queue link (WLINK).
    /// Offset: 16 words (020₈ octal).
    /// Size: 16 bits.
    /// Value: 0 = not in any queue
    ///        >0 = address of next RT-description in queue
    ///        Points to queue head if last in execution queue
    /// </summary>
    public ushort WLINK { get; set; }
    
    // ===== State and Priority (16 bits total, packed) =====
    
    /// <summary>
    /// Raw STATE/PRIORITY word.
    /// Offset: 1 word.
    /// Size: 16 bits.
    /// Bits 0-7: Priority (0-255)
    /// Bits 8-15: State flags
    /// </summary>
    public ushort StatePriorityWord { get; set; }
    
    /// <summary>
    /// Priority value (bits 0-7 of STATE word).
    /// Range: 0-255 decimal (0-377 octal).
    /// Higher value = higher priority.
    /// </summary>
    public byte Priority
    {
        get => (byte)(StatePriorityWord & 0xFF);
        set => StatePriorityWord = (ushort)((StatePriorityWord & 0xFF00) | value);
    }
    
    /// <summary>
    /// Program is waiting (5WAIT flag, bit 8).
    /// </summary>
    public bool IsWaiting
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5WAIT);
        set => StatePriorityWord = value 
            ? BitFieldHelpers.SetBit(StatePriorityWord, SintranAddresses.BIT_5WAIT)
            : BitFieldHelpers.ClearBit(StatePriorityWord, SintranAddresses.BIT_5WAIT);
    }
    
    /// <summary>
    /// Repeat execution requested (5REP flag, bit 9).
    /// </summary>
    public bool RepeatRequested
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5REP);
    }
    
    /// <summary>
    /// Periodic program (5INT flag, bit 10).
    /// </summary>
    public bool IsPeriodic
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5INT);
    }
    
    /// <summary>
    /// Absolute time scheduling (5ABS flag, bit 11).
    /// </summary>
    public bool IsAbsoluteTime
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5ABS);
    }
    
    /// <summary>
    /// Voluntarily waiting (5RWAIT flag, bit 12).
    /// </summary>
    public bool VoluntarilyWaiting
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5RWAIT);
    }
    
    /// <summary>
    /// RT program inhibited (5RTOFF flag, bit 13).
    /// </summary>
    public bool IsInhibited
    {
        get => BitFieldHelpers.TestBit(StatePriorityWord, SintranAddresses.BIT_5RTOFF);
    }
    
    // ===== Timing Information (32 bits each, DOUBLE words) =====
    
    /// <summary>
    /// Scheduled time (DTIM1/DTIM2).
    /// Offset: 2-3 words.
    /// Size: 32 bits (2 words).
    /// Value: Scheduled execution time in basic time units.
    /// </summary>
    public uint ScheduledTime { get; set; }
    
    /// <summary>
    /// Time interval (DTINT1/DTINT2).
    /// Offset: 4-5 words.
    /// Size: 32 bits (2 words).
    /// Value: For periodic programs, the period in basic time units.
    /// </summary>
    public uint TimeInterval { get; set; }
    
    // ===== Program Information (16 bits each) =====
    
    /// <summary>
    /// Start address (STADR).
    /// Offset: 6 words.
    /// Size: 16 bits.
    /// Value: Entry point address when program starts.
    /// </summary>
    public ushort StartAddress { get; set; }
    
    /// <summary>
    /// Segment numbers (SEGM/SEGM2), packed.
    /// Offset: 7 words.
    /// Size: 16 bits.
    /// Bits 0-7: Primary segment number
    /// Bits 8-15: Secondary segment number
    /// </summary>
    public ushort SegmentNumbers { get; set; }
    
    /// <summary>
    /// Primary segment number (bits 0-7).
    /// </summary>
    public byte PrimarySegment
    {
        get => (byte)(SegmentNumbers & 0xFF);
    }
    
    /// <summary>
    /// Secondary segment number (bits 8-15).
    /// </summary>
    public byte SecondarySegment
    {
        get => (byte)(SegmentNumbers >> 8);
    }
    
    // ===== Saved CPU Registers (16 bits each) =====
    
    /// <summary>
    /// Saved P register (program counter).
    /// Offset: 8 words (010₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedP { get; set; }
    
    /// <summary>
    /// Saved X register (index register).
    /// Offset: 9 words (011₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedX { get; set; }
    
    /// <summary>
    /// Saved T register.
    /// Offset: 10 words (012₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedT { get; set; }
    
    /// <summary>
    /// Saved A register (accumulator).
    /// Offset: 11 words (013₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedA { get; set; }
    
    /// <summary>
    /// Saved D register.
    /// Offset: 12 words (014₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedD { get; set; }
    
    /// <summary>
    /// Saved L register (return address).
    /// Offset: 13 words (015₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedL { get; set; }
    
    /// <summary>
    /// Saved status register (STS).
    /// Offset: 14 words (016₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedSTS { get; set; }
    
    /// <summary>
    /// Saved B register (base register for local variables).
    /// Offset: 15 words (017₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SavedB { get; set; }
    
    // ===== Additional Control Fields (16 bits each) =====
    
    /// <summary>
    /// Active segment 1.
    /// Offset: 17 words (021₈ octal).
    /// Size: 16 bits.
    /// Value: Currently loaded segment number.
    /// </summary>
    public ushort ActiveSegment1 { get; set; }
    
    /// <summary>
    /// Active segment 2.
    /// Offset: 18 words (022₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort ActiveSegment2 { get; set; }
    
    /// <summary>
    /// Actual priority (ACTPRI).
    /// Offset: 19 words (023₈ octal).
    /// Size: 16 bits.
    /// Value: Modified priority during execution.
    /// </summary>
    public ushort ActualPriority { get; set; }
    
    /// <summary>
    /// Beginning of reservation queue link (BRESLINK).
    /// Offset: 20 words (024₈ octal).
    /// Size: 16 bits.
    /// Value: 0 = no resources reserved
    ///        >0 = address of first reserved datafield
    /// </summary>
    public ushort BRESLINK { get; set; }
    
    /// <summary>
    /// Reserved segment info (RSEGM).
    /// Offset: 21 words (025₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort ReservedSegment { get; set; }
    
    // ===== Segment Bitmaps (16 bits each) =====
    
    /// <summary>
    /// Segment bitmap word 0 (BITMAP).
    /// Offset: 22 words (026₈ octal).
    /// Size: 16 bits.
    /// Each bit indicates a non-reentrant page that needs clearing.
    /// </summary>
    public ushort SegmentBitmap0 { get; set; }
    
    /// <summary>
    /// Segment bitmap word 1 (BITM1).
    /// Offset: 23 words (027₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SegmentBitmap1 { get; set; }
    
    /// <summary>
    /// Segment bitmap word 2 (BITM2).
    /// Offset: 24 words (030₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SegmentBitmap2 { get; set; }
    
    /// <summary>
    /// Segment bitmap word 3 (BITM3) or Window info.
    /// Offset: 25 words (031₈ octal).
    /// Size: 16 bits.
    /// </summary>
    public ushort SegmentBitmap3 { get; set; }
    
    // ===== Computed Properties =====
    
    /// <summary>
    /// Get string representation of program state.
    /// </summary>
    public string StateString
    {
        get
        {
            var states = new List<string>();
            if (IsWaiting) states.Add("WAITING");
            if (RepeatRequested) states.Add("REPEAT");
            if (IsPeriodic) states.Add("PERIODIC");
            if (IsAbsoluteTime) states.Add("ABSOLUTE_TIME");
            if (VoluntarilyWaiting) states.Add("VOLUNTARY_WAIT");
            if (IsInhibited) states.Add("INHIBITED");
            
            return states.Count > 0 ? string.Join("|", states) : "READY";
        }
    }
    
    /// <summary>
    /// Format address for display.
    /// </summary>
    public string AddressString => OctalHelpers.FormatAddress(Address);
    
    /// <summary>
    /// Get priority as octal string.
    /// </summary>
    public string PriorityOctal => Convert.ToString(Priority, 8).PadLeft(3, '0') + "₈";
}
```

### 5.2 I/O Datafield (Device Control Block)

```csharp
/// <summary>
/// I/O Datafield: Control block for an I/O device, resource, or semaphore.
/// Size: Variable (minimum ~10 words for simple devices, ~200₈ words for mass storage).
/// All offsets are in WORDS from the base address.
/// </summary>
public class IoDatafield
{
    // ===== Location Information =====
    
    /// <summary>
    /// Physical address of this datafield in memory.
    /// </summary>
    public ushort Address { get; set; }
    
    /// <summary>
    /// Size of this datafield in words (device-dependent).
    /// </summary>
    public ushort Size { get; set; }
    
    // ===== Queue Links and Ownership (16 bits each) =====
    
    /// <summary>
    /// Reservation queue link (RESLINK).
    /// Offset: 0 words.
    /// Size: 16 bits.
    /// Value: 0 = resource is free (not reserved)
    ///        >0 = address of next datafield in program's reservation queue
    /// </summary>
    public ushort RESLINK { get; set; }
    
    /// <summary>
    /// RT program that reserved this resource (RTRES).
    /// Offset: 1 word.
    /// Size: 16 bits.
    /// Value: 0 = resource is free
    ///        >0 = address of RT-description that owns this resource
    /// </summary>
    public ushort RTRES { get; set; }
    
    /// <summary>
    /// Beginning of waiting queue link (BWLINK).
    /// Offset: 2 words.
    /// Size: 16 bits.
    /// Value: 0 = no programs waiting for this resource
    ///        >0 = address of first RT-description waiting for this resource
    /// </summary>
    public ushort BWLINK { get; set; }
    
    // ===== Control and Status (16 bits each) =====
    
    /// <summary>
    /// Semaphore control word.
    /// Offset: 3 words.
    /// Size: 16 bits.
    /// Bit 0: Reserved flag
    /// Bits 1-15: Count or state
    /// </summary>
    public ushort Semaphore { get; set; }
    
    /// <summary>
    /// Device status word.
    /// Offset: 4 words.
    /// Size: 16 bits.
    /// Device-dependent status flags.
    /// </summary>
    public ushort Status { get; set; }
    
    /// <summary>
    /// Monitor queue link (MLINK).
    /// Offset: 5 words.
    /// Size: 16 bits.
    /// Value: -1 (0xFFFF) = first element in monitor queue (ready to process)
    ///        0 = not in monitor queue
    ///        >0 = address of next datafield in monitor queue
    /// </summary>
    public ushort MLINK { get; set; }
    
    /// <summary>
    /// Monitor function address (MFUNC).
    /// Offset: 6 words.
    /// Size: 16 bits.
    /// Value: Address of routine to execute when processing this from monitor queue.
    /// </summary>
    public ushort MFUNC { get; set; }
    
    // ===== Device Information (16 bits each) =====
    
    /// <summary>
    /// Device type code.
    /// Offset: 7 words.
    /// Size: 16 bits.
    /// Identifies what kind of device this is.
    /// </summary>
    public ushort DeviceType { get; set; }
    
    /// <summary>
    /// Hardware device number (HDEV).
    /// Offset: 8 words (010₈ octal).
    /// Size: 16 bits.
    /// IOX address for this device.
    /// </summary>
    public ushort HDEV { get; set; }
    
    /// <summary>
    /// Ident code.
    /// Offset: 9 words (011₈ octal).
    /// Size: 16 bits.
    /// Used for interrupt identification.
    /// </summary>
    public ushort Ident { get; set; }
    
    // ===== Computed Properties =====
    
    /// <summary>
    /// Is this resource currently reserved?
    /// </summary>
    public bool IsReserved => RTRES != 0;
    
    /// <summary>
    /// Are there programs waiting for this resource?
    /// </summary>
    public bool HasWaiters => BWLINK != 0;
    
    /// <summary>
    /// Is this datafield in the monitor queue?
    /// </summary>
    public bool InMonitorQueue => MLINK != 0;
    
    /// <summary>
    /// Is this the first element to process in monitor queue?
    /// </summary>
    public bool IsFirstInMonitorQueue => MLINK == 0xFFFF;
    
    /// <summary>
    /// Format address for display.
    /// </summary>
    public string AddressString => OctalHelpers.FormatAddress(Address);
}
```

### 5.3 Page Index Table Entry

```csharp
/// <summary>
/// Page Index Table (PIT) Entry.
/// Size: 16 bits (1 word).
/// Controls access to one 1KB page of memory.
/// </summary>
public struct PitEntry
{
    /// <summary>
    /// Raw 16-bit PIT entry value.
    /// </summary>
    public ushort RawValue { get; set; }
    
    // ===== Permission Bits =====
    
    /// <summary>
    /// Write Permitted (WPM) - bit 15.
    /// </summary>
    public bool WritePermitted
    {
        get => BitFieldHelpers.TestBit(RawValue, 15);
        set => RawValue = value 
            ? BitFieldHelpers.SetBit(RawValue, 15) 
            : BitFieldHelpers.ClearBit(RawValue, 15);
    }
    
    /// <summary>
    /// Read Permitted (RPM) - bit 14.
    /// </summary>
    public bool ReadPermitted
    {
        get => BitFieldHelpers.TestBit(RawValue, 14);
        set => RawValue = value 
            ? BitFieldHelpers.SetBit(RawValue, 14) 
            : BitFieldHelpers.ClearBit(RawValue, 14);
    }
    
    /// <summary>
    /// Fetch Permitted (FPM) - bit 13 (execute permission).
    /// </summary>
    public bool FetchPermitted
    {
        get => BitFieldHelpers.TestBit(RawValue, 13);
        set => RawValue = value 
            ? BitFieldHelpers.SetBit(RawValue, 13) 
            : BitFieldHelpers.ClearBit(RawValue, 13);
    }
    
    /// <summary>
    /// Written In Page (WIP) - bit 12 (dirty bit).
    /// </summary>
    public bool WrittenInPage
    {
        get => BitFieldHelpers.TestBit(RawValue, 12);
        set => RawValue = value 
            ? BitFieldHelpers.SetBit(RawValue, 12) 
            : BitFieldHelpers.ClearBit(RawValue, 12);
    }
    
    /// <summary>
    /// Page Used (PU) - bit 11 (accessed bit).
    /// </summary>
    public bool PageUsed
    {
        get => BitFieldHelpers.TestBit(RawValue, 11);
        set => RawValue = value 
            ? BitFieldHelpers.SetBit(RawValue, 11) 
            : BitFieldHelpers.ClearBit(RawValue, 11);
    }
    
    // ===== Ring Number (bits 10-9) =====
    
    /// <summary>
    /// Ring number (protection level).
    /// Bits 10-9.
    /// Value: 0 = Ring 0 (most privileged)
    ///        1 = Ring 1
    ///        2 = Ring 2 (SINTRAN kernel)
    ///        3 = Ring 3 (least privileged)
    /// </summary>
    public byte Ring
    {
        get => (byte)BitFieldHelpers.ExtractBits(RawValue, 9, 2);
        set
        {
            // Clear bits 10-9, then set new value
            RawValue = (ushort)((RawValue & ~(3 << 9)) | ((value & 3) << 9));
        }
    }
    
    // ===== Physical Page Number (bits 7-0) =====
    
    /// <summary>
    /// Physical page number.
    /// Bits 7-0.
    /// Range: 0-255 (1 byte).
    /// </summary>
    public byte PhysicalPage
    {
        get => (byte)(RawValue & 0xFF);
        set => RawValue = (ushort)((RawValue & 0xFF00) | value);
    }
    
    // ===== Computed Properties =====
    
    /// <summary>
    /// Is page present in memory?
    /// A page is NOT present if WPM=RPM=FPM=0 (triggers page fault).
    /// </summary>
    public bool IsPresent => WritePermitted || ReadPermitted || FetchPermitted;
    
    /// <summary>
    /// Physical address of start of this page (in words).
    /// </summary>
    public uint PhysicalAddress => (uint)(PhysicalPage * SintranAddresses.PAGE_SIZE);
    
    /// <summary>
    /// Format permissions as string (e.g., "RWX" or "R--").
    /// </summary>
    public string PermissionsString =>
        $"{(ReadPermitted ? 'R' : '-')}{(WritePermitted ? 'W' : '-')}{(FetchPermitted ? 'X' : '-')}";
    
    /// <summary>
    /// Create PIT entry from individual components.
    /// </summary>
    public static PitEntry Create(byte physPage, byte ring, bool read, bool write, bool execute)
    {
        var entry = new PitEntry();
        entry.PhysicalPage = physPage;
        entry.Ring = ring;
        entry.ReadPermitted = read;
        entry.WritePermitted = write;
        entry.FetchPermitted = execute;
        return entry;
    }
}
```

---

## 6. Queue Access

### 6.1 Queue Reader Class

```csharp
/// <summary>
/// Reads SINTRAN queue structures from memory.
/// </summary>
public class SintranQueueReader
{
    private readonly IMemoryAccess _memory;
    
    public SintranQueueReader(IMemoryAccess memory)
    {
        _memory = memory ?? throw new ArgumentNullException(nameof(memory));
    }
    
    // ===== Execution Queue =====
    
    /// <summary>
    /// Read all RT programs in the execution queue (ready to run).
    /// Queue is circular: last element links back to BEXQU.
    /// Programs are ordered by priority (highest first).
    /// </summary>
    /// <returns>List of RT-descriptions in priority order</returns>
    public List<RtDescription> ReadExecutionQueue()
    {
        var queue = new List<RtDescription>();
        
        // Read queue head + BWLINK offset (2 words from BEXQU)
        ushort head = SintranAddresses.BEXQU;
        ushort first = _memory.ReadWord((uint)(head + 2));  // BWLINK field
        
        if (first == 0)
            return queue;  // Empty queue
        
        ushort current = first;
        var visited = new HashSet<ushort>();  // Prevent infinite loops
        
        do
        {
            if (visited.Contains(current))
            {
                Console.WriteLine($"Warning: Circular reference detected in execution queue at {OctalHelpers.FormatAddress(current)}");
                break;
            }
            visited.Add(current);
            
            // Read RT-description
            var rtdesc = ReadRtDescription(current);
            if (rtdesc != null)
                queue.Add(rtdesc);
            
            // Follow WLINK
            current = rtdesc?.WLINK ?? 0;
            
        } while (current != head && current != 0 && visited.Count < 1000);
        
        return queue;
    }
    
    // ===== Time Queue =====
    
    /// <summary>
    /// Read all RT programs in the time queue (scheduled for future execution).
    /// Queue is linear: last element has TLINK = 0xFFFF.
    /// Programs are ordered by scheduled time (earliest first).
    /// </summary>
    /// <returns>List of RT-descriptions in time order</returns>
    public List<RtDescription> ReadTimeQueue()
    {
        var queue = new List<RtDescription>();
        
        // Assuming BTIMQU address (needs confirmation from symbols)
        ushort current = _memory.ReadWord(SintranAddresses.BTIMQU);
        
        if (current == 0 || current == 0xFFFF)
            return queue;  // Empty queue
        
        var visited = new HashSet<ushort>();
        
        while (current != 0 && current != 0xFFFF && visited.Count < 1000)
        {
            if (visited.Contains(current))
            {
                Console.WriteLine($"Warning: Circular reference in time queue at {OctalHelpers.FormatAddress(current)}");
                break;
            }
            visited.Add(current);
            
            // Read RT-description
            var rtdesc = ReadRtDescription(current);
            if (rtdesc != null)
            {
                queue.Add(rtdesc);
                current = rtdesc.TLNK;
            }
            else
            {
                break;
            }
        }
        
        return queue;
    }
    
    // ===== Monitor Queue =====
    
    /// <summary>
    /// Read all datafields in the monitor queue (pending monitor activations).
    /// Queue is FIFO: last added has MLINK pointing to earlier elements.
    /// First to process has MLINK = 0xFFFF.
    /// </summary>
    /// <returns>List of datafields in processing order (first to process first)</returns>
    public List<IoDatafield> ReadMonitorQueue()
    {
        var queue = new List<IoDatafield>();
        
        // Assuming MQUEUE address
        ushort current = _memory.ReadWord(SintranAddresses.MQUEUE);
        
        if (current == 0)
            return queue;  // Empty queue
        
        // First, collect all elements
        var allElements = new List<IoDatafield>();
        var visited = new HashSet<ushort>();
        
        while (current != 0 && visited.Count < 1000)
        {
            if (visited.Contains(current))
            {
                Console.WriteLine($"Warning: Circular reference in monitor queue at {OctalHelpers.FormatAddress(current)}");
                break;
            }
            visited.Add(current);
            
            var datafield = ReadDatafield(current);
            if (datafield != null)
            {
                allElements.Add(datafield);
                
                if (datafield.MLINK == 0xFFFF)
                {
                    // This is the first element to process
                    break;
                }
                current = datafield.MLINK;
            }
            else
            {
                break;
            }
        }
        
        // Reverse to get processing order (FIFO)
        allElements.Reverse();
        return allElements;
    }
    
    // ===== Waiting Queue (per resource) =====
    
    /// <summary>
    /// Read all RT programs waiting for a specific resource.
    /// </summary>
    /// <param name="datafieldAddress">Address of the datafield (resource)</param>
    /// <returns>List of RT-descriptions waiting for this resource</returns>
    public List<RtDescription> ReadWaitingQueue(ushort datafieldAddress)
    {
        var queue = new List<RtDescription>();
        
        // Read BWLINK from datafield (offset 2)
        ushort first = _memory.ReadWord((uint)(datafieldAddress + 2));
        
        if (first == 0)
            return queue;  // No waiters
        
        ushort current = first;
        var visited = new HashSet<ushort>();
        
        do
        {
            if (visited.Contains(current))
            {
                Console.WriteLine($"Warning: Circular reference in waiting queue at {OctalHelpers.FormatAddress(current)}");
                break;
            }
            visited.Add(current);
            
            var rtdesc = ReadRtDescription(current);
            if (rtdesc != null)
            {
                queue.Add(rtdesc);
                current = rtdesc.WLINK;
            }
            else
            {
                break;
            }
            
        } while (current != datafieldAddress && current != 0 && visited.Count < 1000);
        
        return queue;
    }
    
    // ===== Reservation Queue (per program) =====
    
    /// <summary>
    /// Read all resources reserved by a specific RT program.
    /// </summary>
    /// <param name="rtdescAddress">Address of the RT-description</param>
    /// <returns>List of reserved datafields</returns>
    public List<IoDatafield> ReadReservationQueue(ushort rtdescAddress)
    {
        var queue = new List<IoDatafield>();
        
        // Read BRESLINK from RT-description (offset 20)
        ushort first = _memory.ReadWord((uint)(rtdescAddress + SintranAddresses.RTDESC_BRESLINK));
        
        if (first == 0)
            return queue;  // No reservations
        
        ushort current = first;
        var visited = new HashSet<ushort>();
        
        do
        {
            if (visited.Contains(current))
            {
                Console.WriteLine($"Warning: Circular reference in reservation queue at {OctalHelpers.FormatAddress(current)}");
                break;
            }
            visited.Add(current);
            
            var datafield = ReadDatafield(current);
            if (datafield != null)
            {
                queue.Add(datafield);
                current = datafield.RESLINK;
            }
            else
            {
                break;
            }
            
        } while (current != rtdescAddress && current != 0 && visited.Count < 1000);
        
        return queue;
    }
    
    // ===== Helper Methods =====
    
    /// <summary>
    /// Read an RT-description from memory.
    /// </summary>
    /// <param name="address">Physical address of RT-description</param>
    /// <returns>Populated RT-description object, or null if invalid</returns>
    public RtDescription ReadRtDescription(ushort address)
    {
        if (address == 0 || address == 0xFFFF)
            return null;
        
        try
        {
            var rtdesc = new RtDescription { Address = address };
            
            // Read all 26 words
            rtdesc.TLNK = _memory.ReadWord(address + 0);
            rtdesc.StatePriorityWord = _memory.ReadWord(address + 1);
            rtdesc.ScheduledTime = _memory.ReadDoubleWord(address + 2);
            rtdesc.TimeInterval = _memory.ReadDoubleWord(address + 4);
            rtdesc.StartAddress = _memory.ReadWord(address + 6);
            rtdesc.SegmentNumbers = _memory.ReadWord(address + 7);
            rtdesc.SavedP = _memory.ReadWord(address + 8);
            rtdesc.SavedX = _memory.ReadWord(address + 9);
            rtdesc.SavedT = _memory.ReadWord(address + 10);
            rtdesc.SavedA = _memory.ReadWord(address + 11);
            rtdesc.SavedD = _memory.ReadWord(address + 12);
            rtdesc.SavedL = _memory.ReadWord(address + 13);
            rtdesc.SavedSTS = _memory.ReadWord(address + 14);
            rtdesc.SavedB = _memory.ReadWord(address + 15);
            rtdesc.WLINK = _memory.ReadWord(address + 16);
            rtdesc.ActiveSegment1 = _memory.ReadWord(address + 17);
            rtdesc.ActiveSegment2 = _memory.ReadWord(address + 18);
            rtdesc.ActualPriority = _memory.ReadWord(address + 19);
            rtdesc.BRESLINK = _memory.ReadWord(address + 20);
            rtdesc.ReservedSegment = _memory.ReadWord(address + 21);
            rtdesc.SegmentBitmap0 = _memory.ReadWord(address + 22);
            rtdesc.SegmentBitmap1 = _memory.ReadWord(address + 23);
            rtdesc.SegmentBitmap2 = _memory.ReadWord(address + 24);
            rtdesc.SegmentBitmap3 = _memory.ReadWord(address + 25);
            
            // Calculate program number if in RT-description table
            if (address >= SintranAddresses.RTDESC_START)
            {
                rtdesc.ProgramNumber = (address - SintranAddresses.RTDESC_START) / SintranAddresses.RTDESC_SIZE;
            }
            
            return rtdesc;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error reading RT-description at {OctalHelpers.FormatAddress(address)}: {ex.Message}");
            return null;
        }
    }
    
    /// <summary>
    /// Read an I/O datafield from memory.
    /// Note: This reads only the standard header (first 10 words).
    /// Device-specific fields must be read separately.
    /// </summary>
    /// <param name="address">Physical address of datafield</param>
    /// <param name="size">Size of datafield in words (optional, default 10)</param>
    /// <returns>Populated datafield object, or null if invalid</returns>
    public IoDatafield ReadDatafield(ushort address, ushort size = 10)
    {
        if (address == 0 || address == 0xFFFF)
            return null;
        
        try
        {
            var datafield = new IoDatafield 
            { 
                Address = address,
                Size = size
            };
            
            // Read standard header (first 10 words)
            datafield.RESLINK = _memory.ReadWord(address + 0);
            datafield.RTRES = _memory.ReadWord(address + 1);
            datafield.BWLINK = _memory.ReadWord(address + 2);
            datafield.Semaphore = _memory.ReadWord(address + 3);
            datafield.Status = _memory.ReadWord(address + 4);
            datafield.MLINK = _memory.ReadWord(address + 5);
            datafield.MFUNC = _memory.ReadWord(address + 6);
            datafield.DeviceType = _memory.ReadWord(address + 7);
            datafield.HDEV = _memory.ReadWord(address + 8);
            datafield.Ident = _memory.ReadWord(address + 9);
            
            return datafield;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error reading datafield at {OctalHelpers.FormatAddress(address)}: {ex.Message}");
            return null;
        }
    }
}
```

### 6.2 System State Snapshot

```csharp
/// <summary>
/// Complete snapshot of SINTRAN system state.
/// </summary>
public class SystemStateSnapshot
{
    /// <summary>
    /// Timestamp when snapshot was taken.
    /// </summary>
    public DateTime Timestamp { get; set; }
    
    /// <summary>
    /// System time (ATIME) when snapshot was taken.
    /// </summary>
    public uint SystemTime { get; set; }
    
    /// <summary>
    /// Monitor time (MTIME) when snapshot was taken.
    /// </summary>
    public uint MonitorTime { get; set; }
    
    /// <summary>
    /// Programs in execution queue (ready to run).
    /// </summary>
    public List<RtDescription> ExecutionQueue { get; set; }
    
    /// <summary>
    /// Programs in time queue (scheduled for future).
    /// </summary>
    public List<RtDescription> TimeQueue { get; set; }
    
    /// <summary>
    /// Datafields in monitor queue.
    /// </summary>
    public List<IoDatafield> MonitorQueue { get; set; }
    
    /// <summary>
    /// All programs waiting for resources (grouped by resource).
    /// </summary>
    public Dictionary<ushort, List<RtDescription>> WaitingQueues { get; set; }
    
    /// <summary>
    /// Total number of RT programs found.
    /// </summary>
    public int TotalPrograms => ExecutionQueue.Count + TimeQueue.Count + 
                                WaitingQueues.Values.Sum(q => q.Count);
}
```

---

*Continued in next part...*

(This document is getting long. I'll continue with sections 7-10 in the next part. Shall I continue?)

## 7. Program State Access
## 7. Program State Access

### 7.1 System State Reader

```csharp
/// <summary>
/// Reads complete SINTRAN system state from memory.
/// </summary>
public class SintranSystemReader
{
    private readonly IMemoryAccess _memory;
    private readonly SintranQueueReader _queueReader;
    
    public SintranSystemReader(IMemoryAccess memory)
    {
        _memory = memory ?? throw new ArgumentNullException(nameof(memory));
        _queueReader = new SintranQueueReader(memory);
    }
    
    /// <summary>
    /// Get complete system state snapshot.
    /// </summary>
    public SystemStateSnapshot GetSystemState()
    {
        var snapshot = new SystemStateSnapshot
        {
            Timestamp = DateTime.Now,
            SystemTime = _memory.ReadDoubleWord(SintranAddresses.ATIME),
            MonitorTime = _memory.ReadDoubleWord(SintranAddresses.MTIME),
            ExecutionQueue = _queueReader.ReadExecutionQueue(),
            TimeQueue = _queueReader.ReadTimeQueue(),
            MonitorQueue = _queueReader.ReadMonitorQueue(),
            WaitingQueues = new Dictionary<ushort, List<RtDescription>>()
        };
        
        // Find all waiting queues by scanning datafields
        var datafields = ScanAllDatafields();
        foreach (var df in datafields)
        {
            if (df.HasWaiters)
            {
                var waiters = _queueReader.ReadWaitingQueue(df.Address);
                if (waiters.Count > 0)
                {
                    snapshot.WaitingQueues[df.Address] = waiters;
                }
            }
        }
        
        return snapshot;
    }
    
    /// <summary>
    /// Scan datafield area to find all device control blocks.
    /// </summary>
    private List<IoDatafield> ScanAllDatafields()
    {
        var datafields = new List<IoDatafield>();
        
        // Scan typical datafield area (020000₈ to 026000₈)
        ushort addr = SintranAddresses.DATAFIELD_START;
        ushort end = SintranAddresses.RTDESC_START;
        
        while (addr < end)
        {
            // Try to read datafield header
            var df = _queueReader.ReadDatafield(addr);
            if (df != null && IsValidDatafield(df))
            {
                datafields.Add(df);
                // Skip to next likely datafield (estimate 200₈ words for disk, 20₈ for simple)
                addr += (ushort)(df.DeviceType >= 100 ? 128 : 16);  // Rough heuristic
            }
            else
            {
                addr += 16;  // Skip ahead
            }
        }
        
        return datafields;
    }
    
    /// <summary>
    /// Check if datafield structure looks valid.
    /// </summary>
    private bool IsValidDatafield(IoDatafield df)
    {
        // Basic sanity checks
        if (df == null) return false;
        
        // MLINK should be 0, 0xFFFF, or a valid address
        if (df.MLINK != 0 && df.MLINK != 0xFFFF && 
            (df.MLINK < 0x1000 || df.MLINK > 0xF000))
            return false;
        
        // RTRES should be 0 or point to RT-description area
        if (df.RTRES != 0 && 
            (df.RTRES < SintranAddresses.RTDESC_START || df.RTRES > 0xE000))
            return false;
        
        return true;
    }
    
    /// <summary>
    /// Get currently executing program (if any).
    /// This requires examining CPU state, which varies by implementation.
    /// </summary>
    public RtDescription GetCurrentProgram(byte currentLevel)
    {
        // The currently executing program is typically:
        // 1. First in execution queue (if on A-LEVEL)
        // 2. Determined by current PCR settings
        
        var execQueue = _queueReader.ReadExecutionQueue();
        return execQueue.FirstOrDefault();
    }
    
    /// <summary>
    /// Find what resource a program is waiting for.
    /// </summary>
    public IoDatafield FindWaitingResource(ushort rtdescAddress)
    {
        var rtdesc = _queueReader.ReadRtDescription(rtdescAddress);
        if (rtdesc == null || !rtdesc.IsWaiting)
            return null;
        
        // Scan all datafields to find which one has this program in its waiting queue
        var datafields = ScanAllDatafields();
        foreach (var df in datafields)
        {
            if (df.BWLINK == 0) continue;
            
            var waiters = _queueReader.ReadWaitingQueue(df.Address);
            if (waiters.Any(w => w.Address == rtdescAddress))
            {
                return df;
            }
        }
        
        return null;
    }
    
    /// <summary>
    /// Get all programs (in all queues).
    /// </summary>
    public List<RtDescription> GetAllPrograms()
    {
        var programs = new HashSet<ushort>();  // Use address as key to avoid duplicates
        var result = new List<RtDescription>();
        
        // Execution queue
        foreach (var prog in _queueReader.ReadExecutionQueue())
        {
            if (programs.Add(prog.Address))
                result.Add(prog);
        }
        
        // Time queue
        foreach (var prog in _queueReader.ReadTimeQueue())
        {
            if (programs.Add(prog.Address))
                result.Add(prog);
        }
        
        // Waiting queues
        var datafields = ScanAllDatafields();
        foreach (var df in datafields)
        {
            if (!df.HasWaiters) continue;
            
            foreach (var prog in _queueReader.ReadWaitingQueue(df.Address))
            {
                if (programs.Add(prog.Address))
                    result.Add(prog);
            }
        }
        
        return result;
    }
}
```

### 7.2 Program State Display Helper

```csharp
/// <summary>
/// Helper for formatting program state for display.
/// </summary>
public static class ProgramStateFormatter
{
    /// <summary>
    /// Format RT-description as human-readable string.
    /// </summary>
    public static string FormatProgram(RtDescription prog)
    {
        var sb = new StringBuilder();
        
        sb.AppendLine($"RT Program #{prog.ProgramNumber}");
        sb.AppendLine($"  Address:     {prog.AddressString}");
        sb.AppendLine($"  Priority:    {prog.Priority} ({prog.PriorityOctal})");
        sb.AppendLine($"  State:       {prog.StateString}");
        sb.AppendLine($"  Start Addr:  {OctalHelpers.FormatAddress(prog.StartAddress)}");
        sb.AppendLine($"  Segments:    Primary={prog.PrimarySegment}, Secondary={prog.SecondarySegment}");
        
        if (prog.IsPeriodic)
        {
            sb.AppendLine($"  Interval:    {prog.TimeInterval} time units");
        }
        
        if (prog.IsWaiting || prog.ScheduledTime > 0)
        {
            sb.AppendLine($"  Sched Time:  {prog.ScheduledTime}");
        }
        
        // Saved registers
        sb.AppendLine($"  Registers:");
        sb.AppendLine($"    P={OctalHelpers.ToOctal(prog.SavedP)} X={OctalHelpers.ToOctal(prog.SavedX)} " +
                     $"T={OctalHelpers.ToOctal(prog.SavedT)} A={OctalHelpers.ToOctal(prog.SavedA)}");
        sb.AppendLine($"    D={OctalHelpers.ToOctal(prog.SavedD)} L={OctalHelpers.ToOctal(prog.SavedL)} " +
                     $"B={OctalHelpers.ToOctal(prog.SavedB)}");
        
        return sb.ToString();
    }
    
    /// <summary>
    /// Format complete system state as text.
    /// </summary>
    public static string FormatSystemState(SystemStateSnapshot snapshot)
    {
        var sb = new StringBuilder();
        
        sb.AppendLine("=".PadRight(80, '='));
        sb.AppendLine("SINTRAN III System State");
        sb.AppendLine($"Timestamp: {snapshot.Timestamp:yyyy-MM-dd HH:mm:ss.fff}");
        sb.AppendLine($"System Time: {snapshot.SystemTime} (ATIME)");
        sb.AppendLine($"Monitor Time: {snapshot.MonitorTime} (MTIME)");
        sb.AppendLine("=".PadRight(80, '='));
        sb.AppendLine();
        
        // Execution Queue
        sb.AppendLine($"EXECUTION QUEUE ({snapshot.ExecutionQueue.Count} programs ready):");
        sb.AppendLine("-".PadRight(80, '-'));
        foreach (var prog in snapshot.ExecutionQueue)
        {
            sb.AppendLine($"  #{prog.ProgramNumber,-3} Priority={prog.Priority,3} " +
                         $"State={prog.StateString,-20} Addr={prog.AddressString}");
        }
        sb.AppendLine();
        
        // Time Queue
        sb.AppendLine($"TIME QUEUE ({snapshot.TimeQueue.Count} programs scheduled):");
        sb.AppendLine("-".PadRight(80, '-'));
        foreach (var prog in snapshot.TimeQueue.OrderBy(p => p.ScheduledTime))
        {
            sb.AppendLine($"  #{prog.ProgramNumber,-3} SchedTime={prog.ScheduledTime,10} " +
                         $"Priority={prog.Priority,3} Addr={prog.AddressString}");
        }
        sb.AppendLine();
        
        // Waiting Queues
        sb.AppendLine($"WAITING QUEUES ({snapshot.WaitingQueues.Count} resources with waiters):");
        sb.AppendLine("-".PadRight(80, '-'));
        foreach (var kvp in snapshot.WaitingQueues.OrderBy(kv => kv.Key))
        {
            sb.AppendLine($"  Resource at {OctalHelpers.FormatAddress(kvp.Key)} " +
                         $"has {kvp.Value.Count} waiter(s):");
            foreach (var prog in kvp.Value)
            {
                sb.AppendLine($"    #{prog.ProgramNumber,-3} Priority={prog.Priority,3} " +
                             $"Addr={prog.AddressString}");
            }
        }
        sb.AppendLine();
        
        // Monitor Queue
        sb.AppendLine($"MONITOR QUEUE ({snapshot.MonitorQueue.Count} pending activations):");
        sb.AppendLine("-".PadRight(80, '-'));
        foreach (var df in snapshot.MonitorQueue)
        {
            sb.AppendLine($"  Datafield at {df.AddressString} " +
                         $"Type={df.DeviceType:X4} MFUNC={OctalHelpers.FormatAddress(df.MFUNC)}");
        }
        
        sb.AppendLine();
        sb.AppendLine($"TOTAL: {snapshot.TotalPrograms} RT programs in system");
        sb.AppendLine("=".PadRight(80, '='));
        
        return sb.ToString();
    }
}
```

---

## 8. MMU and Page Table Access

### 8.1 Page Table Reader

```csharp
/// <summary>
/// Reads and manages ND-100 page tables.
/// </summary>
public class PageTableReader
{
    private readonly IMemoryAccess _memory;
    
    // Page table locations (these are typically in POF area or bank 1)
    // These addresses need to be confirmed from system generation
    private const uint PIT0_BASE = 0x0;     // Basic PIT location (varies)
    private const uint PIT1_BASE = 0x0;     // RPIT location (dynamic, from symbols)
    private const uint PIT2_BASE = 0x0;     // MPIT location (dynamic)
    private const uint PIT3_BASE = 0x0;     // IPIT location (dynamic)
    
    public PageTableReader(IMemoryAccess memory)
    {
        _memory = memory ?? throw new ArgumentNullException(nameof(memory));
    }
    
    /// <summary>
    /// Read a specific Page Index Table.
    /// </summary>
    /// <param name="pitNumber">PIT number (0-3)</param>
    /// <param name="pitBaseAddress">Physical address of PIT in memory</param>
    /// <returns>Array of 64 PIT entries</returns>
    public PitEntry[] ReadPageTable(byte pitNumber, uint pitBaseAddress)
    {
        if (pitNumber > 3)
            throw new ArgumentException("PIT number must be 0-3", nameof(pitNumber));
        
        var entries = new PitEntry[SintranAddresses.PIT_ENTRIES];
        
        for (int i = 0; i < SintranAddresses.PIT_ENTRIES; i++)
        {
            ushort rawValue = _memory.ReadWord(pitBaseAddress + (uint)i);
            entries[i] = new PitEntry { RawValue = rawValue };
        }
        
        return entries;
    }
    
    /// <summary>
    /// Translate virtual address to physical address using a PIT.
    /// </summary>
    /// <param name="virtualAddress">16-bit virtual address</param>
    /// <param name="pit">Page Index Table to use</param>
    /// <returns>Physical address, or null if page fault</returns>
    public uint? TranslateAddress(ushort virtualAddress, PitEntry[] pit)
    {
        if (pit == null || pit.Length != 64)
            throw new ArgumentException("PIT must have 64 entries", nameof(pit));
        
        // Extract page number (bits 15-10) and offset (bits 9-0)
        int pageNumber = virtualAddress >> SintranAddresses.PAGE_NUMBER_SHIFT;
        int pageOffset = virtualAddress & SintranAddresses.PAGE_OFFSET_MASK;
        
        // Get PIT entry
        var entry = pit[pageNumber];
        
        // Check if page is present
        if (!entry.IsPresent)
            return null;  // Page fault
        
        // Calculate physical address
        uint physicalPage = entry.PhysicalPage;
        uint physicalAddress = (physicalPage * SintranAddresses.PAGE_SIZE) + (uint)pageOffset;
        
        return physicalAddress;
    }
    
    /// <summary>
    /// Check if an address is in the POF (Paging Off) area.
    /// POF area is always accessible, even with MMU off.
    /// </summary>
    public static bool IsInPOFArea(uint physicalAddress)
    {
        // POF area: 100000₈ to 177377₈ (32768 to 65279 decimal words)
        return physicalAddress >= SintranAddresses.POF_START && 
               physicalAddress <= SintranAddresses.POF_END;
    }
    
    /// <summary>
    /// Format page table for display.
    /// </summary>
    public static string FormatPageTable(PitEntry[] pit, byte pitNumber)
    {
        var sb = new StringBuilder();
        
        sb.AppendLine($"Page Index Table {pitNumber} (PIT{pitNumber}):");
        sb.AppendLine("Page  PhysPage  Ring  Perms  PU WIP  Status");
        sb.AppendLine("----  --------  ----  -----  -- ---  ------");
        
        for (int i = 0; i < pit.Length; i++)
        {
            var entry = pit[i];
            
            if (!entry.IsPresent)
            {
                sb.AppendLine($"{i,2:D2}    ----      --    ---    -  -    NOT PRESENT");
            }
            else
            {
                sb.AppendLine($"{i,2:D2}    {entry.PhysicalPage,3:D3}       {entry.Ring}     " +
                             $"{entry.PermissionsString}    {(entry.PageUsed ? 'Y' : 'N')}  " +
                             $"{(entry.WrittenInPage ? 'Y' : 'N')}    " +
                             $"Phys={OctalHelpers.FormatAddress(entry.PhysicalAddress)}");
            }
        }
        
        return sb.ToString();
    }
}
```

### 8.2 MMU State

```csharp
/// <summary>
/// Represents complete MMU state.
/// </summary>
public class MmuState
{
    /// <summary>
    /// All 4 Page Index Tables.
    /// </summary>
    public PitEntry[][] PageTables { get; set; } = new PitEntry[4][];
    
    /// <summary>
    /// Physical addresses of each PIT.
    /// </summary>
    public uint[] PitBaseAddresses { get; set; } = new uint[4];
    
    /// <summary>
    /// Current PCR (Paging Control Register) values for each interrupt level.
    /// </summary>
    public byte[] PCRValues { get; set; } = new byte[16];
    
    /// <summary>
    /// Extract PIT numbers from PCR value.
    /// </summary>
    public static (byte npit, byte apit, byte ring) DecodePCR(byte pcr)
    {
        // PCR format:
        // Bits 5-4: NPIT (Normal PIT)
        // Bits 3-2: APIT (Alternative PIT)
        // Bits 1-0: RING
        byte npit = (byte)((pcr >> 4) & 3);
        byte apit = (byte)((pcr >> 2) & 3);
        byte ring = (byte)(pcr & 3);
        
        return (npit, apit, ring);
    }
}
```

### 8.3 Memory Map Reader

```csharp
/// <summary>
/// Reads SINTRAN physical memory map (TMMAP).
/// </summary>
public class MemoryMapReader
{
    private readonly IMemoryAccess _memory;
    
    public MemoryMapReader(IMemoryAccess memory)
    {
        _memory = memory ?? throw new ArgumentNullException(nameof(memory));
    }
    
    /// <summary>
    /// Read TMMAP (Total Memory Map) bitmap.
    /// </summary>
    /// <returns>Bitmap of installed memory banks</returns>
    public ushort[] ReadMemoryMap()
    {
        var tmmap = new ushort[18];  // 20₈ octal = 18 decimal words
        
        for (int i = 0; i < 18; i++)
        {
            tmmap[i] = _memory.ReadWord((uint)(SintranAddresses.TMMAP + i));
        }
        
        return tmmap;
    }
    
    /// <summary>
    /// Check if a specific memory bank is installed.
    /// </summary>
    /// <param name="tmmap">TMMAP bitmap</param>
    /// <param name="bankNumber">Bank number (0-287, each bank = 32 pages = 32KB)</param>
    /// <returns>True if bank is installed</returns>
    public static bool IsBankInstalled(ushort[] tmmap, int bankNumber)
    {
        if (bankNumber < 0 || bankNumber >= 288)  // 18 words * 16 bits
            return false;
        
        int wordIndex = bankNumber / 16;
        int bitIndex = bankNumber % 16;
        
        return BitFieldHelpers.TestBit(tmmap[wordIndex], bitIndex);
    }
    
    /// <summary>
    /// Get total installed memory in words.
    /// </summary>
    public static uint GetTotalMemory(ushort[] tmmap)
    {
        uint banks = 0;
        
        for (int i = 0; i < 288; i++)
        {
            if (IsBankInstalled(tmmap, i))
                banks++;
        }
        
        // Each bank = 32 pages, each page = 1024 words
        return banks * 32 * 1024;
    }
    
    /// <summary>
    /// Format memory map for display.
    /// </summary>
    public static string FormatMemoryMap(ushort[] tmmap)
    {
        var sb = new StringBuilder();
        
        sb.AppendLine("Physical Memory Map (TMMAP):");
        sb.AppendLine($"Total Memory: {GetTotalMemory(tmmap):N0} words " +
                     $"({GetTotalMemory(tmmap) / 512:N0} KB)");
        sb.AppendLine();
        sb.AppendLine("Bank#  Installed  Address Range (Octal)");
        sb.AppendLine("-----  ---------  ---------------------");
        
        for (int bank = 0; bank < 288; bank++)
        {
            if (IsBankInstalled(tmmap, bank))
            {
                uint startAddr = (uint)(bank * 32 * 1024);  // 32 pages per bank
                uint endAddr = startAddr + (32 * 1024) - 1;
                
                sb.AppendLine($"{bank,3}    Yes        {OctalHelpers.FormatAddress(startAddr)} - " +
                             $"{OctalHelpers.FormatAddress(endAddr)}");
            }
        }
        
        return sb.ToString();
    }
}
```

---

## 9. Complete Usage Examples

### 9.1 Basic System Monitoring

```csharp
/// <summary>
/// Example: Monitor SINTRAN system state.
/// </summary>
public class SintranMonitorExample
{
    private readonly IMemoryAccess _memory;
    private readonly SintranSystemReader _systemReader;
    private readonly PageTableReader _pageTableReader;
    private readonly MemoryMapReader _memMapReader;
    
    public SintranMonitorExample(IMemoryAccess memory)
    {
        _memory = memory;
        _systemReader = new SintranSystemReader(memory);
        _pageTableReader = new PageTableReader(memory);
        _memMapReader = new MemoryMapReader(memory);
    }
    
    /// <summary>
    /// Display complete system status.
    /// </summary>
    public void DisplaySystemStatus()
    {
        Console.WriteLine("Reading SINTRAN system state...");
        Console.WriteLine();
        
        // Get system snapshot
        var snapshot = _systemReader.GetSystemState();
        
        // Display formatted output
        Console.WriteLine(ProgramStateFormatter.FormatSystemState(snapshot));
        
        // Show memory map
        var tmmap = _memMapReader.ReadMemoryMap();
        Console.WriteLine();
        Console.WriteLine(MemoryMapReader.FormatMemoryMap(tmmap));
    }
    
    /// <summary>
    /// Monitor a specific program.
    /// </summary>
    public void MonitorProgram(int programNumber)
    {
        ushort rtdescAddr = (ushort)(SintranAddresses.RTDESC_START + 
                                     programNumber * SintranAddresses.RTDESC_SIZE);
        
        var queueReader = new SintranQueueReader(_memory);
        var prog = queueReader.ReadRtDescription(rtdescAddr);
        
        if (prog == null)
        {
            Console.WriteLine($"Program #{programNumber} not found or invalid");
            return;
        }
        
        Console.WriteLine(ProgramStateFormatter.FormatProgram(prog));
        
        // Show reserved resources
        var reservations = queueReader.ReadReservationQueue(rtdescAddr);
        if (reservations.Count > 0)
        {
            Console.WriteLine($"  Reserved Resources ({reservations.Count}):");
            foreach (var res in reservations)
            {
                Console.WriteLine($"    {res.AddressString} Type={res.DeviceType:X4}");
            }
        }
        
        // If waiting, show what it's waiting for
        if (prog.IsWaiting)
        {
            var waitingFor = _systemReader.FindWaitingResource(rtdescAddr);
            if (waitingFor != null)
            {
                Console.WriteLine($"  Waiting for: Resource at {waitingFor.AddressString}");
            }
        }
    }
    
    /// <summary>
    /// Watch execution queue changes (polling example).
    /// </summary>
    public async Task WatchExecutionQueue(CancellationToken cancellationToken)
    {
        var queueReader = new SintranQueueReader(_memory);
        var previousHash = 0;
        
        while (!cancellationToken.IsCancellationRequested)
        {
            var execQueue = queueReader.ReadExecutionQueue();
            
            // Create hash of queue state
            var currentHash = string.Join(",", execQueue.Select(p => p.Address)).GetHashCode();
            
            if (currentHash != previousHash)
            {
                Console.WriteLine($"[{DateTime.Now:HH:mm:ss.fff}] Execution queue changed:");
                foreach (var prog in execQueue)
                {
                    Console.WriteLine($"  #{prog.ProgramNumber} Priority={prog.Priority} " +
                                     $"State={prog.StateString}");
                }
                Console.WriteLine();
                
                previousHash = currentHash;
            }
            
            await Task.Delay(100, cancellationToken);  // Poll every 100ms
        }
    }
}
```

### 9.2 Memory Access with MMU

```csharp
/// <summary>
/// Example: Reading memory with MMU translation.
/// </summary>
public class MmuAwareMemoryAccess : IMemoryAccess
{
    private readonly IMemoryAccess _physicalMemory;
    private readonly PageTableReader _pageTableReader;
    private MmuState _mmuState;
    
    public MmuAwareMemoryAccess(IMemoryAccess physicalMemory, MmuState mmuState)
    {
        _physicalMemory = physicalMemory;
        _pageTableReader = new PageTableReader(physicalMemory);
        _mmuState = mmuState;
    }
    
    /// <summary>
    /// Read word using virtual address (with MMU translation).
    /// </summary>
    public ushort ReadVirtualWord(ushort virtualAddress, byte pitNumber)
    {
        var pit = _mmuState.PageTables[pitNumber];
        
        var physAddr = _pageTableReader.TranslateAddress(virtualAddress, pit);
        if (!physAddr.HasValue)
        {
            throw new Exception($"Page fault at virtual address {OctalHelpers.FormatAddress(virtualAddress)} " +
                              $"in PIT {pitNumber}");
        }
        
        return _physicalMemory.ReadWord(physAddr.Value);
    }
    
    // Implement IMemoryAccess interface (physical access)
    public ushort ReadWord(uint address) => _physicalMemory.ReadWord(address);
    
    public uint ReadDoubleWord(uint address) => _physicalMemory.ReadDoubleWord(address);
    
    public void WriteWord(uint address, ushort value) => _physicalMemory.WriteWord(address, value);
    
    public void WriteDoubleWord(uint address, uint value) => _physicalMemory.WriteDoubleWord(address, value);
    
    public uint? TranslateAddress(ushort virtualAddress, byte pitNumber)
    {
        var pit = _mmuState.PageTables[pitNumber];
        return _pageTableReader.TranslateAddress(virtualAddress, pit);
    }
}
```

### 9.3 Complete Integration Example

```csharp
/// <summary>
/// Complete example integrating all components.
/// </summary>
public class ND100EmulatorIntegration
{
    private readonly IMemoryAccess _memory;
    private readonly SintranSystemReader _systemReader;
    private readonly SintranQueueReader _queueReader;
    
    public ND100EmulatorIntegration(IMemoryAccess memory)
    {
        _memory = memory;
        _systemReader = new SintranSystemReader(memory);
        _queueReader = new SintranQueueReader(memory);
    }
    
    /// <summary>
    /// Display complete dashboard.
    /// </summary>
    public void ShowDashboard()
    {
        Console.Clear();
        Console.WriteLine("╔═══════════════════════════════════════════════════════════════════════════╗");
        Console.WriteLine("║              SINTRAN III System Dashboard - ND-100 Emulator              ║");
        Console.WriteLine("╚═══════════════════════════════════════════════════════════════════════════╝");
        Console.WriteLine();
        
        // System time
        var atime = _memory.ReadDoubleWord(SintranAddresses.ATIME);
        var mtime = _memory.ReadDoubleWord(SintranAddresses.MTIME);
        Console.WriteLine($"System Time: {atime,10}    Monitor Time: {mtime,10}");
        Console.WriteLine();
        
        // Execution queue
        var execQueue = _queueReader.ReadExecutionQueue();
        Console.WriteLine($"┌─ Execution Queue ({execQueue.Count} ready) ───────────────────────────┐");
        foreach (var prog in execQueue.Take(5))
        {
            var status = prog.IsInhibited ? "INHIBITED" : "READY";
            Console.WriteLine($"│ RT#{prog.ProgramNumber,-3} Pri={prog.Priority,3} " +
                             $"{status,-10} Seg={prog.PrimarySegment,3} " +
                             $"P={OctalHelpers.ToOctal(prog.SavedP, 6)} │");
        }
        if (execQueue.Count > 5)
            Console.WriteLine($"│ ... and {execQueue.Count - 5} more                                        │");
        Console.WriteLine("└─────────────────────────────────────────────────────────────────────────┘");
        Console.WriteLine();
        
        // Time queue
        var timeQueue = _queueReader.ReadTimeQueue();
        Console.WriteLine($"┌─ Time Queue ({timeQueue.Count} scheduled) ────────────────────────────┐");
        foreach (var prog in timeQueue.OrderBy(p => p.ScheduledTime).Take(5))
        {
            var timeLeft = (long)prog.ScheduledTime - (long)atime;
            Console.WriteLine($"│ RT#{prog.ProgramNumber,-3} Time={prog.ScheduledTime,10} " +
                             $"Δ={timeLeft,8} Pri={prog.Priority,3}              │");
        }
        if (timeQueue.Count > 5)
            Console.WriteLine($"│ ... and {timeQueue.Count - 5} more                                        │");
        Console.WriteLine("└─────────────────────────────────────────────────────────────────────────┘");
        Console.WriteLine();
        
        // Memory usage
        var memMap = new MemoryMapReader(_memory);
        var tmmap = memMap.ReadMemoryMap();
        var totalMem = MemoryMapReader.GetTotalMemory(tmmap);
        Console.WriteLine($"Physical Memory: {totalMem / 1024,6:N0} KB " +
                         $"({totalMem:N0} words)");
        Console.WriteLine();
    }
    
    /// <summary>
    /// Export system state to JSON for external tools.
    /// </summary>
    public string ExportStateAsJson()
    {
        var snapshot = _systemReader.GetSystemState();
        
        var json = new
        {
            timestamp = snapshot.Timestamp,
            systemTime = snapshot.SystemTime,
            monitorTime = snapshot.MonitorTime,
            executionQueue = snapshot.ExecutionQueue.Select(p => new
            {
                programNumber = p.ProgramNumber,
                address = $"{p.Address:X4}",
                priority = p.Priority,
                state = p.StateString,
                savedRegisters = new
                {
                    P = $"{p.SavedP:X4}",
                    X = $"{p.SavedX:X4}",
                    A = $"{p.SavedA:X4}",
                    D = $"{p.SavedD:X4}",
                    L = $"{p.SavedL:X4}",
                    B = $"{p.SavedB:X4}"
                }
            }),
            timeQueue = snapshot.TimeQueue.Select(p => new
            {
                programNumber = p.ProgramNumber,
                address = $"{p.Address:X4}",
                scheduledTime = p.ScheduledTime,
                priority = p.Priority
            }),
            waitingQueues = snapshot.WaitingQueues.Select(kvp => new
            {
                resourceAddress = $"{kvp.Key:X4}",
                waiters = kvp.Value.Select(p => new
                {
                    programNumber = p.ProgramNumber,
                    priority = p.Priority
                })
            })
        };
        
        return System.Text.Json.JsonSerializer.Serialize(json, new System.Text.Json.JsonSerializerOptions 
        { 
            WriteIndented = true 
        });
    }
}
```

---

## 10. Advanced Topics

### 10.1 Detecting Queue Corruption

```csharp
/// <summary>
/// Validator for detecting queue corruption or invalid states.
/// </summary>
public class QueueValidator
{
    /// <summary>
    /// Validate execution queue integrity.
    /// </summary>
    public static List<string> ValidateExecutionQueue(List<RtDescription> queue, ushort headAddress)
    {
        var errors = new List<string>();
        
        if (queue.Count == 0)
            return errors;  // Empty is valid
        
        // Check priority ordering
        for (int i = 0; i < queue.Count - 1; i++)
        {
            if (queue[i].Priority < queue[i + 1].Priority)
            {
                errors.Add($"Priority violation: Program #{queue[i].ProgramNumber} (pri={queue[i].Priority}) " +
                          $"before #{queue[i + 1].ProgramNumber} (pri={queue[i + 1].Priority})");
            }
        }
        
        // Check circular link
        if (queue.Last().WLINK != headAddress)
        {
            errors.Add($"Broken circular link: Last program WLINK={OctalHelpers.FormatAddress(queue.Last().WLINK)} " +
                      $"should point to head {OctalHelpers.FormatAddress(headAddress)}");
        }
        
        // Check for duplicates
        var addresses = new HashSet<ushort>();
        foreach (var prog in queue)
        {
            if (!addresses.Add(prog.Address))
            {
                errors.Add($"Duplicate program in queue: {OctalHelpers.FormatAddress(prog.Address)}");
            }
        }
        
        return errors;
    }
    
    /// <summary>
    /// Validate time queue integrity.
    /// </summary>
    public static List<string> ValidateTimeQueue(List<RtDescription> queue)
    {
        var errors = new List<string>();
        
        if (queue.Count == 0)
            return errors;
        
        // Check time ordering
        for (int i = 0; i < queue.Count - 1; i++)
        {
            if (queue[i].ScheduledTime > queue[i + 1].ScheduledTime)
            {
                errors.Add($"Time ordering violation: Program #{queue[i].ProgramNumber} " +
                          $"(time={queue[i].ScheduledTime}) after #{queue[i + 1].ProgramNumber} " +
                          $"(time={queue[i + 1].ScheduledTime})");
            }
        }
        
        // Check termination
        if (queue.Last().TLNK != 0xFFFF && queue.Last().TLNK != 0)
        {
            errors.Add($"Time queue not properly terminated: Last TLNK={OctalHelpers.FormatAddress(queue.Last().TLNK)}");
        }
        
        return errors;
    }
}
```

### 10.2 Performance Monitoring

```csharp
/// <summary>
/// Monitor system performance metrics.
/// </summary>
public class PerformanceMonitor
{
    private class ProgramStats
    {
        public int ProgramNumber { get; set; }
        public int ActivationCount { get; set; }
        public uint TotalTimeInQueue { get; set; }
        public int PriorityChanges { get; set; }
        public byte LastPriority { get; set; }
    }
    
    private readonly Dictionary<int, ProgramStats> _stats = new();
    private readonly IMemoryAccess _memory;
    private uint _lastSystemTime;
    
    public PerformanceMonitor(IMemoryAccess memory)
    {
        _memory = memory;
        _lastSystemTime = _memory.ReadDoubleWord(SintranAddresses.ATIME);
    }
    
    /// <summary>
    /// Update statistics from current system state.
    /// </summary>
    public void UpdateStatistics(SystemStateSnapshot snapshot)
    {
        uint currentTime = snapshot.SystemTime;
        uint deltaTime = currentTime - _lastSystemTime;
        
        // Track programs in execution queue
        foreach (var prog in snapshot.ExecutionQueue)
        {
            if (!_stats.TryGetValue(prog.ProgramNumber, out var stats))
            {
                stats = new ProgramStats 
                { 
                    ProgramNumber = prog.ProgramNumber,
                    LastPriority = prog.Priority
                };
                _stats[prog.ProgramNumber] = stats;
            }
            
            stats.ActivationCount++;
            stats.TotalTimeInQueue += deltaTime;
            
            if (stats.LastPriority != prog.Priority)
            {
                stats.PriorityChanges++;
                stats.LastPriority = prog.Priority;
            }
        }
        
        _lastSystemTime = currentTime;
    }
    
    /// <summary>
    /// Get performance report.
    /// </summary>
    public string GetPerformanceReport()
    {
        var sb = new StringBuilder();
        
        sb.AppendLine("Program Performance Statistics:");
        sb.AppendLine("Prog#  Activations  Avg Time  Priority Changes");
        sb.AppendLine("-----  -----------  --------  ----------------");
        
        foreach (var stats in _stats.Values.OrderByDescending(s => s.ActivationCount))
        {
            var avgTime = stats.ActivationCount > 0 ? stats.TotalTimeInQueue / stats.ActivationCount : 0;
            sb.AppendLine($"#{stats.ProgramNumber,-4}  {stats.ActivationCount,11:N0}  {avgTime,8}  {stats.PriorityChanges,16}");
        }
        
        return sb.ToString();
    }
}
```

### 10.3 Deadlock Detection

```csharp
/// <summary>
/// Detect potential deadlocks in resource allocation.
/// </summary>
public class DeadlockDetector
{
    /// <summary>
    /// Check for circular wait conditions (potential deadlock).
    /// </summary>
    public static List<string> DetectDeadlocks(SystemStateSnapshot snapshot, SintranQueueReader queueReader)
    {
        var deadlocks = new List<string>();
        
        // Build resource allocation graph
        // Node: program or resource
        // Edge: program → resource (waiting for)
        //       resource → program (owned by)
        
        var graph = new Dictionary<ushort, List<ushort>>();
        
        // Add edges for waiting queues
        foreach (var kvp in snapshot.WaitingQueues)
        {
            var resourceAddr = kvp.Key;
            var waiters = kvp.Value;
            
            // Find owner of resource
            var datafield = queueReader.ReadDatafield(resourceAddr);
            if (datafield?.RTRES != 0)
            {
                // Resource → Owner
                if (!graph.ContainsKey(resourceAddr))
                    graph[resourceAddr] = new List<ushort>();
                graph[resourceAddr].Add(datafield.RTRES);
                
                // Waiters → Resource
                foreach (var waiter in waiters)
                {
                    if (!graph.ContainsKey(waiter.Address))
                        graph[waiter.Address] = new List<ushort>();
                    graph[waiter.Address].Add(resourceAddr);
                }
            }
        }
        
        // Detect cycles using DFS
        var visited = new HashSet<ushort>();
        var recStack = new HashSet<ushort>();
        
        foreach (var node in graph.Keys)
        {
            if (DetectCycle(node, graph, visited, recStack, out var cycle))
            {
                deadlocks.Add($"Deadlock detected: {string.Join(" → ", cycle.Select(a => OctalHelpers.FormatAddress(a)))}");
            }
        }
        
        return deadlocks;
    }
    
    private static bool DetectCycle(ushort node, Dictionary<ushort, List<ushort>> graph, 
                                   HashSet<ushort> visited, HashSet<ushort> recStack, 
                                   out List<ushort> cycle)
    {
        cycle = new List<ushort>();
        
        if (recStack.Contains(node))
        {
            cycle.Add(node);
            return true;
        }
        
        if (visited.Contains(node))
            return false;
        
        visited.Add(node);
        recStack.Add(node);
        
        if (graph.TryGetValue(node, out var neighbors))
        {
            foreach (var neighbor in neighbors)
            {
                if (DetectCycle(neighbor, graph, visited, recStack, out cycle))
                {
                    cycle.Insert(0, node);
                    return true;
                }
            }
        }
        
        recStack.Remove(node);
        return false;
    }
}
```

---

## Summary

This document provides complete C# implementation for accessing SINTRAN III kernel structures from an ND-100 emulator, including:

1. **Precise data type mappings** - All sizes clearly documented (16-bit words, 32-bit doubles, bit positions)
2. **Physical memory addresses** - Exact locations from symbol files
3. **Complete data structures** - RT-Description, I/O Datafield, PIT entries with all fields
4. **Queue traversal** - Execution, time, monitor, waiting, and reservation queues
5. **MMU support** - Page table reading and virtual-to-physical address translation
6. **Practical examples** - Complete working code for monitoring system state
7. **Advanced features** - Validation, performance monitoring, deadlock detection

### Key Points

- **All addresses are WORD addresses** (not byte addresses)
- **All fields are 16-bit words** unless explicitly stated (DOUBLE = 32 bits)
- **Bit positions are 0-based** (bit 0 = LSB, bit 15 = MSB)
- **Octal notation is standard** in SINTRAN (use conversion helpers)
- **POF area (100000₈-177377₈)** is always accessible, even with MMU off
- **Queue heads** are in physical memory (kernel data area, 0-2000₈)
- **Circular queues** link back to head; **linear queues** end with 0xFFFF or 0

### Using This Code

1. Implement `IMemoryAccess` for your emulator's memory system
2. Load symbol files to get exact addresses
3. Create instances of reader classes
4. Call `GetSystemState()` to snapshot complete system
5. Use formatters to display or export data

### Further Work Needed

- Confirm exact symbol addresses (MQUEUE, BTIMQU, PIT base addresses)
- Handle segment swapping and dynamic memory allocation
- Implement interrupt level tracking (currently executing program)
- Add support for background program descriptors (non-RT programs)
- Implement file system access (open file tables, file descriptors)

---

## 11. Emulator Debug Windows

For complete emulator debug window implementations with C# code, see:

**[EMULATOR-DEBUG-WINDOWS-COMPLETE.md](EMULATOR-DEBUG-WINDOWS-COMPLETE.md)**

This document provides:
- **Process List Window**: Monitor all RT and background programs with state, priority, MMU settings
- **Driver Information Window**: Show all detected drivers with metadata and status
- **MMU Configuration Window**: Display active MMU state, PITs, and per-process memory mapping
- **Complete Integration**: Full working examples with all addresses and sizes documented

---

## 12. ND-500 Integration

For comprehensive ND-500 communication, DMA, and physical address translation, see:

**[05-ND500-DMA-KERNEL.md](05-ND500-DMA-KERNEL.md)**

This document covers:
- **ND-500 Architecture**: Hardware interface, registers, datafields
- **Message Communication**: XMSG protocol, message buffers in multiport memory
- **DMA Operations**: Complete DMA setup with physical address translation
- **CNVWADR Instruction**: Detailed explanation of address conversion for DMA
- **Shared Memory Mapping**: How 5MPM (multiport memory) is configured and accessed
- **Physical Address Calculation**: Step-by-step examples with concrete addresses

---

**End of SINTRAN III Kernel Access Documentation**

*For questions or updates, refer to the NPL source code and symbol files.*

### Related Documents

- [00-SINTRAN-ARCHITECTURE-OVERVIEW.md](00-SINTRAN-ARCHITECTURE-OVERVIEW.md) - System architecture
- [01-BOOT-SEQUENCE.md](01-BOOT-SEQUENCE.md) - Boot process and initialization
- [02-QUEUE-STRUCTURES-DETAILED.md](02-QUEUE-STRUCTURES-DETAILED.md) - Queue mechanisms
- [03-CPU-DETECTION-AND-INITIALIZATION.md](03-CPU-DETECTION-AND-INITIALIZATION.md) - CPU detection
- [04-MMU-CONTEXT-SWITCHING.md](04-MMU-CONTEXT-SWITCHING.md) - MMU reconfiguration
- [NPL-DEVELOPER-GUIDE.md](NPL-DEVELOPER-GUIDE.md) - NORD PL language reference

