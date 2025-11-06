# Debugging Monitor Calls in SINTRAN Emulator

**How to Calculate Monitor Call Addresses and Disassemble Functionality After Boot**

**Version:** 1.0
**Date:** 2025-11-06
**Purpose:** Guide for emulator developers to trace and debug SINTRAN monitor calls

---

## Table of Contents

1. [Overview](#1-overview)
2. [Memory Layout After Boot](#2-memory-layout-after-boot)
3. [Finding GOTAB Dispatch Table](#3-finding-gotab-dispatch-table)
4. [Calculating Monitor Call Addresses](#4-calculating-monitor-call-addresses)
5. [Disassembling Monitor Call Handlers](#5-disassembling-monitor-call-handlers)
6. [Tracing Monitor Call Execution](#6-tracing-monitor-call-execution)
7. [Emulator Debug Features](#7-emulator-debug-features)
8. [Practical Examples](#8-practical-examples)

---

## 1. Overview

### 1.1 What You Need to Know

After SINTRAN boots, you need to:

1. **Locate GOTAB** - The monitor call dispatch table in memory
2. **Extract handler addresses** - For each MON call number
3. **Disassemble handlers** - Convert machine code to readable NPL/assembly
4. **Trace execution** - Follow the call from user code → INT 14 → handler → return

### 1.2 Monitor Call Mechanism (Quick Recap)

```
User Program:
  MON 1                    ; Machine code: 161001 (octal)
       ↓
CPU generates INT 14 (IIC=1)
       ↓
Level 14 handler (ENT14)
       ↓
Extract MON number: T = T AND 0377
       ↓
Lookup in GOTAB: X = GOTAB[1]
       ↓
Jump to handler: JMP ,X
       ↓
M1 (Read File handler)
       ↓
Return to user program
```

---

## 2. Memory Layout After Boot

### 2.1 SINTRAN Resident Image Location

**After bootstrap loads SINTRAN:**

```
Physical Memory (16-bit addresses):
┌──────────────────────────────────┐
│ 0x0000 - 0x00FF                  │ Interrupt vectors (256 words)
├──────────────────────────────────┤
│ 0x0100 - 0x01FF                  │ Page 0 - Monitor data
├──────────────────────────────────┤
│ 0x0200 - 0x7FFF                  │ SINTRAN Monitor Code
│   - INT14 handler (ENT14)        │ ← Around 0x3A09 (octal 072011)
│   - GOTAB table                  │ ← In MP-P2-2.NPL segment
│   - Monitor call handlers        │
│   - Device drivers               │
├──────────────────────────────────┤
│ 0x8000 - 0xFFFF                  │ User programs, RT programs
│                                  │ Swappable memory
└──────────────────────────────────┘
```

**Key Addresses (approximate - varies by version):**

| Symbol | Address (Octal) | Address (Hex) | Description |
|--------|----------------|---------------|-------------|
| ENT14 | 072011 | 0x3A09 | INT 14 entry point |
| BEG14 | 072020 | 0x3A10 | INT 14 dispatcher |
| MONCALL | 072030 | 0x3A18 | Monitor call handler |
| GOTAB | 074000 | 0x3C00 | Monitor call jump table (256 entries) |
| RET14 | 072015 | 0x3A0D | Return from INT 14 |

### 2.2 Virtual Memory Layout

**After paging is initialized:**

```
Virtual Address Space (per program):
┌──────────────────────────────────┐
│ 0x0000 - 0x0FFF (Page 0-7)       │ Ring 0 - User data
├──────────────────────────────────┤
│ 0x1000 - 0x1FFF (Page 8-15)      │ Ring 0 - User code
├──────────────────────────────────┤
│ 0x2000 - 0x7FFF                  │ Ring 1 - RT program area
├──────────────────────────────────┤
│ 0x8000 - 0xBFFF                  │ Ring 2 - Monitor accessible
├──────────────────────────────────┤
│ 0xC000 - 0xFFFF                  │ Ring 3 - Monitor kernel
│   Contains GOTAB and handlers    │
└──────────────────────────────────┘
```

**NOTE**: Addresses depend on which Page Index Table (PIT) is active!

---

## 3. Finding GOTAB Dispatch Table

### 3.1 Method 1: Known Symbol Address

**If you have symbol table from NPL compilation:**

```
From MP-P2-2.NPL symbol table:
  GOTAB = address in Monitor segment

Typical address: 0x3C00 (octal 074000) or similar
```

**Look for:**
- 256 consecutive words (512 bytes)
- Each entry is an address pointing to monitor call handler
- First entry (GOTAB[0]) usually points to MFELL (illegal call handler)

### 3.2 Method 2: Search for Pattern

**Search memory for GOTAB structure:**

```csharp
// GOTAB pattern detection
uint FindGOTAB(Memory memory)
{
    // GOTAB has 256 entries (0x100 words)
    // Entry 1 (M1) and entry 2 (M2) should be close together
    // Most entries point to MFELL (illegal handler)

    for (uint addr = 0x2000; addr < 0x8000; addr++)
    {
        // Read potential GOTAB start
        ushort entry0 = memory.ReadWord(addr);
        ushort entry1 = memory.ReadWord(addr + 2);
        ushort entry2 = memory.ReadWord(addr + 4);

        // Check if these look like valid code addresses
        if (IsValidCodeAddress(entry0) &&
            IsValidCodeAddress(entry1) &&
            IsValidCodeAddress(entry2))
        {
            // Check if many entries point to same address (MFELL)
            int mfellCount = 0;
            ushort mfellAddr = 0;

            for (int i = 0; i < 256; i++)
            {
                ushort entry = memory.ReadWord(addr + (uint)(i * 2));
                if (i == 0) mfellAddr = entry;
                if (entry == mfellAddr) mfellCount++;
            }

            // If >80% of entries are same address, likely GOTAB
            if (mfellCount > 200)
            {
                Console.WriteLine($"Found potential GOTAB at 0x{addr:X4}");
                Console.WriteLine($"  MFELL handler: 0x{mfellAddr:X4}");
                Console.WriteLine($"  M1 (MON 1): 0x{memory.ReadWord(addr + 2):X4}");
                Console.WriteLine($"  M2 (MON 2): 0x{memory.ReadWord(addr + 4):X4}");
                return addr;
            }
        }
    }

    return 0; // Not found
}

bool IsValidCodeAddress(ushort addr)
{
    // Code is typically in range 0x2000-0x8000
    return addr >= 0x2000 && addr < 0x8000;
}
```

### 3.3 Method 3: Trace MON Instruction

**Execute a MON instruction and watch where it goes:**

```csharp
// Set breakpoint on INT 14
void TraceMONCall()
{
    // 1. Set breakpoint at INT 14 entry (ENT14)
    cpu.SetBreakpoint(0x3A09, BreakpointType.Execute);

    // 2. Execute test program with MON 1
    cpu.Memory.WriteWord(0x1000, 0o161001); // MON 1
    cpu.PC = 0x1000;
    cpu.Run();

    // 3. When breakpoint hits, trace execution
    cpu.StepInto(); // ENT14
    cpu.StepInto(); // BEG14
    cpu.StepInto(); // MONCALL

    // 4. Look at X register - should be GOTAB[1]
    ushort handlerAddr = cpu.X;
    Console.WriteLine($"MON 1 handler at: 0x{handlerAddr:X4}");

    // 5. Trace backwards from GOTAB[1] = handlerAddr
    //    to find GOTAB base
    for (uint addr = 0x2000; addr < 0x8000; addr += 2)
    {
        if (cpu.Memory.ReadWord(addr + 2) == handlerAddr)
        {
            Console.WriteLine($"Potential GOTAB at: 0x{addr:X4}");
            break;
        }
    }
}
```

### 3.4 Method 4: Search for ENT14 → GOTAB Reference

**Find the GOTAB reference in ENT14 code:**

```npl
% From MP-P2-2.NPL, line 394:
MONCALL:
   X:=377; T/\X; T=:14MONNO        % Extract MON number
   X:=GOTAB(T); *2BANK; JMP ,X     % This instruction references GOTAB!
```

**Machine code pattern:**
```
072030  X:=377           ; 133377  (LDA X, #377)
072031  T/\X             ; 052000  (AND T, X)
072032  T=:14MONNO       ; 042nnn  (STA T, 14MONNO)
072033  X:=GOTAB(T)      ; 13xnnn  (LDA X, GOTAB(T))
                                    ^^^^^^ Address of GOTAB!
072034  *2BANK           ; ...
072035  JMP ,X           ; 167000  (JMP indirect via X)
```

**Search for this pattern:**

```csharp
uint FindGOTABViaCode(Memory memory)
{
    // Search for instruction sequence
    for (uint addr = 0x3000; addr < 0x5000; addr++)
    {
        ushort inst1 = memory.ReadWord(addr);
        ushort inst2 = memory.ReadWord(addr + 1);
        ushort inst3 = memory.ReadWord(addr + 2);
        ushort inst4 = memory.ReadWord(addr + 3);

        // Check for pattern:
        // X:=377 (LDA X, #377) = 0o133377
        if (inst1 == 0o133377)
        {
            // Next should be T/\X (AND T,X) = 0o052000
            if (inst2 == 0o052000)
            {
                // Next is STA T, 14MONNO
                // Then LDA X, GOTAB(T) - indexed addressing
                if ((inst4 & 0o170000) == 0o130000) // LDA X instruction
                {
                    // Extract GOTAB address from instruction
                    ushort gotabAddr = (ushort)(inst4 & 0o007777);
                    Console.WriteLine($"Found GOTAB reference at 0x{(addr+3):X4}");
                    Console.WriteLine($"GOTAB address: 0x{gotabAddr:X4}");
                    return gotabAddr;
                }
            }
        }
    }
    return 0;
}
```

---

## 4. Calculating Monitor Call Addresses

### 4.1 Direct Table Lookup

**Once you have GOTAB address:**

```csharp
class MonitorCallResolver
{
    private uint _gotabAddress;
    private Memory _memory;

    public MonitorCallResolver(Memory memory, uint gotabAddress)
    {
        _memory = memory;
        _gotabAddress = gotabAddress;
    }

    /// <summary>
    /// Get handler address for monitor call number
    /// </summary>
    public ushort GetHandlerAddress(byte monCallNumber)
    {
        if (monCallNumber > 255)
            throw new ArgumentOutOfRangeException(nameof(monCallNumber));

        uint entryAddr = _gotabAddress + (uint)(monCallNumber * 2);
        ushort handlerAddr = _memory.ReadWord(entryAddr);

        return handlerAddr;
    }

    /// <summary>
    /// Get handler name (if known)
    /// </summary>
    public string GetHandlerName(byte monCallNumber)
    {
        return monCallNumber switch
        {
            0 => "MFELL (Illegal)",
            1 => "M1 (Read File)",
            2 => "M2 (Write File)",
            17 => "M21 (File Op)",
            18 => "M22 (File Op)",
            19 => "M23 (File Op)",
            20 => "M24 (File Op)",
            51 => "M63 (Create Segment)",
            128 => "XMSGY (XMSG Communication)",
            200 => "M310 (Special Op)",
            251 => "M373 (ND-500 Operation)",
            254 => "M376 (System Function)",
            255 => "M377 (System Function)",
            _ => $"MON {monCallNumber}"
        };
    }

    /// <summary>
    /// Dump entire GOTAB table
    /// </summary>
    public void DumpGOTAB()
    {
        Console.WriteLine($"GOTAB at 0x{_gotabAddress:X4}:");
        Console.WriteLine();
        Console.WriteLine("Dec | Oct | Hex | Handler | Name");
        Console.WriteLine("----+-----+-----+---------+------------------------");

        for (int i = 0; i < 256; i++)
        {
            ushort handlerAddr = GetHandlerAddress((byte)i);
            string name = GetHandlerName((byte)i);

            Console.WriteLine($"{i,3} | {i:000} | {i:X2}  | 0x{handlerAddr:X4}  | {name}");
        }
    }

    /// <summary>
    /// Find all unique handler addresses
    /// </summary>
    public Dictionary<ushort, List<byte>> GetHandlerMap()
    {
        var map = new Dictionary<ushort, List<byte>>();

        for (byte i = 0; i < 256; i++)
        {
            ushort addr = GetHandlerAddress(i);

            if (!map.ContainsKey(addr))
                map[addr] = new List<byte>();

            map[addr].Add(i);
        }

        return map;
    }
}
```

### 4.2 Usage Example

```csharp
// After boot
var memory = emulator.CPU.Memory;

// Find GOTAB
uint gotabAddr = FindGOTAB(memory);
Console.WriteLine($"GOTAB found at: 0x{gotabAddr:X4}");

// Create resolver
var resolver = new MonitorCallResolver(memory, gotabAddr);

// Get specific handler
byte monCall = 1; // Read File
ushort handlerAddr = resolver.GetHandlerAddress(monCall);
Console.WriteLine($"MON {monCall} handler at: 0x{handlerAddr:X4}");

// Dump entire table
resolver.DumpGOTAB();

// Find unique handlers
var handlerMap = resolver.GetHandlerMap();
Console.WriteLine($"\nUnique handlers: {handlerMap.Count}");
foreach (var kvp in handlerMap)
{
    Console.WriteLine($"  0x{kvp.Key:X4}: MON {string.Join(", ", kvp.Value)}");
}
```

**Example Output:**

```
GOTAB found at: 0x3C00

MON 1 handler at: 0x4500

GOTAB at 0x3C00:

Dec | Oct | Hex | Handler | Name
----+-----+-----+---------+------------------------
  0 | 000 | 00  | 0x4000  | MFELL (Illegal)
  1 | 001 | 01  | 0x4500  | M1 (Read File)
  2 | 002 | 02  | 0x4600  | M2 (Write File)
  3 | 003 | 03  | 0x4000  | MFELL (Illegal)
...
 51 | 063 | 33  | 0x5200  | M63 (Create Segment)
...
128 | 200 | 80  | 0x6000  | XMSGY (XMSG Communication)
...

Unique handlers: 25
  0x4000: MON 0, 3, 4, 5, 6, 7, 8, 9, ... (MFELL - 200+ calls)
  0x4500: MON 1
  0x4600: MON 2
  0x4700: MON 17, 18, 19, 20
  0x5200: MON 51
  0x6000: MON 128
  ...
```

---

## 5. Disassembling Monitor Call Handlers

### 5.1 Basic Disassembler

```csharp
class ND100Disassembler
{
    private Memory _memory;
    private uint _startAddr;
    private int _maxInstructions;

    public ND100Disassembler(Memory memory)
    {
        _memory = memory;
    }

    /// <summary>
    /// Disassemble from address
    /// </summary>
    public void Disassemble(uint startAddr, int maxInstructions = 50)
    {
        _startAddr = startAddr;
        _maxInstructions = maxInstructions;

        uint addr = startAddr;
        int count = 0;

        Console.WriteLine($"Disassembly from 0x{startAddr:X4}:");
        Console.WriteLine();

        while (count < maxInstructions)
        {
            ushort instruction = _memory.ReadWord(addr);

            string disasm = DisassembleInstruction(instruction, addr);

            Console.WriteLine($"{addr:X4}  {instruction:0000}  {disasm}");

            addr++;
            count++;

            // Stop at RET or EXIT
            if (IsReturnInstruction(instruction))
            {
                Console.WriteLine("(Return from handler)");
                break;
            }
        }
    }

    private string DisassembleInstruction(ushort instruction, uint addr)
    {
        // Extract opcode (bits 15-12)
        int opcode = (instruction >> 12) & 0xF;

        switch (opcode)
        {
            case 0: // Memory reference group
                return DisassembleMemoryRef(instruction);

            case 1: // Register-register
                return DisassembleRegReg(instruction);

            case 2: // Shift/rotate
                return DisassembleShift(instruction);

            case 3: // I/O and special
                return DisassembleIO(instruction);

            case 4: // Load immediate
                return $"LDA A, #{instruction & 0xFFF:X3}";

            case 5: // Arithmetic immediate
                return DisassembleArithImm(instruction);

            case 6: // Skip group
                return DisassembleSkip(instruction);

            case 7: // Jump/call
                return DisassembleJump(instruction);

            case 8: // Monitor call
                return $"MON {instruction & 0xFF}";

            default:
                return $"??? (unknown opcode {opcode})";
        }
    }

    private string DisassembleMemoryRef(ushort instruction)
    {
        int subop = (instruction >> 9) & 0x7;
        int addr = instruction & 0x1FF;

        string[] ops = { "LDA", "STA", "ADD", "SUB", "AND", "ORA", "MIN", "LDT" };
        return $"{ops[subop]} A, [{addr:X3}]";
    }

    private string DisassembleRegReg(ushort instruction)
    {
        // Decode register-register operations
        int subop = instruction & 0xFFF;

        if (subop == 0o0000) return "NOP";
        if (subop == 0o0001) return "COPY A B";
        if (subop == 0o0002) return "COPY B A";
        // ... more register operations ...

        return $"REG {subop:000}";
    }

    private string DisassembleJump(ushort instruction)
    {
        int addr = instruction & 0xFFF;
        bool indirect = (instruction & 0x800) != 0;

        if (indirect)
            return $"JMP ,{addr:X3}  ; Indirect";
        else
            return $"JMP {addr:X3}";
    }

    private bool IsReturnInstruction(ushort instruction)
    {
        // RET, EXIT, WAIT typically end handlers
        // EXIT = octal 164000
        // WAIT = octal 164400

        return instruction == 0o164000 || // EXIT
               instruction == 0o164400 || // WAIT
               (instruction & 0o177000) == 0o167000; // JMP ,X (return via register)
    }

    // ... more disassembly methods ...
}
```

### 5.2 Disassemble with NPL Pseudocode

**Enhanced disassembler with NPL-style output:**

```csharp
string DisassembleToNPL(ushort instruction, uint addr)
{
    // Convert machine code to NPL-style pseudocode

    if ((instruction & 0o170000) == 0o160000) // MON instruction
    {
        int monNum = instruction & 0xFF;
        return $"*MON {monNum}  % Monitor call {monNum}";
    }

    if (instruction == 0o164000)
        return "EXIT  % Return to user program";

    if ((instruction & 0o177000) == 0o167000)
        return "JMP ,X  % Indirect jump via X register";

    if ((instruction & 0o170000) == 0o040000) // STA
    {
        int addr = instruction & 0x1FF;
        return $"A=:MEM[{addr:X3}]  % Store A register";
    }

    if ((instruction & 0o170000) == 0o000000) // LDA
    {
        int addr = instruction & 0x1FF;
        return $"A:=MEM[{addr:X3}]  % Load A register";
    }

    // ... more NPL-style translations ...

    return $"{instruction:0000}  % (raw)";
}
```

### 5.3 Usage Example

```csharp
// Disassemble MON 1 (Read File) handler
var disasm = new ND100Disassembler(memory);

ushort m1Addr = resolver.GetHandlerAddress(1);
Console.WriteLine($"\n=== MON 1 (Read File) Handler at 0x{m1Addr:X4} ===");
disasm.Disassemble(m1Addr, 100);
```

**Example Output:**

```
=== MON 1 (Read File) Handler at 0x4500 ===

4500  042150  STA A, 14MONNO      ; Save parameters
4501  043151  STA D, 14MONP1
4502  044152  STA X, 14MONP2
4503  010200  CALL GET0           ; Get segment context
4504  015300  CALL VALIDATE       ; Validate file descriptor
4505  167000  JMP ,X              ; Jump to file I/O routine
4506  050100  LDA A, RTREF.ACTPRI
4507  164000  EXIT                ; Return to user
(Return from handler)
```

---

## 6. Tracing Monitor Call Execution

### 6.1 Execution Tracer

```csharp
class MonitorCallTracer
{
    private CPU _cpu;
    private MonitorCallResolver _resolver;
    private List<TraceEntry> _traceLog;

    public class TraceEntry
    {
        public uint PC { get; set; }
        public ushort Instruction { get; set; }
        public string Disassembly { get; set; }
        public ushort A { get; set; }
        public ushort D { get; set; }
        public ushort X { get; set; }
        public ushort T { get; set; }
        public string Comment { get; set; }
    }

    public MonitorCallTracer(CPU cpu, MonitorCallResolver resolver)
    {
        _cpu = cpu;
        _resolver = resolver;
        _traceLog = new List<TraceEntry>();
    }

    /// <summary>
    /// Trace execution of a monitor call
    /// </summary>
    public void TraceMONCall(byte monCallNumber)
    {
        _traceLog.Clear();

        Console.WriteLine($"=== Tracing MON {monCallNumber} ===");
        Console.WriteLine();

        // Set breakpoint at handler
        ushort handlerAddr = _resolver.GetHandlerAddress(monCallNumber);
        _cpu.SetBreakpoint(handlerAddr, BreakpointType.Execute);

        // Set breakpoint at EXIT
        _cpu.SetBreakpoint(0o164000, BreakpointType.Instruction);

        // Enable single-step mode
        _cpu.SingleStepMode = true;

        // Execute until breakpoint
        _cpu.Run();

        // Trace each instruction
        while (!_cpu.IsHalted)
        {
            var entry = new TraceEntry
            {
                PC = _cpu.PC,
                Instruction = _cpu.Memory.ReadWord(_cpu.PC),
                A = _cpu.A,
                D = _cpu.D,
                X = _cpu.X,
                T = _cpu.T
            };

            // Disassemble
            var disasm = new ND100Disassembler(_cpu.Memory);
            entry.Disassembly = disasm.DisassembleInstruction(entry.Instruction, entry.PC);

            // Add comment based on context
            entry.Comment = AnalyzeInstruction(entry);

            _traceLog.Add(entry);

            // Print trace line
            Console.WriteLine(FormatTraceEntry(entry));

            // Step one instruction
            _cpu.Step();

            // Stop at EXIT or return
            if (entry.Instruction == 0o164000)
                break;
        }

        Console.WriteLine();
        Console.WriteLine($"Traced {_traceLog.Count} instructions");
    }

    private string AnalyzeInstruction(TraceEntry entry)
    {
        // Add intelligent comments

        if ((entry.Instruction & 0o170000) == 0o160000) // MON
            return "→ Monitor call";

        if (entry.Instruction == 0o164000)
            return "→ Return to user";

        if ((entry.Instruction & 0o177000) == 0o167000) // JMP ,X
            return $"→ Jump to 0x{entry.X:X4}";

        if (entry.PC == _resolver.GetHandlerAddress(1))
            return "→ MON 1 handler entry";

        return "";
    }

    private string FormatTraceEntry(TraceEntry entry)
    {
        return $"{entry.PC:X4}  {entry.Instruction:0000}  {entry.Disassembly,-30}  " +
               $"A={entry.A:X4} D={entry.D:X4} X={entry.X:X4} T={entry.T:X4}  {entry.Comment}";
    }
}
```

### 6.2 Usage Example

```csharp
// Trace MON 1 execution
var tracer = new MonitorCallTracer(cpu, resolver);
tracer.TraceMONCall(1);
```

**Example Output:**

```
=== Tracing MON 1 ===

PC    Inst  Disassembly                     A     D     X     T     Comment
-------------------------------------------------------------------------------
4500  042150  STA A, 14MONNO                1234  0080  0010  0001  → MON 1 handler entry
4501  043151  STA D, 14MONP1                1234  0080  0010  0001
4502  044152  STA X, 14MONP2                1234  0080  0010  0001
4503  010200  CALL GET0                     1234  0080  0010  0001
4600  050100  LDA A, RTREF.ACTPRI           0005  0080  0010  0001
4601  052000  AND A, 0x00FF                 0005  0080  0010  0001
4602  015300  CALL VALIDATE                 0005  0080  0010  0001
4700  040300  LDA A, FILETABLE(X)           0010  0080  0010  0001
4701  067000  TEST A                        0010  0080  0010  0001
4702  074000  SKIP IF A<>0                  0010  0080  0010  0001
4703  167000  JMP ,X                        0010  0080  5000  0001  → Jump to 0x5000
5000  ...     (file I/O routine continues)
...
5100  164000  EXIT                          0080  0000  0010  0001  → Return to user

Traced 45 instructions
```

---

## 7. Emulator Debug Features

### 7.1 Monitor Call Hook System

**Add hooks to intercept monitor calls:**

```csharp
class MonitorCallHookSystem
{
    private CPU _cpu;
    private MonitorCallResolver _resolver;
    private Dictionary<byte, Action<MonitorCallContext>> _hooks;

    public class MonitorCallContext
    {
        public byte CallNumber { get; set; }
        public ushort A { get; set; }
        public ushort D { get; set; }
        public ushort X { get; set; }
        public ushort PC { get; set; }
        public bool Intercepted { get; set; }
        public ushort ReturnA { get; set; }
        public ushort ReturnD { get; set; }
    }

    public MonitorCallHookSystem(CPU cpu, MonitorCallResolver resolver)
    {
        _cpu = cpu;
        _resolver = resolver;
        _hooks = new Dictionary<byte, Action<MonitorCallContext>>();

        // Install INT 14 interceptor
        _cpu.OnInterrupt += HandleInterrupt;
    }

    /// <summary>
    /// Register a hook for specific monitor call
    /// </summary>
    public void RegisterHook(byte monCallNumber, Action<MonitorCallContext> hook)
    {
        _hooks[monCallNumber] = hook;
    }

    private void HandleInterrupt(int level, int interruptCode)
    {
        // Only handle INT 14 (monitor calls)
        if (level != 14 || interruptCode != 1)
            return;

        // Extract MON call number from instruction
        ushort instruction = _cpu.Memory.ReadWord(_cpu.PC - 1); // Previous instruction
        if ((instruction & 0o170000) != 0o160000) // Not MON instruction
            return;

        byte monCallNumber = (byte)(instruction & 0xFF);

        // Check if we have a hook for this call
        if (!_hooks.ContainsKey(monCallNumber))
            return;

        // Create context
        var context = new MonitorCallContext
        {
            CallNumber = monCallNumber,
            A = _cpu.A,
            D = _cpu.D,
            X = _cpu.X,
            PC = _cpu.PC,
            Intercepted = false
        };

        // Invoke hook
        _hooks[monCallNumber](context);

        // If hook intercepted, skip normal handler
        if (context.Intercepted)
        {
            _cpu.A = context.ReturnA;
            _cpu.D = context.ReturnD;
            _cpu.PC = context.PC; // Return to user code
            _cpu.SkipInterruptHandler = true;
        }
    }
}
```

### 7.2 Example Hooks

```csharp
// Install hooks
var hookSystem = new MonitorCallHookSystem(cpu, resolver);

// Hook MON 1 (Read File) to log all file reads
hookSystem.RegisterHook(1, ctx =>
{
    Console.WriteLine($"MON 1 (Read File): FD={ctx.X:X4}, " +
                     $"Buffer=0x{ctx.A:X4}, Count={ctx.D}");
});

// Hook MON 2 (Write File) to intercept terminal output
hookSystem.RegisterHook(2, ctx =>
{
    if (ctx.X == 1) // Terminal
    {
        // Read string from buffer
        string text = ReadString(cpu.Memory, ctx.A, ctx.D);
        Console.WriteLine($"[Terminal Output]: {text}");

        // Simulate success
        ctx.ReturnA = ctx.D; // Bytes written
        ctx.ReturnD = 0;     // No error
        ctx.Intercepted = true; // Skip real handler
    }
});

// Hook MON 128 (XMSG) to trace ND-500 communication
hookSystem.RegisterHook(128, ctx =>
{
    Console.WriteLine($"MON 128 (XMSG): Function={ctx.A:X4}, " +
                     $"Param={ctx.D:X4}");
    // Let it continue to real handler
});
```

### 7.3 Symbol Table Integration

**Load symbol table for better debugging:**

```csharp
class SymbolTable
{
    private Dictionary<ushort, string> _addressToSymbol;
    private Dictionary<string, ushort> _symbolToAddress;

    public void LoadFromFile(string filename)
    {
        _addressToSymbol = new Dictionary<ushort, string>();
        _symbolToAddress = new Dictionary<string, ushort>();

        // Parse symbol file (format depends on your compiler)
        foreach (var line in File.ReadLines(filename))
        {
            // Example format: "SYMBOL_NAME  0x1234"
            var parts = line.Split(new[] { ' ', '\t' },
                                   StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length >= 2)
            {
                string symbol = parts[0];
                ushort address = Convert.ToUInt16(parts[1], 16);

                _addressToSymbol[address] = symbol;
                _symbolToAddress[symbol] = address;
            }
        }
    }

    public string GetSymbol(ushort address)
    {
        if (_addressToSymbol.TryGetValue(address, out string symbol))
            return symbol;
        return $"0x{address:X4}";
    }

    public ushort? GetAddress(string symbol)
    {
        if (_symbolToAddress.TryGetValue(symbol, out ushort address))
            return address;
        return null;
    }
}

// Usage
var symbols = new SymbolTable();
symbols.LoadFromFile("sintran.sym");

// Enhanced disassembly with symbols
string DisassembleWithSymbols(ushort instruction, ushort addr)
{
    string basicDisasm = DisassembleInstruction(instruction, addr);

    // Replace numeric addresses with symbols
    if ((instruction & 0o170000) == 0o040000) // STA
    {
        ushort targetAddr = (ushort)(instruction & 0x1FF);
        string symbol = symbols.GetSymbol(targetAddr);
        return $"STA A, {symbol}";
    }

    return basicDisasm;
}
```

---

## 8. Practical Examples

### 8.1 Complete Debug Session

```csharp
// 1. Boot SINTRAN
var emulator = new ND100Emulator();
emulator.LoadSINTRAN("sintran.img");
emulator.Boot();

Console.WriteLine("SINTRAN booted successfully");

// 2. Find GOTAB
uint gotabAddr = FindGOTAB(emulator.CPU.Memory);
Console.WriteLine($"GOTAB at: 0x{gotabAddr:X4}");

// 3. Create resolver
var resolver = new MonitorCallResolver(emulator.CPU.Memory, gotabAddr);
resolver.DumpGOTAB();

// 4. Load symbols
var symbols = new SymbolTable();
symbols.LoadFromFile("sintran.sym");

// 5. Disassemble key handlers
var disasm = new ND100Disassembler(emulator.CPU.Memory);

Console.WriteLine("\n=== MON 1 (Read File) ===");
disasm.Disassemble(resolver.GetHandlerAddress(1), 50);

Console.WriteLine("\n=== MON 2 (Write File) ===");
disasm.Disassemble(resolver.GetHandlerAddress(2), 50);

Console.WriteLine("\n=== MON 128 (XMSG) ===");
disasm.Disassemble(resolver.GetHandlerAddress(128), 50);

// 6. Install hooks
var hookSystem = new MonitorCallHookSystem(emulator.CPU, resolver);

hookSystem.RegisterHook(1, ctx =>
{
    Console.WriteLine($"[MON 1] Read: FD={ctx.X:X4}, " +
                     $"Buffer=0x{ctx.A:X4}, Count={ctx.D}");
});

hookSystem.RegisterHook(2, ctx =>
{
    Console.WriteLine($"[MON 2] Write: FD={ctx.X:X4}, " +
                     $"Buffer=0x{ctx.A:X4}, Count={ctx.D}");
});

// 7. Run test program
emulator.LoadProgram("test.abs");
emulator.Run();

// 8. Trace specific monitor call
var tracer = new MonitorCallTracer(emulator.CPU, resolver);
tracer.TraceMONCall(1);
```

### 8.2 Automated Monitor Call Analysis

```csharp
// Analyze all monitor call handlers
void AnalyzeAllHandlers()
{
    var resolver = new MonitorCallResolver(memory, gotabAddr);
    var handlerMap = resolver.GetHandlerMap();

    Console.WriteLine("=== Monitor Call Handler Analysis ===\n");

    foreach (var kvp in handlerMap.OrderBy(x => x.Key))
    {
        ushort addr = kvp.Key;
        var calls = kvp.Value;

        Console.WriteLine($"\nHandler at 0x{addr:X4}:");
        Console.WriteLine($"  Used by: MON {string.Join(", ", calls)}");

        // Analyze first few instructions
        var disasm = new ND100Disassembler(memory);
        Console.WriteLine("  First instructions:");
        disasm.Disassemble(addr, 10);

        // Check for common patterns
        ushort inst1 = memory.ReadWord(addr);
        ushort inst2 = memory.ReadWord((uint)(addr + 1));

        if (inst1 == 0o042150 && inst2 == 0o043151)
            Console.WriteLine("  Pattern: Standard parameter save");

        if ((inst1 & 0o177000) == 0o010000)
            Console.WriteLine("  Pattern: Starts with CALL");

        if ((inst1 & 0o177000) == 0o167000)
            Console.WriteLine("  Pattern: Immediate JMP (trampoline)");
    }
}
```

---

## Summary

### Key Steps to Debug Monitor Calls:

1. **Find GOTAB** after boot (search for 256-entry table pattern)
2. **Extract handler addresses** from GOTAB[0-255]
3. **Disassemble handlers** starting at each handler address
4. **Install hooks** to intercept and log monitor calls
5. **Trace execution** step-by-step through handlers
6. **Load symbols** for readable disassembly
7. **Analyze patterns** to understand handler functionality

### Tools You Need:

- ✅ GOTAB finder (search memory for table pattern)
- ✅ Monitor call resolver (GOTAB[n] → handler address)
- ✅ ND-100 disassembler (machine code → assembly)
- ✅ Execution tracer (step-by-step instruction trace)
- ✅ Hook system (intercept monitor calls)
- ✅ Symbol table loader (addresses → names)

### Memory Addresses to Remember:

| Item | Typical Address | How to Find |
|------|----------------|-------------|
| ENT14 | 0x3A09 (072011 octal) | Search for INT 14 vector |
| GOTAB | 0x3C00-0x4000 range | Search for 256-entry table |
| MFELL | Most common entry in GOTAB | Count GOTAB entries |
| M1 | GOTAB[1] | Direct lookup |
| M2 | GOTAB[2] | Direct lookup |

---

**You now have complete tools to debug monitor calls in your SINTRAN emulator!**
