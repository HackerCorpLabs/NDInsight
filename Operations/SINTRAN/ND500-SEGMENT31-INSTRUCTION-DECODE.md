# ND-500 Segment 31 Instruction Decode and Bus Communication

## Exact Mechanism: CALLG to Segment 37₈ (31₁₀) with O-bit

**How the "Other Machine Tag" (O-bit) Triggers ND-500 → ND-100 Communication**

---

## Table of Contents

1. [Overview](#overview)
2. [CALLG Instruction Format](#callg-instruction-format)
3. [Instruction Decode Sequence](#instruction-decode-sequence)
4. [Capability Register Structure](#capability-register-structure)
5. [O-Bit Detection and Trap](#o-bit-detection-and-trap)
6. [Trap Handler to Bus Interface](#trap-handler-to-bus-interface)
7. [Bus Interface Communication](#bus-interface-communication)
8. [Complete Example with Binary Details](#complete-example-with-binary-details)
9. [Source Code Implementation](#source-code-implementation)

---

## Overview

The **segment 31 mechanism** is the core of ND-500 → ND-100 communication. When ND-500 code executes:

```assembly
CALLG   R0, #0x1F000000    ; Segment 37₈ (31₁₀), offset 0
```

The CPU **detects the O-bit** in capability register 31 and **triggers a trap** instead of executing the call. This trap initiates bus communication with ND-100.

### The Key Insight

**Segment 31 is never actually executed.** It's a "phantom segment" that exists only to trigger traps via the O-bit (Other CPU bit) in its capability register.

---

## CALLG Instruction Format

### ND-500 Instruction Encoding

The CALLG (Call with Gate) instruction has the following format:

```
┌─────────────┬─────────────┬─────────────────────────────┐
│   Opcode    │   Register  │      Target Address         │
│   (8 bits)  │   (4 bits)  │      (32 bits)              │
└─────────────┴─────────────┴─────────────────────────────┘
```

**Total instruction size:** 48 bits (6 bytes) or 32 bits (4 bytes) depending on encoding

### Target Address Format (32 bits)

The target address encodes both segment and offset:

```
Target Address = 0x1F000000

Binary breakdown:
  31  30  29  28  27  26  25  24  23  22  21  20  19  18...  0
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───...─┬───┐
│ 0 │ 0 │ 0 │ 1 │ 1 │ 1 │ 1 │ 1 │ 0 │ 0 │ 0 │ 0 │ 0 │ 0 ... 0 │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───...─┴───┘
  │                       │                   │                 │
  └─ Reserved (0)         └─ Segment (5 bits) └─ Offset (24 bits)
                             = 11111₂ = 31₁₀
                             = 37₈
```

**Key fields:**
- **Bits 31-29**: Reserved (0)
- **Bits 28-24**: Segment number (5 bits) = `0x1F` = 31₁₀ = 37₈
- **Bits 23-0**: Offset within segment (24 bits) = `0x000000`

### Assembly Syntax

```assembly
CALLG   R0, #0x1F000000    ; Call segment 31, offset 0
```

**Breaking down the operands:**
- `R0`: Link register (where return address is stored)
- `#0x1F000000`: Immediate target address
  - Segment: `0x1F` (bits 28-24)
  - Offset: `0x000000` (bits 23-0)

---

## Instruction Decode Sequence

### Step 1: Fetch Instruction

**ND-500 CPU Fetch Stage:**

```c
// CPU instruction fetch (cycle 1)
uint32_t instruction_word = FetchFromMemory(PC);

// Decode opcode
uint8_t opcode = (instruction_word >> 24) & 0xFF;

// Check if CALLG instruction
if (opcode == OPCODE_CALLG) {
    // Proceed to CALLG decode
}
```

### Step 2: Extract Target Address

**Decode Stage (cycle 2):**

```c
// Extract target address from CALLG instruction
// For immediate addressing, target is encoded in instruction
uint32_t target_address = instruction_word & 0xFFFFFFFF;

// For our example:
// target_address = 0x1F000000
```

### Step 3: Extract Segment Number

**Segment Extraction (cycle 3):**

```c
// Extract segment number from bits 28-24
uint8_t target_segment = (target_address >> 24) & 0x1F;

// Binary operation:
// 0x1F000000 >> 24 = 0x0000001F
// 0x0000001F & 0x1F = 0x1F (31 decimal)

printf("Target segment: %d (0x%02X, octal %o)\n",
       target_segment, target_segment, target_segment);
// Output: Target segment: 31 (0x1F, octal 37)
```

**Why mask with 0x1F?**
- ND-500 has 32 segments (0-31)
- 5 bits can represent 0-31 (2⁵ = 32)
- Mask ensures only valid segment numbers

### Step 4: Look Up Capability Register

**Capability Register Access (cycle 4):**

```c
// ND-500 CPU has 32 program capability registers
// Each is 16 bits
uint16_t capability = ProgramCapabilities[target_segment];

// For segment 31:
// capability = ProgramCapabilities[31];
// capability = 0xC000 (set during PLACE-DOMAIN)

printf("Segment %d capability: 0x%04X\n", target_segment, capability);
// Output: Segment 31 capability: 0xC000
```

**Where are capability registers stored?**
- In ND-500 CPU register file (fast access)
- Also in process descriptor in 5MPM (persistent storage)
- Loaded during process activation

### Step 5: Check O-Bit (Other CPU Bit)

**Capability Bit Check (cycle 5):**

```c
// Capability register format (16 bits):
// Bit 15: I (Indirect translation)
// Bit 14: O (Other CPU trap)  ← THIS IS THE KEY BIT
// Bit 13-12: Ring level
// Bit 11: X (Execute permission)
// Bit 10: R (Read permission)
// Bit 9: W (Write permission)
// Bits 8-0: Other flags

bool I_bit = (capability & 0x8000) != 0;  // Bit 15
bool O_bit = (capability & 0x4000) != 0;  // Bit 14  ← CHECK THIS

printf("I-bit (Indirect): %d\n", I_bit);
printf("O-bit (Other CPU): %d\n", O_bit);

// For segment 31 with capability 0xC000:
// Binary: 1100 0000 0000 0000
//         ||
//         |└─ Bit 14 (O-bit) = 1  ← OTHER CPU TRAP!
//         └── Bit 15 (I-bit) = 1  ← INDIRECT

// Output:
// I-bit (Indirect): 1
// O-bit (Other CPU): 1  ← TRAP WILL BE TRIGGERED!
```

### Step 6: Trap Decision

**Trap vs Normal Call (cycle 6):**

```c
if (target_segment == 31 && O_bit) {
    // TRAP TO OTHER CPU!
    printf("[CPU] Segment 31 O-bit detected - TRAP!\n");

    // DO NOT execute normal CALLG
    // Instead, invoke trap handler
    TrapHandler_OtherCPU(target_address);

    return;  // Skip normal instruction execution
}

// Normal CALLG execution (not reached for segment 31)
ExecuteNormalCALLG(target_address);
```

**What would happen if O-bit was NOT set?**
- CPU would try to execute code at segment 31
- But segment 31 typically has no actual code loaded
- Would result in page fault or invalid instruction

**Why both I-bit and O-bit must be set?**
- I-bit = 1: Use indirect addressing (standard for all segments)
- O-bit = 1: Trigger "Other CPU" trap (special for segment 31)
- Both required: Safety check to ensure intentional trap

---

## Capability Register Structure

### 16-Bit Capability Register Format

```
Capability Register (16 bits):

Bit:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
     ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
     │ I │ O │Ring L│ X │ R │ W │ M │ U │   Protection Flags (8 bits)  │
     └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
       │   │   │   │   │   │   │   │
       │   │   │   │   │   │   │   └─ User flag
       │   │   │   │   │   │   └───── Modified flag
       │   │   │   │   │   └───────── Write permission
       │   │   │   │   └───────────── Read permission
       │   │   │   └───────────────── Execute permission
       │   │   └───────────────────── Ring level (2 bits: 00=user, 01=shared, 10=system)
       │   └───────────────────────── Other CPU bit (TRIGGERS TRAP!)
       └───────────────────────────── Indirect translation
```

### Segment 31 Capability Value

```c
// Segment 31 is set up during PLACE-DOMAIN:

uint16_t seg31_capability = 0xC000;

// Binary: 1100 0000 0000 0000
//         ││
//         │└─ Bit 14 = 1 (O-bit SET)
//         └── Bit 15 = 1 (I-bit SET)

// All other bits = 0 (no read/write/execute permissions)
// This makes sense: segment 31 is never actually accessed for memory!
```

### Why Segment 31?

**Historical/architectural reasons:**
- Segment numbers 0-30: Normal program/data segments
- Segment 31 (highest segment): Reserved for special purposes
- Using highest segment number makes it easy to detect
- Octal 37 is memorable and distinct

### Setup During Process Loading

**Code from PLACE-DOMAIN:**

```c
void InitializeProcessCapabilities(uint8_t process_num)
{
    uint32_t descriptor_addr = 0x80000000 + (process_num * 512);

    // Initialize program capabilities for segments 0-30
    for (int seg = 0; seg < 31; seg++) {
        uint16_t capability = 0x8C00;  // I=1, Execute=1, Read=1
        WriteWord(descriptor_addr + (seg * 2), capability);
    }

    // Segment 31: SPECIAL - Set O-bit to trigger trap
    uint16_t seg31_cap = 0xC000;  // I=1, O=1
    WriteWord(descriptor_addr + (31 * 2), seg31_cap);

    printf("Process %d: Segment 31 capability = 0x%04X\n",
           process_num, seg31_cap);
}
```

---

## O-Bit Detection and Trap

### Trap Condition Check

**Complete trap detection logic:**

```c
// ND-500 CPU instruction execution stage

void ExecuteCALLG(uint32_t instruction)
{
    // 1. Extract target address
    uint32_t target_address = ExtractCALLGTarget(instruction);

    // 2. Extract segment number
    uint8_t segment = (target_address >> 24) & 0x1F;

    // 3. Load capability register
    uint16_t capability = CPU.ProgramCapabilities[segment];

    // 4. Extract capability bits
    bool I_bit = (capability & 0x8000) != 0;
    bool O_bit = (capability & 0x4000) != 0;

    // 5. CHECK FOR TRAP CONDITION
    if (segment == 31 && O_bit && I_bit) {
        // ========================================
        // TRAP TO OTHER CPU (ND-100)!
        // ========================================

        printf("[ND500-CPU] TRAP: Segment 31 O-bit trap triggered\n");
        printf("[ND500-CPU] PC=0x%08X, Target=0x%08X\n",
               CPU.PC, target_address);

        // Save return address in link register
        CPU.LinkRegister = CPU.PC + 4;  // Next instruction

        // Invoke trap handler
        TrapHandler_HandleOtherCPU(target_address);

        // DO NOT advance PC (trap handler will handle this)
        return;
    }

    // 6. Normal CALLG execution (for other segments)
    ExecuteNormalCall(target_address, capability);
}
```

### Trap Handler Entry

**TrapHandler_HandleOtherCPU function:**

```c
void TrapHandler_HandleOtherCPU(uint32_t target_address)
{
    uint8_t process_num = CPU.CurrentProcess;

    printf("[ND500-TRAP] Process %d entered trap handler\n", process_num);
    printf("[ND500-TRAP] Target address: 0x%08X\n", target_address);

    // STEP 1: Calculate message buffer address
    uint32_t msg_base = 0x80000400 + (process_num * 0x100);

    // STEP 2: Save complete CPU state to message buffer
    SaveCPUState(msg_base + 64);  // Offset +64 for CPU state

    // STEP 3: Read MICFU code (already written by library)
    uint16_t micfu = ReadWord(msg_base + 6);
    printf("[ND500-TRAP] MICFU code: 0x%04X\n", micfu);

    // STEP 4: Set ITMQUEUE flag
    uint16_t flags = ReadWord(msg_base + 2);
    flags |= 0x0001;  // Set bit 0
    WriteWord(msg_base + 2, flags);

    // STEP 5: Signal ND-100 via bus interface
    SignalND100_MonitorCall(process_num, msg_base);

    // STEP 6: Block process
    CPU.IsWaiting = true;
    CPU.CurrentProcess = 0xFF;  // No process running

    printf("[ND500-TRAP] Process %d blocked, waiting for ND-100\n",
           process_num);
}
```

---

## Trap Handler to Bus Interface

### Message Buffer Preparation

**Save CPU State:**

```c
void SaveCPUState(uint32_t save_area)
{
    // Save at message buffer offset +64 (64 bytes for CPU state)

    WriteDoubleWord(save_area + 0,  CPU.PC);      // Program counter
    WriteDoubleWord(save_area + 4,  CPU.STATUS);  // Status register
    WriteDoubleWord(save_area + 8,  CPU.R0);      // Register 0
    WriteDoubleWord(save_area + 12, CPU.R1);      // Register 1
    WriteDoubleWord(save_area + 16, CPU.R2);      // Register 2
    WriteDoubleWord(save_area + 20, CPU.R3);      // Register 3
    WriteDoubleWord(save_area + 24, CPU.R4);      // Register 4
    WriteDoubleWord(save_area + 28, CPU.R5);      // Register 5
    WriteDoubleWord(save_area + 32, CPU.R6);      // Register 6
    WriteDoubleWord(save_area + 36, CPU.R7);      // Register 7
    WriteDoubleWord(save_area + 40, CPU.A);       // Accumulator
    WriteDoubleWord(save_area + 44, CPU.Q);       // Quotient
    WriteDoubleWord(save_area + 48, CPU.D);       // Double word
    WriteDoubleWord(save_area + 52, CPU.L);       // Link
    WriteDoubleWord(save_area + 56, CPU.B);       // Base
    WriteDoubleWord(save_area + 60, CPU.TOS);     // Top of stack

    // Total: 16 registers × 4 bytes = 64 bytes
}
```

### Signal ND-100

**Call to bus interface:**

```c
void SignalND100_MonitorCall(uint8_t process_num, uint32_t msg_addr)
{
    printf("[ND500-TRAP] Signaling ND-100 via 5015 interface\n");

    // Call 5015 interface to send TAG
    Interface5015_WriteTAG(process_num, TAG_MONITOR_CALL_REQUEST);
}
```

---

## Bus Interface Communication

### 5015 Interface (ND-500 Side)

**TAG Register Write:**

```c
// 5015 interface implementation

#define INTERFACE_BASE_5015  0xFFFF0000  // Memory-mapped registers

#define OFFSET_CONTROL       0x00  // Control register
#define OFFSET_STATUS        0x02  // Status register
#define OFFSET_TAG_OUT       0x08  // TAG-OUT register (to ND-100)
#define OFFSET_TAG_IN        0x0A  // TAG-IN register (from ND-100)

void Interface5015_WriteTAG(uint8_t process_num, uint8_t tag_code)
{
    // Construct TAG value
    // High byte: TAG code (0x01 = MONITOR_CALL_REQUEST)
    // Low byte: Process number
    uint16_t tag_value = (tag_code << 8) | process_num;

    printf("[5015] Writing TAG-OUT: 0x%04X\n", tag_value);
    printf("[5015] TAG code: 0x%02X, Process: %d\n", tag_code, process_num);

    // Write to TAG-OUT register (hardware register)
    volatile uint16_t* tag_out_reg =
        (uint16_t*)(INTERFACE_BASE_5015 + OFFSET_TAG_OUT);

    *tag_out_reg = tag_value;

    // Hardware automatically:
    // 1. Latches the TAG value
    // 2. Asserts TAG signal line to 3022
    // 3. Sets TAG_PENDING bit in status register

    printf("[5015] TAG signal sent to 3022 interface\n");
}
```

### Hardware Signal Propagation

**Physical bus communication:**

```
5015 Interface                3022 Interface
(ND-500 Side)                 (ND-100 Side)

┌─────────────┐              ┌─────────────┐
│ TAG-OUT Reg │              │ TAG-IN Reg  │
│   0x0105    │              │             │
└──────┬──────┘              └──────▲──────┘
       │                             │
       │  ┌────────────────────┐     │
       └─►│  TAG Signal Line   │─────┘
          │  (Physical Wire)   │
          │  Propagation: 5-10ns│
          └────────────────────┘

       │                             │
       │  ┌────────────────────┐     │
       └─►│  Interrupt Request │─────┘
          │  Line (Level 12)   │
          └────────────────────┘
```

**Hardware sequence:**
1. ND-500 writes TAG-OUT register (0x0105)
2. 5015 asserts TAG signal line (physical wire goes high)
3. Signal propagates across backplane (5-10 nanoseconds)
4. 3022 detects signal (edge-triggered or level-triggered)
5. 3022 latches TAG value into TAG-IN register
6. 3022 asserts interrupt request line for Level 12
7. ND-100 CPU detects interrupt request

### 3022 Interface (ND-100 Side)

**Interrupt Handling:**

```c
// ND-100 Interrupt Level 12 handler

void ND100_InterruptLevel12_Handler(void)
{
    printf("[ND100-INT12] Interrupt Level 12 triggered\n");

    // Read TAG-IN register via IOX instruction
    uint16_t tag_value = IOX_Read(DEVICE_ND500, OFFSET_RTAG5);

    // Extract fields
    uint8_t tag_code = (tag_value >> 8) & 0xFF;
    uint8_t process_num = tag_value & 0xFF;

    printf("[ND100-INT12] TAG: 0x%04X\n", tag_value);
    printf("[ND100-INT12] TAG code: 0x%02X, Process: %d\n",
           tag_code, process_num);

    // Dispatch based on TAG code
    switch (tag_code) {
        case 0x01:  // MONITOR_CALL_REQUEST
            HandleMonitorCallRequest(process_num);
            break;

        case 0x03:  // PAGE_FAULT
            HandlePageFault(process_num);
            break;

        default:
            printf("[ND100-INT12] Unknown TAG code: 0x%02X\n", tag_code);
            break;
    }
}
```

**IOX Instruction:**

```assembly
; ND-100 assembly to read TAG register

        LDA     T, HDEV          ; Load device base address (660 octal)
        AD      T, #8            ; Add offset for RTAG5 (10 octal = 8 decimal)
        IOXT                     ; Execute IOX (Input/Output Transfer)
                                 ; Result in A register

        ; A now contains TAG value (e.g., 0x0105)
```

### ND-100 Response (TAG Completion)

**After processing monitor call:**

```c
void ND100_SendCompletionTAG(uint8_t process_num)
{
    // Construct completion TAG
    uint16_t tag_value = (0x02 << 8) | process_num;  // 0x02 = OPERATION_COMPLETE

    printf("[ND100] Sending completion TAG: 0x%04X\n", tag_value);

    // Write to TAG-OUT register via IOX
    IOX_Write(DEVICE_ND500, OFFSET_LTAG5, tag_value);

    // This triggers signal back to ND-500
}
```

**IOX Instruction (Write):**

```assembly
; ND-100 assembly to write TAG register

        LDA     A, #0x0205       ; TAG value (OPERATION_COMPLETE, Process 5)
        LDA     T, HDEV          ; Device base
        AD      T, #9            ; Add offset for LTAG5 (11 octal = 9 decimal)
        IOXT                     ; Execute IOX write
```

---

## Complete Example with Binary Details

### Full Execution Trace

**Step-by-step with all binary values:**

#### T+0 μs: ND-500 Executes CALLG

```
Instruction at PC=0x01000230:

Machine code: 0xE4 0x1F 0x00 0x00 0x00
             │    │              │
             │    └─ Segment     └─ Offset
             └─ Opcode (CALLG)

Decoded:
  Opcode:  0xE4 (CALLG instruction)
  Segment: 0x1F (31 decimal, 37 octal)
  Offset:  0x000000
  Target:  0x1F000000
```

#### T+1 μs: Extract Segment Number

```c
target_address = 0x1F000000

Binary: 0001 1111 0000 0000 0000 0000 0000 0000

Shift right 24 bits:
        0000 0000 0000 0000 0000 0000 0001 1111

Mask with 0x1F:
        0000 0000 0000 0000 0000 0000 0001 1111
AND     0000 0000 0000 0000 0000 0000 0001 1111
        ─────────────────────────────────────────
Result: 0000 0000 0000 0000 0000 0000 0001 1111 = 0x1F = 31

segment = 31
```

#### T+2 μs: Load Capability Register

```c
capability = ProgramCapabilities[31];

Binary: 1100 0000 0000 0000 = 0xC000

Bit analysis:
  Bit 15: 1 (I-bit: Indirect translation)
  Bit 14: 1 (O-bit: Other CPU trap)  ← KEY BIT!
  Bits 13-0: 0 (no other permissions)
```

#### T+3 μs: Check O-Bit

```c
O_bit = (capability & 0x4000) != 0

Binary AND:
        1100 0000 0000 0000  (capability = 0xC000)
AND     0100 0000 0000 0000  (mask = 0x4000)
        ─────────────────────
Result: 0100 0000 0000 0000 = 0x4000

0x4000 != 0 → TRUE

O_bit = TRUE  ← TRAP WILL BE TRIGGERED!
```

#### T+4 μs: Trigger Trap

```c
if (segment == 31 && O_bit) {
    // TRAP!
    TrapHandler_HandleOtherCPU(0x1F000000);
}
```

#### T+5-7 μs: Save CPU State

```
Message Buffer Address: 0x80000900 (Process 5)

CPU State Save (64 bytes starting at offset +64):

Offset  Value        Description
+64     0x01000234   PC (next instruction after CALLG)
+68     0x00000000   STATUS
+72     0x00000001   R0 (fd parameter)
+76     0x01000100   R1 (buffer address)
+80     0x0000000E   R2 (count = 14 bytes)
+84     0x00000000   R3
...     ...          (more registers)
+124    0x01001FFC   TOS (top of stack)
```

#### T+8 μs: Set ITMQUEUE Flag

```
Message Buffer Flags (offset +2):

Before: 0000 0000 0000 0000 = 0x0000
OR      0000 0000 0000 0001 = 0x0001  (ITMQUEUE bit)
        ─────────────────────
After:  0000 0000 0000 0001 = 0x0001
```

#### T+9 μs: Write TAG Register

```
TAG Value Construction:

TAG code:     0x01 (MONITOR_CALL_REQUEST)
Process num:  0x05

TAG value = (tag_code << 8) | process_num
          = (0x01 << 8) | 0x05
          = 0x0100 | 0x0005
          = 0x0105

Binary: 0000 0001 0000 0101

Write to 5015.TAG_OUT = 0x0105
```

#### T+10 μs: Hardware Signal

```
5015 Hardware Action:

1. Latch TAG value: 0x0105
2. Assert TAG signal line (goes HIGH)
3. Signal propagates (5-10 nanoseconds)
4. 3022 detects edge
5. 3022 latches value into TAG-IN: 0x0105
6. 3022 asserts INT12 request line
```

#### T+15 μs: ND-100 Reads TAG

```
ND-100 IOX Instruction:

Device: 660₈ (432₁₀)
Offset: 10₈ (8₁₀) = RTAG5

Read value: 0x0105

Extract fields:
  tag_code = (0x0105 >> 8) & 0xFF = 0x01
  process_num = 0x0105 & 0xFF = 0x05

TAG code 0x01 = MONITOR_CALL_REQUEST
Process number = 5
```

#### T+1.24 ms: ND-100 Sends Completion

```
After processing monitor call:

TAG Value Construction:

TAG code:     0x02 (OPERATION_COMPLETE)
Process num:  0x05

TAG value = 0x0205

ND-100 IOX Write:
  Device: 660₈
  Offset: 11₈ (9₁₀) = LTAG5
  Value: 0x0205

Hardware propagates signal back to ND-500
```

#### T+1.26 μs: ND-500 Resumes

```
5015 receives TAG: 0x0205

Extract:
  tag_code = 0x02 (OPERATION_COMPLETE)
  process_num = 0x05

Trap handler resumes process 5:
  1. Read CPU state from message buffer offset +64
  2. Restore all registers
  3. Restore PC = 0x01000234
  4. Clear IsWaiting flag
  5. Resume execution at PC
```

---

## Source Code Implementation

### Complete Implementation Files

**From SINTRAN Emulator:**

#### 1. ND500-CPU-INSTRUCTION-DECODE.cs

```csharp
public class ND500CPU
{
    // Capability registers (32 program + 32 data)
    public ushort[] ProgramCapabilities = new ushort[32];
    public ushort[] DataCapabilities = new ushort[32];

    public void ExecuteInstruction()
    {
        // Fetch instruction
        uint instruction = FetchFromMemory(PC);

        // Decode opcode
        byte opcode = (byte)((instruction >> 24) & 0xFF);

        if (opcode == OPCODE_CALLG)
        {
            // Extract target address
            uint targetAddress = instruction & 0xFFFFFFFF;

            // EXTRACT SEGMENT NUMBER (bits 28-24)
            byte segment = (byte)((targetAddress >> 24) & 0x1F);

            Console.WriteLine($"[ND500-CPU] CALLG to segment {segment} (0x{segment:X2})");

            // CHECK IF SEGMENT 31
            if (segment == 0x1F)  // 31 decimal = 0x1F hex = 37 octal
            {
                // LOAD CAPABILITY REGISTER
                ushort progCap = ProgramCapabilities[31];

                Console.WriteLine($"[ND500-CPU] Segment 31 capability: 0x{progCap:X4}");

                // CHECK O-BIT (bit 14)
                bool O_bit = (progCap & 0x4000) != 0;

                if (O_bit)
                {
                    Console.WriteLine($"[ND500-CPU] O-bit detected - TRAP to Other CPU!");

                    // TRIGGER TRAP
                    _trapHandler.HandleOtherCPUTrap(this, targetAddress);

                    // DO NOT execute normal CALLG
                    return;
                }
            }

            // Normal CALLG execution for other segments
            ExecuteNormalCALLG(targetAddress);
        }
    }
}
```

#### 2. ND500-SEGMENT31-TRAP-HANDLER.cs

```csharp
public class ND500TrapHandler
{
    public void HandleOtherCPUTrap(ND500CPU cpu, uint targetAddress)
    {
        byte processNum = cpu.CurrentProcess;

        Console.WriteLine($"[ND500-TRAP] Process {processNum} - Segment 31 trap");
        Console.WriteLine($"[ND500-TRAP] Target address: 0x{targetAddress:X8}");

        // Calculate message buffer address
        uint msgBase = 0x80000400 + (uint)(processNum * 0x100);

        // SAVE CPU STATE (64 bytes at offset +64)
        SaveCPUState(cpu, msgBase + 64);

        // READ MICFU CODE (written by library before CALLG)
        ushort micfu = _mpm.ReadWord(msgBase + 6);
        Console.WriteLine($"[ND500-TRAP] MICFU: 0x{micfu:X4}");

        // SET ITMQUEUE FLAG
        ushort flags = _mpm.ReadWord(msgBase + 2);
        flags |= 0x0001;  // Set bit 0
        _mpm.WriteWord(msgBase + 2, flags);

        // SIGNAL ND-100 VIA BUS INTERFACE
        _interface.SignalND100MonitorCall(processNum, msgBase);

        // BLOCK PROCESS
        cpu.IsWaiting = true;

        Console.WriteLine($"[ND500-TRAP] Process {processNum} blocked, waiting for ND-100");
    }

    private void SaveCPUState(ND500CPU cpu, uint saveAddress)
    {
        _mpm.WriteDoubleWord(saveAddress + 0, cpu.PC);
        _mpm.WriteDoubleWord(saveAddress + 4, cpu.STATUS);
        _mpm.WriteDoubleWord(saveAddress + 8, cpu.R0);
        // ... save all registers (total 64 bytes)
    }
}
```

#### 3. Interface3022-5015.cs (Bus Communication)

```csharp
public class Interface3022_5015
{
    private ushort _tagOut;  // ND-500 → ND-100
    private ushort _tagIn;   // ND-100 → ND-500

    /// <summary>
    /// Signal ND-100 that ND-500 needs service.
    /// Called by ND-500 trap handler.
    /// </summary>
    public void SignalND100MonitorCall(byte processNum, uint messageAddr)
    {
        // CONSTRUCT TAG VALUE
        // High byte: TAG code (0x01 = MONITOR_CALL_REQUEST)
        // Low byte: Process number
        _tagOut = (ushort)((0x01 << 8) | processNum);

        Console.WriteLine($"[5015] Signaling ND-100: TAG=0x{_tagOut:X4}");
        Console.WriteLine($"[5015] Process {processNum}, message at 0x{messageAddr:X}");

        // TRIGGER ND-100 INTERRUPT LEVEL 12
        TriggerND100Interrupt(12, processNum, messageAddr);
    }

    /// <summary>
    /// Handle IOX instruction from ND-100.
    /// </summary>
    public ushort HandleIOX(byte offset, ushort? writeValue = null)
    {
        switch (offset)
        {
            case 0x08:  // RTAG5 - Read TAG (ND-100 reads from ND-500)
                Console.WriteLine($"[3022] ND-100 read TAG-IN: 0x{_tagOut:X4}");
                return _tagOut;

            case 0x09:  // LTAG5 - Write TAG (ND-100 writes to ND-500)
                if (writeValue.HasValue)
                {
                    _tagIn = writeValue.Value;
                    Console.WriteLine($"[3022] ND-100 wrote TAG-OUT: 0x{_tagIn:X4}");

                    // Process TAG from ND-100
                    ProcessTagFromND100(_tagIn);
                }
                return 0;
        }
    }

    private void ProcessTagFromND100(ushort tagValue)
    {
        byte tagCode = (byte)((tagValue >> 8) & 0xFF);
        byte processNum = (byte)(tagValue & 0xFF);

        if (tagCode == 0x02)  // OPERATION_COMPLETE
        {
            Console.WriteLine($"[5015] ND-100 completed operation for process {processNum}");

            // RESUME ND-500 PROCESS
            _nd500.TrapHandler.ResumeAfterMonitorCall(_nd500, processNum);
        }
    }
}
```

---

## Summary

### The Complete Flow

```
1. ND-500 executes: CALLG R0, #0x1F000000

2. CPU Decode:
   - Extract segment: (0x1F000000 >> 24) & 0x1F = 0x1F (31)

3. Capability Check:
   - Load: ProgramCapabilities[31] = 0xC000
   - Check O-bit: (0xC000 & 0x4000) != 0 → TRUE

4. TRAP Triggered:
   - Call TrapHandler_HandleOtherCPU()
   - Save CPU state (64 bytes)
   - Set ITMQUEUE flag

5. Bus Interface (5015):
   - Construct TAG: (0x01 << 8) | process_num = 0x0105
   - Write TAG-OUT register
   - Assert hardware signal line

6. Bus Propagation:
   - Signal travels 5015 → 3022 (5-10 ns)
   - 3022 latches TAG value
   - 3022 triggers ND-100 interrupt level 12

7. ND-100 Reads TAG:
   - IOX read from offset 0x08 (RTAG5)
   - Read value: 0x0105
   - Extract: tag_code=0x01, process=5

8. ND-100 Processes Request:
   - Read message buffer from 5MPM
   - Execute MICFU function
   - Write result

9. ND-100 Signals Completion:
   - IOX write to offset 0x09 (LTAG5)
   - Write value: 0x0205 (OPERATION_COMPLETE)

10. ND-500 Resumes:
    - 5015 receives TAG 0x0205
    - Restore CPU state from message buffer
    - Clear IsWaiting flag
    - Resume at PC (instruction after CALLG)
```

### Key Points

1. **Segment 37₈ (31₁₀)** is special - never contains actual code
2. **O-bit (bit 14)** in capability register triggers trap
3. **Trap replaces normal CALLG execution** - code never jumps to segment 31
4. **TAG registers** provide fast hardware signaling between CPUs
5. **Message buffer** in 5MPM holds all communication data
6. **ITMQUEUE flag** synchronizes request/completion
7. **No software polling** - hardware interrupts drive everything

This is the complete instruction-level mechanism for ND-500 → ND-100 communication!

---

**Document Version:** 1.0
**Date:** 2025-11-26
**Based On:** SINTRAN III emulator source code analysis
