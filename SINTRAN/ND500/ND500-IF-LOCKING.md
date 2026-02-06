# ND-500 Interface Locking Mechanism

## Overview

The **InterfaceLocked** state (bit 5 of the Status Register) is a critical hardware interlock in the PCB 3022 (ND-500 Interface) that prevents the ND-100 from modifying interface settings while the ND-500 is executing an operation.

This document provides a comprehensive reference for the InterfaceLocked state machine, documenting all lock triggers and unlock mechanisms.

---

## Purpose

When the interface is locked:
- ND-100 **cannot** change control register settings
- ND-100 **cannot** modify the Memory Address Register (MAR)
- ND-100 **cannot** change TAG-OUT values that would conflict with running operations

This prevents race conditions where the ND-100 might modify settings mid-operation, which could:
- Corrupt data transfers
- Cause undefined behavior
- Create timing hazards between the two processors

---

## State Machine

```
                    ┌───────────────────────┐
                    │      UNLOCKED         │
                    │  (InterfaceLocked=0)  │
                    │   Interface ready     │
                    │   ND-100 can modify   │
                    │   all settings        │
                    └───────────┬───────────┘
                                │
        LoadControlRegister (IOX +5) with ActivateND500Operation (bit 2)
                                │
                    ┌───────────▼───────────┐
                    │       LOCKED          │
                    │  (InterfaceLocked=1)  │
                    │   ND-500 running      │
                    │   ND-100 blocked      │
                    │   from modifications  │
                    └───────────┬───────────┘
                                │
          ┌─────────────────────┼─────────────────────┐
          │                     │                     │
     Terminate            ResetActivate         Master Clear
      (IOX +7)          (TAG-OUT code 5)         (IOX +6)
          │                     │                     │
          │              MicroclockStop               │
          │              (RETG5 bit 1)                │
          │                     │                     │
          │              DUNL TAG-IN                  │
          │               (code 14)                   │
          │                     │                     │
          └─────────────────────┼─────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │      UNLOCKED         │
                    └───────────────────────┘
```

---

## Lock Trigger

### ActivateND500Operation (Single Trigger Point)

| Register | IOX Offset | Bit | Value |
|----------|------------|-----|-------|
| LoadControlRegister (LCON5) | +5 | 2 | 0x0004 |

**When triggered:**
- InterfaceLocked = true (bit 5 set)
- ND500Busy = true (bit 2 set)
- ND500Finished = false (bit 3 cleared)

**SINTRAN Code Pattern:**
```npl
; Activate ND-500 operation
A := 4;                     ; ActivateND500Operation bit
T + "LCON5-UNLC5"; *IOXT;   ; Write to control register
```

**Important:** This is the **only** mechanism that locks the interface. Other control register bits do NOT cause locking:
- Bit 0 (EnableInterrupt) - No lock
- Bit 3 (TestMode) - No lock
- Bit 4 (ClearInterrupt) - No lock
- Bit 5 (DisableTagInDecoding) - No lock
- Bit 6 (DMAErrorClear) - No lock
- Bit 7 (CommandChaining) - No lock

---

## Unlock Mechanisms

### 1. Terminate (Normal Completion)

| Register | IOX Offset | Description |
|----------|------------|-------------|
| Terminate (TERM5) | +7 | Normal operation completion |

**Actions:**
- InterfaceLocked = false (cleared)
- ActivateND500Operation = false (cleared in control)
- ND500Busy = false (cleared)
- ND500Finished = true (set)

**SINTRAN Code Pattern:**
```npl
; Terminate ND-500 operation
T + "TERM5-UNLC5"; *IOXT;   ; Write to terminate register
```

---

### 2. MicroclockStop (Diagnostic Halt)

| Register | IOX Offset | Bit | Value |
|----------|------------|-----|-------|
| ReturnGate (RETG5) | +17 | 1 | 0x0002 |

**Actions:**
- InterfaceLocked = false (cleared)
- ND500Busy = false (cleared)
- ND500MicroClockStopped = true (bit 9 set)

**SINTRAN Code Pattern:**
```npl
; Force microcode halt (5MCST routine)
A := 2;                     ; MicroclockStop bit
T + "RETG5-UNLC5"; *IOXT;   ; Write to return gate
```

**Use Case:** Used during boot sequence (5MCST) and forced termination.

---

### 3. ResetActivate (TAG-OUT Code 5)

| Register | IOX Offset | TAG Code |
|----------|------------|----------|
| LoadTagOut (LTAG5) | +9 | 5 |

**Actions:**
- InterfaceLocked = false (cleared)
- **Only** InterfaceLocked is affected; other status bits remain unchanged

**SINTRAN Code Pattern:**
```npl
; Send ResetActivate TAG-OUT
A := 5;                     ; ResetActivate code
T + "LTAG5-UNLC5"; *IOXT;   ; Write to TAG-OUT register
```

**Important:** This mechanism **only** clears the lock bit. It does NOT affect:
- ND500Busy
- ND500Finished
- Other status bits

---

### 4. DUNL TAG-IN (Code 14)

| Source | TAG Code | Description |
|--------|----------|-------------|
| ND-500 CPU (via WriteTag) | 14 | ND-500 initiated unlock |

**Actions:**
- InterfaceLocked = false (cleared)

**Use Case:** The ND-500 can voluntarily release the interface lock by writing TAG code 14.

**How it works:**
1. ND-500 microcode calls `WriteTag(14)`
2. NDBusND500IF receives event via `OnND500WritesTag()`
3. ProcessTagIn() handles code 14 (DUNL)
4. Interface unlocked

---

### 5. Master Clear / Reset

| Register | IOX Offset | Description |
|----------|------------|-------------|
| MasterClear (MCLE5) | +6 | Full interface reset |

**Actions:**
- All status bits cleared (including InterfaceLocked)
- All control bits cleared
- MAR cleared
- DATA register cleared
- Flip-flops reset
- ND-500 CPU reset (if attached)

**SINTRAN Code Pattern:**
```npl
; Full interface reset
T + "MCLE5-UNLC5"; *IOXT;   ; Write to master clear
```

**Note:** This is the most destructive unlock mechanism. Use only when a full reset is required.

---

## Summary Table

| Unlock Mechanism | IOX | Code | InterfaceLocked | ND500Busy | ND500Finished | Other Effects |
|-----------------|-----|------|-----------------|-----------|---------------|---------------|
| **Terminate** | +7 | - | CLEAR | CLEAR | SET | Clears ActivateND500Operation |
| **MicroclockStop** | +17 | bit 1 | CLEAR | CLEAR | unchanged | Sets ND500MicroClockStopped |
| **ResetActivate** | +9 | 5 | CLEAR | unchanged | unchanged | None |
| **DUNL TAG-IN** | - | 14 | CLEAR | unchanged | unchanged | None |
| **Master Clear** | +6 | - | CLEAR | CLEAR | CLEAR | Full reset |

---

## Typical Operation Sequence

```
1. ND-100 sets up registers (DATA, MAR, TAG-OUT)     [UNLOCKED]
         │
         ▼
2. ND-100 writes ActivateND500Operation              [LOCKED]
   (IOX +5 with bit 2 = 0x0004)
         │
         ▼
3. ND-500 executes operation                         [LOCKED]
         │
         ▼
4. ND-500 writes TAG-IN with result/status           [LOCKED]
   (e.g., Monitor call code 8, Page fault code 9)
         │
         ▼
5. ND-100 receives Level 12 interrupt                [LOCKED]
         │
         ▼
6. ND-100 reads status/data                          [LOCKED]
         │
         ▼
7. ND-100 issues Terminate (IOX +7)                  [UNLOCKED]
         │
         ▼
8. Ready for next operation                          [UNLOCKED]
```

---

## Register Reference

### Status Register (Read via IOX +2)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | InterruptEnabled | Interrupt enable status |
| 1 | (unused) | - |
| 2 | ND500Busy | ND-500 operation in progress |
| 3 | ND500Finished | ND-500 operation completed |
| 4 | ND500Error | Error during operation |
| **5** | **InterfaceLocked** | **Interface locked state** |
| 6 | DMAError | DMA transfer error |
| 7 | PowerFault | Power fault detected |
| 8 | ND500PowerOff | ND-500 power is off |
| 9 | ND500MicroClockStopped | Microclock has stopped |

### Control Register (Write via IOX +5)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | EnableInterruptFromND500 | Enable Level 12 interrupt |
| 1 | (unused) | - |
| **2** | **ActivateND500Operation** | **Start operation (LOCKS interface)** |
| 3 | TestMode | Enable diagnostic mode |
| 4 | ClearInterrupt | Clear pending interrupt |
| 5 | DisableTagInDecoding | Disable automatic TAG-IN decode |
| 6 | DMAErrorClear | Clear DMA error flag |
| 7 | CommandChaining | Enable command chaining |

---

## Implementation Notes

### C# Emulator (NDBusND500IF.cs)

The interface lock state is tracked via the `StatusRegisterBits.InterfaceLocked` flag:

```csharp
// Check if locked
public bool isLocked => (statusRegister & StatusRegisterBits.InterfaceLocked) != 0;

// Lock on activate
if ((controlRegister & ControlWordBits.ActivateND500Operation) != 0)
{
    statusRegister |= StatusRegisterBits.InterfaceLocked;
    statusRegister |= StatusRegisterBits.ND500Busy;
    statusRegister &= ~StatusRegisterBits.ND500Finished;
}

// Unlock mechanisms
statusRegister &= ~StatusRegisterBits.InterfaceLocked;
```

### Unit Test Coverage

The following test methods verify the InterfaceLocked state machine:

| Test | Verifies |
|------|----------|
| `Test_NDBusND500IF_InterfaceLocked_InitiallyUnlocked` | Initial state |
| `Test_NDBusND500IF_InterfaceLocked_LocksOnActivate` | Lock trigger |
| `Test_NDBusND500IF_InterfaceLocked_UnlocksOnTerminate` | Terminate unlock |
| `Test_NDBusND500IF_InterfaceLocked_UnlocksOnMicroclockStop` | MicroclockStop unlock |
| `Test_TagOut_Code5_ResetActivate_UnlocksInterface` | ResetActivate unlock |
| `Test_TagIn_Code14_DUNL_UnlocksInterface` | DUNL unlock |
| `Test_InterfaceLocked_UnlocksOnMasterClear` | Master Clear unlock |
| `Test_InterfaceLocked_StateMachine_FullCycle_*` | Full cycle tests |
| `Test_InterfaceLocked_MultipleUnlockMechanisms` | All mechanisms |
| `Test_InterfaceLocked_OnlyActivateLocks` | Only bit 2 locks |

---

## TAG-IN Code Reference

The TAG-IN register carries commands from the ND-500 to the ND-100. There are two categories of TAG-IN codes:

### Low-Level TAG-IN Codes (0-15)

These are **diagnostic/hardware-level** commands used by ND-500 microcode for direct register manipulation. They control the interface at the hardware level.

| Code | Name | Description |
|------|------|-------------|
| 0 | NotUsed | Reserved |
| 1 | DICLK1 | Clock data-in register 1 (low 16 bits) |
| 2 | DICLK2 | Clock data-in register 2 (high 16 bits) |
| 3 | DUCLK | Clock data-out register (both halves) |
| 4 | WACLK | Clock write-address register (copy DATA → MAR) |
| 5 | BRKCLK | Clock break register |
| 6 | TGUCLOCK | Clock tag-out register |
| 7 | CNTCLK | Clock CSCNT register |
| 8 | DIEN | Enable data-in register to bus (CDB) |
| 9 | DUEN | Enable data-out register (least significant) |
| 10 | WAR | Read write-address register |
| 11 | BRKR | Read break register |
| 12 | CNTR | Read CSCNT register |
| 13 | RESBRK | Reset break |
| **14** | **DUNL** | **Unlock interface** (clears InterfaceLocked) |
| 15 | EDUTEN | Enable data line driver |

**Side Effects:**
- Code 1 (DICLK1): Updates low 16 bits of DATA register
- Code 2 (DICLK2): Updates high 16 bits of DATA register
- Code 4 (WACLK): Copies DATA to MAR
- **Code 14 (DUNL): Clears InterfaceLocked bit**

### High-Level TAG-IN Codes (8, 9, 16)

These are **operational codes** used during normal SINTRAN operation. They trigger Level 12 interrupts on the ND-100.

| Code | Name | Description | Triggers Interrupt |
|------|------|-------------|-------------------|
| 8 | MonitorCallRequest | ND-500 process needs a monitor call | Yes |
| 9 | PageFaultRequest | ND-500 process had a page fault | Yes |
| 16 | OperationComplete | ND-500 signals operation done | Yes |

**Process Number:** For codes 8 and 9, bits 8-11 of the TAG-IN value contain the process number (0-15).

### Code 8 Ambiguity

Code 8 appears in **both** categories with different meanings:

| Context | Name | Meaning |
|---------|------|---------|
| Low-level (diagnostic) | DIEN | Enable data-in register to bus |
| High-level (operational) | MonitorCallRequest | ND-500 process needs monitor call |

**Resolution:** The emulator checks for high-level codes first. If the code matches 8, 9, or 16, it's treated as a high-level code that triggers an interrupt. Otherwise, it's processed as a low-level diagnostic code.

### TAG-IN Processing Flow

```
ND-500 calls WriteTag(tagValue)
         │
         ▼
OnND500WritesTag() receives event
         │
         ▼
Store in tagInRegister
         │
         ▼
Extract code from bits 0-4
         │
         ├─── Code 8, 9, or 16? ───► HIGH-LEVEL PATH
         │         │                      │
         │         │               Set ND500Finished
         │         │                      │
         │         │               Trigger Level 12 interrupt
         │         │                      │
         │         └──────────────────────┘
         │
         └─── Code 0-15? ─────────► LOW-LEVEL PATH
                   │                      │
                   │               ProcessTagIn()
                   │                      │
                   │               Handle side effects
                   │               (DUNL unlocks, etc.)
                   │                      │
                   └──────────────────────┘
```

### TAG-IN Register Format

```
Bits 15-12: (unused)
Bits 11-8:  Process number (for codes 8, 9)
Bits 7-6:   (unused)
Bit 5:      Return TAG flag (disables automatic decoding)
Bit 4:      (unused)
Bits 3-0:   TAG-IN code (0-15 for low-level, or 8/9/16 for high-level)
```

**Helper Functions (C#):**
```csharp
public static class TagRegisterFormat
{
    public const ushort CodeMask = 0x001F;      // Bits 0-4
    public const ushort ProcessMask = 0x0F00;   // Bits 8-11
    public const int ProcessShift = 8;

    public static byte GetCode(ushort tagValue)
        => (byte)(tagValue & CodeMask);

    public static byte GetProcessNumber(ushort tagValue)
        => (byte)((tagValue & ProcessMask) >> ProcessShift);

    public static ushort MakeTagValue(byte code, byte processNum)
        => (ushort)(code | (processNum << ProcessShift));
}
```

---

## Related Documentation

- [ND-500 Reference Manual](../../Reference-Manuals/500/ND-05.009.4%20EN%20ND-500%20Reference%20Manual.md)
- [ND-500/ND-100 Interface Comprehensive Guide](ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md)
- [SINTRAN Monitor Calls](../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md)

---

## References

- **PCB 3022**: ND-500 Interface card (in ND-100 chassis)
- **PCB 5015**: ND-500 Control II card (in ND-500 chassis)
- **IOX Base**: Device number 1560₈ (thumbwheel selectable)
- **Interrupt Level**: 12

---

**Last Updated:** 2026-02-05
**Document Version:** 1.1 (added TAG-IN code reference)
**Author:** RetroCore Emulator Project
