# ND-500 Interface Locking Mechanism

> **Corrected 2026-07-08** against the NPL sources and ND-30.013.02 (see
> [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) sections 3.2,
> 9 and 10, and the evidence in
> [ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md)).
> The former "High-Level TAG-IN Codes 8/9/16" content was an emulator invention and
> has been removed; see section "TAG-IN Code Reference" below for what replaced it.
> All IOX offsets in this document are OCTAL.

## Overview

The **InterfaceLocked** state (bit 5 of the Status Register) is a critical hardware interlock in the PCB 3022 (ND-500 Interface) that prevents the ND-100 from modifying interface settings while the ND-500 is executing an operation.

This document provides a comprehensive reference for the InterfaceLocked state machine, documenting all lock triggers and unlock mechanisms.

---

## Purpose

When the interface is locked (per the four-mode decode, ND-30.013.02 section 3.14):
- ND-100 **cannot** load the control register (LCON5 does not decode while locked)
- ND-100 **cannot** read or load the Memory Address Register (MAR)
- ND-100 **cannot** load the status register (LSTA5 requires unlocked + test mode)
- Still available while locked (not test): RSTA5, MCLR5, TERM5, RTAG5/LTAG5
  (TAG loopback), WDAT (DATAX), SLOC5, CLKD5, UNLC5, RETG5

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

### Two lock triggers: CONTROL bit 2 (activate) and SLOC5

| Trigger | IOX Offset (oct) | Detail |
|---------|------------------|--------|
| LoadControlRegister (LCON5) with bit 2 set | +5 | "Activate ND-500 operation (and lock the communication)" - ND-30.013.02 section 3.1 |
| SLOC5 "Set locked" | +14 | write/command register; last step of SINTRAN's enable sequence |

**Real SINTRAN activate-with-message path** (ACT50, MP-P2-N500.NPL:3084-3086):
```npl
ACT50: 5MBBANK; T:=HDEV+LMAR5; *IOXT     % MAR (MS part) := message bank
       A:=X; *IOXT                       % MAR (LS part) := message address
       A:=5; T+"LCON5-LMAR5"; *IOXT      % CONTROL := 5 (int enable + ACTIVATE/lock)
```

**Real SINTRAN enable sequence** (MP-P2-N500.NPL:3089-3092):
```npl
A:=10; T:=HDEV+LCON5;   *IOXT    % test mode (so LSTA5 decodes)
A:=0;  T+"LSTA5-LCON5"; *IOXT    % clear status
A:=1;  T+"LCON5-LSTA5"; *IOXT    % leave test mode, enable interrupt
       T+"SLOC5-LCON5"; *IOXT    % SET LOCK via SLOC5
```

Other control register bits do NOT cause locking:
- Bit 0 (Enable interrupt from ND-500) - No lock
- Bit 3 (Test mode) - No lock
- Bit 4 (ND-500 programmed clear; also clears DMA-error status bit 6) - No lock
- Bit 5 (Disable TAG-IN decoding when locked) - No lock
- Bit 6 (DMA error) - No lock
- Bit 7 (Command chaining) - No lock

---

## Unlock Mechanisms

### 1. Terminate (Normal Completion)

| Register | IOX Offset (oct) | Description |
|----------|------------------|-------------|
| Terminate (TERM5) | +7 | Request the ND-500 to stop |

**Semantics (verified, MP-P2-N500.NPL:2933-2946):** TERM5 REQUESTS a stop. The
ND-500 microcode acknowledges by releasing the interface lock; SINTRAN then POLLS
RSTA5 in a bounded loop until 5ILOCK (bit 5) clears, and falls back to the 5MCST
micro-stop on timeout ("Time out; master clear it").

**Real SINTRAN code (XTER500):**
```npl
T:=HDEV+RSTA5; *IOXT             % read status
IF A BIT 5ILOCK THEN             % ND-500 running, i/f locked
   T+"TERM5-RSTA5"; *IOXT        % terminate request
   T+"RSTA5-TERM5"
   FOR LOOPCOUNTER DO            % wait for nd-500 to unlock i/f
      FOR X:=-20 DO; OD
      *IOXT
      WHILE A BIT 5ILOCK
   OD
FI
```

---

### 2. Micro-stop via the 5MCST sequence (RETG5 stop bit)

| Register | IOX Offset (oct) | Bit | Meaning |
|----------|------------------|-----|---------|
| ReturnGate (RETG5) | +17 | 1 | STOP BIT (bit 0 = reverse tag bus) |

**Semantics (ND-30.013.02 TST02/TST04):** RETG5 A-bit1 sets the microcode stop bit
(STATUS bit 9, 5CLOST, goes up); A-bit0 drives the reverse tag bus. There is NO
evidence that RETG5 itself clears the interface lock - SINTRAN unlocks explicitly
with UNLC5 FIRST:

**Real SINTRAN code (5MCST, CC-P2-N500.NPL:213-217):**
```npl
X5MCST: T:=HDEV
5MCST:  T+UNLC5; *IOXT                   % UNLOCK
        A:=40; T+"LCON5-UNLC5"; *IOXT    % DISABLE TAG-IN DECODING
        A:=2;  T+"RETG5-LCON5"; *IOXT    % set stop bit
```

**Use Case:** terminate timeout ("master clear it") and power-fail paths. Note this
sequence does NOT touch the MCLR5 register.

---

### 3. UNLC5 - the explicit unlock register

| Register | IOX Offset (oct) | Description |
|----------|------------------|-------------|
| UNLC5 "Release locked" | +16 | write/command; clears the lock directly |

Used by SINTRAN as the FIRST step of 5MCST (see mechanism 2) and by the hardware
master-clear reference sequence (ND-30.013.02 section 3.15.1).

---

### 4. ResetActivate (TAG-OUT code 5, driven by the ND-500)

| Source | TAG-OUT code | Description |
|--------|--------------|-------------|
| ND-500 microcode (TAG-OUT register on 5015) | 5 | "Reset activate" - releases the activate/lock |

TAG-OUT is the register the ND-500 MICROCODE drives toward the 3022
(ND-30.013.02 section 3.13); the ND-100 cannot send TAG-OUT codes. This is how the
microcode itself releases the lock on completion. SINTRAN never does this - the
formerly quoted "SINTRAN Code Pattern" writing LTAG5 was fabricated (SINTRAN never
writes LTAG5 at all - see the master reference section 3.3).

---

### 5. DUNL TAG-IN strobe (code 14, written by the ND-100)

| Source | TAG-IN code | Description |
|--------|-------------|-------------|
| ND-100 writes WTAG (offset +11) | 14 (16 oct) | DUNL "unlock" strobe decoded on the 5015 |

TAG-IN is written BY THE ND-100 into the 5015 ("The TAG-IN register on 5015 (I/O
from ND-100)" - ND-30.013.02 section 3.12). The old description here ("the ND-500
can voluntarily release the lock by writing TAG code 14") had the direction
inverted; that inverted model also lives in the current C# emulator (audit D01/D15).
SINTRAN does not use this path.

---

### 6. Master Clear (MCLR5)

| Register | IOX Offset (oct) | Description |
|----------|------------------|-------------|
| MasterClear (MCLR5) | +6 | Command strobe: restart microprogram at control-store address 0 |

**Semantics (ND-30.013.02 sections 3.14, 6.3.1):** executing the IOX is the action;
no data word is involved (in unlocked+test mode this offset loads the DATA register
instead). Available in both locked and unlocked NOT-test modes.

**SINTRAN never issues MCLR5** - the OS stops the machine with the 5MCST sequence
(mechanism 2). MCLR5 is used by test programs and the hardware master-clear
reference sequence (ND-30.013.02 section 3.15.1).

---

## Summary Table

| Unlock Mechanism | IOX (oct) | Code | Who uses it | Notes |
|-----------------|-----------|------|-------------|-------|
| **Terminate** | +7 | - | SINTRAN (XTER500, power-fail) | request; microcode releases lock; SINTRAN polls 5ILOCK |
| **UNLC5** | +16 | - | SINTRAN (5MCST), test programs | direct unlock |
| **5MCST sequence** | +16,+5,+17 | LCON5:=40, RETG5:=2 | SINTRAN | unlock + disable TAG decode + STOP BIT (5CLOST) |
| **ResetActivate** | - | TAG-OUT 5 | ND-500 microcode | normal completion release |
| **DUNL TAG-IN** | +11 | TAG-IN 14 | ND-100 diagnostics | strobe decoded on 5015; not used by SINTRAN |
| **Master Clear** | +6 | - | test programs / loader | microcode restart at CS address 0 |

---

## Typical Operation Sequence

(Verified flow - master reference sections 5 and 7.)

```
1. SINTRAN builds a MESSAGE in mailbox memory        [UNLOCKED]
         |
         v
2. SINTRAN loads MAR (bank, then address)            [UNLOCKED]
   and writes CONTROL := 5 (int enable + activate)   [-> LOCKED]
         |
         v
3. ND-500 microcode DMA-fetches the message,         [LOCKED]
   executes MICFU, writes answer into the message
   (N5STA status word, STOPR stop reason)
         |
         v
4. ND-500 stops/finishes; microcode releases         [-> UNLOCKED]
   the lock (TAG-OUT 5 reset-activate);
   level 12 interrupt raised (ident 16 for twh 0)
         |
         v
5. SINTRAN ISR (5STDRIV): CLE5STATUS reads RSTA5,    [UNLOCKED]
   walks the message queue, dispatches on N5STA
   (DECOMESS -> MCHANDLE for monitor calls)
         |
         v
6. SINTRAN activates the next waiting message        [-> LOCKED]
   (XACT500)
```

---

## Register Reference

(ND-30.013.02 sections 3.1/3.2 - full maps in the master reference section 4.)

### Status Register (read RSTA5, IOX +2)

| Bit | Name (SINTRAN symbol) | Description |
|-----|------|-------------|
| 0 | InterruptEnabled | Interrupt enable status (not settable via LSTA5) |
| 1 | (unused) | - |
| 2 | ND500Busy | ND-500 operation in progress |
| 3 | ND500Finished | ND-500 operation completed |
| 4 | Error (5PAGF) | Inclusive OR of errors |
| **5** | **InterfaceLocked (5ILOC)** | **Interface locked = ND-500 running (not settable via LSTA5)** |
| 6 | DMAError (5DMAER) | DMA transfer error |
| 7 | PowerFault (5PFAIL) | Power fault executed by microprogram; stop bit set |
| 8 | ND500PowerOff (5POWOF) | ND-500 power is/has been off (latched) |
| 9 | ND500MicroClockStopped (5CLOST) | Microclock has stopped (not settable via LSTA5) |
| 10-14 | Stop reason | ND-500 stop reason field (values UNVERIFIED) |
| 15 | Control bit 15 | Mirrors CONTROL register bit 15 (not settable via LSTA5) |

### Control Register (write LCON5, IOX +5)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | EnableInterruptFromND500 | Enable Level 12 interrupt |
| 1 | (unused) | - |
| **2** | **ActivateND500Operation** | **Start operation (LOCKS interface)** |
| 3 | TestMode | Test mode (changes the IOX decode) |
| 4 | ND500ProgrammedClear | ND-500 programmed clear; also clears status bit 6 (DMA error) |
| 5 | DisableTagInDecoding | Disable TAG-IN decode when locked |
| 6 | DMAError | DMA error |
| 7 | CommandChaining | Enable command chaining |
| 8-14 | ND500Operation | Operation field (400 used in the power-fail path) |
| 15 | (unused) | - |

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

The following test methods verify the InterfaceLocked state machine. WARNING: tests
marked (*) encode the OLD fabricated/inverted TAG model or the "only activate locks"
claim and must be revised together with the code fixes in
[ND500-EMULATOR-DISCREPANCY-AUDIT.md](ND500-EMULATOR-DISCREPANCY-AUDIT.md)
(D01, D07, D11, D15):

| Test | Verifies |
|------|----------|
| `Test_NDBusND500IF_InterfaceLocked_InitiallyUnlocked` | Initial state |
| `Test_NDBusND500IF_InterfaceLocked_LocksOnActivate` | Lock trigger |
| `Test_NDBusND500IF_InterfaceLocked_UnlocksOnTerminate` | Terminate unlock |
| `Test_NDBusND500IF_InterfaceLocked_UnlocksOnMicroclockStop` | (*) RETG5 does not unlock per evidence (D11) |
| `Test_TagOut_Code5_ResetActivate_UnlocksInterface` | (*) valid concept, wrong port in emulator (D15) |
| `Test_TagIn_Code14_DUNL_UnlocksInterface` | (*) direction inverted in emulator (D01/D15) |
| `Test_InterfaceLocked_UnlocksOnMasterClear` | Master Clear unlock |
| `Test_InterfaceLocked_StateMachine_FullCycle_*` | Full cycle tests |
| `Test_InterfaceLocked_MultipleUnlockMechanisms` | All mechanisms |
| `Test_InterfaceLocked_OnlyActivateLocks` | (*) SLOC5 also locks |

---

## TAG-IN Code Reference

**Direction (ND-30.013.02 section 3.12):** the TAG-IN register on the 5015 is
written BY THE ND-100 (WTAG, offset +11) and decoded on the 5015 into register
strobes. TAG-OUT (section 3.13) is driven by the ND-500 microcode toward the 3022.
SINTRAN uses NEITHER - the driver never reads RTAG5 or writes LTAG5 (master
reference section 3.3). These codes matter for microcode loading, test programs
and hardware-faithful emulation.

### TAG-IN codes (4-bit, decoded on the 5015)

Names OCR-corrected from ND-30.013.02; positions reliable:

| Code (dec) | Name | Description |
|------|------|-------------|
| 0 | - | Not used |
| 1 | DICLK1 | Clock DATA-IN-1 register |
| 2 | DICLK2 | Clock DATA-IN-2 register |
| 3 | DUCLK | Clock DATA-OUT register (both halves) |
| 4 | WACLK | Clock control-store write-address (WA) register |
| 5 | BRKCLK | Clock BREAK register |
| 6 | TGCLK | Clock TAG-OUT register |
| 7 | CNTCLK | Clock CSCNT register |
| 8 | DIEN | Enable DATA-IN register to bus (CDB) |
| 9 | DUEN | Enable DATA-OUT register (least significant) |
| 10 | WAR | Read write-address register |
| 11 | BRKR | Read BREAK register |
| 12 | CNTR | Read CSCNT register |
| 13 | RESBRK | Reset break |
| **14** | **DUNL** | **Unlock** |
| 15 | EOUTEN | Enable data line driver |

Bits 0-3 carry the code; bit 4 is unused; bit 5 (octal 040) returns TAG-IN bits 0-4
(loopback for verification - read back via RTAG, offset +10).

Note: WACLK clocks the CONTROL-STORE write-address register (used in microcode
loading), not the MAR - the earlier "copy DATA to MAR" side-effect claim was wrong.

### TAG-OUT codes (3-bit, driven by ND-500 microcode)

| Code | Function |
|------|----------|
| 0 | Read MAR |
| 1 | Write MAR |
| 2 | Read STATUS register |
| 3 | Write STATUS register |
| 4 | Read CONTROL register |
| 5 | Reset activate (releases the lock) |
| 6 | Read DATA register (and ND-100 memory) - DMA read |
| 7 | Write DATA register (and then into ND-100 memory) - DMA write |

Bit 3 = 0 means the operation targets the ND-100; bit 7 (MOST) selects the most/
least significant half of the 32-bit data registers.

### The removed "High-Level TAG-IN Codes (8, 9, 16)"

Earlier versions of this document defined operational TAG-IN codes
8 = MonitorCallRequest, 9 = PageFaultRequest, 16 = OperationComplete with a process
number in bits 8-11. **These do not exist.** Code 8 is DIEN, code 9 is DUEN, and
the code field is 4 bits wide so 16 is unrepresentable. They were an
emulator-internal invention (still present in NDBusND500IF.cs - audit D01) that
conflated the hardware TAG strobes with the real signalling path: monitor calls and
page faults reach SINTRAN as MESSAGES (MICFU + STOPR fields) dispatched by the
level-12 driver - see the master reference sections 7.3-7.4 and 10.3.

---

## Related Documentation

- [ND-500 Reference Manual](../../Reference-Manuals/500/ND-05.009.4%20EN%20ND-500%20Reference%20Manual.md)
- [ND-500/ND-100 Interface Comprehensive Guide](ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md)
- [SINTRAN Monitor Calls](../../Reference-Manuals/ND-860228-2-EN%20SINTRAN%20III%20Monitor%20Calls.md)

---

## References

- **PCB 3022**: ND-500 Interface card (in ND-100 chassis)
- **PCB 5015**: ND-500 Control II card (in ND-500 chassis)
- **IOX Base**: thumbwheel-selected - 60, 1060, 660, 760 or 560 octal with ident
  16, 116, 36, 114, 76 (ND-06.015.02 section D.13.1); per-CPU HDEV in SINTRAN
- **Interrupt Level**: 12 (SINTRAN driver level)
- **Authoritative spec**: [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md)

---

**Last Updated:** 2026-07-08
**Document Version:** 2.0 (corrected against NPL sources and ND-30.013.02; removed
fabricated high-level TAG protocol)
**Author:** RetroCore Emulator Project
