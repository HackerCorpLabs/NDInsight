# MON 60 (N500M) - Hardware Interface Mapping Analysis

**Purpose:** Document the relationship between MON 60 subfunctions and the ND-500 bus interface
**Audience:** Developers new to Norsk Data systems
**Date:** 2025-02-05

> **CORRECTED 2026-07-20.** This document originally described a "high-level TAG code
> protocol" (TAG-IN 8 = monitor-call request, TAG-OUT 16 = operation complete, process
> number in TAG bits 8-11, etc.). **That protocol never existed** — it was an emulator
> invention, disproven against ND-30.013.02 (TMP) and the SINTRAN NPL sources. The TAG
> registers are 4-bit register-level strobes between the 3022 and the 5015 used by the
> microcode, the control-store loader and the test programs; the runtime driver never
> exchanges codes through them. Monitor calls travel entirely inside the 5MPM message
> (status word `N5STA`, stop reason `STOPR`), and the ND-500→ND-100 "doorbell" is the
> STATUS-register "finished" bit raising the level-12 interrupt.
> Authoritative reference:
> `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md`
> (sections 4, 5, 7 and 10, incl. 10.3 "The fabricated protocol, for the record").
> The sections below have been rewritten accordingly.

---

## 1. Introduction for Newcomers

### 1.1 What is the ND-100/ND-500 System?

The Norsk Data ND-500 is a **32-bit coprocessor** that works alongside the **16-bit ND-100** main computer. Think of it like a modern GPU working with a CPU - the ND-500 handles heavy computation while the ND-100 manages all I/O, disk access, and system services.

**Key Concept:** The ND-500 has **no direct access to disks, terminals, or any I/O devices**. Every time it needs to read a file, print output, or do anything outside pure computation, it must ask the ND-100 to do it.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         NORSK DATA SYSTEM                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────┐              ┌─────────────────┐                 │
│   │     ND-100      │              │     ND-500      │                 │
│   │  ─────────────  │              │  ─────────────  │                 │
│   │  16-bit CPU     │◄────────────►│  32-bit CPU     │                 │
│   │  SINTRAN OS     │   Hardware   │  Computation    │                 │
│   │  All I/O        │   Interface  │  No I/O         │                 │
│   │  Disk access    │   (3022/5015)│  No disk        │                 │
│   │  Terminals      │              │  No terminals   │                 │
│   └────────┬────────┘              └─────────────────┘                 │
│            │                                                            │
│            ▼                                                            │
│   ┌─────────────────┐                                                  │
│   │  Disks, Tapes   │                                                  │
│   │  Terminals      │                                                  │
│   │  Network        │                                                  │
│   └─────────────────┘                                                  │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 1.2 How Do They Communicate?

The ND-100 and ND-500 communicate through:

1. **5MPM (Multiport Memory)** - Shared RAM both CPUs can read/write
2. **STATUS/CONTROL registers** - the real attention signals: activate (CONTROL bit 2) one way, "finished" + level-12 interrupt the other
3. **3022/5015 Interface Cards** - The physical hardware connecting them

**Analogy:** Think of it like two people in separate rooms:
- The **5MPM** is a shared whiteboard where they write messages
- The **activate strobe and the level-12 interrupt** are the doorbells saying "I wrote something, come read it!" (the TAG registers are NOT doorbells — they are register-level strobes for control-store load, test and microcode DMA)

### 1.3 What is MON 60?

**MON 60 (N500M)** is a SINTRAN III monitor call that lets ND-100 programs control the ND-500. It provides 67 subfunctions for:
- Reading/writing ND-500 memory
- Starting/stopping ND-500 programs
- Managing ND-500 processes
- Loading microcode
- Debugging

---

## 2. Understanding TAG-IN and TAG-OUT (Corrected)

### 2.1 What the TAG registers really are

Both TAG registers live **on the 5015 card** (the ND-500 side). Their names are from
the 5015's perspective (ND-30.013.02 sections 3.12/3.13):

- **TAG-IN** = strobes coming **IN** to the 5015 **from the ND-100** (written via the
  3022's WTAG/LTAG5 IOX offset). Its 4-bit codes clock/enable individual 5015
  registers: DICLK1/DICLK2 (clock DATA-IN halves), DUCLK, WACLK (control-store write
  address), BRKCLK, TGCLK, CNTCLK, DIEN, DUEN, WAR, BRKR, CNTR, RESBRK, DUNL, EOUTEN.
  This is the control-store-load and test/debug path.
- **TAG-OUT** = strobes going **OUT** of the 5015 toward the 3022, **driven by the
  ND-500 microcode**. Its 3-bit codes command the 3022: read/write MAR, read/write
  STATUS, read CONTROL, reset activate, read/write DATA **(and ND-100 memory — this
  is the microcode's DMA path for fetching messages and writing answers)**.

**They are register-level hardware strobes, not a message protocol.** No monitor-call
codes, no process numbers, no completion codes ever travel through them. The runtime
SINTRAN driver does not use them at all; only the control-store loader and the test
programs (TMP) touch them from the ND-100 side.

### 2.2 How signaling actually works

| Direction | Mechanism |
|-----------|-----------|
| ND-100 → ND-500 ("go") | **Activate**: CONTROL register bit 2 via LCON5. "Nothing but an activate or a terminate from the ND-100 can cause the micro program to leave the IDLE loop" (ND-05.012.01 section 13) |
| ND-500 → ND-100 ("done"/"stopped") | Microcode writes answer status into the message (`N5STA`), sets STATUS "finished" (bit 3) + stop reason (STATUS bits 10-14); the 3022 raises **level-12 interrupt** (ident 16₈ for thumbwheel 0) if CONTROL bit 0 is set |
| Request/response payload | Entirely in the **5MPM message block** (N5STA, MICFU, STOPR, MCNO, parameters) |

See `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md`
sections 4, 5, 7 and 10 for the register bit tables and driver flows.

---

## 3. The 3022 Interface Card Registers

The 3022 is the interface card that sits in the ND-100 and connects to the ND-500.

### 3.1 Complete Register Map

**Base Address:** HDEV (device address, typically 100₈-120₈)

| Offset | Octal | Symbol | R/W | Purpose | Plain English |
|--------|-------|--------|-----|---------|---------------|
| +0 | 000 | RMAR5 | Read | Read MAR | "Where is the message?" |
| +1 | 001 | LMAR5 | Write | Load MAR | "Put message HERE" |
| +2 | 002 | RSTA5 | Read | Read Status | "What's ND-500 doing?" |
| +3 | 003 | LSTA5 | Write | Load Status | Set status bits |
| +4 | 004 | RCON5 | Read | Read Control | Read control state |
| +5 | 005 | LCON5 | Write | Load Control | "Wake up ND-500!" |
| +6 | 006 | MCLR5 | Write | Master Clear | "Reset everything!" |
| +7 | 007 | TERM5 | Write | Terminate | "Stop that process!" |
| +8 | 010 | RTAG5 | Read | Read tag (readback) | Diagnostic readback of TAG bits (return-tag path) |
| +9 | 011 | LTAG5/WTAG | Write | Write 5015 TAG-IN strobe | Control-store load / test strobes only — NOT runtime signaling |

### 3.2 Status Register (RSTA5) Explained

When you read RSTA5, each bit tells you something:

```
Bit 0:  INTE     = 1 if interrupts are enabled
Bit 2:  BUSY     = 1 if ND-500 is busy (don't bother it!)
Bit 3:  FIN      = 1 if ND-500 finished and is waiting
Bit 5:  5ILOCK   = 1 if interface is locked (someone else using it)
Bit 6:  5DMAE    = 1 if DMA error happened (bad!)
Bit 7:  5PFAI    = 1 if power failed
Bit 8:  5POWO    = 1 if power was off
Bit 9:  5CLOS    = 1 if clock stopped
Bits 10-14: STOPREASON = Why did ND-500 stop?
```

**STOPREASON Values:**
| Value | Meaning | What To Do |
|-------|---------|------------|
| 0 | Still running | Wait |
| 1 | MOCALL | ND-500 wants a monitor call - process it! |
| 2 | TRAPCODE | Something went wrong - check trap info |
| 3 | 5FMOCALL | Fast monitor call - handle quickly |
| 65 | Normal exit | Program finished successfully |

### 3.3 Control Register (LCON5) Commands

Write these values to LCON5 to control the ND-500:

| Value | What It Does | When To Use |
|-------|--------------|-------------|
| 0 | Disable everything | Initialization |
| 1 | Enable interrupts | Normal operation |
| **5** | **ACTIVATE** | **"Start running!"** |
| 32 (040₈) | Disable TAG-IN | Special diagnostic mode |

---

## 4. TAG Strobe Codes (Corrected — register-level only)

**The former "TAG command code" tables (8 = MonitorCallRequest, 16 = OperationComplete,
etc.) were fabricated and have been removed.** The real codes, from ND-30.013.02
sections 3.12/3.13, are hardware register strobes:

### 4.1 TAG-IN codes (ND-100 → 5015, written via WTAG/LTAG5) — octal

| Code | Name | Function |
|------|------|----------|
| 1 | DICLK1 | clock DATA-IN-1 register |
| 2 | DICLK2 | clock DATA-IN-2 register |
| 3 | DUCLK | clock DATA-OUT register |
| 4 | WACLK | clock control-store write-address (WA) register |
| 5 | BRKCLK | clock BREAK register |
| 6 | TGCLK | clock TAG-OUT register |
| 7 | CNTCLK | clock CSCNT register |
| 10 | DIEN | enable DATA-IN register to CDB bus |
| 11 | DUEN | enable DATA-OUT register (least significant) |
| 12 | WAR | read WA register |
| 13 | BRKR | read BREAK register |
| 14 | CNTR | read CSCNT register |
| 15 | RESBRK | reset break |
| 16 | DUNL | unlock |
| 17 | EOUTEN | enable data line driver |

(The field is 4 bits — the old "code 16 = OperationComplete" was not even
representable. Decimal 8/9 are the DIEN/DUEN strobes.)

### 4.2 TAG-OUT codes (5015 → 3022, driven by ND-500 microcode) — octal

| Code | Function |
|------|----------|
| 0 | read memory address register (MAR) |
| 1 | write MAR |
| 2 | read STATUS register |
| 3 | write STATUS register |
| 4 | read CONTROL register |
| 5 | reset activate |
| 6 | read DATA register (and ND-100 memory) |
| 7 | write DATA register (and then into ND-100 memory) |

Codes 6/7 are how the microcode DMAs messages out of ND-100 memory and writes
answers back. Bit 3 = "ND-100 if 0"; bit 7 = MOST (most/least half of the 32-bit
data registers).

---

## 5. 5MPM Message Buffer

### 5.1 What is 5MPM?

**5MPM = 5 Megabyte Multiport Memory** (though actual size varies)

It's shared RAM that both ND-100 and ND-500 can access. They use it to pass messages back and forth.

**Important:** The same physical memory has different addresses on each CPU:
- ND-100 sees it at: 0x00040000 (example)
- ND-500 sees it at: 0x80040000 (bit 31 set)

### 5.2 Message Buffer Layout

When ND-500 makes a monitor call, it fills out this message structure:

| Offset | Name | Size | Purpose | Who Writes |
|--------|------|------|---------|------------|
| 0 | PLINK | 1 word | Process link | System |
| 2 | N5STA | 1 word | Status flags | Both |
| 6 | MICFU | 1 word | Restart code | ND-100 |
| **9** | **STOPR** | 1 word | **Stop reason (1=monitor call)** | ND-500 |
| 9 | KFLIP | 1 word | Error flag (0=OK, 1=error) | ND-100 |
| **11** | **MCNO** | 1 word | **Monitor call number** | ND-500 |
| 11 | FUNCV | 2 words | Return value | ND-100 |
| **64** | **5AP1** | 2 words | **Input parameter 1** | ND-500 |
| 66 | 5AP2 | 2 words | Input parameter 2 | ND-500 |
| 68 | 5AP3 | 2 words | Input parameter 3 | ND-500 |
| 70 | 5AP4 | 2 words | Input parameter 4 | ND-500 |
| 65 | 5DP1 | 2 words | Output parameter 1 | ND-100 |
| 67 | 5DP2 | 2 words | Output parameter 2 | ND-100 |
| 69 | 5DP3 | 2 words | Output parameter 3 | ND-100 |
| 71 | 5DP4 | 2 words | Output parameter 4 | ND-100 |

### 5.3 Message Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    5MPM MESSAGE BUFFER                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ND-500 FILLS IN:                  ND-100 FILLS IN:                   │
│   ───────────────                   ────────────────                   │
│   MCNO = function code              FUNCV = return value               │
│   STOPR = 1 (monitor call)          KFLIP = 0 (success) or 1 (error)  │
│   5AP1 = parameter 1                5DP1 = output 1                    │
│   5AP2 = parameter 2                5DP2 = output 2                    │
│   5AP3 = parameter 3                5DP3 = output 3                    │
│   5AP4 = parameter 4                5DP4 = output 4                    │
│                                     MICFU = 3 (restart code)           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘

TIME ──────────────────────────────────────────────────────────────────►

  ND-500 (microcode)                  ND-100 (SINTRAN)
    │                                   │
    │ 1. Fill MCNO, STOPR, 5AP1-4       │
    │    (message via TAG-OUT 6/7 DMA)  │
    │ 2. N5STA := ANSWER; STATUS        │
    │    "finished" + stop reason       │
    │──── level-12 interrupt ──────────►│
    │                                   │ 3. Read RSTA5 (status)
    │                                   │ 4. Walk message queue, check N5STA
    │                                   │ 5. STOPR=MOCALL → MCHANDLE
    │                                   │ 6. Fill FUNCV, KFLIP, 5DP1-4
    │                                   │ 7. N5STA/MICFU := restart; LCON5
    │◄──── activate (CONTROL bit 2) ────│
    │ 8. Leave IDLE loop, fetch message │
    │ 9. Read results from 5MPM         │
    │ 10. Continue execution            │
    ▼                                   ▼
```

---

## 6. Complete MON 60 Signal Flow

### 6.1 High-Level Overview

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   ND-500        │     │     5MPM        │     │    ND-100       │
│   Program       │     │  (Shared RAM)   │     │   SINTRAN       │
├─────────────────┤     ├─────────────────┤     ├─────────────────┤
│                 │     │                 │     │                 │
│  MON 60 call    │────►│ Write message   │     │                 │
│                 │     │ (N5STA, STOPR)  │     │                 │
│  STATUS "fin."  │─────┼─ level-12 IRQ ──┼────►│ Interrupt!      │
│                 │     │                 │     │                 │
│  (IDLE loop)    │     │                 │     │ Read message    │◄─┐
│                 │     │                 │◄────│ (N5STA/STOPR)   │  │
│                 │     │                 │     │ Process it      │  │
│                 │     │                 │     │                 │  │
│                 │     │ Write results   │◄────│                 │  │
│                 │     │                 │     │                 │  │
│  activate       │◄────┼─────────────────┼─────│ LCON5 (bit 2)   │  │
│                 │     │                 │     │                 │  │
│  Read results   │◄────│                 │     │                 │  │
│                 │     │                 │     │                 │  │
│  Continue!      │     │                 │     │                 │  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

### 6.2 Detailed Step-by-Step

**Phase 1: ND-500 Makes Request**

```
1. ND-500 program executes MON instruction
2. ND-500 microcode:
   a. Gets message buffer address from process descriptor
   b. Writes MCNO (function code) to message buffer
   c. Writes parameters to 5AP1-5AP4
   d. Sets STOPR = MOCALL (meaning "monitor call")
   e. Writes answer status into the message (N5STA)
   f. Sets STATUS "finished" (bit 3) + stop reason (bits 10-14);
      3022 raises the level-12 interrupt if CONTROL bit 0 is set
3. ND-500 microcode returns to the IDLE loop (waits for activate)
```

**Phase 2: ND-100 Receives and Processes**

```
1. 3022 card generates Level 12 interrupt (ident 16 octal for thumbwheel 0)
2. ND-100 interrupt handler (5STDRIV, NPL:MP-P2-N500.NPL:656-694) runs:
   a. CALL CLE5STATUS              % read RSTA5, clear latched power bits
   b. Check error bits (5PAGF/5DMAER/5PFAIL/5POWOF)
   c. Scan the execution queue from MAILINK, following LINK fields
3. CHN5STATUS dispatches on each message's N5STA;
   answers go to DECOMESS, which reads STOPR
4. STOPR = MOCALL/5FMOCALL -> MCHANDLE dispatcher checks MCNO:
   - If 500-523: Handle directly (fast path)
   - Otherwise: Forward to background kernel
5. Handler executes the requested function
```

**Phase 3: ND-100 Sends Response**

```
1. Handler writes results to message buffer:
   a. FUNCV = return value
   b. 5DP1-5DP4 = output parameters
   c. KFLIP = 0 (success) or 1 (error)
   d. MICFU = restart code (24B 3MONCO = restart after monitor call)
2. Message status set back to "message to ND-500"
3. Activate the ND-500:
   T:=HDEV+LCON5; *IOXT           % CONTROL bit 2 = activate
   (see reference section 5 for the ACT50 / enable-sequence paths)
```

**Phase 4: ND-500 Resumes**

```
1. The activate wakes the microcode out of the IDLE loop
2. ND-500 microcode fetches the message (TAG-OUT 6 DMA via MAR) and:
   a. Reads FUNCV from message buffer
   b. Reads 5DP1-5DP4 output parameters
   c. Checks KFLIP for error
3. If KFLIP = 0: Skip return (success)
   If KFLIP = 1: Direct return (error in A-register)
4. ND-500 program continues
```

---

## 7. Mapping MON 60 Functions to Hardware

### 7.1 Which Functions Use Which Hardware

| MON 60 Function | Signaling | IOX Commands | 5MPM Fields |
|-----------------|-----------|--------------|-------------|
| **Read Register (0B)** | message + activate | RSTA5, LCON5 | MCNO, 5AP1, 5DP1 |
| **Write Register (1B)** | message + activate | RSTA5, LCON5 | MCNO, 5AP1, 5AP2 |
| **Read Memory (2B/3B)** | message + activate | LMAR5, LCON5 | MCNO, N500A, NRBYT |
| **Write Memory (4B/5B)** | message + activate | LMAR5, LCON5 | MCNO, N500A, NRBYT |
| **Run Program (12B)** | message + activate | LCON5 | MCNO, STOPR, FUNCV |
| **Read Control Store (23B)** | TAG-IN strobes (WACLK/CNTCLK) | LTAG5, WDAT | MCNO, 5AP1, 5AP2 |
| **Master Clear (35B)** | strobe | MCLR5 | MCNO |
| **Read Status (41B)** | direct read | RSTA5, RMAR5 | MCNO, 5DP1, 5DP2 |
| **Read Flag (100B)** | message + activate | None | MCNO, 5AP1, 5DP1 |

(All "message + activate" rows signal completion back via STATUS "finished" +
level-12 interrupt — never via TAG codes.)

### 7.2 Fast-Path vs Slow-Path Functions

**Fast-Path (MCNO 500-523):** Handled directly on Level 12 interrupt
- Very fast (~10-20 microseconds)
- Simple operations only
- Examples: Start/stop process, get error code, set priority

**Slow-Path (Other MCNO values):** Forwarded to background kernel
- Slower (~100+ microseconds)
- Complex operations
- Examples: File I/O, memory allocation, domain management

---

## 8. Quick Reference

### 8.1 TAG Direction Summary (corrected)

```
Both TAG registers are ON the 5015 (ND-500 side):
TAG-IN  = ND-100 → 5015  register strobes (control-store load, test/debug)
TAG-OUT = 5015 → 3022    3022-register commands driven by ND-500 microcode
                         (incl. DMA read/write of ND-100 memory, codes 6/7)

RTAG5 (offset 10B) = readback of tag bits (return-tag diagnostic path)
LTAG5 (offset 11B) = write a TAG-IN strobe code
Neither is used by the runtime SINTRAN driver.
```

### 8.2 Real signaling (no "TAG codes" exist)

```
ND-500 → ND-100:  message N5STA := ANSWER; STATUS "finished" (bit 3)
                  + stop reason (bits 10-14) → level-12 interrupt
                  (gated by CONTROL bit 0)
ND-100 → ND-500:  message N5STA := "message to ND-500";
                  activate via CONTROL bit 2 (LCON5)
```

### 8.3 Essential IOX Commands

```npl
% Check ND-500 status (the real "what happened" channel)
T:=HDEV+RSTA5; *IOXT         % A = status bits (finished, stop reason...)

% Activate ND-500 (the real "go" channel)
A:=5; T:=HDEV+LCON5; *IOXT   % bit 0 int-enable + bit 2 activate

% Reset ND-500
T:=HDEV+MCLR5; *IOXT         % Master clear (restarts microcode at CS addr 0)
```

---

## 9. Troubleshooting

### 9.1 Common Problems

| Problem | Likely Cause | Solution |
|---------|--------------|----------|
| ND-500 hangs after MON call | TAG-OUT never written | Check ND-100 interrupt handler |
| Wrong results | Message buffer address wrong | Verify LMAR5 setup |
| "ND-500 not present" error | RSTA5 check failed | Check power, cables, initialization |
| Timeout | BUSY bit stuck | May need MCLR5 reset |

### 9.2 Debugging Tips

1. **Check STATUS (RSTA5) first** - finished/busy/lock/stop-reason live there
2. **Verify 5MPM addresses** - ND-100 and ND-500 see different addresses!
3. **Look at STOPREASON** - Tells you why ND-500 stopped
4. **Check KFLIP** - Non-zero means error occurred

---

## 10. Related Documentation

| Document | Path | Content |
|----------|------|---------|
| MON 60 Functions | `60B_N500M_Functions.md` | All 67 subfunctions |
| MON 60 YAML | `60B_N500M.yaml` | Structured data |
| 3022 Interface | `../../../SINTRAN/ND500/ND500-IF-USAGE-DEEP-ANALYSIS.md` | Hardware details |
| TAG Mechanism | `../../../SINTRAN/ND500/MP-P2-N500.md` | Original TAG docs |
| Message Passing | `../../../SINTRAN/OS/08-MESSAGE-PASSING-DETAILED.md` | 5MPM structure |

---

## 11. Glossary

| Term | Meaning |
|------|---------|
| **3022** | Interface card in ND-100 that connects to ND-500 |
| **5015** | Interface card in ND-500 that connects to ND-100 |
| **5MPM** | Shared memory accessible by both CPUs |
| **HDEV** | Base device address for 3022 card |
| **IOX** | ND-100 I/O instruction for device communication |
| **Level 12** | Interrupt priority level for ND-500 communication |
| **MCNO** | Monitor Call Number (function code) |
| **MON 60** | Monitor call for ND-500 control |
| **NPL** | Norsk Data Programming Language (like C) |
| **TAG-IN** | 5015 register: strobe codes written by ND-100 (CS load / test only) |
| **TAG-OUT** | 5015 register: 3022-command codes driven by ND-500 microcode (incl. DMA) |

---

*Document created: 2025-02-05*
*For newcomers to Norsk Data systems*
