# MON 60 (N500M) - Hardware Interface Mapping Analysis

**Purpose:** Document the relationship between MON 60 subfunctions and the ND-500 bus interface
**Audience:** Developers new to Norsk Data systems
**Date:** 2025-02-05

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
2. **TAG Registers** - Hardware signal lines to get each other's attention
3. **3022/5015 Interface Cards** - The physical hardware connecting them

**Analogy:** Think of it like two people in separate rooms:
- The **5MPM** is a shared whiteboard where they write messages
- The **TAG registers** are doorbells to say "I wrote something, come read it!"

### 1.3 What is MON 60?

**MON 60 (N500M)** is a SINTRAN III monitor call that lets ND-100 programs control the ND-500. It provides 67 subfunctions for:
- Reading/writing ND-500 memory
- Starting/stopping ND-500 programs
- Managing ND-500 processes
- Loading microcode
- Debugging

---

## 2. Understanding TAG-IN and TAG-OUT (Critical!)

### 2.1 The Confusing Names Explained

The names "TAG-IN" and "TAG-OUT" confuse everyone at first. Here's the key:

**The names are from the 3022 interface card's perspective (which sits in the ND-100).**

Think of the 3022 card as a "mailbox" sitting in the ND-100:
- **TAG-IN** = mail coming **IN** to the mailbox (from ND-500)
- **TAG-OUT** = mail going **OUT** of the mailbox (to ND-500)

```
                        ┌─────────────────────────────────┐
                        │     3022 Interface Card         │
                        │     (sits in ND-100)            │
                        │                                 │
     ND-500 ──────────► │  ┌─────────┐                   │
     writes this        │  │ TAG-IN  │  "IN" = coming    │
                        │  │         │  INTO the 3022    │
                        │  └─────────┘  FROM ND-500      │
                        │                                 │
                        │  ┌─────────┐                   │
     ND-500 ◄────────── │  │ TAG-OUT │  "OUT" = going    │
     reads this         │  │         │  OUT of the 3022  │
                        │  └─────────┘  TO ND-500        │
                        │                                 │
                        └─────────────────────────────────┘
```

### 2.2 Clear Direction Table

| Register | Who Writes It | Who Reads It | Data Flows | Used For |
|----------|---------------|--------------|------------|----------|
| **TAG-IN** | ND-500 | ND-100 | ND-500 → ND-100 | ND-500 sends requests |
| **TAG-OUT** | ND-100 | ND-500 | ND-100 → ND-500 | ND-100 sends responses |

### 2.3 IOX Commands (How ND-100 Accesses TAGs)

The ND-100 uses IOX instructions to access the TAG registers:

| IOX Command | Offset | What It Does | English Meaning |
|-------------|--------|--------------|-----------------|
| **RTAG5** | +8 | **R**ead TAG-**IN** | "Read what ND-500 sent me" |
| **LTAG5** | +9 | **L**oad TAG-**OUT** | "Send this to ND-500" |

**Code Example (NPL):**
```npl
% ND-100 reads what ND-500 sent
T:=HDEV+RTAG5; *IOXT          % Execute IOX at device+8
A =: REQUEST                   % A register now contains ND-500's message

% ND-100 sends response to ND-500
A := 16                        % 16 = "operation complete" code
T:=HDEV+LTAG5; *IOXT          % Execute IOX at device+9, sends A to ND-500
```

### 2.4 Complete Communication Example

Here's a step-by-step example of an ND-500 program asking the ND-100 to read a file:

```
STEP 1: ND-500 Prepares Request
─────────────────────────────────────────────────────────────────────────
   ND-500 writes to shared memory (5MPM):
   - "I want to read a file"
   - "The filename is HERE"
   - "Put the data THERE"

   ND-500 writes to its TAG register:
   - Code 8 = "I have a monitor call request"
   - Process number = 3 (my process ID)

   Result: TAG-IN register now contains 0x0308
           (code 8 in low byte, process 3 in next nibble)

STEP 2: ND-100 Receives Request
─────────────────────────────────────────────────────────────────────────
   Hardware generates Level 12 interrupt on ND-100

   ND-100 interrupt handler runs:
   T:=HDEV+RTAG5; *IOXT        % Read TAG-IN
   A now = 0x0308

   ND-100 extracts:
   - Code = 8 (monitor call request)
   - Process = 3

   ND-100 reads shared memory to get full request details

STEP 3: ND-100 Processes Request
─────────────────────────────────────────────────────────────────────────
   ND-100 reads the file from disk
   ND-100 copies file data to shared memory (5MPM)
   ND-100 writes result status to shared memory

STEP 4: ND-100 Signals Completion
─────────────────────────────────────────────────────────────────────────
   A := 16                      % Code 16 = "operation complete"
   T:=HDEV+LTAG5; *IOXT        % Write TAG-OUT

   Result: TAG-OUT register now contains 16
           ND-500 sees this and knows to read results

STEP 5: ND-500 Continues
─────────────────────────────────────────────────────────────────────────
   ND-500 reads TAG-OUT, sees code 16
   ND-500 reads results from shared memory
   ND-500 program continues execution
```

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
| **+8** | **010** | **RTAG5** | **Read** | **Read TAG-IN** | **"What did ND-500 send?"** |
| **+9** | **011** | **LTAG5** | **Write** | **Write TAG-OUT** | **"Send this to ND-500"** |

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

## 4. TAG Command Codes

### 4.1 TAG-IN Codes (ND-500 → ND-100)

These are the codes ND-500 sends to request things from ND-100:

| Code | Name | Meaning | MON 60? |
|------|------|---------|---------|
| 1 | DMARead | "I need you to DMA read for me" | No |
| 2 | DMAWrite | "I need you to DMA write for me" | No |
| 3 | ClearInterrupt | "Clear my interrupt" | No |
| **8** | **MonitorCallRequest** | **"I have a monitor call!"** | **Yes** |
| 9 | PageFaultRequest | "I had a page fault!" | Related |

### 4.2 TAG-OUT Codes (ND-100 → ND-500)

These are the codes ND-100 sends back to ND-500:

| Code | Name | Meaning |
|------|------|---------|
| 1 | DMAComplete | "DMA is done" |
| 2 | InterruptAck | "I got your interrupt" |
| **16** | **OperationComplete** | **"Your monitor call is done!"** |
| 17 | ActivateProcess | "You can run now" |

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

  ND-500                              ND-100
    │                                   │
    │ 1. Fill MCNO, STOPR, 5AP1-4      │
    │ 2. Write TAG-IN = 8              │
    │──────────────────────────────────►│
    │                                   │ 3. Read TAG-IN
    │                                   │ 4. Read message from 5MPM
    │                                   │ 5. Process request
    │                                   │ 6. Fill FUNCV, KFLIP, 5DP1-4
    │                                   │ 7. Write TAG-OUT = 16
    │◄──────────────────────────────────│
    │ 8. Read TAG-OUT                   │
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
│                 │     │                 │     │                 │
│  TAG-IN = 8     │─────┼─────────────────┼────►│ Interrupt!      │
│                 │     │                 │     │                 │
│  (waiting...)   │     │                 │     │ Read message    │◄─┐
│                 │     │                 │◄────│                 │  │
│                 │     │                 │     │ Process it      │  │
│                 │     │                 │     │                 │  │
│                 │     │ Write results   │◄────│                 │  │
│                 │     │                 │     │                 │  │
│  TAG-OUT = 16   │◄────┼─────────────────┼─────│ TAG-OUT = 16    │  │
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
   d. Sets STOPR = 1 (meaning "monitor call")
   e. Sets 5ITMQUEUE flag in status
   f. Writes to 5015 TAG register: code 8 + process number
3. ND-500 enters wait state
```

**Phase 2: ND-100 Receives and Processes**

```
1. 3022 card generates Level 12 interrupt
2. ND-100 interrupt handler (5STDRIV) runs:
   a. T:=HDEV+RTAG5; *IOXT        % Read TAG-IN
   b. Extract process number (bits 8-11)
   c. Calculate message buffer address
3. DECOMESS routine reads message from 5MPM
4. MCHANDEL dispatcher checks MCNO:
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
   d. MICFU = 3 (restart code = 3MONCO)
2. Clear 5ITMQUEUE flag
3. Write TAG-OUT:
   a. A := 16                    % Operation complete
   b. T:=HDEV+LTAG5; *IOXT      % Send to ND-500
```

**Phase 4: ND-500 Resumes**

```
1. ND-500 sees TAG-OUT = 16
2. ND-500 microcode:
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

| MON 60 Function | TAG Codes Used | IOX Commands | 5MPM Fields |
|-----------------|----------------|--------------|-------------|
| **Read Register (0B)** | 8 → 16 | RSTA5 | MCNO, 5AP1, 5DP1 |
| **Write Register (1B)** | 8 → 16 | LSTA5 | MCNO, 5AP1, 5AP2 |
| **Read Memory (2B/3B)** | 8 → 1 → 16 | DMA sequence | MCNO, N500A, NRBYT |
| **Write Memory (4B/5B)** | 8 → 2 → 16 | DMA sequence | MCNO, N500A, NRBYT |
| **Run Program (12B)** | 8 → 16 | LCON5 (5) | MCNO, STOPR, FUNCV |
| **Read Control Store (23B)** | 8 → 16 | CS read | MCNO, 5AP1, 5AP2 |
| **Master Clear (35B)** | 8 → 16 | MCLR5 | MCNO |
| **Read Status (41B)** | 8 → 16 | RSTA5, RMAR5 | MCNO, 5DP1, 5DP2 |
| **Read Flag (100B)** | 8 → 16 | None | MCNO, 5AP1, 5DP1 |

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

### 8.1 TAG Direction Summary

```
TAG-IN  = ND-500 → ND-100  (ND-500 sends requests)
TAG-OUT = ND-100 → ND-500  (ND-100 sends responses)

RTAG5 (+8) = ND-100 reads TAG-IN  = "What did ND-500 say?"
LTAG5 (+9) = ND-100 writes TAG-OUT = "Tell ND-500 this"
```

### 8.2 Common TAG Codes

```
ND-500 sends (TAG-IN):     ND-100 sends (TAG-OUT):
  8 = Monitor call           16 = Operation complete
  9 = Page fault             17 = Activate process
  1 = DMA read request        1 = DMA complete
  2 = DMA write request       2 = Interrupt ack
```

### 8.3 Essential IOX Commands

```npl
% Read what ND-500 sent
T:=HDEV+RTAG5; *IOXT         % A = TAG-IN value

% Send response to ND-500
A:=16; T:=HDEV+LTAG5; *IOXT  % TAG-OUT = 16

% Check ND-500 status
T:=HDEV+RSTA5; *IOXT         % A = status bits

% Activate ND-500
A:=5; T:=HDEV+LCON5; *IOXT   % Start ND-500 running

% Reset ND-500
T:=HDEV+MCLR5; *IOXT         % Master clear
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

1. **Check TAG registers first** - Most problems are TAG signaling issues
2. **Verify 5MPM addresses** - ND-100 and ND-500 see different addresses!
3. **Look at STOPREASON** - Tells you why ND-500 stopped
4. **Check KFLIP** - Non-zero means error occurred

---

## 10. Related Documentation

| Document | Path | Content |
|----------|------|---------|
| MON 60 Functions | `E:\Dev\Ronny\NDInsight\Developer\MON\calls\60B_N500M_Functions.md` | All 67 subfunctions |
| MON 60 YAML | `E:\Dev\Ronny\NDInsight\Developer\MON\calls\60B_N500M.yaml` | Structured data |
| 3022 Interface | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-IF-USAGE-DEEP-ANALYSIS.md` | Hardware details |
| TAG Mechanism | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\MP-P2-N500.md` | Original TAG docs |
| Message Passing | `E:\Dev\Ronny\NDInsight\SINTRAN\OS\08-MESSAGE-PASSING-DETAILED.md` | 5MPM structure |

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
| **TAG-IN** | Register for ND-500 to ND-100 signals |
| **TAG-OUT** | Register for ND-100 to ND-500 signals |

---

*Document created: 2025-02-05*
*For newcomers to Norsk Data systems*
