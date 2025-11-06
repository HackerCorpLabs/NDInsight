# ND-500 Boot Detection and Initialization Mechanism

**Exact Hardware Detection Process During SINTRAN Boot**

**Version:** 1.0
**Last Updated:** November 6, 2025
**Primary Source Files:**
- `PH-P2-OPPSTART.NPL` (SINTR boot routine)
- `MP-P2-N500.NPL` (ND-500 monitor level routines)
- `RP-P2-N500.NPL` (RT-program level routines)
- Hardware documentation: ND-10.004.01 (MPM5), ND-06.014 (3022 Interface)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Hardware Components](#2-hardware-components)
3. [Detection Sequence](#3-detection-sequence)
4. [DETECTND500 - Hardware Detection](#4-detectnd500---hardware-detection)
5. [INIT5MPM - Multiport Memory Initialization](#5-init5mpm---multiport-memory-initialization)
6. [INIT5PROCS - Process Table Initialization](#6-init5procs---process-table-initialization)
7. [LOAD5XMSG - XMSG Kernel Loading](#7-load5xmsg---xmsg-kernel-loading)
8. [Register Reference](#8-register-reference)
9. [Emulator Implementation](#9-emulator-implementation)

---

## 1. Overview

### 1.1 What Happens During Boot

When SINTRAN III boots on an ND-100 system with an optional ND-500 coprocessor, it must detect the presence of the ND-500 hardware and initialize the shared multiport memory (5MPM) system.

**Boot Sequence:**

```
ND-100 Boot
    ↓
SYSEVAL (Detect ND-100/110/120 CPU type)
    ↓
DETECTND500 ← YOU ARE HERE
    ↓
┌─────────────┐
│ ND-500      │
│ Present?    │
└──┬──────┬───┘
   │      │
  YES    NO
   │      │
   ↓      └─→ Continue (ND-100 only mode)
INIT5MPM
   ↓
INIT5PROCS
   ↓
LOAD5XMSG
   ↓
ND-500 Ready
```

### 1.2 Key Questions Answered

This document answers:

1. **How** does SINTRAN detect if ND-500 hardware is present?
2. **What** hardware registers are checked?
3. **What** values indicate "present" vs "not present"?
4. **What** initialization happens if ND-500 is detected?
5. **How** should an emulator implement this detection?

---

## 2. Hardware Components

### 2.1 Physical Hardware

```
┌─────────────────────────────────────────────────────────────────┐
│                        System Configuration                      │
└─────────────────────────────────────────────────────────────────┘

┌──────────────┐                              ┌──────────────┐
│   ND-100     │                              │   ND-500     │
│   Chassis    │                              │   Chassis    │
│              │                              │              │
│  ┌────────┐  │                              │  ┌────────┐  │
│  │ ND-100 │  │                              │  │ ND-500 │  │
│  │  CPU   │  │                              │  │  CPU   │  │
│  └────────┘  │                              │  └────────┘  │
│      │       │                              │      │       │
│  ┌────────┐  │      ┌──────────────┐       │  ┌────────┐  │
│  │  3022  │◄─┼──────┤ MPM5 Module  ├───────┼─►│  5015  │  │
│  │ I/F Card│  │      │ (Separate!)  │       │  │ I/F Card│  │
│  └────────┘  │      └──────────────┘       │  └────────┘  │
│      │       │                              │              │
│  ┌────────┐  │                              │              │
│  │ ND-100 │  │                              │              │
│  │  RAM   │  │                              │              │
│  └────────┘  │                              │              │
└──────────────┘                              └──────────────┘
```

**Key Components:**

| Component | Location | Purpose |
|-----------|----------|---------|
| **ND-100 CPU** | ND-100 Chassis | Control processor, runs SINTRAN |
| **3022 Interface** | ND-100 Chassis | Connection to MPM5, IOX device |
| **MPM5 Module** | **Separate Box** | Shared multiport memory hardware |
| **5015 Interface** | ND-500 Chassis | Connection to MPM5 |
| **ND-500 CPU** | ND-500 Chassis | Computation coprocessor |

### 2.2 3022 Interface Card

The **3022 Interface Card** is installed in the ND-100 chassis and provides:

1. **Hardware device** accessible via IOX instructions
2. **Registers** for control, status, and data transfer
3. **Interrupt capability** (typically level 12)
4. **DMA controller** for 5MPM access

**Device Number:**
- **HDEV**: Hardware device base address (from system configuration)
- Typically: `100₈` to `120₈` (octal), varies by system
- All IOX operations use: `T:=HDEV+offset; *IOXT`

---

## 3. Detection Sequence

### 3.1 Boot Flow in SINTR Routine

**Location:** `PH-P2-OPPSTART.NPL`, SINTR routine (early boot)

```npl
% From PH-P2-OPPSTART.NPL, SINTR boot sequence

SINTR:
   % ... earlier boot code ...

   % Detect ND-100/110/120 CPU type
   CALL SYSEVAL

   % Set CPU timing constants based on detected type
   % (CPULOOPTIME, LPDELAY)

   % Detect ND-500 coprocessor
   CALL DETECTND500

   % Check result
   IF ND500PRESENT THEN
      % ND-500 detected! Initialize it.
      CALL INIT5MPM          % Allocate multiport memory
      CALL INIT5PROCS        % Initialize process table
      CALL LOAD5XMSG         % Load XMSG kernel

      % Calculate max processes
      A:="S500E"-"S500S"=:D:=0
      T:=5PRDSIZE
      *RDIV ST               % A := (S500E - S500S) / 5PRDSIZE
      A=:MX5PROCS           % Store max ND-500 processes
   ELSE
      % No ND-500, continue in ND-100-only mode
      0=:MX5PROCS
   FI

   % ... continue boot ...
```

### 3.2 Global Variables Set

After detection, these global variables are set:

| Variable | Type | Value if Present | Value if Absent |
|----------|------|------------------|-----------------|
| **ND500PRESENT** | Boolean | TRUE (≠0) | FALSE (0) |
| **MX5PROCS** | Integer | 8-16 (calculated) | 0 |
| **HDEV** | Integer | Device number (e.g., 100₈) | N/A |
| **ADRZERO** | Integer | 5MPM physical address | N/A |
| **5MBBANK** | Integer | 5MPM bank number | N/A |

---

## 4. DETECTND500 - Hardware Detection

### 4.1 Purpose

**DETECTND500** checks if:
1. 3022 interface card is installed
2. 5015 interface card responds
3. ND-500 CPU is powered on and functional

### 4.2 Implementation

```npl
% Pseudo-code for DETECTND500 routine
% Based on MP-P2-N500.NPL and hardware documentation

DETECTND500:
   % Save registers
   SAVEA:=A; SAVET:=T; SAVEX:=X

   % Get hardware device number from system configuration
   % This is set during SINTRAN generation based on hardware
   HDEV:="N500DF".HWDEVICE           % Typically 100₈ - 120₈

   % If HDEV not configured, no ND-500
   IF HDEV=0 THEN
      ND500PRESENT:=FALSE
      GO EXIT
   FI

   % Step 1: Master Clear (Initialize interface)
   % This resets the 3022 interface card
   T:=HDEV+MCLR5                     % MCLR5 = 6 (Master Clear register)
   *IOXT                             % Execute IOX

   % Step 2: Read Status Register
   % This tests if the 3022 interface responds
   A:=200                            % Set up illegal instruction trap
   *TRR IIE                          % Enable trap on IOX error
   TRA IIC                           % Clear trap flag

   T:=HDEV+RSTA5                     % RSTA5 = 2 (Read Status register)
   *IOXT                             % Read into A register

   TRA IIC                           % Check if trap occurred
   IF A=0 THEN                       % A=0 means trap occurred
      % IOX failed - no 3022 interface card
      ND500PRESENT:=FALSE
      A:=0; *TRR IIE                 % Disable trap
      GO EXIT
   FI

   % Step 3: IOX succeeded, check status value
   % A now contains RSTA5 value
   A=:STATUS

   % Check for error conditions
   IF STATUS BIT 5DMAER THEN         % Bit 6: DMA error
      ND500PRESENT:=FALSE
      GO EXIT
   FI

   IF STATUS BIT 5PFAIL THEN         % Bit 5: Power fail
      ND500PRESENT:=FALSE
      GO EXIT
   FI

   IF STATUS BIT 5CLOST THEN         % Bit 7: Clock stopped
      ND500PRESENT:=FALSE
      GO EXIT
   FI

   % Step 4: Check if 5015/ND-500 responds
   % Try to read control register
   T:=HDEV+RCON5                     % RCON5 = 4 (Read Control)
   *IOXT

   % If we got here, 3022 interface is present and responding
   % Status has no error bits set
   ND500PRESENT:=TRUE

   % Store hardware device number globally
   "N500DF".HWDEVICE:=HDEV

EXIT:
   % Restore registers
   A:=SAVEA; T:=SAVET; X:=SAVEX
   A:=0; *TRR IIE                    % Disable trap
```

### 4.3 Detection Logic Summary

**Test Sequence:**

1. **Check HDEV configured** → If 0, no ND-500
2. **Master Clear (MCLR5)** → Initialize interface
3. **Read Status (RSTA5)** → Test if 3022 responds
   - If IOX traps (illegal instruction) → No 3022 card
   - If IOX succeeds → Continue
4. **Check Status Bits** → Verify no errors
   - Bit 6 (5DMAER) set → DMA error, fail
   - Bit 5 (5PFAIL) set → Power fail, fail
   - Bit 7 (5CLOST) set → Clock stopped, fail
5. **Read Control (RCON5)** → Test if 5015 responds
6. **All tests pass** → `ND500PRESENT := TRUE`

### 4.4 What Happens in Emulator

**Emulator Implementation:**

```csharp
// Emulator pseudocode for IOX handler

public class ND100Emulator
{
    private ND500Coprocessor _nd500;  // null if no ND-500
    private ushort _nd500DeviceNumber = 0x40;  // 100₈ octal

    public void ExecuteIOXT(ushort deviceOffset)
    {
        ushort device = (ushort)(T_Register >> 8);      // High byte of T
        ushort offset = (ushort)(T_Register & 0xFF);    // Low byte of T

        // Check if this is ND-500 device (3022 interface)
        if (device == _nd500DeviceNumber)
        {
            HandleND500IOX(offset);
        }
        else
        {
            // Other devices...
        }
    }

    private void HandleND500IOX(ushort offset)
    {
        // If no ND-500 configured, trap immediately
        if (_nd500 == null)
        {
            // Illegal instruction trap
            TriggerTrap(TrapType.IllegalInstruction);
            return;
        }

        switch (offset)
        {
            case 0x02:  // RSTA5 - Read Status
                A_Register = _nd500.ReadStatusRegister();
                break;

            case 0x04:  // RCON5 - Read Control
                A_Register = _nd500.ReadControlRegister();
                break;

            case 0x06:  // MCLR5 - Master Clear
                _nd500.MasterClear();
                break;

            // ... other registers ...
        }
    }
}

public class ND500Coprocessor
{
    private bool _powerOn = true;
    private bool _dmaError = false;

    public ushort ReadStatusRegister()
    {
        ushort status = 0;

        if (_powerOn)
            status |= 0x0008;  // Bit 3: 5ALIVE

        if (_dmaError)
            status |= 0x0040;  // Bit 6: 5DMAER

        // Other status bits...

        return status;
    }

    public ushort ReadControlRegister()
    {
        // Return current control settings
        return _controlReg;
    }

    public void MasterClear()
    {
        // Reset interface
        _controlReg = 0;
        _statusReg = 0x0008;  // Set 5ALIVE bit
    }
}
```

---

## 5. INIT5MPM - Multiport Memory Initialization

### 5.1 Purpose

**INIT5MPM** allocates and configures the shared multiport memory (5MPM) region.

**What it does:**
1. Calculates required 5MPM size
2. Allocates contiguous physical pages from ND-100 memory
3. Configures MPM5 hardware address windows
4. Clears all 5MPM memory
5. Sets up structure pointers (process table, message buffers)

### 5.2 Size Calculation

```npl
% Calculate 5MPM size needed

SIZE := 5PRDSIZE * MX5PROCS +        % Process descriptors (6-10 words each)
        55MESSIZE * MX5PROCS +       % Message buffers (128 words each)
        XMSGKERNELSIZE +             % XMSG kernel code (~2KB)
        SHAREDATASIZE                % Shared data area (~1KB)

% Example calculation for 8 processes:
%   5PRDSIZE = 10₈ (8 decimal) words
%   55MESSIZE = 200₈ (128 decimal) words
%   MX5PROCS = 10₈ (8 decimal)
%
%   Total = 8*8 + 8*128 + 2048 + 1024
%         = 64 + 1024 + 2048 + 1024
%         = 4160 words = 8320 bytes = ~17 pages

% Round up to page boundary (512 words = 1024 bytes per page)
PAGES := (SIZE + 511) / 512

% Typical result: 16-32 pages (16KB - 32KB)
```

### 5.3 Implementation

```npl
% Pseudo-code for INIT5MPM routine

INIT5MPM:
   % Calculate size needed (as above)
   SIZE:=5PRDSIZE * MX5PROCS +
         55MESSIZE * MX5PROCS +
         XMSGKERNELSIZE +
         SHAREDATASIZE

   % Round up to page boundary
   PAGES:=(SIZE + 511) / 512

   % Allocate contiguous physical pages
   % This reserves a block of ND-100 physical RAM
   CALL ALLOCPHYSPAGES(PAGES, FIRSTPAGE)

   IF FIRSTPAGE=0 THEN
      % Out of memory!
      CALL ERRFATAL("Cannot allocate 5MPM")
   FI

   % Calculate physical byte address
   ADRZERO:=FIRSTPAGE * 512         % 512 words per page

   % Calculate bank number for bank registers
   5MBBANK:=FIRSTPAGE / 256         % 256 pages per bank

   % Save in global structure
   "N500DF".ADRZERO:=ADRZERO
   "N500DF".5MBBANK:=5MBBANK
   "N500DF".5MPMSIZE:=SIZE
   "N500DF".5MPMPAGES:=PAGES

   % Clear all 5MPM memory
   T:=5MBBANK                       % Select 5MPM bank
   X:=0                             % Start at offset 0

   DO I:=0 TO SIZE-1
      A:=0                          % Zero
      *IOXT X+I                     % Write to 5MPM[X+I]
   OD

   % Calculate structure base addresses (offsets in 5MPM)
   "S500S":=0                                        % Process table at start
   "S500E":=5PRDSIZE * MX5PROCS                    % End of process table
   "MSGBUFFPOOL":="S500E"                          % Message buffers next
   "XMSGBASE":="MSGBUFFPOOL" + (55MESSIZE * MX5PROCS)  % XMSG kernel after
   "SHAREDATABASE":="XMSGBASE" + XMSGKERNELSIZE    % Shared data at end

   % Mark 5MPM pages as "bypass cache" in ND-100 MMU
   % This is CRITICAL for cache coherency!
   DO PAGENUM:=FIRSTPAGE TO FIRSTPAGE+PAGES-1
      CALL SETPAGEFLAGS(PAGENUM, BYPASS_CACHE)
   OD

   % Configure 3022 interface hardware
   CALL CONFIG3022
```

### 5.4 CONFIG3022 - Hardware Configuration

```npl
% Configure 3022 interface card registers

CONFIG3022:
   HDEV:="N500DF".HWDEVICE

   % Master clear (already done in DETECTND500, but do again)
   T:=HDEV+MCLR5; *IOXT

   % Set ADRZERO (5MPM base address) in interface registers
   % This tells the 3022 where 5MPM is in ND-100 physical memory

   % Write high word of address
   A:=ADRZERO SHZ -16               % Get high 16 bits
   T:=HDEV+LMAR5                    % LMAR5 = 1 (Load MAR)
   *IOXT

   % Write low word of address
   A:=ADRZERO/\177777               % Mask to 16 bits
   T:=HDEV+LDAT5                    % LDAT5 = 13 (Load Data)
   *IOXT

   % Enable interrupts on level 12
   A:=10                            % Enable bit
   T:=HDEV+LCON5                    % LCON5 = 5 (Load Control)
   *IOXT

   % Read back status to verify
   T:=HDEV+RSTA5; *IOXT

   IF A BIT 5DMAER OR A BIT 5PAGF THEN
      CALL ERRFATAL("ND-500 interface error after config")
   FI
```

### 5.5 Memory Layout After INIT5MPM

```
5MPM Physical Layout (in ND-100 RAM at ADRZERO)
┌─────────────────────────────────────┐ ADRZERO + 0
│ Process Descriptor 0                │ (S500S)
│   (5PRDSIZE words, typically 8)     │
├─────────────────────────────────────┤
│ Process Descriptor 1                │
│   (5PRDSIZE words)                  │
├─────────────────────────────────────┤
│ Process Descriptor 2                │
│   ...                               │
├─────────────────────────────────────┤
│ Process Descriptor (MX5PROCS-1)     │
├─────────────────────────────────────┤ S500E
│ Message Buffer 0                    │ (MSGBUFFPOOL)
│   (55MESSIZE words, typically 128)  │
├─────────────────────────────────────┤
│ Message Buffer 1                    │
│   (55MESSIZE words)                 │
├─────────────────────────────────────┤
│ Message Buffer 2                    │
│   ...                               │
├─────────────────────────────────────┤
│ Message Buffer (MX5PROCS-1)         │
├─────────────────────────────────────┤ XMSGBASE
│ XMSG Kernel Code                    │
│   (~2KB of ND-500 machine code)     │
├─────────────────────────────────────┤ SHAREDATABASE
│ Shared Data Area                    │
│   (Global variables, locks, queues) │
└─────────────────────────────────────┘ ADRZERO + SIZE
```

---

## 6. INIT5PROCS - Process Table Initialization

### 6.1 Purpose

**INIT5PROCS** initializes all process descriptor slots and message buffers in 5MPM.

### 6.2 Implementation

```npl
% Initialize all process descriptors

INIT5PROCS:
   T:=5MBBANK                       % Select 5MPM bank

   % Initialize each process descriptor slot
   DO PROCNUM:=0 TO MX5PROCS-1
      % Calculate descriptor address in 5MPM
      PROCADDR:="S500S" + (PROCNUM * 5PRDSIZE)

      % Calculate message buffer address
      MSGADDR:="MSGBUFFPOOL" + (PROCNUM * 55MESSIZE)

      % Write process descriptor
      X:=PROCADDR

      PROCADDR;        *IOXT X+0    % XADPROC (self-pointer)
      MSGADDR;         *IOXT X+1    % MESSBUFF (message buffer address)
      0;               *IOXT X+2    % STATUS (inactive)
      0;               *IOXT X+3    % SENDE (send disabled)
      0;               *IOXT X+4    % RECE (receive disabled)
      0;               *IOXT X+5    % 5MSFL (no flags)
      100;             *IOXT X+6    % 5PRIO (default priority)
      0;               *IOXT X+7    % 5RTCODE (no RT code)

      % Clear rest of descriptor
      DO OFFSET:=10₈ TO 5PRDSIZE-1
         0; *IOXT X+OFFSET
      OD

      % Initialize message buffer
      X:=MSGADDR

      0; *IOXT X+0     % PLINK (no next message)
      0; *IOXT X+1     % 5MSFL (flags)
      100; *IOXT X+2   % 5PRIO (priority)
      0; *IOXT X+3     % MICFU (function code)
      0; *IOXT X+4     % 5ERRC (error code)

      % Clear double-word fields
      0; *IOXT X+5     % TODF high
      0; *IOXT X+6     % TODF low
      0; *IOXT X+7     % NRBYT high
      0; *IOXT X+8     % NRBYT low
      0; *IOXT X+9     % N500A high
      0; *IOXT X+10    % N500A low
      0; *IOXT X+11    % N100A high
      0; *IOXT X+12    % N100A low

      % Clear extended message area
      DO OFFSET:=13 TO 55MESSIZE-1
         0; *IOXT X+OFFSET
      OD
   OD

   % Initialize free process queue
   0=:"5FREEPROC"                   % No free processes yet
```

### 6.3 Process Descriptor Structure

```c
// C structure equivalent of process descriptor

struct ND500ProcessDescriptor {
    uint16_t XADPROC;       // +0: Self-pointer (descriptor address)
    uint16_t MESSBUFF;      // +1: Message buffer address
    uint16_t STATUS;        // +2: Status flags
    uint16_t SENDE;         // +3: Send enable (0=inactive)
    uint16_t RECE;          // +4: Receive enable
    uint16_t FLAGS_5MSFL;   // +5: Message flags
    uint16_t PRIO_5PRIO;    // +6: Priority
    uint16_t RTCODE_5RTCODE;// +7: RT code number
    // ... additional fields ...
};
```

### 6.4 Message Buffer Structure

```c
// C structure equivalent of message buffer

struct ND500MessageBuffer {
    uint16_t PLINK;         // +0: Link to next message
    uint16_t FLAGS_5MSFL;   // +1: Message flags
    uint16_t PRIO_5PRIO;    // +2: Priority
    uint16_t MICFU;         // +3: Microcode function code
    uint16_t ERRC_5ERRC;    // +4: Error code

    // Double-word fields (ND-500 uses 32-bit values)
    uint16_t TODF_HIGH;     // +5: File table offset descriptor (high)
    uint16_t TODF_LOW;      // +6: File table offset descriptor (low)
    uint16_t NRBYT_HIGH;    // +7: Number of bytes (high)
    uint16_t NRBYT_LOW;     // +8: Number of bytes (low)
    uint16_t N500A_HIGH;    // +9: ND-500 address (high)
    uint16_t N500A_LOW;     // +10: ND-500 address (low)
    uint16_t N100A_HIGH;    // +11: ND-100 address (high)
    uint16_t N100A_LOW;     // +12: ND-100 address (low)

    // Extended area for I/O parameters
    uint16_t extended[115]; // +13 to +127: Extended I/O data
};
```

---

## 7. LOAD5XMSG - XMSG Kernel Loading

### 7.1 Purpose

**LOAD5XMSG** loads the XMSG kernel (ND-500 communication handler) into 5MPM.

**XMSG** is a small ND-500 program that:
1. Handles ND-500 → ND-100 monitor calls
2. Processes page faults
3. Manages I/O requests
4. Coordinates with ND-100 interrupt handlers

### 7.2 Implementation

```npl
% Load XMSG kernel into 5MPM

LOAD5XMSG:
   % XMSG is stored in a SINTRAN segment :XMSG-KERNEL
   % We need to copy it into 5MPM at XMSGBASE

   % Open XMSG segment
   CALL OPENSEGMENT(":XMSG-KERNEL", SEGADDR, SEGSIZE)

   IF SEGADDR=0 THEN
      CALL ERRFATAL("Cannot open :XMSG-KERNEL segment")
   FI

   % Copy from ND-100 segment to 5MPM
   T:=5MBBANK                       % Select 5MPM bank
   X:="XMSGBASE"                    % Destination in 5MPM
   Y:=SEGADDR                       % Source in ND-100 RAM

   DO I:=0 TO SEGSIZE-1
      A:=Y(I)                       % Read from segment
      *IOXT X+I                     % Write to 5MPM
   OD

   % Set entry point for ND-500
   % The ND-500 will start execution at this address
   "N500DF".XMSGENTRY:="XMSGBASE"

   % Configure ND-500 to start XMSG on first activation
   % (This happens later when first domain is placed)
```

### 7.3 XMSG Kernel Entry Points

**XMSG provides these entry points (ND-500 code):**

```assembly
; XMSG Kernel Entry Points (ND-500 Assembly)

XMSGBASE:
        ; Main entry point
        JUMP    XMSG_INIT

XMSG_MONCALL:
        ; Monitor call handler
        ; Called when ND-500 executes CALLG #0x1F000000
        ; (segment 31 trap)

XMSG_PAGEFAULT:
        ; Page fault handler
        ; Called when ND-500 accesses non-present page

XMSG_IO:
        ; I/O request handler
        ; Called for file I/O operations

XMSG_ACTIVATE:
        ; Process activation handler
        ; Called when ND-100 activates an ND-500 process
```

### 7.4 After LOAD5XMSG

At this point:

✅ ND-500 hardware detected
✅ 5MPM allocated and cleared
✅ Process descriptors initialized
✅ Message buffers cleared
✅ XMSG kernel loaded
✅ ND-500 ready for first domain

**Next step:** User can issue `@ND-500 MYPROGRAM` to place and run a domain.

---

## 8. Register Reference

### 8.1 3022 Interface Registers

**All accessed via:** `T:=HDEV+offset; *IOXT`

| Offset | Octal | Symbol | Name | Direction | Description |
|:------:|:-----:|--------|------|-----------|-------------|
| +0 | 000 | RMAR5 | Read MAR | Read | Read Memory Address Register |
| +1 | 001 | LMAR5 | Load MAR | Write | Load Memory Address Register (5MPM base) |
| +2 | 002 | **RSTA5** | **Read Status** | **Read** | **Status register (detection!)** |
| +3 | 003 | LSTA5 | Load Status | Write | Load Status register |
| +4 | 004 | RCON5 | Read Control | Read | Read Control register |
| +5 | 005 | LCON5 | Load Control | Write | Load Control register (enable interrupts) |
| +6 | 006 | MCLR5 | Master Clear | Write | Reset interface |
| +7 | 007 | TERM5 | Terminate | Write | Terminate ND-500 process |
| +10 | 010 | RTAG5 | Read Tag | Read | Read TAG-IN register |
| +11 | 011 | LTAG5 | Load Tag | Write | Write TAG-OUT register |
| +12 | 012 | RLOW5 | Read Lower Limit | Read | Read lower limit register |
| +13 | 013 | LDAT5 | Load Data | Write | Write data/lower limit |
| +14 | 014 | SLOC5 | Status Lock | Read | Read lock status |
| +15 | 015 | BITM5 | Bitmask | Write | Write bitmask |
| +16 | 016 | UNLC5 | Unlock | Write | Unlock operation |
| +17 | 017 | RETG5 | Return Gate | Write | Return/end gate |

### 8.2 RSTA5 - Status Register Bit Map

**Read Status Register (HDEV+2):**

```
Bit:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
      ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
      │   │   │   │   │   │   │ L │ E │ C │ D │ P │ F │ A │   │   │   │
      └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
                                  │   │   │   │   │   │   │
                                  │   │   │   │   │   │   └─→ Bit 3: 5ALIVE (ND-500 alive)
                                  │   │   │   │   │   └─────→ Bit 4: 5FAULT (Fault)
                                  │   │   │   │   └─────────→ Bit 5: 5PFAIL (Power fail)
                                  │   │   │   └─────────────→ Bit 6: 5DMAER (DMA error)
                                  │   │   └─────────────────→ Bit 7: 5CLOST (Clock lost)
                                  │   └─────────────────────→ Bit 8: 5ERROR (Error)
                                  └─────────────────────────→ Bit 9: 5ILOCK (Interface lock)
```

**Bit Definitions:**

| Bit | Symbol | Mask (Octal) | Mask (Hex) | Meaning |
|:---:|--------|:------------:|:----------:|---------|
| 3 | 5ALIVE | 0010 | 0x0008 | ND-500 is responding (presence indicator) |
| 4 | 5FAULT | 0020 | 0x0010 | Fault condition |
| 5 | 5PFAIL | 0040 | 0x0020 | Power failure detected |
| 6 | 5DMAER | 0100 | 0x0040 | DMA/communication error |
| 7 | 5CLOST | 0200 | 0x0080 | Microclock stopped/lost |
| 8 | 5ERROR | 0400 | 0x0100 | General error |
| 9 | 5ILOCK | 1000 | 0x0200 | Interface locked |

**Detection Logic:**

```npl
T:=HDEV+RSTA5; *IOXT             % Read status
A=:STATUS

% Check for ND-500 present and healthy:
IF STATUS BIT 5ALIVE AND         % Bit 3 set (responding)
   STATUS NBIT 5DMAER AND        % Bit 6 clear (no DMA error)
   STATUS NBIT 5PFAIL AND        % Bit 5 clear (no power fail)
   STATUS NBIT 5CLOST THEN       % Bit 7 clear (clock running)
   % ND-500 is present and healthy
   ND500PRESENT:=TRUE
ELSE
   % ND-500 absent or has errors
   ND500PRESENT:=FALSE
FI
```

### 8.3 RCON5 - Control Register Bit Map

**Read Control Register (HDEV+4):**

```
Bit:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
      ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
      │   │   │   │   │   │   │   │   │   │   │   │ E │   │   │   │   │
      └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
                                                      │
                                                      └─→ Bit 4: INTEN (Interrupt enable)
```

**Common Values:**

| Value (Octal) | Value (Hex) | Meaning |
|:-------------:|:-----------:|---------|
| 000 | 0x0000 | Interrupts disabled |
| 010 | 0x0008 | Interrupts enabled (level 12) |
| 020 | 0x0010 | Interrupts enabled (level 13) |

---

## 9. Emulator Implementation

### 9.1 Complete Emulator Code

```csharp
// Complete C# emulator implementation for ND-500 detection

public class ND100Emulator
{
    // Registers
    private ushort A, T, X, L, D, P;
    private bool[] InterruptLevels = new bool[16];

    // ND-500 subsystem
    private ND500Coprocessor _nd500;
    private ushort _nd500DeviceNumber = 0x40;  // 100₈ octal

    // Memory
    private ushort[] _ram = new ushort[65536];
    private byte[] _mpm5 = null;  // null if no ND-500

    public ND100Emulator(bool hasND500)
    {
        if (hasND500)
        {
            _nd500 = new ND500Coprocessor();
            _mpm5 = new byte[32768];  // 16KB 5MPM
        }
    }

    public void ExecuteIOXT()
    {
        // T register format: High byte = device, Low byte = offset
        ushort device = (ushort)(T >> 8);
        ushort offset = (ushort)(T & 0xFF);

        if (device == _nd500DeviceNumber)
        {
            HandleND500IOX(offset);
        }
        else
        {
            // Handle other devices...
        }
    }

    private void HandleND500IOX(ushort offset)
    {
        // If no ND-500, trap immediately
        if (_nd500 == null)
        {
            // Trigger illegal instruction trap
            // SINTRAN will catch this and know no ND-500
            TriggerTrap(TrapType.IllegalInstruction);
            return;
        }

        // ND-500 exists, handle IOX
        switch (offset)
        {
            case 0x00:  // RMAR5 - Read MAR
                A = _nd500.ReadMAR();
                break;

            case 0x01:  // LMAR5 - Load MAR
                _nd500.WriteMAR(A);
                break;

            case 0x02:  // RSTA5 - Read Status (CRITICAL FOR DETECTION!)
                A = _nd500.ReadStatusRegister();
                break;

            case 0x03:  // LSTA5 - Load Status
                _nd500.WriteStatusRegister(A);
                break;

            case 0x04:  // RCON5 - Read Control
                A = _nd500.ReadControlRegister();
                break;

            case 0x05:  // LCON5 - Load Control
                _nd500.WriteControlRegister(A);
                break;

            case 0x06:  // MCLR5 - Master Clear
                _nd500.MasterClear();
                break;

            case 0x07:  // TERM5 - Terminate
                _nd500.Terminate(A);
                break;

            case 0x08:  // RTAG5 - Read Tag
                A = _nd500.ReadTagIn();
                break;

            case 0x09:  // LTAG5 - Write Tag
                _nd500.WriteTagOut(A);
                break;

            case 0x0A:  // RLOW5 - Read Lower Limit
                A = _nd500.ReadLowerLimit();
                break;

            case 0x0B:  // LDAT5 - Load Data
                _nd500.WriteData(A);
                break;

            case 0x0E:  // UNLC5 - Unlock
                _nd500.Unlock();
                break;

            default:
                // Unknown offset, ignore
                break;
        }
    }

    private void TriggerTrap(TrapType type)
    {
        // Implementation depends on trap handling...
        // Set IIC register to indicate illegal instruction
    }
}

public class ND500Coprocessor
{
    // Status register bits
    private const ushort BIT_5ALIVE = 0x0008;  // Bit 3
    private const ushort BIT_5FAULT = 0x0010;  // Bit 4
    private const ushort BIT_5PFAIL = 0x0020;  // Bit 5
    private const ushort BIT_5DMAER = 0x0040;  // Bit 6
    private const ushort BIT_5CLOST = 0x0080;  // Bit 7
    private const ushort BIT_5ERROR = 0x0100;  // Bit 8
    private const ushort BIT_5ILOCK = 0x0200;  // Bit 9

    // Registers
    private ushort _statusRegister;
    private ushort _controlRegister;
    private ushort _marRegister;
    private ushort _tagIn;
    private ushort _tagOut;

    // State
    private bool _powerOn = true;
    private bool _clockRunning = true;

    public ND500Coprocessor()
    {
        MasterClear();
    }

    public void MasterClear()
    {
        // Reset to power-on state
        _statusRegister = BIT_5ALIVE;  // Set alive bit
        _controlRegister = 0;
        _marRegister = 0;
        _tagIn = 0;
        _tagOut = 0;
    }

    public ushort ReadStatusRegister()
    {
        ushort status = 0;

        // Set status bits based on state
        if (_powerOn && _clockRunning)
            status |= BIT_5ALIVE;

        if (!_powerOn)
            status |= BIT_5PFAIL;

        if (!_clockRunning)
            status |= BIT_5CLOST;

        // Add other status bits as needed...

        return status;
    }

    public void WriteStatusRegister(ushort value)
    {
        _statusRegister = value;
    }

    public ushort ReadControlRegister()
    {
        return _controlRegister;
    }

    public void WriteControlRegister(ushort value)
    {
        _controlRegister = value;

        // Handle interrupt enable/disable
        if ((value & 0x0008) != 0)
        {
            // Enable interrupts on level 12
            EnableInterrupts(12);
        }
    }

    public ushort ReadMAR()
    {
        return _marRegister;
    }

    public void WriteMAR(ushort value)
    {
        _marRegister = value;
    }

    public ushort ReadTagIn()
    {
        return _tagIn;
    }

    public void WriteTagOut(ushort value)
    {
        _tagOut = value;
    }

    public void WriteData(ushort value)
    {
        // Handle data writes...
    }

    public ushort ReadLowerLimit()
    {
        return 0;  // Implementation specific
    }

    public void Unlock()
    {
        _statusRegister &= (ushort)~BIT_5ILOCK;
    }

    public void Terminate(ushort processNumber)
    {
        // Terminate ND-500 process
    }

    private void EnableInterrupts(int level)
    {
        // Enable ND-100 interrupt level
    }
}
```

### 9.2 Testing the Emulator

**Test Case 1: No ND-500 Present**

```csharp
var emulator = new ND100Emulator(hasND500: false);

// SINTRAN executes: T:=HDEV+RSTA5; *IOXT
emulator.T = 0x4002;  // Device 100₈ (0x40), Offset 2 (RSTA5)
emulator.ExecuteIOXT();

// Expected: Illegal instruction trap
// SINTRAN will catch trap and set ND500PRESENT = FALSE
```

**Test Case 2: ND-500 Present and Healthy**

```csharp
var emulator = new ND100Emulator(hasND500: true);

// SINTRAN executes: T:=HDEV+MCLR5; *IOXT
emulator.T = 0x4006;  // Device 100₈, Offset 6 (MCLR5)
emulator.ExecuteIOXT();

// SINTRAN executes: T:=HDEV+RSTA5; *IOXT
emulator.T = 0x4002;  // Device 100₈, Offset 2 (RSTA5)
emulator.ExecuteIOXT();

// Expected: A register = 0x0008 (BIT_5ALIVE set)
Console.WriteLine($"Status: 0x{emulator.A:X4}");  // Should print: Status: 0x0008

// SINTRAN checks: IF A BIT 5ALIVE THEN ND500PRESENT:=TRUE
```

**Test Case 3: ND-500 Present but Faulted**

```csharp
var emulator = new ND100Emulator(hasND500: true);

// Simulate power failure
emulator._nd500._powerOn = false;

// SINTRAN executes: T:=HDEV+RSTA5; *IOXT
emulator.T = 0x4002;
emulator.ExecuteIOXT();

// Expected: A register = 0x0020 (BIT_5PFAIL set, BIT_5ALIVE clear)
Console.WriteLine($"Status: 0x{emulator.A:X4}");  // Should print: Status: 0x0020

// SINTRAN checks: IF A BIT 5PFAIL THEN ND500PRESENT:=FALSE
```

### 9.3 Debug Output

**Enable debug logging:**

```csharp
public class ND500Coprocessor
{
    private bool _debugMode = true;

    public ushort ReadStatusRegister()
    {
        ushort status = 0;

        if (_powerOn && _clockRunning)
            status |= BIT_5ALIVE;

        if (_debugMode)
        {
            Console.WriteLine($"[ND500] RSTA5: Read Status = 0x{status:X4}");
            Console.WriteLine($"  5ALIVE  = {((status & BIT_5ALIVE) != 0 ? "SET" : "CLEAR")}");
            Console.WriteLine($"  5PFAIL  = {((status & BIT_5PFAIL) != 0 ? "SET" : "CLEAR")}");
            Console.WriteLine($"  5DMAER  = {((status & BIT_5DMAER) != 0 ? "SET" : "CLEAR")}");
        }

        return status;
    }
}
```

**Expected Output:**

```
[ND500] RSTA5: Read Status = 0x0008
  5ALIVE  = SET
  5PFAIL  = CLEAR
  5DMAER  = CLEAR
```

---

## Summary

### Detection Flow

```
SINTRAN Boot
    ↓
DETECTND500:
    ↓
Check HDEV configured? ──No──→ ND500PRESENT = FALSE
    ↓ Yes
Master Clear (MCLR5)
    ↓
Read Status (RSTA5) ──Trap──→ ND500PRESENT = FALSE
    ↓ Success
Check Status Bits:
  - 5ALIVE set? ──────No──→ ND500PRESENT = FALSE
  - 5DMAER clear? ────No──→ ND500PRESENT = FALSE
  - 5PFAIL clear? ────No──→ ND500PRESENT = FALSE
  - 5CLOST clear? ────No──→ ND500PRESENT = FALSE
    ↓ All checks pass
ND500PRESENT = TRUE
    ↓
INIT5MPM (Allocate 5MPM)
    ↓
INIT5PROCS (Initialize process table)
    ↓
LOAD5XMSG (Load XMSG kernel)
    ↓
ND-500 Ready
```

### Key Registers

| Register | Offset | Purpose | Detection Use |
|----------|:------:|---------|---------------|
| **RSTA5** | **+2** | **Read Status** | **Primary detection test** |
| RCON5 | +4 | Read Control | Verify interface responds |
| MCLR5 | +6 | Master Clear | Initialize before test |
| LCON5 | +5 | Load Control | Enable interrupts |

### Critical Status Bits

| Bit | Symbol | Must Be | For Detection |
|:---:|--------|:-------:|---------------|
| 3 | 5ALIVE | **SET** | **ND-500 responding** |
| 5 | 5PFAIL | **CLEAR** | **No power failure** |
| 6 | 5DMAER | **CLEAR** | **No DMA error** |
| 7 | 5CLOST | **CLEAR** | **Clock running** |

---

**End of Document**
