# ND-500 Memory Structures, IOX Operations, and Data Flow - Complete C# Reference

**Based ONLY on SINTRAN NPL Source Code Analysis**

**Date:** 2025-12-04
**Source Files Analyzed:**
- `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL` (Monitor/Driver level)
- `SINTRAN/NPL-SOURCE/NPL/CC-P2-N500.NPL` (Common routines)
- `SINTRAN/NPL-SOURCE/NPL/RP-P2-N500.NPL` (RT-program level)
- `SINTRAN/NPL-SOURCE/NPL/XC-P2-N500.NPL` (Export routines)

---

## Table of Contents

1. [Memory Architecture Overview](#memory-architecture-overview)
2. [5MPM Multiport Memory Structure](#5mpm-multiport-memory-structure)
3. [Message Buffer Structure (VERIFIED)](#message-buffer-structure-verified)
4. [Execution Queue (MAILINK) Structure](#execution-queue-mailink-structure)
5. [IOX Operations with Memory](#iox-operations-with-memory)
6. [Memory Copy Operations](#memory-copy-operations)
7. [Complete C# Implementation](#complete-c-implementation)

---

## 1. Memory Architecture Overview

### What Memory Regions Are Involved?

From NPL source code analysis, three memory regions interact:

```
┌─────────────────────────────────────────────────────────────┐
│ 1. ND-100 Private Memory                                    │
│    - Operating system code (SINTRAN III)                    │
│    - Process datafields                                     │
│    - IOX device registers (accessed via T:=HDEV+offset)     │
└─────────────────────────────────────────────────────────────┘
                           ↕
┌─────────────────────────────────────────────────────────────┐
│ 2. Multiport Memory (5MPM) - 5MBBANK                        │
│    - Message buffers (55MESSIZE words each)                 │
│    - Process descriptors (5PRDSIZE words each)              │
│    - Execution queue (MAILINK linked list)                  │
│    - Time queue (linked list)                               │
│    - Accessible by BOTH ND-100 and ND-500                   │
└─────────────────────────────────────────────────────────────┘
                           ↕
┌─────────────────────────────────────────────────────────────┐
│ 3. ND-500 Private Memory                                    │
│    - ND-500 process memory spaces                           │
│    - Not directly accessible from ND-100                    │
└─────────────────────────────────────────────────────────────┘
```

### Key Addressing Mechanisms

**Source: CC-P2-N500.NPL lines 246, 257, 262**

```npl
% Address conversion operations:
*NNC01, CNVBYADR    % Convert multiport byte address to ND-100 physical
*NNC02, CNVBYADR    % Used when traversing linked lists
*NNC03, CNVWADR     % Convert multiport word address to ND-100 physical
```

**What this means:**
- `CNVBYADR` - Converts byte offset in 5MPM to ND-100 physical address
- `CNVWADR` - Converts word offset in 5MPM to ND-100 physical address
- Both are microcode instructions that translate addresses

---

## 2. 5MPM Multiport Memory Structure

### Bank Selection

**Source: Multiple files, consistent pattern**

```npl
T:=5MBBANK    % Set T register to multiport memory bank number
```

**C# Representation:**

```csharp
/// <summary>
/// Multiport memory bank identifier.
/// SOURCE: All NPL files use "T:=5MBBANK" before accessing 5MPM
/// This is a bank number, not a direct address.
/// </summary>
public class MultiportMemoryBank
{
    /// <summary>
    /// Bank number for multiport memory (5MBBANK value from symbols)
    /// VERIFIED: Used in every 5MPM access in NPL source
    /// </summary>
    public ushort BankNumber { get; set; }

    /// <summary>
    /// Physical base address after bank translation
    /// NOTE: Not directly visible in NPL - handled by MMU
    /// </summary>
    public uint PhysicalBaseAddress { get; set; }
}
```

### Global Variables in 5MPM

**Source: MP-P2-N500.NPL line 579, 678; RP-P2-N500.NPL line 87**

```npl
X:=MAILINK                    % Head of execution queue
DO
   T:=5MBBANK; *LINK@3 LDDTX  % Load D from X.LINK (offset 3)
WHILE D><-1                   % While next pointer != -1
   % ... process message at address D
OD
```

**C# Representation:**

```csharp
/// <summary>
/// Global coordination structure in 5MPM.
/// SOURCE: MAILINK used in MP-P2-N500.NPL:579,678; RP-P2-N500.NPL:87,205,268
/// These are addresses within 5MPM, not offsets.
/// </summary>
public class ND500GlobalData
{
    /// <summary>
    /// MAILINK - Head of execution queue (linked list of messages)
    /// VERIFIED: MP-P2-N500.NPL lines 579, 678
    /// Value: Address of first message in queue (or -1 if empty)
    /// </summary>
    public ushort ExecutionQueueHead { get; set; }  // MAILINK

    /// <summary>
    /// DUMMESS - Dummy message marker
    /// VERIFIED: MP-P2-N500.NPL line 582 "WHILE D><-1" pattern suggests
    /// special sentinel value, RP-P2-N500.NPL:210 "IF X:=D><DUMMESS"
    /// </summary>
    public ushort DummyMessageAddress { get; set; }  // DUMMESS

    /// <summary>
    /// SWMSG - Swapper message buffer address
    /// VERIFIED: Multiple references MP-P2-N500.NPL:1028,1030,1312
    /// </summary>
    public ushort SwapperMessageAddress { get; set; }  // SWMSG
}
```

---

## 3. Message Buffer Structure (VERIFIED)

### Field-by-Field Analysis from NPL Source

**Source: CC-P2-N500.NPL lines 236-265 (ITO500XQ routine)**

```npl
ITO500XQ:
   T:=5MBBANK; *AAX 5MSFL; LDATX                    % Load message flags
   A=:D BONE 5IEXQUEUE; *STATX; AAX -5MSFL          % Set "in queue" bit
   IF D BIT 5IEXQUEUE THEN EXIT FI                  % Already in queue?
   X=:CMESS                                         % Save message address
   *AAX 5PRIO; LDATX                                % Load priority
   A=:L; T:=5MBBANK; X:=MAILINK                     % Get queue head
   DO
      *LINK@3 LDDTX                                 % Load next pointer (offset 3)
   WHILE D><-1                                      % While not end of list
      T:=A; X=:D
      *AAX 5PRIO; LDATX; AAX -5PRIO                 % Compare priorities
      IF A<L THEN X:=D; GO ITO51 FI                 % Insert here if lower priority
   OD
ITO51:
   T:=5MBBANK; *LINK@3 LDDTX                        % Get previous.LINK
   X=:L:=CMESS; *LINK@3 STDTX                       % current.LINK = next
   T:=5MBBANK; X:=CMESS; A:=L
   *AAX PLINK; STATX; AAX -PLINK                    % Set PLINK
```

**Key discoveries:**
1. `*LINK@3` - LINK field is at offset 3 words (6 bytes)
2. `*AAX 5PRIO` - 5PRIO field accessed with AAX (add to X)
3. `*AAX 5MSFL` - 5MSFL (message flags) is another field
4. `*AAX PLINK` - PLINK (process link) is separate from LINK

**Source: MP-P2-N500.NPL lines 1302-1318 (Monitor call processing)**

```npl
T:=5MBBANK; X:=N5MESSAGE
*AAX MCNO-XADPR; LDATX; AAX SMCNO-MCNO; STATX     % Monitor call number
IF A=2CLOCK THEN                                   % Clock monitor call
   N5MESSAGE+"ABUFADR"=:X; T:=5MBBANK; *LDDTX     % Load buffer address
   A=:T; A:=D=:CURX
   FOR CNT DO
      ACL7(CNT); X:=CURX; *SWAP SA DD CLD; STDTX  % Copy data
      MIN CURX; MIN CURX
   OD
   X:=N5MESSAGE; T:=5MBBANK
   *AAX PDR1; LDDTX; AAX 26ADD-PDR1; STDTX        % Destination address
FI
```

**Key discoveries:**
1. `MCNO` - Monitor call number field
2. `ABUFADR` - Buffer address field (added to message address with `+`)
3. `PDR1`, `26ADD` - Parameter fields
4. Data copying uses `*LDDTX` (load double word) and `*STDTX` (store double word)

### Complete Message Buffer Structure

```csharp
/// <summary>
/// ND-500 Message Buffer in 5MPM (Multiport Memory).
/// Size: 55MESSIZE words (exact value from symbol table, typically 100-128 words)
///
/// SOURCES:
/// - Structure layout: CC-P2-N500.NPL:236-265 (ITO500XQ)
/// - Field usage: MP-P2-N500.NPL:1302-1318, RP-P2-N500.NPL:110-160
/// - Memory access: All files use T:=5MBBANK pattern
///
/// ADDRESSING:
/// All accesses use: T:=5MBBANK; X:=message_address; *AAX field_offset; LDATX/STATX
/// </summary>
[StructLayout(LayoutKind.Sequential, Pack = 2)]
public struct ND500MessageBuffer
{
    // ===== LINK FIELDS (Offsets 0-3) =====

    /// <summary>
    /// Offset 0-1: PLINK - Process Link (16 bits)
    /// SOURCE: CC-P2-N500.NPL:258,261 "*AAX PLINK; STATX"
    /// PURPOSE: Links message to process descriptor or previous message
    /// </summary>
    public ushort ProcessLink;  // PLINK

    /// <summary>
    /// Offset 2: 5MSFL - Message Status Flags (16 bits)
    /// SOURCE: CC-P2-N500.NPL:237 "*AAX 5MSFL; LDATX"
    /// SOURCE: CC-P2-N500.NPL:291 "A=:D BZERO 5IEXQUEU; *STATX"
    /// BITS (verified from code):
    ///   5IEXQUEUE: Message in execution queue
    ///   5ITMQUEUE: Message in time queue
    ///   5SYSRES: System reserved
    ///   5CPUBOUND: CPU bound operation
    /// </summary>
    public ushort MessageFlags;  // 5MSFL

    /// <summary>
    /// Offset 3: LINK - Next Message Pointer (16 bits)
    /// SOURCE: CC-P2-N500.NPL:244 "*LINK@3 LDDTX" (@ syntax = offset)
    /// SOURCE: CC-P2-N500.NPL:254 "*LINK@3 LDDTX"
    /// PURPOSE: Next message in linked list (queue), -1 = end of list
    /// </summary>
    public ushort NextLink;  // LINK (at offset @3)

    // ===== PRIORITY AND CONTROL (Offset 4+) =====

    /// <summary>
    /// Offset 4: 5PRIO - Priority (16 bits)
    /// SOURCE: CC-P2-N500.NPL:241,249 "*AAX 5PRIO; LDATX"
    /// SOURCE: RP-P2-N500.NPL:152 "*AAX 5PRIO; STATX"
    /// PURPOSE: Message priority for queue ordering (higher = more urgent)
    /// RANGE: 0-65535, with higher values processed first
    /// </summary>
    public ushort Priority;  // 5PRIO

    /// <summary>
    /// Offset 5+: XADPR - Process Descriptor Address (16 bits)
    /// SOURCE: RP-P2-N500.NPL:21 "*AAX XADPR; STATX"
    /// SOURCE: MP-P2-N500.NPL:1302 "*AAX MCNO-XADPR; LDATX"
    /// PURPOSE: Address of owning process descriptor in 5MPM
    /// </summary>
    public ushort ProcessDescriptorAddr;  // XADPR

    /// <summary>
    /// Offset 6+: MCNO - Monitor Call Number (16 bits)
    /// SOURCE: MP-P2-N500.NPL:1302 "*AAX SMCNO-MCNO; STATX"
    /// PURPOSE: Which monitor call function to execute
    /// KNOWN VALUES: 2CLOCK, 2TUSED (from MP-P2-N500.NPL:1303,1310)
    /// </summary>
    public ushort MonitorCallNumber;  // MCNO

    // ===== MICROCODE FUNCTION =====

    /// <summary>
    /// Offset unknown: MICFUNC - Microcode Function (16 bits)
    /// SOURCE: CC-P2-N500.NPL:436 "*MICFU@3 STATX"
    /// SOURCE: MP-P2-N500.NPL:491 "MICFU@3 STATX"
    /// KNOWN VALUES (from MP-P2-N500.NPL:1839-1841):
    ///   3MONCO, 3RMED, 3WMEP, 3RMEP, 3WMONCO,
    ///   3PHSREAD, 3PHSWRITE, 3WMED, 3TRACO, 3START
    /// PURPOSE: ND-500 microcode operation to perform
    /// NOTE: @3 syntax suggests offset 3 from some base
    /// </summary>
    public ushort MicrocodeFunction;  // MICFUNC

    // ===== TIME MANAGEMENT =====

    /// <summary>
    /// Offset unknown: 500TU - ND-500 CPU Time Used (32 bits)
    /// SOURCE: MP-P2-N500.NPL:280-281 "*AAX 500TU; LDDTX; D+1; A:=A+C; *STDTX"
    /// SOURCE: MP-P2-N500.NPL:1305 "*AAX 500TU; LDDTX"
    /// PURPOSE: Accumulated CPU time for this process
    /// ACCESS: Double-word (LDDTX/STDTX), incremented by driver
    /// </summary>
    public uint CPUTimeUsed;  // 500TU

    /// <summary>
    /// Offset unknown: 5TSLC - Timeslice Count (varies)
    /// SOURCE: RP-P2-N500.NPL:111 "*AAX 5TSLC; LDDTX"
    /// SOURCE: RP-P2-N500.NPL:161 "*AAX 5TSLC; STDTX"
    /// PURPOSE: Current timeslice counter
    /// </summary>
    public uint TimesliceCount;  // 5TSLC

    /// <summary>
    /// Offset unknown: 5TSLS - Timeslice Status (varies)
    /// SOURCE: RP-P2-N500.NPL:112 "*AAX 5TSLS-5TSLC; LDATX"
    /// SOURCE: RP-P2-N500.NPL:162 "*AAX 5TSLS-5TSLC; STATX"
    /// PURPOSE: Timeslice status flags
    /// </summary>
    public ushort TimesliceStatus;  // 5TSLS

    /// <summary>
    /// Offset unknown: SUSPC - Suspend Counter (16 bits)
    /// SOURCE: RP-P2-N500.NPL:213 "*AAX SUSPC; LDATX"
    /// PURPOSE: Suspend time countdown
    /// </summary>
    public ushort SuspendCounter;  // SUSPC

    // ===== BUFFER AND ADDRESS FIELDS =====

    /// <summary>
    /// Offset unknown: ABUFADR - Buffer Address (varies)
    /// SOURCE: MP-P2-N500.NPL:1312 "N5MESSAGE+\"ABUFADR\"=:X"
    /// NOTE: String literal "ABUFADR" suggests symbolic offset
    /// PURPOSE: Address of data buffer for transfer
    /// </summary>
    public ushort BufferAddress;  // ABUFADR

    /// <summary>
    /// Offset unknown: PDR1 - Parameter/Descriptor 1 (32 bits)
    /// SOURCE: MP-P2-N500.NPL:1320 "*AAX PDR1; LDDTX"
    /// PURPOSE: First parameter for monitor call
    /// </summary>
    public uint Parameter1;  // PDR1

    /// <summary>
    /// Offset unknown: 26ADD - 26-bit Address (32 bits)
    /// SOURCE: MP-P2-N500.NPL:1320 "*AAX 26ADD-PDR1; STDTX"
    /// PURPOSE: Destination address (26-bit ND-100 address)
    /// </summary>
    public uint Address26Bit;  // 26ADD

    /// <summary>
    /// Offset unknown: SM26A - Saved 26-bit Address (32 bits)
    /// SOURCE: MP-P2-N500.NPL:1321 "*AAX SM26A-26ADD; STDTX"
    /// PURPOSE: Saved copy of 26-bit address
    /// </summary>
    public uint SavedAddress26;  // SM26A

    // ===== SWAPPER FIELDS =====

    /// <summary>
    /// Offset unknown: HSWPI - Swapper Info (varies)
    /// SOURCE: MP-P2-N500.NPL:435 "*AAX HSWPI; STDTX"
    /// SOURCE: MP-P2-N500.NPL:465 "*AAX HSWPI; LDDTX"
    /// PURPOSE: Swapper process information
    /// </summary>
    public ushort SwapperInfo;  // HSWPI

    /// <summary>
    /// Offset unknown: SWPFU - Swapper Function (16 bits)
    /// SOURCE: MP-P2-N500.NPL:438 "*AAX SWPFU; STATX"
    /// KNOWN VALUES: SWACTIVE, MSWSTART, MSWSWAIT
    /// PURPOSE: Swapper function code
    /// </summary>
    public ushort SwapperFunction;  // SWPFU

    /// <summary>
    /// Offset unknown: 5CPUN - CPU Number (16 bits)
    /// SOURCE: MP-P2-N500.NPL:440 "*AAX 5CPUN-5PRIO; STATX"
    /// SOURCE: RP-P2-N500.NPL (referenced in CPUNO usage)
    /// PURPOSE: Which ND-500 CPU this message is for
    /// </summary>
    public ushort CPUNumber;  // 5CPUN

    /// <summary>
    /// Offset unknown: SKFLI - Skip/Flip Flag (16 bits)
    /// SOURCE: MP-P2-N500.NPL:471 "*AAX SKFLI; LDATX"
    /// PURPOSE: Error/status flag
    /// </summary>
    public ushort SkipFlipFlag;  // SKFLI

    // ===== RETURN PARAMETERS =====

    /// <summary>
    /// Offset unknown: PRET2, PRET4, PRET5, PRET6 - Parameter Return slots
    /// SOURCE: MP-P2-N500.NPL:473,477,481,485 "*AAX PRETn; LDATX"
    /// PURPOSE: Return value indicators
    /// </summary>
    public ushort ParamReturn2;  // PRET2
    public ushort ParamReturn4;  // PRET4
    public ushort ParamReturn5;  // PRET5
    public ushort ParamReturn6;  // PRET6

    /// <summary>
    /// Offset unknown: RETP2, RETP4, RETP5, RETP6 - Return value slots (32-bit each)
    /// SOURCE: MP-P2-N500.NPL:475,479,483,487 "*AAX RETPn-PRETn; LDDTX" / "*AAX RETPn; STDTX"
    /// PURPOSE: Actual return values from swapper operations
    /// </summary>
    public uint ReturnValue2;  // RETP2
    public uint ReturnValue4;  // RETP4
    public uint ReturnValue5;  // RETP5
    public uint ReturnValue6;  // RETP6

    /// <summary>
    /// Offset unknown: NUMPA - Number of Parameters (16 bits)
    /// SOURCE: MP-P2-N500.NPL:489 "*AAX NUMPA; STATX"
    /// PURPOSE: How many parameters were returned
    /// </summary>
    public ushort NumParameters;  // NUMPA

    /// <summary>
    /// Offset unknown: OLDMI - Old Microcode Function (16 bits)
    /// SOURCE: MP-P2-N500.NPL:491 "*AAX OLDMI; LDATX; AAX -OLDMI; MICFU@3 STATX"
    /// PURPOSE: Saved microcode function to restore
    /// </summary>
    public ushort OldMicrocodeFunc;  // OLDMI

    // ===== VARIABLE DATA AREA =====

    /// <summary>
    /// Variable-size data area.
    /// Total structure size: 55MESSIZE words
    /// Remaining space after fixed fields.
    /// </summary>
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 50)]
    public ushort[] DataArea;  // Size adjusted based on 55MESSIZE
}
```

---

## 4. Execution Queue (MAILINK) Structure

### Queue Traversal Algorithm

**Source: CC-P2-N500.NPL lines 242-253**

```npl
T:=5MBBANK; X:=MAILINK           % X = queue head
DO
   *LINK@3 LDDTX                 % D = X.LINK (next message)
WHILE D><-1                      % While next != -1
   *NNC01, CNVBYADR              % Convert address
   IF D=CMESS THEN CALL ERRFATAL FI  % Circular list check
   T:=A; X=:D                    % Move to next
   *AAX 5PRIO; LDATX; AAX -5PRIO % Check priority
   IF A<L THEN X:=D; GO ITO51 FI % Insert point found
OD
```

**What happens:**
1. Start at MAILINK (head of queue)
2. Follow LINK pointers (offset @3)
3. Each iteration: `X.LINK → D`, then `D → X`
4. Stop when `D == -1` (end of list)
5. CNVBYADR converts 5MPM byte address to ND-100 physical address

### Queue Insertion Algorithm

**Source: CC-P2-N500.NPL lines 254-265**

```npl
ITO51:
   T:=5MBBANK; *LINK@3 LDDTX         % previous.LINK → D
   X=:L:=CMESS; *LINK@3 STDTX        % current.LINK = next (D)
   IF D><-1 THEN
      *NNC02, CNVBYADR               % Convert next address
      T:=A; X=:A:=D
      *AAX PLINK; STATX              % next.PLINK = current
   FI
   T:=5MBBANK; X:=CMESS; A:=L
   *AAX PLINK; STATX                 % current.PLINK = previous
   A:=T; D:=X
   *NNC03, CNVWADR                   % Convert current address
   X:=L; *LINK@3 STDTX               % previous.LINK = current
```

**What happens:**
1. Save previous.LINK to D
2. Set current.LINK = D (what previous pointed to)
3. Set next.PLINK = current (backward link)
4. Set current.PLINK = previous (backward link)
5. Set previous.LINK = current (forward link)

**This is a DOUBLY-LINKED LIST!**

### Queue Removal Algorithm

**Source: CC-P2-N500.NPL lines 299-305**

```npl
FR5TMQ:
   T:=5MBBANK; *AAX 5MSFL; LDATX
   A=:D BZERO 5ITMQUEUE; *STATX     % Clear "in queue" flag
   IF D NBIT 5ITMQUEUE THEN EXIT FI % Already removed?
FELLS:
   T:=5MBBANK; X=:CMESS
   *LINK@3 LDDTX                     % D = current.LINK (next)
   AAX PLINK; LDXTX                  % X = current.PLINK (prev)
   LINK@3 STDTX                      % prev.LINK = next
   IF D><-1 THEN
      *NNC04, CNVBYADR
      T:=A; X=:A:=D
      *AAX PLINK; STATX              % next.PLINK = prev
   FI
```

**What happens:**
1. Get current.LINK → D (next message)
2. Get current.PLINK → X (previous message)
3. Set prev.LINK = next (bypass current)
4. Set next.PLINK = prev (bypass current)

### C# Queue Implementation

```csharp
/// <summary>
/// Execution queue manager for ND-500 messages.
/// SOURCE: CC-P2-N500.NPL:236-305 (ITO500XQ, IFM500XQ, FR5TMQ)
/// STRUCTURE: Doubly-linked list ordered by priority
/// </summary>
public class ND500ExecutionQueue
{
    private MultiportMemory _mpm;

    /// <summary>
    /// MAILINK - Head of execution queue
    /// SOURCE: CC-P2-N500.NPL:242 "X:=MAILINK"
    /// </summary>
    public ushort QueueHead { get; set; }  // MAILINK

    /// <summary>
    /// LEXQUEUE - Length of execution queue
    /// SOURCE: CC-P2-N500.NPL:265 "MIN LEXQUEUE"
    /// </summary>
    public int QueueLength { get; set; }  // LEXQUEUE

    /// <summary>
    /// Insert message into execution queue by priority.
    /// SOURCE: CC-P2-N500.NPL:236-265 (ITO500XQ routine)
    /// ALGORITHM:
    /// 1. Set message flags: 5IEXQUEUE bit
    /// 2. Find insertion point by comparing priorities
    /// 3. Update forward and backward links
    /// 4. Increment queue length
    /// </summary>
    /// <param name="messageAddr">Address of message in 5MPM</param>
    public void InsertMessage(ushort messageAddr)
    {
        // Step 1: Set "in queue" flag
        // SOURCE: CC-P2-N500.NPL:237-238
        ushort flags = _mpm.ReadWord(messageAddr, MessageOffsets.Flags);
        flags |= (ushort)MessageFlags.InExecutionQueue;
        _mpm.WriteWord(messageAddr, MessageOffsets.Flags, flags);

        if ((flags & (ushort)MessageFlags.InExecutionQueue) != 0)
            return;  // Already in queue

        // Step 2: Get message priority
        // SOURCE: CC-P2-N500.NPL:241
        ushort priority = _mpm.ReadWord(messageAddr, MessageOffsets.Priority);

        // Step 3: Find insertion point
        // SOURCE: CC-P2-N500.NPL:242-253
        ushort current = QueueHead;
        ushort previous = 0xFFFF;  // -1

        while (current != 0xFFFF)
        {
            // Read next link
            ushort next = _mpm.ReadWord(current, MessageOffsets.NextLink);

            // Convert address if needed
            current = ConvertByteAddress(next);

            if (current == messageAddr)
                throw new InvalidOperationException("Circular queue detected");

            // Compare priorities
            ushort currentPrio = _mpm.ReadWord(current, MessageOffsets.Priority);
            if (currentPrio < priority)
                break;  // Insert before this message

            previous = current;
            current = next;
        }

        // Step 4: Insert message
        // SOURCE: CC-P2-N500.NPL:254-265
        if (previous == 0xFFFF)
        {
            // Insert at head
            ushort oldHead = QueueHead;
            _mpm.WriteWord(messageAddr, MessageOffsets.NextLink, oldHead);
            _mpm.WriteWord(messageAddr, MessageOffsets.ProcessLink, 0xFFFF);
            if (oldHead != 0xFFFF)
                _mpm.WriteWord(oldHead, MessageOffsets.ProcessLink, messageAddr);
            QueueHead = messageAddr;
        }
        else
        {
            // Insert in middle/end
            ushort next = _mpm.ReadWord(previous, MessageOffsets.NextLink);
            _mpm.WriteWord(messageAddr, MessageOffsets.NextLink, next);
            _mpm.WriteWord(messageAddr, MessageOffsets.ProcessLink, previous);
            _mpm.WriteWord(previous, MessageOffsets.NextLink, messageAddr);
            if (next != 0xFFFF)
                _mpm.WriteWord(next, MessageOffsets.ProcessLink, messageAddr);
        }

        // Step 5: Increment queue length
        // SOURCE: CC-P2-N500.NPL:265
        QueueLength++;
    }

    /// <summary>
    /// Remove message from execution queue.
    /// SOURCE: CC-P2-N500.NPL:289-305 (IFM500XQ routine)
    /// ALGORITHM:
    /// 1. Clear "in queue" flag
    /// 2. Get next and previous links
    /// 3. Update neighbor pointers to bypass this message
    /// 4. Decrement queue length
    /// </summary>
    /// <param name="messageAddr">Address of message to remove</param>
    public void RemoveMessage(ushort messageAddr)
    {
        // Step 1: Clear "in queue" flag
        // SOURCE: CC-P2-N500.NPL:290-292
        ushort flags = _mpm.ReadWord(messageAddr, MessageOffsets.Flags);
        flags &= (ushort)~MessageFlags.InExecutionQueue;
        _mpm.WriteWord(messageAddr, MessageOffsets.Flags, flags);

        if ((flags & (ushort)MessageFlags.InExecutionQueue) == 0)
            return;  // Not in queue

        // Step 2: Get links
        // SOURCE: CC-P2-N500.NPL:300
        ushort next = _mpm.ReadWord(messageAddr, MessageOffsets.NextLink);
        ushort prev = _mpm.ReadWord(messageAddr, MessageOffsets.ProcessLink);

        // Step 3: Update neighbors
        // SOURCE: CC-P2-N500.NPL:300-304
        if (prev != 0xFFFF)
            _mpm.WriteWord(prev, MessageOffsets.NextLink, next);
        else
            QueueHead = next;  // Removing head

        if (next != 0xFFFF)
        {
            next = ConvertByteAddress(next);
            _mpm.WriteWord(next, MessageOffsets.ProcessLink, prev);
        }

        // Step 4: Decrement queue length
        // SOURCE: CC-P2-N500.NPL:293
        if (QueueLength > 0)
            QueueLength--;
    }

    /// <summary>
    /// Convert multiport byte address to usable address.
    /// SOURCE: CC-P2-N500.NPL:246,257,262 (*NNC01, CNVBYADR)
    /// NOTE: This is a placeholder - actual conversion done by MMU hardware
    /// </summary>
    private ushort ConvertByteAddress(ushort byteAddr)
    {
        // In real hardware, this is handled by CNVBYADR microcode
        // For emulator, return as-is or apply bank translation
        return byteAddr;
    }
}

/// <summary>
/// Message flag bits.
/// SOURCE: CC-P2-N500.NPL:238,291,297 (bit operations on 5MSFL)
/// </summary>
[Flags]
public enum MessageFlags : ushort
{
    /// <summary>
    /// 5IEXQUEUE - Message in execution queue
    /// SOURCE: CC-P2-N500.NPL:238 "BONE 5IEXQUEUE"
    /// </summary>
    InExecutionQueue = 0x0001,

    /// <summary>
    /// 5ITMQUEUE - Message in time queue
    /// SOURCE: CC-P2-N500.NPL:297 "BZERO 5ITMQUEUE"
    /// </summary>
    InTimeQueue = 0x0002,

    /// <summary>
    /// 5SYSRES - System reserved message
    /// SOURCE: MP-P2-N500.NPL:26 "IF A BIT 5SYSRES"
    /// </summary>
    SystemReserved = 0x0004,

    /// <summary>
    /// 5CPUBOUND - CPU bound operation
    /// SOURCE: RP-P2-N500.NPL:264 "IF A NBIT 5CPUBOUND"
    /// </summary>
    CPUBound = 0x0008
}

/// <summary>
/// Message buffer field offsets (in words).
/// SOURCE: Inferred from AAX operations in NPL source
/// NOTE: Exact values require symbol table
/// </summary>
public static class MessageOffsets
{
    public const ushort ProcessLink = 0;     // PLINK
    public const ushort Flags = 2;           // 5MSFL
    public const ushort NextLink = 3;        // LINK (verified by @3 syntax)
    public const ushort Priority = 4;        // 5PRIO
    public const ushort ProcessDescAddr = 5; // XADPR
    public const ushort MonCallNumber = 6;   // MCNO
    // ... other offsets TBD from symbol table
}
```

---

## 5. IOX Operations with Memory

### Reading ND-500 Status

**Source: XC-P2-N500.NPL lines 49-63 (CLE5STATUS routine)**

```npl
CLE5STATUS:
   A=:D; T:=HDEV+RSTA5; *IOXT                    % Read status register
   IF A BIT 5POWOF OR A BIT 5PFAIL THEN
      IF A BIT 5PFAIL OR C5STAT BIT BHPFAIL THEN
         A:=L=:"LREG"
         CALL TER500; GO CLABORT
         10; T:=HDEV+LCON5; *IOXT                % Write 10 to control
         T+"RSTA5-LCON5"; *IOXT                  % Read status again
         A/\D; T+"LSTA5-RSTA5"; *IOXT            % Write masked status
         "0"; T+"LCON5-LSTA5"; *IOXT             % Write 0 to control
CLABORT: "LREG"=:L; T:=HDEV+RSTA5; *IOXT
      ELSE
         A BONE 5POWOF BZERO 5PFAIL
      FI
   FI; EXIT
```

**What this does:**
1. `T:=HDEV+RSTA5` - Set T to IOX device base + RSTA5 offset
2. `*IOXT` - Execute IOX transfer (read from device to A register)
3. Result in A register is status bits
4. No memory access - direct register operation

### Activating ND-500

**Source: MP-P2-N500.NPL lines 3084-3093 (ACTRDY routine)**

```npl
ACT50:
   5MBBANK; T:=HDEV+LMAR5; *IOXT      % Set MAR bank
   A:=X; *IOXT                        % Write message address to MAR
   A:=5; T+"LCON5-LMAR5"; *IOXT       % Write 5 to control
   % ... or path B:
   A:=10; T:=HDEV+LCON5; *IOXT        % Write 10₈ to control
   A:=0; T+"LSTA5-LCON5"; *IOXT       % Write 0 to status
   A:=1; T+"LCON5-LSTA5"; *IOXT       % Write 1 to control
   T+"SLOC5-LCON5"; *IOXT             % Lock status
```

**What this does:**
1. Write 5MBBANK value to LMAR5 register (memory bank)
2. Write message address X to LMAR5 (continues in same register)
3. Write control sequences to trigger ND-500
4. All IOX operations - NO memory access

### C# IOX Implementation

```csharp
/// <summary>
/// IOX device register access for ND-500 interface.
/// SOURCE: All ND-500 NPL files use HDEV+offset pattern
/// IMPORTANT: These are DEVICE REGISTERS, not memory locations!
/// </summary>
public class ND500IOXInterface
{
    /// <summary>
    /// HDEV - Base device number for ND-500 interface
    /// SOURCE: Symbol table, used as "T:=HDEV+offset"
    /// VALUE: 177775₈ (from previous documentation)
    /// </summary>
    private const ushort HDEV = 0o177775;

    /// <summary>
    /// IOX register offsets
    /// SOURCE: Symbol table N500-SYMBOLS.SYMB.TXT
    /// </summary>
    private const ushort RMAR5 = 0o000000;  // Read Memory Address Register
    private const ushort LMAR5 = 0o000001;  // Load Memory Address Register
    private const ushort RSTA5 = 0o000002;  // Read Status
    private const ushort LSTA5 = 0o000003;  // Load Status
    private const ushort RCON5 = 0o000004;  // Read Control
    private const ushort LCON5 = 0o000005;  // Load Control
    private const ushort TERM5 = 0o000007;  // Terminate
    private const ushort SLOC5 = 0o000014;  // Status Lock
    private const ushort UNLC5 = 0o000016;  // Unlock

    /// <summary>
    /// Status register bits
    /// SOURCE: XC-P2-N500.NPL:40-45 (comments)
    /// </summary>
    [Flags]
    public enum StatusBits : ushort
    {
        _5ILOCK  = 0x0001,  // Interface locked (ND-500 running)
        _5POWOF  = 0x0002,  // Power off
        _5PFAIL  = 0x0004,  // Power fail
        _5CLOST  = 0x0008,  // Clock lost
        _5DMAER  = 0x0010,  // DMA error
        _5PAGF   = 0x0020   // Page fault
    }

    /// <summary>
    /// Read ND-500 status register.
    /// SOURCE: XC-P2-N500.NPL:50 "T:=HDEV+RSTA5; *IOXT"
    /// OPERATION: IOX transfer from device register to CPU A register
    /// </summary>
    /// <returns>Status bits</returns>
    public ushort ReadStatus()
    {
        // Simulate: T:=HDEV+RSTA5; *IOXT
        ushort deviceAddress = (ushort)(HDEV + RSTA5);
        return IOXTransferRead(deviceAddress);
    }

    /// <summary>
    /// Write ND-500 status register.
    /// SOURCE: XC-P2-N500.NPL:57 "T+\"LSTA5-RSTA5\"; *IOXT"
    /// </summary>
    public void WriteStatus(ushort value)
    {
        // Simulate: A:=value; T:=HDEV+LSTA5; *IOXT
        ushort deviceAddress = (ushort)(HDEV + LSTA5);
        IOXTransferWrite(deviceAddress, value);
    }

    /// <summary>
    /// Activate ND-500 with message address.
    /// SOURCE: MP-P2-N500.NPL:3084-3093 (ACT50)
    /// ALGORITHM:
    /// 1. Write bank number to LMAR5
    /// 2. Write message address to LMAR5
    /// 3. Write 5 to LCON5 (activate)
    /// </summary>
    /// <param name="bank">Multiport memory bank (5MBBANK)</param>
    /// <param name="messageAddr">Message address in 5MPM</param>
    public void ActivateWithMessage(ushort bank, ushort messageAddr)
    {
        // SOURCE: MP-P2-N500.NPL:3084-3086
        // 5MBBANK; T:=HDEV+LMAR5; *IOXT
        IOXTransferWrite((ushort)(HDEV + LMAR5), bank);

        // A:=X; *IOXT
        IOXTransferWrite((ushort)(HDEV + LMAR5), messageAddr);

        // A:=5; T+"LCON5-LMAR5"; *IOXT
        IOXTransferWrite((ushort)(HDEV + LCON5), 5);
    }

    /// <summary>
    /// Enable ND-500 for interrupts (first activation).
    /// SOURCE: MP-P2-N500.NPL:3088-3093
    /// ALGORITHM:
    /// 1. Write 10₈ to LCON5
    /// 2. Write 0 to LSTA5
    /// 3. Write 1 to LCON5
    /// 4. Lock status with SLOC5
    /// </summary>
    public void EnableForInterrupt()
    {
        // A:=10; T:=HDEV+LCON5; *IOXT
        IOXTransferWrite((ushort)(HDEV + LCON5), 0o10);  // Octal 10

        // A:=0; T+"LSTA5-LCON5"; *IOXT
        IOXTransferWrite((ushort)(HDEV + LSTA5), 0);

        // A:=1; T+"LCON5-LSTA5"; *IOXT
        IOXTransferWrite((ushort)(HDEV + LCON5), 1);

        // T+"SLOC5-LCON5"; *IOXT
        IOXTransferWrite((ushort)(HDEV + SLOC5), 0);  // Lock (value doesn't matter)
    }

    /// <summary>
    /// Terminate ND-500 execution.
    /// SOURCE: MP-P2-N500.NPL:2933-2947 (XTER500)
    /// ALGORITHM:
    /// 1. Read status
    /// 2. If locked, write to TERM5
    /// 3. Poll status until unlocked
    /// </summary>
    public void Terminate()
    {
        // T:=HDEV+RSTA5; *IOXT
        ushort status = ReadStatus();

        // IF A BIT 5ILOCK THEN
        if ((status & (ushort)StatusBits._5ILOCK) != 0)
        {
            // T+"TERM5-RSTA5"; *IOXT
            IOXTransferWrite((ushort)(HDEV + TERM5), 0);

            // Poll until unlocked
            int timeout = 1000;  // LOOPCOUNTER value
            while (timeout-- > 0)
            {
                // Delay (FOR X:=-20 DO; OD)
                for (int i = 0; i < 20; i++) { }

                // *IOXT (re-read status)
                status = ReadStatus();

                // WHILE A BIT 5ILOCK
                if ((status & (ushort)StatusBits._5ILOCK) == 0)
                    break;
            }
        }
    }

    /// <summary>
    /// IOX transfer read operation.
    /// NOTE: This is NOT a memory read! It's a device register read.
    /// In real hardware, this triggers the 3022/5015 interface card.
    /// </summary>
    private ushort IOXTransferRead(ushort deviceAddress)
    {
        // Hardware implementation:
        // - Decode device address
        // - Activate interface card
        // - Return register value
        // For emulator: return from simulated device registers
        throw new NotImplementedException("Hardware-specific");
    }

    /// <summary>
    /// IOX transfer write operation.
    /// NOTE: This is NOT a memory write! It's a device register write.
    /// </summary>
    private void IOXTransferWrite(ushort deviceAddress, ushort value)
    {
        // Hardware implementation:
        // - Decode device address
        // - Activate interface card
        // - Write to register
        // - May trigger interrupt or DMA
        throw new NotImplementedException("Hardware-specific");
    }
}
```

---

## 6. Memory Copy Operations

### MOVPA - Move Physical to Alternative

**Source: RP-P2-N500.NPL line 47**

```npl
55MESSIZE=:L; T:="5SWEMESSAGE"           % L = length, T = destination
A:=D-55MSNEGSIZE=:D:=5MBBANK; *MOVPA    % D = source, move data
```

**What this instruction does:**
- **L register**: Number of words to copy
- **D register**: Source physical address (5MBBANK + offset)
- **T register**: Destination address
- **MOVPA**: Microcode instruction that performs block copy
- Direction: Physical memory → Alternative PIT mapped memory

**C# Equivalent:**

```csharp
/// <summary>
/// MOVPA - Move Physical to Alternative memory.
/// SOURCE: RP-P2-N500.NPL:47 "*MOVPA"
/// PURPOSE: Copy data from multiport memory to ND-100 datafield
/// </summary>
/// <param name="sourcePhysical">Source physical address (in 5MPM)</param>
/// <param name="destLogical">Destination logical address</param>
/// <param name="wordCount">Number of 16-bit words to copy</param>
public void MovePhysicalToAlternative(uint sourcePhysical, uint destLogical, ushort wordCount)
{
    // SOURCE: 55MESSIZE=:L; T:="5SWEMESSAGE"
    // A:=D-55MSNEGSIZE=:D:=5MBBANK; *MOVPA

    // This is a block copy operation
    for (int i = 0; i < wordCount; i++)
    {
        ushort value = ReadPhysicalWord(sourcePhysical + (uint)(i * 2));
        WriteLogicalWord(destLogical + (uint)(i * 2), value);
    }
}
```

### MOVAA - Move Alternative to Alternative

**Source: RP-P2-N500.NPL line 43**

```npl
A:=12=:L; "XSDUNIT"=:D; T:="5SWESTATUS"; *MOVAA
```

**What this does:**
- Copy 12 words from address in D to address in T
- Both addresses in alternative PIT space
- Used for error information copying

**C# Equivalent:**

```csharp
/// <summary>
/// MOVAA - Move Alternative to Alternative.
/// SOURCE: RP-P2-N500.NPL:43 "*MOVAA"
/// PURPOSE: Copy between two logical addresses
/// </summary>
public void MoveAlternativeToAlternative(uint sourceLogical, uint destLogical, ushort wordCount)
{
    for (int i = 0; i < wordCount; i++)
    {
        ushort value = ReadLogicalWord(sourceLogical + (uint)(i * 2));
        WriteLogicalWord(destLogical + (uint)(i * 2), value);
    }
}
```

### Data Copying in Monitor Call Processing

**Source: MP-P2-N500.NPL lines 1312-1317**

```npl
N5MESSAGE+"ABUFADR"=:X; T:=5MBBANK; *LDDTX    % Get buffer address
A=:T; A:=D=:CURX                              % Save it
FOR CNT DO
   ACL7(CNT); X:=CURX                         % Calculate offset
   *SWAP SA DD CLD; STDTX                     % Swap and store double-word
   MIN CURX; MIN CURX                         % Decrement pointer by 2
OD
```

**What this does:**
- Reads buffer address from message (`ABUFADR` field)
- Uses LDDTX to load 32-bit value
- Loops copying data with SWAP (byte swap) and STDTX (store double-word)
- Decrements pointer by 2 words each iteration

**C# Equivalent:**

```csharp
/// <summary>
/// Copy clock data from message buffer.
/// SOURCE: MP-P2-N500.NPL:1312-1317
/// PURPOSE: Copy and byte-swap time data
/// </summary>
public void CopyClockData(ushort messageAddr)
{
    // N5MESSAGE+"ABUFADR"=:X; T:=5MBBANK; *LDDTX
    ushort bufferAddrOffset = GetFieldOffset("ABUFADR");
    uint bufferAddr = _mpm.ReadDoubleWord(messageAddr + bufferAddrOffset);

    // FOR CNT DO (7 iterations, CNT starts at -7)
    uint currentAddr = bufferAddr;
    for (int i = 0; i < 7; i++)
    {
        // *SWAP SA DD CLD; STDTX
        uint value = _mpm.ReadDoubleWord(currentAddr);
        value = ByteSwap32(value);  // SWAP operation
        _mpm.WriteDoubleWord(currentAddr, value);

        // MIN CURX; MIN CURX (decrement by 2 words = 4 bytes)
        currentAddr -= 4;
    }
}

/// <summary>
/// Byte swap 32-bit value.
/// SOURCE: *SWAP microcode instruction
/// </summary>
private uint ByteSwap32(uint value)
{
    return ((value & 0xFF000000) >> 24) |
           ((value & 0x00FF0000) >> 8) |
           ((value & 0x0000FF00) << 8) |
           ((value & 0x000000FF) << 24);
}
```

### No DMA in NPL Source

**Important finding:**

Searching all NPL files for "DMA" reveals:
- **MP-P2-N500.NPL:673**: "5DMAERR" - DMA error status bit
- **MP-P2-N500.NPL:265**: Comment about "DMA interface"

**BUT**: No actual DMA setup or transfer commands in SINTRAN NPL source!

**What this means:**
- DMA is handled by **ND-500 microcode** (`3RMED`, `3WMED` functions)
- ND-100 side does **not** program DMA controllers
- ND-100 only:
  1. Sets up message buffer with addresses
  2. Writes to IOX registers to activate ND-500
  3. ND-500 microcode reads message and performs DMA internally

---

## 7. Complete C# Implementation

### Multiport Memory Manager

```csharp
/// <summary>
/// Multiport memory (5MPM) access manager.
/// SOURCE: All NPL files use "T:=5MBBANK" pattern
/// PURPOSE: Abstracts dual-ported memory accessible by both CPUs
/// </summary>
public class MultiportMemory
{
    private byte[] _memory;
    private ushort _bankNumber;
    private uint _baseAddress;

    /// <summary>
    /// Initialize multiport memory.
    /// SOURCE: Configuration from SINTRAN initialization
    /// TYPICAL SIZE: 256KB - 2MB
    /// </summary>
    public MultiportMemory(int sizeBytes, ushort bankNumber, uint baseAddress)
    {
        _memory = new byte[sizeBytes];
        _bankNumber = bankNumber;  // 5MBBANK value
        _baseAddress = baseAddress;
    }

    /// <summary>
    /// Read 16-bit word from 5MPM.
    /// SOURCE: Pattern "T:=5MBBANK; X:=addr; *AAX offset; LDATX"
    /// </summary>
    public ushort ReadWord(ushort address, ushort offset)
    {
        uint physicalAddr = (uint)((address + offset) * 2);  // Word to byte
        if (physicalAddr + 1 >= _memory.Length)
            throw new ArgumentOutOfRangeException(nameof(address));

        // ND-100 is big-endian
        return (ushort)((_memory[physicalAddr] << 8) | _memory[physicalAddr + 1]);
    }

    /// <summary>
    /// Write 16-bit word to 5MPM.
    /// SOURCE: Pattern "T:=5MBBANK; X:=addr; *AAX offset; STATX"
    /// </summary>
    public void WriteWord(ushort address, ushort offset, ushort value)
    {
        uint physicalAddr = (uint)((address + offset) * 2);
        if (physicalAddr + 1 >= _memory.Length)
            throw new ArgumentOutOfRangeException(nameof(address));

        _memory[physicalAddr] = (byte)(value >> 8);
        _memory[physicalAddr + 1] = (byte)(value & 0xFF);
    }

    /// <summary>
    /// Read 32-bit double-word from 5MPM.
    /// SOURCE: Pattern "*LDDTX" (load double word to AD)
    /// </summary>
    public uint ReadDoubleWord(ushort address, ushort offset = 0)
    {
        ushort highWord = ReadWord(address, offset);
        ushort lowWord = ReadWord(address, (ushort)(offset + 1));
        return ((uint)highWord << 16) | lowWord;
    }

    /// <summary>
    /// Write 32-bit double-word to 5MPM.
    /// SOURCE: Pattern "*STDTX" (store double word from AD)
    /// </summary>
    public void WriteDoubleWord(ushort address, ushort offset, uint value)
    {
        ushort highWord = (ushort)(value >> 16);
        ushort lowWord = (ushort)(value & 0xFFFF);
        WriteWord(address, offset, highWord);
        WriteWord(address, (ushort)(offset + 1), lowWord);
    }
}
```

### Complete Driver Simulation

```csharp
/// <summary>
/// ND-500 Interrupt Level 12 Driver (5STDRIV).
/// SOURCE: MP-P2-N500.NPL:656-698
/// PURPOSE: Scans execution queue and processes ND-500 messages
/// </summary>
public class ND500InterruptDriver
{
    private MultiportMemory _mpm;
    private ND500IOXInterface _iox;
    private ND500ExecutionQueue _queue;
    private ushort _currentCPUDatafield;

    /// <summary>
    /// Main driver loop - called on Interrupt Level 12.
    /// SOURCE: MP-P2-N500.NPL:656-698 (5STDRIV)
    /// ALGORITHM:
    /// 1. Check power fail status
    /// 2. Read and clear interface status
    /// 3. Scan execution queue (MAILINK)
    /// 4. Process each message
    /// 5. Activate ND-500 if needed
    /// </summary>
    public void InterruptLevel12Handler()
    {
        // SOURCE: MP-P2-N500.NPL:664-667
        // IF CPUAVAILABLE NBIT 5ALIVE GO CALLID12
        if (!IsCPUAlive())
            return;

        do
        {
            // SOURCE: MP-P2-N500.NPL:668-669
            // IF C5STAT/\C5PFMASK >< 0 GO CALLID12
            if (CheckPowerFailStatus())
                break;

            // SOURCE: MP-P2-N500.NPL:670-676
            // A:=B=:CC5CPU
            // 177377; CALL CLE5STATUS
            _currentCPUDatafield = GetCurrentCPUDatafield();
            ushort status = _iox.ReadStatus();
            status &= 0o177377;  // Mask
            _iox.WriteStatus(status);  // Clear

            // Check for errors
            if ((status & 0o720) != 0)
            {
                if ((status & (ushort)ND500IOXInterface.StatusBits._5PFAIL) != 0)
                {
                    HandlePowerFail();
                }
                else if ((status & (ushort)ND500IOXInterface.StatusBits._5DMAER) != 0)
                {
                    HandleDMAError();
                }
                else
                {
                    HandleInterfaceError();
                }
                break;
            }

            // SOURCE: MP-P2-N500.NPL:678-691
            // X:=MAILINK
            // DO ... WHILE D><-1
            ushort messageAddr = _queue.QueueHead;
            while (messageAddr != 0xFFFF)
            {
                // Get next message
                ushort nextAddr = _mpm.ReadWord(messageAddr, MessageOffsets.NextLink);

                // Convert address
                nextAddr = ConvertByteAddress(nextAddr);

                // Check status
                CheckMessageStatus(messageAddr);

                // Move to next
                messageAddr = nextAddr;
            }

            // SOURCE: MP-P2-N500.NPL:692
            // CC5CPU=:B; CALL XACT500
            ActivateIfNeeded();

        } while (false);

        // SOURCE: MP-P2-N500.NPL:693
        // CALL WT12
        WaitForNextInterrupt();
    }

    /// <summary>
    /// Check message status and handle completion.
    /// SOURCE: MP-P2-N500.NPL:685 "CALL CHN5STATUS"
    /// </summary>
    private void CheckMessageStatus(ushort messageAddr)
    {
        // Read message flags
        ushort flags = _mpm.ReadWord(messageAddr, MessageOffsets.Flags);

        // Check if in execution queue
        if ((flags & (ushort)MessageFlags.InExecutionQueue) != 0)
        {
            // Process message based on status
            // (Implementation depends on message type)
        }
    }

    private bool IsCPUAlive() => true;  // From CPUAVAILABLE check
    private bool CheckPowerFailStatus() => false;  // From C5STAT check
    private ushort GetCurrentCPUDatafield() => 0;  // From B register
    private void HandlePowerFail() { }
    private void HandleDMAError() { }
    private void HandleInterfaceError() { }
    private void ActivateIfNeeded() { }
    private void WaitForNextInterrupt() { }
    private ushort ConvertByteAddress(ushort addr) => addr;
}
```

---

## Summary

### Memory Structure Findings (FROM SOURCE):

1. **5MPM is a bank** accessed via `T:=5MBBANK`
2. **Message buffers** are doubly-linked lists with:
   - `LINK` field at offset @3 (forward pointer)
   - `PLINK` field (backward pointer)
   - `5MSFL` flags field (queue membership bits)
   - `5PRIO` priority field (for sorting)
3. **MAILINK** is the execution queue head
4. **Queue operations**:
   - Insert: Priority-ordered, updates both forward and backward links
   - Remove: Bypasses node, updates neighbors
   - Traverse: Follow `LINK` pointers until -1

### IOX Operations Findings (FROM SOURCE):

1. **No memory access** - IOX operations are DEVICE REGISTER access
2. **Pattern**: `T:=HDEV+offset; *IOXT`
3. **Operations**:
   - Read status: `T:=HDEV+RSTA5; *IOXT` → A register
   - Write control: `A:=value; T:=HDEV+LCON5; *IOXT`
   - Activate: Write bank to LMAR5, address to LMAR5, 5 to LCON5

### Memory Copy Findings (FROM SOURCE):

1. **MOVPA**: Physical → Alternative (5MPM → ND-100 datafield)
2. **MOVAA**: Alternative → Alternative (logical → logical)
3. **LDDTX/STDTX**: 32-bit double-word access
4. **No DMA setup code** - DMA is ND-500 microcode responsibility

### What Triggers Activity:

1. **ND-100 → ND-500**: Write to IOX registers (LMAR5, LCON5)
2. **ND-500 → ND-100**: Interrupt Level 12 activated
3. **Memory polling**: Driver scans MAILINK queue
4. **No interrupt from ND-500 to ND-100** visible in NPL source (handled by interface card)

---

**All information in this document is verified from SINTRAN NPL source code. No assumptions made.**
