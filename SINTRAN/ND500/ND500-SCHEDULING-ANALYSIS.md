# ND-500 Process Scheduling - Deep Analysis

## Purpose

Complete analysis of how SINTRAN III (running on ND-100) schedules and manages processes on the ND-500 coprocessor. SINTRAN is the **master scheduler** - it maintains all process state and controls which process runs on the ND-500.

**Key Sources**:
- CC-P2-N500.NPL (execution queue management, status routines)
- MP-P2-N500.NPL (XACT500 activation)
- RP-P2-N500.NPL (N500SCHEDULER timeslicer)

---

## 1. Scheduling Architecture Overview

```mermaid
flowchart TB
    subgraph "ND-100 (Master Scheduler)"
        A[SINTRAN Kernel]
        B[Execution Queue<br/>500XQ]
        C[Process Descriptions]
        D[N500SCHEDULER<br/>Timeslicer]
        E[XACT500<br/>Activator]
    end

    subgraph "5MPM Shared Memory"
        F[Message Buffers]
        G[Status Fields]
        H[Priority Values]
    end

    subgraph "ND-500 (Slave Processor)"
        I[Current Process]
        J[Execution]
    end

    A --> B
    A --> C
    A --> D
    D --> E
    E --> F
    B <--> F
    F <--> G
    F <--> H
    E -->|LMAR5/LCON5| I
    I --> J
    J -->|Interrupt/Completion| A

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style B fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
    style E fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style I fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
```

**Key Principle**: The ND-500 does NOT schedule itself. SINTRAN on ND-100:
- Maintains all process state
- Manages the execution queue
- Decides which process runs next
- Physically activates the ND-500 hardware

---

## 2. The Execution Queue (500XQ)

### 2.1 Queue Structure

The execution queue is a **priority-ordered linked list** of messages (processes) waiting for ND-500 CPU time.

```mermaid
flowchart LR
    subgraph "Execution Queue (500XQ)"
        M[MAILINK<br/>Queue Head] --> P1[Process A<br/>Priority 100]
        P1 --> P2[Process B<br/>Priority 80]
        P2 --> P3[Process C<br/>Priority 50]
        P3 --> P4[Process D<br/>Priority 30]
        P4 --> E["-1"<br/>End of Queue]
    end

    style M fill:#3F51B5,stroke:#303F9F,stroke-width:2px,color:#fff
    style P1 fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style E fill:#F44336,stroke:#D32F2F,stroke-width:2px,color:#fff
```

**Queue Properties**:
- **MAILINK**: Queue head pointer (in CPU datafield)
- **LINK**: Forward pointer to next message
- **PLINK**: Backward pointer to previous message
- **5PRIO**: Priority value (higher = more urgent)
- **5IEXQUEUE**: Flag indicating process is in queue

### 2.2 ITO500XQ - Insert To Execution Queue

**Source**: CC-P2-N500.NPL lines 232-266

```mermaid
flowchart TD
    A[ITO500XQ Entry<br/>X = Message] --> B{Already in queue?<br/>5IEXQUEUE set?}
    B -->|Yes| EXIT1[EXIT - Already queued]
    B -->|No| C[Set 5IEXQUEUE flag]
    C --> D[Get message priority<br/>5PRIO]
    D --> E[Start at MAILINK]
    E --> F{Next message exists?}
    F -->|No| G[Append at end]
    F -->|Yes| H[Get next message priority]
    H --> I{Current priority < New priority?}
    I -->|Yes| J[Insert before current]
    I -->|No| K[Move to next message]
    K --> F
    J --> L[Update LINK pointers]
    G --> L
    L --> M[Increment LEXQUEUE counter]
    M --> N[Return]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style C fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style J fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
```

**Key Code** (lines 241-252):

```
Line 241: *AAX 5PRIO; LDATX                    % Get new message priority
Line 242: A=:L; T:=5MBBANK; X:=MAILINK
Line 243: DO
Line 244:    *LINK@3 LDDTX
Line 245: WHILE D><-1                          % Traverse queue
Line 249:    *AAX 5PRIO; LDATX; AAX -5PRIO     % A = current.5PRIORITY
Line 250:    IF A<L THEN                       % Insert before if lower priority
Line 251:       X:=D; GO ITO51
Line 252:    FI
Line 253: OD
```

### 2.3 IFM500XQ - Remove From Execution Queue

**Source**: CC-P2-N500.NPL lines 286-306

```mermaid
flowchart TD
    A[IFM500XQ Entry<br/>X = Message] --> B{In queue?<br/>5IEXQUEUE set?}
    B -->|No| EXIT1[EXIT - Not in queue]
    B -->|Yes| C[Clear 5IEXQUEUE flag]
    C --> D[Decrement LEXQUEUE counter]
    D --> E[Get LINK and PLINK]
    E --> F[Update previous.LINK = current.LINK]
    F --> G{Next exists?}
    G -->|Yes| H[Update next.PLINK = current.PLINK]
    G -->|No| I[Done]
    H --> I
    I --> J[Return]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style C fill:#F44336,stroke:#D32F2F,stroke-width:2px,color:#fff
```

---

## 3. Process Status Management

### 3.1 Status Read/Write Routines

**Source**: CC-P2-N500.NPL lines 679-687

| Routine | Purpose | Code |
|---------|---------|------|
| **RN5STATUS** | Read process status from 5MPM | `*BSET BCM 120 DX; N5STA@3 LDATX` |
| **WN5STATUS** | Write process status to 5MPM | `*N5STA@3 STATX` |

**Note**: RN5STATUS reads twice to "fool the cache" - ensures fresh data from shared memory.

### 3.2 Process Status Values

| Status | Meaning | When Set |
|--------|---------|----------|
| **MSGN500** | Ready for ND-500 CPU | After monitor call completion (MCCO) |
| **WAITING** | Waiting for ND-500 CPU | In queue, not yet selected |
| **I5TMQU** | In time queue | Waiting for timeout |
| **STOPPED** | Process stopped | Explicit stop command |
| **SWPWAIT** | Waiting for swapper | Needs page swap |
| **SWPPING** | Using swapper | Swapper processing request |

### 3.3 Status Transition Diagram

```mermaid
stateDiagram-v2
    [*] --> WAITING: ITO500XQ
    WAITING --> MSGN500: Selected by XACT500
    MSGN500 --> Active: ND-500 activated
    Active --> WAITING: Preempted/Timesliced
    Active --> I5TMQU: MON 5TMOUT
    I5TMQU --> WAITING: Timer expires
    Active --> SWPWAIT: Needs page
    SWPWAIT --> SWPPING: Swapper available
    SWPPING --> MSGN500: Swap complete
    Active --> STOPPED: Stop command
    STOPPED --> [*]: Process terminated

    note right of Active
        Running on ND-500
        SINTRAN sets 5ACTIVE
    end note
```

---

## 4. XACT500 - Process Activation

### 4.1 Overview

**XACT500** is the main routine that selects and activates the next process on ND-500.

**Source**: MP-P2-N500.NPL lines 3057-3099

### 4.2 Activation Flow

```mermaid
flowchart TD
    A[XACT500 Entry] --> B[Read MAILINK from 5MPM]
    B --> C[Check X5CPU status]
    C --> D{MPACTIVE and<br/>no power fail?}
    D -->|No| EXIT1[Return - CPU not ready]
    D -->|Yes| E[Read RSTA5 status]
    E --> F{5CLOST set?<br/>Clock stopped?}
    F -->|Yes| EXIT2[Return - ND-500 stopped]
    F -->|No| G{5ILOCK set?<br/>Already running?}
    G -->|Yes| H[Call XTER500<br/>Terminate first]
    G -->|No| I[Search execution queue]
    H --> I
    I --> J{Find message with<br/>MSGN500 or WAITING?}
    J -->|No| K[Enable for interrupt only]
    J -->|Yes| L[ACT50: Activate ND-500]
    L --> M[Load bank to LMAR5]
    M --> N[Load message address]
    N --> O[Write 5 to LCON5<br/>ACTIVATE!]
    O --> P[Return]
    K --> P

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style L fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style O fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
```

### 4.3 Key Code (ACT50 Activation)

```
Line 3071: X:=MAILINK
Line 3072: DO                                  % Search execution queue
Line 3073:    T:=5MBBANK; *LINK@3 LDDTX       % Next message
Line 3074: WHILE D><-1
Line 3075:    IF X:=D><DUMMESS THEN
Line 3076:       CALL RN5STATUS                % Get message status
Line 3077:       IF A=MSGN500 OR A=WAITING GO ACT50  % Ready for CPU?
Line 3078:    FI
Line 3079: OD

Line 3084: ACT50: 5MBBANK; T:=HDEV+LMAR5; *IOXT    % Load bank
Line 3085:        A:=X; *IOXT                      % Load message address
Line 3086:        A:=5; T+"LCON5-LMAR5"; *IOXT     % ACTIVATE (bits 0+2)
```

### 4.4 Enable for Interrupt (No Process Ready)

When no process is ready, XACT500 enables ND-500 to generate an interrupt when it finishes:

```
Line 3089: A:=10; T:=HDEV+LCON5; *IOXT    % Test mode
Line 3090: A:=0;  T+"LSTA5-LCON5"; *IOXT  % Clear status
Line 3091: A:=1;  T+"LCON5-LSTA5"; *IOXT  % Enable interrupt
Line 3092:        T+"SLOC5-LCON5"; *IOXT  % Lock interface
```

---

## 5. LOWACT500 - Low-Level Activation

### 5.1 Purpose

**LOWACT500** activates ND-500 from **lower interrupt levels** (not from driver level 12).

**Source**: CC-P2-N500.NPL lines 318-326

### 5.2 Mechanism

```mermaid
flowchart TD
    A[LOWACT500 Entry<br/>B = CPU datafield] --> B{ND-5000 system?}
    B -->|Yes| EXIT[Direct EXIT]
    B -->|No| C[XLOWACT500]
    C --> D[Store B in LV12B register]
    D --> E[Store '5STDRIV' driver address]
    E --> F[Trigger Level 12 interrupt]
    F --> G[Return - Driver will activate]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style F fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
```

**Key Code**:

```
Line 322: XLOWACT500:
Line 323:        A:=B; *IRW LV12B DB           % Store CPU datafield
Line 324:        "5STDRIV"; *IRW LV12B DP      % Store driver address
Line 325:        LV12; *MST PID; EXIT          % Trigger Level 12
```

**Why Level 12?**: The ND-500 driver runs at interrupt level 12. LOWACT500 schedules the actual activation to run at the proper level.

---

## 6. N500SCHEDULER - Timeslicer

### 6.1 Overview

**N500SCHEDULER** is the timeslicer for ND-500 processes. It's called from the ND-100 timeslicer (RTSLI) and handles:
- CPU time tracking
- Priority adjustment based on CPU usage
- Timeslice expiration
- Process preemption

**Source**: RP-P2-N500.NPL lines 60-179

### 6.2 Timeslicer Flow

```mermaid
flowchart TD
    A[N500SCHEDULER Entry] --> B{B5STOP set?<br/>ND-500 stopped?}
    B -->|Yes| EXIT1[Return]
    B -->|No| C[Loop over CPU datafields]
    C --> D{Any CPU alive<br/>and running?}
    D -->|No| EXIT2[Return - Nothing to do]
    D -->|Yes| E[NN5S1: Begin timeslicing]
    E --> F[Loop over process descriptions]
    F --> G{Process exists and<br/>SLICE flag set?}
    G -->|No| NEXT[Next process]
    G -->|Yes| H[Get timeslice counters]
    H --> I{Break priority<br/>waiting?}
    I -->|Yes| J[Increase priority]
    I -->|No| K[Calculate CPU time used]
    K --> L{Timeslice expired?}
    L -->|No| NEXT
    L -->|Yes| M[SETALL: Adjust priority]
    M --> N[TSL5CHXQ: Reorganize queue]
    N --> NEXT
    J --> M
    NEXT --> O{More processes?}
    O -->|Yes| F
    O -->|No| P[EDOX: Restart stopped CPUs]
    P --> Q[Return]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style E fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style M fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
    style N fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
```

### 6.3 Timeslice Classes and Priority Tables

The timeslicer uses tables to determine priority adjustments:

| Table | Purpose |
|-------|---------|
| **TSLPRITAB** | Priority value for each timeslice element |
| **TSLTIMTAB** | Time limit for each timeslice element |
| **TSLNEXTAB** | Next element in timeslice chain |
| **TSLLPRITAB** | Low priority threshold per class |
| **TSLBRKELEM** | Break element per class |

### 6.4 Key Timeslice Fields (per process)

| Field | Purpose |
|-------|---------|
| **5TSLC** | Timeslice counter |
| **5TSLS** | Timeslice status |
| **5PRIO** | Current priority |
| **L500C** | ND-500 CPU time used (low 16 bits) |
| **SLICE** | Process is timesliced flag |

### 6.5 TSL5CHXQ - Queue Reorganization

After priority adjustment, **TSL5CHXQ** (called at line 159) reorganizes the execution queue to maintain priority order.

---

## 7. Complete Scheduling Cycle

### 7.1 Sequence Diagram

```mermaid
sequenceDiagram
    participant P as Process
    participant K as SINTRAN Kernel
    participant Q as Execution Queue
    participant X as XACT500
    participant H as Hardware (IOX)
    participant N as ND-500 CPU

    P->>K: Request service (MON call)
    K->>K: Process request
    K->>Q: ITO500XQ (queue message)
    K->>X: Call XACT500
    X->>Q: Search for ready process
    Q-->>X: Return first ready message
    X->>H: Load LMAR5 (bank)
    X->>H: Load LMAR5 (address)
    X->>H: Write LCON5=5 (ACTIVATE)
    H->>N: Hardware activation
    N->>N: Execute process code

    alt Process completes
        N->>H: Set status, interrupt
        H->>K: Interrupt handler
        K->>Q: IFM500XQ (dequeue)
        K->>P: Return result
    else Timeslice expires
        K->>K: N500SCHEDULER
        K->>Q: TSL5CHXQ (reorganize)
        K->>X: Call XACT500
        X->>N: Activate next process
    end
```

### 7.2 State Machine

```mermaid
stateDiagram-v2
    direction LR

    state "ND-100 Domain" as ND100 {
        [*] --> Queued: ITO500XQ
        Queued --> Selected: XACT500 selects
        Selected --> Activated: IOX LCON5=5
    }

    state "ND-500 Domain" as ND500 {
        Activated --> Running: Hardware start
        Running --> Completed: Work done
        Running --> Preempted: Timeslice/Higher priority
    }

    Completed --> Queued: More work
    Completed --> [*]: Process done
    Preempted --> Queued: Back to queue
```

---

## 8. Priority-Based Scheduling Details

### 8.1 Priority Insertion

When a process is inserted into the queue (ITO500XQ), it's placed **before** all processes with lower priority:

```
Line 250: IF A<L THEN                    % If current priority < new priority
Line 251:    X:=D; GO ITO51              % Insert before current
```

### 8.2 Priority Adjustment by Timeslicer

The timeslicer adjusts priority based on CPU time consumed:

```
Line 127: A:=CL5CPU-5TNEXT=:D:=0
Line 128: T:=TSLTUNIT; *RDIV ST          % Compute CPU time used
Line 129: IF A+5TCOUNT<0 GO CONWAIT      % Timeslice finished?
Line 130: CL5CPU=:5TNEXT                 % Set new reference time
Line 131: CTSLSTATUS/\TSLELMSK=:X        % Get timeslice element
Line 132: A:=CTSLSTATUS/\177400\/TSLNEXTAB(X)  % Find next element
```

**Result**: Processes that use more CPU time get lower priority (fair scheduling).

---

## 9. Key Data Structures

### 9.1 CPU Datafield (5CPUDF)

| Field | Purpose |
|-------|---------|
| **MAILINK** | Head of execution queue (MAILI=000022 verified from SYMBOL-1-LIST.SYMB.TXT) |
| **CPUAVAILABLE** | CPU status flags (5ALIVE=bit 13, LV1ACT, LV2ACT) |
| **C5STAT** | CPU status (power fail flags) |
| **HDEV** | Hardware device address |

> **Note:** 5ALIVE (5ALIV=000015) is bit 13 in CPUAVAILABLE, NOT in RSTA5 status register.

### 9.2 Message Buffer (in 5MPM)

| Field | Offset | Purpose |
|-------|--------|---------|
| **LINK** | +0 | Forward pointer in queue |
| **PLINK** | varies | Backward pointer |
| **N5STA** | varies | Process status |
| **5PRIO** | varies | Priority value |
| **5MSFL** | varies | Message flags (5IEXQUEUE) |
| **5TSLC** | varies | Timeslice counter |
| **5TSLS** | varies | Timeslice status |

### 9.3 Process Description (S500S array)

| Field | Purpose |
|-------|---------|
| **RTRES** | Resource reference (0 = not used) |
| **PSTAT** | Process status flags |
| **MESSBUFF** | Pointer to message buffer |
| **SLICE** | Timeslice flag |

---

## 10. Emulator Implementation Notes

### 10.1 What the Emulator Must Track

| Component | Emulator Requirement |
|-----------|---------------------|
| **Execution Queue** | Maintain linked list in 5MPM |
| **Process Status** | Track N5STA field per message |
| **Priority** | Maintain 5PRIO, handle insertion order |
| **Timeslice State** | Track 5TSLC, 5TSLS, L500C |
| **Queue Flags** | Track 5IEXQUEUE in 5MSFL |

### 10.2 Key IOX Commands for Scheduling

| Command | Register | Value | Effect |
|---------|----------|-------|--------|
| Load bank | LMAR5 | 5MBBANK | Set memory bank |
| Load address | LMAR5 | message addr | Set message location |
| **ACTIVATE** | LCON5 | 5 (0x05) | Start ND-500 |
| Enable interrupt | LCON5 | 1 (0x01) | Wait for completion |

### 10.3 Interrupt Flow

```mermaid
sequenceDiagram
    participant N as ND-500
    participant H as Hardware
    participant I as Interrupt Handler
    participant K as Kernel

    N->>H: Process completes, sets status
    H->>I: Generate interrupt (Level 12)
    I->>K: Read completion status
    K->>K: IFM500XQ (dequeue)
    K->>K: Process result
    K->>K: XACT500 (activate next)
```

---

## 11. Related Documents

- [ND500-IF-USAGE-DEEP-ANALYSIS.md](ND500-IF-USAGE-DEEP-ANALYSIS.md) - IOX commands and 500HIST polling
- [ND500-SWAPPER-ANALYSIS.md](ND500-SWAPPER-ANALYSIS.md) - Swapper event-driven scheduling
- [MP-P2-N500.md](MP-P2-N500.md) - Main driver module analysis
- [CC-P2-N500.md](CC-P2-N500.md) - Command/control module analysis
- [RP-P2-N500.md](RP-P2-N500.md) - Runtime/timeslicer analysis
- [../OS/17-SCHEDULER-AND-PRIORITIES.md](../OS/17-SCHEDULER-AND-PRIORITIES.md) - General SINTRAN scheduling

---

## 12. Verification Checklist

- [x] Execution queue structure documented (500XQ)
- [x] ITO500XQ insertion algorithm documented
- [x] IFM500XQ removal algorithm documented
- [x] Process status values and transitions documented
- [x] XACT500 activation flow documented with Mermaid
- [x] LOWACT500 low-level activation documented
- [x] N500SCHEDULER timeslicer documented
- [x] Priority-based scheduling mechanism documented
- [x] Complete scheduling cycle sequence diagram
- [x] Key data structures documented
- [x] Emulator implementation notes included
- [x] No speculation presented as fact

---

**Document Version**: 1.0
**Last Updated**: 2026-01-29
**Sources**: CC-P2-N500.NPL (lines 232-326, 679-687), MP-P2-N500.NPL (lines 3057-3099), RP-P2-N500.NPL (lines 60-179)
