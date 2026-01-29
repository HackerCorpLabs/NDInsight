# ND-500 Swapper (5SWAP) - Deep Analysis

## Purpose

Complete analysis of the SINTRAN III swapper subsystem for ND-500. The swapper is an **event-driven segment loading and page swapping service** - NOT a passive polling mechanism.

**Source**: MP-P2-N500.NPL (lines 1031-1188, 2851-2908, 2970-3040)

---

## 1. What the Swapper Does

The **5SWAP** process running on ND-500 handles:

| Function | Description |
|----------|-------------|
| **Segment allocation** | Allocating disk pages for new memory segments |
| **Page swapping** | Reading/writing memory pages to/from disk |
| **Disk I/O coordination** | Managing disk transfers via queue pool elements |
| **Memory management offload** | Moving work from ND-100 to ND-500 CPU |

---

## 2. Swapper State Machine

### 2.1 Process States (from perspective of processes using the swapper)

| Symbol | Value (Oct) | Value (Dec) | Meaning |
|--------|-------------|-------------|---------|
| LSWPWAIT | 4 | 4 | Process waiting for swapper (blocked in queue) |
| LSWPPING | 6 | 6 | Process using swapper (request being processed) |
| LACTIVE | 12 | 10 | Process active (has swapper result) |

### 2.2 Swapper Internal States

| Symbol | Meaning |
|--------|---------|
| PSWWAIT | Swapper is idle, waiting for work |
| SWPPING | Swapper is actively processing a request |
| PSW1WAIT | Swapper waiting for disk I/O completion |

### 2.3 State Transition Diagram

```mermaid
stateDiagram-v2
    [*] --> LACTIVE: Process running
    LACTIVE --> LSWPWAIT: Needs swapper service
    LSWPWAIT --> LSWPPING: Swapper accepts request
    LSWPPING --> LACTIVE: Swapper completes
    LSWPWAIT --> LSWPWAIT: Swapper busy (queued in FIFO)

    note right of LSWPWAIT
        Process blocked
        Inserted in Swap-wait-FIFO
        if swapper busy
    end note

    note right of LSWPPING
        Swapper processing request
        Disk I/O in progress
    end note
```

---

## 3. Complete Request Flow

### 3.1 High-Level Flow

```mermaid
flowchart TD
    A[Process needs swapper] --> B[Call 5ACTSWAPPER]
    B --> C{Swapper free?<br/>PSWWAIT}
    C -->|Yes| D[Set LSWPPING state]
    C -->|No| E[Insert in Swap-wait-FIFO]
    D --> F[XACTRDY: Select work]
    F --> G[LOWACT500: Activate ND-500]
    G --> H[ND-500 processes request]
    H --> I{Disk I/O needed?}
    I -->|Yes| J[M5TRANS: Disk transfer]
    I -->|No| K[Direct memory operation]
    J --> L[Wait for disk completion]
    L --> M[SWPD4: Swapper idle]
    K --> M
    M --> N{FIFO has waiting?}
    N -->|Yes| O[Dequeue next request]
    N -->|No| P[Stay idle PSWWAIT]
    O --> D
    E --> Q[Wait in queue]
    Q --> O

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style G fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style H fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
    style J fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style M fill:#009688,stroke:#00796B,stroke-width:2px,color:#fff
```

### 3.2 5ACTSWAPPER Subroutine (Lines 2851-2908)

This is the **key entry point** for requesting swapper service.

```mermaid
flowchart TD
    A[5ACTSWAPPER Entry] --> B[Set process to SWPWAIT state]
    B --> C[Read swapper status from SWMSG]
    C --> D{Swapper status = PSWWAIT?}
    D -->|Yes - Swapper free| E[Set swapper to SWPPING]
    D -->|No - Swapper busy| F[Get FIFO pointers from N500DF]
    E --> G[Call XACTRDY]
    G --> H[Call LOWACT500]
    H --> I[Return - request activated]
    F --> J[Calculate FIFO insert position]
    J --> K[Insert message in Swap-wait-FIFO]
    K --> L[Return - request queued]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style E fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style H fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style K fill:#FFA726,stroke:#F57C00,stroke-width:2px,color:#fff
```

**Source** (MP-P2-N500.NPL lines 2862-2906):

```
Line 2862: SWPWAIT; CALL WN5STATUS                    % Mark process waiting
Line 2864: X:=SWMSG; CALL RN5STATUS
Line 2865: IF A=PSWWAIT THEN                          % Swapper free?
Line 2870:    SWPPING; CALL WN5STATUS                 % Mark swapper in use
Line 2871:    X:=SWMSG; CALL XACTRDY                  % Reactivate ND-500
Line 2872:    CALL LOWACT500
Line 2898: ELSE
Line 2899:    T:=5MBBANK; X:="N500DF".X500DF
Line 2900:    *AAX X5MXF; LDATX                       % Get FIFO pointers
Line 2901:    A=:L; *AAX X5SWF-X5MXF; LDDTX
Line 2902:    IF A=:D+1>=L THEN A:=0 FI               % Wrap FIFO
Line 2903:    D SH 1=:L; *AAX X5SWB-X5SWF; LDDTX      % Compute FIFO address
Line 2904:    X:=D+L; T:=A+C; CMSGTOSW; *STDTX        % Insert message
Line 2906: FI
```

---

## 4. FIFO Queue Processing (SWPD4)

When the swapper completes a request, it checks the Swap-wait-FIFO for waiting processes.

### 4.1 SWPD4 Flow Diagram

```mermaid
flowchart TD
    A[SWPD4: Swapper request complete] --> B[Set swapper status to PSWWAIT]
    B --> C[Get FIFO read pointer X5SWF]
    C --> D[Get FIFO write pointer X5SWT]
    D --> E{Read = Write?<br/>FIFO empty?}
    E -->|Yes| F[Return - stay idle]
    E -->|No| G[Read message from FIFO]
    G --> H[Increment read pointer]
    H --> I{Pointer >= max?}
    I -->|Yes| J[Wrap pointer to 0]
    I -->|No| K[Continue]
    J --> K
    K --> L[Get waiting process]
    L --> M{Process still in LSWPWAIT?}
    M -->|No - cancelled| C
    M -->|Yes| N[Call 5ACTSWAPPER]
    N --> O[Process request]

    style A fill:#009688,stroke:#00796B,stroke-width:2px,color:#fff
    style F fill:#FFA726,stroke:#F57C00,stroke-width:2px,color:#fff
    style N fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
```

**Source** (MP-P2-N500.NPL lines 1031-1066):

```
Line 1031: SWPD4: PSWWAIT; X:=SWMSG; CALL WN5STATUS   % Swapper now idle
Line 1032:        T:=5MBBANK; X:="N500DF".X500DF
Line 1033:        *AAX X5SWF; LDDTX                   % Get FIFO read pointer
Line 1034:        A=:D; *AAX X5SWT-X5SWF; LDATX       % Get FIFO write pointer
Line 1035:        IF A=D GO SWPD5                     % FIFO empty? Exit
Line 1040:        % Dequeue and process waiting request
Line 1050:        IF process still in LSWPWAIT        % Still waiting?
Line 1052:           CALL 5ACTSWAPPER                 % Yes, activate it
```

---

## 5. Work Triggering Mechanisms

### 5.1 What Triggers Actual Work

The swapper is **event-driven**, not polling-based. Work is triggered by:

```mermaid
flowchart LR
    subgraph "Triggers"
        A[Process calls 5ACTSWAPPER]
        B[Timer expires in 500HIST]
        C[Disk I/O completes]
        D[ND-500 interrupt]
    end

    subgraph "Activation Path"
        E[XACTRDY: Select highest priority]
        F[LOWACT500: Hardware activation]
        G[ND-500 CPU wakes up]
    end

    subgraph "Work Execution"
        H[Process swapper message]
        I[Disk I/O if needed]
        J[Return result via 5MPM]
    end

    A --> E
    B --> E
    C --> E
    D --> E
    E --> F
    F --> G
    G --> H
    H --> I
    I --> J

    style F fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style G fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
```

### 5.2 XACTRDY - Priority-Based Work Selection (Lines 2970-3040)

```mermaid
flowchart TD
    A[XACTRDY Entry] --> B[Get current ND-500 CPU status]
    B --> C{Swapper message waiting?<br/>A><MSGN500 AND A><WAITING}
    C -->|No| D[Return - nothing to do]
    C -->|Yes| E[Scan all ND-500 CPUs]
    E --> F{CPU alive and no power fail?}
    F -->|No| G[Next CPU]
    F -->|Yes| H[Get maillink from 5MPM]
    H --> I[Check X5CPU status]
    I --> J{MPACTIVE?}
    J -->|No| G
    J -->|Yes| K[Get current process]
    K --> L[Compare priorities]
    L --> M{Higher priority found?}
    M -->|Yes| N[Update selection]
    M -->|No| G
    N --> G
    G --> O{More CPUs?}
    O -->|Yes| E
    O -->|No| P[XKICK500: Activate selected]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style P fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
```

### 5.3 LOWACT500 - Physical ND-500 Activation

**LOWACT500** is the **critical hardware activation routine**. It sets flags that physically wake up the ND-500 CPU.

```
Line 245: CPUAVAILABLE BONE LV2ACT=:CPUAVAILABLE
Line 251: CALL LOWACT500
Line 459: CALL LOWACT500; LTTMR=:TMR    % Reactivate ND-500
```

| Action | Effect |
|--------|--------|
| Set LV2ACT bit in CPUAVAILABLE | Signal that Level 2 work is ready |
| Write to hardware registers | Physical wake-up of ND-500 |
| ND-500 exits idle loop | Begins processing messages |

---

## 6. Swapper Operations

### 6.1 LSWPAGE - Disk I/O (Lines 1070-1114)

```mermaid
flowchart TD
    A[LSWPAGE Entry] --> B[Set status PSW1WAIT]
    B --> C[Extract disk parameters from SWMSG]
    C --> D[Get disk unit, function, address, length]
    D --> E[Allocate queue pool element QP5SW]
    E --> F[Set up transfer parameters]
    F --> G[Call M5TRANS - initiate transfer]
    G --> H[Wait for disk completion]
    H --> I{Transfer successful?}
    I -->|Yes| J[Call OKMONICO - success]
    I -->|No| K[Call EMONICO - error 1055]
    J --> L[Call XACT500 - reactivate]
    K --> L
    L --> M[Process next message]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style G fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style J fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style K fill:#F44336,stroke:#D32F2F,stroke-width:2px,color:#fff
```

**Source** (MP-P2-N500.NPL lines 1070-1095):

```
Line 1070: LSWPAGE:
Line 1071:    X:=SWMSG; PSW1WAIT; CALL WN5STATUS
Line 1072:    T:=5MBBANK; *AAX HSWPI; LDDTX
Line 1080:    % Extract disk parameters
Line 1085:    T:="QP100".QP5SW                    % Swapper queue element
Line 1086:    -1=:X.QP5SW; X:=T                   % "LINK OUT"
Line 1088:    "5SWAP"=:X.RTRES; 0=:X.NLINK        % Mark swapper as owner
Line 1092:    *LDF I (XABSF; STF ABFUN,X          % Function code
Line 1093:    *LDD I (XABLO; STD ABPA2,X          % Disk address
Line 1094:    *LDA I (XABLN; STA ABP31,X          % Byte length
Line 1095:    CALL M5TRANS; GO BUSR
```

### 6.2 LALLOPAGE - Page Allocation (Lines 1170-1188)

```mermaid
flowchart TD
    A[LALLOPAGE Entry] --> B[Set status PSW1WAIT]
    B --> C[Save swapper message to CPU datafield]
    C --> D[Call ND-500 allocation routine]
    D --> E[Set status 5IALLOPAGE]
    E --> F[Get CPU datafield]
    F --> G{Allocation successful?}
    G -->|Yes| H[Return page address]
    G -->|No| I[Return error]
    H --> J[Call XACT500]
    I --> J
    J --> K[Process next message]

    style A fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    style D fill:#9C27B0,stroke:#7B1FA2,stroke-width:2px,color:#fff
    style H fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    style I fill:#F44336,stroke:#D32F2F,stroke-width:2px,color:#fff
```

---

## 7. Communication via 5MPM

### 7.1 Shared Memory Structure

```mermaid
flowchart TB
    subgraph "ND-100 Side"
        A[SINTRAN Kernel]
        B[5ACTSWAPPER]
        C[XACTRDY]
    end

    subgraph "5MPM Multiport Memory"
        D[SWMSG - Swapper message buffer]
        E[N500DF - ND-500 datafield]
        F[Swap-wait-FIFO]
        G[Process descriptions]
    end

    subgraph "ND-500 Side"
        H[5SWAP Process]
        I[LSWPAGE Handler]
        J[LALLOPAGE Handler]
    end

    A --> B
    B --> D
    C --> D
    D --> H
    H --> I
    H --> J
    I --> D
    J --> D
    E --> F

    style D fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style E fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
    style F fill:#E91E63,stroke:#C2185B,stroke-width:2px,color:#fff
```

### 7.2 Key 5MPM Addresses

| Symbol | Purpose |
|--------|---------|
| SWMSG | Swapper message buffer (request/response) |
| N500DF.X500DF | ND-500 system datafield base |
| X5SWF | Swap-wait-FIFO read pointer |
| X5SWT | Swap-wait-FIFO write pointer |
| X5MXF | FIFO maximum size |
| X5SWB | FIFO base address |

---

## 8. Complete Swapper Lifecycle

```mermaid
sequenceDiagram
    participant P as Process
    participant K as ND-100 Kernel
    participant M as 5MPM Memory
    participant S as 5SWAP (ND-500)
    participant D as Disk

    P->>K: Request page (page fault)
    K->>K: 5ACTSWAPPER
    K->>M: Write request to SWMSG
    K->>M: Check swapper status

    alt Swapper free (PSWWAIT)
        K->>M: Set SWPPING status
        K->>K: XACTRDY + LOWACT500
        K->>S: Hardware activation
        S->>M: Read SWMSG request
        S->>D: M5TRANS (disk read)
        D-->>S: Data transfer complete
        S->>M: Write result to SWMSG
        S->>M: Set PSWWAIT (idle)
        S->>K: Interrupt/completion
        K->>P: Resume with page
    else Swapper busy
        K->>M: Insert in Swap-wait-FIFO
        K->>P: Block process (LSWPWAIT)
        Note over S: Later, when current request completes
        S->>M: SWPD4: Check FIFO
        S->>M: Dequeue waiting request
        S->>S: Process request
    end
```

---

## 9. Key Insights for Emulator Developers

### 9.1 The Swapper is NOT Polling

The swapper is **event-driven**:

1. **Process requests service** → 5ACTSWAPPER called
2. **XACTRDY evaluates priority** → selects highest-priority work
3. **LOWACT500 activates ND-500** → hardware interrupt/flag
4. **ND-500 wakes up** → processes request from 5MPM
5. **Completion triggers next** → SWPD4 checks FIFO

### 9.2 Critical Code Paths to Emulate

| Path | Source Lines | Purpose |
|------|--------------|---------|
| 5ACTSWAPPER | 2851-2908 | Request entry point |
| XACTRDY | 2970-3040 | Priority selection |
| LOWACT500 | 245, 251, 459 | Hardware activation |
| SWPD4 | 1031-1066 | FIFO processing |
| LSWPAGE | 1070-1114 | Disk I/O |
| LALLOPAGE | 1170-1188 | Page allocation |

### 9.3 State Transitions to Track

```mermaid
stateDiagram-v2
    direction LR

    [*] --> Idle: System start
    Idle --> ProcessingRequest: 5ACTSWAPPER
    ProcessingRequest --> WaitingDisk: LSWPAGE
    WaitingDisk --> ProcessingRequest: Disk complete
    ProcessingRequest --> Idle: SWPD4

    state Idle {
        [*] --> CheckFIFO
        CheckFIFO --> WaitingWork: FIFO empty
        CheckFIFO --> DequeueNext: FIFO has work
    }
```

---

## 10. Related Documents

- [ND500-IF-USAGE-DEEP-ANALYSIS.md](ND500-IF-USAGE-DEEP-ANALYSIS.md) - IOX commands and 500HIST polling
- [ND-500-INTERFACE.md](ND-500-INTERFACE.md) - Hardware interface overview
- [MP-P2-N500.md](MP-P2-N500.md) - Main driver module analysis
- [../OS/15-DISK-IO-SUBSYSTEM.md](../OS/15-DISK-IO-SUBSYSTEM.md) - Disk I/O and queue pools
- [../OS/17-SCHEDULER-AND-PRIORITIES.md](../OS/17-SCHEDULER-AND-PRIORITIES.md) - Task scheduling

---

## 11. Verification Checklist

- [x] Swapper purpose documented (segment loading, page swapping)
- [x] State machine documented (LSWPWAIT, LSWPPING, LACTIVE)
- [x] 5ACTSWAPPER flow documented with Mermaid diagram
- [x] FIFO queue processing (SWPD4) documented
- [x] Work triggering mechanisms documented (XACTRDY, LOWACT500)
- [x] Disk I/O path (LSWPAGE) documented
- [x] Page allocation path (LALLOPAGE) documented
- [x] 5MPM communication structure documented
- [x] Complete lifecycle sequence diagram included
- [x] No speculation presented as fact

---

**Document Version**: 1.0
**Last Updated**: 2026-01-29
**Source**: MP-P2-N500.NPL (lines 1031-1188, 2851-2908, 2970-3040)
