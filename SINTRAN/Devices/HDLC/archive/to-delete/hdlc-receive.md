# HDLC Reception Flow - Detailed MERMAID Diagrams
**Complete Visual Flow Analysis of SINTRAN HDLC Packet Reception**

## Overview

This document provides detailed MERMAID flowcharts showing every decision point, bit test, and processing path in SINTRAN HDLC reception, from hardware interrupt to user application delivery.

## Main Reception Flow

```mermaid
flowchart TD
    A[Hardware Interrupt Level 13] --> A1{WRTC Interrupt Enables<br/>Check BitIE Flags}
    A1 -->|Disabled| A2[IGNORE IRQ<br/>No Processing]
    A1 -->|Enabled| B[HIINT Entry Line 104436]
    B --> B1[Check WRTC BitIE Settings]
    B1 --> C[Read RRTS Register IOX+10]
    C --> C1{DMA IRQ Enabled?<br/>WRTC DMATransferEnable = 1?}
    C1 -->|No| C2[Character-Only Processing]
    C1 -->|Yes| D[Store in HASTAT Variable]
    D --> E{ACTSW = 0?<br/>Receiver Active?}

    E -->|Yes - Inactive| F[Increment T9 Counter]
    F --> G[GO OUT1 - Exit]

    E -->|No - Active| H{X.21 Error Check<br/>RRTS and X21D or X21S != 0?<br/>0x6000}

    H -->|Yes - X.21 Error| I[X.21 Error Handler]
    I --> J{X.21 Clear?<br/>RRTS.X21S = 1?<br/>bit 14}
    J -->|Yes| K[Set BLDON in HASTAT<br/>Set XBLDN in LKEY]
    J -->|No| L[Continue Error Processing]
    K --> L
    L --> M{List Empty?<br/>RRTS.ListEmpty != 0?<br/>0x0800}

    H -->|No - X.21 OK| M

    M -->|Yes - FATAL| N[RECEIVER SHUTDOWN<br/>ACTSW = 0]
    N --> O[Increment STPCNT]
    O --> P[Copy DMA List to BUFF3]
    P --> G

    M -->|No - Buffers OK| Q[Enter Block Processing Loop]
    Q --> R[MORE: Get LKEY from DMA Descriptor]
    R --> S{XBLDN Test<br/>LKEY.BlockDone = 0?<br/>0x0008}

    S -->|Yes - Block Incomplete| T{Enable Receiver?<br/>LKEY = ERB?}
    T -->|Yes| U[GO FAR ZSTARC<br/>Enable Receiver]
    T -->|No| G
    U --> U1[ZSTARC: Write WRTC=0x35<br/>DataAvailableIE+StatusAvailableIE<br/>+ReceiverActiveIE+SyncFlagIE<br/>+SignalDetectorIE]
    U1 --> G

    S -->|No - Block Complete| V[CALL HNOTRA<br/>Process Buffer Content]
    V --> W[HNOTRA Entry Line 104611]
    W --> X[Access DMA Descriptor<br/>Get Message Size]
    X --> Y[CALL XMPAT<br/>Get DCB from List]
    Y --> Z[Extract LKEY Value<br/>Clean up Descriptor]
    Z --> AA{RCOST Validation<br/>LKEY & LMASK = RSOM+REOM?<br/>0x6377 = 3}

    AA -->|Yes - Valid| BB[SUCCESS PATH<br/>A = 0]
    BB --> CC[CALL SCRET<br/>Set Return Code]
    CC --> DD[CALL SADTS<br/>Store Hardware Status]

    AA -->|No - Invalid| EE{X.21 Clear in LKEY?<br/>LKEY.X21S = 1?<br/>bit 14}
    EE -->|Yes| FF[EX21 Error Handler]
    EE -->|No| GG[EINP Error Handler]
    FF --> HH[ERROR PATH<br/>A = LKEY]
    GG --> HH
    HH --> II[CALL SCRET<br/>Set Error Code]
    II --> JJ[CALL SADTS<br/>Store LKEY Status]
    JJ --> KK[OR Status to DSTAT<br/>Increment HDERC]

    DD --> LL[X-BHEAD Adjustment]
    KK --> LL
    LL --> MM[CALL OCHAIN<br/>Deliver to User/XMSG]
    MM --> MM1[Write WRTC=0x3DC<br/>All BasicIE + BlockEndIE<br/>+FrameEndIE+ListEndIE<br/>+ListEmptyIE+ReceiverOverrunIE]
    MM1 --> NN[Advance DMA Pointer<br/>LIINT + 4]
    NN --> OO[Cache Management<br/>Pre-fetch Next]
    OO --> PP{End of List?<br/>LKEY = NLP?}
    PP -->|Yes| QQ[Set LISTP = LIINT]
    PP -->|No| RR[GO MORE<br/>Process Next Block]
    QQ --> RR
    RR --> R

    style A fill:#ff9999
    style G fill:#ffcccc
    style N fill:#ff6666
    style BB fill:#99ff99
    style MM fill:#66ff66
```

## WRTC Interrupt Enable Sequence Flow

```mermaid
sequenceDiagram
    participant SINTRAN as SINTRAN OS
    participant WRTC as WRTC Register
    participant Hardware as HDLC Hardware
    participant IRQ as IRQ Controller

    Note over SINTRAN,IRQ: Device Initialization Phase
    SINTRAN->>WRTC: Write 0x64 (RECSET)<br/>ReceiverEnable+DataTerminalReady
    WRTC->>Hardware: Basic receiver enabled

    Note over SINTRAN,IRQ: DMA List Setup Phase
    SINTRAN->>WRTC: Write 0x35 (ZSTARC)<br/>DataAvailableIE+ReceiverEnable<br/>+DMATransferEnable
    WRTC->>Hardware: DMA interrupts enabled
    Hardware->>IRQ: Can now trigger Level 13 IRQs

    Note over SINTRAN,IRQ: Active Operation Phase
    SINTRAN->>WRTC: Write 0x3DC (OUT1)<br/>ALL IE flags enabled
    Note right of WRTC: DataAvailableIE+ReceiverEnable<br/>+StatusAvailableIE+DMATransferEnable<br/>+SignalDetectorIE+DataSetReadyIE<br/>+BlockEndIE+FrameEndIE

    Note over SINTRAN,IRQ: Packet Reception
    Hardware->>Hardware: Set BlockEnd + FrameEnd bits
    Hardware->>IRQ: Check WRTC interrupt enables
    IRQ->>SINTRAN: Trigger Level 13 only if enabled
    SINTRAN->>Hardware: Read RRTS, clear DMA bits
```

## Detailed Bit Testing Flow

```mermaid
flowchart TD
    A[RRTS Register Read<br/>16 Bits Available] --> B{Test Sequence}

    B --> C{DataAvailable Test<br/>RRTS bit 0}
    C -->|0 - No Data| D[DROP PACKET<br/>GO OUT1]
    C -->|1 - Data Available| E{Combined X.21 Test<br/>Bits 13-14}

    E -->|!= 0 - Protocol Error| F[X.21 Error Handler]
    E -->|= 0 - Protocol OK| G{ListEmpty Test<br/>RRTS bit 11}

    G -->|1 - FATAL| H[SHUTDOWN RECEIVER<br/>ACTSW = 0]
    G -->|0 - Buffers Available| I[Continue to DMA Processing]

    I --> J[Get LKEY from DMA Descriptor]
    J --> K{BlockDone Test<br/>LKEY bit 3 XBLDN}
    K -->|0 - Block Incomplete| L[Exit Block Processing]
    K -->|1 - Block Complete| M[Continue to Content Validation]

    M --> N{RCOST Test<br/>LKEY & LMASK<br/>0x6377}
    N -->|!= 3 - Invalid Format| O[Protocol Error]
    N -->|= 3 - Valid Format| P[SUCCESS - Deliver Packet]

    Q[Ignored RRTS Bits<br/>1,2,3,5,6,7,8,9,10,12,15] --> R[No Impact on Flow]

    style D fill:#ff6666
    style H fill:#ff3333
    style P fill:#66ff66
    style R fill:#cccccc
```

## Hardware vs Software Control Flow with Interrupt Enables

```mermaid
flowchart LR
    A[Hardware Events] --> A1[WRTC Enable Check<br/>Hardware Validation]
    A1 --> B[RRTS Register<br/>RTSBits flags]
    A --> C[DMA Descriptor<br/>LKEY control flags]

    A1 --> A2[RTCBits.BlockEndIE<br/>RTCBits.FrameEndIE<br/>RTCBits.DMATransferEnable]
    A2 --> A3[IRQ Enable Gate<br/>Hardware decides trigger]

    B --> D[RTSBits.BlockEnd<br/>RTSBits.FrameEnd<br/>RTSBits.ListEnd]
    B --> E[RTSBits.DataAvailable<br/>RTSBits.ListEmpty<br/>RTSBits.X21D or X21S]

    C --> F[LKEY BlockDone<br/>bit 3 XBLDN]
    C --> G[LKEY RCOST<br/>bits 0-1 RSOM+REOM]

    D --> H[IGNORED by SINTRAN<br/>Never Tested]
    E --> I[TESTED by SINTRAN<br/>Controls Flow]
    F --> J[ESSENTIAL for SINTRAN<br/>Block Control]
    G --> K[CRITICAL for SINTRAN<br/>Format Validation]

    A3 --> L[IRQ Level 13 Trigger]
    I --> M[HIINT Processing]
    J --> N[Block Loop Control]
    K --> O[HNOTRA Validation]

    L --> P[Interrupt Delivery]
    M --> Q[Continue/Drop Decision]
    N --> R[Process/Skip Decision]
    O --> S[Success/Error Decision]

    style A2 fill:#ff6666
    style A3 fill:#ff9999
    style H fill:#ffcccc
    style I fill:#99ff99
    style J fill:#66ff66
    style K fill:#33ff33
```

## Status Information Flow to XMSG

```mermaid
flowchart TD
    A[RRTS Hardware Status] --> B[HASTAT Variable]
    C[LKEY DMA Descriptor] --> D[LKEY Processing]

    B --> E[Status Validation<br/>Bits 0,11,13,14]
    D --> F[Block Control<br/>Bit 3 XBLDN]
    D --> G[Format Control<br/>Bits 0-1 RCOST]

    E --> H{Processing Result}
    F --> H
    G --> H

    H -->|Success| I[A = 0<br/>Success Code]
    H -->|Error| J[A = LKEY<br/>Error with Details]

    I --> K[CALL SCRET<br/>Store Return Code]
    J --> K
    K --> L[CALL SADTS<br/>Store Hardware Status]

    L --> M[DCB Structure Creation]
    M --> N[DCB.CRET_STATUS<br/>Return Code]
    M --> O[DCB.ADDSTA<br/>Hardware Status]
    M --> P[DCB.LHAST<br/>Last RRTS Value]
    M --> Q[DCB.UserData<br/>Packet Content]

    N --> R[CALL OCHAIN]
    O --> R
    P --> R
    Q --> R

    R --> S{DCB.MESSID < 0?<br/>XMSG DCB?}

    S -->|Yes| T[XMSG Handler<br/>* XCHAI@3 LDATX]
    S -->|No| U[Standard Driver Queue]

    T --> V[XMSG Processing<br/>Access to All Status]
    U --> W[Application Queue]

    V --> X[Higher-Level Protocols<br/>X.25, LAPB, etc.]
    W --> Y[User Application]

    X --> Z[Application Layer]
    Y --> Z

    style I fill:#99ff99
    style J fill:#ffcc99
    style V fill:#66ccff
    style X fill:#3399ff
```

## Critical Decision Points

```mermaid
flowchart TD
    A[Packet Reception] --> B[Decision Point 1<br/>Combined RRTS Test]

    B --> C{DataAvailable = 1<br/>AND<br/>X21Errors = 0}
    C -->|No| D[IMMEDIATE DROP<br/>GO OUT1]
    C -->|Yes| E[Decision Point 2<br/>Fatal Check]

    E --> F{ListEmpty = 0}
    F -->|No - FATAL| G[RECEIVER SHUTDOWN<br/>ACTSW = 0<br/>NO MORE PACKETS]
    F -->|Yes| H[Decision Point 3<br/>Block Processing]

    H --> I{XBLDN = 1<br/>in LKEY}
    I -->|No| J[SKIP BLOCK<br/>GO OUT1]
    I -->|Yes| K[Decision Point 4<br/>Content Validation]

    K --> L{RCOST = RSOM+REOM<br/>LKEY & LMASK = 3<br/>0x6377}
    L -->|No| M[PROTOCOL ERROR<br/>Increment Counter]
    L -->|Yes| N[SUCCESS<br/>DELIVER PACKET]

    O[Override Priority] --> P[ListEmpty Check<br/>ALWAYS wins]
    P --> Q[X.21 Error Check<br/>Second priority]
    Q --> R[DataAvailable Check<br/>Third priority]
    R --> S[XBLDN Check<br/>Fourth priority]
    S --> T[RCOST Check<br/>Final validation]

    style D fill:#ff6666
    style G fill:#ff3333
    style J fill:#ffcc66
    style M fill:#ff9966
    style N fill:#66ff66
    style P fill:#ff3333
```

## Complete Processing Timeline

```mermaid
gantt
    title SINTRAN HDLC Reception Processing Timeline
    dateFormat X
    axisFormat %s

    section Hardware
    Receive_Frame    :0, 10
    Set_RRTS_Bits    :10, 15
    Set_LKEY_XBLDN   :10, 15
    Trigger_IRQ_13   :15, 20

    section HIINT
    Read_RRTS        :20, 25
    Store_HASTAT     :25, 30
    Activity_Check   :30, 35
    X21_Check        :35, 45
    ListEmpty_Check  :45, 55

    section Block_Loop
    Read_LKEY        :55, 60
    XBLDN_Test       :60, 65
    Call_HNOTRA      :65, 70

    section HNOTRA
    DMA_Access       :70, 75
    Get_DCB          :75, 80
    RCOST_Test       :80, 90
    Status_Calls     :90, 100

    section Delivery
    OCHAIN_Call      :100, 110
    XMSG_Check       :110, 115
    Queue_Deliver    :115, 125

    section Next_Block
    Advance_Pointer  :125, 130
    Cache_Mgmt       :130, 135
    Next_Block       :135, 140
```

## Error Handling Paths

```mermaid
flowchart TD
    A[HDLC Reception Errors] --> B[Error Type Classification]

    B --> C[Fatal Errors<br/>Receiver Shutdown]
    B --> D[Protocol Errors<br/>Packet Drop]
    B --> E[Format Errors<br/>Counter Increment]

    C --> F[ListEmpty = 1<br/>RRTS Bit 11]
    F --> G[ACTSW = 0<br/>Receiver Dead]
    G --> H[STPCNT Increment<br/>Copy DMA List to BUFF3]

    D --> I[DataAvailable = 0<br/>RRTS Bit 0]
    D --> J[X21D/X21S = 1<br/>RRTS Bits 13-14]
    I --> K[Immediate Drop<br/>GO OUT1]
    J --> L[X.21 Error Handler<br/>Protocol State Update]

    E --> M[XBLDN = 0<br/>LKEY Bit 3]
    E --> N[RCOST != 3<br/>LKEY & 0x6377]
    M --> O[Block Skip<br/>Continue to Next]
    N --> P[Error Counter<br/>HDERC Increment]
    P --> Q[DSTAT Update<br/>Status Accumulation]

    Q --> R[SADTS Call<br/>Forward Error to XMSG]
    R --> S[OCHAIN Delivery<br/>Error Status Available]

    style G fill:#ff3333
    style K fill:#ff6666
    style L fill:#ffcc66
    style P fill:#ff9966
    style S fill:#66ccff
```

## Summary

These MERMAID diagrams provide complete visual documentation of:

1. **Main Processing Flow**: From interrupt to delivery
2. **Bit Testing Logic**: Showing which bits matter and why
3. **Hardware vs Software**: The dual control systems
4. **XMSG Integration**: Status forwarding to higher levels
5. **Decision Points**: Critical validation stages
6. **Timeline**: Processing sequence and timing
7. **Error Handling**: All failure modes and their impacts

The diagrams clearly show that SINTRAN HDLC reception involves sophisticated multi-layered validation, with only specific bits controlling the flow while others are completely ignored. The key insight is the separation between hardware status reporting (RRTS) and software flow control (LKEY XBLDN).

## HDLC Initialization Flow (HDSIN Function)

```mermaid
flowchart TD
    A[User Application] --> B[HDLC Init Message<br/>MODUS, CFSIZE, IRTRY, IDISP]
    B --> C[HDSIN Function Entry<br/>Line 105027]

    C --> D{INTSTA Check<br/>Interface Available?}
    D -->|No| E[Return ENCLEAR Error]
    D -->|Yes| F[Read MODUS from Message<br/>IMODU@3 LDATX]

    F --> G{MODUS Value?}
    G -->|0| H[Full-Duplex Mode<br/>CMODI = 0]
    G -->|1| I[Half-Duplex Mode<br/>CMODI = 40 octal]
    G -->|2| J[Maintenance Mode<br/>CMODI = 0]
    G -->|Invalid| K[Return EPAR Error]

    H --> L[Set MAINT = 100<br/>Normal Operation]
    I --> M[Set MAINT = 140<br/>Half-Duplex Operation]
    J --> N[Set MAINT = 140<br/>Maintenance Operation]

    L --> O[Write WRTC Register<br/>Enable Basic Functions]
    M --> O
    N --> O

    O --> P[Read Frame Size<br/>IFSIZ@3 LDATX]
    P --> Q{Frame Size Valid?<br/>CFSIZE >= 1}
    Q -->|No| R[Return EILFZ Error]
    Q -->|Yes| S[Store MAXR = CFSIZE]

    S --> T[Read Retry Count<br/>IRTRY@3 LDATX]
    T --> U[Store XRETRY = IRTRY]
    U --> V[Initialize Counters<br/>CHECK = 0]
    V --> W[Setup Complete<br/>Interface Ready]

    style I fill:#ffcc99
    style M fill:#ffcc99
    style W fill:#99ff99
```

## Half-Duplex Mode Operation Flow

```mermaid
flowchart TD
    A[Half-Duplex Mode Active<br/>CMODI = 40] --> B[Transmission Request]

    B --> C{Check Remote ReadyForSending<br/>RTTS.ReadyForSending = 1?}
    C -->|No| D[Wait for Remote Ready<br/>Remote Station Busy]
    C -->|Yes| E[Assert RTS<br/>TTCBits.RequestToSend = 1]

    D --> F[Monitor Remote Status<br/>Poll RTTS Register]
    F --> G{Timeout Reached?}
    G -->|Yes| H[Transmission Failed<br/>Remote Not Ready]
    G -->|No| C

    E --> I[Write WTTC Register<br/>1134 + CMODI = 1174]
    I --> J[Start Data Transmission<br/>DMA Transfer Begin]

    J --> K[Monitor Transmission<br/>Wait for IRQ Level 12]
    K --> L[HOINT Interrupt<br/>Transmission Complete]

    L --> M{CMODI = 40?<br/>Half-Duplex Check}
    M -->|Yes| N[Clear RTS<br/>TTCBits.RequestToSend = 0]
    M -->|No| O[Keep RTS Active<br/>Full-Duplex Mode]

    N --> P[Write WTTC Register<br/>Turn Off Request To Send]
    P --> Q[Transmission Complete<br/>Line Available for Receive]
    O --> Q

    Q --> R[Monitor for Receive<br/>Watch for Level 13 IRQ]
    R --> S{Receive Activity?}
    S -->|Yes| T[Process Incoming Data<br/>HIINT Handler]
    S -->|No| U[Ready for Next TX<br/>Return to Idle]

    style A fill:#ffcc99
    style E fill:#99ccff
    style N fill:#99ccff
    style Q fill:#99ff99
```

## X.21 Circuit-Switched Mode Flow

```mermaid
flowchart TD
    A[X.21 Mode Initialization] --> B[X.21 State Machine<br/>X2DST = 0 Ready]

    B --> C{Call Direction?}
    C -->|Outgoing| D[State 2: Call Request<br/>T=0, C=ON]
    C -->|Incoming| E[State 1: Incoming Call<br/>Wait for X21BL]

    D --> F[State 3: Proceed to Select<br/>Wait for + Signal]
    F --> G[State 7: Selection Signals<br/>Send Dialing Digits]
    G --> H[State 10: Selection Complete<br/>Wait for DCE Ready]
    H --> I[State 12: Call Progress<br/>Network Processing]
    I --> J[State 14: DATA PHASE<br/>Circuit Established]

    E --> K[Detect X21BL Character<br/>Incoming Call Indication]
    K --> L[Send Call Accepted<br/>Transition to Data Phase]
    L --> J

    J --> M[Enable HDLC Transmission<br/>HXDOK = X21OP]
    M --> N[WTTC Configuration<br/>134 octal NO ModemStatusChangeIE]
    N --> O[HDLC Data Transfer<br/>Standard HDLC Operations]

    O --> P{Call Termination?}
    P -->|Yes| Q[State 20: Call Clearing<br/>T=0, C=OFF]
    P -->|No| R[Continue Data Transfer<br/>Monitor X.21 Signals]

    Q --> S[State 24: DCE Clear<br/>Network Clearing]
    S --> T[State 25: DTE Clear<br/>Local Clearing Complete]
    T --> U[Return to Ready State<br/>X2DST = 0]

    R --> V{X.21 Error Signals?<br/>Check HX21M bits}
    V -->|Yes| W[X.21 Error Handler<br/>Set RRTS X21D/X21S]
    V -->|No| O

    W --> X[Force Circuit Disconnect<br/>Error Recovery]
    X --> Q

    style J fill:#99ff99
    style M fill:#66ff66
    style O fill:#33ff33
    style W fill:#ff9966
```

## Multi-Mode HDLC Controller State Machine

```mermaid
stateDiagram-v2
    [*] --> Uninitialized

    Uninitialized --> Initializing: HDSIN Call

    Initializing --> FullDuplex: MODUS=0
    Initializing --> HalfDuplex: MODUS=1
    Initializing --> Maintenance: MODUS=2
    Initializing --> Error: Invalid MODUS

    FullDuplex --> Transmitting: TX Request
    FullDuplex --> Receiving: RX Data
    FullDuplex --> X21Circuit: X.21 Protocol

    HalfDuplex --> CheckRemote: TX Request
    CheckRemote --> Transmitting: Remote Ready
    CheckRemote --> Waiting: Remote Busy
    Waiting --> CheckRemote: Retry Timer
    Waiting --> Error: Timeout

    Transmitting --> FullDuplex: TX Complete (Full)
    Transmitting --> HalfDuplex: TX Complete (Half)

    Receiving --> FullDuplex: RX Complete (Full)
    Receiving --> HalfDuplex: RX Complete (Half)

    X21Circuit --> CallSetup: Outgoing Call
    X21Circuit --> CallWait: Incoming Call
    CallSetup --> DataPhase: Circuit OK
    CallWait --> DataPhase: Call Accept
    DataPhase --> HDLCActive: Enable HDLC
    HDLCActive --> CallTeardown: End Call
    CallTeardown --> FullDuplex: Return to HDLC

    Maintenance --> DiagnosticMode: Test Functions
    DiagnosticMode --> Maintenance: Test Results

    Error --> [*]: Reset Required

    note right of HalfDuplex
        RTS Control Active
        ReadyForSending Monitoring
        Flow Control Enabled
    end note

    note right of X21Circuit
        State Machine: X2DST
        Signal Processing
        Circuit Management
    end note
```

## Complete HDLC Register Interaction Flow

```mermaid
sequenceDiagram
    participant App as User Application
    participant HDLC as HDLC Driver
    participant HW as Hardware Registers
    participant DMA as DMA Controller
    participant IRQ as Interrupt Controller

    Note over App,IRQ: Initialization Phase
    App->>HDLC: HDSIN(MODUS, CFSIZE, IRTRY)
    HDLC->>HDLC: Parse MODUS: 0=Full, 1=Half, 2=Maint
    HDLC->>HDLC: Set CMODI = (MODUS==1) ? 40 : 0
    HDLC->>HW: Write WRTC = Basic IE Flags
    HDLC->>App: Initialization Complete

    Note over App,IRQ: Transmission Setup
    App->>HDLC: Send Data Request
    HDLC->>DMA: Setup DMA List (LKEY, LBYTC, LMEM)
    HDLC->>HW: Write WDMA = DMA List Address
    HDLC->>HW: Write WDCR = Start DMA Command

    alt Half-Duplex Mode
        HDLC->>HW: Check RTTS.ReadyForSending
        HW-->>HDLC: Remote Status
        HDLC->>HW: Write WTTC = 1134+CMODI (Assert RTS)
    else Full-Duplex Mode
        HDLC->>HW: Write WTTC = 1134+CMODI (RTS Always On)
    end

    Note over App,IRQ: Data Transmission
    HW->>DMA: Begin Frame Transmission
    DMA->>HW: Set RTTS.TransmissionFinished
    HW->>IRQ: Trigger Level 12 IRQ
    IRQ->>HDLC: HOINT Interrupt
    HDLC->>HW: Read RTTS Status

    alt Half-Duplex Mode
        HDLC->>HW: Write WTTC = Clear RTS
    end

    HDLC->>App: Transmission Complete

    Note over App,IRQ: Reception Processing
    HW->>DMA: Receive Frame Data
    DMA->>HW: Set RRTS Flags (DataAvailable, BlockEnd, FrameEnd)
    DMA->>DMA: Set LKEY.XBLDN = 1
    HW->>IRQ: Trigger Level 13 IRQ
    IRQ->>HDLC: HIINT Interrupt

    HDLC->>HW: Read RRTS Register
    HW-->>HDLC: Status with Auto-Clear Behavior

    HDLC->>HDLC: Validate RRTS.DataAvailable = 1
    HDLC->>HDLC: Check RRTS.ListEmpty = 0
    HDLC->>HDLC: Verify RRTS X21D/X21S = 0
    HDLC->>DMA: Check LKEY.XBLDN = 1
    HDLC->>DMA: Validate LKEY & LMASK = 3

    alt All Validations Pass
        HDLC->>HDLC: OCHAIN Delivery to XMSG
        HDLC->>App: Packet Delivered Successfully
    else Validation Failed
        HDLC->>HDLC: Update Error Counters
        HDLC->>App: Error Status Report
    end
```

## Buffer Management and Flow Control

```mermaid
flowchart TD
    A[Incoming HDLC Frame] --> B{Buffer List Status}

    B -->|Buffers Available| C[Normal Processing<br/>RRTS.ListEmpty = 0]
    B -->|No Buffers| D[FATAL: Buffer Exhaustion<br/>RRTS.ListEmpty = 1]

    C --> E[DMA Buffer Assignment<br/>LKEY.XBLDN Processing]
    E --> F[Frame Reception<br/>Data to Memory]
    F --> G[Update Buffer Pointers<br/>Advance LIINT]
    G --> H{More Buffers in List?}

    H -->|Yes| I[Continue Reception<br/>Ready for Next Frame]
    H -->|No| J[Approach Buffer End<br/>RRTS.ListEnd Warning]

    D --> K[Receiver Shutdown<br/>ACTSW = 0]
    K --> L[Increment STPCNT<br/>Stop Event Counter]
    L --> M[Copy DMA List to BUFF3<br/>Debug Information]
    M --> N[Notify Remote Station<br/>Flow Control Action]

    N --> O{Half-Duplex Mode?}
    O -->|Yes| P[Clear ReadyForSending<br/>RTTS Signal Remote Stop]
    O -->|No| Q[Continue Full-Duplex<br/>Data Loss Possible]

    I --> R[Buffer Refill Process<br/>Application Processing]
    J --> S[Buffer Management<br/>Proactive Allocation]
    P --> T[Wait for Buffer Refill<br/>Flow Control Active]

    R --> U{Application Processing Speed}
    U -->|Fast Enough| V[Steady State<br/>No Flow Control Needed]
    U -->|Too Slow| W[Buffer Pressure<br/>Flow Control Required]

    S --> X[Allocate New Buffers<br/>Prevent List Exhaustion]
    T --> Y[Monitor Buffer Status<br/>Ready to Resume]
    Y --> Z[Re-enable Reception<br/>Set ReadyForSending = 1]

    W --> AA{Half-Duplex Available?}
    AA -->|Yes| P
    AA -->|No| BB[Accept Data Loss<br/>Full-Duplex Limitation]

    style D fill:#ff3333
    style K fill:#ff6666
    style P fill:#ffcc99
    style T fill:#ffcc99
    style V fill:#99ff99
    style Z fill:#99ff99
```

## Advanced Error Recovery and Diagnostics

```mermaid
flowchart TD
    A[HDLC Error Detection] --> B{Error Category}

    B --> C[Hardware Errors<br/>Register Level]
    B --> D[Protocol Errors<br/>X.21 or Format]
    B --> E[Buffer Errors<br/>DMA or Memory]
    B --> F[Timing Errors<br/>Synchronization]

    C --> G[RRTS Error Bits<br/>ReceiverOverrun, etc.]
    C --> H[RTTS Error Bits<br/>TransmitterUnderrun, etc.]
    G --> I[Log to BUFF2<br/>Hardware Status History]
    H --> I

    D --> J[X.21 State Errors<br/>HX21M Detection]
    D --> K[RCOST Format Errors<br/>LKEY Validation Failed]
    J --> L[X.21 Error Handler<br/>State Recovery]
    K --> M[Increment HDERC<br/>Error Counter]

    E --> N[ListEmpty Detection<br/>RRTS.ListEmpty = 1]
    E --> O[XBLDN Missing<br/>LKEY.BlockDone = 0]
    N --> P[RECEIVER SHUTDOWN<br/>Complete Stop]
    O --> Q[Block Skip<br/>Continue Processing]

    F --> R[Interrupt Timing<br/>IRQ Level Conflicts]
    F --> S[DMA Timing<br/>Transfer Coordination]
    R --> T[Interrupt Priority<br/>Level 12 vs 13]
    S --> U[DMA Synchronization<br/>Hardware Coordination]

    I --> V[Circular Buffer Logging<br/>BUFF0-BUFF3 Arrays]
    M --> V
    V --> W[Debug Information<br/>Status Analysis]

    L --> X[X.21 State Recovery<br/>Circuit Re-establishment]
    P --> Y[Manual Restart Required<br/>Application Intervention]
    Q --> Z[Continue Operation<br/>Error Logged]

    T --> AA[IRQ Processing<br/>Proper Sequencing]
    U --> AA
    AA --> BB[Normal Operation<br/>Error Resolved]

    W --> CC{Diagnostic Mode?}
    CC -->|Yes| DD[Maintenance Functions<br/>MODUS = 2]
    CC -->|No| EE[Production Logging<br/>Error Statistics]

    style P fill:#ff3333
    style Y fill:#ff6666
    style X fill:#ffcc99
    style BB fill:#99ff99
    style DD fill:#ccccff
```

## Summary - Complete HDLC System Architecture

These comprehensive MERMAID diagrams now document:

1. **HDLC Initialization Flow**: Complete HDSIN function with MODUS selection
2. **Half-Duplex Operation**: RTS/CTS flow control and ReadyForSending monitoring
3. **X.21 Circuit-Switched**: Complete state machine from call setup to data transfer
4. **Multi-Mode State Machine**: All operating modes and transitions
5. **Register Interaction Sequence**: Complete hardware communication timeline
6. **Buffer Management**: Flow control, exhaustion handling, and recovery
7. **Error Recovery**: Comprehensive error handling and diagnostic capabilities

The key insights preserved:
- **Application-controlled mode selection** via MODUS parameter
- **Hardware auto-clear behavior** for DMA status bits
- **Dual control systems**: RRTS (hardware status) vs LKEY (software control)
- **X.21 integration** with standard HDLC operations
- **Half-duplex flow control** for high-speed scenarios
- **Multi-layered validation** from hardware through application delivery

All flows are based on **actual SINTRAN source code analysis** without speculation.