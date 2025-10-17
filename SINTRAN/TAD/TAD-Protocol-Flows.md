# TAD Protocol Flow Diagrams

## Overview

This document provides detailed protocol flow diagrams for SINTRAN III TAD (Terminal Access Device) X.25 communication sequences.

**Parent Documents:**
- `TAD-Protocol-Analysis.md` - Overall protocol analysis
- `TAD-Message-Formats.md` - Message format specifications

---

## Connection Establishment Flow

### Full Connection Sequence

```mermaid
sequenceDiagram
    participant User as User Application
    participant TAD as Local TAD
    participant XMSG as Local XMSG
    participant Net as X.25 Network
    participant RXMSG as Remote XMSG
    participant RTAD as Remote TAD

    Note over TAD: INIBDR Called
    User->>TAD: Connect Request
    TAD->>XMSG: XFOPN (Open Port)
    XMSG-->>TAD: Port Number Assigned
    TAD->>XMSG: XFALM (Allocate Space)
    XMSG-->>TAD: Space Reserved

    loop For each buffer
        TAD->>XMSG: XFGET (Get Buffer)
        XMSG-->>TAD: Buffer ID
        TAD->>TAD: Add to POOLLI
    end

    Note over TAD: INISND Called
    TAD->>TAD: MGETPOOL (Get Buffer)
    TAD->>XMSG: XFSCM (Set Current)
    TAD->>XMSG: XFWHD (Write Dummy Header)
    TAD->>XMSG: XFSND (Send to Partner)
    XMSG->>Net: X.25 Call Request
    Net->>RXMSG: X.25 Call Accept
    RXMSG->>RTAD: Buffer Received

    Note over RTAD: Connection Accepted
    RTAD->>RXMSG: 7TMOD (Terminal Mode)
    RXMSG->>Net: X.25 Data Packet
    Net->>XMSG: X.25 Data Packet
    XMSG->>TAD: BDRINP (Wake Driver)
    TAD->>TAD: GETMES (Parse Message)
    TAD->>User: Mode Settings Applied

    RTAD->>RXMSG: 7TTYP (Terminal Type)
    RXMSG->>Net: X.25 Data Packet
    Net->>XMSG: X.25 Data Packet
    XMSG->>TAD: BDRINP (Wake Driver)
    TAD->>User: Type Settings Applied

    RTAD->>RXMSG: 7DESC (Escape Char)
    RXMSG->>Net: X.25 Data Packet
    Net->>XMSG: X.25 Data Packet
    XMSG->>TAD: BDRINP (Wake Driver)
    TAD->>User: Escape Char Set

    Note over TAD,RTAD: Connection Established
    
    box Blue User Space
    participant User
    end
    box Teal Local TAD
    participant TAD
    participant XMSG
    end
    box Green Network
    participant Net
    end
    box Magenta Remote TAD
    participant RXMSG
    participant RTAD
    end
```

### Connection State Machine

```mermaid
stateDiagram-v2
    [*] --> Disconnected

    Disconnected --> Opening : INIBDR Called
    Opening --> Allocating : XFOPN Success
    Allocating --> Buffering : XFALM Success
    Buffering --> Buffering : XFGET (loop)
    Buffering --> Sending : All Buffers Allocated
    Sending --> Waiting : XFSND Dummy
    Waiting --> Configuring : Partner Responds
    Configuring --> Configuring : Receive 7TMOD/7TTYP/7DESC
    Configuring --> Connected : Configuration Complete

    Connected --> Disconnecting : 7DCON or STOTAD
    Disconnecting --> Releasing : Release Buffers
    Releasing --> Disconnected : XFDCT Complete

    Connected --> Error : Fatal Error (FAERR)
    Error --> Disconnected : Cleanup
    
    classDef initState fill:#9E9E9E,stroke:#616161,stroke-width:2px,color:#fff
    classDef activeState fill:#4CAF50,stroke:#388E3C,stroke-width:2px,color:#fff
    classDef transientState fill:#2196F3,stroke:#1976D2,stroke-width:2px,color:#fff
    classDef errorState fill:#F44336,stroke:#D32F2F,stroke-width:2px,color:#fff
    
    class Disconnected initState
    class Connected activeState
    class Opening,Allocating,Buffering,Sending,Waiting,Configuring,Disconnecting,Releasing transientState
    class Error errorState
```

---

## Data Transfer Flows

### Output Data Flow (User → Remote)

```mermaid
sequenceDiagram
    participant User as User Program
    participant IOTRANS as BDPUT (IOTRANS)
    participant Buffer as Output Buffer
    participant Driver as BDROUT (Driver)
    participant XMSG as XMSG Layer
    participant Remote as Remote TAD

    User->>IOTRANS: Write Byte
    IOTRANS->>IOTRANS: Check CURMES=7BDAT?

    alt No Data Message Active
        IOTRANS->>Buffer: CREMES (Create 7BDAT)
        Buffer-->>IOTRANS: Message Created
    end

    IOTRANS->>Buffer: BYTPUT (Add Byte)
    Buffer-->>IOTRANS: Byte Added

    alt Buffer Full or Timeout
        IOTRANS->>Driver: SNDBUF
        Driver->>XMSG: XFSCM (Set Current)
        Driver->>XMSG: XFSND (Send Buffer)
        XMSG->>Remote: X.25 Data Packet
        Driver->>Buffer: MGETPOOL (New Buffer)
    end

    IOTRANS-->>User: Character Accepted
    
    box Blue User Space
    participant User
    end
    box Teal TAD Driver
    participant IOTRANS
    participant Buffer
    participant Driver
    end
    box Green Network
    participant XMSG
    participant Remote
    end
```

### Input Data Flow (Remote → User)

```mermaid
sequenceDiagram
    participant Remote as Remote TAD
    participant XMSG as XMSG Layer
    participant Driver as BDRINP (Driver)
    participant Buffer as Input Buffer
    participant IOTRANS as BDGET (IOTRANS)
    participant User as User Program

    Remote->>XMSG: X.25 Data Packet
    XMSG->>XMSG: Queue Buffer on Port
    XMSG->>Driver: Wake BDRINP

    Note over Driver: TDRINP Activated
    Driver->>XMSG: XFPST (Port Status)
    XMSG-->>Driver: Buffer Available
    Driver->>XMSG: XFRCV (Receive)
    XMSG-->>Driver: Buffer ID
    Driver->>Driver: GETMES (Parse)
    Driver-->>Driver: 7BDAT Found

    Note over Driver: Data Ready
    User->>IOTRANS: Read Byte
    IOTRANS->>Buffer: BYTGET
    Buffer-->>IOTRANS: Byte Data

    alt Message Empty
        IOTRANS->>Driver: GETMES (Next)
        alt No More Messages
            IOTRANS->>Driver: SNDRFI (Request Input)
            Driver->>XMSG: XFSND (7RFI)
            XMSG->>Remote: X.25 Data Packet
        end
    end

    IOTRANS-->>User: Character Returned
    
    box Green Network
    participant Remote
    participant XMSG
    end
    box Teal TAD Driver
    participant Driver
    participant Buffer
    participant IOTRANS
    end
    box Blue User Space
    participant User
    end
```

### 8-Byte Transfer (BB8OUT/BB8INP)

```mermaid
sequenceDiagram
    participant User as User Program
    participant MON as Monitor Call
    participant TAD as TAD Handler
    participant Buffer as Buffer

    Note over User,Buffer: Output (BB8OUT)
    User->>MON: BB8OUT (A,D,L,X regs)
    MON->>TAD: BM8OUT Entry

    alt No Data Message
        TAD->>Buffer: CREMES (7BDAT, 10 bytes)
    else Buffer Full
        TAD->>TAD: SNDBUF (Send Current)
        TAD->>Buffer: CREMES (7BDAT, 10 bytes)
    end

    loop 4 Words
        TAD->>TAD: BDTCH (Check Termination)
        alt Not Terminated
            TAD->>Buffer: WORDPUT (2 bytes)
        else Terminated (0x00 byte)
            TAD->>TAD: Stop Writing
            TAD-->>User: Return (Success)
        end
    end

    TAD-->>User: Return (8 Bytes Written)

    Note over User,Buffer: Input (BB8INP)
    User->>MON: BB8INP
    MON->>TAD: BB8INP Entry

    loop Until 8 Bytes or Break
        alt CURMES != 7BDAT
            TAD->>Buffer: GETMES
            alt Not Data Message
                TAD->>TAD: SNDREJ (Reject)
            end
        end

        TAD->>Buffer: BYTGET
        Buffer-->>TAD: Byte Data
        TAD->>TAD: Store in Register

        alt REMBYT = -1
            TAD->>TAD: Set Break Flag (bit 17)
            TAD-->>User: Return Early (Break)
        end
    end

    TAD-->>User: Return (Count in T-REG)
```

---

## Buffer Management Flows

### Buffer Pool Management

```mermaid
flowchart TD
    Start([Start]) --> Init[INIBDR: Initialize]
    Init --> Alloc[XFALM: Allocate Space]
    Alloc --> Loop{For Each<br/>Buffer}

    Loop -->|Next| GetBuf[XFGET: Get Buffer]
    GetBuf --> AddPool[MPUTPOOL: Add to POOLLI]
    AddPool --> Loop

    Loop -->|Done| Ready[Pool Ready]
    Ready --> Use{Buffer Needed?}

    Use -->|Output| GetOut[MGETPOOL: Get from Pool]
    GetOut -->|Pool Empty| Wait1[Wait for Return]
    GetOut -->|Success| UseOut[Use for Output]
    UseOut --> SendOut[SNDBUF: Send]
    SendOut --> RetOut[MPUTPOOL: Return to Pool]
    RetOut --> Use

    Use -->|Input| GetIn[XFRCV: Receive from XMSG]
    GetIn --> UseIn[Process Input]
    UseIn --> RetIn[PUTPOOL: Return to Pool]
    RetIn --> Use

    Wait1 --> GetOut
```

### Buffer State Transitions

```mermaid
stateDiagram-v2
    [*] --> Free : XFGET

    Free --> OutputActive : MGETPOOL
    Free --> InputActive : XFRCV
    Free --> TempActive : High-Priority
    Free --> MailActive : BMAIRES

    OutputActive --> Sending : CREMES/BYTPUT
    Sending --> Sent : SNDBUF
    Sent --> Free : MPUTPOOL

    InputActive --> Processing : GETMES
    Processing --> Processed : BYTGET
    Processed --> Free : PUTPOOL

    TempActive --> Processed : High-Pri Processing

    MailActive --> MailSending : MSNDBUF
    MailSending --> Free : Mail Sent
    MailActive --> Free : MRELBUF (Timeout)

    OutputActive --> Relocated : MOVITO
    Relocated --> InputActive : Buffer Moved
```

---

## Escape and Break Handling

### Escape Sequence

```mermaid
sequenceDiagram
    participant User as Terminal
    participant Local as Local TAD
    participant Remote as Remote TAD
    participant Handler as Escape Handler

    User->>Remote: Escape Key Pressed
    Remote->>Remote: Detect Escape Character
    Remote->>Local: 7ESCA (Escape Message)

    Note over Local: High-Priority Processing
    Local->>Local: Check DFLAG 5IESC

    alt Escape Enabled
        Local->>Handler: CALL ESCAPE
        Handler->>Handler: Process Escape Action
        Handler-->>Local: Escape Complete
        Local->>Remote: 7CERS (Escape Response)
        Remote->>User: Escape Processed
    else Escape Disabled
        Local->>Remote: 7CERS (Escape Disabled)
        Remote->>User: Escape Ignored
    end
```

### Break Message Flow

```mermaid
sequenceDiagram
    participant App as Application
    participant Local as Local TAD
    participant Remote as Remote TAD

    App->>Local: Set Break Strategy
    Local->>Local: BDBREA (Create 7BMMX)

    alt Simple Strategy (1-6)
        Local->>Remote: 7BMMX [Strategy, MaxBreak]
    else Custom Table (7)
        Local->>Remote: 7BMMX [7, MaxBreak, Table[20]]
    end

    Remote->>Remote: Apply Break Strategy
    Remote->>Remote: BDTMOD (Update Datafield)
    Remote-->>Local: Acknowledge (Implicit)

    Note over Remote: Detect Break Character
    Remote->>Local: 7BDAT [...data...<break>]
    Local->>Local: REMBYT = -1 (Break Flag)
    Local->>App: Return with Break Indication
```

### Echo Strategy Flow

```mermaid
sequenceDiagram
    participant App as Application
    participant Local as Local TAD
    participant Remote as Remote TAD

    App->>Local: Set Echo Strategy
    Local->>Local: BDECHO (Create 7ECKM)

    alt Simple Strategy (1-6)
        Local->>Remote: 7ECKM [Strategy]
    else Custom Table (7)
        Local->>Remote: 7ECKM [7, Table[20]]
    end

    Remote->>Remote: Apply Echo Strategy
    Remote->>Remote: Update CURECST

    Note over Remote: User Types Character
    alt Echo Enabled for Char
        Remote->>Remote: Echo Character
        Remote->>Local: 7BDAT [char]
        Local->>App: Display Character
    else Echo Disabled for Char
        Remote->>Remote: Silent (No Echo)
    end
```

---

## Error Handling Flows

### Message Rejection Flow

```mermaid
flowchart TD
    Start([Receive Message]) --> Parse[GETMES: Parse Header]
    Parse --> Check{Valid<br/>Message?}

    Check -->|Yes| Type{Message<br/>Type?}
    Check -->|No| Reject[Generate 7REJE]

    Type -->|7BDAT| Process[Process Data]
    Type -->|Control| ProcCtrl[Process Control]
    Type -->|Unknown| Reject

    Reject --> CheckData{Was it<br/>7BDAT?}
    CheckData -->|Yes| AddRFI[Also Generate 7RFI]
    CheckData -->|No| SendRej[Send 7REJE Only]

    AddRFI --> SendBoth[Send 7REJE + 7RFI]
    SendBoth --> RetBuf[Return Buffer to Pool]
    SendRej --> RetBuf

    RetBuf --> End([Continue])
    Process --> End
    ProcCtrl --> End
```

### Ready-For-Input (RFI) Flow

```mermaid
sequenceDiagram
    participant User as User Program
    participant TAD as Local TAD
    participant XMSG as XMSG Layer
    participant Remote as Remote TAD

    User->>TAD: Read Data (BDGET)
    TAD->>TAD: BYTGET (Check Buffer)

    alt Buffer Empty
        TAD->>TAD: SNDRFI Called

        alt Output Buffer Available
            TAD->>TAD: CREMES (7RFI, 0 bytes)
            TAD->>XMSG: SNDBUF
            XMSG->>Remote: 7RFI Message
            TAD->>TAD: Set DFLAG 5RQI
        else No Output Buffer
            alt Input Buffer Available
                TAD->>TAD: MOVITO (Move to Output)
                TAD->>TAD: CREMES (7RFI)
                TAD->>XMSG: SNDBUF
                TAD->>TAD: Set DFLAG 5RQI
            else No Buffers
                TAD->>TAD: Set DFLAG 5WRQI
                Note over TAD: Driver sends RFI<br/>when buffer available
            end
        end

        Remote->>Remote: Prepare Data
        Remote->>XMSG: 7BDAT (Response)
        XMSG->>TAD: BDRINP (Wake)
        TAD->>TAD: Clear DFLAG 5RQI
        TAD->>User: Data Available
    end
```

### Fatal Error Recovery

```mermaid
flowchart TD
    Start([Error Detected]) --> Fatal[FAERR Called]
    Fatal --> SetDead[Set FLAGB 5LSTA]

    SetDead --> CheckProg{DBPROG<br/>Active?}
    CheckProg -->|No| Disconnect

    CheckProg -->|Yes| CheckRes{Reserving<br/>Program?}
    CheckRes -->|No| Disconnect

    CheckRes -->|Yes| CheckRep{FLAGB<br/>5TLREP?}

    CheckRep -->|Yes| MarkLogout[Set FLAGB 5LOGOUT]
    MarkLogout --> Restart[Restart Program]
    Restart --> Disconnect

    CheckRep -->|No| Logout[BDLOUT: Logout User]
    Logout --> Disconnect

    Disconnect[TDDSCN: Disconnect] --> RelBuf[Release TMPBUF]
    RelBuf --> ClrFields[Clear Buffer IDs]
    ClrFields --> ClrXTBLOC[Clear BXTADD]
    ClrXTBLOC --> XFDCT[XFDCT: Disconnect Port]
    XFDCT --> End([Error Handled])
```

---

## Disconnection Flows

### Graceful Disconnection

```mermaid
sequenceDiagram
    participant User as User Application
    participant Local as Local TAD
    participant XMSG as Local XMSG
    participant Net as X.25 Network
    participant Remote as Remote TAD

    User->>Local: Logout / Disconnect Request
    Local->>Local: BDDSCN Called

    Local->>Local: Set FLAGB 5LSTA (Line Dead)

    alt TMPBUF Active
        Local->>XMSG: XFREL (Release TMPBUF)
    end

    Local->>Local: Clear BUFFID, PORTNO
    Local->>Local: Clear OTAD.BUFFID

    Local->>XMSG: XFDCT (Disconnect Port)
    XMSG->>Net: X.25 Clear Request
    Net->>Remote: X.25 Clear Indication

    Remote->>Remote: DSTOTA (Disconnect)

    Note over Local,Remote: Connection Closed
```

### Remote-Initiated Disconnection

```mermaid
sequenceDiagram
    participant Remote as Remote TAD
    participant RXMSG as Remote XMSG
    participant Net as X.25 Network
    participant XMSG as Local XMSG
    participant Local as Local TAD
    participant User as User Application

    Remote->>RXMSG: Disconnect Initiated
    Remote->>RXMSG: 7DCON Message
    RXMSG->>Net: X.25 Data + Clear
    Net->>XMSG: X.25 Data Packet

    XMSG->>Local: BDRINP (Wake)
    Local->>Local: GETMES (Parse)
    Local->>Local: Detect 7DCON

    Note over Local: High-Priority Message
    Local->>Local: DSTOTA Called

    alt User Program Active
        alt FLAGB 5TLREP Set
            Local->>Local: Set FLAGB 5LOGOUT
            Local->>User: Restart Program
        else No TLREP
            Local->>Local: BDLOUT (Logout)
        end
    end

    Local->>Local: TDDSCN (Disconnect)
    Local->>XMSG: Release Buffers
    Local->>XMSG: XFDCT

    Note over Local,Remote: Connection Closed
```

### Forced Disconnection with Cleanup

```mermaid
flowchart TD
    Start([STOTAD Called]) --> SetWin[STADIWINDOW]
    SetWin --> CheckProg{DBPROG<br/>Active?}

    CheckProg -->|No| Disconnect
    CheckProg -->|Yes| CheckMatch{DBPROG =<br/>RTRES?}

    CheckMatch -->|No| Disconnect
    CheckMatch -->|Yes| CheckLogout{FLAGB<br/>5LOGOUT?}

    CheckLogout -->|Set| Disconnect
    CheckLogout -->|Clear| CheckRep{FLAGB<br/>5TLREP?}

    CheckRep -->|Yes| Mark[Set FLAGB 5LOGOUT]
    Mark --> RestartProg[IORESTART: Restart Program]
    RestartProg --> Disconnect

    CheckRep -->|No| Logout[BDLOUT: Logout User]
    Logout --> CheckIO{IO Active?}

    CheckIO -->|Yes| ResetWait[Clear STATUS 5WAIT]
    ResetWait --> DisableEsc[Clear DFLAG 5IESC]
    DisableEsc --> ResetFlags[Clear FLAGB Escape Flags]
    ResetFlags --> CallEsc[CALL ESCAPE with LAST=-1]
    CallEsc --> Disconnect

    CheckIO -->|No| DisableEsc

    Disconnect[TDDSCN: Disconnect] --> SetDead[Set FLAGB 5LSTA]
    SetDead --> CheckPort{PORTNO<br/>Valid?}

    CheckPort -->|No| ClrFields
    CheckPort -->|Yes| RelTmp{TMPBUF<br/>Active?}

    RelTmp -->|Yes| RelBuf[XFREL: Release Buffer]
    RelTmp -->|No| ClrFields

    RelBuf --> ClrFields[Clear All Buffer IDs]
    ClrFields --> SaveXT[Save BXTADD Address]
    SaveXT --> ClrXT[Clear BXTADD]
    ClrXT --> XFDCT[XFDCT: Disconnect Port]
    XFDCT --> Leave[BDRWT: Leave Driver Level]
    Leave --> End([Disconnection Complete])
```

---

## Nowait Mode Flows

### Nowait Mode Operation

```mermaid
stateDiagram-v2
    [*] --> Normal : ISTATE = 0

    Normal --> Nowait : Start Nowait Operation
    Nowait --> Processing : ISTATE < 0

    Processing --> WaitBuffer : Buffer Needed
    WaitBuffer --> Processing : Buffer Available

    Processing --> SendStatus : Operation Complete
    SendStatus --> Normal : 7NOWT Sent (Success)
    SendStatus --> Normal : 7TNOW Sent (Error)

    Processing --> Restart : 7NWRE Received
    Restart --> Processing : Restart User

    Processing --> Error : Fatal Error
    Error --> SendStatus
```

### Nowait Status Reporting

```mermaid
sequenceDiagram
    participant User as User Program
    participant TAD as TAD Handler
    participant XMSG as XMSG Layer
    participant Remote as Remote TAD

    User->>TAD: Start Nowait Operation
    TAD->>TAD: Set ISTATE < 0

    Note over TAD: Nowait Processing
    TAD->>TAD: Process Data

    alt Operation Success
        TAD->>TAD: NOWTSTA (Status = 0)
        TAD->>TAD: Create 7NOWT Message
        TAD->>XMSG: SNDBUF
        XMSG->>Remote: 7NOWT [00]
        TAD->>TAD: Set ISTATE = 0
        TAD->>User: Return (Success)
    else Operation Error
        TAD->>TAD: NOWTSTA (Status != 0)
        TAD->>TAD: Create 7TNOW Message
        TAD->>XMSG: SNDBUF
        XMSG->>Remote: 7TNOW [Error Code]
        TAD->>TAD: Set ISTATE = 0
        TAD->>User: Return (Error)
    end
```

---

## ISIZE Query Flow

### ISIZE Request-Response

```mermaid
sequenceDiagram
    participant User as User Program
    participant Local as Local TAD
    participant XMSG as XMSG Layer
    participant Remote as Remote TAD

    User->>Local: ISIZE Monitor Call
    Local->>Local: BISIZ/PISIZ Entry

    alt Input Buffer Has Data
        Local->>Local: Check CURMES = 7BDAT
        alt Data Message Present
            Local->>Local: Count REMBYT
            Local->>User: Return Byte Count
        else No Data Message
            Local->>Local: GETMES (Find Next)
            Local->>User: Return Byte Count
        end
    else Buffer Empty
        Local->>Local: Clear Input Buffer
        Local->>Local: CREMES (7ISRQ, 0)
        Local->>XMSG: SNDWT (Wait for Response)
        XMSG->>Remote: 7ISRQ Message

        Remote->>Remote: Count Available Data
        Remote->>XMSG: 7ISRS [Size]
        XMSG->>Local: Response Received

        Local->>Local: Extract Size from 7ISRS
        Local->>User: Return Size (bit 15 = break)
    end
```

---

## Complete TAD Session Example

### Terminal Session Flow

```mermaid
sequenceDiagram
    participant Term as Terminal
    participant TAD as Local TAD
    participant Net as Network
    participant RTAD as Remote TAD
    participant Host as Remote Host

    Note over Term,Host: 1. Connection Phase
    Term->>TAD: Connect to Host
    TAD->>Net: INIBDR → XFOPN
    Net->>RTAD: X.25 Call
    RTAD->>TAD: 7TMOD, 7TTYP, 7DESC
    TAD->>Term: Connection Established

    Note over Term,Host: 2. Login Phase
    Term->>TAD: Type Username
    TAD->>Net: 7BDAT [username]
    Net->>RTAD: Data Packet
    RTAD->>Host: Process Login
    Host->>RTAD: Prompt for Password
    RTAD->>Net: 7BDAT [prompt]
    Net->>TAD: Data Packet
    TAD->>Term: Display Prompt

    Term->>TAD: Type Password
    TAD->>Net: 7BDAT [password]
    Net->>RTAD: Data Packet
    RTAD->>Host: Authenticate
    Host->>RTAD: Login Success
    RTAD->>Net: 7BDAT [welcome]
    Net->>TAD: Data Packet
    TAD->>Term: Display Welcome

    Note over Term,Host: 3. Command Session
    Term->>TAD: Type Command
    TAD->>Net: 7BDAT [command]
    Net->>RTAD: Data Packet
    RTAD->>Host: Execute Command
    Host->>RTAD: Command Output
    RTAD->>Net: 7BDAT [output]
    Net->>TAD: Data Packet
    TAD->>Term: Display Output

    Note over Term,Host: 4. Escape Handling
    Term->>TAD: Press Escape Key
    TAD->>Net: 7ESCA
    Net->>RTAD: Escape Signal
    RTAD->>Host: Interrupt Process
    RTAD->>Net: 7CERS
    Net->>TAD: Escape Response
    TAD->>Term: Escape Processed

    Note over Term,Host: 5. Logout Phase
    Term->>TAD: Type LOGOUT
    TAD->>Net: 7BDAT [logout]
    Net->>RTAD: Data Packet
    RTAD->>Host: Logout User
    Host->>RTAD: Logout Complete
    RTAD->>Net: 7DCON
    Net->>TAD: Disconnect Signal
    TAD->>Term: Connection Closed
```

---

## Priority Message Handling

### Message Priority Processing

```mermaid
flowchart TD
    Start([Message Arrival]) --> CheckPri{Priority<br/>Level?}

    CheckPri -->|High| ImmRcv[XFRCV Immediate]
    CheckPri -->|Normal| CheckBuf{Input Buffer<br/>Empty?}

    CheckBuf -->|Yes| NormRcv[XFRCV Normal]
    CheckBuf -->|No| Wait[Wait for Buffer Empty]
    Wait --> CheckBuf

    ImmRcv --> Store[Store in TMPBUF]
    Store --> Parse[XFRHD: Read Header]
    Parse --> Process{Message<br/>Type?}

    Process -->|7ESCA/7RLOC| Escape[Process Escape]
    Process -->|7DCON| Disconnect[DSTOTA]
    Process -->|7CERS| EscResp[Escape Response]
    Process -->|7RECO| ResetConf[Reset Confirm]
    Process -->|7NWRE| NowaitRestart[Restart Nowait]
    Process -->|7ISRS| SizeResp[ISIZE Response]
    Process -->|7ERRS| ErrorResp[Error Response]
    Process -->|7TREP| TrepStatus[TREP Status]

    Escape --> MoveOut{Output Buffer<br/>Empty?}
    MoveOut -->|Yes| MoveBuf[Move TMPBUF to Output]
    MoveOut -->|No| AddPool[Add to Pool]
    MoveBuf --> Continue
    AddPool --> Continue

    EscResp --> CheckWait{Waiting for<br/>Response?}
    CheckWait -->|Yes| Restart1[Restart User]
    CheckWait -->|No| Continue

    SizeResp --> Extract[Extract Size Data]
    Extract --> CheckWait

    ErrorResp --> Extract

    Disconnect --> Continue
    ResetConf --> CheckWait
    NowaitRestart --> RestartUser[Restart User]
    TrepStatus --> UpdateInfo[Update TINFO Flags]

    Restart1 --> Continue
    RestartUser --> Continue
    UpdateInfo --> Continue
    Continue([Continue Processing])

    NormRcv --> NormStore[Store in Input Buffer]
    NormStore --> NormParse[GETMES: Parse Messages]
    NormParse --> NormProc{Message<br/>Type?}

    NormProc -->|7BDAT| ProcData[Process Data]
    NormProc -->|7TMOD| ProcMode[Process Mode]
    NormProc -->|7TTYP| ProcType[Process Type]
    NormProc -->|7DESC| ProcEsc[Process Escape Def]
    NormProc -->|78MOD| Proc8Bit[Process 8-bit]
    NormProc -->|7OPSV| ProcVer[Process Version]
    NormProc -->|7DUMM| ProcDummy[Skip Dummy]
    NormProc -->|Other| Reject[SNDREJ: Reject]

    ProcData --> UserRead[User Reads Data]
    ProcMode --> ApplyMode[Apply Mode Settings]
    ProcType --> ApplyType[Apply Type Settings]
    ProcEsc --> ApplyEsc[Set Escape Char]
    Proc8Bit --> Apply8Bit[Enable 8-bit]
    ProcVer --> ApplyVer[Store Version]
    ProcDummy --> NormParse

    Reject --> SendRej[Send 7REJE]
    SendRej --> NormParse

    UserRead --> Done([Done])
    ApplyMode --> Done
    ApplyType --> Done
    ApplyEsc --> Done
    Apply8Bit --> Done
    ApplyVer --> Done
```

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-Protocol-Flows.md`

**Related Documents:**
- `TAD-Protocol-Analysis.md` - Overall protocol analysis
- `TAD-Message-Formats.md` - Message format specifications
- `TAD-Emulator-Implementation-Guide.md` - C# implementation guide (to be created)
