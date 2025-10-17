# TAD (Terminal Access Device) Protocol Analysis

## Overview

This document analyzes SINTRAN III's TAD protocol implementation for X.25 network communication. TAD enables terminal access over X.25 networks, allowing remote terminals to connect to SINTRAN systems.

**Source Files:**
- `MP-P2-TAD.NPL` (989 lines) - TAD driver with XMSG interface and interrupt handling
- `RP-P2-TAD.NPL` (1480 lines) - TAD protocol routines with message creation/processing

---

## Message Types

### Core Message Type Constants

Defined in `MP-P2-TAD.NPL:328-338`:

| Symbol | Octal Code | Hex | Message Type | Purpose |
|--------|------------|-----|--------------|---------|
| EBUFF | 7DUMM | 0x7D554D | Empty Buffer/Dummy | Padding or empty message |
| BDESC | 7ESCA | 0x7E5343 | Escape | Escape character received |
| RLOCA | 7RLOC | 0x7D4C4F | Remote Local | Remote/local mode switch (NORD-NET) |
| BDDIS | 7DCON | 0x7D434F | Disconnect | TAD disconnection request |
| CESCR | 7CERS | 0x7D455253 | CESC Response | Escape response |
| RESCF | 7RECO | 0x7D45434F | Reset Confirm | Reset confirmation |
| NWREM | 7NWRE | 0x7D5752 | Nowait Restart | Restart nowait mode |
| ISZRS | 7ISRS\2 | 0x7D495253 | ISIZE Response | Buffer size response (2 bytes data) |
| ERRSP | 7ERRS\2 | 0x7D455252 | Error Response | Error code response (2 bytes data) |
| TREPS | 7TREP\2 | 0x7D545245 | TREP Status | Transmission status (2 bytes data) |

### Additional Message Types

Found in `RP-P2-TAD.NPL`:

| Symbol | Octal Code | Message Type | Purpose | Data Size |
|--------|------------|--------------|---------|-----------|
| 7BDAT | 7BDAT | Data Message | User data transmission | Variable |
| 7TMOD | 7TMOD | Terminal Mode | Terminal mode settings | 1 byte |
| 7TTYP | 7TTYP | Terminal Type | Terminal type code | 2 bytes |
| 7DESC | 7DESC | Define Escape | Set escape character | 1 byte |
| 78MOD | 78MOD | 8-bit Mode | Enable 8-bit UMOD | 2 bytes |
| 7OPSV | 7OPSV | OPSYS Version | OS version and protocol | 3 bytes |
| 7REJE | 7REJE | Reject | Reject invalid message | 1 byte (rejected type) |
| 7RFI | 7RFI | Ready For Input | Request more input data | 0 bytes |
| 7RESE | 7RESE | Reset | Reset connection | 0 bytes |
| 7BMMX | 7BMMX | Break Message | Break strategy | 3 or 23 bytes |
| 7ECKM | 7ECKM | Echo Message | Echo strategy | 1 or 21 bytes |
| 7SYCN | 7SYCN | System Control | System control command | 2 bytes |
| 7USCN | 7USCN | User Control | User control command | 2 bytes |
| 7CPCO | 7CPCO | Completion Code | Operation completion | 4 bytes |
| 7NOWT | 7NOWT | Nowait Status | Nowait operation status | 1 byte |
| 7TNOW | 7TNOW | Terminate Nowait | End nowait mode | 1 byte |
| 7ISRQ | 7ISRQ | ISIZE Request | Query buffer size | 0 bytes |
| 7UMOD | 7UMOD | UMOD Strategy | User mode strategy | 2 bytes |

---

## Message Format Structure

### General Message Format

```
+-------------------+-------------------+-------------------+
| Message Type      | Byte Count        | Data Field        |
| (1 byte)          | (1 byte)          | (0-255 bytes)     |
+-------------------+-------------------+-------------------+
```

**Alignment Rules:**
- Messages can start on **even or odd byte boundaries**
- If message starts on **odd byte**, a **pad byte (0x00)** is inserted first
- Byte count includes only the data field, not the header

### Message Header Creation

**Source:** `RP-P2-TAD.NPL:162-178` (`WRMHEAD`)

```npl
WRMHEAD: A=:D; T:=TDTAFI; X:=TDTALA
         IF TDBTPT BIT "0" THEN                    % ODD START, CREATE PAD BYTE
            A SHZ -1; X+A; *LDATX
            A/\177400; *STATX
            MIN TDBTPT; REMSIZ-1=:REMSIZ
         FI
         X:=TDTALA
         TDBTPT SHZ -1; X+A; D=:A; *STATX          % WRITE IN BUFFER
         TDBTPT+2=:TDBTPT; REMSIZ-2=:REMSIZ
         EXIT
```

### Message Parsing

**Source:** `RP-P2-TAD.NPL:224-242` (`GETMES`)

```npl
GETMES: IF BUFFID=0 THEN A:=1; EXIT FI             % NO BUFFER
TSTSP:  IF REMSIZ<2 THEN T=:A; EXIT FI
        T:=TDTAFI; X:=TDTALA; TDBTPT=:D SHZ -1
        X+A; *LDATX
        MIN TDBTPT
        IF D BIT "0" THEN                          % ODD BYTE
           IF A/\377=0 THEN REMSIZ-1=:REMSIZ; GO TSTSP FI  % PAD BYTE
           A=:CURMES; X+1; *LDATX
           A SHZ -10; MIN TDBTPT; GO RCHK
        FI
        A=:T SHZ -10
        IF A=0 THEN
           REMSIZ-1=:REMSIZ; GO TSTSP              % PAD BYTE
        FI
        A=:CURMES:=T/\377; MIN TDBTPT
RCHK:   IF A>REMSIZ-2 THEN A:=3; EXIT FI           % INCONSISTENT MESSAGE
        A=:T+1-=:REMBYT; X:=REMSIZ-2=:REMSIZ
        A:=CURMES; EXITA                           % MESSAGE FOUND
```

**Returns:**
- **Skip return (EXITA)**: Message found
  - `A-REG` = Message type
  - `T-REG` = Number of bytes in data field
  - `CURMES` = Message type
  - `REMBYT` = Bytes remaining in message
- **Error returns**:
  - `A=1`: No input buffer
  - `A=2`: Buffer empty
  - `A=3`: Inconsistent message (bigger than buffer)

---

## TAD Driver Entry Points

### Main Entry Points

**Source:** `MP-P2-TAD.NPL:363-367`

| Entry Point | Line | Purpose | Called From |
|-------------|------|---------|-------------|
| **INIBDR** | 382 | Initialize TAD connection | TADADM (connection phase) |
| **INISND** | 398 | Send initial buffer | TADADM (after INIBDR) |
| **BDRINP** | 439 | Input driver (receive) | XMSG when buffer queued |
| **TDRINP** | 440 | Input driver entry (internal) | BDRINP, BDROUT |
| **BDROUT** | 800 | Output driver (transmit) | Output monitor calls |
| **BERESP** | 638 | Send escape response | Escape handling |
| **STOTAD** | 660 | Stop TAD connection | User logout/disconnect |
| **DSTOTA** | 661 | Disconnect TAD (internal) | STOTAD, FAERR |
| **BDLOUT** | 695 | Logout TAD user | STOTAD when TLREP disabled |
| **BDDSCN** | 713 | Disconnect without logout | Explicit disconnect request |
| **TDDSCN** | 714 | Disconnect (internal) | BDDSCN, DSTOTA |
| **BDRWT** | 847 | Leave driver level | All driver routines |
| **BMAIRES** | 741 | Reserve mail buffer | Mail system (TWMESS) |
| **MSNDBUF** | 769 | Send mail buffer | Mail timeout routine |
| **MRELBUF** | 789 | Release mail buffer | Mail timeout (partner not responding) |
| **FAERR** | 843 | Fatal error handler | Error conditions |

### RPIT Entry Points

**Source:** `RP-P2-TAD.NPL`

| Entry Point | Line | Purpose | Context |
|-------------|------|---------|---------|
| **PUTPOOL** | 20 | Return buffer to free pool | IOF (interrupt off) |
| **GETPOOL** | 44 | Get buffer from free pool | IOF (interrupt off) |
| **MOVITO** | 63 | Move input buffer to output | Buffer relocation |
| **SNDBUF** | 87 | Send output buffer | Output operations |
| **SNDWT** | 84 | Send and wait for response | Operations requiring response |
| **CBRERSP** | 106 | Send escape response callback | Escape handling |
| **BDTOU** | 121 | Normal output timeout | Timeout timer |
| **MTDTOU** | 135 | Mail output timeout | Mail system timeout |
| **WRMHEAD** | 168 | Write message header | Message creation |
| **CREMES** | 198 | Create message | Output operations |
| **CRHEEV** | 263 | Create header (even byte) | Complete messages |
| **CRHEOD** | 293 | Create header (odd byte) | Complete messages |
| **UPDMBC** | 315 | Update message byte count | Byte operations |
| **GETMBC** | 343 | Get message byte count | Query operations |
| **BYTPUT** | 365 | Put byte in buffer | Output operations |
| **WORDPUT** | 398 | Put word in buffer | Output operations |
| **BYTGET** | 431 | Get byte from buffer | Input operations |
| **LOADBYT** | 452 | Load byte (no buffer update) | Scanning operations |
| **STORBYT** | 469 | Store byte in buffer | Output operations |
| **GETMES** | 224 | Get next message | Input processing |
| **REDOM** | 495 | Redo monitor call | Buffer wait conditions |
| **IEDCHK** | 514 | Check delayed escape | Input validation |
| **CNVERR** | 536 | Convert XMSG error | Error handling |

---

## Data Structure Fields

### TAD Input Datafield

| Field | Purpose | Type |
|-------|---------|------|
| **BUFFID** | Current input buffer ID | INTEGER |
| **TDTADD** | Buffer address (TDTAFI:TDTALA) | TRIPLE |
| **TDTAFI** | Buffer address (file index) | INTEGER |
| **TDTALA** | Buffer address (local address) | INTEGER |
| **TDBTPT** | Byte pointer in buffer | INTEGER |
| **REMSIZ** | Remaining size in buffer | INTEGER |
| **CURMES** | Current message type | INTEGER |
| **REMBYT** | Remaining bytes in message | INTEGER |
| **PORTNO** | XMSG port number | INTEGER |
| **PARTNER** | Partner port number | TRIPLE |
| **RPORT** | Remote port number | INTEGER |
| **DFLAG** | Datafield flags | INTEGER |
| **FLAGB** | Additional flags | INTEGER |
| **TINFO** | Terminal information | INTEGER |
| **CESCP** | Escape character | INTEGER |
| **CTTYP** | Terminal type | INTEGER |
| **FBSIZ** | Frame buffer size | INTEGER |
| **NOBUFF** | Number of buffers | INTEGER |
| **LAST** | Last character processed | INTEGER |
| **DERROR** | Error code | INTEGER |
| **BRECST** | Break/echo strategy | INTEGER |
| **OSVTPN** | OS version + TAD protocol | INTEGER |
| **TDRADDR** | TAD resident datafield address | ADDRESS |
| **ISTATE** | Input state (0=normal, <0=nowait) | INTEGER |

### TAD Output Datafield

All input fields plus:

| Field | Purpose | Type |
|-------|---------|------|
| **POOLLI** | Free buffer pool list | TRIPLE |
| **BUDIS** | Buffer displacement | INTEGER |
| **NOBDIS** | Message start displacement | INTEGER |
| **RSPNUM** | Response number waiting | INTEGER |
| **CURBRST** | Current break strategy | INTEGER |
| **CURECST** | Current echo strategy | INTEGER |
| **SCREEN** | Screen/page control | INTEGER |
| **MBFID** | Mail buffer ID | INTEGER |
| **BRCOUNT** | Buffer rotate count | INTEGER |

### Critical Flag Bits

**DFLAG bits:**
| Bit | Name | Purpose |
|-----|------|---------|
| 5CAPITAL | Capital letters mode | Convert to uppercase |
| 5IESC | Escape disabled | Escape processing disabled |
| 5RQI | RFI sent | Ready-for-input message sent |
| 5WRQI | Wait RFI | Waiting to send RFI |

**FLAGB bits:**
| Bit | Name | Purpose |
|-----|------|---------|
| 5LBLOG | Logout on carrier loss | Auto-logout on disconnect |
| 5LSTA | Line status | Line active/dead |
| 5LOGOUT | Logout pending | User logout requested |
| 5TLREP | Terminal reply | Wait for terminal response |
| 5LCHAR | Local character | Local character mode |
| 5ESCLOFF | Escape delayed | Delayed escape action |
| 5WESC | Wait escape | Escape waiting |
| 5WLOC | Wait local | Local mode waiting |

**TINFO bits:**
| Bit | Name | Purpose |
|-----|------|---------|
| 5CRDLY | CR delay | Carriage return delay |
| 58BIT | 8-bit mode | Allow 8-bit characters |
| 5UMOD | User mode | User mode active |
| 5BFUL | Buffer overrun | Buffer overflow occurred |
| 5PAER | Parity error | Parity error detected |
| 5FRER | Framing error | Framing error detected |

---

## TAD Connection Lifecycle

### 1. Connection Establishment

**Sequence:** `INIBDR` → `INISND` → `BDRINP`

#### Phase 1: Initialize Connection (`INIBDR` - Line 382)

```npl
INIBDR: CALL STADIWINDOW                           % SET WINDOW
        IF PORTNO=0 THEN                           % NO PORT OPENED
          0=:TDRADDR.BXTADD; T:=XFOPN; CALL MXMSG  % OPEN XMSG PORT
          A=:PORTNO                                % SET PORTNO IN DATAFIELD
          A:=0=:D; AD=:OTAD.POOLLI                 % POOL EMPTY
          T:=XFALM; FBSIZ; X:=NOBUFF; CALL MXMSG   % ALLOCATE MESSAGE SPACE
          FOR X:=1 TO NOBUFF DO
             T:=XFGET; A:=FBSIZ; CALL MXMSG        % RESERVE BUFFERS
             % ... add to pool ...
          OD
        FI
        GO BDRWT
```

**Operations:**
1. Open XMSG port (`XFOPN`)
2. Allocate message space (`XFALM`)
3. Reserve `NOBUFF` buffers of size `FBSIZ`
4. Build free buffer pool (`POOLLI`)

#### Phase 2: Send Initial Buffer (`INISND` - Line 398)

```npl
INISND: CALL STADIWINDOW
        OTAD=:B; CALL MGETPOOL                     % GET BUFFER FROM POOL
        T:=ITAD=:B; T:=PORTNO=:D
        T:=XFSCM; CALL MXMSG                       % SET CURRENT BUFFER
        TAD:=DMMES; T=:X:=XFWHD; CALL MXMSG        % WRITE DUMMY MESSAGE
        AD:=OTAD.PARTNER; X:=PORTNO
        T:=XFSND; CALL MXMSG                       % SEND BUFFER
        T:=XFDBK; A:=DPITBANK; CALL MXMSG          % SET BANK FOR R/W
        T:=XFWDF; A:="BDRINP"; CALL MXMSG          % SET WAKE-UP MODE
        FLAGB BZERO 5LSTA=:FLAGB                   % SET LINE OK BIT
        GO TDRINP                                  % ENTER RECEIVE MODE
```

**Operations:**
1. Get buffer from free pool
2. Set current buffer (`XFSCM`)
3. Write dummy message header
4. Send initial buffer to partner (`XFSND`)
5. Configure XMSG wake-up for `BDRINP`
6. Enter receive mode

### 2. Data Transfer

#### Sending Data

**Entry:** `BDPUT` (Line 1217) or `BM8OUT`/`BB8OUT` (Line 1307)

**Flow:**
```
User Data → BDPUT → BYTPUT → CREMES (if needed) → Buffer → SNDBUF → XMSG
```

**Key Steps:**
1. Check if current message is data message (`7BDAT`)
2. If not, create new data message (`CREMES`)
3. Add bytes to buffer (`BYTPUT`)
4. If buffer full or timeout, send buffer (`SNDBUF`)

#### Receiving Data

**Entry:** `BDGET` (Line 1257) or `BB4INW`/`BB8INP` (Line 1354, 1419)

**Flow:**
```
XMSG → BDRINP → GETMES → BYTGET → User Data
```

**Key Steps:**
1. Driver receives buffer from XMSG
2. Parse messages with `GETMES`
3. If data message (`7BDAT`), extract bytes
4. If buffer empty, send RFI (`SNDRFI`)

### 3. Disconnection

#### Graceful Disconnect (`BDDSCN` - Line 713)

```npl
BDDSCN: CALL STADIWINDOW
TDDSCN: FLAGB BONE 5LSTA=:FLAGB                    % SET LINE DEAD BIT
        IF PORTNO><0 AND TDRADDR.BXTADD><0 THEN
          IF TMPBUF><0 THEN
             0=:TMPBUF; T:=XFREL; CALL MXMSG       % RELEASE CURRENT BUFFER
          FI
          0=:PORTNO=:BUFFID=:OTAD.BUFFID           % CLEAR DATAFIELD
          TDRADDR=:B; BXTADD=:L; 0=:BXTADD
          T:=XFDCT; *MON 2XMSG                     % DISCONNECT PORT
        FI
```

**Operations:**
1. Set line dead flag (`5LSTA`)
2. Release current buffer if any
3. Clear all buffer IDs
4. Disconnect XMSG port (`XFDCT`)

#### Forced Disconnect with Logout (`STOTAD` - Line 660)

```npl
STOTAD: CALL STADIWINDOW
        IF DBPROG><0 THEN
          IF DBPROG=TDRADDR.RTRES AND FLAGB NBIT 5LOGOUT THEN
             IF FLAGB BIT 5TLREP THEN
                A BONE 5LOGOUT=:FLAGB              % MARK LOGOUT
                "IORESTART"; CALL CXRTACT          % RESTART PROGRAM
             ELSE
                CALL BDLOUT                        % LOGOUT USER
             FI
          FI
        FI
        GO TDDSCN                                  % DISCONNECT
```

---

## Example Message Sequences

### Connection Establishment

```
Local TAD                    Remote TAD
    |                            |
    |------ XFOPN (open) ------->|
    |<----- Port Allocated ------|
    |                            |
    |------ XFALM (alloc) ------>|
    |<----- Space Reserved ------|
    |                            |
    |------ DUMMY Message ------>|
    |                            |
    |<----- 7TMOD (mode) --------|
    |<----- 7TTYP (type) --------|
    |<----- 7DESC (escape) ------|
    |                            |
    |------ Ready ------------->|
```

### Data Transfer

```
Terminal                     TAD                      Remote TAD
    |                         |                            |
    |--- User types "ABC" --->|                            |
    |                         |                            |
    |                         |--- 7BDAT [ABC] ----------->|
    |                         |                            |
    |                         |<-- 7RFI (ready) -----------|
    |                         |                            |
    |                         |<-- 7BDAT [response] -------|
    |                         |                            |
    |<-- Display response ----|                            |
```

### Break Handling

```
Terminal                     TAD                      Remote TAD
    |                         |                            |
    |--- Break key pressed -->|                            |
    |                         |                            |
    |                         |--- 7BMMX [strategy] ------>|
    |                         |                            |
    |                         |<-- 7ECKM [echo] -----------|
    |                         |                            |
    |<-- Echo response -------|                            |
```

### Disconnection

```
Terminal                     TAD                      Remote TAD
    |                         |                            |
    |--- Logout command ----->|                            |
    |                         |                            |
    |                         |--- 7DCON (disconnect) ---->|
    |                         |                            |
    |                         |--- XFREL (release) ------->|
    |                         |--- XFDCT (disconnect) ---->|
    |                         |                            |
    |<-- Connection closed ---|                            |
```

### Error Recovery

```
Local TAD                    Remote TAD
    |                            |
    |------ Invalid Message ---->|
    |                            |
    |<----- 7REJE (reject) ------|
    |                            |
    |------ 7RFI (request) ----->|
    |                            |
    |<----- 7BDAT (data) --------|
```

---

## Continue in Part 2...

*This document continues in `TAD-Protocol-Analysis-Part2.md` with:*
- Detailed message format for each message type
- XMSG integration details
- State machine diagrams
- C# emulator implementation guide
