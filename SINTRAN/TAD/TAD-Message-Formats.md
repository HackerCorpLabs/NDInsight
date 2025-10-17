# TAD Message Format Specifications

## Overview

This document provides detailed format specifications for all TAD protocol messages used in SINTRAN III X.25 communication.

**Parent Document:** `TAD-Protocol-Analysis.md`

---

## Message Header Format

All TAD messages follow this basic structure:

```
┌─────────────────────────────────────────────────────────────┐
│ Optional Pad  │  Message Type  │  Byte Count  │  Data Field │
│   (0-1 byte)  │    (1 byte)    │  (1 byte)    │  (0-255 bytes)│
└─────────────────────────────────────────────────────────────┘
```

- **Pad byte**: `0x00` inserted if message starts on odd byte boundary
- **Message Type**: 7-bit ASCII code (e.g., `7BDAT` = "BDAT")
- **Byte Count**: Number of bytes in data field (not including header)
- **Data Field**: Variable-length payload

---

## Data Messages

### 7BDAT - Data Message

**Purpose:** Transmit user data (terminal input/output)

**Format:**
```
┌────────┬─────────┬──────────────────────────┐
│ 7BDAT  │  Count  │  User Data (0-255 bytes) │
│ (1)    │  (1)    │  (Count bytes)           │
└────────┴─────────┴──────────────────────────┘
```

**Code:** `0x42444154` ("BDAT")

**Data Field:**
- Raw user data bytes
- No special encoding
- Can contain any byte values (0x00-0xFF)
- 7-bit mode: bit 7 cleared unless 8-bit mode enabled

**Source:** Used throughout input/output operations

**Example:**
```
Message: "Hello"
┌───────┬────┬──┬──┬──┬──┬──┐
│ 7BDAT │ 05 │H │e │l │l │o │
└───────┴────┴──┴──┴──┴──┴──┘
```

**Break Handling:**
- Last character in message may be break character
- `REMBYT=-1` indicates break on last byte
- Break triggers special processing in receiver

---

## Control Messages

### 7TMOD - Terminal Mode

**Purpose:** Set terminal operating mode flags

**Format:**
```
┌────────┬─────────┬──────────┐
│ 7TMOD  │   01    │  Flags   │
│ (1)    │  (1)    │  (1)     │
└────────┴─────────┴──────────┘
```

**Code:** `0x544D4F44` ("TMOD")

**Flags Byte:**
| Bit | Name | Purpose |
|-----|------|---------|
| 0 | Capital letters | Convert input to uppercase |
| 1 | CR delay | Insert delay after carriage return |
| 2 | Stop on full page | Pause output when page full |
| 3 | Logout on carrier loss | Auto-logout when connection lost |

**Source:** `RP-P2-TAD.NPL:805-818` (`BTMOD`), `MP-P2-TAD.NPL:155-180` (`BDTMOD`)

**Processing (receive):** `MP-P2-TAD.NPL:661-669`
```npl
DFLAG BZERO 5CAPITAL
IF D BIT "0" THEN T BONE 5CAPITAL FI; T=:DFLAG
T:=TINFO BZERO 5CRDLY=:TINFO
IF D BIT 1 THEN T BONE 5CRDLY=:TINFO FI
0=:OTAD.SCREEN
IF D BIT 2 THEN MIN X.SCREEN FI
T:=FLAGB BZERO 5LBLOG
IF D BIT 3 THEN T BONE 5LBLOG FI; T=:FLAGB
```

**Example:**
```
Set capital letters + CR delay:
┌───────┬────┬────┐
│ 7TMOD │ 01 │ 03 │  (bits 0,1 set)
└───────┴────┴────┘
```

### 7TTYP - Terminal Type

**Purpose:** Set terminal type code

**Format:**
```
┌────────┬─────────┬─────────────────┐
│ 7TTYP  │   02    │  Terminal Type  │
│ (1)    │  (1)    │  (2)            │
└────────┴─────────┴─────────────────┘
```

**Code:** `0x54545950` ("TTYP")

**Terminal Type:** 16-bit terminal type code
- Norsk Data standard terminal codes
- Controls terminal-specific features

**Source:** `RP-P2-TAD.NPL:860-872` (`CSTYP`), `MP-P2-TAD.NPL:188-197` (`BDTTYP`)

**Processing (receive):**
```npl
TDBTPT=:D SHZ -1; T:=TDTAFI; X:=TDTALA+A; *LDATX
IF D BIT "0" THEN
   A SHZ 10=:D; X+1; *LDATX
   A SHZ -10+D
FI
A=:CTTYP
```

**Example:**
```
Set terminal type 0x0123:
┌───────┬────┬─────┬─────┐
│ 7TTYP │ 02 │ 01  │ 23  │
└───────┴────┴─────┴─────┘
```

### 7DESC - Define Escape Character

**Purpose:** Set the escape character for terminal

**Format:**
```
┌────────┬─────────┬────────────────┐
│ 7DESC  │   01    │  Escape Char   │
│ (1)    │  (1)    │  (1)           │
└────────┴─────────┴────────────────┘
```

**Code:** `0x44455343` ("DESC")

**Escape Char:** ASCII code of escape character (typically 0x1B)

**Source:** `RP-P2-TAD.NPL:930-943` (`CSDAE`), `MP-P2-TAD.NPL:242-249` (`BDDESC`)

**Processing (receive):**
```npl
TDBTPT=:D SHZ -1; T:=TDTAFI; X:=TDTALA+A; *LDATX
IF D BIT "0" THEN A/\377 ELSE A SHZ -10 FI; A=:T
CESCP/\177400+T=:CESCP
```

**Example:**
```
Set escape to Ctrl-C (0x03):
┌───────┬────┬────┐
│ 7DESC │ 01 │ 03 │
└───────┴────┴────┘
```

### 78MOD - 8-Bit Mode

**Purpose:** Enable/disable 8-bit character mode

**Format:**
```
┌────────┬─────────┬──────────────┐
│ 78MOD  │   02    │  UMOD Value  │
│ (1)    │  (1)    │  (2)         │
└────────┴─────────┴──────────────┘
```

**Code:** `0x384D4F44` ("8MOD")

**UMOD Value:**
- `0x0000`: 7-bit mode (strip bit 7)
- `0x0001`: 8-bit mode (pass all bits)

**Source:** `MP-P2-TAD.NPL:204-216` (`BD8MOD`)

**Processing (receive):**
```npl
TDBTPT; AD SHZ -1; T:=TDTAFI; X:=TDTALA+A
IF D BIT 17 THEN *LDDTX; AD SH 10 ELSE *LDATX FI
IF A=1 THEN TINFO BONE 58BIT=:TINFO FI
```

**Example:**
```
Enable 8-bit mode:
┌───────┬────┬─────┬─────┐
│ 78MOD │ 02 │ 00  │ 01  │
└───────┴────┴─────┴─────┘
```

### 7OPSV - OPSYS Version and TAD Protocol

**Purpose:** Exchange operating system version and TAD protocol number

**Format:**
```
┌────────┬─────────┬────────────────────────────┐
│ 7OPSV  │   03    │  OS Ver  │  TAD Protocol   │
│ (1)    │  (1)    │  (1)     │  (2)            │
└────────┴─────────┴────────────────────────────┘
```

**Code:** `0x4F505356` ("OPSV")

**Fields:**
- **OS Version:** SINTRAN version number
- **TAD Protocol:** TAD protocol level (3 = full features)

**Source:** `MP-P2-TAD.NPL:223-235` (`BDOPSV`)

**Processing (receive):**
```npl
TDBTPT=:D SHZ -1; T:=TDTAFI; X:=TDTALA+A; *LDATX
IF D BIT "0" THEN
   A SHZ 10=:D; X+1; *LDATX
   A/\377+D
ELSE
   A/\177400=:D; X+1; *LDATX
   A SHZ -10+D
FI
A=:OSVTPN
```

**Example:**
```
SINTRAN L (version 12), Protocol 3:
┌───────┬────┬────┬─────┬─────┐
│ 7OPSV │ 03 │ 0C │ 00  │ 03  │
└───────┴────┴────┴─────┴─────┘
```

---

## Break and Echo Messages

### 7BMMX - Break Message

**Purpose:** Define break character detection strategy

**Format (Simple):**
```
┌────────┬─────────┬──────────┬──────────┬──────────┐
│ 7BMMX  │   03    │ Strategy │  MaxBreak (2 bytes) │
│ (1)    │  (1)    │  (1)     │  (2)                │
└────────┴─────────┴──────────┴──────────┴──────────┘
```

**Format (Table):**
```
┌────────┬─────────┬──────────┬──────────┬─────────────────────┐
│ 7BMMX  │   23    │ Strategy │ MaxBreak │  Break Table (20)   │
│ (1)    │  (1)    │  (1)     │  (2)     │  (20)               │
└────────┴─────────┴──────────┴──────────┴─────────────────────┘
```

**Code:** `0x424D4D58` ("BMMX")

**Strategy Values:**
- `1-6`: Predefined break strategies
- `7`: Custom break table (20 bytes = 8 words of character bitmap)
- `8`: Strategy 8 (if protocol >= 3)
- `9`: Strategy 9 (if protocol >= 3)
- `11`: Custom break table from user's BRKTAB

**MaxBreak:** Maximum break level (0-255)

**Break Table Format:** 8 words (20 bytes) where each bit represents a character:
- Word 0, bit 0 = character 0x00
- Word 0, bit 15 = character 0x0F
- Word 7, bit 0 = character 0x70
- Word 7, bit 15 = character 0x7F

**Source:** `RP-P2-TAD.NPL:766-795` (`BDBREA`)

**Processing (send):**
```npl
IF T=X:=7 THEN T:=23 ELSE T:=3 FI; T=:MSSIZ
A:=7BMMX; T:=MSSIZ; CALL CRHEOD
BRSTR=:D; CALL STORBYT
TDBTPT SHZ-1; T:=TDTALA+A; 41ITAD.BRKMAX
T=:X:=TDTAFI; *STATX
TDBTPT+2=:TDBTPT; REMSIZ-2=:REMSIZ
IF D=7 THEN
   IF AREG=11 THEN 41ITAD.BRKTAB ELSE 41ITAD+"PBRK7" FI
   A=:D; CALL CBRECTA
FI
```

**Example (Strategy 1):**
```
┌───────┬────┬────┬─────┬─────┐
│ 7BMMX │ 03 │ 01 │ 00  │ 10  │  (Strategy 1, MaxBreak=16)
└───────┴────┴────┴─────┴─────┘
```

### 7ECKM - Echo Message

**Purpose:** Define character echo strategy

**Format (Simple):**
```
┌────────┬─────────┬──────────┐
│ 7ECKM  │   01    │ Strategy │
│ (1)    │  (1)    │  (1)     │
└────────┴─────────┴──────────┘
```

**Format (Table):**
```
┌────────┬─────────┬──────────┬─────────────────────┐
│ 7ECKM  │   21    │ Strategy │  Echo Table (20)    │
│ (1)    │  (1)    │  (1)     │  (20)               │
└────────┴─────────┴──────────┴─────────────────────┘
```

**Code:** `0x45434B4D` ("ECKM")

**Strategy Values:**
- `1-6`: Predefined echo strategies
- `7`: Custom echo table (20 bytes = 8 words of character bitmap)

**Echo Table Format:** Same as break table - 8 words where each bit represents whether to echo that character

**Source:** `RP-P2-TAD.NPL:735-758` (`BDECHO`)

**Processing (send):**
```npl
IF A=7 THEN T:=21 ELSE T:=1 FI; T=:MSSIZ
A:=7ECKM; T:=MSSIZ; CALL CRHEOD
AREG=:D; CALL STORBYT
IF D=7 THEN
   41ITAD+"PECH7"=:D; CALL CBRECTA
FI
```

**Example (Strategy 2):**
```
┌───────┬────┬────┐
│ 7ECKM │ 01 │ 02 │
└───────┴────┴────┘
```

---

## Request and Response Messages

### 7RFI - Ready For Input

**Purpose:** Request more input data (buffer empty)

**Format:**
```
┌────────┬─────────┐
│  7RFI  │   00    │
│  (1)   │  (1)    │
└────────┴─────────┘
```

**Code:** `0x52464920` ("RFI ")

**No data field** - just header

**Source:** `RP-P2-TAD.NPL:1115-1147` (`SNDRFI`)

**When Sent:**
- Input buffer empty and user requests input
- After rejecting data message
- In nowait mode when no data available

**Example:**
```
┌──────┬────┐
│ 7RFI │ 00 │
└──────┴────┘
```

### 7REJE - Reject Message

**Purpose:** Reject invalid/unexpected message

**Format:**
```
┌────────┬─────────┬─────────────────┐
│ 7REJE  │   01    │  Rejected Type  │
│ (1)    │  (1)    │  (1)            │
└────────┴─────────┴─────────────────┘
```

**Code:** `0x52454A45` ("REJE")

**Rejected Type:** Message type code that was rejected

**Source:** `RP-P2-TAD.NPL:1161-1201` (`SNDREJ`), `MP-P2-TAD.NPL:926-948` (`REJECT`)

**When Sent:**
- Received inconsistent message (size mismatch)
- Received unexpected control message
- Received message for wrong state

**Processing (send):**
```npl
A:=7REJE; T:=1; CALL CREMES
41ITAD.CURMES; CALL BYTPUT
IF 41ITAD.CURMES=7BDAT THEN
   A:=7RFI; T:=0; CALL CREMES     % Also send RFI after rejecting data
FI
CALL SNDBUF
```

**Example:**
```
Reject TMOD message:
┌───────┬────┬──────┐
│ 7REJE │ 01 │ 7TMOD│
└───────┴────┴──────┘
```

### 7ISRQ - ISIZE Request

**Purpose:** Query buffer/input size

**Format:**
```
┌────────┬─────────┐
│ 7ISRQ  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x49535251` ("ISRQ")

**No data field**

**Source:** `RP-P2-TAD.NPL:991-1000` (`BISIZ/PISIZ`)

**Response:** `7ISRS` message with size

**Example:**
```
┌───────┬────┐
│ 7ISRQ │ 00 │
└───────┴────┘
```

### 7ISRS - ISIZE Response

**Purpose:** Report buffer/input size

**Format:**
```
┌────────┬─────────┬──────────────┐
│ 7ISRS  │   02    │  Size (2)    │
│ (1)    │  (1)    │  (2)         │
└────────┴─────────┴──────────────┘
```

**Code:** `0x49535253` ("ISRS")

**Size:** 16-bit size value
- Number of characters available
- Bit 15 (0x8000) indicates break character present

**Source:** `MP-P2-TAD.NPL:563-570` (processing)

**Processing (receive):**
```npl
X:=TMPBUF; T:=6; CALL DRXACC(XDGER); A SHZ 10=:RDATR
X:=TMPBUF; T:=7; CALL DRXACC(XDGER)
T:=RDATR+A=:RDATR
IF OTAD.RSPNUM=7ISRS THEN
   A:=RDATR; IF D BIT 17 THEN A BONE 17 FI
   A=:DFRDATR:="MFISIZ"; CALL CXRTACT
FI
```

**Example:**
```
100 characters available, no break:
┌───────┬────┬─────┬─────┐
│ 7ISRS │ 02 │ 00  │ 64  │  (0x0064 = 100)
└───────┴────┴─────┴─────┘
```

### 7ERRS - Error Response

**Purpose:** Report error condition

**Format:**
```
┌────────┬─────────┬─────────────────┐
│ 7ERRS  │   02    │  Error Code (2) │
│ (1)    │  (1)    │  (2)            │
└────────┴─────────┴─────────────────┘
```

**Code:** `0x45525253` ("ERRS")

**Error Code:** 16-bit SINTRAN error code

**Source:** `MP-P2-TAD.NPL:571-578` (processing)

**Processing (receive):**
```npl
X:=TMPBUF; T:=6; CALL DRXACC(XDGER); A SHZ 10=:RDATR
X:=TMPBUF; T:=7; CALL DRXACC(XDGER)
T:=RDATR+A=:RDATR
IF OTAD.RSPNUM=7ERRS THEN
   IF TDRADDR.RTRES.STATUS BIT 5WAIT THEN
      RDATR=:DFRDATR; "MFERSP"; CALL CXRTACT
   FI
FI
```

**Example:**
```
Error TER00 (0x0165):
┌───────┬────┬─────┬─────┐
│ 7ERRS │ 02 │ 01  │ 65  │
└───────┴────┴─────┴─────┘
```

---

## System Control Messages

### 7SYCN - System Control

**Purpose:** Send system control command

**Format:**
```
┌────────┬─────────┬─────────────────┐
│ 7SYCN  │   02    │  Control Word   │
│ (1)    │  (1)    │  (2)            │
└────────┴─────────┴─────────────────┘
```

**Code:** `0x5359434E` ("SYCN")

**Control Word:** System-specific control code

**Source:** `RP-P2-TAD.NPL:597-609` (`CTOBAD`)

**Processing (send):**
```npl
IF AREG=23 THEN 7SYCN ELSE 7USCN FI
T:=2; CALL CREMES
DREG; CALL WORDPUT
IF AREG=23 THEN
   IF DREG=1 OR A=13 OR A=17 THEN CALL SNDBUF FI
FI
```

**Auto-send conditions:**
- Control word = 1 (always send)
- Control word = 13 (0x0D - CR)
- Control word = 17 (0x11 - DC1)

**Example:**
```
System control 0x0001:
┌───────┬────┬─────┬─────┐
│ 7SYCN │ 02 │ 00  │ 01  │
└───────┴────┴─────┴─────┘
```

### 7USCN - User Control

**Purpose:** Send user control command

**Format:**
```
┌────────┬─────────┬─────────────────┐
│ 7USCN  │   02    │  Control Word   │
│ (1)    │  (1)    │  (2)            │
└────────┴─────────┴─────────────────┘
```

**Code:** `0x5553434E` ("USCN")

**Control Word:** User-specific control code

**Source:** `RP-P2-TAD.NPL:597-609` (`CTOBAD`)

**Processing (send):**
```npl
IF AREG=23 THEN 7SYCN ELSE 7USCN FI
T:=2; CALL CREMES
DREG; CALL WORDPUT
IF AREG=23 THEN
   % ... system control handling ...
ELSE
   7ERRS; CALL SNDWT     % Send and wait for error response
FI
```

**Always waits for response** (`7ERRS` message)

**Example:**
```
User control 0x0042:
┌───────┬────┬─────┬─────┐
│ 7USCN │ 02 │ 00  │ 42  │
└───────┴────┴─────┴─────┘
```

### 7CPCO - Completion Code

**Purpose:** Report operation completion status

**Format:**
```
┌────────┬─────────┬────────────────────────────┐
│ 7CPCO  │   04    │  Completion Code (4 bytes) │
│ (1)    │  (1)    │  (4)                       │
└────────┴─────────┴────────────────────────────┘
```

**Code:** `0x4350434F` ("CPCO")

**Completion Code:** 32-bit completion code (2 words)

**Source:** `RP-P2-TAD.NPL:1065-1076` (`SNDCP`)

**Processing (send):**
```npl
7CPCO; T:=4; CALL CRHEEV
TDBTPT SHZ -1; T:=TDTAFI; X:=TDTALA+A
CPC1; *STATX
X+1; CPC2; *STATX
TDBTPT+4=:TDBTPT; REMSIZ-4=:REMSIZ
CALL SNDBUF
```

**Example:**
```
Completion code 0x12345678:
┌───────┬────┬─────┬─────┬─────┬─────┐
│ 7CPCO │ 04 │ 12  │ 34  │ 56  │ 78  │
└───────┴────┴─────┴─────┴─────┴─────┘
```

---

## Connection Control Messages

### 7RESE - Reset

**Purpose:** Reset TAD connection to initial state

**Format:**
```
┌────────┬─────────┐
│ 7RESE  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x52455345` ("RESE")

**No data field**

**Source:** `RP-P2-TAD.NPL:558-564` (`CTIBAD`)

**Triggers:** `7RECO` (Reset Confirm) response

**Processing (send):**
```npl
7RESE; T:=0; CALL CREMES
7RECO; CALL SNDWT     % Send reset and wait for confirm
```

**Example:**
```
┌───────┬────┐
│ 7RESE │ 00 │
└───────┴────┘
```

### 7RECO - Reset Confirm

**Purpose:** Acknowledge reset request

**Format:**
```
┌────────┬─────────┐
│ 7RECO  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x5245434F` ("RECO")

**No data field**

**Source:** `MP-P2-TAD.NPL:559-562` (processing)

**Processing (receive):**
```npl
IF X=RESCF THEN                            % RESET-CONF
   IF OTAD.RSPNUM=7RECO GO RSPRST
   GO TDRINP
FI
```

**Example:**
```
┌───────┬────┐
│ 7RECO │ 00 │
└───────┴────┘
```

### 7DCON - Disconnect

**Purpose:** Request TAD disconnection

**Format:**
```
┌────────┬─────────┐
│ 7DCON  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x44434F4E` ("DCON")

**No data field**

**Source:** `MP-P2-TAD.NPL:626-628` (processing)

**Processing (receive):**
```npl
IF X=BDDIS THEN                            % DISCONNECT-MESSAGE
   GO DSTOTA                               % STOP AND DISCONNECT TAD
FI
```

**Triggers:** `DSTOTA` (forced disconnect with cleanup)

**Example:**
```
┌───────┬────┐
│ 7DCON │ 00 │
└───────┴────┘
```

---

## Escape and Local Mode Messages

### 7ESCA - Escape

**Purpose:** Signal escape character received

**Format:**
```
┌────────┬─────────┐
│ 7ESCA  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x45534341` ("ESCA")

**No data field**

**Source:** `MP-P2-TAD.NPL:602-625` (processing)

**Processing (receive):**
```npl
IF X=BDESC OR X=RLOCA THEN
   IF DFLAG NBIT 5IESC THEN                % ESCAPE ENABLED
      IF X=BDESC THEN
         CESCP/\377=:LAST                  % ESCAPE
      ELSE
         IF FLAGB BIT 5LCHAR THEN
            CESCP SHZ-10=:LAST             % LOCAL CHARACTER
         ELSE
            177=:LAST                      % RUBOUT IN NORD-NET
         FI
      FI
      DFLAG BZERO 5RQI=:DFLAG
      CALL ESCAPE                          % Process escape
      TAD:=ERESP; T=:X:=XFWHD
      CALL MXMSG                           % Write escape response
   ELSE                                    % ESCAPE DISABLED
      TAD:=EDRSP; T=:X:=XFWHD
      CALL MXMSG                           % Write escape disabled response
      AD:=OTAD.PARTNER; X:=PORTNO
      T:=XFSND; CALL MXMSG                 % Send response
   FI
FI
```

**Triggers:** Escape response (`7CERS`)

**Example:**
```
┌───────┬────┐
│ 7ESCA │ 00 │
└───────┴────┘
```

### 7CERS - Escape Response

**Purpose:** Acknowledge escape processing

**Format:**
```
┌────────┬─────────┐
│ 7CERS  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x43455253` ("CERS")

**No data field**

**Source:** `MP-P2-TAD.NPL:555-558` (processing), `RP-P2-TAD.NPL:915` (send)

**Processing (receive):**
```npl
IF A=CESCR THEN                            % CESC-RESP
   IF OTAD.RSPNUM=7CERS GO RSPRST
   GO TDRINP
FI
```

**Processing (send):**
```npl
IF TDRADDR.RTRES=RTREF THEN
   7CERS; CALL SNDWT                       % SEND AND WAIT FOR RESPONSE
ELSE
   CALL SNDBUF                             % JUST SEND MESSAGE
FI
```

**Example:**
```
┌───────┬────┐
│ 7CERS │ 00 │
└───────┴────┘
```

### 7RLOC - Remote Local

**Purpose:** Remote/local mode switch (NORD-NET)

**Format:**
```
┌────────┬─────────┐
│ 7RLOC  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x524C4F43` ("RLOC")

**No data field**

**Source:** Same as `7ESCA` processing

**Purpose:** In NORD-NET, this toggles between:
- **Remote mode:** Terminal connected to remote system
- **Local mode:** Terminal connected to local system

**Example:**
```
┌───────┬────┐
│ 7RLOC │ 00 │
└───────┴────┘
```

---

## Nowait Mode Messages

### 7NOWT - Nowait Status

**Purpose:** Report nowait operation status (normal termination)

**Format:**
```
┌────────┬─────────┬──────────┐
│ 7NOWT  │   01    │  Status  │
│ (1)    │  (1)    │  (1)     │
└────────┴─────────┴──────────┘
```

**Code:** `0x4E4F5754` ("NOWT")

**Status:** Operation status code (0 = success)

**Source:** `RP-P2-TAD.NPL:1088-1102` (`NOWTSTA`)

**Processing (send):**
```npl
IF A=0 THEN A:=7NOWT ELSE A:=7TNOW FI
T:=41OTAD=:B; T:=1; CALL CRHEEV
NWS; CALL STORBYT
CALL SNDBUF
```

**Example:**
```
Success (status 0):
┌───────┬────┬────┐
│ 7NOWT │ 01 │ 00 │
└───────┴────┴────┘
```

### 7TNOW - Terminate Nowait

**Purpose:** Report nowait operation termination (error)

**Format:**
```
┌────────┬─────────┬──────────┐
│ 7TNOW  │   01    │  Status  │
│ (1)    │  (1)    │  (1)     │
└────────┴─────────┴──────────┘
```

**Code:** `0x544E4F57` ("TNOW")

**Status:** Error code (non-zero)

**Source:** Same as `7NOWT`

**Example:**
```
Error (status 5):
┌───────┬────┬────┐
│ 7TNOW │ 01 │ 05 │
└───────┴────┴────┘
```

### 7NWRE - Nowait Restart

**Purpose:** Restart nowait operation

**Format:**
```
┌────────┬─────────┐
│ 7NWRE  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x4E575245` ("NWRE")

**No data field**

**Source:** `MP-P2-TAD.NPL:477-481` (processing)

**Processing (receive):**
```npl
IF X=NWREM THEN                            % NOWAIT RESTART
   T:=XFSND; AD:=OTAD.PARTNER; X:=PORTNO
   CALL MXMSG                              % RETURN MESSAGE
   GO FAR DATRES                           % RESTART USER
FI
```

**Example:**
```
┌───────┬────┐
│ 7NWRE │ 00 │
└───────┴────┘
```

---

## Special Messages

### 7DUMM - Dummy Message

**Purpose:** Empty message / padding

**Format:**
```
┌────────┬─────────┐
│ 7DUMM  │   00    │
│ (1)    │  (1)    │
└────────┴─────────┘
```

**Code:** `0x44554D4D` ("DUMM")

**No data field**

**Source:** `MP-P2-TAD.NPL:256-260` (`BDDUMM`)

**Usage:**
- Replace data messages when clearing buffer
- Initial buffer in `INISND`
- Padding/alignment

**Processing (receive):**
```npl
IF A=7DUMM THEN
   CALL BDDUMM
FI

BDDUMM: T:=REMBYT-; T-1; TDBTPT+T=:TDBTPT; REMSIZ-T=:REMSIZ
        0=:REMBYT
        EXITA
```

**Example:**
```
┌───────┬────┐
│ 7DUMM │ 00 │
└───────┴────┘
```

### 7TREP - TREP Status

**Purpose:** Report transmission error status

**Format:**
```
┌────────┬─────────┬───────────────────┐
│ 7TREP  │   02    │  Status Word (2)  │
│ (1)    │  (1)    │  (2)              │
└────────┴─────────┴───────────────────┘
```

**Code:** `0x54524550` ("TREP")

**Status Word:** Transmission status bits

**Status Bits:**
| Bit | Name | Purpose |
|-----|------|---------|
| 2 | Buffer overrun | Receiver buffer full |
| 3 | Parity error | Parity check failed |
| 4 | Framing error | Frame sync lost |

**Source:** `MP-P2-TAD.NPL:482-495` (processing)

**Processing (receive):**
```npl
IF X=TREPS THEN                            % TREP STATUS
   X:=TMPBUF; T:=6; CALL DRXACC(XDGER)
   A SHZ 10=:RDATR
   X:=TMPBUF; T:=7; CALL DRXACC(XDGER)
   T:=RDATR+A=:RDATR
   T:=XFSND; AD:=OTAD.PARTNER; X:=PORTNO
   CALL MXMSG                              % Return message
   T:=RDATR; A:=TINFO
   IF T BIT 2 THEN A BONE 5BFUL FI         % BUFFER OVERRUN
   IF T BIT 3 THEN A BONE 5PAER FI         % PARITY ERROR
   IF T BIT 4 THEN A BONE 5FRER FI         % FRAMING ERROR
   A=:TINFO
   GO TDRINP
FI
```

**Example:**
```
Parity error (bit 3):
┌───────┬────┬─────┬─────┐
│ 7TREP │ 02 │ 00  │ 08  │  (0x0008)
└───────┴────┴─────┴─────┘
```

### 7UMOD - UMOD Strategy

**Purpose:** Set user mode strategy

**Format:**
```
┌────────┬─────────┬──────────────────┐
│ 7UMOD  │   02    │  UMOD Value (2)  │
│ (1)    │  (1)    │  (2)             │
└────────┴─────────┴──────────────────┘
```

**Code:** `0x554D4F44` ("UMOD")

**UMOD Value:** User mode configuration (16-bit)

**Source:** `RP-P2-TAD.NPL:883-896` (`CSUMOD`)

**Processing (send):**
```npl
IF X.PORTNO=0 OR X.OSVTPN/\377<4 THEN EXIT FI  % Protocol must be >=4
7UMOD; T:=2; CALL CREMES
X:=BREG; A:=X.D4; CALL WORDPUT           % D4 contains UMOD strategy
CALL SNDBUF
```

**Requires:** TAD protocol >= 4

**Example:**
```
UMOD strategy 0x0042:
┌───────┬────┬─────┬─────┐
│ 7UMOD │ 02 │ 00  │ 42  │
└───────┴────┴─────┴─────┘
```

---

## Message Priority Levels

TAD messages have two priority levels handled differently by the driver:

### Normal Priority Messages

**Processed sequentially from buffer:**
- `7BDAT` - Data
- `7TMOD` - Terminal mode
- `7TTYP` - Terminal type
- `7DESC` - Define escape
- `78MOD` - 8-bit mode
- `7OPSV` - OPSYS version
- `7DUMM` - Dummy

**Processing:**
- Received only if input buffer empty (`BUFFID=0`)
- Messages queued in receive buffer
- Processed in order by `GETMES`

### High Priority Messages

**Processed immediately:**
- `7ESCA` / `7RLOC` - Escape / remote-local
- `7DCON` - Disconnect
- `7CERS` - Escape response
- `7RECO` - Reset confirm
- `7NWRE` - Nowait restart
- `7ISRS` - ISIZE response
- `7ERRS` - Error response
- `7TREP` - TREP status

**Processing:**
- Received immediately even if input buffer has data
- Stored in temporary buffer (`TMPBUF`)
- Processed before returning to normal priority queue

**Source:** `MP-P2-TAD.NPL:445-496`

```npl
IF XMTHI><T GO FAR NORMP                   % NORMAL PRIORITY
T:=XFRCV; A:=PORTNO; CALL MXMSG           % RECEIVE HIGH PRIORITY
IF T=0 GO BDRWT
X:=D=:TMPBUF
T:=XFRHD; A:=TMPBUF; CALL MXMSG           % READ MESSAGE HEADER
X=:HIGHT                                  % SAVE MESSAGE TYPE
% ... process high priority message ...
GO TDRINP
```

---

## Buffer Management Integration

TAD uses XMSG for buffer management. Key operations:

### Buffer Pool Operations

**Initialize Pool:** (`INIBDR` - Line 382)
```npl
T:=XFALM; FBSIZ; X:=NOBUFF; CALL MXMSG    % Allocate space
FOR X:=1 TO NOBUFF DO
   T:=XFGET; A:=FBSIZ; CALL MXMSG         % Get buffer
   % ... add to pool ...
   CALL MPUTPOOL                          % Add to POOLLI chain
OD
```

**Get Buffer from Pool:** (`GETPOOL` - Line 44)
```npl
POOLLI; IF A=0 AND D=0 GO NOPOL           % Pool empty
A=:T; D=:X; *LDDTX                        % Get next in chain
AD=:POOLLI                                % Update pool pointer
AD=:TDTADD; X+2; *LDATX                   % Get buffer address
A=:BUFFID                                 % Set buffer ID
41ITAD.FBSIZ-BUDIS=:REMSIZ                % Set remaining size
```

**Return Buffer to Pool:** (`PUTPOOL` - Line 20)
```npl
A=:T; D=:X; AD:=POOLLI; *STDTX            % Link old POOLLI
X+2; A:=XREG; *STATX                      % Store buffer ID
AD=:POOLLI                                % Update POOLLI
0=:BUFFID                                 % Clear buffer ID
```

### Buffer State Tracking

| State | Condition | Location |
|-------|-----------|----------|
| **Free** | In POOLLI chain | Buffer pool |
| **Input Active** | ITAD.BUFFID != 0 | Input datafield |
| **Output Active** | OTAD.BUFFID != 0 | Output datafield |
| **Temporary** | TMPBUF != 0 | High-priority processing |
| **Mail** | MBFID != 0 | Mail system |

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\TAD-Message-Formats.md`

**Related Documents:**
- `TAD-Protocol-Analysis.md` - Overall protocol analysis
- `TAD-Protocol-Flows.md` - Protocol flow diagrams (to be created)
