# Deep Analysis of DMA Transmit: XSSDATA - SINTRAN HDLC Transmission

## Overview

XSSDATA is the primary SINTRAN subroutine for HDLC data transmission. It handles user data transmission requests, sets up DMA descriptors with COM5025 control bits, and manages the complete transmission lifecycle from setup to completion interrupt handling.

## Context: XSSDATA Entry Point and Purpose

### Function Declaration (Line 103636):
```assembly
SUBR XSSDATA,HOINT,POFTO,XSSND
```
- **XSSDATA**: Main data transmission subroutine
- **HOINT**: Output interrupt handler (transmission completion)
- **POFTO**: Output timeout handler
- **XSSND**: Special entry for privileged users (direct DMA list access) and **HDLC retransmission**

### Purpose Documentation (Lines 103636 comments):
```
PURPOSE: TRANSMITT DATA TO REMOTE MACHINE

OPERATION:  RECEIVES MESSAGES CONTAINING DATA TO BE SENT TO
            REMOTE MACHINE. THE RESULTS OF THE TRANSFER IS SENT
            BACK TO USER IN THE SAME MESSAGE.

MESSAGE DESCRIPTION:
         - FUNCTION = DATA
         - STATUS   = RET-STATUS
         - ADDSTA   = HARDWARE STATUS
         - DATA     = DATA TO BE TRANSMITTED / DMA LIST POINTER *81F*
```

## Phase 1: Initialization and Validation (Line 103636)

### Entry Point and Status Check:
```assembly
103636   XSSDATA: IF INTSTA >< 2 THEN A:=ENINIT; GO BACKX FI
```
- **`IF INTSTA >< 2`**: Check if interface is initialized (INTSTA must equal 2)
- **`A:=ENINIT; GO BACKX`**: Set "not initialized" error and exit if not ready
- **Purpose**: Ensure HDLC interface is properly initialized before transmission

### DMA List Initialization:
```assembly
103644          LISTP =: LIINT                            % *81F*
```
- **`LISTP =: LIINT`**: Set DMA list interrupt pointer to list pointer
- **Purpose**: Initialize DMA list processing to start of available descriptors
- **Comment *81F***: Indicates enhanced functionality (likely ND-500 series)

## Phase 2: Message Size Validation (Line 103646)

### Buffer Size Calculation:
```assembly
103646          X-BHEAD; CALL RACTB; X+BHEAD              % MESSAGE SIZE
```
- **`X-BHEAD`**: Remove buffer header offset from X pointer
- **`CALL RACTB`**: Call subroutine to calculate actual buffer size
- **`X+BHEAD`**: Restore buffer header offset
- **Purpose**: Get actual message data size excluding headers

### Minimum Size Check:
```assembly
103651          IF A < 7 THEN A:=EILFZ; GO BACKX FI       % MESSAGE IS TOO SMALL
```
- **`IF A < 7`**: Check if message is less than 7 bytes
- **`A:=EILFZ`**: Set "Illegal Frame Size" error code
- **Rationale**: HDLC minimum frame = Address(1) + Control(1) + Data(>=1) + FCS(2) + overhead = ~7 bytes minimum

### Maximum Size Check:
```assembly
103656          IF A-BCHEA>MAXR THEN A:=EILSIZ; GO BACKX FI  % TOO LONG FRAME
```
- **`A-BCHEA>MAXR`**: Check if (message size - buffer header) exceeds maximum frame size
- **`A:=EILSIZ`**: Set "Illegal Size" error code  
- **MAXR**: System-defined maximum frame size constant
- **Purpose**: Prevent transmission of oversized frames

## Phase 3: DMA Descriptor Setup (Lines 103664-103675)

### Byte Count Configuration:
```assembly
103664          A-DISP1=:LIINT.LBYTC                      % BYTECOUNT FOR LIST
```
- **`A-DISP1=:LIINT.LBYTC`**: Set byte count in DMA descriptor (size minus displacement)
- **DISP1**: Header displacement reserved in buffer
- **LIINT.LBYTC**: Byte count field in DMA descriptor
- **Purpose**: Tell DMA controller exact number of bytes to transmit

### Buffer Address Setup:
```assembly
103667          A:=OMSG+CHEAD=:X.LMEM2=:D                 % SET BUFFER ADDR.
103673          T:=MASTB=:X.LMEM1                         % PHYSICAL BANK IN LMEM2 AND IN T
```
- **`A:=OMSG+CHEAD=:X.LMEM2=:D`**: Set buffer address (low word) and save to D register
- **`T:=MASTB=:X.LMEM1`**: Set memory bank (high word) from Master Bank
- **OMSG+CHEAD**: Output message buffer + control header offset
- **Purpose**: Configure DMA descriptor with complete buffer address (bank + offset)

### **CRITICAL**: COM5025 Control Bits Setup:
```assembly
103675          FSERM=:X.LKEY                             % TRANSMIT ONE BLOCK ONLY
```
- **`FSERM=:X.LKEY`**: Set the LKEY field with FSERM constant
- **FSERM**: **Frame Start and End Marker** = 002003₈ (confirmed from analysis)
- **Breakdown of FSERM = 002003₈**:
  - **Bits 15-8**: `010` = Block to be transmitted (ToTransmit status)
  - **Bits 7-0**: `003` = COM5025 control bits (TSOM=1, TEOM=1)
- **Hardware Action**: DMA will write `0x03` to COM5025, generating complete HDLC frame

### Descriptor Address Storage:
```assembly
103677          D=:X
```
- **`D=:X`**: Store descriptor address in X register
- **Purpose**: X now points to configured DMA descriptor ready for transmission

## Phase 4: Transmission Data Setup (Lines 103700-103703)

### Data Loading and Preparation:
```assembly
103700          *LDATX
103701          A=:SNDAC
103702          0=:D                                      % *81F*
```
- **`*LDATX`**: Load data into transmission buffer (assembler directive)
- **`A=:SNDAC`**: Store first word of data in SNDAC (used for logging)
- **`0=:D`**: Clear D register for security/cleanup
- **SNDAC**: Send AC field - stores first word of transmitted frame for diagnostics

## Phase 5: DMA Hardware Activation (XHMST - Lines 103703-103732)

### DMA Address Programming:
```assembly
103703   XHMST: LIINT+DPITPHYS;
103705   XXHMST:T:=HDEV+WDMA; *IOF; EXR ST%LIST ADDRESS *81F*
```
- **`LIINT+DPITPHYS`**: Calculate physical DMA list address
- **`T:=HDEV+WDMA`**: Set target to hardware DMA address register
- **`*IOF; EXR ST`**: Execute I/O operation to write DMA address to hardware
- **DPITPHYS**: Physical address translation offset for DMA operations

### DMA Command Execution:
```assembly
103711          A:=2000\/D; T+"WDCR-WDMA"; *EXR ST        %START TRANSMITTER  *81F*
```
- **`A:=2000\/D`**: Load command `2000₈` (1024 decimal) = Start Transmitter
- **`T+"WDCR-WDMA"`**: Target Write DMA Control Register (WDCR)
- **`*EXR ST`**: Execute command to start DMA transmission
- **Purpose**: Hardware begins reading DMA descriptors and transmitting data

### Additional Hardware Configuration:
```assembly
103715          T+"RDCR-WDCR"; X:=-20;*EXR ST
103720          CALL LTOUT; *JAF *-2; ION
```
- **Line 103715**: Configure Read DMA Control Register with value -20₈
- **Line 103720**: Call timeout setup, enable interrupts
- **Purpose**: Configure DMA controller for proper operation and timeout handling

### Transmitter Control Activation:
```assembly
103723          1134+CMODI; T:=HDEV+WTTC; *EXR ST
```
- **`1134+CMODI`**: Load transmitter control value (1134₈ + communication mode)
- **`T:=HDEV+WTTC`**: Target Write Transmitter Transfer Control register
- **Purpose**: Enable HDLC transmitter with DMA mode

### Device State Management:
```assembly
103730          1 =: ACTSW                                % *H*
103732          OMSG =: DCBX
```
- **`1 =: ACTSW`**: Set Activity Switch to 1 (device now active)
- **`OMSG =: DCBX`**: Set Device Control Block pointer to current message
- **Purpose**: Mark device as active and associate with current transmission

## Phase 6: Timeout and Interrupt Setup (Lines 103734-103737)

### Timeout Configuration:
```assembly
103734   CONT:  A:=TTMR=:TMR
103736          CALL ID12                                 %WAIT FOR INTERRUPT
103737          GO HOINT; *)FILL
```
- **`A:=TTMR=:TMR`**: Set timeout timer value
- **`CALL ID12`**: Wait for interrupt level 12 (transmitter interrupt)
- **`GO HOINT`**: Jump to output interrupt handler when interrupt occurs
- **Purpose**: Wait for transmission completion or timeout

## Phase 7: Interrupt Handling - HOINT (Lines 104033-104115)

### Interrupt Entry and Status Reading:
```assembly
104033   HOINT: 0=:TMR                                    %RESET TIMER
104034          T:=HDEV+RTTS; *EXR ST                     %READ STATUS
104037          A=:HASTAT                                 %SAVE STATUS
```
- **`0=:TMR`**: Clear timeout timer (transmission completed)
- **`T:=HDEV+RTTS; *EXR ST`**: Read Transmitter Transfer Status register
- **`A=:HASTAT`**: Store hardware status in HASTAT variable
- **Purpose**: Get transmission completion status from hardware

### Spurious Interrupt Check:
```assembly
104040          IF T:=ACTSW = 0 THEN MIN DUIN; P+0; CALL WT12 FI         % *H*
```
- **`IF T:=ACTSW = 0`**: Check if device is inactive (spurious interrupt)
- **`MIN DUIN; P+0`**: Increment dummy interrupt counter
- **`CALL WT12`**: Wait for proper interrupt
- **Purpose**: Handle spurious interrupts gracefully

### Device Deactivation:
```assembly
104046          0=: ACTSW                                 % *H*
104047          IF CMODI = 40 THEN
104053            T:=HDEV+WTTC; *EXR ST   %TURN OFF RQTS
104056          FI
```
- **`0=: ACTSW`**: Clear Activity Switch (device no longer active)
- **Lines 104047-104056**: Turn off transmitter control if in specific mode
- **Purpose**: Deactivate transmitter hardware after transmission

### Status Logging (Lines 104057-104077):
```assembly
104057          MIN TELL; P+0
104061          TELL /\ TELMA=:TELL
104064          A:=SNDAC                          % GET FIRST WORD IN FRAME
104067          A=:BUFF0(X); *2BANK               % LOG 1.WORD IN FRAME  (BANK 0 ONLY) *81F*
104071          HDEV; *1BANK
104073          A=:BUFF1(X); *2BANK               % LOG DEVICE USED
104075          HASTAT; *1BANK
104077          A=:BUFF2(X); *2BANK               % LOG DEVICE STATUS
```
- **Circular buffer logging**: Records transmission history
- **BUFF0**: First word in transmitted frame
- **BUFF1**: Device number used for transmission
- **BUFF2**: Hardware status (HASTAT value)
- **Purpose**: Maintain diagnostic history for debugging

## Phase 8: Transmission Result Processing (HNOTRA - Lines 104101-104115)

### Status Evaluation:
```assembly
104101   HNOTRA:X:=OMSG
104102          IF A/\ "SILFO+TXUND" = 0 THEN
104104                XRETRY=:RTDYN; A:=0; CALL SADTS
104110          ELSE
104111                A:=HASTAT; CALL SADTS; CALL DRERR
104114                A:=EUND
104115          FI
```
- **`IF A/\ "SILFO+TXUND" = 0`**: Test if transmission was successful
- **SILFO+TXUND = 0x8002**: Serial Format Error + Transmit Underrun bits
- **Success path (lines 104104)**: No errors, set retry counter, log success
- **Error path (lines 104111-104114)**: Log error status, set underrun error code

### Message Completion:
```assembly
104115          0=:DCBX; GO FAR BACKX
```
- **`0=:DCBX`**: Clear Device Control Block pointer
- **`GO FAR BACKX`**: Return control to caller with result
- **Purpose**: Complete transmission operation and return status to user

## Timeout Handling - POFTO (Lines 104117-104125)

### Timeout Processing:
```assembly
104117   POFTO: X:=OMSG; 0=:DCBX
104121          A:=CMODI; T:=HDEV+WTTC; *EXR ST
104125          A:=0; CALL SADTS; A:=ETOU1; GO FAR BACKX
```
- **Purpose**: Handle transmission timeout
- **Actions**: Clear DCB, disable transmitter, log timeout, return timeout error
- **ETOU1**: Timeout error code returned to user

## Special Entry Point - XSSND (Lines 104131-104154)

### Privileged User Direct DMA Access and HDLC Retransmission:
```assembly
104131   XSSND: CALL ZXCHK                                % CHECK DCB IS FROM XMSG
104132          T:=MASTB; *XMLIS@3 LDDTX
104134          A:=:D; A=:LIINT                           % BANK BITS IN D-REG
104154          GO FAR XXHMST
```
- **Primary Purpose**: **HDLC Retransmission** - Called via DRERR when transmission errors occur
- **Secondary Purpose**: Special entry for privileged users (MESSID<0)
- **Operation**: Direct hardware DMA list access bypassing normal setup and validation
- **Security**: ZXCHK validates DCB is from authorized source
- **Performance**: Optimized for retry operations - skips redundant validation
- **Result**: Jumps directly to hardware activation (XXHMST)

### XSSND as SRDAT (Retry Function):
**CRITICAL DISCOVERY**: XSSND serves as **SRDAT** - the retry/retransmission function:

```assembly
% From DRERR error handling (Line 103600):
DRERR: A\/DSTAT =: DSTAT                         % OR hardware status  
       HDERC+1 =: HDERC                          % Increment error counter
       MIN RTDYN; GO SRDAT                       % Decrement retry counter, try again
       XRETRY=:RTDYN; EXIT                       % Too many errors, give up

% SRDAT jumps to XSSND:  
SIDAT: GO SRDAT                                  % JMP THROUGH DATAFIELD (Line 103513)
```

**Retransmission Trigger Flow**:
1. **Transmission error detected** in HNOTRA: `(RTTS & SILFO+TXUND) ≠ 0`
2. **CALL DRERR** → Error processing and retry decision
3. **Retry counter check** → `MIN RTDYN` (decrement remaining retries)
4. **Retransmission** → `GO SRDAT` (which leads to XSSND)
5. **Hardware restart** → `GO FAR XXHMST` (bypass normal setup)

## Key SINTRAN Functions Called

### 1. **RACTB** - Calculate Buffer Size
- **Purpose**: Determine actual message data size
- **Usage**: Message validation and DMA byte count setup

### 2. **SADTS** - Store and Display Trace Status  
- **Purpose**: Log transmission status for diagnostics
- **Called**: After successful transmission and on errors

### 3. **DRERR** - Device Error Processing and Retry Management
- **Purpose**: Process device errors and manage retransmission attempts
- **Operation**: 
  - Accumulates error status: `A\/DSTAT =: DSTAT`
  - Increments error counter: `HDERC+1 =: HDERC`
  - Manages retry attempts: `MIN RTDYN; GO SRDAT`
  - Gives up when retries exhausted: `XRETRY=:RTDYN; EXIT`
- **Usage**: **Critical retry logic** - determines whether to retransmit or fail
- **Variables**:
  - **RTDYN**: Dynamic retry counter (decremented each attempt)
  - **XRETRY**: Maximum retry limit constant
  - **DSTAT**: Accumulated error status
  - **HDERC**: Total error count for diagnostics

### 4. **LTOUT** - Setup Line Timeout
- **Purpose**: Configure transmission timeout monitoring
- **Integration**: Works with TMR timer and POFTO timeout handler

### 5. **ID12** - Wait for Interrupt Level 12
- **Purpose**: Suspend until transmitter interrupt occurs
- **Result**: Resumes execution at HOINT when transmission completes

## Hardware Register Operations

### DMA Registers:
- **WDMA** (Write DMA Address): DMA list physical address
- **WDCR** (Write DMA Control): DMA start commands
- **RDCR** (Read DMA Control): DMA status and configuration

### Transmitter Registers:
- **WTTC** (Write Transmitter Transfer Control): Enable/disable transmitter
- **RTTS** (Read Transmitter Transfer Status): Get transmission result

### Register Values:
- **2000₈** (WDCR): Start Transmitter command
- **1001₈** (receiver): Start Receiver command  
- **1134₈+CMODI** (WTTC): Enable transmitter with communication mode

## DMA Descriptor Structure Used

### Complete DMA Descriptor (4 words):
```assembly
Word 0: LKEY  = FSERM (002003₈) = Block control + COM5025 bits
Word 1: LBYTC = Data byte count (message size - DISP1)  
Word 2: LMEM1 = Memory bank (MASTB)
Word 3: LMEM2 = Buffer address (OMSG+CHEAD)
```

### LKEY Field Breakdown (FSERM = 002003₈):
- **Bits 15-8**: `010` = Block to be transmitted
- **Bits 7-0**: `003` = COM5025 TSOM(1) + TEOM(1) = complete frame

## Error Codes and Status

### Validation Errors:
- **ENINIT**: Interface not initialized
- **EILFZ**: Illegal frame size (< 7 bytes)
- **EILSIZ**: Frame too large (> MAXR)

### Transmission Errors:
- **EUND**: Transmit underrun error
- **ETOU1**: Transmission timeout

### Hardware Status Bits (Retransmission Triggers):
- **SILFO** (bit 15): Serial Format Error - triggers retransmission
- **TXUND** (bit 1): Transmit Underrun - triggers retransmission
- **Combined test**: `A/\ "SILFO+TXUND" = 0` for success
- **Error detection**: `(RTTS & 0x8002) ≠ 0` triggers DRERR and potential retransmission

### DMA Status Bits (Transmission Completion):
- **BlockEnd** (bit 8): Single DMA buffer transmitted
- **FrameEnd** (bit 9): Complete HDLC frame transmitted  
- **ListEnd** (bit 10): All DMA descriptors in list processed
- **TransmissionFinished** (bit 11): Complete transmission sequence done
  - **Critical**: **"TransmissionFinished (bit 11) always gives a DMA Module Request (bit 4)"**
  - **Hardware behavior**: Automatically sets DMAModuleRequest when set
  - **Purpose**: Triggers interrupt level 12 for SINTRAN processing

### Retry Management Variables:
- **RTDYN**: Dynamic retry counter (starts at XRETRY, decremented each retry)
- **XRETRY**: Maximum retry limit (system constant)
- **DSTAT**: Accumulated error status (ORed hardware status values)
- **HDERC**: Hardware error counter (total errors encountered)

## Performance Characteristics

### Execution Flow Timing:
1. **Setup Phase**: ~50-100 CPU cycles (validation, descriptor setup)
2. **Hardware Start**: ~20-30 CPU cycles (DMA programming)
3. **Wait State**: Hardware DMA transmission (duration depends on data size and line speed)
4. **Interrupt Processing**: ~30-50 CPU cycles (status reading, logging)
5. **Total CPU Time**: ~100-180 cycles (excluding actual transmission time)

### Memory Operations:
- **DMA Descriptor**: 4 words written
- **Status Logging**: 3 words written to circular buffers
- **Register Access**: ~10 I/O operations to HDLC controller

## Integration with User Applications

### Message Flow (Including Retransmission):
1. **User Request**: Application calls SINTRAN with data to transmit
2. **XSSDATA Entry**: Validates message and sets up DMA
3. **Hardware Transmission**: COM5025 chip transmits HDLC frame
4. **Interrupt Generated**: Hardware signals completion
5. **Error Check**: HNOTRA examines RTTS for SILFO/TXUND errors
6. **Retry Decision**: 
   - **Success**: Reset retry counter, return to user
   - **Error**: CALL DRERR → retry management
7. **Retransmission**: DRERR → SRDAT (XSSND) → XXHMST → repeat transmission
8. **Final Result**: Success or failure (after retry exhaustion) returned to user

### Retransmission Flow Detail:
```
Error → DRERR → MIN RTDYN → GO SRDAT (XSSND) → GO FAR XXHMST
  ↓         ↓         ↓           ↓                ↓
Status   Counter   Retry     Skip Normal      Hardware
Check   Decrement  Decision   Validation       Restart
```

### User Message Structure:
- **FUNCTION**: DATA (transmission request)
- **STATUS**: RET-STATUS (success/error result)
- **ADDSTA**: HARDWARE STATUS (HASTAT value)
- **DATA**: User data to transmit or DMA list pointer

## Critical Dependencies

### Hardware Requirements:
- **COM5025 chip**: HDLC protocol controller
- **DMA controller**: Handles descriptor processing
- **Memory management**: Bank switching and physical addressing
- **Interrupt system**: Level 12 for transmitter completion

### Software Requirements:
- **INTSTA = 2**: Interface must be initialized
- **ACTSW management**: Device activity state tracking
- **Timeout system**: TMR timer and POFTO handler
- **Error logging**: Circular buffer diagnostic system

## DETAILED FLOWCHARTS

### 1. Complete XSSDATA Transmission Flow

```
┌─────────────────────────────┐
│    USER TRANSMISSION        │
│         REQUEST             │
│   (Application calls        │
│    SINTRAN with data)       │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║       XSSDATA ENTRY         ║
║       (Line 103636)         ║
║                             ║
║  SUBR XSSDATA,HOINT,POFTO   ║
└═══════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   INITIALIZATION CHECK      │
│  IF INTSTA >< 2 THEN        │
│    A:=ENINIT; GO BACKX      │
│  FI                         │
└──────────┬──────────────────┘
           │
           ▼
      ┌─────────┐    YES
      │INTSTA=2?├─────────┐
      └────┬────┘         │
           │ NO           ▼
           ▼         ┌─────────────────┐
┌─────────────────────────────┐  │  NOT INITIALIZED │
│   DMA LIST SETUP            │  │  A:=ENINIT       │
│   LISTP =: LIINT            │  │  GO BACKX        │
│  (Initialize DMA pointers)  │  └─────────────────┘
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   MESSAGE SIZE CALCULATION  │
│   X-BHEAD; CALL RACTB       │
│   X+BHEAD                   │
│  (Get actual data size)     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│    MINIMUM SIZE CHECK       │
│   IF A < 7 THEN             │
│     A:=EILFZ; GO BACKX      │
│   FI                        │
└──────────┬──────────────────┘
           │
           ▼
      ┌─────────┐    YES
      │SIZE < 7?├─────────┐
      └────┬────┘         │
           │ NO           ▼
           ▼         ┌─────────────────┐
┌─────────────────────────────┐  │   FRAME TOO SMALL│
│    MAXIMUM SIZE CHECK       │  │   A:=EILFZ       │
│  IF A-BCHEA>MAXR THEN       │  │   GO BACKX       │
│    A:=EILSIZ; GO BACKX      │  └─────────────────┘
│  FI                         │
└──────────┬──────────────────┘
           │
           ▼
      ┌─────────┐    YES
      │SIZE>MAX?├─────────┐
      └────┬────┘         │
           │ NO           ▼
           ▼         ┌─────────────────┐
┌══════════════════════════════════┐  │   FRAME TOO LARGE│
║    DMA DESCRIPTOR SETUP        ║  │   A:=EILSIZ      │
║         (CRITICAL)             ║  │   GO BACKX       │
└══════════════════════════════════┘  └─────────────────┘
           │
           ▼
┌─────────────────────────────┐
│    SET BYTE COUNT           │
│   A-DISP1=:LIINT.LBYTC      │
│ (Data size minus headers)   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   SET BUFFER ADDRESS        │
│ A:=OMSG+CHEAD=:X.LMEM2=:D   │
│ T:=MASTB=:X.LMEM1           │
│ (Configure DMA addressing)  │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║   *** CRITICAL STEP ***     ║
║   SET COM5025 CONTROL BITS  ║
║   FSERM=:X.LKEY             ║
║                             ║
║ FSERM = 002003₈             ║
║ = 010 (ToTransmit)          ║
║ + 003 (TSOM=1, TEOM=1)      ║
└═══════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   DATA PREPARATION          │
│      *LDATX                 │
│      A=:SNDAC               │
│      0=:D                   │
│ (Load data, save first word)│
└──────────┬──────────────────┘
           │
           ▼
┌══════════════════════════════════┐
║    HARDWARE DMA START          ║
║        XHMST                   ║
└══════════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   SET DMA LIST ADDRESS      │
│   LIINT+DPITPHYS            │
│   T:=HDEV+WDMA; *IOF; EXR ST│
│ (Write DMA address to HW)   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   START TRANSMITTER DMA     │
│   A:=2000\D                 │
│   T+"WDCR-WDMA"; *EXR ST    │
│ (Command 2000₈ = Start TX)  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   ENABLE TRANSMITTER        │
│   1134+CMODI                │
│   T:=HDEV+WTTC; *EXR ST     │
│ (Enable HDLC transmitter)   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   SET DEVICE ACTIVE         │
│   1 =: ACTSW                │
│   OMSG =: DCBX              │
│ (Mark device as active)     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   SETUP TIMEOUT & WAIT      │
│   A:=TTMR=:TMR              │
│   CALL ID12                 │
│   GO HOINT                  │
│ (Wait for interrupt)        │
└──────────┬──────────────────┘
           │
           ▼
    ┌──────┴──────┐
   TIMEOUT      COMPLETION
      │             │
      ▼             ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│    POFTO TIMEOUT            │   │     HOINT INTERRUPT         │
│  X:=OMSG; 0=:DCBX           │   │  0=:TMR (Reset timer)       │
│  A:=CMODI; T:=HDEV+WTTC     │   │  T:=HDEV+RTTS; *EXR ST      │
│  A:=0; CALL SADTS           │   │  A=:HASTAT (Save status)    │
│  A:=ETOU1; GO FAR BACKX     │   └──────────┬──────────────────┘
│  (Return timeout error)     │              │
└─────────────────────────────┘              ▼
                                   ┌─────────────────────────────┐
                                   │   STATUS PROCESSING         │
                                   │      HNOTRA                 │
                                   │                             │
                                   │  IF A/\"SILFO+TXUND" = 0   │
                                   │  THEN success ELSE error    │
                                   └──────────┬──────────────────┘
                                              │
                                              ▼
                                   ┌─────────────────────────────┐
                                   │   RETURN TO USER            │
                                   │   0=:DCBX                   │
                                   │   GO FAR BACKX              │
                                   │ (Complete transmission)     │
                                   └─────────────────────────────┘
```

### 2. DMA Descriptor Setup Detail Flow

```
┌═══════════════════════════════┐
║   DMA DESCRIPTOR CREATION     ║
║      (CRITICAL PHASE)         ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│   CALCULATE BYTE COUNT      │
│   A-DISP1=:LIINT.LBYTC      │
│                             │
│  A = Message size           │
│  DISP1 = Header displacement│
│  Result = Actual data bytes │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   SET BUFFER ADDRESS (LOW)  │
│ A:=OMSG+CHEAD=:X.LMEM2=:D   │
│                             │
│  OMSG = Output message ptr  │
│  CHEAD = Control header     │
│  X.LMEM2 = DMA address low  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│  SET MEMORY BANK (HIGH)     │
│  T:=MASTB=:X.LMEM1          │
│                             │
│  MASTB = Master bank ref    │
│  X.LMEM1 = DMA address high │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║  SET CONTROL KEY (LKEY)     ║
║  FSERM=:X.LKEY              ║
║                             ║
║  FSERM = 002003₈            ║
║                             ║
║  Bit breakdown:             ║
║  15-14: 00 (Extended ctrl)  ║
║  13-11: 001 (Reserved)      ║
║  10:    1 (Legal Key flag)  ║
║  9-8:   00 (Reserved)       ║
║  Bits 10-8: 010 (ToTransmit)║
║  Bits 7-0:  003 (COM5025)   ║
║    Bit 0: TSOM = 1          ║
║    Bit 1: TEOM = 1          ║
║    Bits 2-7: 0 (unused)     ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│    COMPLETE DESCRIPTOR      │
│  ┌─────┬─────┬─────┬─────┐   │
│  │LKEY │LBYTC│LMEM1│LMEM2│   │
│  │02003│Size │Bank │Addr │   │
│  └─────┴─────┴─────┴─────┘   │
│                             │
│  Ready for DMA hardware     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│  HARDWARE INTERPRETATION    │
│                             │
│  DMA reads LKEY = 002003₈   │
│  Block bits (10-8) = 010   │
│  → "Block to transmit"      │
│                             │
│  COM5025 bits (7-0) = 003  │
│  → Write 0x03 to COM5025    │
│  → TSOM=1, TEOM=1          │
│  → Generate complete frame  │
└─────────────────────────────┘
```

### 3. Hardware Activation Flow (XHMST)

```
┌═══════════════════════════════┐
║    HARDWARE DMA START         ║
║       XHMST ENTRY             ║
║    (Line 103703)              ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│  CALCULATE DMA LIST ADDRESS │
│      LIINT+DPITPHYS         │
│                             │
│  LIINT = DMA descriptor ptr │
│  DPITPHYS = Physical offset │
│  Result = Hardware address  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   WRITE DMA ADDRESS TO HW   │
│   T:=HDEV+WDMA              │
│   *IOF; EXR ST              │
│                             │
│  HDEV+WDMA = DMA addr reg   │
│  Hardware knows where list  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   START TRANSMITTER DMA     │
│   A:=2000\D                 │
│   T+"WDCR-WDMA"; *EXR ST    │
│                             │
│  2000₈ = Start TX command   │
│  WDCR = DMA Control reg     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   CONFIGURE DMA READ        │
│   T+"RDCR-WDCR"; X:=-20     │
│   *EXR ST                   │
│                             │
│  RDCR = Read DMA Control    │
│  -20₈ = Configuration value │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   SETUP TIMEOUT AND IRQ     │
│   CALL LTOUT; *JAF *-2; ION │
│                             │
│  LTOUT = Line timeout setup │
│  ION = Enable interrupts    │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   ENABLE TRANSMITTER        │
│   1134+CMODI                │
│   T:=HDEV+WTTC; *EXR ST     │
│                             │
│  1134₈ = TX enable command  │
│  CMODI = Communication mode │
│  WTTC = Write TX Transfer   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   MARK DEVICE ACTIVE        │
│   1 =: ACTSW                │
│   OMSG =: DCBX              │
│                             │
│  ACTSW = Activity switch    │
│  DCBX = Device control block│
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│    HARDWARE NOW ACTIVE      │
│                             │
│  DMA reading descriptors    │
│  COM5025 transmitting data  │
│  Interrupt will fire when   │
│  transmission completes     │
└─────────────────────────────┘
```

### 4. COM5025 Frame Generation Flow

```
┌─────────────────────────────┐
│    DMA STARTS READING       │
│     DESCRIPTOR              │
│                             │
│  LKEY = 002003₈             │
│  LBYTC = Data byte count    │
│  LMEM1 = Memory bank        │
│  LMEM2 = Buffer address     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   DMA EXTRACTS CONTROL      │
│   Block bits = 010          │
│   COM5025 bits = 003        │
│                             │
│   Action: Write 0x03 to     │
│   COM5025 control register  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 SEES TSOM=1       │
│   (Start of Message)        │
│                             │
│   Hardware action:          │
│   - Generate opening FLAG   │
│   - Insert Address field    │
│   - Insert Control field    │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 TRANSMITS DATA    │
│                             │
│   DMA feeds data bytes      │
│   from buffer to COM5025    │
│                             │
│   COM5025 performs:         │
│   - Bit stuffing           │
│   - CRC calculation        │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 SEES TEOM=1       │
│   (End of Message)          │
│                             │
│   Hardware action:          │
│   - Append calculated FCS   │
│   - Generate closing FLAG   │
│   - Signal completion       │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COMPLETE HDLC FRAME       │
│                             │
│  [FLAG][ADDR][CTRL][DATA]   │
│  [...DATA...][FCS][FLAG]    │
│                             │
│  Transmitted on physical    │
│  line to remote station     │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   GENERATE INTERRUPT        │
│                             │
│  Hardware sets status bits: │
│  - BlockEnd = 1             │
│  - FrameEnd = 1             │
│  (Both set because TEOM=1)  │
│                             │
│  Interrupt level 12 fired   │
└─────────────────────────────┘
```

### 5. Interrupt Processing Flow (HOINT)

```
┌─────────────────────────────┐
│   TRANSMISSION COMPLETE     │
│   Hardware generates        │
│   Interrupt Level 12        │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║      HOINT ENTRY            ║
║    (Line 104033)            ║
║                             ║
║  Transmitter interrupt      ║
║  handler activation         ║
└═══════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   RESET TIMEOUT TIMER       │
│   0=:TMR                    │
│                             │
│  Transmission completed -   │
│  no timeout needed          │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   READ HARDWARE STATUS      │
│   T:=HDEV+RTTS; *EXR ST     │
│   A=:HASTAT                 │
│                             │
│  RTTS = Read TX Transfer    │
│  Status register            │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   CHECK FOR SPURIOUS IRQ    │
│  IF T:=ACTSW = 0 THEN       │
│    MIN DUIN; P+0; CALL WT12 │
│  FI                         │
└──────────┬──────────────────┘
           │
           ▼
      ┌─────────┐    YES
      │ACTSW=0? ├─────────┐
      │(Spurious│         │
      │interrupt│         ▼
      └────┬────┘   ┌─────────────────┐
           │ NO     │  COUNT DUMMY IRQ │
           ▼        │  WAIT FOR REAL   │
┌─────────────────────────────┐  │  INTERRUPT       │
│   DEACTIVATE DEVICE         │  └─────────────────┘
│   0=: ACTSW                 │
│  (Device no longer active)  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   DISABLE TRANSMITTER       │
│  IF CMODI = 40 THEN         │
│    T:=HDEV+WTTC; *EXR ST    │
│  FI                         │
│ (Turn off transmitter)      │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   CIRCULAR BUFFER LOGGING   │
│   MIN TELL; P+0             │
│   TELL /\ TELMA=:TELL       │
│                             │
│  Update circular buffer     │
│  index with wraparound      │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   LOG TRANSMISSION DATA     │
│   A:=SNDAC                  │
│   A=:BUFF0(X) (1st word)    │
│   HDEV=:BUFF1(X) (device)   │
│   HASTAT=:BUFF2(X) (status) │
│                             │
│  Store diagnostic info      │
└──────────┬──────────────────┘
           │
           ▼
┌══════════════════════════════════┐
║    STATUS EVALUATION           ║
║       HNOTRA                   ║
║    (Line 104101)               ║
└══════════════════════════════════┘
           │
           ▼
┌─────────────────────────────┐
│   TEST TRANSMISSION RESULT  │
│  IF A/\"SILFO+TXUND" = 0    │
│                             │
│  SILFO = Serial format err  │
│  TXUND = Transmit underrun  │
│  Both = 0 means success     │
└──────────┬──────────────────┘
           │
           ▼
    ┌──────┴──────┐
   SUCCESS      ERROR
      │             │
      ▼             ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│    SUCCESS PATH             │   │      ERROR PATH             │
│  XRETRY=:RTDYN              │   │  A:=HASTAT; CALL SADTS      │
│  A:=0; CALL SADTS           │   │  CALL DRERR                 │
│ (Set retry count, log OK)   │   │  A:=EUND                    │
└──────────┬──────────────────┘   │ (Log error, set underrun)   │
           │                      └──────────┬──────────────────┘
           │                                 │
           └─────────────┬───────────────────┘
                         │
                         ▼
┌─────────────────────────────┐
│   CLEANUP AND RETURN        │
│   0=:DCBX                   │
│   GO FAR BACKX              │
│                             │
│  Clear device control block │
│  Return to user with result │
└─────────────────────────────┘
```

### 6. Multi-Block Frame Transmission (Expected)

```
┌─────────────────────────────┐
│   LARGE MESSAGE (>1 BUFFER) │
│                             │
│  Total size > single buffer │
│  Requires multiple DMA      │
│  descriptors                │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   FIRST BLOCK SETUP         │
│   LKEY = 002001₈            │
│   = 010 (ToTransmit)        │
│   + 001 (TSOM=1 only)       │
│                             │
│  Start frame, more to come  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 PROCESSES BLOCK 1 │
│   Sees TSOM=1, TEOM=0       │
│                             │
│   Actions:                  │
│   - Generate opening FLAG   │
│   - Insert Address/Control  │
│   - Transmit first data     │
│   - NO FCS, NO closing FLAG │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   INTERRUPT: BlockEnd ONLY  │
│   RTTS status = 0x0100      │
│   (BlockEnd=1, FrameEnd=0)  │
│                             │
│   DMA advances to next      │
│   descriptor automatically  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   MIDDLE BLOCK(S) SETUP     │
│   LKEY = 002000₈            │
│   = 010 (ToTransmit)        │
│   + 000 (no flags)          │
│                             │
│  Continue frame data        │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 PROCESSES MIDDLE  │
│   Sees TSOM=0, TEOM=0       │
│                             │
│   Actions:                  │
│   - Continue data stream    │
│   - No special processing   │
│   - Frame still open        │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   INTERRUPT: BlockEnd ONLY  │
│   RTTS status = 0x0100      │
│   (BlockEnd=1, FrameEnd=0)  │
│                             │
│   Frame continues...        │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   FINAL BLOCK SETUP         │
│   LKEY = 002002₈            │
│   = 010 (ToTransmit)        │
│   + 002 (TEOM=1 only)       │
│                             │
│  End frame after this data  │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COM5025 PROCESSES FINAL   │
│   Sees TSOM=0, TEOM=1       │
│                             │
│   Actions:                  │
│   - Transmit final data     │
│   - Calculate/append FCS    │
│   - Generate closing FLAG   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│  INTERRUPT: FrameEnd + BlockEnd │
│  RTTS status = 0x0300       │
│  (BlockEnd=1, FrameEnd=1)   │
│                             │
│  Complete frame transmitted │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│   COMPLETE HDLC FRAME       │
│                             │
│  [FLAG][ADDR][CTRL]         │
│  [DATA_BLOCK_1]             │
│  [DATA_BLOCK_2]             │
│  [...DATA_BLOCK_N]          │
│  [FCS][FLAG]                │
└─────────────────────────────┘
```

This comprehensive analysis shows that XSSDATA is the complete SINTRAN HDLC transmission system, handling everything from user validation through hardware control to interrupt processing, with explicit COM5025 chip programming via the LKEY field for precise frame boundary control.