# IP-P2-SCSI-DRIV.NPL - Comprehensive Analysis

**File:** `IP-P2-SCSI-DRIV.NPL`

**Purpose:** SCSI Driver for ND-100 with NCR 5386 Controller

---

## Table of Contents

1. [Symbol Definitions](#symbol-definitions)
2. [Entry Points](#entry-points)
3. [Main APIs/Subroutines](#main-apis-subroutines)
4. [Interrupt Flow (SCINT)](#interrupt-flow-scint)
5. [Phase Handlers](#phase-handlers)
6. [Support Routines](#support-routines)
7. [Error Handling](#error-handling)

---

## Symbol Definitions

### Error Conditions (Lines 17-19)
```
7SIDE = 5    % Initiator Detected Error
7SMRJ = 7    % Message Reject
7SMPE = 11   % Message Parity Error
```

### Hardware Register Addresses (Lines 25-69)

#### Controller Registers
```
RLMAR = 00   % Read Memory Address Register bits 14-0
WLMAR = 01   % Write Memory Address Register bits 14-0
REDAT = 02   % Read Data
WRDAT = 03   % Write Data
RSTAU = 04   % Read Status
WCONT = 05   % Write Control
RHMAR = 06   % Read Memory Address Register bits 23-15
WHMAR = 07   % Write Memory Address Register bits 23-15
```

#### NCR 5386 Registers
```
RNDAT = 40   % Read NCR Data Register
WNDAT = 41   % Write NCR Data Register
RNCOM = 42   % Read NCR Command Register
WNCOM = 43   % Write NCR Command Register
RNCNT = 44   % Read NCR Control Register
WNCNT = 45   % Write NCR Control Register
RDESI = 46   % Read Destination ID Register
WDESI = 47   % Write Destination ID Register
RAUXS = 50   % Read Auxiliary Status
WAUXS = 51   % Write Auxiliary Status
ROIDN = 52   % Read Own ID Number
RITRG = 54   % Read Interrupt Register
RSOUI = 56   % Read Source ID
RDIST = 62   % Read Diagnostic Status
RTCM  = 70   % Read Transfer Counter Most significant
WTCM  = 71   % Write Transfer Counter Most significant
RTC2  = 72   % Read Transfer Counter 2nd byte
WTC2  = 73   % Write Transfer Counter 2nd byte
RTCL  = 74   % Read Transfer Counter Least significant
WTCL  = 75   % Write Transfer Counter Least significant
```

#### Status Bits (Lines 30-32, 46)
```
CTBUS = 02   % Controller Busy
NCRIT = 11   % Interrupt from NCR
SCREQ = 15   % REQ from SCSI bus
DARGF = 07   % Data Register Full
ENDMA = 05   % Enable DMA
SLFCO = 07   % Selftest Complete NCR 5386
```

#### NCR Interrupt Register Bits (Lines 51-59)
```
ILCOM = 16   % Illegal Command
RECON = 14   % Reconnect
CONEC = 13   % Connect
DISCO = 12   % Disconnect
BUSSI = 11   % Bus Service Interrupt
FUCOM = 10   % Function Complete
PARIT = 06   % Parity Error
PAUSE = 02   % Paused
TCZRO = 01   % Transfer Count Zero
```

### Status Flags (Lines 71-81)
```
6SFUN = 17   % Function Started
6SIDE = 16   % Send Identify
6SRST = 15   % SCSI Bus Reset
6SMSO = 12   % Message Out Pending
6SRFD = 11   % Return on Command Accepted (first disconnect)
6SDIS = 10   % Disconnect Message Received
6SCCO = 7    % Command Complete Message Received
6SMSI = 6    % Message Received
6SARB = 5    % Arbitration Phase
6SSBT = 4    % Single Byte Transfer
6SCRP = 3    % Connect/Reconnect Phase
```

### Flag Masks (Lines 83-85)
```
CNCLR = 177000   % Mask for flags cleared on connect path
DCCLR = 136400   % Mask for flags cleared on disconnect path
NPCLR = 137000   % Mask for flags cleared on new phase
```

---

## Entry Points

### SCLLD - Main Entry Point (Lines 93-117)

**Line:** 93-117

**Call Sequence:**
```
JPL I (SCLLD
JMP BUSY
JMP ERROR
JMP FIN
```

**Entry Parameters:**
- `A` = Function Type
- `X` = Unit Datafield

**Function Types:**
- **Type 0-2** (Lines 93-99): Normal SCSI operation
  - Save control and return address to `X.SUCON`, `X.SULRG`
  - Clear trigger: `0 = X.SUTRG`
  - Check NCR interface OK (`NCROK`)
  - Call `INITO` to initialize operation
  - If bus free, call `SELEC` to start arbitration

- **Type 3** (Lines 101-105): Enable Timer
  - Save control and return address
  - Set initial status: `"0" BONE 6SFUN = X.SUTHS`
  - Call `ENTIM` to enable timer

- **Type 4** (Lines 106-113): Reset Request
  - Check if reset already in progress (`NCROK NBIT 8SRST`)
  - Save registers (`SCRXR`, `SCRLR`)
  - Perform reset via `SCRST`
  - Return "already in progress" if reset active

**Exit:**
- Line 116: `GO SCWTI` - Busy return

---

## Main APIs/Subroutines

### SCINT - Interrupt Handler (Lines 123-189)

**Line:** 123-189

**Purpose:** Main interrupt service routine for SCSI controller

**Detailed Flow:**

#### Step 1: Read Device Status (Line 123)
```
Line 123: T := HDEV+RSTAU; *IOXT    % Read device status
Line 124: IF X := 64/\A><0 THEN     % Check status bits
```

#### Step 2: Check Controller Busy (Lines 125-126)
```
Line 126: IF A BIT 2 GO SCWTI       % Controller busy - exit
```

#### Step 3: Check SCSI Bus Reset (Lines 126-128)
```
Line 126: IF A =: SCSSR BIT 5 THEN
Line 127:    T := SBRST; GO FAR SCDIS   % SCSI bus reset received
```

#### Step 4: Check Initiator Detected Error (Lines 129-131)
```
Line 129: IF A BIT 4 THEN
Line 130:    CALL SCIDE; A := SCSSR   % Initiator detected error
```

#### Step 5: Check NCR Interrupt (Lines 133-138)
```
Line 133: IF A =: SCSSR BIT 11 THEN        % Interrupt from NCR
Line 134:    "0"; T := HDEV+WCONT; *IOXT   % **CLEAR TO MEMORY** (disable interrupt)
Line 135:    T+"RAUXS-WCONT"; *IOXT        % Read auxiliary status
Line 136:    AD SHZ -10                     % Shift into accumulator
Line 137:    T+"RITRG-RAUXS"; *IOXT        % Read interrupt register
Line 138:    AD SHZ -10                     % Shift into accumulator
Line 139:    A := D =: SCNIS; 0 =: SCCCW   % Save new status
```

#### Step 6: Expected Interrupt Check (Lines 141-143)
```
Line 141: IF A/\177500 = SCEIM THEN     % Expected interrupt
Line 142:    CALL SCISR                   % Call interrupt service routine
```

#### Step 7: Unexpected Interrupt Handling (Lines 144-184)

**Arbitration Timeout** (Lines 145-158):
```
Line 144: A SHZ -10; X := BUSFL
Line 145: IF A=4 AND X><0 THEN
Line 146:    IF X := SCCSU><0 THEN
Line 147:       CALL DCTHR                % Disconnect logical thread
Line 149:    IF BUSFL BIT 6SARB THEN      % Arbitration timeout
Line 150:       MIN SCWAQ.SUTMR; X := 0   % Count retries
Line 151:       IF X><0 THEN
Line 152:          0 =: SCTST; CALL RSTMR % Reset timer
Line 153:          CALL RFWAQ              % Remove from wait queue
Line 154:          T := NESER; CALL TEROP  % Terminate operation
```

**Arbitration Won** (Lines 160-166):
```
Line 160: IF A=1 AND X BIT 6SARB THEN
Line 161:    CALL RFWAQ                    % Remove from arbitration queue
Line 162:    CNCLR/\X.SUTHS BONE 6SCRP     % Fetch flags, set connect phase
Line 163:    A =: BUSFL; CALL CNTHR        % Connect physical path
Line 164:    0 =: SCTST; CALL ENTIM        % Start timer
Line 165:    RBSIR =: SCNIH                % Allow bus service interrupts
```

**Reconnect** (Lines 167-169):
```
Line 167: IF A=20 AND X BZERO 6SARB BZERO 6SRST=0 THEN
Line 168:    X BONE 6SCRP =: BUSFL         % Indicate reconnect
Line 169:    RBSIR =: SCNIH                % Allow bus service interrupts
```

**Selftest Finished** (Lines 171-172):
```
Line 171: IF A=1 AND T := NCROK BIT 8SDIA THEN
Line 172:    GO FAR STFIN                  % Selftest finished
```

**Parity Error** (Lines 174-181):
```
Line 174: IF A=SCEIM SHZ -10 AND D BIT PARIT AND 11/\BUSFL=1 THEN
Line 175:    MIN SCNPE; 0/\0               % Count parity errors
Line 176:    IF 17/\BUSFL=7 THEN
Line 177:       CALL SCMPE                 % Message parity error
Line 179:       CALL SCIDE                 % Initiator detected error
Line 181:    CALL SCISR
```

**Illegal Interrupt** (Lines 182-184):
```
Line 182: ELSE
Line 183:    T := NCRER; GO FAR SCDIS      % Illegal interrupt
```

#### Step 8: Re-enable Interrupt (Line 187)
```
Line 187: 5\/SCCCW; T := HDEV+WCONT; *IOXT   % **ACTIVATE + ENABLE INTERRUPT**
Line 188: GO SCWTI                            % Exit interrupt handler
```

**Key Cross-References:**
- Calls: `SCIDE`, `SCISR`, `DCTHR`, `RSTMR`, `RFWAQ`, `TEROP`, `CNTHR`, `ENTIM`, `SCMPE`
- Branches: `SCDIS`, `BUSFP`, `STFIN`, `SCWTI`

---

### SCTIO - Timeout Routine (Lines 195-273)

**Line:** 195-273

**Purpose:** Handles timeout conditions for SCSI operations

**Entry Parameters:**
- Timer expired (TMR = 0)

**Key Operations:**

#### Direct Timeout Enable (Lines 195-210)
```
Line 195: IF NCROK BIT 8SCLR THEN
Line 196:    SCRSQ =: SCRSL; A := B =: SCRSQ    % Enable direct timeout
Line 197:    IF X := SCCSU><0 THEN
Line 198:       T := X.SUTRG; 0 =: X.SUTRG; CALL TEROP   % Disconnect current
Line 199:       0 =: SCCSU; "0" BONE 6SRST =: BUSFL      % Allow reconnect
```

#### Cleanup After Reset (Lines 203-209)
```
Line 203: DO WHILE X := X.SULINK><0
Line 205:    IF X.SUTHS BIT 6SRST THEN
Line 206:       CALL TEROP; GO FAR EXDRI    % Terminate operation
```

#### Restart Interface (Lines 211-216)
```
Line 211: IF NCROK><0 AND A BZERO 8SRST=0 THEN
Line 212:    0 =: NCROK =: SCTST; CALL RSTMR    % Restart timer
Line 213:    IF BUSFL BZERO 6SRST=0 THEN
Line 214:       CALL SELEC                      % Restart interface
```

#### Reset Return (Lines 217-225)
```
Line 217: IF SCRCO><0 AND NCROK NBIT 8SDIA AND NBIT 8SRIN THEN
Line 218:    0 =: SCRCO; X := SCRXR             % Return from bus reset
Line 219:    IF NCROK BIT 8SERR THEN
Line 220:       T := NCRST; SCRLR =: P          % Error return
Line 222:       T := 0; SCRLR+2 =: P            % OK return
```

#### Timeout Processing (Lines 226-270)

**Reset Timeout** (Lines 228-240):
```
Line 228: IF A BIT 8SRIN GO FAR SCRST    % Timeout on reset receive
Line 229: IF A BIT 8SERR THEN
Line 230:    IF X := SCTQP=0 THEN
Line 231:       IF X := SCWAQ><0 THEN CALL RFWAQ FI
Line 234:    IF X><0 THEN
Line 235:       SCRSQ =: SCRSL; A := B =: SCRSQ    % Direct timeout
Line 235:       T := NCRST; CALL TEROP             % Terminate operation
```

**Arbitration Timeout** (Lines 242-256):
```
Line 243: IF BUSFL BIT 6SARB THEN
Line 244:    MIN SCWAQ.SUTMR; X := 0      % Arbitration timeout
Line 245:    IF X><0 THEN
Line 246:       CALL RFWAQ; X =: SCCSU    % Terminate
Line 247:       IF X.SUTRG=CMTMO THEN
Line 248:          T := CMTMA             % Timeout, not abort
Line 250:          T := ASOTM             % Select timeout
Line 255:    T := ASOTM; GO FAR SCDIS     % Reset interface
```

**Connection Timeout** (Lines 258-268):
```
Line 258: IF X := SCTQP><0 THEN
Line 259:    IF X.SUTMR=0 THEN
Line 260:       IF SCCSU=X THEN
Line 261:          T := CNTMO; GO FAR SCDIS      % Connection timeout
Line 263:       CMTMO =: X.SUTRG                 % Operation timeout
Line 264:       CALL DITIM; ABFLG =: X.SUTHS     % Abort operation
Line 265:       177760/\X.SUCON\/2 =: X.SUCON    % New timeout
Line 266:       SCWAQ =: X.SULINK; X =: SCWAQ    % Insert in arbitration queue
Line 267:       -1 =: X.SUTMR; CALL SELEC        % Send abort
```

**Exit:**
- Line 272: `GO SCWTI` - Return to wait

---

### SELEC - Activate Controller (Lines 376-398)

**Line:** 376-398

**Purpose:** Start arbitration and selection on SCSI bus

**Entry Parameters:**
- `SCWAQ` = Arbitration wait queue

**Detailed Flow:**

#### Save Registers (Line 376)
```
Line 376: TAD =: SVTAD; X =: SAVXR
```

#### Disable Interrupt (Line 377)
```
Line 377: -1 =: SCEIM                    % Disable interrupt
```

#### Check Wait Queue (Lines 378-394)
```
Line 378: IF X := SCWAQ><0 THEN
Line 379:    "0"; T := HDEV+WCONT; *IOXT        % **CLEAR TO MEMORY**
Line 380:    A BONE 6SARB =: BUSFL              % Indicate arbitration
Line 381:    X.SUDLU SHZ -14                    % SCSI ident
Line 382:    T+"WDESI-WCONT"; *IOXT             % Write destination ID
Line 383:    WATFS; T+"WTCM-WDESI"; *IOXT       % Set waiting time (transfer counter MSB)
Line 384:    AD SHZ 10; T+"WTC2-WTCM"; *IOXT    % Transfer counter byte 2
Line 385:    AD SHZ 10; T+"WTCL-WTC2"; *IOXT    % Transfer counter LSB
Line 386:    10; IF X := X.SUCON BIT 4SINA THEN 11 FI
Line 387:    T+"WNCOM-WTCL"; *IOXT              % **SELECT COMMAND TO NCR**
Line 388:    IF X := SCTST>>2 THEN
Line 389:       TMR =: X.SUTMR                  % Save current timer
Line 391:    1 =: SCTST; -5 =: TMR              % Enable select timeout
Line 393:    0 =: BUSFL                         % Bus free
```

#### Enable Interrupt (Line 395)
```
Line 395: 5; T := HDEV+WCONT; *IOXT      % **ENABLE INTERRUPT**
```

#### Restore and Exit (Lines 396-397)
```
Line 396: X := SAVXR; SVTAD
Line 397: EXIT
```

**Commands Written to WCONT:**
1. Line 379: Write `0` - Clear to memory (disable interrupt)
2. Line 395: Write `5` - Activate + enable interrupt

**Cross-References:**
- Called by: `SCLLD` (line 99), `SCDIS` (line 289), timeout routine (lines 214, 267)
- Uses: `SCWAQ` (wait queue), `BUSFL` (bus flags)

---

### NEWPH - New Phase Handler (Lines 659-698)

**Line:** 659-698

**Purpose:** Handle SCSI bus phase transitions and update data pointers

**Entry Parameters:**
- Called via interrupt vector when bus phase changes
- `BUSFL` = Current bus flags
- `SCNIS` = New interrupt status

**Detailed Flow:**

#### Save Old Status (Line 659)
```
Line 659: NEWPH: T := BUSFL               % Old status
```

#### Check if Old Phase Was Data (Lines 660-691)
```
Line 660: IF 36/\T=0 THEN                % Old phase was data
Line 661:    TAD =: SAVRG; K := 1
Line 662:    IF D NBIT TCZRO THEN
```

#### Read Transfer Counter (Lines 663-671)
```
Line 664:    T := HDEV+RTCL; *IOXT       % Read LSB of bytecount
Line 665:    AD SHZ -10
Line 666:    T+"RTC2-RTCL"; *IOXT        % Read byte 2 of bytecount
Line 667:    AD SHZ -10
Line 668:    T+"RTCM-RTC2"; *IOXT        % Read MSB of bytecount
Line 670:    A := 0; D := 0              % Zero bytecount (if TCZRO set)
```

#### Calculate Bytes Transferred (Lines 672-675)
```
Line 672: T := SCCB1; X := SCCB2; AD =: SCCBC    % Save old, store new bytecount
Line 673: *RADD SD CM2 DX; RADD SA CM1 ADC DT    % Get bytes transferred
Line 674: SCCDP; *RADD SX DD; RADD ST ADC DA     % Calculate new memory address
Line 675: AD =: SCCDP SHZ -1; X := A             % Expected MAR
```

#### Handle Odd Byte Transfer (Lines 676-683)
```
Line 676: IF M THEN                        % Stopped on odd byte
Line 677:    IF BUSFL BIT 0 THEN           % Odd byte to ND-100 memory
Line 678:       150; T := HDEV+WCONT; *IOXT     % Set test mode
Line 679:       T+"RLMAR-WCONT"; *IOXT          % Force last byte to memory
Line 680:       "0"; T+"WCONT-RLMAR"; *IOXT     % Clear test mode
Line 682:    D+1; X := X+C                 % Adjust expected MAR
```

#### Verify Memory Address Register (Lines 684-689)
```
Line 684: IF K THEN
Line 685:    T := HDEV+RLMAR; *IOXT        % Read least significant MAR
Line 686:    IF A><D GO MARER
Line 687:    T+"RHMAR-RLMAR"; *IOXT        % Read most significant MAR
Line 688:    IF A><D GO MARER
Line 697: MARER: T := ILMAR; GO FAR SCDIS  % MAR not as expected
```

#### Extract New Phase and Dispatch (Lines 692-695)
```
Line 692: AD SHZ 32 SHZ -15                % Get new phase (bits from interrupt status)
Line 693: X := NPCLR/\T\/A =: BUSFL        % Set new status
Line 695: GOSW DAOPH,DAIPH,COMPH,STAPH,FAR ILOPH,FAR ILIPH,FAR MSOPH,FAR MSIPH
```

**Phase Dispatch Table:**
- Phase 0: DAOPH (Data Out)
- Phase 1: DAIPH (Data In)
- Phase 2: COMPH (Command)
- Phase 3: STAPH (Status)
- Phase 4: ILOPH (Illegal Out)
- Phase 5: ILIPH (Illegal In)
- Phase 6: MSOPH (Message Out)
- Phase 7: MSIPH (Message In)

**Cross-References:**
- Called via: `RBSIR` vector (line 121), `BSISR` vector (line 305)
- Calls phase handlers based on bus phase
- Updates: `SCCDP` (data pointer), `SCCBC` (byte count), `BUSFL` (bus flags)

---

## Phase Handlers

### DAOPH - Data Out Phase (Lines 706-741)

**Line:** 706-741

**Purpose:** Handle SCSI Data Out phase (host to target)

**Entry Parameters:**
- `X` = Current device datafield (SCCSU)
- `SCCDP` = Current data pointer
- `SCCBC` = Current byte count

**Flow:**

#### Set Transfer Direction (Lines 706-707)
```
Line 706: DAOPH: 40 =: SCCCW; GO DATPH
Line 707: % 40 = Data Out direction code
```

#### Common Data Phase Handler (Lines 711-738)
```
Line 711: DATPH: IF X>=0 GO FAR ILLPH        % Phase illegal if no device
Line 712:        T := HDEV+WCONT; *IOXT      % Set transfer direction
Line 713:        SCCDP SHZ -1                % Memory address
```

#### Odd Byte Handling (Lines 714-728)
```
Line 714: IF M THEN                          % Memory address on odd byte
Line 715:    X BONE 6SSBT =: BUSFL; AD =: DWP; SCCBC
Line 716:    *RADD CM1 DD; RADD CM1 ADC DA   % Decrement bytecount
Line 717:    IF A BIT 17 GO BCZRO; AD =: SCCBC  % Bytecount negative
Line 718:    "0" =: SCCCW; *IOXT             % Clear to memory
Line 719:    124; T+"WNCOM-WCONT"; *IOXT     % "Transfer info, single byte" to NCR
Line 720:    IF X BIT 0 THEN                 % Data In
Line 721:       L =: D; CALL RINFO; D =: L := A    % Read byte
Line 722:       T := WP1; X := WP2; *LDATX   % Get MSB
Line 723:       A/\177400\/D; *STATX         % Store new word
Line 725:       T := WP1; X := WP2; *LDATX   % Get byte (Data Out)
Line 726:       T := HDEV+WNDAT; *IOXT       % Send
Line 728:    DWP; D+1; A := A+C; AD SHZ 1 =: SCCDP  % New memory address
```

#### Even Byte DMA Transfer (Lines 730-737)
```
Line 730: T+"WHMAR-WCONT"; *IOXT            % Write MSW of MAR
Line 731: A := D; T+"WLMAR-WHMAR"; *IOXT    % Write LSW of MAR
Line 732: SCCBC; IF A=D AND D=0 GO BCZRO    % Check bytecount
Line 733: T+"WTCM-WLMAR"; *IOXT             % MSB of transfer counter
Line 734: AD SHZ 10; T+"WTC2-WTCM"; *IOXT   % Byte 2 of transfer counter
Line 735: AD SHZ 10; T+"WTCL-WTC2"; *IOXT   % LSB of transfer counter
Line 736: 224; T+"WNCOM-WTCL"; *IOXT        % DMA MODE + TRANSFER INFO
Line 738: EXIT
```

#### Error Handling (Line 740)
```
Line 740: BCZRO: T := BCERR; GO FAR SCDIS   % Byte count error
```

**NCR Commands:**
- Line 719: Command 124 - "Transfer info, single byte"
- Line 736: Command 224 - "DMA mode + Transfer info"

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Calls: `RINFO` (read info byte)
- Branches: `ILLPH`, `BCZRO`, `SCDIS`

---

### DAIPH - Data In Phase (Lines 710-741)

**Line:** 710-741

**Purpose:** Handle SCSI Data In phase (target to host)

**Entry Parameters:**
- Same as DAOPH

**Flow:**

#### Set Transfer Direction (Lines 710-711)
```
Line 710: DAIPH: 140 =: SCCCW
Line 711: DATPH: % Common handler with DAOPH
```

**Differences from DAOPH:**
- Line 710: `140` = Data In direction code (vs `40` for Data Out)
- Lines 720-723: For odd byte transfers, reads from SCSI bus instead of sending
- Otherwise identical flow to DAOPH

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Shares code with `DAOPH` at line 711

---

### COMPH - Command Phase (Lines 745-754)

**Line:** 745-754

**Purpose:** Handle SCSI Command phase (send command block)

**Entry Parameters:**
- `X` = Current device (SCCSU)
- `X.SUCM1`, `X.SUCM2` = Command block address

**Flow:**

#### Validate Phase (Line 745)
```
Line 745: COMPH: IF X>=0 GO FAR ILLPH        % Phase not valid if no device
```

#### Setup Transfer (Lines 746-752)
```
Line 746: 40 =: SCCCW; T := HDEV+WCONT; *IOXT       % Set transfer direction
Line 747: SCCSU.SUCM1; T+"WHMAR-WCONT"; *IOXT       % Write MSW of MAR
Line 748: X.SUCM2; T+"WLMAR-WHMAR"; *IOXT           % Write LSW of MAR
Line 749: "0"; T+"WTCM-WLMAR"; *IOXT                % MSB of transfer counter
Line 750: T+"WTC2-WTCM"; *IOXT                       % Byte 2 of transfer counter
Line 751: 14; T+"WTCL-WTC2"; *IOXT                  % LSB of transfer counter (14 bytes)
Line 752: 224; T+"WNCOM-WTCL"; *IOXT                % DMA MODE + TRANSFER INFO
Line 753: EXIT
```

**Key Details:**
- Command block is always 14 bytes (line 751)
- Uses DMA transfer (command 224)
- Memory address from device structure fields SUCM1/SUCM2

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Branches: `ILLPH`

---

### STAPH - Status Phase (Lines 758-762)

**Line:** 758-762

**Purpose:** Handle SCSI Status phase (receive status byte)

**Entry Parameters:**
- `X` = Current device (SCCSU)

**Flow:**

#### Validate and Transfer (Lines 758-761)
```
Line 758: STAPH: IF X>=0 GO FAR ILLPH                % Phase not valid
Line 759:        124; T := HDEV+WNCOM; *IOXT         % "Transfer info, single byte" to NCR
Line 760:        L =: D; CALL RINFO                  % Read status byte
Line 761:        A =: SCCSU.SUSTA; D =: P            % Save status, return
```

**Key Details:**
- Single byte transfer using programmed I/O
- Status saved to `SCCSU.SUSTA`
- Uses `RINFO` to poll for byte availability

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Calls: `RINFO`
- Branches: `ILLPH`

---

### MSOPH - Message Out Phase (Lines 776-811)

**Line:** 776-811

**Purpose:** Handle SCSI Message Out phase (host to target)

**Entry Parameters:**
- `BUSFL` = Current bus flags
- `CMSGO` = Current message out buffer
- `X` = Current device (SCCSU)

**Flow:**

#### Determine Message to Send (Lines 776-796)

**Send IDENTIFY** (Lines 776-786):
```
Line 776: MSOPH: IF T BIT 6SIDE THEN             % Send identify
Line 777:           IF SCCSU.SUCON BIT 4SRNA THEN
Line 778:              7/\X.SUDLU\/200           % "IDENTIFY" (no disconnect)
Line 780:              7/\X.SUDLU\/300           % "IDENTIFY" + "DISCONNECT ALLOWED"
Line 782:           IF T BIT 6SMSO THEN
Line 783:              X := BUSFL BZERO 6SMSO =: BUSFL    % Mark message transferred
Line 784:              A SHZ 10\/CMSGO; 0 =: CMSGO
Line 787:           X := A
```

**Resend Last Message** (Lines 788-790):
```
Line 788: IF 17/\T-6=0 THEN
Line 789:    X := LMSGO                           % Resend last message
```

**Send Pending Message** (Lines 791-796):
```
Line 791: IF T BIT 6SMSO THEN
Line 792:    BUSFL BZERO 6SMSO =: BUSFL          % Mark message transferred
Line 793:    X := CMSGO; 0 =: CMSGO
Line 795:    X := 10                              % "NO OPERATION"
```

#### Send Message (Lines 797-809)

**Multi-byte Message** (Lines 797-805):
```
Line 797: IF X =: LMSGO<0 THEN                   % Multi-byte message
Line 798:    40 =: SCCCW; T := HDEV+WCONT; *IOXT % Set transfer direction
Line 799:    T := X; SCPMB; *DEPO                % Store message in buffer
Line 800:    T := HDEV+WHMAR; *IOXT              % Write MSW of MAR
Line 801:    A := D; T+"WLMAR-WHMAR"; *IOXT      % Write LSW of MAR
Line 802:    "0"; T+"WTCM-WLMAR"; *IOXT          % MSB of transfer counter
Line 803:    T+"WTC2-WTCM"; *IOXT                % Byte 2 of transfer counter
Line 804:    2; T+"WTCL-WTC2"; *IOXT             % LSB of transfer counter (2 bytes)
Line 805:    224; T+"WNCOM-WTCL"; *IOXT          % DMA MODE + TRANSFER INFO
```

**Single-byte Message** (Lines 806-809):
```
Line 807: 124; T := HDEV+WNCOM; *IOXT            % "Transfer info, single byte"
Line 808: A := X; T+"WNDAT-WNCOM"; *IOXT         % Send message
```

**Message Types:**
- IDENTIFY: 200 (octal) = 0x80 (no disconnect privilege)
- IDENTIFY: 300 (octal) = 0xC0 (disconnect privilege)
- NO OPERATION: 10 (octal) = 0x08

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Uses: `SCPMB` (parameter block), `LMSGO` (last message out)

---

### MSIPH - Message In Phase (Lines 819-870)

**Line:** 819-870

**Purpose:** Handle SCSI Message In phase (target to host)

**Entry Parameters:**
- Called when target sends message

**Flow:**

#### Read First Message Byte (Lines 819-827)
```
Line 819: MSIPH: A := L =: "MIRET"
Line 820:        124; T := HDEV+WNCOM; *IOXT         % "Transfer Info, single byte"
Line 821:        CALL RINFO; A =: CMSGI              % Read message
Line 822:        IF A-1=0 THEN
Line 823:           EMEI1 =: SCNIH                   % Extended message
Line 825:           MSFCS =: SCNIH                   % Function complete next
Line 827:        BUSFL BONE 6SMSI =: BUSFL; GO MIRET % Return
```

#### Extended Message Handler (Lines 830-866)

**Read Message Length** (Lines 833-843):
```
Line 833: EMFC1: 4; T := HDEV+WNCOM; *IOXT          % "Message accepted"
Line 834:        EMEI2 =: SCNIH; EXIT               % Expected interrupt

Line 840: EMRML: 124; T := HDEV+WNCOM; *IOXT        % "Transfer info, single byte"
Line 841:        D := L; CALL RINFO; L := D         % Get message length
Line 842:        IF A =: SCEML>SCMBL GO FAR ILLPH   % Message too large
Line 843:        EMEI3 =: SCNIH; EXIT               % Expected interrupt
```

**Read Message Bytes** (Lines 849-865):
```
Line 849: EMFC2: 4; T := HDEV+WNCOM; *IOXT          % "Message accepted"
Line 850:        EMEI4 =: SCNIH; EXIT

Line 856: EMRMB: A := L =: "MIRET"
Line 857:        "0"; T := HDEV+WTCM; *IOXT         % MSB of transfer counter
Line 858:        T+"WTC2-WTCM"; *IOXT               % Byte 2 of transfer counter
Line 859:        SCEML; T+"WTCL-WTC2"; *IOXT        % LSB of transfer counter
Line 860:        X := 0; 24; T+"WNCOM-WTCL"; *IOXT  % Transfer info
Line 861:        DO
Line 862:           CALL RINFO; T := "SCEMB"+B; *SBYT    % Read and store byte
Line 863:           WHILE X+1<SCEML
Line 864:        OD
Line 865:        EMEI5 =: SCNIH; GO MIRET           % Function complete next
```

**Interrupt Vectors:**
- `EMEI1`: Extended message byte 1 handler
- `EMEI2`: After message accepted
- `EMEI3`: After length received
- `EMEI4`: After message accepted
- `EMEI5`: After all bytes received

**Cross-References:**
- Called by: `NEWPH` phase dispatch (line 695)
- Calls: `RINFO`, `MSGI`
- Related: `MSGIN` (line 307)

---

### MSGIN - Message In Handling (Lines 307-369)

**Line:** 307-369

**Purpose:** Process received SCSI messages

**Entry Parameters:**
- `CMSGI` = Received message byte
- `X` = Current device (SCCSU)

**Flow:**

#### Handle Reconnect (Lines 313-327)
```
Line 313: MSGI:  IF X := SCCSU=0 THEN              % No current device
Line 314:           IF CMSGI BIT 7 THEN            % IDENTIFY message
Line 315:              AD SHZ 15 SHZ -35           % Extract LUN
Line 316:              T := HDEV+RSOUI; *IOXT      % Read bus device number
Line 317:              X := 7/\A := SCDVD(X)
Line 318:              IF X><0 AND D<X.SDNLU THEN
Line 319:                 SULEN; *RMPY SD DA
Line 320:                 X+D+"SDLEN"; T := X.SUTHS
Line 321:                 CNCLR/\T+7 =: BUSFL      % Restore flags
Line 322:                 IF T BIT 6SDIS GO FAR CNTHR   % Connect thread
Line 323:                 GO FAR SCABO                   % Abort
```

#### Process Message (Lines 330-366)

**Message Dispatch** (Line 332):
```
Line 332: GOSW MSG0,MSG1,MSG2,MSG3,MSG4,MSGR,MSGR,MSG7
```

**Message Handlers:**

**MSG0 - Command Complete** (Lines 334-335):
```
Line 334: MSG0: BUSFL BONE 6SCCO =: BUSFL         % Set command complete flag
Line 335:       EXIT
```

**MSG1 - Extended Message** (Lines 340-349):
```
Line 340: MSG1: IF SCEM1=2400 THEN                % Modify data pointer
Line 343:       IF A=2261 THEN                    % Synchronous transfer request
Line 346:       IF A=1002 THEN                    % Extended identify
Line 349:       GO FAR MSGR                        % Message reject
```

**MSG2 - Save Data Pointer** (Lines 337-338):
```
Line 337: MSG2: SCCDP =: X.SUSDP; SCCBC =: X.SUSBC  % Save data pointer
Line 338:       EXIT
```

**MSG3 - Restore Pointers** (Lines 352-353):
```
Line 352: MSG3: X.SUSDP =: SCCDP; X.SUSBC =: SCCBC  % Restore pointers
Line 353:       EXIT
```

**MSG4 - Disconnect** (Lines 355-356):
```
Line 355: MSG4: BUSFL BONE 6SDIS =: BUSFL          % Set disconnect flag
Line 356:       EXIT
```

**MSG7 - Message Reject** (Lines 358-365):
```
Line 358: MSG7: IF LMSGO/\377=5 OR =11 THEN        % Parity/IDE error message rejected
Line 359:          T := TRANE; GO FAR SCABO        % Send "ABORT"
Line 361:       IF A=14 THEN
Line 362:          T := FNIPL; GO FAR SCABO        % Not implemented
Line 364:          T := SPCER; GO FAR SCABO        % Illegal
```

**MSGR - Message Reject** (Line 368):
```
Line 368: MSGR:  GO FAR SCMRJ                      % Return "MESSAGE REJECT"
```

#### Accept Message (Lines 309-310)
```
Line 309: 4; T := HDEV+WNCOM; *IOXT               % "Message Accepted" to NCR
Line 310: BSISR =: SCNIH; GO HOME2                % Allow bus service interrupts
```

**SCSI Messages:**
- 0 = Command Complete
- 1 = Extended Message
- 2 = Save Data Pointer
- 3 = Restore Pointers
- 4 = Disconnect
- 7 = Message Reject

**Cross-References:**
- Called by: `MSIPH` (line 821)
- Calls: `CNTHR`, `SCMRJ`, `SCABO`
- Branches: `SCDIS`

---

## Support Routines

### INITO - Initialize Operation (Lines 414-430)

**Line:** 414-430

**Purpose:** Initialize SCSI operation parameters

**Entry Parameters:**
- `X` = Unit datafield
- `X.SUCON` = Control word

**Flow:**

#### Determine Operation Type (Lines 414-424)
```
Line 414: INITO: IF X.SUCON =: D SHZ -10=0 THEN
Line 415:           INOPR                          % Normal operation
Line 416:           IF D BIT 4SRCA THEN
Line 417:              A BONE 6SRFD                % Return on command accepted
Line 420:        IF A-1=0 THEN
Line 421:           INABO                          % Abort
Line 423:           INBDR                          % Bus device reset
```

**Operation Constants:**
```
Line 410: INOPR := 0; * *-1/1@6SFUN+^; *-1/1@6SIDE+^    % Flags: 6SFUN, 6SIDE
Line 411: INABO := 6; * *-1/1@6SMSO+^                    % Abort: flag 6SMSO
Line 412: INBDR := 14; * *-1/1@6SMSO+^                   % Bus Device Reset: flag 6SMSO
```

#### Setup Operation (Lines 425-429)
```
Line 425: A =: X.SUTHS; -1 =: X.SUSTA              % Initial status
Line 426: X.SUIDP =: X.SUSDP; X.SUIBC =: X.SUSBC   % Initial pointers
Line 427: -2 =: X.SUTMR; "SCWAQ-SULINK"+B; T := X  % Insert in arbitration queue
Line 428: DO A =: X; WHILE X.SULINK><0 OD          % Find end of queue
Line 429: T =: X.SULINK                            % Link into queue
Line 430: EXIT
```

**Cross-References:**
- Called by: `SCLLD` (line 98)
- Sets up: `SUTHS` (status), `SUSTA` (status byte), `SUSDP` (data pointer), `SUSBC` (byte count)

---

### CNTHR - Connect Physical Path (Lines 434-441)

**Line:** 434-441

**Purpose:** Connect the physical SCSI path to a logical unit

**Entry Parameters:**
- `X` = Unit datafield
- `BUSFL` = Bus flags

**Flow:**

```
Line 434: CNTHR: X =: SCCSU                        % Indicate path connected
Line 435:        IF X.SUTHS BIT 6SMSO THEN
Line 436:           A/\377 =: CMSGO                % Restore message
Line 437:           3; T := HDEV+WNCOM; *IOXT      % "Set ATN" to NCR
Line 439:        X.SUSDP =: SCCDP; X.SUSBC =: SCCBC    % Restore pointers
Line 440:        EXIT
```

**Key Operations:**
- Sets `SCCSU` to current device
- If message pending (6SMSO), restore message buffer and set ATN
- Restore saved data pointers

**NCR Command:**
- Line 437: Command 3 - "Set ATN" (Attention)

**Cross-References:**
- Called by: `SCINT` (line 163), `MSGIN` (line 322)
- Updates: `SCCSU`, `CMSGO`, `SCCDP`, `SCCBC`

---

### DCTHR - Disconnect Physical Path (Lines 446-459)

**Line:** 446-459

**Purpose:** Disconnect the physical SCSI path from logical unit

**Entry Parameters:**
- `X` = Unit datafield (SCCSU)
- `BUSFL` = Bus flags

**Flow:**

#### Check Disconnect Conditions (Lines 447-457)
```
Line 447: DCTHR: A := L =: "HOME4"                 % Return address
Line 447:        IF T := X.SUTRG=0 AND BUSFL BIT 6SDIS THEN
```

#### Return on First Disconnect (Lines 448-454)
```
Line 448: IF A BIT 6SRFD THEN
Line 449:    A/\DCCLR\/CMSGO =: X.SUTHS            % Save thread status
Line 450:    T := -1                               % Intermediate return
Line 452:    A/\DCCLR\/CMSGO =: X.SUTHS            % Save thread status
Line 453:    X := 0                                % No return
```

#### Terminate on Error (Lines 456-457)
```
Line 456: CALL TEROP                               % Terminate (T=status)
```

#### Clear and Return (Line 458)
```
Line 458: 0 =: SCCSU; GO HOME4                     % Return
```

**Flag Mask:**
- Line 84: `DCCLR = 136400` - Flags cleared on disconnect

**Cross-References:**
- Called by: `SCINT` (line 147)
- Calls: `TEROP`
- Clears: `SCCSU`

---

### RSTMR - Restart Logical Unit Timer (Lines 465-477)

**Line:** 465-477

**Purpose:** Restart the timer for logical unit operations

**Entry Parameters:**
- `SCTST` = Timer state
- `SCTQP` = Timer queue pointer

**Flow:**

```
Line 465: RSTMR: IF SCTST=0 THEN                   % Timer not locked
Line 466:           IF X =: SAVRX := SCTQP><0 THEN % Queue not empty
Line 467:              IF X.SUTMR =: TMR><0 THEN   % New time value
Line 468:                 0 =: X.SUTMR; X =: SCTST % Indicate timer running
Line 470:                 SCRSQ =: SCRSL; A := B =: SCRSQ  % Direct timeout
Line 473:              0 =: TMR                    % Disable timer
Line 475:           X := SAVRX
Line 477:        EXIT
```

**Key Operations:**
- If timer state is 0 (not locked), check timer queue
- If queue has entry with timer value, start timer
- Otherwise set direct timeout or disable timer

**Cross-References:**
- Called by: Multiple locations (lines 152, 212, 269, 498, 544, 557)
- Updates: `TMR`, `SCTST`

---

### ENTIM - Enable Logical Unit Timer (Lines 486-500)

**Line:** 486-500

**Purpose:** Enable timer for logical unit and insert into timer queue

**Entry Parameters:**
- `L` = Logical unit datafield
- `L.SUCON` = Control word with timeout value
- `L.SUTMR` = Timer value

**Flow:**

#### Save Registers (Line 486)
```
Line 486: ENTIM: TAD =: TITAD; X := :L =: "HOME5"
```

#### Save Current Timer (Lines 487-489)
```
Line 487: IF X := SCTST>>2 THEN
Line 488:    TMR =: X.SUTMR; 0 =: SCTST            % Save current timer
```

#### Calculate Timeout (Line 490)
```
Line 490: 17/\L.SUCON\/SHTIN; T := -1; *EXR SA    % Calculate max time
```

#### Insert in Timer Queue (Lines 491-497)
```
Line 491: D := B; X := "SCTQP-SULINK"+B
Line 492: DO
Line 493:    WHILE X =: B := X.SULINK><0 AND X.SUTMR>=T
Line 494:    T-A                                   % Subtract time
Line 495: OD
Line 496: IF X><0 THEN A-T =: X.SUTMR FI          % Update next element
Line 497: SULINK =: L.SULINK; X =: SULINK; B := D % Link into timer queue
Line 498: T =: X.SUTMR; CALL RSTMR                % Reset timer
```

#### Restore and Return (Line 499)
```
Line 499: TITAD; GO HOME5
```

**Cross-References:**
- Called by: `SCLLD` (line 104), `SCINT` (line 164)
- Calls: `RSTMR`
- Updates: Timer queue (`SCTQP`)

---

### DITIM - Disable Logical Unit Timer (Lines 541-560)

**Line:** 541-560

**Purpose:** Remove logical unit from timer queue

**Entry Parameters:**
- `L` = Logical unit datafield

**Flow:**

#### Save Registers (Line 541)
```
Line 541: DITIM: TAD =: TITAD; X =: SAVX := :L =: "HOME5"
```

#### Check Timer Queue (Lines 542-558)
```
Line 542: IF X := SCTQP><0 THEN
Line 543:    IF X := SCTST>>2 THEN
Line 544:       TMR =: X.SUTMR; 0 =: SCTST         % Save timer
Line 546:    X := "SCTQP-SULINK"+B
Line 547:    DO
Line 548:       X =: D
Line 549:       WHILE X := X.SULINK><L AND X><0
Line 550:    OD
Line 551:    IF X=L THEN                           % Found in queue
Line 552:       IF T := X.SULINK =: D.SULINK><0 THEN
Line 553:          0 =: L.SULINK
Line 554:          X.SUTMR+T.SUTMR =: X.SUTMR     % Add time to next
Line 557:    CALL RSTMR
```

#### Restore and Return (Line 559)
```
Line 559: X := SAVX; TITAD; GO HOME5
```

**Cross-References:**
- Called by: Timer routine (line 264)
- Calls: `RSTMR`
- Entry point for `TEROP` (line 541)

---

### TEROP - Terminate Operation (Lines 508-537)

**Line:** 508-537

**Purpose:** Terminate SCSI operation with status

**Entry Parameters:**
- `X` = Unit datafield
- `T` = Status code
- `BUSFL` = Bus flags

**Flow:**

#### Check Operation Status (Lines 508-533)
```
Line 508: TEROP: IF X.SUTRG=0 THEN                 % No trigger error
Line 509:           IF SCCSU=X THEN                % Currently connected
Line 510:              SCCDP =: X.SUSDP; SCCBC =: X.SUSBC  % Save data pointers
Line 511:              IF BUSFL NBIT 6SMSO THEN    % No message out pending
Line 512:                 IF A BIT 6SCCO THEN      % Command complete
Line 513:                    IF X.SUSTA<0 THEN
Line 514:                       T := NOSST         % No status received
Line 516:                       T := 0             % Command complete
Line 519:                 IF X.SUCON SHZ -10=0 AND T=0 THEN
Line 520:                    T := UNDIS           % Unexpected disconnect
Line 523:              IF T=0 THEN
Line 524:                 IF CMSGO=5 OR =11 THEN
Line 525:                    T := TRANE           % Parity error
Line 527:                    T := MNIBT           % Message not implemented
Line 531:           IF X.SUCON SHZ -10-3=0 AND CMTMO=T THEN
Line 532:              T := 0                      % Timer finished
Line 535:           A =: T                         % Error in operation
Line 537:        0 =: X.SUTHS                      % Indicate free
```

**Status Codes:**
- `0` = Success
- `NOSST` = No status received
- `UNDIS` = Unexpected disconnect
- `TRANE` = Parity error
- `MNIBT` = Message not implemented

**Continues into DITIM** (line 541) to disable timer

**Cross-References:**
- Called by: Multiple locations (lines 154, 198, 206, 223, 235, 456)
- Falls through to: `DITIM`
- Updates: `X.SUSDP`, `X.SUSBC`, `X.SUTHS`

---

### RFWAQ - Remove From Wait for Arbitration Queue (Lines 401-404)

**Line:** 401-404

**Purpose:** Remove unit from arbitration wait queue

**Entry Parameters:**
- `SCWAQ` = Arbitration wait queue head

**Flow:**

```
Line 401: RFWAQ: IF X := SCWAQ><0 THEN
Line 402:           X.SULINK =: SCWAQ; 0 =: X.SULINK
Line 404:        EXIT
```

**Key Operations:**
- Remove head of queue
- Clear link in removed element

**Cross-References:**
- Called by: `SCINT` (lines 153, 161, 246), timeout routine (line 231)
- Updates: `SCWAQ`

---

### RINFO - Read Information Byte from SCSI Bus (Lines 877-886)

**Line:** 877-886

**Purpose:** Poll NCR data register for byte availability

**Entry Parameters:**
- None

**Exit Parameters:**
- `A` = Byte read from SCSI bus

**Flow:**

```
Line 877: RINFO: T := HDEV+RAUXS; -2000 =: RDCNT
Line 878:        FOR RDCNT DO
Line 879:           *IOXT                          % Read auxiliary status
Line 880:           IF A BIT DARGF THEN            % Data register full
Line 881:              T+"RNDAT-RAUXS"; *IOXT      % Read NCR data register
Line 882:              EXIT
Line 884:        OD
Line 885:        GO FAR SCINT                      % Check for interrupt (timeout)
```

**Key Details:**
- Polls up to 2000 times (line 877)
- Checks DARGF bit in auxiliary status (line 880)
- If timeout, jumps to interrupt handler (line 885)

**Cross-References:**
- Called by: `STAPH` (line 760), `MSIPH` (line 821), `EMRML` (line 841), `EMRMB` (line 862)
- May branch to: `SCINT`

---

### SCRST - Start Reset Sequence on SCSI Bus (Lines 568-621)

**Line:** 568-621

**Purpose:** Reset SCSI bus and NCR controller

**Entry Parameters:**
- `NCROK` = NCR status flags
- `SCSSR` = SCSI status register

**Flow:**

#### Check if Already Resetting (Line 568)
```
Line 568: SCRST: IF NCROK=0 THEN                   % Not already resetting
```

#### Save Current Timer (Line 569)
```
Line 569: IF X := SCTST>>2 THEN TMR =: X.SUTMR FI  % Save current timer
```

#### Determine Reset Type (Lines 570-574)
```
Line 570: IF SCSSR BIT 5 THEN
Line 571:    NCROK BONE 8SRIN =: NCROK; T := SBRST  % Receiving reset
Line 573:    T := LIRST                             % Local reset
```

#### Mark Current Operation (Lines 575-581)
```
Line 575: IF X := SCCSU><0 THEN
Line 576:    IF X.SUTRG=0 THEN T =: X.SUTRG FI      % Set status
Line 577:    BUSFL BONE 6SRST =: BUSFL              % Indicate reset started
Line 578:    NCROK BONE 8SCLR =: NCROK              % Recovery necessary
Line 580:    "0" BONE 6SRST =: BUSFL                % Indicate reset started
```

#### Mark All Active Operations (Lines 582-590)
```
Line 582: X := "SCTQP-SULINK"+B
Line 583: DO
Line 584:    WHILE X := X.SULINK><0                 % For all active operations
Line 585:    IF X.SUCON NBIT 4SRST AND X.SUTHS BIT 6SDIS THEN
Line 586:       A BONE 6SRST =: X.SUTHS             % Indicate reset
Line 587:       IF X.SUTRG=0 THEN T =: X.SUTRG FI   % Set status
Line 588:       NCROK BONE 8SCLR =: NCROK           % Recovery necessary
Line 590: OD
```

#### Clear Flags (Lines 592-593)
```
Line 592: NCROK/\CLMSK =: NCROK
Line 594: NCROK BONE 8SRST =: NCROK                 % Indicate reset started
```

**Flag Mask:**
```
Line 565: CLMSK := 0; * *-1/1@8SRIN+^; *-1/1@8SDIA+^; *-1/1@8STMR+^
Line 566:           * *-1/1@8SERR+^; *-1/177777-^
```

#### Lock Interface (Line 595)
```
Line 595: -1 =: SCEIM; 2 =: SCTST                   % Lock interface
```

#### Reset NCR Controller (Lines 596-604)
```
Line 596: "0"; T := HDEV+WNCOM; *IOXT               % Disconnect to NCR
Line 597: 20; T+"WCONT-WNCOM"; *IOXT                % Clear controller
Line 598: T+"RSTAU-WCONT"; *IOXT                    % Read status
Line 599: IF A BIT 5 THEN
Line 600:    NCROK BONE 8SRIN =: NCROK; -1 =: TMR   % Receiving RST
Line 602:    2000; T+"WCONT-RSTAU"; *IOXT           % Set reset on SCSI bus
Line 603:    -10000 =: TMR; FOR TMR DO OD           % Leave "RST" on for 250 us
Line 604:    "0"; *IOXT                             % Clear reset
```

#### Start NCR Diagnostics (Lines 605-618)
```
Line 605: T+"RDIST-WCONT"                           % Read diagnostic status
Line 606: FOR X := -1000 DO
Line 607:    *IOXT
Line 608:    WHILE A NBIT 7                         % Wait for ready
Line 609: OD
Line 610: IF A=200 THEN                             % Selfcheck OK
Line 611:    NCROK BONE 8SDIA =: NCROK
Line 612:    13; T := HDEV+WNCOM; *IOXT             % Start diagnostic
Line 613:    376; T+"WNDAT-WNCOM"; *IOXT            % Write data register
Line 614:    5; T+"WCONT-WNDAT"; *IOXT              % Enable interrupt
Line 615:    -2 =: TMR                              % Diagnostic timer
Line 617:    100000 =: NCROK                        % Error in interface
```

**Timing:**
- Line 603: Reset held for 250 microseconds (10000 loops)
- Line 606: Wait up to 1000 loops for diagnostic ready

**NCR Commands:**
- Line 596: Command 0 - "Disconnect"
- Line 597: Command 20 (octal) - Clear controller (write to WCONT)
- Line 602: Value 2000 (octal) - Set RST on SCSI bus
- Line 612: Command 13 (octal) - Start diagnostic
- Line 613: Value 376 (octal) - Diagnostic test pattern

**Cross-References:**
- Called by: `SCLLD` (line 109), `SCDIS` (line 286), timeout routine (line 228)
- Branches to: `SCTIO` (line 620)

---

### STFIN - Terminate Reset on SCSI Bus (Lines 625-653)

**Line:** 625-653

**Purpose:** Complete reset sequence after diagnostics

**Entry Parameters:**
- `NCROK` = NCR status flags
- `TMR` = Timer value

**Flow:**

#### Check Diagnostics (Lines 625-645)
```
Line 625: STFIN: IF NCROK BIT 8SDIA THEN            % Diagnostics running
Line 626:           A BZERO 8SDIA =: NCROK          % Diagnostics finished
Line 627:           IF TMR><0 AND D BIT DARGF THEN  % Timer OK and data ready
Line 628:              T := HDEV+RNDAT; *IOXT       % Read data register
Line 629:              16; T+"WNCNT-RNDAT"; *IOXT   % Set NCR control
Line 630:              IF A NBIT 1 THEN
Line 631:                 5; T+"WCONT-WNCNT"; *IOXT % Enable interrupt
Line 633:              0 =: TMR
Line 634:              IF T := NCROK BIT 8SPWF THEN
Line 635:                 T BZERO 8SPWF; X := SLTMR % Long timer (power lost)
Line 637:                 X := SRTMR                % Reset timer
Line 639:              IF SCRCO NBIT 0 THEN
Line 640:                 X =: TMR; T BONE 8STMR    % Allow device settle time
Line 642:              T =: NCROK
Line 644:              0 =: TMR; 100000 =: NCROK    % Error in interface
```

#### Reset Timer Expired (Lines 647-651)
```
Line 647: IF A BIT 8STMR THEN
Line 648:    A BZERO 8STMR =: NCROK; 0 =: TMR       % Reset finished
Line 650:    0 =: TMR; 100000 =: NCROK              % Error in interface
```

#### Return to Timeout Routine (Line 652)
```
Line 652: GO FAR SCTIO
```

**NCR Setup:**
- Line 629: Write 16 (octal) to control register - Sets up NCR operating mode

**Cross-References:**
- Called by: `SCINT` (line 172), timeout routine (line 239)
- Branches to: `SCTIO`

---

## Error Handling

### SCDIS - Fatal Error: Reset SCSI Bus (Lines 278-287)

**Line:** 278-287

**Purpose:** Handle fatal errors by resetting SCSI bus

**Entry Parameters:**
- `T` = Error code
- `X` = Current device (SCCSU)

**Flow:**

#### Save Error Status (Lines 278-285)
```
Line 278: SCDIS: IF X := SCCSU><0 THEN
Line 279:           IF X.SUTRG=0 THEN T =: X.SUTRG FI  % Set error code
Line 281:        BUSFL =: SEST1                        % Save status
Line 282:        SCSSR =: SEST2
Line 283:        SCNIS =: SEST3
Line 284:        CMSGI =: SEST4
Line 285:        CMSGO =: SEST5
Line 286:        T =: SCLRE; GO FAR SCRST              % Reset SCSI bus
```

**Saved Status Variables (Lines 89-91):**
```
Line 89: DISP -32
Line 90:    INTEGER SEST1, SEST2, SEST3, SEST4, SEST5
Line 91: PSID
```

**Cross-References:**
- Called from: Multiple error paths (lines 96, 127, 183, 255, 261, 326, 697, 740, 766, 771, 870)
- Branches to: `SCRST`
- Saves to: `SEST1-SEST5`, `SCLRE`

---

### BUSFP - Bus Free Phase (Lines 289-290)

**Line:** 289-290

**Purpose:** Handle bus free phase, check for pending arbitration

**Flow:**

```
Line 289: BUSFP: CALL SELEC                          % Check arbitration queue
Line 290: % EXIT DRIVER...
```

**Cross-References:**
- Called by: `SCINT` (line 157)
- Calls: `SELEC`
- Falls through to: `EXDRI`

---

### EXDRI - Exit Driver (Lines 292-300)

**Line:** 292-300

**Purpose:** Return to caller with status

**Entry Parameters:**
- `X` = LUN datafield or 0
- `T` = Status code

**Flow:**

#### Check Return Address (Line 292)
```
Line 292: EXDRI: IF X=0 GO SCWTI; X.SULRG            % No return
```

#### Normal Return (Lines 293-294)
```
Line 293: IF T=0 THEN A+2 =: P FI                   % Normal return (JMP FIN)
```

#### Intermediate Return (Lines 294-296)
```
Line 294: IF T<0 THEN
Line 295:    A+1                                     % Intermediate return (JMP ERROR)
```

#### Error Return (Lines 297-298)
```
Line 297: ELSE
Line 298:    MIN ERCNT; 0/\0                         % Error return (JMP BUSY)
```

#### Set Return Address (Line 299)
```
Line 299: A =: P
```

**Return Convention:**
- `T = 0`: Jump to FIN (offset +2)
- `T < 0`: Jump to ERROR (offset +1)
- `T > 0`: Jump to BUSY (offset +0)

**Cross-References:**
- Called by: Multiple locations (lines 96, 200, 206, 223, 236, 292)
- Uses: `SULRG` (return address)

---

### Error Message Handlers

#### SCMRJ - Set "Message Reject" (Lines 891-892)

**Line:** 891-892

**Purpose:** Send MESSAGE REJECT to target

**Flow:**

```
Line 891: SCMRJ: 7SMRJ; T := SPCER; GO EMSGO         % Send message
```

**Message Code:**
- `7SMRJ = 7` (octal) = MESSAGE REJECT

**Cross-References:**
- Called by: `MSGIN` (line 368)
- Branches to: `EMSGO`

---

#### SCMPE - Set "Message Parity Error" (Lines 895-896)

**Line:** 895-896

**Purpose:** Send MESSAGE PARITY ERROR to target

**Flow:**

```
Line 895: SCMPE: BUSFL BZERO 6SMSI =: BUSFL          % Throw away message
Line 896:        7SMPE; T := TRANE; GO EMSGO
```

**Message Code:**
- `7SMPE = 11` (octal) = MESSAGE PARITY ERROR

**Cross-References:**
- Called by: `SCINT` (line 177)
- Branches to: `EMSGO`

---

#### SCIDE - Set "Initiator Detected Error" (Lines 900-901)

**Line:** 900-901

**Purpose:** Send INITIATOR DETECTED ERROR to target

**Flow:**

```
Line 900: SCIDE: IF BUSFL BIT 6SMSO AND CMSGO=7SIDE THEN EXIT FI
Line 901:        7SIDE; T := TRANE; GO EMSGO
```

**Message Code:**
- `7SIDE = 5` (octal) = INITIATOR DETECTED ERROR

**Cross-References:**
- Called by: `SCINT` (lines 130, 179)
- Branches to: `EMSGO`

---

#### SCABO - Abort Operation (Lines 907-917)

**Line:** 907-917

**Purpose:** Send ABORT message to target

**Entry Parameters:**
- `T` = Error code
- `X` = Current device (SCCSU)

**Flow:**

```
Line 907: SCABO: IF X := SCCSU><0 THEN
Line 908:           IF X.SUTRG=0 THEN T =: X.SUTRG FI  % Set error message
Line 910:        "6"                                    % ABORT message
```

**Message Code:**
- `"6"` (octal) = ABORT

**Falls through to EMSGO (line 915)**

**Cross-References:**
- Called by: `MSGIN` (lines 323, 359, 362, 364)
- Falls through to: `EMSGO`

---

#### EMSGO - Indicate Message Ready (Lines 903-917)

**Line:** 903-917

**Purpose:** Set message out pending and assert ATN

**Entry Parameters:**
- `A` = Message byte

**Flow:**

```
Line 903: EMSGO: IF X := BUSFL BIT 6SMSO THEN        % Message already pending
Line 915:        A =: CMSGO; T := BUSFL BONE 6SMSO =: BUSFL  % Send message
Line 916:        3; T := HDEV+WNCOM; *IOXT           % "Set ATN" to NCR
Line 917:        EXIT
```

**NCR Command:**
- Line 916: Command 3 - "Set ATN"

**Cross-References:**
- Called by: `SCMRJ`, `SCMPE`, `SCIDE`, `SCABO`
- Updates: `CMSGO`, `BUSFL`

---

## Interrupt Flow Summary

### Complete Interrupt Flow with Line Numbers

1. **Interrupt Entry** (Line 123)
   - Read device status from RSTAU

2. **Initial Status Checks** (Lines 124-132)
   - Check controller busy (line 126)
   - Check SCSI bus reset (lines 126-128)
   - Check initiator detected error (lines 129-131)

3. **NCR Interrupt Processing** (Lines 133-187)
   - **Line 133:** Check NCRIT bit (interrupt from NCR)
   - **Line 134:** Write 0 to WCONT (disable interrupt/clear to memory)
   - **Line 135:** Read auxiliary status (RAUXS)
   - **Line 137:** Read interrupt register (RITRG)
   - **Line 139:** Save status to SCNIS

4. **Expected Interrupt** (Lines 141-143)
   - Check if interrupt matches expected
   - Call SCISR (interrupt service routine)

5. **Unexpected Interrupt Handling** (Lines 144-184)
   - **Arbitration timeout** (lines 145-158)
   - **Arbitration won** (lines 160-166)
   - **Reconnect** (lines 167-169)
   - **Selftest finished** (lines 171-172)
   - **Parity error** (lines 174-181)
   - **Illegal interrupt** (lines 182-184)

6. **Re-enable Interrupt** (Line 187)
   - **Line 187:** Write 5 to WCONT (activate + enable interrupt)

7. **Exit** (Line 188)
   - Return to wait state

### Key Register Operations in SCINT

```
Line 123: Read RSTAU        -> Device status
Line 134: Write 0 to WCONT  -> Disable interrupt
Line 135: Read RAUXS        -> Auxiliary status
Line 137: Read RITRG        -> Interrupt register
Line 187: Write 5 to WCONT  -> Enable interrupt
```

### Interrupt Vectors

**Bus Service Interrupt Handlers:**
- `RBSIR` (Line 121): Points to NEWPH initially
- `BSISR` (Line 305): Used by MSGIN for phase changes

**Extended Message Handlers:**
- `EMEI1` (Line 815): Extended message byte 1
- `EMEI2` (Line 831): After message accepted
- `EMEI3` (Line 838): After length received
- `EMEI4` (Line 847): After message accepted
- `EMEI5` (Line 854): After all bytes received

**Function Complete Handlers:**
- `MSFCS` (Line 816): Standard message handler
- `EMFC1` (Line 815): Extended message function complete
- `EMFC2` (Line 847): Extended message function complete

---

## Cross-Reference Summary

### Major Call Chains

**Initialization:**
```
SCLLD -> INITO -> SELEC
```

**Interrupt Processing:**
```
SCINT -> SCISR -> (phase handlers)
      -> DCTHR -> TEROP -> DITIM
      -> CNTHR
      -> ENTIM -> RSTMR
```

**Phase Changes:**
```
NEWPH -> DAOPH/DAIPH/COMPH/STAPH/MSOPH/MSIPH
      -> MSGIN -> MSG0/MSG1/MSG2/MSG3/MSG4/MSG7
```

**Error Handling:**
```
(error) -> SCDIS -> SCRST -> STFIN -> SCTIO
        -> SCMRJ/SCMPE/SCIDE/SCABO -> EMSGO
```

**Timeout:**
```
SCTIO -> TEROP -> DITIM -> RSTMR
      -> SCRST
      -> SELEC
```

---

## Summary

This SCSI driver implements a complete initiator-mode SCSI interface using the NCR 5386 controller. Key features:

1. **Interrupt-driven operation** with comprehensive interrupt handling
2. **Full SCSI phase support** (Data, Command, Status, Message)
3. **Arbitration and selection** with retry logic
4. **Disconnect/reconnect** support
5. **Timer management** for timeouts and retries
6. **Error recovery** including bus reset
7. **Message handling** including extended messages
8. **DMA transfers** for data and command phases
9. **Parity error detection** and recovery

The driver carefully manages hardware registers, particularly WCONT (control register), which is used to enable/disable interrupts, clear the controller, and set SCSI bus signals like RST.

Critical interrupt flow ensures:
- Interrupts disabled during processing (line 134: write 0 to WCONT)
- Status registers read before being cleared
- Interrupts re-enabled before exit (line 187: write 5 to WCONT)
- Expected interrupts handled via vector table
- Unexpected interrupts analyzed and recovered

The phase handlers implement the full SCSI protocol, including:
- DMA setup for bulk transfers
- Programmed I/O for single-byte transfers
- Proper handling of odd-byte boundaries
- Message protocol support
- Data pointer save/restore

---

## File Information

**Full Path:** `Z:\NorskData\Source Code\Sintran L\NPL\IP-P2-SCSI-DRIV.NPL`

**Total Lines:** 921

**Date Stamp:** 066562 (visible in octal at file header)

**Entry Points Defined:**
- SCLLD (line 93)
- SCINT (line 123)
- SCTIO (line 195)

**Key Global Variables:**
- NCROK: NCR controller status
- BUSFL: Bus flags
- SCCSU: Current device pointer
- SCWAQ: Arbitration wait queue
- SCTQP: Timer queue pointer
- SCCDP: Current data pointer
- SCCBC: Current byte count
