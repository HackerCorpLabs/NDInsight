# HDLC Register Usage Analysis - Complete HDEV+ Operations

## Register Map Reference
```
RRDR  = 0000,  // READ REC.DATA REG.           (HDEV+0)
WPCR  = 0001,  // WRITE PARAM. CONTROL REG.    (HDEV+1)  
RRS   = 0002,  // READ REC STATUS              (HDEV+2)
WSAR  = 0003,  // WRITE SYNC/ADDRESS REG.      (HDEV+3)
WCHL  = 0004,  // WRITE CHAR. LENGTH           (HDEV+4)
WTDR  = 0005,  // WRITE TRANSM. DATA REG.      (HDEV+5)
RTSR  = 0006,  // READ TRANSM. STATUS REG.     (HDEV+6)
WTCR  = 0007,  // WRITE TRANSM. CONTROL REG.   (HDEV+7)
RRTS  = 0010,  // READ REC. TRANSFER STATUS    (HDEV+10)
WRTC  = 0011,  // WRITE REC. TRANSFER CONTROL  (HDEV+11)
RTTS  = 0012,  // READ TRANSM. TRANSFER STATUS (HDEV+12)
WTTC  = 0013,  // WRITE TRANSM. TRANSFER CONTROL (HDEV+13)
RDMA  = 0014,  // READ DMA ADDRESS (LEAST)     (HDEV+14)
WDMA  = 0015,  // WRITE DMA ADDRESS (LEAST)    (HDEV+15)
RDCR  = 0016,  // READ DMA COMMAND REG         (HDEV+16)
WDCR  = 0017   // WRITE DMA COMMAND REG + TRIGGER (HDEV+17)
```

## Individual Register Analysis

### RRTS - Read Receiver Transfer Status (HDEV+10)

**Usage Locations Found:**
1. **Line 043233** - HDLC Interface Detection:
```assembly
T:=D.HDEV+RRTS; *EXR ST; TRA IIC; 1BANK
IF A=0 THEN                         % Interface is present
```
**Purpose**: Test if HDLC hardware interface exists during system initialization.

2. **Line 052422** - Bus Status Check:
```assembly  
HDIBUS:CALL ID13
T:=HDEV+10; *EXR ST
IF A NBIT BROCK THEN
   GO HDIBUS                   % Block not ready, retry
```
**Purpose**: Check if receiver block is ready for operation.

3. **Line 061302** - Interrupt Pointer Read:
```assembly
RETURN: T:=HDEV+10; *IOXT           % Read interrupt pointer
        MNCNTREG; T:=HDEV+5; *IOXT  % Enable for interrupt
```
**Purpose**: Read interrupt status for multibus network interface.

4. **Line 103407** - DMA Block Status:
```assembly
9TBUS: T:=HDEV+10; *EXR ST
       IF A NBIT BROCK GO 9BUSY
```  
**Purpose**: Check DMA block ready status during operations.

5. **Line 104436** - **MAIN RECEIVER INTERRUPT**:
```assembly
HIINT: T:=HDEV+RRTS; *EXR ST                     % Read receiver status
       A=:HASTAT                                 % Store in HASTAT
       IF T:=ACTSW = 0 THEN MIN T9; P+0; GO OUT1 FI % Dummy interrupt counter
```
**Purpose**: **Primary receiver interrupt handler** - reads DMA status for packet reception.

### RTTS - Read Transmitter Transfer Status (HDEV+12)

**Usage Locations Found:**
1. **Line 103220** - DMA Progress Check:
```assembly
9CONT: CALL ID12; T:=HDEV+12; *EXR ST
       IF A NBIT BROCK THEN
          A=:X:=164; GO 9ERROR                % DMA error
```
**Purpose**: Monitor DMA transmission progress, detect errors.

2. **Line 103323** - Initialization Status:
```assembly
CALL ID12; A:=100; T:=HDEV+11; *EXR ST     % Set DTR
T:=HDEV+12; *EXR ST                        % Read status after init
```  
**Purpose**: Read transmitter status after DTR initialization.

3. **Line 104034** - **MAIN TRANSMITTER INTERRUPT**:
```assembly
HOINT: 0=:TMR                                    % Reset timer
       T:=HDEV+RTTS; *EXR ST                     % Read status
       A=:HASTAT                                 % Save status
       IF T:=ACTSW = 0 THEN MIN DUIN; P+0; CALL WT12 FI % Retry counter
```
**Purpose**: **Primary transmitter interrupt handler** - reads DMA status for transmission completion.

### WRTC - Write Receiver Transfer Control (HDEV+11)

**Usage Locations Found:**
1. **Line 052530** - Timeout Recovery:
```assembly
HDITIM: HSTAT BONE 4=:HSTAT; 12=:CERRCODE
        100; T:=HDEV+11; *EXR ST
```
**Purpose**: Set receiver control to value 100 (octal 144) for timeout recovery.

2. **Line 103316** - DTR Control:
```assembly
CALL ID12; A:=100; T:=HDEV+11; *EXR ST     % Set DTR
```
**Purpose**: Set DTR (Data Terminal Ready) signal.

3. **Line 104272** - Clear Receiver State:
```assembly
ZSTARC: IF ACTSW = 0 THEN
           HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST  % Clear old garbage
```
**Purpose**: Clear receiver state before starting DMA operation.

4. **Line 104321** - Set Receiver Mode:
```assembly
OUT1: A:="1734"\/MAINT/\HXDOK
      T:=HDEV+WRTC; * EXR ST
```
**Purpose**: Set receiver transfer control to 1734 (octal) = 1244 decimal = 0x04DC.

5. **Line 104741** - Device Clear:
```assembly
SMCLEAR: T:=HDEV+WTCR; A:=2; *EXR ST             % Send EOM
         A:=100; T:=HDEV+WRTC; *EXR  ST          % Clear receiver
```
**Purpose**: Clear receiver during HDLC shutdown/reset.

6. **Line 105064** - Maintenance Mode:
```assembly
IF A = MAMOD THEN A:=140 ELSE A:=100 FI   % Maintenance or normal
A=:MAINT
T:=HDEV+WRTC; *EXR ST                     % Set mode
```
**Purpose**: Set maintenance mode (140 octal) or normal mode (100 octal).

7. **Line 105235** - Receiver Reset:
```assembly
RECSET: A:=MAINT\/100                       % Combine with base 100
        T:=HDEV+WRTC; *EXR ST
```
**Purpose**: Reset receiver with maintenance flags.

### WTTC - Write Transmitter Transfer Control (HDEV+13)

**Usage Locations Found:**
1. **Line 052542** - Timeout Clear:
```assembly
HDOTIM: HSTAT BONE 4=:HSTAT; 12=:CERRCODE
        "0"; T:=HDEV+13; *EXR ST
```
**Purpose**: Clear transmitter (set to 0) during timeout recovery.

2. **Line 103174** - Control Write:
```assembly
T:=HDEV+13; *EXR ST
IF 9TREG/\77=1 THEN                      % Send data mode
```
**Purpose**: Write transmitter control register from HCTRL variable.

3. **Line 103241** - Half-Duplex Control:
```assembly
IF HCTRL BIT HDHDX THEN
   HDMSCE; T:=HDEV+13; *EXR ST
   T-1; *EXR ST
```
**Purpose**: Set half-duplex mode control, write twice (on/off sequence).

4. **Line 103723** - **DMA TRANSMISSION START**:
```assembly
1134+CMODI; T:=HDEV+WTTC; *EXR ST
1 =: ACTSW                                % Mark active
```
**Purpose**: **Start DMA transmission** with control value 1134+CMODI (likely 1134 octal + mode bits).

5. **Line 104053** - RQTS Control:
```assembly
IF CMODI = 40 THEN
   T:=HDEV+WTTC; *EXR ST   % Turn off RQTS
```
**Purpose**: Turn off RQTS (Request To Send) signal after transmission.

6. **Line 104121** - Timeout Shutdown:
```assembly
POFTO: X:=OMSG; 0=:DCBX
       A:=CMODI; T:=HDEV+WTTC; *EXR ST
```
**Purpose**: Set transmitter control during output timeout.

7. **Line 105207** - Transmitter Clear:
```assembly
TRASET: A:=0
        T:=HDEV+WTTC; *EXR ST
```
**Purpose**: Clear transmitter (set to 0) during reset.

### WDMA - Write DMA Address (HDEV+15)

**Usage Locations Found:**
1. **Line 103204** - Data Send Setup:
```assembly
IF 9TREG/\77=1 THEN                      % Send data
   9AREG; T:=HDEV+15; *IOF; EXR ST
```
**Purpose**: Set DMA address for data transmission.

2. **Line 103261** - Control Block Send:
```assembly
IF A=55 THEN                           % Send control block
   9AREG; T:=HDEV+15; *IOF; EXR ST
```
**Purpose**: Set DMA address for control block transmission.

3. **Line 103302** - Initialization:
```assembly
IF A=52 THEN                        % Initialize
   9AREG; T:=HDEV+15; *IOF; EXR ST
```
**Purpose**: Set DMA address during initialization.

4. **Line 103705** - **TRANSMITTER DMA START**:
```assembly
XHMST: LIINT+DPITPHYS;
XXHMST:T:=HDEV+WDMA; *IOF; EXR ST        % List address
       A:=2000\/D; T+"WDCR-WDMA"; *EXR ST % Start transmitter
```
**Purpose**: **Set DMA list address for transmission** (LIINT+DPITPHYS = physical address).

5. **Line 104277** - **RECEIVER DMA START**:
```assembly
LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST    % Start receiver
A:=1001; T+"WDCR-WDMA"; *EXR ST          % Write command
```
**Purpose**: **Set DMA list address for reception** (LIINT+DPITPHYS = physical address).

6. **Line 105121** - Initialization DMA:
```assembly
A:=XINITA+DPITPHYS; T:=HDEV+WDMA; *IOF; EXR ST
401; T+"WDCR-WDMA";*EXR ST
```
**Purpose**: Set DMA address during HDLC initialization.

## Critical Usage Patterns

### 1. DMA Operation Sequence
```assembly
% Standard DMA Start Pattern:
LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST    % Set DMA address
A:=command; T+"WDCR-WDMA"; *EXR ST       % Write DMA command + trigger
T+"RDCR-WDCR"; X:=-timeout; *EXR ST      % Read back and set timeout
CALL LTOUT; *JAF *-2                     % Link timeout handling
```

### 2. Interrupt Handler Pattern
```assembly
% Transmitter Interrupt:
HOINT: T:=HDEV+RTTS; *EXR ST             % Read status
       A=:HASTAT                         % Store status
       IF T:=ACTSW = 0 THEN MIN DUIN; P+0 FI % Count retries if unexpected

% Receiver Interrupt:  
HIINT: T:=HDEV+RRTS; *EXR ST             % Read status
       A=:HASTAT                         % Store status
       IF T:=ACTSW = 0 THEN MIN T9; P+0 FI % Count dummy interrupts
```

### 3. Control Register Values
```assembly
% WRTC Values Used:
100 (octal 144) = 0x40  = Basic receiver enable
140 (octal 214) = 0x60  = Maintenance mode  
1734 (octal)    = 0x3DC = Full DMA receiver mode
MAINT\/100      = Maintenance flags OR'd with 100

% WTTC Values Used:
0               = Transmitter off
1134+CMODI      = DMA transmission start (1134 octal + mode)
CMODI           = Mode-specific control
HCTRL           = Hardware control register value
```

## Summary

**Primary HDLC Register Usage:**
1. **RRTS (HDEV+10)** - Read in receiver interrupt (HIINT) and status checks
2. **RTTS (HDEV+12)** - Read in transmitter interrupt (HOINT) and DMA monitoring  
3. **WRTC (HDEV+11)** - Configure receiver DMA modes (100, 140, 1734)
4. **WTTC (HDEV+13)** - Configure transmitter DMA (0, 1134+CMODI)
5. **WDMA (HDEV+15)** - Set DMA list addresses (LIINT+DPITPHYS)

**Key Insight**: The SINTRAN code shows extensive use of the DMA-related registers (10-17) but **minimal use of basic data registers (0-7)**, confirming this is a **DMA-based HDLC implementation** rather than character-mode operation.