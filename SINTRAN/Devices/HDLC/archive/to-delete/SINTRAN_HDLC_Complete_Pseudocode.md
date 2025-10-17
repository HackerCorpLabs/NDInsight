# SINTRAN HDLC Complete Pseudocode - Based on Register Usage Analysis

## HDLC Register Map (HDEV+ Offsets)
```assembly
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

## Critical Constants (from SYMBOL-1-LIST.SYMB.TXT)
```assembly
% Status Bit Constants:
TXUND = 000002  % 0x0002, bit 1  - Transmitter Underrun
SILFO = 100000  % 0x8000, bit 15 - Illegal Format/Key Error  
EMTY  = 004000  % 0x0800, bit 11 - List Empty (No Buffers)
HX21M = 060000  % 0x6000, bits 13-14 - X.21 Error Mask
HX21S = 000016  % 0x000E, bits 1,2,3 - Receiver State Check
BLDON = 000010  % 0x0008, bit 3  - Block Done Flag

% DMA Command Values:
% Transmitter: 2000 (octal) = 1024 decimal = 0x400
% Receiver:    1001 (octal) = 513 decimal = 0x201  
% Initialize:  401 (octal) = 257 decimal = 0x101

% Control Values:
% WRTC: 100 (basic), 140 (maintenance), 1734 (DMA mode)
% WTTC: 0 (off), 1134+CMODI (DMA transmission)
```

## 1. DMA Transmitter Operations

### Transmitter DMA Start (XHMST - Line 103705)
```assembly
SUBR XHMST                                   % Start transmitter DMA
    XHMST: LIINT+DPITPHYS;                   % Calculate physical DMA list address
    XXHMST:T:=HDEV+WDMA; *IOF; EXR ST        % IOX+15 - Write DMA address (least)
           
           % DMA Command: Start Transmitter  
           A:=2000\/D; T+"WDCR-WDMA"; *EXR ST % IOX+17 - Write DMA command + trigger
           % 2000 (octal) = 1024 decimal = 0x400 = Start transmitter DMA
           
           % Verify DMA Status
           T+"RDCR-WDCR"; X:=-20;*EXR ST     % IOX+16 - Read DMA status
           CALL LTOUT; *JAF *-2; ION         % Link timeout with 20 count timeout
           
           % Enable Transmission
           1134+CMODI; T:=HDEV+WTTC; *EXR ST % IOX+13 - Write transmitter control
           % 1134 (octal) = 604 decimal = 0x25C = DMA transmit enable + mode
           
           1 =: ACTSW                        % Mark device active
           OMSG =: DCBX                      % Set current DCB
RBUS
```

### Transmitter Interrupt Handler (HOINT - Line 104033)
```assembly
SUBR HOINT                                   % Transmitter interrupt handler
    HOINT: 0=:TMR                            % Reset timer
           T:=HDEV+RTTS; *EXR ST             % IOX+12 - Read transmitter status
           A=:HASTAT                         % Store status in HASTAT variable
           
           % Check for unexpected interrupt
           IF T:=ACTSW = 0 THEN              % Device not active?
              MIN DUIN; P+0; CALL WT12       % Count dummy interrupt, retry
              GO OUT1                        % Exit without processing
           FI
           
           % SUCCESS CHECK - CRITICAL LOGIC
           IF A/\ "SILFO+TXUND" = 0 THEN     % (status AND 0x8002) == 0
              % SUCCESS: Neither illegal format nor underrun
              XRETRY=:RTDYN; A:=0; CALL SADTS % Clear retry count, log success
              
              % Process successful transmission
              IF CMODI = 40 THEN             % RQTS mode?
                 T:=HDEV+WTTC; *EXR ST       % IOX+13 - Turn off RQTS
              FI
              
              % Mark completion
              0=:ACTSW                       % Mark device inactive
              CALL NEXTS                     % Continue with next frame
              
           ELSE
              % TRANSMISSION ERROR
              XRETRY+1=:XRETRY               % Increment retry counter
              
              IF XRETRY > MAXRETRY THEN      % Too many retries?
                 A:=237; CALL DRERR          % Set error 237 (transmission failure)
                 0=:ACTSW                    % Stop device
              ELSE
                 % Retry transmission
                 CALL XHMST                  % Restart DMA
              FI
           FI
           
    OUT1: % Exit point
RBUS
```

## 2. DMA Receiver Operations

### Receiver DMA Start (ZSTARC - Line 104270) 
```assembly
SUBR ZSTARC                                  % Start receiver DMA
    ZSTARC: IF ACTSW = 0 THEN                % Device not active?
               % Clear receiver state first
               HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST  % IOX+11 - Clear old garbage
               
               % Set DMA Address  
               LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST % IOX+15 - Write DMA address
               
               % DMA Command: Start Receiver
               A:=1001; T+"WDCR-WDMA"; *EXR ST      % IOX+17 - Write DMA command + trigger  
               % 1001 (octal) = 513 decimal = 0x201 = Start receiver DMA
               
               % Verify DMA Status
               T+"RDCR-WDCR"; X:=-10; *EXR ST       % IOX+16 - Read DMA status
               CALL LTOUT; *JAF *-2                 % Link timeout with 10 count timeout
               
               % Enable receiver with full DMA mode
               1734\/MAINT/\HXDOK; T:=HDEV+WRTC; *EXR ST % IOX+11 - Enable reception
               % 1734 (octal) = 988 decimal = 0x3DC = Full DMA receiver mode
               
               1=:ACTSW                             % Mark device active
            FI
RBUS
```

### Receiver Interrupt Handler (HIINT - Line 104436)
```assembly
SUBR HIINT                                   % Receiver interrupt handler  
    HIINT: T:=HDEV+RRTS; *EXR ST             % IOX+10 - Read receiver status
           A=:HASTAT                         % Store status in HASTAT variable
           
           % Check for unexpected interrupt
           IF T:=ACTSW = 0 THEN              % Device not active?
              MIN T9; P+0                    % Count dummy interrupt
              GO OUT1                        % Exit without processing
           FI
           
           % PRIMARY STATUS PROCESSING
           
           % 1. X.21 ERROR CHECK (bits 13-14)
           IF A/\ HX21M >< 0 THEN            % X.21 error detected (bits 13-14)?
              IF A BIT HX21S THEN            % Check receiver state (bits 1,2,3)
                 HASTAT BONE BLDON=:HASTAT   % Set block done, terminate cleanly
              FI
              CALL X21ERR                    % Handle X.21 protocol error
              GO OUT1                        % Exit
           FI
           
           % 2. LIST EMPTY CHECK (bit 11) - CRITICAL
           IF HASTAT/\"EMTY" >< 0 THEN       % List empty (no receive buffers)?
              0=:ACTSW                       % Stop device immediately
              STPCNT+1=:STPCNT               % Increment stop counter
              GO OUT1                        % Exit - all packets dropped
           FI
           
           % 3. DATA AVAILABLE CHECK (bit 0) - CRITICAL  
           IF A NBIT 0 OR A/\60000><0 THEN   % No data OR additional X.21 error?
              GO OUT1                        % Drop packet, exit
           FI
           
           % PACKET PROCESSING - Only reached if all checks pass
           CALL PROCPKT                      % Process received packet
           
           % Continue receiving
           IF ACTSW >< 0 THEN                % Still active?
              CALL ZSTARC                    % Restart receiver DMA
           FI
           
    OUT1: % Exit point
RBUS
```

## 3. DMA Initialization Operations

### HDLC Initialization DMA Setup (Line 105121)
```assembly
SUBR HDSIN_DMA_INIT                          % HDLC initialization DMA setup
    % Set DMA Address for Initialization
    A:=XINITA+DPITPHYS; T:=HDEV+WDMA; *IOF; EXR ST % IOX+15 - Write init DMA address
    
    % DMA Command: Initialize
    401; T+"WDCR-WDMA";*EXR ST               % IOX+17 - Write DMA command + trigger
    % 401 (octal) = 257 decimal = 0x101 = Initialize DMA
    
    % Verify DMA Status
    T+"RDCR-WDCR"; X:=-10;*EXR ST            % IOX+16 - Read DMA status
    CALL LTOUT; * JAF *-2                    % Link timeout with 10 count timeout
RBUS
```

### DMA Clear Operation (Line 104747)
```assembly
SUBR SMCLEAR_DMA                             % Clear DMA during shutdown
    SMCLEAR: T:=HDEV+WTCR; A:=2; *EXR ST     % IOX+7 - Send EOM to avoid issues
             * TRR 10; IOF                   % Clear cache
             A:=100; T:=HDEV+WRTC; *EXR ST   % IOX+11 - Clear receiver
             A:=140; *EXR ST                 % IOX+11 - Set maintenance mode
             T+"WDCR-WRTC";*EXR ST; ION      % IOX+17 - Clear DMA command register
RBUS
```

## 4. Status Bit Analysis - What Code Cares About After RRTS/RTTS

### RTTS (Transmitter Status) - Post-Read Processing:
```assembly
% After T:=HDEV+RTTS; *EXR ST; A=:HASTAT
% The code ONLY cares about:

1. SILFO (bit 15) + TXUND (bit 1) = 0x8002
   % SUCCESS: IF (status AND 0x8002) == 0 
   % FAILURE: IF (status AND 0x8002) != 0
   
2. No other RTTS bits are checked
   % SINTRAN does NOT check DMAModuleRequest or other status bits
   % Success/failure decision is ONLY based on SILFO+TXUND
```

### RRTS (Receiver Status) - Post-Read Processing:
```assembly  
% After T:=HDEV+RRTS; *EXR ST; A=:HASTAT
% The code checks these bits in order:

1. HX21M (bits 13-14) = 0x6000
   % X.21 protocol error detection
   
2. HX21S (bits 1,2,3) = 0x000E  
   % Receiver state check (if X.21 error detected)
   
3. EMTY (bit 11) = 0x0800
   % List empty - stops all reception if set
   
4. DataAvailable (bit 0) = 0x0001
   % Must be 1 for packet processing
   
5. Recheck X.21 (bits 13-14) with mask 60000 (octal) = 0x6000
   % Final validation before packet processing
```

## 5. Key Insights for HDLC Emulator

### Critical Success/Failure Logic:
1. **Transmitter SUCCESS**: `(RTTS & 0x8002) == 0` (no illegal format, no underrun)
2. **Receiver SUCCESS**: `(RRTS & 0x0001) != 0 && (RRTS & 0x6000) == 0` (data available, no X.21 error)
3. **Device STOP**: `(RRTS & 0x0800) != 0` (list empty - no receive buffers)

### DMA Command Values:
1. **Start Transmitter**: `WDCR = 0x400` (2000 octal)
2. **Start Receiver**: `WDCR = 0x201` (1001 octal)  
3. **Initialize**: `WDCR = 0x101` (401 octal)
4. **Clear/Stop**: `WDCR = 0x000`

### Register Usage Pattern:
```assembly
% Standard SINTRAN HDLC DMA sequence:
1. WDMA (IOX+15) - Set DMA address
2. WDCR (IOX+17) - Write command + trigger DMA
3. RDCR (IOX+16) - Read status with timeout
4. WRTC/WTTC (IOX+11/13) - Enable receiver/transmitter
5. Wait for interrupt
6. RRTS/RTTS (IOX+10/12) - Read final status in interrupt
```

**Root Cause Analysis**: The SINTRAN HDLC logic is sound. Issues stem from incorrect status bit values in hardware emulation, not from bit interpretation logic.