# DMA Send and Receive Pseudocode - Complete SINTRAN Analysis

## DMA Send Operation (HOINT - Transmitter Interrupt)

### Complete Transmitter Interrupt Handler (Line 104034)
```assembly
SUBR HOINT                                    % Line 104033 - Transmitter interrupt
    INTEGER TMR:=0                           % Timer register
    INTEGER HASTAT                           % Hardware status storage
    INTEGER ACTSW                            % Activity switch
    INTEGER TELL:=0                          % Interrupt counter
    INTEGER DUIN:=0                          % Retry counter (dummy interrupts)
    INTEGER SNDAC:=0                         % AC field of buffer to transmit
    INTEGER ARRAY BUFF0(BUFSIZ)              % Frame data logging
    INTEGER ARRAY BUFF1(BUFSIZ)              % Device number logging
    INTEGER ARRAY BUFF2(BUFSIZ)              % Device status logging
    INTEGER CMODI                            % Communication mode
    
    HOINT: 
        % Step 1: Reset timer
        0=:TMR                               % Reset timer
        
        % Step 2: Read transmitter status - IOX+12
        T:=HDEV+RTTS; *EXR ST                % READ TRANSMITTER TRANSFER STATUS
        A=:HASTAT                            % Store status in HASTAT variable
        
        % Step 3: Check if unexpected interrupt
        IF T:=ACTSW = 0 THEN                 % Device not active?
           MIN DUIN; P+0; CALL WT12 FI       % Increment retry counter (DUIN)
           
        % Step 4: Mark device inactive  
        0=: ACTSW                            % Clear activity switch
        
        % Step 5: Turn off RQTS if X.21 mode
        IF CMODI = 40 THEN                   % X.21 mode?
           T:=HDEV+WTTC; *EXR ST             % IOX+13 - Turn off RQTS signal
        FI
        
        % Step 6: Increment interrupt counter for logging
        GO HNOTRA                            % Patch for trace (if enabled)
        MIN TELL; P+0                        % Increment interrupt counter
        TELL /\ TELMA=:TELL                  % Wrap counter (TELMA = BUFSIZ-1)
        
        % Step 7: Log transmission data for debugging
        A:=SNDAC                             % Get first word in transmitted frame
        X:=TELL; *1BANK
        A=:BUFF0(X); *2BANK                  % Log first word in frame
        HDEV; *1BANK  
        A=:BUFF1(X); *2BANK                  % Log device number (HDEV)
        HASTAT; *1BANK
        A=:BUFF2(X); *2BANK                  % Log device status (HASTAT)
        
    HNOTRA: % Trace patch entry point
        X:=OMSG                              % Get current message pointer
        
        % Step 8: **CRITICAL SUCCESS/ERROR CHECK**
        IF A/\ "SILFO+TXUND" = 0 THEN        % Line 104102 - Check for success
           % SUCCESS PATH:
           % SILFO = 0x8000 (bit 15) = Illegal format
           % TXUND = 0x0002 (bit 1)  = Transmitter underrun
           % Combined mask = 0x8002
           % If (HASTAT & 0x8002) == 0, transmission was successful
           
           XRETRY=:RTDYN; A:=0; CALL SADTS   % No retry needed, clear error
        ELSE
           % ERROR PATH:
           % Either Illegal format OR Transmitter underrun occurred
           A:=HASTAT; CALL SADTS; CALL DRERR % Handle error, may retry
           A:=EUND                           % Set underrun error code
        FI
        
        % Step 9: Complete transmission
        0=:DCBX; GO FAR BACKX                % Clear current DCB, return to message handler
RBUS
```

## DMA Receive Operation (HIINT - Receiver Interrupt)  

### Complete Receiver Interrupt Handler (Line 104436)
```assembly
SUBR HIINT                                   % Line 104436 - Receiver interrupt
    INTEGER HASTAT                           % Hardware status storage  
    INTEGER ACTSW                            % Activity switch
    INTEGER STPCNT                           % Stop counter
    INTEGER TELL:=0                          % Interrupt counter
    INTEGER T9:=0                            % Dummy interrupt counter
    INTEGER T1:=0, T2:=0                     % Temp variables for list copying
    INTEGER LISTP, LIINT                     % DMA list pointers
    INTEGER ARRAY BUFF1(BUFSIZ)              % Device number logging
    INTEGER ARRAY BUFF2(BUFSIZ)              % Device status logging
    INTEGER ARRAY BUFF3(11)                  % List keys when stopped
    INTEGER ARRAY POINTER P0BUFF:=BUFF0      % Frame data pointer
    
    HIINT:
        % Step 1: Read receiver status - IOX+10  
        T:=HDEV+RRTS; *EXR ST                % READ RECEIVER TRANSFER STATUS
        A=:HASTAT                            % Store status in HASTAT variable
        
        % Step 2: Check if unexpected interrupt
        IF T:=ACTSW = 0 THEN                 % Not expecting data?
           MIN T9; P+0; GO OUT1 FI           % Increment dummy interrupt counter, exit
           
        % Step 3: **X.21 ERROR PROCESSING**
        IF A/\ HX21M >< 0 THEN               % X.21 error bits (13-14)?
           % HX21M = 0x6000 = bits 13-14 = X.21 modem status
           T:=2000; X:=LIINT+T; T:=X.LKEY    % Get DMA list key
           A\/ LIINT.LKEY=:X.LKEY            % Save error status in list
           
           IF A BIT HX21S THEN               % X.21 clear indication?
              % HX21S = 0x000E = bits 1-3 = X.21 receiver state
              HASTAT BONE BLDON=:HASTAT      % Set block done bit
              LIINT.LKEY BONE XBLDN=:X.LKEY  % Set XBLDN to terminate
           FI
        FI
        
        % Step 4: **LIST EMPTY PROCESSING - CRITICAL**
        IF HASTAT/\"EMTY" >< 0 THEN          % List empty (bit 11)?
           % EMTY = 0x0800 = bit 11 = List Empty
           % This STOPS the receiver completely!
           0=:ACTSW                          % Stop device
           MIN STPCNT                        % Increment stop counter (STPCNT)
           P+0                               % Skip return
           
           % Save DMA list state when stopped for debugging
           LISTP=:T1; 0=:T2
           DO WHILE T1.LKEY >< "NLP"         % Copy DMA list keys
              X:=T2; *1BANK
              A=:BUFF3(X); *2BANK            % Save list key
              MIN T2; 0/\0; T1+4=:T1
           OD
        FI
        
    MORE: % Main receive processing loop
        % Step 5: **BLOCK PROCESSING - CRITICAL**
        A:=2000; X:=LIINT+A; X.LKEY          % Get current list entry
        A:=LIINT.LKEY=:D                     % Store key in D register
        
        IF A NBIT XBLDN THEN                 % No more filled blocks (BlockEnd=0)?
           % XBLDN = 0x0008 = bit 3 = Block done
           % If BlockEnd NOT set, no more blocks to process
           IF A = "ERB" THEN GO FAR ZSTARC FI % If empty, enable receiver
           GO FAR OUT1                       % Exit processing
        FI
        
        % Step 6: Log received data for debugging
        GO HNOTRA                            % Trace patch
        MIN TELL; P+0                        % Increment interrupt counter
        TELL /\ TELMA=:TELL=:X               % Wrap counter, use as index
        
        HDEV; *1BANK
        A=:BUFF1(X); *2BANK                  % Log device number
        HASTAT; *1BANK  
        A=:BUFF2(X); *2BANK                  % Log device status (HASTAT)
        
        % Step 7: Get frame data
        T:=LIINT.LMEM1; X:=X.LMEM2; *AC@3 LDATX % Get memory pointers
        X:=TELL; *1BANK
        A=:P0BUFF(X); *2BANK                 % Log first word of received frame
        
    HNOTRA: % Trace patch entry point
        % Step 8: Get received message information
        X:=LIINT; A:=2000; X+A; X.DLSTS      % Get DMA list status
        A:=LIINT.LBYTC+DISP1=:T              % Get received message size
        
        % Step 9: Find corresponding DCB (Data Control Block)
        CALL XMPAT                           % Find DCB from DCB list
        X=:L; A:=LIINT.LKEY=:D; 0=:X.LKEY; 0=:X.LMEM2; X:=L
        
        % Step 10: **FRAME STATUS PROCESSING**
        IF A /\ "LMASK" = 3 THEN             % Good frame received?
           A:=0; CALL SCRET; CALL SADTS      % Success - clear error, store status
        ELSE
           % Frame error processing
           IF A BIT HX21S THEN EX21 ELSE EINP FI % X.21 or input error
           CALL SCRET                        % Set return code
           A:=D; CALL SADTS; A\/DSTAT=:DSTAT % Store status, update error flags
           HDERC+1=:HDERC                    % Increment error counter
        FI
        
        % Step 11: Send message back to user
        X-BHEAD; CALL OCHAIN                 % Queue message for user
        
        % Step 12: Move to next DMA list entry
        LIINT+4=:LIINT                       % Advance to next list entry
        A+2000; A.LKEY                       % Check next entry
        IF LIINT.LKEY=NLP THEN               % End of list?
           LISTP=:LIINT                      % Reset to start of list
        FI
        
        GO FAR HDSTA                         % Check for more messages
        
    OUT1: % Exit sequence
        % Step 13: **RECEIVER CONTROL UPDATE**
        A:="1734"\/MAINT/\HXDOK              % Calculate control value
        % 1734 (octal) = 1244 decimal = 0x04DC
        T:=HDEV+WRTC; * EXR ST               % IOX+11 - Write receiver control
        GO FAR HDSTA                         % Check for more messages
RBUS
```

## Additional Registers Read After RRTS/RTTS

### After RTTS Read (Line 104053):
```assembly
% Additional register operations in transmitter interrupt:
T:=HDEV+WTTC; *EXR ST                       % IOX+13 - Turn off RQTS
```

### After RRTS Read (Line 104321):  
```assembly
% Additional register operations in receiver interrupt:
T:=HDEV+WRTC; * EXR ST                      % IOX+11 - Update receiver control
```

## Critical Status Bit Analysis

### What SINTRAN Cares About After RTTS Read:
1. **SILFO (0x8000, bit 15)** - Illegal format error
2. **TXUND (0x0002, bit 1)** - Transmitter underrun error
3. **Combined check: (HASTAT & 0x8002) == 0** means SUCCESS
4. **If error bits set**: Increment retry counter, may retransmit

### What SINTRAN Cares About After RRTS Read:
1. **HX21M (0x6000, bits 13-14)** - X.21 modem error status
2. **HX21S (0x000E, bits 1-3)** - X.21 receiver state bits
3. **EMTY (0x0800, bit 11)** - List empty (STOPS receiver completely)
4. **XBLDN (0x0008, bit 3)** - Block done (more blocks available)
5. **LMASK bits** - Frame validation mask

## DMA Setup Operations - Complete WDCR/RDCR Usage

### 1. Transmitter DMA Start (XHMST - Line 103705):
```assembly
SUBR XHMST                                   % Start transmitter DMA
    XHMST: LIINT+DPITPHYS;                   % Calculate physical DMA list address
    XXHMST:T:=HDEV+WDMA; *IOF; EXR ST        % IOX+15 - Write DMA address (least significant)
           
           % DMA Command: Start Transmitter + Command Value
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

### 2. Receiver DMA Start (ZSTARC - Line 104270):
```assembly
SUBR ZSTARC                                  % Start receiver DMA
    ZSTARC: IF ACTSW = 0 THEN                % Device not active?
               % Clear receiver state
               HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST  % IOX+11 - Clear old garbage
               
               % Set DMA Address
               % ADD DPITPHYS TO GET PHYSICAL ADDRESS
               LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST % IOX+15 - Write DMA address
               
               % DMA Command: Start Receiver 
               A:=1001; T+"WDCR-WDMA"; *EXR ST      % IOX+17 - Write DMA command + trigger
               % 1001 (octal) = 513 decimal = 0x201 = Start receiver DMA
               
               % Verify DMA Status
               T+"RDCR-WDCR"; X:=-10; *EXR ST       % IOX+16 - Read DMA status  
               CALL LTOUT; *JAF *-2                 % Link timeout with 10 count timeout
               
               1=:ACTSW                             % Mark device active
            FI
RBUS
```

### 3. Initialization DMA Setup (Line 105121):
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

### 4. X.21 DMA Operations (Line 110754):
```assembly
SUBR X21_DMA_SETUP                           % X.21 HDLC DMA setup
    % Clear DMA
    T:=X2DHD+XWDCR; *EXR ST                  % IOX+17 - Clear DMA command register
    
    % Wait for DMA Ready
    X:=-30; T:=X2DHD+XRDCR; *EXR ST          % IOX+16 - Read DMA status with timeout
    *JPC TODMA; JAF *-2                      % Jump on timeout, retry with 30 count
    
    % Initialize X.21 DMA
    A:=X2DUI.XINITA+DPITPHYS                 % Calculate physical address
    T:=X2DHD+XWDMA; *IOF; EXR ST             % IOX+15 - Write DMA address
    401; T+"XWDCR-XWDMA"; *EXR ST            % IOX+17 - Write DMA command (401 octal)
    
    % Verify X.21 DMA Status
    X:=-30; T+"XRDCR-XWDCR"; *EXR ST         % IOX+16 - Read status with 30 timeout
    *JPC TODMA; JAF *-2                      % Jump on timeout
    
    % Start X.21 Receiver
    X.LIINT+DPITPHYS; T:=X2DHD+XWDMA; *EXR ST % IOX+15 - Set receiver DMA address
    A:=1001; T+"XWDCR-XWDMA"; *EXR ST        % IOX+17 - Start receiver (1001 octal)
    X:=-30; T+"XRDCR-XWDCR"; *EXR ST         % IOX+16 - Verify with 30 timeout
    *JPC TODMA; JAF *-2
RBUS
```

### 5. DMA Clear Operation (Line 104747):
```assembly
SUBR SMCLEAR_DMA                             % Clear DMA during shutdown
    SMCLEAR: T:=HDEV+WTCR; A:=2; *EXR ST     % IOX+7 - Send EOM to avoid issues
             * TRR 10; IOF                   % Clear cache
             A:=100; T:=HDEV+WRTC; *EXR ST   % IOX+11 - Clear receiver  
             A:=140; *EXR ST                 % IOX+11 - Set maintenance mode
             T+"WDCR-WRTC";*EXR ST; ION      % IOX+17 - Clear DMA command register
RBUS
```

## WDCR Command Values Analysis

### DMA Command Values Used:
```assembly
% Transmitter Commands:
2000 (octal) = 1024 decimal = 0x400  % Start transmitter DMA

% Receiver Commands:  
1001 (octal) = 513 decimal = 0x201   % Start receiver DMA

% Initialization Commands:
401 (octal) = 257 decimal = 0x101    % Initialize DMA

% Clear Commands:
0 (written implicitly)              % Clear/stop DMA
```

## RDCR Status Check Pattern

### Standard DMA Verification Sequence:
```assembly
% Pattern used throughout SINTRAN:
T+"RDCR-WDCR"; X:=-timeout; *EXR ST  % IOX+16 - Read DMA status
CALL LTOUT; *JAF *-2                 % Link timeout, retry if timeout
% Where timeout is:
% -10 = 10 count timeout (receiver)
% -20 = 20 count timeout (transmitter) 
% -30 = 30 count timeout (X.21)
```

## Key Insights

1. **SINTRAN does NOT check DMAModuleRequest bit** - it relies on success/error bits
2. **Transmitter success** = `(HASTAT & 0x8002) == 0` (no SILFO or TXUND)  
3. **Receiver stops** if EMTY (ListEmpty) bit set - critical for emulator
4. **Block processing continues** only if XBLDN (BlockEnd) bit set
5. **Multiple status registers** updated after main status read (WTTC, WRTC)