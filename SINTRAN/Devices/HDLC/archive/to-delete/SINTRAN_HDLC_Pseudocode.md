# SINTRAN HDLC Driver Pseudocode Analysis

## HDLC Driver Structure with Register Names and Variables

### Main HDLC Entry Point
```assembly
SUBR HDLC                                    % Line 102772
    INTEGER MESSM:=0                         % Line 102772 - Message counter  
    INTEGER RSCUR                            % Current message pointer
    INTEGER MESSID                           % Message ID
    INTEGER XXUBF, XXSBF, XXSBK             % Buffer pointers
    
    HDLC: A:=L=:"RSRET"                      % Save return address
          IF HINIF >< -1 THEN                % Pool initiated?
             MASTB =: XXSBK                  % Bank number to copy routine
             MAX -1 SHZ -1 + BUFST =: D      % Buffer calculations
             A:=BUFST
             X:=B; T:=MASTB; *AAX XBBNK      % Set bank
             CALL ZBINI; CALL PZCRA          % Initialize pools
             -1=:HINIF                       % Mark as initiated
          FI
          
          IF DDD0 = FSEND THEN               % Send operation?
             % SEND MESSAGE PATH
             IF DDD3 < 0 THEN T:=EMSGS; GO RSRET FI
             IF A > DDD4 THEN T:=EMAXS; GO RSRET FI
             % Allocate buffer and queue message
             CALL ICHAIN                     % Queue input message
             CALL STDEV                      % Activate driver
          ELSE
             % RECEIVE MESSAGE PATH  
             CALL DOCHAIN; GO CHEMTY         % Remove from output queue
             % Process received message
          FI
RBUS
```

### HDLC Transmitter Interrupt Handler
```assembly
SUBR HOINT                                   % Line 104033 - Transmitter interrupt
    INTEGER TMR:=0                           % Timer register
    INTEGER HASTAT                           % Hardware status storage
    INTEGER ACTSW                            % Activity switch
    INTEGER TELL:=0                          % Interrupt counter
    INTEGER SNDAC:=0                         % AC field of buffer to transmit
    INTEGER ARRAY BUFF0(BUFSIZ)              % Frame data buffer
    INTEGER ARRAY BUFF1(BUFSIZ)              % Device number buffer  
    INTEGER ARRAY BUFF2(BUFSIZ)              % Device status buffer
    
    HOINT: 0=:TMR                            % Reset timer
           T:=HDEV+RTTS; *EXR ST             % IOX+12 - Read Transmitter Transfer Status
           A=:HASTAT                         % Store status in HASTAT
           
           IF T:=ACTSW = 0 THEN              % Device not active?
              MIN DUIN; P+0; CALL WT12 FI    % Increment retry counter (DUIN)
              
           0=: ACTSW                         % Clear activity switch
           
           IF CMODI = 40 THEN                % X.21 mode?
              T:=HDEV+WTTC; *EXR ST         % IOX+13 - Turn off RQTS
           FI
           
           GO HNOTRA                         % Patch for trace
           MIN TELL; P+0                     % Increment interrupt counter
           TELL /\ TELMA=:TELL               % Wrap counter
           
           % Log transmission data
           A:=SNDAC                          % Get first word in frame
           X:=TELL; *1BANK
           A=:BUFF0(X); *2BANK              % Log first word in frame
           HDEV; *1BANK  
           A=:BUFF1(X); *2BANK              % Log device number
           HASTAT; *1BANK
           A=:BUFF2(X); *2BANK              % Log device status
           
           % Process transmission completion
           IF A/\ "SILFO+TXUND" = 0 THEN    % Line 104102 - Success check
              XRETRY=:RTDYN; A:=0; CALL SADTS % No retry needed
           ELSE
              A:=HASTAT; CALL SADTS; CALL DRERR % Handle error
           FI
RBUS
```

### HDLC Receiver Interrupt Handler
```assembly
SUBR HIINT                                   % Line 104436 - Receiver interrupt
    INTEGER HASTAT                           % Hardware status storage
    INTEGER ACTSW                            % Activity switch  
    INTEGER STPCNT                           % Stop counter
    INTEGER TELL:=0                          % Interrupt counter
    INTEGER T9:=0                            % Dummy interrupt counter
    INTEGER T1:=0, T2:=0                     % Temp variables
    INTEGER LISTP, LIINT                     % DMA list pointers
    INTEGER ARRAY BUFF1(BUFSIZ)              % Device number buffer
    INTEGER ARRAY BUFF2(BUFSIZ)              % Device status buffer
    INTEGER ARRAY BUFF3(11)                  % List keys when stopped
    INTEGER ARRAY POINTER P0BUFF:=BUFF0      % Buffer pointer
    
    HIINT: T:=HDEV+RRTS; *EXR ST             % IOX+10 - Read Receiver Transfer Status
           A=:HASTAT                         % Store status in HASTAT
           
           IF T:=ACTSW = 0 THEN              % Not expecting anything?
              MIN T9; P+0; GO OUT1 FI        % Increment dummy interrupt counter
              
           % X.21 Error Processing
           IF A/\ HX21M >< 0 THEN            % X.21 error bits (13-14)?
              T:=2000; X:=LIINT+T; T:=X.LKEY % Get DMA list key
              A\/ LIINT.LKEY=:X.LKEY         % Save error status
              IF A BIT HX21S THEN            % X.21 clear indication?
                 HASTAT BONE BLDON=:HASTAT   % Set block done
                 LIINT.LKEY BONE XBLDN=:X.LKEY % Set XBLDN to terminate
              FI
           FI
           
           % List Empty Processing
           IF HASTAT/\"EMTY" >< 0 THEN       % List empty (bit 11)?
              0=:ACTSW                       % Stop device
              MIN STPCNT                     % Increment stop counter
              P+0                            % Skip return
              % Save DMA list state when stopped
              LISTP=:T1; 0=:T2
              DO WHILE T1.LKEY >< "NLP"      % Copy DMA list
                 X:=T2; *1BANK
                 A=:BUFF3(X); *2BANK         % Save list keys
                 MIN T2; 0/\0; T1+4=:T1
              OD
           FI
           
    MORE: % Process received blocks
           A:=2000; X:=LIINT+A; X.LKEY       % Get list entry
           A:=LIINT.LKEY=:D
           
           IF A NBIT XBLDN THEN              % No more filled blocks (BlockEnd=0)?
              IF A = "ERB" THEN GO FAR ZSTARC FI % Enable receiver if empty
              GO FAR OUT1                    % Exit processing
           FI
           
           GO HNOTRA                         % Trace patch
           MIN TELL; P+0                     % Increment interrupt counter
           TELL /\ TELMA=:TELL=:X            % Wrap and use as index
           
           % Log received data
           HDEV; *1BANK
           A=:BUFF1(X); *2BANK              % Log device number
           HASTAT; *1BANK  
           A=:BUFF2(X); *2BANK              % Log device status
           T:=LIINT.LMEM1; X:=X.LMEM2; *AC@3 LDATX % Get frame data
           X:=TELL; *1BANK
           A=:P0BUFF(X); *2BANK             % Log first word in frame
           
           % Process the received frame
           CALL XMPAT                       % Find DCB entry
           % Continue with frame processing...
           
    OUT1: % Exit sequence
           A:="1734"\/MAINT/\HXDOK          % Line 104316
           T:=HDEV+WRTC; * EXR ST           % IOX+11 - Write Receiver Transfer Control
           GO FAR HDSTA                     % Check for more messages
RBUS
```

### Register Operation Functions
```assembly
% IOX Register Operations (from hardware interface)
HDEV+RRDR     % IOX+0  - Read Receiver Data Register
HDEV+WPCR     % IOX+1  - Write Parameter Control Register  
HDEV+RRS      % IOX+2  - Read Receiver Status
HDEV+WSAR     % IOX+3  - Write Sync/Address Register
HDEV+WCHL     % IOX+4  - Write Character Length
HDEV+WTDR     % IOX+5  - Write Transmitter Data Register
HDEV+RTSR     % IOX+6  - Read Transmitter Status Register
HDEV+WTCR     % IOX+7  - Write Transmitter Control Register
HDEV+RRTS     % IOX+10 - Read Receiver Transfer Status
HDEV+WRTC     % IOX+11 - Write Receiver Transfer Control
HDEV+RTTS     % IOX+12 - Read Transmitter Transfer Status  
HDEV+WTTC     % IOX+13 - Write Transmitter Transfer Control
HDEV+RDMA     % IOX+14 - Read DMA Address (Least)
HDEV+WDMA     % IOX+15 - Write DMA Address (Least)
HDEV+RDCR     % IOX+16 - Read DMA Command Register
HDEV+WDCR     % IOX+17 - Write DMA Command Register + Trigger
```

### DMA Control Functions
```assembly
SUBR ZSTARC                                  % Line 104270 - Start receiver
    ZSTARC: IF ACTSW = 0 THEN                % Device not active?
               HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST % Clear old garbage
               LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST % Start receiver DMA
               A:=1001; T+"WDCR-WDMA"; *EXR ST % Write DMA command
               T+"RDCR-WDCR"; X:=-10; *EXR ST  % Read back command
               CALL LTOUT; *JAF *-2            % Link timeout
               1=:ACTSW                        % Mark device active
            FI
RBUS

% DMA Buffer Management
SUBR HDREC                                   % Line 104171 - Process received data
    HDREC: IF INTSTA >< 2 THEN A:=ENINIT; GO FAR BACKX FI
           X-BHEAD; CALL RBYTC; X+BHEAD; A-BCHEA  % Get byte count
           IF A < MAXR THEN A:=ETOSM; GO FAR BACKX FI
           CALL HXST                         % Process frame
           A:=X+CHEAD=:D:=MASTB             % Buffer address
           % Continue processing...
RBUS
```

### Status Bit Constants (from SYMBOL-1-LIST.SYMB.TXT)
```assembly
% Receiver Status Bits (RRTS register)
EMTY=004000      % 0x0800 = bit 11 - List Empty (stops receiver)
HX21M=060000     % 0x6000 = bits 13-14 - X.21 modem status
HX21S=000016     % 0x000E = bits 1-3 - X.21 clear indication  
BLDON=000010     % 0x0008 = bit 3 - Block done
XBLDN=000010     % 0x0008 = bit 3 - Block end (same as BLDON)

% Transmitter Status Bits (RTTS register)  
SILFO=100000     % 0x8000 = bit 15 - Illegal format
TXUND=000002     % 0x0002 = bit 1 - Transmitter underrun
```

### Buffer and Message Management
```assembly
% Buffer Structure (from analysis)
MASTB            % Bank number for buffer operations
BUFST            % Buffer start address  
MAX              % Maximum buffer size
BHEAD            % Buffer header size
BUFSIZ=20        % Buffer size constant
TELMA=BUFSIZ-1   % Counter wrap mask

% Message Queue Operations
SUBR ICHAIN, OCHAIN                          % Line 21716
    ICHAIN: IF IQUEU =0 THEN                 % Input queue empty?
               X=:IQUEU; T:=MASTB; * BCHAI@3 STATX % Link message
            FI
    OCHAIN: % Similar for output queue
RBUS

% Message Processing
SUBR DOCHAIN, DICHAIN                        % Line 21766
    DOCHAIN: IF OQUEU =0 THEN                % Output queue empty?
                EXIT                         % Nothing to process
             FI
             X:=A; T:=MASTB; * BCHAI@3 LDATX % Get next message
             A=:OQUEU; 0=:ISTATE; EXITA     % Update queue
RBUS
```

## Summary of Key Functions:

1. **HDLC** (102772) - Main entry point for send/receive operations
2. **HOINT** (104033) - Transmitter interrupt handler with TELL counter
3. **HIINT** (104436) - Receiver interrupt handler with STPCNT, T9 counters  
4. **ZSTARC** (104270) - Start receiver DMA operation
5. **HDREC** (104171) - Process received frame data
6. **ICHAIN/OCHAIN** (21716) - Message queue management
7. **DOCHAIN/DICHAIN** (21766) - Message dequeue operations

The driver uses IOX+10 (RRTS) and IOX+12 (RTTS) for status, IOX+11 (WRTC) and IOX+13 (WTTC) for control, and maintains counters for interrupts, stops, and retries.