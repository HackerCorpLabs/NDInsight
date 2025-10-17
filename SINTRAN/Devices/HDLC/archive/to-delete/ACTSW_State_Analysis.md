# ACTSW (ACTivity SWitch) Complete State Analysis

## Overview
ACTSW (ACTivity SWitch) at address 000074 is the **master device state control variable** in the SINTRAN HDLC system. It acts as a boolean flag that controls whether the HDLC device (transmitter or receiver) should be processing interrupts and handling DMA operations.

## ACTSW Value Meanings

### Value 0 (INACTIVE STATE)
```assembly
0 =: ACTSW(000074)                           % Device inactive/stopped
```
**Meaning**: Device is **NOT** supposed to be processing HDLC operations
**Usage**: 
- Device is idle/stopped
- DMA operations completed successfully  
- Fatal errors occurred and device is shut down
- Buffer exhaustion caused receiver shutdown
- Initialization/reset state

### Value 1 (ACTIVE STATE)  
```assembly
1 =: ACTSW(000074)                           % Device active/running
```
**Meaning**: Device is **ACTIVELY** processing HDLC operations
**Usage**:
- DMA transmission in progress
- DMA reception in progress  
- Device ready to handle interrupts
- Retry operations in progress

## ACTSW State Transitions - Complete Analysis

### Transmitter State Machine

#### 1. ACTIVATION (0 → 1): Start Transmission
```assembly
% Location: XHMST subroutine (Line 103705) - Start transmitter DMA
SUBR XHMST                                   % X HDLC MaSter STart
    % Set up DMA registers and hardware
    LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST    % Set DMA address
    A:=2000\/D; T+"WDCR-WDMA"; *EXR ST       % Start transmitter DMA (0x400)
    1134+CMODI; T:=HDEV+WTTC; *EXR ST        % Enable transmission
    
    1 =: ACTSW                               % *** ACTIVATE DEVICE ***
    OMSG =: DCBX                             % Set current DCB
RBUS
```
**Triggers**: 
- New transmission request from upper layers
- Retry after recoverable transmission error
- System startup/initialization

#### 2. SUCCESS DEACTIVATION (1 → 0): Transmission Complete
```assembly
% Location: HOINT success path (Line 104033+)
IF A/\ "SILFO+TXUND" = 0 THEN                % Transmission success?
   % Success processing
   XRETR=:RTDYN; A:=0; CALL SADTS            % Save retry stats, log success
   
   % Handle RQTS cleanup if needed
   IF CMODI = 40 THEN                        % RQTS mode?
      T:=HDEV+WTTC; *EXR ST                  % Turn off RQTS signal
   FI
   
   0=:ACTSW                                  % *** DEACTIVATE DEVICE ***
   CALL NEXTS                                % Process next frame in queue
   SUCCCNT+1=:SUCCCNT                        % Increment success counter
FI
```
**Triggers**:
- Successful transmission (SILFO+TXUND bits both clear)
- No hardware errors detected
- Frame transmitted to line successfully

#### 3. ERROR DEACTIVATION (1 → 0): Fatal Error
```assembly
% Location: HOINT failure path - retry limit exceeded
IF XRETR > MAXR(000115) THEN                % Too many retries? (>77)
   A:=237; CALL DRERR                        % Set fatal error code 237
   0=:ACTSW                                  % *** DEACTIVATE DEVICE ***
   CALL ERRNOT                               % Notify upper layers
FI
```
**Triggers**:
- Retry counter exceeds MAXR limit (77 decimal)
- Persistent hardware failures (SILFO or TXUND errors)
- Unrecoverable transmission problems

#### 4. RETRY MAINTENANCE (1 → 1): Stay Active
```assembly
% Location: HOINT failure path - within retry limit
IF XRETR <= MAXR(000115) THEN               % Still within retry limit?
   CALL XHMST                                % Restart DMA transmission
   % ACTSW remains 1 (stays active)         % *** MAINTAIN ACTIVE STATE ***
FI
```
**Triggers**:
- Transmission error but within retry limit
- Recoverable hardware errors (underrun, format errors)
- System performance issues causing transmission failures

### Receiver State Machine

#### 1. ACTIVATION (0 → 1): Start Reception
```assembly
% Location: ZSTARC subroutine (Line 104270) - Start receiver DMA
SUBR ZSTARC                                  % Z STart ReCeiver
    ZSTARC: IF ACTSW = 0 THEN                % Device not active?
               % Clear receiver state first
               HXDOK/\MAINT; T:=HDEV+WRTC; *EXR ST  % Clear old garbage
               
               % Set DMA Address and start receiver
               LIINT+DPITPHYS; T:=HDEV+WDMA; *EXR ST % Set DMA address
               A:=1001; T+"WDCR-WDMA"; *EXR ST      % Start receiver DMA (0x201)
               
               % Enable receiver with full DMA mode
               1734\/MAINT/\HXDOK; T:=HDEV+WRTC; *EXR ST % Enable reception
               
               1=:ACTSW                             % *** ACTIVATE DEVICE ***
            FI
RBUS
```
**Triggers**:
- System startup/initialization
- Recovery after buffer exhaustion
- Manual receiver restart after errors

#### 2. BUFFER EXHAUSTION DEACTIVATION (1 → 0): Critical Failure
```assembly
% Location: HIINT buffer check (Line 104436+)
IF HASTA/\"EMTY" >< 0 THEN                   % List empty (no receive buffers)?
   0=:ACTSW                                  % *** DEACTIVATE DEVICE ***
   STPCNT+1=:STPCNT                          % Increment stop counter
   GO OUT1                                   % Exit - all packets dropped
FI
```
**Triggers**:
- System memory exhaustion (no receive buffers available)
- DMA list empty (EMTY bit 11 set in RRTS)
- Critical system resource shortage

#### 3. NORMAL OPERATION MAINTENANCE (1 → 1): Continue Reception
```assembly
% Location: HIINT success path - after packet processing
CALL PROCPKT                                 % Process received packet

% Continue receiving if still active
IF ACTSW >< 0 THEN                          % Still active?
   CALL ZSTARC                               % *** RESTART RECEIVER DMA ***
FI                                           % ACTSW remains 1
```
**Triggers**:
- Successful packet reception and processing
- Valid data available (DataAvailable bit set, no X.21 errors)
- Normal operational flow

## ACTSW Validation Logic

### Spurious Interrupt Detection
Both interrupt handlers check ACTSW first to detect spurious interrupts:

#### Transmitter (HOINT):
```assembly
HOINT: 0=:TMR                                % Reset timer
       T:=HDEV+RTTS; *EXR ST                 % Read transmitter status
       A=:HASTA                              % Store hardware status
       
       IF T:=ACTSW = 0 THEN                  % *** CRITICAL CHECK ***
          MIN DUIN; P+0; CALL WT12           % Count unexpected interrupt
          GO OUT1                            % Exit - spurious interrupt
       FI
```

#### Receiver (HIINT):
```assembly
HIINT: T:=HDEV+RRTS; *EXR ST                % Read receiver status
       A=:HASTA                              % Store hardware status
       
       IF T:=ACTSW = 0 THEN                  % *** CRITICAL CHECK ***
          MIN T9; P+0                        % Count dummy interrupt
          GO OUT1                            % Exit - spurious interrupt
       FI
```

**Purpose**: Prevents processing interrupts when device should be inactive, which can happen due to:
- Hardware race conditions
- Multiple interrupt sources
- Cleanup timing issues
- DMA completion vs. interrupt timing

## ACTSW State Diagram

```
                    ┌─────────────────────┐
                    │   SYSTEM STARTUP    │
                    │   ACTSW = 0         │
                    │   (INACTIVE)        │
                    └──────────┬──────────┘
                               │
                               │ Start DMA Operation
                               │ (XHMST/ZSTARC)
                               ▼
                    ┌─────────────────────┐
    ┌──────────────►│   ACTIVE STATE      │◄──────────────┐
    │               │   ACTSW = 1         │               │
    │               │   (DMA RUNNING)     │               │
    │               └──────────┬──────────┘               │
    │                          │                          │
    │                          │ Interrupt occurs         │
    │                          ▼                          │
    │               ┌─────────────────────┐               │
    │               │  INTERRUPT HANDLER  │               │
    │               │  Check HASTA status │               │
    │               └──────────┬──────────┘               │
    │                          │                          │
    │            ┌─────────────┴─────────────┐            │
    │            │                           │            │
    │            ▼                           ▼            │
    │ ┌─────────────────────┐     ┌─────────────────────┐ │
    │ │     SUCCESS         │     │      FAILURE        │ │
    │ │                     │     │                     │ │
    │ │ • Transmit complete │     │ • Hardware errors   │ │
    │ │ • Packet processed  │     │ • Buffer exhaustion │ │
    │ │ • No errors         │     │ • X.21 protocol err │ │
    │ └─────────┬───────────┘     └─────────┬───────────┘ │
    │           │                           │             │
    │           ▼                           ▼             │
    │ ┌─────────────────────┐     ┌─────────────────────┐ │
    │ │   DEACTIVATE        │     │    ERROR ANALYSIS   │ │
    │ │   ACTSW = 0         │     │                     │ │
    │ │                     │     │ ┌─────────────────┐ │ │
    │ │ • Mark inactive     │     │ │  RECOVERABLE?   │ │ │
    │ │ • Process next      │     │ │                 │ │ │
    │ │ • Update stats      │     │ │ • Retry < MAXR? │ │ │
    │ └─────────────────────┘     │ │ • Not fatal?    │ │ │
    │                             │ └─────┬───────────┘ │ │
    │                             │       │     │       │ │
    │                             │      YES   NO       │ │
    │                             │       │     │       │ │
    │                             │       │     ▼       │ │
    │                             │       │ ┌─────────────────────┐
    │                             │       │ │   FATAL ERROR       │
    │                             │       │ │   ACTSW = 0         │
    │                             │       │ │                     │
    │                             │       │ │ • Stop device       │
    │                             │       │ │ • Notify errors     │
    │                             │       │ │ • Increment HDERC   │
    │                             │       │ └─────────────────────┘
    │                             │       │
    │                             │       ▼
    │                             │   ┌─────────────────────┐
    │                             │   │    RETRY OPERATION  │
    │                             │   │    ACTSW = 1        │
    │                             │   │                     │
    │                             │   │ • Increment XRETR   │
    │                             │   │ • Restart DMA       │
    │                             │   │ • Maintain active   │
    │                             │   └─────────────────────┘
    │                             │           │
    │                             └───────────┴─────────────────┘
    │
    └─ Next Operation Request (NEXTS/ZSTARC)
```

## Critical ACTSW Relationships

### 1. DMA State Synchronization
ACTSW ensures DMA hardware state matches software expectations:
- **ACTSW = 1**: DMA should be running, expect interrupts
- **ACTSW = 0**: DMA should be stopped, interrupts are spurious

### 2. Error Recovery Control
ACTSW controls retry behavior:
- **Stay active (1)**: Continue retry attempts within limits
- **Go inactive (0)**: Stop after fatal errors or success

### 3. Buffer Management Integration
ACTSW coordinates with buffer availability:
- **Receiver**: EMTY bit forces ACTSW = 0 (buffer exhaustion)
- **Transmitter**: Success/failure determines ACTSW state

### 4. Interrupt Processing Validation  
ACTSW prevents race conditions:
- Hardware interrupts vs. software state changes
- Multiple interrupt sources
- DMA completion timing issues

## Diagnostic Values

### Normal Operation Patterns
```assembly
% Successful transmission cycle:
ACTSW = 0 → 1 → 0 (start → active → complete)

% Retry transmission cycle:  
ACTSW = 1 → 1 → 1 → 0 (active → retry → retry → complete)

% Receiver operation:
ACTSW = 0 → 1 → 1 → 1... (start → active → continuous processing)
```

### Error Condition Patterns
```assembly
% Fatal transmission error:
ACTSW = 1 → 1 → 1 → 0 (active → retry → fatal → stopped)

% Buffer exhaustion:
ACTSW = 1 → 0 (active → forced stop)

% Spurious interrupt detection:
ACTSW = 0, interrupt occurs → count error, exit
```

## Debugging ACTSW Issues

### Key Monitoring Points
1. **ACTSW value** at interrupt entry
2. **State transition triggers** (what caused 0→1 or 1→0)
3. **Spurious interrupt counters** (DUIN, T9)
4. **Correlation with hardware status** (HASTA values)

### Common Problems
1. **ACTSW stuck at 0**: Device not restarting after operations
2. **ACTSW stuck at 1**: Device not properly completing operations
3. **High spurious counts**: ACTSW/hardware synchronization issues
4. **Rapid state changes**: Possible hardware timing problems

ACTSW serves as the **master control gate** ensuring that SINTRAN HDLC interrupt processing only occurs when the device is legitimately active and ready to handle DMA operations.