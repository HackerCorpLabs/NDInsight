# HDLC Variable Reference - Complete Symbol Analysis

## Variable Classification from SYMBOL-1-LIST.SYMB.TXT and SYMBOL-2-LIST.SYMB.TXT

### Core Status and Control Variables

| Variable | Symbol | Address/Value | Type | Purpose | Read/Write Pattern |
|----------|--------|---------------|------|---------|-------------------|
| **HASTAT** | HASTA | 000076 | Memory Variable | **HA**rdware **STA**tus storage | R/W: Updated every interrupt |
| **ACTSW** | ACTSW | 000074 | Memory Variable | **ACT**ivity **SW**itch (device state) | R/W: 0=inactive, 1=active |
| **TMR** | TMR | 177773 | Memory Variable | Transmission timer | W: Reset to 0 on interrupt |
| **XRETRY** | XRETR | 000105 | Memory Variable | **X** **RETR**y counter | R/W: Incremented on failure, cleared on success |

### Status Bit Constants (Read-Only)

| Constant | Symbol | Value (Octal) | Value (Hex) | Bit Position | Purpose |
|----------|--------|---------------|-------------|--------------|---------|
| **SILFO** | SILFO | 100000 | 0x8000 | 15 | Illegal Format/Key Error |
| **TXUND** | TXUND | 000002 | 0x0002 | 1 | Transmitter Underrun |
| **EMTY** | EMTY | 004000 | 0x0800 | 11 | List Empty (No Buffers) |
| **BLDON** | BLDON | 000010 | 0x0008 | 3 | Block Done Flag |
| **HX21M** | HX21M | 060000 | 0x6000 | 13-14 | X.21 Error Mask |
| **HX21S** | HX21S | 000016 | 0x000E | 1,2,3 | Receiver State Check |
| **EUND** | EUND | 000102 | 0x0042 | 1,6 | Underrun Error Code |
| **ERB** | ERB | 001000 | 0x0200 | 9 | Error Block Indicator |

### Subroutines (Code Addresses)

| Subroutine | Symbol | Address (Octal) | Address (Hex) | Purpose | Call Pattern |
|------------|--------|-----------------|---------------|---------|--------------|
| **SADTS** | SADTS | 104735 | 0x89DD | Store And Display Transmission Status | Called on every interrupt |
| **DRERR** | DRERR | 104751 | 0x89E9 | Device Retry Error handler | Called on transmission failure |
| **NEXTS** | ? | Not found | ? | Next transmission in queue | Called after successful transmission |
| **XHMST** | ? | Not found | ? | Restart transmitter DMA | Called for retry attempts |

### Counters and Statistics Variables

| Variable | Symbol | Address | Type | Purpose | Update Pattern |
|----------|--------|---------|------|---------|----------------|
| **RTDYN** | RTDYN | 000065 | Memory Variable | **R**e**T**ry **DYN**amics (statistics) | W: Copy of XRETRY on success |
| **HDERC** | HDERC | 000066 | Memory Variable | **H**ar**D**ware **ER**ror **C**ounter | W: Incremented on errors |
| **T9** | ? | Not found | Memory Variable | Dummy interrupt counter (HIINT) | W: Decremented on spurious interrupts |
| **DUIN** | ? | Not found | Memory Variable | Unexpected interrupt counter (HOINT) | W: Decremented on spurious interrupts |
| **STPCNT** | ? | Not found | Memory Variable | Stop counter (receiver) | W: Incremented when receiver stops |

### Buffer Management Variables

| Variable | Symbol | Address | Type | Purpose | Access Pattern |
|----------|--------|---------|------|---------|----------------|
| **MASTB** | MASTB | 000041 | Memory Address | Master status table base | R: Used in SADTS for status logging |
| **BUFFA** | BUFFA | 000054 | Memory Address | Buffer A address | R/W: Circular buffer management |
| **BUFFE** | BUFFE | 000011 | Memory Address | Buffer E address | R/W: Circular buffer management |
| **BUFFI** | BUFFI | 000014 | Memory Address | Buffer I address | R/W: Circular buffer management |
| **RBUFF** | RBUFF | 073274 | Memory Address | Receive buffer | R/W: DMA receive operations |
| **WBUFF** | WBUFF | 073306 | Memory Address | Write buffer | R/W: DMA transmit operations |

### Device Control Constants

| Constant | Symbol | Value (Octal) | Value (Hex) | Purpose | Usage |
|----------|--------|---------------|-------------|---------|-------|
| **MAXR** | MAXR | 000115 | 0x004D | Maximum retry count | R: Compared with XRETRY |

### Additional Timer Variables

| Variable | Symbol | Address | Type | Purpose | Usage Pattern |
|----------|--------|---------|------|---------|---------------|
| **5TMR** | 5TMR | 177773 | Memory Variable | Timer 5 | R/W: System timing |
| **TTMR** | TTMR | 177774 | Memory Variable | Transmit timer | R/W: Transmission timeouts |
| **SUTMR** | SUTMR | 000035 | Memory Variable | Setup timer | R/W: Initialization timing |
| **LTTMR** | LTTMR | 000023 | Memory Variable | Link timer | R/W: Link establishment |

## Detailed Read/Write Patterns

### HASTAT (HASTA) - **HA**rdware **STA**tus Storage
```assembly
% WRITE PATTERNS:
HIINT: T:=HDEV+RRTS; *EXR ST; A=:HASTAT    % Store receiver status
HOINT: T:=HDEV+RTTS; *EXR ST; A=:HASTAT    % Store transmitter status

% READ PATTERNS:
IF A/\ HX21M >< 0 THEN                      % Test X.21 errors
IF HASTAT/\"EMTY" >< 0 THEN                 % Test list empty
IF A/\ "SILFO+TXUND" = 0 THEN               % Test transmission success
HASTAT BONE BLDON=:HASTAT                   % Set block done flag
```

### ACTSW - **ACT**ivity **SW**itch  
```assembly
% WRITE PATTERNS:
1 =: ACTSW                                  % Mark device active
0 =: ACTSW                                  % Mark device inactive

% READ PATTERNS:
IF T:=ACTSW = 0 THEN                        % Check if device active
IF ACTSW >< 0 THEN                          % Check if still active (restart)
```

### XRETRY (XRETR) - **X** **RETR**y Counter
```assembly
% WRITE PATTERNS:
XRETRY+1=:XRETRY                            % Increment on failure
0=:XRETRY                                   % Clear on success
XRETRY=:RTDYN                               % Save to statistics

% READ PATTERNS:
IF XRETRY > MAXRETRY THEN                   % Check retry limit
```

### TMR - Transmission Timer
```assembly
% WRITE PATTERNS:
0=:TMR                                      % Reset timer on interrupt entry

% READ PATTERNS:
% (Timer is primarily written, not read in interrupt handlers)
```

## Memory Layout Analysis

### Variable Address Ranges
- **000000-000177**: System variables and counters
  - ACTSW (000074), HASTA (000076), XRETR (000105)
- **177700-177777**: Hardware registers and timers  
  - TMR (177773), TTMR (177774)
- **070000-077777**: Buffer areas
  - RBUFF (073274), WBUFF (073306)
- **100000+**: Subroutine code
  - SADTS (104735), DRERR (104751)

### Access Patterns Summary

#### High-Frequency Variables (Every Interrupt)
- **HASTAT**: Read/written every interrupt for status decisions
- **ACTSW**: Read every interrupt for activity validation
- **TMR**: Written every transmitter interrupt for timeout control

#### Medium-Frequency Variables (On Errors/State Changes)
- **XRETRY**: Updated on transmission failures and successes
- **HDERC**: Incremented on hardware errors for statistics
- **RTDYN**: Updated on successful transmissions for statistics

#### Low-Frequency Variables (Diagnostic/Buffering)
- **MASTB**: Used in status logging subroutines
- **Buffer addresses**: Used during DMA setup and data transfer

## Critical Variable Dependencies

### Success/Failure Decision Chain
1. **HASTAT** ← Hardware register read
2. **HASTAT** → Bit mask tests (SILFO+TXUND, HX21M, EMTY)
3. **Test results** → ACTSW, XRETRY updates
4. **ACTSW state** → Device restart/stop decisions

### Error Handling Chain  
1. **Hardware error** → HASTAT status bits
2. **Error detection** → HDERC increment, XRETRY increment
3. **Retry limit** → ACTSW=0 (stop) or device restart

### Buffer Management Chain
1. **EMTY bit in HASTAT** → STPCNT increment
2. **Buffer addresses** → DMA setup for retries
3. **MASTB** → Status logging in circular buffers

## SINTRAN 5-Character Abbreviation Pattern

SINTRAN symbols are limited to 5 characters, leading to logical abbreviations:

### Confirmed Abbreviations
- **HASTA** = **HA**rdware **STA**tus (central status storage)
- **ACTSW** = **ACT**ivity **SW**itch (device state control)
- **XRETR** = **X** **RETR**y (retry attempt counter)
- **RTDYN** = **R**e**T**ry **DYN**amics (retry statistics)
- **HDERC** = **H**ar**D**ware **ER**ror **C**ounter (error tracking)
- **MASTB** = **MAST**er status table **B**ase (logging infrastructure)

## Missing Symbols Analysis

Several variables referenced in the source code were not found in the SYMBOL files:

### Likely Local Variables/Labels (Not in Symbol Tables)
- **NEXTS**: Probably **NEXT** frame in **S**equence (local label)
- **XHMST**: Likely **X** **H**DLC **M**aster **ST**art (local subroutine)
- **T9**: May be **T**imer/**T**emp register **9** (local variable)
- **DUIN**: May be **D**evice **U**nexpected **IN**terrupt (local counter)  
- **STPCNT**: Likely **ST**o**P** **C**ou**NT**er (local variable)

### Possible Explanations
1. **Local Labels**: Not exported to symbol tables
2. **Macro Definitions**: Expanded inline during compilation
3. **Register Names**: CPU register references, not memory variables
4. **Conditional Compilation**: Only included in specific builds

This comprehensive variable reference provides the foundation for understanding SINTRAN HDLC interrupt processing with exact memory locations, access patterns, and the logical 5-character abbreviation system used throughout the codebase.