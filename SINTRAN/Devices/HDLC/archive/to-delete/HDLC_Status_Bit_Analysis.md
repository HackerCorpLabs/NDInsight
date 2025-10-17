# HDLC Status Bit Analysis

This document provides a comprehensive analysis of how RRTS (Read Receiver Transfer Status) and RTTS (Read Transmitter Transfer Status) values are read, stored, and processed in the SINTRAN HDLC driver based on analysis of the s3vs-4-L-RONNY.symb file.

## Status Register Read Operations

### RRTS (Read Receiver Transfer Status) - IOX+10

**Primary Read Locations:**
1. **HIINT (HDLC Input Interrupt Handler) - Line 104436:**
   ```assembly
   HIINT: T:=HDEV+RRTS; *EXR ST     % READ RECEIVER STATUS
          A=:HASTAT                 % SAVE STATUS
   ```

2. **Interface Detection - Line 20034:**
   ```assembly
   T:=D.HDEV+RRTS; *EXR ST; TRA IIC; 1BANK
   IF A=0 THEN                     % INTERFACE IS PRESENT
   ```

3. **Modem Input Driver - Line 51080:**
   ```assembly
   HODI1: T:=HDEV+BRRTS; *EXR ST   % Banked RRTS read
          A=:T                      % ENTRY HERE IF X21
   ```

### RTTS (Read Transmitter Transfer Status) - IOX+12

**Primary Read Locations:**
1. **HOINT (HDLC Output Interrupt Handler) - Line 104033:**
   ```assembly
   HOINT: 0=:TMR                    % RESET TIMER
          T:=HDEV+RTTS; *EXR ST     % READ STATUS
          A=:HASTAT                 % SAVE STATUS
   ```

2. **Transmitter Status Check - Line 51395:**
   ```assembly
   T:=HDEV+BRTTS;*EXR ST; BSKP ONE 10 DA  % READ TRANSMITTER TRANSFER STATUS
   ```

## Status Storage and Processing Flow

### 1. Status Read → Storage
- Hardware status registers are read using `T:=HDEV+[RRTS|RTTS]; *EXR ST`
- Status value is automatically loaded into A register
- A register value is saved to `HASTAT` variable: `A=:HASTAT`

### 2. Status Processing Pipeline
After storage in `HASTAT`, the status undergoes systematic bit analysis:

```
Hardware → A Register → HASTAT Variable → Bit Analysis → Actions
```

## Detailed Bit Analysis

### RRTS (Receiver Status) Bit Processing

#### X.21 Protocol Error Detection (Bits 13-14)
**Location: Line 104450**
```assembly
IF A/\ HX21M >< 0 THEN          % X21-ERROR? (Tests bits 13-14)
   TRR 10                       % Context switch
   T:=2000; X:=LIINT+T; T:=X.LKEY
   A\/ LIINT.LKEY=:X.LKEY       % Save error state
   IF A BIT HX21S THEN          % X21 CLEAR INDICATION? (Bit 14)
      HASTAT BONE BLDON=:HASTAT % Set BLOCK DONE flag
      LIINT.LKEY BONE XBLDN=:X.LKEY
   FI
FI
```

**Bit Mapping:**
- `HX21M` mask tests **X21D (Bit 13)** + **X21S (Bit 14)** 
- `HX21S` specifically tests **X21S (Bit 14)** - X.21 Clear Indication

#### Buffer Empty Detection (Bit 11)
**Location: Line 104463**
```assembly
IF HASTAT/\"EMTY" >< 0 THEN     % LIST EMPTY? (Bit 11)
   0=:ACTSW                     % DEVICE STOPPED
   MIN STPCNT                   % INCREMENT STOP COUNTER
   P+0                          % SKIP INSTRUCTION
```

**Bit Mapping:**
- `"EMTY"` constant tests **ListEmpty (Bit 11)** - DMA list exhausted

#### Data Processing Control
**Location: Line 104483**
```assembly
IF A NBIT XBLDN THEN            % ANY MORE FILLED BLOCKS?
   IF A = "ERB" THEN GO FAR ZSTARC FI  % Enable receiver if error block
   GO FAR OUT1
FI
```

**Bit Mapping:**
- `XBLDN` tests **BlockEnd (Bit 8)** - DMA block completion

#### Signal Status Monitoring
**Location: Line 51083 (Modem Driver)**
```assembly
IF A NBIT 0 OR A/\60000><0 THEN  % NO DATA OR X21 ERROR?
```

**Bit Mapping:**
- `NBIT 0` tests **DataAvailable (Bit 0)** - Character ready for read
- `A/\60000` tests upper bits including **X21D/X21S (Bits 13-14)**

### RTTS (Transmitter Status) Bit Processing

#### Underrun and Error Detection
**Location: Line 104046**
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN   % No underrun/error?
   XRETRY=:RTDYN; A:=0; CALL SADTS  % Retry mechanism
ELSE
   A:=HASTAT; CALL SADTS; CALL DRERR  % Error handling
   A:=EUND                      % Set underrun error code
FI
```

**Bit Mapping:**
- `TXUND` tests **TransmitterUnderrun (Bit 1)** - Buffer not loaded in time
- `SILFO` likely tests **TransmissionFinished (Bit 11)** - DMA completion

#### Transmission Status Control
**Location: Line 51397**
```assembly
BSKP ONE 10 DA                  % Skip if bit 10 clear
```

**Bit Mapping:**
- Tests **ListEnd (Bit 10)** - DMA list completion

#### Ready for Sending Check
Various locations test transmitter readiness:
```assembly
IF A BIT 6 THEN                 % Ready for sending?
```

**Bit Mapping:**
- **ReadyForSending (Bit 6)** - DCE ready signal (CCITT 106)

## Status Bit Constants and Masks

### Receiver Status Constants
- **`HX21M`**: Mask for X.21 error detection (Bits 13-14)
  - Tests: `X21D | X21S` (0x6000)
- **`HX21S`**: X.21 Clear Indication (Bit 14)
  - Tests: `X21S` (0x4000)
- **`"EMTY"`**: List Empty condition (Bit 11)
  - Tests: `ListEmpty` (0x0800)
- **`BLDON`**: Block Done flag (used for setting, not testing hardware bit)
- **`XBLDN`**: External Block Done (Bit 8)
  - Tests: `BlockEnd` (0x0100)

### Transmitter Status Constants
- **`"TXUND"`**: Transmitter Underrun (Bit 1)
  - Tests: `TransmitterUnderrun` (0x0002)
- **`"SILFO"`**: Likely Transmission Finished (Bit 11)
  - Tests: `TransmissionFinished` (0x0800)
- **`"SILFO+TXUND"`**: Combined underrun/completion mask
  - Tests: `TransmitterUnderrun | TransmissionFinished` (0x0802)

## Action Matrix Based on Status Bits

### Receiver Actions

| Bit(s) | Condition | Action | Code Location |
|--------|-----------|---------|---------------|
| 0 | DataAvailable = 0 | Stop processing | Line 51083 |
| 8 | BlockEnd = 1 | Continue to next block | Line 104483 |
| 11 | ListEmpty = 1 | Stop device, increment counter | Line 104463 |
| 13-14 | X21D/X21S ≠ 0 | Error handling, context switch | Line 104450 |
| 14 | X21S = 1 | Set BLDON flag, terminate | Line 104456 |

### Transmitter Actions

| Bit(s) | Condition | Action | Code Location |
|--------|-----------|---------|---------------|
| 1 | TransmitterUnderrun = 1 | Error handling, set EUND | Line 104046 |
| 6 | ReadyForSending = 0 | Wait or error | Various |
| 10 | ListEnd = 1 | Skip next operation | Line 51397 |
| 11 | TransmissionFinished = 0 | Retry transmission | Line 104046 |
| 15 | Illegal = 1 | Restart transmitter | Not shown |

## Diagnostic and Logging

### Status Logging Buffers
After bit processing, status values are logged to circular buffers:

```assembly
CALL SADTS          % Store status in diagnostic buffers
```

**Buffer Structure:**
- **BUFF0**: Frame sequence numbers
- **BUFF1**: Device numbers  
- **BUFF2**: Device status (HASTAT values)
- **BUFF3**: Device keys when stopped

### Error Counters
- **HDERC**: Hardware error counter (incremented by DRERR)
- **STPCNT**: Receiver stop counter (buffer exhaustion)
- **TMR**: Timer for timeout detection

## Summary

The SINTRAN HDLC driver implements a sophisticated status monitoring system:

1. **Hardware status registers** (RRTS/RTTS) are read during interrupt processing
2. **Status values** are stored in the HASTAT variable for analysis
3. **Individual bits** are tested using masks and bit operations
4. **Specific actions** are taken based on bit combinations:
   - **X.21 protocol errors** trigger context switches and error recovery
   - **Buffer conditions** control device start/stop operations
   - **DMA completion flags** manage block and list processing
   - **Underrun conditions** initiate retry mechanisms
5. **Status history** is maintained in circular buffers for diagnostics
6. **Error counters** track system reliability metrics

This creates a robust real-time HDLC communication system capable of handling various error conditions and protocol states while maintaining diagnostic visibility into hardware operations.