# HDLC Complete Register Analysis - SINTRAN Source Code Findings

## Executive Summary

This document provides the definitive analysis of HDLC register bit usage in SINTRAN based on comprehensive source code analysis and actual constant values. The analysis reveals that:

1. **Transmission logic is CORRECT** - the SILFO+TXUND check properly validates transmission success
2. **Reception logic has naming confusion** - but actual bit positions are correct
3. **Your C# bit definitions are ACCURATE** - they match SINTRAN's actual usage
4. **The root cause of issues is likely in emulator implementation**, not bit interpretation

## SINTRAN Constants (from SYMBOL-1-LIST.SYMB.TXT)

| Constant | Octal | Hex | Bits | Meaning |
|----------|--------|-----|------|---------|
| TXUND | 000002 | 0x0002 | 1 | Transmitter Underrun |
| SILFO | 100000 | 0x8000 | 15 | Illegal Format/Key Error |
| EMTY | 004000 | 0x0800 | 11 | List Empty (No Buffers) |
| HX21M | 060000 | 0x6000 | 13-14 | X.21 Error Mask |
| HX21S | 000016 | 0x000E | 1,2,3 | Receiver State (NOT X.21 Clear!) |
| BLDON | 000010 | 0x0008 | 3 | Block Done Flag |
| XBLDN | 000010 | 0x0008 | 3 | External Block Done |
| ERB | 001000 | 0x0200 | 9 | Error Block Indicator |
| EUND | 000102 | 0x0042 | 1,6 | Underrun Error Code |

## RRTS (Read Receiver Transfer Status) Complete Analysis

### Critical Processing Logic (Line 104436+)

```assembly
HIINT: T:=HDEV+RRTS; *EXR ST     % Read receiver status
       A=:HASTAT                 % Store in HASTAT variable
       
       % Primary checks in order:
       IF A/\ HX21M >< 0 THEN                    % X.21 error check (bits 13-14)
          IF A BIT HX21S THEN                    % Receiver state check (bits 1,2,3)
             HASTAT BONE BLDON=:HASTAT           % Terminate cleanly
          FI
       FI
       
       IF HASTAT/\"EMTY" >< 0 THEN              % List empty check (bit 11)
          0=:ACTSW                              % Stop device
       FI
       
       IF A NBIT 0 OR A/\60000><0 THEN          % Data available + X.21 check
          return; % Drop packet
       FI
```

### Bit-by-Bit RRTS Analysis

| Bit | C# Enum Name | SINTRAN Usage | Effect on Packet Processing |
|-----|--------------|---------------|----------------------------|
| 0 | DataAvailable | **CRITICAL** - Must be 1 | Packet dropped if clear |
| 1 | StatusAvailable | Part of HX21S (0x000E) | Used for receiver state routing |
| 2 | ReceiverActive | Part of HX21S (0x000E) | Used for receiver state routing |
| 3 | SyncFlagReceived | Part of HX21S (0x000E) | Used for receiver state routing |
| 4 | DMAModuleRequest | Cleared on read | Not used in processing logic |
| 5 | SignalDetector | Not referenced | No direct impact |
| 6 | DataSetReady | Not referenced | No direct impact |
| 7 | RingIndicator | Not referenced | No direct impact |
| 8 | BlockEnd | XBLDN test | Controls block processing |
| 9 | FrameEnd | Not directly referenced | DMA status |
| 10 | ListEnd | Not directly referenced | DMA status |
| 11 | ListEmpty | **CRITICAL** - EMTY test | Stops receiver if set |
| 12 | Reserved | Not used | No impact |
| 13 | X21D | **CRITICAL** - HX21M mask | Triggers X.21 error handling |
| 14 | X21S | **CRITICAL** - HX21M mask | Triggers X.21 error handling |
| 15 | ReceiverOverrun | Not referenced | No direct impact |

### RRTS Processing Decision Tree

```
RRTS Read → HASTAT Storage
    ↓
X.21 Error Check (bits 13-14)
    ├─ If error detected → Handle X.21 error
    │   └─ If receiver active (bits 1,2,3) → Terminate frame
    └─ No error → Continue
    ↓
List Empty Check (bit 11)
    ├─ If empty → Stop device, drop all packets
    └─ Buffers available → Continue
    ↓
Data Available Check (bit 0) + X.21 Recheck
    ├─ No data OR X.21 error → Drop packet
    └─ Data available, no errors → Process packet
```

## RTTS (Read Transmitter Transfer Status) Complete Analysis  

### Critical Processing Logic (Line 104033+)

```assembly
HOINT: T:=HDEV+RTTS; *EXR ST     % Read transmitter status
       A=:HASTAT                 % Store in HASTAT variable
       
       IF A/\ "SILFO+TXUND" = 0 THEN            % Success check (bits 15,1)
          XRETRY=:RTDYN; A:=0; CALL SADTS       % Clear retry, log success
       ELSE
          A:=HASTAT; CALL SADTS; CALL DRERR     % Log error, retransmit
          A:=EUND                               % Set underrun error
       FI
```

### Bit-by-Bit RTTS Analysis

| Bit | C# Enum Name | SINTRAN Usage | Effect on Transmission |
|-----|--------------|---------------|------------------------|
| 0 | TransmitBufferEmpty | Not referenced | Hardware status only |
| 1 | TransmitterUnderrun | **CRITICAL** - TXUND | Triggers retransmission |
| 2 | TransmitterActive | Not referenced | Hardware status only |
| 3 | Reserved | Not used | No impact |
| 4 | DMAModuleRequest | Cleared on read | Not used in logic |
| 5 | Reserved | Not used | No impact |
| 6 | ReadyForSending | Tested in some paths | DCE ready status |
| 7 | Reserved | Not used | No impact |
| 8 | BlockEnd | Not directly referenced | DMA status |
| 9 | FrameEnd | Not directly referenced | DMA status |
| 10 | ListEnd | BSKP ONE 10 test | Skip operation if set |
| 11 | TransmissionFinished | Part of SILFO mask | **NOT an error!** |
| 12-14 | Reserved | Not used | No impact |
| 15 | Illegal | **CRITICAL** - SILFO | Triggers retransmission |

### RTTS Success/Failure Logic

```
RTTS Read → HASTAT Storage
    ↓
SILFO+TXUND Check (bits 15,1)
    ├─ Both clear (0) → SUCCESS
    │   ├─ Clear retry state
    │   └─ Log successful transmission
    └─ Either set → ERROR
        ├─ Log error condition
        ├─ Increment error counter
        └─ Retransmit packet
```

## Critical Findings and Corrections

### 1. Transmission Logic is CORRECT ✅

The SINTRAN logic `IF A/\ "SILFO+TXUND" = 0 THEN` is **correctly implemented**:

```csharp
// SILFO+TXUND = 0x8000 | 0x0002 = 0x8002
if ((rtts & 0x8002) == 0)  // Both SILFO and TXUND clear
{
    // SUCCESS: No illegal format AND no underrun
    transmissionSuccess();
}
else
{
    // ERROR: Either illegal format OR underrun
    retransmitPacket();
}
```

### 2. HX21S Naming Confusion Clarified ✅

**HX21S = 0x000E** is NOT the X.21 Clear Indication bit. It tests receiver state:

- **Bit 1**: StatusAvailable
- **Bit 2**: ReceiverActive  
- **Bit 3**: SyncFlagReceived

**Real X.21 Clear Indication is bit 14** (part of HX21M mask 0x6000).

### 3. Your C# Bit Definitions are CORRECT ✅

```csharp
X21D = 1<<13,              // ✅ Matches HX21M mask bit 13
X21S = 1<<14,              // ✅ Matches HX21M mask bit 14  
ReceiverOverrun = 1<<15,   // ✅ Standard HDLC meaning
```

## Implementation Guidance for HDLC Emulator

### Normal LAPB Packet Reception

```csharp
// Minimal RRTS for successful packet processing:
ReceiverStatusBits rrts = ReceiverStatusBits.DataAvailable;  // 0x0001

// SINTRAN processing:
// - DataAvailable = 1 ✅ Pass data check
// - X.21 errors = 0 ✅ No protocol errors  
// - ListEmpty = 0 ✅ Buffers available
// Result: Packet processed successfully
```

### Normal Transmission Success

```csharp
// RTTS for successful transmission:
TransmitterStatusBits rtts = 
    TransmitterStatusBits.TransmitBufferEmpty |  // Optional
    TransmitterStatusBits.ReadyForSending;       // Optional

// Critical: Keep bits 1 and 15 CLEAR
// - TransmitterUnderrun (bit 1) = 0 ✅
// - Illegal (bit 15) = 0 ✅
// Result: (rtts & 0x8002) == 0 → Success
```

### Error Conditions

#### Receiver Buffer Exhaustion
```csharp
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.ListEmpty;  // Bit 11 set

// Result: SINTRAN stops receiver, drops all packets
```

#### X.21 Protocol Error
```csharp
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.X21D;  // Bit 13 set

// Result: SINTRAN triggers X.21 error handling
```

#### X.21 Connection Clear
```csharp
ReceiverStatusBits rrts = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.X21S;  // Bit 14 set

// Result: SINTRAN terminates connection
```

#### Transmitter Underrun
```csharp
TransmitterStatusBits rtts = 
    TransmitterStatusBits.TransmitterUnderrun;  // Bit 1 set

// Result: (rtts & 0x8002) != 0 → Retransmission
```

#### Illegal Format Error
```csharp
TransmitterStatusBits rtts = 
    TransmitterStatusBits.Illegal;  // Bit 15 set

// Result: (rtts & 0x8002) != 0 → Retransmission
```

## Debugging Recommendations

### For Packet Loss Issues

1. **Log RRTS values** when packets are dropped:
   ```csharp
   ushort rrts = ReadRRTS();
   Console.WriteLine($"RRTS=0x{rrts:X4} DataAvail={(rrts&1)!=0} X21Err={(rrts&0x6000)!=0} ListEmpty={(rrts&0x800)!=0}");
   ```

2. **Check specific failure points**:
   - DataAvailable (bit 0) must be set
   - X.21 errors (bits 13-14) must be clear
   - ListEmpty (bit 11) must be clear

### For Retransmission Issues

1. **Log RTTS values** when retransmission occurs:
   ```csharp
   ushort rtts = ReadRTTS();
   Console.WriteLine($"RTTS=0x{rtts:X4} Underrun={(rtts&2)!=0} Illegal={(rtts&0x8000)!=0}");
   ```

2. **Verify success conditions**:
   - TransmitterUnderrun (bit 1) must be clear
   - Illegal (bit 15) must be clear
   - Success when (rtts & 0x8002) == 0

## Conclusion

The SINTRAN HDLC implementation is **logically sound**. Your packet transmission and reception issues are likely caused by:

1. **Incorrect status bit values** in your emulator
2. **Timing issues** with status register reads
3. **Multiple reads** clearing DMA status bits
4. **Buffer management** not properly simulated

The bit positions and logic in your C# code are correct and match the SINTRAN implementation exactly. Focus debugging efforts on ensuring your emulated hardware provides the correct status bit patterns that SINTRAN expects.