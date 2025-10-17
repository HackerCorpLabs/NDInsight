# RTSR vs RTTS Register Analysis - SINTRAN HDLC Usage

## Register Distinction - Critical Understanding

SINTRAN HDLC uses **two separate transmitter status registers**:

### RTSR (HDEV+6) - Read Transmitter Status Register  
```assembly
RTSR = 0006  % Basic HDLC transmitter status (IOX+6)
```
**Purpose**: Basic HDLC protocol status (flags, sync, etc.)  
**SINTRAN Usage**: **NOT used in main interrupt processing**  
**Content**: Traditional HDLC status bits

### RTTS (HDEV+12) - Read Transmitter Transfer Status
```assembly  
RTTS = 0012  % DMA transmitter transfer status (IOX+12)
```
**Purpose**: DMA transfer status and completion information  
**SINTRAN Usage**: **PRIMARY register used in HOINT interrupt handler**  
**Content**: DMA status bits 8-15 + HDLC status bits 0-7

## SINTRAN Interrupt Handler Usage

### HOINT (Transmitter Interrupt) - Uses RTTS
```assembly
HOINT: 0=:TMR                                % Reset timer
       T:=HDEV+RTTS; *EXR ST                 % IOX+12 - Read RTTS (NOT RTSR!)
       A=:HASTA                              % Store status
       
       IF A/\ "SILFO+TXUND" = 0 THEN         % Test DMA transfer success
          % Success processing
       ELSE  
          % Error/retry processing
       FI
```

**Key Point**: SINTRAN reads **RTTS (HDEV+12)** for transmission status, not RTSR (HDEV+6).

### HIINT (Receiver Interrupt) - Uses RRTS
```assembly
HIINT: T:=HDEV+RRTS; *EXR ST                % IOX+10 - Read receiver transfer status
       A=:HASTA                             % Store status
       
       IF A NBIT 0 OR A/\60000><0 THEN      % Test data available + X.21 errors
          GO OUT1                           % Drop packet
       FI
       
       CALL PROCPKT                         % Process packet
```

**Key Point**: SINTRAN reads **RRTS (HDEV+10)** for reception status.

## Your C# Enum Analysis

Based on your enum comments mentioning "DMA MODULE" and "Bits 8-15 are cleared when reading", your enum is for **RTTS/RRTS** (transfer status registers), not RTSR (basic status register).

### For Receiver (RRTS - HDEV+10):
```csharp
// Your enum appears to be for RRTS (Receiver Transfer Status)
// SINTRAN validation for packet reception:

/// <summary>
/// DataAvailable (bit 0) - CRITICAL for packet processing
/// SINTRAN Logic: IF A NBIT 0 THEN GO OUT1 (drop packet)
/// Must be SET for packet to be processed
/// </summary>
DataAvailable = 1 << 0,

/// <summary>  
/// ListEmpty (bit 11) - CRITICAL system condition
/// SINTRAN Logic: IF HASTA/\"EMTY" >< 0 THEN 0=:ACTSW (shutdown receiver)
/// Setting this bit SHUTS DOWN the entire receiver
/// Only set if emulating buffer exhaustion
/// </summary>
ListEmpty = 1 << 11,

/// <summary>
/// X21D (bit 13) - X.21 protocol error
/// SINTRAN Logic: IF A/\ HX21M >< 0 THEN CALL X21ERR (protocol error)
/// Setting this triggers X.21 error handling
/// </summary>
X21D = 1 << 13,

/// <summary>
/// X21S (bit 14) - X.21 protocol error  
/// SINTRAN Logic: IF A/\ HX21M >< 0 THEN CALL X21ERR (protocol error)
/// Setting this triggers X.21 error handling
/// </summary>
X21S = 1 << 14
```

## Register Map Summary

| Register | Address | Name | SINTRAN Usage | Your Enum |
|----------|---------|------|---------------|-----------|
| **RTSR** | HDEV+6 | Read Transmitter Status Register | **Not used** in interrupts | **NO** |
| **RTTS** | HDEV+12 | Read Transmitter Transfer Status | **Used** in HOINT | **YES** (for transmit) |
| **RRTS** | HDEV+10 | Read Receiver Transfer Status | **Used** in HIINT | **YES** (for receive) |

## Conclusion

1. Your C# enum is for **RRTS/RTTS** (DMA transfer status), not RTSR (basic status)
2. **SINTRAN never uses RTSR (HDEV+6)** in interrupt processing
3. **SINTRAN uses RTTS (HDEV+12)** for transmitter interrupts  
4. **SINTRAN uses RRTS (HDEV+10)** for receiver interrupts
5. The confusion comes from the similar names - focus on **RTTS/RRTS** for DMA operations

For packet reception, you need to implement **RRTS (HDEV+10)** behavior, where setting **DataAvailable (bit 0)** indicates a packet is ready for processing.