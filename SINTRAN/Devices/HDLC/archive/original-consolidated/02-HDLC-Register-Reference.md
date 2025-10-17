# HDLC Register Reference - Complete Register Map

**Complete COM5025 register map and bit definitions for SINTRAN HDLC implementation**

---

## Register Map Overview

### IOX Address Mapping

**Base Address**: HDEV (hardware-configured)  
**Register Count**: 18 registers (HDEV+0 through HDEV+17)

| Offset | Register | Read/Write | Full Name | Purpose |
|:------:|:---------|:----------:|-----------|---------|
| +0 | **RRDR** | Read | Read Receiver Data Register | Read received character data |
| +1 | **WPCR** | Write | Write Parameter Control Register | Configure parameters |
| +2 | **RRS** | Read | Read Receiver Status | Receiver status flags |
| +3 | **WSAR** | Write | Write Sync/Address Register | Set SYNC/address byte |
| +4 | **WCHL** | Write | Write Character Length | Set bits per character (5-8) |
| +5 | **WTDR** | Write | Write Transmitter Data Register | Write character to transmit |
| +6 | **RTSR** | Read | Read Transmitter Status Register | Transmitter status flags |
| +7 | **WTCR** | Write | Write Transmitter Control Register | Transmitter control |
| +10 | **RRTS** | Read | Read Receiver Transfer Status | **DMA receiver status** |
| +11 | **WRTC** | Write | Write Receiver Transfer Control | **DMA receiver control** |
| +12 | **RTTS** | Read | Read Transmitter Transfer Status | **DMA transmitter status** |
| +13 | **WTTC** | Write | Write Transmitter Transfer Control | **DMA transmitter control** |
| +14 | **RDMA** | Read | Read DMA Address (Least) | Read current DMA address |
| +15 | **WDMA** | Write | Write DMA Address (Least) | Set DMA descriptor address |
| +16 | **RDCR** | Read | Read DMA Command Register | Read DMA status |
| +17 | **WDCR** | Write | Write DMA Command Register + Trigger | Start DMA operation |

---

## RRTS - Read Receiver Transfer Status (IOX+10)

**Critical Register**: Primary status register for DMA receive operations and X.21 interface.

### Bit Map

| Bit | Name | Type | Interrupt | Clear Behavior | Description |
|:---:|:-----|:----:|:---------:|:---------------|:------------|
| 0 | **RXD** | Status | Level 13 | Auto | Data Available - character ready |
| 1 | **RXSA** | Status | Level 13 | Auto | Status Available - status info ready |
| 2 | **RXA** | Status | - | Auto | Receiver Active - within frame |
| 3 | **SFR** | Status | - | Read/Clear | Sync/Flag Received |
| 4 | **DMAR** | Trigger | Level 13 | **Instant** | DMA Module Request (always reads as 0) |
| 5 | **SD** | Status | Level 13 | - | Signal Detector (CCITT 109) |
| 6 | **DSR** | Status | Level 13 | - | Data Set Ready (CCITT 107/X.21 I) |
| 7 | **RI** | Status | Level 13 | - | Ring Indicator (CCITT 125) |
| 8 | **BE** | DMA | Level 13 | On Read | Block End - DMA block complete |
| 9 | **FE** | DMA | Level 13 | On Read | Frame End - DMA frame complete |
| 10 | **LE** | DMA | Level 13 | On Read | List End - DMA list complete |
| 11 | **EMTY** | DMA | Level 13 | On Read | List Empty - no buffers available |
| 12-14 | - | Reserved | - | On Read | Reserved (cleared on read) |
| 13 | **X21D** | Error | Level 13 | **Persistent** | X.21 Data Error |
| 14 | **X21S** | Error | Level 13 | **Persistent** | X.21 Clear Indication |
| 15 | **OR** | Error | Level 13 | **Persistent** | Receiver Overrun |

### SINTRAN Bit Constants

| Constant | Value | Bits | Purpose |
|----------|-------|------|---------|
| **EMTY** | 0x0800 | 11 | List Empty flag |
| **HX21M** | 0x6000 | 13-14 | X.21 Error Mask |
| **HX21S** | 0x000E | 1-3 | Receiver State (not X.21!) |

### Critical Processing Logic

**SINTRAN Reception Check (Line 104436+):**
```assembly
HIINT: T:=HDEV+RRTS; *EXR ST     % Read RRTS
       A=:HASTAT                 % Store in HASTAT
       
       % Check 1: X.21 protocol errors
       IF A/\ HX21M >< 0 THEN    % Test bits 13-14
          IF A BIT HX21S THEN    % Test bits 1-3 (receiver state)
             HASTAT BONE BLDON=:HASTAT  % Terminate cleanly
          FI
       FI
       
       % Check 2: Buffer availability
       IF HASTAT/\"EMTY" >< 0 THEN   % Test bit 11
          0=:ACTSW                   % Stop device
       FI
       
       % Check 3: Data valid
       IF A NBIT 0 OR A/\60000><0 THEN  % No data OR X.21 error
          return; % Drop packet
       FI
```

**C# Implementation:**
```csharp
public bool ShouldProcessPacket(ReceiverStatusBits rrts)
{
    // Check 1: Data available
    if ((rrts & ReceiverStatusBits.DataAvailable) == 0)
        return false;  // No data
    
    // Check 2: X.21 protocol errors
    if ((rrts & (ReceiverStatusBits.X21D | ReceiverStatusBits.X21S)) != 0)
        return false;  // X.21 error
    
    // Check 3: Buffer exhaustion
    if ((rrts & ReceiverStatusBits.ListEmpty) != 0)
        return false;  // No buffers
    
    return true;  // Process packet
}
```

### Hardware Auto-Clear Behavior

**Cleared IMMEDIATELY (before read completes):**
- Bit 4 (DMAR): DMA Module Request

**Cleared AFTER read completes:**
- Bits 8-12: DMA status (BE, FE, LE, EMTY, Reserved)

**Persistent (require WRTC device clear):**
- Bit 13 (X21D): X.21 Data Error
- Bit 14 (X21S): X.21 Clear Indication
- Bit 15 (OR): Receiver Overrun

---

## RTTS - Read Transmitter Transfer Status (IOX+12)

**Critical Register**: Primary status register for DMA transmit operations.

### Bit Map

| Bit | Name | Type | Interrupt | Clear Behavior | Description |
|:---:|:-----|:----:|:---------:|:---------------|:------------|
| 0 | **TXBE** | Status | Level 12 | Auto | Transmit Buffer Empty |
| 1 | **TXU** | Error | Level 12 | **Persistent** | Transmitter Underrun |
| 2 | **TXA** | Status | - | Auto | Transmitter Active |
| 3 | - | Reserved | - | - | Reserved |
| 4 | **DMAR** | Trigger | Level 12 | **Instant** | DMA Module Request (always reads as 0) |
| 5 | - | Reserved | - | - | Reserved |
| 6 | **RFS** | Status | Level 12 | - | Ready for Sending (CCITT 106) |
| 7 | - | Reserved | - | - | Reserved |
| 8 | **BE** | DMA | Level 12 | On Read | Block End - DMA block complete |
| 9 | **FE** | DMA | Level 12 | On Read | Frame End - DMA frame complete |
| 10 | **LE** | DMA | Level 12 | On Read | List End - DMA list complete |
| 11 | **TRFIN** | DMA | Level 12 | On Read | Transmission Finished |
| 12-14 | - | Reserved | - | On Read | Reserved (cleared on read) |
| 15 | **ER** | Error | Level 12 | **Persistent** | Illegal Key/Format Error |

### SINTRAN Bit Constants

| Constant | Value | Bits | Purpose |
|----------|-------|------|---------|
| **TXUND** | 0x0002 | 1 | Transmitter Underrun |
| **SILFO** | 0x8000 | 15 | Illegal Format/Key Error |

### Critical Transmission Check

**SINTRAN Success Logic (Line 104033+):**
```assembly
HOINT: T:=HDEV+RTTS; *EXR ST     % Read RTTS
       A=:HASTAT                 % Store in HASTAT
       
       % SUCCESS CHECK - CRITICAL
       IF A/\ "SILFO+TXUND" = 0 THEN    % (status & 0x8002) == 0
          % SUCCESS: Neither illegal format nor underrun
          XRETRY=:RTDYN; A:=0; CALL SADTS  % Clear retry, log success
          0=:ACTSW                         % Mark inactive
          CALL NEXTS                       % Next frame
       ELSE
          % TRANSMISSION ERROR
          XRETRY+1=:XRETRY                 % Increment retry
          IF XRETRY > MAXRETRY THEN
             A:=237; CALL DRERR            % Report error
          ELSE
             CALL RETRANSMIT               % Retry
          FI
       FI
```

**C# Implementation:**
```csharp
public bool IsTransmissionSuccessful(TransmitterStatusBits rtts)
{
    // SINTRAN: IF A/\ "SILFO+TXUND" = 0 THEN success
    // SILFO (0x8000) = Illegal Format (bit 15)
    // TXUND (0x0002) = Transmitter Underrun (bit 1)
    
    const TransmitterStatusBits ErrorMask = 
        TransmitterStatusBits.Illegal | 
        TransmitterStatusBits.TransmitterUnderrun;
    
    return (rtts & ErrorMask) == 0;  // Success if both clear
}
```

### Recommended RTTS Values

**Successful Transmission:**
```csharp
// Option 1: Minimal success
rtts = 0x0800;  // Bit 11: Transmission Finished only

// Option 2: With status flags
rtts = 0x0841;  // Bits 11, 6, 0: TRFIN + RFS + TXBE
```

**Transmission Error:**
```csharp
// Underrun error
rtts = 0x0002;  // Bit 1: Transmitter Underrun

// Illegal format error
rtts = 0x8000;  // Bit 15: Illegal Key/Format
```

---

## WRTC - Write Receiver Transfer Control (IOX+11)

**Purpose**: Control DMA receiver operations and device initialization.

### Control Values

| Value (Octal) | Value (Hex) | Purpose |
|:-------------:|:-----------:|:--------|
| 000 | 0x00 | **Device Clear** - reset receiver to idle state |
| 040 | 0x20 | **Maintenance Mode** - diagnostic mode |
| 100 | 0x40 | **Basic Receiver Enable** - normal operation |
| 140 | 0x60 | **Maintenance + Clear** - thorough reset |
| 1734 | 0x3DC | **DMA Mode Enable** - full DMA operation |

### Device Clear Sequence

**SINTRAN X.21 Error Clearing (X21SH function):**
```assembly
X21SH: A:=0; T:=X2DHD+XWRTC; *EXR ST     % DEVICE CLEAR
       A:=40; *EXR ST                     % MAINTENANCE MODE
       *AAT 6; EXR ST                     % CLEAR DMA
       A:=0; T:=X2DHD+XWRTC; *EXR ST      % NORMAL MODE
```

**C# Implementation:**
```csharp
public void ClearReceiverErrors()
{
    // Step 1: Device Clear
    WriteRegister(WRTC, 0x00);  // Clear all receiver state
    
    // Step 2: Maintenance Mode (optional for thorough reset)
    WriteRegister(WRTC, 0x20);
    
    // Step 3: Clear DMA controller
    ClearDMAController();
    
    // Step 4: Return to normal
    WriteRegister(WRTC, 0x40);  // Basic receiver enable
}
```

---

## WTTC - Write Transmitter Transfer Control (IOX+13)

**Purpose**: Control DMA transmitter operations.

### Control Values

| Value (Octal) | Value (Hex) | Purpose |
|:-------------:|:-----------:|:--------|
| 0 | 0x00 | **Transmitter Off** - disable transmission |
| 1134+CMODI | 0x25C+mode | **DMA Transmit Enable** - start transmission |

**Note**: CMODI is a mode flag (typically 0 or 040 octal).

### Transmitter Start Sequence

**SINTRAN Transmission Start (XHMST function):**
```assembly
XHMST: LIINT+DPITPHYS;                   % Calculate DMA address
       T:=HDEV+WDMA; *IOF; EXR ST        % Write DMA address
       A:=2000\/D; T+"WDCR-WDMA"; *EXR ST % Write DMA command (0x400)
       T+"RDCR-WDCR"; X:=-20;*EXR ST     % Verify status
       CALL LTOUT; *JAF *-2; ION         % Timeout check
       1134+CMODI; T:=HDEV+WTTC; *EXR ST % Enable transmission
       1 =: ACTSW                        % Mark active
```

**C# Implementation:**
```csharp
public void StartTransmission(uint descriptorAddress)
{
    // Step 1: Write DMA descriptor address
    WriteRegister(WDMA, (ushort)(descriptorAddress & 0xFFFF));
    
    // Step 2: Write DMA command to start transmitter
    WriteRegister(WDCR, 0x0400);  // 2000 octal = Start transmitter
    
    // Step 3: Verify DMA started
    ushort status = ReadRegister(RDCR);
    if ((status & 0x8000) == 0)  // Check DMA active bit
        throw new Exception("DMA failed to start");
    
    // Step 4: Enable transmitter DMA mode
    WriteRegister(WTTC, 0x025C);  // 1134 octal = DMA transmit enable
    
    // Step 5: Mark device active
    deviceActive = true;
}
```

---

## WDMA/WDCR - DMA Control Registers

### WDMA (IOX+15) - Write DMA Address

**Purpose**: Set DMA descriptor list start address (least significant word).

**Usage:**
```assembly
A:=descriptor_address_low
T:=HDEV+WDMA
*EXR ST
```

**C# Implementation:**
```csharp
public void SetDMAAddress(uint physicalAddress)
{
    // Write least significant 16 bits
    ushort addressLow = (ushort)(physicalAddress & 0xFFFF);
    WriteRegister(WDMA, addressLow);
    
    // Most significant bits handled by DMA controller
    // or separate upper address register (hardware-dependent)
}
```

### WDCR (IOX+17) - Write DMA Command + Trigger

**Purpose**: Start DMA operation with specified command.

**Command Values:**

| Command | Octal | Hex | Purpose |
|---------|:-----:|:---:|:--------|
| **Start Transmitter** | 2000 | 0x0400 | Initiate DMA transmission |
| **Start Receiver** | 1001 | 0x0201 | Initiate DMA reception |
| **Initialize** | 401 | 0x0101 | Reset DMA controller |

**Usage:**
```assembly
A:=2000     % Command: Start transmitter
T:=HDEV+WDCR
*EXR ST
```

### RDCR (IOX+16) - Read DMA Command/Status

**Purpose**: Read current DMA operation status.

**Status Bits** (hardware-specific, typical layout):
- Bit 15: DMA Active
- Bit 14: DMA Error
- Bits 0-13: Command/status information

---

## Other Registers (Brief)

### Basic COM5025 Registers

**RRDR (IOX+0)** - Read Receiver Data:
- Returns last received character
- Used in non-DMA mode

**WTDR (IOX+5)** - Write Transmitter Data:
- Send character to transmit
- Used in non-DMA mode

**RRS (IOX+2)** - Read Receiver Status:
- Low-level receiver status
- Frame sync, parity, etc.

**RTSR (IOX+6)** - Read Transmitter Status:
- Low-level transmitter status
- Buffer state, etc.

**WPCR (IOX+1)** - Write Parameter Control:
- Configure protocol mode
- Set HDLC/SDLC/BiSync mode

**WSAR (IOX+3)** - Write Sync/Address:
- Set synchronization byte
- Set station address

**WCHL (IOX+4)** - Write Character Length:
- Set bits per character (5-8)
- Typically 8 for HDLC

**WTCR (IOX+7)** - Write Transmitter Control:
- Low-level transmitter control
- Enable/disable, mode settings

---

## Register Access Patterns

### Initialization Sequence

**SINTRAN Device Initialization:**
```assembly
% Step 1: Device Clear
A:=100; T:=HDEV+WRTC; *EXR ST

% Step 2: Maintenance Mode
A:=140; *EXR ST

% Step 3: Configure Parameters
A:=hdlc_mode; T:=HDEV+WPCR; *EXR ST

% Step 4: Set Character Length
A:=8; T:=HDEV+WCHL; *EXR ST

% Step 5: Return to Normal
A:=100; T:=HDEV+WRTC; *EXR ST
```

### Transmission Sequence

1. Set up DMA descriptor list in memory
2. Write descriptor address to WDMA (IOX+15)
3. Write command to WDCR (IOX+17) to start DMA
4. Enable transmitter via WTTC (IOX+13)
5. Wait for interrupt (Level 12)
6. Read RTTS (IOX+12) to check status
7. Handle completion or error

### Reception Sequence

1. Set up receive buffer list in memory
2. Write buffer address to WDMA (IOX+15)
3. Write command to WDCR (IOX+17) to start receiver
4. Enable receiver via WRTC (IOX+11)
5. Wait for interrupt (Level 13)
6. Read RRTS (IOX+10) to check status
7. Process received data or handle error

---

## Quick Reference Tables

### Interrupt Summary

| Level | Source Register | Condition | Handler |
|:-----:|:---------------:|-----------|---------|
| 12 | RTTS (IOX+12) | Transmit completion/error | HOINT |
| 13 | RRTS (IOX+10) | Receive completion/error | HIINT |

### Error Bit Summary

| Error | Register | Bit | Clear Method |
|-------|:--------:|:---:|--------------|
| Transmitter Underrun | RTTS | 1 | Device Clear |
| Illegal Format/Key | RTTS | 15 | Device Clear |
| X.21 Data Error | RRTS | 13 | WRTC Clear |
| X.21 Clear Indication | RRTS | 14 | WRTC Clear |
| Receiver Overrun | RRTS | 15 | WRTC Clear |
| List Empty | RRTS | 11 | Auto-clear on read |

### Success Detection

| Operation | Register | Test | Success When |
|-----------|:--------:|------|--------------|
| **Transmission** | RTTS | `(rtts & 0x8002) == 0` | Both error bits clear |
| **Reception** | RRTS | `(rrts & 0x0001) != 0` | Data available |
| **X.21 Status** | RRTS | `(rrts & 0x6000) == 0` | No protocol errors |

---

## Related Documentation

- **[01-HDLC-Hardware-Reference.md](01-HDLC-Hardware-Reference.md)** - Hardware specifications and constants
- **[03-HDLC-DMA-Operations.md](03-HDLC-DMA-Operations.md)** - DMA descriptor structure
- **[04-HDLC-Interrupt-Handlers.md](04-HDLC-Interrupt-Handlers.md)** - Interrupt processing details
- **[06-HDLC-Emulator-Guide.md](06-HDLC-Emulator-Guide.md)** - C# implementation guide

---

**Document Version**: 2.0 (Consolidated)  
**Last Updated**: 2025-10-17  
**Source Files**: HDLC_Complete_Register_Analysis.md, HDLC_Register_Usage_Analysis.md, HDLC_Status_Bit_Analysis.md, RTSR_vs_RTTS_Register_Analysis.md, Receiver_DMA_Status_Bits_Analysis.md, Receiver_Enable_Bits_Analysis.md, Transmitter_Enable_Bits_Analysis.md

