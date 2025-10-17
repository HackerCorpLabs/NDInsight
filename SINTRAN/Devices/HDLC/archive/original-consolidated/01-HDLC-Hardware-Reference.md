# HDLC Hardware Reference - COM5025 and X.21 Interface

**Complete hardware specification for SINTRAN HDLC implementation**

---

## Table of Contents

1. [Hardware Overview](#hardware-overview)
2. [COM5025 Multi-Protocol Controller](#com5025-multi-protocol-controller)
3. [X.21 Interface](#x21-interface)
4. [Critical Hardware Constants](#critical-hardware-constants)
5. [Hardware Behavior](#hardware-behavior)
6. [IOX Bus Integration](#iox-bus-integration)

---

## Hardware Overview

### Architecture

```
SINTRAN OS (NPL Code)
    ↓ IOX Bus Commands
ND-100 IOX Bus (HDEV+0 to HDEV+17)
    ↓ Register Access
COM5025 Multi-Protocol Controller
    ↓ DMA Descriptors  
DMA Hardware Module
    ↓ Framing/Protocol
X.21 Physical Interface
    ↓ Serial Communication
Remote Device
```

### Key Components

| Component | Purpose | Interface |
|-----------|---------|-----------|
| **COM5025 Chip** | HDLC frame processing, CRC generation | Register-based control |
| **DMA Module** | High-speed data transfer without CPU | Descriptor-based lists |
| **X.21 Interface** | Physical serial communication | CCITT X.21 standard |
| **IOX Bus** | ND-100 I/O extension bus | Memory-mapped I/O |

---

## COM5025 Multi-Protocol Controller

### Chip Overview

**Manufacturer**: Standard Microsystems Corporation (SMC) / AMD  
**Type**: Multi-Protocol Communications Controller  
**Protocols Supported**: HDLC, SDLC, BiSync, Async  
**Data Rate**: Up to 2 Mbps  
**Features**: Automatic CRC, flag generation, zero insertion/deletion

### COM5025 Control Bits (Used in DMA Descriptors)

These bits are embedded in the LKEY field of DMA descriptors (bits 7-0):

| Bit | Name | Purpose | Effect |
|:---:|:-----|:--------|:-------|
| 0 | **TSOM** | Transmit Start Of Message | Generate opening FLAG sequence (0x7E) |
| 1 | **TEOM** | Transmit End Of Message | Generate closing FLAG + append CRC |
| 2 | **TABORT** | Transmit Abort | Send ABORT sequence (7+ consecutive 1s) |
| 3 | **TGA** | Transmit Go Ahead | Send GA (Go Ahead) character |
| 4-7 | - | Additional Control | COM5025-specific functions |

### Frame Structure

**HDLC Frame Format:**
```
FLAG | ADDRESS | CONTROL | DATA | CRC-16 | FLAG
0x7E | 1 byte  | 1 byte  | N bytes | 2 bytes | 0x7E
```

**CRC Calculation:**
- **Polynomial**: CRC-16-CCITT (x^16 + x^12 + x^5 + 1)
- **Initial Value**: 0xFFFF
- **Covers**: Address + Control + Data fields
- **Hardware Automatic**: COM5025 handles CRC generation/checking

---

## X.21 Interface

### X.21 Standard Overview

**CCITT Recommendation X.21**: Interface between Data Terminal Equipment (DTE) and Data Circuit-terminating Equipment (DCE) for synchronous operation on public data networks.

### X.21 Signals

| Signal | CCITT No. | Direction | Purpose |
|--------|-----------|-----------|---------|
| **T** (Transmit) | 103 | DTE → DCE | Serial data transmission |
| **R** (Receive) | 104 | DCE → DTE | Serial data reception |
| **C** (Control) | 105 | DTE → DCE | Control signal (ON/OFF) |
| **I** (Indication) | 106 | DCE → DTE | Response signal (ON/OFF) |
| **Signal Element Timing** | 114 | DCE → DTE | Bit synchronization clock |
| **Byte Timing** | 115 | DCE → DTE | Byte/character synchronization |

### X.21 State Encoding in RRTS

**Bits 13-14 (HX21M = 0x6000)**: X.21 Protocol Status

| Bit 14 (X21S) | Bit 13 (X21D) | State | Meaning |
|:-------------:|:-------------:|:------|:--------|
| 0 | 0 | **Normal** | No X.21 protocol errors |
| 0 | 1 | **Data Error** | X.21 data link fault detected |
| 1 | 0 | **Clear Indication** | DCE requesting connection termination |
| 1 | 1 | **Reserved** | Undefined state |

### X.21 Error Handling

**SINTRAN Detection Logic:**
```assembly
IF (RRTS & HX21M) != 0 THEN    % HX21M = 0x6000 = bits 13-14
    % X.21 protocol error detected
    % Check receiver state for clean termination
    IF (RRTS & HX21S) != 0 THEN  % HX21S = 0x000E = bits 1-3
        % Receiver active - terminate cleanly
        TERMINATE_FRAME();
    FI
FI
```

**C# Implementation:**
```csharp
public void HandleX21Status(ReceiverStatusBits rrts)
{
    if ((rrts & ReceiverStatusBits.X21Mask) != 0)  // 0x6000
    {
        // X.21 protocol error detected
        if ((rrts & ReceiverStatusBits.X21D) != 0)
        {
            // Data link fault
            HandleX21DataError();
        }
        
        if ((rrts & ReceiverStatusBits.X21S) != 0)
        {
            // Clear indication - connection terminating
            HandleX21ClearIndication();
        }
    }
}
```

---

## Critical Hardware Constants

### From SYMBOL-1-LIST.SYMB.TXT

| Constant | Octal | Hex | Binary | Bits | Description |
|----------|-------|-----|--------|------|-------------|
| **TXUND** | 000002 | 0x0002 | 0000000000000010 | 1 | Transmitter Underrun Error |
| **SILFO** | 100000 | 0x8000 | 1000000000000000 | 15 | Illegal Format/Key Error |
| **EMTY** | 004000 | 0x0800 | 0000100000000000 | 11 | List Empty (No Buffers) |
| **HX21M** | 060000 | 0x6000 | 0110000000000000 | 13-14 | X.21 Error Mask |
| **HX21S** | 000016 | 0x000E | 0000000000001110 | 1-3 | Receiver State Bits |
| **BLDON** | 000010 | 0x0008 | 0000000000001000 | 3 | Block Done Flag |
| **ERB** | 001000 | 0x0200 | 0000001000000000 | 9 | Error Block Indicator |
| **EUND** | 000102 | 0x0042 | 0000000001000010 | 1,6 | Underrun Error Code |
| **FSERM** | 002003 | 0x1003 | 0001000000000011 | 10,0-1 | Single Frame Transmission |

### DMA Command Values

| Command | Octal | Hex | Decimal | Purpose |
|---------|-------|-----|---------|---------|
| **Transmitter Start** | 2000 | 0x400 | 1024 | Start DMA transmission |
| **Receiver Start** | 1001 | 0x201 | 513 | Start DMA reception |
| **Initialize** | 401 | 0x101 | 257 | Initialize DMA controller |

### Control Register Values

| Register | Value (Octal) | Value (Hex) | Purpose |
|----------|---------------|-------------|---------|
| **WRTC** | 100 | 0x40 | Basic receiver control |
| **WRTC** | 140 | 0x60 | Maintenance mode |
| **WRTC** | 1734 | 0x3DC | DMA mode enable |
| **WTTC** | 0 | 0x00 | Transmitter off |
| **WTTC** | 1134+CMODI | 0x25C+mode | DMA transmission enable |

---

## Hardware Behavior

### Auto-Clear Behavior

**Critical Discovery**: Some status bits auto-clear on read, others are persistent.

#### RRTS (Receiver Transfer Status) - IOX+10

**Auto-Clear Bits (cleared after IOX+10 read):**
- Bit 4: DMA Module Request (cleared at start of IOX instruction)
- Bits 8-12: DMA status (Block End, Frame End, List End, List Empty)

**Persistent Bits (require explicit clear via WRTC):**
- Bit 13: X21D (X.21 Data Error)
- Bit 14: X21S (X.21 Clear Indication)
- Bit 15: Receiver Overrun

#### RTTS (Transmitter Transfer Status) - IOX+12

**Auto-Clear Bits (cleared after IOX+12 read):**
- Bit 4: DMA Module Request (cleared at start of IOX instruction)
- Bits 8-12: DMA status (Block End, Frame End, List End, Transmission Finished)

**Persistent Bits (require Master/Device Clear):**
- Bit 1: TXU (Transmitter Underrun)
- Bit 15: ER (Illegal Key/Format)

### Interrupt Behavior

**Level 12 Interrupts (Transmitter):**
- Transmit Buffer Empty (bit 0)
- Transmitter Underrun (bit 1)
- Ready for Sending signal change (bit 6)
- **DMA completion** (bit 4, auto-clears, status in bits 8-15)

**Level 13 Interrupts (Receiver):**
- Data Available (bit 0)
- Status Available (bit 1)
- Signal Detector change (bit 5)
- Data Set Ready change (bit 6)
- Ring Indicator change (bit 7)
- **DMA completion** (bit 4, auto-clears, status in bits 8-15)

### DMA Module Request Sequence

**Hardware Behavior (from official specification):**

```
1. DMA operation completes
2. Hardware sets bit 4 (DMA Module Request)
3. Hardware triggers interrupt (Level 12 or 13)
4. CPU begins IOX instruction to read status register
5. Hardware clears bit 4 IMMEDIATELY (before read completes)
6. CPU reads register (bit 4 = 0, bits 8-15 = DMA status)
7. Hardware clears bits 8-15 AFTER read completes
```

**Critical Insight**: Bit 4 is **never seen as 1** by software because it auto-clears before the read instruction completes.

**C# Emulation:**
```csharp
public ushort ReadRRTS()
{
    // Step 1: Clear DMA request bit immediately
    currentRRTS &= ~0x0010;  // Clear bit 4
    
    // Step 2: Return full status including DMA bits
    ushort result = currentRRTS;
    
    // Step 3: Clear DMA status bits after return
    currentRRTS &= 0x00FF;   // Clear bits 8-15
    
    return result;
}
```

---

## IOX Bus Integration

### Memory-Mapped I/O

**Base Address**: HDEV (set by hardware configuration)  
**Register Offsets**: HDEV+0 through HDEV+17  
**Access Method**: IOX instructions (EXR ST)

### IOX Instruction Sequence

**Read Register:**
```assembly
T:=HDEV+RRTS    % Set T register to register address
*EXR ST         % Execute IOX instruction, result in A register
A=:HASTAT       % Store result in variable
```

**Write Register:**
```assembly
A:=value        % Set A register to value to write
T:=HDEV+WTTC    % Set T register to register address
*EXR ST         % Execute IOX instruction
```

### Register Access Timing

**Typical Timing:**
- Register read: ~2-4 CPU cycles
- Register write: ~2-4 CPU cycles
- DMA transfer: Parallel to CPU (no wait states)

**Interrupt Latency:**
- Level 12/13 interrupt: ~10-20 CPU cycles to handler entry
- SINTRAN overhead: Variable based on system load

---

## Hardware Specifications Summary

| Specification | Value |
|---------------|-------|
| **Maximum Data Rate** | 2 Mbps (COM5025 limit) |
| **Minimum Data Rate** | ~1200 bps (typical) |
| **CRC Polynomial** | CRC-16-CCITT |
| **Frame Format** | HDLC (ISO 13239) |
| **Clock Source** | External (X.21 Signal Element Timing) |
| **DMA Block Size** | Variable (1-65535 bytes) |
| **Buffer Count** | Limited by available memory |
| **Interrupt Levels** | 12 (transmit), 13 (receive) |

---

## Related Documentation

- **[02-HDLC-Register-Reference.md](02-HDLC-Register-Reference.md)** - Complete register map and bit definitions
- **[03-HDLC-DMA-Operations.md](03-HDLC-DMA-Operations.md)** - DMA descriptor structure and operations
- **[06-HDLC-Emulator-Guide.md](06-HDLC-Emulator-Guide.md)** - C# implementation guide

---

**Document Version**: 2.0 (Consolidated)  
**Last Updated**: 2025-10-17  
**Source Files**: HDLC_Hardware_Specification_Analysis.md, SINT RAN_COM5025_Interface_Deep_Analysis.md, X21_Bits_Detailed_Analysis.md, HDLC_Constants_Analysis.md, Critical_Bit_Usage_Analysis.md

