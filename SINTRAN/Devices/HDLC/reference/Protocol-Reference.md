# HDLC Protocol Implementation

**LAPB, X.25, and protocol-level analysis for SINTRAN HDLC**

---

## Protocol Layers

### LAPB (Link Access Procedure Balanced)

**Layer**: Data Link Layer (Layer 2)  
**Standard**: ISO/IEC 13239  
**Mode**: Balanced (both stations equal)

**LAPB Frame Structure**:
```
FLAG | ADDRESS | CONTROL | DATA | FCS | FLAG
0x7E | 1 byte  | 1 byte  | N    | 2   | 0x7E
```

**Control Field Types**:
- **I-frames** (Information): Bit 0 = 0, carry user data + sequence numbers
- **S-frames** (Supervisory): Bits 0-1 = 01, flow control (RR, RNR, REJ)
- **U-frames** (Unnumbered): Bits 0-1 = 11, link management (SABM, DISC, UA, DM)

### X.25 Protocol

**Layer**: Network Layer (Layer 3)  
**Runs over**: LAPB (Layer 2)  
**Requires**: X.21 interface for call setup

**X.25 Packet Types**:
- **Call Setup**: CALL REQUEST, CALL ACCEPT
- **Data Transfer**: DATA packets
- **Flow Control**: RR (Receive Ready), RNR (Receive Not Ready)
- **Call Clearing**: CLEAR REQUEST, CLEAR CONFIRMATION

### Protocol Mode Detection

**SINTRAN supports both modes:**

**Pure LAPB Mode** (Current implementation):
- No X.21 signaling required
- Simple frame exchange
- Bits 13-14 (X21D/X21S) kept clear
- HX21S bits (1-3) not used

**X.25 Network Mode** (Advanced):
- Full X.21 interface required
- Call setup/clearing via X.21
- HX21S bits (1-3) track receiver state
- Connection management needed

## SINTRAN Protocol Variables

### Key Variables

| Variable | Purpose | Values |
|----------|---------|--------|
| **INTSTA** | Interface Status | 0=uninitialized, 2=initialized |
| **ACTSW** | Active Switch | 0=idle, 1=active |
| **HASTAT** | Hardware Status | Last RRTS/RTTS value |
| **XRETRY** | Retry Counter | 0-MAXRETRY |
| **LISTP** | List Pointer | Current DMA descriptor |
| **LIINT** | List Interrupt | DMA list interrupt pointer |

### State Tracking

```assembly
% Initialization check
IF INTSTA >< 2 THEN
   A:=ENINIT; GO BACKX  % Error: not initialized
FI

% Activity check
IF ACTSW = 0 THEN
   % Device idle - can start operation
ELSE
   % Device busy - queue or wait
FI
```

## Pseudocode Reference

### Transmission Pseudocode (XSSDATA)

```
XSSDATA:
    1. Check INTSTA == 2 (initialized)
    2. Set LISTP = LIINT (DMA list pointer)
    3. Build DMA descriptor with LKEY = FSERM (0x1003)
    4. Calculate physical address of descriptor
    5. Write address to WDMA (IOX+15)
    6. Write DMA command 0x0400 to WDCR (IOX+17)
    7. Enable transmitter via WTTC (IOX+13) = 0x025C
    8. Set ACTSW = 1 (mark active)
    9. Wait for HOINT interrupt
    10. Check RTTS: success if (RTTS & 0x8002) == 0
    11. If error: retry up to MAXRETRY times
    12. Return status to caller
```

### Reception Pseudocode (XSSREC)

```
XSSREC:
    1. Check INTSTA == 2 (initialized)
    2. Allocate receive buffer list
    3. Set LKEY = 0x0400 (empty receiver block)
    4. Write buffer address to WDMA
    5. Write DMA command 0x0201 to WDCR
    6. Enable receiver via WRTC (IOX+11)
    7. Set ACTSW = 1
    8. Wait for HIINT interrupt
    9. Check RRTS:
       - If bit 0 = 0: no data, ignore
       - If bits 13-14 != 0: X.21 error, handle
       - If bit 11 = 1: buffer empty, stop
    10. Process received data from buffers
    11. Return data to caller
```

## PAD Connection Protocol

**PAD**: Packet Assembler/Disassembler - converts between async terminal and packet-switched network

### PAD Connection Sequence

```
1. Terminal connects to PAD (async serial)
2. PAD establishes X.25 call to remote host
3. PAD assembles terminal characters into X.25 packets
4. PAD disassembles X.25 packets to terminal characters
5. On disconnect: PAD sends CLEAR REQUEST
```

### SINTRAN PAD Support

SINTRAN handles PAD connections through:
- **LAPB layer**: Reliable frame transport
- **X.25 layer**: Packet encapsulation
- **TAD protocol**: Terminal access device extensions

## Protocol Constants

### Frame Constants

| Constant | Value | Purpose |
|----------|-------|---------|
| **FLAG** | 0x7E | Frame delimiter |
| **FSERM** | 0x1003 | Single frame LKEY (TSOM+TEOM) |
| **MAXRETRY** | 3-5 | Maximum transmission retries |

### Error Codes

| Code | Octal | Meaning |
|------|-------|---------|
| **ENINIT** | 236 | Device not initialized |
| **ETRANS** | 237 | Transmission failure |
| **EUND** | 102 | Underrun error (bits 1,6) |

## Protocol State Machines

### Transmission State Machine

```
IDLE
  ↓ [User sends data]
SETUP_DMA
  ↓ [Write registers]
TRANSMITTING (ACTSW=1)
  ↓ [Level 12 interrupt]
CHECK_STATUS
  ├─ [(RTTS & 0x8002) == 0] → SUCCESS → IDLE
  └─ [(RTTS & 0x8002) != 0] → ERROR → RETRY or FAIL
```

### Reception State Machine

```
IDLE
  ↓ [Setup buffers]
RECEIVING (ACTSW=1)
  ↓ [Level 13 interrupt]
CHECK_STATUS
  ├─ [RRTS bit 0 = 1, no errors] → PROCESS_DATA → RECEIVING
  ├─ [RRTS bit 11 = 1] → BUFFER_EMPTY → IDLE
  └─ [RRTS bits 13-14 != 0] → X21_ERROR → HANDLE_ERROR
```

---

**See Also**:
- [01-HDLC-Hardware-Reference.md](01-HDLC-Hardware-Reference.md) - X.21 protocol details
- [04-HDLC-Interrupt-Handlers.md](04-HDLC-Interrupt-Handlers.md) - State machine implementation
- [06-HDLC-Emulator-Guide.md](06-HDLC-Emulator-Guide.md) - Complete C# implementation

**Document Version**: 2.0 (Consolidated)  
**Source Files**: SINTRAN_HDLC_Complete_Pseudocode.md, SINTRAN_HDLC_Pseudocode.md, LAPB_vs_X25_Protocol_Handling.md, PAD_Connection_Deep_Analysis.md, HDLC_Variable_Reference.md, SINTRAN_Variable_Name_Analysis.md

