# HDLC Reception Analysis: Complete SINTRAN Flow and Requirements
**Comprehensive Analysis of NORSK DATA SINTRAN HDLC Packet Reception**

## Executive Summary

### What We Are Trying to Achieve
Create a perfect HDLC controller emulator that successfully delivers packets to SINTRAN applications by understanding and implementing the exact hardware register patterns and validation logic that SINTRAN expects.

### The Challenge
SINTRAN's HDLC reception involves complex multi-layered validation:
1. **Hardware Status Validation** (RRTS register analysis)
2. **DMA Descriptor Processing** (XBLDN flag and block management)
3. **Buffer Content Validation** (RCOST pattern matching)
4. **Protocol State Management** (X.21 error handling)

### Key Hurdles Overcome
- **Bit Importance Confusion**: Only 5 out of 16 RRTS bits actually matter to SINTRAN
- **Hardware vs Software Flags**: RRTS BlockEnd/FrameEnd bits are ignored; XBLDN in DMA descriptor is critical
- **Multiple Validation Layers**: Success requires passing ALL validation stages
- **Priority Processing Order**: ListEmpty overrides everything, causing receiver shutdown

## Complete SINTRAN HDLC Reception Flow

### Overview Architecture
```
Hardware Interrupt → HIINT (Hardware Validation) → HNOTRA (Content Validation) → OCHAIN (User Delivery)
```

## PART 1: HARDWARE REGISTERS AND BIT ANALYSIS

### HDLC Hardware Register Map

Based on complete SINTRAN source code analysis:

#### Read Registers (Input to SINTRAN)
| Register | Offset | Purpose | When Read | Critical for Success |
|----------|--------|---------|-----------|---------------------|
| **RRTS** | HDEV+10 (IOX+10) | Read Receiver Transfer Status | Every level 13 IRQ | **YES** - Only hardware status register read |

#### Write Registers (SINTRAN to Hardware)
| Register | Offset | Purpose | When Written | Critical for Setup |
|----------|--------|---------|--------------|-------------------|
| **WRTC** | HDEV+11 (IOX+11) | Write Receiver Transfer Control | Device setup | **YES** - Enables interrupts |
| **WDMA** | HDEV+15 (IOX+15) | Write DMA Address | DMA list setup | **YES** - Buffer management |
| **WDCR** | WDMA+2 (IOX+17) | Write DMA Command | Start DMA | **YES** - Enables DMA |

### CRITICAL: WRTC Interrupt Enable Sequence Analysis

**From SINTRAN source code analysis, WRTC register patterns:**

#### SINTRAN WRTC Write Patterns (IOX+11)
| Context | Value | Hex | Binary Pattern | Enabled Interrupts |
|---------|-------|-----|----------------|-------------------|
| **ZSTARC (Receiver Start)** | 0x35 | 0x0035 | `0000000000110101` | DataAvailable + EnableReceiver + DMAModule + DeviceClear |
| **OUT1 (Active Operation)** | 0x3DC | 0x03DC | `0000001111011100` | **ALL DMA interrupts enabled** |
| **RECSET (Basic Setup)** | 0x64 | 0x0040 | `0000000001000000` | EnableReceiver + DTR only |

**Key Discovery**: SINTRAN writes **0x3DC** to WRTC during active operation, enabling:
- DataAvailableIE (bit 0) - Character-level interrupts
- EnableReceiver (bit 2) - Hardware receiver enable
- EnableReceiverDMA (bit 3) - DMA request enable
- **DMAModuleIE (bit 4) - CRITICAL: DMA interrupt enable**
- DTR (bit 6) - Modem control
- ModemStatusChangeIE (bit 7) - Status change interrupts
- **BlockEndIE (bit 8) - REQUIRED for packet completion IRQ**
- **FrameEndIE (bit 9) - REQUIRED for frame completion IRQ**

#### EMULATOR CRITICAL REQUIREMENT
Your C# emulator log shows correct flag setting but may lack interrupt enable check:
```
RX_FLAGS_SET: BlockEnd, FrameEnd | Total_Status=DMAModuleRequest, BlockEnd, FrameEnd
RX_CONTROL_STATE: DMAModuleIE=True, DMAModuleRequest=True
```

**The emulator must verify:**
1. **BlockEndIE bit 8** is enabled in WRTC before triggering BlockEnd IRQ
2. **FrameEndIE bit 9** is enabled in WRTC before triggering FrameEnd IRQ
3. **DMAModuleIE bit 4** is enabled in WRTC before any DMA IRQ

**Interrupt Enable Logic:**
```csharp
public void TriggerReceiverInterrupt()
{
    // Only trigger if corresponding interrupt enable bits are set
    bool canTriggerBlockEnd = (currentWRTC & 0x0100) != 0;  // BlockEndIE
    bool canTriggerFrameEnd = (currentWRTC & 0x0200) != 0;  // FrameEndIE
    bool canTriggerDMAModule = (currentWRTC & 0x0010) != 0; // DMAModuleIE

    if ((currentRRTS & 0x0100) != 0 && canTriggerBlockEnd)   // BlockEnd set + enabled
        TriggerInterrupt(13);
    if ((currentRRTS & 0x0200) != 0 && canTriggerFrameEnd)  // FrameEnd set + enabled
        TriggerInterrupt(13);
    if ((currentRRTS & 0x0010) != 0 && canTriggerDMAModule) // DMAModuleRequest set + enabled
        TriggerInterrupt(13);
}
```

## Usage of HDLC Controller Registers

**Complete Analysis of SINTRAN HDLC Controller Setup, Initialization, and Cable Detection**

### HDLC Register Usage Patterns from SINTRAN Source Code

#### 1. Device Initialization Sequence (RECSET Function - Line 105233)

**Phase 1: Device Clear and Basic Setup**
```assembly
RECSET: A:=MAINT\/100                    ; Load maintenance mask (0x3D) OR 0x40
        T:=HDEV+WRTC; *EXR ST            ; Write to WRTC (IOX+11)
```

**Complete Bit Analysis**:
- **MAINT = 0x3D** sets bits 0,2,3,4,5:
  - Bit 0: `DataAvailableIE` = 1 - Data Available interrupts on level 13
  - Bit 2: `EnableReceiver` = 1 - Incoming serial data stream enabled
  - Bit 3: `EnableReceiverDMA` = 1 - Data Available causes DMA requests
  - Bit 4: `DMAModuleIE` = 1 - DMA module requests cause level 13 interrupts
  - Bit 5: `DeviceClear_SelectMaintenance` = 1 - Device clear + maintenance mode
- **Combined with 0x40**:
  - Bit 6: `DTR` = 1 - Data Terminal Ready signal (CCITT 108/X.21 C signal)
- **Final WRTC = 0x7D (125 decimal)** = Complete receiver initialization

#### 2. X.21 Initialization Sequence (Line 110125)

**Phase 2: Communication Protocol Setup**
```assembly
A:=0; T:=X2DHD+XWRTC; *EXR ST           ; Disable receiver first
A:=40; *EXR ST                          ; Set maintenance mode
A:=147; T:=X2DHD+XWPCR; *EXR ST         ; Parameter Control: 8-bit, strip sync
X21SY; T:=X2DHD+XWSAR; *EXR ST          ; Sync Address Register
A:=100; T:=X2DHD+XWTTC; *EXR ST         ; Transmitter control
```

**Complete Register Values with Bit Analysis:**

**WPCR = 0x67 (147 octal, 103 decimal)**:
- Bit 0-1: Character length = 00 (8-bit characters)
- Bit 4: Parity enable = 0 (no parity checking)
- Bit 6: Strip sync = 1 (remove sync characters from received data stream)
- Communication mode: X.21 synchronous protocol

**WSAR = X21SY (typically 0x16 = 22 decimal)**:
- X.21 standard sync character pattern
- Hardware will detect this pattern for frame synchronization

**WTTC = 0x40 (64 decimal)**:
- Bit 6: `RequestToSend` = 1 - RQTS signal (CCITT circuit 105) to DCE
- Controls modem/DCE Ready for Sending response

#### 3. Device Clear and Maintenance Setup (Line 110224)

**Phase 3: Hardware Reset Sequence**
```assembly
X21SH: A:=0; T:=X2DHD+XWRTC; *EXR ST    ; Device clear
       A:=0; T:=X2DHD+XWRTC; *EXR ST    ; Second device clear (out of maintenance)
       A:=107; T:=X2DHD+XWPCR; *EXR ST  ; BCP mode, no VRC control
       A:=377; T:=X2DHD+XWSAR; *EXR ST  ; Sync character = 0xFF
       A:=0; T:=X2DHD+XWCHL; *EXR ST    ; Character length = 8 bits
```

**Critical Device Clear Pattern with Bit Analysis:**

**Double WRTC write with A=0**:
- Clears all `RTCBits` flags: `DataAvailableIE`, `StatusAvailableIE`, `EnableReceiver`, `EnableReceiverDMA`, `DMAModuleIE`, `DeviceClear_SelectMaintenance`, `DTR`, `ModemStatusChangeIE`
- Ensures complete hardware reset and exits maintenance mode

**WPCR = 0x47 (107 octal, 71 decimal)**:
- Bit 0-1: Character length = 00 (8-bit characters)
- Bit 4: Parity enable = 0 (no parity checking)
- Bit 6: Strip sync = 1 (remove sync characters from data)
- Communication mode: Basic Control Procedure (BCP)

**WSAR = 0xFF (255 decimal)**:
- All-ones sync pattern for hardware initialization phase
- Hardware will detect this pattern during setup

**WCHL = 0x00**:
- Character length register = 0 (8-bit character mode)

#### 4. Modem Status and Cable Detection Analysis

**From SINTRAN Source Code - Signal Monitoring:**

**`RTSBits.SignalDetector` (RRTS bit 5) Usage:**
- **SINTRAN Reality**: **COMPLETELY IGNORED** - Never tested in any decision logic
- **Hardware Purpose**: CCITT 109 signal status change detection from DCE
- **Impact on Driver**: **NONE** - Driver continues regardless of signal presence
- **Emulator Setting**: Can be any value (0 or 1) - SINTRAN will not check it

**`RTSBits.DataSetReady` (RRTS bit 6) Usage:**
- **SINTRAN Reality**: **COMPLETELY IGNORED** - Never tested for cable presence
- **Hardware Purpose**: CCITT 107 status or X.21 I signal monitoring from DCE
- **Impact on Driver**: **NONE** - Driver operates without DSR checking
- **Emulator Setting**: Can be any value (0 or 1) - SINTRAN will not check it

**`RTSBits.RingIndicator` (RRTS bit 7) Usage:**
- **SINTRAN Reality**: **COMPLETELY IGNORED** - No incoming call detection
- **Hardware Purpose**: CCITT 125 status change for dial-up connections
- **Impact on Driver**: **NONE** - Point-to-point operation assumed
- **Emulator Setting**: Can be any value (0 or 1) - SINTRAN will not check it

**Cable Detection Reality:**
SINTRAN **does not perform cable detection**. The driver assumes:
- Cable is always connected
- Modem/DCE signals are always present
- Communication link is always available
- **Failure detection relies on protocol timeouts, not hardware signals**

#### 5. Active Operation Register Configuration

**During Active Communication (OUT1 Function - Line 104321):**
```assembly
OUT1: A:="1734"\/MAINT/\HXDOK         ; Load full operational mask
      T:=HDEV+WRTC; * EXR ST          ; Enable all interrupts
```

**Full Operational Pattern (1734 octal = 0x3DC = 988 decimal)**:
- Bit 2: `RTCBits.EnableReceiver` = 1 - Incoming serial data stream enabled
- Bit 3: `RTCBits.EnableReceiverDMA` = 1 - Data Available causes DMA requests
- Bit 4: `RTCBits.DMAModuleIE` = 1 - DMA module requests cause level 13 interrupts
- Bit 6: `RTCBits.DTR` = 1 - Data Terminal Ready signal maintained
- Bit 7: `RTCBits.ModemStatusChangeIE` = 1 - DCE status change interrupts enabled
- Bit 8: `RTCBits.BlockEndIE` = 1 - **CRITICAL: Block End interrupts enabled**
- Bit 9: `RTCBits.FrameEndIE` = 1 - **CRITICAL: Frame End interrupts enabled**
- **This is the critical pattern your emulator must recognize for active operation**

#### 6. Register Access Patterns and Expected Responses

**SINTRAN Register Write Sequence:**

**1. WRTC (IOX+11) - Receiver Transfer Control**
   - **Initialization (0x7D)**: `RTCBits.DataAvailableIE | RTCBits.EnableReceiver | RTCBits.EnableReceiverDMA | RTCBits.DMAModuleIE | RTCBits.DeviceClear_SelectMaintenance | RTCBits.DTR`
   - **Active Operation (0x3DC)**: `RTCBits.EnableReceiver | RTCBits.EnableReceiverDMA | RTCBits.DMAModuleIE | RTCBits.DTR | RTCBits.ModemStatusChangeIE | RTCBits.BlockEndIE | RTCBits.FrameEndIE`
   - **Expected Response**: Hardware immediately clears `DeviceClear_SelectMaintenance` bit, activates interrupt enables

**2. WPCR (IOX+1) - Parameter Control Register**
   - **X.21 Mode (0x67)**: 8-bit characters, strip sync enabled, no parity
   - **BCP Mode (0x47)**: 8-bit characters, strip sync enabled, Basic Control Procedure
   - **Expected Response**: Communication protocol reconfigured, sync detection updated

**3. WSAR (IOX+3) - Sync/Address Register**
   - **X.21 Sync (0x16)**: Standard X.21 sync character pattern
   - **Initialization (0xFF)**: All-ones pattern for hardware setup phase
   - **Expected Response**: Hardware sync detection pattern configured

**4. WCHL (IOX+4) - Character Length**
   - **Always (0x00)**: 8-bit character mode configuration
   - **Expected Response**: UART character assembly configured for 8-bit data

**5. WTCR (IOX+7) - Transmitter Control**
   - **Sync Mode (0x01)**: TSOM (Transmit Start of Message) = 1, send sync, start transmitter
   - **Data Mode (0x00)**: TSOM = 0, clear start of message, continue with data
   - **Expected Response**: Transmitter state machine enters requested mode

**6. WTTC (IOX+13) - Transmitter Transfer Control**
   - **Enable (0x40)**: `TTCBits.RequestToSend` = 1 (RQTS signal to DCE)
   - **With additional interrupt enables**: Various `TTCBits` flags OR'd with base pattern
   - **Expected Response**: Transmitter interrupt enables configured, RQTS signal activated

**Hardware Response Patterns Expected by SINTRAN:**

**After WRTC Write:**
- `RTCBits.DeviceClear_SelectMaintenance` bit should self-clear immediately
- Interrupt enable bits (`RTCBits.DataAvailableIE`, `RTCBits.DMAModuleIE`, `RTCBits.BlockEndIE`, `RTCBits.FrameEndIE`) should become active
- Hardware should be ready for DMA operations

**After WPCR Write:**
- Communication protocol parameters should be reconfigured
- Sync detection patterns should change based on strip sync setting
- Character assembly should follow new bit length and parity parameters

**After WSAR Write:**
- Hardware should start looking for new sync character pattern
- Frame synchronization logic should reset to detect new sync pattern

**After WTCR Write:**
- Transmitter should enter requested state (sync transmission or data mode)
- TSOM (Transmit Start of Message) flag should be updated accordingly

#### 7. Critical Initialization Dependencies

**Emulator Implementation Requirements:**

**1. Device Clear Processing:**
   ```csharp
   if ((value & RTCBits.DeviceClear_SelectMaintenance) != 0)
   {
       // Reset all interrupt enables immediately
       currentWRTC &= ~(RTCBits.DataAvailableIE | RTCBits.StatusAvailableIE |
                       RTCBits.DMAModuleIE | RTCBits.ModemStatusChangeIE |
                       RTCBits.BlockEndIE | RTCBits.FrameEndIE | RTCBits.ListEndIE);
       // Clear DeviceClear bit (self-clearing)
       currentWRTC &= ~RTCBits.DeviceClear_SelectMaintenance;
       // Reset communication controller state
       ResetCommunicationController();
   }
   ```

**2. Parameter Control Response:**
   ```csharp
   public void WriteWPCR(byte value)
   {
       // Decode communication parameters from SINTRAN usage patterns
       bool stripSync = (value & 0x40) != 0;           // Bit 6
       bool parityEnable = (value & 0x10) != 0;        // Bit 4
       int characterLength = 8;                        // Always 8-bit in SINTRAN

       // Reconfigure communication protocol based on detected mode
       if (value == 0x67) // X.21 mode
           ConfigureCommunicationProtocol(stripSync: true, parityEnable: false, characterLength: 8);
       else if (value == 0x47) // BCP mode
           ConfigureCommunicationProtocol(stripSync: true, parityEnable: false, characterLength: 8);
   }
   ```

**3. Expected Signal Behavior in RRTS Register:**
   - **`RTSBits.SignalDetector`**: Can be any value - SINTRAN completely ignores it
   - **`RTSBits.DataSetReady`**: Can be any value - SINTRAN completely ignores it
   - **`RTSBits.RingIndicator`**: Can be any value - SINTRAN completely ignores it
   - **`RTSBits.ReceiverActive`**: Should reflect actual HDLC frame reception state
   - **`RTSBits.SyncFlagReceived`**: Should indicate sync pattern detection events
   - **`RTSBits.DataAvailable`**: **CRITICAL** - Must be 1 for packet processing
   - **`RTSBits.ListEmpty`**: **CRITICAL** - When 1, causes fatal receiver shutdown

### 8. Modem Control and Status Analysis

**Complete Analysis of SINTRAN WTTC ModemStatusChangeIE and RTTS ReadyForSending Usage**

#### WTTC Register Modem Status Interrupt Enable Usage

**From SINTRAN Source Code Analysis:**

**WTTC ModemStatusChangeIE (bit 7) = 0x80 Pattern:**
```assembly
Line 103723: 1134+CMODI; T:=HDEV+WTTC; *EXR ST    % HDLC Transmit Control
Line 111032: A:=134;  T:=X2DHD+XWTTC;  *EXR ST    % X.21 Transmit Control
```

**Decoded WTTC Values:**
- **1134 octal = 0x25C (604 decimal)**:
  - Bit 2: `TTCBits.TransmitterEnabled` = 1 - Transmitter enabled
  - Bit 3: `TTCBits.EnableTransmitterDMA` = 1 - DMA enabled
  - Bit 4: `TTCBits.DMAModuleIE` = 1 - DMA interrupts enabled
  - Bit 6: `TTCBits.RequestToSend` = 1 - RQTS signal to DCE
  - Bit 7: `TTCBits.ModemStatusChangeIE` = 1 - **MODEM STATUS INTERRUPTS ENABLED**
  - Bit 8: `TTCBits.BlockEndIE` = 1 - Block completion interrupts
  - Bit 9: `TTCBits.FrameEndIE` = 1 - Frame completion interrupts

- **134 octal = 0x5C (92 decimal)** (X.21 mode):
  - Bit 2: `TTCBits.TransmitterEnabled` = 1 - Transmitter enabled
  - Bit 3: `TTCBits.EnableTransmitterDMA` = 1 - DMA enabled
  - Bit 4: `TTCBits.DMAModuleIE` = 1 - DMA interrupts enabled
  - Bit 6: `TTCBits.RequestToSend` = 1 - RQTS signal to DCE
  - **Note**: ModemStatusChangeIE (bit 7) = 0 in X.21 mode

#### RTTS ReadyForSending (bit 6) Usage Analysis

**Critical Discovery from SINTRAN Source Code:**

**Half-Duplex Mode Ready-For-Sending Check:**
```assembly
Line 103440-103247: % Half-duplex operation
IF HCTRL BIT HDHDX THEN                      % If half-duplex enabled
   HDMSCE; T:=HDEV+13; *EXR ST               % Write ModemStatusChangeIE to WTTC
   T-1; *EXR ST                              % Read RTTS register
   IF A BIT HDRFS THEN CALL ID12 FI          % If ReadyForSending=1, wait for interrupt
FI
```

**Symbol Definitions from Source:**
```assembly
SYMBOL HDHDX=5,HDRFS=6,HDMSCE=200,BROCK=10,BLOVF=13
```

**Decoded Constants:**
- **HDHDX = 5** → Bit 5 in control register (Half-Duplex mode enable)
- **HDRFS = 6** → Bit 6 = `TTSBits.ReadyForSending` in RTTS register
- **HDMSCE = 200 octal = 0x80** → `TTCBits.ModemStatusChangeIE` write value

#### Complete Modem Control Flow Analysis

**1. Full-Duplex Mode (Normal Operation):**
- SINTRAN writes WTTC with `ModemStatusChangeIE` = 1 (enables modem status interrupts)
- **ReadyForSending status is IGNORED** - no polling or waiting
- Transmission proceeds immediately regardless of DCE Ready-For-Sending state
- **Assumption**: DCE is always ready in full-duplex point-to-point links

**2. Half-Duplex Mode (Special Handling):**
- SINTRAN enables `ModemStatusChangeIE` by writing HDMSCE (0x80) to WTTC
- **ReadyForSending is ACTIVELY MONITORED** after RQTS assertion
- If `TTSBits.ReadyForSending` = 1, SINTRAN waits for DCE response via `CALL ID12`
- This implements proper half-duplex handshake: Request → Wait → Ready → Transmit

**3. X.21 Mode (Protocol-Specific):**
- SINTRAN writes WTTC=0x5C (134 octal) **without** `ModemStatusChangeIE`
- **ReadyForSending is IGNORED** in X.21 mode
- X.21 protocol handles flow control at higher levels

#### Modem Status Interrupt Processing

**When ModemStatusChangeIE is enabled, SINTRAN expects:**
1. **Level 12 interrupt** when DCE status signals change
2. **ReadyForSending bit changes** trigger interrupt
3. **Half-duplex coordination** via RQTS → RFS handshake

**Emulator Implementation Requirements:**

```csharp
public void WriteWTTC(ushort value)
{
    currentWTTC = value;

    // Check if ModemStatusChangeIE is enabled
    bool modemStatusIEEnabled = (value & TTCBits.ModemStatusChangeIE) != 0;

    if (modemStatusIEEnabled)
    {
        // Enable modem status change interrupt monitoring
        EnableModemStatusInterrupts();

        // Check if half-duplex mode (from control register)
        bool halfDuplexMode = (controlRegister & 0x20) != 0; // HDHDX bit 5

        if (halfDuplexMode)
        {
            // In half-duplex, monitor ReadyForSending changes
            MonitorReadyForSendingChanges();
        }
    }

    // Handle RequestToSend signal
    if ((value & TTCBits.RequestToSend) != 0)
    {
        SetRequestToSendSignal(true);

        // In half-duplex, DCE should respond with ReadyForSending
        if (halfDuplexMode)
        {
            // Simulate DCE response after brief delay
            ScheduleReadyForSendingResponse();
        }
    }
}

private void OnReadyForSendingChange(bool ready)
{
    // Update RTTS register
    if (ready)
        currentRTTS |= TTSBits.ReadyForSending;
    else
        currentRTTS &= ~TTSBits.ReadyForSending;

    // Trigger interrupt if ModemStatusChangeIE enabled
    if ((currentWTTC & TTCBits.ModemStatusChangeIE) != 0)
    {
        TriggerInterrupt(12); // Level 12 transmitter interrupt
    }
}
```

#### Key Findings Summary

**1. ModemStatusChangeIE Usage:**
- **Enabled in full-duplex mode** (1134 octal pattern) for general modem monitoring
- **Disabled in X.21 mode** (134 octal pattern) - protocol handles flow control
- **Critical in half-duplex mode** - enables RQTS/RFS handshake coordination

**2. ReadyForSending Monitoring:**
- **IGNORED in full-duplex mode** - immediate transmission assumed
- **ACTIVELY MONITORED in half-duplex mode** - proper DCE coordination
- **IGNORED in X.21 mode** - higher-level protocol flow control

**3. Emulator Requirements:**
- Must implement **conditional interrupt generation** based on WTTC ModemStatusChangeIE
- Must simulate **DCE ReadyForSending response** to RQTS in half-duplex mode
- Must **ignore modem status** in full-duplex and X.21 modes per SINTRAN behavior

**Key Discovery**: SINTRAN HDLC driver operates in a **"cable-blind"** mode - it assumes physical connectivity and relies entirely on protocol-level error detection and timeout mechanisms for failure detection.

### RRTS Register Complete Bit Analysis

Based on extensive SINTRAN source code analysis (lines 104436-104675):

```
RRTS Register Bit Map (IOX+10):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 15  │ 14  │ 13  │ 12  │ 11  │ 10  │  9  │  8  │  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ OR  │X21S │X21D │Rsvd │EMTY │ LE  │ FE  │ BE  │ RI  │ DSR │ SD  │ DMR │ SFR │ RXA │RXSA │ RXD │
│0x8000│0x4000│0x2000│0x1000│0x0800│0x0400│0x0200│0x0100│0x0080│0x0040│0x0020│0x0010│0x0008│0x0004│0x0002│0x0001│
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
```

### DEFINITIVE BIT USAGE ANALYSIS

**Based on complete SINTRAN source code analysis:**

| Bit | Name | SINTRAN Test | Effect if Wrong | Criticality | **CONFIRMED USAGE** |
|-----|------|--------------|-----------------|-------------|---------------------|
| **0** | DataAvailable | `IF A NBIT 0` | **DROP PACKET** | **CRITICAL** | **DIRECTLY TESTED** |
| **1** | StatusAvailable | Not tested | None | None | **INFORMATIONAL** |
| **2** | ReceiverActive | Not individually tested | None | Low | **STATUS ONLY** |
| **3** | SyncFlagReceived | Part of HX21S pattern | Operational | Medium | **DUAL PURPOSE** |
| **4** | DMAModuleRequest | Auto-clear | No interrupt | **CRITICAL** | **INTERRUPT TRIGGER** |
| **5** | SignalDetector | **NEVER TESTED** | **NONE** | **NONE** | **IGNORED BY SINTRAN** |
| **6** | DataSetReady | **NEVER TESTED** | **NONE** | **NONE** | **IGNORED BY SINTRAN** |
| **7** | RingIndicator | Not tested | None | None | **INFORMATIONAL** |
| **8** | BlockEnd | **NEVER TESTED** | **NONE** | **NONE** | **IGNORED BY SINTRAN** |
| **9** | FrameEnd | **NEVER TESTED** | **NONE** | **NONE** | **IGNORED BY SINTRAN** |
| **10** | ListEnd | **NEVER TESTED** | **NONE** | **NONE** | **IGNORED BY SINTRAN** |
| **11** | ListEmpty | `IF HASTAT/\"EMTY\" >< 0` | **SHUTDOWN RECEIVER** | **FATAL** | **DIRECTLY TESTED** |
| **12** | Reserved | Not tested | None | None | **UNUSED** |
| **13** | X21D | `IF A/\\ HX21M >< 0` | **ERROR HANDLING** | **CRITICAL** | **DIRECTLY TESTED** |
| **14** | X21S | `IF A/\\ HX21M >< 0` + `IF A BIT HX21S` | **ERROR HANDLING** | **CRITICAL** | **DUAL USAGE** |
| **15** | ReceiverOverrun | Not tested | None | Low | **STATUS ONLY** |

### Critical Hardware Constants

From SINTRAN symbol definitions:
```assembly
HX21M = 060000₈ = 0x6000 = Bits 13-14 (X.21 Error Mask)
EMTY  = 004000₈ = 0x0800 = Bit 11 (List Empty)
HX21S = 000016₈ = 0x000E = Bits 1,2,3 (Also used as bit 14 test)
XBLDN = 000010₈ = 0x0008 = Bit 3 (Block Done - DMA descriptor flag)
BLDON = 000010₈ = 0x0008 = Bit 3 (Same as XBLDN)
```

## PART 2: SINTRAN PROCESSING FLOW ANALYSIS

### HIINT: Hardware Interrupt Handler (Lines 104436-104546)

```assembly
HIINT: T:=HDEV+RRTS; *EXR ST                     % READ RECEIVER STATUS
       A=:HASTAT                                 % STORE STATUS
       IF T:=ACTSW = 0 THEN MIN T9; P+0; GO OUT1 FI % ACTIVITY CHECK
       IF A/\ HX21M >< 0 THEN                    % X.21 ERROR CHECK
          % X.21 error handling code...
       FI
       IF HASTAT/\"EMTY" >< 0 THEN               % LIST EMPTY CHECK (FATAL)
          0=:ACTSW                               % SHUTDOWN RECEIVER
          MIN STPCNT                             % COUNT STOP EVENTS
          % Copy DMA list for diagnostics...
       FI

MORE:  A:=LIINT.LKEY=:D                          % GET LKEY FROM DMA DESCRIPTOR
       IF A NBIT XBLDN THEN                     % TEST BLOCK DONE (CRITICAL)
          IF A = "ERB" THEN GO FAR ZSTARC FI     % ENABLE RECEIVER
          GO FAR OUT1                            % EXIT - NO COMPLETE BLOCKS
       FI
       GO HNOTRA                                 % PROCESS BLOCK CONTENT
```

### HNOTRA: Buffer Content Processor (Lines 104611-104675)

```assembly
HNOTRA: X:=LIINT; A:=2000; X+A; X.DLSTS          % ACCESS DMA DESCRIPTOR
        A:=LIINT.LBYTC+DISP1=:T                  % GET MESSAGE SIZE
        CALL XMPAT                               % GET DCB FROM DCB LIST
        X=:L; A:=LIINT.LKEY=:D; 0=:X.LKEY; 0=:X.LMEM2; X:=L
        IF A /\ "LMASK" = 3 THEN                 % RCOST VALIDATION
              A:=0; CALL SCRET; CALL SADTS       % SUCCESS PATH
        ELSE
              IF A BIT HX21S THEN EX21 ELSE EINP FI % ERROR PATH
              CALL SCRET; A:=D; CALL SADTS
              A\/DSTAT=:DSTAT; HDERC+1=:HDERC
        FI
        X-BHEAD; CALL OCHAIN                     % DELIVER TO USER
        LIINT+4=:LIINT                           % ADVANCE TO NEXT DESCRIPTOR
        A+2000; A.LKEY                           % CACHE MANAGEMENT
        IF LIINT.LKEY=NLP THEN LISTP=:LIINT FI   % END OF LIST CHECK
        GO MORE                                  % PROCESS MORE BLOCKS
```

## PART 3: CRITICAL ANALYSIS - XBLDN vs RRTS BLOCKS BITS

### The Fundamental Confusion: Two Different Block Processing Systems

**CRITICAL INSIGHT**: SINTRAN uses TWO completely different "block done" systems that operate independently:

1. **RRTS Hardware Status Bits** (BlockEnd/FrameEnd) - **COMPLETELY IGNORED BY SINTRAN**
2. **DMA Descriptor XBLDN Flag** - **ESSENTIAL FOR SINTRAN FLOW CONTROL**

### Deep Dive: Why SINTRAN Ignores RRTS BlockEnd/FrameEnd

**The Architecture:**
- **Hardware Level**: RRTS register reports real-time hardware events
- **Software Level**: DMA descriptors provide structured processing control

**From SINTRAN source code analysis:**
```assembly
% SINTRAN NEVER tests these RRTS bits:
% - Bit 8 (BlockEnd)   - 0x0100 - NEVER REFERENCED
% - Bit 9 (FrameEnd)   - 0x0200 - NEVER REFERENCED
% - Bit 10 (ListEnd)   - 0x0400 - NEVER REFERENCED

% INSTEAD, SINTRAN uses DMA descriptor flags:
104537    IF A NBIT XBLDN THEN         % Test XBLDN in LKEY (DMA descriptor)
104541          GO FAR OUT1             % Exit if block not complete
104546    FI
```

### XBLDN: The Real Block Processing Control

**XBLDN = 000010₈ = 0x0008 = Bit 3 in LKEY field of DMA descriptor**

#### DMA Descriptor Structure (4 words):
```assembly
Word 0: LKEY  = Block control + COM5025 bits + Status flags
Word 1: LBYTC = Byte count
Word 2: LMEM1 = Memory bank
Word 3: LMEM2 = Buffer address
```

#### LKEY Field Structure (Word 0):
```
LKEY Field (16 bits):
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│ 15  │ 14  │ 13  │ 12  │ 11  │ 10  │  9  │  8  │  7  │  6  │  5  │  4  │  3  │  2  │  1  │  0  │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│                Block Control Bits (15-8)                 │           RCOST + Control (7-0)      │
│  15-13: Block Status (011=RcvdOK, 010=Transmit)         │  Bit 3: XBLDN (Block Done) **CRITICAL**│
│  12-10: Block Type                                        │  Bit 2: TEOM (Transmit End Message) │
│  9-8:   Reserved                                          │  Bit 1: TSOM (Transmit Start Msg)   │
│                                                           │  Bit 0: RSOM/REOM (Receive flags)   │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
```

### Complete Flow Impact Analysis: How Each Bit Affects Reception

**SINTRAN Reception Flow and Bit Impact:**

#### **PHASE 1: HIINT Hardware Validation (RRTS Register Processing)**

**Processing Order and Bit Impact:**

```assembly
HIINT: T:=HDEV+RRTS; *EXR ST     % READ RRTS (only hardware register read)
       A=:HASTAT                 % STORE in HASTAT for testing

% 1. ACTIVITY CHECK (not bit-related)
IF T:=ACTSW = 0 THEN MIN T9; P+0; GO OUT1 FI

% 2. X.21 ERROR CHECK - TESTS BITS 13-14
IF A/\ HX21M >< 0 THEN           % HX21M = 0x6000 = bits 13-14
    % BITS 13-14 IMPACT: X21D=1 OR X21S=1 → Protocol error handling
    % ERROR PATH: Log error, handle X.21 state changes
    IF A BIT HX21S THEN          % Also test bit 14 individually
        HASTAT BONE BLDON=:HASTAT
        LIINT.LKEY BONE XBLDN=:X.LKEY
    FI
FI

% 3. LIST EMPTY CHECK - TESTS BIT 11 (FATAL)
IF HASTAT/\"EMTY" >< 0 THEN      % EMTY = 0x0800 = bit 11
    % BIT 11 IMPACT: EMTY=1 → RECEIVER SHUTDOWN (fatal)
    0=:ACTSW                     % SHUTDOWN RECEIVER
    MIN STPCNT                   % Count stop events
    % Copy DMA list to BUFF3 for diagnostics
    % NO FURTHER PROCESSING - RECEIVER DEAD
FI
```

**KEY INSIGHT**: Only bits 0, 11, 13, and 14 are tested from RRTS. All other bits are ignored.

#### **PHASE 2: Block Processing Loop (DMA Descriptor Validation)**

```assembly
MORE: % Main processing loop
% 4. DMA DESCRIPTOR ACCESS
A:=LIINT.LKEY=:D                 % Get LKEY from DMA descriptor (NOT RRTS!)

% 5. BLOCK COMPLETION CHECK - TESTS BIT 3 IN LKEY (NOT RRTS)
IF A NBIT XBLDN THEN             % XBLDN = 0x0008 = bit 3 in LKEY
    % XBLDN=0 IMPACT: Block incomplete → Exit processing
    IF A = "ERB" THEN GO FAR ZSTARC FI  % Enable receiver
    GO FAR OUT1                  % EXIT - no complete blocks
FI

% 6. CONTINUE TO CONTENT PROCESSING
GO HNOTRA
```

**KEY INSIGHT**: XBLDN is tested from DMA descriptor LKEY field, NOT from RRTS register.

#### **PHASE 3: Content Validation (HNOTRA Buffer Processing)**

```assembly
HNOTRA:
% 7. RCOST VALIDATION - TESTS LOW 8 BITS OF LKEY
IF A /\ "LMASK" = 3 THEN         % LMASK = 0x6377, test low 8 bits
    % RCOST=3 IMPACT: Valid frame format → Success path
    A:=0; CALL SCRET; CALL SADTS % Set success status
ELSE
    % RCOST!=3 IMPACT: Invalid format → Error path
    IF A BIT HX21S THEN EX21 ELSE EINP FI
    CALL SCRET; A:=D; CALL SADTS
    A\/DSTAT=:DSTAT; HDERC+1=:HDERC  % Increment error counter
FI

% 8. PACKET DELIVERY
X-BHEAD; CALL OCHAIN             % DELIVER TO USER APPLICATION
```

### Complete Bit Impact Matrix

**RRTS Register Bit Impact Analysis:**

| RRTS Bit | Value | SINTRAN Test | Flow Impact | Criticality |
|-----------|-------|--------------|-------------|-------------|
| **0** DataAvailable | **Must=1** | `IF A NBIT 0` in combined test | **0=Drop packet** | **CRITICAL** |
| **1** StatusAvailable | Any | Not tested | None | None |
| **2** ReceiverActive | Any | Not individually tested | None | Low |
| **3** SyncFlagReceived | Any | Part of HX21S pattern | None individually | Low |
| **4** DMAModuleRequest | 1→0 | Not tested (auto-clears) | **Triggers interrupt** | **CRITICAL** |
| **5** SignalDetector | Any | **NEVER TESTED** | **NONE** | **NONE** |
| **6** DataSetReady | Any | **NEVER TESTED** | **NONE** | **NONE** |
| **7** RingIndicator | Any | Not tested | None | None |
| **8** BlockEnd | Any | **NEVER TESTED** | **NONE** | **NONE** |
| **9** FrameEnd | Any | **NEVER TESTED** | **NONE** | **NONE** |
| **10** ListEnd | Any | **NEVER TESTED** | **NONE** | **NONE** |
| **11** ListEmpty | **Must=0** | `IF HASTAT/\"EMTY" >< 0` | **1=Shutdown receiver** | **FATAL** |
| **12** Reserved | Any | Not tested | None | None |
| **13** X21D | **Must=0** | `IF A/\\ HX21M >< 0` | **1=Protocol error** | **CRITICAL** |
| **14** X21S | **Must=0** | `IF A/\\ HX21M >< 0` + `IF A BIT HX21S` | **1=Protocol error** | **CRITICAL** |
| **15** ReceiverOverrun | Any | Not tested | None | Low |

**DMA Descriptor LKEY Bit Impact Analysis:**

| LKEY Bit | Value | SINTRAN Test | Flow Impact | Criticality |
|-----------|-------|--------------|-------------|-------------|
| **0** RSOM | **Must=1** | Part of `IF A /\\ "LMASK" = 3` | **0=RCOST validation fail** | **CRITICAL** |
| **1** REOM | **Must=1** | Part of `IF A /\\ "LMASK" = 3` | **0=RCOST validation fail** | **CRITICAL** |
| **2** TEOM | Any | Not tested in receive | None | None |
| **3** XBLDN | **Must=1** | `IF A NBIT XBLDN` | **0=Exit block processing** | **CRITICAL** |
| **4-7** RCOST | Must=0 | Part of `IF A /\\ "LMASK" = 3` | **!=0 affects validation** | Medium |
| **8-15** Block Control | Varies | Not individually tested | Control DMA operation | Medium |

### Critical Flow Decision Points

#### **Decision Point 1: RRTS Combined Test (Line 104xxx)**
```assembly
IF A NBIT 0 OR A/\60000><0 THEN GO OUT1 FI
```
**Translation**: `IF (DataAvailable==0) OR (X21Errors!=0) THEN drop_packet`
- **Bit 0=0**: Packet dropped immediately
- **Bits 13-14!=0**: Protocol error, packet dropped

#### **Decision Point 2: ListEmpty Fatal Check (Line 104473)**
```assembly
IF HASTAT/\"EMTY" >< 0 THEN 0=:ACTSW FI
```
**Translation**: `IF (ListEmpty==1) THEN shutdown_receiver`
- **Bit 11=1**: **Receiver dies, no further packets processed**

#### **Decision Point 3: XBLDN Block Processing (Line 104537)**
```assembly
IF A NBIT XBLDN THEN GO FAR OUT1 FI
```
**Translation**: `IF (XBLDN_in_LKEY==0) THEN exit_processing`
- **LKEY bit 3=0**: Block incomplete, skip processing

#### **Decision Point 4: RCOST Validation (Line 104631)**
```assembly
IF A /\ "LMASK" = 3 THEN success_path ELSE error_path FI
```
**Translation**: `IF ((LKEY & 0x6377) == 3) THEN deliver ELSE error`
- **LKEY low 8 bits != 3**: Protocol format error

### Two Parallel "Block Done" Systems

**System 1: RRTS Hardware Status (IGNORED by SINTRAN flow control)**
```csharp
// Hardware sets these, but SINTRAN never tests them for flow control:
rrts |= 0x0100;  // BlockEnd (bit 8) - IGNORED
rrts |= 0x0200;  // FrameEnd (bit 9) - IGNORED
rrts |= 0x0400;  // ListEnd (bit 10) - IGNORED

// These are purely for hardware diagnostics and status reporting
```

**System 2: LKEY Software Control (ESSENTIAL for SINTRAN flow control)**
```csharp
// SINTRAN tests this for actual flow control:
lkey |= 0x0008;  // XBLDN (bit 3) - ESSENTIAL
lkey |= 0x0003;  // RSOM+REOM - ESSENTIAL

// This is what actually controls packet processing
```

### Hardware Setup vs Software Processing

**During Reception:**
1. **Hardware** sets both RRTS BlockEnd/FrameEnd AND LKEY XBLDN
2. **Hardware** triggers level 13 interrupt via RRTS bit 4
3. **SINTRAN** reads RRTS for status validation (bits 0,11,13,14)
4. **SINTRAN** reads LKEY for block processing control (bit 3)
5. **SINTRAN** ignores RRTS BlockEnd/FrameEnd completely
6. **SINTRAN** uses LKEY XBLDN for actual flow control

### Data Forwarding to Higher-Level Processing (XMSG/Application Layer)

**What Gets Forwarded to XMSG and User Applications:**

Based on SINTRAN source code analysis, here's exactly what data is passed up the protocol stack:

#### **OCHAIN: The User Delivery Mechanism (Line 104656)**

```assembly
104656    X-BHEAD; CALL OCHAIN              % DELIVER TO USER APPLICATION
```

**OCHAIN delivers the complete DCB (Device Control Block) containing:**

1. **User Data**: The actual received packet content
2. **Status Information**: Set by SCRET and SADTS calls
3. **Message Metadata**: DCB headers and control information

#### **Status Data Forwarded via SADTS (Store Hardware Status)**

```assembly
104635    A:=0; CALL SCRET; CALL SADTS      % SUCCESS PATH
104647    A:=D; CALL SADTS                  % ERROR PATH (D contains LKEY)
```

**SADTS Implementation (Line 103557):**
```assembly
SADTS: T:=MASTB; * ADSTA@3 STATX     % Store A-reg in DCB.ADDSTA field
       EXIT
```

**What SADTS Forwards:**
- **Success Case**: A=0 (success status)
- **Error Case**: A=LKEY value (includes XBLDN and RCOST bits)

**Key Insight**: **XBLDN and RCOST bits ARE forwarded to XMSG via SADTS in error cases!**

#### **Status Data Forwarded via SCRET (Store Return Code)**

```assembly
104635    CALL SCRET                        % Store return code
104646    CALL SCRET                        % Store error return code
```

**SCRET Implementation (Line 103562):**
```assembly
SCRET: T:=MASTB; * CRET@3 STATX      % Store A-reg in DCB.RET-STATUS field
       EXIT
```

**What SCRET Forwards:**
- **Success Case**: Return code 0
- **Error Case**: Various error codes (EINP, EX21, etc.)

#### **RRTS Status Forwarded to Applications**

**SINTRAN maintains HASTAT (the RRTS value) and forwards it via SADTS:**

```assembly
104647    A:=D; CALL SADTS; A\/DSTAT=:DSTAT   % Store LKEY in ADDSTA
% Note: HASTAT (RRTS) is also stored in various contexts
```

**From SPSTA routine (Line 105361):**
```assembly
105361    A:=HASTAT; * LHAST@3 STATX        % Store RRTS in LHAST field
```

#### **Complete DCB Structure Forwarded to XMSG**

**When OCHAIN delivers to XMSG/applications, the DCB contains:**

```csharp
/// <summary>
/// DCB fields accessible to XMSG and user applications
/// </summary>
public struct DCB_Fields_Forwarded_To_XMSG
{
    // Message data
    public byte[] UserData;          // The actual packet content

    // Status fields set by HDLC driver
    public ushort CRET_STATUS;       // Set by SCRET - return code
    public ushort ADDSTA;            // Set by SADTS - hardware status
    public ushort LHAST;             // Last hardware status (RRTS value)
    public ushort ORERR;             // OR'ed error status
    public ushort ERRNO;             // Error counter

    // Message metadata
    public ushort MESSID;            // Message ID
    public ushort BYTEC;             // Byte count
    public ushort BMAXR;             // Buffer size
    public ushort BHEAD;             // Header size
}
```

#### **Critical Discovery: XBLDN and RRTS Both Accessible to XMSG**

**In Error Cases:**
```assembly
104647    A:=D; CALL SADTS                  % D contains LKEY (includes XBLDN)
```

**XMSG Applications Can Access:**
- **LKEY value** (including XBLDN bit 3) via DCB.ADDSTA field
- **RRTS value** (all 16 bits) via DCB.LHAST field
- **Combined error status** via DCB.ORERR field

**In Success Cases:**
```assembly
104635    A:=0; CALL SCRET; CALL SADTS      % Success indicators
```

**XMSG Applications Get:**
- **Success status** (0) in both CRET_STATUS and ADDSTA
- **RRTS value** still available in LHAST field
- **Packet data** in user data area

#### **XMSG Higher-Level Processing Capabilities**

**From SINTRAN source code analysis (OCHAIN routine):**

```assembly
021725   OCHAIN: T:=MASTB; * BBID@3 LDATX
021727           IF A < 0 THEN                      % DOES DCB BELONG TO XMSG
021730              * XCHAI@3 LDATX                 % YES - XMSG PROCESSING
021731              IF A >< 0 THEN
021732                 * ION; COPY SA DP            % CALL XMSG HANDLER
021734              FI
021735           FI
```

**XMSG Can:**
1. **Access all DCB fields** including HDLC status information
2. **Implement higher-level protocols** (X.25, LAPB, etc.)
3. **Perform additional validation** beyond HDLC level
4. **Handle protocol state management**
5. **Manage session-level flow control**

#### **Flow of Status Information**

```
Hardware
    ↓
HIINT (reads RRTS, processes LKEY)
    ↓
HNOTRA (validates RCOST, calls SADTS/SCRET)
    ↓
OCHAIN (delivers DCB to XMSG/application)
    ↓
XMSG Layer (can access RRTS, LKEY, and all status)
    ↓
User Application (receives processed data + status)
```

**Summary**: Both XBLDN (from LKEY) and RRTS bits ARE forwarded to higher-level processing through the DCB structure, giving XMSG and applications access to complete hardware status information for their own processing decisions.

## PART 4: VALIDATION REQUIREMENTS AND SUCCESS PATTERNS

### Required RRTS Pattern for Success

```csharp
/// <summary>
/// Optimal RRTS pattern for successful reception
/// Based on complete SINTRAN source code analysis
/// </summary>
public ushort GetSuccessfulRRTS()
{
    ushort rrts = 0x036D;  // Base pattern with operational bits

    // CRITICAL BITS (must be correct):
    rrts |= 0x0001;        // Bit 0: DataAvailable = 1 (MUST SET)
    rrts &= ~0x0800;       // Bit 11: ListEmpty = 0 (MUST CLEAR)
    rrts &= ~0x2000;       // Bit 13: X21D = 0 (MUST CLEAR)
    rrts &= ~0x4000;       // Bit 14: X21S = 0 (MUST CLEAR)

    // RECOMMENDED BITS (operational status):
    rrts |= 0x0004;        // Bit 2: ReceiverActive = 1
    rrts |= 0x0008;        // Bit 3: SyncFlagReceived = 1
    rrts |= 0x0020;        // Bit 5: SignalDetector = 1 (ignored but good status)
    rrts |= 0x0040;        // Bit 6: DataSetReady = 1 (ignored but good status)
    rrts |= 0x0100;        // Bit 8: BlockEnd = 1 (ignored but matches hardware)
    rrts |= 0x0200;        // Bit 9: FrameEnd = 1 (ignored but matches hardware)

    // NOTE: Bit 4 (DMAModuleRequest) triggers interrupt then auto-clears
    // Hardware clears bits 8-15 after read

    return rrts;
}
```

### Required RCOST Pattern for Success

```csharp
/// <summary>
/// RCOST (low 8-bit) pattern for successful frame reception
/// Must pass: (LKEY & 0x6377) == 3
/// </summary>
public byte GetSuccessfulRCOST()
{
    byte rcost = 0x03;  // RSOM(bit 0) + REOM(bit 1) = perfect pattern
    return rcost;
}
```

### Required DMA Descriptor LKEY Pattern

```csharp
/// <summary>
/// Complete LKEY pattern for successful block reception
/// </summary>
public ushort GetSuccessfulLKEY(ushort dmaControlBits)
{
    ushort lkey = (ushort)((dmaControlBits << 8) | 0x0B);  // Control + RCOST
    lkey |= 0x0008;  // Bit 3: XBLDN (Block Done) - CRITICAL for SINTRAN
    return lkey;
}
```

## PART 5: CRITICAL SUCCESS/FAILURE PATHS

### Processing Order and Priority

**SINTRAN processes in this exact order:**

1. **Activity Check**: `IF T:=ACTSW = 0` → Ignore if receiver inactive
2. **X.21 Error Check**: `IF A/\\ HX21M >< 0` → Handle protocol errors
3. **⚠️ FATAL: List Empty Check**: `IF HASTAT/\"EMTY\" >< 0` → **SHUTDOWN RECEIVER**
4. **Block Done Check**: `IF A NBIT XBLDN` → Exit if block incomplete
5. **RCOST Validation**: `IF A /\\ "LMASK" = 3` → Validate packet format
6. **Success**: `CALL OCHAIN` → Deliver to user application

### Critical Failure Points

#### 1. Immediate Packet Drop
```assembly
IF A NBIT 0 OR A/\60000><0 THEN GO OUT1 FI
```
- **DataAvailable = 0**: Packet dropped immediately
- **X.21 Errors = 1**: Protocol error, packet dropped

#### 2. Receiver Shutdown (FATAL)
```assembly
IF HASTAT/\"EMTY\" >< 0 THEN 0=:ACTSW FI
```
- **ListEmpty = 1**: **Entire receiver shuts down, no further packets processed**

#### 3. Block Processing Exit
```assembly
IF A NBIT XBLDN THEN GO FAR OUT1 FI
```
- **XBLDN = 0**: Block incomplete, exit processing loop

#### 4. RCOST Validation Failure
```assembly
IF A /\\ "LMASK" = 3 THEN ... ELSE ... FI
```
- **RCOST != 3**: Protocol error, increment error counter

## PART 6: IMPLEMENTATION EXAMPLES

### Complete C# Implementation

```csharp
public class HDLCReceiverEmulation
{
    private ushort currentRRTS = 0;
    private DMADescriptor currentDescriptor;

    /// <summary>
    /// Handle successful frame reception with all validation requirements
    /// </summary>
    public void OnFrameReceived(byte[] frameData, bool isLastInList, bool hasMoreBuffers)
    {
        // Step 1: Set up RRTS register for hardware status
        SetupSuccessfulRRTS(isLastInList, hasMoreBuffers);

        // Step 2: Set up DMA descriptor with proper LKEY/RCOST
        SetupDMADescriptor(frameData);

        // Step 3: Trigger level 13 interrupt
        TriggerReceiverInterrupt();
    }

    private void SetupSuccessfulRRTS(bool isLastInList, bool hasMoreBuffers)
    {
        // Base success pattern
        currentRRTS = 0x036D;

        // CRITICAL: These bits MUST be correct for SINTRAN
        currentRRTS |= 0x0001;   // DataAvailable = 1 (tested by SINTRAN)
        currentRRTS &= ~0x2000;  // X21D = 0 (tested by SINTRAN)
        currentRRTS &= ~0x4000;  // X21S = 0 (tested by SINTRAN)

        // FATAL: Never set ListEmpty unless you want receiver shutdown
        if (isLastInList && !hasMoreBuffers)
        {
            // WRONG: currentRRTS |= 0x0800;  // Would shutdown receiver!
            // CORRECT: Set ListEnd (ignored by SINTRAN anyway)
            currentRRTS |= 0x0400;  // ListEnd (informational only)
        }
        currentRRTS &= ~0x0800;  // Ensure ListEmpty = 0

        // Set DMA Module Request (triggers interrupt, then auto-clears)
        currentRRTS |= 0x0010;   // Bit 4: DMAModuleRequest
    }

    private void SetupDMADescriptor(byte[] frameData)
    {
        // CRITICAL: XBLDN must be set for SINTRAN block processing
        ushort lkey = 0x0B;      // RCOST: RSOM + REOM = 3
        lkey |= 0x0008;          // XBLDN (Block Done) - ESSENTIAL for SINTRAN
        lkey |= (0x06 << 8);     // Block control (received OK)

        currentDescriptor.LKEY = lkey;
        currentDescriptor.LBYTC = (ushort)frameData.Length;
        currentDescriptor.LMEM1 = bufferBank;
        currentDescriptor.LMEM2 = bufferAddress;
    }

    /// <summary>
    /// SINTRAN reads RRTS register (IOX+10)
    /// Must implement hardware clearing behavior
    /// </summary>
    public ushort ReadReceiverTransferStatus()
    {
        ushort result = currentRRTS;

        // Hardware behavior: Clear specific bits on read
        currentRRTS &= ~0x0010;  // Clear DMAModuleRequest (bit 4)
        currentRRTS &= 0x00FF;   // Clear DMA status bits (8-15)

        return result;
    }
}
```

### Priority Processing Example

```csharp
/// <summary>
/// Example showing how conflicts are resolved by SINTRAN processing order
/// </summary>
public void ProcessingOrderExample()
{
    // Scenario: RRTS has both success bits AND ListEmpty
    ushort conflictedRRTS = 0x0B01;  // DataAvailable + BlockEnd + FrameEnd + ListEmpty

    // SINTRAN Processing Order:
    // 1. Activity check (assume active)
    // 2. X.21 check: (0x0B01 & 0x6000) == 0 → OK
    // 3. ListEmpty check: (0x0B01 & 0x0800) != 0 → TRIGGERED!
    // 4. Result: RECEIVER SHUTDOWN - Block/Frame bits completely ignored

    Console.WriteLine("ListEmpty overrides everything - receiver shuts down");
    Console.WriteLine("BlockEnd/FrameEnd bits are never reached in processing");
}
```

## PART 7: DEBUGGING AND VALIDATION

### SINTRAN Status Logging

SINTRAN maintains extensive logging for debugging:

```assembly
% Circular logging buffers (Lines 51729-51732):
BUFF0(BUFSIZ)  % First word in received frame
BUFF1(BUFSIZ)  % Device number used
BUFF2(BUFSIZ)  % Device status (HASTAT value - the RRTS we returned)
BUFF3(11)      % List keys when device stopped
```

### Validation Checklist

**For each received packet, verify:**

✅ **RRTS Register**:
- Bit 0 (DataAvailable) = 1
- Bit 11 (ListEmpty) = 0
- Bits 13-14 (X21D/X21S) = 0
- Bit 4 triggers interrupt then auto-clears
- Bits 5,6,8,9,10 can be any value (ignored)

✅ **DMA Descriptor LKEY**:
- Bit 3 (XBLDN) = 1
- Low 8 bits = 0x03 (RSOM + REOM)
- Validation: (LKEY & 0x6377) == 3

✅ **Processing Flow**:
- Hardware interrupt on level 13
- HIINT processes RRTS validation
- HNOTRA processes LKEY validation
- OCHAIN delivers to user application

## PART 8: X.21 MODE ANALYSIS

**CRITICAL DISCOVERY:** SINTRAN supports both **point-to-point HDLC/X.25 mode** and **X.21 circuit-switched mode** with fundamentally different operational characteristics.

### X.21 vs Point-to-Point HDLC/X.25 Mode Differences

#### 1. **Protocol Layer Separation**

**Point-to-Point HDLC/X.25 Mode:**
- Direct HDLC frame transmission over permanent links
- Fixed physical connection (leased lines, direct cables)
- X.25 packet switching over established HDLC links
- No call establishment/teardown procedures

**X.21 Mode:**
- Circuit-switched network interface (like ISDN or telephone network)
- Call establishment and teardown procedures required
- Dynamic connection setup with dialing sequences
- HDLC frames transmitted only after circuit establishment

#### 2. **HDLC Register Usage Differences**

**WTTC Register Patterns (Based on SINTRAN Source Code):**

```csharp
// Point-to-Point HDLC/X.25 Mode (Full-Duplex)
// Line 103723: 1134+CMODI; T:=HDEV+WTTC; *EXR ST
// 1134 octal = 0x025C = 604 decimal
public void WriteWTTC_FullDuplex()
{
    currentWTTC = TTCBits.TransmitBufferEmptyIE |      // Bit 0: Enable TX interrupts
                  TTCBits.DataModuleEnable |           // Bit 2: Enable data transmission
                  TTCBits.TransmitEnable |             // Bit 3: Enable transmitter
                  TTCBits.RequestToSend |              // Bit 4: RTS control
                  TTCBits.TransmitHalfDuplexMode |     // Bit 6: Half-duplex control
                  TTCBits.ModemStatusChangeIE |        // Bit 7: Monitor modem status changes
                  TTCBits.DMATransferEnable |          // Bit 9: Enable DMA transfers
                  TTCBits.DMAModeSelect;               // Bit 10: DMA mode selection
}

// X.21 Mode (Circuit-Switched)
// Line 111032: A:=134; T:=X2DHD+XWTTC; *EXR ST
// 134 octal = 0x005C = 92 decimal
public void WriteWTTC_X21Mode()
{
    currentWTTC = TTCBits.TransmitBufferEmptyIE |      // Bit 0: Enable TX interrupts
                  TTCBits.DataModuleEnable |           // Bit 2: Enable data transmission
                  TTCBits.TransmitEnable |             // Bit 3: Enable transmitter
                  TTCBits.RequestToSend |              // Bit 4: RTS control
                  TTCBits.TransmitHalfDuplexMode |     // Bit 6: Half-duplex operation
                  // NOTE: ModemStatusChangeIE (Bit 7) is DISABLED in X.21 mode
                  TTCBits.DMATransferEnable |          // Bit 9: Enable DMA transfers
                  TTCBits.DMAModeSelect;               // Bit 10: DMA mode selection
}
```

**Key Difference:** X.21 mode **disables** `TTCBits.ModemStatusChangeIE` (bit 7) because X.21 uses a different signaling protocol.

#### 3. **X.21 State Machine and Call Control**

**SINTRAN X.21 Driver implements complete ITU-T X.21 state machine:**

**Connection States (X2DST values from SINTRAN source):**
- **State 0**: Ready/Idle - waiting for outgoing call or incoming indication
- **State 3**: Call request sent, waiting for proceed to select
- **State 7**: Sending selection signals (dialing)
- **State 10**: Selection complete, waiting for DCE ready
- **State 12**: Call progress signals received
- **State 14**: **Data Transfer Phase** - HDLC frames can be transmitted
- **State 20**: Call clearing initiated
- **State 24**: DCE clear confirmation
- **State 25**: DTE clear confirmation

**X.21 Call Establishment Sequence:**
```csharp
// From SINTRAN X.21 driver analysis
public enum X21States
{
    Ready = 0,          // Idle state, ready for calls
    CallRequest = 3,    // Call establishment phase
    SelectSignals = 7,  // Transmitting dialing digits
    SelectComplete = 10, // Waiting for network response
    CallProgress = 12,  // Receiving call progress information
    DataPhase = 14,     // **HDLC DATA TRANSMISSION ACTIVE**
    CallClearing = 20,  // Call teardown initiated
    DCEClear = 24,      // Network clearing
    DTEClear = 25       // Local clearing
}
```

#### 4. **X.21 Special Signal Characters**

**SINTRAN recognizes specific X.21 control characters:**

```csharp
// From SINTRAN symbol definitions and X.21 driver code
public static class X21Signals
{
    public const byte X21BL = 0x07;   // Bell character - incoming call indication
    public const byte X21SY = 0x16;   // SYN character - synchronization
    public const byte X21PL = 0x2B;   // Plus character '+' - end of selection
    public const byte X21NL = 0x85;   // Clear request signal
    // public const byte X21KO = 0x?;   // Call progress signal (value needs analysis)
}
```

**Call Progress Monitoring:**
```assembly
; From SINTRAN X.21 driver - Line 107567
IF X2D00 = "X21BL" OR A = "X21SY" THEN  ; Ignore sync chars during charging
    GO KAST
FI
```

#### 5. **X.21 Circuit vs HDLC Frame Distinction**

**Critical Operational Difference:**

**Point-to-Point Mode:**
- HDLC controller is ALWAYS ready for data transmission
- No call establishment required
- RRTS/RTTS reflect only HDLC frame status
- Modem status changes monitored for line quality

**X.21 Mode:**
- HDLC controller only transmits when `X2DST = 14` (Data Phase)
- Must establish circuit before any HDLC transmission
- RRTS/RTTS reflect both X.21 signaling and HDLC status
- X.21 protocol errors override HDLC processing

#### 6. **Error Handling Differences**

**X.21 Mode Error Processing (HX21M/HX21S bits):**

```assembly
; From HIINT (HDLC Input Interrupt) - Line 104450
IF A/\ HX21M >< 0 THEN                  ; X21-ERROR?
    ; Save X.21 error in DMA descriptor
    A\/ LIINT.LKEY=:X.LKEY             ; YES, SAVE IT
    IF A BIT HX21S THEN                 ; X21 CLEAR INDICATION?
        HASTAT BONE BLDON=:HASTAT       ; YES, SET BLOCK DONE
        LIINT.LKEY BONE XBLDN=:X.LKEY   ; TO TERMINATE
    FI
FI
```

**From SYMBOL-1-LIST.SYMB.TXT:**
- **HX21M = 060000 octal = 0x6000 = bits 13,14** (X.21 error mask)
- **HX21S = 000016 octal = 0x000E = bits 1,2,3** (X.21 status bits)

**In C# terms:**
```csharp
// X.21 error detection in RRTS
if ((currentRRTS & (RTSBits.X21D | RTSBits.X21S)) != 0)
{
    // X.21 protocol error detected
    if ((currentRRTS & RTSBits.X21S) != 0)  // X.21 clear indication
    {
        // Force block termination
        hastat |= RTSBits.BlockDone;  // Software flag
        lkey |= RTSBits.XBLDN;        // DMA descriptor flag
    }
}
```

#### 7. **HDLC Transmission Authorization**

**X.21 Mode Lock Mechanism:**
```assembly
; Line 107573: IF HXDOK ><X21OP THEN ELOCK; GO BACKX FI
; X21OP = X.21 Open signal, HXDOK = HDLC lock status
```

**HDLC controller can only transmit when:**
1. X.21 circuit is established (`X2DST = 14`)
2. X.21 driver authorizes HDLC operation (`HXDOK = X21OP`)
3. No X.21 protocol errors present

#### 8. **Dual Driver Architecture**

**SINTRAN implements two separate but coordinated drivers:**

1. **X.21 Driver** (`X21-DRIV:NPL`)
   - Handles circuit establishment/teardown
   - Manages X.21 state machine
   - Controls HDLC driver authorization
   - Processes X.21 signaling characters

2. **HDLC Driver** (Standard HDLC driver)
   - Handles HDLC frame transmission/reception
   - Performs DMA operations
   - Reports to X.21 driver for authorization
   - Same register interface but different behavioral context

### Implementation Requirements for X.21 Mode

**Emulator must support:**

1. **X.21 State Machine Implementation**
   ```csharp
   public enum X21Mode { PointToPoint, CircuitSwitched }
   public X21States currentX21State = X21States.Ready;
   public bool hdlcTransmissionAuthorized =>
       (mode == X21Mode.PointToPoint) ||
       (mode == X21Mode.CircuitSwitched && currentX21State == X21States.DataPhase);
   ```

2. **WTTC Register Mode-Dependent Behavior**
   ```csharp
   public void WriteWTTC(ushort value)
   {
       if (operatingMode == X21Mode.CircuitSwitched)
       {
           // Force disable ModemStatusChangeIE in X.21 mode
           value &= ~(ushort)TTCBits.ModemStatusChangeIE;
       }
       currentWTTC = value;
   }
   ```

3. **X.21 Error Integration with RRTS**
   ```cshort
   public ushort ReadRRTS()
   {
       ushort result = baseRRTS;

       if (operatingMode == X21Mode.CircuitSwitched)
       {
           // Add X.21 protocol status
           if (x21ProtocolError) result |= (ushort)(RTSBits.X21D | RTSBits.X21S);
           if (x21ClearIndication) result |= (ushort)RTSBits.X21S;
       }

       return result;
   }
   ```

**X.21 mode represents a fundamental paradigm shift from permanent HDLC links to dynamic circuit-switched networking, requiring completely different control flow and state management while using the same underlying HDLC hardware registers.**

## PART 9: HALF-DUPLEX MODE ANALYSIS

**CRITICAL DISCOVERY:** SINTRAN implements sophisticated half-duplex flow control for high-speed HDLC communication, particularly essential for **1 Mbit/s+ transmission speeds** where receiver buffer overrun becomes a critical issue.

### Half-Duplex Mode vs Full-Duplex Mode

#### 1. **Fundamental Operation Difference**

**Full-Duplex Mode (Default):**
- Simultaneous bidirectional communication
- Both transmitter and receiver operate independently
- RRTS register reflects receiver status only
- No RTS (Request To Send) flow control

**Half-Duplex Mode (Flow Control):**
- Controlled bidirectional communication
- **RTS control prevents receiver overrun**
- **ReadyForSending monitoring enables flow control**
- Used for high-speed links (1+ Mbit/s)

#### 2. **WTTC Register Control Patterns**

**From SINTRAN Source Code Analysis:**

**CMODI Variable Controls Half/Full Duplex:**
```assembly
; From HDSIN (HDLC Initialization) - Line 105045
IF A=:D = HALF THEN T:=40 ELSE T:=0 FI  ; HALF=1, set T=40 octal
T=:CMODI                                ; SET HALF OR FULL DUPLEX
```

**CMODI Values:**
- **CMODI = 0**: Full-Duplex Mode
- **CMODI = 40 octal = 0x0020**: Half-Duplex Mode

**WTTC Register Manipulation in Half-Duplex:**
```assembly
; From XHMST (Transmit Start) - Line 103723
1134+CMODI; T:=HDEV+WTTC; *EXR ST

; From HOINT (Transmit Complete) - Line 104047
IF CMODI = 40 THEN
    T:=HDEV+WTTC; *EXR ST   ; TURN OFF RQTS (RequestToSend)
FI
```

**In C# terms:**
```csharp
// Full-Duplex Mode (CMODI = 0)
// WTTC = 1134 octal = 0x025C
public void WriteWTTC_FullDuplex()
{
    currentWTTC = TTCBits.TransmitBufferEmptyIE |        // Bit 0
                  TTCBits.DataModuleEnable |             // Bit 2
                  TTCBits.TransmitEnable |               // Bit 3
                  TTCBits.RequestToSend |                // Bit 4: Always ON
                  TTCBits.TransmitHalfDuplexMode |       // Bit 6
                  TTCBits.ModemStatusChangeIE |          // Bit 7
                  TTCBits.DMATransferEnable |            // Bit 9
                  TTCBits.DMAModeSelect;                 // Bit 10
}

// Half-Duplex Mode (CMODI = 40 octal = 0x0020)
// WTTC = 1134+40 = 1174 octal = 0x027C
public void WriteWTTC_HalfDuplex()
{
    currentWTTC = TTCBits.TransmitBufferEmptyIE |        // Bit 0
                  TTCBits.DataModuleEnable |             // Bit 2
                  TTCBits.TransmitEnable |               // Bit 3
                  TTCBits.RequestToSend |                // Bit 4: Controlled
                  TTCBits.TransmitHalfDuplexMode |       // Bit 6: FORCED ON
                  TTCBits.ModemStatusChangeIE |          // Bit 7
                  TTCBits.DMATransferEnable |            // Bit 9
                  TTCBits.DMAModeSelect;                 // Bit 10
}
```

**Key Difference:** Half-duplex mode **forces** `TTCBits.TransmitHalfDuplexMode` (bit 6) to be set via the CMODI offset.

#### 3. **RTS Control Mechanism**

**RequestToSend Control in Half-Duplex:**
```assembly
; From HOINT (Output Interrupt Handler) - Line 104047
IF CMODI = 40 THEN                      ; If half-duplex mode
    T:=HDEV+WTTC; *EXR ST               ; Write WTTC register
FI                                      ; This clears RTS bit
```

**RTS Protocol Implementation:**
```assembly
; From HODUT (HDLC Output) - Line 103107
T:=107                                  ; Value 107 octal = RTS ON
T=:A; T:=HDEV+BWTTC; *EXR ST           ; Turn RTS ON for transmission

; Later in same routine - Line 103126
A:=105;T+"BWTTC-BWTCR";*EXR ST         ; Turn RTS OFF after transmission
```

**RTS Values:**
- **107 octal = 0x0047**: RTS ON + other control bits
- **105 octal = 0x0045**: RTS OFF + other control bits
- **Bit 4 difference**: RequestToSend control

#### 4. **ReadyForSending Monitoring**

**Half-Duplex Flow Control Logic:**

SINTRAN monitors the remote station's **ReadyForSending** (RTTS bit 6) to prevent transmission when the receiver cannot accept data.

```assembly
; From SINTRAN analysis - Remote station flow control
T:=HDEV+RTTS; *EXR ST                  ; Read transmitter status
IF A NBIT 6 THEN                       ; ReadyForSending = 0?
    ; Remote station not ready - WAIT
    GO WAIT_FOR_READY
FI
; Continue with transmission
```

**In C# terms:**
```csharp
public bool CanTransmit()
{
    if (operatingMode == HalfDuplexMode)
    {
        ushort rtts = ReadRTTS();

        // Check if remote station is ready to receive
        if ((rtts & TTSBits.ReadyForSending) == 0)
        {
            // Remote station buffer full - cannot transmit
            return false;
        }
    }

    return true; // Full-duplex or remote station ready
}
```

#### 5. **High-Speed Buffer Management**

**Why Half-Duplex is Essential at 1+ Mbit/s:**

At high transmission speeds, **receiver buffer overrun** becomes critical:

1. **1 Mbit/s = 125,000 bytes/second**
2. **SINTRAN interrupt processing** takes time
3. **DMA buffer chain exhaustion** can occur
4. **Flow control prevents data loss**

**Buffer Exhaustion Detection:**
```assembly
; From HIINT (Receiver Interrupt) - Line 104472
IF HASTAT/\"EMTY" >< 0 THEN            ; ListEmpty bit set?
    ; FATAL: No more receive buffers
    ; RECEIVER SHUTDOWN - notify remote to stop
FI
```

**Flow Control Sequence:**
```csharp
public void HandleBufferExhaustion()
{
    if (operatingMode == HalfDuplexMode)
    {
        // Clear ReadyForSending to signal remote station
        currentRTTS &= ~TTSBits.ReadyForSending;

        // Turn off RTS to prevent local transmission
        currentWTTC &= ~TTCBits.RequestToSend;
        WriteWTTC(currentWTTC);

        // Wait for buffer availability
        while (IsBufferListEmpty())
        {
            WaitForBufferRefill();
        }

        // Re-enable when buffers available
        currentRTTS |= TTSBits.ReadyForSending;
        currentWTTC |= TTCBits.RequestToSend;
        WriteWTTC(currentWTTC);
    }
}
```

#### 6. **Application-Controlled Mode Selection**

**From SINTRAN Source Code Analysis:**

Half-duplex mode is **explicitly set by user applications** via the HDLC initialization (HDSIN) function:

```assembly
; From HDSIN (HDLC Initialization) - Line 105035
T:=MASTB; * IMODU@3 LDATX              ; Read MODUS from user message
IF A=:D = HALF THEN T:=40 ELSE T:=0 FI ; HALF=1 sets CMODI=40, else CMODI=0
T=:CMODI                               ; Store duplex mode setting
```

**User Message Structure (from Line 105027 comments):**
- **MODUS field**: `0 = Full-duplex, 1 = Half-duplex, 2 = Maintenance mode`
- Applications **explicitly specify** which mode they want
- **No automatic speed-based selection** - user code decides

**Mode Selection is Application Decision:**
```csharp
// User application must explicitly choose duplex mode
public class HDLCInitMessage
{
    public int MODUS;     // 0=Full-duplex, 1=Half-duplex, 2=Maintenance
    public int CFSIZE;    // Max frame size
    public int IRTRY;     // Number of retries
    // ... other parameters
}

public void InitializeHDLC(HDLCMode requestedMode)
{
    var initMsg = new HDLCInitMessage
    {
        MODUS = (int)requestedMode,  // Application decides based on requirements
        // ... other settings
    };
    SendHDLCInitMessage(initMsg);
}
```

**Why Applications Choose Half-Duplex:**
1. **High-speed links** where flow control is essential
2. **Limited buffer scenarios** where overrun is likely
3. **Reliable delivery requirements** where retransmissions are costly
4. **Network topology** requiring controlled medium access

#### 7. **Transmission Timing Control**

**Half-Duplex Transmission Sequence:**
```assembly
; From SINTRAN HDLC driver analysis
1. Check ReadyForSending (remote station buffer status)
2. Assert RTS (RequestToSend)
3. Wait for CTS (ClearToSend) or equivalent
4. Transmit frame
5. Clear RTS after transmission complete
6. Monitor for receive activity
```

**Timing Implementation:**
```csharp
public async Task<bool> TransmitFrame(byte[] frameData)
{
    if (operatingMode == HalfDuplexMode)
    {
        // Step 1: Check remote station readiness
        if (!await WaitForRemoteReady(timeoutMs: 100))
            return false; // Remote station busy

        // Step 2: Assert RTS
        currentWTTC |= TTCBits.RequestToSend;
        WriteWTTC(currentWTTC);

        // Step 3: Wait for line access (hardware CTS)
        await WaitForClearToSend();

        // Step 4: Transmit
        bool success = await TransmitData(frameData);

        // Step 5: Clear RTS
        currentWTTC &= ~TTCBits.RequestToSend;
        WriteWTTC(currentWTTC);

        return success;
    }
    else
    {
        // Full-duplex: direct transmission
        return await TransmitData(frameData);
    }
}
```

#### 8. **Performance Impact and Benefits**

**Half-Duplex Mode Benefits:**
1. **Prevents data loss** at high speeds
2. **Reduces retransmissions** due to buffer overrun
3. **Ensures reliable delivery** in resource-constrained systems
4. **Maintains protocol efficiency** despite speed limitations

**Performance Trade-offs:**
1. **Reduced throughput** (bidirectional becomes sequential)
2. **Increased latency** (RTS/CTS overhead)
3. **Protocol complexity** (flow control logic)

**Optimal Use Cases:**
- **High-speed links** (1+ Mbit/s)
- **Limited receiver buffers**
- **CPU-intensive applications** (where interrupt processing is slow)
- **Long-distance connections** (where retransmissions are expensive)

### Implementation Requirements for Half-Duplex Mode

**Emulator must support:**

1. **CMODI Variable Control**
   ```csharp
   public class HDLCController
   {
       private int cmodi = 0; // 0=Full-duplex, 0x20=Half-duplex

       public void SetDuplexMode(HDLCMode mode)
       {
           cmodi = (mode == HDLCMode.HalfDuplex) ? 0x20 : 0x00;
       }
   }
   ```

2. **WTTC Register Offset Calculation**
   ```csharp
   public void WriteWTTC(ushort baseValue)
   {
       // Add CMODI offset for half-duplex
       ushort actualValue = (ushort)(baseValue + cmodi);

       if ((cmodi & 0x20) != 0) // Half-duplex mode
       {
           actualValue |= TTCBits.TransmitHalfDuplexMode; // Force bit 6
       }

       currentWTTC = actualValue;
   }
   ```

3. **RTS Control Logic**
   ```csharp
   public void ControlRTS(bool enable)
   {
       if ((cmodi & 0x20) != 0) // Half-duplex mode only
       {
           if (enable)
               currentWTTC |= TTCBits.RequestToSend;
           else
               currentWTTC &= ~TTCBits.RequestToSend;

           WriteWTTCRegister(currentWTTC);
       }
   }
   ```

4. **ReadyForSending Status**
   ```csharp
   public ushort ReadRTTS()
   {
       ushort result = baseRTTS;

       if ((cmodi & 0x20) != 0) // Half-duplex mode
       {
           // Reflect buffer availability in ReadyForSending
           if (HasAvailableBuffers())
               result |= TTSBits.ReadyForSending;
           else
               result &= ~TTSBits.ReadyForSending;
       }

       return result;
   }
   ```

**Half-duplex mode is SINTRAN's solution to high-speed HDLC communication challenges, providing essential flow control to prevent data loss when receiver processing cannot keep up with transmission rates, particularly critical at 1+ Mbit/s speeds where buffer exhaustion becomes inevitable without proper throttling.**

## PART 10: DEEP ANALYSIS CONCLUSIONS

### What We Discovered

1. **Only 5 of 16 RRTS bits matter**: Bits 0, 11, 13, 14, and partially 3
2. **Hardware vs Software flags**: RRTS BlockEnd/FrameEnd ignored; LKEY XBLDN critical
3. **Processing order is crucial**: ListEmpty check happens before block processing
4. **Bits 5 and 6 completely ignored**: SignalDetector/DataSetReady have zero impact
5. **SINTRAN validation is multi-layered**: Hardware status, then software content

### Implementation Strategy

**Focus debugging on the bits SINTRAN actually tests:**
- RRTS bit 0 (DataAvailable)
- RRTS bit 11 (ListEmpty)
- RRTS bits 13-14 (X21D/X21S)
- LKEY bit 3 (XBLDN)
- LKEY low 8 bits (RCOST pattern)

**Ignore bits that don't affect success:**
- RRTS bits 1,2,5,6,7,8,9,10,12,15
- These exist for hardware compatibility but SINTRAN never tests them

### Final Success Pattern

```csharp
// Perfect RRTS for reception success
ushort perfectRRTS = 0x036D;  // All critical bits correct

// Perfect LKEY for block processing
ushort perfectLKEY = 0x060B;  // Block control + XBLDN + RCOST

// Result: Packet delivered successfully to SINTRAN application
```

This analysis provides the complete understanding needed to implement a fully compatible HDLC controller emulator that successfully delivers packets to SINTRAN applications.