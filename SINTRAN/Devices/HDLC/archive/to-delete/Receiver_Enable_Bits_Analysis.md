# Receiver Enable Bits (WRTC) Analysis - DMA Receive Configuration

## Overview: WRTC Register (IOX+11)

The **Write Receiver Transfer Control (WRTC)** register configures how the HDLC receiver operates, especially for DMA mode. SINTRAN writes specific bit patterns to this register to set up reception parameters.

## SINTRAN Usage Patterns from Source Code

### Key WRTC Values Used by SINTRAN

From the source code analysis, SINTRAN uses these specific values:

| Value | Octal | Binary | Usage Context | Purpose |
|-------|--------|---------|--------------|---------|
| **100** | 144 | 01100100 | Device clear | Clear receiver state |
| **105** | 145 | 01101001 | Start receiver | Enable basic reception |
| **140** | 214 | 10001100 | Maintenance mode | Enable maintenance + DTR |
| **300** | 454 | 100101100 | Modem status | Enable modem status monitoring |
| **304** | 456 | 100110100 | DMA + Modem status | Full DMA with status monitoring |
| **305** | 457 | 100110101 | DMA + All interrupts | Complete DMA setup |
| **1001** | 1771 | 1111101001 | Full DMA operation | Complete DMA receiver setup |

## Detailed Analysis of Each SINTRAN Usage

### Value 100 (Octal 144) - Device Clear
**Source locations: Lines 076006, 076017, 076033, 104741, 110746, 110765**

```assembly
100; T+"BWRTC-BRRDR"; *EXR ST   % STOP RECEIVER
A:=100; T:=HDEV+BWRTC; *EXR ST
```

**Bit breakdown (100 = 0x40 = 01000000):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DTR;                          // Bit 6 = 1
    // Total: 0x40 = 64 decimal = 100 octal
```

**Purpose**: Device clear operation
- Keep DTR (Data Terminal Ready) active (bit 6)
- Clear all other receiver operations
- Used to stop/reset receiver

### Value 105 (Octal 145) - Basic Receiver Start  
**Source location: Line 076011**

```assembly
105; *EXR ST                    % START AND SEARCH FOR SYN
```

**Bit breakdown (105 = 0x45 = 01000101):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DataAvailableIE |             // Bit 0 = 1
    ReceiverEnableBits.EnableReceiver |              // Bit 2 = 1
    ReceiverEnableBits.DTR;                          // Bit 6 = 1
    // Total: 0x45 = 69 decimal = 105 octal
```

**Purpose**: Basic character-mode reception
- Enable receiver (bit 2)
- Enable data available interrupt (bit 0)
- Keep DTR active (bit 6)

### Value 140 (Octal 214) - Maintenance Mode
**Source locations: Lines 104745, 110752**

```assembly  
A:=140; *EXR ST                 % SET MAINTENANCE MODUS, CLEAR PULSE
```

**Bit breakdown (140 = 0x60 = 01100000):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DeviceClear_SelectMaintenance | // Bit 5 = 1
    ReceiverEnableBits.DTR;                            // Bit 6 = 1
    // Total: 0x60 = 96 decimal = 140 octal
```

**Purpose**: Maintenance/loopback mode
- Enable maintenance mode (bit 5) - loops TX data back to RX
- Keep DTR active (bit 6)
- Used for testing and diagnostics

### Value 300 (Octal 454) - Modem Status Monitoring
**Source locations: Lines 102201, 110407, 110503**

```assembly
300; T:=X2DHD+XWRTC; *EXR ST    % MUST SEE DCE CLEAR REQUEST  
300=:A; T:=X2DHD+XWRTC; *EXR ST % GET RID OF OVERRUN SITUATION
```

**Bit breakdown (300 = 0xC0 = 11000000):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DTR |                         // Bit 6 = 1
    ReceiverEnableBits.ModemStatusChangeIE;          // Bit 7 = 1
    // Total: 0xC0 = 192 decimal = 300 octal
```

**Purpose**: Monitor modem status changes
- Enable modem status change interrupts (bit 7)
- Keep DTR active (bit 6)
- Used for X.21 protocol state detection

### Value 304 (Octal 456) - DMA + Modem Status
**Source locations: Lines 102133, 111106**

```assembly
304/\HDXOK; T:=HDEV+BWRTC; *EXR ST   % NO, ENABLE FOR STATUS CHANGE
304; T:=X2DHD+XWRTC; *EXR ST         % TAKE CARE OF MODEM STATUS
```

**Bit breakdown (304 = 0xC4 = 11000100):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.EnableReceiver |              // Bit 2 = 1
    ReceiverEnableBits.DTR |                         // Bit 6 = 1  
    ReceiverEnableBits.ModemStatusChangeIE;          // Bit 7 = 1
    // Total: 0xC4 = 196 decimal = 304 octal
```

**Purpose**: Receiver with modem status monitoring
- Enable receiver (bit 2)
- Monitor modem status changes (bit 7)
- Keep DTR active (bit 6)

### Value 305 (Octal 457) - DMA + All Interrupts
**Source location: Line 103043**

```assembly
305/\HDXOK; T:=HDEV+BWRTC;*EXR ST
```

**Bit breakdown (305 = 0xC5 = 11000101):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DataAvailableIE |             // Bit 0 = 1
    ReceiverEnableBits.EnableReceiver |              // Bit 2 = 1
    ReceiverEnableBits.DTR |                         // Bit 6 = 1
    ReceiverEnableBits.ModemStatusChangeIE;          // Bit 7 = 1
    // Total: 0xC5 = 197 decimal = 305 octal
```

**Purpose**: Full interrupt-driven reception
- Enable data available interrupt (bit 0)
- Enable receiver (bit 2)  
- Monitor modem status (bit 7)
- Keep DTR active (bit 6)

### Value 1001 (Octal 1771) - Full DMA Operation
**Source location: Line 111053**

```assembly
A:=1001; T+"XWDCR-XWDMA"; *EXR ST % START RECEIVER (BANK 1)
```

**Note**: This appears to be written to a different register (WDCR, not WRTC), but represents the complete DMA setup value.

**Bit breakdown (1001 = 0x201 = 1000000001):**
```csharp
ReceiverEnableBits enableBits = 
    ReceiverEnableBits.DataAvailableIE |             // Bit 0 = 1
    // Bit 9 = 1 (likely FrameEndIE for DMA)
    // Total: 0x201 = 513 decimal = 1001 octal
```

## Critical DMA Enable Bits Pattern

### Standard HDLC DMA Receiver Setup
**Based on SINTRAN patterns, a typical DMA setup would be:**

```csharp
ReceiverEnableBits dmaSetup = 
    ReceiverEnableBits.EnableReceiver |              // Bit 2 - MANDATORY
    ReceiverEnableBits.EnableReceiverDMA |           // Bit 3 - MANDATORY for DMA
    ReceiverEnableBits.DMAModuleIE |                 // Bit 4 - For DMA interrupts
    ReceiverEnableBits.DTR |                         // Bit 6 - Keep modem connection
    ReceiverEnableBits.ModemStatusChangeIE;          // Bit 7 - Monitor line status
    // Total: 0xDC = 220 decimal = 334 octal (not directly seen in SINTRAN)
```

### Key Observations from SINTRAN Code

1. **SINTRAN rarely sets EnableReceiverDMA (bit 3) explicitly** in the analyzed code
2. **Most operations use character-mode reception** with DataAvailableIE (bit 0)
3. **DTR (bit 6) is almost always kept active** (present in all major values)
4. **ModemStatusChangeIE (bit 7) is frequently used** for line monitoring
5. **DMA interrupt enable bits (8-10) are NOT used** in character operations

### HDXOK Mask Usage

**Pattern**: Many WRTC writes use `XXX/\HDXOK` format:

```assembly
304/\HDXOK; T:=HDEV+BWRTC; *EXR ST   % Apply HDXOK mask
305/\HDXOK; T:=HDEV+BWRTC;*EXR ST
```

**This suggests**: HDXOK is a system mask that modifies the base receiver configuration based on hardware capabilities or operational mode.

## Emulator Implementation Requirements

### For DMA Reception Setup
```csharp
public void ConfigureReceiverForDMA(bool enableInterrupts, bool monitorModem)
{
    ReceiverEnableBits config = 
        ReceiverEnableBits.EnableReceiver |          // Bit 2 - MANDATORY
        ReceiverEnableBits.EnableReceiverDMA |       // Bit 3 - MANDATORY for DMA
        ReceiverEnableBits.DTR;                      // Bit 6 - Keep connection
        
    if (enableInterrupts)
    {
        config |= ReceiverEnableBits.DMAModuleIE;    // Bit 4 - DMA interrupts
    }
    
    if (monitorModem)
    {
        config |= ReceiverEnableBits.ModemStatusChangeIE; // Bit 7 - Modem status
    }
    
    WriteWRTC(config);
}
```

### SINTRAN Pattern Recognition
```csharp
public void HandleWRTCWrite(ushort value)
{
    switch (value)
    {
        case 100:  // Device clear
            ClearReceiver();
            SetDTR(true);
            break;
            
        case 105:  // Basic character reception
            SetupCharacterMode();
            EnableReceiver(true);
            SetDTR(true);
            break;
            
        case 140:  // Maintenance mode
            SetMaintenanceMode(true);
            SetDTR(true);
            break;
            
        case 300:  // Modem status monitoring
            EnableModemStatusMonitoring(true);
            SetDTR(true);
            break;
            
        case 304:  // Receiver + modem status
            EnableReceiver(true);
            EnableModemStatusMonitoring(true);
            SetDTR(true);
            break;
            
        case 305:  // Full interrupt mode
            EnableReceiver(true);
            EnableDataAvailableInterrupts(true);
            EnableModemStatusMonitoring(true);
            SetDTR(true);
            break;
    }
}
```

## Summary

SINTRAN receiver configuration patterns show:

1. **DTR (bit 6) almost always active** - maintains modem connection
2. **Character-mode reception preferred** over pure DMA in most cases  
3. **Modem status monitoring (bit 7) frequently used** for line management
4. **Device clear (100) and basic enable (105)** are fundamental operations
5. **Maintenance mode (140)** used for testing and diagnostics

**For DMA emulation**: Your receiver must support both character-mode (with DataAvailableIE) and pure DMA mode (with EnableReceiverDMA), with proper DTR and modem status handling.

The **EnableReceiverDMA (bit 3)** is the critical bit for DMA operation - when set, RXDA triggers DMA requests instead of CPU interrupts.