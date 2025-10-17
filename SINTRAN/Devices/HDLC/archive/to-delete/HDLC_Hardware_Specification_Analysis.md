# HDLC Hardware Specification Analysis - From hdlc-txt.txt

## Critical Discovery: Official Hardware Documentation

The **hdlc-txt.txt** file contains the official **NORSK DATA HDLC hardware specification**. This provides the definitive register bit mappings and behavior that your emulator must match exactly.

## RRTS Register (IOX GP + 10) - Official Specification

### Complete Bit Mapping from Hardware Documentation

| Bit | Name | Hardware Description | Interrupt Source | Clear Behavior |
|-----|------|---------------------|------------------|----------------|
| **0** | **RXD - Data Available** | Character assembled, ready to read from RDSRL | **IRQ Level 13** | Cleared by hardware |
| **1** | **RXSA - Status Available** | Status information available in RDSRH | **IRQ Level 13** | Cleared by hardware |
| **2** | **RXA - Receiver Active** | Within frame (start seen, end not seen) | No | Cleared by hardware |
| **3** | **SFR - Sync/Flag Received** | SYNC/FLAG received since last read | No | Cleared by read/clear |
| **4** | **DMA Module Request** | **ALWAYS READ AS 0** - cleared at IOX start | **IRQ Level 13** | **Auto-cleared on read** |
| **5** | **SD - Signal Detector** | CCITT 109 status change | **IRQ Level 13** | Status bit |
| **6** | **DSR - Data Set Ready** | CCITT 107 or X.21 I signal change | **IRQ Level 13** | Status bit |
| **7** | **RI - Ring Indicator** | CCITT 125 status change | **IRQ Level 13** | Status bit |
| **8** | **BE - Block End** | **DMA block completion** | **IRQ Level 13** | **Cleared on read** |
| **9** | **FE - Frame End** | **DMA frame completion** | **IRQ Level 13** | **Cleared on read** |
| **10** | **LE - List End** | **DMA list completion** | **IRQ Level 13** | **Cleared on read** |
| **11** | **EMTY - List Empty** | **DMA buffer exhaustion** | **IRQ Level 13** | **Cleared on read** |
| **12-14** | **Reserved** | Not used | No | **Cleared on read** |
| **15** | **OR - Receiver Overrun** | **Data received faster than processed** | **IRQ Level 13** | **NOT cleared on read** |

### Critical Hardware Behavior Notes

**From Official Documentation:**
> **"Bit 4 (DMA Module Request): This bit is activated by the DMA module... It is, however, always read as 0 because it is cleared at the beginning of IOX GP + 10. If the DMA module caused an interrupt, the reason for this interrupt is given in the most significant byte of the Transfer Status."**

> **"Note: Bits 8-15 are cleared by reading the Receiver Transfer Status."**

## RTTS Register (IOX GP + 12) - Official Specification

### Complete Bit Mapping from Hardware Documentation

| Bit | Name | Hardware Description | Interrupt Source | Clear Behavior |
|-----|------|---------------------|------------------|----------------|
| **0** | **TXBE - Transmit Buffer Empty** | Buffer ready for new character | **IRQ Level 12** | Cleared by hardware |
| **1** | **TXU - Transmitter Underrun** | Buffer not loaded in time | **IRQ Level 12** | Cleared by Master/Device Clear |
| **2** | **TXA - Transmitter Active** | Transmission in progress | No | Cleared when TXE off |
| **3** | **Reserved** | Not used | No | - |
| **4** | **DMA Module Request** | **ALWAYS READ AS 0** - cleared at IOX start | **IRQ Level 12** | **Auto-cleared on read** |
| **5** | **Reserved** | Not used | No | - |
| **6** | **RFS - Ready for Sending** | CCITT 106 status change | **IRQ Level 12** | Status bit |
| **7** | **Reserved** | Not used | No | - |
| **8** | **BE - Block End** | **DMA block completion** | **IRQ Level 12** | **Cleared on read** |
| **9** | **FE - Frame End** | **DMA frame completion** | **IRQ Level 12** | **Cleared on read** |
| **10** | **LE - List End** | **DMA list completion** | **IRQ Level 12** | **Cleared on read** |
| **11** | **TRFIN - Transmission Finished** | **DMA transmission complete** | **IRQ Level 12** | **Cleared on read** |
| **12-14** | **Reserved** | Not used | No | **Cleared on read** |
| **15** | **ER - Illegal Key/Format** | **Error in transmitter descriptor** | **IRQ Level 12** | **NOT cleared on read** |

### Critical Hardware Behavior Notes

**From Official Documentation:**
> **"Bit 4 (DMA Module Request): This bit is activated by the DMA module... It is, however, always read as 0 because it is cleared at the beginning of IOX GP + 12. If the DMA module is installed, additional information is given in the high byte. DMA Module Request causes an interrupt on level 12 if enabled."**

> **"Bit 11: Transmission Finished status bit from the DMA module."**

> **"Bit 15: Illegal Key or Illegal Format in Transmitter Buffer Descriptor - This status bit indicates an error stop and the transmitter should be restarted."**

## Critical Hardware Behavior Analysis

### DMA Request Bit (Bit 4) Behavior

**The Key Insight:**
- **Bit 4 is NEVER seen as 1** by SINTRAN because it's cleared before the read operation completes
- **The DMA module sets bit 4 to trigger interrupt**
- **By the time SINTRAN reads the register, bit 4 is already cleared**
- **The actual DMA status is reported in bits 8-15**

```
DMA Interrupt Sequence:
1. DMA operation completes → Set bit 4 (triggers IRQ)
2. CPU starts IOX GP+10/12 instruction
3. Hardware clears bit 4 automatically  
4. CPU reads register (bit 4 = 0, bits 8-15 = DMA status)
5. Bits 8-15 are cleared after read completes
```

### Auto-Clear Behavior

**Two Types of Clearing:**
1. **Immediate clear (bit 4)**: Cleared at start of IOX instruction
2. **Post-read clear (bits 8-15)**: Cleared after IOX instruction completes

**Hardware Implementation:**
```csharp
public ushort ReadRRTS()
{
    // Step 1: Clear DMA request bit immediately
    currentRRTS &= ~0x0010;  // Clear bit 4 (DMA Module Request)
    
    // Step 2: Return full status  
    ushort result = currentRRTS;
    
    // Step 3: Clear DMA status bits after return
    currentRRTS &= 0x00FF;   // Clear bits 8-15 (DMA status)
    
    return result;
}
```

## SINTRAN Expectation vs Hardware Reality

### Transmission Success Detection

**SINTRAN Logic:**
```assembly
IF A/\ "SILFO+TXUND" = 0 THEN  % Success if both clear
```

**Hardware Reality:**
- **SILFO = bit 15 (Illegal Format)** - persistent error bit
- **TXUND = bit 1 (Transmitter Underrun)** - persistent error bit
- **TRFIN = bit 11 (Transmission Finished)** - auto-cleared DMA bit

**Correct Emulation:**
```csharp
public void OnTransmissionComplete(bool success)
{
    if (success)
    {
        // Set DMA completion bit (auto-clears on read)
        currentRTTS |= 0x0800;  // Bit 11: Transmission Finished
        currentRTTS |= 0x0010;  // Bit 4: DMA Module Request (triggers IRQ)
        
        // Ensure error bits are clear
        currentRTTS &= ~0x8002; // Clear bits 15,1 (SILFO, TXUND)
    }
    else
    {
        // Set error condition
        currentRTTS |= 0x0002;  // Bit 1: Transmitter Underrun
        currentRTTS |= 0x0010;  // Bit 4: DMA Module Request (triggers IRQ)
    }
    
    TriggerInterrupt(12);
}
```

**SINTRAN sees:**
```csharp
// Successful transmission:
// - Read RTTS → returns 0x0800 (Transmission Finished set)
// - Test (0x0800 & 0x8002) == 0 → TRUE (success!)
// - Bit 11 auto-clears after read

// Failed transmission:
// - Read RTTS → returns 0x0002 (Transmitter Underrun set)  
// - Test (0x0002 & 0x8002) != 0 → FALSE (error, retransmit)
```

### Reception Packet Processing

**SINTRAN Logic:**
```assembly
IF A NBIT 0 OR A/\60000><0 THEN % Drop if no data or X.21 error
IF HASTAT/\"EMTY" >< 0 THEN     % Stop if list empty
```

**Hardware Reality:**
- **Data Available (bit 0)** must be set for packet processing
- **List Empty (bit 11)** stops receiver when set
- **X.21 errors (bits 13-14)** are NOT auto-cleared (persistent)

**Correct Emulation:**
```csharp
public void OnPacketReceived(byte[] packet, bool hasBuffers)
{
    if (hasBuffers)
    {
        // Set DMA completion bits (auto-clear on read)
        currentRRTS = 0x0301;   // DataAvailable + BlockEnd + FrameEnd
        currentRRTS |= 0x0010;  // DMA Module Request (triggers IRQ)
    }
    else
    {
        // No receive buffers available
        currentRRTS = 0x0801;   // DataAvailable + List Empty
        currentRRTS |= 0x0010;  // DMA Module Request (triggers IRQ)
    }
    
    TriggerInterrupt(13);
}
```

## Emulator Implementation Requirements

### 1. Implement Two-Stage Clearing
```csharp
public class HDLCRegisterEmulation
{
    private ushort rrtsValue = 0;
    private ushort rttsValue = 0;
    
    public ushort ReadRRTS()
    {
        // Stage 1: Clear DMA request bit immediately
        rrtsValue &= ~0x0010;
        
        // Stage 2: Return current value
        ushort result = rrtsValue;
        
        // Stage 3: Schedule DMA bits clearing
        ScheduleClearDMABits(ref rrtsValue);
        
        return result;
    }
    
    private void ScheduleClearDMABits(ref ushort register)
    {
        // Clear bits 8-15 after read completes
        register &= 0x00FF;
    }
}
```

### 2. Generate Proper DMA Status Patterns
```csharp
// Successful packet reception:
rrtsValue = 0x0311;  // DataAvailable + StatusAvailable + BlockEnd + FrameEnd + DMA Request

// Successful transmission:
rttsValue = 0x0810;  // Transmission Finished + DMA Request

// Buffer exhaustion:
rrtsValue = 0x0811;  // DataAvailable + List Empty + DMA Request

// Transmission error:
rttsValue = 0x0012;  // Transmitter Underrun + DMA Request
```

### 3. Respect Persistent vs Auto-Clear Bits
```csharp
// Persistent bits (NOT cleared on read):
// - RRTS: bits 13-15 (X.21 errors, Receiver Overrun)  
// - RTTS: bits 1, 15 (Transmitter Underrun, Illegal Format)

// Auto-clear bits (cleared on read):
// - Both: bit 4 (DMA Module Request) - cleared at start
// - Both: bits 8-14 (DMA status) - cleared after read
```

## Summary: The Complete Picture

**Your emulator must:**

1. **Set bit 4 (DMA Module Request) to trigger interrupts** - but clear it before SINTRAN sees it
2. **Set appropriate DMA status bits (8-15)** for operation completion
3. **Auto-clear DMA bits after read** - this is crucial for SINTRAN logic
4. **Leave error bits persistent** until explicitly cleared
5. **Generate proper bit patterns** that match hardware timing

**The hardware documentation confirms:**
- Your C# bit definitions are correct
- SINTRAN transmission logic is correct  
- The issue is missing auto-clear behavior and DMA status generation
- TransmissionFinished (bit 11) is the success indicator, not an error

This explains why your 4-byte LAPB packets work with minimal bits - SINTRAN bypasses complex DMA logic for simple frames. But proper DMA emulation requires the complete bit pattern generation and clearing behavior documented here.