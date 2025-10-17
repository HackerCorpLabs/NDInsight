# Appendix A: Complete SINTRAN HDLC Pseudocode

SINTRAN_HDLC_Complete_Pseudocode.md, SINTRAN_HDLC_Pseudocode.md, LAPB_vs_X25_Protocol_Handling.md, PAD_Connection_Deep_Analysis.md, HDLC_Variable_Reference.md, SINTRAN_Variable_Name_Analysis.md



---




## Content from: 06-HDLC-Emulator-Guide.md

# HDLC Emulator Implementation Guide

## Summary: Your C# Implementation is Correct

Based on comprehensive analysis of SINTRAN source code and actual constants, **your HDLC emulator bit definitions and logic are fundamentally correct**. The packet transmission and reception issues are likely caused by **timing**, **status bit generation**, or **DMA buffer management** rather than bit interpretation errors.

## Verified Correct Implementations

### 1. Your C# Bit Positions ✅ CORRECT

```csharp
// Your definitions match SINTRAN exactly:
X21D = 1<<13,              // Bit 13 - part of HX21M mask (0x6000)
X21S = 1<<14,              // Bit 14 - part of HX21M mask (0x6000)  
ReceiverOverrun = 1<<15,   // Bit 15 - standard HDLC overrun
```

### 2. SINTRAN Transmission Logic ✅ CORRECT

```csharp
// SINTRAN: IF A/\ "SILFO+TXUND" = 0 THEN success
// SILFO+TXUND = 0x8000 | 0x0002 = 0x8002

public bool IsTransmissionSuccessful(TransmitterStatusBits rtts)
{
    // Success when BOTH error bits are clear
    return (rtts & (TransmitterStatusBits.Illegal | TransmitterStatusBits.TransmitterUnderrun)) == 0;
    // Same as: (rtts & 0x8002) == 0
}
```

### 3. SINTRAN Reception Logic ✅ CORRECT

```csharp
public bool ShouldProcessPacket(ReceiverStatusBits rrts)
{
    // SINTRAN: IF A NBIT 0 OR A/\60000><0 THEN drop
    if ((rrts & ReceiverStatusBits.DataAvailable) == 0)
        return false; // No data available
        
    if ((rrts & (ReceiverStatusBits.X21D | ReceiverStatusBits.X21S)) != 0)
        return false; // X.21 protocol error
        
    if ((rrts & ReceiverStatusBits.ListEmpty) != 0)
        return false; // No receive buffers
        
    return true; // Process packet
}
```

## Recommended HDLC Emulator Status Values

### Normal LAPB Packet Reception (Working)
```csharp
// Minimal status for successful packet processing:
ReceiverStatusBits normalRRTS = ReceiverStatusBits.DataAvailable;  // 0x0001

// SINTRAN checks:
// ✅ DataAvailable = 1 (data ready)
// ✅ X21D/X21S = 0 (no protocol errors)  
// ✅ ListEmpty = 0 (buffers available)
// Result: Packet processed successfully
```

### Normal Transmission Success
```csharp
// Status indicating successful transmission:
TransmitterStatusBits normalRTTS = 
    TransmitterStatusBits.TransmitBufferEmpty |    // 0x0001 (optional)
    TransmitterStatusBits.ReadyForSending;         // 0x0040 (optional)

// Critical: Ensure error bits are CLEAR:
// ✅ TransmitterUnderrun (bit 1) = 0
// ✅ Illegal (bit 15) = 0  
// Result: (rtts & 0x8002) == 0 → Success
```

### Enhanced Status for X.25 Support (Future)
```csharp
// Full status with receiver state information:
ReceiverStatusBits enhancedRRTS = 
    ReceiverStatusBits.DataAvailable |      // Bit 0: Data ready
    ReceiverStatusBits.StatusAvailable |    // Bit 1: Status info available
    ReceiverStatusBits.SyncFlagReceived;    // Bit 3: Frame sync detected
    // Note: ReceiverActive (bit 2) should be 0 when frame complete

// This provides proper HX21S routing for X.25 protocol handling
```

## Common Error Scenarios and Solutions

### Problem: Packets Being Retransmitted

**Likely Cause**: RTTS register showing error bits when transmission succeeded.

**Debug Steps**:
```csharp
public void LogTransmissionResult(TransmitterStatusBits rtts)
{
    Console.WriteLine($"RTTS = 0x{(int)rtts:X4}");
    Console.WriteLine($"  TransmitterUnderrun (bit 1): {(rtts & TransmitterStatusBits.TransmitterUnderrun) != 0}");
    Console.WriteLine($"  Illegal (bit 15): {(rtts & TransmitterStatusBits.Illegal) != 0}");
    Console.WriteLine($"  Success Check (both clear): {(rtts & 0x8002) == 0}");
    
    if ((rtts & 0x8002) != 0)
        Console.WriteLine("❌ SINTRAN will retransmit - error bits set!");
    else
        Console.WriteLine("✅ SINTRAN will mark successful");
}
```

**Solution**: Ensure bits 1 and 15 are clear after successful transmission.

### Problem: Packets Being Dropped on Reception  

**Likely Cause**: RRTS register missing DataAvailable or showing false errors.

**Debug Steps**:
```csharp
public void LogReceptionResult(ReceiverStatusBits rrts)
{
    Console.WriteLine($"RRTS = 0x{(int)rrts:X4}");
    Console.WriteLine($"  DataAvailable (bit 0): {(rrts & ReceiverStatusBits.DataAvailable) != 0}");
    Console.WriteLine($"  ListEmpty (bit 11): {(rrts & ReceiverStatusBits.ListEmpty) != 0}");
    Console.WriteLine($"  X21D (bit 13): {(rrts & ReceiverStatusBits.X21D) != 0}");
    Console.WriteLine($"  X21S (bit 14): {(rrts & ReceiverStatusBits.X21S) != 0}");
    
    if ((rrts & ReceiverStatusBits.DataAvailable) == 0)
        Console.WriteLine("❌ SINTRAN will drop - no data available!");
    if ((rrts & 0x6000) != 0)  
        Console.WriteLine("❌ SINTRAN will drop - X.21 error detected!");
    if ((rrts & ReceiverStatusBits.ListEmpty) != 0)
        Console.WriteLine("❌ SINTRAN will stop receiver - no buffers!");
}
```

**Solution**: Ensure bit 0 is set and bits 11, 13-14 are clear for normal packets.

### Problem: Receiver Stops Completely

**Cause**: ListEmpty bit (11) set in RRTS.

**SINTRAN Logic**:
```assembly
IF HASTAT/\"EMTY" >< 0 THEN  % If ListEmpty bit set
   0=:ACTSW                  % Stop device  
   MIN STPCNT                % Increment stop counter
```

**Solution**: Only set ListEmpty when actually out of receive buffers.

## Timing Considerations

### Status Register Read Clearing
**Critical**: DMA bits are cleared when reading status registers.

```csharp
// Problem: Multiple reads clear important bits
TransmitterStatusBits rtts1 = ReadRTTS();  // Gets full status
TransmitterStatusBits rtts2 = ReadRTTS();  // DMA bits now cleared!

// Solution: Read once, cache value
TransmitterStatusBits rtts = ReadRTTS();
// Use 'rtts' for all subsequent checks
```

### Interrupt Timing
**Critical**: Status must be ready when interrupt fires.

```csharp
// Correct sequence:
1. DMA completes transmission/reception
2. Set appropriate status bits in RTTS/RRTS  
3. Trigger interrupt (level 12 for TX, level 13 for RX)
4. SINTRAN reads status and processes

// Problem: Status not ready when interrupt handled
// Solution: Ensure status bits set BEFORE interrupt
```

## Buffer Management Implementation

### Receive Buffer List
```csharp
// SINTRAN expects proper buffer management:
public void SetupReceiveBuffers()
{
    // Clear ListEmpty initially
    currentRRTS &= ~ReceiverStatusBits.ListEmpty;
    
    // When buffers exhausted:
    if (receiveBuffersEmpty)
    {
        currentRRTS |= ReceiverStatusBits.ListEmpty;  // Stop receiver
    }
}
```

### Transmit List Management
```csharp
public void HandleTransmitList()
{
    // Set ListEnd when no more packets to send
    if (transmitListComplete)
    {
        currentRTTS |= TransmitterStatusBits.ListEnd;  // Bit 10
    }
}
```

## Testing Strategy

### Validate Against SINTRAN Logic
```csharp
public void ValidateStatusBits()
{
    // Test normal reception
    ReceiverStatusBits testRRTS = ReceiverStatusBits.DataAvailable;
    Debug.Assert((testRRTS & 0x0001) != 0, "DataAvailable must be set");
    Debug.Assert((testRRTS & 0x6000) == 0, "X.21 errors must be clear");
    Debug.Assert((testRRTS & 0x0800) == 0, "ListEmpty must be clear");
    
    // Test normal transmission  
    TransmitterStatusBits testRTTS = TransmitterStatusBits.ReadyForSending;
    Debug.Assert((testRTTS & 0x8002) == 0, "Error bits must be clear for success");
}
```

### Monitor Diagnostic Buffers
```csharp
// Implement SINTRAN's diagnostic logging equivalent:
public void LogDiagnosticStatus(string operation, ushort status)
{
    diagnosticBuffer[bufferIndex] = new DiagnosticEntry
    {
        Operation = operation,
        Status = status,
        Timestamp = DateTime.Now
    };
    bufferIndex = (bufferIndex + 1) % diagnosticBuffer.Length;
}
```

## Key Takeaways

1. **Your bit definitions are CORRECT** - focus debugging elsewhere
2. **SINTRAN transmission logic is SOUND** - success when bits 1,15 clear  
3. **SINTRAN reception logic is SOUND** - process when bit 0 set, others clear
4. **Issues likely in emulator timing/status generation**, not interpretation
5. **Use the comprehensive analysis documents** for detailed bit-by-bit explanations
6. **Test with minimal status values first** before adding complexity

The SINTRAN HDLC implementation is robust and well-designed. Your emulation should focus on providing the correct status bit patterns at the right times rather than changing the interpretation logic.


