# DMA Buffer List Interrupt Analysis - Multiple Buffers & Frames

## The Critical Question

**When DMA transmission starts with a list pointer containing multiple buffers and potential multiple frames, do you create interrupts after each buffer/frame, or only when the entire list is complete?**

## Answer: Multiple Interrupts per List

Based on analysis of the hardware specification and SINTRAN code, **the DMA controller generates MULTIPLE interrupts during list processing**, not just one at the end.

## Hardware DMA Interrupt Sources (from hdlc-txt.txt)

### RTTS (Transmitter) Interrupt Sources
From the official hardware specification:

| Bit | Name | When Triggered | Purpose |
|-----|------|----------------|---------|
| **8** | **Block End (BE)** | **After each buffer** | Signals single buffer completion |
| **9** | **Frame End (FE)** | **After each frame** | Signals HDLC frame completion |
| **10** | **List End (LE)** | **After entire list** | Signals all buffers processed |
| **11** | **Transmission Finished (TRFIN)** | **After entire list** | Signals DMA operation complete |

### Key Hardware Behavior
> **"DMA Module Request causes an interrupt on level 12 if enabled"**  
> **"Note that Transmission Finished (Transmitter Transfer Status, bit 11) always gives a DMA Module Request (bit 4)"**

## DMA List Processing Sequence

### Example: 3-Buffer List Transmission

```
DMA List: [Buffer1] → [Buffer2] → [Buffer3] → END

Interrupt Sequence:
1. Start transmission → Set up list
2. Buffer1 complete → IRQ12 (BlockEnd=1, FrameEnd=1)
3. Buffer2 complete → IRQ12 (BlockEnd=1, FrameEnd=1)  
4. Buffer3 complete → IRQ12 (BlockEnd=1, FrameEnd=1)
5. List complete → IRQ12 (ListEnd=1, TransmissionFinished=1)
```

**Result: 4 interrupts total** (3 buffer completions + 1 list completion)

## SINTRAN Code Evidence

### List End Check (Line 103115)
```assembly
T:=HDEV+BRTTS;*EXR ST; BSKP ONE 10 DA     % READ TRANSMITTER TRANSFER STATUS
```

**Analysis:**
```csharp
// BSKP ONE 10 DA = Skip if bit 10 (ListEnd) is SET
if ((rtts & 0x0400) != 0)  // ListEnd bit set
{
    skipNextOperation();  // List is complete
}
else
{
    continueProcessing(); // More buffers to process
}
```

**This shows SINTRAN expects to check ListEnd bit during processing**, implying multiple interrupts occur before the list is complete.

## Interrupt Generation Pattern

### After Each Buffer Completion
```csharp
public void OnBufferComplete(bool isLastBuffer, bool isLastInFrame)
{
    TransmitterStatusBits rtts = 0;
    
    // Always set for buffer completion
    rtts |= TransmitterStatusBits.BlockEnd;        // Bit 8
    rtts |= TransmitterStatusBits.DMAModuleRequest; // Bit 4 (triggers IRQ)
    
    // Set if this buffer ends a frame
    if (isLastInFrame)
    {
        rtts |= TransmitterStatusBits.FrameEnd;     // Bit 9
    }
    
    // Set if this is the last buffer in the list
    if (isLastBuffer)
    {
        rtts |= TransmitterStatusBits.ListEnd;              // Bit 10
        rtts |= TransmitterStatusBits.TransmissionFinished; // Bit 11
    }
    
    // Update hardware register
    currentRTTS = rtts;
    
    // Trigger interrupt
    TriggerInterrupt(12);  // HOINT handler
}
```

### SINTRAN Processing Response
```csharp
// SINTRAN HOINT handler processes each interrupt:
public void HOINT_Handler()
{
    // Read status (clears DMA bits automatically)
    TransmitterStatusBits rtts = ReadRTTS();
    
    // Check for completion
    if ((rtts & 0x8002) == 0)  // SILFO+TXUND check
    {
        // Success - check what completed
        if ((rtts & 0x0400) != 0)  // ListEnd
        {
            // Entire list complete
            markTransmissionComplete();
        }
        else if ((rtts & 0x0100) != 0)  // BlockEnd
        {
            // Just one buffer complete, continue with next
            continueWithNextBuffer();
        }
    }
    else
    {
        // Error occurred
        handleTransmissionError();
    }
}
```

## Receiver Side: Same Pattern

### RRTS (Receiver) Multiple Interrupts
```csharp
public void OnReceiveBufferFilled(bool isLastBuffer, bool isFrameComplete)
{
    ReceiverStatusBits rrts = ReceiverStatusBits.DataAvailable;
    rrts |= ReceiverStatusBits.StatusAvailable;
    rrts |= ReceiverStatusBits.DMAModuleRequest;    // Triggers IRQ
    
    // Always set for buffer completion
    rrts |= ReceiverStatusBits.BlockEnd;            // Bit 8
    
    // Set if frame is complete
    if (isFrameComplete)
    {
        rrts |= ReceiverStatusBits.FrameEnd;        // Bit 9
    }
    
    // Set if no more receive buffers
    if (isLastBuffer)
    {
        rrts |= ReceiverStatusBits.ListEnd;         // Bit 10
        // Note: ListEmpty (bit 11) is different - means no buffers available
    }
    
    currentRRTS = rrts;
    TriggerInterrupt(13);  // HIINT handler
}
```

## Practical Implementation Strategy

### Multi-Buffer DMA Transmission
```csharp
public class HDLCDMATransmitter
{
    private Queue<DMABuffer> transmitList;
    private bool transmissionActive = false;
    
    public void StartTransmission(DMABuffer[] buffers)
    {
        transmitList = new Queue<DMABuffer>(buffers);
        transmissionActive = true;
        
        // Start with first buffer
        ProcessNextBuffer();
    }
    
    private void ProcessNextBuffer()
    {
        if (!transmitList.Any())
        {
            // List complete
            OnListComplete();
            return;
        }
        
        DMABuffer currentBuffer = transmitList.Dequeue();
        
        // Simulate buffer transmission
        Task.Delay(SimulateTransmissionTime(currentBuffer)).ContinueWith(t => 
        {
            // Buffer complete
            bool isLastBuffer = !transmitList.Any();
            bool isFrameEnd = currentBuffer.IsFrameEnd;
            
            OnBufferComplete(isLastBuffer, isFrameEnd);
            
            if (!isLastBuffer)
            {
                // Continue with next buffer
                ProcessNextBuffer();
            }
        });
    }
    
    private void OnListComplete()
    {
        // Final interrupt with ListEnd + TransmissionFinished
        TransmitterStatusBits rtts = 
            TransmitterStatusBits.ListEnd |               // Bit 10
            TransmitterStatusBits.TransmissionFinished |  // Bit 11
            TransmitterStatusBits.DMAModuleRequest;       // Bit 4 (IRQ trigger)
            
        currentRTTS = rtts;
        TriggerInterrupt(12);
        
        transmissionActive = false;
    }
}
```

## Summary: Multiple Interrupts Required

**Your DMA emulator must generate:**

1. **One interrupt per buffer completion** (BlockEnd bit set)
2. **Additional interrupt for frame completion** (FrameEnd bit set)  
3. **Final interrupt for list completion** (ListEnd + TransmissionFinished bits set)

**For a 3-buffer, 2-frame list:**
- Buffer1 complete → IRQ (BlockEnd + FrameEnd)
- Buffer2 complete → IRQ (BlockEnd)  
- Buffer3 complete → IRQ (BlockEnd + FrameEnd + ListEnd + TransmissionFinished)

**This explains why SINTRAN has the ListEnd check** - it needs to differentiate between "buffer done, continue" and "list done, transmission complete" interrupts.

**The key insight:** DMA operations are **progressive** with **incremental status updates**, not **monolithic** with **single completion notification**.