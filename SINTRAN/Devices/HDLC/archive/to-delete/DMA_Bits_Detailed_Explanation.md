# DMA Status Bits Detailed Explanation - Block, Frame, and List Concepts

## The Core Confusion Explained

Your confusion about **BlockEnd**, **FrameEnd**, and **ListEnd** bits stems from the **hierarchical structure** of DMA operations. These bits represent **different levels of completion** in a complex multi-layered system.

## DMA Hierarchy: Block → Frame → List

### Understanding the Three Levels

```
DMA LIST (Complete Operation)
├── FRAME 1 (HDLC Frame)
│   ├── BLOCK 1 (Memory Buffer)    ← BlockEnd triggers here
│   ├── BLOCK 2 (Memory Buffer)    ← BlockEnd triggers here
│   └── BLOCK 3 (Memory Buffer)    ← BlockEnd + FrameEnd triggers here
├── FRAME 2 (HDLC Frame)  
│   ├── BLOCK 4 (Memory Buffer)    ← BlockEnd triggers here
│   └── BLOCK 5 (Memory Buffer)    ← BlockEnd + FrameEnd triggers here
└── END OF LIST                    ← ListEnd + TransmissionFinished triggers here
                                     ↓ (Hardware automatically sets)
                                   DMAModuleRequest triggers here
```

### What Each Bit Means

| Bit | Name | Meaning | Scope |
|-----|------|---------|-------|
| **8** | **BlockEnd** | **One memory buffer processed** | Single buffer |
| **9** | **FrameEnd** | **Complete HDLC frame transmitted/received** | Single HDLC frame |
| **10** | **ListEnd** | **Entire DMA descriptor list processed** | Entire descriptor list |
| **11** | **TransmissionFinished** | **Complete transmission sequence done** | Complete operation |

**CRITICAL HARDWARE RELATIONSHIP**: 
**"TransmissionFinished (bit 11) always gives a DMA Module Request (bit 4)"** - Hardware automatically sets DMAModuleRequest when TransmissionFinished is set.

## Block vs Frame vs List - Detailed Definitions

### Block (Memory Buffer)
- **Physical entity**: Contiguous memory region (e.g., 1024 bytes)
- **DMA unit**: Smallest unit of DMA transfer
- **Multiple blocks** can contain **parts of one HDLC frame**
- **One block** can contain **multiple small HDLC frames**

### Frame (HDLC Protocol Unit)  
- **Protocol entity**: Complete HDLC frame with flags, address, control, data, FCS
- **Variable length**: Can be 4 bytes (minimal) to several KB
- **Can span multiple blocks** if frame is large
- **Multiple frames** can fit in one block if frames are small

### List (DMA Operation)
- **Operation entity**: Complete set of buffers for one DMA operation
- **Contains**: All blocks needed for current transmission/reception batch
- **Managed by**: SINTRAN OS buffer allocation system
- **Completion**: Signals end of current DMA batch, may need new list

## SINTRAN Usage Analysis

### Are These Bits Used Differently for RX vs TX?

**Answer: NO - Both RX and TX use all three bits, but with different purposes.**

### Receiver Usage (RRTS Register)

#### BlockEnd (Bit 8) - Receiver
```assembly
IF A NBIT XBLDN THEN                 % Line 104537 - Check BlockEnd
   IF A = "ERB" THEN GO FAR ZSTARC FI % No blocks, enable receiver  
   GO FAR OUT1                       % Exit processing
FI
```

**Receiver BlockEnd Logic:**
- **When SET**: More receive buffers available for processing
- **When CLEAR**: No more filled receive buffers, stop processing
- **SINTRAN Action**: Continue processing if SET, stop if CLEAR

#### FrameEnd (Bit 9) - Receiver  
- **When SET**: Complete HDLC frame received and stored
- **Purpose**: Signals to protocol stack that full frame is ready
- **SINTRAN Action**: Pass frame to upper layer protocol handling

#### ListEnd (Bit 10) - Receiver
- **When SET**: All receive buffers in current list processed
- **Purpose**: Signals need for new receive buffer list
- **SINTRAN Action**: Allocate new receive buffer list, restart receiver

### Transmitter Usage (RTTS Register)

#### BlockEnd (Bit 8) - Transmitter
- **When SET**: One transmit buffer has been sent
- **Purpose**: Signals progress through transmit buffer list
- **SINTRAN Action**: Continue with next buffer or check completion

#### FrameEnd (Bit 9) - Transmitter
- **When SET**: Complete HDLC frame transmitted
- **Purpose**: Signals successful frame transmission to protocol
- **SINTRAN Action**: Update transmission statistics, continue or complete

#### ListEnd (Bit 10) - Transmitter
```assembly
BSKP ONE 10 DA                      % Line 103115 - Skip if ListEnd SET
```

**Transmitter ListEnd Logic:**
- **When SET**: Entire transmit list completed, no more buffers
- **When CLEAR**: More transmit buffers remain in list
- **SINTRAN Action**: Stop transmission if SET, continue if CLEAR

## Practical Examples

### Example 1: Large Frame Transmission (3 Blocks)
```
Frame Size: 3000 bytes
Block Size: 1024 bytes each

DMA Sequence:
1. Block1 sent (1024 bytes) → IRQ: BlockEnd=1, FrameEnd=0, ListEnd=0, TransmissionFinished=0
2. Block2 sent (1024 bytes) → IRQ: BlockEnd=1, FrameEnd=0, ListEnd=0, TransmissionFinished=0
3. Block3 sent (952 bytes)  → IRQ: BlockEnd=1, FrameEnd=1, ListEnd=1, TransmissionFinished=1
                                  → Hardware AUTO-SETS: DMAModuleRequest=1
```

### Example 2: Multiple Small Frames (1 Block Each)
```
3 Frames: 200, 300, 150 bytes
Block Size: 1024 bytes each

DMA Sequence:
1. Frame1 in Block1 → IRQ: BlockEnd=1, FrameEnd=1, ListEnd=0, TransmissionFinished=0
2. Frame2 in Block2 → IRQ: BlockEnd=1, FrameEnd=1, ListEnd=0, TransmissionFinished=0
3. Frame3 in Block3 → IRQ: BlockEnd=1, FrameEnd=1, ListEnd=1, TransmissionFinished=1
                       → Hardware AUTO-SETS: DMAModuleRequest=1
```

### Example 3: Multiple Frames in One Block
```
5 Small frames: 50 bytes each = 250 bytes total
Block Size: 1024 bytes

DMA Sequence:
1. All frames sent → IRQ: BlockEnd=1, FrameEnd=1, ListEnd=1, TransmissionFinished=1
                    → Hardware AUTO-SETS: DMAModuleRequest=1
   (FrameEnd represents "last frame completed")
```

## SINTRAN Constants Analysis

### BLDON vs XBLDN
```
BLDON = 000010 (octal) = 0x0008 = bit 3  
XBLDN = 000010 (octal) = 0x0008 = bit 3
```

**These are the SAME value (bit 3)** but used in **different contexts**:
- **BLDON**: SINTRAN software flag for "block done" processing
- **XBLDN**: Hardware status bit from DMA controller (should be bit 8, but constants show bit 3)

**This suggests a mapping discrepancy** - the hardware BlockEnd (bit 8) may be mapped to SINTRAN's internal bit 3 for processing.

## Implementation for Your Emulator

### Receiver Implementation
```csharp
public void OnDataReceived(byte[] frameData, bool moreBuffersAvailable)
{
    ReceiverStatusBits rrts = ReceiverStatusBits.DataAvailable;
    
    // Always set when buffer is filled
    rrts |= ReceiverStatusBits.BlockEnd;        // Bit 8
    
    // Set when complete HDLC frame received  
    if (isCompleteFrame)
    {
        rrts |= ReceiverStatusBits.FrameEnd;    // Bit 9
    }
    
    // Set when no more receive buffers in list
    if (!moreBuffersAvailable)
    {
        rrts |= ReceiverStatusBits.ListEnd;     // Bit 10
    }
    
    // Clear XBLDN (bit 8) in SINTRAN processing means "no more blocks"
    // So when BlockEnd is SET, SINTRAN sees "more blocks available"
}
```

### Transmitter Implementation
```csharp
public void OnTransmitProgress(bool blockComplete, bool frameComplete, bool listComplete)
{
    TransmitterStatusBits rtts = 0;
    
    if (blockComplete)
    {
        rtts |= TransmitterStatusBits.BlockEnd;          // Bit 8
    }
    
    if (frameComplete)  
    {
        rtts |= TransmitterStatusBits.FrameEnd;          // Bit 9
    }
    
    if (listComplete)
    {
        rtts |= TransmitterStatusBits.ListEnd;           // Bit 10
        rtts |= TransmitterStatusBits.TransmissionFinished; // Bit 11
        
        // CRITICAL: Hardware automatically sets DMAModuleRequest when TransmissionFinished is set
        rtts |= TransmitterStatusBits.DMAModuleRequest;  // Bit 4 (auto-set by hardware)
    }
    
    // Trigger interrupt only if DMAModuleRequest is set
    if ((rtts & TransmitterStatusBits.DMAModuleRequest) != 0)
    {
        TriggerInterrupt(12);
    }
}
```

## Key Insights

1. **All three bits are used by BOTH RX and TX** - your comments "Used by RX module only" are **incorrect**

2. **Different semantic meanings**:
   - **BlockEnd**: Memory management level
   - **FrameEnd**: Protocol level  
   - **ListEnd**: Operation level

3. **SINTRAN uses these for flow control**:
   - BlockEnd: Continue buffer processing
   - FrameEnd: Complete protocol frame handling
   - ListEnd: End current DMA operation, possibly start new one

4. **Progressive completion signaling** - not just "all done" but "what level is done"

These bits provide **granular completion status** allowing SINTRAN to manage complex multi-level DMA operations efficiently. Your emulator needs to set the appropriate bits based on what level of operation just completed.

## DETAILED FLOWCHARTS - DMA Status Bit Generation

### 1. DMA Status Bit Hierarchy Flow

```
┌═══════════════════════════════┐
║       DMA OPERATION           ║
║         INITIATED             ║
║                               ║
║  - Multiple descriptors       ║
║  - Multiple frames possible   ║
║  - Multiple blocks per frame  ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│     PROCESS FIRST BLOCK     │
│                             │
│ - DMA transfers buffer      │
│ - COM5025 processes data    │
│ - Block completion detected │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║      SET BLOCKEND (BIT 8)     ║
║                               ║
║  "One memory buffer           ║
║   completely processed"       ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│   CHECK FRAME COMPLETION    │
│                             │
│ Is this the last block      │
│ of the current HDLC frame?  │
└──────────┬──────────────────┘
           │
           ▼
    ┌──────┴──────┐
   MORE         FRAME
  BLOCKS        COMPLETE
    │              │
    ▼              ▼
┌─────────────────────────────┐   ┌═══════════════════════════════┐
│   CONTINUE WITH NEXT BLOCK  │   ║     SET FRAMEEND (BIT 9)      ║
│                             │   ║                               ║
│ - Process next buffer       │   ║  "Complete HDLC frame         ║
│ - More BlockEnd interrupts  │   ║   transmitted/received"       ║
│ - Frame still in progress   │   └═══════════════════════════════┘
└─────────────────────────────┘                   │
                                                  ▼
                                    ┌─────────────────────────────┐
                                    │   CHECK LIST COMPLETION     │
                                    │                             │
                                    │ Are there more frames       │
                                    │ in the DMA descriptor list? │
                                    └──────────┬──────────────────┘
                                               │
                                               ▼
                                        ┌──────┴──────┐
                                       MORE         LIST
                                      FRAMES       COMPLETE
                                        │              │
                                        ▼              ▼
                                    ┌─────────────────────────────┐   ┌═══════════════════════════════┐
                                    │  CONTINUE WITH NEXT FRAME   │   ║    SET LISTEND (BIT 10)       ║
                                    │                             │   ║                               ║
                                    │ - Process next frame        │   ║ "Entire DMA descriptor        ║
                                    │ - More FrameEnd interrupts  │   ║  list processed"              ║
                                    │ - List still in progress    │   └═══════════════════════════════┘
                                    └─────────────────────────────┘                   │
                                                                                      ▼
                                                                        ┌═══════════════════════════════┐
                                                                        ║ SET TRANSMISSIONFINISHED (11) ║
                                                                        ║                               ║
                                                                        ║ "Complete transmission        ║
                                                                        ║  sequence done"               ║
                                                                        └═══════════════════════════════┘
                                                                                      │
                                                                                      ▼
                                                                        ┌═══════════════════════════════┐
                                                                        ║   HARDWARE AUTO-SETS          ║
                                                                        ║   DMAMODULEREQUEST (BIT 4)    ║
                                                                        ║                               ║
                                                                        ║ "TransmissionFinished always  ║
                                                                        ║  gives DMA Module Request"    ║
                                                                        └═══════════════════════════════┘
                                                                                      │
                                                                                      ▼
                                                                        ┌─────────────────────────────┐
                                                                        │   TRIGGER INTERRUPT 12      │
                                                                        │                             │
                                                                        │ SINTRAN HOINT/HIINT         │
                                                                        │ processes completion        │
                                                                        └─────────────────────────────┘
```

### 2. Multi-Block Frame Status Generation Flow

```
┌═══════════════════════════════┐
║      LARGE HDLC FRAME         ║
║    (3000 bytes, 3 blocks)     ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│     PROCESS BLOCK 1         │
│   (1024 bytes, TSOM=1)      │
│                             │
│ - DMA transfers buffer 1    │
│ - COM5025 starts new frame  │
│ - Block 1 completion        │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║        INTERRUPT 1            ║
║                               ║
║ RTTS Status:                  ║
║ ├─ BlockEnd = 1               ║
║ ├─ FrameEnd = 0               ║
║ ├─ ListEnd = 0                ║
║ ├─ TransmissionFinished = 0   ║
║ └─ DMAModuleRequest = 0       ║
║                               ║
║ Frame continues...            ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│     PROCESS BLOCK 2         │
│   (1024 bytes, no flags)    │
│                             │
│ - DMA transfers buffer 2    │
│ - COM5025 continues frame   │
│ - Block 2 completion        │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║        INTERRUPT 2            ║
║                               ║
║ RTTS Status:                  ║
║ ├─ BlockEnd = 1               ║
║ ├─ FrameEnd = 0               ║
║ ├─ ListEnd = 0                ║
║ ├─ TransmissionFinished = 0   ║
║ └─ DMAModuleRequest = 0       ║
║                               ║
║ Frame still continues...      ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│     PROCESS BLOCK 3         │
│   (952 bytes, TEOM=1)       │
│                             │
│ - DMA transfers buffer 3    │
│ - COM5025 ends frame        │
│ - Block 3 completion        │
│ - Frame completion          │
│ - List completion           │
└──────────┬──────────────────┘
           │
           ▼
┌═══════════════════════════════┐
║        FINAL INTERRUPT        ║
║                               ║
║ RTTS Status:                  ║
║ ├─ BlockEnd = 1               ║
║ ├─ FrameEnd = 1               ║
║ ├─ ListEnd = 1                ║
║ ├─ TransmissionFinished = 1   ║
║ └─ DMAModuleRequest = 1 ◄─────║
║    (AUTO-SET BY HARDWARE)     ║
║                               ║
║ Complete operation done!      ║
└═══════════════════════════════┘
```

### 3. SINTRAN Status Bit Processing Flow

```
┌═══════════════════════════════┐
║     HARDWARE INTERRUPT        ║
║      (Level 12)               ║
└═══════════════════════════════┘
                │
                ▼
┌─────────────────────────────┐
│   READ HARDWARE STATUS      │
│                             │
│ T:=HDEV+RTTS; *EXR ST       │
│ A=:HASTAT                   │
└──────────┬──────────────────┘
           │
           ▼
┌─────────────────────────────┐
│    ANALYZE STATUS BITS      │
│                             │
│ ┌─ BlockEnd (8)             │
│ ├─ FrameEnd (9)             │
│ ├─ ListEnd (10)             │
│ ├─ TransmissionFinished(11) │
│ ├─ DMAModuleRequest (4)     │
│ ├─ SILFO Error (15)         │
│ └─ TXUND Error (1)          │
└──────────┬──────────────────┘
           │
           ▼
    ┌──────┴──────┐
   ERROR        SUCCESS
   CHECK         PATH
    │              │
    ▼              ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│     ERROR EVALUATION        │   │    SUCCESS PROCESSING       │
│                             │   │                             │
│ IF A/\"SILFO+TXUND" != 0:   │   │ IF A/\"SILFO+TXUND" == 0:   │
│                             │   │                             │
│ ┌─ SILFO (bit 15) = 1  OR   │   │ ┌─ SILFO (bit 15) = 0       │
│ └─ TXUND (bit 1) = 1        │   │ └─ TXUND (bit 1) = 0        │
└──────────┬──────────────────┘   └──────────┬──────────────────┘
           │                                 │
           ▼                                 ▼
┌─────────────────────────────┐   ┌─────────────────────────────┐
│      RETRY LOGIC            │   │   COMPLETION PROCESSING     │
│                             │   │                             │
│ CALL DRERR                  │   │ Check DMA status bits:      │
│ ├─ MIN RTDYN (retry count)  │   │                             │
│ ├─ GO SRDAT (retransmit)    │   │ ┌─ FrameEnd: Frame done     │
│ └─ or GIVE UP if exhausted  │   │ ├─ ListEnd: List done       │
└─────────────────────────────┘   │ └─ TransmissionFinished:    │
                                  │   Complete operation done   │
                                  └──────────┬──────────────────┘
                                             │
                                             ▼
                                  ┌─────────────────────────────┐
                                  │   CONTINUE OR COMPLETE      │
                                  │                             │
                                  │ IF more operations needed:  │
                                  │   Setup next DMA operation  │
                                  │ ELSE:                       │
                                  │   Return success to user    │
                                  └─────────────────────────────┘
```

## Updated Key Insights

1. **All four bits are used by BOTH RX and TX** with **identical hardware behavior** but **different semantic contexts**

2. **Hardware automatically links TransmissionFinished to DMAModuleRequest** - this is a **critical hardware relationship**

3. **Progressive status reporting** allows SINTRAN to:
   - **BlockEnd**: Track buffer-level progress
   - **FrameEnd**: Handle protocol-level completion  
   - **ListEnd**: Manage operation-level completion
   - **TransmissionFinished**: Detect sequence completion and trigger interrupts

4. **Emulator implementation** must respect the **hierarchical relationship** and **automatic hardware behaviors**