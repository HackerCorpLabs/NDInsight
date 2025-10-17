# Understanding HDLC Packets

**How HDLC packets work in SINTRAN**

---

## HDLC Frame Structure

Every HDLC frame follows this structure:

```text
┌──────┬─────────┬─────────┬──────────┬────────┬──────┐
│ FLAG │ ADDRESS │ CONTROL │   DATA   │  FCS   │ FLAG │
│ 0x7E │ 1 byte  │ 1 byte  │ N bytes  │2 bytes │ 0x7E │
└──────┴─────────┴─────────┴──────────┴────────┴──────┘
```

### Field Details

1. **Opening FLAG (0x7E)**: Marks the start of a frame
2. **ADDRESS**: Identifies the destination/source
3. **CONTROL**: Frame type and sequence numbers
4. **DATA**: User payload (variable length)
5. **FCS (Frame Check Sequence)**: CRC-16-CCITT for error detection
6. **Closing FLAG (0x7E)**: Marks the end of a frame

---

## Frame Types

### I-Frames (Information)
- **Purpose**: Carry user data
- **Control**: 0x00-0x7F (bit 0 = 0)
- **Contains**: Sequence numbers for reliable delivery

### S-Frames (Supervisory)
- **Purpose**: Flow control and acknowledgments
- **Control**: 0x01, 0x05, 0x09, 0x0D
- **Types**: RR (Receive Ready), RNR (Receive Not Ready), REJ (Reject)

### U-Frames (Unnumbered)
- **Purpose**: Control functions
- **Control**: Various (bit pattern 11xxxxxx)
- **Examples**: SABM (Set Async Balanced Mode), DISC (Disconnect), UA (Unnumbered Ack)

---

## Packet Lifecycle

### Transmission

```mermaid
sequenceDiagram
    participant App as Application
    participant SINTRAN as SINTRAN OS
    participant DMA as DMA Controller
    participant COM as COM5025
    participant Line as Physical Line
    
    App->>SINTRAN: Send Data
    SINTRAN->>SINTRAN: Validate & Setup
    SINTRAN->>DMA: Configure Descriptor
    DMA->>COM: Transfer Data
    COM->>COM: Add HDLC Framing
    COM->>Line: Transmit Frame
    COM-->>SINTRAN: Interrupt (Complete)
    SINTRAN-->>App: Success/Failure
    
    box Blue Application Layer
    participant App
    end
    box Teal Operating System
    participant SINTRAN
    end
    box Orange DMA
    participant DMA
    end
    box Purple Hardware
    participant COM
    participant Line
    end
```

### Reception

```mermaid
sequenceDiagram
    participant Line as Physical Line
    participant COM as COM5025
    participant DMA as DMA Controller
    participant SINTRAN as SINTRAN OS
    participant App as Application
    
    Line->>COM: Receive Frame
    COM->>COM: Strip Framing & Check CRC
    COM->>DMA: Write Data to Memory
    DMA-->>SINTRAN: Interrupt (Data Ready)
    SINTRAN->>SINTRAN: Validate & Process
    SINTRAN->>App: Deliver Data
    
    box Purple Hardware
    participant Line
    participant COM
    end
    box Orange DMA
    participant DMA
    end
    box Teal Operating System
    participant SINTRAN
    end
    box Blue Application Layer
    participant App
    end
```

---

## See Also

**Previous**: [Getting Started](01-Getting-Started.md)  
**Next**: [Hardware Overview](03-Hardware-Overview.md)  
**Reference**: [Protocol Reference](../reference/Protocol-Reference.md)

