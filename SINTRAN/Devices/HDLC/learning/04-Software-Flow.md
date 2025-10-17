# SINTRAN HDLC Software Flow

**How SINTRAN software manages HDLC operations**

## Key Subroutines

### XSSDATA - Transmit Data
Main transmission entry point that validates, prepares DMA descriptors, and initiates transmission.

### HIINT - Receiver Interrupt Handler  
Processes incoming frames, validates status, and delivers data to applications.

### HOINT - Transmitter Interrupt Handler
Handles transmission completion, checks for errors, manages retries.

### PROCPKT - Process Packet
Extracts and validates received frame data.

### ZSTARC - Restart Receiver
Prepares receiver for next frame after successful reception.

## Activity State Management

The **ACTSW** (Activity Switch) variable controls all interrupt processing:
- **ACTSW = 0**: Device inactive, interrupts ignored
- **ACTSW = 1**: Device active, interrupts processed

## See Also

**Previous**: [Hardware Overview](03-Hardware-Overview.md)  
**Next**: [Register Reference](../reference/Register-Reference.md)  
**Deep Dive**: [XSSDATA Analysis](../deep-dives/Deep-Dive-XSSDATA.md)
