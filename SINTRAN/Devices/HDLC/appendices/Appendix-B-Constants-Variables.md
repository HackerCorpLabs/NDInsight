# Appendix B: Constants and Variables Reference

HDLC_Constants_Analysis.md, Critical_Bit_Usage_Analysis.md



---




## Content from: 03-HDLC-DMA-Operations.md

# HDLC DMA Operations

**DMA descriptor structure and buffer management for SINTRAN HDLC**

---

## DMA Descriptor LKEY Field Structure

The LKEY field (16 bits) in DMA descriptors controls both buffer status and COM5025 chip operations:

```
Bits 15-11: Extended control/reserved
Bit  10:    Legal Key flag (must be 1 for valid descriptor)
Bits 9-8:   Block status (Empty/Full/ToTransmit/Transmitted)
Bits 7-0:   COM5025 register value (direct chip control)
```

### Block Status Encoding (Bits 10-8)

| Bits | Octal | Hex | Status | Usage |
|:----:|:-----:|:---:|:-------|:------|
| 010 | 1000 | 0x0400 | **Empty Receiver Block** | Available for incoming data |
| 011 | 1400 | 0x0600 | **Full Receiver Block** | Contains received data |
| 100 | 2000 | 0x0800 | **Block to Transmit** | Ready for transmission |
| 101 | 2400 | 0x0A00 | **Transmitted Block** | Transmission complete |
| 110 | 3000 | 0x0C00 | **New List Pointer** | Chain to next list |

### COM5025 Control Bits (Bits 7-0)

| Bit | Name | Effect |
|:
