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
|:---:|:-----|:-------|
| 0 | **TSOM** | Transmit Start of Message - generate opening FLAG |
| 1 | **TEOM** | Transmit End of Message - generate closing FLAG + CRC |
| 2 | **TABORT** | Transmit Abort - send ABORT sequence |
| 3 | **TGA** | Transmit Go Ahead - send GA character |

## Common LKEY Values

### Single Frame Transmission

**FSERM = 0x1003** (002003 octal)
```
0001 0000 0000 0011
││││ ││            └─ Bits 0-1: TSOM(1) + TEOM(1) = complete frame
││││ └└─ Bits 8-9: 01 = Block to transmit
│└└└─ Bit 10: 1 = Legal key
└─ Bits 11-15: 0 = Standard control
```

### Multi-Block Frame Transmission

**First Block**: 0x0801 - TSOM only  
**Middle Blocks**: 0x0800 - No flags  
**Last Block**: 0x0802 - TEOM only

## DMA Operations

### Transmit Sequence (XSSDATA/XHMST)

1. Build DMA descriptor list with LKEY values
2. Write descriptor address to WDMA (IOX+15)
3. Write DMA command 0x0400 to WDCR (IOX+17)
4. Enable transmitter via WTTC (IOX+13) = 0x025C
5. Wait for Level 12 interrupt
6. Read RTTS to check transmission status

### Receive Sequence (XSSREC/XHRST)

1. Allocate receive buffer list with LKEY = 0x0400 (empty)
2. Write buffer address to WDMA
3. Write DMA command 0x0201 to WDCR
4. Enable receiver via WRTC (IOX+11)
5. Wait for Level 13 interrupt
6. Read RRTS to check reception status
7. Process data from buffers marked full (0x0600)

---

**See Also**:
- [01-HDLC-Hardware-Reference.md](01-HDLC-Hardware-Reference.md) - Hardware constants
- [02-HDLC-Register-Reference.md](02-HDLC-Register-Reference.md) - Register details
- [04-HDLC-Interrupt-Handlers.md](04-HDLC-Interrupt-Handlers.md) - Interrupt processing

**Document Version**: 2.0 (Consolidated)  
**Source Files**: Deep_Analysis_of_DMA_Transmit_XSSDATA.md, DMA_Send_Receive_Pseudocode.md, DMA_Bits_Detailed_Explanation.md, SINTRAN_DMA_Operations_Diagram.md, and 4 others

