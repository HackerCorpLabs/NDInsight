# Getting Started with SINTRAN HDLC

**A gentle introduction to the SINTRAN III HDLC subsystem for newcomers**

---

## What is SINTRAN HDLC?

SINTRAN III's HDLC subsystem provides reliable, frame-based serial communication over synchronous X.21 interfaces. It enables:
- Terminal connections to remote systems
- Network communication via X.25 packet switching
- Reliable data transfer with error detection and recovery

Think of it as a sophisticated serial communication system that handles the complexities of framing, error checking, and flow control automatically.

---

## System Architecture Overview

\\\mermaid
graph TB
    User[User Application] -->|Send/Receive Data| SINTRAN[SINTRAN OS]
    SINTRAN -->|Configure/Control| DMA[DMA Controller]
    SINTRAN -->|Interrupt Handlers| CPU[CPU]
    DMA -->|Read/Write| Memory[System Memory]
    DMA -->|Transfer Data| COM5025[COM5025 Chip]
    COM5025 -->|HDLC Frames| X21[X.21 Interface]
    X21 -->|Physical Line| Remote[Remote System]
\\\

### Key Components Explained

#### 1. User Application
Your program that wants to send or receive data. It simply calls SINTRAN system functions - the complexity is hidden.

#### 2. SINTRAN OS
The operating system software that manages HDLC operations:
- Prepares data for transmission
- Processes received data
- Handles errors and retries
- Manages buffers

#### 3. DMA Controller
Hardware that transfers data between memory and the COM5025 chip without CPU intervention. This makes communication efficient.

#### 4. COM5025 Chip
The heart of the HDLC system. This specialized chip:
- Adds HDLC framing (flags, addresses, control)
- Calculates and checks CRC (error detection)
- Handles bit stuffing
- Manages timing

#### 5. X.21 Interface
The physical connection - wires and signals that connect to remote systems.

---

## How Data Flows (Simple Example)

### Sending Data

\\\
Step 1: User Application
"Send 'HELLO' to remote system"
        ↓
Step 2: SINTRAN Prepares
- Validates request
- Allocates buffer
- Sets up DMA descriptor
        ↓
Step 3: DMA Transfer
- Reads data from memory
- Writes to COM5025 chip
        ↓
Step 4: COM5025 Processing
- Adds opening FLAG (0x7E)
- Adds address and control bytes
- Transmits 'HELLO'
- Calculates and adds CRC
- Adds closing FLAG (0x7E)
        ↓
Step 5: Physical Transmission
Frame sent over X.21 line to remote system
\\\

**What Actually Goes on the Wire:**
\\\
[FLAG][ADDRESS][CONTROL][H][E][L][L][O][CRC1][CRC2][FLAG]
 0x7E    0x01     0x00    ... data ...  computed   0x7E
\\\

### Receiving Data

\\\
Step 1: Physical Reception
Frame arrives on X.21 line
        ↓
Step 2: COM5025 Processing
- Detects opening FLAG
- Strips flags and framing
- Checks CRC
- Signals "data ready"
        ↓
Step 3: DMA Transfer
- COM5025 writes to memory via DMA
- Interrupt generated when complete
        ↓
Step 4: SINTRAN Processing
- Interrupt handler validates frame
- Extracts user data
- Delivers to application
        ↓
Step 5: User Application
Receives 'HELLO' message
\\\

---

## The Four Layers

SINTRAN HDLC is organized in layers, like a stack:

\\\
┌─────────────────────────────┐
│  Application Layer          │ ← Your program
├─────────────────────────────┤
│  X.25 Packet Layer          │ ← Network routing
├─────────────────────────────┤
│  LAPB (Link) Layer          │ ← Reliable delivery
├─────────────────────────────┤
│  HDLC (Framing) Layer       │ ← Frame structure
├─────────────────────────────┤
│  X.21 (Physical) Layer      │ ← Wires and signals
└─────────────────────────────┘
\\\

Each layer has a specific job and talks only to the layers directly above and below it.

---

## Key Terminology (Simple Definitions)

- **HDLC**: The protocol that defines how data is framed for transmission
- **Frame**: A complete unit of data with headers, data, and error checking
- **DMA**: Hardware that moves data without bothering the CPU
- **Interrupt**: Hardware signal that tells the CPU "something happened!"
- **Buffer**: Memory area where data is stored temporarily
- **Register**: Small, fast storage location in hardware for control/status
- **COM5025**: The chip that does the actual HDLC work
- **X.21**: The physical connection standard
- **LAPB**: Higher-level protocol ensuring reliable delivery
- **X.25**: Network protocol for routing packets

---

## What Makes SINTRAN HDLC Special?

### 1. Direct Hardware Control
SINTRAN doesn't just configure the COM5025 chip - it directly embeds hardware control values in its DMA descriptors. This gives precise control over frame boundaries.

### 2. Efficient DMA Operation
Data transfers happen without CPU involvement, making the system fast and responsive.

### 3. Sophisticated Error Handling
Automatic retry mechanisms, timeout handling, and error recovery built into the system.

### 4. Interrupt-Driven
The CPU only gets involved when necessary (frame complete, error occurred), not for every byte.

---

## Common Use Cases

### 1. Terminal Connections
Connecting a terminal to a remote NORD computer system via synchronous modem.

### 2. PAD (Packet Assembler/Disassembler)
Converting between asynchronous terminal traffic and synchronous X.25 packets.

### 3. Network Communications
Connecting NORD systems together in a network using X.25.

### 4. Point-to-Point Links
Direct synchronous connections between two systems.

---

## What You'll Learn Next

Now that you understand the big picture, the next documents will explain:

1. **[Understanding Packets](02-Understanding-Packets.md)** - What's in an HDLC frame and how it works
2. **[Hardware Overview](03-Hardware-Overview.md)** - Details about COM5025, X.21, and registers
3. **[Software Flow](04-Software-Flow.md)** - How SINTRAN software manages everything

---

## Prerequisites

**To understand this documentation**, you should be familiar with:
- Basic computer architecture (CPU, memory, I/O)
- Hexadecimal and octal number systems
- Basic networking concepts (though we'll explain specifics)

**No prior HDLC knowledge required** - we'll teach you everything!

---

## See Also

**Next**: [Understanding Packets](02-Understanding-Packets.md) - Learn about HDLC frame structure  
**Reference**: [Quick Reference Card](../Quick-Reference-Card.md) - Key constants and patterns  
**Deep Dive**: [COM5025 Interface](../deep-dives/Deep-Dive-COM5025-Interface.md) - Advanced hardware details

---

**Ready to learn more?** Continue to [Understanding Packets](02-Understanding-Packets.md) →
