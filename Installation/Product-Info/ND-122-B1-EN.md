## Page 1

# ND COMPUTER SYSTEMS

```mermaid
flowchart TD
    A[Internal Timing and Control] -->|MR 0-14| B
    A --> C
    C[Module Selection] -->|3| Memory
    Memory -->|MR 15-17| D
    Memory -->|16| Memory Data Bus
    subgraph Memory
        D[18]
    end
    B[32 Kbytes/21 bits Y-BLOCK] -->|14| E
    E[32 Kbytes/21 bits X-BLOCK] -->|14| B
    E -->|15| F
    F --> Memory
    B -->|MRO| E
```

## ND 122 MOS MEMORY, 128 Kbytes/21 bits
## ND 124 MOS MEMORY, 256 Kbytes/21 bits
## ND 127 MOS MEMORY, 768 Kbytes/21 bits
## ND 156 MOS MEMORY, 64 Kbytes/21 bits

### INTRODUCTION

The ND 156 MOS Memory Module 64 Kbytes/21 bits is used as a fast primary storage in the NORD-10/S local memory, the NORD-10/S, the ND-100 and the ND-500 Multiport Memory System, and as NORD-50 memory. The ND 156 requires one slot in the memory system.

ND 122 equals 2 x ND 156, upgrading one CPU.  
ND 124 equals 4 x ND 156, upgrading one CPU.  
ND 127 equals 12 x ND 156, upgrading one CPU.

### FEATURES

- Modular and flexible design
- Excellent data reliability using single bit error correction as standard
- Low power requirements
- Small physical dimensions
- Internal pipeline feature for increased bandwidth
- Asynchronous operation
- External refresh
- Maintenance test features

122/124/127/156-B1-3000-0881

---

## Page 2

# Product Description

The memory modules can adjust to various requirements.

The modules have asynchronous operation where the control lines are activated by an internal timing and control network.

The micro instruction codes in ND 156 are organized in two groups, the X-and the Y-block, each holding 32 Kbytes/21 bits. Address bus bit 0 will select between the two groups. During consecutive addressing, block selection will toggle and an overlap (pipeline) effect is obtained. This will in average result in a greater bandwidth.

# Specification

| Specification                                | Value               |
|----------------------------------------------|---------------------|
| Data format                                  | 16 data bits        |
|                                              | 5 control code bits |
| Read Access time (Request to Data Ready)     | 280 ns              |
| Write Access time                            | 130 ns              |
| Address bus hold time for Read and Write access (Request to Address Ready) | 130 ns              |
| Cycle time                                   | ≥ 380 ns            |
| Precharge time                               | 150 ns              |

Note: NORSK DATA reserves the right to change specifications without given notice!

[Various contact details for Norsk Data and ND Comtec]

---

