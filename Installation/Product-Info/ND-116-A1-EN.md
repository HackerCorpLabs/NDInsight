## Page 1

# ND COMPUTER SYSTEMS

```mermaid
flowchart TB
    A[ND-100 Bus BD 0-23] -->|0-23<br>MPX Address/Data Bus| B(Module Module Selection)
    A --> BD[BD 0-21]
    BD -->|16| C(Error Code)
    BD -->|16| D(Data)
    C -->|16| E(Error Detect and<br>Correction Network)
    D --> E
    E -->|22 bit<br>Data/ECC<br>from Memory| F(6 bit ECC<br>Generation)
    F -->|22 bit<br>Data/ECC<br>to Memory| G(256 Kbytes/22 bits or 512 Kbytes/22 bits<br>Memory Array)
    B -->|Memory<br>Address| G
    B -->|Thumbwheel| H(Lower Limit)
    H -->|Display| I(Upper Limit)
    B --> J(Add Module<br>Size)
    J --> I
    I --> K(Memory Timing/<br>Control and<br>Refresh Network)
```

## ND 116 MOS MEMORY, 256 Kbytes/22 bits
## ND 117 MOS MEMORY, 512 Kbytes/22 bits

### INTRODUCTION

The ND 116/117 MOS Memory Modules are used as primary storage in the ND-100 Computer System. Any combination of ND 116, ND 117 and the MOS memories ND 113 and ND 115 is allowed. ND 116/117 require one slot in the ND-100 rack.

### FEATURES

- 6 bit Error Correction Code increases data reliability
  - all single bit errors are corrected
  - all double bit errors are reported
- Modular and flexible design
- Requires one crate position
- Small physical dimensions
- Internal cycle control/timing
- Asynchronous operation
- Internal refresh address register
- Maintenance test features

### PRODUCT DESCRIPTION

The memory modules are designed according to user requirements, data reliability, high density and flexibility.

For each 16 bit word, 6 Error Correction Control (ECC) bits are generated. The 6 ECC bits guarantee that single bit errors are corrected and double bit errors are detected. All single bit errors are assigned an error code, making it possible to log all memory failures.

116/117-A1–6000-0182

---

## Page 2

# ND 116 and ND 117 Memory Modules

The ND 116 and ND 117 memory modules may be freely used in all address ranges up to 16 Mword. The address range for a particular module is defined either by module crate position or by thumbwheel selection. The address range may be set in 64 Kword increments.

For minimum interaction with other system parts, the modules contain refresh and memory cycle control logic.

# SPECIFICATION

| Specification         | Details                           |
|-----------------------|-----------------------------------|
| Data format           | 16 bit data                       |
|                       | 6 Error Correction                |
|                       | Control bits                      |
| Memory cycle times:   |                                   |
| Read Access time      | 270 ns                            |
| Write Access time     | 170 ns                            |
| Bus Hold time         | 500 ns                            |
| Power requirements    | +5V                               |
| Stand-by power        | 15 minutes                        |

Please add 30 ns if Error Correction must be performed.

---

[Logos and Contact Information]

- ND Norsk Data
- ND Comtec

[Photo: Company logos and contact information, including phone numbers and addresses for various locations.]

Note: NORSK DATA reserves the right to change specifications without notice.

---

