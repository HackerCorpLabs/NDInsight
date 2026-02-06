# ND-500 Addressing Modes Reference

This document provides a comprehensive reference of all addressing modes for the NORD-500 CPU, covering both assembler syntax and binary encoding for disassembly purposes.

**Sources:**
- ND-60.113.02 EN - NORD-500 Assembler Reference Manual
- ND-05.009.4 EN - ND-500 Reference Manual

---

## Table of Contents

1. [Overview](#overview)
2. [Operand Specifier Structure](#operand-specifier-structure)
3. [Data Part Length Specifiers](#data-part-length-specifiers)
4. [Address Code Summary Table](#address-code-summary-table)
5. [Detailed Addressing Modes](#detailed-addressing-modes)
   - [Local Addressing](#1-local-addressing)
   - [Local Post-Indexed Addressing](#2-local-post-indexed-addressing)
   - [Local Indirect Addressing](#3-local-indirect-addressing)
   - [Local Indirect Post-Indexed Addressing](#4-local-indirect-post-indexed-addressing)
   - [Record Addressing](#5-record-addressing)
   - [Pre-Indexed Addressing](#6-pre-indexed-addressing)
   - [Absolute Addressing](#7-absolute-addressing)
   - [Absolute Post-Indexed Addressing](#8-absolute-post-indexed-addressing)
   - [Constant Operand Addressing](#9-constant-operand-addressing)
   - [Register Addressing](#10-register-addressing)
   - [Descriptor Addressing](#11-descriptor-addressing)
   - [Alternative Addressing](#12-alternative-addressing)
6. [Direct Operands](#direct-operands)
7. [Address Code Encoding Rules](#address-code-encoding-rules)
8. [Register Names](#register-names)
9. [Post-Index Scaling Factors](#post-index-scaling-factors)

---

## Overview

The NORD-500 CPU uses a rich set of addressing modes to access operands in memory and registers. An instruction consists of an instruction code followed by zero or more operand specifiers. Each operand specifier can be 1 to 9 bytes long.

Operand specifiers are divided into two main categories:
- **General Operands**: Accessed via an address code (most common)
- **Direct Operands**: Found immediately after the instruction code (used in specific instructions)

---

## Operand Specifier Structure

```
┌──────────────┬──────────────┬─────────────┐
│  Prefix(es)  │ Address Code │  Data Part  │
│  (0-2 bytes) │(2 bits/1 byte)│ (0-8 bytes) │
└──────────────┴──────────────┴─────────────┘
```

**Total length:** 1 to 9 bytes

### Components:

| Component | Size | Description |
|-----------|------|-------------|
| Prefix | 0-2 bytes | Optional ALT or DESC prefix |
| Address Code | 2 bits or 1 byte | Specifies addressing mode and data part length |
| Data Part | 0-8 bytes | Displacement, address, or constant value |

---

## Data Part Length Specifiers

The assembler uses length specifiers to force a particular storage format:

| Specifier | Name | Size | Description |
|-----------|------|------|-------------|
| `:S` | Short | 6 bits | Short format (displacement unit = 4 bytes) |
| `:B` | Byte | 8 bits (1 byte) | Byte displacement/constant |
| `:H` | Halfword | 16 bits (2 bytes) | Halfword displacement/constant |
| `:W` | Word | 32 bits (4 bytes) | Word displacement/constant/address |
| `:F` | Float | 32 bits (4 bytes) | Single precision floating-point constant |
| `:D` | Double | 64 bits (8 bytes) | Double precision floating-point constant |

**Notes:**
- `:W` and `:F` use the same address code (317B/0CFH) for constants
- Short displacements (`:S`) use word units (multiply by 4 for byte address)
- Byte, halfword, and word displacements are always in byte units
- Displacements are treated as **unsigned** values

---

## Address Code Summary Table

### Complete Address Code Reference

| Address Mode | Size | Assembler Syntax | Octal Code | Hex Code | Effective Address |
|-------------|------|------------------|------------|----------|-------------------|
| **LOCAL** | :S | `B.<displ>` | 1dd | 040H+xx | ea = (B) + d×4 |
| **LOCAL** | :B | `B.<displ>:B` | 301B | 0C1H | ea = (B) + d |
| **LOCAL** | :H | `B.<displ>:H` | 302B | 0C2H | ea = (B) + d |
| **LOCAL** | :W | `B.<displ>:W` | 303B | 0C3H | ea = (B) + d |
| **LOCAL P.I.** | :B | `B.<displ>:B(Rn)` | 324B+y | 0D4H+y | ea = (B) + d + p×(Rn) |
| **LOCAL P.I.** | :H | `B.<displ>:H(Rn)` | 330B+y | 0D8H+y | ea = (B) + d + p×(Rn) |
| **LOCAL P.I.** | :W | `B.<displ>:W(Rn)` | 334B+y | 0DCH+y | ea = (B) + d + p×(Rn) |
| **LOCAL IND** | :B | `IND(B.<displ>:B)` | 305B | 0C5H | ea = ((B) + d) |
| **LOCAL IND** | :H | `IND(B.<displ>:H)` | 306B | 0C6H | ea = ((B) + d) |
| **LOCAL IND** | :W | `IND(B.<displ>:W)` | 307B | 0C7H | ea = ((B) + d) |
| **LOCAL IND P.I.** | :B | `IND(B.<displ>:B)(Rn)` | 344B+y | 0E4H+y | ea = ((B) + d) + p×(Rn) |
| **LOCAL IND P.I.** | :H | `IND(B.<displ>:H)(Rn)` | 350B+y | 0E8H+y | ea = ((B) + d) + p×(Rn) |
| **LOCAL IND P.I.** | :W | `IND(B.<displ>:W)(Rn)` | 354B+y | 0ECH+y | ea = ((B) + d) + p×(Rn) |
| **RECORD** | :S | `R.<displ>` | 2dd | 080H+xx | ea = (R) + d×4 |
| **RECORD** | :B | `R.<displ>:B` | 311B | 0C9H | ea = (R) + d |
| **RECORD** | :H | `R.<displ>:H` | 312B | 0CAH | ea = (R) + d |
| **RECORD** | :W | `R.<displ>:W` | 313B | 0CBH | ea = (R) + d |
| **PRE-INDEXED** | :B | `Rn.<displ>:B` | 364B+y | 0F4H+y | ea = (Rn) + d |
| **PRE-INDEXED** | :H | `Rn.<displ>:H` | 370B+y | 0F8H+y | ea = (Rn) + d |
| **PRE-INDEXED** | :W | `Rn.<displ>:W` | 374B+y | 0FCH+y | ea = (Rn) + d |
| **ABSOLUTE** | :W | `<label>` | 304B | 0C4H | ea = a |
| **ABSOLUTE P.I.** | :W | `<label>(Rn)` | 340B+y | 0E0H+y | ea = a + p×(Rn) |
| **CONSTANT** | :S | `<const>:S` | 0cc | 000H+xx | op = c |
| **CONSTANT** | :B | `<const>:B` | 315B | 0CDH | op = c |
| **CONSTANT** | :H | `<const>:H` | 316B | 0CEH | op = c |
| **CONSTANT** | :W | `<const>:W` | 317B | 0CFH | op = c |
| **CONSTANT** | :F | `<const>:F` | 317B | 0CFH | op = c |
| **CONSTANT** | :D | `<const>:D` | 314B | 0CCH | op = c |
| **REGISTER** | - | `Rn` | 320B+y | 0D0H+y | op = (Rn) |
| **DESCRIPTOR** | - | `DESC(<op>)(Rn)` | 360B+y | 0F0H+y | ea = A + p×(Rn) |
| **ALTERNATIVE** | - | `ALT(<op>)` | 310B | 0C8H | (prefix) |

### Legend

| Symbol | Meaning |
|--------|---------|
| `ea` | Effective address |
| `op` | Operand value |
| `(X)` | Contents of X |
| `d` | Displacement value |
| `a` | Absolute address |
| `c` | Constant value |
| `p` | Post-index scaling factor |
| `B` | Base register (local base) |
| `R` | Record register |
| `Rn` | General register R1-R4 |
| `y` | Register encoding: 0=R1, 1=R2, 2=R3, 3=R4 |
| `xx` | 6-bit value (0-63 decimal, 0-77 octal) |
| `dd` | 6-bit displacement |
| `cc` | 6-bit constant |
| `P.I.` | Post-Indexed |
| `IND` | Indirect |

---

## Detailed Addressing Modes

### 1. Local Addressing

**Purpose:** Address relative to the Base register (B), typically used for local variables.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `B.<displ>` | Assembler selects optimal format |
| `B.<displ>:S` | Force short displacement (6 bits, word units) |
| `B.<displ>:B` | Force byte displacement |
| `B.<displ>:H` | Force halfword displacement |
| `B.<displ>:W` | Force word displacement |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Short | 100B + dd (1dd) | 040H + xx | None (6 bits in code) |
| Byte | 301B | 0C1H | 1 byte displacement |
| Halfword | 302B | 0C2H | 2 bytes displacement |
| Word | 303B | 0C3H | 4 bytes displacement |

#### Effective Address Calculation

```
Short:    ea = (B) + d × 4      (d = 0..63 words = 0..252 bytes)
Byte:     ea = (B) + d          (d = 0..255 bytes)
Halfword: ea = (B) + d          (d = 0..65535 bytes)
Word:     ea = (B) + d          (d = 0..2³²-1 bytes)
```

#### Example

```
Assembly:  BY1 =: B.400B
           (Store byte register 1 to local offset 400 octal)

Binary (Octal):
  034B      ; Instruction: BY1 =: (store byte)
  302B      ; Address code: Local, halfword displacement
  001B      ; Displacement low byte
  000B      ; Displacement high byte

If B = 1000B, then:
  ea = 1000B + 400B = 1400B
```

---

### 2. Local Post-Indexed Addressing

**Purpose:** Access array elements relative to Base register with index scaling.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `B.<displ>(Rn)` | Assembler selects format |
| `B.<displ>:B(Rn)` | Byte displacement with post-index |
| `B.<displ>:H(Rn)` | Halfword displacement with post-index |
| `B.<displ>:W(Rn)` | Word displacement with post-index |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Byte | 324B + y | 0D4H + y | 1 byte displacement |
| Halfword | 330B + y | 0D8H + y | 2 bytes displacement |
| Word | 334B + y | 0DCH + y | 4 bytes displacement |

Where `y` = 0 (R1), 1 (R2), 2 (R3), or 3 (R4)

#### Effective Address Calculation

```
ea = (B) + d + p × (Rn)

Where p = post-index scaling factor based on data type
```

#### Example

```
Assembly:  BI2 := B.170:H(R3)
           (Load bit register 2 from indexed local)

Binary (Octal):
  176005B   ; Instruction: BI2 :=
  332B      ; Address code: Local P.I., halfword, R3 (330B + 2)
  000B      ; Displacement low
  170B      ; Displacement high

If B = 10000B, R3 = 400B, data type = BI (p = 1/8):
  ea = 10000B + 170B + 400B/10B = 10230B
```

---

### 3. Local Indirect Addressing

**Purpose:** Access data through a pointer stored at a local offset. Common for subroutine arguments.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `IND(B.<displ>)` | Assembler selects format |
| `IND(B.<displ>:B)` | Byte displacement indirect |
| `IND(B.<displ>:H)` | Halfword displacement indirect |
| `IND(B.<displ>:W)` | Word displacement indirect |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Byte | 305B | 0C5H | 1 byte displacement |
| Halfword | 306B | 0C6H | 2 bytes displacement |
| Word | 307B | 0C7H | 4 bytes displacement |

#### Effective Address Calculation

```
ea = ((B) + d)

The value at address (B)+d is used as the final address.
```

#### Example

```
Assembly:  F4 + IND(B.120B:B)
           (Add to float register 4 from indirect local)

Binary (Octal):
  133B      ; Instruction: F4 +
  305B      ; Address code: Local indirect, byte displacement
  120B      ; Displacement

If B = 400B, and memory[520B] = 1000B:
  Intermediate address = 400B + 120B = 520B
  ea = (520B) = 1000B
```

---

### 4. Local Indirect Post-Indexed Addressing

**Purpose:** Access array elements through a pointer with index scaling. Used for subroutine array arguments.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `IND(B.<displ>)(Rn)` | Assembler selects format |
| `IND(B.<displ>:B)(Rn)` | Byte displacement indirect post-indexed |
| `IND(B.<displ>:H)(Rn)` | Halfword displacement indirect post-indexed |
| `IND(B.<displ>:W)(Rn)` | Word displacement indirect post-indexed |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Byte | 344B + y | 0E4H + y | 1 byte displacement |
| Halfword | 350B + y | 0E8H + y | 2 bytes displacement |
| Word | 354B + y | 0ECH + y | 4 bytes displacement |

#### Effective Address Calculation

```
ea = ((B) + d) + p × (Rn)
```

#### Example

```
Assembly:  H4 := IND(B.60B)(R4)
           (Load halfword register 4 from indirect indexed)

Binary (Octal):
  013B      ; Instruction: H4 :=
  347B      ; Address code: Local indirect P.I., byte disp, R4 (344B + 3)
  060B      ; Displacement

If B = 600B, memory[660B] = 2000B, R4 = 150B, data type = H (p = 2):
  Intermediate = (600B + 60B) = (660B) = 2000B
  ea = 2000B + 2 × 150B = 2000B + 320B = 2320B
```

---

### 5. Record Addressing

**Purpose:** Address relative to the Record register (R), used for accessing fields in data structures/records.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `R.<displ>` | Assembler selects format |
| `R.<displ>:S` | Short displacement (6 bits, word units) |
| `R.<displ>:B` | Byte displacement |
| `R.<displ>:H` | Halfword displacement |
| `R.<displ>:W` | Word displacement |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Short | 200B + dd (2dd) | 080H + xx | None (6 bits in code) |
| Byte | 311B | 0C9H | 1 byte displacement |
| Halfword | 312B | 0CAH | 2 bytes displacement |
| Word | 313B | 0CBH | 4 bytes displacement |

#### Effective Address Calculation

```
Short:    ea = (R) + d × 4
Others:   ea = (R) + d
```

#### Example

```
Assembly:  BY1 =: R.400B:H
           (Store byte to record offset)

Binary (Octal):
  034B      ; Instruction: BY1 =:
  312B      ; Address code: Record, halfword displacement
  001B      ; Displacement low
  000B      ; Displacement high

If R = 1000B:
  ea = 1000B + 400B = 1400B
```

---

### 6. Pre-Indexed Addressing

**Purpose:** Address relative to a general register. Useful for pointer-based access.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `Rn.<displ>` | Assembler selects format |
| `Rn.<displ>:B` | Byte displacement from Rn |
| `Rn.<displ>:H` | Halfword displacement from Rn |
| `Rn.<displ>:W` | Word displacement from Rn |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Byte | 364B + y | 0F4H + y | 1 byte displacement |
| Halfword | 370B + y | 0F8H + y | 2 bytes displacement |
| Word | 374B + y | 0FCH + y | 4 bytes displacement |

#### Effective Address Calculation

```
ea = (Rn) + d
```

#### Example

```
Assembly:  D2 * R3.400B
           (Multiply double register 2 by value at R3+offset)

Binary (Octal):
  165B      ; Instruction: D2 *
  372B      ; Address code: Pre-indexed, halfword, R3 (370B + 2)
  001B      ; Displacement low
  000B      ; Displacement high

If R3 = 10000B:
  ea = 10000B + 400B = 10400B
```

---

### 7. Absolute Addressing

**Purpose:** Direct access to a specific memory address.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `<label>` | Absolute address (always 4 bytes) |
| `<label>:W` | Explicit word format |

#### Binary Encoding

| Address Code (Octal) | Address Code (Hex) | Data Part |
|---------------------|-------------------|-----------|
| 304B | 0C4H | 4 bytes (absolute address) |

#### Effective Address Calculation

```
ea = a (the 4-byte address value)
```

#### Example

```
Assembly:  D2 * 2002044522B
           (Multiply by value at absolute address)

Binary (Octal):
  165B      ; Instruction: D2 *
  304B      ; Address code: Absolute
  020B      ; Address byte 0
  010B      ; Address byte 1
  111B      ; Address byte 2
  122B      ; Address byte 3

ea = 2002044522B
```

---

### 8. Absolute Post-Indexed Addressing

**Purpose:** Access array elements at an absolute base address with index scaling.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `<label>(Rn)` | Absolute address with post-index |

#### Binary Encoding

| Address Code (Octal) | Address Code (Hex) | Data Part |
|---------------------|-------------------|-----------|
| 340B + y | 0E0H + y | 4 bytes (absolute address) |

#### Effective Address Calculation

```
ea = a + p × (Rn)
```

#### Example

```
Assembly:  W1 := 2000B(R2)
           (Load word from indexed absolute)

Binary (Octal):
  020B      ; Instruction: W1 :=
  341B      ; Address code: Absolute P.I., R2 (340B + 1)
  000B      ; Address byte 0
  000B      ; Address byte 1
  004B      ; Address byte 2 (2000B)
  000B      ; Address byte 3

If R2 = 200B, data type = W (p = 4):
  ea = 2000B + 4 × 200B = 2000B + 1000B = 3000B
```

---

### 9. Constant Operand Addressing

**Purpose:** Embed constant values directly in the instruction stream.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `<constant>` | Assembler selects optimal format |
| `<constant>:S` | Short constant (6 bits, 0-63) |
| `<constant>:B` | Byte constant |
| `<constant>:H` | Halfword constant |
| `<constant>:W` | Word constant |
| `<constant>:F` | Float constant (same code as :W) |
| `<constant>:D` | Double float constant |

#### Binary Encoding

| Size | Address Code (Octal) | Address Code (Hex) | Data Part |
|------|---------------------|-------------------|-----------|
| Short | 000B + cc (0cc) | 000H + xx | None (6 bits in code) |
| Byte | 315B | 0CDH | 1 byte constant |
| Halfword | 316B | 0CEH | 2 bytes constant |
| Word | 317B | 0CFH | 4 bytes constant |
| Float | 317B | 0CFH | 4 bytes constant |
| Double | 314B | 0CCH | 8 bytes constant |

#### Constant Conversion Table

When the instruction data type differs from the constant size:

| Instruction Type | :S | :B | :H | :W | :F | :D |
|-----------------|----|----|----|----|----|----|
| BI | BZ | IOS | IOS | IOS | IOS | IOS |
| BY | SX | NC | IOS | IOS | IOS | IOS |
| H | SX | SX | NC | IOS | IOS | IOS |
| W | SX | SX | SX | NC | NC | IOS |
| F | CF | CF | CF | NC | NC | IOS |
| D | CDF | CDF | CDF | 32LZ | 32LZ | NC |

**Legend:**
- **NC**: No conversion required
- **SX**: Sign extended
- **CF**: Convert to float
- **CDF**: Convert to double float
- **BZ**: Bit zero of constant
- **32LZ**: 32 least significant bits zero-filled
- **IOS**: Illegal operand specifier trap

**Note:** Constants are illegal for write instructions (store, swap) and as subroutine arguments.

#### Example

```
Assembly:  W1 + 150B:B
           (Add byte constant to word register)

Binary (Octal):
  124B      ; Instruction: W1 +
  315B      ; Address code: Constant, byte
  150B      ; Constant value (104 decimal)
```

---

### 10. Register Addressing

**Purpose:** Use a register directly as the operand.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `Rn` | Register n (R1-R4) as operand |
| `Wn` | Word register |
| `Hn` | Halfword register |
| `BYn` | Byte register |
| `BIn` | Bit register |
| `Fn` | Float register |
| `Dn` | Double float register |

#### Binary Encoding

| Address Code (Octal) | Address Code (Hex) |
|---------------------|-------------------|
| 320B + y | 0D0H + y |

Where y = 0 (R1), 1 (R2), 2 (R3), 3 (R4)

#### Usage Notes

- Register operand illegal in CALL/CALLG argument lists
- Illegal as BMOVE destination
- Illegal for TSET, RDUS instructions
- The instruction data type determines register interpretation (Wn, Fn, Dn, etc.)

---

### 11. Descriptor Addressing

**Purpose:** Access array elements through a descriptor (length + base address), with automatic bounds checking and index increment.

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `DESC(<operand>)(Rn)` | Access via descriptor with index register |

Where `<operand>` can be any general operand except ALT, constant, or register.

#### Binary Encoding

| Address Code (Octal) | Address Code (Hex) | Followed By |
|---------------------|-------------------|-------------|
| 360B + y | 0F0H + y | `<operand>` specifier |

#### Descriptor Format in Memory

```
┌────────────────────────────────────┐
│ Word 0: Length (number of elements)│
├────────────────────────────────────┤
│ Word 1: Start Address (A)          │
└────────────────────────────────────┘
```

#### Effective Address Calculation

```
ea = A + p × (Rn)

Where A = contents of second word of descriptor
```

#### Behavior

```
if (Rn)+1 > descriptor.length then
    descriptor range trap condition
endif
if (Rn)+1 >= descriptor.length then
    1 → status.K  (set K flag)
endif
if not descriptor range trap then
    perform addressing with Rn as post-index
    if data access then
        (Rn)+1 → Rn  (auto-increment index)
    endif
endif
```

#### Example

```
Assembly:  H2 := DESC(B.100B)(R3)
           (Load halfword from descriptor-addressed array)

Binary (Octal):
  011B      ; Instruction: H2 :=
  362B      ; Address code: Descriptor, R3 (360B + 2)
  301B      ; Nested operand: Local, byte displacement
  100B      ; Displacement to descriptor

If B = 400B, descriptor at 500B contains [100B, 2000B], R3 = 50B:
  Descriptor address = 400B + 100B = 500B
  Length = 100B (64 elements)
  A = 2000B
  ea = 2000B + 2 × 50B = 2000B + 120B = 2120B
  After access: R3 = 51B
```

---

### 12. Alternative Addressing

**Purpose:** Access operands in an alternative domain (for inter-domain communication).

#### Assembly Notation

| Syntax | Description |
|--------|-------------|
| `ALT(<operand>)` | Access operand in alternative domain |

Where `<operand>` can be any operand except ALT, register, or constant.

#### Binary Encoding

| Address Code (Octal) | Address Code (Hex) | Followed By |
|---------------------|-------------------|-------------|
| 310B | 0C8H | `<operand>` specifier |

#### Usage Notes

- Only the final memory access goes to the alternative domain
- Indirect addresses and descriptors are read from the current domain
- Requires parameter access to the referenced segment
- Can combine with DESC: `ALT(DESC(<operand>)(Rn))`

---

## Direct Operands

Direct operands have no address code; the data part immediately follows the instruction code.

### Displacement Addressing (Program Relative)

**Used by:** GO, IF \<rel\> GO, LOOP, LOOPI, LOOPD

| Size | Instruction Variants | Description |
|------|---------------------|-------------|
| Byte | B IF ... GO | Signed 8-bit displacement |
| Halfword | H IF ... GO | Signed 16-bit displacement |
| Word | W GO only | Signed 32-bit displacement |

**Calculation:**
```
New P = (P) + displacement
```
Displacement is relative to the first byte of the current instruction.

### Absolute Program Addressing

**Used by:** CALL

```
CALL <address>
```
Four bytes following instruction code contain the absolute program address.

### Absolute Data Addressing

**Used by:** INIT, ENTM, ENTF, ENTFN

```
INIT <stack_address>, <frame_size>, <total_demand>
ENTM <stack_bottom>
ENTF <data_area_address>
```

---

## Address Code Encoding Rules

### Short Codes (2-bit Address Code)

When bits 7-6 of the first byte are NOT both 1:

| Bit Pattern | Octal Range | Mode | Data Part |
|-------------|-------------|------|-----------|
| 00xxxxxx | 000B-077B | Short Constant | 6 bits in code |
| 01xxxxxx | 100B-177B | Local Short | 6 bits in code |
| 10xxxxxx | 200B-277B | Record Short | 6 bits in code |

The 6 low bits contain the value directly.

### Full Byte Codes

When bits 7-6 are both 1 (11xxxxxx, 3xxB range), a full byte specifies the address code, followed by a variable-length data part.

### Register Encoding

For address codes that include a register reference (+y):

| y Value | Register |
|---------|----------|
| 0 | R1 |
| 1 | R2 |
| 2 | R3 |
| 3 | R4 |

---

## Register Names

### Data Type Specific Names

| Data Type | Registers | Description |
|-----------|-----------|-------------|
| BI | BI1, BI2, BI3, BI4 | Bit (1 bit) |
| BY | BY1, BY2, BY3, BY4 | Byte (8 bits) |
| H | H1, H2, H3, H4 | Halfword (16 bits) |
| W | W1, W2, W3, W4 | Word (32 bits) |
| F | F1, F2, F3, F4 | Float (32 bits) |
| D | D1, D2, D3, D4 | Double float (64 bits) |
| R | R1, R2, R3, R4 | Generic register |

### Special Registers

| Register | Description |
|----------|-------------|
| B | Base register (local base) |
| R | Record register |

---

## Post-Index Scaling Factors

The post-index scaling factor (p) multiplies the index register value based on data type:

| Data Type | Scaling Factor (p) | Description |
|-----------|-------------------|-------------|
| BI (Bit) | 1/8 | 1 bit per element |
| BY (Byte) | 1 | 1 byte per element |
| H (Halfword) | 2 | 2 bytes per element |
| W (Word) | 4 | 4 bytes per element |
| F (Float) | 4 | 4 bytes per element |
| D (Double) | 8 | 8 bytes per element |
| Descriptor | 8 | 8 bytes per descriptor |

**Example:** For halfword data with R3 = 10:
```
Physical index = 2 × 10 = 20 bytes
```

---

## Quick Reference: Address Code Decoding

### Decoding Algorithm

```
1. Read first byte of operand specifier
2. Check bits 7-6:
   - 00: Short constant (0cc), value = bits 5-0
   - 01: Local short (1dd), displacement = bits 5-0 × 4
   - 10: Record short (2dd), displacement = bits 5-0 × 4
   - 11: Full address code, see table below

3. For 3xx codes, decode the full byte:
   - 300B: (unused/reserved)
   - 301B-303B: Local (B/H/W displacement)
   - 304B: Absolute
   - 305B-307B: Local Indirect (B/H/W displacement)
   - 310B: Alternative (prefix)
   - 311B-313B: Record (B/H/W displacement)
   - 314B: Constant Double
   - 315B-317B: Constant (B/H/W)
   - 320B-323B: Register (R1-R4)
   - 324B-327B: Local P.I. byte (R1-R4)
   - 330B-333B: Local P.I. halfword (R1-R4)
   - 334B-337B: Local P.I. word (R1-R4)
   - 340B-343B: Absolute P.I. (R1-R4)
   - 344B-347B: Local Indirect P.I. byte (R1-R4)
   - 350B-353B: Local Indirect P.I. halfword (R1-R4)
   - 354B-357B: Local Indirect P.I. word (R1-R4)
   - 360B-363B: Descriptor (R1-R4)
   - 364B-367B: Pre-indexed byte (R1-R4)
   - 370B-373B: Pre-indexed halfword (R1-R4)
   - 374B-377B: Pre-indexed word (R1-R4)
```

---

---

## Disassembly Display Conventions

### Register Notation

The official ND-500 assembler uses the following register notation:

| Notation | Description | Example |
|----------|-------------|---------|
| `Rn` | General register (n=1-4) | `R1`, `R2`, `R3`, `R4` |
| `Wn` | Word register | `W1`, `W2`, `W3`, `W4` |
| `Hn` | Halfword register | `H1`, `H2`, `H3`, `H4` |
| `BYn` | Byte register | `BY1`, `BY2`, `BY3`, `BY4` |
| `BIn` | Bit register | `BI1`, `BI2`, `BI3`, `BI4` |
| `Fn` | Float register | `F1`, `F2`, `F3`, `F4` |
| `Dn` | Double float register | `D1`, `D2`, `D3`, `D4` |

### Post-Indexed Mode Display Format

Post-indexed operands use **parentheses** around the index register:

| Correct Format | Incorrect Formats |
|----------------|-------------------|
| `B.100(R2)` | `B.100+R2`, `B.100+r2`, `B.100+I2` |
| `B.OFFSET:H(R3)` | `B.OFFSET:H+R3` |
| `IND(B.20)(R1)` | `IND(B.20)+R1` |
| `LABEL(R4)` | `LABEL+R4` |

### Recommended Disassembly Output Formats

| Addressing Mode | Disassembly Format | Example |
|-----------------|-------------------|---------|
| Local | `B.<offset>` | `B.100` or `B.100:H` |
| Local P.I. | `B.<offset>(Rn)` | `B.100(R2)` or `B.100:H(R3)` |
| Local Indirect | `IND(B.<offset>)` | `IND(B.20)` or `IND(B.20:B)` |
| Local Indirect P.I. | `IND(B.<offset>)(Rn)` | `IND(B.20)(R1)` |
| Record | `R.<offset>` | `R.40` or `R.40:H` |
| Pre-Indexed | `Rn.<offset>` | `R3.100` or `R3.100:H` |
| Absolute | `<address>` | `12345678` or `LABEL` |
| Absolute P.I. | `<address>(Rn)` | `12345678(R2)` |
| Constant | `<value>` | `100` or `100:B` |
| Register | `Rn` | `R1`, `W2`, `F3`, `D4` |
| Descriptor | `DESC(B.<offset>)(Rn)` | `DESC(B.100)(R1)` |
| Alternative | `ALT(<operand>)` | `ALT(B.100)` |

### Size Suffix Display

When disassembling, the size suffix should be displayed to clarify the displacement/constant format:

| Size | Suffix | When to Display |
|------|--------|-----------------|
| Short | `:S` | Optional (default for short codes) |
| Byte | `:B` | When displacement is 1 byte |
| Halfword | `:H` | When displacement is 2 bytes |
| Word | `:W` | When displacement is 4 bytes |
| Float | `:F` | For float constants (same code as :W) |
| Double | `:D` | For double constants |

**Note:** Some disassemblers omit the suffix for the most common/natural size for each mode.

### Numeric Format Recommendations

| Format | Usage | Example |
|--------|-------|---------|
| Octal with B suffix | Traditional ND format | `100B`, `377B` |
| Hexadecimal with H suffix | Alternative format | `40H`, `0FFH` |
| Decimal (no suffix) | Small constants | `10`, `255` |

**Note:** Hexadecimal numbers starting with A-F must be prefixed with `0` (e.g., `0FFH` not `FFH`).

---

## Version History

- Document created from ND-60.113.02 EN and ND-05.009.4 EN reference manuals

