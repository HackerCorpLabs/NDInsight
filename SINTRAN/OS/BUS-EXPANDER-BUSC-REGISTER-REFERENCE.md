# BUS EXPANDER and BUSC Register Reference

**Complete Port Addresses and Register Definitions**

**Version:** 1.0  
**Date:** 2025-01-XX  
**Status:** Complete  
**Source:** Analysis of SINTRAN III source code (`PH-P2-OPPSTART.NPL`)

---

## Table of Contents

1. [BUS EXPANDER](#1-bus-expander)
2. [BUSC Devices](#2-busc-devices)
3. [Register Usage Examples](#3-register-usage-examples)
4. [Memory Limit Format](#4-memory-limit-format)

---

## 1. BUS EXPANDER

### 1.1 Device Addresses

**BUS EXPANDER #1:**
- **Base Address:** 100000₈ (32768 decimal, 0x8000 hex)
- **Address Range:** 100000₈ - 100003₈ (4 registers)
- **Device Number:** 10₈ (from hardware device table)

### 1.2 Register Map

Based on code analysis (`PH-P2-OPPSTART.NPL:2410`), BUS EXPANDER uses a simple presence test:

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Purpose |
|--------|----------------|-------------------|----------|--------|---------|
| +0 | 100000₈ | 32768 / 0x8000 | **Status/Test** | Read | Device presence test |

### 1.3 Detection Method

```npl
% From PH-P2-OPPSTART.NPL, lines 2409-2411
*"8BEX1
T:=100000; *IOXT; TRA IIC
IF A=0 THEN MEMTYPE BONE BBEXPANDER=:MEMTYPE FI
```

**Detection Process:**
1. Execute `IOXT` (I/O Transfer) to address 100000₈
2. Check accumulator result:
   - **A=0:** BUS EXPANDER present → Set `MEMTYPE |= BBEXPANDER`
   - **A≠0:** I/O error (device absent)

**Note:** BUS EXPANDER is used for memory expansion and may indicate MPM4 presence. The code only tests for device presence; no detailed register definitions are found in the source code examined.

### 1.4 Register Definitions

**Status/Test Register (100000₈):**
- **Purpose:** Device presence detection
- **Read Operation:** `IOXT` to 100000₈
- **Result:** 
  - Returns 0 if device present
  - Returns non-zero if device absent (I/O error)

**Other Registers (100001₈ - 100003₈):**
- Not used in SINTRAN boot code
- May be used for configuration or status in other contexts

---

## 2. BUSC Devices

### 2.1 Device Addresses

**BUSC Device Addressing:**
- **Base Address:** 100200₈ (32768 decimal, 0x8040 hex)
- **Address Formula:** `100200₈ + (NBUSCN × 4)`
- **Device Count:** Up to 18 devices (NBUSCN 0-17)
- **Device Number Range:** 20₈ - 37₈ (from hardware device table)

**Complete BUSC Address Table:**

| BUSC # | NBUSCN | Base Address (Octal) | Base Address (Dec/Hex) | Address Range | Device # |
|--------|--------|---------------------|------------------------|---------------|----------|
| BUSC #0 | 0 | 100200₈ | 32768 / 0x8040 | 100200₈-100203₈ | 20₈ |
| BUSC #1 | 1 | 100204₈ | 32772 / 0x8044 | 100204₈-100207₈ | 21₈ |
| BUSC #2 | 2 | 100210₈ | 32776 / 0x8048 | 100210₈-100213₈ | 22₈ |
| BUSC #3 | 3 | 100214₈ | 32780 / 0x804C | 100214₈-100217₈ | 23₈ |
| BUSC #4 | 4 | 100220₈ | 32784 / 0x8050 | 100220₈-100223₈ | 24₈ |
| BUSC #5 | 5 | 100224₈ | 32788 / 0x8054 | 100224₈-100227₈ | 25₈ |
| BUSC #6 | 6 | 100230₈ | 32792 / 0x8058 | 100230₈-100233₈ | 26₈ |
| BUSC #7 | 7 | 100234₈ | 32796 / 0x805C | 100234₈-100237₈ | 27₈ |
| BUSC #8 | 8 | 100240₈ | 32800 / 0x8060 | 100240₈-100243₈ | 30₈ |
| BUSC #9 | 9 | 100244₈ | 32804 / 0x8064 | 100244₈-100247₈ | 31₈ |
| BUSC #10 | 10 | 100250₈ | 32808 / 0x8068 | 100250₈-100253₈ | 32₈ |
| BUSC #11 | 11 | 100254₈ | 32812 / 0x806C | 100254₈-100257₈ | 33₈ |
| BUSC #12 | 12 | 100260₈ | 32816 / 0x8070 | 100260₈-100263₈ | 34₈ |
| BUSC #13 | 13 | 100264₈ | 32820 / 0x8074 | 100264₈-100267₈ | 35₈ |
| BUSC #14 | 14 | 100270₈ | 32824 / 0x8078 | 100270₈-100273₈ | 36₈ |
| BUSC #15 | 15 | 100274₈ | 32828 / 0x807C | 100274₈-100277₈ | 37₈ |
| BUSC #16 | 16 | 100300₈ | 32832 / 0x8080 | 100300₈-100303₈ | (if extended) |
| BUSC #17 | 17 | 100304₈ | 32836 / 0x8084 | 100304₈-100307₈ | (if extended) |

### 2.2 Register Map

Based on code analysis (`PH-P2-OPPSTART.NPL:2420-2427`), each BUSC device has 4 registers:

| Offset | Address (Octal) | Address (Dec/Hex) | Register | Access | Purpose |
|--------|----------------|-------------------|----------|--------|---------|
| +0 | Base+0 | Base+0 | **Status/Test** | Read | Device presence test, Read memory limits |
| +1 | Base+1 | Base+1 | **Reserved** | - | Not used in boot code |
| +2 | Base+2 | Base+2 | **Reserved** | - | Not used in boot code |
| +3 | Base+3 | Base+3 | **Control** | Write | Enable read limits |

### 2.3 Register Definitions

#### 2.3.1 Status/Test Register (Base+0)

**Address:** `100200₈ + (NBUSCN × 4) + 0`

**Read Operations:**

1. **Device Presence Test:**
   ```npl
   T:=NBUSCN*4+100200; *IOXT; TRA IIC
   IF A=0 THEN
       % Device present
   ELSE
       % Device absent
   FI
   ```
   - **Result A=0:** BUSC device present
   - **Result A≠0:** I/O error (device absent)

2. **Read Memory Limits:**
   ```npl
   T+3; A:=100; *IOXT      % Enable read limits
   T-3; *IOXT              % Read limits
   ```
   - **After enabling:** Read from Base+0 returns memory limits
   - **Returns:** A register = lower limit, D register = upper limit (combined)

#### 2.3.2 Control Register (Base+3)

**Address:** `100200₈ + (NBUSCN × 4) + 3`

**Write Operation:**
```npl
T+3; A:=100; *IOXT    % Enable read limits
```

**Purpose:** Enable reading memory limits from Status/Test register
- **Value:** 100₈ (64 decimal, 0x40 hex)
- **Effect:** Enables read limits mode
- **Note:** Must be written before reading limits from Base+0

### 2.4 Detection and Memory Limit Reading

**Complete BUSC Detection Flow:**

```npl
% From PH-P2-OPPSTART.NPL, lines 2418-2433
*"8MPM4
0=:NBUSCN; 0=:XA
FOR NBUSCN TO 17 DO
    % Calculate BUSC base address
    A:=NBUSCN*4+100200=:T; *IOXT; TRA IIC
    
    IF A=0 THEN
        % Device present - set bit flag
        NBUSCN SH 3+XBONE; X:=XA
        *EXR SX                                % BSET BONE XX DD
        X=:XA
        MEMTYPE BONE BMPM4=:MEMTYPE
        
        % Enable read limits
        T+3; A:=100; *IOXT                      % ENABLE READ LIMITS
        
        % Read memory limits
        T-3; *IOXT                              % READ LIMITS
        
        % Parse limits
        A=:D/\377 SH 6:=:D SHZ -10 SH 6:=:D
        IF A><D THEN D-1 ELSE A:=0; D:=0 FI     % TEST FOR EMPTY MPM4 PORT
    ELSE
        A:=0; D:=0
    FI
    
    % Store memory range in DMPM4 array
    X:=NBUSCN+X; AD=:DMPM4(X)
OD; XA=:NBUSCN
```

---

## 3. Register Usage Examples

### 3.1 BUS EXPANDER Detection

```npl
% Test BUS EXPANDER #1
T:=100000; *IOXT; TRA IIC
IF A=0 THEN
    % BUS EXPANDER present
    MEMTYPE BONE BBEXPANDER=:MEMTYPE
ELSE
    % BUS EXPANDER absent
FI
```

### 3.2 BUSC Device Detection

```npl
% Test BUSC #5 (NBUSCN=5)
NBUSCN:=5
A:=NBUSCN*4+100200=:T    % T = 100224₈
*IOXT; TRA IIC

IF A=0 THEN
    % BUSC #5 present
    MEMTYPE BONE BMPM4=:MEMTYPE
ELSE
    % BUSC #5 absent
FI
```

### 3.3 BUSC Memory Limit Reading

```npl
% Read memory limits from BUSC #3 (NBUSCN=3)
NBUSCN:=3
T:=NBUSCN*4+100200       % T = 100214₈ (base address)

% Step 1: Enable read limits
T+3; A:=100; *IOXT        % Write 100₈ to control register (100217₈)

% Step 2: Read limits
T-3; *IOXT                % Read from status register (100214₈)
% Result: A = lower limit (bits 0-7), D = combined limits

% Step 3: Parse limits
A=:D/\377 SH 6:=:D        % Extract lower limit: D = (D AND 377₈) << 6
D SHZ -10 SH 6:=:D        % Extract upper limit: D = (D >> 10) << 6

% Step 4: Validate
IF A><D THEN
    D-1                   % Valid range: A to D-1
ELSE
    A:=0; D:=0             % Empty port
FI

% Store in DMPM4 array
X:=NBUSCN+X; AD=:DMPM4(X)
```

---

## 4. Memory Limit Format

### 4.1 Limit Register Format

When reading memory limits from BUSC Base+0 register (after enabling with Base+3):

**Register Content (D register):**
```
Bits:  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
      ┌─────────────────────────────────────────────────────────────┐
      │ Upper Limit (bits 10-15) │ Lower Limit (bits 0-7) │ 0 0 0 0 │
      └─────────────────────────────────────────────────────────────┘
```

**Parsing Algorithm:**
```npl
% From PH-P2-OPPSTART.NPL, line 2428
A=:D/\377 SH 6:=:D SHZ -10 SH 6:=:D
```

**Step-by-Step:**
1. **Save A:** `A=:D` - Save accumulator (lower limit bits 0-7)
2. **Extract Lower Limit:** `D/\377 SH 6` - Mask bits 0-7, shift left 6 bits
   - Result: Lower limit in page units (page = 1024 words)
3. **Extract Upper Limit:** `D SHZ -10 SH 6` - Shift right 10 bits, then left 6 bits
   - Result: Upper limit in page units

**Example:**
```
If D register = 0x1234 (hex) = 0001100110100 (binary):
- Lower limit bits (0-7): 00110100 = 64₈ = 52 decimal
- Upper limit bits (10-15): 000110 = 6₈ = 6 decimal

After parsing:
- Lower limit: 64₈ << 6 = 6400₈ pages
- Upper limit: 6₈ << 6 = 600₈ pages
```

### 4.2 Empty Port Detection

```npl
IF A><D THEN D-1 ELSE A:=0; D:=0 FI
```

**Logic:**
- **If A ≠ D:** Valid memory range exists → Store range A to D-1
- **If A = D:** Empty port (no memory) → Set A=0, D=0

---

## 5. Summary

### 5.1 BUS EXPANDER

| Aspect | Value |
|--------|-------|
| **Base Address** | 100000₈ |
| **Registers** | 4 (100000₈-100003₈) |
| **Detection** | IOXT to 100000₈, A=0 if present |
| **Purpose** | Memory expansion indicator |
| **Register Details** | Minimal - only presence test used |

### 5.2 BUSC Devices

| Aspect | Value |
|--------|-------|
| **Base Address** | 100200₈ |
| **Device Count** | Up to 18 (NBUSCN 0-17) |
| **Address Formula** | 100200₈ + (NBUSCN × 4) |
| **Registers per Device** | 4 |
| **Detection** | IOXT to Base+0, A=0 if present |
| **Memory Limits** | Read from Base+0 after enabling Base+3 |
| **Control Register** | Base+3, write 100₈ to enable read limits |
| **Purpose** | MPM4 memory controller |

### 5.3 Key Differences

| Feature | BUS EXPANDER | BUSC |
|---------|--------------|------|
| **Purpose** | Memory expansion indicator | MPM4 memory controller |
| **Memory Limits** | Not read | Read via Base+3 enable + Base+0 read |
| **Device Count** | 1 (tested) | Up to 18 |
| **Register Usage** | Presence test only | Presence test + memory limit reading |

---

## 6. Code References

### 6.1 Source Code Locations

| Routine | File | Lines | Purpose |
|---------|------|-------|---------|
| BUS EXPANDER Test | `PH-P2-OPPSTART.NPL` | 2409-2411 | Detect BUS EXPANDER presence |
| BUSC Scan | `PH-P2-OPPSTART.NPL` | 2418-2433 | Detect BUSC devices and read limits |
| MPM4 Memory Processing | `PH-P2-OPPSTART.NPL` | 2462-2471 | Process BUSC memory ranges |

### 6.2 Key Symbols

| Symbol | Value | Purpose |
|--------|-------|---------|
| `BBEXPANDER` | Bit flag | BUS EXPANDER detected |
| `BMPM4` | Bit flag | MPM4 memory detected |
| `NBUSCN` | 0-17 | BUSC device number |
| `DMPM4` | Array | Stores BUSC memory ranges |
| `XBONE` | Bit table | Tracks detected BUSC devices |

---

**End of Document**
