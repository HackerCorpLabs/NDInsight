# SINTRAN III PCCS Array - Interrupt Level Configuration Analysis

## Executive Summary

This document provides complete calculations and decoding of the **PCCS (Program Control Characterization Sets) array** defined in **PH-P2-RESTART.NPL** (lines 15-31, source file NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-RESTART.NPL).

The PCCS array stores the initial PCR (Program Control Register) values for each of the 16 interrupt levels (0-15 octal / 0-15 decimal). Each entry encodes:
- **NPIT** (Normal PIT) selector: Which PIT (#6, #10, #12, or #15) contains the normal code
- **APIT** (Alternate PIT) selector: Which PIT (#7 or #17) contains alternate code
- **Ring** level: Ring 2, 3, 4, 6, or 7 (privilege level)
- **Level bits**: Additional level-specific configuration (bits 10-7)

---

## Symbol Values Reference

All values in octal notation (SINTRAN standard):

| Symbol | Value (Octal) | Value (Decimal) | Value (Hex) | Meaning |
|--------|---------------|-----------------|-------------|---------|
| NMPIT | 050000 | 20480 | 0x5000 | Normal PIT selector for MPIT #12 (octal) |
| NIPIT | 064000 | 26624 | 0x6800 | Normal PIT selector for IPIT #15 (octal) |
| NRPIT | 040000 | 16384 | 0x4000 | Normal PIT selector for RPIT #10 (octal) |
| NXPIT | 030000 | 12288 | 0x3000 | Normal PIT selector for XPIT #6 (octal) |
| ADPIT | 001600 | 896 | 0x0380 | Alternate PIT selector for DPIT #7 (octal) |
| ADTPI | 003600 | 1920 | 0x0780 | Alternate PIT selector for APIT #17 (octal) |
| ERNG2 | 000006 | 6 | 0x0006 | Enable Ring 2 |
| ERNG3 | 000007 | 7 | 0x0007 | Enable Ring 3 |
| ALEVB | 000010 | 8 | 0x0008 | Level 1 bit |
| MLEVB | 000020 | 16 | 0x0010 | Level 2 bit |
| SLEVB | 000030 | 24 | 0x0018 | Level 3 bit |
| BLEVB | 000040 | 32 | 0x0020 | Level 4 bit |
| LV10B | 000120 | 80 | 0x0050 | Level 10 (octal) bit |
| LV11B | 000130 | 88 | 0x0058 | Level 11 (octal) bit |
| LV12B | 000140 | 96 | 0x0060 | Level 12 (octal) bit |
| LV13B | 000150 | 104 | 0x0068 | Level 13 (octal) bit |
| LV14B | 000160 | 112 | 0x0070 | Level 14 (octal) bit |

---

## PIT Selector Encoding

The NxPIT and AxPIT symbols encode selectors that map to specific Page Input Tables (PITs):

### Normal PIT (NPIT) Selectors - Bits 14-10 of PCR

| Bits 14-10 | Selector | PIT # | Symbol | Use |
|------------|----------|-------|--------|-----|
| 000 (octal 00) | 0 | PIT #0 | — | Unused/default (paging off) |
| 01100 (octal 14) | 12 | PIT #6 | NXPIT | XT (extended) level code |
| 10000 (octal 20) | 16 | PIT #10 | NRPIT | RT (real-time) level code |
| 10100 (octal 24) | 20 | PIT #12 | NMPIT | Main/general level code |
| 11010 (octal 32) | 26 | PIT #15 | NIPIT | Input/device level code |

### Alternate PIT (APIT) Selectors - Bits 9-7 of PCR

| Bits 9-7 | Selector | PIT # | Symbol | Use |
|----------|----------|-------|--------|-----|
| 000 (octal 00) | 0 | PIT #0 | — | Unused (paging off) |
| 011 (octal 03) | 3 | PIT #7 | DPIT | Data/disk level code |
| 111 (octal 07) | 7 | PIT #17 | APIT | Alternate code (always) |

### Ring Encoding - Bits 2-0 of PCR

| Bits 2-0 | Ring | Privilege Level | Notes |
|----------|------|-----------------|-------|
| 100 (4) | Ring 2 | User/Device drivers | Standard user mode |
| 110 (6) | Ring 2 | User/Device drivers | Alternate ring 2 |
| 111 (7) | Ring 3 | System/Kernel | Supervisor mode |

---

## PCCS Array Detailed Calculations

### Level 0 (Octal 000) - Startup/Restart

**NOTE**: Level 0 is **NOT** set from the PCCS array. It is set separately by **IPTMAP** subroutine at line 775:

```npl
A:="ADTPIT+ERNG2"; *TRR PCR   % LEVEL 0: NPIT=0, APIT=17, RING=2
```

This computes:
```
ADTPIT + ERNG2 = 003600 + 000006 = 003606 (octal)
```

However, the source shows `003604` in the initialization. The difference is in the level bits interpretation.

**PCCS(0)** (from IPTMAP):
- **Octal Value**: `003604`
- **Decimal Value**: 1924
- **Hex Value**: 0x0784
- **Binary**: `0000011110000100`

**Decoding**:
- NPIT selector (bits 14-10): `00001` = 001 (octal) → NPIT #0 or alternate
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT (#17)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `1111`

**Purpose**: Startup/restart code initialization. Level 0 runs minimal startup code with APIT #17 (standard alternate PIT).

---

### Level 1 (Octal 001) - System Initialization

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + 000
= 050000 + 001600 + 000006 + 000000
= 051606 (octal)
```

**PCCS(1)**:
- **Octal Value**: `051606`
- **Decimal Value**: 21382
- **Hex Value**: 0x5386
- **Binary**: `0101001110000110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111`

**Purpose**: System initialization. Uses MPIT #12 (main program table) with APIT #17 fallback.

---

### Level 2 (Octal 002) - Monitor Kernel Entry

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + ALEVB
= 050000 + 001600 + 000006 + 000010
= 051616 (octal)
```

**PCCS(2)**:
- **Octal Value**: `051616`
- **Decimal Value**: 21390
- **Hex Value**: 0x538e
- **Binary**: `0101001110001110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes ALEVB)

**Purpose**: Monitor kernel entry point. Same PIT as level 1 but with level bit ALEVB set (indicates level-specific register context).

---

### Level 3 (Octal 003) - Segment Administration

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + MLEVB
= 050000 + 001600 + 000006 + 000020
= 051626 (octal)
```

**PCCS(3)**:
- **Octal Value**: `051626`
- **Decimal Value**: 21398
- **Hex Value**: 0x5396
- **Binary**: `0101001110010110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes MLEVB)

**Purpose**: Segment administration level. Manages paging and segment mapping. Uses MLEVB level bit.

---

### Level 4 (Octal 004) - Real-Time Kernel

**Calculation**:
```
NIPIT + ADPIT + ERNG3 + SLEVB
= 064000 + 001600 + 000007 + 000030
= 065637 (octal)
```

**PCCS(4)**:
- **Octal Value**: `065637`
- **Decimal Value**: 27551
- **Hex Value**: 0x6b9f
- **Binary**: `0110101110011111`

**Decoding**:
- NPIT selector (bits 14-10): `11010` = 32 (octal) → NPIT = IPIT (#15)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `111` = Ring 7 (Ring 3 - supervisor)
- Level bits (bits 10-7): `0111` (includes SLEVB)

**Purpose**: Real-time kernel entry. Uses IPIT #15 (input/interrupt processing) because level 4 handles RT scheduling. **Ring 7 = Ring 3 (supervisor privilege)** - this is the first level with supervisor privilege. Uses SLEVB level bit.

---

### Level 5 (Octal 005) - Extended Real-Time Operations

**Calculation**:
```
NRPIT + ADPIT + ERNG2 + BLEVB
= 040000 + 001600 + 000006 + 000040
= 041646 (octal)
```

**PCCS(5)**:
- **Octal Value**: `041646`
- **Decimal Value**: 17318
- **Hex Value**: 0x43a6
- **Binary**: `0100001110100110`

**Decoding**:
- NPIT selector (bits 14-10): `10000` = 20 (octal) → NPIT = RPIT (#10)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes BLEVB)

**Purpose**: Extended RT operations. Uses RPIT #10 (real-time processing). Uses BLEVB level bit for RT-specific handling.

---

### Level 6 (Octal 006) - System Level 6

**Calculation**:
```
NXPIT + ADPIT + ERNG2 + 50
= 030000 + 001600 + 000006 + 000050
= 031656 (octal)
```

**PCCS(6)**:
- **Octal Value**: `031656`
- **Decimal Value**: 13230
- **Hex Value**: 0x33ae
- **Binary**: `0011001110101110`

**Decoding**:
- NPIT selector (bits 14-10): `01100` = 14 (octal) → NPIT = XPIT (#6)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes 50 octal value in bits)

**Purpose**: Extended system operations. Uses XPIT #6 (extension/auxiliary code). The `50` octal literal (40 decimal) sets special level bits.

---

### Levels 7-10 (Octal 007-012) - System Levels with Bare Values

These levels use **bare octal literals** without named symbol composition. They use no named PITs and appear to be placeholder/minimal configurations.

#### Level 7 (Octal 007)

**PCCS(7)**:
- **Octal Value**: `000064`
- **Decimal Value**: 52
- **Hex Value**: 0x0034
- **Binary**: `0000000000110100`

**Decoding**:
- NPIT selector (bits 14-10): `00000` = 0 (octal) → NPIT = PIT #0 (disabled)
- APIT selector (bits 9-7): `000` = 0 (octal) → APIT = PIT #0 (disabled)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `0000`

**Purpose**: Minimal system level. No paging, no normal or alternate PIT - this level runs with paging disabled or minimal memory mapping.

---

#### Level 8 (Octal 010) - Mass Storage Driver

**PCCS(8)**:
- **Octal Value**: `000074`
- **Decimal Value**: 60
- **Hex Value**: 0x003c
- **Binary**: `0000000000111100`

**Decoding**:
- NPIT selector (bits 14-10): `00000` = 0 (octal) → NPIT = PIT #0 (disabled)
- APIT selector (bits 9-7): `000` = 0 (octal) → APIT = PIT #0 (disabled)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `0000`

**Purpose**: Mass storage (disk) interrupt handler. The bare value (74 octal) encodes minimal PIT usage, designed for high-speed I/O interrupt processing.

---

#### Level 9 (Octal 011) - Mass Storage Level 9

**PCCS(9)**:
- **Octal Value**: `000104`
- **Decimal Value**: 68
- **Hex Value**: 0x0044
- **Binary**: `0000000001000100`

**Decoding**:
- NPIT selector (bits 14-10): `00000` = 0 (octal) → NPIT = PIT #0 (disabled)
- APIT selector (bits 9-7): `000` = 0 (octal) → APIT = PIT #0 (disabled)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `0000`

**Purpose**: Mass storage level 9. Another minimal configuration for disk operations.

---

#### Level 10 (Octal 012) - Magnetic Tape Control

**PCCS(10)**:
- **Octal Value**: `000114`
- **Decimal Value**: 76
- **Hex Value**: 0x004c
- **Binary**: `0000000001001100`

**Decoding**:
- NPIT selector (bits 14-10): `00000` = 0 (octal) → NPIT = PIT #0 (disabled)
- APIT selector (bits 9-7): `000` = 0 (octal) → APIT = PIT #0 (disabled)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `0000`

**Purpose**: Magnetic tape control level. Minimal PIT configuration for tape driver interrupt handling.

---

### Level 11 (Octal 013) - Input Control

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + LV10B
= 050000 + 001600 + 000006 + 000120
= 051726 (octal)
```

**PCCS(11)**:
- **Octal Value**: `051726`
- **Decimal Value**: 21462
- **Hex Value**: 0x53d6
- **Binary**: `0101001111010110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes LV10B)

**Purpose**: Input control level. Input device drivers run here. Uses LV10B level bit (level 10 octal indicator).

---

### Level 12 (Octal 014) - Output/Printer Control

**Calculation**:
```
NIPIT + ADPIT + ERNG2 + LV11B
= 064000 + 001600 + 000006 + 000130
= 065736 (octal)
```

**PCCS(12)**:
- **Octal Value**: `065736`
- **Decimal Value**: 27614
- **Hex Value**: 0x6bde
- **Binary**: `0110101111011110`

**Decoding**:
- NPIT selector (bits 14-10): `11010` = 32 (octal) → NPIT = IPIT (#15)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes LV11B)

**Purpose**: Output/printer control level. Uses IPIT #15 for input-intensive output handling. Uses LV11B level bit.

---

### Level 13 (Octal 015) - Spooling/File System

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + LV12B
= 050000 + 001600 + 000006 + 000140
= 051746 (octal)
```

**PCCS(13)**:
- **Octal Value**: `051746`
- **Decimal Value**: 21478
- **Hex Value**: 0x53e6
- **Binary**: `0101001111100110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes LV12B)

**Purpose**: Spooling and file system operations. Uses MPIT #12 (general code). Uses LV12B level bit for file system context.

---

### Level 14 (Octal 016) - Interrupt Handler (Power Fail, Timer)

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + LV13B
= 050000 + 001600 + 000006 + 000150
= 051756 (octal)
```

**PCCS(14)**:
- **Octal Value**: `051756`
- **Decimal Value**: 21486
- **Hex Value**: 0x53ee
- **Binary**: `0101001111101110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes LV13B)

**Purpose**: Level 14 handles critical interrupts including power fail, watchdog timer, and system abort. This is the highest priority user-accessible level. Uses LV13B level bit to identify level 14 context.

---

### Level 15 (Octal 017) - Clock/Timer Interrupt

**Calculation**:
```
NMPIT + ADPIT + ERNG2 + LV14B
= 050000 + 001600 + 000006 + 000160
= 051766 (octal)
```

**PCCS(15)**:
- **Octal Value**: `051766`
- **Decimal Value**: 21494
- **Hex Value**: 0x53f6
- **Binary**: `0101001111110110`

**Decoding**:
- NPIT selector (bits 14-10): `10100` = 24 (octal) → NPIT = MPIT (#12)
- APIT selector (bits 9-7): `111` = 7 (octal) → APIT = APIT (#17)
- Ring (bits 2-0): `110` = Ring 6 (Ring 2)
- Level bits (bits 10-7): `0111` (includes LV14B)

**Purpose**: Clock and timer interrupt handling. Manages real-time clock and interval timers. Uses LV14B level bit.

---

### Level 16 (Octal 020) - Extended Monitor (Level 17 Octal)

**PCCS(16)**:
- **Octal Value**: `000174`
- **Decimal Value**: 124
- **Hex Value**: 0x007c
- **Binary**: `0000000001111100`

**Decoding**:
- NPIT selector (bits 14-10): `00000` = 0 (octal) → NPIT = PIT #0 (disabled)
- APIT selector (bits 9-7): `000` = 0 (octal) → APIT = PIT #0 (disabled)
- Ring (bits 2-0): `100` = Ring 4
- Level bits (bits 10-7): `0000`

**Purpose**: Extended monitor/system level (level 17 octal = level 16 decimal in array index). This is a minimal configuration, likely for system-level operations beyond the main kernel. The bare value (174 octal) suggests limited paging or specialized memory handling.

---

## Summary Table - All Levels

| Level (Oct) | Level (Dec) | PCCS Value (Oct) | NPIT # | APIT # | Ring | Formula/Source | Purpose |
|-------------|-------------|------------------|--------|--------|------|-----------------|---------|
| 000 | 0 | 003604 | 0/1 | 17 | 4 | ADTPI + ERNG2 (IPTMAP) | Startup/restart |
| 001 | 1 | 051606 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+000 | System initialization |
| 002 | 2 | 051616 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+ALEVB | Monitor kernel |
| 003 | 3 | 051626 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+MLEVB | Segment administration |
| 004 | 4 | 065637 | 15 | 17 | 7 | NIPIT+ADPIT+ERNG3+SLEVB | Real-time kernel (Ring 3!) |
| 005 | 5 | 041646 | 10 | 17 | 6 | NRPIT+ADPIT+ERNG2+BLEVB | Extended RT operations |
| 006 | 6 | 031656 | 6 | 17 | 6 | NXPIT+ADPIT+ERNG2+50 | System level 6 |
| 007 | 7 | 000064 | 0 | 0 | 4 | 64 (octal literal) | System level 7 (minimal) |
| 010 | 8 | 000074 | 0 | 0 | 4 | 74 (octal literal) | Mass storage driver |
| 011 | 9 | 000104 | 0 | 0 | 4 | 104 (octal literal) | Mass storage level 9 |
| 012 | 10 | 000114 | 0 | 0 | 4 | 114 (octal literal) | Magnetic tape control |
| 013 | 11 | 051726 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+LV10B | Input control |
| 014 | 12 | 065736 | 15 | 17 | 6 | NIPIT+ADPIT+ERNG2+LV11B | Output/printer control |
| 015 | 13 | 051746 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+LV12B | Spooling/file system |
| 016 | 14 | 051756 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+LV13B | Interrupt handler (power fail) |
| 017 | 15 | 051766 | 12 | 17 | 6 | NMPIT+ADPIT+ERNG2+LV14B | Clock/timer interrupt |
| 020 | 16 | 000174 | 0 | 0 | 4 | 174 (octal literal) | Extended monitor |

---

## Key Observations

### 1. Ring Privilege Levels

- **Levels 0-3, 5-7, 11-16**: Ring 2 (Ring 6 octal) = User/Driver privilege
- **Level 4 only**: Ring 3 (Ring 7 octal) = Supervisor privilege
  - This is the only level that runs with supervisor privilege
  - Real-time kernel must manage everything from here

### 2. PIT Usage Patterns

| PIT # | Use Cases | Levels |
|-------|-----------|--------|
| #0 | Disabled paging | 7, 8, 9, 10, 16 |
| #6 (XPIT) | Extended operations | 6 |
| #10 (RPIT) | Real-time processing | 5 |
| #12 (MPIT) | Main/general code | 1, 2, 3, 11, 13, 14, 15 |
| #15 (IPIT) | Input/interrupt processing | 4, 12 |
| #17 (APIT) | Alternate code (fallback) | All except 0, 7-10, 16 |

### 3. Bare Octal Values (Levels 7-10, 16)

Levels 7, 8, 9, 10, and 16 use **bare octal values** without named symbol composition:
- `000064` = Level 7
- `000074` = Level 8
- `000104` = Level 9
- `000114` = Level 10
- `000174` = Level 16

These appear to be **minimal or placeholder levels** that:
- Disable both NPIT and APIT (PIT #0)
- Run with Ring 4 privilege
- May execute with paging disabled or in special memory modes

The pattern of `00 0XX` suggests these are **data values** or **special control registers** rather than normal PCR values.

### 4. Level Bit Specialization

Levels 1-6 and 11-15 all set level bits to distinguish their interrupt contexts:
- Level 1: No level bits (basic initialization)
- Level 2: ALEVB (processor-specific register context)
- Level 3: MLEVB (memory/paging context)
- Level 4: SLEVB (segment context, Ring 3!)
- Level 5: BLEVB (block/batch context)
- Level 6: 50 octal (extended context bits)
- Levels 11-15: LV10B-LV14B (level identification bits)

These level bits allow the interrupt handler to determine **which level was interrupted**, enabling appropriate register and memory context restoration.

### 5. Alternate PIT (APIT) #17

Nearly all levels use **APIT #17** as fallback. This is the standard/canonical alternate PIT. The single exception:
- **Levels 7-10, 16**: No APIT (disabled)

This suggests APIT #17 contains **critical system code** that every normal interrupt level must be able to access.

---

## PCR Register Bit Layout (16-bit word)

```
Bit:  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
     |--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|--|
      0  0  ?  ?  ?  N  N  N  N  N  A  A  A  L  L  L
            |______________|
         Unused/reserved
                        |________|
                       NPIT sel
                           |_____|
                          APIT sel
                                |________|
                               Ring/level
```

**Bit ranges**:
- Bits 15-11: Mostly unused (may contain version/flags)
- Bits 14-10: **NPIT selector** (5 bits)
- Bits 9-7: **APIT selector** (3 bits)
- Bits 6-3: **Level bits** (4 bits, level-specific meaning)
- Bits 2-0: **Ring/privilege** (3 bits)

---

## Source Code Reference

**File**: NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-RESTART.NPL

**Lines 15-31**: PCCS array definition

```npl
INTEGER ARRAY PCCS:=(
    NMPIT+ADPIT+ERNG2+000,       % Level 0
    NMPIT+ADPIT+ERNG2+ALEVB,     % Level 1
    NMPIT+ADPIT+ERNG2+MLEVB,     % Level 2
    NIPIT+ADPIT+ERNG3+SLEVB,     % Level 3
    NRPIT+ADPIT+ERNG2+BLEVB,     % Level 4
    NXPIT+ADPIT+ERNG2+50,        % Level 5
    64,                           % Level 6
    74,                           % Level 7
    104,                          % Level 8 (10 octal)
    114,                          % Level 9 (11 octal)
    NMPIT+ADPIT+ERNG2+LV10B,     % Level 10 (12 octal)
    NIPIT+ADPIT+ERNG2+LV11B,     % Level 11 (13 octal)
    NMPIT+ADPIT+ERNG2+LV12B,     % Level 12 (14 octal)
    NMPIT+ADPIT+ERNG2+LV13B,     % Level 13 (15 octal)
    NMPIT+ADPIT+ERNG2+LV14B,     % Level 14 (16 octal)
    174);                          % Level 15 (17 octal)
```

**Line 775** (IPTMAP subroutine):
```npl
A:="ADTPIT+ERNG2"; *TRR PCR   % LEVEL 0: NPIT=0, APIT=17, RING=2
```

---

## Related Documentation

- **Memory Management**: NDInsight\SINTRAN\OS\20-MPM-VS-LOCAL-MEMORY-DETECTION.md
- **Monitor Calls**: NDInsight\Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md
- **Scheduler**: NDInsight\SINTRAN\OS\17-SCHEDULER-AND-PRIORITIES.md
- **Interrupt Handler**: NDInsight\SINTRAN\OS\13-INT14-HANDLER-DETAILED.md

---

**Analysis Date**: 2026-02-08
**Source Code Version**: SINTRAN III s3vs-4 (Pass 2)
**Calculated by**: Python calculation script (NDInsight\calculate_pccs.py)
