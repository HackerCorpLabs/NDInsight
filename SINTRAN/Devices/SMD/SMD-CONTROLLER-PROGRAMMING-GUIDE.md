# SMD Disk Controller Programming Guide

**ND-100 SMD (Storage Module Device) Controller - Complete Register Map, I/O Sequences, and Programming Model**

**Version:** 1.0
**Date:** 2026-04-21
**Status:** Complete
**Sources:** nd100x emulator (`~/repos/nd100x/src/devices/smd/`), SINTRAN III NPL source (`IP-P2-DISK-START.NPL`), SINTRAN boot trace analysis (17,402 IOX accesses, 731 GO commands)

---

## Table of Contents

1. [Overview](#1-overview)
2. [IOX Register Map](#2-iox-register-map)
3. [Control Word Register (CWR)](#3-control-word-register-cwr)
4. [Status Register](#4-status-register)
5. [Flip-Flop Registers](#5-flip-flop-registers)
6. [Block Address Registers](#6-block-address-registers)
7. [Seek Condition Register](#7-seek-condition-register)
8. [ECC Registers](#8-ecc-registers)
9. [Controller Types](#9-controller-types)
10. [Complete Read Sequence](#10-complete-read-sequence)
11. [Complete Write Sequence](#11-complete-write-sequence)
12. [Seek Sequence](#12-seek-sequence)
13. [Device Clear and Error Recovery](#13-device-clear-and-error-recovery)
14. [Interrupt vs Polling](#14-interrupt-vs-polling)
15. [CHS to LBA Conversion](#15-chs-to-lba-conversion)
16. [SINTRAN NPL Source Cross-Reference](#16-sintran-npl-source-cross-reference)

---

## 1. Overview

The SMD controller sits on the ND-100 I/O bus and manages up to 4 disk units. It uses DMA to transfer data between disk and ND-100 physical memory. The controller occupies 8 consecutive IOX addresses.

**Key characteristics:**
- 8 IOX register addresses (base + 0 through base + 7)
- 24-bit core address (via flip-flop: HI8 then LO16)
- 24-bit word counter (via flip-flop: HI8 then LO16)
- CWR bit 15 multiplexes registers (selects alternate function for 4 of 8 registers)
- Level 11 interrupt for mass storage
- Operation type selected by opcode in CWR bits 11-14 (NOT separate GO bits)
- Active bit (CWR bit 2) is the GO trigger for ALL operations

**Controller base addresses:**

| Thumbwheel | Base Address | IDENT Code | Name |
|------------|-------------|------------|------|
| 0 | 1540 | 017 | SMD 1540 (primary) |
| 1 | 1550 | 020 | SMD 1550 (secondary) |
| 2 | 0540 | 023 | SMD 540 |
| 3 | 0550 | 006 | SMD 550 |

---

## 2. IOX Register Map

All addresses shown for base = 1540 (octal). CWR15 = Control Word Register bit 15 (register multiplex bit).

| IOX Addr | R/W | CWR15=0 | CWR15=1 |
|----------|-----|---------|---------|
| **1540** | R | Read Core Address (flip-flop) | Read Word Counter (flip-flop) |
| **1541** | W | Load Core Address (flip-flop: HI8, LO16) | Count Memory Address (test mode) |
| **1542** | R | Read Seek Condition | Read ECC Count |
| **1543** | W | Load Block Address I (head/sector) | Load Block Address II (cylinder) |
| **1544** | R | Read Status Register | Read ECC Pattern |
| **1545** | W | Load Control Word (CWR) | Load Control Word (CWR) |
| **1546** | R | Read Block Address I | Read Block Address II |
| **1547** | W | Load Word Counter (flip-flop: HI8, LO16) | Load ECC Control |

**SINTRAN NPL symbols** (from `IP-P2-DISK-START.NPL:416-420`):

| Symbol | Offset | Register |
|--------|--------|----------|
| RSC | +2 | Read Seek Condition |
| LBA | +3 | Load Block Address |
| RSR | +4 | Read Status Register |
| LCO | +5 | Load Control Word |

---

## 3. Control Word Register (CWR)

**IOX 1545 (Write only)**

```
Bit 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
+----+---+---+---+---+----+---+---+---+---+---+---+---+---+---+---+
|MUX |      OpCode       |MRC|    Unit Select   |A17|A16| DC|TST|ACT|EIE|IEN|
+----+---+---+---+---+----+---+---+---+---+---+---+---+---+---+---+
```

| Bit(s) | Name | Description |
|--------|------|-------------|
| 0 | IntEn | Enable interrupt on device not active |
| 1 | ErrIntEn | Enable interrupt on errors |
| 2 | Active | **GO trigger** - starts the operation specified by OpCode |
| 3 | TestMode | Test mode (maintenance only) |
| 4 | DeviceClear | Clears active flip-flop, all registers, all errors, all flip-flops |
| 5 | AddrBit16 | Core address bit 16 (old controllers without flip-flops only) |
| 6 | AddrBit17 | Core address bit 17 (old controllers without flip-flops only) |
| 7-9 | UnitSelect | Unit number (0-3, supports up to 8) |
| 10 | MRC | Marginal recovery cycle (maintenance) |
| 11-14 | OpCode | Device operation (see table below) |
| 15 | MUX | Register multiplex bit - selects alternate register functions |

**Operation Codes (bits 11-14):**

| Code | Mnemonic | CWR Contribution (octal) | Description |
|------|----------|-------------------------|-------------|
| 0 | M0 | 000000 | Read Transfer |
| 1 | M1 | 004000 | Write Transfer |
| 2 | M2 | 010000 | Read Parity Transfer |
| 3 | M3 | 014000 | Compare Transfer |
| 4 | M4 | 020000 | Initiate Seek |
| 5 | M5 | 024000 | Write Format |
| 6 | M6 | 030000 | Seek Complete Search |
| 7 | M7 | 034000 | Return To Zero Seek |
| 8 | M8 | 040000 | Run ECC Operation |
| 9 | M9 | 044000 | Select Release |

**Critical:** The opcode is carried through ALL CWR writes during setup, not just the GO command. For a write transfer, every CWR write during setup has OpCode=1 (04000 octal).

---

## 4. Status Register

**IOX 1544 (Read only, CWR15=0)**

```
Bit 15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
+----+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|MUX |OCY|DNR|   |   |CMP|   |ADM|HE2|TMO|ILL|HWE|RDY|ACT|EIE|IEN|
+----+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```

| Bit | Name | Description |
|-----|------|-------------|
| 0 | IntEn | Interrupt enabled |
| 1 | ErrIntEn | Error interrupt enabled |
| 2 | Active | Controller is active (busy) |
| 3 | Ready | Ready for transfer |
| 4 | HardwareError | Inclusive OR of bits 5, 6, 7, 8, and 13 |
| 5 | IllegalLoad | Load attempted while Active=1 |
| 6 | Timeout | Operation timed out |
| 7 | HardwareError2 | Disk fault, missing clocks, ECC parity error |
| 8 | AddressMismatch | CHS address exceeds disk geometry |
| 10 | ComparerError | Compare transfer found mismatch |
| 13 | DiskUnitNotReady | Selected disk unit not ready |
| 14 | OnCylinder | Heads positioned on correct cylinder |
| 15 | MUX | Readback of CWR bit 15 |

**Side effect:** Reading the status register resets ALL flip-flops (core address, word counter, ECC control).

---

## 5. Flip-Flop Registers

The SMD 10MHz and 15MHz controllers use flip-flops for registers wider than 16 bits. The flip-flop determines whether the next write goes to the HI or LO portion.

### 5.1 Core Address (IOX 1541, CWR15=0)

24-bit DMA memory address. Two successive writes required:

| Write # | Flip-Flop State | Data Written |
|---------|----------------|--------------|
| 1st | false (initial) | HI 8 bits (A-reg bits 0-7 -> address bits 16-23) |
| 2nd | true | LO 16 bits (A-reg bits 0-15 -> address bits 0-15) |

### 5.2 Word Counter (IOX 1547, CWR15=0)

24-bit transfer size in words. Two successive writes required:

| Write # | Flip-Flop State | Data Written |
|---------|----------------|--------------|
| 1st | false (initial) | HI 8 bits (A-reg bits 0-7 -> count bits 16-23) |
| 2nd | true | LO 16 bits (A-reg bits 0-15 -> count bits 0-15) |

### 5.3 ECC Control (IOX 1547, CWR15=1)

Also uses flip-flop (HI8 then LO16), same as word counter but selected by CWR15=1.

### 5.4 Flip-Flop Reset

Flip-flops are reset to the initial state (next write = HI portion) by:

1. **Reading the Status Register** (IOX 1544 with CWR15=0)
2. **Device Clear** (CWR bit 4)
3. **Transfer completion** (automatic after DMA finishes)

**This is critical for correct operation.** SINTRAN reads the status register (step 2 of the I/O sequence) specifically to reset all flip-flops before loading new parameters.

---

## 6. Block Address Registers

The block address uses **CWR bit 15** as a multiplexer, NOT flip-flops:

### 6.1 Block Address I (IOX 1543, CWR15=0)

Head and sector address:

```
Bits 0-7:   Sector number
Bits 8-15:  Head (surface) number
```

### 6.2 Block Address II (IOX 1543, CWR15=1)

Cylinder number:

```
Bits 0-15:  Cylinder number (0-65535)
```

To load both, you must:
1. Clear CWR15 -> write Block Address I (head/sector) to IOX 1543
2. Set CWR15 -> write Block Address II (cylinder) to IOX 1543

---

## 7. Seek Condition Register

**IOX 1542 (Read only, CWR15=0)**

| Bit(s) | Name | Description |
|--------|------|-------------|
| 0-7 | SeekComplete | Bitmask: bit N = unit N seek complete (one bit per revolution after positioning) |
| 8-10 | UnitSelected | Unit number from last CWR write |
| 11 | SeekError | Selected unit seek error (cleared only by M7-ReturnToZero) |
| 12 | isSMD15MHz | Always 1 for 15MHz controller, always 0 for NORD-10 controller |
| 13 | ECCCorrectable | ECC error is correctable (after M8 operation) |
| 14 | ECCParityError | Hardware fault in ECC polynomials |
| 15 | AddressField | Last field read was address field |

---

## 8. ECC Registers

### 8.1 ECC Control (IOX 1547, CWR15=1)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | ResetECC | Reset ECC polynomials to zero state |
| 1 | ForceParity | Force ECC parity error (maintenance) |
| 2 | Long | Extend sector data field by 64 bits (maintenance) |
| 3-6 | Format A-D | Format selection (15MHz controllers, used during formatting) |

**SINTRAN writes 011 octal (= ResetECC + bit 3 ECC Correction) before every data transfer.**

### 8.2 ECC Pattern (IOX 1544, CWR15=1)

| Bit(s) | Description |
|--------|-------------|
| 0-10 | Error pattern |
| 11-13 | Always 1 |
| 14 | Always 0 (15MHz) / Always 1 (old NORD-10 controller) - distinguishes controller type |
| 15 | Always 1 (readback of CWR15) |

### 8.3 ECC Count (IOX 1542, CWR15=1)

ECC error bit position counter.

---

## 9. Controller Types

| Type | Constant | Flip-Flops | CWR bits 5-6 | Notes |
|------|----------|------------|-------------|-------|
| BIG DISC | CONTR_BIG_DISC | No | Core addr HI bits | 33/66 MB disks |
| ECC DISC | CONTR_ECC_DISC | No | Core addr HI bits | 30/60/90 MB |
| SMD 10MHz | CONTR_SMD_10MHZ | **Yes** | Unused | Legacy |
| SMD 15MHz (ND632) | CONTR_SMD_15MHZ | **Yes** | Unused | Standard |

Old controllers (BIG DISC, ECC DISC) encode core address bits 16-17 directly in CWR bits 5-6 instead of using the flip-flop mechanism. SMD 10/15MHz controllers use the two-write flip-flop for full 24-bit addressing.

---

## 10. Complete Read Sequence

**M0-Read: Transfer data from disk to memory**

The base CWR for reads is **000000** (OpCode M0 = 0).

```
Step  IOX Addr  Register              A-register Value         Purpose
----  --------  --------------------  -------------------------  --------------------------
 1    1545      Load Control Word     000000                     Base CWR (M0-Read, no flags)
 2    1544      Read Status           (discard value)            Verify idle + RESET FLIP-FLOPS
 3    1543      Load Block Address I  head<<8 | sector           Head and sector (CWR15=0)
 4    1545      Load Control Word     100000                     Set CWR15 (base | 0100000)
 5    1543      Load Block Address II cylinder                   Cylinder number (CWR15=1)
 6    1547      Load ECC Control      000011                     ResetECC + ECC Correction (CWR15=1)
 7    1545      Load Control Word     000000                     Clear CWR15 (back to base)
 8    1541      Load Core Address     000000                     HI 8 bits (flip-flop 1st write)
 8    1541      Load Core Address     memory_address             LO 16 bits (flip-flop 2nd write)
 9    1547      Load Word Counter     000000                     HI 8 bits (flip-flop 1st write)
 9    1547      Load Word Counter     word_count                 LO 16 bits (flip-flop 2nd write)
10    1545      Load Control Word     000005                     GO: base | Active(04) | IntEn(01)
11    1544      Read Status           (poll until Active=0)      Wait for completion
```

### Read Sequence Notes

- Step 2 is essential: reading status resets all flip-flops to their initial state
- Steps 3-5: Block address uses CWR15 mux, NOT flip-flops
- Step 6: ECC control is loaded while CWR15=1 (from step 4)
- Steps 8-9: Core address and word counter each require TWO writes (HI then LO)
- Step 10: GO command = base CWR | Active (bit 2) | IntEn (bit 0)
- After completion: controller clears Active, sets SeekComplete for the unit

### Read Sequence as ND-100 Assembly

```asm
; --- M0-Read: Read disk sector to memory ---
; Assumes: T = HDEV (base IOX address, e.g., 1540)

        ; Step 1: Write base CWR (M0 opcode = 0, no active)
        SAA  0                  ; A = 000000
        IOXT                    ; IOX HDEV+5 (1545) - Load Control Word
        TRA  IIC

        ; Step 2: Read status to verify idle and reset flip-flops
        SAT  HDEV+4             ; T = 1544
        IOXT                    ; IOX 1544 - Read Status -> A
        TRA  IIC

        ; Step 3: Load Block Address I (head/sector), CWR15=0
        SAA  head_sector        ; A = (head << 8) | sector
        SAT  HDEV+3             ; T = 1543
        IOXT                    ; IOX 1543 - Load Block Address I
        TRA  IIC

        ; Step 4: Set CWR15 to select Block Address II and ECC registers
        SAA  0100000            ; A = CWR with MUX bit set
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - Load Control Word (CWR15=1)
        TRA  IIC

        ; Step 5: Load Block Address II (cylinder), CWR15=1
        SAA  cylinder           ; A = cylinder number
        SAT  HDEV+3             ; T = 1543
        IOXT                    ; IOX 1543 - Load Block Address II
        TRA  IIC

        ; Step 6: Load ECC Control = 011 (ResetECC + Correction), CWR15=1
        SAA  011                ; A = 011 octal
        SAT  HDEV+7             ; T = 1547
        IOXT                    ; IOX 1547 - Load ECC Control
        TRA  IIC

        ; Step 7: Clear CWR15 (write base CWR)
        SAA  0                  ; A = 000000 (M0 base)
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - Load Control Word (CWR15=0)
        TRA  IIC

        ; Step 8: Load Core Address (flip-flop: HI8 then LO16)
        SAA  0                  ; A = 0 (upper 8 bits, bits 16-23)
        SAT  HDEV+1             ; T = 1541
        IOXT                    ; IOX 1541 - Core Address HI
        TRA  IIC
        SAA  mem_addr           ; A = memory address bits 0-15
        IOXT                    ; IOX 1541 - Core Address LO
        TRA  IIC

        ; Step 9: Load Word Counter (flip-flop: HI8 then LO16)
        SAA  0                  ; A = 0 (upper 8 bits)
        SAT  HDEV+7             ; T = 1547
        IOXT                    ; IOX 1547 - Word Counter HI
        TRA  IIC
        SAA  word_count         ; A = number of words to transfer
        IOXT                    ; IOX 1547 - Word Counter LO
        TRA  IIC

        ; Step 10: GO = Active(04) | IntEn(01)
        SAA  05                 ; A = 000005 (Active + IntEn)
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - GO!
        TRA  IIC

        ; Step 11: Wait for completion (interrupt or poll)
        ; Option A: Interrupt
        CALL ID11               ; Wait for Level 11 interrupt

        ; Option B: Poll
POLL:   SAT  HDEV+4             ; T = 1544
        IOXT                    ; IOX 1544 - Read Status
        TRA  IIC
        BSTA 2,POLL             ; If bit 2 (Active) still set, loop
```

---

## 11. Complete Write Sequence

**M1-Write: Transfer data from memory to disk**

The base CWR for writes is **004000** (OpCode M1 = 1, bits 11-14 = 0001).

```
Step  IOX Addr  Register              A-register Value         Purpose
----  --------  --------------------  -------------------------  --------------------------
 1    1545      Load Control Word     004000                     Base CWR (M1-Write opcode)
 2    1544      Read Status           (discard value)            Verify idle + RESET FLIP-FLOPS
 3    1543      Load Block Address I  head<<8 | sector           Head and sector (CWR15=0)
 4    1545      Load Control Word     104000                     Set CWR15 (base | 0100000)
 5    1543      Load Block Address II cylinder                   Cylinder number (CWR15=1)
 6    1547      Load ECC Control      000011                     ResetECC + ECC Correction (CWR15=1)
 7    1545      Load Control Word     004000                     Clear CWR15 (back to base)
 8    1541      Load Core Address     000000                     HI 8 bits (flip-flop 1st write)
 8    1541      Load Core Address     memory_address             LO 16 bits (flip-flop 2nd write)
 9    1547      Load Word Counter     000000                     HI 8 bits (flip-flop 1st write)
 9    1547      Load Word Counter     word_count                 LO 16 bits (flip-flop 2nd write)
10    1545      Load Control Word     004005                     GO: base | Active(04) | IntEn(01)
11    1544      Read Status           (poll until Active=0)      Wait for completion
```

### Write Sequence Notes

- **Only difference from read:** the base CWR is 004000 instead of 000000
- The opcode (04000) is present in EVERY CWR write during setup (steps 1, 4, 7, 10)
- Step 10: GO = 004000 | 04 | 01 = 004005
- DMA direction is reversed: controller reads FROM memory, writes TO disk

### Write Sequence as ND-100 Assembly

```asm
; --- M1-Write: Write memory sector to disk ---
; Assumes: T = HDEV (base IOX address, e.g., 1540)

        ; Step 1: Write base CWR (M1 opcode = 04000)
        SAA  04000              ; A = M1-Write base CWR
        IOXT                    ; IOX HDEV+5 (1545) - Load Control Word
        TRA  IIC

        ; Step 2: Read status to verify idle and reset flip-flops
        SAT  HDEV+4             ; T = 1544
        IOXT                    ; IOX 1544 - Read Status -> A
        TRA  IIC

        ; Step 3: Load Block Address I (head/sector), CWR15=0
        SAA  head_sector        ; A = (head << 8) | sector
        SAT  HDEV+3             ; T = 1543
        IOXT                    ; IOX 1543 - Load Block Address I
        TRA  IIC

        ; Step 4: Set CWR15 (carry opcode: 04000 | 0100000 = 0104000)
        SAA  0104000            ; A = M1 base + MUX bit
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - Load Control Word (CWR15=1)
        TRA  IIC

        ; Step 5: Load Block Address II (cylinder), CWR15=1
        SAA  cylinder           ; A = cylinder number
        SAT  HDEV+3             ; T = 1543
        IOXT                    ; IOX 1543 - Load Block Address II
        TRA  IIC

        ; Step 6: Load ECC Control = 011, CWR15=1
        SAA  011                ; A = 011 octal
        SAT  HDEV+7             ; T = 1547
        IOXT                    ; IOX 1547 - Load ECC Control
        TRA  IIC

        ; Step 7: Clear CWR15 (write M1 base CWR)
        SAA  04000              ; A = M1 base (no MUX, no Active)
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - Load Control Word (CWR15=0)
        TRA  IIC

        ; Step 8: Load Core Address (flip-flop: HI8 then LO16)
        SAA  0                  ; A = 0 (upper 8 bits)
        SAT  HDEV+1             ; T = 1541
        IOXT                    ; IOX 1541 - Core Address HI
        TRA  IIC
        SAA  mem_addr           ; A = source memory address bits 0-15
        IOXT                    ; IOX 1541 - Core Address LO
        TRA  IIC

        ; Step 9: Load Word Counter (flip-flop: HI8 then LO16)
        SAA  0                  ; A = 0 (upper 8 bits)
        SAT  HDEV+7             ; T = 1547
        IOXT                    ; IOX 1547 - Word Counter HI
        TRA  IIC
        SAA  word_count         ; A = number of words to write
        IOXT                    ; IOX 1547 - Word Counter LO
        TRA  IIC

        ; Step 10: GO = M1 base | Active(04) | IntEn(01)
        SAA  04005              ; A = 004005
        SAT  HDEV+5             ; T = 1545
        IOXT                    ; IOX 1545 - GO!
        TRA  IIC

        ; Step 11: Wait for completion
        CALL ID11               ; Wait for Level 11 interrupt
```

---

## 12. Seek Sequence

**M4-Seek: Position heads to cylinder without data transfer**

SINTRAN's BSEEK routine (`IP-P2-DISK-START.NPL:436-446`) uses a slightly different pattern because seek doesn't need core address, word counter, or ECC:

```
Step  IOX Addr  Register              Value                      Purpose
----  --------  --------------------  -------------------------  --------------------------
 1    1545      Load Control Word     unit<<7                    Unit select, reset CWR
 2    1543      Load Block Address I  head<<8 | sector           Physical address word 1
 3    1545      Load Control Word     unit<<7 | bit17            Unit select + set CWR15
 4    1543      Load Block Address II cylinder                   Physical address word 2
 5    1545      Load Control Word     unit<<7 | 020004           Initiate seek: M4(020000) + Active(04)
```

After issuing seek, SINTRAN either:
- **Polls** (WSEEK routine): reads RSR (IOX 1544) in a loop checking bit 3 (complete)
- **Waits for interrupt**: enables interrupt + seek complete search (CWR = 030005) then calls ID11

The seek condition register (IOX 1542) tells which unit completed (bits 0-7 bitmask).

---

## 13. Device Clear and Error Recovery

**Device Clear** is CWR bit 4 (value 020 octal). It resets:

- Active flip-flop (stops any operation)
- All controller errors
- Core address register (to 0)
- Block address registers I and II (to 0)
- Word counter (to 0)
- All flip-flops (core address, word counter, ECC)

**SINTRAN uses Device Clear only for error recovery** (appeared once in 17,402 trace lines). Normal I/O setup does NOT issue Device Clear.

For a clean start without destroying registers, write CWR=0 (or CWR=opcode for writes). This clears Active without resetting address registers.

**Error recovery pattern from SINTRAN:**
```
1. Issue DeviceClear (CWR bit 4)
2. Re-load all parameters (block address, core address, word count)
3. Retry the operation
```

---

## 14. Interrupt vs Polling

### 14.1 Interrupt Mode (Normal SINTRAN Operation)

SINTRAN always sets IntEn (CWR bit 0) for data transfers.

```
; Setup and GO with IntEn
SAA  base_cwr | 05          ; Active(04) + IntEn(01)
IOX  1545                    ; GO!

; Wait for Level 11 interrupt
CALL ID11                    ; Blocks until interrupt fires

; On interrupt return:
; - A register contains IDENT code
; - Look up datafield in ITB11[IDENT-1]
; - Read status to check for errors
IOX  1544                    ; Read Status Register
; Check bit 4 (HardwareError)
```

The interrupt fires when Active goes from 1 to 0 (transfer complete). The IDENT code is used to dispatch to the correct controller's datafield via the ITB11 table.

After the interrupt, **SMDReadEnd** in the emulator:
1. Clears Active bit
2. Sets ReadyForTransfer
3. Resets all flip-flops
4. Sets SeekComplete bit for the drive

### 14.2 Polling Mode

Used in SINTRAN's WSEEK routine for seek completion and in boot loaders:

```
; Poll loop
POLL:
    IOX  1544               ; Read Status Register
    ; Check bit 2 (Active) - if set, still busy
    BSTA 2,POLL             ; Branch if Active still set
    ; Check bit 4 (HardwareError) - error occurred
    BSTA 4,ERROR            ; Branch if error
    ; Transfer complete
```

**Status register key bits for polling:**

| Bit | Check | Meaning |
|-----|-------|---------|
| 2 | = 0 | Operation complete (Active cleared) |
| 3 | = 1 | Ready for next transfer |
| 4 | = 1 | Error occurred (check bits 5-8, 13) |
| 14 | = 1 | On cylinder (seek complete) |

### 14.3 Seek Complete with Interrupt

SINTRAN's pattern for seek with interrupt notification:

```npl
; From BSEEK (IP-P2-DISK-START.NPL:465-467)
A:=unit SHZ 7              ; Unit select
A \/ 030005                 ; M6-SeekComplete(030000) + Active(04) + IntEn(01)
T:=HDEV+LCO; *IOXT         ; IOX 1545 - GO!
CALL ID11                   ; Wait for seek complete interrupt

; Read seek condition to determine which unit completed
T:=HDEV+RSC; *IOXT         ; IOX 1542 - Read Seek Condition
; Bits 0-7 contain per-unit seek complete bitmask
```

---

## 15. CHS to LBA Conversion

The SMD controller uses CHS (Cylinder/Head/Sector) addressing. The emulator converts to LBA for file access:

```
LBA = (Cylinder * HeadsPerCylinder + Head) * SectorsPerTrack + Sector
FilePosition = LBA * BytesPerSector
```

**Disk geometries (from `diskSMD.h`):**

| Disk Type | Capacity | Heads | Sectors/Track | Cylinders | Bytes/Sector |
|-----------|----------|-------|---------------|-----------|-------------|
| DISK_38_MB | 38 MB | 5 | 18 | 411 | 1024 |
| DISK_75_MB | 75 MB | 5 | 18 | 823 | 1024 |
| DISK_150_MB | 150 MB | 10 | 18 | 823 | 1024 |
| DISK_288_MB | 288 MB | 19 | 18 | 823 | 1024 |
| DISK_474_MB | 474 MB (Eagle) | 20 | 24+1 | 842 | 1024 |
| DISK_515_MB | 515 MB (FSD) | 24 | 26+1 | 711 | 1024 |
| DISK_825_MB | 825 MB (XMD) | 16 | 44+1 | 1024 | 1024 |

**Word count** must be an integer multiple of words per sector (512 words for 1024-byte sectors) for M0-M3 operations. Maximum transfer is one full cylinder or 16MW (24 bits).

---

## 16. SINTRAN NPL Source Cross-Reference

### Key Routines

| Routine | File | Address | Purpose |
|---------|------|---------|---------|
| CTRDISK | IP-P2-DISK-START.NPL:21 | 053706 | Generic disk driver - parameter extraction |
| STRDISK | IP-P2-DISK-START.NPL:532 | 055665 | Level 11 DMA routine for SMD and ST-506 |
| BSEEK | IP-P2-DISK-START.NPL:412 | 055412 | Initiate seek with parallel seek support |
| WSEEK | IP-P2-DISK-START.NPL:450 | 055473 | Wait/poll for seek completion |
| TOSECT | IP-P2-DISK-START.NPL:388 | 055357 | Convert logical address to CHS |
| DSORT | IP-P2-DISK-START.NPL:270 | 055056 | Elevator sort for disk queue |
| STRNS | MP-P2-DISK-START.NPL:52 | — | Start transfer (monitor level) |
| STRETRANS | MP-P2-DISK-START.NPL:91 | — | Transfer complete (wake program) |

### NPL Register Symbols

From `IP-P2-DISK-START.NPL:416-420`:
```npl
SYMBOL RSC=2,         % Read Seek Condition  (HDEV+2 = IOX 1542)
       LBA=3,         % Load Block Address   (HDEV+3 = IOX 1543)
       RSR=4,         % Read Status Register (HDEV+4 = IOX 1544)
       LCO=5;         % Load Control Word    (HDEV+5 = IOX 1545)
```

### NPL Datafield Variables

| Variable | Purpose |
|----------|---------|
| CTRG | Control register word being built |
| CADRG | Block Address (double word, CWR15 mux) |
| CDRG | Block Address (single word, old format) |
| CXRG | Sector/word count |
| MEMAD | DMA memory address |
| HSTAT | Hardware status after completion |
| HDEV | Base IOX address (1540 for primary SMD) |
| TRGINI | Initial CWR value (saved for retry) |

### Data Flow Through SINTRAN

```
User Program
  -> MON 1/2 (file I/O)
    -> MTRANS (monitor level)
      -> STRNS (allocate queue, activate Level 11)
        -> STRDISK (Level 11: queue management, seek scheduling)
          -> CTRDISK (parameter extraction from queue element)
            -> TRNSF/BDISK (hardware register programming - GO)
              -> ID11 (wait for interrupt)
            -> STRETRANS (completion: check status, wake program)
```

---

## Source Files

| File | Location |
|------|----------|
| nd100x SMD device header | `~/repos/nd100x/src/devices/smd/deviceSMD.h` |
| nd100x SMD device implementation | `~/repos/nd100x/src/devices/smd/deviceSMD.c` |
| nd100x SMD disk types | `~/repos/nd100x/src/devices/smd/diskSMD.h` |
| SINTRAN disk driver (Level 11) | `SINTRAN/NPL-SOURCE/NPL/IP-P2-DISK-START.NPL` |
| SINTRAN disk driver (Monitor) | `SINTRAN/NPL-SOURCE/NPL/MP-P2-DISK-START.NPL` |
| SINTRAN boot trace analysis | `BSD/bsd2nd/docs/porting-log/017-smd-controller-analysis-and-driver-fix.md` |
