# ND-12.055.1 Ethernet II Controller - Comprehensive Unit Test Specification

**Document Version**: 1.0
**Date**: 2025-12-25
**Reference**: ND-12.055.1 EN Ethernet II Controller Manual

---

## Table of Contents

1. [Overview](#1-overview)
2. [Test Environment Setup](#2-test-environment-setup)
3. [Initialization Tests](#3-initialization-tests)
4. [Ethernet Control Register Tests](#4-ethernet-control-register-tests)
5. [Ethernet Status Register Tests](#5-ethernet-status-register-tests)
6. [I/O Address Space Tests](#6-io-address-space-tests)
7. [LANCE Integration Tests](#7-lance-integration-tests)
8. [Interrupt System Tests](#8-interrupt-system-tests)
9. [Memory System Tests](#9-memory-system-tests)
10. [Loopback Mode Tests](#10-loopback-mode-tests)
11. [Transceiver Control Tests](#11-transceiver-control-tests)
12. [Error Handling Tests](#12-error-handling-tests)
13. [COSMOS Statistics Tests](#13-cosmos-statistics-tests)

---

## 1. Overview

This document specifies comprehensive unit tests for the ND-12.055.1 Ethernet II Controller. The controller interfaces an ND-100 computer to an Ethernet network using:

- MC68000 local processor (10 MHz)
- Am7990 LANCE (Local Area Network Controller for Ethernet)
- Am7992B SIA (Serial Interface Adapter)
- MC68901 MFP (Multi-Function Peripheral)
- 512KB local DRAM with parity

### Hardware Architecture

```
ND-100 Bus <-> ND-100 Interface <-> 68000 <-> LANCE <-> SIA <-> Transceiver <-> Ethernet
                                      |
                                     MFP
                                      |
                                    DRAM (512KB)
```

### Register Access Methods

| Access Type | Method |
|-------------|--------|
| Ethernet Control/Status | IOXT instruction with T register |
| I/O Space (EF0000-EF01FF) | 68000 memory-mapped I/O |
| LANCE CSR | Via RDP (EF00A0) and RAP (EF00A2) |

---

## 2. Test Environment Setup

### Required Components

```
MockND100Bus          - Simulates ND-100 bus interface
MockMemory            - 512KB DRAM simulation with parity
MockLANCE             - Am7990 LANCE simulation
MockMFP               - MC68901 MFP simulation
MockTransceiver       - Ethernet transceiver simulation
EthernetIIController  - Device under test
```

### Base Addresses

| Component | Address |
|-----------|---------|
| I/O Space Base | 0xEF0000 |
| LANCE RDP | 0xEF00A0 |
| LANCE RAP | 0xEF00A2 |
| MFP Base | 0xEF00C0 |
| DRAM Base | 0x000000 |
| DRAM Size | 0x080000 (512KB) |

### Ethernet Device Numbers (Thumbwheel 12J)

| Setting | Device Number (Octal) | Ident Code (Octal) |
|---------|----------------------|-------------------|
| 0 | 140360 | 140034 |
| 1 | 140364 | 140035 |
| 2 | 140370 | 140036 |
| 3 | 140374 | 140037 |

---

## 3. Initialization Tests

### ETHII-INIT-001: Master Clear Reset

**Description**: Verify Master Clear pulse from ND-100 properly resets the controller.

**Preconditions**:
- Controller is powered and running
- 68000 is executing code

**Test Steps**:
1. Assert Master Clear signal from ND-100
2. Wait for Master Clear pulse duration (>50us)
3. Deassert Master Clear
4. Read Ethernet Status Register

**Expected Results**:
- Bit 5 (HALT) = 1 (68000 halted)
- Bit 4 (RESET) = 1 (reset active)
- LED marked 'RESET' on card is illuminated (red)
- LED marked 'HALT' on card is illuminated (red)
- All local I/O activity stopped

**Pass Criteria**: Status register shows HALT=1 and RESET=1

**Register Reference**: Ethernet Status Register bits 4-5

---

### ETHII-INIT-002: Controller Startup Sequence

**Description**: Verify ND-100 can start controller by clearing HALT and RESET bits.

**Preconditions**:
- Controller has been reset (HALT=1, RESET=1)

**Test Steps**:
1. Read Ethernet Status Register, verify HALT=1, RESET=1
2. Write to Ethernet Control Register with HALT=0, RESET=0
3. Wait for 68000 boot sequence (fetch from DRAM address 0)
4. Read Ethernet Status Register

**Expected Results**:
- After write: 68000 fetches system stack pointer from DRAM[0x00-0x03]
- After write: 68000 fetches restart address from DRAM[0x04-0x07]
- Status shows HALT=0, RESET=0
- 68000 begins executing firmware

**Pass Criteria**: Controller starts and Status register shows HALT=0, RESET=0

**Register Reference**: Ethernet Control Register bits 4-5, Ethernet Status Register bits 4-5

---

### ETHII-INIT-003: Power-On Reset State

**Description**: Verify controller enters correct state after power-on.

**Preconditions**:
- Controller power is applied

**Test Steps**:
1. Apply power to controller
2. Wait for power stabilization
3. Read Ethernet Status Register
4. Read MODCR registers (EF0020-EF0026)

**Expected Results**:
- Status register bit 5 (HALT) = 1
- Status register bit 4 (RESET) = 1
- EPROMMODE (EF0020) = 0
- PARITYDIS (EF0022) = 0
- BREAKMODE (EF0024) = 0
- SPARE (EF0026) = 0

**Pass Criteria**: All registers in documented power-on state

**Register Reference**: Ethernet Status Register, MODCR registers

---

### ETHII-INIT-004: Delayed Clear Pulse

**Description**: Verify SCIP register is reset 200us after power low.

**Preconditions**:
- Controller is running normally
- SCIP has been written (interrupt pending)

**Test Steps**:
1. Write to SCIP address range to set interrupt
2. Assert power low signal
3. Wait exactly 200us
4. Verify SCIP register has been cleared

**Expected Results**:
- SCIP cleared 200us after power low
- No spurious interrupts to ND-100

**Pass Criteria**: SCIP cleared at correct timing

**Register Reference**: SCIP (EF0080-EF009F)

---

## 4. Ethernet Control Register Tests

### ETHII-CTRL-001: INIT Bit (Bit 0) - Enable SCIP Interrupt

**Description**: Test SCIP interrupt enable/disable via bit 0.

**Preconditions**:
- Controller is running (HALT=0, RESET=0)

**Test Steps**:
1. Write 0x0001 to Ethernet Control Register (SCIPEN=1)
2. Write to SCIP address range
3. Verify ND-100 receives level 12 interrupt
4. Write 0x0000 to Ethernet Control Register (SCIPEN=0)
5. Write to SCIP address range
6. Verify no interrupt generated

**Expected Results**:
- With SCIPEN=1: SCIP write generates ND-100 level 12 interrupt
- With SCIPEN=0: SCIP write does NOT generate interrupt

**Pass Criteria**: Interrupt behavior matches SCIPEN state

**Register Reference**: Ethernet Control Register bit 0

---

### ETHII-CTRL-002: ND Interrupt Bit (Bit 2)

**Description**: Test ND-100 to 68000 interrupt via bit 2.

**Preconditions**:
- Controller is running
- 68000 interrupt handler is installed at level 3

**Test Steps**:
1. Install 68000 interrupt handler for MFP interrupt (vector 116)
2. Write 0x0004 to Ethernet Control Register (NDINT=1)
3. Verify 68000 receives level 3 interrupt
4. Verify interrupt vector 116 is used
5. Clear interrupt by writing 0x0000

**Expected Results**:
- 68000 receives interrupt on level 3
- MFP interrupt vector 116 is generated
- Interrupt can be cleared by writing NDINT=0

**Pass Criteria**: Interrupt delivered to 68000 via MFP

**Register Reference**: Ethernet Control Register bit 2, MFP vector 116

---

### ETHII-CTRL-003: Start OPCOM Bit (Bit 3)

**Description**: Test OPCOM start signal to 68000.

**Preconditions**:
- Controller is running

**Test Steps**:
1. Write 0x0008 to Ethernet Control Register (OPCOM=1)
2. Verify 68000 receives level 6 interrupt
3. Verify OPCOM handler is invoked

**Expected Results**:
- 68000 receives interrupt on level 6
- Direct OPCOM communication enabled

**Pass Criteria**: Level 6 interrupt delivered

**Register Reference**: Ethernet Control Register bit 3, 68000 interrupt level 6

---

### ETHII-CTRL-004: RESET Bit (Bit 4)

**Description**: Test software reset of 68000 via bit 4.

**Preconditions**:
- Controller is running (RESET=0)

**Test Steps**:
1. Verify Status register RESET=0
2. Write 0x0010 to Ethernet Control Register (RESET=1)
3. Read Ethernet Status Register
4. Verify 68000 is in reset state
5. Write 0x0000 to clear RESET
6. Verify 68000 resumes

**Expected Results**:
- Setting RESET=1 puts 68000 in reset
- Status register reflects RESET=1
- RED LED marked 'RESET' illuminates
- Clearing RESET=0 allows 68000 to restart

**Pass Criteria**: RESET bit controls 68000 reset state

**Register Reference**: Ethernet Control Register bit 4, Ethernet Status Register bit 4

---

### ETHII-CTRL-005: HALT Bit (Bit 5)

**Description**: Test software halt of 68000 via bit 5.

**Preconditions**:
- Controller is running (HALT=0)

**Test Steps**:
1. Verify Status register HALT=0
2. Write 0x0020 to Ethernet Control Register (HALT=1)
3. Read Ethernet Status Register
4. Verify 68000 is halted
5. Write 0x0000 to clear HALT
6. Verify 68000 resumes

**Expected Results**:
- Setting HALT=1 halts 68000
- Status register reflects HALT=1
- RED LED marked 'HALT' illuminates
- Clearing HALT=0 allows 68000 to resume

**Pass Criteria**: HALT bit controls 68000 halt state

**Register Reference**: Ethernet Control Register bit 5, Ethernet Status Register bit 5

---

### ETHII-CTRL-006: Power Low Bit (Bit 6)

**Description**: Test power low signal to 68000.

**Preconditions**:
- Controller is running
- 68000 power-low handler installed

**Test Steps**:
1. Write 0x0040 to Ethernet Control Register (PWRLOW=1)
2. Verify 68000 receives level 7 interrupt
3. Verify power-low handler saves registers
4. Verify HALT and RESET driven low within 50us

**Expected Results**:
- Level 7 (highest priority) interrupt to 68000
- Handler saves all 68000 registers
- Controller enters safe state within 50us

**Pass Criteria**: Power-low sequence executes correctly

**Register Reference**: Ethernet Control Register bit 6, 68000 interrupt level 7

---

### ETHII-CTRL-007: Disable Check Bit (Bit 8)

**Description**: Test parity check disable for external parity testing.

**Preconditions**:
- Controller memory contains valid parity

**Test Steps**:
1. Write 0x0100 to Ethernet Control Register (DISCHK=1)
2. Write data with incorrect parity to DRAM
3. Read the data back
4. Verify NO parity error is reported
5. Write 0x0000 to re-enable parity check
6. Read the same data
7. Verify parity error IS reported

**Expected Results**:
- With DISCHK=1: Parity errors ignored
- With DISCHK=0: Parity errors reported
- 'PERR' LED behavior matches

**Pass Criteria**: DISCHK controls parity checking

**Register Reference**: Ethernet Control Register bit 8

**Note**: This test should only be run by stand-alone programs, not under SINTRAN.

---

### ETHII-CTRL-008: Unused Bits Behavior

**Description**: Verify unused bits (1, 7, 9-15) have no effect.

**Preconditions**:
- Controller is in known state

**Test Steps**:
1. Record current state
2. Write 0xFE82 (all unused bits set)
3. Verify controller state unchanged
4. Read Status register
5. Verify no side effects

**Expected Results**:
- Writing to unused bits has no effect
- Controller operation unchanged

**Pass Criteria**: Unused bits are ignored

**Register Reference**: Ethernet Control Register bits 1, 7, 9-15

---

## 5. Ethernet Status Register Tests

### ETHII-STAT-001: Bank Number Reading (Bits 15-8)

**Description**: Verify bank number can be read correctly.

**Preconditions**:
- Thumbwheels 7J and 9J set to known values

**Test Steps**:
For each thumbwheel combination (0,0) through (1,15):
1. Set thumbwheels to test value
2. Read Ethernet Status Register
3. Extract bits 15-8
4. Verify bank number matches expected value
5. Verify bits 8 and 9 are always 0 (512KB boundary)

**Expected Results**:

| Thumbwheel 7J | Thumbwheel 9J | Expected Bank | Physical Pages (hex) |
|---------------|---------------|---------------|----------------------|
| 0 | 0-3 | 0 | 000-0FF |
| 0 | 4-7 | 4 | 100-1FF |
| 0 | 8-11 | 8 | 200-2FF |
| 0 | 12-15 | 12 | 300-3FF |
| 1 | 0 | 16 | 400-4FF |

- Bits 8-9 always = 0 (controller starts on 512KB boundary)

**Pass Criteria**: Bank number matches thumbwheel setting

**Register Reference**: Ethernet Status Register bits 8-15

---

### ETHII-STAT-002: Memory Size Bit (Bit 6)

**Description**: Verify memory size bit is always 0 (512KB).

**Preconditions**:
- Controller with 512KB DRAM

**Test Steps**:
1. Read Ethernet Status Register
2. Extract bit 6
3. Verify bit 6 = 0

**Expected Results**:
- Bit 6 always = 0 (512KB memory)

**Pass Criteria**: Bit 6 = 0

**Register Reference**: Ethernet Status Register bit 6

---

### ETHII-STAT-003: HALT Status (Bit 5)

**Description**: Verify HALT status reflects 68000 state.

**Preconditions**:
- Controller can be halted/resumed

**Test Steps**:
1. Ensure 68000 is running
2. Read Status register, verify HALT=0
3. Write HALT=1 to Control register
4. Read Status register, verify HALT=1
5. Write HALT=0 to Control register
6. Read Status register, verify HALT=0

**Expected Results**:
- Status HALT bit accurately reflects 68000 halt state

**Pass Criteria**: Status bit matches Control register setting

**Register Reference**: Ethernet Status Register bit 5

---

### ETHII-STAT-004: RESET Status (Bit 4)

**Description**: Verify RESET status reflects 68000 state.

**Preconditions**:
- Controller can be reset/started

**Test Steps**:
1. Ensure 68000 is running
2. Read Status register, verify RESET=0
3. Write RESET=1 to Control register
4. Read Status register, verify RESET=1
5. Write RESET=0 to Control register
6. Read Status register, verify RESET=0

**Expected Results**:
- Status RESET bit accurately reflects 68000 reset state

**Pass Criteria**: Status bit matches Control register setting

**Register Reference**: Ethernet Status Register bit 4

---

### ETHII-STAT-005: Interrupt Set Status (Bit 2)

**Description**: Verify interrupt set status for ND-100 level 12.

**Preconditions**:
- SCIP interrupt enabled (SCIPEN=1)

**Test Steps**:
1. Ensure no pending interrupt (bit 2 = 0)
2. Write to SCIP address range
3. Read Status register, verify bit 2 = 1
4. Service the interrupt
5. Read Status register, verify bit 2 = 0

**Expected Results**:
- Bit 2 = 1 when interrupt pending for ND-100
- Bit 2 = 0 after interrupt serviced

**Pass Criteria**: Interrupt status accurate

**Register Reference**: Ethernet Status Register bit 2

---

### ETHII-STAT-006: Interrupt Enable Status (Bit 0)

**Description**: Verify interrupt enable status onto ND-100 bus.

**Preconditions**:
- Controller running

**Test Steps**:
1. Write SCIPEN=0 to Control register
2. Read Status register, verify bit 0 = 0
3. Write SCIPEN=1 to Control register
4. Read Status register, verify bit 0 = 1

**Expected Results**:
- Bit 0 reflects SCIPEN setting from Control register

**Pass Criteria**: Enable status matches control setting

**Register Reference**: Ethernet Status Register bit 0

---

## 6. I/O Address Space Tests

### ETHII-IO-001: Address Decode Mirroring

**Description**: Verify I/O addresses are decoded twice (EF00XX = EF01XX).

**Preconditions**:
- Controller running

**Test Steps**:
For each I/O register:
1. Write to EF00XX address
2. Read from EF01XX address
3. Verify same value
4. Write to EF01XX address
5. Read from EF00XX address
6. Verify same value

**Expected Results**:
- EF00A0 and EF01A0 access same register
- EF00C0 and EF01C0 access same register
- All mirrored pairs work correctly

**Pass Criteria**: Mirroring works for all registers

**Register Reference**: I/O address space

---

### ETHII-IO-002: PROFF Register (EF0010-EF001F)

**Description**: Test protection off register.

**Preconditions**:
- Memory protection enabled
- Controller in supervisor mode

**Test Steps**:
1. Verify protected memory cannot be written
2. Write 0x0001 to PROFF address
3. Verify protected memory CAN now be written
4. Write 0x0000 to PROFF address
5. Verify protected memory cannot be written again

**Expected Results**:
- PROFF=1 bypasses protection table
- PROFF=0 enforces protection table

**Pass Criteria**: Protection bypass works

**Register Reference**: PROFF (EF0010-EF001F)

---

### ETHII-IO-003: MODCR Registers (EF0020-EF003F)

**Description**: Test all MODCR sub-registers.

**Preconditions**:
- Controller running

**Test Steps**:

For EPROMMODE (EF0020):
1. Read, verify = 0 after reset
2. Write 1, read back, verify = 1
3. Write 0, read back, verify = 0

For PARITYDIS (EF0022):
1. Read, verify = 0 after reset
2. Write 1, read back, verify = 1
3. Verify parity generation disabled
4. Write 0, read back, verify = 0

For BREAKMODE (EF0024):
1. Read, verify = 0 after reset
2. Write 1, read back, verify = 1
3. Write 0, read back, verify = 0

For SPARE (EF0026):
1. Read, verify = 0 after reset
2. Write 1, read back, verify = 1
3. Write 0, read back, verify = 0

**Expected Results**:
- All registers clear after reset
- All registers read/write correctly

**Pass Criteria**: All MODCR registers work as specified

**Register Reference**: MODCR (EF0020-EF003F)

---

### ETHII-IO-004: MERRSTAT Register (EF0040-EF005F)

**Description**: Test parity error status register.

**Preconditions**:
- Controller running

**Test Steps**:
1. Read MERRSTAT register
2. Verify format: bits 15,11 = 0, bits 5,4 = 0
3. Force a parity error
4. Read MERRSTAT
5. Verify error information:
   - Bit 10: Write to parity (0=enabled, 1=disabled)
   - Bit 9: Address bit 18
   - Bit 8: Address bit 17
   - Bit 7: NGACK (error source)
   - Bit 6: BGACK (error source)
   - Bit 3: Parity error in high byte
   - Bit 2: Parity error in low byte
   - Bit 1: Parity bit read with high byte
   - Bit 0: Parity bit read with low byte

**Expected Results**:

Error source decoding:
| NGACK (bit 7) | BGACK (bit 6) | Device |
|---------------|---------------|--------|
| 0 | 0 | ND-100 |
| 0 | 1 | None |
| 1 | 0 | LANCE |
| 1 | 1 | 68000 |

**Pass Criteria**: Error status correctly reported

**Register Reference**: MERRSTAT (EF0040-EF005F)

---

### ETHII-IO-005: EAREN Register (EF0060-EF007F)

**Description**: Test error address enable register.

**Preconditions**:
- Controller running

**Test Steps**:
1. Force a memory error at known address
2. Read EAREN register
3. Verify address bits A1-A16 match error location
4. Force error at different address
5. Verify new address reported

**Expected Results**:
- Error address (bits A1-A16) correctly captured
- Address available on 68000 data bus

**Pass Criteria**: Error address correctly captured

**Register Reference**: EAREN (EF0060-EF007F)

---

### ETHII-IO-006: SCIP Register (EF0080-EF009F)

**Description**: Test Status Change In PIOC register.

**Preconditions**:
- SCIPEN enabled in Control register

**Test Steps**:
1. Verify no pending interrupt (Status bit 2 = 0)
2. Write any value to SCIP address range
3. Verify ND-100 level 12 interrupt generated
4. Verify Status register bit 2 = 1
5. Service interrupt
6. Verify Status register bit 2 = 0

**Expected Results**:
- Writing to SCIP generates ND-100 level 12 interrupt
- Interrupt persists until serviced

**Pass Criteria**: SCIP interrupt mechanism works

**Register Reference**: SCIP (EF0080-EF009F)

---

### ETHII-IO-007: LANCE Registers (EF00A0-EF00A7)

**Description**: Test LANCE access via RDP and RAP.

**Preconditions**:
- Controller running

**Test Steps**:
1. Write 0 to RAP (select CSR0)
2. Read RDP, verify CSR0 value
3. Write 1 to RAP (select CSR1)
4. Write test value to RDP
5. Read RDP, verify value
6. Write 2 to RAP (select CSR2)
7. Write test value to RDP
8. Read RDP, verify value
9. Write 3 to RAP (select CSR3)
10. Write 0x0007 to RDP
11. Read RDP, verify 0x0007

**Expected Results**:
- RAP selects CSR register (0-3)
- RDP reads/writes selected CSR
- Only even addresses used (16-bit access)

**Pass Criteria**: All LANCE CSRs accessible

**Register Reference**: LANCE (EF00A0, EF00A2)

---

### ETHII-IO-008: XCVPW Register (EF00A8-EF00AF)

**Description**: Test transceiver power control.

**Preconditions**:
- Controller running

**Test Steps**:
1. Write 0 to XCVPW
2. Verify transceiver 12V power OFF
3. Verify ETHSTAT bit 2 reflects power state
4. Write 1 to XCVPW
5. Verify transceiver 12V power ON
6. Verify ETHSTAT bit 2 reflects power state
7. Verify yellow LED (8) reflects power state

**Expected Results**:
- XCVPW=0: Transceiver power OFF
- XCVPW=1: Transceiver power ON (12V)
- Yellow LED (8) illuminates when power ON

**Pass Criteria**: Transceiver power control works

**Register Reference**: XCVPW (EF00A8-EF00AF)

---

### ETHII-IO-009: LANRESET Register (EF00B0-EF00B7)

**Description**: Test LANCE hardware reset.

**Preconditions**:
- LANCE is initialized and running

**Test Steps**:
1. Initialize LANCE (set CSR0 INIT, wait for IDON)
2. Start LANCE (set CSR0 STRT)
3. Verify CSR0 shows TXON, RXON set
4. Write any value to LANRESET address
5. Read CSR0
6. Verify LANCE in reset state (STOP set)

**Expected Results**:
- Writing to LANRESET resets LANCE
- CSR0 shows STOP=1 after reset

**Pass Criteria**: LANCE hardware reset works

**Register Reference**: LANRESET (EF00B0-EF00B7)

---

### ETHII-IO-010: ETHSTAT Register (EF00B8-EF00BF)

**Description**: Test hardware status register.

**Preconditions**:
- Controller running

**Test Steps**:
1. Read ETHSTAT
2. Extract bit 2 (power enable, active low)
3. Extract bit 0 (LAN interrupt, active low)
4. Enable transceiver power
5. Read ETHSTAT, verify bit 2 = 0
6. Disable transceiver power
7. Read ETHSTAT, verify bit 2 = 1
8. Generate LANCE interrupt
9. Read ETHSTAT, verify bit 0 = 0
10. Clear LANCE interrupt
11. Read ETHSTAT, verify bit 0 = 1

**Expected Results**:
- Bit 2 = 0 when power enabled (active low)
- Bit 2 = 1 when power disabled
- Bit 0 = 0 when LAN interrupt active (active low)
- Bit 0 = 1 when no LAN interrupt

**Pass Criteria**: Hardware status correctly reported

**Register Reference**: ETHSTAT (EF00B8-EF00BF)

---

### ETHII-IO-011: MFP Registers (EF00C0-EF00FF)

**Description**: Test MFP (68901) register access.

**Preconditions**:
- Controller running

**Test Steps**:
1. Verify only ODD addresses access MFP (base + 1, 3, 5, ...)
2. Read GPIP (EF00C1)
3. Write/Read DDR (EF00C5)
4. Write/Read IERA (EF00C7)
5. Write/Read IERB (EF00C9)
6. Read VR (EF00D9), verify vector base
7. Access all 24 MFP registers via displacement 1-55 (1-37 hex)

**Expected Results**:
- MFP accessible at odd addresses only
- All 24 registers readable/writable as per MC68901 spec

**Pass Criteria**: All MFP registers accessible

**Register Reference**: MFP (EF00C0-EF00FF)

---

## 7. LANCE Integration Tests

### ETHII-LANCE-001: CSR0 STOP Bit

**Description**: Verify STOP bit behavior.

**Preconditions**:
- LANCE is running

**Test Steps**:
1. Write 0x0004 to CSR0 (STOP=1)
2. Read CSR0
3. Verify STOP=1, TXON=0, RXON=0
4. Verify all other CSRs become writable

**Expected Results**:
- STOP=1 halts all LANCE activity
- TXON and RXON cleared
- CSR1, CSR2, CSR3 become writable

**Pass Criteria**: STOP bit functions correctly

**Register Reference**: LANCE CSR0 bit 2

---

### ETHII-LANCE-002: CSR0 STRT Bit

**Description**: Verify START bit behavior.

**Preconditions**:
- LANCE is initialized (IDON=1)

**Test Steps**:
1. Verify IDON=1 in CSR0
2. Write 0x0002 to CSR0 (STRT=1)
3. Read CSR0
4. Verify STRT=0 (self-clearing)
5. Verify TXON=1 (unless MODE.DTX=1)
6. Verify RXON=1 (unless MODE.DRX=1)

**Expected Results**:
- STRT enables transmitter and receiver
- STRT bit self-clears
- TXON/RXON set unless disabled by MODE

**Pass Criteria**: START sequence works

**Register Reference**: LANCE CSR0 bits 1, 4, 5

---

### ETHII-LANCE-003: CSR0 INIT Bit

**Description**: Verify INIT bit and initialization sequence.

**Preconditions**:
- LANCE is stopped
- Init block configured in DRAM

**Test Steps**:
1. Configure init block at known address:
   - MODE word at offset 0
   - PADR (6 bytes) at offset 2
   - LADRF (8 bytes) at offset 8
   - RDRA (4 bytes) at offset 16
   - TDRA (4 bytes) at offset 20
2. Write init block address to CSR1 and CSR2
3. Write 0x0001 to CSR0 (INIT=1)
4. Poll CSR0 for IDON=1
5. Verify INIT=0 (self-clearing)
6. Verify MODE, PADR, LADRF loaded correctly

**Expected Results**:
- INIT triggers initialization sequence
- IDON=1 when complete
- INIT self-clears
- All init block values loaded

**Pass Criteria**: Initialization completes successfully

**Register Reference**: LANCE CSR0 bits 0, 8

---

### ETHII-LANCE-004: CSR0 Interrupt Flags

**Description**: Verify all CSR0 interrupt flags.

**Preconditions**:
- LANCE is running with INEA=1

**Test Steps**:

For RINT (bit 10):
1. Configure receive ring
2. Receive a valid packet
3. Verify RINT=1, INTR=1
4. Write 1 to RINT to clear
5. Verify RINT=0

For TINT (bit 9):
1. Configure transmit ring
2. Transmit a packet
3. Verify TINT=1, INTR=1
4. Write 1 to TINT to clear
5. Verify TINT=0

For IDON (bit 8):
1. Perform initialization
2. Verify IDON=1
3. Write 1 to IDON to clear
4. Verify IDON=0

For MERR (bit 11):
1. Cause memory timeout (25.6us)
2. Verify MERR=1, ERR=1
3. Write 1 to MERR to clear

For MISS (bit 12):
1. Fill all receive buffers (OWN=0)
2. Receive a packet
3. Verify MISS=1, ERR=1

For CERR (bit 13):
1. Force collision error
2. Verify CERR=1, ERR=1

For BABL (bit 14):
1. Transmit >1518 bytes
2. Verify BABL=1, ERR=1

**Expected Results**:
- Each flag set on appropriate condition
- INTR=1 when any interrupt flag set with INEA=1
- ERR=1 when BABL, CERR, MISS, or MERR set
- Flags clear when written with 1

**Pass Criteria**: All interrupt flags work correctly

**Register Reference**: LANCE CSR0 bits 8-15

---

### ETHII-LANCE-005: CSR1/CSR2 Init Block Address

**Description**: Verify init block address registers.

**Preconditions**:
- LANCE is stopped

**Test Steps**:
1. Write 0xABCD to CSR1 (low 16 bits)
2. Write 0x0012 to CSR2 (high 8 bits, bits 23:16)
3. Read CSR1, verify 0xABCD
4. Read CSR2, verify 0x0012
5. Perform INIT
6. Verify LANCE reads from address 0x12ABCD

**Expected Results**:
- CSR1 holds bits 15:0 of address
- CSR2 holds bits 23:16 of address
- Address masked to INIT_ADDR_MASK (0xFFFFFE)

**Pass Criteria**: Init block address correctly set

**Register Reference**: LANCE CSR1, CSR2

---

### ETHII-LANCE-006: CSR3 Bus Control

**Description**: Verify CSR3 bus master interface bits.

**Preconditions**:
- LANCE is stopped

**Test Steps**:

For BCON (bit 0):
1. Write 0x0001 to CSR3
2. Read CSR3, verify BCON=1
3. Verify byte control behavior on DMA

For ACON (bit 1):
1. Write 0x0002 to CSR3
2. Read CSR3, verify ACON=1
3. Verify ALE timing behavior

For BSWP (bit 2):
1. Write 0x0004 to CSR3
2. Read CSR3, verify BSWP=1
3. Verify byte swap on DMA transfers

**Expected Results**:
- Only bits 0-2 are significant
- Higher bits masked off (CSR3_MASK = 0x0007)

**Pass Criteria**: CSR3 controls bus interface correctly

**Register Reference**: LANCE CSR3 bits 0-2

---

### ETHII-LANCE-007: Ring Buffer Setup

**Description**: Verify receive and transmit ring configuration.

**Preconditions**:
- Init block with ring descriptors configured

**Test Steps**:
1. Configure RDRA in init block:
   - Bits 15:0 = ring base address low
   - Bits 23:16 = ring base address high
   - Bits 31:29 = RLEN (ring length = 2^RLEN entries)
2. Configure TDRA similarly
3. Perform INIT
4. Verify ring lengths: 1, 2, 4, 8, 16, 32, 64, or 128 entries

For each valid RLEN (0-7):
1. Set RLEN in RDRA/TDRA
2. Perform INIT
3. Verify ring wraps at correct boundary

**Expected Results**:

| RLEN | Ring Entries |
|------|--------------|
| 0 | 1 |
| 1 | 2 |
| 2 | 4 |
| 3 | 8 |
| 4 | 16 |
| 5 | 32 |
| 6 | 64 |
| 7 | 128 |

**Pass Criteria**: Ring buffers configured correctly

**Register Reference**: Init block RDRA, TDRA fields

---

### ETHII-LANCE-008: Receive Descriptor Test

**Description**: Verify receive message descriptor (RMD) handling.

**Preconditions**:
- LANCE initialized and started
- Receive ring configured

**Test Steps**:
1. Set up RMD with OWN=1 (LANCE owns)
2. Configure RMD0 = buffer address low
3. Configure RMD1 = (OWN | HADR)
4. Configure RMD2 = buffer size (2's complement)
5. Receive a packet
6. Read RMD1:
   - Verify OWN=0 (host owns now)
   - Verify STP=1 (start of packet)
   - Verify ENP=1 (end of packet, single buffer)
7. Read RMD3:
   - Verify MCNT = actual received bytes

**Expected Results**:
- OWN cleared after receive
- STP/ENP indicate packet boundaries
- MCNT contains message byte count

**Pass Criteria**: RMD correctly updated

**Register Reference**: RMD0-RMD3 fields

---

### ETHII-LANCE-009: Transmit Descriptor Test

**Description**: Verify transmit message descriptor (TMD) handling.

**Preconditions**:
- LANCE initialized and started
- Transmit ring configured

**Test Steps**:
1. Set up TMD with OWN=1, STP=1, ENP=1
2. Configure TMD0 = buffer address low
3. Configure TMD1 = (OWN | STP | ENP | HADR)
4. Configure TMD2 = buffer size (2's complement)
5. Set TDMD in CSR0
6. Wait for transmission
7. Read TMD1:
   - Verify OWN=0 (host owns now)
8. Read TMD3 for any error info

**Expected Results**:
- OWN cleared after transmit
- TMD3 contains error status if any

**Pass Criteria**: TMD correctly updated

**Register Reference**: TMD0-TMD3 fields

---

### ETHII-LANCE-010: TDMD Transmit Demand

**Description**: Verify transmit demand bit.

**Preconditions**:
- LANCE started
- Transmit ring has pending packet (OWN=1)

**Test Steps**:
1. Configure TMD with packet to transmit
2. Read CSR0, note transmitter state
3. Write TDMD=1 to CSR0
4. Verify LANCE immediately polls transmit ring
5. Verify packet transmitted without waiting for poll timer

**Expected Results**:
- TDMD forces immediate transmit ring poll
- TDMD self-clears

**Pass Criteria**: Transmit demand works

**Register Reference**: LANCE CSR0 bit 3

---

## 8. Interrupt System Tests

### ETHII-INT-001: 68000 Level 7 - Power Low

**Description**: Verify power low interrupt handling.

**Preconditions**:
- 68000 interrupt handler installed for level 7

**Test Steps**:
1. Install level 7 handler
2. Assert power low signal
3. Verify interrupt on level 7 (highest priority)
4. Verify handler saves all registers
5. Verify HALT and RESET driven low within 50us

**Expected Results**:
- Level 7 is non-maskable
- Handler executes immediately
- Controller enters safe state

**Pass Criteria**: Power low interrupt works

**Register Reference**: 68000 interrupt level 7

---

### ETHII-INT-002: 68000 Level 6 - OPCOM

**Description**: Verify OPCOM interrupt handling.

**Preconditions**:
- 68000 running
- OPCOM handler installed

**Test Steps**:
1. Install level 6 handler
2. Set OPCOM bit in Ethernet Control Register
3. Verify 68000 receives level 6 interrupt
4. Verify direct ND-100 communication available

**Expected Results**:
- Level 6 interrupt generated
- OPCOM path established

**Pass Criteria**: OPCOM interrupt works

**Register Reference**: 68000 interrupt level 6

---

### ETHII-INT-003: 68000 Level 5 - Parity Error

**Description**: Verify parity error interrupt handling.

**Preconditions**:
- 68000 running
- Parity error handler installed
- PARITYDIS=0 (parity enabled)

**Test Steps**:
1. Install level 5 handler
2. Disable parity write (PARITYDIS=1)
3. Write data with bad parity
4. Enable parity check (PARITYDIS=0)
5. Read the bad data
6. Verify level 5 interrupt
7. Read PARITYDIS in handler
8. If BREAKMODE=1, treat as breakpoint

**Expected Results**:
- Level 5 interrupt on parity error
- Handler can distinguish error from breakpoint

**Pass Criteria**: Parity error interrupt works

**Register Reference**: 68000 interrupt level 5, PARITYDIS, BREAKMODE

---

### ETHII-INT-004: 68000 Level 4 - Test Console

**Description**: Verify PTC console interrupt handling.

**Preconditions**:
- PTC connected to 10-pin connector
- 68000 running

**Test Steps**:
1. Install level 4 handler via MFP
2. Send character from PTC
3. Verify level 4 interrupt
4. Read character from MFP USART

**Expected Results**:
- Level 4 interrupt from PTC via MFP
- USART data accessible

**Pass Criteria**: PTC interrupt works

**Register Reference**: 68000 interrupt level 4

---

### ETHII-INT-005: 68000 Level 3 - MFP

**Description**: Verify MFP interrupt handling.

**Preconditions**:
- 68000 running
- MFP configured

**Test Steps**:

For each MFP interrupt source:
1. Enable interrupt in MFP IERA/IERB
2. Trigger interrupt condition
3. Verify level 3 interrupt to 68000
4. Verify correct vector used

| MFP Source | Vector | Condition |
|------------|--------|-----------|
| I7 (Write violation) | 117 | Write to protected memory |
| I6 (ND-100 interrupt) | 116 | NDINT set in Control register |
| Timer C | 105 | RTC tick |
| USART Rx buffer full | 114 | Character received |
| USART Rx error | 113 | Receive error |
| USART Tx buffer empty | 112 | Ready to transmit |
| USART Tx error | 111 | Transmit error |
| I5 (LANCE error) | 107 | LANCE memory access error |

**Expected Results**:
- Each MFP source generates level 3 interrupt
- Correct vector used for each source

**Pass Criteria**: All MFP interrupts work

**Register Reference**: 68000 interrupt level 3, MFP vectors

---

### ETHII-INT-006: 68000 Level 2 - LANCE

**Description**: Verify LANCE interrupt handling.

**Preconditions**:
- LANCE initialized and started
- INEA=1 in CSR0

**Test Steps**:
1. Install level 2 handler
2. Cause LANCE interrupt (e.g., receive packet)
3. Verify level 2 interrupt to 68000
4. Read CSR0 to determine interrupt source
5. Clear interrupt by writing 1 to flag bit
6. Verify interrupt clears

**Expected Results**:
- Level 2 interrupt when LANCE INTR=1
- Source identifiable via CSR0 flags

**Pass Criteria**: LANCE interrupt works

**Register Reference**: 68000 interrupt level 2, LANCE CSR0

---

### ETHII-INT-007: ND-100 Level 12 - SCIP

**Description**: Verify SCIP interrupt to ND-100.

**Preconditions**:
- Controller running
- SCIPEN=1

**Test Steps**:
1. Install ND-100 level 12 handler
2. Enable SCIP (SCIPEN=1)
3. Write to SCIP address range from 68000
4. Verify ND-100 receives level 12 interrupt
5. Read Ethernet Status Register from ND-100
6. Verify bit 2 = 1 (interrupt set)
7. Service interrupt
8. Verify bit 2 = 0

**Expected Results**:
- ND-100 receives level 12 interrupt
- Status register reflects interrupt state

**Pass Criteria**: SCIP interrupt works

**Register Reference**: SCIP, Ethernet Status Register bit 2

---

### ETHII-INT-008: Interrupt Priority

**Description**: Verify interrupt priority handling.

**Preconditions**:
- Multiple interrupt sources enabled

**Test Steps**:
1. Trigger level 2 interrupt (LANCE)
2. While handling, trigger level 5 (parity)
3. Verify level 5 preempts level 2
4. While handling level 5, trigger level 7 (power low)
5. Verify level 7 preempts level 5
6. Return from level 7
7. Verify return to level 5
8. Return from level 5
9. Verify return to level 2

**Expected Results**:
- Higher priority interrupts preempt lower
- Correct return sequence

**Pass Criteria**: Priority nesting works correctly

---

## 9. Memory System Tests

### ETHII-MEM-001: DRAM Size Verification

**Description**: Verify 512KB DRAM is accessible.

**Preconditions**:
- Controller running

**Test Steps**:
1. Write test pattern to address 0x000000
2. Read back and verify
3. Write test pattern to address 0x07FFFE (last word)
4. Read back and verify
5. Write different patterns throughout
6. Verify no address aliasing

**Expected Results**:
- Full 512KB (0x000000-0x07FFFF) accessible
- No aliasing or overlap

**Pass Criteria**: Full memory accessible

**Register Reference**: DRAM address space

---

### ETHII-MEM-002: DRAM Access Priority

**Description**: Verify access priority: ND-100 > LANCE > 68000.

**Preconditions**:
- All three bus masters active

**Test Steps**:
1. Configure LANCE for continuous DMA
2. Have 68000 attempt memory access
3. Initiate ND-100 memory access
4. Verify ND-100 access completes first
5. Verify LANCE access completes before 68000
6. Measure access latencies

**Expected Results**:
- ND-100 has highest priority
- LANCE has medium priority
- 68000 has lowest priority

**Pass Criteria**: Priority order verified

**Register Reference**: DRAM access

---

### ETHII-MEM-003: Byte Parity Generation

**Description**: Verify byte parity on memory writes.

**Preconditions**:
- Parity enabled (PARITYDIS=0)

**Test Steps**:
1. Write 0x00 to memory (even parity)
2. Read MERRSTAT, verify correct parity bits
3. Write 0xFF to memory (even parity)
4. Read MERRSTAT, verify correct parity bits
5. Write 0x55 to memory (even parity)
6. Verify parity bit stored
7. Write 0xAA to memory (even parity)
8. Verify parity bit stored

**Expected Results**:
- Parity generated for each byte
- Correct parity based on data

**Pass Criteria**: Parity generation correct

**Register Reference**: MERRSTAT bits 0-1

---

### ETHII-MEM-004: Parity Error Detection (ND-100)

**Description**: Verify ND-100 parity error detection.

**Preconditions**:
- Controller running

**Test Steps**:
1. Disable parity write (DISCHK=1)
2. Write data with intentionally bad parity
3. Enable parity check (DISCHK=0)
4. Have ND-100 read the data
5. Verify parity error signal on bus
6. Verify 'PERR' LED illuminates
7. Read MERRSTAT to identify error source

**Expected Results**:
- Parity error detected
- PERR LED on
- MERRSTAT shows NGACK=0, BGACK=0 (ND-100)

**Pass Criteria**: ND-100 parity error detected

**Register Reference**: MERRSTAT, PERR LED

**Note**: Only run in stand-alone programs, not SINTRAN.

---

### ETHII-MEM-005: Parity Error Detection (68000)

**Description**: Verify 68000 parity error detection.

**Preconditions**:
- Controller running
- Level 5 handler installed

**Test Steps**:
1. Write 1 to PARITYDIS
2. Write data to memory (bad parity generated)
3. Write 0 to PARITYDIS
4. Have 68000 read the data
5. Verify level 5 interrupt
6. Read MERRSTAT in handler
7. Verify NGACK=1, BGACK=1 (68000)

**Expected Results**:
- Level 5 interrupt to 68000
- MERRSTAT correctly identifies 68000

**Pass Criteria**: 68000 parity error detected

**Register Reference**: MERRSTAT bits 6-7

---

### ETHII-MEM-006: Parity Error Detection (LANCE)

**Description**: Verify LANCE parity error detection.

**Preconditions**:
- LANCE doing DMA
- Intentionally bad parity in buffer

**Test Steps**:
1. Create buffer with bad parity
2. Configure LANCE to read from buffer
3. Start LANCE DMA
4. Verify MERR set in CSR0
5. Read MERRSTAT
6. Verify NGACK=1, BGACK=0 (LANCE)

**Expected Results**:
- LANCE detects parity error
- CSR0 MERR set
- MERRSTAT identifies LANCE

**Pass Criteria**: LANCE parity error detected

**Register Reference**: LANCE CSR0 MERR, MERRSTAT

---

### ETHII-MEM-007: Memory Protection Table

**Description**: Verify SRAM protection table.

**Preconditions**:
- Controller running in user mode

**Test Steps**:
1. Clear protection for segment at address X
2. Write to address X
3. Verify write succeeds
4. Set protection for segment at address X
5. Attempt write to address X
6. Verify bus error (write protect violation)
7. Verify level 3 interrupt (MFP vector 117)

Protection table addressing:
- DRAM divided into 512-byte segments
- Each segment has 1 bit in SRAM
- SRAM address = 15360K + segment_base

**Expected Results**:
- Unprotected segments writable
- Protected segments generate bus error
- MFP vector 117 interrupt generated

**Pass Criteria**: Protection table works

**Register Reference**: Protection table, MFP vector 117

---

### ETHII-MEM-008: Supervisor Mode Bypass

**Description**: Verify supervisor mode bypasses protection.

**Preconditions**:
- Protection enabled for test segment

**Test Steps**:
1. Set protection for segment X
2. Switch 68000 to user mode
3. Attempt write to segment X
4. Verify write fails (bus error)
5. Switch 68000 to supervisor mode
6. Attempt write to segment X
7. Verify write succeeds

**Expected Results**:
- User mode respects protection
- Supervisor mode bypasses protection

**Pass Criteria**: Supervisor bypass works

**Register Reference**: 68000 supervisor mode

---

### ETHII-MEM-009: PROFF Protection Override

**Description**: Verify PROFF register bypasses protection.

**Preconditions**:
- Protection enabled for test segment

**Test Steps**:
1. Set protection for segment X
2. Verify write to X fails
3. Write 1 to PROFF
4. Verify write to X succeeds
5. Write 0 to PROFF
6. Verify write to X fails again

**Expected Results**:
- PROFF=1 bypasses all protection
- PROFF=0 restores protection

**Pass Criteria**: PROFF override works

**Register Reference**: PROFF (EF0010-EF001F)

---

### ETHII-MEM-010: Power Failure Memory Preservation

**Description**: Verify DRAM contents preserved on power failure.

**Preconditions**:
- Standby power available

**Test Steps**:
1. Write known pattern to DRAM
2. Trigger power failure sequence
3. Wait for Master Clear
4. Wait for power restore
5. Read DRAM contents
6. Verify pattern preserved

**Expected Results**:
- DRAM contents intact after power cycle
- DRAM refresh maintained by standby power

**Pass Criteria**: Memory preserved

**Register Reference**: Power failure handling

---

## 10. Loopback Mode Tests

### ETHII-LOOP-001: Internal Loopback Enable

**Description**: Verify internal loopback mode.

**Preconditions**:
- LANCE initialized

**Test Steps**:
1. Set MODE.LOOP=1 and MODE.INTL=1 in init block
2. Perform initialization
3. Start LANCE
4. Configure transmit descriptor
5. Transmit packet
6. Verify packet appears in receive ring
7. Verify external transceiver NOT used

**Expected Results**:
- Packet loops back internally
- No external transmission
- Receive ring receives transmitted data

**Pass Criteria**: Internal loopback works

**Register Reference**: LANCE MODE LOOP (bit 2), INTL (bit 6)

---

### ETHII-LOOP-002: External Loopback Enable

**Description**: Verify external loopback mode.

**Preconditions**:
- LANCE initialized
- Loopback connector on transceiver

**Test Steps**:
1. Set MODE.LOOP=1 and MODE.INTL=0 in init block
2. Perform initialization
3. Start LANCE
4. Configure transmit descriptor
5. Transmit packet
6. Verify packet goes through external path
7. Verify packet received via SIA

**Expected Results**:
- Packet transmitted externally
- Packet loops back through transceiver
- Packet received normally

**Pass Criteria**: External loopback works

**Register Reference**: LANCE MODE LOOP (bit 2), INTL (bit 6)

---

### ETHII-LOOP-003: Loopback Packet Size

**Description**: Verify loopback packet size limits.

**Preconditions**:
- Internal loopback enabled

**Test Steps**:
1. Transmit 8-byte packet (minimum for loopback)
2. Verify received correctly
3. Transmit 32-byte packet (maximum for internal loopback without FCS)
4. Verify received correctly
5. Attempt 7-byte packet
6. Verify error reported
7. Attempt 33-byte packet (without FCS)
8. Verify error reported

**Expected Results**:
- 8-32 bytes valid for internal loopback (without FCS)
- Outside range generates error

**Pass Criteria**: Size limits enforced

**Register Reference**: LANCE loopback

---

### ETHII-LOOP-004: CRC Logic Check

**Description**: Verify CRC generation/check in loopback.

**Preconditions**:
- Internal loopback enabled
- MODE.DTCR=0 (CRC enabled)

**Test Steps**:
1. Transmit packet with CRC generation enabled
2. Receive packet
3. Verify no CRC error in RMD1
4. Corrupt received CRC
5. Verify CRC error detected

**Expected Results**:
- Valid CRC passes check
- Invalid CRC detected

**Pass Criteria**: CRC logic verified

**Register Reference**: LANCE MODE DTCR, RMD1 CRC bit

---

### ETHII-LOOP-005: Force Collision Mode

**Description**: Verify forced collision in internal loopback.

**Preconditions**:
- Internal loopback enabled
- MODE.COLL=1, MODE.INTL=1

**Test Steps**:
1. Set MODE.LOOP=1, MODE.COLL=1, MODE.INTL=1
2. Perform initialization
3. Start LANCE
4. Attempt transmission
5. Verify TMD1.ERR=1
6. Verify TMD3.RTRY=1

**Expected Results**:
- Transmission fails with retry error
- No actual transmission occurs

**Pass Criteria**: Forced collision works

**Register Reference**: LANCE MODE COLL (bit 4), TMD3 RTRY

---

## 11. Transceiver Control Tests

### ETHII-XCV-001: Transceiver Power Enable

**Description**: Verify transceiver 12V power control.

**Preconditions**:
- Controller running

**Test Steps**:
1. Write 0 to XCVPW
2. Measure transceiver power (should be 0V)
3. Read ETHSTAT, verify bit 2 = 1 (power off, active low)
4. Write 1 to XCVPW
5. Measure transceiver power (should be 12V)
6. Read ETHSTAT, verify bit 2 = 0 (power on, active low)
7. Verify yellow LED (8) illuminates

**Expected Results**:
- XCVPW controls 12V power to transceiver
- ETHSTAT reflects power state
- Yellow LED indicates power state

**Pass Criteria**: Power control works

**Register Reference**: XCVPW, ETHSTAT bit 2

---

### ETHII-XCV-002: Current Switch Protection

**Description**: Verify current switch disconnects on overload.

**Preconditions**:
- Transceiver power enabled

**Test Steps**:
1. Enable transceiver power
2. Simulate short circuit condition
3. Verify current switch disconnects power
4. Verify ETHSTAT bit 2 = 1 (power off)

**Expected Results**:
- Automatic power disconnect on overload
- ETHSTAT reflects disconnect

**Pass Criteria**: Overcurrent protection works

**Register Reference**: XCVPW, ETHSTAT bit 2

---

### ETHII-XCV-003: Low 5V Protection

**Description**: Verify transceiver disconnect on low 5V.

**Preconditions**:
- Transceiver power enabled

**Test Steps**:
1. Enable transceiver power
2. Reduce 5V supply below threshold
3. Verify transceiver power disconnected

**Expected Results**:
- Transceiver disconnected on low 5V

**Pass Criteria**: Low voltage protection works

---

### ETHII-XCV-004: Jabber Detection

**Description**: Verify jabber detection and power-off.

**Preconditions**:
- LANCE running
- Transceiver powered

**Test Steps**:
1. Start transmission
2. Simulate hanging transmitter (no end-of-frame)
3. Wait for jabber timeout
4. Verify power-off command issued
5. Verify BABL set in CSR0

**Expected Results**:
- Jabber detected
- Transceiver power disabled
- BABL interrupt generated

**Pass Criteria**: Jabber protection works

**Register Reference**: LANCE CSR0 BABL

---

### ETHII-XCV-005: Heartbeat Check

**Description**: Verify heartbeat (SQE) monitoring.

**Preconditions**:
- LANCE running
- Transceiver powered

**Test Steps**:
1. Transmit frame successfully
2. Verify heartbeat (SQE) received within 2us
3. Simulate missing heartbeat
4. Verify CERR set in CSR0

**Expected Results**:
- Missing heartbeat detected
- CERR reported

**Pass Criteria**: Heartbeat monitoring works

**Register Reference**: LANCE CSR0 CERR

---

## 12. Error Handling Tests

### ETHII-ERR-001: Babbling Transmitter

**Description**: Verify babbling transmitter detection.

**Preconditions**:
- LANCE running

**Test Steps**:
1. Configure transmit buffer > 1518 bytes
2. Set STP=1, ENP=0 on first descriptor
3. Chain multiple descriptors totaling > 1518 bytes
4. Start transmission
5. Verify transmission aborted
6. Verify CSR0 BABL=1, ERR=1
7. Verify TINT=1

**Expected Results**:
- Transmission aborted at 1518 bytes
- BABL error reported
- Transmitter timeout

**Pass Criteria**: Babble detection works

**Register Reference**: LANCE CSR0 BABL

---

### ETHII-ERR-002: Collision Detection

**Description**: Verify collision error handling.

**Preconditions**:
- LANCE running

**Test Steps**:
1. Simulate collision during transmission
2. Verify jam pattern transmitted
3. Verify LANCE waits random backoff
4. Verify retry occurs
5. Simulate 16 collisions
6. Verify CERR set and RTRY in TMD3
7. Verify transmission aborted

**Expected Results**:
- Backoff and retry on collision
- Maximum 16 retries
- Error after excessive collisions

**Pass Criteria**: Collision handling works

**Register Reference**: LANCE CSR0 CERR, TMD3 RTRY

---

### ETHII-ERR-003: Late Collision

**Description**: Verify late collision detection.

**Preconditions**:
- LANCE running

**Test Steps**:
1. Start transmission
2. Simulate collision after slot time (51.2us)
3. Verify packet written to receive Silo
4. Verify CRC error bit set
5. Verify LCOL set in TMD3

**Expected Results**:
- Late collision detected
- Packet flagged with error
- LCOL reported

**Pass Criteria**: Late collision detection works

**Register Reference**: TMD3 LCOL

---

### ETHII-ERR-004: Memory Timeout

**Description**: Verify memory timeout error.

**Preconditions**:
- LANCE doing DMA

**Test Steps**:
1. Configure DMA to non-existent address
2. Start LANCE operation
3. Wait for 25.6us timeout
4. Verify CSR0 MERR=1, ERR=1
5. Verify transmitter and receiver turned off

**Expected Results**:
- Memory timeout after 25.6us
- MERR error reported
- LANCE stops

**Pass Criteria**: Memory timeout detected

**Register Reference**: LANCE CSR0 MERR

---

### ETHII-ERR-005: Missed Packet

**Description**: Verify missed packet detection.

**Preconditions**:
- LANCE running
- All receive buffers owned by host (OWN=0)

**Test Steps**:
1. Mark all receive descriptors OWN=0
2. Receive incoming packet
3. Verify CSR0 MISS=1, ERR=1
4. Verify packet discarded
5. Verify MISS statistic incremented

**Expected Results**:
- Packet missed due to no buffer
- MISS error reported

**Pass Criteria**: Missed packet detected

**Register Reference**: LANCE CSR0 MISS

---

### ETHII-ERR-006: CRC Error

**Description**: Verify CRC error detection.

**Preconditions**:
- LANCE running

**Test Steps**:
1. Receive packet with corrupted FCS
2. Verify RMD1 CRC=1, ERR=1
3. Verify packet delivered but flagged

**Expected Results**:
- CRC error detected
- Packet available with error flag

**Pass Criteria**: CRC error detected

**Register Reference**: RMD1 CRC

---

### ETHII-ERR-007: Framing Error

**Description**: Verify framing (alignment) error detection.

**Preconditions**:
- LANCE running

**Test Steps**:
1. Receive packet with non-byte-aligned length
2. Verify RMD1 FRAM=1, CRC=1
3. Verify packet flagged

**Expected Results**:
- Framing error detected
- Also appears as CRC error (octet error)

**Pass Criteria**: Framing error detected

**Register Reference**: RMD1 FRAM

---

### ETHII-ERR-008: Overflow Error

**Description**: Verify receive FIFO overflow detection.

**Preconditions**:
- LANCE receiving
- DMA intentionally delayed

**Test Steps**:
1. Delay DMA response
2. Receive large packet rapidly
3. Verify RMD1 OFLO=1
4. Verify part of packet lost

**Expected Results**:
- FIFO overflow detected
- Packet truncated

**Pass Criteria**: Overflow detected

**Register Reference**: RMD1 OFLO

---

### ETHII-ERR-009: Underflow Error

**Description**: Verify transmit FIFO underflow detection.

**Preconditions**:
- LANCE transmitting
- DMA intentionally delayed

**Test Steps**:
1. Configure large transmit buffer
2. Delay DMA during transmission
3. Verify TMD3 UFLO=1
4. Verify packet truncated

**Expected Results**:
- FIFO underflow detected
- Transmission aborted

**Pass Criteria**: Underflow detected

**Register Reference**: TMD3 UFLO

---

### ETHII-ERR-010: Buffer Error

**Description**: Verify buffer error detection.

**Preconditions**:
- LANCE running

**Test Steps**:

For receive:
1. Chain receive buffers
2. Ensure next buffer OWN=0 before complete
3. Verify RMD1 BUFF=1

For transmit:
1. Chain transmit buffers
2. Ensure next buffer OWN=0 or no ENP
3. Verify TMD3 BUFF=1

**Expected Results**:
- Buffer chaining error detected

**Pass Criteria**: Buffer error detected

**Register Reference**: RMD1 BUFF, TMD3 BUFF

---

### ETHII-ERR-011: Loss of Carrier

**Description**: Verify loss of carrier detection.

**Preconditions**:
- LANCE transmitting

**Test Steps**:
1. Start transmission
2. Remove carrier signal (RENA) during transmission
3. Verify TMD3 LCAR=1
4. Verify packet transmitted but flagged
5. Verify no retry attempted

**Expected Results**:
- Carrier loss detected
- Transmission completes but errors
- No retry on carrier loss

**Pass Criteria**: Carrier loss detected

**Register Reference**: TMD3 LCAR

---

### ETHII-ERR-012: Write Protection Violation

**Description**: Verify write protection violation handling.

**Preconditions**:
- Memory protection enabled for test segment

**Test Steps**:
1. Enable protection for segment X
2. Attempt write from 68000 user mode
3. Verify bus error
4. Verify MFP interrupt vector 117
5. Verify level 3 interrupt to 68000

**Expected Results**:
- Write blocked
- Bus error generated
- MFP vector 117 delivered

**Pass Criteria**: Write protection works

**Register Reference**: Protection table, MFP vector 117

---

## 13. COSMOS Statistics Tests

### ETHII-STAT-001: Frame Transmit Statistics

**Description**: Verify transmit statistics tracking.

**Preconditions**:
- Controller running COSMOS

**Test Steps**:
1. Transmit N frames successfully
2. Query "frames transmitted successfully"
3. Verify count = N
4. Transmit frame requiring 1 retry
5. Verify "after one collision" increments
6. Transmit frame requiring multiple retries
7. Verify "after multiple collisions" increments
8. Cause 16 collisions on single frame
9. Verify "frames aborted" increments

**Expected Results**:
- All transmit counters accurate
- Categories mutually exclusive

**Pass Criteria**: Transmit stats correct

**Register Reference**: TMD flags

---

### ETHII-STAT-002: Frame Receive Statistics

**Description**: Verify receive statistics tracking.

**Preconditions**:
- Controller running COSMOS

**Test Steps**:
1. Receive N valid frames
2. Query "frames received and given to user"
3. Verify count = N
4. Fill ENNS buffers, receive frame
5. Verify "received and dropped" increments
6. Clear all receive descriptors, receive frame
7. Verify "missed" increments

**Expected Results**:
- All receive counters accurate

**Pass Criteria**: Receive stats correct

**Register Reference**: RMD flags, CSR0 MISS

---

### ETHII-STAT-003: Error Statistics

**Description**: Verify error statistics tracking.

**Preconditions**:
- Controller running COSMOS

**Test Steps**:
For each error type, cause error and verify counter:

| Error Type | How to Cause | Counter |
|------------|--------------|---------|
| CRC | Corrupt FCS | "CRC errors" |
| Alignment | Non-byte boundary | "alignment errors" |
| FIFO overflow | Slow DMA | "FIFO overflows" |
| Buffer overflow | No next buffer | "buffer overflows" |
| Carrier loss | Remove RENA | "loss of carrier" |
| Underflow | Slow DMA | "transmit underflow" |
| Late collision | After 51.2us | "late collision" |
| Heartbeat | No SQE | "missing heartbeat" |
| Jabber | >1518 bytes | "jabber detected" |
| Memory error | Timeout | "memory error" |

**Expected Results**:
- Each error type counted separately
- Counters accurate

**Pass Criteria**: Error stats correct

**Register Reference**: CSR0, TMD, RMD flags

---

### ETHII-STAT-004: Restart Statistics

**Description**: Verify restart counter.

**Preconditions**:
- Controller running COSMOS

**Test Steps**:
1. Note current "restarts" count
2. Issue ND-100 restart command
3. Verify "restarts" increments

**Expected Results**:
- Restart counted accurately

**Pass Criteria**: Restart stat correct

---

## Appendix A: Test Data Patterns

### Memory Test Patterns

| Pattern | Value | Purpose |
|---------|-------|---------|
| All zeros | 0x0000 | Stuck-at-1 detection |
| All ones | 0xFFFF | Stuck-at-0 detection |
| Checkerboard | 0x5555 | Adjacent bit coupling |
| Inverse checkerboard | 0xAAAA | Adjacent bit coupling |
| Walking 1 | 0x0001, 0x0002, ... | Individual bit test |
| Walking 0 | 0xFFFE, 0xFFFD, ... | Individual bit test |
| Address as data | addr | Address decode test |

### Ethernet Test Frames

Minimum frame:
```
Destination: FF:FF:FF:FF:FF:FF (broadcast)
Source: 08:00:26:xx:xx:00 (ND format)
Type: 0x0800
Data: 46 bytes padding
FCS: 4 bytes CRC32
Total: 64 bytes
```

Maximum frame:
```
Destination: 6 bytes
Source: 6 bytes
Type: 2 bytes
Data: 1500 bytes
FCS: 4 bytes
Total: 1518 bytes
```

---

## Appendix B: Register Quick Reference

### Ethernet Control Register (Write)

```
Bit 15-9: Unused
Bit 8:    DISCHK - Disable check bit
Bit 7:    Unused
Bit 6:    PWRLOW - Power low signal
Bit 5:    HALT - Halt 68000
Bit 4:    RESET - Reset 68000
Bit 3:    OPCOM - Start OPCOM
Bit 2:    NDINT - ND interrupt to 68000
Bit 1:    Unused
Bit 0:    SCIPEN - Enable SCIP interrupt
```

### Ethernet Status Register (Read)

```
Bit 15-8: BANK - Bank number
Bit 7:    Unused
Bit 6:    MEM512K - Always 0
Bit 5:    HALT - Halt status
Bit 4:    RESET - Reset status
Bit 3:    Unused
Bit 2:    INTSET - Interrupt set for ND-100
Bit 1:    Unused
Bit 0:    INTEN - Interrupt enabled
```

### LANCE CSR0 Flags

```
Bit 15: ERR  - Error (BABL|CERR|MISS|MERR)
Bit 14: BABL - Babble
Bit 13: CERR - Collision error
Bit 12: MISS - Missed packet
Bit 11: MERR - Memory error
Bit 10: RINT - Receive interrupt
Bit 9:  TINT - Transmit interrupt
Bit 8:  IDON - Initialization done
Bit 7:  INTR - Interrupt flag
Bit 6:  INEA - Interrupt enable
Bit 5:  RXON - Receiver on
Bit 4:  TXON - Transmitter on
Bit 3:  TDMD - Transmit demand
Bit 2:  STOP - Stop
Bit 1:  STRT - Start
Bit 0:  INIT - Initialize
```

---

## Appendix C: Timing Requirements

| Parameter | Value |
|-----------|-------|
| Master Clear duration | >50us |
| Power-low to safe state | <50us |
| Delayed Clear pulse | 200us after power low |
| Memory timeout | 25.6us |
| Slot time (collision window) | 51.2us |
| Heartbeat (SQE) timeout | 2us |
| Address mismatch detection | 4.8us |
| Runt packet threshold | 51.2us |

---

**End of Document**

Full path: `E:\Dev\Ronny\NDInsight\Reference-Manuals\Devices\ETHII-UnitTests.md`
