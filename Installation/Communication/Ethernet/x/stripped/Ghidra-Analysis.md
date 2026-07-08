# ND Ethernet II Controller (PCB 3094) - 68000 Firmware Ghidra Analysis Guide

This document tells an LLM (or a human) how to load the extracted 68000
firmware into Ghidra, lay out the memory map, annotate the I/O and mailbox
regions, and reference the interrupt vectors, so the driver can be reversed
cleanly. Everything below is cross-checked between three sources; each fact
is tagged VERIFIED (agrees across the manual and the emulator source, or was
checked against the bytes) or ASSUMPTION.

## Source references

- **Firmware binary** (this folder): `encos-ser-all-banks-68k.bin` (512KB) -
  the combined driver. Byte-identical to a live controller RAM capture
  (see [README.md](README.md) for extraction and verification).
- **Per-bank binaries** (this folder): `encos-ser-b0-68k.bin` .. `b3`.
- **Emulator source (authoritative for I/O behavior)**:
  `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`
  (contains the full 68000 memory map, I/O decode, mailbox layout, MFP/LANCE
  wiring, and interrupt architecture as C# with extensive comments).
- **Hardware manual**: `..\..\..\..\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`
  (registers, thumbwheels, bank mapping, MFP vectors). NDWiki: 3094.
- **PD sheet / install doc**: `..\ND-210580-02-EN.md`
- **Chips**: AMD Am7990 LANCE (Ethernet), Motorola MC68901 MFP
  (USART/timers/interrupt controller), MC68000 @ 10 MHz.

---

## 1. Board summary (VERIFIED)

The Ethernet II controller is an intelligent board: a **MC68000** with **no
EPROM** (EPROM was never fitted). All firmware is uploaded from the ND-100
into the controller's 512KB DRAM before the 68000 is released from reset.
HW designer Arve Maoy; firmware author Ronald Gordon Jones.

- CPU: MC68000, 10 MHz, big-endian, 24-bit address bus. HALT and RESET are
  driven directly by the ND-100 via the Ethernet control register.
- Local memory: 512KB DRAM (256K x 18 with byte parity), plus 1Kbit SRAM.
- Ethernet: AMD Am7990 LANCE + SIA + transceiver.
- MFP: MC68901 - serial test console (PTC/USART), 4 timers (Timer C = RTC),
  and the interrupt controller for 16 sources.

The ND-100 sees the DRAM as an ordinary ND-100 memory bank (mapped as a
PIOC / "Ether" memory range from the SINTRAN generation MMPIOCS table). Two
thumbwheels place that bank in ND-100 physical address space.

---

## 2. Ghidra import parameters (VERIFIED)

- Format: Raw Binary
- Processor / Language: **68000 : big endian** (`68000:BE:32:default`)
- Base / load address: **0x00000000**
- File: `encos-ser-all-banks-68k.bin` (load the whole 512KB; the flat layout
  is verified against the live RAM capture, so cross-bank references resolve)

Initial disassembly anchors from the reset vector (VERIFIED from bytes):

| Address | Meaning |
|---------|---------|
| 0x000000 | Initial SSP = **0x000005C8** (vector 0, long) |
| 0x000004 | Initial PC  = **0x00001CFE** (vector 1, long) - disassemble here first |

After import: define 0x000000-0x0003FF as the 256-entry 68000 vector table
(64 longwords), then disassemble from 0x1CFE.

Note (ASSUMPTION): the firmware author's own disassembly (per the C# comments)
uses a file called `pioc-ether-68k-code.bin` with the SAME addresses cited
below (0x25F0, 0x30CA, 0x4610, 0x57F2, etc.), so those C# address annotations
line up 1:1 with this image at base 0. Confirm opportunistically as you go.

---

## 3. 68000 memory map (VERIFIED - manual + emulator agree)

Set these up as Ghidra memory blocks so references resolve and the I/O
region is not mistaken for code/data. `NDBusEthernetII.cs` (class
`NDEthernetMemory`, methods `FindMemoryBank` / `ReadMemory` / `WriteMemory`)
is the authority for the decode.

| 68000 range | Block name | R/W | Contents |
|-------------|------------|-----|----------|
| 0x000000-0x07FFFF | DRAM | RW | 512KB local DRAM: vectors, code, data, LANCE buffers. Loaded by ND-100. |
| 0x080000-0xEEFFFF | (unmapped) | - | Spare / EPROM option (never fitted). Access = bus error. |
| 0xEF0000-0xEF01FF | IO | RW | Device I/O space (see section 4). EF01xx mirrors EF00xx (bit 8 ignored). |
| 0xF00000-0xF7FFFF | PROTECT | RW | Protection table (per-page write protect for DRAM). |
| 0xF80000-0xFFFFFF | RAM_IMAGE | RW | Mirror of DRAM 0x000000-0x07FFFF (the ND-100 shared window; LANCE + 68000 also see it). |

Reset/probe behavior (VERIFIED from emulator): a read or write beyond the
512KB DRAM raises a 68000 **bus error** (vector 2, at 0x08). The firmware
uses this deliberately to size RAM at startup - do not treat early bus-error
handling as a fault path.

---

## 4. I/O space EF0000-EF01FF (VERIFIED - manual page 37, emulator ETH_IOMem)

Decoded twice: `0xEF00xx` == `0xEF01xx` (address bit 8 ignored), so PIOC and
Ethernet I firmware both work. Exception: the emulator treats `0xEF0180` as
the SCIP mirror of `0x0080`, and notes a separate timer controller around
`0xEF0100-0xEF0121`.

| Address range | Name | R/W | Function |
|---------------|------|-----|----------|
| 0xEF0000-0xEF000F | (not used) | - | - |
| 0xEF0010-0xEF001F | PROFF | W | Protection Off - writing 1 makes the 68000 ignore the protect table |
| 0xEF0020-0xEF003F | MODCR | RW | Mode control. 0x20=EPROMMODE, 0x22=PARITYDIS, 0x24=BREAKMODE, 0x26=SPARE (single-bit each, cleared on reset) |
| 0xEF0040-0xEF005F | MERRSTAT | R | Memory/parity error status (see manual page 39) |
| 0xEF0060-0xEF007F | EAREN | R | Error Address Enable - returns A1-A16 of a memory error on the 68000 bus |
| 0xEF0080-0xEF009F | SCIP | W | Status Change In PIOC - a write triggers **INT12 to the ND-100** |
| 0xEF00A0-0xEF00A7 | LANCE | RW | Am7990: 0xEF00A0 = RDP (Register Data Port), 0xEF00A2 = RAP (Register Address Port). 16-bit, even addresses. |
| 0xEF00A8-0xEF00AF | XCVPW | W | Transceiver 12V power switch (1=on, 0=off) |
| 0xEF00B0-0xEF00B7 | LANRESET | W | LANCE hardware reset (any access triggers) |
| 0xEF00B8-0xEF00BF | ETHSTAT | R | Hardware status: bit2=power enable, bit0=LAN interrupt (significant when ZERO) |
| 0xEF00C0-0xEF00FF | MFP (MC68901) | RW | MFP registers, base 0xEF00C0 + displacement 1-0x37, **odd addresses only** |
| 0xEF0100-0xEF0121 | TIMER | RW | Separate timer controller (AM9513-like); 0xEF0120 status is read-to-clear (ASSUMPTION: per emulator, not in manual excerpt) |
| 0xEF0180 | SCIP (mirror) | W | Write 0x0001 here to raise INT12 to ND-100 (firmware's preferred path) |

MFP register displacement note (VERIFIED from emulator `InitializeMFPFromFirmware`):
the firmware's MFP init (function at 0x25F0) writes VR=0x40, IERA=IERB=0xFF,
timer prescale/data regs, USART UCR=0x88, etc. MFP register index = (addr -
0xEF00C0) >> 1. The firmware also touches the MFP via 0xEF0080 as a GPIP base
(the emulator remaps 0xEF0080-BF to the MFP by +0x40).

---

## 5. Exception vectors / interrupt architecture (VERIFIED - manual page 28, emulator)

68000 autovector layout in the vector table (all in DRAM, loaded by ND-100):

| 68000 level | Vector addr | Source | Vector type | Meaning |
|-------------|-------------|--------|-------------|---------|
| Reset | 0x000000/04 | - | - | SSP + PC |
| 2 | 0x000008 | LANCE | AutoVector | Ethernet packet TX/RX |
| 3 | 0x00000C | MFP (and ND-100) | **Vectored** | MFP interrupt sources (see below) |
| 4 | 0x000010 | Test Console (PTC) | AutoVector | Serial debug |
| 5 | 0x000014 | MERR | AutoVector | Memory parity error |
| 6 | 0x000018 | ND-100 OPCOM | AutoVector | ND-100 OPCOM request; also timer/RTC (vector 0x1E) |
| 7 | 0x00001C | Power low | AutoVector (NMI) | Power failure from ND-100 |

MFP vectored interrupts (level 3). The manual lists these "system vector"
labels; the firmware programs MFP VR=0x40 and installs handlers at
`(VR & 0xF0) | channel`, so the ACTUAL vector numbers are 0x40-0x4F, not the
decimal labels below (VERIFIED from emulator comments - treat the decimal
column as documentation labels, the hex as what you'll see in the table):

| Manual label (octal) | Source | MFP channel | Actual vector (VR-based) |
|----------------------|--------|-------------|--------------------------|
| 117 | Write violation by 68000 (GPIO I7) | 7 | 0x4F |
| 116 | ND-100 requesting interrupt (GPIO I6) | 6 | 0x4E |
| 114 | USART receive buffer full | - | (USART RX) |
| 113 | USART receive error | - | (USART RX err) |
| 112 | USART transmit buffer empty | - | (USART TX) |
| 111 | USART transmit error | - | (USART TX err) |
| 107 | LANCE memory access error (GPIO I5) | 5 | 0x45 |
| 105 | Real-time clock (Timer C) | - | (Timer C) |

GPIO active-low signals: I5=/LANERROR, I6=/NCINT (ND-100 int), I7=/WRIV
(write violation). I0-I4 not connected.

---

## 6. Ethernet control/status registers (ND-100 side) (VERIFIED - manual pages 44-45)

These are NOT in the 68000 address space - they are the ND-100's IOX device
registers. Included because the driver's ND-100 half (segment ENCOSE0,
program ENNS0) drives them, and they gate the 68000. Accessed via IOXT with
the device base in T.

**Ethernet control register** (WRITE, device base + 1 or + 3):

| bit | function |
|-----|----------|
| 8 | disable check bit (parity) |
| 6 | power low |
| 5 | halt |
| 4 | reset |
| 3 | start OPCOM (-> 68000 level 6) |
| 2 | ND interrupt (-> 68000 MFP GPIO I6, vector 0x4E) |
| 0 | enable SCIP interrupt |

The ND-100 releases the 68000 by writing this register with halt=0 and
reset=0. Coming out of reset, the 68000 fetches SSP+PC from the first 8 bytes
of DRAM.

**Ethernet status register** (READ, device base + 0 or + 2):

| bit | function |
|-----|----------|
| 15-8 | bank number (bits 8,9 always 0 - starts on half-MB boundary) |
| 6 | memory is 512 Kbytes (always 0) |
| 5 | halt |
| 4 | reset active |
| 2 | interrupt set for ND-100 on level 12 |
| 0 | interrupt enabled onto ND-100 bus |

---

## 7. Device / thumbwheel addresses (VERIFIED - manual page 55-56, emulator ctor)

| Thumbwheel 12J | Eth # | Device base (octal) | Ident (octal) | Int level |
|----------------|-------|---------------------|---------------|-----------|
| 0 | 1 | 140360-140363 | 140034 | 12 |
| 1 | 2 | 140364-140367 | 140035 | 12 |
| 2 | 3 | 140370-140373 | 140036 | 12 |
| 3 | 3(sic 4) | 140374-140377 | 140037 | 12 |

Bank placement in ND-100 physical space (thumbwheels 7J/9J, manual Table 2):

| 7J 9J | Bank | PIOC addr space (KB) | Physical page (hex) |
|-------|------|----------------------|---------------------|
| 0 0-3 | 0 | 0-512 | 00-FF (bank 0 not valid) |
| 0 4-7 | 4 | 512-1024 | 100-1FF |
| 0 8-11 | 8 | 1024-1536 | 200-2FF |
| 0 12-15 | 12 | 1536-2048 | 300-3FF |
| 1 0 | 16 | 2048-2560 | 400-4FF |

The emulator uses bank 16 (physical page 0x400) as its default placement.

Ronny's note (from the source): the DRAM data path to the ND-100 appears to
be only 8 bits wide, so the ND-100 sees 512K addresses to reach 512KB of RAM
but only the low byte transfers. (ASSUMPTION / observation - relevant only to
the ND-100-side load code, not the 68000 disassembly.)

---

## 8. Mailbox / communication block (from emulator - firmware disassembly)

Source: `NDBusEthernetII.cs`, enum `MailboxField`, verified by the firmware
author against their disassembly. These are word-aligned addresses in DRAM.
IMPORTANT: this layout was derived from the **test/diagnostic** firmware
(the `ETH_TEST` variant) mailbox protocol; the production XMSG server may use
the same block or a superset. Treat offsets as VERIFIED for the test firmware,
ASSUMPTION for the production server until confirmed in this image.

Create Ghidra labels for these so data references read clearly:

Command mailbox (0x0400) - ND-100 writes, 68000 reads:

| Addr | Label | Purpose |
|------|-------|---------|
| 0x0400 | CMD_SEMAPHORE | 1=command ready, 0=consumed |
| 0x0402 | CMD_STATUS | command type / function code |
| 0x0404 | CMD_PARAM1 | first parameter |
| 0x0406 | CMD_TEST_NUM | test number -> dispatch table at 0x0948 |

Result mailbox (0x0440) - 68000 writes, ND-100 reads:

| Addr | Label | Purpose |
|------|-------|---------|
| 0x0440 | RESULT_SEMAPHORE | 1=result ready, 0=consumed |
| 0x0442 | RESULT_STAT_CODE | 1=pass, 2=fail, 3=running, 6=byte report, 10=unknown cmd |
| 0x0444 | RESULT_ERR_CODE | error code (0=OK) |
| 0x0446 | RESULT_TEST_NO | which test produced this |
| 0x0448 | RESULT_LOOP_COUNT | loop iteration |
| 0x044A | RESULT_ERR_COUNT | total errors |
| 0x044C | RESULT_ERR_ADDR | error address (high word) |
| 0x044E | RESULT_ERR_ADDR_LO | error address (low word) |
| 0x0450 | RESULT_EXP_DATA | expected value (high word) |
| 0x0452 | RESULT_EXP_DATA_LO | expected value (low word) |
| 0x0456 | RESULT_FOUND_DATA | actually found |
| 0x0458-0x0462 | RESULT_INFO2_0..5 | extra diagnostic info |

Status/config area (0x0880):

| Addr | Label |
|------|-------|
| 0x0880 | STAT_SEMAPHORE |
| 0x0882 | STAT_CODE |
| 0x0884 | FUNC_CODE (polled by 68K main loop) |
| 0x0886 | ERR_CODE |
| 0x0888 | TEST_NO |
| 0x088A | LOOP_COUNT |
| 0x088C | ERR_COUNT |
| 0x088E | ERR_ADDR |
| 0x0890 | EXP_DATA |
| 0x0892 | FOUND_DATA |
| 0x08A2 | MAIN_LOOP_ADDR (68K main loop return addr) |
| 0x0908 | CMD_BUFFER |
| 0x090E | CMD_TEST_NUM_68K |
| 0x0948 | (test dispatch table) |

Semaphore protocol (VERIFIED from emulator comments): ND-100 writes command +
sets CMD_SEMAPHORE, then writes control-reg bit 2 (ND_INT) -> MFP GPIO I6 ->
MFP vector fires on 68000 -> 68000 reads command, dispatches, writes result +
sets RESULT_SEMAPHORE, then writes 0x0001 to SCIP (0xEF0180) -> INT12 to
ND-100 -> ND-100 reads result and clears RESULT_SEMAPHORE.

---

## 9. Known firmware routine addresses (from emulator comments - ASSUMPTION until confirmed)

These are cited in `NDBusEthernetII.cs` against `pioc-ether-68k-code.bin`.
They should match this image at base 0 (same reset PC region). Verify each
by disassembling and confirming the described behavior.

| Address | Described function |
|---------|--------------------|
| 0x1CFE | Reset entry (VERIFIED - reset PC) |
| 0x25F0 | mfp_setup_regs - programs MFP VR/IER/timers/USART |
| 0x2598 | timer_init - starts MFP timers |
| 0x30CA | test_dispatch_loop |
| 0x30D6 | IPL lowered to 0 |
| 0x3338 | MFP reinit after RESET instruction |
| 0x45E0 / 0x4610 | hw_init_and_dispatch (executes RESET instruction; posts first INT12 result) |
| 0x57F2 | TRAP vector table init (stack-frame allocator w/ inline params) |
| 0x0948 | test dispatch table |

---

## 10. Suggested reversing workflow

1. Import at base 0, define the vector table (0x0-0x3FF), disassemble from 0x1CFE.
2. Label the I/O accesses: search for references to 0xEF00xx / 0xEF01xx and
   apply the section-4 names. LANCE (0xEF00A0/A2) and SCIP (0xEF0180) accesses
   mark the Ethernet TX/RX and the ND-100 signaling paths.
3. Label the DRAM mailbox addresses (section 8) so the command/result handling
   reads clearly.
4. Confirm the section-9 routine addresses; they give you the boot/init spine.
5. For the ND-100 side of the load protocol, pair this with the ENCOSE0
   supervisor image (see [README.md](README.md), `SEGLOAD`/`READPIO`/`POSUERR`)
   - the two meet at the control/status registers (section 6) and the mailbox.
6. Cross-check any I/O behavior question against `NDBusEthernetII.cs`, which is
   a working emulation of this exact board.
