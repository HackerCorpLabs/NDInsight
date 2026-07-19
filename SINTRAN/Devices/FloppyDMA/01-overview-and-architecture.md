# 1. Overview and Architecture

## 1.1 What the card is  [MANUAL §Preface, §1]

The 3106/3112 is a **microprocessor-based controller/formatter** that performs control and
data transfer between the ND-100 CPU and a **floppy disk drive** or a **QIC-02 streaming
tape drive**. One card handles both worlds; which world is active is chosen by a bit in the
Hardware Control Word (bit 5, "Enable Streamer").

| Controller | Card No. | ND No. | Media |
|------------|----------|--------|-------|
| Floppy and Streamer Controller 8"          | 3106 | ND-630 | 8" floppy + QIC-02 streamer |
| Floppy and Streamer Controller 8" and 5¼"  | 3112 | ND-317 | 8" + 5¼" floppy + QIC-02 streamer |

A maximum of **four floppy drives and four streamer drives** may be connected at the same
time. The 3112 is a superset of the 3106: identical except that it also handles 5¼" media
and adds programmable write-precompensation. Where a card number is not named, the text
applies to both. [MANUAL §Preface]

The 3106/3112 **fully replaces the older 3027 floppy controller.** For stand-alone (LOAD-
button) use, a floppy needs a current **FLO-MON** (Floppy Monitor 2010F or newer) written on
it; too-old a FLO-MON produces error 51 ("wrong bootstrap"). [MANUAL §1]

## 1.2 On-card hardware  [MANUAL §1, §8]

The controller is built around these chips:

| Chip | Role |
|------|------|
| **Z80A** | On-card microprocessor running the control firmware (`34300G.bin`). |
| **AM9517A** | DMA controller — moves the Command Block and data between ND-100 memory and the card's local RAM. |
| **FD1797** (Western Digital) | Floppy disk controller/formatter. Does the low-level track/sector/CRC work. |
| **Z80-CTC** | Counter/timer + interrupt controller. Its channel 0 is "interrupt from the ND-100"; other channels vector floppy/DMA/streamer/timer interrupts. |
| **AD558** | D/A converter used to trim the VCO of the data separator. |
| PAL10L8 + 2 flip-flops | 16-bit-word ↔ 8-bit-byte conversion during ND-100 DMA. |
| PAL16L8 ×3 (344/345/346) | Memory chip-select decode, ND-100 DMA control, streamer control. |
| WP-PROM (3112 only) | Programmable write-precompensation lookup (1K×4). |
| CMOS watchdog counter | Timeout (~10 s), armed on control-word load, reset on each floppy↔controller data transfer. Fires error 26 (Timeout). |

A hardware **compare circuit** verifies data read from / written to the diskette (inherited
from the 3027). The **data separator** is an analog phase-locked loop with a VCO trimmed to
4 MHz during self-test (mis-trim → error 24). Precompensation is done outside the FD1797 and
can be switched on/off (and, on the 3112, its amount programmed) from the Z80. [MANUAL §2.1]

## 1.3 The "mailbox" model  [MANUAL §1, Fig. 2]

Very little information is passed to the controller through IOX instructions. Instead, the
ND-100 builds a **Command Block (CB)** in ND-100 main memory, tells the controller *where*
that block is (via two IOX pointer writes), and then *activates* the controller. The
controller DMAs the block into its own RAM, executes it, and DMAs a **status block** back
into ND-100 memory just after the command block.

The activation sequence for a read/write operation is exactly three steps: [MANUAL §1]

1. **Enter the Command Block** into ND-100 memory (6 words: command, floppy address, DMA
   memory address, word/sector count).
2. **Load the Command-Block pointer** into the controller with two IOX writes:
   `IOX DEVNO+5` (pointer high, bits 16–23) and `IOX DEVNO+7` (pointer low, bits 0–15).
3. **Activate** the controller by loading the Hardware Control Word (`IOX DEVNO+3`).

When the control word is loaded, the card interrupts its own Z80, which DMAs the command
block in, analyses and executes the command, DMAs the 6-word status block back to ND-100
memory (placed after the command block), and finally raises **READY-FOR-TRANSFER (RFT)**,
which interrupts the ND-100 if interrupts are enabled. [MANUAL §1]

```
        ND-100 side                          3112 card (Z80)
   ┌───────────────────────┐          ┌──────────────────────────┐
   │ Command Block in RAM  │          │  Z80 + FD1797 + AM9517    │
   │  +0 Command           │  IOX +5  │                          │
   │  +1 Device address    │  IOX +7  │  1. CTC ch0 int on        │
   │  +2 Mem addr 23-16    │─pointer─►│     control-word load     │
   │  +3 Mem addr 15-0     │          │  2. DMA CB into 2080h     │
   │  +4 Options/WC 23-16  │          │  3. decode + run command  │
   │  +5 Word/Record count │          │  4. FD1797 read/write     │
   │  +6 Status 1 ◄────────┼─DMA back─┤  5. DMA status to CB+6..  │
   │  +7 Status 2 ◄────────┤          │  6. set RFT → int ND-100  │
   │  +10..+13 last/remain │          │                          │
   └───────────────────────┘          └──────────────────────────┘
```

## 1.4 On-card memory map  [MANUAL §9.3, cross-checked FIRMWARE]

The Z80 address space (16-bit, `ram:` in Ghidra):

| Z80 address | Contents |
|-------------|----------|
| `0000h–1FFFh` | **PROM/EPROM** — the microprogram (`34300G.bin`, an 8 KB 2764). |
| `2000h–27FFh` | **RAM 1** (512 bytes) — stack, data fields, Z80 scratch. |
| `2800h–35FFh` | **RAM 2** (3584 bytes) — data buffer to/from floppy. |
| `3000h+` | Possible RAM 3/RAM 4 extension. |

RAM 1 sub-layout [MANUAL §9.3]:

| Z80 address | Contents |
|-------------|----------|
| `2000h` | Stack area (also self-test mailbox scratch). |
| `2070h` | IM2 interrupt-vector table (16 bytes, copied from PROM `0076h` at reset). [FIRMWARE @083c] |
| `2080h` | **ND-100 transfer & Z80 data area** — the Command Block DMA'd in from the host lands here. [MANUAL §9.3, FIRMWARE @030c] |
| `2100h` | Floppy data field (FD1797 working state; e.g. saved status at `2109h`). [FIRMWARE] |
| `2200h` | **Buffer area** — sector data staging (also the up/down-load buffer). [MANUAL §9.3] |

The controller **auto-sizes its own RAM** at power-on, so the buffer can be enlarged just by
adding chips. [MANUAL §9.4]

## 1.5 Interrupt structure  [MANUAL §8, FIRMWARE]

The Z80 runs in **interrupt mode 2 (IM2)**; the vector table high byte is `20h`, so vectors
live at `2070h`. The CTC provides the interrupt sources: [MANUAL §8, FIRMWARE @0076]

| CTC channel | Source | Z80 port |
|-------------|--------|----------|
| 0 | **Interrupt from ND-100** (control-word load / new command) | `10h` |
| 1 | Interrupt from streamer exception | `11h` |
| 2 | Interrupt from floppy controller (FD1797) | `12h` |
| 3 | Interrupt from DMA controller | `13h` |
| 4 | Interrupt from compare error | `14h` |
| 5 | Interrupt from streamer ready | `15h` |
| 6, 7 | Interrupt from timer | `16h`, `17h` |

The 16-byte vector table copied to `2070h` (verified by hexdump of PROM `0076h`) points at
the real ISRs; the two most important for the floppy path are the **host-command ISR**
(`ram:030c`, fired by CTC ch0) and the **FDC result ISR** (`ram:0d0f`, fired by CTC ch2).
See [`07-firmware-internals.md`](07-firmware-internals.md).

## 1.6 What the ND-100 sees vs. what the Z80 sees

Two distinct register views exist and must not be confused:

- **ND-100 side (IOX):** the 8 IOX registers at `DEVNO+0..+7`. This is what a SINTRAN driver
  or the emulator's ND-100 bus model touches. Documented in
  [`02-programming-interface.md`](02-programming-interface.md).
- **Z80 side (I/O ports):** the AM9517/FD1797/CTC/streamer/display registers plus the
  ND-100-interface latches (`50h–57h`). This is what the firmware touches. Documented in
  [`07-firmware-internals.md`](07-firmware-internals.md) §"Port map".

The bridge between them is the **ND-100-interface register file** (`50h–57h` on the Z80
side), which the manual §8 lists as write-only (`ADL/ADM/ADH/DD-T/DLO/DHI/NSTAT/NFINI`) and
read-only (`CW1/POL/POM/POH/CW2/MAR0-7/MAR8-15/MAR16-23`) latches. An emulator that models
the card at the **IOX level** does not need these latches; an emulator that runs the actual
Z80 firmware does.
