# 7. Firmware Internals (Z80 ROM `34300G.bin`)

This documents the **on-card Z80 firmware** as reverse-engineered in the Ghidra project
`E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND-FLOPPY-3112.gpr`. You only need this if you
emulate the card faithfully enough to run the real ROM, or want to validate a functional model
against ground truth. Addresses are Ghidra `ram:` offsets; every claim is verified in code
unless marked **[INFERENCE]**.

The Ghidra project has been fully annotated (all `FUN_ram_*` renamed, all `DAT_ram_*` named,
all I/O ports labelled, with plate/EOL comments) — open it to read the code directly.

## 7.1 Z80 I/O port map  [MANUAL §8 + FIRMWARE]

> **Authoritative source = MANUAL §8.** Note: the FD1797 register offsets follow the standard
> Western-Digital layout (cmd/status at +0, track +1, sector +2, data +3). Some firmware
> annotation passes mislabelled `71/72/73`; the manual/WD179x mapping below is correct.

| Port | Name | Function |
|------|------|----------|
| `10h`–`17h` | Z80-CTC | Ch0 = int from ND-100; ch1 = streamer exception; ch2 = FD1797; ch3 = DMA; ch4 = compare error; ch5 = streamer ready; ch6/7 = timer. |
| `20h`–`27h` | AM9517A DMA | `20h` ch0 addr, `21h` ch0 count, `22h/23h` ch1, `24h/25h` ch2, `26h/27h` ch3. |
| `28h` | DMA command/status | DCOM (write) / DSTAT (read). |
| `29h` | DMA request | DRQ. |
| `2Ah` | DMA mask (single bit) | DMSK. |
| `2Bh` | DMA mode | DMOD. |
| `2Ch` | DMA clear byte-pointer flip-flop | DCFF. |
| `2Dh` | DMA temp / master clear | DTEMP / DMC. |
| `2Fh` | DMA mask (all bits) | DMSMW. |
| `40h` | Display data | SDISP — 7-segment digit data. |
| `41h` | Display enable/mode | ENDISP. |
| `50h` | W: ADL (DMA addr 0–7) / R: CW1 (control word low) | ND-100 interface. |
| `51h` | W: ADM (DMA addr 8–15) / R: POL (ptr/data in 0–7) | |
| `52h` | W: ADH (DMA addr 16–23) / R: POM (ptr/data in 8–15) | |
| `53h` | W: DD-T (DMA direction & test) / R: POH (ptr 16–23) | |
| `54h` | W: DLO (data out 0–7) / R: CW2 (control word 16–23) | |
| `55h` | W: DHI (data out 8–15) / R: MAR 0–7 (readback) | |
| `56h` | W: NSTAT (status read by ND-100) / R: MAR 8–15 | |
| `57h` | W: NFINI (set RFT & finish) / R: MAR 16–23 | |
| `60h` | Streamer read/write data | SRD/SWR. |
| `61h` | Streamer status/mode | SS/SMR. |
| `70h` | FD1797 command (W) / status (R) | FCCOM/FCST. |
| `71h` | FD1797 track | FCTRK. |
| `72h` | FD1797 sector | FCSEC. |
| `73h` | FD1797 data | FCDAT. |
| `74h` | FD1797 device-select & mode | FDVSEL. |
| `75h` | FD1797 clear | FCCLR. |
| `76h` | D/A converter (VCO trim) | FADC. |
| `77h` | Floppy drive status | FLSTAT. |
| `61h` (reset) | Strap/mode input | Read at reset then discarded — **[INFERENCE]** possibly a config strap; not used by firmware. |

### FD1797 status register bits (port `70h` read)  [WD179x standard, cross-checked FIRMWARE]

| Bit | Meaning |
|-----|---------|
| 0 | Busy |
| 1 | DRQ / Index |
| 2 | Lost data / Track 0 |
| 3 | CRC error |
| 4 | Record not found |
| 5 | Record type / spin-up |
| 6 | **Write protect** |
| 7 | **Not ready** |

The firmware raises: error 16₈ from bit 6 (write protect), 20₈ from bit 7 (not ready), 21₈
from bit 0 (busy on start), 05₈ from bit 3 (CRC), 06₈/07₈ from bit 4 (record not found).

## 7.2 On-card RAM map (verified variable names)  [FIRMWARE]

RAM 1 (`2000h`–`27FFh`). Ghidra names in the project after annotation:

| Addr | Name | Meaning |
|------|------|---------|
| `2000h` | stack area | Z80 stack (SP starts `2070h`, grows down; `206Eh` = top slot). |
| `2070h` | IM2 vector table | 16 bytes copied from PROM `0076h` at reset. Live vector for host-command ISR is patched here (`= 030Ch`, or `= 1BFxh` during autoload chunking). |
| `2080h` | `cmd_block_from_host` | The 12-word Command Block DMA'd in from ND-100. |
| `2081h` | `cmd_block_cmd_lo` | Low byte of command word. |
| `2082h` | `cmd_block_device_addr` / running host target during autoload | |
| `2089h`/`208Ah` | `cmd_block_word_count` / `cmd_block_sector_count` | |
| `208Ch` | `host_status_hi_DHI` | High status byte → error code `(code&0x3F)<<1` → word bits 9–14. |
| `208Dh` | `host_status_lo_DLO` | Low status byte → flags: b3 ready, b4 OR-of-errors, b5 deleted, b6 retry, b7 hard error → word bits 0–7. |
| `2098h` | `host_command_word_CW` | The control word read from CW1/CW2. |
| `209Ah`/`209Ch` | host CB pointer low/high | From POL/POM/POH. |
| `209Dh` | `current_command_code` | `cmd & 0x3F`. |
| `2100h`–`213Ah` | floppy state block | Per-drive state, FDVSEL, side/sector, saved FDC cmd/status, format-track work. |
| `2104h` | `drive_select_code` | FDVSEL value. |
| `2108h` | saved FDC command | Reloaded by `ReissueFdcCommand`. |
| `2109h` | saved FDC status | Latched by the FDC wait routines. |
| `2129h`/`2139h` | per-drive / per-unit state arrays | |
| `2160h`+ | IY control block | `ctrl_flags`, op-select, cb-state, bootstrap-validation fields. |
| `2194h` | `status_error_code_6bit` | The 6-bit error/status code. |
| `2195h` | `status_error_class` | Class field: `0x80` hard error, `0x40` completion, `0x00` no report. |
| `2200h` | `sector_data_staging_buf` | Sector data + up/down-load + bootstrap-scan buffer. |
| `20CAh`/`20CCh` | live DMA count pair | |

## 7.3 Reset and self-test  [FIRMWARE @0000, @003b, @0086]

`ColdResetEntry` (`ram:0000`) → `IN A,(0x61)` (discarded) → `OUT(0x74)` → `SystemReset`
(`ram:003b`). Sets up IM2 (`I = 0x20`, vectors at `2070h`, table copied from PROM `0076h`),
runs the ROM checksum + RAM test (`MainLoop` @0086 — despite the name this is the self-test,
**not** the command loop), the DMA self-test (`reset_int_timer_dma` @0130), and the floppy
readiness/seek verify (`FloppyStreamerSelfTest` @0163). On any failure it displays `E0..E7`
forever. On success it drops into the real command loop.

## 7.4 The command loop and host-command ISR  [FIRMWARE @0268, @030c]

The idle loop is `ram:0268`: `EI; HALT` — the Z80 sleeps until CTC ch0 (interrupt from the
ND-100) fires, then processes the mailbox and loops.

`HostCmd_ISR` (`ram:030c`) is the heart of the host interface:
1. Clears the status/error code.
2. Reads the control word: `IN(0x54)`→low (CW2), `IN(0x50)`→high (CW1) → `2098h`.
3. Reads the Command-Block pointer: `IN(0x51)/IN(0x52)/IN(0x53)` (POL/POM/POH).
4. Decodes what to do from the control-word bits:
   - **bit 8** ("fetch & execute") → default dispatch → command execution / `Autoload_BootstrapLoad`.
   - **bit 2** ("activate autoload") gate (`CP 0x04`) → `ram:1c05` (read + 7-seg display).
   - validation failures → error-ack path `ram:038d/0396/03e0` → still completes with a status.
5. DMAs the command block into `2080h`, parses it, executes, DMAs the status block back, sets RFT.

## 7.5 Command-block parse and transfer setup  [FIRMWARE @085e, @0bf9]

`ParseCmdBlock_SetupTransfer` (`ram:085e`) reads the CB fields: 24-bit ND-100 memory address
at `+5..+7` → `20C4h/20C6h`, transfer length at `+9..+B`, direction/geometry from `+0x19`,
mode from `+0x55`. `ComputeTrackSector_DivMod` (`ram:0b4b`) converts the logical sector
address to (track, sector, side) by repeated subtraction with a range guard (overflow →
error via `RST 08h`). `SetupTransfer_CopyParams_DmaAddr` (`ram:0bf9`) loads the AM9517 24-bit
DMA address. 512-byte-sector handling is gated on `(IX+0x19)` bit 2.

## 7.6 FD1797 command primitives  [FIRMWARE @0c9d, @0e1c, @102f, @103f, @113d, @1325]

| Routine | FD1797 op | Purpose |
|---------|-----------|---------|
| `FdcRestore_Retry` (`102f`) | `0x08` | Restore/recalibrate to track 0. |
| `FdcSeek_Retry` (`103f`) | `0x18` | Seek with verify (track → FCDAT). |
| `FdcStepIn_Retry` (`102b`) | `0x48` | Step in. |
| `FdcReadAddress_Verify` (`0fe2`) | `0xC4` | Read-Address, verify drive/head, 4× side-flip retry. |
| `FdcReadSectorLoop_Verify` (`113d`) | `0x88` | Multi-sector Read-Sector loop. |
| `FDC_WriteTrack_Format` (`1325`) | `0xF0/0xF2` | Write-Track (format), DMA-fed from `2200h`, 3× retry. |
| `FdcForceInterrupt` (`0750`) | `0xD0/0xD4` | Force-interrupt / abort. |
| `FdcWaitComplete_SaveCmdStatus` (`1075`) | — | Save cmd → `2108h`, busy-wait, latch status → `2109h`. |

`FdcTransfer_NextSector_ISR` (`0d50`) is the DMA-complete / next-sector state machine driven
from the FDC interrupt. `Seek_Servo_Positioner` (`14ec`) is a successive-approximation DAC
servo positioner (CTC timers + FADC/FDVSEL, 3× retry).

## 7.7 The error-report mechanism (`RST 08h` + code byte)  [FIRMWARE @0008, @1e8d, @1ef8]

This is the elegant core to understand:

- Every status/error is reported by calling a **2-byte stub** of the form `CF nn` = `RST 08h`
  followed by a **code byte `nn`**. There is a whole table of these stubs at `ram:1e8d`.
- **`RST 08h`** (`ram:0008`) saves registers, sets `HL = SP` (which points at the return
  address = the `nn` byte just after the `RST`), and jumps to `StatusReport_Dispatch`
  (`ram:1ef8`).
- **`StatusReport_Dispatch`** (`ram:1ef8`) reads that code byte, splits it:
  - `code = byte & 0x3F` → the 6-bit error number (→ `2194h`, and `(code)<<1` into the host
    status high byte `208Ch`).
  - `class = byte & 0xC0` → `2195h`. **`0x80` = hard error** (full controller reinit before
    reporting); **`0x40` = normal completion report**; **`0x00` = return, no host report**.
- The completion builder `build_and_present_host_status_word` (`ram:06b3`/`06b4`) assembles
  the status word (sets bit 3 always, bit 4 if code≠0, and drives DLO/DHI to the host).
- The code is also shown as **two octal digits** on the 7-segment display (`DisplayStatusCode_LCD`
  `ram:1c8c`), which is why the documented table is octal.

Verified stub examples: `0x8E`→016 write-protected (`1ea2`), `0x90`→020 drive-not-ready
(`1ea6`), `0x91`→021 controller-busy (`1ea8`), `0x85`→005 CRC (`1e8d`), `0x88`→010 format-not-
found (`1e93`), `0xA8`→050 no-bootstrap (`1ec8`), `0xA9`→051 wrong-bootstrap (`1eca`),
`0xAF`→057 RAM error (`1ed6`).

## 7.8 Autoload

See [`04-boot-and-autoload.md`](04-boot-and-autoload.md) for the full state machine. Entry
`Autoload_BootstrapLoad` (`ram:1ae8`); buffer read `Floppy_ReadToBuffer2200` (`ram:09f8`);
`'!'` (`0x21`) `CPIR` scan; oct 50 / oct 51 errors.

## 7.9 Streamer routines  [FIRMWARE]

`Streamer_IssueCommand` (`ram:17d9`, `0xA0`/`0xC0` variants), `Streamer_WaitStatusReady`
(`ram:1a22`), `StreamerCmd_CompleteAndReport` (`ram:18c9`) on ports `60h/61h`. See
[`08-streamer-tape.md`](08-streamer-tape.md).

## 7.10 Error-text DMA images  [FIRMWARE @0783, @078c]

The twin handlers `DmaChunkCompleteHandleErrorFinish` (`ram:0783`) /
`DmaChunkCompleteFinish_NoStaging` (`ram:078c`) format human-readable error text
("`** LOAD-ERROR: nn **`", "`** WRONG BOOTSTRAP ! **`") into the `2200h` staging buffer and
DMA it to the host, patching the two ASCII digits with the decimal error code
(`AND 7; ADD 0x30`). The captured images are the `ND Code\*.txt` `DEPOSIT` scripts (see
[`04-boot-and-autoload.md`](04-boot-and-autoload.md) §4.4).
