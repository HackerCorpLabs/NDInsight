# 9. Self-Test and Test Macros

## 9.1 Power-on self-test  [MANUAL §6.1]

On a **Master Clear (MC)** the Z80 runs a self-test. On the rear-edge display this shows as:
display first **OFF**, then **000** on success. During the test **drive 0 is selected and
restored**. On failure the display shows codes **E70–E75** (and E76/E77). If the display is
unlit or shows other codes, the processor cannot run the self-test at all.

Self-test phases and their failure codes:

| Phase | Test | Fail code (octal) |
|-------|------|-------------------|
| 1 | PROM read + checksum | 70 |
| 2 | RAM test | 71 |
| 3 | Z80A-CTC (timer/interrupt) | 72 |
| 4 | AM9517-4 DMA controller in test mode | 73 |
| 5 | VCO SN74LS629 measured + adjusted into PLL range | 74 |
| 6 | FD1797 floppy controller | 75 |
| 7 | MARs (Memory Address Registers) | 77 |
| 8 | QIC-02 bus registers | 76 |

**[FIRMWARE @0086, @01ac-01e7]** In the ROM the self-test is `MainLoop` (`ram:0086`, actually
the ROM-checksum + RAM test) feeding a small display scheme that writes `(0x38 + class) << 1`
to the host data register and shows `E0..E7` for classes 0–7. This is a **separate** code
space from the disk error codes (§6).

## 9.2 Background RAM test  [MANUAL §6.2]

The RAM used for floppy data buffering is tested **continuously** while idle. It starts after
~3 minutes idle and takes ~30 minutes per 1 KB. **Any new ND-100 access stops it.** It can be
disabled by test macro **T25** (re-enabled on the next clear).

## 9.3 Behaviour on self-test / RAM-test failure  [MANUAL §6, note]

> If errors are found during self-test or the RAM test, the controller **will not carry out
> commands** — this protects diskette data. **Bit 4 (OR of errors)** and **bit 7 (hard error
> — DMA transfer)** in the Hardware Status Word are set. The status field is **not** written
> to ND-100 memory; however, **Status Word 1 is written to the controller data register** and
> can be read via **IOX DEVNO+0**.

An emulator that models self-test failures must therefore expose the failure through IOX +0,
not through the CB+6 memory writeback.

## 9.4 Test mode (control-word bit 3)  [MANUAL §7]

Setting **bit 3** of the Hardware Control Word puts the card in **test mode** (State II).
Then **bits 9–15** of the control word select one of the test routines, and the pointer
registers **POL/POM/POH** (Z80 read-only ports `51h/52h/53h`) carry parameters instead of a
Command-Block pointer. `X` in the control-word column below is `0` (no interrupt) or `2`
(interrupt when finished). [MANUAL §7.2, §7.3]

Tests T13, T14, T16, T17, T18 write Status Word 1 to the data-out register; **all** tests
write status to the data-out register on error (read via `IOX DEVNO+0`).

### 9.4.1 Test table  [MANUAL §7.3, Table 2]

| Test | Control word (oct) | Action |
|------|--------------------|--------|
| 0  | 00041X | Do nothing (set RFT) |
| 1  | 00141X | Stop controller (test timeout) |
| 2  | 00241X | Copy POL & POM to DLO & DHI (data-out) |
| 3  | 00341X | Copy POH to DLO (bits 0–7) |
| 4  | 00441X | Copy POH to the address given by POL & POM |
| 5  | 00541X | Load DLO with the byte addressed by POL & POM |
| 6  | 00641X | Load POL & POM with memory size (upper RAM address) |
| 7  | 00741X | Write POM to the register addressed by POL |
| 8  | 01041X | Load DLO with the register addressed by POL |
| 9  | 01141X | DMA input test (Z80 → ND-100). POH = Z80 block no. (block 1 = 2000h, addr = 2000h + 80h·(POH−1)); POL & POM = ND-100 address (first 64 Kwords only) |
| 10 | 01241X | DMA output test (ND-100 → Z80). Same parameters as T9 |
| 11 | 01341X | Compare test. POL & POM = start; compares two following 128-byte blocks in Z80 memory; DLO & DHI = remaining bytes after a compare error (0 = OK) |
| 12 | 01441X | Display test — count 0 to 9 |
| 13 | 01541X | **Load ND-100 → Z80.** Address in ND-100 and Z80 + word count fetched from ND-100 memory; POH/POM/POL point to the parameter field |
| 14 | 01641X | **Load Z80 → ND-100** (parameters as T13) |
| 15 | 01741X | **Start program** at the address in POL & POM |
| 16 | 02041X | Generate CRC error. POL = sector, POM = track, POH = FDVSEL |
| 17 | 02141X | Destroy track. POM = track, POH = FDVSEL |
| 18 | 02241X | Destroy 1 sector. POM = track, POH = FDVSEL |
| 19 | 02341X | TAP-TAP test. POH = number of taps |
| 20 | 02441X | Stop display |
| 21 | 02541X | Change to interrupt address in PROM (for RAM test) |
| 22 | 02641X | Load stack pointer (POL & POM = value) |
| 23 | 02741X | Read stack pointer |
| 24 | 03041X | Execute FD1797 command. POH = FD1797 track reg, POL = FD1797 command reg; on finish POM = FD1797 track reg, POL = FD1797 status |
| 25 | 03141X | Disable RAM self-test (not started after 3 min idle; re-enabled on clear) |

> Note: The three combined together — **T13 (load code into Z80 RAM)**, **T15 (start it)**,
> and **T14 (read Z80 RAM back out)** — let a developer inject and run arbitrary Z80 code on
> the card. T7/T8/T24 give access to all controller registers. This is how ND maintenance
> software exercises the card.

## 9.5 Up/Down-load address field (T13 / T14)  [MANUAL §7.4]

T13 and T14 (unlike the other tests) use a **parameter field in ND-100 memory** — POL/POM/POH
point to it — to specify addresses and byte count:

**In Z80 RAM (8-bit each):**

| Z80 addr | Field |
|----------|-------|
| `20F0h` | ND-100 load address (low) |
| `20F1h` | ND-100 load address (high) |
| `20F2h` | Z80 address (low) |
| `20F3h` | Z80 address (high) |
| `20F4h` | Byte count (low) |
| `20F5h` | Byte count (high) |

*(The manual's table has an OCR typo repeating `20F2H`; the sequence is `20F0h`–`20F5h`.)*

**In ND-100 (16-bit each):** parameter field +0 = ND-100 load address, +1 = Z80 address,
+2 = byte count. POL, POM, POH point to this field.

## 9.6 Relevance to emulation

The test macros are only needed if you emulate the card faithfully enough to run ND
maintenance/diagnostic programs. For a functional floppy model, the important takeaways are:
(1) a **self-test failure** must be reported via IOX +0 with hardware-status bits 4+7 set and
no CB writeback; (2) **test-mode (control bit 3)** repurposes bits 9–15 and the pointer
registers, so the normal command path must not treat a test-mode control word as a floppy
command.
