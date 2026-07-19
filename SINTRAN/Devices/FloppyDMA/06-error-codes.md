# 6. Error Codes

Error codes are **octal**, delivered in **bits 9–14 of Status Word 1** (CB+6 in ND-100
memory), and simultaneously shown as **two octal digits on the 7-segment display** on the
rear edge of the card. [MANUAL §3.9] The firmware forms the code as `(code & 0x3F) << 1` in
the high status byte, so the value lands at bits 9–14. [FIRMWARE @06b4, @1f1c]

> **Delivery — important.** The numeric error code lives in the **memory Status Word 1**
> (CB+6), *not* in the IOX Hardware Status Word. The Hardware Status Word (IOX +2/+4) carries
> only the OR-of-errors summary (bit 4) and the hard-error flag (bit 7). See
> [`02-programming-interface.md`](02-programming-interface.md) §2.4 and §2.7.

## 6.1 Complete error-code table  [MANUAL §3.9]

| Octal | Meaning | Class |
|-------|---------|-------|
| 00 | OK | — |
| 01–04 | Not used | |
| 05 | CRC error | disk |
| 06 | Sector not found | disk |
| 07 | Track not found | disk |
| 10 | Format not found (diskette not formatted) | disk |
| 11 | Diskette defect (impossible to format) | disk |
| 12 | Format mismatch | disk |
| 13 | Illegal format specified | disk |
| 14 | Single sided diskette inserted | disk |
| 15 | Double sided diskette inserted | disk |
| **16** | **Write protected diskette/cartridge** | disk |
| 17 | Deleted record | disk |
| 20 | Drive not ready | drive |
| 21 | Controller busy on start | controller |
| 22 | Lost data (over-/under-run) | controller |
| 23 | Track zero not detected | drive |
| 24 | VCO frequency out of range | hardware |
| 25 | Microprogram out of range | firmware |
| 26 | Timeout (watchdog) | controller |
| 27 | Undefined error | controller |
| 30 | Track out of range | disk |
| 31 | Not used | |
| 32 | Compare error (during data compare) | disk |
| 33 | Internal DMA errors | controller |
| 34–37 | Not used | |
| 40 | ND-100 bus error, command fetch | bus |
| 41 | ND-100 bus error, status transfer | bus |
| 42 | ND-100 bus error, data transfer | bus |
| 43 | Illegal command | firmware |
| 44 | Word count not zero | firmware |
| 45 | Illegal completion (continuous transfer) | firmware |
| 46 | Addr-reg error | controller |
| 47 | Not used | |
| **50** | **No bootstrap found on diskette** | autoload |
| **51** | **Wrong bootstrap (out-of-date FLO-MON)** | autoload |
| 52 | Not used | |
| **53** | **Error during Autoload** | autoload |
| 54–57 | Not used | |
| 60 | Streamer handshake error | streamer |
| 61 | Streamer status transfer error | streamer |
| 62 | Bad cartridge | streamer |
| 63 | No cartridge installed | streamer |
| 64 | End of tape, cartridge full | streamer |
| 65 | Streamer drive error | streamer |
| 66 | Unidentified exception | streamer |
| 67 | Illegal command to streamer | streamer |
| 70 | PROM checksum error | self-test |
| 71 | RAM error | self-test |
| 72 | CTC error | self-test |
| 73 | DMA-CTRL error | self-test |
| 74 | VCO error | self-test |
| 75 | FLOPPY controller error | self-test |
| 76 | Streamer data register error | self-test |
| 77 | ND-100 register error | self-test |

## 6.2 Explanations of the key codes  [MANUAL §3.10]

- **05 CRC error** — a CRC written after each sector's data does not match the CRC computed
  on read-back. Indicates bad media, dirty heads, or cable/termination interference.
- **06 Sector not found** — the desired sector's address field did not pass under the heads in
  5 revolutions. Destroyed format, bad media, or dirty heads.
- **07 Track not found** — no ID field with this track number.
- **10 Format not found** — the diskette has not been formatted.
- **11 Diskette defect** — a formatted track keeps failing verification even after a reformat.
- **12 Format mismatch** — specified format ≠ diskette's format; Status Word 2 gives the
  diskette's actual format.
- **13 Illegal format specified** — format field is 3 or 8 (illegal). Tested on every r/w.
- **14 / 15 sides mismatch** — double-sided specified with single-sided media (14), or the
  reverse (15).
- **16 Write protected** — a write was attempted on a write-protected diskette/cartridge; the
  write is aborted. **[FIRMWARE @0e84, @1ea2]** detected from FD1797 status bit 6.
- **17 Deleted record** — an unspecified deleted record was read.
- **20 Drive not ready** — operation attempted on an unavailable/non-existent drive; also
  covers "no diskette", "diskette inserted the wrong way", power failure, or a bad drive
  number. **[FIRMWARE @0d26, @1ea6]** from FD1797 not-ready (status bit 7).
- **21 Controller busy on start** — the FD1797 was busy when the program tried to activate it.
  "Should never occur." **[FIRMWARE @0d21, @1ea8]** from FD1797 busy (status bit 0).
- **22 Lost data** — DMA could not feed the FD1797 fast enough (over/under-run). "Should never
  occur."
- **23 Track zero not detected** — no track-zero signal after a return-to-zero seek. Can be
  caused by too-fast a step rate (NB: SINTRAN patch note).
- **24 VCO frequency out of range** — self-test could not trim the data-separator VCO to
  4 MHz. Fault in the VCO or its D/A converter.
- **25 Microprogram out of range** — the Z80 fetched from non-existent memory (bus reads
  `FFh` = `RST 038h`), which vectors to this error. Firmware/Z80 fault.
- **26 Timeout** — the ~10 s watchdog aborted a command that ran too long. Often a DMA fault.
- **27 Undefined error** — FD1797 gave an unexpected interrupt with no error bits set.
- **30 Track out of range** — track number higher than the largest track on the floppy.
- **32 Compare error** — the hardware verify read back different data than was written/read.
- **33 Internal DMA errors** — after each DMA the Memory Address Register is checked and was
  wrong.
- **40/41/42 ND-100 bus errors** — the ND-100 `BERROR` line was asserted during command-fetch
  (40), status-transfer (41), or data-transfer (42) DMA. Usually a bad CB pointer (IOX +5/+7)
  or a memory parity error in the CB region.

## 6.3 Self-test error display  [MANUAL §6]

The self-test failure codes **70–77** are shown on the display as `E0`–`E7`-style codes and
also written to the data-out register (readable via `IOX DEVNO+0`) when the status block
cannot be DMA'd. During any self-test/RAM-test failure the card **refuses to run commands**
(to protect diskette data) and sets **bit 4 (OR of errors)** + **bit 7 (hard error)** in the
Hardware Status Word. See [`09-testing-and-test-macros.md`](09-testing-and-test-macros.md).

> **[FIRMWARE]** Note there are *two* distinct code schemes in the ROM: (1) the disk/command
> error codes above, delivered via the `RST 08h`+code-byte mechanism through Status Word 1
> (table at `ram:1e8d`); and (2) a smaller **power-on self-test** scheme that writes
> `(0x38 + class) << 1` to the host data register and shows `E0..E7` on the LEDs
> (`ram:01ac-01e7`). They are not the same code space — don't conflate them.
