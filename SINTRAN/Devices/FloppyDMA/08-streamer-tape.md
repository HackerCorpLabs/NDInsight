# 8. Streamer / QIC-02 Tape — Its Own Section

> **Scope.** The 3106/3112 is a combined floppy **and** streaming-tape controller. **Only the
> floppy half is emulated today.** This file documents the tape half separately so it can be
> implemented as a distinct later work item. Everything here is from the manual (§3.5.1,
> §3.8.1, §4.1) plus the firmware's streamer routines; it is **not** yet reflected in any
> emulator model.

## 8.1 How the streamer is selected  [MANUAL §3.6 Table 1]

The floppy and the streamer share the same Command-Block mailbox and the same IOX registers.
Which device a command targets is chosen by the **Hardware Control Word**:

- **State III** (bit 8 set, bits 2/3/5 clear) → command executes on the **floppy drive**.
- **State IV** (bit 5 "Enable Streamer" **and** bit 8 set) → command executes on the
  **streamer**.

So enabling bit 5 in the control word routes the command word (CB+0) through the streamer
command decode instead of the floppy decode.

## 8.2 Streamer command word  [MANUAL §3.8.1]

```
 15 .. 12 | 11 10  9  8 |  7  6 |  5  4  3  2  1  0
 Not used | Record length| Unit  |     Function
```

| Field | Bits | Meaning |
|-------|------|---------|
| Function | 0–5 | Command (table below). |
| Unit | 6–7 | Select streamer unit 0–3. |
| Record length | 8–11 | `0` = 512 bytes/record; 1–17₈ reserved. |
| Not used | 12–15 | Zero. |

### Streamer function codes  [MANUAL §3.8.1, §4.1]

| Octal | Hex | Command | Notes |
|-------|-----|---------|-------|
| 00 | 0x00 | **Read data** | Tape → ND-100. Must start at BOT or after a file mark. Record count = amount. |
| 01 | 0x01 | **Write data** | ND-100 → Tape. Always starts at BOT. Needs ≥ 90 KB/s or it under-runs (stops, rewinds, resumes). |
| 07 | 0x07 | **Erase tape** | Whole tape erased BOT→EOT in one pass (wide erase head). |
| 10 | 0x08 | **Advance to EOT** | Read forward to the next file mark, then stop. Repeatable. |
| 12 | 0x0A | **Write EOT** | Write a file mark at the current position. |
| 13 | 0x0B | **Rewind to BOT** | Move tape to physical beginning. |
| 20 | 0x10 | **Read status** | Drive status → ND-100 memory. |
| 36 | 0x1E | **Read extended status** | Full status block (board/PROM/Z80 regs/drive status) → ND-100 (see §8.4). |
| 56 | 0x2E | **Check cartridge** | Read from BOT, verify CRC, stop at first error; failing block address in LAST MEMADDR. |
| 57 | 0x2F | **Test cartridge capacity** | Write all tracks to measure capacity; block count in REMAINING WORDS (÷2048 → MB; 1 block = 512 B). |
| 70 | 0x38 | **Retention cartridge** | BOT→EOT→BOT to re-tension tape. Do before using a long-idle tape. |
| 74 | 0x3C | **Continuous read-transfer** | See §8.3. |
| 75 | 0x3D | **Continuous write-transfer** | See §8.3. |

## 8.3 Continuous transfer  [MANUAL §4.1.11–4.1.15]

A QIC streaming tape is not a start/stop device; if the host can't keep it fed it under-runs
(writes a termination block, stops, refills) — wasteful. Continuous-transfer mode keeps the
tape streaming:

- **Continuous read (74₈):** the interrupt is given when all requested data has reached the
  ND-100, but the controller keeps reading ahead to fill its buffer (~30 ms to fill). The host
  should issue the next read within ~30 ms. A continuous-read series **must be terminated by a
  plain Read Data (00)** to get correct final status.
- **Continuous write (75₈):** the interrupt is given when the host→controller-buffer transfer
  finishes, **without** waiting for the data to reach tape. The buffer keeps the tape
  streaming while the host prepares the next transfer (~30 ms to drain). A continuous-write
  series **must be terminated by a plain Write Data (01)** for correct status.

**Points to note** [MANUAL §4.1.15]:
- An interrupt is given on **every** transfer. For continuous *read*, status is fully correct.
  For continuous *write*, status is **not final** (data not yet on tape) — an error occurring
  between transfers is reported with the **next** transfer's interrupt.
- During continuous-write, **bit 4 (OR of errors) in the Hardware Status Word is an OR of the
  current and preceding transfer**; Status Word 2 tells *which* transfer failed.
- The **last** transfer of a series must be **non-continuous** to get a true status.

## 8.4 Read extended status (36₈)  [MANUAL §4.1.7]

Writes a status image into ND-100 memory as if a Read Data occurred. Word count > 32₈ or = 0
is clamped to 32₈. Layout (relative to Memaddr):

| Offset | Contents |
|--------|----------|
| +0 | Board no. → Board Status |
| +1 | 15–8 sub version no. / 7–0 µprog version no. → PROM Status |
| +2 | Buffer start address |
| +3 | Buffer size |
| +4 | Z80 IX register |
| +5 | Z80 AF register |
| +6 | Z80 BC register |
| +7 | Z80 DE register |
| +10 | Z80 HL register |
| +11 | Z80 SP register |
| +12 | Z80 PC register |
| +13 | Z80 IY register → Z80 Status |
| +14 | Status 1 |
| +15 | Status 2 |
| +16 | Last addr 23–16 |
| +17 | Last memory address 15–0 |
| +20 | Remaining words 23–16 |
| +21 | Remaining words 15–0 → Controller Status |
| +22 | Status byte 0 / Status byte 1 |
| +23 | Status byte 2 / Status byte 3 |
| +24 | Status byte 4 / Status byte 5 → Streamer Drive Status |

### QIC-02 drive status bytes  [MANUAL §4.1.7]

**Status byte 0:**

| Bit | Meaning |
|-----|---------|
| 0 | File mark detected |
| 1 | Bad block not located |
| 2 | Unrecoverable data error |
| 3 | End of media (also EOT variant) |
| 4 | Write-protected cartridge |
| 5 | Unselected drive |
| 6 | Cartridge not in place |
| 7 | Status byte 0 bits active |

**Status byte 1:**

| Bit | Meaning |
|-----|---------|
| 0 | Power-on / reset occurred |
| 1 | End of recorded data |
| 2 | Reserved for bus parity error |
| 3 | Beginning of media / BOT |
| 4 | Eight or more read retries for one block |
| 5 | No data detected |
| 6 | Illegal command |
| 7 | Status byte 1 bits active |

## 8.5 Streamer Status Word 2  [MANUAL §3.5.1]

When the streamer is active, Status Word 2 (CB+7) carries: bit 4 = a copy of the SS (Streamer
Status) register; bit 10 = if an error occurred in the **previous** transfer after the
interrupt, this byte holds the error code (0 = no error).

## 8.6 Firmware notes for the streamer  [FIRMWARE]

The on-card firmware has a streamer command path distinct from the floppy path:
`Streamer_IssueCommand` (`ram:17d9`) and its `0xA0`/`0xC0` variants (`ram:17c7`/`ram:17dd`),
`Streamer_WaitStatusReady` (`ram:1a22`, spins until the streamer-status low two bits are set),
and `StreamerCmd_CompleteAndReport` (`ram:18c9`). The QIC-02 registers live on Z80 ports
`60h` (read/write data), `61h` (status/mode). The streamer error codes are 60–67₈ (see
[`06-error-codes.md`](06-error-codes.md)). These are documented for a future implementation;
they are **not** exercised by the current floppy-only emulator.

## 8.7 Erase/read/write head geometry  [MANUAL §4.1.3, Fig. 3]

The erase head is wider than the read/write heads, which is why a single forward pass erases
the whole track. (Relevant only to physical fidelity, not functional emulation.)
