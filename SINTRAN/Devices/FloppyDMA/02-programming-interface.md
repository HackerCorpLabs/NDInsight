# 2. Programming Interface (Register Reference)

This is the core reference for anyone driving or emulating the card at the **ND-100 IOX
level**. Every table is quoted from ND-11.021.1 with the section number, and annotated with
the confirming firmware behaviour where known.

All numbers are **octal** unless suffixed `h` (hex) or explicitly "decimal". ND-100 words
are **16-bit, big-endian**; bit 15 is the MSB, bit 0 the LSB.

---

## 2.1 IOX register map  [MANUAL §3.1]

The card occupies 8 IOX addresses at `DEVNO+0 .. DEVNO+7`. **Reading or writing an unused
IOX does NOT raise an IOX error** — the card silently ignores it.

| IOX | Direction | Name | Function |
|-----|-----------|------|----------|
| `DEVNO+0` | Read  | Read Data | Read a data/status byte from the controller's data-out register. Used to recover Status Word 1 after a self-test failure (when the status block cannot be DMA'd). |
| `DEVNO+1` | —     | Not used | — |
| `DEVNO+2` | Read  | Read Status | **Hardware Status Word** (§3.7). |
| `DEVNO+3` | Write | Load Control Word | **Hardware Control Word** (§3.6) — activates the controller. |
| `DEVNO+4` | Read  | Read Status | **Hardware Status Word** — *identical to `+2`* (§3.1 Note 1). |
| `DEVNO+5` | Write | Load Pointer High | Command-Block pointer bits **16–23**. |
| `DEVNO+6` | —     | Not used | — |
| `DEVNO+7` | Write | Load Pointer Low / Load Data | Command-Block pointer bits **0–15**; also "load data" in some test modes. |

> **[MANUAL §3.1 Note 1] — why +2 and +4 are duplicates:**
> *"Reading either status gives the same result. They are duplicated to make it possible for
> microprograms in the ND-100 CPU to perform both Binary Format Load and Mass Storage Load
> (1560x and 21560)."*
> An emulator **must** return the *same* Hardware Status Word from both `+2` and `+4`.
> Returning Status Word 2 (or a distinct register) from `+4` is a known bug in past models.

## 2.2 Thumb-wheel / device number  [MANUAL §3.2]

| Thumb wheel | Device number |
|-------------|---------------|
| 0 | `1560₈` |
| 1 | `1570₈` |
| 2–15 | Not used |

Per unit (from the RetroCore model / SINTRAN config): unit 0 → IOX base `01560₈`, ident code
`21₈`, interrupt level **11**; unit 1 → base `01570₈`, ident code `22₈`, level 11.

---

## 2.3 The Command Block (CB)  [MANUAL §3.3]

Built by the ND-100 in its own memory. The controller DMAs it in, executes it, and writes
the **status part (CB+6 … CB+13)** back by DMA at the end of command execution.

| CB word | Bits 15–8 | Bits 7–0 |
|---------|-----------|----------|
| `+0` | **Command word** (see §3.8 / [`03-floppy-commands.md`](03-floppy-commands.md)) | |
| `+1` | **Device address 15–0** (logical sector address on the floppy; track 0 / side 0 / sector 1 = address 0). Unused for streamer. | |
| `+2` | Device address 23–16 | **Memory address 23–16** |
| `+3` | **Memory address 15–0** (ND-100 physical address for the data DMA) | |
| `+4` | **Options** (see below) | Word Count 23–16 |
| `+5` | **Word Count / Record Count 15–0** | |
| `+6` | **Status 1** ◄ written back by controller | |
| `+7` | **Status 2** ◄ written back by controller | |
| `+10` | Last memory address 23–16 ◄ | |
| `+11` | Last memory address 15–0 ◄ | |
| `+12` | Not used | Remaining Words 23–16 ◄ |
| `+13` | Remaining Words 15–0 ◄ | |

Rules [MANUAL §3.3]:
- **Unused bytes must be zero.**
- `CB+6 … CB+13` is the **status part**, DMA-written by the controller when the command
  finishes.
- **Device Address** = the *logical* sector address, counted from the start of the floppy
  (track 0/side 0/sector 1 = 0). Not used for streamer.
- **Memory Address** = where in ND-100 memory data is read/written.

### Options field (CB+4, bits 15–8)  [MANUAL §3.3]

Bits 8–14 are Reserved (set to zero). Only **bit 15** is defined:

| Bit 15 | Meaning of CB+5 |
|--------|-----------------|
| 1 | CB+5 is a **Word Count** |
| 0 | CB+5 is a **Record (sector) Count** |

The controller inspects bit 15 to decide how to interpret the count.

### Status part written back

- `CB+10/+11` **Last memory address** (24-bit) — for `Check floppy` and streamer `Check
  cartridge`, this is the address *after* the sector/block that failed; the failing
  sector/side/track is encoded in the low part (see [`03-floppy-commands.md`](03-floppy-commands.md) §Check floppy).
- `CB+12/+13` **Remaining words** (24-bit) — bytes/words not transferred; for streamer
  `Test cartridge capacity` this is the block count written.

---

## 2.4 Status Word 1 — CB+6 (memory writeback)  [MANUAL §3.4]

**This is the word that carries the numeric error code.** It is written by DMA into CB+6.

| Bit | Meaning |
|-----|---------|
| 0 | Not used |
| 1 | RFT (Ready For Transfer) — interrupt enabled |
| 2 | Device active (same as hardware status word) |
| 3 | Device ready for transfer |
| 4 | **OR of errors** (set if any error) |
| 5 | Deleted record |
| 6 | Retry on controller (internal retry happened; not an error) |
| 7 | Hard error |
| 8 | Not used |
| **9–14** | **Error code from controller** (see §3.9 / [`06-error-codes.md`](06-error-codes.md)) |
| 15 | Not used |

> **[FIRMWARE @06b4, @1f1c — verified]** The firmware forms the numeric code as
> `high_status_byte = (code & 0x3F) << 1`. Because that byte is the *high* byte of the
> 16-bit word, the code's LSB lands at **word bit 9**, occupying **bits 9–14** exactly as the
> manual's §3.9 states (*"these error codes are given in bits 9-15 of status word 1"* — the
> value fits in bits 9–14; bit 15 stays 0). Bit 8 is always 0 (the `<<1` clears it).
>
> **This confirms error code = `code << 9`, NOT `code << 8`.** The RetroCore C# model's
> `errorCode << 9` is correct; a `: 7` bitfield starting at bit 8 (as older nd100x C did) is
> wrong.
>
> **[FIRMWARE @06bc, @06c3 — verified]** Bit 4 ("OR of errors") is set whenever the 6-bit
> code is non-zero (and additionally on a DMA-transfer error). Bit 3 ("ready for transfer")
> is set unconditionally at completion. Bit 5 ("deleted record") is set from the FDC
> deleted-address-mark path (@0edf). These flags are computed once, at completion, into a
> latch — not recomputed on each host read.

---

## 2.5 Status Word 2 — CB+7 (memory writeback)  [MANUAL §3.5]

Status Word 2 carries the **disk format / geometry** for floppy, or a copy of the streamer
status for tape. It is delivered **only in memory at CB+7** — it is *not* exposed on any IOX
status read.

### 2.5.1 Floppy, card 3106  [MANUAL §3.5.2.1]

| Bit | Meaning |
|-----|---------|
| 1 | Bytes/sector |
| 2 | Double sided |
| 3 | Double density |
| 4 | "Format read from diskette" valid (for Read Format, or on error 12 format-mismatch) |
| 7 | Not used |
| 9 | Selected unit |
| 11 | Not used |

### 2.5.2 Floppy, card 3112  [MANUAL §3.5.2.2]

| Bit | Meaning |
|-----|---------|
| 1 | Bytes/sector |
| 2 | Double sided |
| 3 | Double density |
| 4 | **5¼" drive** |
| 5 | Non-standard / "format read from diskette" valid (Read Format, or error 12) |
| 6 | **96 tpi** |
| 7 | Not used |
| 9 | Selected unit |
| 12 | Sector/track |
| 15 | Not used |

### 2.5.3 Streamer  [MANUAL §3.5.1]

| Bit | Meaning |
|-----|---------|
| 4 | A copy of the SS register (Streamer Status) |
| 10 | If an error occurred in the PREVIOUS transfer after the interrupt, this byte holds the error code; zero = no error. |

> **Known deviation:** past emulators place bytes/sector at bits 0–1 and selected-unit at
> bits 8–9. The manual (3112, §3.5.2.2) puts bytes/sector at bit 1, double-sided bit 2,
> double-density bit 3, 5¼" bit 4, 96 tpi bit 6, selected unit **bit 9**, sector/track
> bit 12. Full conformance requires the manual layout; the deviation is tied to the media-
> format input contract and is flagged in [`10-implementation-guide.md`](10-implementation-guide.md).

---

## 2.6 Hardware Control Word — IOX DEVNO+3 (write)  [MANUAL §3.6]

Loading this word **activates** the controller. Loading it interrupts the Z80 (CTC ch0).

| Bit | Meaning |
|-----|---------|
| 0 | Not used |
| 1 | **Enable interrupt on RFT** |
| 2 | **Activate Autoload** (boot floppy monitor from floppy — see [`04-boot-and-autoload.md`](04-boot-and-autoload.md)) |
| 3 | **Test Mode** (bits 9–15 then select a test — see [`09-testing-and-test-macros.md`](09-testing-and-test-macros.md)) |
| 4 | **Device Clear** (deselects drive) |
| 5 | **Enable Streamer** (route command to tape instead of floppy) |
| 6 | Not used |
| 7 | Not used |
| 8 | **Fetch Command & Execute** (the normal "run the command block" bit) |
| 9–15 | Meaning depends on bits 2/3/5/8 — see the state table below |

### Control-word state table  [MANUAL §3.6, Table 1]

The combination of bits **2, 3, 5, 8** selects one of four operating states:

| State | b2 | b3 | b5 | b8 | Action | Meaning of bits 9–15 |
|-------|----|----|----|----|--------|----------------------|
| I | 1 | X | X | X | **Load floppy monitor** (autoload/boot) | Not used — should be 0 |
| II | 0 | 1 | X | X | Run controller in **test mode** | Specifies the test (see §7) |
| III | 0 | 0 | 0 | 1 | **Fetch command from ND-100 and execute on the floppy drive** | b9 step rate, b10 in use, b11 disable precomp, b12 96 TPI, b13 compare, b14/b15 reserved |
| IV | 0 | 0 | 1 | 1 | Fetch command and execute **on the streamer** | Not used — should be 0 |

So the everyday "do a floppy command" activation is **State III**: bit 8 set, bits 2/3/5
clear, with the per-operation modifiers in bits 9–13.

---

## 2.7 Hardware Status Word — IOX DEVNO+2 or +4 (read)  [MANUAL §3.7]

Returned identically from `+2` and `+4`. **Carries no numeric error code.**

| Bit | Meaning |
|-----|---------|
| 0 | Not used |
| 1 | RFT / Interrupt Enabled |
| 2 | Device Active |
| 3 | Device Ready for Transfer |
| 4 | **OR of Errors** |
| 5 | Not used |
| 6 | **Streamer Active** |
| 7 | **Hard Error — DMA Transfer** |
| 8–10 | (unused) |
| 11 | Reserved |
| 12–13 | (unused) |
| 14 | **Streamer interface** present |
| 15 | **Dual density controller** — *always 1* on a 3106/3112 DMA card |

> **[MANUAL §6 note + FIRMWARE]** Bit 15 = 1 is how the ND-100 microcode / SINTRAN
> distinguishes this DMA (dual-density) controller from the older PIO card. It belongs on the
> **hardware status word only**, and must **not** appear in the CB+6 memory Status Word 1
> (where bit 15 is "not used"). The firmware never writes bit 15 into the status latch (the
> `code<<1` reaches at most bit 14, @06b9), which is consistent with bit 15 being asserted by
> the card's gate-array/PAL hardware, not by the Z80.
>
> **[MANUAL §6, Self-test note]** After a **self-test/RAM-test failure**, the card refuses to
> execute commands (to protect diskette data). It sets **bit 4 (OR of errors)** and **bit 7
> (hard error)** in the hardware status word, does **not** DMA a status block to ND-100
> memory, but does write Status Word 1 to the data-out register so the host can recover it
> via `IOX DEVNO+0`.

---

## 2.8 Command Word — CB+0  [MANUAL §3.8]

The command word tells the controller *what* to do. Layout differs between floppy and
streamer. See [`03-floppy-commands.md`](03-floppy-commands.md) for the floppy command word in
full and [`08-streamer-tape.md`](08-streamer-tape.md) for the streamer command word.

Floppy command word (3112), summarised:

```
 15 14 13 12 | 11 10  9  8 |  7  6 |  5  4  3  2  1  0
 Copy dest.  |   Format    | Unit  |      Function
```

- **Bits 0–5** — function code (Read=0, Write=1, Format floppy=41₈, Identify=70₈, …).
- **Bits 6–7** — unit select (0–3).
- **Bits 8–13** — format (bits 8–9 sector size, bit 10 sides, bit 11 density; see
  [`05-floppy-formats.md`](05-floppy-formats.md)).
- **Bits 12–13 / 14–15** — copy-destination unit for the Copy-floppy command.

Sector-size encoding (bits 9,8): [MANUAL §3.8.2]

| b9 b8 | Bytes/sector |
|-------|--------------|
| 0 0 | 512 |
| 0 1 | 256 |
| 1 0 | 128 |
| 1 1 | 1024 |

Bit 10: 0 = single sided, 1 = double sided. Bit 11: 0 = single density, 1 = double density.
