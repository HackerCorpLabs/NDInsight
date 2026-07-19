# 3. Floppy Command Descriptions

All command codes are in the **function field (bits 0–5)** of the Command Word at CB+0.
Codes are octal. The tables below are the 3112 set (the 3106 set is identical except it has
no `Identify floppy`). [MANUAL §3.8.2, §4.2]

## 3.1 Floppy command word layout  [MANUAL §3.8.2.2]

```
 15 14 13 12 | 11 10  9  8 |  7  6 |  5  4  3  2  1  0
 Copy dest.  |   Format    | Unit  |      Function
```

| Field | Bits | Meaning |
|-------|------|---------|
| Function | 0–5 | The command (table below). |
| Unit | 6–7 | Drive select 0–3. |
| Format | 8–13 | b8/b9 sector size, b10 sides, b11 density (see [`05-floppy-formats.md`](05-floppy-formats.md)). |
| Copy dest. | 12–13 (3106) / 14–15 | Destination drive for `Copy floppy`. |

## 3.2 Function codes  [MANUAL §3.8.2.2]

| Octal | Hex | Command | Data transfer |
|-------|-----|---------|---------------|
| 00 | 0x00 | **Read data** | Floppy → ND-100 |
| 01 | 0x01 | **Write data** | ND-100 → Floppy |
| 02 | 0x02 | **Find EOT** (read without data transfer) | none (status only) |
| 05 | 0x05 | **Write EOT** (write deleted record) | ND-100 → Floppy (mark) |
| 20 | 0x10 | **Read status** (3106 lists this; see note) | status only |
| 41 | 0x21 | **Format floppy** (whole diskette) | ND-100 → Floppy |
| 42 | 0x22 | **Read format** | → Status Word 2 |
| 43 | 0x23 | **Read deleted record** | Floppy → ND-100 |
| 44 | 0x24 | **Write deleted record** | ND-100 → Floppy |
| 54 | 0x2C | **Copy floppy** (drive → drive) | internal |
| 55 | 0x2D | **Format track** (one track, one side) | ND-100 → Floppy |
| 56 | 0x2E | **Check floppy** (read + CRC test) | none (status only) |
| 70 | 0x38 | **Identify floppy** *(3112 only)* | status only |

> **Note on codes 0x38 / 0x10.** The manual's floppy command table (§3.8.2.2) lists `70₈ =
> Identify floppy` as a real host command on the 3112. Be aware that the on-card firmware
> *also* uses the raw constant `0x38` internally as the base of the **self-test** status code
> written to the display/host (`(0x38 + class) << 1`, FIRMWARE @01cc) — that internal use is
> unrelated to the host command word. When emulating at the IOX/command level you only care
> about the **command-word function field**; treat `70₈` as Identify. The critical
> requirement (§below) is that **every command, including Identify and any unrecognised
> function code, must run to completion and signal RFT** — the firmware acknowledges even
> rejected commands with an error status; it never silently drops one.

## 3.3 Command semantics  [MANUAL §4.2]

### Read data (00)
Data is read from floppy to ND-100 memory. Start address is the **logical sector address**
(CB+1); the length is either a **word count** or a **sector count** per Options bit 15.
Transfer always **starts at the beginning of a sector**, but the word count may be any number
of words (so a partial final sector is allowed). [MANUAL §4.2.1]

### Write data (01)
Same as Read data, but ND-100 → diskette. [MANUAL §4.2.2]

> **Write-protect:** attempting Write on a write-protected diskette raises **error 16₈
> ("Write protected diskette/cartridge")** and the write is aborted. [MANUAL §3.10, §4]
> **[FIRMWARE @0e84, @1ea2 — verified]** The firmware reads FD1797 status bit 6 (write-
> protect) via `IN (0x70)`; on a write command it calls the error stub `0x1ea2` (`RST 08h`,
> code byte `0x8E` → `0x0E` = octal 016), which is a **hard-error class** report. Because the
> `RST 08h` path never returns to the caller — it diverts to reinit + host completion — the
> **write never proceeds**.

### Find EOT (02)
Like Read data but data goes only to the controller's local buffer — **no transfer to
ND-100 except status**. Bit 5 of Status Word 1 tells whether an EOT (deleted record) was
hit. [MANUAL §4.2.3]

### Write EOT (05)
The sector named in CB is read into local memory and **written back as a deleted record**.
[MANUAL §4.2.4]

### Format floppy (41)
The diskette in the selected drive is fully formatted to the format in the command word.
After writing, each formatted track is verified; if a track keeps failing, **error 11₈
("diskette defect / impossible to format")** is raised. [MANUAL §4.2.5, §3.10]

### Read format (42)
The format is read off the diskette and returned in **Status Word 2** (CB+7). The disk
address + format in the CB indicate where to read the format from. [MANUAL §4.2.6]

### Read deleted record (43) / Write deleted record (44)
Read/write a record explicitly marked as a deleted record. [MANUAL §4.2.7–8]

### Copy floppy (54)
Copies the **entire** diskette from one drive to another. Destination drive is in the copy-
destination field of the command word. [MANUAL §4.2.9]

### Format track (55)
Formats **one track on one side**. Used to make IBM-compatible diskettes. [MANUAL §4.2.10]
> **[FIRMWARE @1325, @1240/@1245]** The firmware builds the FD1797 Write-Track byte image
> from a template (variant A `0x12b4`, variant B `0x12e9`) via a run-length expander
> (`BuildTrackImageRuns` @129a), staging it in the `0x2200` buffer, then issues WD179x
> Write-Track (0xF0/0xF2), with 3× retry.

### Check floppy (56)
Reads data into local memory only, to test for CRC errors. **Stops at the first error.** The
address of the failing sector is reported in **LAST MEMADDR** (CB+10/+11) — and the note is
important: *the address given is to the sector **after** the one that failed*. The layout of
that word: [MANUAL §4.2.11]

```
 15 | 14 13 12 11 10 9 8 7 6 5 4 3 | 2 1 0
Side |        Track number         | Sector
```

### Identify floppy (70, 3112 only)
Returns identifying/status information about the drive/diskette to the host. Must complete
and interrupt like any other command. [MANUAL §3.8.2.2]

## 3.4 Completion behaviour (applies to every command)  [MANUAL §1, FIRMWARE]

1. Controller DMAs the command block from ND-100 into Z80 RAM `2080h`.
2. Executes the command (FD1797 + AM9517).
3. DMAs the **6-word status block** back into ND-100 memory at **CB+6 … CB+13**
   (Status 1, Status 2, last address, remaining words).
4. Sets **RFT**, raising an ND-100 interrupt if control-word bit 1 enabled it.

> **[FIRMWARE @030c, @03e3]** An invalid/rejected command (e.g. pointer validation fails, or
> an unimplemented function) branches to an error-exit that loads a status/message code, sets
> the host status registers, and **still hands completion back to the ND-100** — it is never
> silently dropped. An emulator's command dispatch must therefore have a *default* case that
> completes with an error, not a no-op.
