# 5. Summary of Floppy Formats

The format is selected by **bits 8–13 of the command word** (see
[`02-programming-interface.md`](02-programming-interface.md) §2.8). Sector size = bits 9,8;
sides = bit 10; density = bit 11. The **format number** in the tables below is the octal
value of that field. [MANUAL §3.8.2, §5]

## 5.1 Sector-size / sides / density encoding  [MANUAL §3.8.2.2]

| b9 b8 | Bytes/sector |  | b10 | Sides |  | b11 | Density |
|-------|--------------|--|-----|-------|--|-----|---------|
| 0 0 | 512 |  | 0 | Single | | 0 | Single |
| 0 1 | 256 |  | 1 | Double | | 1 | Double |
| 1 0 | 128 |  | | | | | |
| 1 1 | 1024 | | | | | | |

## 5.2 Card no. 3106 formats  [MANUAL §5.1]

| Format (oct) | Type | Sector size | Sides / density |
|--------------|------|-------------|-----------------|
| 0 | IBM SYS-32-II | 512 B | single side, single density |
| 1 | IBM 3600 | 256 B | single side, single density |
| 2 | IBM 3740 | 128 B | single side, single density |
| 3 | Illegal | | |
| 4 | Non IBM | 512 B | double side, single density |
| 5 | Non IBM | 256 B | double side, single density |
| 6 | Non IBM | 128 B | double side, single density |
| 7 | Illegal | | |
| 10 | Non IBM | 512 B | single side, double density |
| 11 | IBM SYS-34 | 256 B | single side, double density |
| 12 | Illegal | | |
| 13 | Non IBM | 1024 B | single side, double density |
| 14 | Non IBM | 512 B | double side, double density |
| 15 | IBM SYS-34 | 256 B | double side, double density |
| 16 | Illegal | | |
| 17 | Non IBM | 1024 B | double side, double density |

## 5.3 Card no. 3112 formats  [MANUAL §5.2]

The 3112 has all 3106 formats (0–17) **plus** 5¼" formats:

| Format (oct) | Type | Sector size | Sides / density |
|--------------|------|-------------|-----------------|
| 0–17 | *(identical to the 3106 table above)* | | |
| 20 | Illegal | | |
| 21 | Illegal | | |
| 22 | Basic 5¼" | 128 B | single sided, single density |
| 23–33 | Illegal | | |
| 34 | IBM PC | | |
| 35–57 | Illegal | | |
| 60–77 | User specified | | |

## 5.4 Notes and format-related errors

- **Illegal formats** (3, 7, 12, 16, and the 3112 illegal ranges): specifying one on a
  read/write raises **error 13₈ ("Illegal format specified")**. The firmware tests the
  format field and rejects 3 and 8. [MANUAL §3.10]
- **Format mismatch:** if the specified format ≠ the format actually on the diskette →
  **error 12₈**, and Status Word 2 returns the diskette's actual format. [MANUAL §3.10]
- **Single/double-sided mismatch:** specifying double-sided with a single-sided diskette →
  **error 14₈**; the reverse → **error 15₈**. [MANUAL §3.10]
- **Read format (command 42₈)** returns the on-diskette format into **Status Word 2** (CB+7).
  On the 3112, Status Word 2 also carries 5¼"-drive (bit 4), 96 tpi (bit 6) and sector/track
  (bit 12). See [`02-programming-interface.md`](02-programming-interface.md) §2.5.2.

## 5.5 The common SINTRAN dual-density format  [cross-check: RetroCore model]

The everyday ND double-density floppy is **8 sectors/track × 77 tracks × 2 sides**, 1024
bytes/sector (format 17₈), giving 616 pages; the single-density IBM SYS-32-II (format 0,
512 B/sector, 8 sectors × 77 tracks, single side) gives 154 pages. These are the two the ND
floppy driver most commonly uses.

> The device address in CB+1 is a **logical sector number** counted from track 0 / side 0 /
> sector 1 = 0. An emulator maps `(logical sector) → (track, side, sector)` using the selected
> format's geometry (sectors/track and sides). The `Check floppy` error report packs
> side/track/sector into one word — see [`03-floppy-commands.md`](03-floppy-commands.md) §Check floppy.
