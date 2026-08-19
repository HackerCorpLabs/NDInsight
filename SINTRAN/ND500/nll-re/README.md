# NLL reverse-engineering material

Working material for the Linkage-Loader carve. NLL is the ND-500 domain
`LINKAGE-LOAD-H02`, the program that owns both `DESCRIPTION-FILE:DESC` and the
`:LINK` files - see [NLL-LINKAGE-LOADER-OVERVIEW.md](../NLL-LINKAGE-LOADER-OVERVIEW.md)
for what the product is, and
[CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17.md](../nd-500-mon/CARVE-ANSWER-FOUR-OPEN-QUESTIONS-2026-08-17.md)
question 3 for the carve that decoded the `:LINK` format out of this code.

## What is committed, and what is not

| File | Size | Committed |
|---|---|---|
| `LINKAGE-LOAD-H02.PSEG` | 123,989 bytes | Yes - the ND-500 code, the carve evidence |
| `LINKAGE-LOAD-H02.UTIL` | 2,440 bytes | Yes - 7-bit text, "Utilities for LINKAGE-LOADER" |
| `LINKAGE-LOAD-H02.DSEG` | 2,184,977 bytes | No - re-extract from the floppy image |
| `LINKAGE-LOAD-H02.PSEG.dis` | 1,998,562 bytes | No - regenerate from the PSEG, command below |

The `.dis` is left out because it regenerates exactly, and because its own
header line records the absolute path of whatever file it was run on - which
must not be committed. The DSEG is left out for its size; it is a plain
extraction with nothing derived in it.

## Where the binaries come from

Floppy image `ND-disk-00042.img` (the 210319 H02 media - the Linkage-Loader
product floppy, `210319H02-XX-01D` in the archive). The image is not part of
this repository; the image set is indexed in the nd500x repository's
`docs/EXTERNAL-ARTIFACTS.md`.

Extract with `ndtool` from the nd500x repository:

    ndtool -x -F 'FLOPPY-USER/*' -o <output-dir> ND-disk-00042.img

Take file sizes from `ndtool -t <image>`, never from the extracted copies: a
file with a non-zero byte count but zero allocated pages extracts as empty.

## Regenerating the disassembly

There is no ND-500 processor in Ghidra. `nd500-dis` from the `pcc-nd500` tree
(also indexed in `docs/EXTERNAL-ARTIFACTS.md`) disassembles a raw PSEG and
annotates monitor calls inline. It is a WSL binary and runs from Windows via
`wsl`, so the ND-500 side of a carve does not need the Linux session.

    nd500-dis -a -b 0xB0000000 -noansi LINKAGE-LOAD-H02.PSEG -O LINKAGE-LOAD-H02.PSEG.dis

31,747 lines. Verified 2026-08-17: this command reproduces the listing the
carve was read from, identical on every line except the `; File:` header that
names the input path.

The base address is not a guess. NLL's own domain entry in the DESC file it
ships with gives a start address of `0xB0000DD1` - segment 22 - and
PBITMAP = DBITMAP = 2^22, bit 22 set for that same segment. Segment 22 also
matches the live `CONVERT-DOM` observation already on record.

## Key addresses in the PSEG listing

All virtual, from the carve:

| Address | What |
|---|---|
| `B001166C` | the `:LINK` serializer - sorts by the value word at +8, copies 32 bytes per entry |
| `B0011769` | the 32-byte copy itself (`bmove ... $0x20`) |
| `B00116D7` | the page rounding that produces the 2048-byte header page in K-era files |
| `B00068CC` | CLOSE-SEGMENT worker |
| `B0006D6D` | the SMAX call whose argument makes every `:LINK` exactly 32k+1 bytes |
| `B0001A2C` | CLOSE-SEGMENT dispatch stub (command index 20) |
| `B001C214` | RFILE wrapper |
| `B001C23A` | WFILE wrapper |
| `B001CC5F` | SMAX wrapper |
| `B001CC73` | SETBT wrapper |

The command tables live in the DSEG, not the PSEG: a descriptor table of
12-byte `{pointer, 0, length}` entries at DSEG `0x1368`, and an index-parallel
table of 92 code pointers at DSEG `0x4A0C`.
