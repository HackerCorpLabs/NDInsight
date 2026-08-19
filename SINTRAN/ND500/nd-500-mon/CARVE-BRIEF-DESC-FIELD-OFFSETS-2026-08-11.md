# Carve brief: DESCRIPTION-FILE:DESC segment-entry field offsets

**Audience:** a Claude session on the Windows side with the `ghidra` MCP server available
(`mcp__ghidra__disassemble`, `get_disassembly`, `xrefs`, `search_bytes`, `set_comment`,
`rename_symbol`). Written 2026-08-11 by the WSL nd500x session, which has no Ghidra access.

Paths below are relative to the NDInsight repository root.

## The question

`DESCRIPTION-FILE:DESC` has a 192-byte Segment Entry whose field offsets past `SNAME` were
marked **UNABLE TO DETERMINE** in `SINTRAN/File-Formats/DESCRIPTION-FILE-FORMAT.md` after five
failed methods. This brief asks for one thing: **read the field offsets out of the ND-500
Monitor's own code**, which is the program that reads and writes this file.

## What is already established (empirical, do not re-derive)

Byte offsets from the start of a 192-byte Segment Entry, 4-byte **big-endian**, sizes stored as
**value = size - 1** (a last-byte offset, which is why every search for the literal file size
failed):

| Offset | Field | Evidence |
|---|---|---|
| +88 | PLB | 0 in all four entries |
| **+92** | **PSIZE - 1** | 4/4 exact |
| +96 | DLB | 0 in three entries, 75834 in the fourth |
| **+100** | **DSIZE - 1** | 3/4 exact |
| +104 | DEBUGINFO | 0 for both scratch segments, non-zero for both real programs |

Measured against four real entries in two independently produced DESC files:

| Segment | `.pseg` | +92 | `.dseg` | +100 |
|---|---|---|---|---|
| SCRATCH-SEG-01 (H02 floppy) | 5 | 4 | 1029 | 1028 |
| LINKAGE-LOAD-H02 | 123989 | 123988 | 2184977 | 2109654 (only mismatch) |
| SCRATCH-SEG-01 (LED floppy) | 5 | 4 | 1029 | 1028 |
| LED-B03 | 223695 | 223694 | 394525 | 394524 |

**CONVERT-DOMAIN is not a witness.** Patching +92 from 123988 to 16384 in a real DESC and
re-running `CONVERT-DOM-A03` under nd500x produced a byte-identical 2,316,049-byte `.DOM`. It
asks the filesystem (MON 62B) instead of reading these fields.

## What to confirm or refute

The prediction, in ND-100 **word** offsets (the machine is word-addressed, so byte/2):

| Word offset | Field |
|---|---|
| `54B` | PLB |
| `56B` | PSIZE |
| `60B` | DLB |
| `62B` | DSIZE |
| `64B` | DEBUGINFO |

A hit would look like a double-word load/store (`LDD`/`STD`) at `,X 56` / `,X 62` style
displacements off a record base pointer, in code reachable from the DESC file read. Report the
real offsets whatever they turn out to be - a refutation is as valuable as a confirmation.

Also still open: `LINKAGE-LOAD-H02` is the only entry with non-zero DLB (75834) and the only one
whose DSIZE does not match its file (2109655 vs 2184977, difference 75322 = DLB - 512). Its
`.dseg` has no leading zero run (data starts at byte 4), so it is not a hole below DLB. If the
code shows how DLB and DSIZE combine, that resolves it.

## The binary and how to load it

File: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog` (253,356 bytes), the ND-100 program
`MON-DEBUG:PROG`, ND-500/5000 Loader/Debug Monitor J04.

Two banks, **both based at word 0**, so they must be two Ghidra programs (or bank 2 as an
overlay). Raw Binary, ND-100, big-endian:

| Program | Base | File offset | Length |
|---|---|---|---|
| bank 1 (code) | 0 | `0x200` | `0x1FECA` (65,381 words) |
| bank 2 (data) | 0 | `0x20200` | `0x1DBAC` (60,886 words) |

Header words W0..W5 verified as `11B, 11B, 0B, 177544B, 0B, 166725B`; bank 2 ends exactly at the
last file byte. The separate `nd-500-mon-j04-bank1.bin` / `-bank2.bin` in the same directory are
byte-identical extracts if a plain load is preferred.

Import landmarks: bank 2 `0x21B9` (`020671B`) = command dispatch array; bank 2 `0x41DC`
(`040734B`) = `DESCRIPTION-FILE'`; bank 1 word 1 = `MON-DEBUG:PROG''` packed 2 chars/word.

## Where to start - already located from the bytes

Code lives in bank 1, strings in bank 2, at the **same** word addresses (PTM selects the bank),
so a bank-1 pointer word holding `040734B` is a reference to the bank-2 string.

| Bank 1 address | What |
|---|---|
| `016277B` | pointer word -> `040734B` = `DESCRIPTION-FILE'` (only reference in the image) |
| `016302B` | pointer word -> `040745B` = `.DESC` |
| `016213B` | `LDT +52` loads the filename pointer - real code |
| `016226B` | `LDT +44` loads the `.DESC` pointer - real code |
| `016200B`-`016240B` | builds both as PLANC descriptors (`SAA 20` = length 16, matches `DESCRIPTION-FILE`), then `JPL I` out |
| `041445B` | pointer word -> `046205B` = the `(SYSTEM)` / `ERROR IN PLACE SEGMENT` string group |
| `041403B`, `041435B` | real code loading that pointer - adjacent to the segment loader |
| `042115B`-`043010B` | the SEGMENT LOADER (445 words), issues MON 60B subfn `006B` ISEGLOAD at `042230B` and `042535B` |

File-I/O primitives, all in bank 1 (69 MON instructions total in the image):

| Address | Call |
|---|---|
| `176471B`, `177152B` | MON 50B OPEN |
| `176740B`, `177216B` | MON 117B RFILE |
| `176776B` | MON 120B WFILE |
| `176505B` | MON 62B GetBytesInFile |
| `176362B`, `177221B` | MON 43B CLOSE |

The `177152B` / `177216B` / `177221B` trio is the data-bank loader (documented in
`nd-500-mon-j04.prog.md`), not the DESC path. So the DESC read almost certainly goes through the
`176740B` RFILE helper.

**The path to walk:** `016200B` (open DESCRIPTION-FILE:DESC) -> its caller -> the RFILE call that
reads a record -> the code that indexes the returned buffer. The field displacements are in that
last step.

## Traps that cost this session time

- `nd-500-mon-j04.prog.asm` disassembles **pointer words as instructions**. At `016277B` it
  prints `MIN ,B -44`; that is the pointer to the string, not code. `016264B`-`016277B` is a
  literal pool. Do not read the shipped `.asm` straight through in these regions.
- P-relative effective address = **address of the instruction + displacement** (verified:
  `016253 JPL I 33 -> 016306`), not next-instruction-relative.
- `JPL` is opcode `0o134`, not `0o130`. A resolver with that wrong silently finds nothing and
  reads as "this routine makes no calls".
- `LDF`/`STF` here move 3-word PLANC descriptors (pointer + length), not floats.

## What to report back

1. The word offsets actually used for the segment-entry fields, with the addresses of the
   instructions that use them.
2. Whether sizes really are stored as size-1, from the code (e.g. an `AAA 1` / `+1` on the way
   in or out).
3. Anything about DLB's role that explains the LINKAGE-LOAD-H02 mismatch.
4. If the record read turns out not to be in this program at all, say so plainly - that is a
   real result and redirects the search to NLL.
