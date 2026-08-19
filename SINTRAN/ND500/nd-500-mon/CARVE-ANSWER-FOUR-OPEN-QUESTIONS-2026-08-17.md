# Carve answer: the four open questions of 2026-08-17

**Answers:** `CARVE-BRIEF-FOUR-OPEN-QUESTIONS-2026-08-17.md` (same directory)
**Date:** 2026-08-17 (Windows session with Ghidra)
**Binaries:**
- `nd-500-mon-j04.prog` (MON-DEBUG:PROG, ND-500 Loader/Debug Monitor J04) - Q1, Q2, Q3
- `tools/sintran-segment-carver/versions/L-VSX-500/re/006-S3FS.dis` (SINTRAN L file system
  segment, with the FILSYS symbol table) - Q4

**Method:** same as the 2026-08-11 DESC answer - walk the shipped disassembly, then re-read
every load-bearing word (instructions, pool words, string pointers, constants) from the raw
bytes through the Ghidra import. Ghidra `nd-500-mon-j04.prog` now carries pre-comments at the
Q1/Q2 addresses. Q1 and Q2 were additionally checked against all 13 real DESC files in
`SINTRAN/File-Formats/samples/`.

**Verdicts in one line each:**

1. **Domain entry: SOLVED.** All field offsets code-proven; two manual placements were wrong
   (PBITMAP/DBITMAP are at bytes 48/52, not 46/50; procPrior+flag are one packed word).
2. **Bytes 74-84: ADJUDICATED - the manual was right.** COMSEGSIZE and N100SEGNO arrays; the
   "two counted byte strings" reading was a misread loop idiom.
3. **:LINK: DECODED.** MON-DEBUG:PROG is ruled out; the writer is NLL's own ND-500 code,
   carved the same day with `nd500-dis` (WSL tool, runnable from Windows): the file is the
   loader table dumped at CLOSE-SEGMENT as 32-byte cells in ascending value order, and the
   `32k+1` length is the SMAX off-by-one. Verified against 10 of the 11 samples.
4. **OPENF: open does NOT clear the byte count - CLOSE does.** Sequential-write open sets the
   in-core session max byte pointer to -1; the object entry on disk is rewritten from it at
   close. The emulator's `"r+b"` is right at open, but the truncation belongs at close.

---

## Q1. Domain-entry field offsets - SOLVED

The domain-entry printer is `014035B`-`014520B`. Shape per field: `SAA len` + `SWAP` +
`LDT <pool>` (label descriptor) + `STF ,X 6` + `JPL I -> 013177B` (print label), then
`LDX <pool>` = buffer `037651B` and one field load, printed via `013224B` (double), `013357B`
(number), or `013741B` (flag bit). Each label print is followed by exactly one field load -
the pairing is unambiguous, as with the segment entry.

Two literal pools serve the routine: `014230B`-`014254B` and `014453B`-`014470B`. Both hold
the buffer pointer `037651B` (raw bytes `3f a9`, verified) plus all label string pointers,
each byte-verified against the bank-2 string run at bytes `0x80C6`-`0x81BF`.

| Byte | Word | Field | Size | Evidence (all raw-byte-verified) |
|---|---|---|---|---|
| 0 | `0` | SEGLINK | 4 | `014325B LDD ,X 0` (`2c 00`), label `$Segptr:` |
| 4 | `2` | DNAME | 16 | name loop `014061B`-`014121B`: `AAT 2` (`f6 02`) + `LBYT`, `SAT 17` limit = exactly 16 iterations; `'` (47B) tested as terminator at `014066B` |
| 20 | `12B` | CHILDDOMAINS | 6 | `014407B AAT 12` (`f6 0a`) + `LBYT` loop, label `$Child domains   : `; count from byte 27 |
| 26 | `15B` hi | MOTHER | 1 | `014157B LDA ,X 15` (`4c 0d`) + `SHA ZIN SHR 10` (high byte), label `$Owner:` |
| 27 | `15B` lo | CHILDINDEX | 1 | `014176B LDA ,X 15` + `SHL 8`+`SHR 8` (low byte), label `  Childindex:` - and the SAME byte is the loop count for the child list (`014375B`) and the "$No child domains" test (`014357B`) |
| 28-29 | `16B` | FLAG+PRIOR word | 2 | bit 15 `alton` (`014441B`, `w>>15`), bit 14 `dinuse` (`014472B`, `(w<<1)>>15`), bit 13 `occup` (`014505B`, `(w<<2)>>15`); PRIOR = bits 5-12 (`014216B`, `(w<<3)>>8`, label `  Prior:`) |
| 30 | `17B` | STADR | 4 | `014132B LDD ,X 17` (`2c 0f`), label `  Start address:` |
| 34 | `21B` | ENABLEINT | 4 | `014261B LDD ,X 21` (`2c 11`), label `$Enableint:` |
| 38 | `23B` | THA | 4 | `014277B LDD ,X 23` (`2c 13`), label `  THA:` |
| 42 | `25B` | SYSENABL | 4 | `014312B LDD ,X 25` (`2c 15`), label `  Sysenable:` |
| 46 | `27B` | (unprinted) | 2 | no access anywhere in the printer; zero in all samples |
| 48 | `30B` | PBITMAP | 4 | `014340B LDD ,X 30` (`2c 18`), label `  PSEG use:` |
| 52 | `32B` | DBITMAP | 4 | `014353B LDD ,X 32` (`2c 1a`), label `  DSEG use:` |

Total: words 0-33B = 56 bytes exactly.

**Where the manual was wrong:**

- **PBITMAP/DBITMAP sit at bytes 48/52, not 46/50.** Byte 46-47 (word `27B`) is an unprinted
  word - it is also the missing 2 bytes that made the manual's field list sum to 54 in a
  56-byte entry.
- **procPrior and flag are not two bytes.** They are one 16-bit word: flag bits at the top
  (15/14/13), an 8-bit priority spanning the byte boundary at bits 5-12.
- The monitor's flag-bit label strings are `alton`, `dinuse`, `occup` (bank 2 bytes
  `0x81AE`/`0x81B4`/`0x81BA`). Candidate expansions ("ALT on"? "D in use"? "occupied"?) are
  guesses; the raw strings are what the code prints.

**Sample check (13/13 DESC files, script over `samples/`):** every used entry parses; MOTHER
= 0xFF (no mother) and CHILDINDEX = 0 on every root domain; `dinuse` set on every used entry
and no unused one; STADR = 0 for SCRATCH-DOMAIN and segment-1 addresses (`0x8000004`...) for
real programs; PBITMAP = DBITMAP = 2 everywhere = bitmap with bit 1 set, matching STADR in
segment 1 (supports the bitmap reading). **Stronger confirmation found later the same
session:** the LINKAGE-LOAD-H02 domain entry (16-char name, no terminator - initially missed
by a name filter that required the apostrophe) has STADR 0xB0000DD1 = segment 22 and
PBITMAP = DBITMAP = 4194304 = 2^22 - bit 22 for segment 22, and segment 22 matches the
CONVERT-DOM live data point already on record. The bitmap reading is confirmed by two
independent segment numbers. No sample exercises the child machinery, priority, `alton` or
`occup` - those parts are code-proven only.

Also confirmed in passing: the header line prints `$Domain ` + the domain INDEX (from a
bank-2 variable at word `040127B`) + `: ` - so the index printed by LIST-DOMAIN commands is
positional, not stored in the entry.

## Q2. Segment-entry bytes 74-84 - the MANUAL was right

Word `37B` (byte 62) is **COMSEGNO**, the number of common segments. It is not a character
count. Three independent code facts settle it:

1. The monitor prints word `37B` itself under its own label `$Comsegno: ` (`015150B`
   `LDX 174` -> pool `015344B` = `037705B` buffer (`3f c5`), `015151B LDA ,X 37`).
2. The same count bounds **four** parallel arrays, two of which are not byte arrays at all:
   - `$Comsegaddr: ` - **uint16[5]** at word `40B` (byte 64): `015233B AAX 40` (`f7 20`),
     word-indexed loop `015220B`-`015245B`;
   - `$Comsegsize: ` - byte[5] at word `45B` (byte 74): `015270B AAT 45` (`f6 25`) + `LBYT`;
   - `$N100segno:  ` - byte[5] at word `50B` (**byte 80**, not 79): `015324B AAT 50`
     (`f6 28`) + `LBYT`;
   - `$Addsgelem:  ` - **12-byte elements x5** at word `102B` (byte 132): `015372B MPY`
     with constant **6** (words) at `015553B` (`00 06`), `LDD` of each element's first
     double. 5 x 12 = 60 bytes = exactly the rest of the 192-byte entry.
   A "character count" cannot bound a word array and a double array.
3. The `LBYT` loops print elements through `013301B`, which is a thin wrapper (D :=
   zero-extended A, call resident library number converter at `172340B`) - the **same
   routine that prints the domain child-domain list as numbers**. The string path is
   `013177B` with a {pointer,length} descriptor, and these loops never touch it.

So: bytes 74-78 = COMSEGSIZE[5], byte 79 pad, bytes 80-84 = N100SEGNO[5], byte 85 pad.
Max common segments = 5 (the three arrays and the element block all size for 5).

The earlier "two counted byte strings" record in `DESCRIPTION-FILE-FORMAT.md` /
`desc-format.json` / `desc.h` is retracted; both docs are updated (desc.h is in the WSL tree
and needs the same edit: comsegno u16@62 verified, comsegaddr u16[5]@64, comsegsize u8[5]@74,
pad@79, n100segno u8[5]@80, pad@85, indplog/inddlog bitfield@130, addsgelem 12-byte x5 @132).

Bonus fields pinned by the same run: word `101B` (byte 130) = `  Indplog: ` bits 10-15 /
`  Inddlog:  ` bits 5-9 (`015164B`, `015200B`).

All 13 samples have COMSEGNO = 0 - consistent, and why file evidence alone could never
adjudicate this.

## Q3. :LINK - reader identified AND record layout decoded

**Negative result, byte-verified: `MON-DEBUG:PROG` never opens a `:LINK` file.**

- Its file-type tables in bank 2 hold `rw` + `PSEG`/`DSEG` (byte `0x8D8A`) and
  `R` + `PSEG PSEG DSEG DATA PROG DATA PROG` (byte `0x996E`). No `LINK`.
- Every occurrence of the string `LINK` in the whole image (13 hits, all inspected) is
  something else: `$END OF LINK$` / `$ERROR IN LINK$` belong to the debugger's FOLLOW-LINK
  memory-chain walker (its NEXT/EXIT command table sits right beside them at bank 2
  `0xE2B6`); `LINK KEY INCORRECT`, `LINK SEGMENT:`, `SEGMENT MODIFIED AFTER LINKING` are the
  linked-segment machinery; `PAGELINK`/`RXLINK` are table names.
- The J04 binary contains no loader commands at all (no OPEN-SEGMENT, CLOSE-SEGMENT,
  LOAD-SEGMENT, GLOBAL-ENTRIES strings). It is the debug monitor; the loader is elsewhere.

**Positive identification: the reader/writer is NLL itself, and NLL is ND-500 code.**
NLL is the ND-500 domain `LINKAGE-LOAD-H02` (`:PSEG` 123,989 bytes, `:DSEG` 2,184,977
bytes), extracted this session from the floppy image `ND-disk-00042.img` (the 210319 H02
media, outside this repository) and staged at, relative to the repository root:

    SINTRAN/ND500/nll-re/LINKAGE-LOAD-H02.PSEG   (committed)
    SINTRAN/ND500/nll-re/LINKAGE-LOAD-H02.UTIL   (committed - 7-bit text, "Utilities for LINKAGE-LOADER")
    SINTRAN/ND500/nll-re/LINKAGE-LOAD-H02.DSEG   (NOT committed - 2.1 MB, re-extract per nll-re/README.md)

The DSEG carries the full loader command table (OPEN-SEGMENT, CLOSE-SEGMENT, LINK-SEGMENT,
LIBRARY-SEGMENT-LINK, FORCE-SEGMENT-LINK, SET-AUTO-LINK-SEGMENT, SET-AUTO-LOAD-FILE,
GLOBAL-ENTRIES, LIST-NRF-ENTRIES, ...) and the file-type list `NRF'BRF'LINK'SYMB'DATA'RTFIL'`
- the `LINK` type the ND-100 monitor lacks. The other consumer is the symbolic debugger,
`(SYSTEM)DEBUGGER`, also an ND-500 domain (the monitor invokes it by that literal name,
bank 2 `0x42B2`/`0x450E`).

**The carve happened the same day.** There is no ND-500 processor in Ghidra, but
`nd500-dis` (`bin/nd500-dis` in the `pcc-nd500` tree indexed by the nd500x repository's
`docs/EXTERNAL-ARTIFACTS.md` - a WSL binary, callable from Windows via `wsl`; note the
brief's "WSL cannot reach Ghidra" is one-directional) handles raw PSEG files and even
annotates monitor calls. The 31,747-line listing the carve was read from is not committed
(its own header line records the absolute path of the input file); it regenerates exactly
from the committed PSEG with the command in `SINTRAN/ND500/nll-re/README.md`, verified
line-for-line 2026-08-17 (base 0xB0000000 -
the DESC domain entry gives STADR 0xB0000DD1 = segment 22, and PBITMAP/DBITMAP = 2^22, a
second independent confirmation of the Q1 bitmap reading).

**The carve chain** (all virtual addresses in the PSEG listing):

- Command dispatch: descriptor table at DSEG 0x1368 (12-byte {ptr,0,len} entries), handler
  table at DSEG 0x4A0C (92 code pointers, index-parallel). CLOSE-SEGMENT = index 20 ->
  stub `B0001A2C` -> worker `B00068CC`.
- All file I/O funnels through single MON wrappers: RFILE `B001C214`, WFILE `B001C23A`,
  SMAX `B001CC5F`, SETBT `B001CC73` (monitor calls appear as `call $0xF8000000 + 2*monno`,
  annotated by nd500-dis).
- **The serializer `B001166C`** (reached from the close loop, size consumed by the SMAX call
  at `B0006D6D`): walks the loader-table hash chains (next pointer = entry word +0), and
  selection-sorts by the value word +8 - the manual's "numerically sorted order". Per entry:
  mark dumped (set bit 4 of word +4, `B0011763`), **copy exactly 32 bytes**
  (`by bmove @b.0x44,r2.(0x0),$0x20` at `B0011769`), overwrite the written record's word +0
  with the address of the next record (`B0011771`-`B0011776`), advance by 32. After the last
  record: store one zero word (`B0011783`) and return cursor & 0x7FFFFFF = 32k.
- **The 32k+1 law:** the caller passes 32k to SMAX; per Q4, SMAX stores it directly as the
  max byte pointer and the byte count = max byte pointer + 1. So every :LINK is 32k+1 bytes
  and the trailing 0x00 is the first byte of the zero end-word. Closed.

**Record layout (32-byte cell, verified against 10/11 samples - full table in
`SINTRAN/File-Formats/link-format.json` and `LINK-FILE-FORMAT.md` section 4a):**
link@0 (K-era+: 0x18000000 | next record's file position; 1982: -1; H00: old in-memory
elink), sl@4, nleOper@5, ident@6, cw@7 (cwBits layout - the dump filter tests bits 0/4/5),
val@8 (ascending, 100% of pairs in all conforming samples), size@12, name@16 (sl bytes, rest
heap slack - which is what looked like "shifting fields"). K-era+ files start with one
2048-byte header page, zero except word 0 = 0xFFFFFFFF (COBOL-85's "marker once then
zeros"), from the serializer's page rounding at `B00116D7`. Still open: the L-era
SL202-FO-L27 string/module regions and the non-symbol node types in tail cells.

## Q4. OPENF: open does NOT clear the byte count; CLOSE rewrites it

Carved in the SINTRAN L file system segment (`006-S3FS.dis` + FILSYS symbols). The relevant
state lives in two places:

- **Object entry** (on disk): `OBYTE` = word `62B` = the stored **max byte pointer** (byte
  count - 1; `NBYTS` = word `60B` = page count). Symbol values from
  `SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT`.
- **Datafield** (in-core, per open file): word `17B` = current byte pointer (double), word
  `21B` = session max byte pointer (double). Proven by the SMAX/SETBT worker: `SMAXB@072620B`
  / `SBYTE@072622B` share a body; the SMAX path stores the argument with `STD ,X 21`
  (`072737B`); SETBT stores value-1 into `,X 17` (`073011B`/`073032B`).

**What open does - `SOFT@066123B`** (datafield init, called from `FFILE@065144B`):

1. Generic init (`066156B`-`066172B`): byte pointer := -1 (`STD ,X 17`), and the session max
   byte pointer is COPIED from the object entry: `LDD ,X 62` (object) -> `STD ,X 21`
   (datafield) at `066172B`. **The object entry itself is not written.**
2. Then per access code (`066355B`-`066512B`):
   - **access 0 (sequential write, W): `066360B`-`066374B` OVERWRITES the session max byte
     pointer with -1** (`SAA -1` -> D -> `STD ,X 21` at `066374B`). The file's readable
     length for this session becomes 0. The object entry is still untouched.
   - access 1 (R) and 4 (RW): no `,X 21` store - the stored byte count stays visible.
   - access 5 (WA, append): `066445B`-`066450B` sets BOTH `,X 17` and `,X 21` from object
     `,X 62` - positioned at end, length kept. (Access 11B does the same at `066506B`.)
3. Object-entry byte counts are written at exactly six places in the whole segment, and none
   is in the open path: `COBJE@061502B` and `CHIGV@063315B` write **-1 at file creation**;
   `DWOBJ@104410B` copies entry to entry; `VERSI@164466B` writes -1 (new version);
   `GPADR@074521B` and `FCL2@070132B` write back from the datafield.

**What close does - `FCL2@070132B`:** the writeback block at `071241B`-`071314B` stores
datafield `,X 25` -> object `,X 64`, updates the page count (object `,X 60`), then
**object `,X 62` := datafield `,X 21`** (`071311B`-`071314B`), then decrements the object's
open count (`,X 50`, `071315B`-`071321B`). The writeback is not gated on access mode - for a
read-open it rewrites the unchanged value (self-neutralizing); the only gate seen is a
datafield-word-7 flag bit at `071016B`, consistent with the manual's "the maximum byte
pointer is not updated when DC is used". The sequential-write path maintains datafield `21B`
as bytes are written (the mass of `STD ,X 21` sites through `073312B`-`102775B`).

**So the answer to the brief's question is: no at open, yes by close.**

- Opening an existing file W does not touch the object entry. If the emulator process died
  at that instant, the file on disk would still hold its old byte count and pages.
- But the session max byte pointer is -1 from the open, and close writes it back. A program
  that opens W, writes nothing, and closes (or is aborted - SINTRAN closes a terminating
  program's files, though the abort path itself was not separately carved this session)
  leaves the file with byte count 0. The 2,316,049-byte `:DOM` at 0 bytes is exactly this
  semantics, whether the truncation lands at open (the old `"wb"` bug) or at close (real
  SINTRAN).

**What this means for nd500x/ndmonlib:**

- `"r+b"` at open is CORRECT - keep it. Real SINTRAN does not destroy data at open either.
- The missing piece is close-time semantics: for an access-0 file, track the highest byte
  written this session (starting from 0, not from the old size) and truncate the host file
  to that length when the guest CLOSES the file normally. Reads through an access-0 file
  number should see an empty file (RMAX = 0) even before close.
- Update the comment at `ACCESS_SEQ_WRITE` in
  `external/ndmonlib/src/support/mon_file_table.c`: the open question is now answered; cite
  this document. (That file is in the WSL tree - not editable from this session.)

---

## Traps: all five held, nothing new

Pointer pools, P-relative EA = instruction address + displacement, `JPL` = `0o134`,
`LDF`/`STF` = 3-word descriptors, banks both based at 0 - all confirmed again. One addition:
the domain printer's pools at `014230B`-`014254B` and `014453B`-`014470B`, and the segment
printer's pools at `015131B`-`015145B` and `015343B`-`015354B`, are also printed as
instructions by the shipped `.asm` (same literal-pool trap).

## Files touched this session

- `SINTRAN/File-Formats/DESCRIPTION-FILE-FORMAT.md` - domain entry now code-proven; 74-84
  adjudicated; new pinned fields at 130/132.
- `SINTRAN/File-Formats/desc-format.json` - same, JSON validated.
- `SINTRAN/File-Formats/LINK-FILE-FORMAT.md` - format DECODED (new section 4a); open
  questions 1 and 2 closed, 3 partly.
- `SINTRAN/File-Formats/link-format.json` - NEW, machine-readable :LINK layout (validated).
- `SINTRAN/ND500/nll-re/` - NLL PSEG and UTIL committed with a README covering extraction,
  the verified `nd500-dis` regeneration command, and the key PSEG addresses; the DSEG and the
  disassembly listing are deliberately not committed.
- Ghidra `nd-500-mon-j04.prog` - pre-comments at every Q1/Q2 address named above.
- **Still to do in the WSL tree** (unreachable from Windows): `pcc-nd500`'s `desc.h`
  (domain-entry fields + 74-84 correction), `ndmonlib` `mon_file_table.c` comment (Q4).
