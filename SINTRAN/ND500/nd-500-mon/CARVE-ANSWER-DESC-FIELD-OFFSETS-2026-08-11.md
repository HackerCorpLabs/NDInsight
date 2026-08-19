# Carve answer: DESCRIPTION-FILE:DESC segment-entry field offsets

**Answers:** `CARVE-BRIEF-DESC-FIELD-OFFSETS-2026-08-11.md` (same directory)
**Binary:** `nd-500-mon-j04.prog` (MON-DEBUG:PROG, ND-500/5000 Loader/Debug Monitor J04)
**Method:** walked the code in the shipped `nd-500-mon-j04.prog.asm`; every load-bearing word
(instruction words, pool words, string pointers, constants) re-read from the raw bytes through
the Ghidra import of the same file. Ghidra program `nd-500-mon-j04.prog` now carries pre-comments
at every address named below.
**Date:** 2026-08-11

---

## 1. The prediction is CONFIRMED - all five offsets, from the monitor's own code

The monitor has a routine that reads one raw 192-byte segment entry from the DESC file into a
fixed buffer at bank-1 word `037705B` and then prints the fields with their names. The name
strings sit in bank 2; each print block is: load label descriptor, print label, `LDX` the buffer
base from the literal pool word at `014716B` (verified bytes: `3F C5` = `037705B`), load the
field, print it. The pairing of label to load is unambiguous - each label print is immediately
followed by exactly one field load.

| Word offset | Bytes | Width | Label printed | Field load instruction | Verdict |
|---|---|---|---|---|---|
| `54B` | +88 | double | `$PLB:` (bank 2 `040340B`) | `014575B  026054  LDD ,X 54` | **CONFIRMED** |
| `56B` | +92 | double | `  Psize:` (`040346B`) | `014610B  026056  LDD ,X 56` | **CONFIRMED** |
| `60B` | +96 | double | `$DLB:` (`040362B`) | `014636B  026060  LDD ,X 60` | **CONFIRMED** |
| `62B` | +100 | double | `  Dsize:` (`040370B`) | `014651B  026062  LDD ,X 62` | **CONFIRMED** |
| `64B` | +104 | double | `$Debuginfo:` (`040404B`) | `014677B  026064  LDD ,X 64` | **CONFIRMED** |

The instruction word at `014575B` was verified against the raw bytes in Ghidra (`2C 2C` =
`026054B` = `LDD ,X 54`), as were the buffer pointer and the `$PLB:` string pointer
(`040340B` -> bank 2 byte `0x81C0` = `$PLB:`).

### New fields the same routine names, past the known ones

The print run continues past DEBUGINFO. Same buffer base `037705B` in every case:

| Word offset | Bytes | Width | Label | Field load |
|---|---|---|---|---|
| `66B` | +108 | double | `  Dlinkdate:` | `014712B  LDD ,X 66` |
| `70B` | +112 | single | `  Absfixad: ` | `014750B  LDA ,X 70` |
| `71B` | +114 | single | `$Lowlogfix:` | `014763B  LDA ,X 71` |
| `77B` | +126 | single | `Plologfix:` | `014623B  LDA ,X 77` |
| `100B` | +128 | single | `Puplogfix:` | `014664B  LDA ,X 100` |

(The odd print order - PLB, Psize, Plologfix, DLB, Dsize, Puplogfix, Debuginfo, Dlinkdate,
Absfixad, Lowlogfix - is because the output is three fields per line; `$` in the label is the
new-line marker.)

This matches the brief's empirical table exactly: `+104` is DEBUGINFO and it is a **double**,
and the "DEBUGINFO non-zero for real programs" observation now has a neighbour explanation -
`+108` is a link **date**, which is also non-zero only for really linked programs.

### And one more, at the very start of the entry

| Word offset | Bytes | Width | Meaning | Evidence |
|---|---|---|---|---|
| `0` | +0 | double | **byte position of the NEXT segment entry in the DESC file; 0 = end of chain** | `016436B  LDD ,X 0` off `037705B`, value passed straight to the record reader as the seek position; loop at `016422B`-`016445B` |

Segment entries are a linked list, not an indexed array. The head of the list is word 0 of the
owning domain entry (`016412B  LDD ,X 0` off the domain buffer `037651B`, same treatment).

**Verified against the real file bytes (2026-08-11, this session).** Walked the structure with
a script over `E:\Dev\Ronny\ND500UC\Floppy\210319H02-XX-01D\description-file.desc` (the vendor
floppy's own copy) and the two copies under WSL `~/ND500USERS/`:

- Domain entries found exactly where `56*i + 256*(i div 32 + 1)` says: `SCRATCH-DOMAIN'` at
  byte 256, `LINKAGE-LOAD-H02` at byte 312.
- Domain word 0 -> `0x4000` and `0x40C0`; each is a real segment entry (names
  `(210319H02:FLOPPY-USER)SCRATCH-SEG-01'` / `...LINKAGE-LOAD-H02'`, and the +88/+92/+96/+100
  fields carry the known values: 0/4/0/1028 and 0/123988/75834/2109142).
- Both chains terminate with word 0 = 0, no cycles.
- **Silent control:** no other 192-byte slot in the segment pages carries an in-file pointer
  at word 0 - only the chain targets do. The links are structure, not coincidence.
- Witness status: all three on-disk copies (floppy original, `FLOPPY-USER`, `SYSTEM`) are
  byte-identical right now - the `SYSTEM` copy that held the LED-B03 entries (the second
  independent witness in `DESCRIPTION-FILE-FORMAT.md`) was overwritten with the H02 content
  on 2026-08-10. The LED numbers in that doc predate the overwrite; re-verifying them needs
  the LED install re-run under nd500x.
- The +100 value read here (`00 20 2E D6` = 2,109,142) confirms the transcription-error
  correction already recorded in `DESCRIPTION-FILE-FORMAT.md` sections 5-6:
  `DLB + stored + 1 = 75834 + 2109142 + 1 = 2184977` = the `.dseg` file size, exactly.

## 2. How the record is read - the full chain

The brief guessed the DESC read goes through the `176740B` RFILE helper. **It does not.** The
DESC path never uses RFILE at all:

1. **`013527B` READ-SEGMENT-ENTRY(D = byte position).** Builds a read descriptor
   `{buffer 037705B, last byte index 277B}` - constants at `013545B` (`000277B` = 191, bytes
   verified `00 BF`) and `013546B` (`037705B`) - then calls:
2. **`013406B` SEEK+READ.** Fetches the DESC file number from the global at `040124B`, calls
   the `176624B` wrapper = **MON 74 SETBT** (set byte pointer) with the position, then loops
   calling the `176262B` wrapper = **MON 1 INBT** (read one byte) and stores each byte into the
   buffer with `SBYT` (loop at `013430B`-`013444B`).
3. The file was opened by the descriptor-builder at `016200B` the brief located (the
   `DESCRIPTION-FILE` / `.DESC` strings), and the printer then indexes the buffer directly.

So the buffer holds the **raw file record byte-for-byte**, which is what makes the word-offset
to file-byte-offset mapping exact (word `54B` = byte 88, and so on).

Domain entries have their own reader, **`013454B` READ-DOMAIN-ENTRY(index)**, which computes

```
position = 56*index + 256*(index div 32 + 1)
```

(multiply constant `{0, 70B}` = 56 at `013520B`-`013521B`; divide by `40B` = 32 and `AAA 1`
at `013473B`-`013475B`; shift left 8 = *256 at `013476B`) and reads **56 bytes** (descriptor
last-index `67B` = 55) into `037651B`. That gives the DESC file layout:
**2048-byte pages, each = 256-byte header/bitmap + 32 domain entries of 56 bytes**, with the
first page's entries starting at byte 256. Segment entries live wherever the chain links point.
The exact routing of the divide (which half of the double goes through `RDIV`) is read from the
compiler idiom, not stepped through - but 56*32 + 256 = 2048 exactly, so the geometry is not
in doubt.

## 3. Sizes as size-1: what the code does and does not say

- The monitor **never adjusts** the size fields. The printer prints the stored values raw - no
  `AAA 1` anywhere on the path. So on a live system, `Psize:` in this listing displays the
  stored value, whatever it is. There is **no read-side +1 in this program**, and no other code
  in the image touches word `56B` or `62B` of the buffer (searched the whole disassembly - the
  five `LDD`s above are the only accesses past word `50B` besides the ones in this table).
- **But** the same inclusive-bound convention shows up twice in the reader itself: the 192-byte
  segment record is read with length word `277B` = **191** = 192-1, and the 56-byte domain
  entry with `67B` = **55** = 56-1. These are PLANC-style inclusive last indexes (`0:n-1`
  ranges), the exact convention the brief measured in PSIZE/DSIZE. So "stored value = last byte
  index, not byte count" is the house style of the program that owns this file - consistent
  with, though not a write-side proof of, the empirical size-1 finding.
- The **writer** of these fields is not in this program. MON-DEBUG only reads and displays.
  Write-side proof of the -1 must come from NLL (the linkage loader).

## 4. DLB and the LINKAGE-LOAD-H02 mismatch

**Resolved - but by the file bytes, not by this program's code.** MON-DEBUG only prints DLB
and Dsize side by side; it never combines them. The mismatch itself turned out to be a
transcription error in the empirical table: the stored +100 value is 2,109,142 (raw
`00 20 2E D6`), not 2,109,654, and then `DLB + stored + 1 = .dseg file size` holds exactly
for all four entries. Recorded in `DESCRIPTION-FILE-FORMAT.md` sections 5-6 and re-verified
byte-for-byte in this session (see the chain-verification notes in section 1 above).

## Also observed in the same printer (lower offsets, for the format doc)

All off the segment-entry buffer `037705B`:

- word `36B` (byte 60): a **flags word**; individual bits are printed via `SHA n` + `SHA ZIN
  SHR 17` bit tests at `015743B`-`016052B` (bits labelled by more bank-2 strings around byte
  `0x8110`-`0x81C0`, not decoded here).
- word `37B` (byte 62): a character **count** used for two byte-string prints: one string
  starting at word `45B` (byte 74), one at word `50B` (byte 80) (`LBYT` loops at
  `015256B`-`015301B` and `015311B`-`015335B`).
- The domain-entry walker loops indexes 0..253 (limit `000376B` = 254 at `016504B`).
- Domain-entry fields printed from `037651B` include doubles at words `0, 17B, 21B, 23B, 25B,
  30B, 32B` with labels `$Domain  : `, `  Start address:`, `$Owner:`, `  Childindex:` in the
  bank-2 string run at bytes `0x80C6`-`0x8140` - label-to-offset pairing for these was not
  worked through.

## Traps confirmed (nothing new hit)

The brief's four traps all held. One addition: the shipped `.asm` prints pool words as
instructions in the `014714B`-`014735B`, `013544B`-`013551B` and `016462B`-`016506B` regions
too - same literal-pool trap as `016264B`-`016277B`.
