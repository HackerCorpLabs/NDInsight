# Object entry bytes 32-35, and the OWN access tier - settled from ND manuals

**Date:** 2026-07-30
**Scope:** read-only research. No source file was modified.
**Status of the answer:** all four questions are answered, three of them **VERIFIED**
against two independent official ND manuals plus a real annotated `@DUMP-OBJECT-ENTRY`
listing printed in one of them.

This document does **not** edit
[`object-entry.md`](object-entry.md). It contradicts that file in two places and
those contradictions are called out in section 6 for a human to reconcile.

---

## 1. The question

`object-entry.md` records the 64-byte object entry with these four bytes unsettled:

| byte | current claim there | verdict there |
|------|---------------------|---------------|
| 32 | file-type code (0 DATA, 1 PROG, 2 SYMB, 3 TEXT) | VERIFIED |
| 33 | unexplained | OPEN |
| 34 | owning user index | VERIFIED |
| 35 | file slot low byte, word 34-35 = `[user | slot]` | INFERRED |

Asked: (1) what does SINTRAN write into 34-35, (2) what is byte 33, (3) is there a
per-file reserving user, (4) is the OWN access tier 3 bits or 5 bits.

---

## 2. What was searched

- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\` - the 45 NPL files and the 3 symbol
  sets. Searched for `OBJENT`, `OBJE`, `OBJIN`, `OBIND`, `USIND`, `FILNO`, file index.
  **Result: no object-entry field layout.** The FILSYS symbol tables list the
  routines (`CROBJ=063726`, `DLOBJ=064146`, `WOBJE=055750`, `COBJE=061502`,
  `ROBJE=055566`) but carry no field-offset symbols. The NPL tree is monitor part 2
  and does not contain the file-system module source.
  See `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\FILSYS-SYMBOLS.SYMB.TXT`
  lines 391, 413-416, 957, 1175, 1876, 2461, 2614.
- `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\006-S3FS\006-S3FS.asm`
  - read the `CROBJ` body at line 15653 (address `063726B`) and the `DLOBJ` body at
  line 15774 (address `064146B`). Both are PLANC output using B-relative local frames
  and indirect calls through a tail pointer table, so the field stores are not
  directly legible without resolving the pool words. **I did not extract field
  offsets from the disassembly** and I am not going to claim any. See section 5.
- `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\code-logic\s3fs-code-map.md` and
  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\mon-analysis\41B-ReadObjectEntry\README.md`
  - routine map and the MON 41 carve. Neither documents the field layout.
- `E:\Dev\Ronny\NDInsight\Developer\MON\calls\215B_GetObjectEntry.yaml` and
  `216B_SetObjectEntry.yaml` - both say "See appendix C". **That pointer is what
  broke the problem open.**
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`
  APPENDIX C: FILE SYSTEM ENTRIES.
- `E:\Dev\Ronny\NDInsight\Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md`
  appendix F.6 Object Entry, and the worked `@DUMP-OBJECT-ENTRY` example in the
  Preventive Maintenance chapter.

---

## 3. What was found

### 3.1 Appendix C of the Monitor Calls manual - a byte-indexed layout table

`E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`
line 28839 opens `# APPENDIX C: FILE SYSTEM ENTRIES`. The object-entry table starts at
line 28896. This table is indexed in **BYTES**, not words - it says so in its column
header (`| BYTE | OBJECT INFORMATION |`) and its rows run 0:1 through 60:63, i.e. 64
bytes. Verbatim, the rows that matter:

```
| 0:1   | Various, bit 15: set if object entry in use.
          bit 14: set if currently opened for write.
          bit 13: set if file is reserved.
          bit 12: set if the file is modified.
          bit 10-0: terminal number of last user opening the file. 0 if opened by RT program. |
| 26:27 | File access, bit 14-9: public access
          bit 9-4: friend access.
          bit 4-0: own access. |
| 28:29 | Attributes, bit 15-12: object block number
          bit 8-0: logical file type
          bit 8: set if temporary file.  bit 7: set if library file.
          bit 8: set if magnetic-tape file. [OCR error, see 3.4]
          bit 5: set if allocated file.  bit 4: set if contiguous file.
          bit 3: set if indexed file.    bit 2: set if spooling file.
          bit 1: set if peripheral file. bit 0: set if terminal file. |
| 30:31 | Device number. |
| 32:33 | User index in main directory of reserving user. |
| 34:35 | Object index of this object entry. |
| 36:37 | Current open count. |
| 38:39 | Total open count. |
```

There is **no file-type code byte anywhere in the entry.**

### 3.2 The System Supervisor manual, appendix F.6 - the same entry, word-indexed

`E:\Dev\Ronny\NDInsight\Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md`
line 16149 `# F.6 Object Entry`. Its word table (line 16157 onward) is **OCR-damaged**:
the word-number column mixes octal and decimal, and the values `22B` and `24B` each
appear twice. Do not use that column. Two things in F.6 are clean and decisive:

Line 16155:

> Each user area can have up to 4096 files, divided into object blocks of 256 files
> each. That means each directory can have 256 user areas * 4096 files = 1,048,576 files.

Line 16247, the note on the version-pointer words:

```
### THE 3 FILE VERSION POINTERS:
| Bit No. |        |
| 15 8 7 0 | (Dec.) |
| User Index | Object Index |

- The object index is relative to the object block.
```

Line 16241, the note on the preceding word:

```
### LAST RESERVING USER:
| Bit No. |        |
| 15 8 7 0 |        |
```

and line 16169 names that word `LAST RESERVING USER`.

### 3.3 A real annotated `@DUMP-OBJECT-ENTRY` listing

Same file, lines 13697-13725. This is the strongest single piece of evidence: an
actual octal dump of `USER 000 OBJECT ENTRY 025` with ND's own per-word labels.
Reproduced in full:

```
110001                 U/W/R/M TERM. NO OF RESERVING USER
044105 047124 026515 047504 042447 000000 000000 000000
                    HENT-MODE
051531 046502         TYPE (SYMB)
000025 000025         POINTERS TO NEXT AND PREVIOUS VERSION
002377                 ACCESS WORD
000040                 OBJBL (BITS 017-014)/TEMP/L/M/A/C/I/S/P/T
000000                 DEVICE NUMBER
000000                 MAIN DIR INDEX / USER INDEX OF RESERVING USER
000025                 OBJECT INDEX OF THIS ENTRY
000001 000004         CURRENT AND TOTAL OPEN COUNT
106561 001127         DATE CREATED (1985.5.24 16.09.23)
110714 162713         LAST DATE OPENED FOR READ (1986.1.30 14.23.11)
110714 162713         LAST DATE OPENED FOR WRITE (1986.1.30 14.23.11)
000000 000001         PAGES IN FILE
000000 003015         MAX. BYTE POINTER
040000 044765         FILE POINTER
```

Count the words: 1 header + 8 name + 2 type + 2 version pointers + 1 access +
1 attributes + 1 device + **1 reserving user + 1 object index** + 2 open counts +
6 dates + 2 pages + 2 max byte pointer + 2 file pointer = 32 words = 64 bytes. The
word order matches Appendix C's byte order exactly, position for position. The two
manuals are independent and agree.

### 3.4 The answers

**Q1 - bytes 34-35.** The word is the **object index of this object entry**, split
`[bit 15-8 = user index | bit 7-0 = object index relative to the object block]`.
**VERIFIED** - Appendix C line 28907 (`34:35 Object index of this object entry`) for
the field identity; System Supervisor line 16247 for the byte split; line 13707
for ND's own label on a real dump.

The existing `object-entry.md` reading `[user | file-slot]` is therefore **correct in
shape**, and the "INFERRED" tag on byte 35 can be upgraded, with one important
correction: byte 35 is the object index **within the 256-entry object block**, not a
global slot. The full file number that `@DUMP-OBJECT-ENTRY` prompts for
(`FILE NO. (0-07777)`, line 13699, i.e. 0 to 4095) is
`objectBlockNumber * 256 + byte35`, where `objectBlockNumber` is bits 15-12 of the
attributes word at bytes 28-29 (Appendix C line 28903, and the dump label
`OBJBL (BITS 017-014)` at line 13705 - octal bits 17-14 are decimal bits 15-12).

Cross-check against the real PACK-ONE bytes already quoted in `object-entry.md`
section 1: SINTRAN has bytes 34-35 = `0000`, MACM-AREA = `0001`, SEGFIL0 = `0002`.
They are objects 0, 1 and 2 of user SYSTEM (user index 0, object block 0). The
version-pointer words at 22-25 hold the same values (`0000`/`0000`, `0001`/`0001`,
`0002`/`0002`), which is exactly the self-linked single-version chain the same
manual describes at line 13799 ("They should be set to the same value as this
version pointer"). Consistent. **VERIFIED.**

**Q2 - byte 33.** It is the **user index of the reserving user**, the low half of the
`LAST RESERVING USER` word at bytes 32-33. **VERIFIED** as a field
(Appendix C line 28906; System Supervisor lines 16169 and 13707).

**Byte 32 is the main directory index of that reserving user.** The high/low split of
the word is **VERIFIED** (F.6 line 16241 shows the word divided at `15 8 7 0`; the
dump label at line 13707 is the two-part `MAIN DIR INDEX / USER INDEX OF RESERVING
USER`). Which half holds which is **INFERRED but strongly supported**: the same
Monitor Calls manual packs a directory/user index pair the same way for MON 215B and
216B - `LDT INDEX %Left byte: Dir index. Right byte: User index.`
(`E:\Dev\Ronny\NDInsight\Developer\MON\calls\215B_GetObjectEntry.yaml`, the `mac`
example). Left byte is the high byte. So byte 32 = directory index, byte 33 = user
index.

**Byte 32 is NOT a file-type code.** Neither manual has a file-type code byte
anywhere in the entry, and the annotated dump accounts for all 32 words with none
left over. See section 6.

**Q3 - is a reserving user stored on disk?** **Yes. Refuted.** The previous search
concluded there is no per-file reserving user in the entry. There is: the word at
bytes 32-33, plus a matching flag - Appendix C, byte 0 bit 13 "set if file is
reserved" - plus F.6's own key at line 16197 (`R = Reserved peripheral file`). The
earlier reasoning (RESERVE-FILE applies to peripheral files and is dropped at
logout) is not wrong about the semantics; it is wrong that it leaves no disk trace.
Note the field is the *last* reserving user (F.6's name for it), so it is a stale
record, not a live lock. **VERIFIED as a field. Its exact lifecycle is UNKNOWN** -
nothing found says when SINTRAN clears it.

**Q4 - OWN access tier width: 5 bits.** **VERIFIED, arithmetically.**
The dump at line 13703 gives a real access word for a `:SYMB` file:
`002377` octal = 1279 decimal = `0 0100 1111 1111` binary.

| tier | bits | value | meaning |
|------|------|-------|---------|
| OWN | 4-0 | `11111` = 0x1F | R W A C D, all five |
| FRIEND | 9-5 | `00111` = 0x07 | R W A |
| PUBLIC | 14-10 | `00001` = 0x01 | R |

That is a coherent permission set for a source file. Under a 3-bit OWN tier the
same word decodes to nothing sensible. The 5-bit tier width is independently
corroborated by the **user entry** friend table, which the same Appendix C
(line 28866) spells out bit by bit: `bit 12 directory, bit 11 common, bit 10 append,
bit 9 write, bit 8 read` - five bits, and relative to that field's base the order is
`0=R, 1=W, 2=A, 3=C, 4=D`, exactly the R/W/A/C/D bit assignment `object-entry.md`
section 4.2 currently marks INFERRED. **That INFERRED tag can be upgraded to
VERIFIED.**

Appendix C's own text for the access word ("bit 14-9 public, bit 9-4 friend, bit 4-0
own") is internally inconsistent - it overlaps at bits 9 and 4 and gives two 6-bit
fields. The tier boundaries 14-10 / 9-5 / 4-0 are what the arithmetic above
requires. Treat the manual's bit numbers here as OCR or typesetting damage.

So: `AccessBits & 0x1F` is **right**, and the adjacent "bits 2-0" comment is **wrong**.

### 3.5 Two bonus findings, both of which close OPEN items in `object-entry.md`

**Byte 0 flag bits (section 4.1 of the old doc, marked OPEN).** Appendix C line 28899
gives them: bit 15 in use, bit 14 open for write, bit 13 file reserved, bit 12 file
modified, bits 10-0 terminal number of the last user to open the file (0 if opened by
an RT program). This explains SEGFIL0's header `0x90` = bit 15 + bit 12 = in use +
modified. The dump listing confirms it live: header `110001` octal = `0x9001` -
identical high byte, plus terminal number 1 in the low bits. The other sample header
in the same manual, `150001` (line 13670) = `0xD001` = in use + open for write +
modified, terminal 1. **VERIFIED.**

**Bytes 28-29 (attributes).** `object-entry.md` section 4.3 models only bits 7-0.
Two fields are missing from every implementation: **bits 15-12 = object block
number** and **bit 8 = temporary file**. Both manuals agree
(Appendix C line 28903; dump label `OBJBL (BITS 017-014)/TEMP/L/M/A/C/I/S/P/T` at
line 13705). The dump's letter order maps bit 8 down to bit 0 as
TEMP, L(ibrary), M(agnetic tape), A(llocated), C(ontiguous), I(ndexed), S(pooling),
P(eripheral), T(erminal) - which matches the NDFS bit assignments for bits 7-0, and
shows NDFS's letter "B" for bit 2 should be **S** for spooling. Appendix C's line
"bit 8: set if magnetic-tape file" repeats bit 8 and is an OCR error for bit 6; the
dump's positional letter list is the reliable one. Sample value `000040` octal = bit 5
= Allocated, matching the three PACK-ONE entries. **VERIFIED.**

**Bytes 56-59.** The dump labels this word pair `MAX. BYTE POINTER`, not "bytes in
file". That is the same thing as NDFS's stored-value-plus-one convention and does not
change any decode, but it is the field's real name.

---

## 4. What remains unknown

- **When SINTRAN clears the reserving-user word at bytes 32-33.** Not found.
- **Whether byte 32 or byte 33 holds the directory index.** INFERRED from the MON
  215B/216B `INDEX` register convention, not read from file-system code or a dump
  with a non-zero value. Every sample available (PACK-ONE entries, the manual's dump)
  has the whole word zero.
- **The actual store instructions in `CROBJ` / `COBJE` / `WOBJE`.** Not traced. The
  question "what does SINTRAN itself WRITE" is answered here from ND's own format
  documentation and a real dump, not from the code. That is a different class of
  evidence and I am flagging it rather than dressing it up.
- **Bytes 22-25 version-chain semantics** remain as `object-entry.md` has them, now
  with the added fact that those two words use the same `[user index | object index]`
  packing (F.6 line 16247 calls them, together with bytes 34-35, "THE 3 FILE VERSION
  POINTERS"), and that all versions of a file live in the same object block
  (F.6 line 16294).

---

## 5. A note on the disassembly

I opened `006-S3FS.asm` at `CROBJ` (`063726B`, line 15653) and `DLOBJ` (`064146B`,
line 15774) and did not get field offsets out of them. Both are PLANC compiler output:
the entry sequence is `STD I n` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB k`, after
which nearly every memory reference is B-relative into a local frame and every call is
`JPL I` through a pointer table sitting past the code body. `DLOBJ`'s X-relative loads
(`LDA ,X 26`, `LDD ,X 64`, `LDD ,X 60`, `LDA ,X 41`, `LDA ,X 42`, `LDA ,X 50`) reach
displacements above 37 octal, so **X there is not pointing at a 32-word object entry**
and those offsets must not be read as object-entry fields. Resolving them needs the
pool words traced, which I did not do. Recorded here so nobody re-derives a wrong
field table from those loads.

---

## 6. Are any of the four implementations wrong?

Yes. All four share one wrong field, and the C# has a second one.

### 6.1 Byte 32 read as a file-type code - WRONG in all four

| file | line | code |
|------|------|------|
| `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\src\object_entry.c` | 65 | `out->file_type = data[offset + 32];` |
| `E:\Dev\Ronny\norskdata-ndfs\ndfs-py\src\ndfs\object_entry.py` | 180 | `entry.file_type = data[offset + 32]` |
| `E:\Dev\Ronny\norskdata-ndfs\ndfs-ts\src\object-entry.ts` | 146 | `entry.fileType = data[offset + 32];` |
| `E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Elements\ObjectEntry.cs` | 271 | `entry.FileType = data[offset + 32];` |

Byte 32 is the main directory index of the reserving user. The 0=DATA / 1=PROG /
2=SYMB / 3=TEXT mapping (`ObjectEntry.cs` lines 167-176, and the equivalents) is not
a SINTRAN field. It reads correct on ordinary files only because an unreserved file
has zero there and zero maps to "DATA". The real file type is the 4-character text at
bytes 18-21, which all four already read correctly.

Practical impact: any file that has ever been reserved by a user on a non-zero
directory would be reported with a bogus type code. On the write path the C# is worse
than the readers: `ObjectEntry.cs` line 412 writes `buffer[offset + 32] = (byte)FileType;`
unconditionally, so a round trip through the C# **destroys** the reserving-user
directory index. The C, Python and TypeScript ports base their output on the
preserved raw bytes, which limits the damage to entries built from scratch.

### 6.2 `UserIndexOfReservingUser` reading byte 34 - WRONG in the C#

`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Elements\ObjectEntry.cs` line 299:

```csharp
entry.UserIndexOfReservingUser = data[offset + 34];
```

The property name is right and the offset is wrong; it should be **byte 33**. The
comment block above it (lines 286-298) states that no reserving-user field exists in
the entry, that byte 33 is the one remaining open byte, and that this property is an
alias for `UserIndex`. All three statements are refuted by Appendix C. Byte 33 *is*
the reserving user. The property was correctly named and then wired to the wrong byte.

### 6.3 What the four get right

- `disk_object_index` / `diskObjectIndex` read as the big-endian word at 34-35
  (`object_entry.c` line 67, `object-entry.ts` line 150) is **correct**.
- Reading byte 34 as the owning user index is **correct** - it is the high half of
  that word.
- Reading byte 35 as the file slot is **correct in shape**, but it is the object index
  *within the object block*, so the full file number needs bits 15-12 of the word at
  bytes 28-29 added as `block * 256`. No implementation reads that field.
- `AccessBits & 0x1F` for the OWN tier is **correct** (see 3.4 Q4).
- No implementation models byte 0 bits 14/13/12, attributes bits 15-12 and bit 8, or
  the reserving-user word. These are omissions, not decode errors, and the raw-bytes
  preservation in the C/Python/TypeScript ports keeps them safe on a round trip.

---

## 7. Contradictions with the existing `object-entry.md`

Left unreconciled by design. Two rows of its section 2 table and the matching prose
in section 4.4 and 4.5 disagree with this document:

1. Byte 32 is marked **VERIFIED** as the file-type code. It is not a file-type code.
   The "verification" in section 4.4 - byte 32 = 0 and type text `DATA` agreeing on
   all three PACK-ONE samples - is a coincidence: all three samples are unreserved
   SYSTEM files on the main directory, so byte 32 is 0, and 0 happens to be the DATA
   code in the invented mapping. Three samples that are all zero cannot distinguish
   the two hypotheses.
2. Byte 33 is marked **OPEN**. It is the user index of the reserving user.

Section 4.5's reading of bytes 34-35 as `[user | file-slot]` is confirmed, and its
INFERRED tag on byte 35 and on the R/W/A/C/D letter-to-bit map in section 4.2 can both
be upgraded to VERIFIED, with the object-block qualification in 3.4 Q1 added.

---

## 8. Sources

| what | full path | locator |
|------|-----------|---------|
| Appendix C, object entry byte table | `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md` | appendix at line 28839, object table at line 28896, user entry at line 28866 |
| Appendix F.6, object entry | `E:\Dev\Ronny\NDInsight\Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md` | line 16149; version-pointer split line 16247; reserving-user line 16241 |
| Real annotated `@DUMP-OBJECT-ENTRY` | same file | lines 13697-13725 (second example), 13667-13671 (first example) |
| MON 215B/216B, "see appendix C", INDEX byte packing | `E:\Dev\Ronny\NDInsight\Developer\MON\calls\215B_GetObjectEntry.yaml`, `216B_SetObjectEntry.yaml` | `mac` example |
| FILSYS symbols | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\FILSYS-SYMBOLS.SYMB.TXT` | lines 391, 413-416, 957, 1175, 1876, 2461, 2614 |
| S3FS disassembly (opened, no field data extracted) | `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\006-S3FS\006-S3FS.asm` | `CROBJ` line 15653, `DLOBJ` line 15774 |
| Existing format notes | `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\on-disk-format\object-entry.md` | sections 2, 4.1-4.5 |
| Implementations | `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\src\object_entry.c` (65), `E:\Dev\Ronny\norskdata-ndfs\ndfs-py\src\ndfs\object_entry.py` (180), `E:\Dev\Ronny\norskdata-ndfs\ndfs-ts\src\object-entry.ts` (146), `E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Elements\ObjectEntry.cs` (271, 299, 412) | as noted |
