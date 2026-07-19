# Object (file) entry - 64 bytes (Phase 2)

Every file version on a directory device has one **object entry**: a 64-byte
record in the *object file* holding the file name, type, access rights,
file-type flags, size, dates, and the block pointer to the file's data. The
object file is an **indexed** file: the `object_file_ptr` in the
[master block](directory-label.md) points at an index block, whose contiguous
block-pointers each name a data page holding **32 object entries** (2048 / 64).

Sources: real disk `~/repos/nd100x/SMD0.IMG` (PACK-ONE); NDFS
`ndfs-c/src/object_entry.c` + `include/ndfs/object_entry.h` (the reader that
round-trips these bytes); `ndtool --stat` (independent cross-reader); carved
`006-S3FS` `ROBJE`/`FOBJB`/`RINDX` (producing/consuming code). On-disk multi-byte
values are **big-endian words**.

---

## 1. Locating a real object entry

`object_file_ptr = 0x400048FC` -> INDEXED, block **44374B (18684)**. The index
block at page 18684 begins:

```
0247e000: 0000 48fd 0000 2e3a ...   (block 44374B = 18684, byte 0)
```

First index entry `0000 48FD` = CONTIGUOUS block **44375B (18685)** - the first
object **data page**. (Further index entries `0000 2E3A` = 27072B/11834 and
more list the object file's other data pages; the object file on PACK-ONE spans
several pages totalling 188 files.) Data page 18685 holds 32 in-use entries;
its first three (192 bytes):

```
0247e800: 8000 5349 4e54 5241 4e27 0000 0000 0000   <- entry 0: SINTRAN
0247e810: 0000 4441 5441 0000 0000 0007 0020 0000
0247e820: 0000 0000 0000 0000 b8af 2bf6 0000 0000
0247e830: 0000 0000 0000 003f ffff ffff 0000 0001
0247e840: 8000 4d41 434d 2d41 5245 4127 0000 0000   <- entry 1: MACM-AREA
0247e850: 0000 4441 5441 0001 0001 0007 0020 0000
0247e860: 0000 0001 0000 0000 b8af 2bf7 0000 0000
0247e870: 0000 0000 0000 0040 ffff ffff 0000 0040
0247e880: 9000 5345 4746 494c 3027 0000 0000 0000   <- entry 2: SEGFIL0
0247e890: 0000 4441 5441 0002 0002 00e7 0020 0000
0247e8a0: 0000 0002 0000 0045 b8af 2c0b 9b20 9940
0247e8b0: 9b20 9940 0000 2710 010a 4fff 0000 0080
```

---

## 2. Field layout (offsets within the 64-byte entry)

| Off | Field | Size | Verdict | Notes |
|-----|-------|------|---------|-------|
| 0 | Header byte | 1 | **VERIFIED** | bit 7 (`0x80`) = **in use**. Other bits of byte 0/1: see 4.1 |
| 1 | Header low byte | 1 | INFERRED | part of the 16-bit header word |
| 2-17 | Object name | 16 | **VERIFIED** | ASCII, `0x27`-terminated |
| 18-21 | File type text | 4 | **VERIFIED** | e.g. `DATA`; `0x27`-terminated if < 4 |
| 22-23 | `next_version` | 2 | INFERRED | version-chain word |
| 24-25 | `prev_version` | 2 | INFERRED | version-chain word |
| 26-27 | Access bits | 2 | **VERIFIED** | 3 x 5-bit tiers, see 4.2 |
| 28-29 | File-type flags | 2 | **VERIFIED** | `L M A C I B P T`, see 4.3 |
| 30-31 | Device number | 2 | INFERRED | 0 for ordinary disk files |
| 32 | File-type code | 1 | **VERIFIED** | 0 DATA, 1 PROG, 2 SYMB, 3 TEXT |
| 33 | reserved | 1 | OPEN | low byte of the object-index word |
| 34 | User index (owner) | 1 | **VERIFIED** | index into the [user file](user-entry.md) |
| 35 | (object slot low byte) | 1 | INFERRED | word 34-35 = `[user | file-slot]` |
| 36-37 | `current_open_count` | 2 | INFERRED | |
| 38-39 | `total_open_count` | 2 | INFERRED | |
| 40-43 | Date created | 4 | **VERIFIED** | ND timestamp |
| 44-47 | Last read date | 4 | **VERIFIED** | ND timestamp (0 = never) |
| 48-51 | Last write date | 4 | **VERIFIED** | ND timestamp (0 = never) |
| 52-55 | Pages in file | 4 | **VERIFIED** | 32-bit page count |
| 56-59 | Bytes in file - 1 | 4 | **VERIFIED** | actual = stored + 1 |
| 60-63 | File pointer | 4 | **VERIFIED** | block pointer (see [directory-label 3.2](directory-label.md#32-block-pointer-encoding-the-2-bit-type--30-bit-page-id)) |

Reader offsets: `ndfs_oe_from_bytes()` (`object_entry.c` lines 43-84) reads name
at +2, type at +18, `next_version`/`prev_version`/`access_bits`/`file_type_flags`/
`device_number` at +22/+24/+26/+28/+30, file-type code at +32, user index at +34,
open counts at +36/+38, three dates at +40/+44/+48, pages at +52, bytes-1 at +56,
file pointer at +60. Each offset below decodes correctly on the real bytes.

---

## 3. Worked decode - entry 0 (`SINTRAN:DATA`)

| Off | Bytes | Field | Value |
|-----|-------|-------|-------|
| 0-1 | `80 00` | Header | in-use |
| 2-17 | `53 49 4E 54 52 41 4E 27 00...` | Name | `SINTRAN` |
| 18-21 | `44 41 54 41` | Type | `DATA` |
| 22-25 | `00 00 00 00` | next/prev version | 0 / 0 |
| 26-27 | `00 07` | Access | 0x0007 -> OWN=RWA, FRIEND=none, PUBLIC=none |
| 28-29 | `00 20` | Flags | 0x0020 -> **A** (allocated) |
| 30-31 | `00 00` | Device | 0 |
| 32 | `00` | Type code | 0 = DATA |
| 34-35 | `00 00` | User idx / slot | owner index 0 (SYSTEM) |
| 40-43 | `B8 AF 2B F6` | Date created | 1996-02-23 18:47:54 (per `ndtool`) |
| 44-51 | `00...` | Last read / write | never |
| 52-55 | `00 00 00 3F` | Pages | 63 |
| 56-59 | `FF FF FF FF` | Bytes - 1 | wraps to 0 (byte count unknown; `ndtool` shows 0) |
| 60-63 | `00 00 00 01` | File pointer | CONTIGUOUS, block 1 -> 63 pages at pages 1..63 |

`ndtool --stat SYSTEM/SINTRAN:DATA` independently reports: Type DATA, Contiguous,
FileTypeAsText **A**, 63 pages, FilePointer BlockID 1, OWN=READ,WRITE,APPEND,
FRIEND=NONE, PUBLIC=NONE, DateCreated 1996-02-23 18:47:54. Every value matches the
byte decode. **VERIFIED.**

Two more real examples confirm the file-pointer / size fields:
- **MACM-AREA:DATA** (entry 1): pages `00 00 00 40` = 64, pointer `00 00 00 40` =
  CONTIGUOUS block 64 -> occupies pages 64..127 (directly after SINTRAN).
- **SEGFIL0:DATA** (entry 2): pages `00 00 27 10` = 10000, bytes-1 `01 0A 4F FF`
  -> 17453056 bytes, pointer `00 00 00 80` = CONTIGUOUS block 128. `ndtool` shows
  `17453056 bytes 10000 pages`. **VERIFIED.**

---

## 4. Field semantics

### 4.1 Header word (bytes 0-1)

Bit 7 of byte 0 (`0x80`) = **in use** - the only header bit NDFS models
(`NDFS_OBJECT_IN_USE`, `object_entry.c` line 33). **VERIFIED.**
On the real disk most entries have byte 0 = `0x80`, but **SEGFIL0** has byte 0 =
`0x90` (`0x80` + `0x10`, i.e. bit 4). The meaning of the extra byte-0 flag bits
(and byte 1) is **OPEN** - likely write-open / modified / system-file markers;
resolve against `006-S3FS` `COBJE`/`WOBJE` (change/write object entry). NDFS
preserves bytes it does not model via a verbatim `raw[64]` copy so they survive a
round trip (`object_entry.c` lines 38-41, 100-114).

### 4.2 Access bits (bytes 26-27) - three 5-bit tiers

The 16-bit access word packs three permission tiers, each 5 bits:

```
 bit 15 14 | 13 12 11 10 | 9 8 7 6 5 | 4 3 2 1 0
   (unused) [   PUBLIC   ] [ FRIEND ] [   OWN    ]
```

| Tier | Bits | Shift |
|------|------|-------|
| OWN | 0-4 | 0 |
| FRIEND | 5-9 | 5 |
| PUBLIC | 10-14 | 10 |

Within a tier (5 bits), the letter mapping (NDFS `object_entry.h` lines 36-55):

| Letter | Bit | Meaning |
|--------|-----|---------|
| R | 0 (`0x01`) | Read |
| W | 1 (`0x02`) | Write |
| A | 2 (`0x04`) | Append |
| C | 3 (`0x08`) | Common (execute) |
| D | 4 (`0x10`) | Directory (delete) |

Worked: SINTRAN access `0x0007` -> OWN = `0x07` = R+W+A, FRIEND = 0, PUBLIC = 0.
SEGFIL0 access `0x00E7` -> OWN = `0x07` (RWA), FRIEND = `0x07` (RWA), PUBLIC = 0.
`ndtool --stat` decodes these identically. **VERIFIED** (values + tier split).
The exact R/W/A/C/D *letter-to-bit* assignment is **INFERRED** from NDFS/`ndtool`
(a reader that round-trips real access words); the official manual confirms the
tier order **public, friend, owner** and the letter set **RWACD**
(`ND-60.128.5` `SET-FILE-ACCESS`, `SET-DEFAULT-FILE-ACCESS`), but does not pin
each bit position.

### 4.3 File-type flags (bytes 28-29) - `L M A C I B P T`

`file_type_flags` bit field (NDFS `object_entry.h` lines 35-43):

| Letter | Bit | Mask | Meaning |
|--------|-----|------|---------|
| T | 0 | `0x0001` | Terminal device file |
| P | 1 | `0x0002` | Peripheral device file |
| B | 2 | `0x0004` | (B) Spooling file |
| I | 3 | `0x0008` | Indexed file |
| C | 4 | `0x0010` | Contiguous file |
| A | 5 | `0x0020` | Allocated file |
| M | 6 | `0x0040` | Magnetic-tape file |
| L | 7 | `0x0080` | Library file |

(The mnemonic order `L M A C I B P T` is high-bit-first.) On PACK-ONE, SINTRAN /
MACM-AREA / SEGFIL0 all have flags `0x0020` = **A** (allocated); `ndtool` prints
`FileTypeAsText : A` and `FileType : Contiguous` for them. **VERIFIED** (the A bit
against real bytes + `ndtool`). The remaining bit meanings are **INFERRED** from
NDFS names, corroborated by the manual's file-class vocabulary (indexed /
contiguous / spooling / peripheral). A TERMINAL device object has type bytes
`27 00 00 00` (empty type) - see `object_entry.c` line 52.

### 4.4 File-type code (byte 32)

Single byte: 0 = DATA, 1 = PROG, 2 = SYMB, 3 = TEXT (`object_entry.h` line 70).
Distinct from the 4-char type *text* at bytes 18-21. **VERIFIED** (byte 32 = 0 and
type text `DATA` agree on all three sample entries; `ndtool` prints `Type: DATA`).

### 4.5 Owner (byte 34) and object-index word

Byte 34 is the **user index** of the owning account (0 = SYSTEM on PACK-ONE),
resolved to a name via the [user file](user-entry.md). NDFS reads the 16-bit word
at 34-35 as `disk_object_index = [user | file-slot]` (`object_entry.c` lines
65-67); the low byte (35) is the file slot. Byte 33 (low byte of the word at
32-33) is preserved but unmodelled - **OPEN**.

### 4.6 Size fields (bytes 52-59)

- **Pages in file** (52-55): 32-bit page count.
- **Bytes in file - 1** (56-59): the *actual* byte length is stored **minus one**;
  the reader adds 1 (`object_entry.c` lines 79-81). `0xFFFFFFFF` therefore means
  "byte count not tracked" (wraps to 0), as on the contiguous system files
  SINTRAN / MACM-AREA. **VERIFIED.**

### 4.7 File pointer (bytes 60-63)

Same 2-bit-type + 30-bit-block-id encoding as the master-block pointers
([directory-label 3.2](directory-label.md#32-block-pointer-encoding-the-2-bit-type--30-bit-page-id)).
For a **contiguous** file it is the first data page (SINTRAN -> block 1). For an
**indexed** file it is the index block, walked by `006-S3FS` `RINDX`/`FINDX`.
**VERIFIED** for the contiguous case from real bytes; the indexed-walk is the
subject of Phase 6.

---

## 5. Producing / consuming code (`006-S3FS`)

| Addr (octal) | Symbol | Role |
|--------------|--------|------|
| 55563B / 55566B / 55750B | `FOBJB` / `ROBJE` / `WOBJE` | Find / read (MON 41) / write object block |
| 61502B | `COBJE` | Change object entry |
| 63726B / 64146B | `CROBJ` / `DLOBJ` | Create / delete object |
| 51453B / 52066B | `RINDX` / `FINDX` | Read / walk index block (indexed object file) |

The carved `41B-ReadObjectEntry` (`ROBJE`) worker reads an object entry via
`FOBJB` -> `RINDX` -> `RBLOC`, confirming the object file is an indexed file whose
64-byte records are read one 64-byte slot at a time. See
`.../re/mon-analysis/41B-ReadObjectEntry/`.

---

## 6. Fields marked OPEN

- Byte 0 extra flag bits (SEGFIL0 = `0x90`) and byte 1 - header bits beyond
  in-use. Resolve via `COBJE`/`WOBJE`.
- Byte 33 (low byte of the 32-33 word) - preserved-but-unmodelled.
- Bytes 22-25 `next_version`/`prev_version` exact chaining semantics vs the
  34-35 object-index word (INFERRED from NDFS; not byte-traced to the writer).

**Provenance:** real bytes `SMD0.IMG`; reader `object_entry.c`/`.h`; cross-reader
`ndtool --stat`; producer `006-S3FS` `ROBJE`/`COBJE`/`WOBJE`.
</content>
