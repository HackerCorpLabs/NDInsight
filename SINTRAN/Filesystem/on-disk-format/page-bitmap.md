# Page / block allocation bitmap - the bit file (Phase 4)

The **bit file** is the volume's allocation bitmap: **one bit per page**,
`0 = free`, `1 = used`. It is a **contiguous** file; the `bit_file_ptr` in the
[master block](directory-label.md) gives its first page. To allocate a page the
filesystem finds a `0` bit and sets it; to free a page it clears the bit.

Sources: real disk `~/repos/nd100x/SMD0.IMG` (PACK-ONE); NDFS
`ndfs-c/src/bit_file.c` + `include/ndfs/bit_file.h` (the reader that reproduces
`ndtool`'s free/used counts); `ndtool -i` (independent cross-reader); carved
`006-S3FS` `GPAGE`/`ALPAG`/`RLPAG`/`RPAGE`/`WPAGE`/`TESTB` (producing/consuming
code).

---

## 1. On-disk location

`bit_file_ptr = 0x00004824` -> type **00 CONTIGUOUS**, block **44044B (18468)**.
The bitmap starts at page 18468 (byte offset `18468 * 2048 = 0x02412000`) and
spans `ceil(total_pages / 8)` bytes = `ceil(38400 / 8) = 4800` bytes -> under one
page. **VERIFIED** (pointer bytes + span computed from PACK-ONE's 38400 pages).

Raw start of the bitmap (`xxd -s $((18468*2048)) -l 64`):

```
02412000: ffff ffff ffff ffff ffff ffff ffff ffff
02412010: ffff ffff ffff ffff ffff ffff ffff ffff
02412020: ffff ffff ffff ffff ffff ffff ffff ffff
02412030: ffff ffff ffff ffff ffff ffff ffff ffff
```

All `0xFF` at the start = the low pages (the resident system image, object/user
files, and the bitmap's own pages) are fully allocated.

---

## 2. Byte / bit ordering (VERIFIED)

For page (block) `N`:

```
byte_index = N >> 3        (N / 8)
bit_index  = N & 7         (N mod 8)
used       = (bitmap[byte_index] >> bit_index) & 1
```

(`bit_file.c` lines 53-62.) So **page 0 = bit 0 of byte 0** (mask `0x01`), page 7
= bit 7 of byte 0 (mask `0x80`), page 8 = bit 0 of byte 1. Bits run
**LSB-first within each byte**, bytes in ascending page order. **VERIFIED.**

`byte0 = 0xFF` therefore means pages 0-7 are all used.

---

## 3. Reserved blocks 0-6 (VERIFIED in NDFS)

Blocks **0-6 are reserved** to the system and are never handed out; the first
allocatable block is **7** (`NDFS_FIRST_ALLOC_BLOCK = 7`, `types.h` line 84).
NDFS enforces this: `ndfs_bf_find_free()` scans from block 7
(`bit_file.c` line 116) and `ndfs_bf_allocate()` rejects any start below 7
(line 158). On the real disk byte 0 = `0xFF`, so blocks 0-6 are marked used,
consistent with the reservation. **VERIFIED (NDFS + real bytes).** That the same
0-6 reservation is enforced by `006-S3FS` `GPAGE`/`ALPAG` is **INFERRED** -
confirming it in the carved allocator is **OPEN-Q3** in the
[foundation README](../README.md#6-open-questions---what-each-later-phase-needs).

Page 0 itself holds the boot code + master block; the low pages also hold the
object file, user file, and the bitmap - `image_creator.c` marks page 0, the
object/user file index+data pages, and the bitmap pages as used when it builds a
fresh volume (`image_creator.c` lines 214-245).

---

## 4. Whole-bitmap verification against `ndtool`

Counting set bits across the full 4800-byte bitmap on PACK-ONE:

```
set bits (used pages) = 14277
free pages            = 38400 - 14277 = 24123
```

`ndtool -i SMD0.IMG` reports **Total 38400 / Used 14277 / Free 24123** - an exact
match. The bitmap is the authoritative free-space map (not the master block's
`unreserved_pages`, which reads 11428 on this disk). **VERIFIED.**

The first byte that is not `0xFF` is at byte index **1564** (value `0x03`),
i.e. the first partially-free region is around pages 12512-12519 - the low ~12500
pages are a solid allocated run (system files SINTRAN/MACM-AREA/SEGFIL0 occupy
pages 1..10127, then user files). **VERIFIED.**

---

## 5. Allocation semantics (NDFS model)

| Operation | NDFS routine | Behaviour |
|-----------|--------------|-----------|
| Test used | `ndfs_bf_is_used` | read bit for page N |
| Mark used | `ndfs_bf_mark_used` | set bit (allocate) |
| Mark free | `ndfs_bf_mark_free` | clear bit (release) |
| Find 1 free | `ndfs_bf_find_free` | first `0` from block 7 upward |
| Find N contiguous | `ndfs_bf_find_free_range` | first run of N free from block 0 upward |
| Allocate range | `ndfs_bf_allocate` | reject start < 7; fail if any already used |

Single-page and contiguous allocation search **upward** in NDFS. However the
`@CREATE-FILE` rules in the official manual place contiguous files in the
**highest** free page range - the opposite direction. Whether real SINTRAN
`GPAGE`/`ALPAG` searches up or down is **OPEN-Q3**; NDFS's upward search still
reads existing disks correctly because the bitmap is direction-agnostic on read.
**INFERRED** (allocation direction), **VERIFIED** (read/test semantics).

---

## 6. Producing / consuming code (`006-S3FS`)

| Addr (octal) | Symbol | Role |
|--------------|--------|------|
| 50627B | `ALPAG` | Allocate page (mark used in bit file) |
| 50632B / 50635B | `XRLPA` / `RLPAG` | Release page |
| 51025B | `TPAGF` | Test page free |
| 51120B | `RSPAG` | Reserve/set page |
| 51353B / 51355B | `TESTB` / `TESTP` | Test bit / test page |
| 76205B | `GPAGE` | Get (allocate) a page - core bitmap primitive |
| 101707B / 101711B | `RPAGE` / `WPAGE` | Read / write a bit-file page |
| 60147B / 60151B | `DLSPA` / `DLPAG` | Release / delete page(s) |

Prior analysis (`SINTRAN Structures/SINTRAN-STRUCTURES.md`) notes an in-core
bitmap-area constant `5BITM = 000010B`; its relationship to the on-disk bit file
is **OPEN**.

---

## 7. Summary

| Aspect | Value | Verdict |
|--------|-------|---------|
| Location | contiguous, first page from `bit_file_ptr` (18468 on PACK-ONE) | VERIFIED |
| Unit | 1 bit / page, `0`=free `1`=used | VERIFIED |
| Bit order | LSB-first per byte; page 0 = byte0 bit0 | VERIFIED |
| Span | `ceil(total_pages / 8)` bytes (4800 on PACK-ONE) | VERIFIED |
| Reserved | blocks 0-6, first alloc = 7 | VERIFIED (NDFS + bytes) / INFERRED in carved allocator |
| Alloc direction | upward (NDFS) vs highest-range (manual) | OPEN-Q3 |

**Provenance:** real bytes `SMD0.IMG` (bitmap popcount = `ndtool` used/free);
reader `bit_file.c`/`.h`; cross-reader `ndtool -i`; producer `006-S3FS`
`GPAGE`/`ALPAG`/`RLPAG`.
</content>
