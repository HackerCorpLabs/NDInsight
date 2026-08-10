# Page / block allocation bitmap - the bit file (Phase 4)

The **bit file** is the volume's allocation bitmap: **one bit per page**,
`0 = free`, `1 = used`. It is a **contiguous** file; the `bit_file_ptr` in the
[master block](directory-label.md) gives its first page. To allocate a page the
filesystem finds a `0` bit and sets it; to free a page it clears the bit.

Sources: real disk `~/repos/nd100x/SMD0.IMG` (PACK-ONE); NDFS
`ndfs-c/src/bit_file.c` + `include/ndfs/bit_file.h` (the reader that reproduces
`ndtool`'s free/used counts — note this is NOT independent corroboration, see section 2);
carved
`006-S3FS` `GPAGE`/`ALPAG`/`RLPAG`/`RPAGE`/`WPAGE`/`TESTB` (producing/consuming
code).

---

## 1. On-disk location

`bit_file_ptr = 0x00004824` -> type **00 CONTIGUOUS**, block **44044B (18468)**.
The bitmap starts at page 18468 (byte offset `18468 * 2048 = 0x02412000`) and
spans `ceil(total_pages / 8)` bytes, rounded up to a whole 16-bit word,
= `ceil(38400 / 8) = 4800` bytes -> under one page. (38400 is a multiple of 16, so
the rounding is invisible here; on a 616-page floppy it is the difference between
77 and 78 bytes, and without it pages 608-615 are unreachable.) **VERIFIED** (pointer bytes + span computed from PACK-ONE's 38400 pages).

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

## 2. Word / bit ordering (CORRECTED 2026-08-02)

For page (block) `N`, the bit file is an array of **16-bit WORDS**:

```
word_index = N >> 4        (N / 16)
bit        = N & 15        (bit 0 = LSB)
used       = (word[word_index] >> bit) & 1
```

On the big-endian byte array that means:

```
byte_index = (N >> 3) ^ 1
bit_index  = N & 7
```

So **page 0 is bit 0 of the LOW byte of word 0 — i.e. byte 1, not byte 0**. Page 8 is
bit 0 of the HIGH byte (byte 0). Pages 0-7 live in odd bytes, pages 8-15 in even bytes.

### Authority

SINTRAN III System Supervisor, **ND-30.003.007 EN, appendix F.2 "Bit-File"**, states the
formula verbatim:

```
PAGE = BLOCK*400B + WORD*20B + BIT
```

`400B` = 256 pages per bit-file block, **`20B` = 16 pages per WORD**. Because the formula
ADDS the bit number, page `x` sits at **bit 0, the least significant bit**. The Norwegian
edition **ND-30.003.7 NO** carries the identical formula (`SIDE = BLOKK*400B + ORD*20B +
BIT`).

The manual also prints a worked example: a bit-file word holding `313B` with the
Free/Used state of each of the 16 pages it covers. `313B = 0b0000000011001011`, i.e.
pages `x+0, x+1, x+3, x+6, x+7` used. That vector is now a regression test in every port.

SINTRAN's own allocator agrees: `TPAGF` at 51043B forms the word index with
`SHA ZIN SHR 4` — **page / 16**, not page / 8. See
[`../code-logic/allocation.md`](../code-logic/allocation.md) section 1, which had this
right all along and honestly flagged the intra-word direction as OPEN; the manual closes
it as LSB-first.

### What this section used to say, and why it was wrong

This section previously asserted `byte_index = N >> 3, bit_index = N & 7` and marked it
**VERIFIED**. It was not verified — and the evidence cited could not have verified it:

- The cited check was a **popcount** match between `bit_file.c` and `ndtool -i`
  (14277 used / 24123 free). **Popcount is invariant under byte-swapping within a word.**
  It cannot distinguish the two conventions, even in principle.
- The other cited evidence, "the first 4800 bytes are all `0xFF`", is likewise
  byte-swap invariant.
- The remaining source was `ndfs-c/src/bit_file.c` — a modern re-implementation, not
  SINTRAN. Citing it here made the doc and the code confirm each other in a circle.

That false **VERIFIED** propagated into four independent implementations (`ndfs-c`,
`ndfs-py`, `ndfs-ts`, `RetroFS.NDFS`), all of which read and wrote the bitmap
byte-swapped. Because each was wrong in both its reader and its writer, every round-trip
test passed and the defect survived for months.

### How it was caught, and the invariant that catches it

A page that holds a real file's data cannot also be marked free. Measured across three
genuine ND media:

| Image | file data pages | reported FREE, byte convention | reported FREE, word convention |
|---|---|---|---|
| `BIGDISK0-L.IMG` (75 MB pack) | 14 543 | 32 | **0** |
| `210319H02-XX-01D.img` (floppy) | 201 | 4 | **0** |
| `Nd-210523I01-XX-01D.img` (floppy) | 481 | 3 | **0** |

The byte convention also fragments the pack implausibly — 39 used/free transitions
against 17 — producing 8-block-aligned holes that are artefacts of the swap rather than
real allocation.

**Consequence of the bug:** free-page search returned pages SINTRAN had already
allocated, so writing to a real pack would overwrite live file data. Reading was
unaffected, which is why `list`/`extract` always looked correct.

**Lesson for this document set:** a "VERIFIED" tag is only as good as whether the check
could have failed. Popcount against a byte-swap is a check that cannot fail. Prefer
invariants that are asymmetric — compared against something the codebase did not itself
produce.

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
match.

> **This is a COUNT check and nothing more.** It confirms the same number of bits is set,
> which is true under any permutation of them - it is invariant under the byte-swap that
> section 2 corrects, and `ndtool` is not an independent reader in any case. Useful for
> spotting a lost or duplicated page; useless for bit ORDER. The bitmap is the authoritative free-space map (not the master block's
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
| Bit order | **16-bit WORD addressed**: page N = bit `N%16` of word `N/16`, LSB-first. On the byte array: byte `(N>>3)^1`, bit `N&7` — so **page 0 is byte 1**, not byte 0. | VERIFIED (ND-30.003.007 F.2 + 3 real media — see section 2) |
| Span | `ceil(total_pages / 8)` bytes **rounded up to a whole 16-bit word** (4800 on PACK-ONE; 78 not 77 on a 616-page floppy) | VERIFIED |
| Reserved | blocks 0-6, first alloc = 7 | VERIFIED (NDFS + bytes) / INFERRED in carved allocator |
| Alloc direction | upward (NDFS) vs highest-range (manual) | OPEN-Q3 |

**Provenance:** ND-30.003.007 EN appendix F.2 (`PAGE = BLOCK*400B + WORD*20B + BIT`, where
`20B` = 16) and the identical formula in the Norwegian edition ND-30.003.7 NO; SINTRAN's own
allocator `TPAGF` 51043B (`SHA ZIN SHR 4` = page/16); and the file-data-versus-free invariant
measured on three genuine ND media (`BIGDISK0-L.IMG`, `210319H02-XX-01D.img`,
`Nd-210523I01-XX-01D.img`) — **0** contradictions under this reading, 32/4/3 under the
discredited byte reading. Producer `006-S3FS` `GPAGE`/`ALPAG`/`RLPAG`.

**NOT provenance, and why it must not be cited again:** this footer previously read
*"real bytes `SMD0.IMG` (bitmap popcount = `ndtool` used/free) … cross-reader `ndtool -i`"*.
Neither item is evidence for a bit ORDER.

- **Popcount is invariant under byte-swapping.** It returns the identical number whichever
  convention is correct, so it could never have failed — yet it was the basis for the
  VERIFIED tag that stood here for months.
- **`ndtool` is not an independent cross-reader.** It links the very library whose
  `bit_file.c` is cited as "the reader" (`ndfs-c/CMakeLists.txt`:
  `target_link_libraries(ndtool ndfs)`), so it cannot disagree with it. "`ndtool` agrees" is
  a restatement, not a check.

Evidence for an ordering claim must be something that changes when the ordering changes.
</content>
