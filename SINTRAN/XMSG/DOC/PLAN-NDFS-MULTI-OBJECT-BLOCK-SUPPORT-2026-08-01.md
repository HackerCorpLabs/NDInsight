# Plan: full multi-object-block support in NDFS (2026-08-01)

Scope requested: read more than 1000 files in a user area, create more than 1000, an API for
adding (and possibly removing) object blocks, unit tests that exercise all of it, and per-user
block metadata surfaced in the RetroCommander UI.

Applies to all four implementations: `E:\Dev\Ronny\norskdata-ndfs` (C, Python, TypeScript) and
`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS` (C#), plus the RetroCommander UI.

Ground truth: [NDFS-OBJECT-BLOCKS-DECODED-2026-08-01.md](NDFS-OBJECT-BLOCKS-DECODED-2026-08-01.md).
Already landed: byte 47 is parsed, written and documented in all four ports; the three
`norskdata-ndfs` documents that asserted a 256-file limit are corrected.

---

## 1. Analysis of the request - three things worth correcting up front

### 1.1 "Read > 1000 files" needs the read path fixed, not just the field

Reading beyond 256 is **not** currently possible, and the reason is not the byte-47 field (now
done). Every implementation walks only pages `U*8 .. U*8+7` - one object block. Files in a second
block are simply never seen. Worse, the ones that ARE seen get their file number from raw physical
position, so a second-block file would report a number and owner that are both wrong.

So the read work is two changes, and both are required before a single file above 256 is visible:
walk all `ACOBL` blocks, and compute the number as `block*256 + slot`.

### 1.2 "Add/remove object blocks" - SINTRAN only adds

`@GIVE-OBJECT-BLOCKS` **adds** to what the user already has; the manual is explicit ("The number of
object blocks is added to the object blocks already given to the user area"). **No remove command
is documented anywhere in `ND-30.003.007`**, and none was found while carving.

That is not an oversight in the manual - removing a block is dangerous by construction. Blocks are
allocated on demand and a user's files live in them; freeing block *n* silently orphans every file
numbered `n*256 .. n*256+255`, and file numbers are stable identifiers a client may be holding.

**Recommendation.** Provide:
 - `GiveObjectBlocks(user, count)` - raises MXOBL, matching SINTRAN. Capped at 16.
 - Block *allocation* stays implicit, on demand during create, exactly as SINTRAN does it (this is
   why our verified pack has MXOBL 4 but ACOBL 2).
 - `SetMaxObjectBlocks(user, n)` for lowering, **refusing** if any allocated block would fall above
   the new maximum.
 - **No "free an allocated block" API** unless we find evidence SINTRAN has one. If we ever add
   one it must refuse while the block holds any entry, and it must be documented as OUR extension,
   not SINTRAN behaviour.

### 1.3 The object file must be SubIndexed - this is the real structural cost [VERIFIED]

A user's object block *n* occupies pages `n*512 + U*8 .. +7`. An **Indexed** object file is one
index block = **512 page pointers**, so it can address pages 0..511 only - which is exactly block 0
for users 0..63. **Block 1 begins at page 512 and is unreachable in an Indexed object file.**

Measured on the pack that has a second block:

```
object_file_pointer   type=SubIndexed   block_id=18079
user_file_pointer     type=Indexed      block_id=18686
bit_file_pointer      type=Contiguous   block_id=18468
```

So "give this user another object block" is not always a nibble change. On a pack whose object file
is Indexed it requires **converting the object file to SubIndexed** first: allocate a sub-index
block, move the existing index block under it, and rewrite `MasterBlock.ObjectFilePointer`. That is
a structural rewrite of a live filesystem and must be treated as such - the riskiest operation in
this plan.

**UNKNOWN:** whether SINTRAN's own `@GIVE-OBJECT-BLOCKS` performs that conversion, refuses, or
assumes the object file is already sub-indexed. Our pack was already SubIndexed before the
experiment, so the experiment does not answer it. See section 5.

---

## 2. An unknown that must not be guessed: users above 63

512 pages per index block divided by 8 pages per user gives **64 users per index block**. But the
user file holds **256 users** (8 pointers x 32 entries).

Under the naive reading of `n*512 + U*8`, user 64's block 0 would land on page 512 - the same page
as user 0's block 1. They cannot both be right.

**All our evidence is user 8, blocks 0 and 1.** Possible resolutions include a different stride for
high users, a per-user block table we have not found, or a genuine limit of 64 users on a pack with
multi-block users. **Do not implement any behaviour for users >= 64 until this is settled.** The
implementation should assert/refuse rather than silently compute a colliding page.

How to settle it: create a user with index >= 64 on a test pack, give it files, and see where its
entries land.

---

## 3. Work breakdown

### 3.1 Read path (unblocks "read > 1000 files")

For each implementation:

1. A helper that enumerates a user's object-block pages:
   `for n in 0 .. ACOBL-1: pages n*512 + U*8 .. +7`.
2. Directory walk uses it instead of the fixed `U*8..U*8+7`.
3. **File number computed, not taken from physical position:**
   ```
   block      = page / 512
   slot       = (page % 512 - U*8) * 32 + entryInPage
   fileNumber = block * 256 + slot
   ```
   Keep the physical position as a separate field - the write-back path needs it.

Ready-made regression vector: on the BIGMAN pack, `F0500` must report **FILE 307, owner 8**. A
position-derived implementation reports index 18483 and owner 72.

### 3.2 Create path (unblocks "add > 1000 files")

4. Free-slot search spans all **allocated** blocks.
5. When all allocated blocks are full and `ACOBL < MXOBL`: allocate the next block (8 pages, wired
   in at `n*512 + U*8`), raise ACOBL, continue.
6. When `ACOBL == MXOBL`: fail with the "user object table is full" error - which is now correct
   rather than premature.
7. If the object file is Indexed and block >= 1 is needed: either convert to SubIndexed (section
   1.3) or refuse with a clear message. **Refusing is the correct first version** - conversion is a
   separate, larger piece of work with its own tests.

### 3.3 API

8. `GiveObjectBlocks(user, count)` / `SetMaxObjectBlocks(user, n)` per section 1.2, in all four
   ports, with the 1..16 clamp already present in the byte-47 writers.
9. Expose read-only derived values: `MaxFiles = MXOBL * 256`, `AllocatedFiles = ACOBL * 256`,
   and the count of entries actually in use.

### 3.4 Tests

10. Byte-47 round trip for a multi-block user (**already verified against the real pack** - promote
    it to a unit test with a synthetic fixture so it runs without the pack).
11. Directory walk finds entries in block 2+.
12. File number for a second-block file - the `F0500` = 307 vector.
13. Create past 256 allocates a block and raises ACOBL, rather than failing.
14. Create past `MXOBL*256` fails cleanly.
15. Create past 256 on an **Indexed** object file fails with the structural message (section 3.2.7).
16. A large-scale test: give 4 blocks, create 1000+ files, read them all back, verify numbering is
    contiguous across the block boundary and that number 255 -> 256 crosses correctly.

Test 16 is the one the request is really about, and it is the one that would have caught all of
this years ago.

### 3.5 RetroCommander UI

17. Surface per user: **allocated blocks**, **max blocks**, **max files** (`MXOBL*256`), **files in
    use**, alongside the existing pages used / reserved. The natural place is wherever user quota is
    already displayed.
18. A user at `ACOBL == MXOBL` with a full last block is at a hard limit and should be visibly
    distinct from one that can still grow - that distinction is invisible today, which is precisely
    how "creating file 257 fails" becomes a mystery.

---

## 4. Ordering

| Step | Why this order |
|---|---|
| 3.1 read path | Nothing else can be verified until multi-block files are visible |
| 3.4 tests 10-12 | Lock the read path against the real vector before touching create |
| 3.2 create path | Depends on a correct read path to verify against |
| 3.4 tests 13-16 | Including the 1000-file test |
| 3.3 API | Thin once the mechanics work |
| 3.5 UI | Needs the API |
| section 1.3 conversion | Separate piece, only if we decide we need it |

---

## 5. Experiments still outstanding

- **Users >= 64** (section 2) - blocks any implementation for high user indices.
- **Does `@GIVE-OBJECT-BLOCKS` convert an Indexed object file?** Take a pack with an Indexed object
  file, run the command, and compare the master block pointer before and after. Determines whether
  section 3.2.7 refuses forever or is a stopgap.
- **A third block** - our stride evidence is one transition (block 0 -> 1). A user with >512 files
  would confirm `n*512` generalises. Test 16 produces exactly that if it creates 1000+ files, so
  this is nearly free once the create path works.
