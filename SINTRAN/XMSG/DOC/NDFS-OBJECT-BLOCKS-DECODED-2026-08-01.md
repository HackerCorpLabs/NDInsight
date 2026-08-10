# NDFS object blocks decoded: MXOBL/ACOBL and where blocks 2-16 live (2026-08-01)

> ## CORRECTED 2026-08-02 — the file-number formula here is wrong for a relocated block
>
> This document derives the logical block from the **physical** index-block group
> (`block = page / 512`). Measured live on a 201-user pack, SINTRAN **relocates** a user's
> overflow object block when another user needs that group — user 8's overflow was watched
> moving group 2 -> 3 -> 4 as users 136 and 200 created files, with all 300 files surviving
> and staying numbered FILE 0..299.
>
> So the logical block is the **ordinal rank** of the group among those the user occupies,
> not the group number. On that pack the formula below gives one user 0..255 then
> **1024..1067**, where SINTRAN says **256..299**.
>
> The `F0500 = FILE 307` vector below is still correct — but it holds under **both** readings,
> because nothing had displaced that pack's overflow block, so rank happened to equal group
> number. It never could have discriminated them.
>
> Correct rule and evidence: `norskdata-ndfs/docs/NDFS-OBJECT-BLOCKS-SPEC.md` sections 6.1-6.2.



Until today no code in any of the four NDFS implementations knew that a SINTRAN user can own more
than one object block. This documents the encoding and the on-disk placement, both measured on a
real SINTRAN III K pack with a control group.

Everything here is **VERIFIED** against
`F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG` unless marked otherwise.

---

## 1. The pack, and why it is a clean experiment

On 2026-08-01, node 100 got a purpose-built user:

```
CREATE-USER BIGMAN
GIVE-USER-SPACE BIGMAN 4000
USER-STATISTICS BIGMAN     ->  MAXIMUM NUMBER OF FILES : 256
GIVE-OBJECT-BLOCKS BIGMAN,3
USER-STATISTICS BIGMAN     ->  MAXIMUM NUMBER OF FILES : 1024
```

then 482 one-page files were created under it. **BIGMAN is the only user on the pack with more than
one object block**, so every other user is a control.

---

## 2. User-entry byte 47 = MXOBL / ACOBL, zero-based nibbles [VERIFIED]

All four implementations carry a comment saying byte 47 holds "mxobl/acobl nibbles" and none of them
parse it. Decoded across every user on the pack:

```
idx=0  SYSTEM        byte47=0x00   hi=0 lo=0
idx=1  FLOPPY-USER   byte47=0x00   hi=0 lo=0
idx=2  UTILITY       byte47=0x00   hi=0 lo=0
...
idx=8  BIGMAN        byte47=0x31   hi=3 lo=1     pages_used=499
```

| nibble | meaning | BIGMAN | cross-check |
|---|---|---|---|
| **high** = `MXOBL - 1` | maximum object blocks the user may have | 3 -> **4 blocks** | `MAXIMUM NUMBER OF FILES : 1024` = 4 x 256 |
| **low** = `ACOBL - 1` | object blocks actually allocated so far | 1 -> **2 blocks** | 482 files needs ceil(482/256) = 2 |

**Both nibbles are zero-based**: a default user reads `0x00`, meaning 1 max and 1 allocated, i.e. 256
files. That is why every untouched user on the pack is `0x00`.

Three independent quantities agree (the operator command, the reported maximum, and the file count),
so the decode is not a coincidence of one sample.

Neighbouring bytes, for completeness: bytes 42-43 and 44-45 both hold the user index
(BIGMAN: `00 08 00 08`), and byte 46 is zero for every user - so `46-47` may be a 16-bit field with
only the low byte in use. **INFERRED**, not established.

---

## 3. Where object blocks 2..16 live [VERIFIED]

The object file is one global structure for the whole pack, reached from
`MasterBlock.ObjectFilePointer`. Each index block holds 512 page pointers; each page holds 32
64-byte entries. A user's object block is 8 pages = 256 entries.

Every implementation assumes user `U` owns exactly pages `U*8 .. U*8+7`. BIGMAN's 483 entries were
found at:

```
pages  64.. 71      =        U*8         block 1  (as assumed)
pages 576..583      = 512 +  U*8         block 2  (previously unknown)
```

> **Object block `n` (0-based) for user `U` occupies pages `n*512 + U*8` through `n*512 + U*8 + 7`.**

The object file is therefore a two-dimensional array indexed by (block, user): successive object
blocks of the same user are a **stride of 512 pages** apart, which is exactly one index block.

**Scope of the evidence:** the stride is measured from ONE transition (block 1 -> block 2) on ONE
user. It is consistent with the 512-pointer index-block structure, which is why it is stated as the
rule, but a user with three or more allocated blocks would confirm it. Getting there needs a user
with more than 512 files.

---

## 4. The file number, and the bug this causes [VERIFIED]

Given the page a 64-byte entry sits in and its position within that page:

```
block      = page / 512
slot       = (page % 512 - U*8) * 32 + entryInPage
fileNumber = block * 256 + slot
```

Checked against the only file number measured independently, on the wire
([SINTRAN-FILE-NUMBER-IS-16-BIT-2026-08-01.md](SINTRAN-FILE-NUMBER-IS-16-BIT-2026-08-01.md)):
`F0500` is `FILE 307`, and its entry is at page 577, position 19:

```
block = 577 / 512                = 1
slot  = (577 % 512 - 64)*32 + 19 = (65 - 64)*32 + 19 = 51
file  = 1*256 + 51               = 307      MATCHES
```

**The bug in all four implementations.** They set the object index from the raw physical position:

```
objectIndex = page * 32 + entryInPage       ->  577*32 + 19 = 18483
```

and then treat its high byte as the owning user (`18483 >> 8` = **72**, a user that does not exist)
and its low byte as the file number (`18483 & 0xFF` = **51**, which is the slot, not the number).

Byte 34 of the entry still carries the true owner (8), which is why directory listings look correct
and the defect stayed invisible: **only files in a user's second or later object block are affected,
and no pack in the project had one until today.**

---

## 5. What each implementation must change

Applies to `E:\Dev\Ronny\norskdata-ndfs` (C, Python, TypeScript) and
`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS` (C#).

1. **Parse byte 47** into `MaxObjectBlocks = (b47 >> 4) + 1` and
   `AllocatedObjectBlocks = (b47 & 0x0F) + 1`, and write it back the same way. Today the byte
   survives only because the raw record is copied wholesale.
2. **Stop deriving the file number from physical position.** Compute it with the formula in
   section 4, or read it from the entry, and keep the physical position as a separate field for the
   write-back path.
3. **Walk all allocated blocks**, not just `U*8..U*8+7`: for `n` in `0 .. ACOBL-1`, read pages
   `n*512 + U*8`.
4. **Free-slot search** must span allocated blocks, and must report "needs a new block" rather than
   "table full" while `ACOBL < MXOBL`.
5. **Allocating a new block** raises `ACOBL`; refusing past `MXOBL` (max 16) is the correct
   behaviour, matching SINTRAN's own limit of 4096 files.
6. **Fix the documentation that asserts the wrong invariant**:
   `E:\Dev\Ronny\norskdata-ndfs\docs\NDFS-FORMAT.md` lines 226-228 and 344-356,
   `E:\Dev\Ronny\norskdata-ndfs\CLAUDE.md` line 55, and
   `E:\Dev\Ronny\norskdata-ndfs\docs\NDFS-VALIDATION-PLAN.md` line 51 (which plans a test for "what
   happens at the 256th file per user" - the answer is now known).

---

## 6. Reproducing

```
python probe_mxobl.py     # byte 47 for every user, with the control group
python probe_blocks.py    # page ranges per user, showing the 512-page stride
```

Both are read-only and use the `ndfs-py` port to parse the pack
(`E:\Dev\Ronny\norskdata-ndfs\ndfs-py\src`).

---

## 7. Related

- [SINTRAN-FILE-NUMBER-IS-16-BIT-2026-08-01.md](SINTRAN-FILE-NUMBER-IS-16-BIT-2026-08-01.md) - the wire format of the number, and the experiment that produced this pack
- [XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md](XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md) - the object entry is shipped verbatim by the COSMOS file server, so this defect reaches the wire
- [PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md](PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md) section 2
