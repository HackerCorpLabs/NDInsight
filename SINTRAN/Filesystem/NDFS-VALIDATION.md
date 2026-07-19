# NDFS report validation against the SINTRAN kernel + real disk

This document validates an external NDFS (Norsk Data File System)
reverse-engineering report - reconstructed from real disk images by an
independent effort that did **not** have the producing kernel code - against the
**producing SINTRAN III kernel** we hold (the carved `006-S3FS` segment from the
real SINTRAN L image) and the **real disk** `SMD0.IMG` (volume PACK-ONE).

**Rule of evidence:** each claim is graded
- **CONFIRMED** - the kernel routine and/or real disk bytes prove it,
- **CORRECTED** - the kernel proves a different answer (given here with evidence),
- **STILL-OPEN** - neither the carved kernel nor the disk settles it (with exactly
  what would).

Kernel anchors (all octal, in `006-S3FS`): writer **`WXDIR` = 37702B**, reader
**`RXDIR` = 37643B**, enter/validate **`CHDSI` = 37763B**, release
**`REENB` = 40162B**, create-directory **`CRDIR` = 136741B**. On-disk multi-byte
values are **big-endian**. The `CHDSI` logic below is the validation core of the
full mount flow; for the complete end-to-end trace (command dispatch -> `ENDIR`
140176B -> the page-0 device read -> `CHDSI` -> write-back) see
[`code-logic/enter-directory.md`](code-logic/enter-directory.md). Full field detail:
[on-disk-format/extended-info-block.md](on-disk-format/extended-info-block.md) and
[on-disk-format/directory-label.md](on-disk-format/directory-label.md).

---

## 1. Claim-by-claim table

### Page-0 layout

| NDFS claim | Verdict | Evidence |
|------------|---------|----------|
| Boot sector 0x0000-0x03FF | CONFIRMED (structure) | Boot/loader region; kernel FS routines never read below word 1750B. |
| "Reserved/unused" 0x0400-0x07CF | STILL-OPEN | Kernel FS routines (`RXDIR`/`WXDIR`/`CHDSI`) touch only words 1750-1757B and the master block at 1760B. This 1600-byte gap is boot-loader territory (FLOMON/BPUN), not filesystem metadata - see Q6. |
| Extended-info block at 0x07D0, 16 B, hard-disk only | CONFIRMED | `RXDIR`/`WXDIR` operate on an 8-word (16 B) block; floppy PACK images are FLOMON with boot remnants there. |
| Master block at 0x07E0, 32 B | CONFIRMED | Matches [directory-label.md](on-disk-format/directory-label.md); PACK-ONE decodes cleanly. |

### Master block

| NDFS claim | Verdict | Evidence |
|------------|---------|----------|
| Directory name: 16 B ASCII, terminated by one `0x27` then NULs (not a padded fixed field) | CONFIRMED | PACK-ONE `50 41 43 4B 2D 4F 4E 45 27 00...` = `PACK-ONE'`; a 16-char name has no terminator (floppy cross-check). |
| Object-file ptr @0x10, User-file ptr @0x14, Bit-file ptr @0x18 | CONFIRMED | Real disk decode + NDFS reader agree; see directory-label.md. |
| Unreserved pages @0x1C, uint32 BE, semantics INFERRED | CONFIRMED (field) / STILL-OPEN (semantics) | Value `0x2CA4`=11428 on PACK-ONE; see Q3. |
| BlockPointer = 2-bit type + 30-bit id; blockId 0 in index = sparse hole | CONFIRMED | Matches directory-label.md; type 00/01/10/11 = contiguous/indexed/sub-indexed/reserved. |

### Extended-info block (the part NDFS marked LOW/MEDIUM confidence)

| NDFS claim | Verdict | Evidence |
|------------|---------|----------|
| w1750 checksum (2 B) | CONFIRMED | `WXDIR` stores it in word 0; `CHDSI` validates. |
| w1751-1753 reserved 1/2/3, always 0, purpose unknown | CONFIRMED (0) / STILL-OPEN (purpose) | Kernel sums them into the checksum but never tests/sets them; Q5. |
| w1754 FLAG WORD, purpose 100% unknown | **CORRECTED** | Bit 15 = "directory entered/in use". `CHDSI` sets it on enter, `REENB` clears it on release. Q2. |
| w1755 System Number = "last system number" (guessed) | CONFIRMED | It is the system that has the directory entered; `CHDSI` compares + stores it. Q4. |
| w1756-1757 Pages Available = capacity? (guessed) | CONFIRMED (capacity) | `CHDSI` reads/compares/writes it as the directory's total page capacity. Q3. |
| Checksum = XOR of six words, then + SystemNumber | **CORRECTED** | The kernel uses a **16-bit ADDITIVE SUM of all seven** following words (1751-1757). XOR is a coincidental look-alike on this sample. Q1. |
| Accept states: Valid / ValidLowByteOnly / Invalid | **CORRECTED** | The kernel writes and compares a full 16-bit sum. "ValidLowByteOnly" is not a kernel concept; and on a bad checksum the kernel **rebuilds** the block rather than rejecting it. Q1/Q7. |

---

## 2. What we now know that NDFS did not

1. **The checksum is a plain 16-bit additive sum, not XOR.** Proven from *both*
   the writer (`WXDIR` 37702B) and the validator (`CHDSI` 37763B), which run the
   identical `ADD ,X 0` accumulation loop over words 1751-1757. The XOR-then-add
   formula reproduced PACK-ONE only because its one overlapping set bit (bit 15,
   shared by flag `0x8000` and pages-lo `0x9051`) carries out past bit 15 under
   ADD exactly where it cancels under XOR.

2. **The flag word has a real meaning: bit 15 = "directory entered".** Set at
   enter (`CHDSI`), cleared at release (`REENB`). PACK-ONE's `0x8000` means it was
   left entered (not cleanly released).

3. **The system number is the current owner, enforced.** `CHDSI` rejects an enter
   when bit 15 is set *and* the stored system number differs from the entering
   system (and is non-zero) - a cross-system interlock.

4. **A bad checksum triggers self-repair, not rejection.** `CHDSI` zeroes the
   8-word block and rebuilds it (writes capacity, stamps owner + flag, recomputes
   the checksum). The block is a self-healing convenience record, not a mount gate.

5. **Pages-available is the directory capacity** (compared against device
   geometry by `CHDSI`), distinct from the master block's separate
   `unreserved_pages` counter and from the bitmap's live free/used counts.

---

## 3. The 7 ranked open questions - answered

### Q1 - checksum routine, XOR-then-ADD shape, low-byte-only legitimacy
**VERIFIED (corrected).** The routine is `WXDIR` = **37702B** (writer) with the
matching validator inside `CHDSI` = **37763B**. The real formula is:

```
checksum(word 1750B) = ( w1751 + w1752 + w1753 + w1754 + w1755 + w1756 + w1757 ) mod 2^16
```

a **16-bit additive sum** of the seven words after the checksum - **not**
XOR-then-ADD. Decisive disassembly (writer):

```
037716  062000  ADD ,X 0     ; A += word    (ADD, not REXO)
037720  146401  RADD AD1 0 DD ; loop counter 1..7
037723  006000  STA ,X 0     ; store sum in word 0
```

Numeric check on PACK-ONE: `0x8000 + 0x0066 + 0x9051 = 0x110B7 -> 0x10B7` =
stored checksum. The **low-byte-only** acceptance is **not legitimate** from the
kernel's view: it writes/compares a full 16-bit value. (Full derivation and the
XOR-coincidence explanation:
[extended-info-block.md §2](on-disk-format/extended-info-block.md#2-checksum---a-16-bit-additive-sum-kernel-corrected).)

### Q2 - flag word (1754B) bit definitions
**VERIFIED (partial).** **Bit 15 (`0x8000`) = "directory entered / in use".**
`CHDSI` sets it on enter (`BSET ONE 170 DA` at 040124 - `170` octal = bit 15);
`REENB` clears it on release (`BSET ZRO 170 DA` at 040201). PACK-ONE's `0x8000`
is this bit. **Bits 0-14: STILL-OPEN** - not tested or set by any carved routine;
`0` on the real disk.

### Q3 - Unreserved Pages (master +0x1C) vs Pages Available (1756-7)
**VERIFIED for pages-available; unreserved partly open.**
<a id="unreserved-pages-vs-pages-available"></a>

- **Pages Available (extended info, words 1756-1757)** = the directory's **total
  page capacity**. `CHDSI` reads it and compares it to a device-geometry figure
  (`LDD ,X 6` at 040027), and writes the geometry value on rebuild (`STD ,X 6` at
  040077). PACK-ONE = 36945. **Capacity, kept in the extended-info block, stamped
  and checked at enter time.** VERIFIED.
- **Unreserved Pages (master block, +0x1C)** = a **separate 32-bit bookkeeping
  counter in the 32-byte master block**, not in the extended-info block, and
  **not** touched by `CHDSI`/`WXDIR`. PACK-ONE = 11428, which is neither the
  capacity (36945) nor the bitmap's live free (24123) or used (14277) counts. Its
  exact update rule is **STILL-OPEN**, routed to the `CRDIR`/`GMAIN` master-block
  writer (OPEN-Q1/Q5 in the [foundation README](README.md#6-open-questions---what-each-later-phase-needs)).

So: pages-available = capacity (VERIFIED); unreserved-pages = a distinct
master-block counter (semantics OPEN); the **bitmap** remains the authority for
live free/used pages.

### Q4 - System Number (1755) = last system that entered?
**VERIFIED.** Yes. `CHDSI` treats word 5 as the current owner: it errors if the
stored number is non-zero and differs from the entering system (040113-040120),
otherwise stores the entering system's number (`STA ,X 5` at 040122). PACK-ONE =
102.

### Q5 - Reserved words 1751-1753: truly reserved, or version/type fields?
**Kernel shows them as truly reserved on this revision.** They are included in the
checksum sum but never individually tested or written by any carved routine, and
are `0` on the real disk. Whether another SINTRAN version repurposes them is
**STILL-OPEN** (would need a differently-versioned disk or that version's source).

### Q6 - does anything live in 0x0400-0x07CF (the 1600-B gap)?
**STILL-OPEN, but bounded.** No carved filesystem routine reads or writes below
page-0 word 1750B; the whole 0x000-0x07CF region is boot-loader space
(FLOMON/BPUN/raw bootstrap), examined by the bootstrap, not by the filesystem.
Confirming byte-level use of that gap requires analysing the boot loader, not
`006-S3FS`. From the filesystem's side it is not metadata.

### Q7 - charset/length validation at mount, and bad-checksum behaviour
**Bad checksum: VERIFIED.** SINTRAN does **not** refuse or merely warn. On a
mismatched-or-zero checksum `CHDSI` **zeroes the 8-word extended-info block and
rebuilds it** (writes capacity, stamps owner + flag, recomputes the checksum via
`WXDIR`) - i.e. **repair/re-initialise**. Decisive lines: compare at 040017
(`SKP IF DA EQL ST`), branch to rebuild at 040020/040021, zero-fill loop at
040063-040071.

**Directory-name charset/length validation at mount: STILL-OPEN.** `CHDSI`
validates the extended-info checksum, not the master-block name. Name validation
(if any) would sit in the master-block reader `GMAIN` = 47653B / `GDIRA` =
30225B - not yet decoded to that depth. The independent NDFS reader validates the
name as printable ASCII, but that is the reader's rule, not proven kernel
behaviour.

---

## 4. Resolved vs open - summary

| Q | Topic | Status |
|---|-------|--------|
| 1 | Checksum algorithm + low-byte form | **VERIFIED** (corrected to additive sum; low-byte-only rejected) |
| 2 | Flag-word bits | **VERIFIED** for bit 15 = entered; other bits OPEN |
| 3 | Unreserved vs pages-available | **VERIFIED** pages-available = capacity; unreserved semantics OPEN |
| 4 | System number = owner | **VERIFIED** |
| 5 | Reserved words 1751-1753 | Reserved on this revision (kernel-shown); cross-version OPEN |
| 6 | 0x0400-0x07CF gap | STILL-OPEN (boot-loader space, outside the FS segment) |
| 7 | Bad-checksum behaviour | **VERIFIED** (repair/rebuild); name-charset validation OPEN |

**Provenance:** carved `006-S3FS` SINTRAN L bytes (`WXDIR` 37702B, `RXDIR`
37643B, `CHDSI` 37763B, `REENB` 40162B); real disk `~/repos/nd100x/SMD0.IMG`
page-0 bytes 0x07D0-0x07FF; independent NDFS C library as the report under test.
