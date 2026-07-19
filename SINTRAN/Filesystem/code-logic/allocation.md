# Page-bitmap allocation logic (Phase 5)

How `006-S3FS` allocates and frees pages: the bit-file (allocation bitmap)
primitives, the **search direction** (the Phase-4 open question, resolved below
from the bytes), and how file/user/directory creation claims bitmap pages and
writes the object/user record.

**Evidence tags:** **VERIFIED** = traced in the `006-S3FS` bytes; **INFERRED** =
deduced from a helper pointer or a carved cross-check; **OPEN** = the bytes
examined do not settle it. Disassembly:
[`../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm).
Companion: [`s3fs-code-map.md`](s3fs-code-map.md), [`file-io.md`](file-io.md).

---

## 1. The bit file - what a "page" and a "bit" are

- The bit file is the on-disk **allocation bitmap**: **one bit per page**,
  **0 = free, 1 = used** (VERIFIED against real disk + NDFS `bit_file.h`; see the
  [Filesystem README](../README.md) §4.4).
- A **page = 1024 words = 2048 bytes** (VERIFIED: the page<->word scale factor is
  `SHA ZIN 12` = arithmetic shift left by 12₈ = 10₁₀ places = ×1024 words; it
  appears at `RDPAG` 107536B and 107620B, with the inverse `SAD ZIN SHR 12` on
  the read side - see [`file-io.md`](file-io.md) §3).
- The bitmap is stored 16 bits per word. Page number -> word index is a right
  shift by 4 (`SHA ZIN SHR 4` at `TPAGF` 51043B) and the low 4 bits select the
  bit within the word.
- The `@CHANGE-BIT-FILE` / `@DUMP-BIT-FILE` docs describe the bit file as divided
  into **20₈-word blocks** (16 decimal words = 256 pages per bit-file block)
  (ND-60.128.5 lines 1286, 4295).
- **Blocks 0-6 are reserved** (boot, label, object/user/bit-file roots); the
  lowest allocatable page is **block 7** (VERIFIED in NDFS `NDFS_FIRST_ALLOC_BLOCK`;
  the same floor shows up in the scan as the lower bound - see §4).

---

## 2. The one-bit primitives - `ALPAG` / `RLPAG` / `TPAGF` / `TESTB`

These operate on **one already-chosen page**; they do not search.

### `ALPAG` 50627B / `XRLPA` 50632B / `RLPAG` 50635B - mark used / free (VERIFIED)

Three entry points that set the SSK/SSM flags and drop into a shared read-modify-
write body at 50637B:

```
ALPAG 50627  BSET ONE SSK   ; SSK=1
      50630  BSET ONE SSM   ; SSM=1  -> "mark used"
      50631  JMP 6 -> 50637
XRLPA 50632  BSET ZRO SSK   ; SSK=0
      50633  BSET ONE SSM   ; SSM=1
      50634  JMP 3 -> 50637
RLPAG 50635  BSET ZRO SSK   ; SSK=0
      50636  BSET ZRO SSM   ; SSM=0  -> "mark free"
```

The shared body (50637B+) computes the word address of the target bit, reads the
old value (`BSTA 0 DX` captures the previous bit), then sets or clears bit 10 of
the word per SSM (50645-50651: `BSKP ONE SSM` -> `BSET ONE 10 DX` else
`BSET ZRO 10 DX`), and updates the free-page count. SSK selects whether the
previous value is returned / checked. This is the classic bitmap set-bit /
clear-bit with the free counter maintained alongside. (VERIFIED from the bytes;
the exact free-counter field offset is INFERRED.)

### `TPAGF` 51025B - is this page free? (VERIFIED)

Takes a page number in `,B 2`, forms the word index (`SHA ZIN SHR 4` = page/16 at
51043B), loads the bitmap word (`LDT ,X 1`), builds a one-bit test with
`AND`/`SHA`/`ADD` then `EXR SA` (execute the assembled instruction) and returns
free/used. Pure single-bit test, no side effect.

### `TESTB` 51353B / `TESTP` 51355B - the free-page **scanner** (VERIFIED - key routine)

`TESTB` is `TESTP` with A pre-loaded (`SAA 20`). `TESTP` is the routine that
**scans the bitmap for a free page**, and its scan direction resolves the open
question (§4).

---

## 3. `GPAGE` 76205B - allocate one file page (VERIFIED spine)

`GPAGE` is the core "give me a page for a growing file" allocator (called from
the write/expand paths). Its prologue sets up a 21₈-word frame (`SAB 21`), then:

- Checks device/reservation state and issues `MON 140` (ReservationInfo / WHDEV)
  and `MON 125` (ForceRelease) around the bit-file device (76244-76262B) -
  i.e. it reserves the bit-file device before touching it (VERIFIED bytes).
- Reads the directory/bit-file descriptor (`LDX ,B 0`, `LDT ,X 15`, `LDA ,X 16`,
  `SHA ZIN SHR 10` at 76263-76267B).
- Runs the free-bit search and, on success, marks the page and charges quota.

**The two calls that prove `GPAGE`'s allocation model** (VERIFIED): `GPAGE`'s
PLANC frame pointer-pool holds the addresses of **`RSPAG` (51120B)** and
**`CUSED` (55206B)** at words 76771B and 76770B:

```
076770  055206      ; = CUSED  (change pages-used = quota accounting)
076771  051120      ; = RSPAG  (reserve/set a page range)
```

So `GPAGE` allocates by calling **`RSPAG`** to reserve the page in the bitmap and
**`CUSED`** to charge the owning user's quota. Because `RSPAG` is the range-reserve
primitive, single-page growth and contiguous allocation go through the **same**
reservation search - which is what makes the direction question answerable from
one routine (§4). (The precise page number `GPAGE` hands to `RSPAG`, and the
allocation-failure path, are INFERRED from the surrounding logic; the two pool
addresses are VERIFIED byte-for-byte.)

---

## 4. Search direction - RESOLVED: **high-to-low (downward from the top)**

**Open question (README OPEN-Q3):** NDFS's reference reader allocates *upward*
from block 7 (`ndfs_bf_find_free`), but the `@CREATE-FILE` doc (rule 3) says
"Contiguous files are positioned in the **highest** page address range possible"
(ND-60.128.5 line 2157). Which is real?

**Answer from the `006-S3FS` bytes: the scan runs DOWNWARD from the high end of
the bitmap toward block 7.** The NDFS reader's upward `find_free` is a modern
simplification and does **not** match the real filesystem for allocation.

### Byte evidence

`RSPAG` (the reserve primitive `GPAGE` and contiguous-create both use) calls the
scanner `TESTP` through its frame pointer-pool:

```
051227  051355      ; = TESTP  (RSPAG's scan helper)
051230  051353      ; = TESTB
```

`TESTP` (51355B) scans by **decrementing** the bitmap **word index** `X`:

```
051363  LDT ,B 10          ; T = lower-bound word (the block-7 floor)
051364  SKP IF DX LST ST   ; loop continues while X >= floor...
051365  JMP 3 -> 051370
051366  RADD CLD SD DL     ; ...terminate when X < floor (nothing free)
051367  EXIT
051370  JAP 4 -> 051374
051371  SAA 170            ; reset the in-word bit counter (170₈)
051372  AAX -1             ; X-- : step to the NEXT-LOWER bitmap word
051373  JMP -10 -> 051363
051374  LDT ,X 0           ; load bitmap word at X
051375  AAT 1
051376  SKP IF DT EQL 0    ; if word == -1 (all ones = all used)...
051377  JMP 4 -> 051403
051400  SAA 170
051401  AAX -1             ; ...skip it, X-- again (downward)
051402  JMP -17 -> 051363
051403  AAT -1             ; found a word with a free bit: locate it
051404  ADD 42
051405  EXR SA
```

`AAX -1` at 51372B and 51401B is the direction proof: the word index only ever
**decreases**, and the loop's guard (`SKP IF DX LST ST`, T = the floor word at
`,B 10`) stops it at the low bound. So allocation starts at the **top** of the
bitmap and walks **down** to the block-7 floor - exactly the "highest page
address range" the `@CREATE-FILE` doc describes, and the opposite of NDFS's
naive upward search. (VERIFIED.)

### What this means

- **Contiguous files** (`CRALF`) reserve a run of pages high-first via `RSPAG` ->
  `TESTP` -> downward scan. This is why the doc restricts a default contiguous
  file to half the disk (the bit file sits mid-disk and contiguous space grows
  downward from the top - ND-60.128.5 line 2073). (VERIFIED direction; the
  run-length fit logic in `RSPAG` at 51232-51344B is INFERRED in detail.)
- **Indexed-file single pages** (`GPAGE`) take the highest free page via the same
  `RSPAG`/`TESTP` path. (VERIFIED that `GPAGE` routes through `RSPAG`; that the
  count is 1 is INFERRED.)
- **The block-7 floor** is enforced as the scan's lower bound word `,B 10`, not by
  a separate guard. (VERIFIED that a low-bound word terminates the scan; that its
  value equals block 7 exactly is INFERRED - it is a per-directory field.)

> OPEN: the exact in-word bit ordering (whether bit 0 or bit 15 of a word is the
> lower page) is not fully pinned - `TESTP` resets the in-word counter to 170₈
> and steps it, but the intra-word direction was not byte-confirmed here. It does
> not change the coarse (word-level) high-to-low result above.

---

## 5. Contiguous range reserve - `RSPAG` 51120B (VERIFIED spine)

`RSPAG` sets up a 21₈-word frame and drives `TESTB`/`TESTP` to find and mark a
run of free pages. Structure (from the bytes):

- 51127-51132B: `STX ,B 7` (bitmap descriptor), then `RADD CLD SX DA; ADD 65;
  STA ,B 10` builds the **upper starting point + floor** for the scan.
- 51145-51170B: reads the requested count / bounds into frame slots (`,B 12`,
  `,B 13`, `,B 14`, `,B 15`).
- 51232-51344B: the fit loop - for each candidate the run length is measured
  (`ADD ,B 17` / `SUB ,B 17` around `,B 16`) and on a successful run it marks the
  pages. It calls `TESTP` (51355B) / `TESTB` (51353B) via the pool at 51227/51230B.
- On failure it returns error 37₈ (`SAA 37; STA ,B 2` at 51336B) = NO SPACE.

(The exact loop arithmetic is INFERRED; the `TESTP` call and the downward scan it
performs are VERIFIED.)

---

## 6. File allocation - `CRALN` / `CRALF` / `EXPFI` / `SFACC` dispatcher (105564B, VERIFIED)

Four public entries share **one body** at 105564B, forked on (SSM,SSK):

```
SFACC 105552  BSET ONE SSM / BSET ONE SSK   ; set file access
EXPFI 105555  BSET ONE SSM / BSET ZRO SSK   ; expand file
CRALN 105560  BSET ONE SSK  (SSM=0 @105563) ; create-allocated INDEXED
CRALF 105562  BSET ZRO SSK  / BSET ZRO SSM  ; create-allocated CONTIGUOUS
```

The shared body (frame `SAB 145`, a large 145₈-word frame) at 105564B+:

- 105571-105574B: `SHA LIN 2` / `BSET BAC 0 DA` builds a mode selector from the
  (SSM,SSK) flags into `,B 123` - the two flags become a 0..3 case index.
- 105575-105621B: copies the caller's arguments (page count, file id, type) into
  frame slots `,B 135..144`.
- 105622-105660B and 105721-105776B: the case dispatch - it compares `,B 123`
  against 3 / 2 (`SAT 3` / `SAT 2` at 105630B, 105722B, 105736B) to pick the
  create-contiguous vs create-indexed vs expand vs set-access arm, then calls the
  underlying object-create / page-reserve helpers through the frame pool at
  106023-106036B (that pool holds the addresses the arms `JPL I` into).

So **create-contiguous (`CRALF`) and create-indexed (`CRALN`) are the same code**
selected by two status bits; contiguous ends up in `RSPAG`'s downward range
reserve, indexed sets up an index block and grows page-by-page via `GPAGE`.
(VERIFIED: the four entries, the flag fork, the case index build, and the arm
selection; INFERRED: the exact target of each pool pointer.)

`EXPFI` is MON 231 (ExpandFile) - see
[`231B-ExpandFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/231B-ExpandFile/README.md);
`CRALN`/`CRALF` are the create-and-allocate targets reached from MON 221
(CreateFile,
[`221B-CreateFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/221B-CreateFile/README.md))
and `@CREATE-FILE`/`@ALLOCATE-FILE`.

---

## 7. Object-entry creation - `CROBJ` 63726B (VERIFIED spine)

Creating a file also creates its **object entry** (the 64-byte file record).
`CROBJ` (63726B):

- Frame `SAB 51`; validates the target object-file slot (63745B `BSKP ZRO 40 DA`,
  63753B `BSKP ONE 30 DA` check "in use"/type bits; error 70₈ if taken).
- 63757-63767B: stamps constants into the new entry (`SAA 20; STA ,X ,B 23` sets
  the header/in-use word; `SAA 4; STA ,X ,B 33`; `SAA 5; STA ,X ,B 35` set type/
  flag fields - the object-entry offsets 23/33/35₈ within the record frame).
- 63770-64011B: fills owner/name/version fields from the caller frame.
- 64012-64071B: walks the object file to place the entry, then writes it back
  (`WOBJE` path).

(VERIFIED: the prologue, the in-use/type stamps, the slot search; INFERRED: the
exact mapping of each stamped word to a documented object-entry byte offset -
cross-check against the Phase-2 object-entry decode in
[`../on-disk-format/`](../on-disk-format/README.md).)

`DLOBJ` 64146B is the mirror (delete object entry); it clears the record and is
reached from `MDLFI` (delete file).

---

## 8. User allocation & quota - `INSUS` / `CHNUS` / `CUSED` (INFERRED + one VERIFIED link)

- **`CUSED` 55206B** - change pages-used: the quota counter `GPAGE` bumps when a
  page is allocated and the delete path decrements. VERIFIED that `GPAGE`
  references `CUSED` (its pool word 76770B = 55206B); the field it updates is the
  user entry's "pages used" (offset 28-31, see README §4.3). The internal
  arithmetic of `CUSED` is INFERRED.
- **`INSUS` 62314B / `CHNUS` 62206B** - insert / change a user entry (create
  account, set quota/friends). Role INFERRED from the symbol names and the user-
  entry layout; not byte-traced here. New-user page reservation for the account's
  home directory presence would run through the same `ALPAG`/`RSPAG` primitives.
- **`RUSER` 53246B / `WUSER` 53410B** - read / write a user entry; the persistence
  side of the above. INFERRED.

> OPEN: the full new-user allocation path (`@CREATE-USER` -> `INSUS` -> which
> pages get claimed) was not byte-traced; it is routed to a later pass. The quota
> hook (`CUSED` called from `GPAGE`) is the one VERIFIED allocation<->user link.

---

## 9. Free / delete path

- **`RLPAG` 50635B** marks one page free (§2).
- **`DLPAG` 60151B / `DLSPA` 60147B / `DFPAG` 74510B** release a file's pages
  (called from the delete path). Roles INFERRED.
- **`DELPG` 110472B** (MON 272 DeletePage) deletes the pages of an open file
  between two page numbers and frees their bits - see
  [`272B-DeletePage`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/272B-DeletePage/README.md)
  and [`file-io.md`](file-io.md) §5.
- **`MDLFI` 106063B** (MON 54 DeleteFile) deletes the whole file: releases all
  pages, deletes the index blocks, then `DLOBJ` clears the object entry. Shares
  the dispatcher body at 106065B with rename/set-temp/set-perm (see
  [`file-io.md`](file-io.md) §5).

---

## 10. Summary of resolved / open items

| Item | Result | Evidence |
|------|--------|----------|
| Get-page primitive | **`GPAGE` 76205B** (routes through `RSPAG` + `CUSED`) | VERIFIED (pool words 76770/76771B) |
| Free-page primitive | **`RLPAG` 50635B** (`ALPAG`/`XRLPA` siblings) | VERIFIED |
| Range reserve (contiguous) | **`RSPAG` 51120B** -> `TESTP` | VERIFIED |
| Free-page scanner | **`TESTP` 51355B** | VERIFIED |
| **Search direction** | **HIGH -> LOW (downward from top to block-7 floor)** | VERIFIED (`AAX -1` at 51372B/51401B) |
| Doc vs NDFS | Real code matches the **doc** ("highest range"), **not** NDFS's upward `find_free` | VERIFIED |
| Page/word scale | page = 1024 words (`SHA ZIN 12`) | VERIFIED |
| Create dispatcher | **105564B**, (SSM,SSK) selects contiguous/indexed/expand/access | VERIFIED |
| Object-entry create | **`CROBJ` 63726B** | VERIFIED spine |
| Quota hook | **`CUSED` 55206B** called by `GPAGE` | VERIFIED |
| In-word bit order | not pinned | OPEN |
| New-user allocation path | not byte-traced | OPEN |

---

**Last updated:** Phase 5. Evidence base: carved `006-S3FS` SINTRAN L bytes,
ND-60.128.5 docs, NDFS C library cross-check.
