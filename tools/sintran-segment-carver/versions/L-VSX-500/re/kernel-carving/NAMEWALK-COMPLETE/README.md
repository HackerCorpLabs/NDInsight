# NAMEWALK-COMPLETE: the directory-entry + name-walk machinery (L07, 006-S3FS)

Byte-verified carve of the **complete name-resolution machinery** that
`@ENTER-DIRECTORY` uses: how a directory spec is parsed and how an object name
is matched to an on-disk directory-entry index. This carves COLDE's name-walker
callees **fully** - every path.

- Disassembly source: `../../segments-ref/006-S3FS/006-S3FS.asm`
- Binary: `../../../segments/006-S3FS.bin` (load base `26000B`, big-endian)
- sha256(006-S3FS.bin) = `b4a563d0715a6304a35167ec5ff0090c92b0b99dd743aef159ac6b182e080585`
- Byte offset (decimal) of any address `A` = `(A_oct - 26000B) * 2`.
- Companion files: `NAMEWALK-COMPLETE.ASM`, `NAMEWALK-COMPLETE.pseudo.c`.

All addresses/values are **OCTAL**. Every claim tagged **VERIFIED** (read from
the bytes here), **INFERRED**, or **OPEN**.

This folder does **not** re-carve COLDE's entry split (that is in
`../COLDE-CONNECT/`) nor the block-0 device read (`CHDSI->RXDIR->RCBLO`, also in
`../COLDE-CONNECT/`). It carves the **callees** COLDE reaches while resolving
the name.

---

## 1. Headline result

The whole name-walk family issues **NO device transfer**. It is pure in-core
walking over two per-directory tables (VERIFIED - every callee's body only
touches tables and resident helpers, never a driver vector):

| Table | 2-word entry accessor (read) | write / default siblings |
|-------|------------------------------|--------------------------|
| **DIRECTORY** table | `GDIRT 050124B` | `PDIRT 050132B`, `GDDRT 050121B`, `PDDRT 050127B` |
| **NAME** table | `GNAMT 050223B` | `PNAMT 050231B`, `GDNMT 050220B`, `PDNMT 050226B` |

The block-0 read that the SCSI mount is missing is **not** in this machinery; it
is `CHDSI -> RXDIR -> RCBLO -> driver` and the read-vs-fn42 fork lives in the
SCSI disk driver (065-S3SIPIT). See `../COLDE-CONNECT/`.

---

## 2. Full call graph (VERIFIED from resolved pointer words)

```
ENDIR 140176B  (@ENTER-DIRECTORY top; see ../ENTER-DIRECTORY/)
  |
  |-- CLPAR   044777B   parse/classify the spec's leading letter(s)
  |     |-- 003752B     resident enter/setup
  |     `-- GTTCH 030070B   get one spec character   (called x2)
  |
  |-- COLDE   132072B   cold-enter: DRIVE the name walk (NO device I/O)
  |     |-- 003752B     resident enter/setup
  |     |-- CLPAR  044777B   classify parameter
  |     |-- GDIRT  050124B   read directory-table entry (loop)
  |     |-- GNAMT  050223B   read name-table entry
  |     |-- 004735B    resident helper (COLDE-only; role OPEN)
  |     |-- GNAMI  047536B   pack ASCII name + probe name table
  |     |     `-- GNAMT 050223B   fetch name words
  |     `-- GNAMA  030235B   name-entry address arithmetic (leaf)
  |
  `-- GDIRE   131732B   resolve NAME -> directory-entry index (hash-chain walk)
        |-- 003752B     resident enter/setup
        |-- GNAMT  050223B   walk the NAME-table hash chain
        `-- GDIRT  050124B   read candidate DIRECTORY entries + confirm match

Shared leaves used by the accessors:
  GDIRA 030225B   directory-entry address arithmetic (leaf; byte-identical to GNAMA)
  GNAMA 030235B   name-entry address arithmetic       (leaf)
  GNEXM 050025B   scan directory table for NEXT matching entry (via GDIRT)
```

Pointer-word resolutions (the JPL/JMP indirection targets):

```
CLPAR : [045121]=003752 setup   [045122]=030070 GTTCH
        [045123]=040730 token-handler   [045124]=045650 return
GNAMI : [047643]=003752 setup   [047646]=050223 GNAMT
GDIRT-body : [050212]=003752 setup
GNEXM : [050112]=003752 setup   [050117]=050124 GDIRT
GDIRE : [132044]=003752 setup   [132046]=050223 GNAMT   [132052]=050124 GDIRT
```

---

## 3. What each helper does, and its paths

### CLPAR `044777B` - parse/classify the spec parameter
Reads **two characters** through `GTTCH (030070B)`, then runs a decision tree
matching them against **ASCII letters** {A B D F I L O P S U X} (octal
101,102,104,106,111,114,117,120,123,125,130). A recognised token jumps to the
accepted-token handler (`[045123]=040730`); an unrecognised one rejects at
`045125B`.

- Paths: **recognised token** -> handler -> return; **reject** -> `045125B`.
- VERIFIED: it calls GTTCH and compares ASCII letters (bytes).
- **Naming discrepancy (OPEN):** `../COLDE-CONNECT/` labelled CLPAR "clear
  parameter block". The bytes show a **character parser**, not a memory clear.
  The exact grammar it accepts is OPEN, but it is definitively a parser.

### GDIRT `050124B` / GNAMT `050223B` - 2-word table-entry accessors
Each is the **read/user** stub of a 4-way `BSET`-split family that falls into
one shared body (exactly the RDISK/WDISK and COLDE/DCOLD/XCOLD idiom):

```
SSM=1 SSK=0  get DEFAULT entry   (GDDRT 050121 / GDNMT 050220)
SSM=0 SSK=0  get USER    entry   (GDIRT 050124 / GNAMT 050223)   <- walk reads
SSM=1 SSK=1  put DEFAULT entry   (PDDRT 050127 / PDNMT 050226)
SSM=0 SSK=1  put USER    entry   (PDIRT 050132 / PNAMT 050231)
```

Shared directory body `050134B`: `enter_setup`; compute entry address
`(index+1)*W + base`; range-check against upper/lower bounds (error status
`174B` below-range); then copy **exactly two words** either table->params (GET)
or params->table (PUT), selected by `SSK`. Name body `050233B` is
structurally identical.

- Paths: GET (SSK=0), PUT (SSK=1); DEFAULT (SSM=1) vs USER (SSM=0); plus the
  above/below range-error arms.
- VERIFIED: BSET flag ordering, 2-word copy, range checks, `(index+1)*W+base`.
- INFERRED: SSM = default-vs-user (from the `G*D*` / `G*I*` naming).

### GDIRA `030225B` / GNAMA `030235B` - leaf entry-address calculators
6-word **leaf** routines (no stack frame). Given base pointer in A and entry
index in T, compute `entry_addr = (index+1)*W` and exchange into X. They are
**byte-identical** (VERIFIED) - the directory table and name table share the
same calculator and stride.

- VERIFIED: `(index+1)*W` shape (MPY and ADD use the same operand), and that
  GDIRA == GNAMA byte-for-byte.
- OPEN: the source of the stride operand `W` (P-relative constant vs caller
  B-relative) - not resolvable from these 6 words alone.

### GNAMI `047536B` - build/search the name index
Packs an ASCII object name (loop bounded at 7 chars) into name-table word form,
then probes the NAME table (reading slots via `GNAMT`) with a **3-way** result
dispatch and returns the resolved slot:

- Paths: **free slot** (`047600B`, advance buffer +16B, retry/insert; table-full
  check at `047606B`, statuses `30B`/`31B`); **match** (`047627B`); **grow**
  (`047633B`); **error arm** (`047640B`).
- VERIFIED: char-pack loop, GNAMT probe, 3-way dispatch, statuses.
- INFERRED: exact packed-word field layout.

### GDIRE `131732B` - GET DIRECTORY ENTRY BY NAME (the name-matcher)
The core name->entry resolver. Reads the NAME-table chain head via `GNAMT`,
then **hash-walks a chain**: for each cursor it reads directory entry words via
`GDIRT`, skips empty slots, strips a flag bit from the target name, folds the
entry's hash bits and does a **primary** compare then a **secondary masked**
compare. On full match it re-reads the entry (`GDIRT`), classifies the
entry-type bits, and returns the matched index. Chain advance adds `30B` to the
hash bucket and `++cursor`.

- Paths: **full match** -> classify + return index (`132015..132040B`);
  **primary mismatch** / **empty slot** -> advance (`132004B`); **chain
  exhausted** -> status `33B` "not found" (`132011B`); **table-read error** arm
  (`132041B`).
- VERIFIED: GNAMT/GDIRT calls, hash-chain loop, dual compare, not-found `33B`.
- INFERRED: precise hash-fold semantics and the entry-type classification codes
  (`147B`/`34B`/`26B`).

### GNEXM `050025B` - scan for next matching directory entry
Linear scan of the directory table (entry stride `55B`) via `GDIRT`, unsigned
key compare, until match; status `43B` on not-found. Part of the same family,
used for enumeration rather than COLDE's primary lookup.

---

## 4. How a directory name is parsed and matched (end to end)

1. **Parse.** `CLPAR` tokenises the spec's leading letter(s) via `GTTCH`,
   recognising a small keyword/type-code set (VERIFIED it is a parser).
2. **Pack.** `GNAMI` turns the ASCII object name into the packed name-table
   word form, probing the NAME table (via `GNAMT`) for a slot.
3. **Match.** `GDIRE` hash-walks the NAME table (`GNAMT`) and cross-checks the
   DIRECTORY table (`GDIRT`) until the matching **directory-entry index** is
   found, or returns `33B` = not found.
4. Underneath, `GDIRT`/`GNAMT` (with their default/put siblings) are the shared
   2-word entry accessors, and `GDIRA`/`GNAMA` are the shared leaf address
   calculators.

Only **after** the name is resolved does `ENDIR` reach the single block-0
device read (`CHDSI->RXDIR->RCBLO`, carved in `../COLDE-CONNECT/`). Nothing in
this name-walk touches a device.

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Status |
|---|-------|--------|
| 1 | GDIRT/GNAMT are the read/user stubs of 4-way `BSET` splits (SSM,SSK) into one shared body | VERIFIED |
| 2 | Shared body copies exactly 2 words, GET vs PUT by SSK, with range checks (`174B` below-range) | VERIFIED |
| 3 | Entry address = `(index+1)*W + base` (dir body) / `(index+1)*55B` (GNEXM) | VERIFIED |
| 4 | GDIRA and GNAMA are 6-word leaves, byte-identical, compute `(index+1)*W` | VERIFIED |
| 5 | GDIRE resolves NAME->entry index by hash-walking NAME(GNAMT)+DIRECTORY(GDIRT) tables | VERIFIED |
| 6 | GDIRE "not found" == status `33B`; classifies matched entry type before return | VERIFIED |
| 7 | CLPAR reads 2 chars via GTTCH and matches ASCII letters {A,B,D,F,I,L,O,P,S,U,X} | VERIFIED |
| 8 | GNAMI packs the ASCII name (<=7 chars) + 3-way name-table probe (free/match/grow), via GNAMT | VERIFIED |
| 9 | Whole family issues NO device transfer (only tables + resident helpers) | VERIFIED |
| 10 | SSM selects DEFAULT vs USER table | INFERRED (from `G*D*`/`G*I*` naming) |
| 11 | CLPAR's exact accepted grammar / the token set semantics | OPEN |
| 12 | Source of GDIRA/GNAMA stride operand `W` (P-rel constant vs B-rel) | OPEN |
| 13 | GNAMI packed-word field layout; GDIRE hash-fold + type-code meanings (`147B`/`34B`/`26B`) | INFERRED / OPEN |
| 14 | resident `004735B` (COLDE-only helper) role; `003752B` = common enter/setup | OPEN / INFERRED |
| 15 | CLPAR = "clear parameter block" (prior label) vs parser (bytes) | CONTRADICTED - it is a parser |

---

## 6. dd byte proofs (big-endian .bin)

All reproduced with
`dd if=006-S3FS.bin bs=1 skip=<off> count=2 | od -An -tx1`:

```
CLPAR  044777B  off 15358 = 22 51   (021121 STD I 121)
GNAMI  047536B  off 18108 = 22 44   (021104 STD I 104)
GDIRT  050124B  off 18600 = f8 38   (174070 BSET ZRO SSM)
GNAMT  050223B  off 18726 = f8 38   (174070 BSET ZRO SSM)
GDIRA  030225B  off  2346 = cc 6f   (146157 RADD CLD SA DX)
GNAMA  030235B  off  2362 = cc 6f   (146157 RADD CLD SA DX)
GDIRE  131732B  off 69556 = 22 49   (021111 STD I 111)
```

Each opcode word matches the disassembly in
`../../segments-ref/006-S3FS/006-S3FS.asm` at the same address. GDIRA and GNAMA
share the same first word (`cc 6f`), consistent with the VERIFIED byte-identity
of the two leaves.

---

## 7. Cross-links

- `../COLDE-CONNECT/` - COLDE/DCOLD/XCOLD entry split; the block-0 read path
  `CHDSI->RXDIR->RCBLO->driver` (this folder carves COLDE's name-walker callees).
- `../ENTER-DIRECTORY/` - ENDIR top level, MON 124B PRSRV, the device seam.
- `../FUNCTION-42-RETURN/`, `../SCSDISK-TRANSFER/`, `../SCSI-DRIVER/`,
  `../SMD-DRIVER-BASELINE/`, `../RCBLO/` - the device-driver half (out of the
  name walk).
- Symbols: `../../segments-ref/006-S3FS/006-S3FS.symbols.txt` (FILSYS-SYMBOLS).
