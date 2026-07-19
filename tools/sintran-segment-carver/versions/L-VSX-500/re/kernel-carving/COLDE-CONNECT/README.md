# COLDE-CONNECT: cold-enter / device-connect carve (L07, segment 006-S3FS)

Byte-verified carve of the `@ENTER-DIRECTORY` cold-enter path, answering "where
does the SCSI mount fail to issue the block-0 read after the connect succeeds?"

- Disassembly source: `../../segments-ref/006-S3FS/006-S3FS.asm`
- Binary: `../../../segments/006-S3FS.bin` (load base `26000B`, big-endian)
- Byte offset (decimal) of any address `A` = `(A_oct - 26000B) * 2`.

All addresses/values are OCTAL. Every claim is tagged VERIFIED (read from the
bytes) or INFERRED or OPEN.

---

## 1. Headline result

**The block-0-read decision is NOT in segment 006-S3FS (FILSYS).**

- `COLDE` is a **directory-tree / name-match walker**. It issues **no device
  transfer** at all. (VERIFIED: its entire call set is directory/name helpers.)
- The directory **block-0 read is issued exactly once** by
  `CHDSI -> RXDIR -> RCBLO -> per-device driver vector`, and FILSYS **never
  retries** it. (VERIFIED.)
- There is **no device-type / SUTYP / 5SCIN branch in FILSYS** that gates the
  read. The read is issued unconditionally (guarded only by a resource-reserve
  bit-test in `CHDSI` at `037773B`, which is not about device type). (VERIFIED.)
- Therefore the read-vs-`fn42`-init fork lives in the **SCSI disk driver
  (065-S3SIPIT, SCSI disk layer at NPL-label + 376B)**, on the test
  `if (SUTYP.5SCIN clear)`. Whether that init path **falls through to the fn-0
  read** or **returns** is **OPEN from static FILSYS bytes** and is the one
  thing that settles the bug. (See section 6.)

Mechanism classification: **(A) "connect consumes the single transfer, read
never retried."** FILSYS issues one block-0 transfer; if the driver services it
as `fn-42` init and returns success, `CHDSI` sees success-but-bad-header and
takes its re-init/error arm (`040063B`) which does **not** re-issue `RXDIR`.
It is **not** the "two-step, second step skipped" shape, because FILSYS has no
explicit separate connect step. (VERIFIED for the FILSYS half; the driver half
is the OPEN item.)

---

## 2. Call graph (routines + addresses, VERIFIED from pointer words)

```
ENDIR 140176B  (@ENTER-DIRECTORY top)
  |-- 140202B  enter-setup            [140227]=003752B  (resident, ubiquitous)
  |-- 140211B  CLPAR      044777B     clear parameter block
  |-- 140213B  COLDE      132072B  ---+   cold-enter (name lookup) -- NO device I/O
  |               |-- 132077B  enter-setup  003752B
  |               |-- 132112B  CLPAR        044777B
  |               |-- 132141B  GDIRT        050124B   get directory entry (loop)
  |               |-- 132171B  GNAMT        050223B   name table
  |               |-- 132402B  helper       004735B   (resident)
  |               |-- 132403B  GNAMI        047536B   name init
  |               |-- 132406B  GNAMA        030235B   get name
  |-- 140222B  GDIRE      131732B     get directory entry
  |-- 140252B  PRSRV  (MON 124B)      the ONE MON call; JAP@140253B => not the abort
  |-- 140402B  CHDSI      037763B  ---+   check disk info == BLOCK-0 READ PATH
                  |-- 037767B  enter-setup  003752B
                  |-- 037773B  BSKP ZRO 100 DA   guard = resource-reserve bit (NOT device type)
                  |-- 040000B  RXDIR    037643B ---+  read directory block 0
                  |               |-- 037647B  enter-setup 003752B
                  |               |-- 037650-037651B  block := 0   (RADD CLD 0 DA/DD)
                  |               |-- 037652B  RCBLO   035766B ---+  issue transfer
                  |                               |-- 035772B enter-setup 003752B
                  |                               |-- 035773B rdpage      037101B (resident)
                  |                               |-- 035777B GDIRT       050124B  -> descriptor
                  |                               |-- 036101-036102B  driver := desc[14] (ABFUN vector)
                  |                               |-- 036135B  JPL I ,B 10  ==> DRIVER (one dispatch, no retry)
                  |                                            (variant sites 036142/036167/036174B)
                  |-- 040017-040021B  validate header; mismatch/zero -> 040063B re-init (NO re-read)
```

Order of the device-relevant calls is fixed by the bytes:
`COLDE (140213B) -> GDIRE (140222B) -> CHDSI (140402B)`.

---

## 3. Where the "connect" (SCSI function 42) is triggered

There is **no explicit FILSYS connect call**. The only device transfer in the
whole mount is `CHDSI -> RXDIR -> RCBLO`'s single dispatch through `desc[14]`
(`JPL I ,B 10` at `036135B`). On SMD/Winchester that dispatch reads block 0
directly (fn-42 is a no-op there -- see `../SMD-DRIVER-BASELINE/`). On SCSI the
same dispatch, with `SUTYP.5SCIN` clear, is diverted by the driver into fn-42
(INQUIRY / READ-CAPACITY / control-record). So **the connect and the block-0
read are the same single FILSYS transfer**, and the driver chooses which one
happens. (INFERRED for the driver divert -- from the prior driver carves; the
FILSYS single-dispatch/no-retry is VERIFIED here.)

`COLDE` also calls a resident helper `004735B`; whether it performs any device
reserve/connect is OPEN (out-of-segment), but it issues no FILSYS-visible
transfer and cannot be the block-0 read.

---

## 4. The decision instruction

- In FILSYS: **none.** The read at `RXDIR (040000B in CHDSI)` is unconditional.
- The real fork is the driver test `SUTYP.5SCIN clear?` in 065-S3SIPIT (SCSI
  disk layer, NPL-label + 376B). Its two arms:
  - fall through to the fn-0 enqueue  => block 0 gets read (mount would work),
  - return after fn-42 init           => block-0 read is lost (observed bug).

**Which arm the SCSI init path takes is undetermined from static 006-S3FS
bytes.** It requires carving 065-S3SIPIT or the DAP check in section 6.

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Status |
|---|-------|--------|
| 1 | `COLDE`/`DCOLD`/`XCOLD` are the RDISK/WDISK-style read/write split; `COLDE` sets `SSK:=0` (read) | VERIFIED (132070-132106B) |
| 2 | `COLDE` issues NO device transfer; call set = {003752,004735,CLPAR,GDIRT,GNAMI,GNAMA,GNAMT} | VERIFIED (pointer words resolved) |
| 3 | `ENDIR` calls `COLDE`(140213B) then `GDIRE`(140222B) then `CHDSI`(140402B) | VERIFIED |
| 4 | Block-0 read = `CHDSI->RXDIR->RCBLO`; `RXDIR` sets block:=0 at 037650-037651B | VERIFIED |
| 5 | `RCBLO` dispatches the driver via `JPL I ,B 10` (desc[14]) once, no retry loop | VERIFIED (036135B + 036136B return) |
| 6 | `CHDSI` bad-header arm (040063B) re-inits the descriptor, does NOT re-issue `RXDIR` | VERIFIED |
| 7 | No device-type/SUTYP/5SCIN branch gates the read inside FILSYS | VERIFIED (read path is unconditional) |
| 8 | On SCSI the single dispatch is diverted to fn-42 when 5SCIN is clear | INFERRED (from FUNCTION-42-RETURN / SCSDISK-TRANSFER carves) |
| 9 | Whether the driver fn-42 path falls through to the fn-0 read or returns | OPEN (in 065-S3SIPIT, not FILSYS) |
| 10 | Resident `004735B` (COLDE-only) role; resident `003752B` = common enter/setup | INFERRED (003752B ubiquitous prologue); 004735B OPEN |

---

## 6. The one settling check

Break the SCSI mount and confirm, in order:

1. `RCBLO` driver dispatch `036135B` executes **once** for block 0
   (confirms single-issue). Inspect the descriptor `desc[14]` vector and the
   `fn`/`ABFUN` and `SUTYP.5SCIN` (desc offset 23, bit 7) passed in.
2. Confirm `CHDSI`/`RXDIR`/`RCBLO` are **not re-entered** for block 0
   (confirms no FILSYS retry).
3. Then break **inside the SCSI disk driver (065-S3SIPIT, NPL-label + 376B)**
   at the `SUTYP.5SCIN` test and single-step: does the fn-42 init path
   **fall through to the fn-0 read enqueue** (`SCLLD`) or **return**?
   That branch is the whole bug.

DAP note: `COLDE` entry is `132072B` (`BSET ZRO SSK`, bytes `F8 10`); but
`COLDE` is the wrong place to break for the read -- break `RCBLO` `036135B` and
the driver instead.

---

## 7. dd byte proofs (big-endian .bin)

```
COLDE  132072B  off 69748  = F8 10   (174020 BSET ZRO SSK)
       132073B  off 69750  = 22 62   (021142 STD I 142)
RXDIR->RCBLO 037652B off 10068 = BA 13 (135023 JPL I 23 -> RCBLO)
RCBLO JPL I,B 10 036135B off 8378 = BB 08 (135410)
CHDSI->RXDIR 040000B off 10240 = BA 63 (135143 JPL I 143 -> RXDIR)
ENDIR->COLDE 140213B off 76054 = BA 13 (135023 JPL I 23 -> COLDE)
ptr [140236]=COLDE   off 76092 = B4 3A (132072)
ptr [040143]=RXDIR   off 10438 = 3F A3 (037643)
```
All reproduced with `dd if=006-S3FS.bin bs=1 skip=<off> count=2 | od -An -tx1`.

---

## 8. Cross-links (other kernel-carving folders)

- `../ENTER-DIRECTORY/` -- ENDIR / single MON 124B PRSRV; PRSRV is not the abort.
- `../FUNCTION-42-RETURN/` -- SCSI fn-42 returns success, leaves 5SCIN set.
- `../SCSDISK-TRANSFER/` -- fn-0 block-0 request WITH 5SCIN set reaches SCLLD enqueue.
- `../SCSI-DRIVER/`, `../SMD-DRIVER-BASELINE/` -- SMD reads block 0 directly (fn-42 no-op).
- `../RCBLO/` -- the read-cache-block driver-dispatch routine.
- `../PRSRV-124B/` -- the MON 124B force-reserve call.
