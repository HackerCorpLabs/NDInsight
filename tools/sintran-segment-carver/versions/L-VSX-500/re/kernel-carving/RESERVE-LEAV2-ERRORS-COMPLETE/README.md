# RESERVE / LEAV2 / ENTER-DIRECTORY error paths - complete carve

Closes the last two gaps in the `@ENTER-DIRECTORY` carves for SINTRAN III
VSX/500 **L07** (running system L-VSX-500, symbols L07):

- **PART 1** - the resource reserve/release family and **PRSRV's uncarved jump
  target `LEAV2 = 027417B`**.
- **PART 2** - the error-code / error-message emission paths the mount can hit.

**Evidence grades.** VERIFIED = re-read from the carved `.bin` with `dd`
(byte offset in the `.bin` = `(addr_oct - loadbase) * 2`, decimal) and/or
`nd100-dis`. INFERRED = reasoned from bytes + manual/symbols. OPEN = crosses
into a foreign/low-resident overlay whose live mapping is a runtime fact.
All addresses and values are **octal**.

Deliverables:
- [`RESERVE-LEAV2-ERRORS-COMPLETE.ASM`](RESERVE-LEAV2-ERRORS-COMPLETE.ASM)
- [`RESERVE-LEAV2-ERRORS-COMPLETE.pseudo.c`](RESERVE-LEAV2-ERRORS-COMPLETE.pseudo.c)

Cross-links:
[`../PRSRV-124B/`](../PRSRV-124B/) (the PRSRV trampoline + block-0 reserve flow),
[`../ENTER-DIRECTORY/`](../ENTER-DIRECTORY/) (device-agnostic mount path),
[`../RCBLO/`](../RCBLO/) (block-0 dispatch seam),
[`../../segments-ref/003-S3CP/003-S3CP.asm`](../../segments-ref/003-S3CP/003-S3CP.asm).

---

## PART 1 - the reserve family and LEAV2

### 1.1 Dispatch (re-confirmed by dd on `044-S3IDPIT.bin`, MCTAB @ 005620B)

`MON N -> ENT14 -> GOTAB[N]=MFELL -> CALLP -> MCTAB[N] -> worker`. The four
reserve/release slots land exactly on the L07 SYMBOL-1-LIST addresses:

| MON | Symbol | worker | note |
|-----|--------|--------|------|
| 122B | `RESRV` | `037103B` | ReserveResource |
| 123B | `RELES` | `037156B` | ReleaseResource |
| 124B | `PRSRV` | `037076B` | **ForceReserve** (the mount's `MON 124`) |
| 125B | `PRLS`  | `037147B` | ForceRelease |
| 070B | `COMSB` | `050673B` | @-command interpreter (called by EXECC) |

Bodies are resident-monitor code, **byte-identical** in `071-S3SM`, `070-S3SSM`
and `003-S3CP` (all load base `30000B`). dd re-reads (`071-S3SM.bin`):
`PRSRV[037076]=044322`, `const[037020]=000003`, `PRSRV ptr[037107]=027417`,
`RESRV[037103]=036776`, `EXECC[037110]=045053` - **all VERIFIED**.

### 1.2 Two shapes in the family

- **PRSRV (124B) is a 2-word trampoline** (`LDA -56` -> A:=3 ; `JMP I 10` ->
  `027417 = LEAV2`). It never enters the reserve executor. (Carved in
  `../PRSRV-124B/`; reproduced here as the entry to LEAV2.)
- **RESRV / PRLS / RELES funnel into the shared executor `EXECC = 037110B`**,
  which **builds a command line and runs it via `MON 70B` (COMSB)** - i.e. a
  plain reserve/release is implemented as an internally-synthesised
  `RESERVE ..` / `RELEASE ..` command. Both `MON 70B` sites (037124, 037154)
  are VERIFIED. **The only MON call the reserve family issues is `MON 70B`.**

### 1.3 LEAV2 = 027417B - what it does, and what A=3 does

`027417B` is **below** the System Monitor's `30000B` load base, so it is not in
`071-S3SM`. Using the skill's sibling-coherence discriminator on the
monitor-leave family

```
3ENTE=027346  L3EAV=027371  LEAVX=027404  LEAV2=027417  3LEAV=027430
```

the **only** overlays where all five land on **parallel entries** are
**`013-S3SCP` and `041-S3IMED`** (byte-identical, load base `26000B`). In
`006-S3FS`/`012-S3SFS` those addresses land mid-instruction-stream (027417 there
= `004007 STA 7`); in `022-S3RFAC` mid-loop - both **rejected** as incoherent.
dd (`013-S3SCP.bin` byteoff 1566) = `030625 044662 172765 004662` - **VERIFIED**,
and `041-S3IMED.bin` identical.

**LEAV2 is a "pop one context frame and leave" routine** - one member of a
parallel family. It:
1. saves the caller's F (`027417 STF ,B -153`),
2. **loads the frame-stack pointer into A (`027420 LDA ,B -116`) - overwriting
   the A=3 that PRSRV passed in**,
3. descends it by one 11-word (`013B`) frame (`027421 AAA -13`),
4. bounds-checks (`027424 SUB 36` ; `027425 JAN -> 027452` underflow/refill),
5. joins the common tail (`027437`) that restores F/D from the popped frame,
   sets the return link `L := frame[0]` (`027447`), and `EXIT`s (`027451`).

The family entries differ only in the `SUB` immediate (L3EAV 63, LEAVX 50,
**LEAV2 36**, 3LEAV 25) = which frame level to leave to.

**Does PRSRV reserve anything, or just return?** Under this overlay:
**A=3 is discarded at `027420` and is not the value in A at the EXIT, so PRSRV =
"set A=3, then monitor-leave/return" - it runs no reserve executor and raises no
`147B`.** This is exactly what the live console shows on the failing SCSI mount:
no `147B`, and the mount proceeds past `MON 124`.

**Honest caveat (OPEN).** Whether `013-S3SCP` is the overlay *actually mapped*
at `027417` when the System Monitor runs PRSRV is a **runtime** fact not
decidable from static bytes (the coherence test identifies the best-fit carve,
not the live page mapping). So whether the `3` is a vestigial constant (this
overlay) or a live resource-type selector consumed by a reserve primitive in a
*different* co-resident overlay is **OPEN**. What is firmly anchored regardless:
**MON 124 PRSRV returns success on every device type, SCSI included** (bytes +
live console). A live nd100x/DAP break at `027417` with the monitor mapping in
place would settle which overlay is live.

---

## PART 2 - the ENTER-DIRECTORY error paths

Segment `006-S3FS` (base `26000B`), worker `ENDIR = 140176B`.

### 2.1 The FILSYS error-return convention (VERIFIED skeleton)

SINTRAN filesystem workers use a **skip-return**:

- **Success**: the worker executes `MIN ,B 4` (set the OK marker; seen at
  140722 / 037666 / 037743) and returns through the resident epilogue
  **`SPOP = 003776`**, which takes the **L+1 (skip)** return - the caller's
  instruction *after* its "`JMP I <err>`" error return.
- **Error**: the worker does `SAA <code> ; JMP I <ptr>` to a per-site error
  exit. The exit does **`STA ,B 2`** (park the code in frame slot `,B 2`), runs
  site cleanup, then joins the common tail (`140723`/`140726`) which returns
  through `SPOP` **without** the `,B 4` marker -> the **L+0 (non-skip)** error
  return, so the caller falls into its `JMP I <err>` and propagates the code
  upward (in A / `,B 2`).

Cleanup on the way out: release the directory lock via **`UNLOC = 010506`**
(`[140741]=010506`, `[141037]=010506`, VERIFIED symbol), and - if the unit had
been reserved - **`MON 125` ForceRelease** at `141014` (VERIFIED `153125`) to
un-reserve it. `SPUSH = 003752` / `SPOP = 003776` are the resident frame
push/pop; `LOCK/XLOCK = 010500`, `UNLOC/XUNLO = 010506` the lock/unlock
primitives (all named from L07 symbols).

### 2.2 The ENTER-DIRECTORY error-code table (VERIFIED codes + meanings)

Meanings from `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`.

| Code | Meaning (manual text) | Raised at | Why in the mount | Evidence |
|------|-----------------------|-----------|------------------|----------|
| **147B** (103) | Device unit reserved for special use - "Device must be reserved for special use to be accessed the way you tried." | `140254 SAA 147` after `MON 124` returns A<0 | the unit ForceReserve was refused | VERIFIED dd (`f167`) |
| **145B** (101) | Illegal on tape device - "This function is not allowed on magnetic tape." | `140261 SAA 145` after a device-type-bit test | ENTER-DIRECTORY on a mag-tape unit | VERIFIED dd (`f165`); bit test INFERRED |
| **42B** (34) | Main directory not last one released - "Attempt to release the main directory while some other directory is still entered." | `140315 SAA 42` | main-directory ordering guard hit during the enter/release bookkeeping | VERIFIED dd (`f122`) |
| **32B** (26) | Directory entered - "Attempt to access a directory erroneously while it is entered." | `140370 SAA 32` (already-entered guard after the name match) | the directory is already entered on that unit | VERIFIED dd (`f11a`) |
| **35B** (29) | Master block transfer error - "Unable to read system information on the device." | `037747 SAA 35` in `WXDIR` | page-0 (master block) write-back failed | VERIFIED (`../ENTER-DIRECTORY`) |

All five error exits funnel through `STA ,B 2` -> cleanup -> common tail ->
`SPOP`, delivering the code up to the @-command executor.

### 2.3 How an error code becomes a console message

The @-command executor receives the FILSYS error return and, on its error path,
calls the resident error-message printer **`ERMSG = 016714B`** (`ERMON =
114574B`), which maps the numeric code to its text in the SINTRAN
error-message table and writes it to the terminal as `<message> (<code>B)`.
`ERMSG`'s **body** is in a lower resident overlay (`016714 < 26000B`) and is
**OPEN** here; its **role** is named/known from the L07 symbols. The message
**texts** live in the error-string segments `005-S3ERRS` / `011-S3ERRL` /
`014-S3ERRP` (not carved here).

### 2.4 Red herring - "APPROACHING END OF ACCOUNTING FILE" (243B)

Already established (see `../PRSRV-124B/` sec (e)): error **243B** is set in
exactly one place - `RP-P2-ACCRT.NPL`, the RT-accounting collector that runs at
**logout** - and has **no call edge** from the ENTER-DIRECTORY mount path.
**Not raised by the mount.** Documented here only so it is not re-hunted.

---

## VERIFIED / INFERRED / OPEN

| # | Claim | Verdict |
|---|-------|---------|
| 1 | `PRSRV[037076]=044322` (LDA -56 -> A=3), `[037020]=3`, `[037107]=027417` | VERIFIED (dd) |
| 2 | `027417 = LEAV2`; coherent overlay = `013-S3SCP` == `041-S3IMED` (dd `030625 044662 172765 004662`) | VERIFIED (dd + sibling coherence) |
| 3 | LEAV2 = pop-one-frame/leave; descends framePtr `,B -116` by 11 words, restores F/D, `L:=frame[0]`, EXIT | VERIFIED (dd) |
| 4 | A=3 overwritten at `027420`, unused; so PRSRV = leave/return, no reserve executor, no 147B | VERIFIED (bytes) + live console (no 147B) |
| 5 | Whether `013-S3SCP` is the live overlay at monitor time / whether A=3 is a live selector elsewhere | OPEN (runtime; DAP settles) |
| 6 | `RESRV`/`PRLS`/`RELES` funnel into `EXECC 037110`; issue `MON 70B` (COMSB) x2 | VERIFIED (dd) |
| 7 | FILSYS convention: success `MIN ,B 4` -> SPOP L+1; error `STA ,B 2` -> cleanup -> common tail -> SPOP L+0 | VERIFIED (bytes) |
| 8 | Error exits release dir lock via `UNLOC 010506` and `MON 125` ForceRelease if unit was reserved | VERIFIED (dd `153125` + symbols) |
| 9 | ENDIR codes 147/145/42/32 SAA immediates + manual meanings | VERIFIED (dd all four) |
| 10 | Code reaches console via `ERMSG 016714` (role); body + text tables in lower/error overlays | VERIFIED (symbol/role) / body OPEN |
| 11 | 243B "approaching end of accounting file" not on mount path (RP-P2-ACCRT, logout) | VERIFIED |

**Provenance.** `071-S3SM.bin` (PRSRV/RESRV/EXECC family, base 30000B),
`013-S3SCP.bin` + `041-S3IMED.bin` (LEAV2 family, base 26000B), `006-S3FS.bin`
(ENDIR error exits, base 26000B), `044-S3IDPIT.bin` (MCTAB, base 4000B), all
under `tools/sintran-segment-carver/versions/L-VSX-500/segments/`; symbols
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/{SYMBOL-1-LIST,FILSYS-SYMBOLS}.SYMB.TXT`;
error meanings `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`;
accounting `SINTRAN/NPL-SOURCE/NPL/RP-P2-ACCRT.NPL`. Disassembly via `nd100-dis`
(WSL, little-endian input).

## See also
- [`../PRSRV-124B/README.md`](../PRSRV-124B/README.md) - PRSRV trampoline + block-0 reserve flow.
- [`../ENTER-DIRECTORY/README.md`](../ENTER-DIRECTORY/README.md) - device-agnostic mount path + error raise sites.
- [`../RCBLO/README.md`](../RCBLO/README.md) - block-0 dispatch seam.
