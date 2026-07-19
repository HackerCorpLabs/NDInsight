# Tier-2 MON-call oracle for NC — 123B RELES, 54B MDLFI, 503B DVINST

Resolving three SINTRAN III VSX/500 **L07** MON-call divergences between two emulators,
using carved L07 bytes (and live-recovered L07 code) as the tie-breaker. All addresses and
values are **octal** unless a decimal is named. Every claim below is tagged **VERIFIED**
(byte-proven or live-captured on real SINTRAN L07) or **INFERRED** (structure/semantics imply it).

Source layers used:
- Carved segment listings: `/mnt/d/ND/t/re/segments-ref/<seg>/<seg>.asm` + `.symbols.txt`
- Resident image: `/mnt/d/ND/t/resident/SINTRAN-DATA_commoncode.bin` (base 0)
- Live-recovered reservation code: `../../OS/21-SEMAPHORES-RECOVERED-CODE.md`
- NPL L07 source: `../../NPL-SOURCE/NPL/RP-P2-N500.NPL`
- Existing ND-500 carve: `/mnt/d/ND/t/re/mon-analysis/511B-DVIO/`
- Error-code decode: `../../../Reference-Manuals/ND-60.145.7A EN ND FORTRAN Reference Manual.md`
  (decimal | octal | text) and the Monitor Calls manual appendix A.

Error-code decode (decimal = octal = text), the three that matter:
| dec | oct | meaning |
|-----|-----|---------|
| 5   | 5   | Device not reserved |
| 46  | 56  | No such file name |
| 124 | 174 | Illegal parameter |

---

## Q1 — 123B RELES (ReleaseResource): release a NEVER-reserved device

### VERDICT: **succeeds silently — no error 5.**

Releasing a *valid* device/resource that was never reserved is a designed **idempotent no-op**;
it returns with no error. Error 5 ("Device not reserved") is **not** raised by RELES on this
path — error 5 is raised by the I/O operations that *require* a reservation, not by the release.

### Proof

The RELES handler bytes are **not present in the static carve**: `RELES=37156B` in
`SINTRAN-DATA_commoncode` falls inside a 1024-word zeroed hole (`37040B..41037B` are `000000`
in `SINTRAN-DATA_commoncode.bin`), i.e. the resident monitor image was paged out at carve time.
So RELES cannot be answered from the static segment bytes alone.

It **is** answered from the **live-recovered L07 machine code** in
[`../../OS/21-SEMAPHORES-RECOVERED-CODE.md`](../../OS/21-SEMAPHORES-RECOVERED-CODE.md), captured
with the CPU stopped inside the mapped monitor segment on a running SINTRAN III VSX/500 L system.

The release primitive **BRELEASE (`BRELE = 010610`)** decides the "not reserved" case explicitly:

```
010623  044400  LDA ,B 0     A := DF.RESLI          (reserve-link of the datafield)
010624  131073  JAZ *+73      RESLI = 0  ->  jump to 010717  (restore registers + EXIT)
...
010717  054270  LDX *-110    restore X
010720  034263  LDF *-115    restore T,A,D
010721  125265  JMP I *-113  return via saved L
```

- **VERIFIED (live L07):** when the datafield's `RESLI = 0` (the resource is not reserved),
  `010624 JAZ` branches straight to the register-restore/return at `010717`. No error code is
  loaded; control returns normally. The two error calls in BRELEASE are reached only for
  *inconsistent* or *third-party-owned* datafields, **not** for the plain not-reserved case:
  - `010627 JPL I *+75` — reached only if `RESLI != 0` **and** `RTRES = 0` (chained but ownerless — an internal inconsistency).
  - `010633 JPL I *+71` — reached only if the datafield is owned by a *different* RT program (`X != owner`, `X != 0`).
  A never-reserved datafield has `RESLI = 0` and never reaches either — it exits at `010624`.

- The RELES *monitor wrapper* (`RELES = 037156B`, captured but annotation preliminary in
  chapter 21 §4) loads only **file-oriented** error codes on its captured branches —
  `037174 SAA 132` (=err 90 "No file opened with this number"),
  `037176 SAA 126` (=err 86 "Not opened for random read"),
  `037203 SAA 133` (=err 91 "Not mass storage file") — i.e. parameter-validation errors for the
  *file-number* form of the call. None of them is error 5. The device-release tail (past `037217`,
  not in the capture) performs the actual release via the BRELEASE primitive shown above.

**Emulator guidance:** `MON 123 (RELES)` on a valid device number that the caller has not
reserved must **return success (error code 0)** and leave the reservation state untouched.
Emit an error only for a bad/undefined device or file *parameter* (the 86/90/91 family), never
error 5, and never for the idempotent "already free" case.

**Honest caveat:** BRELEASE's `RESLI=0 -> silent exit` is VERIFIED live; the RELES wrapper's
device tail is not in the capture, but the primitive's *explicit* handling of `RESLI=0` as the
normal return path is the design proof that release-of-unreserved is a no-op, not error 5.

---

## Q2 — 54B MDLFI (DeleteFile): empty vs nonexistent name

### VERDICT: **empty/blank name -> error 124 (Illegal parameter); nonexistent (well-formed) name -> error 46 (No such file name).** The two cases differ.

### Proof / structure

`MDLFI = 106063B` in `006-S3FS` (load base `26000B`) **is** carved (VERIFIED bytes). It is a
shared entry that selects the file operation with two status bits and then joins the common
file-function body:

```
106063  174070  BSET ZRO SSM     function selector bit SSM := 0
106064  174020  BSET ZRO SSK     function selector bit SSK := 0   (00 = delete)
106065  021111  STD I 111        stash the parameter pointer
...
106071  135106  JPL I 106  -> ptr @106177   call the name-parse routine (resident)
...
106114  135066  JPL I 66  -> ptr @106202    call the object/name lookup (resident)
106115  124057  JMP 57  -> 106174           on skip-return (lookup failed): error path
106116  044523  LDA ,B 123                  success: continue with function code
...
106174  004402  STA ,B 2         ILDWD: store the returned error code into B+2 (the caller's error word)
```

- **VERIFIED:** the not-found error is **propagated**, not hard-coded, at `106174` (`STA ,B 2`
  stores whatever error code the lookup routine returned in A into the caller error word).
- **VERIFIED:** both candidate error constants are physically present in `006-S3FS`:
  `SAA 56` (=`170456`, error 46 "No such file name") and `SAA 174` (=`170574`, error 124
  "Illegal parameter") both occur in the segment.
- The name-parse (`JPL I 106 -> 106177`) and the directory/object lookup (`JPL I 66 -> 106202`)
  reach through the segment's **indirect pointer pool** (`106177..106211`) into **resident**
  routines (e.g. pool word `106177 = 003752`, a low resident address). That resident code is in
  the zeroed/uncarved region, so the exact branch that *chooses* 124 vs 46 is **not** statically
  in `006-S3FS`.

### Which code for which case — INFERRED (strong)

Grounded in (a) the two error-code meanings, (b) both constants living in the file-system
segment, and (c) SINTRAN's fixed name-handling order:
1. **Empty/blank filename** is rejected by the **name parser** (the `JPL I 106` step) *before*
   any directory search — a zero-length name is a malformed parameter → **error 124 (Illegal
   parameter)**. It never reaches the lookup.
2. **Well-formed name that matches no file** passes the parser, reaches the **object lookup**
   (`JPL I 66`), which fails and returns **error 46 (No such file name)**, propagated at `106174`.

**Emulator guidance:** `MON 54 (MDLFI)` — empty/blank name string → **124**; syntactically valid
name with no matching file → **46**. (Note "124" here is **decimal** = `174B`; do not confuse
with `124B` = decimal 84 "Not opened for sequential read" in the octal-numbered manual appendix.)

**Honest caveat:** the empty-vs-nonexistent split is INFERRED — the deciding branch lives in the
resident name-parse/lookup, which is not in the static carve. The MDLFI entry stub, the
error-propagation site (`106174`), and the presence of both error constants in `006-S3FS` are
VERIFIED.

---

## Q3 — 503B DVINST (InputString): break strategy + returned byte count

### VERDICT: the terminating **break character IS read, stored, and COUNTED**; `MaxNo` is an **inclusive** ceiling. The returned count therefore **includes the break byte**. Of the two emulators, the one returning **14** (break byte counted) matches real SINTRAN; the **12** result under-counts by dropping the terminator (and stopping one read short of the inclusive max).

### Proof (L07 NPL, corroborated by the 026-S3IMPIT carve)

The character-move + break loop for DVINST (`SMCNO=503`) and DVIO (`SMCNO=511`) is
`IBTBREAK` / `IIBM` in [`../../NPL-SOURCE/NPL/RP-P2-N500.NPL`](../../NPL-SOURCE/NPL/RP-P2-N500.NPL):

```
IIBM:   (130246)
  130262  CALL IOTRANS; GO FAR TMWT      read ONE char (break/echo tables applied by term driver); none -> wait
  130264  X:=...5FYLLE; *SBYT            store the byte in the ND-500 buffer at offset 5FYLLE
  130270  A:=X+1 =: 5FYLLE              increment fill count  <-- count bumped BEFORE any break test
  130273  GO IBTBREAK

IBTBREAK: (130325)
  130325  A =: D                         D := new 5FYLLE
  130326  IF X.SMCNO=511 THEN X.11MXBRK ELSE X.MAXBYT   max := MAXBYT (DVINST) / 11MXBRK (DVIO)
  130335  IF D>=A GO N5RST              5FYLLE >= MAX  ->  done   (INCLUSIVE: the char that hit max is already stored+counted)
  130337  IF T:=RSISTE>=0 THEN
  130342     IF HENTE=T GO N5RST        this char was the break char (matched break table) -> done
          ELSE
  130346     IF BRECHOFL BIT 5BREAK GO N5RST   break flag set by input driver -> done
  130351  GO IIBM                       otherwise read the next char

N5RST: (130353)  ... restart user, return the count:
  130406  A =: D                         D = number of chars = 5FYLLE
  130407  IF X.SMCNO = 511 THEN A:=0; AD=:X.11NOCRET   (DVIO)
  130416  ELSE A:=0; AD=:X.NOCHRET                      (DVINST) <-- returned byte count = 5FYLLE
```

- **VERIFIED (L07 NPL):** order is **store byte → increment `5FYLLE` → test break/max**
  (`130264` → `130270` → `130325`). The break/terminating character is written into the buffer and
  the counter is incremented **before** the loop decides to stop. Hence the returned count
  (`NOCHRET`, `130416`) **includes the break character**.
- **VERIFIED (L07 NPL):** the max test `130335 IF D>=A` is `>=` against the post-increment count,
  so `MaxNo` is an **inclusive** ceiling (the char reaching `MaxNo` is kept and counted).
- **Break-char detection** happens inside `IOTRANS`/the ND-100 terminal input driver using the
  break strategy + break tables (`BreakStrat=8` → user table `BreakT1..BreakT4`, 128 bits, bit set
  = break). The ND-500 side (`IBTBREAK`) only observes the result via `RSISTE`/`HENTE` (a matched
  break char) or the `5BREAK` flag. Terminating chars are exactly those flagged by the break table;
  a break also occurs on reaching `MaxNo`.
- **Echo** is independent of break: whether the (break) char is echoed is decided by the echo
  strategy/echo table in the input driver, not by this counter — echo does not change `NOCHRET`.
- **Corroboration (carved L07):** the carved DVIO analysis at `/mnt/d/ND/t/re/mon-analysis/511B-DVIO/README.md`
  documents the sibling DVIO handler `DVIO=141027B` in `026-S3IMPIT` (byte-proven), whose input
  phase is this same `XNINSTR`/`IBMOVE` path; its write-back mask and `NOCHRET`/`11NOCRET` split
  match the NPL above (DVINST writes `NOCHRET`, DVIO writes `11NOCRET`).

### Breaking the 12-vs-14 tie

Real SINTRAN never stops *before* storing and counting the terminating byte, and `MaxNo` is
inclusive. So the correct return is the **higher** count that **includes the break character**:
**14**. The 12-result emulator is wrong in one (or both) of the exact ways this loop forbids:
(a) it excludes the break terminator from the count, and/or (b) it treats `MaxNo` as exclusive and
stops one read early.

**Honest caveat:** the *rule* (store+count-then-test; break byte counted; `MaxNo` inclusive) is
VERIFIED from L07 NPL and consistent with the 026-S3IMPIT carve. Pinning the literal "14" to NC's
run assumes NC's break table flags the terminator inside the data window (the normal case); the
loop guarantees that whichever byte NC's break table selects as the terminator is itself read,
stored, and counted.

---

## Summary of verdicts

| Q | Call | Divergence | Verdict | Decisive evidence |
|---|------|-----------|---------|-------------------|
| 1 | 123B RELES | error 5 vs silent | **Silent success** (no error 5) | BRELEASE `010624 JAZ` (RESLI=0 → exit `010717`), live L07 |
| 2 | 54B MDLFI | 124 vs 46 | **empty → 124; nonexistent → 46** | MDLFI `106063B` carved; err propagated `106174`; both `SAA 56`/`SAA 174` in `006-S3FS` |
| 3 | 503B DVINST | 12 vs 14 | **14** (break byte counted; `MaxNo` inclusive) | `IBTBREAK` RP-P2-N500.NPL: store+incr (`130264/130270`) before break test (`130335`); count → `NOCHRET` (`130416`) |
