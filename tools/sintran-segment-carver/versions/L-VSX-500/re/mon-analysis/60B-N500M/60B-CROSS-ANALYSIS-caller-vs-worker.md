# MON 60B - cross-analysis: caller side vs worker side

Two independent reverse-engineering efforts met at MON 60B:

- **Worker side (this repo, E:):** `60B-N500M/` - the SINTRAN resident handler `N500M @ 030416B`
  (5PIT), the `5IFUNC` dispatch table, every handler body, the `5NOPAR` common path, and the error
  handlers. Source of truth: the carved L07 bytes + the NPL worker source `5P-P2-MON60.NPL`.
- **Caller side (other session, D:):** `.../mon60-callers/` - the ND-500 Monitor J04 program
  (`nd-500-mon-j04.prog`, runs on the ND-100), 159 call sites -> 123 thunks -> 1 gateway -> 1 MON 60,
  101 per-subfunction folders. Source of truth: the disassembled monitor program bytes.

**Both used `5P-P2-MON60.NPL` for the code->handler map, and they agree exactly** - strong mutual
validation of the `5IFUNC` table. The two halves are complementary: caller = "command -> code ->
thunk -> MON 60"; worker = "code -> handler -> common path -> ND-500 system monitor".

---

## 1. The complete MON 60B path (both halves joined)

```
N500: <operator command>                                    [caller side, D:]
  -> handler routine sets X := b.-176 (param block), fills params
  -> JPL I <disp> -> bank1[P+disp] = thunk addr
  -> thunk (3 words):  SAA <subfn> ; JMP I 1 ; .word 146244  (loads subfunction code into A)
  -> gateway 146244: build param block; MON 60 at 146256
       (on error ECSLOAD=002032B -> auto-load control store + retry)
  ------------------------------------------- MON 60 -------------------------------------------
  -> ENT14 -> GOTAB[60B]=MFELL -> CALLP -> MCTAB[60B]=N500M=030416B   [worker side, E:, byte-verified]
  -> N500M dispatcher: range-check 177B ; A := 5IFUNC[code] ; A =: P
  -> 5IFUNC handler (param marshalling, or 5NOPAR)
  -> 5NOPAR common path: package moncall info onto ND-500 data segment
  -> FPT2ENTRY = ENTER ND-500 SYSTEM MONITOR   [the "more than MON 60" code - not yet carved]
```

The join point is exact: the caller's `SAA <subfn>` code is the same code that indexes the worker's
`5IFUNC` table.

---

## 2. Mutual validation (independent derivations that agree)

| Fact | Caller side (bytes) | Worker side (bytes+NPL) |
|------|---------------------|--------------------------|
| `5IFUNC` code->handler map | table from `5P-P2-MON60.NPL` | table from same NPL - **identical** |
| `117B` ABORT + `122B` LOGOUT share one routine | caller routine `110333B` (sites `110346`/`110355`) | one shared worker body `IPRABORT`/`ILOGOFF` -> `60B-117B-IPRABORT/` |
| `024B`/`157B` both WRITE CONTROL STORE | duplicate handler `IWCNT` | NPL "equal for func=157"; `60B-024B-IWCNTS/` |
| `034B`->`035B` = STOP-ND-500 (micro-stop -> master-clear) | caller routine `122507B` | both `5NOPAR` (MSTOP/MSTCL) |
| worker overlay = 5PIT | (n/a) | `050-S3I5PIT`, confirmed via release-doc PIT map |
| priority codes `037/007/054/041/103/133` | PROVEN call sites | matching `5IFUNC` handlers |

---

## 3. What each side CLOSES for the other

### 3.1 Worker side closes the caller's open item #2 (the >142B identities)
The caller's `INDEX.md` section 5 lists "~20 subfunctions above `142B` are undocumented ... need the
SINTRAN `5IFUNC` dispatch table ... UNKNOWN here." **The worker carve resolves all of them** (handler
bodies + pseudo-C in `60B-N500M/`):
`143B IMO5RT`, `144B ICHACPU`, `145B ISSTDOM`, `150B ILI5TQU`, `154B IDBUGSW`, `157B IWCNT`,
`160B IN5SEGLOAD`, `161B INDFSYDOM`, `173B ICPUSTAT`. (The caller's own `SUBFUNCTION-TABLE.md` also
lists these from the NPL - so their table already closes their INDEX's open item; the worker carve
adds the executable bodies.)

### 3.2 Caller side enriches the worker's `5NOPAR` codes
The worker lumped ~70 codes as `5NOPAR` (no param-prep). The caller's table carries the **verbatim
`FUNCTION=` purpose** for many of them, which the worker folders should reference:

| code | purpose (from caller table / NPL FUNCTION= comment) |
|------|------|
| `002/003` | logical program / data memory READ |
| `023` | READ CONTROL STORE (equal for func=157) |
| `026/030` | data / program memory examine (4 bytes) |
| `032` | physical data memory read (ABSMR) |
| `041` | **READ ND-500 INTERFACE STATUS** (STATUS command) |
| `051` | READ INTERFACE (COMMUNICATION), IODATUT REGISTER |
| `057` | READ MICRO PROGRAM VERSION |
| `060` | LIST MEMORY CONFIGURATION |
| `070/072` | read process / phys-segment table entry from the sys.mon |
| `073` | READ FROM A PHYSICAL SEGMENT |
| `105` | SET PRIORITY |
| `116` | LOG OFF OWN PROCESS |
| `121` | READ FROM SWAPPER'S DATA MEMORY (logical addrs) |
| `141` | SET BLOCK SIZE OF A FILE |
| `142` | REDEFINE DEFAULT INFANT FILE (DEFINF) |
| `156` | READ SYSTEM INFO |
| `166` | DUMP-TRACE-MEMORY |
| `170` | READ ND-500 CPU-TYPE AND MIC.VERSION |
| `172` | READ HW SCRATCH REGISTER FILE |

These are still `5NOPAR` server-side (no param marshalling); the work happens in the ND-500 system
monitor reached via `FPT2ENTRY`. The purposes are logic, not a reason to change the worker handler.

### 3.3 Caller side confirms the control-store gate (ties to the emulator hang)
Caller `INDEX.md` section 1: the gateway (`146244`) on error `ECSLOAD` (`002032B` @ `146304`) or
`004017B` (@ `146305`) **auto-loads the control store and retries - so ANY MON 60 can trigger a
control-store load if the ND-500 microcode is not resident.** This is the byte-level mechanism behind
the OBSERVED emulator behaviour earlier this session (even `VERSION` hung on "Loading Control
Store"). Confirms the control-store-gate analysis in `SINTRAN\ND500\ND500-STATUS-AND-INDEX.md`.

---

## 4. Reconciliations (naming / edge cases - both sides agree on the resolution)

1. **`130B` = START STANDARD DOMAIN (`ISFSYDOM`), NOT "PLACE-DOMAIN".** The authoritative NPL purpose
   is START STANDARD DOMAIN (family `127B` define / `130B` start / `131B` delete). The yaml client
   name `PLADOM` and the operator keyword PLACE-DOMAIN sit on this code, but the caller side flags
   (INDEX section 4) that the PLACE-DOMAIN keyword was NOT confirmed - PLACE-DOMAIN likely brackets
   `055B` START-PLACE + `006B` LOAD-ONE-SEGMENT + `056B` END-PLACE instead. **Worker + caller agree:
   lead with START STANDARD DOMAIN for `130B`.** (Worker folder note updated accordingly.)
2. **`147B`**: `5IFUNC[147]=ILLFUNC`, yet a `FUNCTION=147` comment ("ESCAPE TYPED WHILE RUNNING STD
   DOMAIN") exists - reached by a path OTHER than the `5IFUNC` index. Both sides flag it; unresolved.
   (Worker `60B-ERRORS/` marks 146/147 as ILLFUNC.)
3. **`ISTOHIAT` vs `ISTOHIST` (064B)**: the NPL `5IFUNC` array spells it `ISTOHIAT` (line 1326); the
   handler label is `ISTOHIST` (line 1464). Same routine, NPL typo. Worker folder uses the label
   `ISTOHIST`; caller table uses the array spelling `ISTOHIAT`. Cosmetic only.
4. **`5NOPAR` is not a no-op** (both sides state this): it is the common forward path to the ND-500
   system monitor. A code can be fully functional and still be `5NOPAR` (e.g. `041` READ STATUS).

---

## 5. Combined open front (both sides point at the same next target)

Both efforts converge on ONE remaining piece: the **ND-500 SYSTEM MONITOR** reached by `FPT2ENTRY`
(worker `5NOPAR` common path) - the code that builds/consumes the 5MPM message and actually drives
the ND-500. Caller side has proven everything up to the `MON 60`; worker side has proven everything
through the `5IFUNC` handler and into `FPT2ENTRY`. Neither has carved the system monitor itself.
That is the concrete next carve (parent SCOPE NOTE; `ND500-STATUS-AND-INDEX.md` Phase 1/3).

Also open: locate the **bank-2 `5IFUNC` table** in L07 for byte-verified per-handler `.ASM`; and the
caller's **22 unresolved thunks** (frame-relative dynamic dispatch) + which operator command reaches
`130B`.

---

Sources: worker `60B-N500M/` (this dir) + `5P-P2-MON60.NPL`; caller
`/mnt/d/ND/500/ND-500(0) System Package for SINTRAN IIIVSX L/mon60-callers/{INDEX.md,SUBFUNCTION-TABLE.md}`
and `nd-500-mon-j04.prog.{asm,md}`.
