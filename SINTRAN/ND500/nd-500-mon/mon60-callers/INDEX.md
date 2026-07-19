# MON 60 (N500M) Callers - Master Index

How the ND-500/5000 Monitor J04 (`nd-500-mon-j04.prog`, running on the ND-100) drives
the ND-500 through the `MON 60` monitor call, and which operator command uses which
`MON 60` subfunction.

Base directory:
`SINTRAN/ND500/nd-500-mon/mon60-callers/`

Primary sources (all claims here are read from these):
- Disassembly: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`
- Monitor analysis: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.md`
  (section 5 = the gateway; **section 6 = the full 123-thunk / 159-call-site table** - the
  authoritative per-thunk caller list, not duplicated here)
- Subfunction names/params: `Developer/MON/calls/60B_N500M.yaml`

All addresses are OCTAL (ND convention). Facts are marked PROVEN (read from bytes) or
INFERRED (reasoned - e.g. command-name to subfunction where the handler was not traced).

---

## 1. The mechanism (PROVEN)

The monitor never touches the ND-500 bus hardware directly (zero `IOX`/`IOXT`). Every
ND-500 operation funnels through **one** `MON 60` instruction:

```
159 call sites  ->  123 thunks (146310..147070)  ->  1 gateway (146244)  ->  1 MON 60 (146256)  ->  SINTRAN
```

- **Thunk** (3 words, verified): `SAA <subfn>; JMP I 1; .word 146244`. The `SAA` loads the
  MON 60 subfunction code into A; `JMP I 1` jumps through the 3rd word (the gateway pointer).
- **Gateway** (`146244`): builds the parameter block, issues `MON 60` at `146256`, then
  `JMP 2` = skip return (success) or `JPL I 23` = error. On error code `ECSLOAD` (`002032B`,
  at `146304`) or `004017B` (at `146305`) it auto-loads the control store and retries - so
  ANY `MON 60` can trigger a control-store load if the ND-500 microcode is not resident.
- **A call site** (verified pattern): the handler sets `X := b.-176` (its MON 60 parameter
  block), stores the subfunction's parameters into the block, then `JPL I <disp>` where
  `bank1[P+disp]` is a POINTER WORD holding a thunk address. Example (PROVEN):
  `010214 JPL I 71` -> pointer `010305`, `bank1[010305]=146340` = thunk `SAA 7` = SWLOD.

To read any call site: `JPL I <disp>` -> `EA = P+disp` -> `bank1[EA]` = thunk addr ->
that thunk's `SAA n` = the subfunction. The full resolved list is in
`nd-500-mon-j04.prog.md` section 6.

---

## 2. Operator command -> MON 60 subfunction

The `N500:` operator commands map to `MON 60` subfunctions as below. "Handler call site"
is the resolved `MON 60` call site from `nd-500-mon-j04.prog.md` section 6 where PROVEN;
where only the subfunction identity is certain (from the yaml) but the exact handler was
not traced, the command->subfunction is marked INFERRED.

### 2.1 ND-500 initialisation / setup (the priority set - carved to folders)

| Command | Subfn | Name | Thunk | Call site | Status |
|---------|-------|------|-------|-----------|--------|
| LOAD-CONTROL-STORE       | `037B` | LDCS   | `146445` | `006114` | PROVEN |
| LOAD-SWAPPER             | `007B` | SWLOD  | `146340` | `010214` | PROVEN |
| START-SWAPPER            | `054B` | STSWP  | `146525` | `010217` | PROVEN |
| GIVE-N500-PAGES          | `052B` | G500P  | `146517` | `010232` | PROVEN |
| TAKE-N500-PAGES          | `053B` | T500P  | `146522` | `010245` | PROVEN |
| STATUS                   | `041B` | RSTAT  | `146461` | `127566` | PROVEN |
| PLACE-DOMAIN             | `130B` | PLADOM | `146712` | `043171` | PROVEN |
| DEFINE-STANDARD-DOMAIN   | `127B` | DEFDOM | `146707` | `046056` | PROVEN |
| DELETE-STANDARD-DOMAIN   | `131B` | DELDOM | `146715` | `007347` | PROVEN |
| LIST-STANDARD-DOMAINS    | `132B` | LSTDOM | `146720` | `007352` | PROVEN |
| LIST-EXECUTION-QUEUE     | `133B` | LSTEXQ | `146723` | `111445` | PROVEN |
| LIST-SYSTEM-PARAMETERS   | `103B` | RSYSP  | `146624` | `073132` | PROVEN |
| SET-SYSTEM-PARAMETERS    | `104B` | WSYSP  | `146627` | `073354` | PROVEN |
| DEFINE-MEMORY-CONFIGURATION | `040B` | DEFM | `146450` | `135361` | PROVEN |
| MEMORY-CONFIGURATION     | `060B` | LIMEM  | `146541` | `135532` | PROVEN |
| MASTER-CLEAR             | `035B` | MSTCL  | `146456` | `005736`/`005744`/`122514` | PROVEN |
| MICRO-START              | `025B` | MICST  | `146415` | `006307`/`130130`/`130361`/`131140` | PROVEN |
| MICRO-STOP               | `034B` | MSTOP  | `146453` | `006312`/`122512` | PROVEN |
| VERSION (microprog vers) | `057B` | MPVER  | `146536` | `005577`/`132132` | PROVEN |

### 2.2 Other commands mapped by subfunction (INFERRED from name + yaml unless noted)

| Command | Subfn | Name | Thunk | Call site(s) |
|---------|-------|------|-------|--------------|
| OPEN-FILE (connect)      | `013B` | CNCFI  | `146351` | `036440` |
| CLOSE-FILE               | `014B` | CLSFI  | `146354` | `005123` |
| LIST-OPEN-FILES          | `017B` | LISOP  | `146365` | `005157` |
| SET-BLOCK-SIZE           | `141B` | SETBLK | `146742` | `005154` |
| DEFINE-SWAP-FILE         | `045B` | DEFSW  | `146475` | (no static caller) |
| DELETE-SWAP-FILE         | `047B` | DELSW  | `146503` | `007430` |
| TIME-USED                | `020B` | TIMUS  | `146370` | `005162` |
| WHO-IS-ON                | `021B` | WHO    | `146373` | `005165`/`007445` |
| SET-FLAG                 | `101B` | SPFLAG | `146621` | `005223` |
| GET-FLAG                 | `100B` | RFLAG  | `146616` | `005264` |
| SET-PRIORITY             | `105B` | SPRIO  | `146632` | `006406`/`006444`/`006513` |
| SET-PROCESS-NAME         | `074B` | SPRNM  | `146602` | `010112` |
| ABORT-PROCESS            | `117B` | ABORT  | `146657` | `110346` |
| LOGOUT-PROCESS           | `122B` | LOGOUT | `146670` | `110355` |
| ATTACH-PROCESS (link)    | `106B` | LNKPR  | `146635` | `006711` |
| PLACE-DEBUGGER           | `134B` | PLADBG | `146726` | `002560` |
| ACTIVATE stopped proc    | `136B` | ACTIV  | `146731` | `010610` |
| SET-HISTOGRAM            | `062B` | HIDEF  | `146544` | `040133` |
| START-HISTOGRAM          | `063B` | HISTA  | `146547` | `010440` |
| STOP-HISTOGRAM           | `064B` | HISTP  | `146552` | `010435` |
| PRINT-HISTOGRAM          | `065B` | HISTN  | `146555` | `040437` |
| RELEASE-HISTOGRAM        | `066B` | HIREL  | `146560` | `010443`/`110130` |
| START-PROCESS-LOG-ONE    | `111B` | SLOG1  | `146643` | `110161` |
| START-PROCESS-LOG-ALL    | `115B` | SLOGA  | `146654` | `110143` |
| PRINT-PROCESS-LOG (read) | `113B` | RLOG   | `146646` | `110116`/`110243`/`110310` |
| RELEASE-LOG-BUFFER       | `114B` | RELLOG | `146651` | `006676` |
| START-MONCALL-LOG        | `124B` | SMONLOG| `146676` | `007312` |
| PRINT-MONCALL-LOG        | `125B` | PMONLOG| `146701` | `111232` |
| STOP-MONCALL-LOG         | `126B` | XMONLOG| `146704` | `007320` |
| COMPARE/LOOK CONTROL-STORE (read CS) | `023B` | REACS | `146407` | `123556`/`124201` |
| (write control store)    | `024B` | WRICS  | `146412` | `123420` |
| LOOK-AT-PHYSICAL-SEGMENT (read) | `073B` | RPHSG | `146420` | `056407` |

Note: several commands issue MULTIPLE subfunctions (e.g. the swapper/paging block at
`010200`-`010260` issues SWLOD, STSWP, G500P, T500P; STATUS reads RSTAT plus MPVER). The
per-command folders document the full sequence each handler runs. Commands that do NOT go
through `MON 60` at all (pure ND-100-side operations, macros, formatting) are not listed.

---

## 3. Full thunk table (PROVEN, address -> subfunction)

The 123 thunks at `146310`-`147070`, one per source-declared ND-500 interface routine, in
declaration order (not numeric order). Duplicate thunks exist for a few subfunctions
(`006B` `007B` `036B` `037B` `050B` `073B` `151B` `152B`) - at most one of each pair has a
caller. This binary's subfunction space runs `0`-`177B` (`FUNCMAX=177`), extending ~20
subfunctions past the 94-entry NDInsight yaml (codes above `142B`, and `036B`/`042B`/`046B`/
`107B`, are undocumented there). See `nd-500-mon-j04.prog.md` section 6.2.

The complete `thunk -> subfunction -> caller` table with all 159 call sites is in
`SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.md`
section 6. It is not duplicated here to keep a single source of truth.

---

## 4. Per-subfunction folders - COMPLETE (101 folders)

Every MON 60 subfunction that has a resolvable caller in this binary is carved into its own
subfolder of this directory. **101 folders, 303 files** (each folder holds three files:
`<name>.asm` annotated handler/case listing, `<name>.pseudo.c` C pseudo-code, and
`README.md` with the authoritative purpose, server handler, every call site + enclosing
routine, the parameter block filled before the call, skip/error paths, and an explicit
unknown/inferred list).

Two naming conventions are used:

- **8 priority folders are named by operator command** (the ND-500 initialisation/setup set):
  `LOAD-CONTROL-STORE/`, `LOAD-SWAPPER/`, `START-SWAPPER/`, `STATUS/`,
  `START-STANDARD-DOMAIN/` (subfunction 130B; the operator keyword PLACE-DOMAIN was NOT
  confirmed - named by authoritative purpose), `LIST-STANDARD-DOMAINS/`,
  `LIST-SYSTEM-PARAMETERS/`, `LIST-EXECUTION-QUEUE/`.
- **93 folders are named `<CODE>B-<MNEMONIC>`** (e.g. `037B` is covered by `LOAD-CONTROL-STORE/`;
  `012B-RUNN/`, `144B-CHANGE-CPU/`, `170B-READ-CPU-TYPE/`, ...). Operator-command keywords were
  deliberately NOT invented for these - the bank-2 command-string table was not consulted, so
  they are named by subfunction to avoid guessing.

Undocumented subfunctions (no `FUNCTION=` comment in the NPL, generic `5NOPAR` dispatch) are
named `<CODE>B-UNDOC/`: `036B` `155B` `162B` `163B` `164B` `165B` `167B` `171B`.

Verification (all folders): every call site's `JPL I <disp>` was resolved
`bank1[P+disp] -> thunk -> SAA <code>` from the bytes; octal=hex=decimal cross-checked;
error/success return polarity applied per `nd-500-mon-j04.prog.md` section 5.4; ASCII-only;
no fabricated names (unknowns are labelled). See each folder's README for its
proven-vs-inferred split.

---

## 5. Open items

- **22 thunks have no statically resolvable caller** (listed in `nd-500-mon-j04.prog.md`
  section 6.1). They are either dead or reached through a frame-relative `JPL I ,B <disp>`
  dynamic dispatch a static resolver cannot follow. UNKNOWN.
- **~20 subfunctions above `142B` are undocumented** in the NDInsight yaml. Their identities
  need the SINTRAN `5IFUNC` dispatch table at `5P-P2-MON60.NPL:1405-1575`. UNKNOWN here.
- **INFERRED command->subfunction rows** (section 2.2) map by command name + yaml purpose;
  the exact handler function was not traced for those. To promote to PROVEN, trace the
  handler enclosing each listed call site (as done for the priority set).
