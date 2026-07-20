# D4 plan - phases and tasks (2026-07-20)

**Full path:** `SINTRAN/ND500/D4-PLAN-PHASES-AND-TASKS-2026-07-20.md`

Refactors the scattered "next actions" (`HANDOFF-3022-SWAPPER-TRACK-2026-07-20.md` section 5),
priority lists (`OPEN-QUESTIONS-REGISTER-2026-07-20.md` section "Priority" + 2.3), and pending
chores into one ordered plan. This file is the **plan of record**; the register is the **unknowns of
record**; the finding doc (`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` 12d-12l) is the **evidence**.

**Goal (D4 acceptance):** a real LINKAGE-LOAD domain runs on the functional `CpuND500` under live
SINTRAN III L on the ND-5800 image and reaches the `NLL:` prompt.

**Route legend:** `CARVE` = bytes we already hold; `MICROCODE` = the ND-5000 microcode-CPU track;
`LIVE` = boot-harness experiment; `MANUAL` = a document; `CODE` = a pure RetroCore change.
Each task lists the open-question IDs it closes and an acceptance test. **Standing rule: UNKNOWN
beats a plausible wrong answer** - four claims in this track were retracted after being stated too
strongly.

---

## Phase 0 - Hygiene (no dependencies, can run anytime)

Cleanup that does not need any carve/microcode answer. Do these to keep the tree honest.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 0.1 | **Remove all references to the nonexistent WIOM instruction.** Inventory done: clean `FINAL_STATUS_REPORT.md:363`, `INSTRUCTIONS_MISSING_DOCUMENTATION.md:352/404`, `SESSION_COMPLETE_2025-10-16.md:326`, `TOVERIFY/ND-Systems/nd500-asm-research.md`, nd500x `Riom.c`. Each removal states WHY (no manual/index/opcode entry). **Do NOT touch** the deliberate corrective notes (`Riom.cs`, `ND100Bridge.md`, `swapper-k01-deep-analysis.md`, handoff). | CODE/DOC | task #19 | grep for `WIOM` returns only corrective notes |
| 0.2 | **RIOM residue 4a** - decide whether `_private` should default to `0x40000` or `0`. The 3022 bridge ctor sets 0; the field default is `0x40000`. Currently unproven convention (C4). | CODE | C4 / Q-SWP-09 | default justified in a comment or changed |
| 0.3 | **RIOM residue 4b** - the narrowing `ushort` overloads `ND100BusAccess` always takes are untouched; mark or widen. | CODE | - | overloads documented or removed |
| 0.4 | **NDIX/octobus assessment** (agent died on weekly limit). Assess whether NDIX evidence (NEC-01 5015 unit-select codes, `(phys+private)/2`, MON 600/`PC_OMC`, ND-05.012.01 section 13) requires octobus code changes or is doc-only. | MANUAL/CODE | C7 / Q-OTH-05 | written verdict: code change vs TODO-only |

---

## Phase 1 - Unblock the current D4 stop (swapper null-deref at PC=0x0800913B)

The swapper executes to `0x913B`, does a correct RIOM, then null-derefs a message pointer
(`w4 laddr r2.(10)` with `r2=0`). This is the literal current stop. Cheapest path forward.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 1.1 | **LIVE probe: capture `0x420E30` AT the moment of the RIOM**, not after RUN. The post-RUN dump shows zeros but does not establish transfer-time state. Decides: empty message handed in, or lost between? | LIVE | A5 / Q-SWP-08 | value of `0x420E30` logged at RIOM issue |
| 1.2 | If the message is genuinely empty: **is "no work" the expected answer**, and is the swapper meant to branch earlier on a field we do not supply? Carve the swapper path around `0x913B`. | CARVE | A5 / Q-SWP-08 | branch condition at 0x913B identified |
| 1.3 | **Is the swapper's message buffer PRIVATE or SHARED ND-100 memory?** `SetND100PrivateOffset(0)` is unproven. Carve `SWMSG` alloc in `MSINIT`. | CARVE | C4 / Q-SWP-09 | private-vs-shared decided from bytes |

---

## Phase 2 - MMU model correctness (the biggest structural gap)

The emulator hand-builds capabilities because it does not know who builds the real tables. Until
this phase lands, the MMU "cannot converge" (observed: program 0/1; data 0/1/13). Mostly MICROCODE
track; carve fallbacks noted.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 2.1 | **Who builds the PCB (32 prog + 32 data caps) and the PST, and when?** 8 MB scan found 0 PSTE candidates -> SINTRAN does not build them in PLACE-DOMAIN. Candidates: swapper-once-running, microcode at process start, CPU-internal. **Highest value - unblocks 2.2/2.4/2.6/2.7.** | MICROCODE (fallback: carve swapper startup) | A1 / Q-MMU-01 | builder + timing identified |
| 2.2 | **Where does `PSTP` come from?** The 21B block has no PST-base entry. "The single missing anchor." | MICROCODE | A3 / Q-MMU-02 | PSTP source identified |
| 2.3 | **Where does the program SEGMENT number come from at 3START?** 21B sends `P=0x04` (no segment bits) yet the swapper runs at segment 1. | MICROCODE | A2 / Q-SWP-03 | segment source identified |
| 2.4 | **21B register word order** - high halfword first or low? Decides what `PS` actually is. "Our biggest single unknown." | MICROCODE | Q-SWP-04 | word order fixed from evidence |
| 2.5 | **Is the MMU enabled at swapper start, or does the swapper enable it?** Carve route: disassemble from `SWAPPER-K01.PSEG` entry. | MICROCODE/CARVE | A4 / Q-MMU-03 | start MMU state decided |
| 2.6 | **Does the microcode perform the documented walk exactly** (`PS->PST[PS]->PCB->cap->psn->PST[psn]->PT->PTE`, 256 B/domain)? PSTE 4 B, `PFN==0` = not present? | MICROCODE | Q-MMU-04/05 | walk confirmed or corrected |
| 2.7 | **Correct capability set for the swapper at start**; is segment 13 (`0x68000044`) meaningful? Current all-64-caps-same-tables is a declared hack. | MICROCODE | Q-MMU-06 | real cap set installed |
| 2.8 | **Segment-capability bit layout** (contradiction C9): claim A (bit13=S,12=P,11=W, 11-bit seg) vs claim B (W=15,P=14,S=13, 12-bit). | MANUAL ND-05.009.4 | Q-MMU-08 | layout resolved |

---

## Phase 3 - Transport + descriptors (mostly CLOSED, verify residue)

The swapper PSEG/DSEG delivery question (old B1 / Q-SWP-01) is **CLOSED**: `MON 131` ABSTR disk DMA
into ND-100 physical memory, page from `MON 61` FIXC5 (register section 2.0). Remaining residue:

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 3.1 | **Decode the descriptor SINTRAN DMAs to `0x6F000`** (`02 C0` at +3). May answer 2.1/2.3. | CARVE | B2 / Q-SWP-02 | descriptor decoded |
| 3.2 | **What does the microcode place in a trap report?** Our trap fields are demonstrably wrong (fault at instruction fetch printed as DATA READ). Which word is access type, which the failing logical address, how is the segment encoded? | MICROCODE | C5 / Q-TRP-01 | trap-report layout fixed |

---

## Phase 4 - RUN precondition -> the NLL: prompt (the D4 finish line)

Once the swapper is genuinely alive and building tables, RUN must accept the domain. These gate the
final `NLL:` assert.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 4.1 | **PLACE-DOMAIN / RECOVER-DOMAIN subfunction NUMBERS and parameter-block layouts** (FUNCS/5IFUNC). Briefing forbids implementing as fact. Gates the D4 `NLL:` assert. | CARVE | Q-DOM-01 | subfunction numbers byte-proven |
| 4.2 | **Where is the exact branch deciding "runnable state" / printing message #14** ("NO WELL DEFINED PROGRAM IN MEMORY")? LIVE BP @ `030624`. | LIVE | Q-DOM-06 | branch + condition captured |
| 4.3 | **Segment-table stride and field encodings that RUN checks**; do RUN / LIST-ACTIVE-SEGMENTS read Table A or a mirrored ND-100 `S500S` array? | LIVE/CARVE | Q-DOM-07 / Q-SWP-13 | RUN's read target proven |
| 4.4 | **Where is `N500M`'s executable body?** Data reached via `CALLP`/`MCTAB[N]`, or an overlay? Do not extract subfunctions until proven. | CARVE | Q-DOM-08 | N500M body located |

---

## Phase 5 - Parallel octobus / CS-load track (separate blocker family)

Independent of the swapper track; needed for the octobus-provisioned boot path. Prioritised from
register 2.3. Not on the 3022 critical path to D4 but shares B1-class answers.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 5.1 | **ACCP parameter-pointer units** (byte / 16-bit word / MAR-scaled) and what SINTRAN compares on VPARP return. "The prime suspect" for the CS-load stall. | LIVE | Q-CSL-02/03 | units proven; stall explained |
| 5.2 | **CONOMD runtime `5OMDNO` allocation model** beyond 0/3/4. `CONOMD` body @`040062` uncarved. "A harness that hardcodes an assumed `5OMDNO` tests itself." | CARVE (else LIVE) | Q-OCT-13 | allocation model carved |
| 5.3 | **MFACK oracle** - why carved L07 MFACK reads `LMFIELD` word 3 (byte count, not source station), range-tests 2..6, uses it as `MOCTSTATION`. Until resolved the C6 ack oracle is `[OPEN]`. | CARVE/LIVE | Q-OCT-14 | MFACK behaviour explained |
| 5.4 | **DIOC provisioning** - who creates the port descriptor, writes `PDF.DRPRT`/`DOMDF.DLPRT`; what initializes `DOMDF`; how DDS-DEVICES:CNFG binds station+SCSI unit+LUN. Which SCSIHDD a request targets is currently unknowable. | CARVE | Q-DEV-01/02 | binding path carved |
| 5.5 | **Does the B30 image ever ACCEPT MICFU 21B (3WREG)**, or is `20,21 -> MSG_ILLEG` the truth? Decides register-image vs context-block swapper start. | MICROCODE | C1 / Q-MBX-09 | dispatch behaviour proven |

---

## Phase 6 - Method / release confidence (not carve targets)

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 6.1 | **Establish whether the 4 SHR failures are pre-existing** - impossible from the current dirty tree (~50 foreign modified files). Needs a clean tree. | LIVE (clean tree) | D1 / Q-PROC-01 | run on clean tree |
| 6.2 | **Boot-harness flakiness** - `Nd500_D4_RunDomain_RealCpu_Capture` passes and crashes for identical code (uncatchable StackOverflow on the CPU thread suspected). Never attribute a failure to a change from one run; take >=2 samples. | LIVE | D2 / Q-PROC-02 | root cause or documented workaround |

---

## Critical path to D4 (the short version)

The minimum chain to the `NLL:` prompt, in order:

1. **1.1** capture `0x420E30` at the RIOM  ->  is the message empty?
2. **2.1 + 2.2 + 2.3 + 2.4** the MMU anchors (PCB/PST builder, PSTP, segment number, word order) - **the real bottleneck**, MICROCODE track owns most of it.
3. **4.1 + 4.2** PLACE-DOMAIN subfunctions + the RUN runnable-state branch.
4. **D4 assert:** `NLL:` prompt reached in `Nd500_D4_RunDomain_RealCpu_Capture`.

Phases 0, 3.1, 5, 6 are off the critical path and can proceed in parallel or be deferred.

---

## Dependency notes

- **Phase 2 is the true bottleneck** and is mostly owned by the microcode track, which is currently
  rate-limited (weekly API limit resets Jul 22, 1pm Oslo). Phases 0, 1.1, 3.1, 4.2 are things this
  track can do now without them.
- Phase 1.1 (LIVE probe) is the single cheapest next action and needs no external answer.
- Do NOT start Phase 4.1 subfunction extraction before Q-DOM-08 (4.4) proves where `N500M`'s body
  is - the briefing forbids implementing subfunctions as fact.
