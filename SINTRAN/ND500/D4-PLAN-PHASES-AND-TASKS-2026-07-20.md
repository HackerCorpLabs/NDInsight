# D4 plan - phases and tasks (2026-07-20)

**Full path:** `SINTRAN/ND500/D4-PLAN-PHASES-AND-TASKS-2026-07-20.md`

> ## STATUS 2026-08-02 - READ THIS FIRST; the phase tables below are HISTORICAL 
> The octobus swapper track is **GREEN**: `FullFlow_Octobus_Login_Nd500_Status_StartSwapper_Capture`
> ran 2/2 PASS (status/start-swapper/list/stop-system all OK), deterministic. **Do NOT re-chase the
> "Loading Swapper" verify stall, the 0x913B swapper deref, SWMSG, or stop-system** - all closed.
> The 9 completed tasks (#13/#15/#17/#19/#20/#21/#22/#23/#24) were pruned from the live task list on
> 2026-08-02. The DONE/RESOLVED rows below are kept ONLY as the carve/evidence record.
> **The only genuinely-open work:** production wiring toward D4 (#10 -> #9/#11/#14/#16 -> #12), and
> correctness debt (#25 sub-word WIDTH, #26 CSIT divergences, #27 verify the real 5800 CS-load path).
> See `nd500-octobus-frontier-2026-08` (memory) and `OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md` section 7.8.

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

## UPDATE 2026-07-21 - critical-path REFRAME + Track A/B split  [decision: run BOTH in parallel]

Two developments post-date the 2026-07-20 plan below and change the critical path:

1. **The CS-load "Loading Swapper" verify stall is the DOMINANT upstream blocker** [dual-ring evidence
   2026-07-21; memory `nd500-d4-path-to-nll`, finding doc sec 11]. During PLACE-DOMAIN the ND-100 NEVER
   leaves the CS-load verify loop (place trace maxes at PC `0xDAD3`; `SPLAC`(`0xE85F`) / `ENDPL`(`0xE90D`)
   NEVER reached). RUN's "well defined program" precondition needs `ENDPL` to build `S500S` from the
   swapper's mailbox answers, so it can NEVER be met while the stall persists. RE-PRIORITIZES: the
   swapper fn-5 / empty-MESSBUFF crash (Phase 1) is SECONDARY - fixing it alone cannot unblock RUN.
   **The CS-load verify stall (was Phase 5.1, mislabeled "not on the critical path") is now the Track A
   critical path.**
2. **The real microcode CPU `CpuND5000` now exists** and can execute the 5800's 128-bit CONTROL-STORE
   swapper - the exact thing the functional `CpuND500` cannot, which is WHY Track A uses the classic
   SWAPPER-K01 macrocode as a stand-in. This opens Track B (new Phase 7).

**DECISION (Ronny, 2026-07-21): run BOTH tracks in parallel.**
- **Track A (near-term critical path):** functional `CpuND500` + SWAPPER-K01 macro stand-in; make the
  CS-load verify stall the #1 fix (Phase 5.1 -> promoted). Phases 1-4 still apply once CS-load clears.
- **Track B (parallel, new Phase 7):** run the GENUINE 128-bit CS swapper on `CpuND5000`. Resolves the
  un-executable-swapper root cause; CS-load becomes real, not faked. Jointly owned with the microcode
  session (skill `nd5000-microcode`); coordinate via `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-*` docs.
- **Cross-check:** the mailbox differential oracle (`MailboxOracleRunner`, microcode vs servicer, now
  141/141 byte-parity) adjudicates any Track-A-servicer vs Track-B-microcode disagreement. The servicer's
  answer/ring side is already byte-verified against the real microcode (finding #3), so an octobus mailbox
  divergence is a real bug, not a modeling guess.

Phase numbers below are unchanged; this reframe only moves the CRITICAL PATH (Phase 5.1 up, Phase 1 down
to secondary) and ADDS Phase 7. The "Critical path to D4" section at the end is AMENDED by this block.

---

## Phase 0 - Hygiene (no dependencies, can run anytime)  -  COMPLETE 2026-07-20

Cleanup that does not need any carve/microcode answer. Do these to keep the tree honest.
**All four items (0.1-0.4) done 2026-07-20. Comment/doc-only changes; no behavior change.**

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 0.1 | **DONE 2026-07-20.** Removed all WIOM references. First re-verified the premise against the manual (`ND-05.009.4` TOC + section headers: 16.23 = RIOM read-only, 16.24 = TSB-clear; no "Write I/O processor memory" section) because the NDIX evidence doc had cited "RIOM/WIOM section 16.23" - that claim was FALSE, propagated from a hallucinated See-Also + example code in the generated `riom.md`. Cleaned: 4 RetroCore status reports, `ND500-DOCS/instructions/asm/riom.md` (body+See-Also, the origin), `wdus.md`, `Operations/.../ND500-MONITOR-CALL-ARCHITECTURE.md`, `NDIX-KERNEL-INTERFACE-EVIDENCE` (marked correction), register C7 line, and two stale spots in `ND100Bridge.md`. Left: corrective notes, `tmpDoc/` scratch copy, `nd500x` (not on this machine). | CODE/DOC | task #19 | grep `WIOM` returns only corrective notes [MET] |
| 0.2 | **DONE 2026-07-20.** `_private` default left at `0x40000` (changing it needs the C4 carve; other consumers may rely on it) with a doc-comment recording that the 3022 path forces 0 and the private-vs-shared question is C4-open. Comment-only. | CODE | C4 / Q-SWP-09 | default justified in a comment [MET] |
| 0.3 | **DONE 2026-07-20.** `ND100BusAccess` documented as the 16-bit segment-31 register path, distinct from RIOM's 24-bit DMA (which uses the uint overloads directly). Left ushort rather than widen speculatively. Comment-only. | CODE | - | overloads documented [MET] |
| 0.4 | **DONE 2026-07-20.** Verdict: **NO octobus code change; TODO/doc-only.** The NDIX evidence is the 3022/segment-31 mailbox path (fecall=MON 600B, `(phys+private)/2` = the existing bridge arithmetic it corroborates, §13 message protocol), NOT octobus frame transport. Surfaced two 3022/bridge-track TODOs (MON-600B FE_INIT for an NDIX guest; segment-31-vs-segment-6 OMC check). Full verdict in `NDIX-KERNEL-INTERFACE-EVIDENCE-2026-07-20.md` section 9. | MANUAL/CODE | C7 / Q-OTH-05 | written verdict [MET] |

---

## Phase 1 - Unblock the current D4 stop (swapper null-deref at PC=0x0800913B)

**LIVE D4 EVIDENCE 2026-07-21 (ran `Nd500_D4_RunDomain_RealCpu_Capture` twice, both PASS observationally, RUN->N500:):**
- Run 1 (carve-correct seg-1 mapping): the RIOM at 0x82FF read an ALL-ZERO message (no work).
  The swapper processes it and at PC=0x0800913B does `laddr r2.0x0A` with r2=0 -> DATA SEGMENT 0
  offset 10 -> **MMU protection violation** ("user access to kernel data segment 0, cap=0x0000") ->
  "The Swapper stopped" (194 instructions). SINTRAN's RUN then prints "NO WELL DEFINED PROGRAM".
  So the earlier "null-deref" is actually an **unmapped-segment-0** protection violation, and the
  swapper stopping is WHY RUN has no well-defined program.
- Run 2 (blanket-map all 32 segments -> the two tables, an experiment): PASSES the seg-0 trap - the
  swapper runs **876,990 instructions** past 0x913B - but then reads UNINITIALIZED garbage
  (`0x55555555`) from aliased **DATA SEGMENT 13** (VA 0x6800004C, at PC=0x0800878B) and fatal-errors
  -> "system error - ND-500(0) CPU locked / Fatal error from Swapper". RUN still "NO WELL DEFINED
  PROGRAM". **Blanket aliasing is a confirmed DEAD END** (reverted): the swapper needs each segment
  mapped to its OWN correct physical region, not DSEG duplicates.
- **NEXT (Q-MMU-06):** carve what DATA SEGMENT 0 (offset 10) and SEGMENT 13 (offset 0x4C) must
  resolve to for the swapper. These are the two concrete, ordered per-segment blockers to the swapper
  completing cold-start -> which unblocks RUN. The Step-0 SWPINFO instrumentation is moot now: the
  live RIOM dump already shows the message is empty; the issue is the swapper's segment access, not
  the SWPINFO value.


**FIX PLAN (2026-07-20, from `EMULATOR-SWPINFO-GAP-ANALYSIS-2026-07-20.md` + `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`):**
Root cause = the emulator wakes the swapper (process 0, the pager) to process a work message that
does not exist yet. On D4 bring-up nothing is page-faulting behind the start, so `SWMSG.SWPINFO==0`
is LEGITIMATE - the swapper should IDLE, not deref. The emulator never sources SWPINFO itself; it
relies on SINTRAN's `SWMESS` posting it, which only happens when there is real work.
- **Necessary but NOT sufficient for `NLL:`**: gating the idle swapper stops the CRASH but produces
  no running domain. Reaching the prompt also needs Phase 4 (PLACE-DOMAIN + RUN actually posting work
  so `SWPINFO!=0`).
- **TWO OPEN CONCERNS before landing a gate (do not guess):**
  1. The 5800 image dispatches via `OnStartProcessSamson` (context block), NOT `OnStartProcess`
     (`Nd500MicrocodeServicer.cs:525-538`). The gate must go in the path D4 actually uses.
  2. The swapper runs past init and SELF-ANNOUNCES (MON 377B) before 0x913B. A gate that just
     "doesn't wake the run thread" would skip that announce and could break the alive-handshake. The
     correct behavior is: run + announce, then IDLE on no-work - not "never run".
- **CORRECTION 2026-07-20 (static code check):** the investigation's proposed fix (gate
  `OnStartProcess` / don't wake the run thread) is WRONG - the swapper issues its `MON 377B` announce
  from its OWN code at 0x823F (`Nd500CpuProcessBridge.cs:473-475`), BEFORE 0x913B, so not running it
  skips the announce. The swapper MUST run. The real fix lives in the swapper's own idle path: on a
  cold-start with no work it should init + announce + reach an IDLE-WAIT, not deref `r2=0` at 0x913B.
  Finding WHERE that idle branch is (or why we drive past it) needs a LIVE SINGLE-STEP of the swapper
  around 0x0800913B with SWPINFO=0 (dap-debugger + the D4 harness), not just the Step-0 log. The
  Step-0 log still gives the start-time SWPINFO value; the single-step gives the idle-branch location.
- **Gate-1 fix PRE-CLEARED 2026-07-20:** the D4 test `Nd500_D4_RunDomain_RealCpu_Capture`
  (`Nd100SintranNd500BootHarnessTests.cs:1051`) is OBSERVATIONAL - its only assert (line 1156)
  accepts `NLL:` OR `N500:` and fails only on a hang; NOTHING asserts the 0x913B trap as success. So
  parking the swapper can only help. The harness already dumps `LastRiomDecode` + the RIOM buffer
  (`DSEG+0x240BC`) vs ND-100 source (`0x420E30`), so the trace will show both the new START-SWPINFO
  line and the post-hoc buffer. Landing the gate is unblocked once the 3 facts are read.
- **Step 0 APPLIED 2026-07-20 (compiles, 0 errors):** `Nd500MicrocodeServicer.cs` 23B/25B dispatch
  now logs `MAILBOX START-SWPINFO ...` with SWMSG.SWPINFO words[0o104]/[0o105] (HSWPI=0o104 confirmed
  `[V]` L07). Pure logging, no behaviour change. AWAITING a `Nd500_D4_RunDomain_RealCpu_Capture` run
  (Ronny) to read: (a) is SWPINFO zero at start? (b) does this line fire ALONGSIDE the swapper's MON
  377B announce (i.e. announce still happens)? (c) micfu 23B vs 25B. Then land the idle-on-no-work
  behavior in the confirmed start-path.


The swapper executes to `0x913B`, does a correct RIOM, then null-derefs a message pointer
(`w4 laddr r2.(10)` with `r2=0`). This is the literal current stop. Cheapest path forward.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 1.1 | **ROOT CAUSE FOUND 2026-07-20 (carve `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`).** The swapper's first work-message pointer is `SWMSG.SWPINFO` (offset `HSWPI=104`), set by SINTRAN's `SWMESS`/`5ACTSWAPPER` via `CNVWADR` to the activating message. If the swapper is kicked WITHOUT SINTRAN having posted that message, `SWPINFO=0`, the RIOM pulls an empty message, and the swapper null-derefs `r2.(10)` at `0x913B` - the exact D4 stop. So the emulator activates the swapper before/without SINTRAN's SWMESS posting SWPINFO. FIX DIRECTION: ensure SINTRAN's SWMESS runs and posts SWMSG (with SWPINFO) before the swapper reads it, OR that the emulator's activation carries the real SWPINFO pointer. A LIVE probe of `0x420E30` at the RIOM would still confirm, but the mechanism is now known. | CARVE (was LIVE) | A5 / Q-SWP-08 | root cause + fix direction identified [MET] |
| 1.2 | Superseded by 1.1: the empty message is NOT an expected "no work" answer - it is a missing SWPINFO post. | CARVE | A5 / Q-SWP-08 | [SUPERSEDED by 1.1] |
| 1.3 | **Is the swapper's message buffer PRIVATE or SHARED ND-100 memory?** `SetND100PrivateOffset(0)` is unproven. Carve `SWMSG` alloc in `MSINIT`. | CARVE | C4 / Q-SWP-09 | private-vs-shared decided from bytes |

---

## Phase 2 - MMU model correctness (the biggest structural gap)

The emulator hand-builds capabilities because it does not know who builds the real tables. Until
this phase lands, the MMU "cannot converge" (observed: program 0/1; data 0/1/13). Mostly MICROCODE
track; carve fallbacks noted.

**Independent corroboration 2026-07-21** (`SWAPPER-START-CPU-MMU-SETUP-CARVE-2026-07-21.md`,
grep-confirmed): tasks 2.1/2.2/2.3/2.6 all re-verified from the B30 microword listing -
`MM,PSTP`/`MM,PUWP` are written at EXACTLY 4 sites, all CPU-INIT, from CONSTANTS (`INIT_SAM_3`
`014572`/`014573`; macro-start `017731`/`017732` = `PSTP:=0`, `PUWP:=4`), never per-process, never from
the image load address; ZERO `IMM,*`/`DMM,*` page-table WRITES. Per process only DOMAIN+SEGMENT switch
(`MM,PS`/`MM,PHS`/`MM,DOM`/`MM,ADOM` via `CNTXTLOAD`). NEW concrete offsets for the emulator's
process-start model: register context block = `0o4000 + 0o400*proc` BYTES (64-word stride), **P (code
entry) = ctx offset 0x00** (`014757` `IAC,P`), data base A1-A4 = ctx 0x20-0x2C; EXECUTE `014636` resumes
macro fetch at P. Still `[INFERRED]`/UNVERIFIED (feeds 2.6): whether an `MM,PSTP` write fans out to both
IMM+DMM units or they are read-back aliases - model rec: update both on a `MM,PSTP` write.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 2.1 | **RESOLVED 2026-07-20 (agents 1+5 + SINTRAN carve).** NOT the microcode, NOT the swapper, and **NOT SINTRAN either** - `SWMESS` writes only the 5MPM mailbox message, never a context block/PST/PCB. The context block is microcode-written (`CNTXTSAVE`) and SINTRAN-read-only (`CNTXP=57`, `REGBS=200`=0o400 stride = matches `CNTXTLOAD`). PST/PCB appear nowhere in SINTRAN; PSTP is a microcode constant. DISCRIMINATOR: since SINTRAN seeds no context block, the 23B/CNTXTLOAD path would read an unseeded block on a COLD start and could not yield P=4/PS=1; the **ACCP `MACRO_STARTL` cold-start vector** yields P=4/PS=1/DOM=1 as microcode literals -> cold-start fits, 23B is warm-restart. [OPEN] confirm via live dump of `CNTXPAGE+ADRZERO` before first instruction. See `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`. | MICROCODE + CARVE | A1 / Q-MMU-01 | builder resolved (ACCP cold-start); live-dump confirm open |
| 2.2 | **CORROBORATED 2026-07-20 (agents 1+2, consistent citations; spot-check before emulator change).** `PSTP` = physical constant `2`, written ONLY at CPU init (`INIT_SAM_3 @014573: MM,PSTP := SC13`, SC13=PSTBASE=2 @000021); CNTXTLOAD never touches it. Not per-process, not in any handover. EMULATOR FIX: seed PSTP=2 at CPU-init. See `MICROCODE-ANSWER-PSTP-AND-SEGMENT-2026-07-20.md`. | MICROCODE | A3 / Q-MMU-02 | PSTP source identified [MET, spot-check pending] |
| 2.3 | **CORROBORATED 2026-07-20 (agents 1+2+3).** The segment is a SEPARATE register `PS`, loaded from the context block (`NEW_PS_1 @015043`), independent of P. P carries offset only. Cold-start hardcodes `MM,PS:=1`, `P:=4`, `DOM/ADOM:=1`; fetch VA=`(PS<<27)|P`=`0x08000004`. EMULATOR FIX: model PS as a distinct register; do NOT derive a segment from the 32-bit P. Same doc. | MICROCODE | A2 / Q-SWP-03 | segment source identified [MET, spot-check pending] |
| 2.4 | **[PROVISIONAL 2026-07-20, agents 1+3 agree, byte-verified fn dispatch]** MOOT on B30: fn 16/17/20/21 (register examine/deposit + 3RREG/3WREG) ALL = MSG_ILLEG (`MICRO-5800-B30.md:6831-6835`). The microcode never packs halfwords, so word order is a SINTRAN-side/classic convention unresolvable here (no classic-144-bit image exists). ALSO FLAGGED: Ref-Manual Fig 2 register order (TOS/LL/HL/THA after R) conflicts with our B1 assumption - our current 21B block layout (task #15) may be wrong. Start is context-block, not register-image. See `MICROCODE-ANSWER-21B-REGISTER-ORDER-2026-07-20.md`. | MICROCODE | Q-SWP-04 | word order fixed from evidence [MOOT on B30/PROVISIONAL] |
| 2.5 | **CONFIRMED 2026-07-20 (agent 5, swapper bytes).** MMU is already ON at entry - the swapper never enables translation (PMON/PMOF absent), runs translated in segment 1, and only drops data translation transiently via DMOF (always restored by DMON). EMULATOR: current "enable both program+data MMU at start" is CORRECT. See `CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md`. | MICROCODE/CARVE | A4 / Q-MMU-03 | start MMU state decided [MET] |
| 2.6 | **ANSWERED 2026-07-20 (agent 4): the walk is HARDWARE, not microcode.** Two units IMM+DMM each hold PSTP/PS/DOM/ADOM; the microcode only loads pointers and reads units back on fault - it never reads PCB/PSTE/PTE from memory. So the walk cannot be byte-verified from B30. CAVEAT: the PCB 256B / pcb_pc@0 / pcb_dc@64 / PFN==0 / PSTE encodings we model come from `ND500_MMU_SPECIFICATION.md` which is RE'd from **NDIX-C (Unix), a different OS** - same silicon, but software-sourced, not microcode-proven. Do not treat those offsets as verified. `MICROCODE-ANSWER-MMU-WALK-AND-ENABLE-2026-07-20.md`. | MICROCODE | Q-MMU-04/05 | walk confirmed HW / encodings remain software-sourced |
| 2.7 | **Correct capability set for the swapper at start**; is segment 13 (`0x68000044`) meaningful? Current all-64-caps-same-tables is a declared hack. | MICROCODE | Q-MMU-06 | real cap set installed |
| 2.8 | **Segment-capability bit layout** (contradiction C9): claim A (bit13=S,12=P,11=W, 11-bit seg) vs claim B (W=15,P=14,S=13, 12-bit). | MANUAL ND-05.009.4 | Q-MMU-08 | layout resolved |

---

## Phase 3 - Transport + descriptors (mostly CLOSED, verify residue)

The swapper PSEG/DSEG delivery question (old B1 / Q-SWP-01) is **CLOSED**: `MON 131` ABSTR disk DMA
into ND-100 physical memory, page from `MON 61` FIXC5 (register section 2.0). Remaining residue:

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 3.1 | **Decode the descriptor SINTRAN DMAs to `0x6F000`** (`02 C0` at +3). May answer 2.1/2.3. | CARVE | B2 / Q-SWP-02 | descriptor decoded |
| 3.2 | **ANSWERED 2026-07-20 (`MICROCODE-ANSWER-TRAP-REPORT-FIELDS-2026-07-20.md`).** Layout (vendor 13.16): STOPR@link.11, trapping P@12, restart P@14, TRAPN@16, fault params (LA+status) in link.17..22. LA = FULL 32-bit VA (seg = top 5 bits, offset = low 27) - our LA/segment were RIGHT (`0x080081A5`->seg 1/100645B matched SINTRAN). Access type is COMPOSITE: prog-vs-data = fault class (IMM TRAP_IFC vs DMM TRAP_DFC), read-vs-write = WR bit in the MMS status word, keyed by TRAPN. ROOT CAUSE of the wrong print: `MMS_SIX0`=`0xC0000000` = class in the TOP 2 bits (hardware bit order); the emulator likely built the status word in software-struct bit order -> SINTRAN mis-decoded fetch as DATA READ. FIX: correct trap number per fault class + status word in hardware bit order + LA at the per-trap offset. OPEN: exact per-trap offset in link.17..22 + WR/class bit numbers (DECOERRMESS carve / live single-step TRAP_GEN3 @013534). | MICROCODE | C5 / Q-TRP-01 | trap-report layout fixed [MET, bit-numbers open] |

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
register 2.3. Shares B1-class answers.

**PROMOTED 2026-07-21: task 5.1 (the CS-load "Loading Swapper" verify stall) is now the Track A
CRITICAL PATH** - see the UPDATE block at the top. The earlier "not on the 3022 critical path" framing
is RETRACTED: dual-ring evidence shows PLACE-DOMAIN never leaves the CS-load verify loop (PC 0xDAD3), so
`ENDPL`/`SPLAC` never build `S500S` and RUN can never accept the domain. Concrete 5.1 sub-question for
this track: on the ND-5800 image, why does the ND-100 CS-load driver never emit `RETG5:=0` and never
exit `[0xD000..0xDAD3]` - i.e. what read-back does the "Loading Swapper" verify wait on that never
arrives (live single-step of the CS-load verify + the servicer's CS readback path). Acceptance: the
ND-100 leaves the verify loop and reaches `SPLAC`/`ENDPL`.

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

## Phase 7 - Track B: run the GENUINE 128-bit CS swapper on CpuND5000 (NEW 2026-07-21, parallel)

The functional `CpuND500` cannot execute the 5800's 128-bit control-store swapper, which is why Track A
substitutes the classic SWAPPER-K01 macrocode and fakes the CS-load. The real microcode CPU `CpuND5000`
(microcode session, skill `nd5000-microcode`) CAN execute it, so Track B runs the swapper the image
actually ships - making CS-load real and removing the stand-in's protocol-mismatch risk. Jointly owned;
coordinate via `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-*` docs. The mailbox differential oracle is the
cross-check between the two engines.

| # | Task | Route | Closes | Acceptance |
|---|---|---|---|---|
| 7.1 | **Wire `CpuND5000` as the octobus ND-500 CPU in the D4/boot harness** (alongside, or swapped for, the functional `CpuND500`). Establishes whether the real microcode swapper even boots from the loaded CS on this image. | CODE + MICROCODE | - | CpuND5000 attached; ticks the loaded CS without faulting on attach |
| 7.2 | **Resolve the octobus completion-wake blocker** (STATUS stall): servicer answer/ring side is byte-verified (finding #3), so the gap is SINTRAN-side `XN500`/`CHN5STATUS`/`5RRTWT` (or ND-100 ident delivery). Carve `XN500`'s drain: what list it walks, how it matches the parked `ITO500XQ` process to the answered msg, MAILINK vs X5FIF ring. | CARVE + LIVE | octobus STATUS stall | parked ND-100 process wakes; STATUS table prints |
| 7.3 | **Does the real CS-load complete on CpuND5000** (the "Loading Swapper" verify), i.e. does Track B sidestep Track A's Phase 5.1 stall by actually loading + running real microcode rather than faking it? Compare RETG5:=0 emission live. | LIVE | Q-CSL (Track B) | ND-100 CS-load driver emits RETG5:=0 and proceeds |
| 7.4 | **Run the real swapper cold-start on CpuND5000** and observe whether it parks cleanly (no fn-5/empty-MESSBUFF crash) - the genuine microcode may produce the completion the stand-in cannot. Feed any divergence back through the oracle. | LIVE + MICROCODE | Phase-1 root cause (Track B view) | swapper parks; mailbox answers built |
| 7.5 | **Converge Track A and Track B at RUN**: whichever track first has a parked/alive swapper + built `S500S`, drive PLACE-DOMAIN -> RUN -> `NLL:` (shared Phase 4). | LIVE | D4 | `NLL:` reached |

---

## Critical path to D4 (REWRITTEN 2026-07-20 after `CARVE-RUN-TO-WORK-POSTING-CHAIN-2026-07-20.md`)

> **AMENDED 2026-07-21 (see the UPDATE block at the top):** the minimum chain below is still correct,
> but the CURRENT stop is NOT the swapper 0x913B crash - it is the CS-load "Loading Swapper" verify
> stall UPSTREAM of it (Phase 5.1, now Track A critical path): PLACE-DOMAIN never leaves the verify loop
> (PC 0xDAD3), so `ENDPL`/`SPLAC` never build `S500S` and RUN never reaches GATE 2. GATE 1 (swapper
> park) is downstream of clearing that stall. Track B (Phase 7) attacks the same finish line via the
> real microcode swapper in parallel.

**Key reframe:** the `NLL:` prompt is printed by the DOMAIN/loader's own MON call routed through
`DECOMESS` -> `MCHANDEL` - **the swapper is NOT involved** in reaching `NLL:`. The swapper's work
chain (`5ACTSWAPPER`) only becomes load-bearing if the domain demand-pages; a RESIDENT-placed
`LINKAGE-LOAD-H02` (the 14B RESIWR burst) can reach `NLL:` with the swapper IDLE.

The carved chain: RUN -> RUNN precondition [V, task #13] -> **3START the DOMAIN (23B)** -> domain
executes on CpuND500 -> STOPs for service -> `DECOMESS`@135161 reads STOPR -> `MOCALL` -> `MCHANDEL`
prints `NLL:`  (page-fault branch `TRAPCODE`/`TRAPN=46` -> `5ACTSWAPPER` -> swapper only if paging).

**The current stop is UPSTREAM of the domain ever running:** the swapper's own cold-start (process 0)
null-derefs at 0x913B and never PARKS, so RUN still fails "NO WELL DEFINED PROGRAM" and the domain is
never 3START'd. So the minimum chain to `NLL:` is now:

1. **GATE 1 (Phase 1):** swapper cold-start completes + self-announces + PARKS on no-work (SWPINFO=0)
   instead of null-derefing. (Step 0 instrumentation applied; behavior fix pending the harness trace.)
2. **GATE 2 (Phase 4):** RUN 3STARTs the placed domain on the CpuND500 (23B, already real in the servicer).
3. **Domain runs** and issues its MON call; `DECOMESS` -> `MCHANDEL` prints `NLL:`. The emulator must
   deliver the domain's STOP messages faithfully (MON call = STOPR=MOCALL). Hand-posting SWPINFO is
   NOT a valid shortcut.
4. **D4 assert:** `NLL:` reached in `Nd500_D4_RunDomain_RealCpu_Capture`.

Phase 2 (MMU anchors, RESOLVED) matters for the domain running correctly; it is not itself the stop.
The swapper-work chain (5ACTSWAPPER/paging) is OFF the critical path if the domain is resident.
Phases 0, 3, 5, 6 are off the critical path.

---

## Dependency notes

- **Phase 2 is the true bottleneck** and is mostly owned by the microcode track, which is currently
  rate-limited (weekly API limit resets Jul 22, 1pm Oslo). Phases 0, 1.1, 3.1, 4.2 are things this
  track can do now without them.
- Phase 1.1 (LIVE probe) is the single cheapest next action and needs no external answer.
- Do NOT start Phase 4.1 subfunction extraction before Q-DOM-08 (4.4) proves where `N500M`'s body
  is - the briefing forbids implementing subfunctions as fact.
