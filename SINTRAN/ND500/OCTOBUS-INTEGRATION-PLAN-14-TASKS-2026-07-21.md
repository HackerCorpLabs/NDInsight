# Octobus ND-500/5000 integration plan - the 14 tasks (2026-07-21)

**Full path:** `SINTRAN/ND500/OCTOBUS-INTEGRATION-PLAN-14-TASKS-2026-07-21.md`

**Goal (D4 acceptance):** a real LINKAGE-LOAD-H02 domain runs on an ATTACHED ND-500 CPU under live
SINTRAN III L on the ND-5800 image, over the octobus interface, and reaches the `NLL:` prompt.

This consolidates the octobus integration tracker (6 done / 1 in progress / 7 open) into one ordered
plan with a concrete next action + acceptance per task. Companion docs: the carving-track plan
`D4-PLAN-PHASES-AND-TASKS-2026-07-20.md` (swapper/MMU carves) and the status of record
`ND500-STATUS-AND-INDEX.md`. Rules: carved bytes / live-harness / differential-oracle evidence win;
mark every claim [V]/[INFERRED]/[OPEN]; UNKNOWN beats a plausible wrong answer.

**Two tracks, run in parallel (Ronny decision 2026-07-21):**
- **Track A** - functional `CpuND500` + SWAPPER-K01 macro stand-in; fix the CS-load "Loading Swapper"
  verify stall (the current dominant upstream blocker).
- **Track B** - the REAL microword `CpuND5000` executes the genuine 128-bit CS swapper the image ships.
- **Cross-check** - the mailbox differential oracle (`MailboxOracleRunner`, microcode vs servicer,
  141/141 byte-parity) adjudicates any engine disagreement.

---

## The 14 tasks

| # | Task | Track | Status | Next action / acceptance |
|---|------|-------|--------|--------------------------|
| 1 | Octobus frame + ACCP command layer (presence/selftest/CS-load/LPARP/VPARP) | A+B | **DONE [V live]** | ACCP bring-up completes: MFACK -> selftest -> 128x LCS0 -> STAMIC0 -> ENKICK. |
| 2 | Mailbox base self-discovery from loaded CS (START_MESS/SAMSON_CPU) | A+B | **DONE [V live]** | header=mpmStart+START_MESS(0x8800)=0x428800, ext=+0x428900; matches live writes. |
| 3 | Mailbox answer engine (`Nd500MicrocodeServicer`): N5STA lifecycle + MICFU dispatch | A+B | **DONE [V oracle]** | copy family 28/28 + 3RMICV byte-exact vs the real B30 microcode. |
| 4 | Answer-ring insert (X5FIF) = WINDOW-RELATIVE BYTE offset (finding #3) | A+B | **DONE [V]** | `SYS_DATAF 025636`/`GIVEINT 025427`: base used directly as a byte addr; servicer routed through `ResolveMailboxLink`; oracle 141/141. |
| 5 | Differential oracle: same mailbox tests through microcode CPU AND servicer | A+B | **DONE [V]** | `MailboxOracleRunner` [Values] Engine; parity asserted. |
| 6 | Full 21B register/context block applied at start (task #15 legacy) | A | **DONE [V]** | 181 R1/R2 tests green; `ApplyRegisterBlockValue`. |
| 7 | **Diagnose octobus X5BEX-resolves-to-zeros stall (STATUS / START-SWAPPER)** | A+B | **DONE [V live]** | RE-BASELINE (below): root cause NAMED - it is NOT a mailbox-wake failure. The wake path runs (XN500/CHN5STATUS/5RRTWT all fire, PIL=12); "resolves-to-zeros" = the ND-5000 CPU *state* dump reads all zeros = CpuND500/CpuND5000 integration gap. Rolls into tasks 9/11. |
| 8 | **CS-load "Loading Swapper" verify stall** (Track A dominant blocker) | A | OPEN | Why the ND-100 CS-load driver never emits `RETG5:=0` / never exits `[0xD000..0xDAD3]`; live single-step the verify + servicer CS-readback. Acceptance: ND-100 leaves the verify loop, reaches `SPLAC`/`ENDPL`. |
| 9 | **Config plumbing (G2): CpuTypeAndModel / SystemParameters / ACCP identity** | A+B | OPEN | Model the CPU-type/model + system-parameter + ACCP identity so SINTRAN's generation checks (`CPUAVAILABLE & 7 == 3 SAMSON`) and 3RMICV report agree with the loaded image. Acceptance: no generation-mismatch; values sourced from the loaded CS, not hardcoded. |
| 10 | **Production wiring (G1): machine-level `AttachNd500Cpu` for classic 3022** | A | OPEN | Bring the 3022/`NDBusND500IF` path to the same one-call machine attachment the octobus `AttachNd5000Cpu` has. Acceptance: `AttachNd500Cpu` wires CPU + shared MPM + doorbell + run thread; a wiring test passes. |
| 11 | **Wire `CpuND5000` into the D4/boot harness (Track B start)** | B | **BLOCKED [V code]** | NOT wireable today: the whole attach path is typed to the functional `CpuND500` - `AttachNd5000Cpu(... CpuND500? cpu)` (`ND100Machine.ND5000.cs:108/134`) -> `octobus.AttachCpu(CpuND500,station)` -> `stationObj.AttachRealCpu(CpuND500,...)`. Wiring the microword CPU needs a SHARED interface (or overloads) across octobus station + `Nd500CpuProcessBridge`, which straddles `CpuND5000.cs` (microcode session's file). Needs a coordination decision before it can proceed. Acceptance: shared attach interface exists; CpuND5000 ticks the loaded CS without faulting on attach. |
| 12 | **RUN/init command routes operator RUN to the attached CpuND500** | A+B | OPEN | Operator RUN -> RUNN precondition (task #13 [V]) -> `3START` the placed domain on the attached CPU. Acceptance: RUN issues 23B to the CPU for a placed domain (no "NO WELL DEFINED PROGRAM"). |
| 13 | **Completion wake: parked `ITO500XQ` process restarts on answer** | A+B | **DONE [V live]** | RE-BASELINE: the wake already works - `XN500`(135123)/`CHN5STATUS`(135205)/`5RRTWT`(132352) all execute PIL=12 (181/105/31 hits); the parked process restarts. Do NOT chase the ident-routing hypothesis (DISPROVEN). |
| 14 | **Acceptance: green D4 - RUN LINKAGE-LOAD-H02 reaches `NLL:`** | A+B | OPEN | `Nd500_D4_RunDomain_RealCpu_Capture` reaches `NLL:` (harness is FLAKY - take >=2 samples). Acceptance: `NLL:` printed. |

(8 DONE, 5 OPEN. RE-BASELINE 2026-07-21 closed tasks 7 + 13: the completion wake is NOT the blocker.
The remaining real blockers are task 8 (CS-load "Loading Swapper" verify stall, Track A) and the
CPU-STATE integration tasks 9 + 11 (the ND-5000 status/process dumps read all zeros because the
attached CPU state is not real). 10/12 are wiring; 14 is the gate.)

---

## RE-BASELINE 2026-07-21 [V, live boot-harness + ND-100 PC instrumentation] - the wake is NOT broken

A boot-harness run (`FullFlow_Octobus_...`) with a temporary ND-100 PC-hook on the wake routines
DISPROVED the prior diagnosis. GROUND TRUTH:
- **`XN500`(135123), `CHN5STATUS`(135205), `5RRTWT`(132352) ALL execute on PIL=12** (181 / 105 / 31
  hits, ran 317 times then stopped on its own - not an infinite loop). So the octobus answer interrupt
  DOES reach the input/drain path and the parked-process restart (`5RRTWT`) fires. **The completion wake
  WORKS.**
- **The ident-routing hypothesis (40B -> OOCT0) is DISPROVEN** - do NOT "fix" the octobus input ident;
  it correctly reaches `IOCT0`/`5STDRIV`/`XN500`. (The `NDBusOctobus.cs:1774-1778` OPEN QUESTION is a
  real doc ambiguity but NOT the wake bug.)
- **The boot goes MUCH further than the memory recorded** [V console
  `sintran-octobus-capture-octobus-fullflow.txt`]: `@nd-500` -> J04 monitor -> `memory-configuration`
  (full table) -> `status` ("> Loading Control Store"/"> Loading Swapper" + register dump) ->
  `process-status` prints a REAL process table (`Proc 1 SYSTEM idle`) -> `start-swapper` reached.
  The `status=STALL`/`list=STALL` in the OUTCOME line are the harness's prompt-marker detector missing a
  marker, NOT a hang - the commands produce real output.
- **"X5BEX-resolves-to-zeros" = the STATUS register dump reads all zeros** (ZERO/CARRY/SIGN/FLAG/OVERFLOW
  = 0) i.e. the ND-5000 CPU *state* is not real - a CpuND500/CpuND5000 integration gap (Track A/B, tasks
  9/11), NOT a mailbox-wake failure. Also seen early: `ND-5000 timeout: ACCP was terminated; Microprogram
  has stopped` - to be characterised.

**So task 7 is effectively RESOLVED as diagnosed (the wake works); the remaining octobus work is the
CPU-STATE integration (real register values in the status/process dumps) + the harness prompt markers,
NOT the completion wake.** The stale "completion-wake blocker" framing in `nd5000-octobus` / the
octobus skill / `nd500-d4-path-to-nll` should be corrected.

## OBSOLETE (superseded by the RE-BASELINE above) - the X5BEX-resolves-to-zeros / completion-wake stall

**What is measured [V, live + writes-only MPM log]:** the STATUS command's ACCP flow COMPLETES
(LSSYSPAR ... 128x LCS0 ... STAMIC0 ... ENKICK ... TRAP_OCBM model/version), then the monitor HANGS.
The ND-100 has issued its `MON 60` RSTAT ONCE and PARKED in the ND-500 exec queue `ITO500XQ` - there
is NO poll; completion is async. The servicer walks `X5BEX=0xBE30 -> node 0x42BE30` whose `N5STA=0`
(a list/queue header), follows the LINK to the real 3RMICV message, and ANSWERS it (N5STA:=3 + ring
insert + GIVEINT ident) - this side is byte-verified against the real microcode. **But the parked
process never wakes**, so SINTRAN re-posts the request in a retry loop and STATUS stalls.

**So the gap is the ND-100-side WAKE, not the ND-500 answer.** The wake path (NPL, logic only - carve
the bytes to confirm): the octobus completion interrupt -> `5STDRIV`/`XN500` (`MP-P2-N500.NPL:134607`/
`134723`) drains the X5FIF ring from `X5HEN` to `X5FYL` -> `CHN5STATUS` (`135004`) reads each entry's
`N5STA` -> on ANSWER, `CALL 5RRTWT` (`132152`) restarts the parked ND-100 process (`ACTRT`/ITO500XQ).

**Diagnostic results (2026-07-21):**
1. **Differential chain walk - DONE [V oracle].** `MailboxOracleRunner.RunChainToAnswer` +
   `X5BexChain_LeadingZeroStaNode_RealMessageAnswered_BothEngines`: the microcode CPU and the servicer
   behave IDENTICALLY on the `X5BEX -> N5STA=0 head -> real message` chain - both walk past the zero-STA
   head node, answer the real message (N5STA:=3), insert its byte offset into the ring (`ring[0]=0x2A00`),
   and advance X5FYL to 1. **So the ND-500 answer side is NOT the problem; the blocker is purely the
   ND-100 completion wake.**
2. **XN500 wake path carved [NPL-logic, cross-check bytes pending].** `XN500 @134723` drains the X5FIF
   ring `WHILE X5HEN != X5FYL`; for each entry it converts the ring value to the message address
   (`*CNVBYADR ... =:N5MESSAGE`), reads the message's `5MSFL` word, and **only calls `CHN5STATUS` IF
   `5MSFL BIT 5IEXQ` (5IEXQ = 0o15 = BIT 13, the "in ex-queue" flag)** [`MP-P2-N500.NPL:715`].
   `CHN5STATUS @135004` then reads N5STA: `ANSWER(3)` -> `DECOMESS` (-> `MCHANDEL`/restart);
   `5ERANSWER(4)` -> `DECOERRMESS`; `MSGN500(1)`/`WAITING(2)` -> `XTER500` (timeout). Symbols confirmed
   L07: X5HEN=w3, X5FYL=w4, X5MXF=w5, X5FIF=w6 (= finding #3 byte offsets), 5IEXQ=BIT 13.
3. **Emulator doorbell is already correct [V code].** `OctobusND5000Station.AnswerWritten` fires the
   level-13 input ident (40B) = "the XN500 drain doorbell", and the servicer advances X5FYL + inserts the
   ring entry (finding #3). So the trigger + ring are wired.

**PRIME SUSPECT (task 13):** the answered message's `5MSFL BIT 5IEXQ` (bit 13) is CLEAR - the servicer
answers a message that is NOT the one SINTRAN parked into the ex-queue (consistent with the observed
retry loop re-posting a fresh 3RMICV each cycle), so `XN500` drains the ring entry but SKIPS
`CHN5STATUS` -> no restart. Alternative: the servicer's ring-entry format does not match what
`*CNVBYADR` expects, so `N5MESSAGE` resolves to the wrong message.

**STATIC ANALYSIS EXHAUSTED (2026-07-21).** Every emulator piece on the answer side is verified correct:
- the answer + ring insert are byte-exact vs the real microcode (differential oracle, `X5BexChain` test);
- the ring-entry format is therefore what real hardware produces (so `CNVBYADR` will resolve it);
- `AnswerWritten` fires the level-13 GIVEINT frame, and inbound octobus frames DO raise the ND-100
  level-13 input interrupt (ident 37B, `ITB13` datafields, `NDBusOctobus.cs:851-853`) = the `XN500`
  doorbell path.
**STRONG LEAD (2026-07-21, carve): the answer interrupt may vector to the WRONG SINTRAN handler (ident
routing), so `XN500` never runs.**
- SINTRAN L07 level-13 ident table [V symbols]: `ITB13=154075`; `ITB13+37B -> IOCT0=123511` (INPUT
  driver -> `5STDRIV` -> `XN500`); `ITB13+40B -> OOCT0=123537` (OUTPUT driver). So the wake needs the
  answer interrupt to arrive on the INPUT ident that maps to `IOCT0`.
- The emulator octobus card sets interface-0 INPUT ident = **40B** (`NDBusOctobus.cs:1785`), which in
  SINTRAN's table is `OOCT0` (OUTPUT). This is flagged as an UNRESOLVED OPEN QUESTION in the code itself
  (`NDBusOctobus.cs:1774-1778`): TPE's `LIST-OCTOBUS-DEVICES` shows Receive=40B, but the carved ITB13
  says +40B=OOCT0. The 37B/40B-vs-40B/41B conflict was never settled. If SINTRAN indexes ITB13 directly
  by the returned ident, an input frame with ident 40B runs `OOCT0` (output), NOT `IOCT0`/`XN500` -> no
  wake. THAT would fully explain "answer written correctly, process never wakes".
- nd5000 wake DIFFERS from classic: `LOWACT500` (`CC-P2-N500.NPL:319`), which installs `5STDRIV` as the
  level-12 driver, EXITS IMMEDIATELY on nd5000 (`*NNJ03=* EXIT`). So the classic level-12 activation is
  bypassed; the nd5000 wake routing (which level-13 ident/handler reaches `XN500`) must be carved.

**Next (AUTONOMOUS carve):** resolve the ITB13 index model - does SINTRAN's level-13 dispatch index
`ITB13` directly by the returned ident (so 40B=OOCT0=output), or via a base/offset (the code's "re-read"
hypothesis)? Carve the level-13 ident dispatch + the nd5000 octobus interface install (which ident it
arms for INPUT that reaches `IOCT0`/`5STDRIV`/`XN500`). Then a unit test can assert the emulator raises
that ident. This is checkable without a full boot; a live run only CONFIRMS the fix end to end.

---

## LIVE FINDING 2026-07-21 [V, classic-3022 D4 run] - swapper crash is an EMPTY message body, not a null pointer

A `Nd500_D4_RunDomain_RealCpu_Capture` run (classic 3022 / `CpuND500`) captured the swapper's MON 377B
exchange decisively:
- SINTRAN's RESTART write-back delivers a **NON-ZERO** SWPINFO pointer `@0x080240B4 := 0x00210718`
  (= ND-100 byte 0x420E30 = MESSBUFF) + message-control `@0x080240B0 := 5`.
- The MESSBUFF the pointer addresses (`0x420E30`) is **all zeros**; RIOM pulls 15 zero words, and the
  swapper null-derefs at `PC=0x0800913B` (MMU PV, read addr 0x0A, r2=0) -> `CRASHED`.
- This **disproves** `EMULATOR-SWPINFO-GAP-ANALYSIS-2026-07-20.md`'s "SWPINFO reads zero" premise (the
  pointer is fine; the body is empty) and its option-1 fix (a `SWPINFO==0` gate never fires). Corrected
  in that doc's LIVE CORRECTION header. Root remains the D4 RUN-precondition: no real domain activation
  -> MESSBUFF never populated. [OPEN: carve message-control=5 + empty-body semantics before any gate.]

## Dependencies / ordering

- **Task 8 (CS-load verify stall) gates RUN on Track A** - `ENDPL`/`SPLAC` build `S500S` from swapper
  mailbox answers; RUN cannot accept the domain until CS-load clears. It is the near-term critical path.
- **Task 13 (completion wake) gates STATUS/START-SWAPPER** on both tracks - the parked process must wake.
- **Tasks 9/10/11 (wiring)** have no carve dependency and can proceed now (CODE).
- **Task 14** is the gate; needs 8 (or Track B 11+8-equivalent) + 12 + 13.
- Task 7's differential step is the single cheapest next action and needs no external answer.
