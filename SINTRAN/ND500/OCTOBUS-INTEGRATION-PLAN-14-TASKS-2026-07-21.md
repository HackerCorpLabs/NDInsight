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
| 8 | **S3SM5 swapper-table software wait** (Track A dominant blocker; RENAMED from "CS-load verify stall") | A | **OPEN - CONFIRMED ROOT [V live+disasm]** | S3SM5 spins NON-terminating in `[0xC499..0xDAD3]` (1.5M+ iters), never reaching its MSWIN body-fill (octal 162155) -> empty body -> 0x913B. CORRECTION 2026-07-21c (`CARVE-S3SM5-CSLOAD-VERIFY-LOOP-2026-07-21.md`): this band is NOT a 3022 CS-load poll (ZERO 3022 IOX in it [V]); it is S3SM5 PLANC swapper-management code (MON 116B/50B/43B/61B/76B/217B + table scans) doing a SOFTWARE WAIT on ND-100 table state that the REAL swapper (proc 0) would build by answering the swapper msg + building descriptor/segment/process tables (5ACTSWAPPER/XACTRDY/LSWPWAIT). Faked functional swapper never runs/answers -> the wait never flips. **FIX IS SERVICER-SIDE (functional swapper builds the tables), NOT a 3022 register** (NDBusND500IF CS-load/RETG5/5CLOST already modeled). [OPEN=exact polled cell + tight loop; needs a live PC histogram over the band.] Acceptance: S3SM5 leaves the wait, reaches its MSWIN builders / `SPLAC`/`ENDPL`. |
| 9 | **Config plumbing (G2): CpuTypeAndModel / SystemParameters / ACCP identity** | A+B | OPEN | Model the CPU-type/model + system-parameter + ACCP identity so SINTRAN's generation checks (`CPUAVAILABLE & 7 == 3 SAMSON`) and 3RMICV report agree with the loaded image. Acceptance: no generation-mismatch; values sourced from the loaded CS, not hardcoded. |
| 10 | **Production wiring (G1): machine-level `AttachNd500Cpu` for classic 3022** | A | OPEN | Bring the 3022/`NDBusND500IF` path to the same one-call machine attachment the octobus `AttachNd5000Cpu` has. Acceptance: `AttachNd500Cpu` wires CPU + shared MPM + doorbell + run thread; a wiring test passes. |
| 11 | **Wire `CpuND5000` into the D4/boot harness (Track B start)** | B | **DESIGNED [V] - unblock path defined** | Design done (`TRACKB-SHARED-ND500-CPU-INTERFACE-DESIGN-2026-07-21.md`): HYBRID = extract a small RetroCore interface `INd500ProcessCpu` (run-thread lifecycle + `ParkOnIdle()`) that `CpuND500` implements (no body changes) + a `CpuND5000Adapter` in `Emulated.HW`. **`CpuND5000.cs` needs NO changes** (adapter reaches it via public `Cs`/`Regs`/`State`/`Memory`/`Tick()`/`Run()`/`RaiseTrap()`); only 2 OPTIONAL conveniences requested (coordination doc `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-SHARED-CPU-INTERFACE-2026-07-21.md`). Retype `AttachRealCpu`/`AttachNd5000Cpu`/`Nd5000CpuAttachment.Cpu` to the interface; add `AttachMicrocodeCpu` that skips the functional bridge. [OPEN=boot-from-CS: CpuND5000 boots from the loaded 128-bit CS + owns the mailbox, so the station's CS-load must land in `CpuND5000.Cs` (DUCS checksum preserved) and the C# servicer/bridge is DISABLED for it.] Acceptance: CpuND5000 ticks the loaded CS without faulting on attach. |
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
  in that doc's LIVE CORRECTION header.
- Reconciled with the Q-MMU-06 swapper-asm carve (memory `nd500-d4-path-to-nll`): control=5 is fn code
  **MSWIN** ("init/activate working set", swapper handler idx 5, the deepest paging worker - NOT a
  no-op). SINTRAN legitimately posts this reason via `5ACTSWAPPER @144762B` (MP-P2-N500.NPL:2857),
  copying `SWFUN=MSWIN` + `CNVWADR(requester msg)=0x210718` into SWMSG. So it is NOT "no activation".
  The empty part is the **REQUESTER's MESSBUFF body at 0x420E30** (a reused 200B/process-1 buffer),
  which its **SENDER never filled** - and that sender is OUTSIDE the carved NPL tree (placement /
  segment-admin / init, possibly uncarved 030-S3SM5). **THE real [OPEN]: carve who posts the MSWIN
  message and why its body is empty.** Do NOT build a gate on control=5 (it is a legitimate work
  reason; genuine "no work" is the LNEWSWAP-EMPTY path that zeroes HSWPI and never restarts the swapper).

## LIVE FINDING 2026-07-21b [V, gated ND-100 trace] - S3SM5 CS-load verify loop is the wall; the empty body is downstream

Ran the D4 harness with the place-domain trace gate widened to `[0xC1F0,0xE480)` to span BOTH S3SM5
message builders (MSWIN swap-in @octal 140771=0xC1F9, body-fill @octal 162155=0xE46D). Result:
- The traced place-domain execution (1.5M in-range instructions, cap hit = "loop did not terminate")
  is CONFINED to `[0xC499, 0xDAD3]` = S3SM5's CS-load "Loading Swapper" VERIFY LOOP. Cross-checked
  against `030-S3SM5.dis`: that band, and both builders, are ALL S3SM5 code (140771=`LDA ,B -77`,
  162155=`LDD ,X -26`, 155323/0xDAD3=`STZ ,B -200`), so the executing overlay IS S3SM5 (no overlay
  confound).
- **Neither builder (0xC1F9, 0xE46D) is ever reached.** S3SM5 spins 1.5M+ non-terminating iterations
  in `[0xD000..0xDAD3]` and never advances to its own MSWIN body-fill code.
- **CONCLUSION [V]: the swapper's MSWIN body is empty because S3SM5 is STUCK in the CS-load
  "Loading Swapper" verify loop and never runs the fill (162155).** So the causal chain is:
  CS-load verify stall (task 8) -> S3SM5 never fills the body -> swapper derefs empty body ->
  `0x913B` crash. **Task 8 is the confirmed root; the empty body + 0x913B crash are downstream
  symptoms.** This validates the memory's Track-A critical-path call from the message-builder angle.
- The reason-5 + pointer the swapper's MON 377B receives is a partial/stale SWMSG set up before the
  fill; the 15-word body fill (162155) is the step that never runs.
- **CORRECTION 2026-07-21c [V disasm, `CARVE-S3SM5-CSLOAD-VERIFY-LOOP-2026-07-21.md`]:** the band is
  NOT a "CS-load verify loop" as labelled above - there is ZERO 3022 IOX in it. It is S3SM5 PLANC
  swapper-management code (MON 116B/50B/43B/61B/76B/217B + table scans) doing a SOFTWARE WAIT on
  ND-100 table state the REAL swapper would build. So the task-8 FIX is SERVICER-side (make the
  functional swapper answer + build the descriptor/segment/process tables), NOT a 3022 register.
- **HISTOGRAM DONE 2026-07-21 [V, live D4 trace PC histogram]:** the non-terminating hot region is
  PINNED = OUTER scan loop octal **155205..155303** (0xDA85..0xDAC3, 585 non-converging iters,
  back-edge **155225 `JMP -103 ->155122`**) calling the LEAF chain-walk **155310..155323**
  (0xDAC8..0xDAD3, `LDX ,X ,B -41`/`LDX ,X ,B -51` + `JPL I 126 ->155450`). A table/chain scan
  spinning, not a single-cell poll; scans via mem[27]/mem[26] table pointers + B-relative locals,
  exit tests `JAZ`@155264 + `BSKP ONE 10 DA`@155302.
- **FINAL BYTE-VERIFIED 2026-07-21e** (`CORRECTION-HOT-LOOP-IS-IOX-POLL-NOT-S3SM5-2026-07-21.md`):
  the hot loop **IS `030-S3SM5`** (the `.bin` at these PCs matches the RUNTIME trace byte-for-byte:
  0xDA50=`D64F` MON117B, 0xDAB3=`BA14`, 0xDAC8=`CC7E`, 0xDACA/0xDAAD=`D10D` IOXT). There is NO overlay
  confound. What was wrong is the agent-generated **`030-S3SM5.dis` FILE - it is CORRUPT** (its words
  disagree with the `.bin` at the same addresses), which caused BOTH the "cell 27B table scan"
  (da22546/e830dda) AND my "not-S3SM5" claim (85b446b). BYTE-VERIFIED TRUTH: the hot loop is an
  `030-S3SM5` **device-poll-with-timeout** at runtime 0xDA40..0xDAD0 (base 0x4000): `MON 117B` @0xDA50
  + dynamic `IOXT` @0xDAAD/0xDAB7 (device addr `[[B-2E]-3]+0xB`) + retry counter `[B-7A]` + `RDIV` 100;
  it polls device/swapper readiness, times out, prints "The Swapper stopped".
  **CASCADE: the corrupt `.dis` also underlies `CARVE-S3SM5-MSWIN-STAMP-AND-FILL-...` (builders @140771/
  162155, MICFU/SWFUN stamp) and `CARVE-S3SM5-CSLOAD-VERIFY-LOOP-...` (cell 27B) - those address-level
  S3SM5 claims are now SUSPECT and must be re-derived from a CORRECT disassembly.** Still SOLID (runtime/
  byte-verified): empty MSWIN body -> 0x913B crash; place-domain spins in this poll then times out.
  **NEXT: regenerate a CORRECT `030-S3SM5` disassembly (diagnose the .dis tooling bug - byte-order/
  alignment), then decode MON 117B + the IOX readiness register + why the emulator never satisfies it.**
  METHOD LESSON: compare the executed/`.bin` WORD, not just the address, before trusting a `.dis`.

## Dependencies / ordering

- **Task 8 (CS-load verify stall) gates RUN on Track A** - `ENDPL`/`SPLAC` build `S500S` from swapper
  mailbox answers; RUN cannot accept the domain until CS-load clears. It is the near-term critical path.
- **Task 13 (completion wake) gates STATUS/START-SWAPPER** on both tracks - the parked process must wake.
- **Tasks 9/10/11 (wiring)** have no carve dependency and can proceed now (CODE).
- **Task 14** is the gate; needs 8 (or Track B 11+8-equivalent) + 12 + 13.
- Task 7's differential step is the single cheapest next action and needs no external answer.
