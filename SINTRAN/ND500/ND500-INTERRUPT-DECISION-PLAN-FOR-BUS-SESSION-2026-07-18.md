# ND-500 Level-12 Interrupt: Decision & Fix Plan (architect -> bus-interface session)

**Date:** 2026-07-18
**From:** Architect session
**To:** ND-500 bus-interface session
**Goal:** finally DECIDE the 3022 level-12 interrupt model (R2-8 reversal / R2-9) with
evidence, then FIX the bus logic with regression cover — no guessing.
**Companion analysis:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-INTERRUPT-HOLD-CHALLENGE-2026-07-17.md`
(read its section 0 first — the plain-language model + why it is the STANDARD model, not an
ND-500 invention).

---

## 0. The one idea that makes this decidable

There is NO direct hardware oracle: no TPE test programs exist for the 3022, so we cannot
force `disable-while-pending -> re-enable -> IDENT` at the register level on real hardware.

BUT the question becomes MOOT if we can show SINTRAN never enters the ambiguous scenario.
So the decision reduces to a triangulation of three sources we DO have:

1. **The carved SINTRAN 3022 driver** — authoritative for the SOFTWARE contract (when the
   driver enables/disables, activates, IDENTs; whether it ever relies on hold/re-assert).
2. **A live emulator run** — SINTRAN + `N500:` commands with full 3022 IOX logging + DAP —
   authoritative for WHICH scenarios actually occur in real command flow.
3. **The carved microcode / MON-call decode** — authoritative for HOW the interrupt is
   generated at completion (GIVEINT / MOCALL / TAG-OUT).

**Decision rule (state it up front so the evidence maps cleanly):**
- If the carve + traces show SINTRAN ALWAYS IDENTs before any subsequent activate AND never
  disables-while-pending -> edge-vs-level is UNOBSERVABLE by real software -> adopt the
  pending-latch model (physically correct, matches Ronny's ruling) and PIN it with citation.
  It is a safe strict-superset: it also handles the never-observed cases correctly.
- If the traces show the driver DOES hit an ambiguous case -> that trace IS the oracle; the
  model must reproduce it exactly.

Either outcome lands on the SAME model (the dedicated pending latch, doc section 5b). The
investigation de-risks; it does not change the model. What it PRODUCES is: proof + a set of
captured real scenarios turned into regression tests.

---

## 1. Phase 1 — Carve the SINTRAN interrupt contract (STATIC, do first)

Read the level-12 path in the carve and answer six questions, each graded [V]/[D]/[?].
Anchors (from the bus-interface skill / byte-located):
- Level-12 ISR / driver routines (same MPIT overlay, SYMBOL-pinned):
  `5STDR=135010`, `CHN5S=135205`, `DECOM=135361`, `MCHAN=137206`, `MONIC=023030`.
- GOSW level-12 dispatch table `@137625B-137650B`; dispatcher `N5MPA=137525B`;
  `L12MIN/MAX=500B/523B`.
- `WRTAG @051116B` (wraps every TAG strobe in `LCON5:=44B` then `UNLC5`, `LCON5:=40B`).
- `XTER500` (TERM5 + poll), `ACT50` (LMAR5 x2 + `LCON5:=5` = int-enable+activate),
  enable sequence (`LCON5:=10 test`, `LSTA5:=0`, `LCON5:=1`, `SLOC5`).

Questions:
1. **Does the level-12 ISR execute an IDENT** for the 3022, and at what PC? (Confirms the
   acknowledge exists and is the latch-clear point.)
2. **On IDENT, does the driver (or the hardware convention) clear interrupt-enable?**
   Ronny's ruling: yes, 3022 clears BOTH enable and the line on IDENT (matches the 7 other
   controllers). Confirm nothing in the ISR contradicts it.
3. **Does the driver ever write `LCON5` with bit 0 CLEAR (disable interrupts) while a
   completion is unacknowledged?** (Tests the "disable-while-pending" leg.)
4. **Does `WRTAG`'s `LCON5:=44B` activate ever run while an interrupt is pending-unacked?**
   `44B = bits 2(activate)+5(disable-TAG-decode)`, bit 0 CLEAR. This is the R2-9 trigger:
   an activate that clears `Finished` out from under an unacked interrupt.
5. **Does the driver re-enable (`LCON5:=1`/`5`) expecting a held request to re-fire, or does
   it always re-issue the operation?** (Tests whether hold/re-assert is RELIED UPON.)
6. **Ordering per command:** does the ISR always IDENT + service BEFORE the next `ACT50`?
   (If yes, the ambiguous windows never open in normal flow.)
7. **Poll-driven or interrupt-driven? — the DETECTION-vs-SIDE-EFFECT question.** Is the
   3022 completion DETECTED by polling `RSTA5` (finished bit) in a driver loop, with level
   12 merely a wake/verify side-effect, OR is level 12 the actual detection mechanism the
   ISR depends on? (This decides the severity of the completion-latency question in §7b/3 and
   of the whole hold/re-assert model — a poll-driven driver barely cares about interrupt
   edge timing; an interrupt-driven one does.) SEE the octobus-carve cross-input below.

   **ANSWERED 2026-07-18 = INTERRUPT-DRIVEN [V-NPL]** (`CARVE-ANSWER-Q7-COMPLETION-POLL-VS-
   INTERRUPT.md`). `5STDRIV` (MP-P2-N500.NPL:659, octal 134610) runs on the level-12 line,
   re-armed only by `WT12` (:693) — a pure interrupt loop, NO busy-poll. The `RSTA5` read
   (via CLE5STATUS) is tested ONLY against the error group (A/720 = bits 4/6/7/8); no
   finished bit is examined. On no-error it drains MAILINK and reads `N5STA` (MPM offset 2)
   to pick answer/5ERANSWER/restart — a shared-memory read, NOT a hardware poll. `XACT500`
   (:3057) reads `RSTA5` once, no wait loop; the only RSTA5 spin is `XTER500`'s STOP
   handshake (5ILOCK), not completion. No IOX IDENT read in `5STDRIV`. Decision tests: (1)
   branch-on-finished-bit = NO; (2) entered=>finished, status only discriminates type/error
   = YES; (3) poll loop outside 5STDRIV = NO; (4) status(error)-before-drain, payload from
   N5STA not RSTA5, master-clear only on timeout, ident not read. HONEST GAP: [V-NPL logic]
   + [V symbols]; the L07 5STDR bytes (=0x135010, ~0x80-word drift from the :134610 NPL
   listing) were NOT disassembled — logic-robust, byte cross-check is the one follow-up.
   RECONCILE FLAG (bus session, not blocking): the carve's "RSTA5 has no finished bit" (whole
   map = error/power/clock/lock, XC-P2-N500.NPL:41-45) conflicts with the emulator's
   `ND500Finished`=STATUS bit 3 + the `$29/$09` canary oracle — best read as "SINTRAN doesn't
   TEST a finished bit (it uses level 12)"; whether bit 3 exists/is-set in HW is separate.

   **CONSEQUENCE (severity branch taken):** Q7 = interrupt-driven => R2-8/R2-9 (the dedicated
   pending latch, §5b) is the REAL detection path, NOT a rarely-exercised edge case. The
   pending latch must land before the next live deferred-answer trace. Completion-latency
   (§7b/3): the carve says there is NO ND-100 poll window, so mid-activate vs after-activate
   firing cannot be masked by polling — the requirement is simply ORDER the answer/N5STA
   write BEFORE the level-12 edge. The emulator's `SetOperationComplete` already writes the
   answer before `CheckTriggerInterrupt`, so this ordering is ALREADY satisfied; latency is a
   non-issue, ordering is the invariant (and it holds).

Deliverable: a short contract table (the seven answers + citations), graded.

### Cross-input from the octobus station-number carve (2026-07-18) — a HYPOTHESIS for Q7

`CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md` established, byte-cited, that TPE's
LIST-HW-CONFIG octobus path is **status-POLLING for control flow** (`+2` bit3 / `+6` bit3 /
`+0`-after-transmit), with the input ident (40B) "enabled and verified as a diagnostic but
NEVER the detection mechanism." The carver generalized this to "TPE treats interrupts as
verifiable side-effects; all control flow is status-polling."

CARRY IT AS A HYPOTHESIS, NOT A CONCLUSION — caveats (UPDATED 2026-07-18 by the octobus
session's PROMPT-2a answer, which corrected my first framing):
- **The split is TPE(poll) vs SINTRAN(interrupt), NOT config-vs-multibyte inside TPE.** My
  earlier "TPE multibyte-receive is interrupt-driven" was WRONG: the octobus carve shows
  TPE's multibyte receive is ALSO poll-driven (`octobus_receive_multibyte_message` @d2be,
  gated by the shared RFT poll `octobus_poll_rft` @d591; [strong inference, not a fresh
  line-walk]). TPE polls throughout; the level-13 idents (40B/41B) are enabled + cross-checked
  as a DIAGNOSTIC only, never the detector.
- **SINTRAN != TPE:** TPE = poll-to-WAIT; SINTRAN = interrupt-to-WAKE. The closer analog for
  a SINTRAN driver is SOCTO (SINTRAN's level-13 input ISR: interrupt-woken, then poll-drains
  +2 bit3), NOT TPE. Unifying invariant across both: STATUS is the source of truth for the
  payload; the interrupt is a wake/diagnostic, never the payload.

**THIS HYPOTHESIS IS NOW CLOSED by the direct Q7 carve above (INTERRUPT-DRIVEN).** The
octobus/TPE pattern was only ever a hypothesis to test against `5STDR`; Q7 tested it directly
from `5STDRIV`'s own bytes and found 5STDR is interrupt-driven (level-12 line is the detector,
N5STA read from MPM to discriminate — NOT a poll-drain like SOCTO, because the 3022 answer is
in MPM shared memory, not a hardware FIFO). Retained here as confirmatory context only.

USE: if Phase-1 Q7 finds SINTRAN's 3022 driver is poll-driven for completion (level 12 = a
wake/verify side-effect, not the detector), then the completion-latency concern (§7b/3) and
the hold/re-assert edge cases are LOW severity — the pending-latch model is still correct
but its subtle timing is rarely exercised. If it finds the ISR is the real detector, timing
matters and the latch model earns its keep. Either way the model is unchanged; only the
severity/urgency shifts.

CONCRETE MICRO-PATTERN to look for in `5STDR` (the octobus poll shape, byte-cited in the
station-number carve — a template, NOT a claim about SINTRAN): master-clear FIRST; read the
STATUS register BEFORE any transmit; poll a STATUS bit (RFT-equivalent) for detection, N
iterations; read the DATA/result register ONCE after; NO master-clear between transmit and
read; the input ident enabled + cross-checked as a diagnostic but never the detector. If
`5STDR` shows this read-order/no-clear-between/poll-for-detection shape, that is positive
evidence SINTRAN follows the poll-driven family pattern → Q7 = poll-driven.

---

## 2. Phase 2 — Live capture (DYNAMIC): SINTRAN + N500 commands + logs/DAP

Boot SINTRAN with ND-500-MON available; drive real commands; capture the real register
timeline. Tooling: retrocore CLI/MCP (`cli_run`, `cpu_trace_on`/`cpu_trace_filter`/
`cpu_trace_dump`) + dap-debugger MCP (breakpoints, memory/register reads). If a live run
needs Ronny at the console, hand him the exact `N500:` command list and the log points.

**Instrument (log every one, with driver PC + value + resulting device state):**
- 3022 IOX writes: `LCON5`(off 5 — enable/activate/test/prog-clear), `LSTA5`(3),
  `TERM5`(7), `MCLR5`(6), `SLOC5`(14o), `UNLC5`(16o), `LTAG5`(11o).
- 3022 IOX reads: `RSTA5`(2 — the status poll), `RCON5`(4), `RTAG5`(10o).
- Level-12: every `SetInterruptBit(12)` assert/deassert AND every `IDENT(12)` (with the
  ISR PC that took it).
- Device state snapshot at each event: `controlRegister`, `statusRegister` (esp. bit 0
  enable, bit 2 busy, bit 3 finished, bit 5 lock, bits 10-14 stop reason), `InterruptBits`.

**Command set (progressively exercises more of the path):**
- `N500: STATUS` (041B RSTAT — the one command doing a live `RSTA5` read; lightest path).
- `N500: VERSION` (057B RMVER — descriptor-cached, minimal hardware).
- A command that drives a real **activate + completion + level 12** end-to-end (e.g. one
  that produces a MON-call/stop answer). This is the one that shows the enable ->
  activate -> completion -> interrupt -> IDENT -> TERM5 cycle in full.
- **Back-to-back** commands (to catch: is IDENT of command N always done before command
  N+1's activate? = Phase 1 Q6 confirmed dynamically).

**Reconstruct** the annotated timeline per command; explicitly mark whether any of the
ambiguous windows (Q3 disable-while-pending, Q4 activate-while-pending) actually opened.

Deliverable: 3-4 annotated traces + a one-line verdict: "ambiguous windows observed: yes/no".

---

## 3. Phase 3 — Reconcile with the carve microcode (completion side)

Cross-check that the interrupt GENERATION matches the decoded ND-500 side so the "event"
that sets pending is the right one:
- Classic completion = activate/finish protocol -> `ND500Finished` -> level 12 (TAG-OUT
  completion, carve section 10). The MON-call answer-in-place (`MSG_END` -> TAG-OUT) is the
  ASYNC completion that sets finished at a stop (already modeled: `AnswerWritten ->
  SetOperationComplete`).
- Confirm the pending latch should be set at EXACTLY those two points (sync activate answer
  + async stop answer) and — open question B — whether TERM5's second level-12 pulse is a
  third latch point. Use the carve to settle B ([V] if the driver/microcode shows TERM5
  raising its own interrupt; the S1 harness already bakes in "two interrupts per classic
  message").

Deliverable: the definitive list of latch-set points + the TERM5/B resolution.

---

## 4. Phase 4 — Encode scenarios as unit tests (REGRESSION)

Every captured real sequence becomes a deterministic NUnit test that drives the exact
register order/values from the trace (`nd500IF.Write(LCON5, ...)`, `Read(RSTA5)`,
`DbgIdent(12)`, `TERM5`). This is what makes the decision permanent and regression-proof.

Test set (NUnit, `Assert`, no FluentAssertions, no LINQ/foreach in product):
1. **Captured-flow tests** — one per annotated trace from Phase 2 (the real driver
   sequence, asserting the real observed `InterruptBits`/status at each step).
2. **The finalized-model tests** (doc section 5b), using REAL acknowledge not the
   `InterruptBits = 0` shortcut:
   - 11-step hold/re-assert: complete -> pending set -> line up (enabled) -> disable ->
     line drops, pending stays -> re-enable -> line re-asserts -> `DbgIdent(12)` ->
     pending clears -> line stays clear across later enable toggles.
   - activate-survival: complete A -> pending -> (no IDENT) -> activate B -> `Finished`
     may clear, pending stays, line stays asserted when enabled -> `DbgIdent(12)` clears it.
3. **Canary conversion** — convert both `ClassicCanary_*` in
   `Nd500R2ReviewTestPackage.cs`: `InterruptBits = 0` -> `DbgIdent(12)`, and flip the R2-8
   assert (`ClassicCanary_IntEnableOff_AsyncFinishedSets_ButNoLevel12` final assert
   `Is.Zero` -> `Is.EqualTo(LEVEL12)`, comment edge -> hold-until-IDENT). This is the
   test-owner change the bus session owns.

Gate: full ND500IF category (match `TestCategory=ND500IF`, the 181-test count) + full
ND-500 suite for regression; octobus suite unaffected.

---

## 5. Phase 5 — Product fix (architect lands; contract from Phases 1-3)

Once Phases 1-3 deliver the contract, the architect edits `NDBusND500IF.cs` to the section
5b model. Exact shape (so the bus session can review against it):
- Add `_level12InterruptPending` (thread-safe — under `_statusLock` or `Volatile`; it is
  written from IOX thread AND CPU thread, the R2-1 hazard — refinement C).
- `CheckTriggerInterrupt` -> `UpdateLevel12Output`: `line = _level12InterruptPending &&
  (statusRegister & InterruptEnabled)`; NEVER writes the latch.
- Latch SET: `SetOperationComplete` (both sync + async) [+ `Terminate` per B].
- Latch NEVER touched by the activate branch (it may still `ClearStatusBits(Finished)`).
- `UpdateLevel12Output` called on EVERY `LCON5` bit-0 write (not only in the activate
  branch) so enable transitions re-evaluate the line.
- Override `IDENT(level)`: on `level==InterruptLevel`, clear the latch AND clear
  interrupt-enable at its SOURCE (control bit 0) + the status mirror (refinement A/D), then
  `base.IDENT`.
- Latch CLEAR also at `Reset()` + BOTH MCLR5 strobe paths (`:1726`, `:1903`).

Validate: re-run the Phase 2 `N500:` command set after the change; diff the driver-visible
outcomes AND the 3022 trace against the pre-change capture -> MUST be identical (regression).

---

## 6. Division of labor & sequence

- **Bus session:** Phase 1 (carve contract), Phase 3 (microcode reconcile + settle B),
  Phase 4 tests incl. the canary conversion. Report the contract table + the decision-rule
  inputs.
- **Architect:** Phase 2 (live SINTRAN + `N500:` capture with 3022 IOX logging / DAP —
  decided 2026-07-18: architect runs it, models the harness on the octobus
  `OctobusTpeBootHarnessTests.cs` + deterministic RTC) AND Phase 5 product change from the
  delivered contract; re-run regression.
- **Ronny:** final hardware authority on any point the carve leaves `[?]`.

Sequence: 1 -> 2 -> 3 (evidence) -> decision-rule verdict -> 4 (tests) -> 5 (fix) ->
regression. Nothing is committed to product until the contract verdict is in.

---

## 7b. Cross-input: the octobus TPE interrupt bug (2026-07-18) — READ THIS

Source: `E:\Dev\Ronny\ND5000UC\OCTOBUS-NOANSWER-STATION10-BUGREPORT-2026-07-18.md` (octobus
session, base `c2a319fe7`, fix implemented + verified). It is the SAME class of bug as
R2-9, caught on real diagnostic software, and it changes how much weight Phase 2 carries.

What they found: the level-13 INPUT interrupt was EDGE-LATCHED on frame arrival (one request
FF, set on arrival, cleared by IDENT). A 12-frame reply arrived as a burst -> collapsed onto
one FF -> IDENT cleared it once -> TPE read ONE frame -> the other 11 sat unread -> timeout
-> "No answer from station 10". Fix A: make it LEVEL-SENSITIVE — after popping a frame,
re-assert the request while the FIFO still holds data. Same principle as R2-9: hold/
re-assert while the request is unacknowledged; never edge-latch-and-lose.

Mapping to ND-500 (consistent, no contradiction):
- Octobus condition = FIFO-not-empty (naturally persists) -> re-assert on it, NO separate
  latch (the "paper-tape model" working because the condition persists).
- ND-500 condition = "unacknowledged completion" — NO natural persistent bit (`Finished` is
  clobbered by activate) -> needs the EXPLICIT `_level12InterruptPending` latch. The latch
  IS the persistent-condition representation. Same solution, different representation.

Five impacts baked into THIS plan:
1. **Phase 2 is load-bearing, not confirmation.** Their register-level repro PASSED 4/4 and
   could NOT see the bug (no CPU/interrupt context); only the full-CPU boot harness caught
   it. CONCLUSION: the 181 register-level ND500IF tests STRUCTURALLY cannot catch an
   interrupt-hold bug. Phase 2 (full SINTRAN boot + `N500:` + running interrupts) is the ONLY
   thing that can confirm/refute the model. Do not skip or down-weight it.
2. **Reuse their harness.** Model the ND-500 Phase 2 boot harness on
   `OctobusTpeBootHarnessTests.cs` (boots a floppy on `ND100Machine`, drives the console,
   asserts on output). Their RTC determinism fix (runtime `WallClockPacing`,
   `MachineEnvironment.Test -> tick-based`, `NDBusRTC.cs` + `ND100Memory.cs`) is what makes a
   REPRODUCIBLE ND-500 boot harness possible — depend on it.
3. **NEW Phase-1/2 question — completion-interrupt LATENCY.** Their secondary cause was
   zero-latency synchronous delivery: the interrupt fired MID-SEND (wrong context). The
   classic 3022 analog: a SYNCHRONOUS classic completion fires level 12 INSIDE the driver's
   own `ACT50` IOX. Investigate: does SINTRAN's 3022 driver tolerate level 12 firing
   mid-activate, or expect it AFTER the operation (like TPE did)? If the latter, the classic
   completion may need a small delivery latency too (analogous to octobus Part B). Add this
   to Phase 1 (carve: does the ISR run re-entrant to ACT50?) and Phase 2 (trace: when does
   level 12 actually fire relative to the ACT50 IOX?).
4. **Model direction validated.** Edge-latched interrupt collapse is now a PROVEN real-world
   failure (a shipping TPE diagnostic fails on it) — third-party corroboration of the
   level/pending model over edge, independent of Ronny's ruling + the R2-9 reasoning.
5. **Rebase/coordination.** Rebase this plan onto the octobus fix commit (hash TBD).
   `NDBusRTC.cs` + `ND100Memory.cs` are now shared baseline the ND-500 harness depends on.
   The classic-3022 completion path is UNAFFECTED by their octobus-card inbound-latency
   change (different card; loopback/classic untouched) — but verify no test-infra ripple
   (their `PumpInbound`/`Clock()` pumping is octobus-FIFO-specific).

## 7. What "finally decided" looks like

A one-page verdict: the six Phase-1 answers, the Phase-2 "ambiguous windows observed:
yes/no", the Phase-3 latch-set-point list + B resolution — concluding either "hold/re-assert
is relied upon, proven by trace X" or "unobservable in real flow; pending-latch model
adopted as the correct superset, pinned." Plus the green Phase-4 suite. After that the
architect lands Phase 5 and the interrupt model is closed.
