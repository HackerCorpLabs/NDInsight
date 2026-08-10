# Remediation plan — octobus / ACCP track (2026-07-20, refactored into phases)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\REMEDIATION-PLAN-OCTOBUS-TRACK-2026-07-20.md`

> ## STATUS 2026-08-02 - READ THIS FIRST; the blocker/phase tables below are HISTORICAL 
> Every blocker in this doc (B0-B3) and the whole timeout chain is **RESOLVED**. The octobus swapper
> track is **GREEN**: FullFlow ran 2/2 PASS (status/start-swapper/list/stop-system all OK). **Do NOT
> re-chase SWMSG, the mailbox base, the CS-load stall, or stop-system** - all closed. The 9 completed
> tasks (#13/#15/#17/#19/#20/#21/#22/#23/#24) were pruned from the live task list on 2026-08-02; the
> P0-Tx/P1-Tx rows below are kept only as the evidence record.
> **The only genuinely-open work:** production wiring toward D4 (#10 -> #9/#11/#14/#16 -> #12), and
> correctness debt (#25/#26/#27). See `nd500-octobus-frontier-2026-08` (memory) and
> `OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md` section 7.8.

Companion to `OPEN-QUESTIONS-REGISTER-2026-07-20.md` §2. The register says what we do not know;
this says what we do about it, ordered by dependency. **Blockers first** — anything below a blocker
cannot start on evidence until the blocker clears.

## Governing rules (bind at every task)

1. **No hardcoded value the hardware makes configurable.** Derive it, or cite the manual/carve on
   the same line.
2. **A gate that cannot fail is not a gate.** No place may both produce and verify the same datum.
3. **Silent wrong answers are worse than loud failures.** Dead error state gets wired or deleted.
4. **Measure before fixing.** Each task names its verification. "Test went green" is not
   verification if the test asserts the assumption.
5. **UNKNOWN beats a plausible wrong answer.** A task blocked on evidence stays blocked and carries
   a `[?]` in code — it does not get a guess.
6. **Carry the grade at the point of use.** Citing a `[D]`/`[INFER]` fact re-states its grade. A
   number with no citation is a placeholder, not a fact. (This is the failure mode behind all four
   2026-07-20 retractions — see the guardrail at the bottom.)

---

#  BLOCKERS  — resolve these first; they gate the phases below

| B | Blocker | Type | Gates | Status |
|---|---|---|---|---|
| **B0** | Account weekly API limit (resets **Jul 22, 1pm Europe/Oslo**) — no carve/audit agents until then | capacity | every carve + agent task | OPEN until Jul 22 |
| **B1** | **Q-OCT-22** — is `START_MESS` actually patched into control-store page 0, and with what value? | CARVE | **P1-T1** (mailbox base). If refuted, the whole base-derivation approach changes | Brief ready: `CARVE-REQUEST-Q-OCT-22-CS-PAGE0-PATCHER-2026-07-20.md`. Blocked on B0 |
| **B2** | **Q-OCT-24** — does L07 clear X5ACT with a single `STZTX` (halfword), or byte-at-a-time? | LIVE trace | **P1-T6** (activation machinery) **and now P1-T1** — see coupling | Verified [V]: `STZ`/`STZTX` cannot emit byte writes. The clear opcode L07 executes is unconfirmed. Blocked on a harness run. **Coupling found 2026-07-20 (reg §2.6a):** the X5ACT `0xFFFF→0` write is *also* what triggers mailbox **self-discovery** (`OnMpmActivationWrite`→`ConfigureMailbox`, `OctobusND5000Station.cs:747`), which re-bases the mailbox off the P1-T1 window-start guess. So if this write isn't detected, BOTH activation AND the mailbox base fail → N500TMR timeout. Sharpest harness diagnostic: does the `"X5ACT self-discovery:"` log line appear, and does its `header=` match `INZ500`'s `5FPMAILBOX`? |
| **B3** | **Q-SWP-04** — 32-bit register word order (high-first vs low-first) in the 21B image / MPM | MICROCODE | **P2-T1** (the three contradictory word orders). Do NOT unify on a guess | OPEN — shared with the 3022 track |

**Rule for the blockers:** each has a concrete resolution route already written. None is "think
harder" — B1 is a carve brief, B2 is a one-run instruction trace, B3 is a microcode read. Resolve,
then unblock the dependent task. Do not implement the dependent task ahead of its blocker.

---

# Phase 0 — Unblock (run the experiments)

Nothing here writes production code; it produces the evidence the phases need.

| Task | Depends | Action | Done when |
|---|---|---|---|
| **P0-T1** | B0 | Dispatch the Q-OCT-22 carve (brief already written) | Verdict: `START_MESS` patched (with source value) or not. Register §2.6 + B1 updated |
| **P0-T2** | — (needs harness only) | Instruction-trace the octobus boot harness at the X5ACT clear; record the actual store opcode(s) | Q-OCT-24 answered [V]. B2 updated |
| **P0-T3** | B0 | Read the microcode for 21B/MPM 32-bit word order | B3 (Q-SWP-04) answered [V] |
| **P0-T4** | B0 | Mine `ND-05.012.01` §13 (vendor mailbox protocol + function table) — **Q-NDX-01**, high value/cheap | Function-value semantics vendor-anchored; feeds Q-MBX-04/11/13 |

---

# Phase 1 — Evidence-backed fixes

Each either has its evidence already (P1-T2..T5) or is gated on a Phase-0 verdict (P1-T1, P1-T6).

| Task | Depends | Now (defect) | Fix | Verify |
|---|---|---|---|---|
| **P1-T1** mailbox base **→ LIVE-CONFIRMED as THE blocker; fix now concrete (reg §2.1c/§2.1d)** | none — carved formula in hand | **Live harness (2026-07-20):** boot completes CS-load+STAMIC0+ENKICK, then the mailbox is **never walked** ("NO mailbox MICFU processed"), so `3RMICV` is never answered → `Micro program.: 0` → J04 monitor-internal FATAL → `ND-500(0) timeout`. Root cause: `OnMpmActivationWrite` **sniffs for the first `0xFFFF→0`** and mis-bases (run3: 4096 candidates, 1 spurious). The `mpmStart` guess AND the sniff are both wrong | **Read `START_MESS` from the loaded CS — ACTUAL CORRECT WAY, Q-OCT-22 RESOLVED 2026-07-21 (§2.1e):** live CS dump shows word 026 `START_MESS`=**0x8800** (patched; placeholder 0x2000), word 025 `SAMSON_CPU`=**0x0001**. `header=mpmStart+_controlStore[0x16*8+7]=0x428800`, `extblock=header+SAMSON_CPU*256`, `X5ACT=extblock+0x0A`. No resident read/MMU/sniff — the servicer reads its own `_controlStore[]` like the microcode. Base the ext-block there, arm `X5ACT:=-1`, trigger the walk, answer `N5STA:=3` | Harness: servicer processes `3RMICV`, `Micro program.:` shows real version (not 0), no monitor-internal FATAL, `start-swapper` proceeds past "Loading Control Store". **Dead ends (don't repeat):** `0xFFFF→0` sniff → noise (0x800000); resident `5FPMAILBOX` read → 0 (needs MMU). OCB 202B report refuted (run3). |
| ~~**P1-T2** CS-load gate can fail~~ **→ RECLASSIFIED: likely NON-defect** | JRWCS carve 2026-07-20 | ~~gate can't fail = defect~~. **Carved JRWCS (`030-S3SM5:045771`):** sum loop `046036-046045` sums read-back words; `046046` calls **ABSLD** (`044656`) which reads the addend from `base+N*8` of the **same param block** (`LDD ,X 21` descriptor, `044671-044701`); `046052-046053` compares. Both operands are read back from the one shared param area — so on real HW the addend is what the **ACCP dump wrote**, making `addend==Σ(words)` tautological *by design*. The emulator's self-consistent serving is **faithful** | **Do NOT implement the artificial-failure fix** — it would inject a failure mode real HW lacks. Real gate only catches param-area transfer faults, not CS-content corruption | **One residual [confirm, don't assume]:** read `ND-05.017.01` CMRWC(025B) to confirm the ACCP *computes+writes* the addend (vs ND-100 preloading it). If ACCP writes it → close as non-defect. See reg §2.9a |
| **P1-T3** window collision **(CONFIRMED, latent — not on critical path)** | — | **Verified 2026-07-20:** `ND100Memory.cs:246-291` `FindMemoryBank` tests `_nd500` (3022, `:267-273`) **before** `_octobus` (`:281-285`); both default `0x420000` (`NDBusND500IF.cs:782`, `NDBusOctobus.cs:1789`); no overlap guard → 3022 silently shadows octobus. **BUT** the octobus-only harness registers no 3022 (`_nd500==null`), so it resolves correctly today — this bites only if both cards coexist | Detect overlap at attach; **throw** | Unit test: both cards same base → throws. *Defer behind critical-path items (presence gate, P1-T1)* |
| **P1-T4** dead error state **→ RECLASSIFIED: not a defect, off-path** | — | **Audited 2026-07-20:** the names (`Error=1<<4`, `DMAError=1<<6`, `ND500Operation`, `ND500StopReason`) are `[Flags]` **enum members documenting the 3022 status-register bit layout** (comments cite each bit position); grep shows no logic read/write, i.e. pure documentation. In `NDBusND500IF` (3022) — **not on the octobus path** | **Do NOT delete** — that violates the standing keep-comments rule (these document the real hardware register). Leave as-is, or add an "unmodelled bit" note only if a bit is *read* but never set (none are). No action needed for the octobus track | n/a |
| **P1-T5** phantom station **→ RECLASSIFIED: intentional, off-path** | — | **Audited 2026-07-20 vs `OCTOBUS-NOANSWER-STATION10-BUGREPORT`:** station 10 (SCSI) is **required** by the TPE diagnostic — TPE test 4/5 query wire 10; those tests **pass** after the interrupt-model fix. It is station 10, not the ND-5000's 70B → **irrelevant to the SINTRAN timeout** | Optional hygiene only: if moved out of the ctor, the TPE boot harness must still register it or tests 4/5 break. Not worth the risk now | TPE octobus tests stay green |
| **P1-T6** X5ACT byte machinery | **B2** (Q-OCT-24) | `ND100Memory.cs:495-524` byte-path hook + comment claim byte-at-a-time; store instr can't emit it | If B2 = single `STZTX`: remove the byte-path machinery as dead code, rely on the halfword hook `:543`. If B2 = byte-at-a-time: keep, and document the real source | Harness activation still fires; no dead branch left |

---

# Phase 2 — Correctness landmines (decision, not carve)

| Task | Depends | Now | Fix |
|---|---|---|---|
| **P2-T1** word-order unification | **B3** (Q-SWP-04) | `NDBusND500IF.cs:1531-51` (low-first) vs `:1064-92` (MSB-first) vs `:2292` (high-first) — internal contradiction | Pin from B3, route all three through ONE helper. **Until B3: add `[?]` at each site naming Q-SWP-04 — do NOT unify on a guess** (unifying wrong turns 3 visible bugs into 1 invisible one) |
| **P2-T2** semaphore give-up | — | `Nd500MicrocodeServicer.cs:690,931` — after 10k tries on X5SEM, **proceeds unlocked** | Fail the op (5ERANSWER/throw), never corrupt shared state. (MPM-5 has a hardware LOCK test-and-set cycle, `ND-10.004.01:2349` — contention should not occur single-CPU; if it does, surface it) |
| **P2-T3** fabricated alive record | — | `Nd500MicrocodeServicer.cs:797-804` synthesises a MON 377B stop — swapper "alive" without running | Gate behind explicit `FakeSwapper` option, default **off**; doc-comment that it makes bring-up lie |
| **P2-T4** invented register bits | — | `NDBusOctobus.cs:1031-70,1079-1219` — control/transmit/receive bit enums uncited (file already DEBUNKs an identical invented enum at `:1236`) | Cite each field or mark `[UNCERTAIN]` + log on uncited-bit-dependent behaviour |
| **P2-T5** timing constants | — | `NDBusOctobus.cs:1488-89` — `INBOUND_LATENCY=8`/`INTERVAL=2`, "not datasheet-exact" | Keep, add a regression test pinning "No answer from Octobus station N" must not appear |

---

# Phase 3 — Blocked on answers already asked (do NOT implement on inference)

Gated on B0 + the named register item. Each keeps its `[?]` accurate until the answer lands.

| Task | Blocked on | Action when answered |
|---|---|---|
| **P3-T1** real sequencer start | Q-OCT-19 (MICROCODE) | Replace `_microprogramRunning` bool (`OctobusND5000Station.cs:1503-16`) with a real start via the `CPU.ND5000` package + ACCP Access-Module bridge (an architecture step) |
| **P3-T2** 16B/17B legality | Q-MBX-09 (MICROCODE) | Fix the Classic-vs-Samson split (`Nd500MicrocodeServicer.cs:378,491`) from fact — the inference class that caused the D4 blocker |
| **P3-T3** ACCP interrogation 2/1/3 | Q-OCT-18 (UNANSWERABLE from firmware; needs ND-05.020.01 or capture) | Implement the reply layout exactly; until then the CPU can't pass INIT_SAMSON against our ACCP |
| **P3-T4** ACCP pointer units | Q-CSL-02 (LIVE) — CS-load-stall prime suspect | Fix LPARP scaling (`OctobusND5000Station.cs:1672-92`) |
| **P3-T5** `5OMDNO` allocation | Q-OCT-13 (CARVE `CONOMD` @`040062`) | Remove any assumed OMD from the harness ("a harness that hardcodes an assumed `5OMDNO` tests itself") |
| **P3-T6** MFACK `LMFIELD` word 3 | Q-OCT-14 (CARVE) | C6 ack oracle stays `[OPEN]` until resolved |
| **P3-T7** context-block stride | Q-MMU-* (MICROCODE) — base `[V]`, stride `[D]` | Fix `Nd500MicrocodeServicer.cs:537`; wrong stride loads a neighbour's context |
| **P3-T8** chain LINK convention | MICROCODE — multi-message chains untested (every live LINK=−1) | `Nd500MicrocodeServicer.cs:199` — wrong shift corrupts unrelated memory |

---

# Phase 4 — Carves to commission (all gated on B0)

| Carve | Register ID | Pays for |
|---|---|---|
| ND-100 CS page-0 patcher | Q-OCT-22 = **B1** | Validates P1-T1 (also its blocker) |
| `CNVWADR`/`CNVBYADR` body (`055160`) | Q-OCT-23 / Q-CSL-16 | The ÷2 MFbus→ND-100-word scaling (do not assume plain −ADRZERO) |
| Is `3RMICV`/watchdog issued before the octobus CS-load? | Q-ACT-03 | Whether a pre-CS-load answerer must exist — bears on the timeout gate |
| Regions 23/25 — independent bases or same field? | Q-OCT-21 | Live trace only proved they *resolve* the same |
| `LDSWA`/`RUNSW` byte content | Q-SWP-11 | The (A)/(B) seam, post-retraction |

---

# Sequencing

```
B0 (Jul 22) ─┬─► P0-T1 (Q-OCT-22) ──► P1-T1 mailbox base ─┐
             ├─► P0-T3 (Q-SWP-04) ──► P2-T1 word order    │
             └─► P0-T4 (ND-05.012.01 §13) ──► P3-T2, P3-T8 │
                                                            │
harness ───────► P0-T2 (Q-OCT-24) ──► P1-T6 X5ACT ─────────┤
                                                            ▼
P1-T2..T5 (evidence in hand, no blocker) ───────────► re-run octobus boot harness
                                                            │
                                    timeout persists ──► P4 carves (Q-ACT-03, Q-CSL-02)
                                    timeout clears  ──► the three real gates (register §2.1):
                                       (a) 5MPM status = ANSWER  (b) 5ALIVE set  (c) level-12 IDENT
                                            │
                                            ▼
                                    P3-T1 (real sequencer) becomes the main line
```

**The three ND-500(0) timeout gates** (register §2.1, `N500TMR` `RP-P2-N500.NPL:300-341`): (a) 5MPM
status word becomes `ANSWER`; (b) `5ALIVE` set in `CPUAVAILABLE`; (c) a level-12 **hardware**
interrupt whose `IDENT PL12` indexes `ITB12` (`0o153563`). Phases 0-1 exist to reveal *which* of the
three we fail — not to guess.

---

# Guardrail — the 2026-07-20 retraction pattern (do not repeat)

Four claims retracted in one day, all one failure mode: **a plausible mechanism asserted from
partial evidence, then cited downstream as fact.** Concrete traps:

- Guessing semantics from a symbol name (`SWIP`→swapper, `PRT`→process, `IL12Q`, `5FYLL`). A
  mnemonic says where to look, never what code does.
- Assuming a PC below one segment's load base is "the resident" — check every segment's base.
- Two console strings printed in sequence read as two branches — they were two steps of one state
  machine.
- Citing another track's **emulator** behaviour as hardware corroboration.
- Illustrative teaching numbers ("Segment 5, Offset 0x1200") hardening into facts across citations.

**Six inferences were load-bearing and findable; going and finding them is the work, not the
footnote.** Rule 6 (carry the grade at point of use) is the standing defence.

## Explicitly NOT in this plan

- Re-deriving retracted material (register §2.0): swapper is macrocode; `LDSWA` has no CPU-type
  branch; `IL12Q` is the disk queue; swapper image arrives by `MON 131` ABSTR disk DMA;
  `CALL_SWIP`=switch-processor not swapper; `MSG_PRT`=programmed trap not process probe.
- The 3022 track's D4/MMU chain (register §1) — theirs, except shared B3/Q-SWP-04.
- Anything requiring the ACCP EPROM (Q-CSL-12, Q-OTH-10) or the missing MPM channel specs
  ND-10.005/006, ND-05.011 — unanswerable, not deferred.

---

#  UPDATE 2026-07-21 — timeout RESOLVED; the line is now "run the swapper" 

**P1-T1 is DONE and verified (this closes the whole B1/Q-OCT-22 → mailbox-base chain).**
Live harness run11 (`FullFlow_Octobus_Login_Nd500_Status_StartSwapper_Capture`, current code):
`ND-500(0) timeout` = **0** (was 22); servicer processes `3RMICV(1)` **4×** at the real
START_MESS-derived mailbox nodes `0x428E30`/`0x42C130`; test **Passed**. Fix chain =
START_MESS base (read `_controlStore[0x16*8+7]`=0x8800) + Clock-driven microcode-IDLE-loop
poll + transport-aware X5BEX/LINK resolution (window-relative byte, not 3022 `<<1`). No
regression in the 3022/servicer path (see [[nd5000-timeout-convergence]]).

## Grounded current state (run11 OUTCOME line — MEASURED, not inferred)
```
ENTER=OK  login=OK  nd-500=OK   status=STALL  start-swapper=STALL  list=STALL  stop-system=STALL
[after start-swapper]  ND-5000  st=56  PC=0x00000000  stopMode=WAIT  octobus.IN=0x0105
```
Meaning: the fatal timeout is gone (login/@nd-500 reach the monitor), **but the ND-5000 CPU
never leaves WAIT — PC stays 0, it never executes the swapper.** `status`/`start-swapper`/
`list` STALL because the monitor waits on a running CPU that stays parked.

## WHAT'S LEFT — download swapper → run it → show in LIST-ACTIVE-PROCESSES
Critical path is #2 → #3 → #4 → #5; #1 and #6 bracket it.

| # | Item | Now (measured/where) | What must happen | Grade |
|---|------|----------------------|------------------|-------|
| **1** | **Swapper image actually loaded** | PC=0 after start-swapper — unconfirmed the image reached MPM | Confirm `start-swapper` DMAs the swapper **macrocode** image into the MPM window (retracted-material note: arrives by `MON 131` ABSTR disk DMA, NOT the mailbox copy engine). Trace the MPM window for the image landing where the start-context PC will point. | [NEEDS VERIFY] |
| **2** | **Start the ND-5000 CPU (THE blocker — P3-T1)** | `stopMode=WAIT PC=0`; `_microprogramRunning` is a **bool stub** `OctobusND5000Station.cs:1503-16` | Apply the start/context block (21B register context → P=swapper entry, B, domain) AND unpark the CPU (leave WAIT) so the real `CPU.ND5000` executes. Replace the bool with a real sequencer start via the CPU.ND5000 package + ACCP Access-Module bridge. | [D] gated on Q-OCT-19 |
| **3** | **Run-thread stability (Task #12)** | ND-5000 runaway crash / headless park model | Once the CPU executes, the run thread must not crash/hang. Fix the park/run model so the swapper can run to a MON stop. | [D] |
| **4** | **Service swapper MON calls while running** | block-copy MICFU family calibrated ([[nd5000-mailbox-copy-engine]]) but never exercised by a really-running CPU | Drive the swapper's MON calls through the servicer once the CPU runs; fix gaps that only appear under real execution. | [partial] |
| **5** | **Real "alive"/status — NOT fabricated (P2-T3)** | `Nd500MicrocodeServicer.cs:797-804` synthesises a MON 377B "alive" WITHOUT the CPU running — this is why `status` looks like it should answer but the process isn't real | Gate the fake behind an explicit `FakeSwapper` option (default OFF). With #2/#3 done, the answer comes from the actually-running swapper → LIST-ACTIVE-PROCESSES shows it for real. | [must-fix] |
| **6** | **End-to-end assertion** | OUTCOME line only captures STALL | Extend the harness to drive LIST-ACTIVE-PROCESSES and ASSERT the swapper appears (turn the capture test into a pass/fail gate). | [test] |

**Guardrail:** #5 means today's "it almost works" is partly a LIE (fabricated alive record).
Do not count the swapper as "showing up" until it shows up with the fake OFF and the CPU
actually executing (#2/#3). See the retraction guardrail above — no symbol-name inference.

## Tangential (not on this critical path)
- ND500 bus-interface **test-suite hang** fixed 2026-07-21 by marking the exhaustive
  CPU-instruction corpora `[Explicit]` (`TestND500_GenerateComprehensiveTests`,
  `TestComprehensiveExportAndRun`, long NC drives in `TestNC_AnalyzeExecution`) — they ran a
  16MB machine per case ×23,728 cases and blew past the CI hang timeout, masking ~1700 other
  tests. Not the swapper line; keeps `dotnet test Emulated.Tests.ND500` able to exit.
