# Remediation plan — octobus / ACCP track (2026-07-20, refactored into phases)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\REMEDIATION-PLAN-OCTOBUS-TRACK-2026-07-20.md`

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

# ░░ BLOCKERS ░░ — resolve these first; they gate the phases below

| B | Blocker | Type | Gates | Status |
|---|---|---|---|---|
| **B0** | Account weekly API limit (resets **Jul 22, 1pm Europe/Oslo**) — no carve/audit agents until then | capacity | every carve + agent task | OPEN until Jul 22 |
| **B1** | **Q-OCT-22** — is `START_MESS` actually patched into control-store page 0, and with what value? | CARVE | **P1-T1** (mailbox base). If refuted, the whole base-derivation approach changes | Brief ready: `CARVE-REQUEST-Q-OCT-22-CS-PAGE0-PATCHER-2026-07-20.md`. Blocked on B0 |
| **B2** | **Q-OCT-24** — does L07 clear X5ACT with a single `STZTX` (halfword), or byte-at-a-time? | LIVE trace | **P1-T6** (activation machinery). If single `STZTX`, the byte-path hook is dead code built on a misobservation | Verified [V]: `STZ`/`STZTX` cannot emit byte writes. The clear opcode L07 executes is unconfirmed. Blocked on a harness run |
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
| **P1-T1** mailbox base | **B1** (Q-OCT-22) | `ND100Machine.ND5000.cs:155` defaults header to `mpmStart` (offset 0) — a guess | If B1 confirms: derive `START_MESS`/`SAMSON_CPU` from **loaded** CS words `000026`/`000025`; keep X5ACT self-discovery as a **cross-check** that logs loudly on disagreement. If B1 refutes: use self-discovery as primary | MPM dump: signature-found mailbox == derived base |
| **P1-T2** CS-load gate can fail | — (evidence in hand) | `OctobusND5000Station.cs:316-352,1760-1791` computes the checksum it then serves — gate can't fail | Serve read-back from the stored image only; compute addend from what was stored | New corrupt-one-word test **must** yield `EILOCS 002103B`; positive path stays green |
| **P1-T3** window collision | — | `ND100Memory.cs:266-285` — 3022 + octobus both default `0x420000`, 3022 wins silently | Detect overlap at attach; **throw** | Unit test: both cards same base → throws |
| **P1-T4** dead error state | — | `NDBusND500IF.cs:1556-67,1506-07,429-96,414-16` — `Error`/`DMAError`/`ND500Operation`/`ND500StopReason`/`DisableTagInDecoding…` declared, never read/written | Wire each with a cited source, or delete + `[?]`-comment. No silent third option | grep: every `StatusRegisterBits` member has a read+write, or an explicit unmodelled note |
| **P1-T5** phantom station | — | `NDBusOctobus.cs:1815-19` registers a SCSI station at wire 10 in the **device ctor**, every machine | Move to the tests that need it | Boot harness shows no station 10 unless a test adds it. **Check against `OCTOBUS-NOANSWER-STATION10-BUGREPORT`** — may be this artifact |
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
