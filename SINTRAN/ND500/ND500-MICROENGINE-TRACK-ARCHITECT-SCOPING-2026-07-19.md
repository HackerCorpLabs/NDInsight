# ND-500 "Microengine" Track — Architect Scoping (REVISED after code map)

**For**: ND-500 BUS-INTERFACE LLM + ND-5000 OCTOBUS LLM + Ronny. **From**: architect. **Date**: 2026-07-19.
**Why now**: with the reachable-interface ladder complete on 3022 and octobus blocked only at A1, the
"microengine" was believed to be the critical path for both interfaces' deeper work. **A read-only
code map corrected that** — see §2. Everything gated below is unblocked by a much smaller effort than
the name implies.

Grades: **[V]** verified from code, **[?]** working model, **[TC]** to-carve, **[DECISION]** needs Ronny.

---

## 1. Goal (acceptance criterion) — unchanged

**A placed domain's start address executes on the emulated ND-500 and produces real MON-call traffic
through the mailbox** — concretely, phase **D4** turns green: `RUN` on `LINKAGE-LOAD-H02`
(SA:26000006721, C3-confirmed well-formed) reaches the `NLL:` prompt. That one assert proves
activation + ISA execution + trap/MON-call terminal round-trip end-to-end.

Today (F5 dump): `MICRO PROGRAM STOPPED / STATUS 040000 / MAR 0`; `RUN` → "NO WELL DEFINED PROGRAM IN
MEMORY", no active segments. C3 proved the domain FILE is well-formed — but see §2b: a harness test
with the real `CpuND500` attached proved **wiring alone does NOT reach D4**; PLACE-DOMAIN is not
completing the placement.

## 2b. REVISION (2026-07-19, harness-verified) — wiring is necessary but NOT sufficient

With the real `CpuND500` attached to the 3022, `RUN LINKAGE-LOAD-H02` still prints "NO WELL DEFINED
PROGRAM IN MEMORY" and issues **ZERO activation** — the CPU never leaves WAIT. Per ND-60.136 that error
= "RUN before a successful PLACE/DEBUG-PLACE/RECOVER-DOMAIN", and `LIST-ACTIVE-SEGMENTS OWN` is **empty**
afterward. **So PLACE-DOMAIN itself is not completing — the blocker is SINTRAN-side, UPSTREAM of the
CPU wiring.** [V, boot harness]

**Consequence: the domain carve moves ONTO the D4 critical path** (was: parallel/off-path). The pending
carve (RUN precondition flag + PLACE-DOMAIN write site) settles the gating fork:
- **(a) PLACE-DOMAIN completion needs a genuine ND-500 response we currently FAKE** (the swapper) — i.e.
  placement requires the swapper to actually run and map the domain's segments; or
- **(b) it's ND-100-side bookkeeping** — a flag/table PLACE-DOMAIN sets that the emulator isn't setting.

**Servicer real-vs-faked audit (classic, 2026-07-19)** [V] — the "register the CPU correctly" gaps that
become critical the moment placement completes:
- REAL (live CPU): 23B/25B start, 24B/26B MON-continue, MON/trap stop answers.
- **STASHED, no live path:** 20B RegisterRead, 21B RegisterWrite — there is **no live register-read path
  at all**.
- **FAKED:** 22B swapper start — the CPU never runs the swapper. (Routing 22B like 23B was tried and
  REVERTED: no process-0 register image is ever stashed on this path.)
- 3RMICV version/cpu-param **hardcoded**.
- 23B start applies **only reg0→P**; L/B/R/I/A/E left unmapped.
- LOAD-SWAPPER on this image = "> Loading Control Store" (128-bit CS reload; ND-5000/5800-provisioned,
  micro 11930).

**Hypothesis to test (NOT asserted):** the faked 22B swapper start may BE the upstream blocker — if
placement needs the real swapper to map segments and we never run it (and never stash its register
image), placement can't complete. The carve's (a)-vs-(b) answer decides. Detail:
`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md`.

---

## 2. KEY FINDING — the ISA/microengine is ALREADY BUILT AND GREEN [V]

The name misled us. A full read-only map of `CpuND500` + `Instructions\` + the servicer bridge shows:

- **`CpuND500` is a functional ND-500 *macro*-instruction interpreter** [V]. Real fetch/decode/execute
  pipeline (`CpuND500.Execute.cs` `TickCpuUnsafe`), full architectural register file (PC/ST/I/A/E/L/B/
  R/TOS/DITBASE/…), threaded run loop parking on `stopMode` via `AutoResetEvent _wake`
  (`CpuND500.ProcessControl.cs:213-297`).
- **~240 instructions implemented across all families** (MOVE 57, ARITHMETIC 36, SYSTEM 33, FLOAT_MATH
  28, BRANCH 20, CALL 18, STRING 18, …) with real flag/overflow/IEEE-float semantics. Only ≈17
  NotImplemented markers in 12 files — edge instructions, not the mainline. Plan claims a green
  1848-test ND-500 suite.
- **Real MMU** (Physical Segment Table + L1/L2 page-walk, `CpuND500.MMU.cs`), **domain switching**
  (DIT/PCB, `CpuND500.Domain.cs`), and a **full trap system** (local THA dispatch, mother-domain
  propagation, sink-to-ND-100, `CpuND500.Trap.cs`).
- **The CALLG→MON-call seam is wired end-to-end** [V]: CALLG into segment 31 → park (WAIT) →
  `MonitorCallSink.OnMonitorCall` → `Nd500CpuProcessBridge` reads arg values → `servicer.
  AnswerMonitorCallStop` → MOCALL stop record to ND-100. Trap path symmetric via `AnswerTrapStop`.
- **Start-a-process-at-address is wired** [V]: `OnStartProcess`→`StartProcessFromRegisterImage`
  (classic, sets `regs.PC=P`, arms, wakes) and `OnStartProcessSamson`→`StartProcessFromContextBlock`
  (5000). MON/trap park→answer→resume (3MONCO/3WMONCO/3TRACO) all implemented.

**It is unambiguously functional-ISA emulation — nothing interprets 128-bit microcode words.** The
SAMSON mailbox microcode is reimplemented as the C# `Nd500MicrocodeServicer` running *beside* the CPU;
the loaded CONTROL-STORE.DATA is treated as an opaque blob for version/model reporting only. So the
Option-A-vs-B "decision" from the prior draft is **settled by evidence: Option B, already done.** Not
a fork.

---

## 3. The REAL gap — production machine wiring, not ISA [V]

The entire engine above is **reachable only from tests.** The single structural blocker (plan gap
**G1**): **`NDBusOctobus.AttachCpu` (and the 3022 `AttachRealCpu`) have NO production caller.** Today
a running `CpuND500` is constructed + attached only inside test harnesses (`AttachRealCpu`). There is
no operator/machine path that stands up a running ND-500 and routes a `RUN` to it.

Ordered gap list between "PLACE-DOMAIN loads clean" and "RUN executes + emits MON-call traffic":

| # | Gap | Evidence | Size |
|---|---|---|---|
| 1 | **Production machine wiring (G1)** — a machine/init path that instantiates the interface card + `CpuND500`, calls `AttachCpu`/attach, wires `MailboxDoorbell→WakeRunThread` + `OnRunThreadPark→DrainDoorbells`, starts the thread parked | plan §1.2 G1; `AttachRealCpu` test-only | **The blocker** |
| 2 | **No `ND5000` (or classic `RUN`-to-CPU) machine-init command** — only `@ND-500` monitor exists; nothing triggers #1 from an operator RUN | map §3 | Small |
| 3 | **Config not plumbed (G2)** — `CpuTypeAndModel`/`SystemParameters`/ACCP identity have no INI/CLI path (also feeds octobus A1's model/version) | plan §1.2 G2 | Small |
| 4 | **Deterministic boot-harness → operator flow** — the "load via 14B/3START, run to MON, assert GIVEINT" acceptance pattern exists as a *test*, not driven from RUN | plan Phase 0 | Small–med |
| 5 | **CNTXTSAVE-on-stop gap (R2-5)** — context block goes stale on run (save-on-stop unimplemented); fine for single in-place resume, incomplete for full swap | `Bridge.cs:133-136` | Med, deferrable |
| 6 | **Stubbed edge instructions / Phase-3 microcode→macrocode worker bodies** — only if a specific workload hits them | map §2; plan §3 | Workload-driven |

**Net: this is a wiring + init-command + config track, not a CPU-execution track.** Most of it is
already scoped as the DOMINO plan's **Phase 0 ("Production wiring of ONE ND-5000 CPU")** — adopt that
rather than reinvent.

---

## 4. Workstream (REVISED — placement blocker now leads)

**Critical path (order):**
1. **Domain carve — PLACE-DOMAIN completion + the RUN precondition flag** (§2b). This is now the FIRST
   critical-path item, not parallel. It returns the (a)-vs-(b) fork: genuine-ND-500-response-we-fake
   (the swapper) vs ND-100-side bookkeeping. Everything below is gated on knowing which.
2. **If (a):** un-fake the swapper — make 22B swapper start deliver a real process-0 register image and
   run it (map segments). Requires the "register the CPU correctly" fixes: full 23B reg mapping
   (L/B/R/I/A/E, not just reg0→P) + a live register file for 20B/21B.
   **If (b):** implement the ND-100-side placement bookkeeping the emulator is missing.
3. **Config plumbing (G2)** — INI/CLI for `CpuTypeAndModel`/`SystemParameters`/ACCP identity. Also
   supplies octobus's next-tier reporting values (0x38 / micro 0x2E9A). Can proceed anytime.
4. **Production wiring (G1)** — the non-test attach: card + `CpuND500` + `AttachCpu` + doorbell/park +
   thread-start-parked, lifted from `AttachRealCpu`. **Necessary but proven NOT sufficient alone** (§2b).
5. **RUN/init command** — operator path that triggers #4 and routes activation to the attached CPU.
6. **Acceptance** — green D4 `RUN`→`NLL:`, then drive the "run to MON, assert round-trip" flow.
7. **Defer** CNTXTSAVE-on-stop (R2-5) and edge instrs until a workload demands them.

---

## 5. Cross-interface leverage — one track, both interfaces [V]

`CpuND500` + `Nd500MicrocodeServicer` + `Nd500CpuProcessBridge` are **shared**, and the wiring gap is
the **same shape on both** (`AttachCpu` octobus / `AttachRealCpu` 3022, both test-only). So the
production-wiring pattern built for one interface **transfers directly** to the other — Bug A
precedent (one `CpuND500` change fixed both). Transports differ only in activation arrival (ACT50 vs
X5ACT) and completion return (level-12 vs GIVEINT), both already implemented.

---

## 6. Sequencing + the one decision [DECISION]

The functional-ISA question is closed. The real decision is **which interface gets production wiring +
RUN first**:

- **3022/classic first** — A1 is CLEAR today, so a green D4 `RUN`→`NLL:` is reachable *now* with only
  the wiring/init work; it proves the whole RUN→execute→MON-call→`NLL:` loop end-to-end while the
  octobus A1 carve finishes. Then octobus reuses the identical wiring pattern. **(Architect rec.)**
- **Octobus/ND-5000 first** — matches the existing DOMINO plan's Phase 0 (already written for octobus),
  but is blocked behind the octobus A1 (EWRON model/version) fix, so it can't reach D4 until A1 clears.

Recommended order (REVISED per §2b — domain carve is now on the critical path):
1. **Domain carve — PLACE-DOMAIN completion + RUN precondition flag** FIRST (bus LLM carving now). Its
   (a)-vs-(b) answer determines whether the next step is "un-fake the swapper + real register mapping"
   or "ND-100-side placement bookkeeping".
2. **Config plumbing (G2)** — anytime; also feeds octobus's next-tier reporting.
3. **The placement fix** from #1's fork, THEN **production wiring + RUN** (wiring is necessary but not
   sufficient — §2b).
4. Green D4 → unlock the parked ladder (D/E/G, F6-ring populate), then mirror to octobus.

---

## 7. Risks / open questions (much reduced)

- **Not an ISA risk** — execution, MMU, traps, MON-call seam are built + green in tests. Main residual
  execution risk is the **CNTXTSAVE-on-stop gap (R2-5)** for full swap; single in-place resume is fine.
- **Octobus A1 — CLEARED (2026-07-19), was a disk fixture.** `EWRON` came from the swapper's CS-loader
  range-checking the SYSTEM/CONTROL-STORE FILE version against `[026354B,027337B]`; the disk shipped a
  5200 microcode (0x2B16) on an ND-5800 → swapped to MICRO-5800-B30.DATA (0x2E9A) → cleared, CS
  download now runs. So octobus is NOT A1-gated for its wiring mirror. (The `Micro program.: 0` /
  canned-3RMICV **reporting** gap is a separate NEXT-TIER item, fed by G2 config #1.)
- **Octobus live boot is currently a FACADE** [V, validation audit 2026-07-19] — the real `CpuND500`
  stays parked at PC=0/WAIT the whole run; the ACCP handshake (STARTMIC/STOPMIC/CMALI/CS-load/3RMICV)
  is canned/flag-only, so activation never reaches 3START. **Mirroring the 3022 production wiring
  (run-thread enabled, activation→3START) is exactly what converts it to the real CPU executing** —
  the byte-store X5ACT self-discovery fix becomes load-bearing at that point. Same wiring track,
  second interface.
- **Domain-carve fidelity** — PLACE-DOMAIN subfn order + description-file layout still [TC]; affects how
  faithfully D/E/G assert beyond the `NLL:` prompt, not whether RUN executes.
- **Which generation first** — the bridge already forks classic vs Samson5800; the choice is the §6
  decision.
