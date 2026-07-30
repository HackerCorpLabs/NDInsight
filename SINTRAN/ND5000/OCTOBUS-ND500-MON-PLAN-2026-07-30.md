# PLAN: get the ND-500 monitor fully working over the OCTOBUS

**Date**: 2026-07-30
**Scope**: everything needed to move from "the administrative commands work" to "ND-500 user
programs run", on the octobus transport (ND-100 + ACCP + 5MPM + ND-5000/SAMSON).

Companion documents:
- `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` - the shutdown path
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - the numbered gaps G1-G10

---

## 0. Definition of done

Three levels, in order. Each is a real milestone; do not skip ahead.

| Level | Meaning | Test that proves it |
|---|---|---|
| **L1 Administrative** | every monitor command the harness drives completes | DONE - full ladder green |
| **L2 Clean lifecycle** | a domain can be placed, run, and terminated, and the machine shuts down without `ERRFATAL` or `ESPTIMOUT` | the ladder plus PLACE/RUN/NLL. NOTE: "no 244B during the run" was removed as a criterion on 2026-07-30 - a 244B is sent in every run including a clean one, so its absence was never a valid success signal. What matters is that `accpIdle` ends `False`. |
| **L3 Multi-CPU / MFbus** | more than one ND-5000, and MFbus memory controllers answering | out of reach today, see the backlog |

**L1 is reached.** `memory-configuration, status, start-swapper, who-is-on,
list-active-processes, process-status, version, list-standard-domains, list-domain,
list-table last-n500-msg, exit, stop-system` all complete, the swapper runs its message loop and
allocates 7110B pages, and `stop-system` really halts the ND-100 (runState RUN -> OPCOM).

**What L1 does NOT mean:** no domain has been placed or run. "The monitor works" currently means
the administrative and swapper-bring-up commands work.

---

## 1. Workstream A - G10: why does SINTRAN terminate the ACCP mid-run?  **[TOP PRIORITY]**

Everything else is downstream of this, so it goes first.

**Symptom**: across a whole boot the ND-5000 station receives ZERO kicks. SINTRAN sends exactly one
kick in a run - `CLRKICK` (3) at `stop-system`, frame `0xB843` to station 56 (70B). It is correctly
formed, correctly routed, and delivered. The station drops it because `_accpIdle` is set.

**That drop is correct.** `_accpIdle` is set only by emergency **244B TERMINATE ACCP**, which stops
the microprogram; kicks are delivered to the microprogram. Real hardware behaves the same. Do NOT
relax the `_accpIdle` guard - that hides a real timeout behind behaviour the hardware does not have.

**The actual defect**: 244B is what the monitor sends **on ACCP timeout** (ND-05.020.01 ch. 5.3.9).
So one of our ACCP exchanges goes unanswered, or answers too late, and SINTRAN gives up.

### Steps

1. **Instrument the 244B arrival.** In `OctobusND5000Station.HandleEmergency`, record: a monotonic
   timestamp, the last N ACCP commands received with their replies, and whether each got a reply at
   all. Surface as a test-visible field (NOT `Log()` - station logging does not reach TestContext;
   that mistake already cost a full investigation cycle).
2. **Run the ladder and read the record.** The command immediately before 244B, and any command with
   no reply, is the suspect.
3. **Decide unanswered vs late.** SINTRAN's timeout is a wall-clock/counted wait on the ND-100 side.
   If the reply exists but arrives after the deadline, the fix is latency, not a missing handler.
   These need different fixes - do not conflate them.
4. **Fix the specific exchange**, then confirm 244B no longer arrives during a normal run.

**Done when**: a full ladder run records zero 244B, `_accpIdle` stays false, and the `CLRKICK` at
`stop-system` reaches `ExecuteClearFunctions()` so `X5CLR` reads 0 afterwards.

**Watch for**: this may be the same root as the standalone ACCP reporting
`MFbus controller not found at Octobus stations 2-7`. If the ACCP is waiting on a memory controller
that never answers, the timeout is a symptom of the backlog item in section 6, not of our ACCP
command layer. Establish which before building anything.

---

## 2. Workstream B - finish the kick table

G1 (kick 3), G4 (kick 2) and G6 (unhandled-kick diagnostic) are done. Kick 3 was settled by
EXECUTING the real B30 microcode at `OCB_KICK03` (CS 025522), which writes `X5CLR := 0`,
`X5CCL := 1`, `X5PRO := -1`.

| Gap | Work | Priority |
|---|---|---|
| **G2** kick 6 `IDLEKICK` | `TER51` (`MP-P2-N500.NPL:2950`) sends it in a loop and leaves only when `X5PRO` reads -1, else `TER52: ESPTIMOUT`. So **every ND-500 terminate currently times out.** Minimum fix: `X5PRO := -1` plus parking the CPU. | **P1** |
| **G3** `OCB_CLNUP` | Reached from kicks 4/5/6. It REQUEUES the in-progress message (`N5STA := 1`, back to `MSGN500`) - it does NOT discard. An implementation that drops the message is wrong in a way that surfaces much later as lost messages. Prerequisite for an honest G2. | P2 |
| **G7** `X5ACT` re-arm | Nothing writes `_extBlockBase + 0x0A`. Probably benign on the control-store-derived path (which triggers on a write to the cell, not a `0xFFFF -> 0` transition) but NOT on the `wasMinusOne` fallback. One assertion in the existing mailbox tests either closes it or promotes it. | P2, cheap |
| **G9** `OCB_WAITSEX` | Spin on global header word 0o24. The carve says bit 15 of the mask arms it, but executing the routine with bit 15 SET produced the identical writes and never entered the spin. Trigger unknown - **do not guess it into code.** | leave open |

**Method for G2/G3**: same as G1 - run the real microcode at `OCB_KICK06` (CS 025561) and
`OCB_CLNUP` (CS 025570) and read the write trace. That technique is written up in the
`nd5000-microcode` skill. It beats reading the carve summary, which has been imprecise twice.

---

## 3. Workstream C - L2: actually place and run a domain

This is the substance of "make it work", and nothing here is started.

1. **Floppy fixture.** PLACE/RUN need the 210319H02 floppy image wired into the octobus harness.
2. **PLACE** a standard domain; confirm the monitor reads the description file and builds the
   domain tables.
3. **RUN**; confirm the ND-5000 executes user macro code from the placed domain, not just the
   swapper.
4. **NLL / MON handling**: `NLL:` comes from the DOMAIN's MON call via MCHANDEL, not from the
   swapper - so this exercises a path the current ladder never touches.
5. **Terminate cleanly** - which needs **G2** (kick 6), so sequence C after B.

**Expect MMU work.** A real domain has genuine capability and page tables; the swapper path we
currently exercise does not prove those. Two facts to keep in front: translation is always on, and a
zero capability is a PROTECT VIOLATION decided before the physical segment table is consulted. A
protect violation during a MON call is answered in place and CONSUMES the activation message, which
then makes the next MON call fail for a reason that looks nothing like an MMU problem. That exact
chain has already burned a full day.

---

## 4. Workstream D - harness reliability

Small, but it protects every measurement above.

1. **`status` intermittently reports STALL** at the 300s wall-clock limit. It is a timeout, not a
   hang. Either raise it, or better, replace the wall-clock wait with a progress-based one.
2. **Rule already learned twice, keep applying it**: a STALL means "not within N host seconds",
   never "it never happened"; and a success marker must be something the machine actually emits on
   the channel being scanned. Both mistakes have manufactured phantom root causes in this project.
3. The virgin-pack fix is in (the working image is now always recopied). Keep it - without it, runs
   are not comparable.

### D.4 The ND500 suite has ONE intermittent failure, and it MOVES  [root cause found, fix NOT applied]

**Corrected diagnosis (2026-07-30, autonomous check).** I previously called this "a cross-fixture
state leak needing bisection". That was wrong, and the correction matters because it would have sent
the next person bisecting for a leak that does not exist.

Two consecutive full runs of the SAME code:

| Run | Failing test | Result |
|---|---|---|
| 1 | `FaultingAccess_RestartsAndCompletesIdentically` | 1891 pass / 1 fail |
| 2 | `AttachRealCpu_ThreadedClassic_TakenStartWakesAndStopCompletesAsync` | 1891 pass / 1 fail |

Different test each time, and `FaultingAccess` passes 8/8 in isolation. So it is intermittent, and
both failures are resource/timing shaped (one threaded-async deadline, one read-back returning
`0xFFFFFFFF`).

**The cause: 256 MB allocated PER TEST.** `NDSharedMemory`'s constructor allocates two 128 MB byte
arrays (`sharedSegment` + `noCacheSegment`) with no pooling, no reuse and no disposal:

```csharp
sharedSegment  = new byte[SHARED_SEG_SIZE];   // 128MB
noCacheSegment = new byte[NO_CACHE_SEG_SIZE]; // 128MB
```

`new NDSharedMemory()` appears in roughly a dozen `[SetUp]` methods across the nd500if fixtures, plus
indirectly via `NDBusND500IF` and `NDBusOctobus`. `[SetUp]` runs PER TEST, so a full run churns
hundreds of gigabytes through the large-object heap. That is exactly the shape that makes a
timing-sensitive threaded test miss a deadline and an allocation-heavy test read back garbage.

**Deliberately NOT fixed autonomously.** The obvious fixes are design decisions with real trade-offs
and they touch PRODUCTION code:
- lazy per-segment allocation - behaviour-preserving, but saves little if both segments get touched;
- pooling / sharing one buffer across instances - saves the most, but risks cross-instance bleed,
  which would be a genuinely dangerous change to make while the user is away;
- keeping it per-instance but making the size configurable so tests can ask for a small window.

The third is probably right (tests do not need 256 MB), but choosing is the user's call. **Do not
"fix" the flaky test by retrying it** - the allocation churn is the bug.

---

## 5. Workstream E - remaining test debt

| Item | State | Note |
|---|---|---|
| `Emulated.Tests.ND100` | 361 pass / 0 fail | green |
| `Emulated.Tests.ND500` | 1891 pass / 1 fail, **and the failing test MOVES** | See D.4 below - it is intermittent, not an ordering leak. My earlier "cross-fixture state leak" diagnosis was WRONG. |
| nd500x `instruction_validation` | 431 -> **269** | Two causes found: the generated test file was stale (resynced), AND the generator had the double register pair SWAPPED (`e1` high instead of `a1`). Proven by running RetroCore's own CPU against the file RetroCore generated - it failed the same cases as the C port. 27 sites fixed. Remainder is a separate expected-VALUE bug in the float generator. |
| nd500x `ote_instructions` | 1 fail | `CTE1 loaded in privileged mode` |
| nd500x `mon_calls` | 2 fail | "could not open scratch file" - looks environmental |
| ND-5000 microcode suite | 39 fail | loop/frame/return/double/RIOM-flag families; has its own campaign doc `COVERAGE-GREEN-PLAN-2026-07-30.md` |

**Standing rule applied throughout**: 11 of the 12 tests fixed today were stale expectations, not
broken code. When a test fails, read the implementation's citations first - if it cites a manual
section, a microcode address or a dated live trace and the test cites nothing, the test is the
suspect.

---

## 6. BACKLOG - MFbus controller devices (stations 2-7)

**We do not know how to build these yet. Recording it so it is not lost.**

What is known:
- The standalone ACCP reports `MFbus controller not found at Octobus stations 2-7`.
- Stations 2-7 are the MFbus memory controllers - the follow-up to MPM-5.
- That reframes the ACCP selftest failures as "no memory controller answered", rather than a fault
  in our ACCP command layer.

Why it is hard: we have no octobus "client" device model at all. Every station we implement today is
either the ND-100 card, the ND-5000, or the SCSI DIOC. An MFbus controller is a different kind of
participant and we have no carve, no manual section located, and no trace of one answering.

Suggested first moves, cheapest first:
1. **Find out whether L2 needs them at all.** If the monitor only probes stations 2-7 and tolerates
   silence, this stays backlog. If the ACCP timeout in Workstream A traces back to this probe, it
   becomes urgent - so A answers this question for free.
2. **Locate the specification** before writing code. `ND-05.017.01` is the octobus info-byte
   reference; the MFbus controller's own document has not been identified yet.
3. **Model presence only, first.** A station that answers "present" to the probe and nothing else may
   be enough to stop the selftest complaint, and is a much smaller commitment than emulating memory
   behaviour.

Do not start step 3 before step 1 says it is needed.

---

## 7. Suggested order

```
A (G10, ACCP timeout)            <- unblocks everything, and answers the MFbus question
  -> B/G2 (kick 6)               <- last P1 in the kick table; needed for clean terminate
     -> B/G3 (OCB_CLNUP requeue)
  -> D (harness reliability)     <- cheap, protects all measurement; can run in parallel
     -> C (PLACE / RUN / NLL)    <- the real L2 milestone
        -> E (test debt)         <- ongoing, does not block C
           -> backlog: MFbus
```

**Start with A.** It is the only item that both unblocks the rest and tells us whether the MFbus
backlog is urgent or can stay parked.
