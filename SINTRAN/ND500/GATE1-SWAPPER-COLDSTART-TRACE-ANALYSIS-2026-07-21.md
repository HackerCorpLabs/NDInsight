# GATE-1 swapper cold-start trace analysis (2026-07-21)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\GATE1-SWAPPER-COLDSTART-TRACE-ANALYSIS-2026-07-21.md`

Advances `D4-PLAN-PHASES-AND-TASKS-2026-07-20.md` **Phase 1 / GATE 1** and closes its open action
"needs a LIVE SINGLE-STEP of the swapper around 0x0800913B with SWPINFO=0 to find WHERE the idle
branch is (or why we drive past it)". The single-step is now DONE.

**Evidence:** live run of `Nd500_D4_RunDomain_RealCpu_Capture` (2026-07-21, PASS observationally,
RUN->N500:). Instruction trace: `sintran-nd500-trace-swapper.txt` (194 instrs, entry 0x08000004 ->
crash). SWAPMAP dump: `d4_run.log` lines 4004-4084. Both in the NDInsight session scratchpad.

## The instruction-level path (verified, not inferred)

| PC | what | note |
|----|------|------|
| `0x08000004` | `init` (MACRO_STARTL cold vector, P=4) | cold-start entry - CORRECT [V] |
| `0x08000004`-`0x08000198` | init: banner compares, counters, ctsb | runs to completion |
| `0x08008237` | `if-kgo $8` (main loop head) | K clear -> NOT taken -> runs on |
| `0x0800823F` | `call $0xF80000FF,$4,...` = **MON 377B self-announce** | argc=4 [V log:4082] - **the announce HAPPENS** |
| `0x08008255` | `if-kgo $8` | K clear -> falls through |
| `0x080082EE` | `h riom 0x080240B4,0x080240BC,...` | reads work message -> **ALL ZEROS** [V log:4079] |
| `0x080082FF` | `r:= $0x80240BC` | R := message base |
| `0x0800831D`/`0x08008323` | `test 0x08028F60` / `if=go $33` | **0x08028F60 non-zero -> NOT taken** |
| `0x08009094`/`0x0800909A` | `test 0x08028F60` / `if=go $0xA9` (in sub 0x0800908A) | **0x08028F60 non-zero -> NOT taken** |
| `0x0800910B`-`0x0800913B` | build pointer from empty message, `laddr r2.(10)` r2=0 | -> seg-0 read protection violation -> "The Swapper stopped" |

## The discovery that changes the fix direction

**`0x08028F60` is the work-vs-idle mode discriminator.** It is tested twice (0x0800831D and
0x08009094); on BOTH an `if=go` would branch AWAY from the work-processing path if it were ZERO. It
is NON-ZERO, so the swapper commits to processing the message.

**There is NO "empty message -> idle" branch on this path.** The SWAPPER-K01 macro stand-in
unconditionally processes whatever message the RIOM returns. On an all-zero message it derefs a null
pointer (`r2=0`) at `0x0800913B`. So the plan's earlier framing - "the swapper should idle on no-work"
- does not hold for this code: **the swapper expects a NON-EMPTY message when it is activated.**

## What the emulator must therefore change (upstream of 0x913B)

The stop is NOT a missing swapper idle branch. It is that the swapper is **23B-activated
(MICFU=0x13 MSG_START, N5STA=0x0002 - observed on the final activate) with an empty work message**.
Two candidate fixes (Phase 1.1 SWPINFO-ordering, still cross-lane servicer + SINTRAN):

1. **Post the work message before activation:** ensure SINTRAN's `SWMESS`/`5ACTSWAPPER` fills SWMSG
   (with `SWPINFO`, offset `HSWPI=0o104`) at `0x00210718` before the swapper RIOMs it. Then the
   message the swapper reads at `0x080082EE` is non-empty and the pointer at `0x0800913B` is valid.
2. **Do not 23B-activate the swapper on cold-start with no work:** the swapper's cold-start
   (MACRO_STARTL) should init + announce + reach a doorbell-WAIT, and only be 23B-activated when real
   work (a non-empty SWMSG) exists. Requires knowing what SINTRAN does between cold-start and the
   first activate.

**Open (to settle which fix):** does SINTRAN legitimately 23B-activate process 0 on cold-start with
no work (=> fix 2, the emulator/servicer must not deliver that activation), or is the activation
correct and SWMESS simply hasn't posted SWPINFO yet (=> fix 1, ordering)? Distinguish with a live BP
on SINTRAN's `SWMESS`/`5ACTSWAPPER` vs the servicer's 23B dispatch: which runs first, and is
`0x00210718` non-zero when the swapper RIOMs it.

## ORDERING PROBE RESULT (2026-07-21) — corrects the fix direction above

Re-ran with the harness's built-in MESSBUFF `MpmTrace` (thread-tagged R/W over window
`0x420E00..0x421000`) + the ND-100-source dump at `0x420E30`. Decisive, and it OVERTURNS both
"fix" candidates above:

- **SINTRAN posts the message CORRECTLY and FULLY.** Thread T15 (ND-100/SINTRAN) writes `0x420E30`
  **1164 times**: `0x420E30-33 = FF FF FF FF` (link sentinel -1), `0x420E34-35` = N5STA cycling
  01/02/03, `0x420E3C-3D = 00 0B` = MICFU ResidentRead. Not a missing post, not an offset (the whole
  header region IS written), not a SWPINFO gap.
- **The problem is TIMING.** The swapper cold-starts, RIOMs `0x420E30`, and crashes **during
  PLACE-DOMAIN** ("Loading Swapper", log +43796ms, crash at line 4004) — while `0x420E30` is still
  zero (crash-time dump: all zeros). SINTRAN's 1164 writes to `0x420E30` come **later, during RUN**
  (post-crash). Proof: the crash-time source dump = zeros; the post-RUN `MpmTrace` = fully populated.
- **What starts it:** during "Loading Swapper" SINTRAN issues a **23B MSG_START** that the servicer
  routes to `OnStartProcessSamson` -> `WakeRunThread` -> swapper cold-start (P=4). The swapper is
  ALREADY running when it issues the `ResidentRead@0x420E30` (its own RIOM), so the start preceded it.

## ROOT CAUSE (evidence-backed) — it is the Track-A stand-in, NOT the 3022 bus interface

The 3022 bus interface and SINTRAN are both behaving correctly: SINTRAN starts the swapper during
"Loading Swapper" and posts its message; the interface delivers activations and RIOMs faithfully. The
blocker is that the **SWAPPER-K01 macro stand-in** (Track A's substitute for the real 5800 128-bit
CS swapper, which `CpuND500` cannot execute) has **no init-and-park cold-start**: it inits, announces,
then unconditionally enters a message-processing loop that expects a NON-EMPTY message. Cold-started
before real work exists, it derefs `r2=0` at `0x0800913B`. The real 5800 CS swapper would init and
idle/park; the macro stand-in does not.

**Consequence for the ND500-bus-interface lane:** there is no 3022/`NDBusND500IF` bug to fix here.
Resolution paths:
1. **Track B (proper):** run the genuine 5800 CS swapper on `CpuND5000` (octobus/microcode lane) — it
   would park correctly on cold-start-with-no-work. Off this (3022) lane.
2. **Stand-in park (workaround, this lane):** after the swapper's MON 377B announce, prevent the K01
   stand-in from entering the deref when no work is posted — e.g. treat the cold-start-with-empty-
   MESSBUFF seg-0 read as a benign park rather than a fatal trap. Semantically delicate (must not mask
   a genuine protection fault); needs a precise guard keyed on "empty MESSBUFF at RIOM + P in swapper
   init range", not a blanket seg-0 suppression.

## ATTEMPT + NEGATIVE RESULT (2026-07-21): silent park does NOT work

Tried option 2 in the least-invasive form: in `Nd500CpuProcessBridge.OnUnhandledTrap`, for the exact
signature (`trappingPc == 0x0800913B` + seg-0 fault offset `< 0x100`), return `true` (CPU stays parked
- the caller already set `stopMode |= WAIT`) WITHOUT calling `servicer.AnswerTrapStop`, so no fatal
trap record reaches SINTRAN. Compiled clean. **Result: the D4 re-run HUNG** (5-min inactivity dump)
instead of completing.

The single run is confounded (the boot disk is a writable image run 1 already wrote to and Setup does
NOT re-copy it per run; the D4 test is documented-flaky, Phase 6.2; console output was lost on abort),
so the hang is NOT cleanly attributable. BUT the design is independently suspect and that is the real
lesson:

**SINTRAN 23B-activates the swapper and then POLLS the mailbox for its answer (N5STA:=3).** A silent
park writes no answer, so SINTRAN waits indefinitely. The crash was actually *better-behaved* than the
silent park: it produced a fatal answer (STOPR=2) that let SINTRAN move on (print "Swapper stopped" ->
observationally PASS). Suppressing the crash WITHOUT substituting an answer just converts a clean
observable fatal into a hang.

**Consequence:** a working stand-in fix must **answer the activation benignly** (a non-fatal "no work /
init complete" completion SINTRAN accepts and proceeds from), NOT merely suppress the trap. That is the
faked-swapper-answer path the project deliberately removed (`Nd500CpuProcessBridge` `OnMonitorCallRestart`
note: "old intercept existed only to keep a FAKED, codeless swapper parked but alive"). This reinforces
Track B (the genuine 5800 CS swapper on `CpuND5000`, which answers correctly) as the clean fix. The
attempt was **reverted** to restore the known-good observational-PASS tree.

## What is NOT the problem (ruled out by this trace)

- The MON 377B announce is fine (it happens, argc=4). A "don't run the swapper" gate is wrong.
- The cold-start vector is fine (P=4 MACRO_STARTL, matches Phase 2.1).
- MMU mapping at entry is fine (program+data enabled; the seg-0 fault is a *consequence* of r2=0,
  not a mapping bug - `InstallSwapperMapping` dom=0 progSeg=1 dataSeg=1 P=0x08000004 [V log:4075]).
