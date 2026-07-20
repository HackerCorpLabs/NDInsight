# ND-500 D4 RUN Blocker - Verified Finding (2026-07-19)

**For**: architect (microengine-track scoping) + Ronny. **From**: ND-500 bus-interface / production-wiring LLM.
**Bottom line**: the D4 acceptance (`RUN` on `LINKAGE-LOAD-H02` reaches `NLL:`) is **blocked SINTRAN-side,
upstream of the CPU wiring**. Attaching a real `CpuND500` and closing the G1/G2 wiring gaps is **not
sufficient** to reach D4 on this SINTRAN-L image. This revises the scoping doc's premise that D4 is
"purely an execution-reachability gap - the domain loads clean".

**ROOT CAUSE (carve-confirmed, see section 8):** RUN's "well defined program" state is set ONLY on the
all-success path of the monitor's PLACE handler (START-STANDARD-DOMAIN @ `043011`, a chain of MON 60
place calls). That chain requires SINTRAN's REAL placement to complete - including the **swapper
(process 0) actually running** to build the ND-500 segment/process tables. The emulator **fakes the
swapper (22B StartProcessZero)**, so placement never completes and RUN never sees a runnable program.
**The faked swapper is the true upstream D4 blocker.**

Grades: **[V]** verified from a live harness run, **[?]** working model, **[TC]** to-carve.

---

## 1. What was tested

Added `Nd500_D4_RunDomain_RealCpu_Capture` to the boot harness
(`Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs`). It:

1. Boots SINTRAN III L on the SMD pack servicer-only (reliable), logs in SYSTEM, enters `@ND-500`.
2. Attaches a REAL `CpuND500` (Classic) to the 3022 via `AttachRealCpu` (16 MB local bus + shared MPM
   Port B), thread parked at WAIT. Boot + login stay servicer-only; the engine is attached just before
   the domain work, so the flaky boot path is untouched.
3. `PLACE-DOMAIN (FLOPPY-USER)LINKAGE-LOAD-H02` (the ndtool-placed NLL domain, C3-confirmed well-formed,
   SA:26000006721).
4. `LIST-ACTIVE-SEGMENTS OWN`, `LIST-PROCESS-TABLE-ENTRY OWN`.
5. `RUN`, driven through the instrumented pump (MICFU histogram + ND-100 PC histogram + ND-500
   PC/stopMode), waiting for `NLL:` or `N500:`.

Two runs total: one clean, and one with an extra `LOAD-SWAPPER`/`START-SWAPPER` re-init (a hypothesis
that was disproven - see section 3).

---

## 2. Verified results (clean run) [V]

- **PLACE-DOMAIN loads segments.** It issues a long burst of `14B ResidentWrite` mailbox messages
  (segments -> ND-500 memory) followed by one `21B RegisterWrite` (a process register image). No error
  is printed; it returns to `N500:`.
- **But SINTRAN's own tables are EMPTY afterward.** `LIST-ACTIVE-SEGMENTS OWN` and
  `LIST-PROCESS-TABLE-ENTRY OWN` both print nothing (blank -> `N500:`).
- **RUN refuses to activate.** `RUN` prints `NO WELL DEFINED PROGRAM IN MEMORY` and returns to `N500:`.
  During RUN the ONLY 3022 traffic is the background `ReadMicroVersion` (3RMICV) watchdog poll - there
  is **no StartProcess (23B), no StartProcessZero (22B), no activation of any domain**.
- **The CpuND500 never runs.** Before, during, and after RUN: `ND-500 PC=0x00000000 stopMode=WAIT`.
  The engine is never handed a start, because SINTRAN never issues one.

**Conclusion [V]**: RUN fails a SINTRAN-side precondition ("a well defined program in memory") and
never crosses the interface. The blocker is therefore NOT in the emulator's CPU wiring, the servicer's
StartProcess routing, or the ND-500 ISA. The domain's segments reach ND-500 memory, but SINTRAN does
not register the domain as a runnable program, so RUN short-circuits before any activation.

---

## 3. Disproven hypothesis - the swapper stub (22B) [V that it is NOT the fix here]

The servicer stubs `22B StartProcessZero` (start-swapper) with a synthetic answer and a TODO
("actually EXECUTING the swapper needs the ND-500 CPU integration"). Hypothesis: the swapper must
execute to build the ND-500 segment/process tables that RUN reads; wire 22B to the CpuND500 like 23B.

Tested by (a) routing 22B through `ProcessHost.OnStartProcess` (Classic + host attached), and
(b) re-running `LOAD-SWAPPER`/`START-SWAPPER` on the attached CPU before PLACE-DOMAIN. **Disproven:**

- **`LOAD-SWAPPER SWAPPER` prints `> Loading Control Store`.** On this image LOAD-SWAPPER reloads the
  128-bit CONTROL STORE (microcode), it does NOT load a swapper *process* register image. So no 21B
  process-0 context is ever stashed, and `OnStartProcess` has nothing to start (it declines).
- **The CS reload STOPS the micro clock** (3022 STATUS `ND500MicroClockStopped` = 0x0200). PLACE-DOMAIN
  then emits only bare micro-clock activates (`mar=0`) and loads **no** segments - i.e. the re-init made
  the state strictly worse than the clean run.
- RUN still printed `NO WELL DEFINED PROGRAM IN MEMORY`.

Both speculative changes were **reverted**: the servicer is back to the synthetic 22B answer (with an
expanded comment recording this investigation), and the harness test keeps only the clean
attach -> place-domain -> run path.

---

## 3a. Authoritative meaning of the error (manual) + refined root-cause direction

**ND-60.136 (ND-500 Loader Monitor), error-message section [V]:**
> **NO WELL DEFINED PROGRAM IN MEMORY** - "A RUN, CONTINUE or GO command was specified before any
> PLACE, DEBUG-PLACE or RECOVER-DOMAIN command was executed."

So RUN's precondition is simply "a domain was successfully PLACE/DEBUG-PLACE/RECOVER-DOMAIN'd." The
test DID run `place-domain` first with no visible error - therefore **PLACE-DOMAIN is not actually
completing the placement** (the "program placed" state RUN checks is never set), even though it emitted
load traffic. That is the real thing to carve.

**Refined observation (multiple runs, some non-determinism from the flaky boot) [V]:**
- In one run, PLACE-DOMAIN emitted a long burst of `14B ResidentWrite` (segments) + a `21B RegisterWrite`
  (process image) - a genuine load attempt - yet placement still did not stick.
- In another (clean-boot) run, `place-domain` instead printed **`> Loading Control Store` /
  `> Loading Swapper`** and did only bare micro-clock activates (`mar=0`, STATUS `ND500MicroClockStopped`),
  with NO 21B RegisterWrite - i.e. it got stuck at the CS-load + swapper-start phase of placement.
- Common thread: **PLACE-DOMAIN itself drives the CS load + swapper start as part of placing a domain,
  and the swapper (process 0) is FAKED in the servicer (22B StartProcessZero answered synthetically -
  the CPU never executes it).** The strong working model [?] is: the swapper must actually RUN to
  allocate ND-500 memory and build the segment/process tables for the placed domain; because it is
  faked, placement cannot complete, so RUN reports no well-defined program. This is exactly the
  "are we faking the CPU?" concern - a real-vs-faked audit of the CpuND500 integration is in progress
  to confirm precisely which responses are synthesized.

## 4. Why this is consistent with the image being ND-5000-provisioned [?]

Prior finding (memory `nd500-microcode-files.md`, and `nd500-mon-init-docs`): the SINTRAN-L image's
`CONTROL-STORE.DATA` is **262144 bytes = 128-bit x 16384 = ND-5800 format**, and the harness `VERSION`
command self-reports **micro program 11930 = 11xxx = ND-5000 family**. So this image is provisioned for
an ND-5000/5800 CPU, and `LOAD-SWAPPER` loading a 128-bit control store fits that exactly.

Note the `CpuND500` functional interpreter does NOT execute the 128-bit microcode (the CS is an opaque
version/model blob to it), so CS *width* alone would not stop it from interpreting the domain's ND-500
*macro* code. The blocker observed here is a step earlier: SINTRAN never reaches activation. Whether the
"well defined program" precondition is (a) a pure ND-100-side monitor bookkeeping gap, or (b) tied to
the ND-5000 provisioning / a table the ND-5000 microcode would populate, is **[TC]** - a SINTRAN ND-500
monitor carve (nd-500-mon J04 / N500M, the RUN command's precondition and what PLACE-DOMAIN sets that
RUN reads).

---

## 5. Impact on the microengine-track plan

- **G1/G2 (production wiring + config) remain valid and worth doing** - a production machine path that
  attaches a `CpuND500` behind the 3022 (mirroring the octobus `AttachNd5000Cpu`) is real, needed
  infrastructure, and the harness proves the one-call `AttachRealCpu` recipe works. But by itself it
  **does not reach D4** on this image.
- **The D4 acceptance is gated on a SINTRAN-side carve**, not on the wiring. The scoping doc placed the
  domain carve "in PARALLEL, off the execution critical path" - this finding moves it ONTO the critical
  path for D4: without understanding RUN's precondition and what PLACE-DOMAIN must leave behind, RUN
  never activates and the engine is never exercised end-to-end via a real operator RUN.
- **Open decision (for Ronny/architect)**: whether to (i) carve the classic/ND-500 monitor RUN
  precondition on this image, (ii) obtain/boot a genuinely classic-500-provisioned SINTRAN image (144-bit
  CS, e.g. Bo Goran's 10509/10609 floppy when it arrives) where the 3022 classic path is the intended
  topology, or (iii) pivot D4 to the octobus/ND-5800 path that this image is actually provisioned for.

---

## 6. Artifacts

- Test: `Nd500_D4_RunDomain_RealCpu_Capture` in
  `Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs` (observational; asserts only that RUN
  returns to a prompt, never hangs - it is a diagnostic/regression capture of the exact blocker).
- Servicer: `Emulated.HW\ND\CPU\ND500\Servicer\Nd500MicrocodeServicer.cs` 22B comment updated with the
  investigation result (behavior unchanged).
- Raw transcripts (this session): scratchpad `proc-progress.txt`,
  `sintran-3022-trace-d4-rundomain.txt`, `sintran-boot-capture-d4-rundomain.txt`.

---

## 7. CpuND500 integration audit - REAL vs FAKED (2026-07-19) [V from code]

Ronny asked whether the classic 3022 path actually drives the CpuND500 (live register read/write,
real start/stop) or fakes it. Verified per-operation map (`Nd500MicrocodeServicer.cs`,
`Nd500CpuProcessBridge.cs`, `CpuND500.ProcessControl.cs`):

| Mailbox op | Class | Anchor |
|---|---|---|
| 23B StartProcess / 25B TrapContinue | REAL (arms + wakes real thread, executes) | servicer:487 -> bridge:89 -> ProcessControl:84 |
| 24B/26B MonitorCallContinue | REAL (writes live cpu.regs, write-backs, resumes) | servicer:527 -> bridge:163 |
| MON/trap STOP answers | REAL (live saved-P + live memory arg values) | Trap.cs:249 / bridge:247 |
| 13B/14B Resident read/write | REAL memory (shared MPM array; CPU sees via Port-B backing) | servicer:284/315 |
| **20B RegisterRead** | **FAKED/STASHED** - serves stashed 21B image, never cpu.regs | servicer:442-455 |
| **21B RegisterWrite** | **FAKED/STASHED** - only stashes, never writes cpu.regs | servicer:402-408 |
| **22B StartProcessZero (swapper)** | **FAKED** - synthetic answer, CPU never starts | servicer:328-338 |
| **3RMICV version / cpu-parameter** | **FAKED (hardcoded 0x2E9A / 0)** | servicer:50, 350-351 |

**Overarching:**
- **A. No live register-read path exists on the classic 3022 servicer.** The only register value ever
  returned to the ND-100 is P (via MON/trap stop). L/B/R/I1-4/A1-4/E1-4/status are never read from
  cpu.regs. The register file is effectively write-one-way at start - and even the start applies only
  register 0 -> P (ProcessControl.cs:92-102); the rest of the 21B image stays unmapped in
  PendingRegisterImage.
- **B. Start/stop genuinely controls the CPU thread for 23B/25B/24B/26B** (StopMode/WAIT + WakeRunThread
  + real ExecuteOneClockCycle). **EXCEPTION: 22B StartProcessZero never touches the CPU** - it only
  flips 3022 status/answer bits.
- **C. Synthesized-and-returned-as-if-from-the-ND-500:** micro version (0x2E9A), cpu-parameter (0 on
  classic), 20B register reads (stash/zeros), 22B swapper answer.
  - NOTE / correction: the mapping pass speculated `CpuParameter=0` might trigger "wrong microprogram"
    and gate PLACE-DOMAIN. The servicer's own comment cites a byte-carve (carver R5) that **SINTRAN does
    NOT consume this halfword**, so 0 is a safe default. Trusting the carve over the speculation,
    CpuParameter is **probably NOT** the PLACE-DOMAIN gate - so it was not worth a flaky boot run to
    probe. It still should be sourced correctly (plan item 3) for the octobus A1 path and honesty.

**Plan additions ("register the CPU correctly")** - tracked as tasks:
1. Wire 20B RegisterRead to the live cpu.regs (needs a register-number -> regs field map).
2. Apply the FULL 21B image at start (P + L/B/R/I/A/E), not just P.
3. Source 3RMICV MicroVersion/CpuParameter from the loaded control store, not a hardcoded property
   (the G2 config item; needed for octobus A1. NOTE: carver R5 says SINTRAN does not consume
   CpuParameter, so this is correctness/identity, NOT the expected D4 fix).
4. 22B StartProcessZero -> real CPU start, once the swapper-context source is resolved (the D4 blocker
   is upstream: PLACE-DOMAIN drives a control-store load + swapper start, not a process-0 register image).
5. Add a test asserting the CPU reads back what a 14B ResidentWrite wrote (guards the shared-MPM invariant).

---

## 8. RUN precondition carve result (nd-500-mon J04) [V from disassembly]

Carve of `nd-500-mon-j04.prog.asm` + `-bank2.bin` + `mon60-callers\START-STANDARD-DOMAIN\README.md`:

- **The error is monitor-internal** (MON-DEBUG:PROG, not SINTRAN). Message-descriptor table at bank-2
  word `022500` (3-word entries `{text-addr, 0, length}`); "NO WELL DEFINED PROGRAM IN MEMORY" =
  entry #14 (`016B`) @ `022552`, text @ `023445`, length `041`=33 chars. [V]
- **RUN / GO / CONTINUE all issue RUNN = MON 60 subfn `12B`** ("start program"). Command-name table
  (bank 2): `RUN`@`012473`, `GO`@`012456`, `CONTINUE`@`012466`, all -> thunk `146346` (RUNN). The RUNN
  issue site is `030624`-`030636`; MON-60 error join at `030521`. [V]
- **The runnable-state test itself (the exact branch to "print message #14") is [TC]** - not statically
  resolvable: block-structured PLANC with 439 ENTER trampolines, the message-print path uses
  computed/relative addressing (table base `022500` / entry `022552` / msg-number are not stored as
  constants anywhere in either bank - full-image scan), and the command-name -> handler binding is the
  long-standing open question 9. Isolating it needs a LIVE single-step (breakpoint @ `030624` or a
  data-watch on the descriptor read), not more static scanning. Cell `042150/042151` (written by RUN @
  `030622/030623`) was RULED OUT - it is a shared 32-bit scratch/address global, multi-writer, and RUN
  writes it but never reads it. [V ruled-out]
- **KEY (Q4): PLACE-DOMAIN is NOT ND-100-only on this image.** [V]/[?] The monitor's PLACE handler is
  START-STANDARD-DOMAIN @ `043011`-`045462`: a sequence of MON 60 calls to SINTRAN - `130B`
  START-STANDARD-DOMAIN (thunk `146712`), `140B` SRESPL, `55B` SPLAC, `6B` PLACE per-segment (thunk
  `146335`), `56B` EPLAC. Any MON-60 error takes the routine's LEAVE(value) path; the "program defined /
  runnable" write is reached ONLY when they ALL succeed. Those calls run inside SINTRAN's level-12
  driver, and on this ND-5000/5800-provisioned image placement drives a CONTROL-STORE load + swapper
  (process-0) start. **Because 22B StartProcessZero is faked, the swapper never executes, the place
  chain never fully succeeds, and RUN reports message #14.**
- **Secondary gates [V]:** control store loaded (ECSLOAD `2032B` gateway @ `146304`;
  `$CONTROL STORE NOT SUCCESSFULLY LOADED` exists), swapper started, ND-500 available
  (SET-ND-500-UNAVAILABLE flag).
- **Contradiction noted:** `SINTRAN-DOMAIN-SETUP-DEEP-DIVE.md` claims PLACE is ND-100-only with
  demand-paging - but that doc is a [?] reconstruction, not byte-verified. Trust the live D4 finding +
  the carved MON-60 chain over it.

**Consequence for the fix**: reaching D4 requires the swapper (process 0) to GENUINELY EXECUTE so the
MON-60 place chain completes. On this ND-5800 image the swapper is not started via a classic 21B
register image (none is stashed on the 3022 path - section 3a), so the swapper-context source for a real
22B start is itself [TC]. This makes the swapper-execution path (and likely the octobus/ND-5000 model
the image is provisioned for) the real D4 work item - a decision point recorded in section 5.

---

## 9. Swapper-start mechanism on this image - carve result (2026-07-19)

Ronny directed a classic-3022 swapper carve (staying on the 3022; ND-5000/octobus is a separate LLM).
Result from the boot harness (instrumented PLACE-DOMAIN):

- **PLACE-DOMAIN starts the swapper via the MICRO-CLOCK / control-store path, NOT the classic
  21B(image)+22B(start) mailbox sequence [V harness, ? mechanism].** When the CS/swapper are not yet
  loaded, PLACE-DOMAIN prints `> Loading Control Store` + `> Loading Swapper` and drives LCON5 with
  `ctrl 0x20/0x24` (micro-clock stop/start), leaving STATUS `ND500MicroClockStopped` (0x0200). No `22B
  StartProcessZero`, no `21B RegisterWrite`, no `14B` are issued for the swapper on that path; the
  CpuND500 stays parked (PC=0). So the swapper is 128-bit CONTROL-STORE microcode started by the micro
  clock - the 5800 provisioning - which the FUNCTIONAL CpuND500 (a macro-instruction interpreter) does
  not execute.
- The `14B ResidentWrite` + `21B RegisterWrite` seen in an earlier run were the **DOMAIN** load + the
  domain's process context, not the swapper (that run had the CS/swapper already loaded from an earlier
  step, so PLACE-DOMAIN went straight to the domain).
- **Correction of the record**: routing `22B` to `ProcessHost.OnStartProcess` (like 23B) was applied and
  reverted TWICE. It is the CORRECT wiring for a genuine classic-500 image (21B stashes P, 22B starts),
  and the flawed first refutation (re-running LOAD-SWAPPER, which reloads the CS and stops the micro
  clock) was not a valid test - but on THIS image 22B is never exercised for the swapper, so the routing
  cannot reach D4 here and was reverted to keep the servicer honest (22B stub + a comment recording all
  of this).

**Residual uncertainty [TC]**: the harness boot is non-deterministic about WHEN the CS/swapper load, so a
definitive answer to "how does SINTRAN's ND-500 driver start the swapper on the 3022 for this image"
should come from carving the SINTRAN driver (5STDRIV / the ND-500 subsystem init), not harness
observation. The strong working model is micro-clock/control-store start.

**Path options to D4 on the classic 3022 (for decision):**
1. **Functional swapper** - reimplement, in C#, the EFFECT of the swapper (populate the ND-500
   segment/process tables it builds) so PLACE-DOMAIN's MON-60 chain completes and RUN issues a `23B`
   StartProcess for the DOMAIN (which CpuND500 CAN interpret). Mirrors how Nd500MicrocodeServicer already
   reimplements the mailbox microcode in C#. Requires carving what tables the swapper builds and where.
2. **Genuine classic-500 image** - use a 144-bit-CS image (e.g. Bo Goran's 10509/10609) where the swapper
   IS started via 21B+22B and the (already-written) 22B routing would run it on CpuND500.
3. **Accept that this specific 5800 image's swapper needs the ND-5800 microengine** (separate octobus/5000
   LLM) - out of this track's scope.

---

## 10. 5STDRIV swapper-start carve - DEFINITIVE (2026-07-19)

Driver = **`5STDRIV`** in `SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL` (banner @octal `134607`
"N500 - P12DCN - 5STDRIV ... Level 12 ND5000 communication driver kernel"). Byte-verified:

- **22B StartProcessZero is a WATCHDOG (`P0START` @`134500`), NOT the swapper start [V].** It inserts a
  WATCHDOG message into the ex-queue. This DEFINITIVELY closes the 22B rabbit hole - the twice-tried
  22B->CPU routing was aimed at the wrong message; my final revert was correct.
- **The classic swapper start is 23B StartProcess, which is ALREADY REAL/wired [V].** LOAD-SWAPPER
  (MON-60 7B SWLOD -> LDSWA @`143551`) does 14B(image)+12B+**21B (3WREG, P=swapper entry)**; START-SWAPPER
  (MON-60 54B STSWP -> RUNSW @`163621` -> MSWSTART @`133661`) sets `MICFU := 3START` = **mailbox 23B**.
  Since 23B already routes to `CpuND500.StartProcessFromRegisterImage` (section 7), **on a genuine
  classic-500 image the swapper would run on CpuND500 today** - the only gap is that 23B applies just
  reg0->P; the full 21B context must be applied (plan item / task: "apply full 21B image at start").
- **This ND-5800 image DISABLES the classic path [V]:** on the 5800 B30 microcode **MICFU 21B = MSG_ILLEG**
  (classic-only). So 14B/21B/23B is not used; the swapper is 128-bit control-store microcode started via
  the **micro clock (LCON5 ctrl 0x20/0x24)**, which the functional `CpuND500` cannot execute. The
  exact CPU-type branch in `LDSWA` (segment 030-S3SM5, not in the NPL tree) that selects control-store
  load vs the 14B/21B wire is [TC].

**What the swapper builds (functional-swapper input) [V shape]:** descriptor tables in ND-500 memory
(from `swapper\swapper-k01-handlers.md`): Table A "slot table" @ `0x08038000`, 0o144 words/entry (primary
per-segment/process descriptors; `[0x128A4]` = valid-slot count); Table B (seg-4, 0o400 stride); Table C
(seg-6 page map, per-page state bitfields); Table D (seg-5); a 29-entry fn-code table @ DSEG `0x26198`
(handler idx 24 = create-segment-descriptor: allocate slot `id*0o144`, bmove a 0o144-word template from
`[0x437274]`, init bitfields). Page moves via RPHS/PCTSB/DCTSB; disk I/O via MON 377B (MON 255 N5SWAP).
**[TC]:** whether the tables RUN/LIST-ACTIVE-SEGMENTS read are this Table A or a separate ND-500 System
Monitor (030-S3SM5) structure mirrored in the ND-100 `S500S` process-descriptor array (process 0 =
`5SWPROC`).

**Q5 - can C# replicate the swapper without running microcode? YES [?, well-supported]:** the swapper's
job is deterministic descriptor-table bookkeeping over known ND-500 memory structures + page moves + MON
377B disk requests - the exact pattern `Nd500MicrocodeServicer` already uses to reimplement mailbox
microcode. A functional swapper would populate the segment/process descriptor tables so the MON-60 place
chain (55B/6B/56B) completes and RUN issues a **23B StartProcess for the DOMAIN** (which CpuND500 runs).

### Refined path decision
- **A. Genuine classic-500 image (cheapest, highest-confidence):** the swapper-start (23B) is ALREADY
  real. Doing "apply full 21B context at start" likely reaches D4 on a 144-bit image (e.g. Bo Goran's
  10509/10609 when it arrives) with minimal new code. Best ROI if a classic image is coming.
- **B. Functional swapper for THIS 5800 image (larger):** reimplement the swapper's table-building in C#.
  Needs the [TC] descriptor-field encodings RUN checks + resolving whether the monitor tables == Table A
  (disassemble 030-S3SM5, or live single-step RUN @`030624`).
- **C. ND-5800 microengine:** other (octobus/5000) LLM's scope.

---

## 11. Functional-swapper hack - 030-S3SM5 carve (2026-07-19)

Ronny's decision: no classic-500 microcode image exists, so HACK the 5800 image to run (functional swapper
in C#). Carve of 030-S3SM5 (disassembly at `tools\sintran-segment-carver\versions\L-VSX-500\re\
segments-ref\030-S3SM5\030-S3SM5.asm`):

**CRITICAL CORRECTION [V]: 030-S3SM5 is ND-100 code** (the compiled 5STDRIV driver + ND-500 system
monitor = MP-P2-N500.NPL + RP-P2-N500.NPL), **not ND-500 code**. So the segment/process descriptor
tables that RUN / LIST-ACTIVE-SEGMENTS / LIST-PROCESS-TABLE-ENTRY read are **ND-100-side** (the `S500S`
process-descriptor array + a segment table in this segment's data fields), **NOT** the ND-500-memory
Table A @ `0x08038000` (that is the swapper's paging bookkeeping). This resolves the section-10 [TC].

**Consequence - the functional swapper is NARROWER than "write the ND-500 tables":** the real swapper
runs on the ND-500 and reports results back via mailbox ANSWERS; SINTRAN's own ND-100 place code
(`SPLAC 164137` -> `STPLA` -> `ENDPL 164415`) writes `S500S` + the segment table from those answers. So
the C# functional swapper must (a) make SINTRAN believe the micro-engine is alive, and (b) ANSWER the
place-chain mailbox messages with the right data - then real SINTRAN builds its own tables. [V direction]

**Alive handshake to fake [V]:** after the micro-clock start the driver reads interface status
(`RSTA5`->`500STATUS`, MP-P2-N500 @`145571`) and proceeds only when **`5CLOST` (bit 9 = 0x0200
ND500MicroClockStopped) is CLEAR**, `5ILOCK` clear, and **`MAILINK.X5CPU = MPACTIVE`**.

**Step-1 hypothesis CORRECTED (LCON5 carve, 2026-07-19) - the earlier "clear 5CLOST on LCON5 0x24"
idea was WRONG and would have been dangerous [V]:**
- LCON5 bit 5 (0x20) = "disable TAG-IN decoding when locked", **NOT** a micro-clock start
  (ND500-BUS-INTERFACE-REFERENCE.md 4.1; `CC-P2-N500.NPL:215` `A:=40 ... DISABLE TAG-IN DECODING`).
  Bit 2 (0x04) = activate+lock. **`0x24` = activate|disable-TAG = the WRTAG wrapper written on EVERY
  TAG strobe during a control-store load** (ND500-CS-LOAD-TRACE-FINDINGS sec 2/3). Clearing 5CLOST on
  LCON5=0x24 would fire mid-CS-load-word and corrupt the CS-load gate - the exact Bug-B interference.
- **There is NO LCON5 bit that starts the micro clock [V].** The clock is started ONLY by `RETG5:=0`
  (stop-bit clear) = MON-60 25B MICRO-START (`FUNCS MPSTA @153006B`), which the emulator ALREADY models
  (`NDBusND500IF.cs:2115`). 5CLOST cannot be set/reset via LSTA5 - it reflects the real clock.
- **True meaning of the "Loading Swapper" stall (5CLOST stuck):** the swapper's control-store LOAD is
  not completing its read-back verify, so the driver never issues its own `RETG5:=0` MICRO-START (same
  class as the earlier "Loading Control Store" stall fixed by the 32-bit memory-source correction,
  CS-LOAD-TRACE sec 6a). [?] OR the 5800 swapper-load epilogue in `030-S3SM5` `LDSWA` differs [TC].
- **So Step 1 is NOT an LCON5 hack.** The real seam is upstream: make the swapper CS-load verify succeed
  so SINTRAN emits `RETG5:=0` itself, OR (functional-swapper hack) detect the swapper-CS-load completion
  and synthesize the MICRO-START effect (clear 5CLOST) + the swapper's mailbox answers. Which one is
  correct needs a LIVE single-step: instruction BP at the swapper-load verify / the `RETG5` write to see
  whether a `RETG5:=0` is ever emitted on this 5800 swapper path.

**Verified field constants (L07 N500-SYMBOLS) for the process descriptor [V]:** `S500S = 115542B`
(base); `PSTAT` disp `4` (run-status word); `5RUNS` mask `7`; `5ACTIVE = 1` (runnable); `5INMCALL=2`;
`5INCOMM=4`; `SUSPSTAT=11`; `SLICE` bit `13`; `XADPR` disp `144` (ptr to S500S descriptor); `RTDLGADDR`
disp `25` (ptr to RT register block holding P/SA); `SWPFU` disp `101`; `SWACTIVE=0`. Place-chain
routines: `SPLAC 164137`, `STPLA 164246`, `EPLAC/ENDPL 164407/164415`, `GPRTE 165771` (proc-table get,
keys on descriptor field `+40B`), segment-table routines `SSGTE 166110`/`FSEGT 166145`/`GSGTE 166352`.
`ENDPL` builds entries via BFILL/MOVEW and tests **bit 10 of segment field `-10B`** (a present/valid bit).

**Domain start [V]:** = a mailbox message with MICFU `3START`, register block via `RTDLGADDR` (P/SA lives
there); already routes to `CpuND500` (section 7/10). On 5800, MICFU-21B is MSG_ILLEG so the classic wire
is unused, but the DOMAIN start still goes through the mailbox `3START` path once placement completes.

**Still [TC] (needs a LIVE single-step, not more static carving):** exact segment-table stride/field
offsets (routines `166006`-`167651B`, un-sourced region); the exact single flag `ENDPL`/`GPRTE` set that
the J04 RUN test reads; the exact DATA each place-chain answer must carry. Recommended capture:
instruction breakpoints at `SPLAC 164137` / `ENDPL 164415` / `GPRTE 165771` + data-watch `S500S=115542B`
during a real PLACE-DOMAIN + RUN (dap-debugger), OR incrementally: fake the alive handshake (Step 1),
run the harness, and instrument the place-chain messages SINTRAN then sends to learn what answers it wants.

## 12. Trace-driven correction - the blocker advanced past CS-load to 17B DEPRG (2026-07-19)

Method: OFFLINE instruction-trace-to-file (`CpuBase.EnableTraceFile` + a new PC-range trigger gate
`SetTraceFileRange`, wired into the boot harness) + the 3022 register log (`Trace3022` ring buffer),
resolved against the SINTRAN-L symbol tables by a hex-aware Python resolver. Artifacts in the session
scratchpad (`sintran-nd100-trace-place-domain.txt` + `.routines.txt`, and the harness task output with
the 3022 log). This is the "add logging + symbols + analyse offline" path (no live single-step needed).

**CORRECTS sections 10/11's "stuck in CS-load / Loading Swapper" framing.** What the full run actually
shows:

1. **CS-load COMPLETES.** In the full run the 3022 log records `RETG5 RESTART/MICRO-START (0x00) ->
   5CLOST CLEARED` (twice). So `RETG5:=0` (MON-60 25B MICRO-START) DOES fire and 5CLOST clears on this
   5800 image - the `csStore[]` read-back model in NDBusND500IF works end-to-end. The earlier
   "OLDLO/LWRTG loop forever / MICRO-START never fires" read was an artifact of a CAPPED CPU-trace
   window (400k instrs) landing mid-CS-download; it was NOT the steady state. Section 11's open [?] "does
   RETG5:=0 ever fire on this 5800 swapper path" is now answered: YES.

2. **The blocker moved UP one level** to a repeating post-CS-load mailbox bring-up cycle (one reused
   message buffer): `N x 14B RESIWR -> 12B MSG_CACHE -> 21B 3WREG -> 17B 3DEPR -> 1B 3RMICV`, repeating.
   The servicer (built `Nd500Generation.Classic`, NDBusND500IF.cs:933) ACCEPTS every message EXCEPT
   **17B (3DEPR = DEPOSIT REGISTER)**, which has no case -> `default` -> 5ERANSWER(4)
   [Nd500MicrocodeServicer.cs:608-612]. SINTRAN gets the error and re-sends the cycle = the "NO WELL
   DEFINED PROGRAM" wall now sits at 17B, one step past the 21B blocker of catalog 7c/7d.

3. **17B carve (see ND500-MAILBOX-MESSAGE-CATALOG.md 7c/7c-bis):** `3DEPR=000017`, N5XXC dispatch slot
   17 = `DEPRG`, queue-only, twin of 16B `EXARG` (examine register); a CLASSIC-500 register-family code.
   SINTRAN only needs ANSWER(3) vs 5ERANSWER(4) - no data read-back on the deposit path.

**Fix direction (Classic-configured D4 servicer):** implement a servicer case for 17B (and its 16B twin)
answering ANSWER(3), queue-only, same contract as 21B. Whether the deposited register value must reach
CpuND500 to satisfy a LATER place-chain check is untested; the IMMEDIATE gate is only the error answer.
NEXT: add the 17B handler, re-run D4, and if the cycle advances, instrument the NEXT message SINTRAN
sends (the incremental "learn what answers it wants" loop from section 11).

### 12a. RESULT - 17B fix VERIFIED; blocker advanced to swapper execution (2026-07-19)

Implemented the 17B/16B servicer cases (Classic ANSWER(3), queue-only ACK; N5MicroFunction
DepositRegister=15 / ExamineRegister=14) and re-ran `Nd500_D4_RunDomain_RealCpu_Capture`. Servicer/
mailbox suite 94/94 green (no regression). The 3022 mailbox ring now runs the bring-up **LINEARLY -
the repeating cache/21B/17B/watchdog loop is GONE:**

```
CS-load (RETG5 MICRO-START, 5CLOST clear) -> 44 x 14B RESIWR -> 12B CACHE -> 21B 3WREG
   -> 17B 3DEPR x3   (processed=1 - ANSWERED, was the stuck point)
   -> 23B 3START x2  (processed=0 = taken by the CpuND500 host, message stays WAITING)
```

Console: `place-domain (FLOPPY-USER)LINKAGE-LOAD-H02` -> `> Loading Control Store` ->
`> Loading Swapper` -> then a **NEW** failure:

```
ERROR * 76B:41B * ... ND-500(0) Trap / Illegal instruction code / Shadow process 5SWAP
FATAL * 21B:77B * ... ND-500(0) Monitor Internal / CPU locked / "The Swapper stopped"
ILLEGAL INSTRUCTION CODE  at program address:  0   4000B
```

So the 17B DEPRG fix let the whole classic bring-up complete THROUGH `23B 3START`, and the swapper
(process 0) now genuinely RUNS on CpuND500 - and traps on an **illegal instruction at ND-500 program
address 4000B (=0x800)**. RUN still fails ("NO WELL DEFINED PROGRAM") because the swapper crashed
before building the S500S/segment tables.

**This is the functional-swapper EXECUTION blocker (task #18 / [[nd500-microcode-files]]), a distinct
harder problem than the mailbox handshake.** Ruled out: it is NOT my ACK-only 17B leaving P wrong -
`OnStartProcess` sets P from the **21B image** (`ProcessZeroRegisterImage` via
`StartProcessFromRegisterImage`, Nd500CpuProcessBridge.cs:104-109), NOT from the 17B deposits. So the
swapper started at the 21B-supplied P and executed real ND-500 memory until the illegal instruction.

**Open (needs a CpuND500 execution trace, next diagnostic):** what P did the 21B image set; is 4000B
the intended swapper entry or a run-away; what byte pattern at 4000B fails to decode; and whether the
swapper image (44x 14B RESIWR targets, live-2026-07-17 = 0x5A000-0x6F000 + 0x24800) actually covers the
P the 21B block points at. Candidate causes: (a) CpuND500 macro-decoder gap on a swapper instruction;
(b) wrong/short swapper image so P lands in unwritten memory; (c) a register the 17B deposits set (that
my ACK-only does NOT apply) is needed for correct execution even though it is not P.

### 12b. Swapper execution trace - ROOT CAUSE = MMU/mapping gap (2026-07-19)

Armed a CpuND500 instruction trace on the attached real CPU (harness `_traceNd500Swapper`,
`AttachRealCpuNow` -> `EnableTraceFile`), re-ran D4. Trace `sintran-nd500-trace-swapper.txt`
(self-limited: the CPU locks at the trap). DEFINITIVE:

- The swapper starts executing at **P = 0x04** (first traced PC; = the documented swapper PSEG entry
  PSEG+4, so P is the swapper's LOGICAL entry).
- **Every fetched instruction is bytes `55 55`** (disassembled `w2 + B.0x54`), PC stepping 0x04, 0x06,
  ... to **0x800 (=4000B)** where it traps INVALID. The register trace shows I2 cycling the classic
  memory-test values `0x55555555 -> 0xAAAAAAAA -> 0xFFFFFFFF`: the CPU is executing UNINITIALIZED
  local memory (0x55 fill), NOT the swapper image.
- **Why:** `14B RESIWR` writes the swapper image via `host.WriteNd100Word(Nd500AddressBase + dest, ...)`
  and `Nd500AddressBase => SharedMemoryStart` (NDBusND500IF.cs:2179) = the **MPM shared-memory window**
  (ND-100 byte 0x420000+). But CpuND500 fetches P=0x04 through its **MMU** (`FetchVirtualMemory`, L1/L2
  page tables, CpuND500.Fetch/Memory/Loader). With NO page-table mapping loaded for the swapper's PSEG,
  logical 0x04 -> physical 0x04 in the CPU's LOCAL `nd500mem` (`GenericMachineMemory`, 0x55-initialized)
  = garbage. So the loaded image (in the MPM window) is never reached by execution.

**ROOT CAUSE: the swapper runs with an UNCONFIGURED MMU** - its logical PSEG entry (P=0x04) is not
mapped to where the image was placed. This is the functional-swapper memory-model seam (task #18): the
real swapper start (CS-load microcode + the 21B register/segment context) would establish the PSEG/DSEG
page tables; the C# functional path does not. Rules out the decoder-gap and 17B-register hypotheses -
the CPU never reaches real swapper code at all.

**Next (still diagnostic, measure-before-fix):** confirm (1) the exact P from the 21B image and (2) the
14B RESIWR dest addresses this run (enable the servicer DEBUG_DETAIL 14B/21B logs, or add a one-line
log), and (3) whether CpuND500's MMU holds ANY page-table entries when 3START fires. Then the fix is one
of: map the swapper PSEG/DSEG into the CpuND500 MMU pointing at the 14B image location; OR place the 14B
image into the CPU's local execution memory at the physical address P resolves to; OR set P to the
image's actual (mapped) address. Which is correct depends on where the real swapper expects its PSEG
(MPM window vs local ND-500 memory) - a task-#18 architecture decision.

### 12c. MMU mapping implemented + VERIFIED - and it proves the swapper has NO macro-code (2026-07-19)

Chosen fix #2 (build the swapper PSEG/DSEG mapping) IMPLEMENTED:
- `CpuND500.MapExistingPhysicalRegion(domain, seg, isProgram, physByteBase, byteLen, writable)`
  (Loader.cs) - builds a PS_ASI page table pointing at ALREADY-POPULATED physical frames (no copy),
  installs PST + domain capability. The referenced frames route to the MPM window automatically.
- `Nd500CpuProcessBridge.InstallSwapperMapping()` - on the first 23B 3START, derives PSEG (largest
  contiguous 14B run) + DSEG (lowest dest up to PSEG) from the servicer's new `ResiwrLog`, maps
  program seg-0 -> PSEG and data seg-0 -> DSEG, and enables the program+data MMU.

Exact placement captured (D4 harness `DumpSwapperMapping`): 21B image reg[0]=P=**0x04**, CAD=CED=**0**;
44x 14B RESIWR placed **DSEG @0x24800 (contiguous) then PSEG @0x5A000-0x6F7FF (43 pages)** in the MPM
window. With the mapping installed the swapper's illegal-instruction trap MOVED from 0x800 (unmapped
0x55 garbage) to **0x04** (the real logical entry, phys 0x5A004) - i.e. the mapping is correct and the
CPU now fetches the placed image.

**BUT the placed image is DATA-ONLY - it contains NO executable macro-code [V].** A byte scan of all
44 placed pages: **40 pages are entirely zero; only 4 have content, and it is TABLES not code:**
- `0x6E000`: `49 00 4A 00 4B 00 ...` = sequential PFNs (0x49 = 0x24800>>11 = the DSEG base frame) - a
  page/segment table SINTRAN built for the swapper.
- `0x6E800`: `DF 00 E0 00 ...` = higher PFNs (0xDF = 0x6F800>>11 = the PSEG top).
- `0x6F000` (`02 C0 ...`) + `0x24800`+0xFB (`03 ...`) = small descriptors.

The swapper entry (logical 0x04 -> phys 0x5A004) lands in a ZERO page: opcode 0x00 = illegal -> trap.
So the 44 RESIWR transfers placed only the swapper's PAGE TABLES + zero data pages - the swapper's
executable code is NOT in that particular set of transfers.

> **RETRACTION (2026-07-19, Ronny's correction).** An earlier revision of this section concluded from
> the above that "the swapper's instructions are control-store microcode, so there is no macro-code
> swapper to run". **That conclusion was WRONG and is withdrawn.** The correct model:
>
> - **Microcode** is what "> Loading Control Store" downloads into the CPU's **control storage** (the
>   internal 128-bit microengine store). That is the CPU's own instruction-decoding firmware.
> - **ND-500 code - the swapper, and every domain - is NOT microcode.** It is ordinary ND-500
>   executable code living in an ordinary **executable memory area / segment**, which the functional
>   `CpuND500` macro interpreter executes directly out of memory.
>
> The swapper therefore **runs in its own segment**, and the fact that one set of 14B transfers held
> only tables says nothing about microcode - it only says the swapper's code segment was placed
> **somewhere else** and still has to be located. Direct evidence that the interpreter can run a real
> swapper: injecting the K-rev `SWAPPER-K01.PSEG` at the derived PSEG base and setting P to its entry
> made `CpuND500` **execute real instructions** (PC 0x08000004 -> 0x08000021) before diverging on a
> K-vs-L data-layout mismatch. A "microcode wall" would have made that impossible.
>
> Follow-up in progress: a broad dense-region scan of the whole 8 MB MPM window to find where the L
> system's swapper code segment actually sits (see section 12d).

**CONSEQUENCE for D4:** the remaining work is to LOCATE the swapper's executable segment in the placed
image, map that segment, and set P to its real entry (the `MapExistingPhysicalRegion` /
`InstallSwapperMapping` machinery is already in place and byte-verified to map correctly). The C#
functional-swapper fallback (emulate the swapper's EFFECT: build the descriptor tables the place-chain
needs + answer the 3START mailbox stop) remains the backstop if the code segment cannot be located.

**Scope reality:** this is a multi-step iterative build (fake alive -> observe place chain -> answer with
right data -> RUN), not a one-shot. Each iteration is gated on a ~2-min flaky boot-harness run.

### 12d. SWAPPER CODE LOCATED - SINTRAN places the real executable itself [V] (2026-07-19)

A broad dense-region scan of the whole 8 MB MPM window (harness `DumpSwapperMapping`, swapper injection
DISABLED so the placed memory is pristine) found the swapper's executable segment:

- **Physical `0x06F800`, 19 dense pages (~35,180 non-zero of 38,912 bytes).** Its first 64 bytes are
  **byte-for-byte identical** to `SINTRAN\ND500\swapper\SWAPPER-K01.PSEG` (38,161 bytes on disk),
  including the embedded `REV` and `-K01` build strings:
  `00 00 00 00 DC 08 02 42 54 CF 00 00 00 24 CE 1F / 44 1A 1A 45 4A 47 2E C4 08 01 28 18 CF 52 45 56 / ...`
- Injection was OFF for this run, so **SINTRAN itself placed this code** - it arrives by the separate
  "> Loading Swapper" path, NOT by any 14B RESIWR (whose destinations stop at `0x6F7FF`).

**This settles the retraction in 12c: the swapper IS ordinary ND-500 executable code, in its own
segment, sitting in memory, ready to run.** There was never a microcode wall - only a wrong base.

**Why the earlier mapping missed it - and the correct rule.** `DeriveSwapperRegions` had guessed the
PSEG as the largest contiguous 14B RESIWR run (`0x5A000-0x6F7FF`), which is 40/44 pages ZERO - that is
the DSEG plus tables. But SINTRAN hands over the answer explicitly, in the two page tables it DMAs:

| RESIWR dest | first words (big-endian) | meaning |
|---|---|---|
| `0x6E000` | `0049 004A 004B 004C ...` | **DATA page table** - PFN `0x49 << 11` = `0x24800` = DSEG base |
| `0x6E800` | `00DF 00E0 00E1 00E2 ...` | **PROGRAM page table** - PFN `0xDF << 11` = `0x06F800` = PSEG base |
| `0x6F000` | `.. 02 C0 ..` at +0x3 | segment descriptor referencing the tables |

Both tables are strictly ascending consecutive PFNs, and the program table's run length (`0xDF..0xF1`
= 19 pages) matches the dense region exactly. The 21B image's `P = 0x04` then means **PSEG+4**, the same
entry offset the K-rev swapper uses.

### 12e. THE SWAPPER NOW EXECUTES - three mapping bugs found and fixed [V] (2026-07-19)

With the code segment located, the swapper genuinely runs on the functional `CpuND500`, and each
remaining stop was a real, separately-diagnosed emulator defect - not a wall. Instrumentation added
first (`CpuND500.LastProtectionViolation`, `CpuND500.LastPageFault`, `Nd500CpuProcessBridge.
LastSwapperMapReport`) so each trap names the exact MMU check that rejected it, because
"PROTECT VIOLATION" alone has four possible causes and "PAGE FAULT" five.

Verified mapping actually installed (harness print):
`mapped dom=0 progSeg=0 dataSeg=1 P=0x00000004 PSEG=0x0006F800+0x9800 DSEG=0x00024800+0x35800`
- PSEG length `0x9800` = 19 pages = exactly the located swapper code.
- DSEG `0x24800` + `0x35800` = 107 pages, from SINTRAN's data page table.

| # | Symptom (SINTRAN console) | Diagnosed cause | Fix |
|---|---|---|---|
| 1 | illegal instruction at entry | PSEG base derived from RESIWR extents = zero pages | derive from SINTRAN's page tables (12d) |
| 2 | `PROTECT VIOLATION / DATA segment` at program address `21B` | data capability installed in segment 0, but the swapper's data is in **segment 1** (fault VA `0x08024255`, `>> 27 = 1`, `cap=0x0000`) | map DSEG into segment 1; program stays in segment 0 |
| 3 | same trap, `reason=(unspecified)` -> page-level check | `MapExistingPhysicalRegion` wrote `pte = (pfn << 2) \| 0x1`, treating bit 0 as a PRESENT bit. Per `ReadPageTableEntry`/`WritePageTableEntry` bit 0 is **protection** (`PG_W`=0 writable, `PG_R`=1 read-only) and validity is `PFN != 0` - so EVERY mapped page, including the writable DSEG, was read-only | `pte = (pfn << 2) \| (writable ? PG_W : PG_R)` |

Bug 3 is a genuine `CpuND500` loader defect, independent of the D4 work.

**Progress achieved:** the swapper's PC advanced `0x04` -> `0x11` -> **`0x52`** (SINTRAN: program
address `130B`) as each fix landed. The protect violations are gone.

**Current stop:** `PAGE FAULT at program address 0 130B / Logical address 1 100645B / DATA segment
READ access / Contents of Physical segment Table = 0`. Octal `100645` = `0x81A5`, which lies INSIDE
the mapped 107-page DSEG (L2 page index 16), so the reason is not yet established - the page-fault
reason capture was added for exactly this and the next run will name the failing walk step. NOT
guessed here.

Also fixed en route (latent, kept): `MapExistingPhysicalRegion` built capabilities without `DC_PAC`,
which `TranslateVirtualAddress` STEP 5 rejects for any non-privileged PCB. Marked in-code as a
PERMISSIVE emulator-side assumption - SINTRAN never sends its real capability word over the mailbox
(it only DMAs page tables), so the genuine PAC/privilege bits are not observable.

### 12f. The swapper runs deep - and hits the MMU's memory-resident-tables gap (2026-07-19)

Continuing the trap-by-trap bring-up after 12e, each fix moved the swapper further:

| Swapper PC reached | Trap that stopped it (reason from the new instrumentation) | Hand-fix applied |
|---|---|---|
| `0x11` | write VA `0x08024255`, `cap=0x0000` - no data capability in **segment 1** | map DSEG into segment 1 |
| `0x52` / `0x58` | PAGE FAULT VA `0x080081A5`, `cap=0x0000`, **`isInstruction=True`** - no PROGRAM capability in segment 1 (offset `0x81A5` is inside the same `0x9800` PSEG) | map PSEG into segment 1 as program too |
| `0x080082EE` .. `0x82FF` (SINTRAN: program address `1 101377B`) | write VA `0x00000002`, `cap=0x0000` - no data capability in **segment 0** | NOT applied - see below |

So the swapper now executes real code across both logical segments. But the pattern is the tell: the
emulator needs a NEW hand-built capability every few instructions.

**ROOT CAUSE of that pattern [V, by code inspection]: `CpuND500`'s MMU does not use memory-resident
MMU tables.** `InitializeMMU` (`CpuND500.MMU.cs:293`) allocates `PST` and `PCBTable` as **C# arrays**,
and `TranslateVirtualAddress` indexes those arrays directly. The `PSTP` register (Physical Segment
Table Pointer, `Registers.cs:805`) exists but takes no part in translation. On real ND-500 hardware
the MMU walks a PCB and a PST that live IN MEMORY, which SINTRAN and the swapper populate themselves.

Consequence: every capability/PST entry SINTRAN or the swapper writes into ND-500 memory is INVISIBLE
to the emulated MMU, so translation only ever sees what C# loader calls installed. That is why the
bring-up needs a manual `MapExistingPhysicalRegion` per newly-touched segment.

Honest scope note: the gap itself is verified. Whether THIS particular swapper builds its own PCB/PST
entries in memory is **NOT yet verified** - it is the natural explanation for the trap cascade, but it
has not been proven by disassembling the swapper's writes. Do not treat it as established.

**Decision point (architecture, not a bug fix):** either teach `CpuND500`'s MMU to walk memory-resident
PCB/PST (correct, larger, touches the CPU core the architect owns), or keep hand-mapping capabilities
trap-by-trap (fast, but a growing pile of assumptions with no ground truth behind them).
**Ronny chose the memory-resident walk.** What that needs, and what is missing, is in 12g.

### 12g. The PST base cannot be anchored yet - NEGATIVE result [V] (2026-07-19)

The memory-resident walk needs a PST base. Findings:

- **SINTRAN DOES send `PS`:** 21B image `reg[18] = 0x48480003`, and register 18 = `regs.PS`
  (`CpuND500.ProcessControl.cs:152`). Per the spec that is the anchor: `PST[PS]` -> PCB table address.
- **SINTRAN sends NO `PSTP`.** The whole 0-based register block is
  `0=P 1=L 2=B 3=R 4..7=I1..I4 8..11=A1..A4 12..15=E1..E4 16=ST1 17=ST2 18=PS 19=TOS 20=LL 21=HL
  22=THA 23=CED 24=CAD 25..28=mic1..4 29..36=OTE/CTE/MTE/TEMM` - there is no PST-base entry at all.
  In the emulator `PSTP` is only ever written by `MMUConfiguration.cs:568`; no instruction sets it.
- **`PS`'s value is ambiguous:** the image's halfword order is `INFERRED [D]` in the code comment,
  so `reg[18]` is either `0x48480003` or `0x00034848`. NOT resolved.
- **PST hunt in the MPM window: 0 candidates [V].** Scanned all 8 MB for a 32-bit PSTE whose PFN
  names either page table SINTRAN built (`0x6E800` -> PFN `0xDD`, `0x6E000` -> PFN `0xDC`, PSTE =
  `(PFN << 2) | mode`). Nothing matched.

**Interpretation (HYPOTHESIS, not established):** the most likely reason nothing matched is that on
real hardware **the SWAPPER builds the PST/PCB**, and ours stops at `PC=0x080082FF` long before doing
so - so there is nothing to find yet. Alternatives not excluded: the PST lives in ND-500 LOCAL memory
(the scan covered only the MPM window), or the in-memory PSTE encoding differs from the spec's
`(PFN << 2) | mode`.

**Consequence:** hand-mapping and the memory-resident walk are NOT competing options - hand-mapping is
what gets the swapper far enough for the walk to have real tables to read. Continue trap-by-trap,
watching for the swapper writing PST/PCB-shaped data; implement the walk once that is observed.

### 12h. Hand-mapping is UNBOUNDED - the swapper needs a memory-resident PCB [V] (2026-07-19)

Two further probes settled the approach:

**Probe 1 - identity window over physical 0: IMPOSSIBLE BY CONSTRUCTION [V].** Mapping data segment 0
as an identity window based at physical 0 page-faulted immediately:
`PS_ASI PTE not present: L2=0 PTE@0x00097800 PFN=0 (segment 0, cap=0xC010, psn=16)`.
Reason, from the spec: an ND-500 PTE has **no present bit** - bit 0 = protection (`PG_W`/`PG_R`),
bit 1 unused, bits 31-2 = PFN (`ND500_MMU_SPECIFICATION.md:505-526`); a valid bit exists ONLY in the
*software* shadow PTE, explicitly "NOT used by hardware" (:537-542); and the spec's own reference walk
tests `if (pte.pg_pfnum == 0) page_fault()` (:643-645). So "PFN 0 = not present" is the real
convention, the emulator is CORRECT, and **physical page 0 can never be mapped**. Hypothesis dropped.

**Probe 2 - both data segments through SINTRAN's one data table: WORKED, then exposed the real
problem.** The swapper cleared segment 0 and ran on to `PC=0x08000753` (SINTRAN: program address
`1 3523B`), where it writes `VA 0x68000044` = **segment 13**, `cap=0x0000`.

**Conclusion [V]:** the segments the swapper touches (program 0, 1; data 0, 1, 13, ...) are open-ended
and have NO basis in the two page tables SINTRAN supplies. Hand-mapping cannot converge. On real
hardware all 32 program + 32 data capabilities come from the **memory-resident PCB**, which the
emulated MMU never reads (12f). And SINTRAN evidently does not build a PST/PCB in ND-500 memory during
PLACE-DOMAIN - the MPM PST hunt found 0 candidates (12g), and in this emulator MPM is the only memory
SINTRAN can write (physical addresses below the window size route to MPM).

**So the open question is: WHO builds the PCB/PST, and WHEN?** Candidates, none verified: the swapper
itself once it runs far enough; the microcode at process start (e.g. from the process CONTEXT BLOCK -
see `CNTXT-BLOCK-DECODE-2026-07-17.md` and the `LCNTXT` instruction); or the control store keeps them
internally. **This is the exact question for the ND-5000 / microcode track** - if the microcode
contains the MMU table-walk and the process-start capability load, it answers it outright.

**Current run (deliberate HACK, labelled in code):** point ALL 32 program and 32 data segments at the
two tables we do have, and let the swapper run as far as it can. This is permissive and wrong as an
architecture (it can never fault where the real machine would), but it is an evidence-gathering run:
if the swapper completes and builds its own PST/PCB, those real tables can then be read and the proper
memory-resident walk implemented against them.

### 12i. MILESTONE - the swapper RUNS CLEAN and TALKS to SINTRAN [V] (2026-07-19)

With capabilities available for every segment, the behaviour changed qualitatively:

| Before | After (blanket mapping) |
|---|---|
| `stopMode=CRASHED` | **`stopMode=WAIT`** - parked on a monitor call, the normal swapper state |
| protect violation / page fault every few instructions | **`LastProtectionViolation: (none)`, `LastPageFault: (none)`** - ZERO MMU traps |
| "The Swapper stopped" (died) | **"Fatal error from Swapper / ERROR CODE: 200B"** - the swapper REPORTED status through the monitor protocol |

So the real ND-500 swapper executes its whole startup path on the functional `CpuND500`, needs no
faking, and communicates with SINTRAN. Final PC `0x08000687`, parked at WAIT.

**Error code 200B = HARDWARE FAULT [likely, not proven].** Cited from existing carve work:
`ND500-STATUS-AND-INDEX.md:107` ("mp hwfault(200B)/trap(201B)") and
`ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md:983` ("Error code (hwfault=200b)"). CAVEAT: both
citations are the ND-5000/ACCP code space; that the SWAPPER uses the same namespace is NOT verified.

**Why a hardware fault is the EXPECTED outcome here:** the blanket mapping deliberately gives every
one of the 64 capabilities the same two page tables, so any consistency check the swapper makes on its
segments sees wrong data. The swapper is correctly reporting that its environment is broken. Fixing
the fault means giving it REAL capabilities - i.e. the memory-resident PCB/PST question in 12g/12h,
which is exactly the open ask for the microcode track.

**Do NOT ship the blanket mapping.** It is an evidence probe only; it can never fault where real
hardware would. (Superseded 2026-07-20 by the carve-derived segment-1 mapping, 12j.)

### 12j. CARVE RESOLVES THE MAPPING - and finds the real blocker: the DSEG is never loaded (2026-07-20)

**The swapper is linked at SEGMENT 1 [CARVE, decisive].** `swapper/swapper-k01-pseg.asm` disassembles
at base `0x08000000` and its own operands are segment-1 addresses, matching our live traps exactly:

| Carved instruction | Operand | Our live trap |
|---|---|---|
| `1000000004: init $1000441124,$44,$17504` | `0x08024254` = stack bottom (deep-analysis 5.1: DSEG `0x24254..0x26197`, 8004 bytes) | write `0x08024255` at program address `0o21` |
| `1000000122: call $1000100645` | `0x080081A5` | page fault `0x080081A5`, `isInstruction=True` |
| `1000000026: comp2 $1000224030,...` | `0x08012818` vs ASCII `REV.` / `-K01` | build-tag self-check; `MON 0B LEAVE` on mismatch |

So code and data share logical segment 1, separated by the I/D split (program capability -> PSEG,
data capability -> DSEG). The blanket 32-segment hack is REMOVED; only the segment NUMBER comes from
the carve, both regions still come from SINTRAN's own page tables.

**Still OPEN:** SINTRAN's 21B image sends `P = 0x00000004` - offset only, NO segment bits (neither
halfword order yields `0x08000004`). Something supplies segment 1 and we do not model it. The
emulator currently takes it from the carve.

**THE REAL BLOCKER, found by carving the faulting instruction [V]:**
With the correct mapping the swapper runs to `PC=0x080082EE` and faults writing `VA 0x00000002`.
That address is not arbitrary - the carve shows the instruction is the swapper's DMA intake:

```
1000101356: h riom $1000440264,$1000440274,$1000440074+   ; = 0x240B4, 0x240BC, 0x2408C
```

which is exactly the RIOM triple the deep analysis documents (`h riom [0x240B4] -> buffer [0x240BC]`,
count from the 29-entry table at `0x2408C`). `RIOM` is fully implemented in the emulator
(`Instructions/IO/Riom.cs`). It writes to ~0 because it reads **zero** out of those descriptor cells.

**Why they are zero: the swapper has NOT been handed a valid swapper MESSAGE. [V]**

**POISONED PRIOR, now DISPROVEN - "the swapper's DSEG is never loaded".** An earlier revision of this
section claimed that, from a dense-region scan showing nothing at `0x24800`. **Both the reasoning and
the conclusion were wrong:**
- The dense scan cannot decide it - it flags a page only at >= 25% non-zero sampled bytes, and a
  218 KB data segment that is mostly zeros with a few tables sits below that threshold.
- A content-signature probe settles it. **The DSEG IS loaded, exactly at `0x24800`:**
  `DSEG+0x2408C @phys 0x04888C` = `00 00 00 08 00 00 00 0B 00 00 00 08` (the 29-entry count table)
  and `DSEG+0x26198 @phys 0x04A998` = `08 00 83 D8 08 00 83 F7 08 00 84 74` (the function table),
  both byte-identical to `swapper/SWAPPER-K01.DSEG`; a window-wide sweep for the fn-table signature
  hits once, at `0x04A998`, implying DSEG base `0x024800` - i.e. precisely what SINTRAN's DATA page
  table said. **So placement AND mapping are both correct for code and data.**

**The actual reason the RIOM descriptors are zero: they are RUNTIME variables [V].**
`SWAPPER-K01.DSEG +0x240B4` (RIOM ND-100 source) and `+0x240BC` (ND-500 buffer) are all-zero **on
disk too** - the swapper fills them from its SWMSG before issuing RIOM. Zero descriptors therefore
never evidenced a loading problem; they mean the swapper reached its intake without having been given
a valid message.

**So the open thread is the MESSAGE path, not the loader.** Per `5SWRT` (`RP-P2-N500.NPL:12-58`)
SINTRAN computes `A:=SWMSG+"SWPINFO"=:D:=5MBBANK; AD=:DSWMSG` - the PHYSICAL address of `SWPINFO`
inside `SWMSG` - which is what the swapper's RIOM must use as its ND-100 source.

### 12k. SINTRAN's answer is CORRECT; the bug is in the emulator's RIOM operand handling [V] (2026-07-20)

**SINTRAN delivers the pointer properly.** Instrumenting the swapper's monitor-call exchange:

```
MON 377B argc=4 ret=0x08008255
  [0] @0x08012A28=0x00000001  [1] @0x080240B0=0x00000000
  [2] @0x080240B4=0x00000000  [3] @0x0802428C=0x00000000
  RESTART write-back: @0x080240B0:=0x00000005  @0x080240B4:=0x00210718
```

`0x00210718` is an ND-100 **word** address: `x2 = 0x420E30`, exactly the buffer named in the 3022
`MSGHDR link` field. So the MON 377B round trip, the write-back path and SINTRAN's answer are all
working - the swapper gets its source pointer.

**The RIOM decode then shows the emulator mishandling it [V]:**

```
RIOM @PC=0x080082FF: src=0x00000021 dest=0x00000000 count=15
 | op0 mode=ABSOLUTE    ea=0x080240B4
 | op1 mode=ABSOLUTE    ea=0x080240BC
 | op2 mode=ABSOLUTE_PI ea=0x08024046
```

1. **Source truncated:** `op0` names the cell holding `0x00210718`, but `Riom.cs` reads it with
   `DataType.H` -> `0x0021`. It must be read as a WORD. The subsequent `if (nd100SourceAddr > 0xFFFF)`
   check would also reject the true value, and the whole ND-100 bridge
   (`ReadND100Word`/`MapND100ToPhysical`) is `ushort`-wide, so it cannot carry a 22-bit word address
   at all.
2. **Destination dereferenced instead of addressed:** `op1`'s CONTENTS are zero, so `dest=0` and the
   transfer walks into ND-500 address 0 - the observed `write ... at 0x00000002`. Its effective
   address `0x080240BC` is the real buffer, which the carve corroborates: the swapper NEVER stores to
   `0x240BC` and LOADS the message out of it on the instruction right after RIOM.

**Why the existing RIOM unit tests do not contradict this:** `Emulated.Tests.ND500\Instructions\
TestND500_Instructions_IO.cs` encodes the buffer as `$<addr>` = CONSTANT mode, where "read the value"
happens to give the address. But `Riom.cs`'s own operand spec lists Operand 2 (Buffer, **Write**) as
"LOCAL, RECORD, REGISTER, PRE_INDEXED, ABSOLUTE" - **CONSTANT is not a legal mode for it**. The tests
therefore encode an addressing mode the hardware does not allow for that operand; the live swapper
uses ABSOLUTE, where value and address differ and only the address is correct.

**Status:** diagnosis complete and evidence-backed; the fix touches shared CPU code plus three
existing tests, so it is called out here rather than slipped in silently.

### 12l-pre. "Control Store" and "Swapper" are ONE state machine, not two branches [V, octobus track]

Correcting a framing used earlier in this document and in the boot-harness notes: the two console
strings are NOT alternative paths selected by CPU generation. Per the octobus/ACCP track's carve
(`HANDOFF-FROM-OCTOBUS-TRACK-TO-MICROCODE-TRACK-2026-07-20.md` section 1):

- `> Loading Control Store` and `> Loading Swapper` are **steps 0 and 3 of the single `500IN` init
  state machine** (`075150`), gated by independent bits of one done-mask (complete = `0o217`). Bit 0
  is tested before bit 3 - which is exactly why this track observed both printed in that order.
- **`LDSWA` (`143551`-`143621`) contains no CPU-type test.** Its only descriptor test is
  `143564 BSKP ZRO 30 DA` = the "swapper already loaded" done-bit.
- The real generation discriminator in that segment is
  `(CPUAVAILABLE & 000007) == 3 /* SAMSON */`, used 20+ times, but **not** in the swapper-load or
  control-store-load steps, which run unconditionally.

So the earlier reading - "this image starts the swapper via the control-store path INSTEAD OF the
classic 21B mailbox" - is wrong as stated. Both steps run; the open question is not which path was
chosen but what each step actually transfers.

### 12l. RIOM FIXED - swapper advances to 0x913B; message source is EMPTY (2026-07-20)

**Fix applied** (Ronny approved changing shared CPU code + the tests):
- `Riom.cs`: source operand read as **W** (was `H`, truncating `0x00210718` -> `0x0021`);
  destination = **operand 1's EFFECTIVE ADDRESS** (was its contents, which are 0); source validated
  against the **24-bit** ND-100 bus and the transfer loop no longer wraps at `0xFFFF`.
- `CpuND500.ND100Bridge.cs`: added `uint` overloads of `MapND100ToPhysical` / `ReadND100Word` /
  `WriteND100Word` (the `ushort` ones forward, so existing callers are untouched) - the old path
  could not express a 22-bit word address at all.
- `Nd500CpuProcessBridge` ctor: `SetND100PrivateOffset(0)`, because 3022 RIOM sources are ABSOLUTE
  ND-100 physical word addresses; the 0x40000 default would have relocated every transfer.
- `TestND500_Instructions_IO.cs`: buffer operand re-encoded as ABSOLUTE (bare number) instead of
  CONSTANT (`$`), which the operand spec does not permit for a write operand; and
  `Test_RIOM_AddressWraparound` -> `Test_RIOM_HighPhysicalSourceAddress`, since asserting a wrap at
  `0xFFFF` encoded the old implementation rather than the hardware. **6/6 IO tests green.**

**Result [V]:** RIOM now decodes correctly -
`RIOM @PC=0x080082FF: src=0x00210718 dest=0x080240BC count=15` - and the swapper runs on from
`0x82FF` to **`PC=0x0800913B`** (SINTRAN: program address `1 110473B`), i.e. it accepted the intake
and moved into message processing.

**New stop, and an honest caveat.** The swapper now faults READING `VA 0x0000000A` (data segment 0,
`cap=0x0000`). The carve shows the neighbourhood is a string move whose address is computed as
`w4 laddr r2.(10)` - i.e. `r2 = 0`, so it is dereferencing a null pointer taken from the message.
A post-run dump shows **both** the delivered buffer (`DSEG+0x240BC`) and the ND-100 source
(`0x420E30`) are ALL ZERO, so RIOM faithfully copied an empty message.

- **NOT established:** that the source was empty AT TRANSFER TIME - the dump runs after RUN, so the
  buffer could have been consumed or cleared in between.
- **NOT established:** that the `_private = 0` change mattered. The observed fault is byte-identical
  before and after, because the source reads zero either way.
- **[OPEN] - and the arithmetic behind `_private = 0` is WEAKER than it looked.** The justification
  was `0x210718 * 2 = 0x420E30`, the buffer named in the 3022 `MSGHDR link`. But those two numbers
  live in DIFFERENT ADDRESS SPACES: `ADRZERO` is documented as *the ND-100's view* of the shared
  window ("tells the ND-100 at what address it will see the MPM5 memory"), so an ND-100 physical
  address landing inside 5MPM says nothing about where - or whether - that page is mapped in the
  swapper's ND-500 logical space. The numeric agreement is real but may be coincidence across spaces.
  Worse, the one document asserting the swapper's message buffer is in PRIVATE ND-100 memory derives
  that from the RIOM manual text itself, so it is circular. **Nothing in the tree establishes whether
  that buffer is private or shared.** Treat `_private = 0` as an unproven emulator convention, not a
  derived fact.

**Next question:** why is `SWPINFO` empty when the swapper asks for work? Plausible and unproven: the
swapper is meant to find no work and park, and the null-pointer dereference is what a real swapper
would never reach because it would have taken an earlier "no work" branch on a field we are not
supplying. `0x420E30` is a known 128-word (200B) message buffer (see
`CARVE-ANSWER-3022-FLAG-POLL-RING-2026-07-19.md`), so the next probe is to capture it at the moment
of the RIOM rather than at end of run.

**NEXT:** find out how the DSEG content is supposed to arrive. The PSEG arrives on the
"> Loading Swapper" path with no RESIWR naming it; the DSEG presumably arrives the same way and our
emulation delivers only the PSEG half. That is now the top D4 question - ahead of the PCB/PST
question, because a swapper with no data cannot succeed however correct the MMU is.

**Harness note:** this boot harness is FLAKY - identical configurations both passed and died with
"Test host process crashed" and an empty progress log. Take at least two samples before believing any
single run; an early attempt wrongly blamed a code change for what was harness noise.

**Also observed [V]:** SINTRAN's trap console display disagrees with what actually happened. For the
`0x080081A5` fault SINTRAN printed "DATA segment READ access ... Logical address 1 100645B", while the
CPU's own record shows an INSTRUCTION fetch (`isInstruction=True`). Likewise the earlier segment-1
write was displayed as "Logical address 0 0B". So the trap fields we hand SINTRAN (access type and/or
failing address) are wrong somewhere in the trap-report path. Open item; not chased yet.

**Implemented (RetroCore `Nd500CpuProcessBridge.DeriveSwapperRegions`):** find the ascending-PFN tables
among the RESIWR pages (>= 3 consecutive PFNs), then classify - the table whose target IS inside a
RESIWR destination is the DSEG; the one whose target NO RESIWR ever wrote is the PSEG (because the code
comes by the other path). Map both with `MapExistingPhysicalRegion` (fetch-only program cap, writable
data cap) at segment `P >> 27`, domain `CAD`. The swapper-injection path, the `AnnounceSwapperAlive`
fake and the 3MONCO "parked but alive" intercept were all REMOVED - the real code runs.
