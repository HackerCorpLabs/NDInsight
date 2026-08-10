# Plan: log and root-cause the NLL install, and why the new domain does not work

**Date**: 2026-07-30
**Subject**: ND-500 LINKAGE-LOADER, product 210319, installed onto the SINTRAN III VSX/500 L pack
via Route B. The install stalls; this is the plan to find out exactly why.
**Procedure being executed**: [ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md](../../Installation/ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md) section 5.2.

---

## CORRECTIONS 2026-07-30, later the same day - READ FIRST

Three things below are wrong. They are left in place rather than deleted so the reasoning error
is visible, but **do not act on sections 0-3 without reading this**.

**C1. The transcript in section 0 is from 28 July, not from the run I attributed it to.** I read
`sintran-octobus-capture-nll-install.txt` without checking its timestamp; its own header says
`28 JULY 1998` while that day's runs print `30 JULY 1998`. **There is no evidence
`RECOVER-DOMAIN` ran at all on 30 July.** Same mistake shape as the 244B field-versus-event error
earlier the same day: trusting a filename instead of confirming the artefact was freshly written.

**C2. H1's "console versus run thread" fork is FALSIFIED.** With the run thread ON, the console
works: login, `ENTER-DIRECTORY` and `LIST-DIRECTORIES-ENTERED` all succeeded and the floppy
mounted as `DIR INDEX 40`. The 2026-07-28 harness comment claiming the run thread kills the
console is **stale**. Section 3 of this plan therefore chases a bug that no longer exists - skip
it.

**C3. The real cause of "no ND-500" in an INTERACTIVE session is now known and fixed.**
`device add ND5000 0` creates the octobus **card only**. Attaching a CPU is a separate explicit
step (`AttachNd5000Cpu`), and before 2026-07-30 that was called **only from the test harness** -
no interactive host called it at all. So a CLI session always answered:

```
ND-500(0) error:      No ND-500(0) CPU found
```

Verified live. A `Nd5000 attach` CLI command was added to close this gap. **This does not explain
the harness stall** (the harness always attached a CPU); it explains why the interactive machine
could never reproduce the harness in the first place.

**What remains genuinely open**: with the floppy attached AND the run thread on, `@nd-500` did
not answer and the test host process died. "Died" is vstest's report
(`The active test run was aborted. Reason: Test host process crashed`) - **there is no exception,
stack or dump yet**, so treat it as an unexplained process exit, not a diagnosed crash. Re-run
with `--blame-crash` to get a real stack before theorising.

---

## 0. The observation, stated exactly

Route B ran as far as this and stopped:

```
@enter-dir,,f-d-1,0
@list-directories-entered,,terminal
DIR INDEX  0 : DISC-75MB-1   UNIT 0 : PACK-ONE
DIR INDEX 40 : FLOPPY-DISC-1 UNIT 0 : 210319H02-XX-01D
@nd-500

ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17
ND-5000 timeout:      ACCP was terminated; Microprogram has stopped

ND-5000: recover-domain (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02

> Loading Control Store
> Loading Swapper
```

Nothing after `> Loading Swapper`. `COPY-DOMAIN`, `LIST-DOMAIN`, the local-copy run and the
verifying `LIST-FILES` never executed. Transcript is 33 lines total:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-capture-nll-install.txt`
(outside the repo, so an absolute path is unavoidable here).

**What is confirmed working** and must not be re-investigated:

- The floppy mounts and is readable by SINTRAN (`DIR INDEX 40`, correct volume label).
- The ND-500/5000 monitor starts and accepts commands.
- `RECOVER-DOMAIN` parses, is accepted, and begins real work.
- The media is intact: 8 files under `FLOPPY-USER` including a complete `LINKAGE-LOAD-H02`
  domain, re-verified with ndtool after the rename.

**What is NOT yet known**: everything after `Loading Swapper`.

---

## 1. The leading hypothesis, and why it is not "the domain is broken"

**H1: the ND-500 CPU is not executing at all in this run, by configuration.**

The install test requires `RETROCORE_ND5000_RUNTHREAD=0`. That flag maps to
`AttachNd5000Cpu(startRunThread: false)`, documented in `ND100Machine.ND5000.cs` as *"leave it for
deterministic single-thread test driving"*. The ND-5000 runs on its **own host thread**; the
ND-100 machine's clock loop does **not** tick it. So with the flag set there is no thread to
execute ND-500 instructions.

`RECOVER-DOMAIN` is not a file copy. NLL is itself an **ND-500 domain**, so recovering it means
placing and RUNNING ND-500 code. `> Loading Control Store` / `> Loading Swapper` is the ND-100
side preparing the ND-500 and then waiting for it. If nothing executes, that wait never ends.

**If H1 is right, the domain is not broken and neither is the install procedure.** The stall is a
harness configuration artefact.

**And the trap that makes this the whole problem**: the flag is not optional. The harness comment
at `Nd100SintranNd5000OctobusBootHarnessTests.cs:227-231` records that with the run thread ON,
*"every octobus fullflow run stalls at the FIRST ESC -> ENTER, so the interactive terminal path is
dead while the boot itself is healthy"* - i.e. nothing can be typed at all.

So the two conditions the install needs are currently **mutually exclusive**:

| | Console usable | ND-500 executes |
|---|---|---|
| `RUNTHREAD=0` | YES | **NO** |
| `RUNTHREAD=1` (default) | **NO** | YES |

**That fork is the actual blocker.** It is a harness/threading defect, not a SINTRAN one, and no
amount of domain-level logging will move the install until it is fixed.

### Competing hypotheses, kept alive deliberately

- **H2: the swapper load itself fails or never completes**, independently of H1. The earlier
  fullflow ladder reports `start-swapper=OK`, but that is a different code path from a
  monitor-initiated load during `RECOVER-DOMAIN`.
- **H3: the ACCP timeout banner is real and load-bearing.** The monitor prints
  `ND-5000 timeout: ACCP was terminated; Microprogram has stopped` **on entry, before any
  command**. If the microprogram really is stopped at that point, `RECOVER-DOMAIN` may be waiting
  on a CPU the monitor already considers dead. See section 5 - this also bears on a claim already
  sent to the ACCP team.
- **H4: the domain genuinely does not work** (bad description file, segment access, quota). This
  is the user's stated question and it stays on the list, but **it cannot be tested until the run
  reaches `COPY-DOMAIN`**, which it currently never does.

Order matters: H1 gates H2/H3, and all three gate H4. Testing H4 now would produce a
false negative.

---

## 2. Step 1 - settle H1 in one cheap run before building anything

Do not instrument first. One run decides it.

**Experiment**: run the existing Route B install with `RETROCORE_ND5000_RUNTHREAD` **unset**
(default true) and drive the console as before.

- If the console is dead at the first ESC, H1 is confirmed as a fork and the work moves to
  section 3.
- If the console works and `RECOVER-DOMAIN` proceeds past `Loading Swapper`, then the run-thread
  claim is stale, and the install may simply complete. Either outcome is progress.

**Additional cheap check, same run or offline**: assert what the ND-500 CPU was actually doing
while the ND-100 waited. Log at the stall point: CPU run state, PC, StopMode, and instructions
retired since `RECOVER-DOMAIN` was typed. **Zero instructions retired proves H1 outright** and
needs no further argument.

---

## 3. Step 2 - fix the console-versus-run-thread conflict

Only if section 2 confirms the fork. This is the real engineering.

**Symptom to reproduce first**: with the run thread on, boot is healthy but ESC never produces
`ENTER`. So the ND-100 keeps running while its terminal input path stops working.

Diagnosis order, cheapest first:

1. **Establish where the character dies.** Instrument the four points a typed character passes:
   `SerialDataOutput` entry, the terminal device's input register/queue, the ND-100 interrupt
   raise, and the driver read. Log each with a sequence number and a host timestamp. The first
   point that never fires is the answer. Do NOT guess between them.
2. **Test the obvious mechanism: an unsynchronised shared structure.** Two host threads now touch
   the machine (ND-100 clock loop, ND-5000 run thread). Candidates in order of likelihood:
   shared interrupt state, the device register file, and the MPM aliasing. A lock-contention or
   torn-state bug here would present exactly as "boot fine, input dead".
3. **Test the second mechanism: starvation.** If the ND-5000 thread spins hot (the microcode IDLE
   loop is a poll), it can starve the ND-100 loop on a busy box. Measure both threads' actual
   progress rates over the same wall-clock window. This is measurable, so measure it rather than
   reasoning about it.
4. **Fix, then re-run the section 2 experiment as the regression test.**

**Deliverable**: the octobus fullflow ladder green WITH the run thread on. That is the gate for
everything below.

---

## 4. Step 3 - only now, instrument the install itself

With a machine that both executes ND-500 code and accepts typing, add install-level logging.
Log at four layers, all correlated by a single monotonic sequence number so a stall can be placed
in one of them without ambiguity:

1. **Monitor command layer** - each command typed, each prompt returned, and the wall-clock gap
   between them. Makes "which command stalled" unambiguous instead of inferred from a truncated
   transcript.
2. **ND-100 to ND-500 handoff** - the PLACE/RUN path: context block contents, the activation
   write, and the ND-500's first PC. A domain that never starts and a domain that starts and
   faults look identical from the console; here they do not.
3. **ND-500 execution** - PC trace (bounded ring), any fault/trap, and the MON calls the domain
   makes. `NLL:` never appearing is consistent with both "never started" and "started and died",
   and this separates them.
4. **File system** - every file the install opens, creates or writes, with the SINTRAN error code
   on failure. This is the layer that would catch the Route A module-4 silent failure, and it is
   the layer that answers the user's literal question about the new domain.

**Reuse, do not rebuild**: the harness already has the exchange log, the frame trace, the MPM
access/writes traces and the MON call log. Extend those with the correlating sequence number
rather than adding a parallel logging system.

---

## 5. Step 4 - resolve the ACCP timeout banner (owed to the other team regardless)

`ND-5000 timeout: ACCP was terminated; Microprogram has stopped` is printed by the monitor on
entry, unprompted.

Today's ACCP work told the other team that 244B TERMINATE is an **unconditional bring-up step**
with a 100%-answered command history behind it, and that it is not evidence of a timeout. **This
banner says "timeout" in SINTRAN's own words, so that claim is now in question.** It has already
been sent to them, so this is owed work, not optional.

Method: find the code that emits this text (it is monitor-side, so the ND-500-MON carve, not the
NPL kernel), and determine whether the string is (a) emitted from a genuine timeout path, or
(b) a shared status-message table entry reused for "microprogram not running", which would make
the wording misleading rather than the finding wrong.

**Do not resolve this by reasoning about which is more likely. Find the emitting code.** Then
correct the ACCP documents whichever way it falls, as was done for the 244B counting error.

---

## 6. What would falsify each hypothesis

Stated up front so the answer is not fitted to a preferred conclusion.

| Hypothesis | Confirmed by | Falsified by |
|---|---|---|
| H1 no execution | zero ND-500 instructions retired during the stall | a non-zero, advancing PC while the ND-100 waits |
| H2 swapper load fails | swapper load starts and does not complete, with the CPU running | swapper reaches its message loop and the ND-100 still waits |
| H3 timeout is real | the banner traces to a genuine timeout path AND the microprogram is stopped at that moment | the banner is a shared table entry, or the microprogram is demonstrably running after it prints |
| H4 domain broken | `COPY-DOMAIN` runs and the domain still fails to place or run | the domain places and runs once H1-H3 are cleared |

---

## 7. Order of work - REVISED after the corrections above

The original order (below) assumed a console/run-thread fork that does not exist. Current order:

1. **Rebuild with the new `Nd5000 attach` command and drive the machine interactively.** This is
   now the fastest route to the install: a human at the console can see each prompt and react,
   instead of a scripted test guessing at timeouts. Needs the running RetroCore closed first.
2. **Get a current-day transcript of `RECOVER-DOMAIN`.** Nothing is known about it on the fixed
   build; the only evidence is two days old (C1).
3. **Get a real stack for the process exit** (`--blame-crash`), floppy attached with the run
   thread on. Do not theorise about it before then.
4. Section 5 - the ACCP timeout banner. Unchanged and still owed.

The original section 3 (fix the console/run-thread conflict) is **dropped** - falsified by C2.

## 7b. Original order of work, and why

1. Section 2 - one run, settles H1. **Cheapest, and gates everything.**
2. Section 3 - fix the run-thread/console fork. **The actual blocker.**
3. Section 4 - install instrumentation. Only useful once the install can progress.
4. Section 5 - the ACCP banner. Independent of the install, owed to the other team, and can be
   done in parallel by anyone reading the monitor carve.

**Explicitly NOT doing yet**: Route C (offline ndtool placement). It sidesteps the emulator and
would put files on the pack, but it cannot do `DEFINE-STANDARD-DOMAIN` and it cannot tell us why
the machine stalls. Using it now would produce a pack that might work while leaving the actual
defect undiagnosed. Reconsider only if the live path is judged not worth fixing.

---

## 8. Assumptions and unknowns, marked

- **ASSUMPTION**: that `> Loading Swapper` is the last thing printed because the ND-100 is
  waiting on the ND-500, rather than because output stopped being captured. Section 2's
  instruction-retired check settles this and should be done before anything is built on it.
- **UNVERIFIED**: whether the run-thread/console conflict still exists. The harness comment
  dates from 2026-07-28 and has not been re-tested since today's octobus fixes landed.
- **UNVERIFIED**: whether `RECOVER-DOMAIN` on this monitor version requires a swap file. The
  install document lists "a swap file defined" as a prerequisite whose status on this pack was
  never determined. If section 2 clears H1 and the stall persists, check this next.
- **NOT ASSUMED**: that the domain on the floppy is good. It is untested, and stays untested
  until the run reaches `COPY-DOMAIN`.

---

## Related documents

- [ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md](../../Installation/ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md) - the procedure, 3 routes, 11 known failure modes
- [ND-500-MON Setup and Operations Guide](../../Reference-Manuals/500/ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md) - domains, PLACE-DOMAIN, swap files
- [ACCP-EMULATION-STATUS-AND-HANDOFF.md](ACCP-EMULATION-STATUS-AND-HANDOFF.md) - the 244B finding this plan's section 5 re-opens
- [OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md](OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md) - the gap register
