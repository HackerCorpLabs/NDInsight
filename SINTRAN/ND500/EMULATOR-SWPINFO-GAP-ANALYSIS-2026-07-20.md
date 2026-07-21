# EMULATOR: why is SWMSG.SWPINFO zero when the swapper reads it? (2026-07-20)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\EMULATOR-SWPINFO-GAP-ANALYSIS-2026-07-20.md`
**Track:** RetroCore ND-500 emulator (read-only static analysis; no code changed).
**Question:** the D4 swapper runs to `PC=0x0800913B` then null-derefs (`w4 laddr r2.(10)`, r2=0)
because its work-message pointer `SWMSG.SWPINFO` (field `HSWPI=0o104`) reads zero. Which of the four
candidate causes is it, and what is the exact fix?

**Grades:** `[V]` = verified by direct static reading of the cited RetroCore source; `[I]` = inference
from that reading; `[LIVE]` = needs a live trace / harness output to settle. ASCII only; octal shown
as `NNNB` or `0oNNN`, hex as `0xNN`.

---

## BOTTOM LINE (verdict)

**It is CAUSE 1 + CAUSE 3 (one mechanism), NOT cause 2 and NOT cause 4.**

The emulator writes `SWMSG.SWPINFO` NOWHERE itself - it depends 100% on the REAL SINTRAN (running on
the emulated ND-100) to post it, via SINTRAN's answer to the swapper's `MON 377B` (delivered as a
24B/26B restart write-back). The swapper is being STARTED and driven into its message-dispatch loop
(the emulator runs the real swapper code on `CpuND500`) in a system state where SINTRAN has NOT posted
a real activating work message. So SINTRAN answers the swapper's `MON 377B` with `SWPINFO = 0`, the
`RIOM` pulls an empty message, and the dispatch null-derefs. This is exactly the carve's conclusion
(`CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md` section 6): the swapper is kicked without a real
page-faulting ND-500 process behind it, so `SWMESS`/`5ACTSWAPPER`'s `CNVWADR(activating-message)`
yields zero.

- **Cause 2 (address mismatch) is NOT the current failure.** With `SWPINFO = 0` the `RIOM` source is
  `0`, not a mismatched `0x460E30`-vs-`0x420E30`. The `_private = 0` override
  (`Nd500CpuProcessBridge.cs:75`) is already correct; it only starts to matter AFTER `SWPINFO` becomes
  non-zero. So cause 2 is a LATENT downstream issue, not the stop at 0x913B. `[V]`
- **Cause 4 (CNVWADR / field offset) is NOT an emulator bug.** The emulator performs no `CNVWADR` and
  writes no `SWPINFO` field. `CNVWADR` is SINTRAN's, on the ND-100 side. `[V]`

**The single load-bearing fact still missing** (`[LIVE]`): whether SINTRAN's `SWPINFO` is zero
*already in `SWMSG` at 23B-start time* (-> cause 1/3 confirmed, the start is premature/workless), or
is non-zero in `SWMSG` but arrives as zero through the `MON 377B` write-back path (-> would reopen
cause 2). The emulator can read this directly (see the fix) - it does not require the CPU round-trip.

---

## 1. The evidence chain, static, file:line

### 1.1 The emulator never posts SWPINFO - it relays SINTRAN [V]

Grepped the whole `Emulated.HW\ND\CPU\ND500` tree for the `SWPINFO`/`HSWPI` field (word offset `0o104`
inside `SWMSG`, byte `0x88`; the 32-bit pointer spans `HSWPI=0o104`..`SWPIN=0o105` per the carve). The
only hits are COMMENTS (`Riom.cs:255/269/313/315`, `Nd500CpuProcessBridge.cs:474`). No servicer or
bridge code ever writes message word `0o104`. The servicer's message writes cover only
`N5STA/SENDE/X5CPU/N500A(7)/N500A_LO(10B)/STOPR(11B)/NUMPA(12B)/MCNO(13B)/MSWMC(14B)/TRAPN(16B)` plus
the `0x40`/`0x80` arg slots (`Nd500MicrocodeServicer.cs:757-773`, `891-903`). Conclusion: the emulator
holds no copy of `SWPINFO` and fabricates none - it can only carry whatever SINTRAN produces. `[V]`

### 1.2 How the swapper actually obtains SWPINFO in the emulator [V]

Carved swapper MAIN (`CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md` section 3.1): clears message-control
`0x240B0/0x240B4` -> `MON 377B` sub-fn 1 (ask ND-100 for a message) -> `h riom ...` (DMA-pull it) ->
`jumpg` dispatch. In the emulator that `MON 377B` is intercepted at
`Nd500CpuProcessBridge.OnMonitorCall` (`Nd500CpuProcessBridge.cs:457-491`), which calls
`servicer.AnswerMonitorCallStop` (`Nd500MicrocodeServicer.cs:746`): it writes a MOCALL stop record and
answers the process message, PARKING the CPU. Real SINTRAN on the ND-100 then services `MON 377B` and
issues a 24B/26B restart. The restart's write-back is read by `ReadRestartRecord`
(`Nd500MicrocodeServicer.cs:814-840`) and applied by `ApplyRestartWriteBack`
(`Nd500CpuProcessBridge.cs:383-412`), which does `cpu.WriteVirtualMemory32(addr, value)` into the
swapper's DSEG cell `0x240B4`. `RIOM` then reads operand 0's VALUE = that cell = `SWPINFO`
(`Riom.cs:258`) and copies from ND-100 physical `SWPINFO*2` (`Riom.cs:408,423`;
`CpuND500.ND100Bridge.cs:82-91`). So the `SWPINFO` the swapper sees is precisely SINTRAN's `MON 377B`
answer value. `[V]`

### 1.3 It is observed zero [V, from the code's own note]

`Nd500CpuProcessBridge.cs:66-74` (the `_private` derivation comment) states plainly: the 0x210718 ->
0x420E30 agreement "has also never been observably confirmed, because the source reads zero either way
in the runs so far." That is the emulator author recording that SINTRAN's `MON 377B` write-back
delivers zero. The D4 harness instruments exactly this: `_bridge.MonCallLog`, the `RESTART write-back`
lines, and the side-by-side dump of `DSEG+0x240BC` vs ND-100 `0x420E30`
(`Nd100SintranNd500BootHarnessTests.cs:1101-1121`). `[V]`

### 1.4 Why SINTRAN's SWPINFO is legitimately zero [I, from carve]

`SWMSG.SWPINFO = CNVWADR(activating-message-address)`, set by `SWMESS` (`133654`) or `5ACTSWAPPER`
(`145006`) - and ONLY set when a real activating process/message exists
(`CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md` sections 1, 6). In the D4 flow the swapper is
brought up as a placement/bring-up formality, with no page-faulting ND-500 domain process behind it,
so there is no activating message to convert -> `SWPINFO` stays zero. The emulator's own machinery
runs the swapper CPU into its dispatch loop regardless (see section 2), so the zero becomes a
null-deref instead of a benign "no work, go back to sleep". `[I]`

---

## 2. The emulator-side trigger (why the swapper runs at all with no work)

`Nd500CpuProcessBridge.OnStartProcess` (`Nd500CpuProcessBridge.cs:111-160`) runs the real swapper on
`CpuND500` whenever a 21B register image was stashed (`servicer.ProcessZeroRegisterImage != null`,
line 126) and a 23B `3START` arrives (`Nd500MicrocodeServicer.cs:515-544`). It then installs the MMU
mapping (`InstallSwapperMapping`, line 147-151) and wakes the run thread (line 156-157). Nowhere in
this path does it check that the activating `SWMSG` carries a non-zero `SWPINFO` - i.e. that SINTRAN
has real work for the swapper. This is the "functional swapper" hack (comment lines 134-146): it runs
the swapper's macro-code to make it do real work, but there is no gate ensuring there IS work. `[V]`

Note also the currently-UNUSED `AnnounceSwapperAlive` (`Nd500MicrocodeServicer.cs:797-804`): an older
"announce alive without running the CPU" path (synthesizes the `MON 377B` self-announce, VALUE=1). It
is defined but not called anywhere (grep: only the definition). The current design deliberately runs
the real CPU instead - which is exactly why an empty activation now crashes rather than being absorbed
by the synthetic announce. `[V]`

---

## 3. What is CERTAIN vs what needs a LIVE trace

**Certain from static reading:**
- The emulator never sources `SWPINFO` itself; it relays SINTRAN's `MON 377B` write-back. (Rules out
  cause 4 as an emulator bug.) `[V]`
- With `SWPINFO = 0`, the `RIOM` source is 0; the `_private` value is irrelevant to the current stop.
  (Rules out cause 2 as the CURRENT failure.) `[V]`
- `OnStartProcess` drives the swapper into its dispatch loop with no check that a real work message
  exists. `[V]`

**Needs a LIVE trace to finish the proof (one value):**
- Read `SWMSG.SWPINFO` = `(ReadNd100Word(msgBase + 0o104*2) << 16) | ReadNd100Word(msgBase + 0o105*2)`
  at the moment the 23B `3START` is dispatched (`Nd500MicrocodeServicer.cs:515`). `msgBase` for the
  swapper start IS `SWMSG` (the activation message). This single value settles it:
  - `SWPINFO == 0` at 23B time -> CONFIRMS cause 1/3: SINTRAN posted no work; the start is
    premature/workless. (Expected, given section 1.3.)
  - `SWPINFO != 0` at 23B time but the `MON 377B` write-back still delivers 0 -> REOPENS cause 2: the
    write-back address/routing is wrong. (Contradicted by section 1.3's "reads zero either way", but
    only a direct read excludes it.)
- The D4 harness `MonCallLog` + `RESTART write-back` lines already capture the `MON 377B` side; add the
  23B-time `SWPINFO` read to close the loop. This does not require the CPU to run.

---

## 4. Proposed fix

### 4.1 Step 0 - the decisive instrumentation (do first, zero risk) [CERTAIN action]

**File:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND500\Servicer\Nd500MicrocodeServicer.cs`
**Method:** the `StartProcess`/`TrapContinue` case at line 515-544, right before it calls
`ProcessHost.OnStartProcess(msgBase, micfu)` (line 542).

Read and log `SWMSG.SWPINFO` from the activating message:

```
// HSWPI = 0o104 word offset inside SWMSG; SWPINFO is the 32-bit pointer at 0o104..0o105
// (CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md sec 1). This is the value SINTRAN will
// later hand the swapper for its MON 377B / RIOM. Reading it here says WHETHER SINTRAN posted
// a real work message BEFORE the swapper is ever run - the whole SWPINFO=0 question.
uint swpinfo = ((uint)host.ReadNd100Word(msgBase + (0o104 * 2)) << 16)
             |  host.ReadNd100Word(msgBase + (0o105 * 2));
host.ServicerLog($"MAILBOX 23B 3START @word 0x{msgBase >> 1:X6}: SWMSG.SWPINFO=0x{swpinfo:X8}");
```

This is a pure read (no behaviour change) and produces the one fact needed to lock the verdict.

### 4.2 Step 1 - the actual fix, branch-selected by Step 0's value

- **If `SWPINFO == 0` at 23B time (expected):** the swapper is being started with no work. The correct
  behaviour is NOT to drive the real dispatch loop into a null message. Two viable fixes; pick per how
  faithfully SINTRAN's own path should be reproduced:
  1. **Gate the run (smallest):** in `Nd500CpuProcessBridge.OnStartProcess`
     (`Nd500CpuProcessBridge.cs:111`), after loading the register image / installing the mapping, do
     NOT wake the run thread when the activating `SWMSG.SWPINFO == 0`; leave the CPU parked (WAIT) so
     it "sleeps" until a later 5ACTSWAPPER activation carries a real `SWPINFO`. This mirrors real
     hardware, where the swapper's `MON 377B` blocks until SINTRAN has a message.
  2. **Restore the aliveness-only announce:** for a workless START-SWAPPER, use
     `AnnounceSwapperAlive` (`Nd500MicrocodeServicer.cs:797`) to satisfy SINTRAN's "swapper free"
     handshake WITHOUT running the CPU, and only run the real `CpuND500` for activations that actually
     carry `SWPINFO != 0`. This restores the previously-working "alive but idle" state and confines
     real execution to real work.
  Recommended: option 1 (keeps the real-CPU path, adds one guard, does not resurrect dead code). The
  deeper upstream question - *why does SINTRAN never post a real activation on the RUN path* - is the
  pre-existing D4 RUN-precondition blocker and is out of scope for this null-deref fix; option 1 stops
  the crash and leaves the swapper correctly idle until that upstream work lands.

- **If `SWPINFO != 0` at 23B time (only if Step 0 surprises us):** the bug is in delivery, i.e. cause
  2. Fix the `MON 377B` answer/write-back so the non-zero pointer reaches DSEG `0x240B4`, and re-check
  the `_private`/`MapND100ToPhysical` mapping (`CpuND500.ND100Bridge.cs:82-91`) so `RIOM`'s source
  resolves to the 5MPM message buffer.

---

## 5. Regression risk to the currently-working run-to-0x913B

- **Step 0 (the read/log)** is a pure read of two ND-100 words at the existing `msgBase`; it cannot
  change any behaviour. Zero regression risk. `[V]`
- **Step 1 option 1 (park instead of wake when SWPINFO==0)** changes behaviour ONLY on the workless
  start that currently crashes - the swapper no longer reaches 0x913B, which is the point. It does NOT
  touch the path where `SWPINFO != 0`, so a genuine work activation still runs exactly as today. The
  one risk: if some OTHER consumer relies on the swapper always reaching 0x913B (e.g. a test asserting
  the null-deref trap), it would change - but that trap is the bug, not a contract. Confirm no test
  asserts the 0x913B trap as success before landing. `[I]`
- **Step 1 option 2 (announce-alive)** is higher risk: it reintroduces a code path
  (`AnnounceSwapperAlive`) that was deliberately parked, and splits swapper handling into two modes;
  more surface for divergence from SINTRAN's real handshake. Prefer option 1. `[I]`
- Do NOT change the `_private = 0` override (`Nd500CpuProcessBridge.cs:75`) as part of this fix - it is
  correct for this path and only bites once `SWPINFO` is non-zero (open question C4 in
  `OPEN-QUESTIONS-REGISTER-2026-07-20.md`). `[V]`

---

## 6. Evidence index

| Claim | Source (RetroCore unless noted) | Grade |
|---|---|---|
| Emulator writes no SWPINFO field; relays SINTRAN | grep `Emulated.HW\ND\CPU\ND500`; `Nd500MicrocodeServicer.cs:757-773` | [V] |
| SWPINFO delivered via MON 377B -> 24B/26B write-back | `Nd500CpuProcessBridge.cs:457-491,383-412`; `Nd500MicrocodeServicer.cs:746,814-840` | [V] |
| RIOM source = operand0 value = the DSEG SWPINFO cell | `Instructions/IO/Riom.cs:258,408,423` | [V] |
| Source observed zero in runs so far | `Nd500CpuProcessBridge.cs:66-74` (author note) | [V] |
| OnStartProcess runs swapper with no work-message gate | `Nd500CpuProcessBridge.cs:111-160`; `Nd500MicrocodeServicer.cs:515-544` | [V] |
| _private=0 override correct; latent, not current cause | `Nd500CpuProcessBridge.cs:75`; `CpuND500.ND100Bridge.cs:48,82-91` | [V] |
| AnnounceSwapperAlive defined but uncalled | grep -> only `Nd500MicrocodeServicer.cs:797` | [V] |
| SWPINFO=CNVWADR(activating msg), only set with real work | `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md` sec 1,6 | [I] |
| HSWPI=0o104, SWPIN=0o105 (32-bit pointer) | same carve, L07 symbol pins | [V, carve] |
| 23B-time SWPINFO value (the remaining discriminator) | needs live D4 run / Step-0 log | [LIVE] |

---

**File written:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\EMULATOR-SWPINFO-GAP-ANALYSIS-2026-07-20.md`
