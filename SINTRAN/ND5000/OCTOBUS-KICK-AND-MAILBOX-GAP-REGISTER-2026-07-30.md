# OCTOBUS kick + mailbox GAP REGISTER

**Date**: 2026-07-30
**Scope**: the ND-100 <-> ND-5000 (SAMSON) octobus path in RetroCore. What SINTRAN and the real
B30 microcode do at the kick / mailbox layer, versus what `OctobusND5000Station` actually
implements.
**Method**: SINTRAN NPL as the requirement, the **real B30 microcode executed** as the behaviour
oracle, RetroCore source as the current state. No inference where execution could answer instead.

Companion: `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` (the shutdown path that exposed
gap G1).

---

## 0. The alignment result - kick 3 is now SETTLED by execution, not by reading

The blocker on implementing kick 3 was the acknowledge value. The carve summary
(`ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` correction 3) says *"write X5CLR back with bit 15
cleared"*, which cannot be literally right: SINTRAN writes `0o77` = `0x003F`, which already has
bit 15 clear, while `ST0PSYS` polls the cell for **zero**.

Resolved by **running the real microcode** (`MICRO-5800-B30.DATA`) from `OCB_KICK03` with
`X5CLR = 0o77`, in the new test
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\MailboxClrKickTests.cs`.
The write trace is unambiguous:

```
W: CS 025627: [ext+0x12] w2 := 00000001      X5CCL := 1      (cache-clear counter)
W: CS 014702..014736: [0x800..0x870] w4 := 0 (a block of 24 word writes - the PCB/context region)
W: CS 025536: [ext+0x10] w2 := 00000000      X5CLR := 0      <-- THE ACKNOWLEDGE
W: CS 025421: [ext+0x0C] w2 := 0000FFFF      X5PRO := -1     (ND-500 IDLE = "forget process")
```

**So the acknowledge is a plain zero write to `X5CLR`, not a bit-15 mask operation.** The carve
wording was a compression of the listing. SINTRAN's poll-for-zero is satisfied, and the earlier
"unresolved contradiction" is closed.

Independently corroborated from the raw microwords (the rendered `.md` mis-renders ORCON/MARG, so
these are raw `lo` halves) and from the xref table `MICRO-5800-B30.LABE`:

```
OCB_KICK03   025522*  <- referenced from 016433 (the OCB_DEC_K kick table)
MSG_CLEAR_1  016132*  <- referenced from 025527   (cache clear by mask)
OCB_WAITSEX  025543*  <- referenced from 025547   (the bit-15 spin)
OCB_KICK031  025550*  OCB_KICK032 025552*  OCB_KICK06 025561*

025525  lo=...2B564210  MARG 0x10 = X5CLR                    (the read)
025535  lo=...2B5E4210  MARG 0x10 = X5CLR                    (the write-back)
025543  lo=...2B644228  MARG 0x28 = GLOBAL header word 0o24  (the OCB_WAITSEX spin)
```

`OCB_WAITSEX` was **not** entered on either mask tested (the run with bit 15 set produced the same
26 writes), so the spin's trigger condition is still `[OPEN]` - recorded as an observation in the
test rather than encoded as a guess.

### 0.1 The kick-3 contract our station must implement

Three mailbox side effects, all in the per-CPU extension block:

| Cell | Word | Byte | Value | Why |
|---|---|---|---|---|
| `X5CLR` | 0o10 | +0x10 | **0** | `ST0PSYS`'s wait loop exits only on zero, else `ERRFATAL` |
| `X5CCL` | 0o11 | +0x12 | **1** | the cache-clear counter SINTRAN reads/compares |
| `X5PRO` | 6 | +0x0C | **-1** | ND-500 IDLE - the "forget process" half of the mask |

Plus: **read the mask from `X5CLR`, never assume `0o77`.** `LMPCLR`
(`MP-P2-N500.NPL:1222`, commented *"Clear-function; ND5000 only"*) takes it from the swapper's
message at `SWMSG + 5DP2`, so it is runtime data and bit 15 genuinely can be set.

---

## 0.2 CORRECTION (same day): fixing G1 does NOT fix stop-system - G10 sits above it

After implementing G1 the live harness STILL showed `X5CLR = 0x003F` after `stop-system`. Chased
with real observables rather than inference (the station's `Log()` never reaches TestContext, so
"I saw no log line" is not evidence - see the marker rule). Added `KickCounts[64]` +
`KicksDroppedDisabled` on the station and `TxFrameCount` / `TxKickCount` / `TxLastKickDest` /
`TxLastKickRoute` on the card. Measured, one run:

```
OCTOBUS TX  before-stop-system: frames=840 kicks=0
OCTOBUS TX  after-stop-system:  frames=841 kicks=1 lastKickFrame=0xB843 lastKickDest=56 route=fabric
KICKS (station):                NONE RECEIVED  droppedDisabled=0 kicksEnabled=True
```

Reading it:

- SINTRAN sends **exactly one kick in the whole run** - `CLRKICK` during `stop-system`. `0xB843`
  decodes as C=1, K=1, M=0, station bits = 56 (70B), info = 3. Correctly formed, correctly aimed.
- `route=fabric` - a station IS registered at 56 and `OctobusFabric.SendFrame` was called. The
  delivery frame is `0x8143` (station field rewritten to the source, K preserved), so
  `HandleFrame` really did receive it.
- The station still counted nothing, because the kick is dropped BEFORE the kick branch by this
  guard in `OctobusND5000Station.HandleFrame`:

```csharp
if (_accpIdle) {                                       // ACCP terminated (244B)
    ...
    if (!accpMultibyte && !accpBodyByte) return null;  // comment: "keep ignoring kicks"
}
```

**This drop is AUTHENTIC, not a bug.** `_accpIdle` is set in exactly one place - emergency
**244B TERMINATE ACCP** - which STOPS the microprogram (`_microprogramRunning = false`). A kick is
delivered to the microprogram via AOB + ATRAP + OMESS, so a stopped microprogram cannot execute it.
Real hardware behaves the same way: `ST0PSYS` asks a terminated CPU to clear its cache, nothing
answers, and the poll exhausts into `ERRFATAL`.

Also worth recording: kick 1 was never sent either. That is correct and not a gap - activation goes
through the `X5ACT := 0` write (ACT51), and the kick is the PREEMPT path only.

### G10 - something makes SINTRAN TERMINATE the ACCP mid-run  **[P1, NEW, OPEN]**

244B is what the ND-500 monitor sends **on ACCP timeout** (manual chapter 5.3.9). So during the
ladder some ACCP exchange of ours failed to answer, or answered too late, and SINTRAN gave up on the
microprogram. Everything after that point runs against a terminated ACCP - which is why `stop-system`
reaches `ERRFATAL` no matter how correct the kick-3 handler is.

**This is now the top open item, above every other gap here.** The next step is to find WHICH
exchange timed out: log the 244B arrival with a timestamp and the preceding ACCP command, then work
back to the command that went unanswered. Do NOT "fix" the `_accpIdle` guard to let kicks through -
that would paper over a real timeout with behaviour the hardware does not have.

---

## 1. GAP REGISTER

Severity: **P1** = a SINTRAN path provably breaks (an `ERRFATAL`/timeout we can name).
**P2** = incorrect but no known SINTRAN caller. **P3** = robustness / diagnostics.

### The kick dispatch table

`OCB_DECODE` -> `OCB_MES_K` -> `VECT := word & 0o77` -> `OCB_DEC_K` (016430), 64 entries. In
RetroCore, `OctobusND5000Station.cs:1330-1367` loads every kick into the AOB and raises
`KickReceived`, but **only `kickNumber == 1` drives any behaviour** (`WalkQueue()`).

| Kick | SINTRAN sender | Microcode handler | Our station | Gap |
|---|---|---|---|---|
| 0 | - | `NOTREC` | logged | **G6 FIXED** |
| 1 `N100KICK` | `ACT52` @145520 | `ACTIVATE` | `WalkQueue()` | none |
| 2 | none found in NPL | `ACTIVATE` (shares 1) | `WalkQueue()` | **G4 FIXED** |
| 3 `CLRKICK` | `ST0PSYS` @147467, `LMPCLR` @136732 | `OCB_KICK03` 025522 | `ExecuteClearFunctions()` | **G1 FIXED** |
| 4, 5 | none found in NPL | `OCB_KICK05` 025553 | logged, not implemented | **G5** (P2) |
| 6 `IDLEKICK` | `TER51` @145230 | `OCB_KICK06` 025561 | logged, not implemented | **G2** (P1) |
| 7-63 | n/a | `OCB_KICK64` -> `UNLOCK_QUE` + `NOTREC 204` | logged | **G6 FIXED** (unlock still owed) |

**Implemented 2026-07-30** in `OctobusND5000Station.cs`: the kick handler is now an explicit
`switch` over the table instead of an `if (kickNumber == 1)`, so kicks 2 and 3 do their real work
and every unhandled number logs UNCONDITIONALLY naming the microcode routine it should have run.
Tests: `OctobusMailboxO1Tests.KickPath_Kick3_*` (station level, 14/14 green) and
`MailboxClrKickTests` (real-microcode oracle, 3/3 green).

---

### G1 - kick 3 (`CLRKICK`) is a no-op  **[P1, CONFIRMED LIVE - NOW FIXED]**

**Requirement**: `ST0PSYS` writes `X5CLR := 0o77`, sends kick 3, then polls `X5CLR` for zero up to
1000 times; `CALL ERRFATAL` if it never clears (`MP-P2-N500.NPL:3759`).

**Evidence**: measured on the live octobus harness - `X5CLR` = `0x0000` before `stop-system`,
`0x003F` after. Nothing consumed it. `X5CLR` appears in RetroCore only in doc comments
(`IServicerHost.cs:116`, `OctobusND5000Station.cs:628`).

**FIXED 2026-07-30**: `OctobusND5000Station.ExecuteClearFunctions()` reads the mask from
`X5CLR` (never assumes `0o77`), then writes `X5CCL := 1`, `X5PRO := -1`, and `X5CLR := 0` **last**
(the release signal must come after the other two, or SINTRAN can proceed while they are stale) -
all three under `_mpm.SyncRoot`, since the ND-100 polls `X5CLR` from another thread and must never
see a half-applied acknowledge.

Deliberately not modelled: the mask's cache / data-TSB / dump bits have no physical effect because
we model no ND-5000 cache or TSB - there is nothing to invalidate, so acknowledging IS the complete
correct behaviour here.

**Note**: the `ST0PSYS` poll is **bounded**, so this can never HANG stop-system - it degrades to
`ERRFATAL`. Do not reach for this gap to explain a hang.

---

### G2 - kick 6 (`IDLEKICK`) is a no-op  **[P1]**

**Requirement**: `TER51` (`MP-P2-N500.NPL:145230`) is the ND-500 TERMINATE path:

```npl
TER51:  FOR LOOPCOUNTER DO
            IDLEKICK; CALL XKICK500
            CALL GETC5PROC
            IF A=-1 GO OKRET            % X5PRO idle -> terminated cleanly
        OD
TER52:  ESPTIMOUT                       % never went idle -> timeout error
```

So SINTRAN sends kick 6 in a loop and leaves only when `X5PRO` reads `-1`. With kick 6 ignored,
`X5PRO` never becomes `-1` and every terminate falls into `ESPTIMOUT`.

**This is the same shape as G1**: a mailbox cell SINTRAN polls that our station never writes. It
was invisible for the same reason - the failure is a timeout on a path nothing in the harness
exercises yet.

**Microcode** (`OCB_KICK06` 025561, per the carve): `CNTXTSAVE` if a process is loaded, `SET_IDLE`,
`OCB_CLNUP`, `UNLOCK_QUE`, `PRNOWR(SC14)`, `IDLE`. The observable `TER51` depends on is
`X5PRO := -1`. A test asserting exactly that against the real microcode is in
`MailboxClrKickTests.OcbKick06_IdleKick_SetsX5ProIdle_WhichTer51PollsFor`.

**Fix**: implement kick 6 to set `X5PRO := -1` and park the CPU idle. `CNTXTSAVE` and `OCB_CLNUP`
are larger pieces - see G3.

---

### G3 - `OCB_CLNUP` requeue semantics not modelled  **[P2]**

`OCB_CLNUP` (025570) is reached from kicks 4/5/6. Per the carve it does NOT walk/discard the
message region: it un-claims the **in-progress** message - `DPA := ADR_MESS`, check `5CPUN@-6`
against this CPU, clear `MSGME` (srf 2021), `MSG_CCMOVE`, then **write `N5STA := 1`** (back to
`MSGN500`), returning the message to the queue **unanswered**.

**So kicks 4/5/6 REQUEUE in-flight work, they do not drop it.** Any implementation of G2/G5 that
discards the current message is wrong in a way that will look like lost messages much later.

Not yet modelled at all in our station.

---

### G4 - kick 2 not mapped to ACTIVATE  **[P2]**

`OCB_DEC_K` sends both 1 and 2 to `ACTIVATE`. We handle only 1. No NPL sender for kick 2 was
found, so nothing known breaks - but the mapping is one line and its absence is a silent
divergence from the table.

---

### G5 - kicks 4/5 (`OCB_KICK05`) not implemented  **[P2]**

`OCB_KICK05` (025553, shared by kicks 4 and 5; there is no separate `OCB_KICK04`): `SET_IDLE`,
`LOCK_QUE`, `OCB_CLNUP`, `UNLOCK_QUE`, `PRNOWR(0)`, `NOTREC 204` - a stop-and-clean-queue. No NPL
sender found, so this is unproven-need; do not build it ahead of a caller. Listed for completeness
of the table. Depends on G3.

---

### G6 - unrecognised kicks are silently swallowed  **[P3]**

Kick 0 and kicks 7-63 must reach `NOTREC` (`OCB_KICK64` = `UNLOCK_QUE` + `NOTREC 204`), i.e. the
real machine **reports** an unrecognised kick and releases the queue lock. Ours drops them with no
diagnostic (a `DEBUG_DETAIL`-only log for disabled kicks, nothing for unhandled numbers).

Two consequences: a wrong kick number is undiagnosable, and the `UNLOCK_QUE` is skipped, so a
queue lock taken before a bad kick would leak.

**Fix**: default branch that logs the kick number unconditionally, plus the queue unlock.

---

### G7 - `X5ACT` is never re-armed  **[P2, needs one check]**

The microcode's IDLE loop, on finding work, **re-arms `X5ACT` to 1** and then walks the `X5BEX`
chain. I found no write to `_extBlockBase + 0x0A` anywhere in `OctobusND5000Station.cs` - the cell
is read, hooked and logged only.

Why it may work anyway today: on the CS-derived path `OnMpmActivationWrite` triggers on *a write to
the cell*, not on a `0xFFFF -> 0` transition, so a repeated `0` write still wakes us. The
`wasMinusOne` fallback path, though, requires a real `0xFFFF -> 0`, and that path cannot fire twice
if nothing ever restores the cell.

**Graded [P2, verify] rather than asserted**: I have not proven a SINTRAN path that breaks. The
check is cheap - assert `X5ACT` after a walk in the existing mailbox tests.

---

### G8 - `X5CCL` (cache-clear counter) never written  **[P2]**

`X5CCL` (word 0o11, byte +0x12) is documented as *"cache-clear counter (read/compared)"* and the
executed microcode sets it to 1 during `OCB_KICK03`. We never write it. Subsumed by the G1 fix
(section 0.1), listed separately because any other cache-clear path owes the same write.

---

### G9 - `OCB_WAITSEX` trigger condition unknown  **[OPEN, not a defect yet]**

`OCB_WAITSEX` (025543) spins on **global header** word `0o24` (byte 0x28) until zero. The carve says
it is armed when the original `X5CLR` had bit 15 set, but running the real microcode with
`X5CLR = 0x803F` produced the *same* 26 writes and did not enter the spin, so the stated trigger is
not confirmed.

**Not on the critical path**: `ST0PSYS`'s `0o77` never sets bit 15. It could matter for `LMPCLR`,
whose mask is runtime data. Leave unimplemented with an explicit comment; do not guess.

---

## 1.1 Harness reproducibility bug - FIXED, and it invalidated one run

`Nd100SintranNd5000OctobusBootHarnessTests.EnsureWorkingCopy` refreshed the working pack only when
it was MISSING or a DIFFERENT SIZE. The pack is a fixed-size disk image, so after the first run the
copy never happened again - while SINTRAN WRITES to the pack during boot and login. State
accumulated run over run, and any damage was permanent.

Not theoretical: two runs of identical code gave a fully green ladder and then
`nd-500=STALL status=STALL start-swapper=STALL` with a garbage mailbox base of `0x7FFFF6` (the
ND-500 monitor never came up at all). Forcing an unconditional copy restored the green ladder
immediately, which confirms the diagnosis.

**Fixed**: the working copy is now always overwritten from source in `SetUp`. Costs a second or two
against a multi-minute boot. A harness whose starting state depends on how many times it has been
run cannot be used as evidence about the machine, which is the entire purpose of the fixture.

---

## 2. Not gaps - recorded so they are not re-opened

- **`ST0PSYS`'s wait loop cannot hang.** `L` runs `-1000 -> 0`; worst case is `ERRFATAL`.
- **`stop-system` itself works.** The historical `STALL` was an unmatchable harness marker, not the
  machine. Full ladder green - see the companion doc.
- **Kick 1 is never sent during a normal boot, and that is correct.** Activation is the
  `X5ACT := 0` write (ACT51); the kick is the PREEMPT path only. `TxKickCount` was 0 for the whole
  run until `stop-system`.
- **The `_accpIdle` kick drop is correct behaviour.** A terminated ACCP (244B) has a stopped
  microprogram, and kicks are delivered to the microprogram. Do not "fix" it to let kicks through.
- **`LMPCLR` does not poll `X5CLR`.** It writes, kicks, and answers OK
  (`OKMONICO`/`XACTRDY`). That is why G1 is silent on the swapper path and only surfaces at
  shutdown.
- **The ND-500 (3022) path does not share these gaps.** It runs *different microcode* from the
  ND-5000, and we have no ND-500 microcode yet (expected within weeks as of 2026-07-30). Octobus is
  the focus; do not start ND-500-side microcode work on the strength of this document.

---

## 3. Order of work

**Done 2026-07-30: G1, G4, G6** (diagnostic half), plus the harness-reproducibility fix below.

**0. G10 FIRST** - SINTRAN terminates the ACCP mid-run (section 0.2). Until that is understood,
everything downstream runs against a stopped microprogram and no amount of kick-handler correctness
changes the outcome.

Then:

1. **G7** (verify `X5ACT` re-arm) - one assertion in the existing mailbox tests; either closes the
   gap or promotes it to a defect.
2. **G2** (kick 6) - `X5PRO := -1` at minimum, which is what `TER51` actually polls. Full fidelity
   needs G3. This is the last known **P1**.
3. **G3** (`OCB_CLNUP` requeue) - prerequisite for an honest G2/G5. Getting this wrong shows up much
   later as lost messages, so do not fake it.
4. **G6 remainder** - the `UNLOCK_QUE` that `OCB_KICK64` performs; currently we log but do not
   release a queue lock.
5. **G8** - subsumed by G1 for the kick-3 path; any other cache-clear path owes the same `X5CCL` write.
6. **G5**, **G9** - do NOT build ahead of a proven caller / a resolved trigger.

---

## 4. Sources

- `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL` - `ST0PSYS` @3759, `LMPCLR` @1222, `TER51` @2950 (145230), `ACT51/ACT52` @3012/3032, `XKICK500` @3278
- `SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT` - `X5CPU=4 X5ACT=5 X5PRO=6 X5CLR=0o10 X5CCL=0o11`, `MPACT=1`, `5ALIV=0o15`
- `SINTRAN/ND5000/ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` - `OCB_DEC_K` table, kick handlers, `OCB_CLNUP`
- `MICRO-5800-B30.DATA` + `MICRO-5800-B30.LABE` (in `RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\MC\`) - executed oracle + label xref
- `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\MailboxClrKickTests.cs` - the executed kick-3 / kick-6 tests
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs:1330-1367` - the kick handler as it stands
