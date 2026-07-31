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

> **LATER STATUS, read this before acting on the section below:** G10 is **FIXED AND VERIFIED**
> (2026-07-30). The analysis here is still correct about the kick being dropped by the `_accpIdle`
> guard, but its conclusion - that some ACCP exchange timed out - is **WRONG**. 244B TERMINATE is a
> normal bring-up step sent after 3 fully-answered commands. See the corrected G10 entry below.

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

### G10 - `_accpIdle` sticks after the 244B TERMINATE  **[P1, FIXED AND VERIFIED 2026-07-30]**

**The original title and premise of this gap were WRONG, and both are corrected here rather than
overwritten**, because the wrong version is what a reader would otherwise act on.

Originally filed as "something makes SINTRAN TERMINATE the ACCP mid-run", reasoning that 244B is
what the ND-500 monitor sends **on ACCP timeout** (manual chapter 5.3.9), so some exchange of ours
must have gone unanswered. **That inference was wrong.** Measured, not inferred: the 244B arrives
after exactly 3 ACCP commands with **all 3 answered**, and a full run answers **149 of 149**. It is
present in a fixed run and a broken run alike, at the same place in the ladder, with the same three
commands behind it.

**244B TERMINATE is an unconditional, NORMAL bring-up step. Receiving one is not a fault signal and
is not evidence of a timeout.** SINTRAN sends it, then restarts the microprogram.

The actual defect was the flag, not the terminate: `_accpIdle` was cleared only by `ContinueAccp`
and `ResetStation`, and never by `STAMIC0` / `CONTMIC` / `RESTMIC`. So the microprogram restarted
but the flag stayed set, and the guard in `OctobusND5000Station.HandleFrame` dropped **every kick
for the rest of the session**. That is why `stop-system` reached `ERRFATAL` no matter how correct
the kick-3 handler was.

The standing warning still holds and is now better founded: do NOT "fix" this by letting kicks
through the `_accpIdle` guard. The guard is authentic - a stopped microprogram genuinely cannot
execute a kick. The fix is to clear the flag on the restart paths that really do restart it.

**Verified:** `k3=1`, `X5CLR=0000`, `X5CCL=0001`, `accpIdle=False`, full ladder green.

**How we got it wrong:** the first clean capture predated the footer field that records the 244B
snapshot, so the line was simply not written. We read a missing FIELD as a missing EVENT, and built
a timeout theory on top of it.

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

**G3 probe 2026-07-30 - still open, and NOT reproducible the easy way.** Drove the real microcode
through `OCB_KICK06` (which calls `OCB_CLNUP` at 0o25564) with a message laid down, chained from
X5BEX, `N5STA = 2`. Result: **`N5STA` came back unchanged at `0002`** - the carve's "writes
N5STA := 1 (MSGN500)" did not happen.

Reading: `OCB_CLNUP` un-claims the CPU's CURRENT in-progress message, which it finds via
`ADR_MESS` (0o17334) and the `MSGME` cell (srf 0o2021), plus a `5CPUN@-6` check that the message
belongs to this CPU. Simply pointing X5BEX at a message does not make it "in progress", so the
routine has nothing to un-claim and no-ops. Pinning the contract needs that srf state set up
first - that is the next step, not more guessing.

Useful side result: our kick-6 implementation does NOT touch `N5STA`, so it is not consuming or
corrupting queued work - it is incomplete, not wrong. The test
`MailboxClrKickTests.OcbKick06_WithMessage_ShowsWhatOcbClnupDoesToIt` guards that.

### G3 probe 3, 2026-07-31 - the carve's `N5STA := 1` claim is NOT REPRODUCIBLE

Probe 2 supplied the state probe 1 was missing - `MSGME` (srf 0o2021) pointing at the message, and
the `5CPUN` ownership word at message-6 - and **swept the CPU number 0..15** rather than guessing
one, because a wrong guess and a genuine no-op look identical.

```
SUMMARY: 16/16 runs REACHED OCB_CLNUP, 0 changed N5STA, 0 requeued to MSGN500(1), owner=NONE FOUND
```

**All 16 runs REACHED `OCB_CLNUP` and none of them wrote `N5STA`.** The reachability watch is
proven non-vacuous by a control in the same test that asserts the watch reports FALSE for an address
the path never executes - so "reached" is a measurement, not an assumption.

This is **evidence of absence**, not absence of evidence, and it is the first result here that
actually contradicts the carve rather than merely failing to confirm it. Two readings remain open
and this probe does NOT choose between them:

1. The carve's "**write `N5STA := 1`**" is **wrong** - possibly read off a neighbouring routine or
   a mis-rendered listing, the same class of error already found in correction 3 of the catalog.
2. "In progress" needs **more than `MSGME` + `5CPUN`**, and the routine is still declining early.

**Do NOT implement `N5STA := 1` in the station on the strength of the carve.** It is now a claim
with a failed reproduction against the real B30 microcode, and the project rule is that executing
microcode outranks a carve summary.

Test: `MailboxClrKickTests.OcbClnup_SweepOwningCpu_ReportsWhichOneRequeuesTheMessage`.

### G3 probes 4 and 5, 2026-07-31 - reading 2 is CONFIRMED, and reading 1 is not needed

Probe 4 traced the microprogram counter step by step from the moment `OCB_CLNUP` is entered, and
diffed a run with `MSGME` set against one without.

**`OCB_CLNUP` occupies 0o25570..0o25604, but only FOUR microwords execute:**

```
step  CS
   0  25570   <- OCB_CLNUP entry
   1  17334   <- ADR_MESS
   2    104
   3    105
   4  25571
   5  25572
   6  25573
   7    104
   8    105
   9  25565   <- back in the CALLER (OCB_KICK06)
```

**The tail never runs.** `MSGME` is cleared at 0o25601, `MON_ERR?` is at 0o25605 and `MSG_CCMOVE`
at 0o25611 - none are reached. That is the whole explanation for "no `N5STA` write was ever
observed": the code that would do it is in a tail this path does not enter.

**The two traces NEVER diverge**, so `MSGME` does not steer this path at all - which retires the
assumption behind probes 2 and 3.

Probe 5 then logged every memory READ while the routine ran. **`OCB_CLNUP`'s executed microwords
perform ZERO memory reads** (the one logged read, `CS 25511 [0x2000] r2 = 0`, happens after control
has already left the routine). So:

- The early exit is decided by **register / srf state**, NOT by anything in memory.
- That is why supplying `MSGME` and the `5CPUN` word in memory changed nothing - the routine never
  looks at memory before deciding.
- The `5CPUN@-6` ownership check the carve describes **cannot be happening on this path**; it would
  require a memory read, and there is none.

**Status: reading 2 from probe 3 is CONFIRMED** - "in progress" needs state we are not setting, and
it lives in the srf/registers. Reading 1 (the carve is simply wrong) is neither needed nor
supported: the `N5STA := 1` code may well be correct and simply unreached.

Tests: `OcbClnup_TracePath_ShowsWhereItLeavesEarly`,
`OcbClnup_LogReads_NamesTheCellThatDecidesTheEarlyExit`.

### G3 probe 6, 2026-07-31 - THE BRANCH IS NAMED. Hand-decoded from the microword bits

Raw microwords from `MICRO-5800-B30.DATA` (16 bytes/word, word N at file offset N*16):

```
CS 25570 : 0000000000017006000000001EDC0000
CS 25571 : 50080000000180180000000000150000
CS 25572 : 40000001180E8000000000002B7B0000
CS 25573 : 400000006C0150218120000000440000
```

Decoded with `tools/microcode-5000-def.json` (SEQUENCER: `COND_SEQ` 69, `SEQ_TRUE` 68-65,
`SEQ_FALSE` 64-61, `INVSEQ` 60, `TESTOBJ` 58-53; ADDRESS: `ABS_ADDR` 31-16):

| CS | COND_SEQ | TESTOBJ | ABS_ADDR | note |
|---|---|---|---|---|
| 25570 | 0 | - | `17334` | unconditional -> ADR_MESS |
| 25571 | 0 | - | `25` | |
| 25572 | 0 | - | `25573` | |
| **25573** | **1** | **0o11 = `COND,MZRO` (Z from ALU operation)** | **`104`** | **THE BRANCH** |

**`ABS_ADDR` is independently corroborated, not assumed.** Three of the four words carry a value
that matches the OBSERVED execution exactly: `17334` = ADR_MESS (trace step 1), `25573` (step 6),
`104` (step 7). The field definition and the live trace agree.

**The answer to G3's open question:** `OCB_CLNUP` calls `ADR_MESS`, then at **CS 0o25573 branches on
the ALU zero flag to 0o104** - the return path. `ADR_MESS` returned **ZERO**, meaning **"this CPU has
no current message"**, so the routine correctly declines and returns after four microwords. The
body at 0o25574..0o25604 - the `MSGME` clear, and whatever writes `N5STA` - is only entered when
`ADR_MESS` returns NON-ZERO.

**Consequence for the carve: it is NOT disproven.** The `N5STA := 1` claim describes the body, and
this path never enters the body. Probe 3's "evidence of absence" is now correctly scoped: it is
evidence that the body did not run, NOT evidence that the body does something different. **The
earlier framing here overstated it and is corrected by this entry.**

**Remaining G3 question, now the only one:** what makes `ADR_MESS` (0o17334) return non-zero. It
reads no memory on this path, so the answer is in srf/register state. `microcode-5000-def.json` has
no SRF field group, so naming the cell needs either that field added or a register-level trace
through `ADR_MESS`.

**Still do not implement `N5STA := 1` from the carve** - it remains unobserved. But it is now
unobserved-because-unreached, which is a much weaker objection than "reproduction failed".

### G3 probe 7, 2026-07-31 - operand decode. The carve's `DPA := ADR_MESS` is CONFIRMED

Same four words, now decoded through the OPERANDS group (`A_OP` 6 bits, `B_OP`, `DEST`) and the
`IAC`/`MEMORY` groups:

| CS | A_OP | DEST | meaning |
|---|---|---|---|
| 25570 | `A,BM00` | `D,SC14` | set up, then jump to ADR_MESS |
| 17334 (ADR_MESS) | `A,BM12` | `D,RFA1` | computes a register-file address; **one microword**, returns via 0o104 |
| 25571 | `A,BM00` | `D,NONE` | no destination - a condition-setting step |
| **25572** | **`A,RF1`** | **`D,DAC,DPA`** | **`DPA := RF1`** |
| **25573** | **`A,SC13`** | `D,SC12` | **the zero test is on `SC13`** |

**Two things settle here.**

1. **`DPA := current message (ADR_MESS)` is CONFIRMED** at CS 0o25572 - the carve's first step is
   right, byte-decoded. `ADR_MESS` writes `RFA1` (a register-file ADDRESS), and 0o25572 then loads
   `DPA` from `RF1` (the register-file DATA at that address). That is a clean two-step
   address-then-fetch, and it means the carve's description of this routine is not fabricated -
   the parts we can reach check out.
2. **The exit test at 0o25573 is on `SC13`, NOT on `DPA`.** So the routine does not decline because
   the message pointer is null; it declines on a separate scratch value. `SC13` is **not written
   anywhere in these four words**, so it arrives from the CALLER (`OCB_KICK06`).

**Corrects probe 6's wording.** Probe 6 said "`ADR_MESS` returned ZERO, meaning no current message".
That inference does not survive the operand decode - the tested register is `SC13`, and nothing
observed says `ADR_MESS` returned zero. The honest statement is: **the routine exits on `SC13` being
zero, and what `SC13` means is not yet known.**

**Next:** find where `OCB_KICK06` (0o25561..0o25567) sets `SC13`. The trace already has the caller's
path - 0o25565, 0o25505-0o25512, 0o25566, 0o25416, 0o25567, 0o24670 - so this is another
bounded hand-decode, not a search.

### G3 probe 8, 2026-07-31 - `SC13` is general-purpose, and forcing it DOES NOTHING. Contradiction.

Three facts, each independently checked:

1. **`SC13` is not a dedicated flag.** Scanning all 16384 words of the B30 image for `DEST = 0o26`
   finds **600 writers**. It is a general-purpose scratch register.
2. **`OCB_KICK06` never writes it.** Decoding 0o25561..0o25567: DESTs are `SC12`, `SC14`, `SC14`,
   `NONE`, `NONE`, `SC12`, `NONE`. No `SC13`. Nor does `OCB_CLNUP` itself. Only four words execute
   before `OCB_CLNUP` is entered (0o25561-0o25564), and none of them touch it.
3. **The ALU op at 0o25573 is `ALU,A`** - "A OPERAND DIRECT THROUGH THE ALU". `B_OP` is `X1` but is
   ignored by this op. So the zero flag should be exactly `SC13 == 0`.

That predicts a clean experiment: preset `SC13` (WRF index 22) non-zero and the body should run.

```
SC13       reachedBody  N5STA  writes
00000000         False  0002   24
00000001         False  0002   24
00002800         False  0002   24
FFFFFFFF         False  0002   24
```

**It changes NOTHING - not even the write count.** All four runs are byte-identical in effect.

**This is a contradiction, and it is the finding.** The decode says the branch tests `SC13` through
a pass-through ALU op, and nothing between the preset and the test writes `SC13`. Forcing it should
change the outcome. It does not. So at least one of these is false:

- the `A_OP` decode (0o66 = `A,SC13`),
- the WRF index mapping (`Registers.cs` says 20-23 = SC11-SC14, so SC13 = 22),
- the assumption that `ALU_TRUE` is the field in force - **0o25573 also has `COND_ALU` in play, and
  if the ALU takes `ALU_FALSE` instead, the operation is a different one entirely**,
- or our emulator's handling of `COND_ALU` / `ALU_FALSE` / the Z flag on this path.

**The last two are the most likely and the most interesting**, because a `COND_ALU` mishandling
would be an EMULATOR DEFECT, not a carve question - and it would silently affect every conditional
microword, not just this one. That is worth more than G3 itself.

**Do not conclude "the harness enters cold" either** - that was probe 8's hypothesis and this
experiment REFUTED it. Recorded so it is not re-adopted.

Test: `MailboxClrKickTests.OcbClnup_NonZeroSc13_ShowsTheEarlyExitWasAHarnessArtefact` (the name
records the hypothesis; the FINDING line records that it failed).

### G3 probe 9, 2026-07-31 - CONTRADICTION RESOLVED, and it points at an EMULATOR DEFECT

**The `COND_ALU` suspicion from probe 8 is WITHDRAWN.** Checked directly: `COND_ALU = 0` at
0o25573, so `ALU_TRUE` (`ALU,A`) really is in force, and the emulator maps `A_OP 0o66` to
`Wrf[22] = SC13` correctly (`OperandRouter.ReadA`: group 1, `reg = sel - 32 = 22`). Both suspects
cleared - I flagged them and they were wrong.

**Why forcing `SC13` did nothing:** it was being overwritten before the branch read it. Sampling
`SC13` at the moment 0o25573 executes shows `00000000` in every run including the `FFFFFFFF`
preset. Logging only the ticks where `SC13` CHANGES:

```
  preset SC13 = FFFFFFFF (immediately after write)
  CS 14672 WROTE SC13: FFFFFFFF -> 00000800
  CS 14722 WROTE SC13: 00000800 -> 00000000
  CS 14737 WROTE SC13: 00000000 -> 80000000
  CS 25571 WROTE SC13: 80000000 -> 00000000     <-- immediately before the branch
  reached CS 25573 (the branch)
```

Two structural facts fall out, both correcting earlier entries here:

1. **0o25562 is a CALL, not a fall-through.** Execution goes 0o25561 -> 0o25562 -> **0o14666** and
   only later returns to 0o25563/0o25564/0o25570. So `SC13` is not caller state inherited from
   before the kick - it is COMPUTED by the subroutine at 0o14666. Probe 8's "nothing writes SC13
   between entry and the test" was wrong because it only looked at the straight-line words.
2. **The word at 0o25571 zeroes `SC13`, one word before 0o25573 tests it.** That makes the early
   exit UNCONDITIONAL in effect: `OCB_CLNUP` can never enter its body, for ANY mailbox state.

**And 0o25571 should not be writing anything.** Decoded from its microword
`50080000000180180000000000150000`: `DEST = 0o30` (= 24 = `D,NONE`), `OR_ENABLE = 0`, `ORCON = 0`.
The emulator's own `WriteDest` treats destination 24 as a no-op and returns. Emulator and
`microcode-5000-def.json` agree that `DEST` is bits 83-76, so this is not a field-position
disagreement.

**So something OTHER than the DEST field is writing `SC13` during that word.** [OPEN]

Two readings, and this probe does NOT choose:

- **(a) EMULATOR DEFECT** - an unintended write to `Wrf[22]` somewhere in `Tick()`. If so it is not
  a G3 issue at all: it would silently corrupt a scratch register on any word of this shape, and it
  happens to land one word before a conditional that reads it. **This is the higher-value
  possibility and should be chased first.**
- **(b) Tick attribution is off by one** - the change is committed during 0o25571's tick but
  originates in a deferred write from an earlier word (the codebase already models exactly this for
  `LC`, see `Registers.LcLoadPipe` and the deferral note in `Registers.cs`). Then the real writer is
  0o25570 or earlier and the DEST decode of THAT word is what to check.

**Next:** read `CpuND5000.Tick()` for writes to `Wrf` outside `WriteDest`, and for any deferred
write pipeline that could commit a cycle late. That distinguishes (a) from (b) directly.

### G3 probe 10 - RETRACTED IN FULL by probe 11. There is NO emulator anomaly.

> **DO NOT ACT ON THE SECTION BELOW.** Its conclusion - "a register no code path should write is
> being written" - was **my own measurement error**, not a defect. `State.Mpc` is updated BEFORE a
> word executes, so the address my probe logged was the NEXT word, not the writer. The real writer
> is **CS 0o16554**, a word carrying a perfectly legitimate `DEST = SC13`. Kept visible with this
> banner because "suspected emulator defect" is exactly the kind of claim that gets re-adopted if it
> is quietly deleted. Full correction in probe 11 below.

### G3 probe 10, 2026-07-31 - reading (b) is RULED OUT. This is an emulator anomaly. [OPEN]

**The decode is not in dispute.** The emulator's own `Microword` agrees with the hand-decode of the
raw image, field for field:

```
  emulator decode CS 25570: AOp=0   Dest=27
  emulator decode CS 25571: AOp=0   Dest=30      <- 0o30 = 24 = D,NONE
  emulator decode CS 25572: AOp=214 Dest=350
  emulator decode CS 25573: AOp=66  Dest=25
```

So probe 9's reading (b) - "my hand-decode is wrong / field positions disagree" - **is ruled out**.
Both decoders say the word that writes `SC13` has no destination.

**Static inspection cannot explain the write:**

- `OperandRouter.WriteDest` is called from exactly ONE place (`CpuND5000.cs:1890`), and for
  `sel = 24` it returns immediately without writing.
- The only other `Wrf` writes in the CPU are the AAP register-pair path (`Wrf[4+rin]` and
  `Wrf[12+rin]`, `rin` 0-3), which can only reach indices 4-7 and 12-15. **`SC13` is index 22 and is
  unreachable from there.**
- The one cross-word mechanism that could make a word write somewhere unexpected is
  `_pendingOrconD` / `ResolveOrDestination`, and it is gated on `word.Dest == 31`. 0o25571 has
  `Dest = 24`.

**So: a register that no code path should write is being written, deterministically, one word before
a conditional that reads it.** That is an emulator anomaly, not a carve question. Status **[OPEN]**
- named, reproducible, and NOT explained.

**Deliberately not guessed at.** Three hypotheses have already died this session (harness-enters-cold,
`COND_ALU`, decode-disagreement); a fourth invented from static reading would be worth nothing.

**What would settle it in one run:** a conditional breakpoint or a temporary write-barrier on
`Regs.Wrf[22]` inside `CpuND5000.Tick()`, reporting the call stack on change. That requires editing
`src/CpuND5000.cs`, which is currently carrying someone else's uncommitted work - so it was NOT
touched. This is the first thing to do once that file is free.

**Impact if it is a defect:** `OCB_CLNUP` can never execute its body under our emulator, so kicks
4/5/6 can never requeue in-flight work, and G3 cannot be closed by observation at all until this is
fixed. It would also affect any microword that reads a scratch register written this way - which is
far wider than this routine.

### G3 probe 11, 2026-07-31 - the "anomaly" was MY BUG. Retracting probe 10.

Put a write barrier inside the CPU (temporary, since reverted - `src` is untouched) reporting the CS
address whenever a word writes `Wrf[22]`:

```
[SC13-BARRIER] CS 16554 writes SC13 := 00000000
```

**The writer is CS 0o16554, and its `DEST` field really is `SC13` (0o26).** Nothing improper
happened. There is no unexplained write, no defect, and nothing to fix in the emulator.

**The error was mine, and it is worth naming precisely.** My probe logged `State.Mpc` before calling
`Tick()` and labelled it "the word that wrote". But **`State.Mpc` is updated BEFORE the word
executes**, so that address is the NEXT word, not the executing one. Every "CS X WROTE SC13" line in
probes 9 and 10 is off by one word - which is how a routine word (0o25571, `D,NONE`) got blamed for
a write made elsewhere, and how that turned into a reported "emulator anomaly".

**Three consequences:**

1. **Probe 10 is retracted in full** (banner added above; text kept so the wrong claim is not
   re-adopted). The `COND_ALU` suspicion of probe 8 was already withdrawn in probe 9. **Two
   suspected emulator defects were raised this session and BOTH were my own measurement errors.**
2. **The G3 mechanism is now plain and involves no defect:** `SC13` is computed by the subroutine
   chain called from 0o25562, last written at **0o16554** with the value **0**, and 0o25573 then
   branches on that zero straight to the return. `OCB_CLNUP` declines because the state it is asked
   about genuinely says "nothing to clean up".
3. **The test's log wording is fixed** so it can never mislabel again - it now prints "Mpc now X =
   NEXT word, NOT the writer" and carries a comment explaining what the false report cost.

**Method lesson, recorded because it burned three probes:** in this emulator, do not read
`State.Mpc` as "the instruction that just ran". Attribute writes with an in-CPU barrier, not by
sampling the program counter around `Tick()`.

**G3 status:** the exit is fully explained and correct. What remains is unchanged and unblocked -
to see the body run, `SC13` must be non-zero at 0o25573, which means setting up whatever 0o16554's
subroutine reads. That is real mailbox state, not a harness trick.

### G3 probe 12, 2026-07-31 - `0o16554` is `SCAN_ACCP`. Delivering an AOB word does NOT open the gate.

**The writer is identified by an exact label match**, not by proximity: `MICRO-5800-B30.LABE` lists
`SCAN_ACCP 016554*`. So the value 0o25573 branches on is **the ACCP scan result**, and the
neighbouring labels (`SCAN_ACCP1` 0o16560, `SCAN_ACCP2` 0o16562, `SCAN_ACCP3` 0o16564) confirm the
routine starts exactly there.

That gives a clean hypothesis: our `StubAccpController` presents an idle ACCP, `SCAN_ACCP` writes
`SC13 := 0`, and `OCB_CLNUP` returns because there is nothing to clean up.

Tested by delivering an asynchronous word into AOB with ATRAP asserted - the shape of a forwarded
octobus kick - and re-running:

```
accpDelivers  reachedBody  SC13@branch  N5STA
       False        False     00000000  0002
        True        False     00000000  0002
```

**Negative. `SC13` is still zero at the branch and the body is still not reached.** So delivering
into AOB is not what `SCAN_ACCP` inspects. The hypothesis is NOT confirmed, and is recorded as such
rather than being quietly upgraded because the label match looked convincing.

**What this does and does not establish:**

- **Established:** the gate value is produced by `SCAN_ACCP`, byte-verified by label.
- **NOT established:** that ACCP activity of any kind flips it. One delivery shape was tried and it
  did nothing.

**Next:** `SCAN_ACCP` most likely tests `AFLAG` bits rather than AOB occupancy (AFLAG is the ACCP
status register, and this file already carries "AFLAG bits 7/8 unknown" as an open item). Decode
0o16554-0o16564 for which flag it reads, then drive that bit rather than guessing at delivery
shapes. Test: `MailboxClrKickTests.OcbClnup_WithBusyAccp_ChecksWhetherScanAccpIsTheGate`.

### G3 probe 13, 2026-07-31 - `SC13 := AFLAG`, and our AFLAG CANNOT satisfy the scan. [DEFECT]

`SCAN_ACCP` decoded (0o16554 onward):

| CS | A operand | DEST | test | branch |
|---|---|---|---|---|
| 16554 | **`A,SPEC,AFLAG`** | **`D,SC13`** | - | -> 16555 |
| 16555 | `A,BM13` (mask **bit 11**) | NONE | - | -> 16556 |
| 16556 | `A,BM14` (mask **bit 12**) | NONE | `COND,MZRO` | -> 16560 |
| 16560 | `A,BM05` (mask **bit 5**) | NONE | `COND,MZRO` | -> 16562 |
| 16562 | `A,BM06` (mask **bit 6**) | NONE | `COND,MZRO` | -> 16564 |
| 16564 | `A,BM00` | NONE | `COND,MZRO` | -> 104 (return) |

**So `SC13` IS the AFLAG word**, byte-verified: 0o16554 reads `A,SPEC,AFLAG` straight into `SC13`.
That fully explains `OCB_CLNUP`: 0o25573 tests `SC13` for zero, so **an ACCP with AFLAG == 0 makes
the routine return immediately.** The whole chain from G3's symptom to its cause is now closed.

**And here is a REAL emulator gap - visible in the source, not inferred from a measurement.**
`AccessModule.ReadAflag()` is:

```csharp
return ((uint)AobFull << AobfBit) | ((uint)AibFull << AibfBit);   // AOBF bit 9, AIBF bit 10
```

**It can only ever produce bits 9 and 10.** `SCAN_ACCP` tests bits **5, 6, 11 and 12**. Those four
bit tests can NEVER fire in our emulator, whatever the ACCP stub does. That is why probe 12's
delivered AOB word changed nothing: it sets bit 9, which `SCAN_ACCP` does not look at.

**This is a genuine defect and this time it is not a measurement artefact** - it is a two-line
method that structurally cannot produce the bits the microcode reads. Contrast the two false alarms
earlier in this file (`COND_ALU`, the "unexplained SC13 write"), both of which were my own errors:
here the evidence is the source text itself plus a byte-verified decode of the reader.

**Scope beyond G3:** `SCAN_ACCP` is called from the `OCB_WAITSEX` spin as well (catalog correction
3 - "SCAN_ACCP each pass"), so every path that polls the ACCP is scanning a status word that can
never report four of its conditions.

**This also extends the existing "AFLAG bits 7/8 unknown" open item** - the unmodelled set is wider
than bits 7/8. Known real bits now: 9 AOBF, 10 AIBF (modelled); 5, 6, 11, 12 read by `SCAN_ACCP`
(unmodelled); 7, 8 previously flagged unknown.

**Next:** find what the real ACCP asserts in AFLAG bits 5, 6, 11, 12 before implementing anything -
`ND-05.020.01` section 5.1.3 is the AFLAG reference already cited in `AccessModule`, and the ACCP
firmware carve (`ACCP-COMPLETE-REFERENCE.md`) may name them from the other side. **Do not invent
bit meanings to make the scan pass.**

### G3 probe 14, 2026-07-31 - AFLAG dispatch bits IMPLEMENTED from the verified map. Gate still shut.

**The bit meanings did not need inventing - they were already carved.**
`ACCP-COMPLETE-REFERENCE.md` "AFLAG bit map [V, but see the warning]" has all of them, and it
independently confirms this session's decode (it states the same octal BM naming: BM05 = bit 5,
BM13 = bit 11, BM14 = bit 12):

| Bit | Meaning | Confidence |
|---|---|---|
| 5 | async-trap word pending (`TRAP_OCBA` / `TRAP_ATRP`) | [V] |
| 6 | other trap (`TRAP_OTRP`, NOTREC 210) | [V] |
| 7 | data-fault indication | **[OPEN]** |
| 8 | instruction-fault indication | **[OPEN]** |
| 9 | AOB has data | [V] |
| 10 | AIB busy | [V] |
| 11 | power-fail warning (`TRAP_PWF`) | [V] |
| 12 | **OCB kick / message pending** (`TRAP_OCBAK` / `TRAP_OMESS`) | [V] |

**Implemented** in `AccessModule`: bits 5, 6, 11, 12 are now composed by `ReadAflag` alongside the
existing 9 and 10, with the provenance and the OCB_CLNUP consequence in the XML docs. **Bits 7 and
8 were deliberately left out** - the source marks them [OPEN], never re-verified after an
off-by-one correction shifted every dispatch bit. Adding them to make a poll succeed is exactly the
failure mode this interface already suffered.

This is a **real fix regardless of G3**: `SCAN_ACCP` runs on every pass of the `OCB_WAITSEX` spin
too, so until now every ACCP poll in the emulator read a status word that could not report four of
its conditions.

**But the gate is still shut, and this is NOT explained:**

```
accpDelivers  reachedBody  SC13@branch  N5STA
       False        False     00000000  0002
  AFLAG after setting OcbPending = 00001000 (expect bit 12 = 0x1000)
        True        False     00000000  0002
```

`ReadAflag()` demonstrably returns `0x1000` (checked in-run, so the change is live and not a stale
build), yet `SC13` is still `0` when 0o25573 executes. Given that an in-CPU barrier showed
`SCAN_ACCP` is the ONLY writer of `SC13` in this run, `SC13` should hold `0x1000`.

**Not guessed at.** Plausible directions, none tested: the busy run may take a different route
(dispatching `TRAP_OCBAK`) and arrive at 0o25573 having re-scanned an AFLAG that was cleared by the
dispatch; or `SCAN_ACCP` may run before the flag is visible on that path. **The next step is to log
`SC13` changes and the CS path for the busy run specifically** - attributing writes with an in-CPU
barrier, per the method lesson in probe 11, not by sampling `State.Mpc`.

Regression check after the `src` change: mailbox + ACCP + octobus fixtures **50/50 green**.

**Note on the harness:** this iteration was blocked for a while by an unrelated uncommitted edit to
`src/CpuND5000.cs` (a `_aapProductForQ` field never assigned, CS0649-as-error). Not ours; left
alone; it compiles again now.

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

### G7 - NOT A GAP. Withdrawn 2026-07-30, my claim was wrong

`X5ACT` IS re-armed: `OctobusND5000Station.cs:1163`,
`WriteNd100Word(_extBlockBase + 5 * 2, 1); // re-arm (IDLE_2 writes 1 [V])`, covered by the
passing test `OctobusMailboxO1Tests.IdlePath_ReArmsX5ActToOne_BeforeConsuming`. The real
microcode agrees - running the B30 IDLE loop leaves `X5ACT=0001`.

**How I got it wrong:** I searched for the byte offset `0x0A` and concluded "nothing writes it".
The code writes `5 * 2`. A grep for one spelling of an address is not evidence that nothing
touches it - check the symbolic form too, or assert on the VALUE instead of searching for the
write.

Original (incorrect) text follows for the record.

### G7 - `X5ACT` is never re-armed  **[WITHDRAWN - see above]**

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

**Done 2026-07-30: G1, G4, G6** (diagnostic half), **G10**, **G2**, plus the harness-reproducibility
fix below. **G7 withdrawn** - it was never a gap.

- **G10** (top item): `_accpIdle` was cleared only by `ContinueAccp` and `ResetStation`, never by
  `STAMIC0`/`CONTMIC`/`RESTMIC`. SINTRAN sends 244B TERMINATE as a NORMAL bring-up step (measured:
  after 3 commands, all answered; 0 of 149 unanswered in a whole run), then restarts the
  microprogram - and the flag stuck, so every kick was dropped for the rest of the session.
  Verified: `k3=1`, `X5CLR=0000`, `X5CCL=0001`, `accpIdle=False`, full ladder green.
- **G2**: kick 6 writes `X5PRO := -1`, so `TER51` completes instead of `ESPTIMOUT`.

**0. G10 - DONE.** Was listed here as "first, until understood". It is understood and fixed: the
244B TERMINATE is a normal bring-up step, and the defect was `_accpIdle` never being cleared on the
microprogram-restart paths. See the corrected G10 entry above.

Remaining, in order:

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
