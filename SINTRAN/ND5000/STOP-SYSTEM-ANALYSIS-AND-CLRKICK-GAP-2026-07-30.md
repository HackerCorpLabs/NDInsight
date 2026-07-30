# STOP-SYSTEM: what it actually does, and the CLRKICK gap on the octobus

**Date**: 2026-07-30
**Scope**: SINTRAN III L `stop-system` on the ND-100 with an ND-5000 (SAMSON) attached over the
OCTOBUS. Written because the octobus boot harness reported `stop-system=STALL` on every run
since it was written, and the swapper work (see `../ND500/OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md`
section 7.7) left it as the last open item.
**Status**: the STALL is explained and fixed. One real correctness gap on our side is identified
and NOT yet implemented (section 5).

---

## 1. Headline

Two separate things were tangled together under one `STALL` flag:

| # | Finding | Kind |
|---|---|---|
| 1 | The harness waited for three strings SINTRAN never prints. `stop-system` could never be reported OK. | **Harness bug. FIXED.** |
| 2 | Our octobus station never executes the `X5CLR` clear-functions nor acknowledges them, so `ST0PSYS`'s wait loop always times out into `ERRFATAL`. | **Real gap. NOT yet implemented** - see section 5. |

Finding 1 fully accounts for the `STALL` flag. Finding 2 does NOT cause a hang (the wait loop is
bounded - section 3), so it was invisible behind finding 1.

### 1.1 Measured, live, one run - both findings confirmed

With the observable corrected, the full octobus ladder is green:

```
OUTCOME: ENTER=OK login=OK nd-500=OK status=OK start-swapper=OK list=OK stop-system=OK
```

and the new mailbox dump confirms finding 2 at the same time:

```
----- STOP-SYSTEM mailbox (before-stop-system) ext=0x428900 runState=RUN
      X5CPU=0001 X5ACT=0001 X5PRO=FFFF X5CLR=0000 X5CCL=0000 -----
----- stop-system console output -----
                                            <-- EMPTY. stop-system prints nothing, as designed.
----- STOP-SYSTEM mailbox (after-stop-system)  ext=0x428900 runState=OPCOM
      X5CPU=0001 X5ACT=0001 X5PRO=FFFF X5CLR=003F X5CCL=0000 -----
```

Read it line by line - every element of the section 2 analysis is visible:

- `runState` goes **RUN -> OPCOM**: the ND-100 really halts. `stop-system` works.
- The console window between the two dumps is **empty**, which is the direct proof that the old
  text-matching check could never have passed.
- `X5CPU=0001` = `MPACTIVE`, so the section 2 loop 2 activity test **did** match and the machine
  really did enter the wait loop.
- `X5CLR` goes **0x0000 -> 0x003F**. `0x3F` = `0o77`, exactly what loop 1 wrote, still sitting
  there afterwards. Nothing consumed it. This is finding 2 measured rather than inferred: the
  wait loop spun its 1000 iterations and fell into `ERRFATAL`.

---

## 2. What `stop-system` actually does

The monitor-level continuation of the command is `ST0PSYS`, `MP-P2-N500.NPL:3759` (octal
`147433`). Header comment: *"TERMINATE ALL ND-500S (IF ANY) AND SIMULATE POWER FAILURE"*.

```npl
ST0PSYS: *IOF                                          % interrupts off
   IF PN500D><0 AND 5MSINIT BIT 5INBUF THEN            % ND-500 present AND mailbox initialised
     CALL SLOCK; GO BYPASS                             % take the ND-500 semaphore
     "S5CPUDF"=:B
     DO WHILE B<<="E5CPUDF"                            % ---- loop 1: ask every CPU to clear ----
        IF CPUAVAILABLE BIT 5ALIVE AND A/\5CPUTYPE=SAMSON THEN
           T:=5MBBANK; X:=MAILINK; *AAX X5CPU; LDATX
           IF A><0 THEN                                % CPU active?
              77; *AAX X5CLR-X5CPU; STATX              % X5CLR := 0o77
              CLRKICK; CALL XKICK500                   % kick 3
           FI
        FI
        B+5CPUDFSZ
     OD
     "S5CPUDF"=:B
     DO WHILE B<<="E5CPUDF"                            % ---- loop 2: wait for them ----
        IF CPUAVAILABLE BIT 5ALIVE AND A/\5CPUTYPE=SAMSON THEN
           T:=5MBBANK; X:=MAILINK; *AAX X5CPU; LDATX
           IF A-MPACTIVE=0 THEN                        % CPU active?
              1000-=:L                                 % bounded: -1000 .. 0
              *AAX X5CLR-X5CPU
              DO  *LDATX
                  WHILE A><0 AND L><0                  % Functions executed ?
                  L+1
              OD
              IF L=0 THEN CALL ERRFATAL FI             % gave up
           FI
        FI
        B+5CPUDFSZ
     OD
     FOR X:=0 TO 10000 DO D+1; A:=A+C; OD              % delay reset-cpu
BYPASS: "RS5CPU"; *IRW LV12B DP                        % "reset cpu" to the ACCP
     LV12; *MST PID
     *ION
     FOR X:=0 TO 10000 DO D+1; A:=A+C; OD              % delay stop-system
     CALL SUNLOCK
   FI
   "PPWFAIL"; *IRW LV14B DP                            % continue in "power fail" on level 14
   LV14;      *MST PID
   *ION
   GO STUPR
```

So the sequence is, in order:

1. **Interrupts off**, then gate on `PN500D != 0 AND 5MSINIT BIT 5INBUF` - an ND-500/5000 is
   configured AND its mailbox has been initialised. If not, everything below is skipped and it
   goes straight to the power-fail step.
2. **Take the ND-500 semaphore** (`SLOCK`).
3. **Loop 1 - request the clear.** For each CPU datafield between `S5CPUDF` and `E5CPUDF` whose
   `CPUAVAILABLE` has `5ALIVE` set and whose `5CPUTYPE` is `SAMSON`: read `X5CPU` from the
   per-CPU mailbox extension block; if it is non-zero, write `0o77` into `X5CLR` and send kick
   `CLRKICK` (=3) via `XKICK500`. The NPL comment spells out what `0o77` means:
   *"Clear data tsb+cache+dump+forget process"*.
4. **Loop 2 - wait for the clear.** Walk the same table again. This time the activity test is
   `X5CPU = MPACTIVE` (`MPACT=000001`, so **1**), not merely non-zero. If it matches, poll
   `X5CLR` up to 1000 times waiting for it to read **zero** - the NPL comment is
   *"Functions executed ?"*. If the counter runs out, `CALL ERRFATAL`.
5. **Reset the CPU.** After a spin delay, send `RS5CPU` to the ACCP on level 12.
6. **Simulate power failure.** `PPWFAIL` on level 14, then `GO STUPR`. The power-fail handler is
   what actually stops the processor. `PH-P2-RESTART.NPL:349` carries the matching note:
   *"CAN ALSO BE CALLED BY THE STOP-SYSTEM COMMAND"*.

**Therefore `stop-system` produces no terminating console message by design.** Its successful
outcome is the ND-100 halting. On real hardware that is the front-panel RUN indicator going out.

### 2.1 Mailbox words involved

Per-CPU extension block, word offsets from the L07 symbol tables
(`SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT`); byte offset = word * 2:

| Symbol | Word | Byte | Role in stop-system |
|---|---|---|---|
| `X5CPU` | 4 | 0x08 | activity test; loop 1 wants non-zero, loop 2 wants exactly `MPACTIVE`=1 |
| `X5ACT` | 5 | 0x0A | not used by stop-system (that is the activation work flag) |
| `X5PRO` | 6 | 0x0C | not used by stop-system |
| `X5CLR` | 0o10 = 8 | 0x10 | **the clear-function mask.** SINTRAN writes `0o77`, then polls for 0 |
| `X5CCL` | 0o11 = 9 | 0x12 | cache-clear counter, read/compared elsewhere |

Constants: `MPACT=000001`, `5ALIV=000015`, `STUPR=032034`.

---

## 3. Why the gap in section 5 does NOT hang stop-system

Loop 2's wait is **bounded**: `L` starts at `-1000` and increments, and the loop condition is
`A><0 AND L><0`. An ND-500 that never clears `X5CLR` makes the loop run 1000 iterations and exit
with `L=0`, which calls `ERRFATAL` - it does not spin forever.

This matters for interpreting the evidence. It means the historical `stop-system=STALL` could
never have been caused by a missing `X5CLR` acknowledgement, and any theory that reached for the
clear-function handshake to explain the STALL was reaching past a bounded loop. The STALL has a
completely different cause (section 4).

---

## 4. The harness bug (FIXED)

Both boot harnesses detected a completed `stop-system` like this:

```csharp
stopped = RunUntilAny(atStop, 30_000,
    "RUN indicator is off", "CPU stopped", "WAIT instruction");
```

`RunUntilAny` scans `ConsoleText()`, and `_console` is appended to **only** by `OnConsoleChar`,
the emulated terminal's character callback. Those three strings are not SINTRAN output at all -
they are the wording of a **`Logger.Dbg`** line inside our own ND-100 `WAIT` handler:

```
Emulated.HW/ND/CPU/ND100/Instructions.SystemControlInstructions.cs:391
  Logger.Dbg($"WAIT instruction ran with IONI off. CPU stopped after the WAIT instruction. RUN indicator is off.");
```

`Logger` output never reaches `_console`, so the match could not succeed on any run, ever. On top
of that the check is self-defeating by construction: per section 2 a **successful** `stop-system`
halts the ND-100, so the machine deliberately prints nothing more. Waiting for console output to
prove the console went silent cannot work.

### 4.1 The correct observable

Our `WAIT` handler models the halt as a run-state change:

```csharp
if (!regs.IONI) { ...; regs.runState = Registers.STATE.OPCOM; return; }
```

`OPCOM` is the front-panel state in which RUN is off. So "stopped cleanly" is
`runState != STATE.RUN`, and the fix is a run-state poll rather than a text match:

```csharp
private int RunUntilStopped(int timeoutMs)   // 0 = stopped, -1 = timeout
{
    timeoutMs = Scaled(timeoutMs);
    var sw = System.Diagnostics.Stopwatch.StartNew();
    while (sw.ElapsedMilliseconds < timeoutMs)
    {
        Pump();
        var st = _machine?.Dbg_Cpu?.regs.runState;
        if (st.HasValue && st.Value != Emulated.HW.ND.CPU.ND100.Registers.STATE.RUN) return 0;
        Thread.Sleep(1);
    }
    return -1;
}
```

Applied in both harnesses:

- `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd5000OctobusBootHarnessTests.cs`
  (octobus) - plus a new `DumpStopSystemMailbox(tag)` that prints `runState` and
  `X5CPU/X5ACT/X5PRO/X5CLR/X5CCL` before and after the command, so a stop that does not complete
  names its own reason instead of needing a fresh investigation.
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs`
  (3022) - same defect, and there it sits behind a hard
  `Assert.That(stopped, Is.GreaterThanOrEqualTo(0), ...)`. That whole fixture is `[Explicit]`,
  which is the only reason a permanently-false assert stayed hidden.

### 4.2 Lesson

This is the **third** false `STALL` in this one investigation (the first two are recorded in
memory `harness-stall-is-wallclock` and section 7.7 of the swapper handoff). The first two were
wall-clock timeouts; this one is different and worse - the marker was unmatchable in principle.

**Rule: a success marker must be something the machine actually emits on the channel being
scanned.** Before treating a harness flag as evidence about the machine, confirm the marker can
ever fire - grep for the string and check which sink produces it. A marker that comes from
`Logger`, from a comment, or from a manual's prose is not an observable.

---

## 5. The real gap: CLRKICK / X5CLR is not implemented (OPEN)

Measured: `X5CLR` reads `0x003F` after `stop-system` returns (section 1.1). SINTRAN wrote `0o77`
and nothing ever consumed it.

`X5CLR` appears in RetroCore **only in comments**. Nothing reads it, executes the coded clear
functions, or writes it back:

```
Emulated.HW/ND/CPU/ND500/Servicer/IServicerHost.cs:116   (doc comment)
Emulated.HW/ND/CPU/NDBUS/OctobusND5000Station.cs:628     (doc comment)
```

And in the station's kick handler, only kick 1 does anything
(`OctobusND5000Station.cs:1330-1367`): every kick is loaded into the AOB and raises
`KickReceived`, but only `kickNumber == 1` (`N100KICK`, the preempt doorbell) drives
`WalkQueue()`. **Kick 3 (`CLRKICK`) is a no-op.** Consequently `X5CLR` keeps whatever SINTRAN
wrote, loop 2 in section 2 spins 1000 times and falls into `ERRFATAL`.

### 5.1 What the real ND-5000 microcode does

From `ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` (correction 3, and the `OCB_DEC_K` table): kick
3 dispatches to **`OCB_KICK03`** at microcode address `025522`, which is *the CLRKICK cache-clear
protocol*:

1. read `X5CLR` (extension block word `0o10`, ORCON `0x10`);
2. `MSG_CLEAR_1` - cache clear by mask;
3. write `X5CLR` back with bit 15 cleared (acknowledge);
4. if the original bit 15 was set, spin in `OCB_WAITSEX` on **global header** word `0o24`
   (byte 0x28) until it reads zero, calling `SCAN_ACCP` each pass;
5. resume `EXECUTE` if a process is loaded, else fall through to `KICK06`.

### 5.2 An unresolved contradiction - do not implement past this

Step 3 as worded ("write `X5CLR` back with bit 15 cleared") does **not** satisfy SINTRAN.
`ST0PSYS` writes `0o77` = `0x003F`, which already has bit 15 clear; clearing bit 15 of `0x003F`
leaves `0x003F`, and loop 2 polls for **zero**. Under that literal reading the poll could never
succeed on real hardware either, which cannot be right.

**I do not know which side of this is imprecise.** The plausible readings are that the microcode
clears the mask bits it has executed (so a fully-executed `0o77` leaves 0), or that the catalog's
wording is a compression of the actual listing. The catalog is a carve summary, and its own
item 3 is already a *correction* of an earlier wrong description of the same routine. Resolving
it needs the B30 listing for `025522`, which is not in either repo. **Do not guess this into
code.**

What IS unambiguous is the requirement from SINTRAN's side, and it is version-independent:

> After `CLRKICK`, the ND-5000 must make `X5CLR` read **zero** within 1000 ND-100 poll
> iterations, or SINTRAN calls `ERRFATAL`.

That is the contract our station has to satisfy, and it can be implemented from the SINTRAN side
alone without depending on the ambiguity above. The bit-15 / `OCB_WAITSEX` branch is a separate
matter: `ST0PSYS`'s `0o77` never sets bit 15, so that branch is unreachable from stop-system. It
may be reachable from the swapper's path (next section), where the mask is runtime data.

### 5.3 The other writer of X5CLR - the swapper's clear-function message

`MP-P2-N500.NPL:1222-1235`, label `LMPCLR`, comment **"Clear-function; ND5000 only"** - so this
path is octobus-specific by construction:

```npl
LMPCLR:
   T:=5MBBANK; X:=SWMSG; *AAX 5DP2; LDATX      % mask arrives in the swapper message, word 5DP2
   A=:SWPCLRMASK
   CALL SLOCK; 0/\0
   A:="S5CPUDF":=:B=:SWCPU
   DO WHILE B<<="E5CPUDF"
      IF B><SWCPU AND CPUAVAILABLE BIT 5ALIVE
      AND C5STAT NBIT BHPFAIL AND SPREF=0 THEN
         T:=5MBBANK; X:=MAILINK; *AAX X5CPU; LDATX; AAX -X5CPU
         IF A=MPACTIVE THEN
            SWPCLRMASK; T:=5MBBANK; *AAX X5CLR; STATX
            CLRKICK; CALL XKICK500
         FI
      FI
      B+5CPUDFSZ
   OD
   CALL SUNLOCK
```

Two things worth noting:

- The mask is **not** a constant here - it is `SWPCLRMASK`, taken from the swapper's own message
  at `SWMSG + 5DP2`. So bit 15 (the `OCB_WAITSEX` trigger) genuinely can be set on this path.
  Per the no-hardcoding rule, our implementation must read the mask from `X5CLR`, never assume
  `0o77`.
- `LMPCLR` **does not poll** `X5CLR`. It writes, kicks, and answers OK
  (`OKMONICO`/`XACTRDY`). Only `ST0PSYS` waits. So a missing clear handshake is silent on the
  swapper path and only becomes visible at shutdown - which is exactly how it stayed hidden.

### 5.4 Recommended implementation shape (not yet written)

In `OctobusND5000Station.cs`, alongside the existing `kickNumber == 1` case:

- on kick 3, read the mask from `ExtBlockBase + 0x10`;
- perform the modelled clear (our cache/TSB models, per mask bit - the `0o77` bits are
  "data tsb + cache + dump + forget process" per the NPL comment);
- write `X5CLR := 0`, satisfying the section 5.2 contract;
- leave the `OCB_WAITSEX` global-header-word-`0o24` spin **unimplemented with an explicit
  comment**, since no verified SINTRAN write we have sets bit 15 and the semantics are open.

The station already exposes `ExtBlockBase` publicly, so no new plumbing is needed.

---

## 6. Files touched

| File | Change |
|---|---|
| `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd5000OctobusBootHarnessTests.cs` | `RunUntilStopped` + `DumpStopSystemMailbox`; stop-system now uses the run-state observable |
| `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs` | same `RunUntilStopped` fix (this fixture is `[Explicit]`) |
| `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` | this document |

## 7. Sources

- `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL:3759` - `ST0PSYS`
- `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL:1222` - `LMPCLR`
- `SINTRAN/NPL-SOURCE/NPL/PH-P2-RESTART.NPL:102,349` - `X5CLR` zeroing on restart; stop-system note
- `SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT` - `X5CPU/X5ACT/X5PRO/X5CLR/X5CCL`, `MPACT`, `5ALIV`
- `SINTRAN/ND5000/ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` - `OCB_DEC_K`, `OCB_KICK03` (025522)
- `SINTRAN/ND5000/CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md` - extension-block layout table
- `SINTRAN/ND500/OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md` section 7.7 - prior context
