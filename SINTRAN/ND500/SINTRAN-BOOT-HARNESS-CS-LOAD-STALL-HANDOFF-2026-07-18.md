# SINTRAN boot harness - "status" CS-load stall: architect hand-off (2026-07-18)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\SINTRAN-BOOT-HARNESS-CS-LOAD-STALL-HANDOFF-2026-07-18.md`
**Evidence grade: OBSERVED** (RetroCore emulator, real SINTRAN III L + nd-500-mon J04).

## What was built (works)

Automated headless harness:
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs`
([Explicit], namespace Emulated.Tests.ND100). Boots SINTRAN III L from the BIGDISK0-L SMD
image (raw block-0), auto-logs-in as SYSTEM, enters `@nd-500`, and drives `status`. Boot
milestone PASSES (`SINTRAN III RUNNING`). Reproduces the interactive transcript faithfully up to
`N500: status` -> `> Loading Control Store`.

Build recipe = the .ini exactly: direct `new ND100Machine(MachineEnvironment.Test)` (defaults
already = ND100CX/MMS2/FPP48/2MB); `CreateAndRegisterDevice` FX0/SMD0/HD0/ND500-0/TERM5-7;
console device 1 auto-created; mount FROM DATA (non-destructive scratchpad copy);
`LoadBootStrapperFromDevice("BD0")`. Pump = `machine.Run(pc,200_000)` batches, poll console for
markers (wall-clock timeout).

## The stall (Bug B - the real blocker)

`status` stalls at `> Loading Control Store`; never reaches `> Loading Swapper`. The harness
`RunUntilAnyInstrumented` traces STATUS + 3022.L12 + ND100.aggL12 + MICFU per pump. Timeline:
- 0..~40 s: STATUS oscillates `0x0020` (locked) <-> `0x0000` (idle), MICFU=null throughout.
- ~+40 s: STATUS = `0x0200` (= the CS-load doc's "stopped for verify" signature), then
  `0x0021` (int-en+locked) then oscillates `0x0020` <-> `0x0008` (finished) indefinitely.
- Never MICRO-STARTs, never prints "Error in loading Control Store", never sends a 13B message.

So it is stuck in the **CS readback/verify phase** (ND500-CS-LOAD-TRACE-FINDINGS section 2),
BEFORE MICRO-START and the 13B memory test (which is why MICFU stays null - the CS load is a
register/TAG-level op, not a mailbox message).

### KEY FACT: same code works in the app, stalls in the harness
Ronny confirmed (this session, live) that `status` completes in RetroCore.exe TODAY - flag dump
and all (`Loading Control Store` -> `Loading Swapper` -> ZERO/CARRY/SIGN/FLAG/OVERFLOW). Same
current working tree. So this is NOT a code regression and NOT a protocol gap - it is a
**harness-vs-app machine-driving difference** in the CS verify phase.

### Hypotheses RULED OUT (with evidence)
1. Interrupt-delivery / level-12: PROVEN GOOD. Caught STATUS=0x0021 with 3022.L12=1 AND
   ND100.aggL12=1 together (ND100Machine.cs:929 feeds interruptBits&0x1000 to the CPU).
2. CPU spin / master-clear re-park bug (Bug A, below): emulating its fix in the harness
   (repark-on-spin: park CpuND500 to WAIT when it spins at PC=0) did NOT resolve the stall.
3. CS-load PROTOCOL code: the existing unit tests pass (LoadControlStore_FullCycle_...,
   LoadControlStore_VerifyReadback_ReturnsWrittenParts - 3/3, 373 ms).
4. CPU model: stalls with BOTH the default SimulatedND500 (no run thread) AND a real CpuND500
   + run thread. Independent of the CPU.
5. Code regression: app works with the same code.

### What is LEFT (for the architect - needs app run-model + frozen NDBusND500IF internals)
The verify-phase readback (`ND500-CS-LOAD-TRACE-FINDINGS` section 2: DUEN/EDUTEN/CLKD5 ->
LCON5:=$28 -> IOX READ offset 6) works in isolation (unit test) and in the app, but not under
the harness's batched `machine.Run(200_000)` + `Thread.Sleep(1)` pump. Prime suspect: a driving/
timing/state difference between the app's run loop (BootDevice runs `machine.Run(addr, -1)`
continuously) and the harness's batched pump - something that perturbs the DATA-OUT readback or
the lock/test-mode state between the CLKD5 latch and the offset-6 read. This is the black-box
limit; resolving it needs the app's exact run model.

## Bug A (SEPARATE, real, worth fixing on its own)
`CpuND500.Reset()` (Emulated.HW\ND\CPU\ND500\CpuND500.ProcessControl.cs:42-43) does
`regs.Clear(); regs.stopMode = StopMode.NONE;` - it clears PC to 0 and clears the stop state but
does NOT re-park into the microcode IDLE (WAIT) state. So after any master-clear the CpuND500
run thread spins at PC=0 in garbage. `NDBusND500IF` calls `nd500Cpu.Reset()` from three
master-clear sites (lines 1776, 1925 ND500ProgrammedClear, 1964 MasterClear). SINTRAN's boot
master-clear (5MCST) hits these. Identical to the octobus OctobusND5000Station.HandleEmergency
bug (octobus LLM, 2026-07-18) - same `CpuND500.Reset()`. `AttachRealCpu` documents/works around
this at attach (sets stopMode=WAIT) but the reset paths do not. FIX: `CpuND500.Reset()` should
set `stopMode = StopMode.WAIT` (re-park to idle), which fixes BOTH generations at once. This is
NOT the cause of the status stall (Bug B) but is a real runaway-thread bug.

## Reproduce
`dotnet test Emulated.Tests\Emulated.Tests.csproj --filter "FullyQualifiedName~Nd100SintranNd500BootHarnessTests"`
Transcripts + trace: scratchpad `sintran-boot-capture-{boot,fullflow}.txt`.
See also [[emulator-nd500-interface-wiring]], ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md.
