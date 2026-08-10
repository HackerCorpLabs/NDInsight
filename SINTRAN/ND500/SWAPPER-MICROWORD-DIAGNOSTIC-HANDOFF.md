# Self-service swapper-start diagnostic - microword-gap loop (handoff to the CpuND5000 LLM)

You own `CpuND5000` (the microword ND-5000 CPU). This test drives the REAL ND-500 swapper into your
microcode and, on any `throw`, dumps everything you need to add the missing microword case - so you
can run -> read -> fix -> re-run yourself without round-tripping through the octobus/harness LLM.

## Run it

```
dotnet test "Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\HackerCorpLabs.Emulation.CPU.ND5000.Tests.csproj" --filter "FullyQualifiedName~SwapperStartDiagnosticTests" -c Debug
```

Test file: `RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\SwapperStartDiagnosticTests.cs`
(written by the octobus LLM; observational). It loads the real `SWAPPER-K01.PSEG/DSEG` at link addr
`0x08000000`, posts a MICFU-23 (3START) mailbox message, and single-steps ONE microword per `Tick()`.

## CRITICAL: read the DUMP, not the pass/fail

The test **Passes** even when the swapper throws (it only asserts `macroCount > 0`). Ignore green/red;
read the console block it prints:

```
=== N macro instruction(s), T microword ticks. P=0x... ===
=== Stop reason: InvalidOperationException: <the throw message> ===
========== THROW DUMP ... ==========
(1) executing macro:  P(Npc)=0x08...   opcode=0xNN (0oNNN)
(2) Regs.InstrDt=..   Regs.OcaKind=..   Regs.PrevOcaKind=..
(3) last 16 State.Mpc (oldest -> throwing word last), decimal(octal): ... -> M(0oNNNN)
    throwing word Mpc = M (0oNNNN)
```

- `throwing word Mpc` (both decimal AND octal - no base guessing) = the CS address of the microword
  that threw. Your own throw strings print CS in **octal** (`Convert.ToString(State.Mpc, 8)`), so
  "CS 4420" in the message == `0o4420` == 2320 decimal in the dump.
- The 16-deep Mpc ring is the CS trace INTO the throwing word.

## The loop

1. Run. Read the `Stop reason` + the throwing-word Mpc.
2. If it's `... not implemented yet` (a missing microword case: memory op, operand select, dest, etc.)
   -> that's yours. Implement it in `CpuND5000.cs`, re-run, repeat.
3. `macroCount`/`ticks` rising across runs = progress (the swapper is walking forward). Advancing
   within the SAME opcode (e.g. `0o201` winding through `0o44xx`) is normal - one macro instruction
   has a long microcode body.

## When to STOP and hand back (NOT your lane)

Stop the loop and flag it if the stop is NOT an unimplemented-microword throw - i.e. any of:
- an **MMU/translate fault** (page fault / protection) - shared `Nd500Mmu` / mapping, octobus LLM.
- a **mailbox / servicer / 3START-context** issue (bad PCB, wrong N5STA, message shape) - octobus LLM.
- a **NUCLEUS nk-op** doing something wrong (nkMove/nkSend/nkReceive/nkGetInfo are your microcode, but
  the port/message SEMANTICS are per `ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md` sec 7.3) -
  coordinate.
- the swapper **parks on a monitor call / completes** (stopMode WAIT, no throw) - success, hand back.

## Progress so far (2026-07-22)

Fixed and passed, in order: data-type-7 -> `ORD,OP` (constant operand) -> `QVACC` (0o4406) ->
`RD,PX` (0o4407). CURRENT gap: **operand select `A,SPEC,LA` at Mpc 0o4420**, opcode `0o201` at swapper
P=0x08000055 (InstrDt=-1, OcaKind=0, PrevOcaKind=2). Macro count reached 17, ticks ~532.
