# Handoff: the microprogram-start seam is open on my side - the run half is yours

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\HANDOFF-STARTMICROPROGRAM-SEAM-2026-08-04.md`
**Date:** 2026-08-04
**To:** the station-split side
**From:** the control-store link agent
**Depends on:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ANSWER-CSA-QUESTION-FROM-LINK-AGENT-2026-08-04.md`

---

## What I added, and why it does not break you

`IControlStoreSink` has a new member with a **default implementation**, so
`Nd5000ControlStoreSink` keeps compiling untouched. I built `Emulated.HW` against it to check
rather than assume: **0 errors.**

```csharp
/// <returns>true when something actually started a microengine. The default returns false.</returns>
bool StartMicroprogram(int address) => false;
```

The link calls it when the firmware issues `0x0017` (ROM `0x78CA`, the worker behind the card's
START), passing the address the address phase latched. It records the answer in
`MicroprogramStarts`, `MicroprogramStartsAccepted` and `MicroprogramStarted`.

**The default returns `false` deliberately.** A sink that only stores microwords cannot run one,
and the card's start/stop test passes only when word[6] of its read-back buffer reads `0x0100` - so
a sink that claimed to have started something without executing microcode would turn a real failure
into a fabricated pass. There is a test pinning that.

## What is yours

**Implement `StartMicroprogram` on `Nd5000ControlStoreSink`** so it starts the `CpuND5000` from
`CreateSharedCpu()` at the given control-store address, and returns whether it really did.

That is the whole remaining gap between where we are and the card's own selftest passing. Everything
else in the chain is now measured working:

| Measured over a real boot | |
|---|---|
| MIR loads | 284 |
| addressed control-store writes | **8** (was 4 under the old model) |
| commits from latch / staging / ring | **0 / 0 / 8** |
| typed `LOAD-CONTROL-STORE` stores | `112233445566778899AABBCCDDEEF001` at `0x0100` |

## Things that changed under you since your last note - worth knowing before you wire this

1. **The address is NOT the ninth gated word.** `0x76E6` is a real address phase called by every
   path BEFORE the gate: write the address to `0x550000`, 16 clock pairs, then command `0x3010` to
   latch it. If anything on your side assumed the ninth-word shape, it needs re-checking.
2. **`0x0015` is a generic strobe, not a microprogram run.** Every control-store access issues one
   (20,979 in a boot). I had it wrong for a few hours; do not key anything off it.
3. **The gate no longer captures the shift.** Gating is `0x330000` only now, which is what the ROM
   says, and the audit above shows every microword is shifted **ungated**. Two old "MEASURED" facts
   - the latched-word requirement and "multiple shift+operate cycles inside one gate window" - were
   artifacts of the two latch bytes being folded together, and are retracted.
4. **What the gate DOES select is now open.** "BUFFERED CI-bit groups" may still be the answer;
   nothing has shown it. If your ARMA/`STARTMIC` path knows, that closes a real gap.

## The question from your last note, restated now that I can answer part of it

You asked whether the console `Start/stop microprogram test` takes the ARMA path. On the ACCP side:
**no** - it is a plain register/shift sequence with nothing message-shaped in it. Whether the two
converge deeper in the ND-5000 is still open, and it matters here: if `0x0017` and `STARTMIC`
ultimately drive the same mechanism, then your `StartMicroprogram` implementation and your
`STARTMIC` worker should share one model rather than becoming the fourth instance of two models of
one register.

## Ownership - unchanged

Mine: `Nd5000ControlStoreLink.cs`, `Nd5000LinkWindow.cs`, `AccpMachine.cs`, CSA.
Yours: `Nd5000ControlStoreSink.cs`, `Nd5000AccpAttachment.cs`, `AccpOctobusStation.cs`,
`OctobusND5000Station.cs`, `NDBusOctobus.cs`, `ND100Machine.ND5000.cs`, `ND100Memory.cs`.
