# Handoff: the ACCP control-store model, corrected end to end (2026-08-04)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\HANDOFF-ACCP-CONTROL-STORE-MODEL-CORRECTED-2026-08-04.md`
**WSL path:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND5000/HANDOFF-ACCP-CONTROL-STORE-MODEL-CORRECTED-2026-08-04.md`
**Date:** 2026-08-04
**Scope:** the ACCP (ND-324716, MC68000) to ND-5000 control-store link in RetroCore
**Companions:** `HANDOFF-ACCP-LINK-SEAM-CONTRACT-2026-08-04.md` (the living contract),
`ANSWER-CSA-QUESTION-FROM-LINK-AGENT-2026-08-04.md`,
`HANDOFF-STARTMICROPROGRAM-SEAM-2026-08-04.md`

---

## THE HEADLINE

**Five "MEASURED" facts about this link were wrong, and all five came from one root cause:** the two
latch bytes at `0x330000` and `0x330001` were being folded into a single register. `0x330001` sees
**42,297** writes to `0x330000`'s **208**, and holds bit 1 set almost permanently - so folding them
held the control-store gate open across every shift sequence **by accident**, and the whole staging
model was calibrated against that accident.

| Claim | Status |
|---|---|
| "The address is the NINTH GATED word" | **REPLACED** - it is an address PHASE ending in command `0x3010` |
| "The completed word must be LATCHED (gate closes and reopens)" | **REFUTED** - a workaround for the spurious gate close |
| "Multiple shift+operate cycles inside ONE gate window" | **REFUTED** - the gated staging buffer is never used at all |
| "`0x0015` is the microprogram-run command" | **REFUTED** - a generic strobe, 20,979 per boot |
| "`0x00FF` is an unmodelled register reading all-ones" | **REFUTED** - a hard-coded immediate at ROM `0xCDA6` |

Anything else derived while the fold was active deserves the same scrutiny.

## THE CORRECTED MODEL

**The address phase - `0x76E6`, called by EVERY path BEFORE the gate opens:**

```
76ee  3d40 0014            move.w D0,(0x14,A6)              ; the address parameter
7704  33ee 0014 00550000   move.w (0x14,A6),(0x00550000).l  ; the address, written ONCE
770c  3084 / 3085          move.w D4,(A0) / move.w D5,(A0)  ; 16 clock pairs
7714  30bc 3010            move.w #0x3010,(A0)              ; LATCH IT AS THE ADDRESS
7728  30bc 0015            move.w #0x0015,(A0)              ; a generic strobe
```

**Command words:**

| Word | Meaning | ROM |
|---|---|---|
| `0x0018` | perform the addressed control-store write | `0x7446`, `0x766C` |
| `0x2018` | load the MIR | `0x774C` |
| `0x2010` | begin read-back verify | `0x775A` |
| `0x2011` | shift in one word | `0x77B6` |
| `0x3010` | latch the address | `0x7714` |
| `0x0015` | generic strobe, every access issues one | `0x7728`, `0x7904` |
| `0x0017` | arm a microprogram start | `0x78FC` |
| `0x0010`/`0x000F` | the clock pair, not commands | |

**Gating:** `0x330000` only. Bit 2 is the console path (`0x7434`), bit 1 the boot path (`0x765A`);
both store the same `0x001144EE` shadow to `0x330000`. `0x330001` has its own shadow at
`0x001144EF`, is a different register, and **what its bits select is OPEN.**

**The gate does not capture the shift.** Audited by counting where each commit sourced its words
over a real boot: **latch / staging / ring = 0 / 0 / 8.** Every microword is shifted out UNGATED.
What the gate does select is unknown; "BUFFERED CI-bit groups" may still be the answer, but nothing
has shown it.

**The shift ring needs NINE slots.** The address travels through the same `0x550000` port right
after the eight halves, so eight slots evicted the first half and the microword committed one word
out of step (`33445566...F0010100` stored where `11223344...DDEEF001` was expected).

## THE P0 ANSWER: EIGHT HALFWORDS

Asked: does microword `0x3FF0`'s low 64 bits get lost on write, or is it supplied by hardware?

**Neither - the firmware writes all 128 bits.** From the shift engine at `0x7776`:

```
778a  lea     0x001144F0,A3      ; buffer start
7790  lea     (0x10,A3),A4       ; END = start + 0x10 = 16 BYTES
779c  move.w  #8,D3              ; 8 clock pairs per halfword
77a0  move.w  (A3),(A2)          ; halfword -> 0x550000
77aa  addq.l  #2,A3              ; next halfword
77ac  cmpa.l  A3,A4
77ae  bne.s   779C
```

A3 walks `0x1144F0` to `0x114500` in steps of 2 - **16 bytes, eight halfwords, 128 bits.** The
length is a hard-coded constant, so every path through `0x7776` sends eight; there is no
four-halfword variant. `0x77B6` (shift in) is identical.

So if `0x3FF0`'s low half arrives wrong, it is a bug between the shift and the store, not a missing
hardware contribution.

> **The runtime counter is built but its numbers are NOT quoted here.** The test run came back with
> plausible values while the build had FAILED (`CpuND5000.cs(2073,70): error CS0103: 'DfToD' does
> not exist`), so they came from a stale DLL. Confirm against the counter once that builds.

## MEASURED STATE

| | |
|---|---|
| ACCP suite | **141 passed, 0 failed** |
| MIR loads per boot | 284 |
| addressed control-store writes per boot | **8** (4 under the old model) |
| commits from latch / staging / ring | **0 / 0 / 8** |
| typed `LOAD-CONTROL-STORE` stores | `112233445566778899AABBCCDDEEF001` at `0x0100` |

## WHAT IS OPEN

1. **Implement `IControlStoreSink.StartMicroprogram`** against the shared `CpuND5000` - the station
   side owns this. It is the only thing between here and the card passing its own start/stop
   selftest, whose sole pass condition is **word[6] of `0x001144F0` reading `0x0100`**.
2. **What the `0x330000` gate actually selects**, now that it demonstrably does not capture the
   shift.
3. **What the `0x330001` bits mean** - the start/stop sequence sets and clears bits 0..3 and ORs in
   `0x5C`.
4. **Do `0x0017` and the octobus `STARTMIC`/ARMA path drive the same mechanism?** If so they must
   share one model - two models of one register is the defect this whole document is about.

## THE METHOD THAT ACTUALLY WORKED

Three hand-decodes of `failed at CSA: 00FFH` produced three different wrong answers. Two
instruments in `AccpMachine`, both off by default, settled it in ~20 seconds of run time each:

- **`WatchWordAddress`** -> `WatchWordHits`: one 16-bit cell; old value, new value, instruction
  count, and the PC of the instruction that had just retired.
- **`TrapPcAddress` + `TrapFrameOffset`** -> `TrapPcHits`: D0, A6 and a frame word when a chosen
  instruction retires - for values living in a stack frame, which have no address to watch until A6
  is known.

Fixture: `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\Nd5000CsaFailureTraceTests.cs`

**Rules earned the hard way today:**

- When a value cannot be traced statically, stop reading and measure.
- A number a program prints is not necessarily a number it computed - check for an immediate first.
- A round trip that agrees with itself proves nothing about what the far end is.
- A green suite is not evidence that the REASON is right. Counters that name which path ran are.
- Assert the build succeeded before believing any test number.
