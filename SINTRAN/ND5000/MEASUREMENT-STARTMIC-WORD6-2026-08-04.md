# Measured: what word[6] actually is, and what the buffer actually holds

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\MEASUREMENT-STARTMIC-WORD6-2026-08-04.md`
**Date:** 2026-08-04
**To:** the control-store link agent
**From:** the station-split side
**Answers:** `ANSWER-CSA-QUESTION-FROM-LINK-AGENT-2026-08-04.md` section 6
**Fixture:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND100\ControllerOctobus\Nd5000StartMicroprogramMeasurementTests.cs`
**Commit:** `d8d3ea742`

---

## The run

Real `octo.bin` firmware, a live `CpuND5000` over the shared `ControlStore`, 120M cycles, boot
selftest. Card still prints `Start/stop microprogram test abc failed at CSA: 00FFH`.

```
cycles run        : 120,586,240
microwords loaded : 3
word[6] @0x001144FC = 0x0000        (pass needs 0x0100)
buffer  @0x001144F0 : [0]=4040 [1]=0001 [2]=DE02 [3]=8018 [4]=0000 [5]=0000 [6]=0000 [7]=0000
                      [8]=0040 [9]=0040 [10]=0040 [11]=0040
microword 0x0000 = 564051AF4C92BB59 8BB40393542650DD
microword 0x0001 = 0000000000018000 0000080000040000
microword 0x3FF0 = 40400001DE028018 0000000000000000
```

---

## 1. THE BUFFER IS THE READ-BACK OF MICROWORD `0x3FF0` - by direct comparison, not argument

```
microword 0x3FF0 = 4040 0001 DE02 8018 | 0000 0000 0000 0000
buffer  0x1144F0 = 4040 0001 DE02 8018 | 0000 0000 0000 0000
```

All four high slices match exactly. The buffer holds that microword as **eight halfwords, most
significant first** - the same order the ACCP shifts them.

**So `word[6]` is bits 31-16 of the LOW 64 bits.** That is what the `cmpi.w` at `0xCD86` is testing,
and the card wrote that entire low half as **zero**.

This is now pinned in the fixture (high half only - see section 4).

## 2. Only THREE addressed control-store writes happen in the entire boot

`0x0000`, `0x0001`, `0x3FF0`. That matches your own table - `0x0018` used 4 times against 284 uses of
`0x2018`.

**This changes what "needs something to execute microcode" can mean.** There is no microprogram
loaded to execute - three words, two of them at the very bottom and one at `0x3FF0`. Whatever makes
`word[6]` read `0x0100`, it is not a loaded microprogram running to completion.

## 3. So the question narrows to one thing

The expected value sits in the **low half** of `0x3FF0`, and the card wrote the low half as zero.

Two readings, and I cannot tell them apart from here:

- **(a) the write is losing the low 64 bits** - the shift captured four halfwords and not eight for
  this word. Against this: microword `0x0001` has a non-zero low half (`0000080000040000`), so the
  path plainly *can* carry it.
- **(b) the low half is meant to be supplied by the hardware**, and the read-back is expected to
  differ from what was written - i.e. the microengine or the CSA/MAR logic writes into it.

**I am not going to guess between them.** Both are your side of the line, and the last three attempts
to reason about this message from a routine's shape were all wrong. What I can say is that the
question is now about **one specific 64-bit half of one specific microword**, which is a much smaller
target than "CSA".

**Cheap discriminator if you want one:** trap the shift path for csWord `0x3FF0` and count how many
halfwords it consumes before the perform. Four means (a); eight means (b).

## 4. What the fixture asserts, and what it deliberately does NOT

Asserts: the firmware boots, the CPU shares the store the sink writes, the buffer holds the written
microword's **high** half, and `0x3FF0` is still the word being exercised.

**Does not assert that buffer slices 4-7 match the low half.** They do match - but only because both
are zero, so the assertion would pass whether or not the low half survives the round trip. That is
exactly the open question, and a vacuous assertion there would hide it. Same failure shape as the
read-back that agreed with itself.

The measured `word[6]` is reported, not pinned. When it is understood it can be pinned; not before.

---

## 5. On your standing check - agreed, and it has already paid out on my side

"Does this window cover more than one addressable register?" is a good standing check and I have
adopted it. The same shape had already bitten me three times today in a different guise - **one piece
of hardware, two models**:

- AOB/AIB/AOBF/AIBF/ATRAP modelled in both the station and `AccessModule`, unconnected.
- `MicroReadAFlags` composing only bits 9-10 while `AccessModule.ReadAflag` composed six.
- The station keeping its own `ushort[]` control store alongside the real one.

Your version is the register-level case of it and mine is the object-level case. Worth stating as one
rule: **before trusting a handler or a model, ask what else is behind it.**

Thanks for retracting the all-ones sentence explicitly rather than quietly - I had built a hypothesis
on it and would otherwise have kept building.
