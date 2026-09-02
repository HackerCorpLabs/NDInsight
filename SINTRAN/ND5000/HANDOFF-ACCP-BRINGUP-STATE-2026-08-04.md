# Handoff: where the ACCP bring-up actually stands

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\HANDOFF-ACCP-BRINGUP-STATE-2026-08-04.md`
**Date:** 2026-08-04
**From:** the station-split side
**To:** the control-store link agent, and the ND-5000 microcode agent
**Supersedes nothing** - it collects today's two measurements and says what is left.

---

## 1. The one-line state

**The octobus path works end to end; the microengine does not run.** The real ND-324716 firmware is
on the fabric, takes commands, and answers them with its own code. What it answers is "microprogram
not running" - and that is true, not a bug in the path.

## 2. What is now measured, not argued

| Measured | Where |
|---|---|
| The buffer at `0x001144F0` **is the read-back of microword `0x3FF0`**, eight halfwords, most significant first. `word[6]` is bits 31-16 of the **LOW** 64 bits, which the card wrote as zero. | `MEASUREMENT-STARTMIC-WORD6-2026-08-04.md` |
| Only **three** addressed control-store writes happen in the whole boot: `0x0000`, `0x0001`, `0x3FF0`. There is no loaded microprogram to execute. | same |
| `CSA: 00FFH` is a **hard-coded immediate** at ROM `0xCDA6`. It measures nothing. | `ANSWER-CSA-QUESTION-FROM-LINK-AGENT-2026-08-04.md` |
| The card **answers ALIVE** over the octobus with `FF 07` = MFNACK error 7, "microprogram not running" - matching our carved model, reached from different evidence. | `MEASUREMENT-ALIVE-ANSWERED-BY-REAL-CARD-2026-08-04.md` |
| With **no** command sent, the card emits **zero** frames across the same run. The reply is caused by the command. | same |

Two of those five are controls rather than findings. That ratio is deliberate.

## 3. THE blocker, and it is one 64-bit half of one microword

Everything above converges on the same place. The start/stop microprogram test's only pass condition
is `word[6] == 0x0100`, that word lives in the **low half of microword `0x3FF0`**, and the card wrote
that half as zero.

**Two readings remain, and they are yours to tell apart:**

- **(a) the write loses the low 64 bits** - the shift captured four halfwords, not eight. Against it:
  microword `0x0001` has a non-zero low half, so the path plainly *can* carry one.
- **(b) the hardware supplies the low half**, and the read-back is meant to differ from what was
  written.

**Discriminator, cheap:** trap the shift path for csWord `0x3FF0` and count halfwords consumed before
the perform. **Four means (a); eight means (b).**

I am not choosing between them. The last three attempts to reason about this message from a routine's
shape were all wrong, and two small instruments settled it each time.

## 4. What is wired on my side, and what is deliberately NOT

**Wired** (RetroCore `fc0ae6fcd`, `6d8504064`):

- `NDBusOctobus.AttachAccpCard(machine, station, instructionsPerClock)` - the real card on the
  fabric, advanced by `Clock()`.
- `ND100Machine.AttachAccpFirmware(station, octobus, instructionsPerClock, ...)` - the machine-level
  door.
- `AccpOctobusStation.InstructionsRun` - the machine keeps no running total, and a card being
  advanced is otherwise indistinguishable from one merely registered.

**Deliberately not:** the real card is **opt-in**. Ronny's call, today. The emulated command layer in
`OctobusND5000Station` stays the default and stays registered on its own station number. Nothing
relying on the emulated `ALIVE`/`CMSYSPAR` replies changes behaviour by attaching the real card.

**A budget of 0 parks the card**, and that is the default when one is attached without a budget. A
hung-looking card is easier to spot than one silently eating host time.

**`AccpMachine.Run` counts INSTRUCTIONS, not clock cycles.** I named the budget "cycles" first; it
read as a timing figure it is not.

## 5. Open, and honestly unexplained

**The ALIVE reply repeats about 5,120 times** - error code `07` on the first, `06` on every repeat.
Not the probe echoing (it never transmits). Not free-running chatter (the silent control proves it).
Something reads a different state after the first answer. **ASSUMPTION, unverified:** a card nobody
acknowledges may be retransmitting - our probe deliberately does not answer. Nothing measured supports
or refutes that.

Whether the console `0x0017`/`0x0015` and octobus `STARTMIC`/ARMA converge deeper in the ND-5000
hardware is **still genuinely open**, and still a good question.

## 6. The rule both sides have now paid for twice

Your standing check - **"does this window cover more than one addressable register?"** - and my
object-level version of it - **one piece of hardware, two models** - are the same defect. Between us
they have cost `0x2018`/`0x0018`, `0x330001`/`0x330000`, the botched first split, the second control
store, the second AFLAG model, and the second AOB/AIB register file.

Stated once, for both:

> **Before trusting a handler or a model, ask what else is behind it. And before believing a reply,
> run the same thing with nothing asked - agreement with yourself is not evidence.**
