# Measured: the real ACCP answers ALIVE over the octobus

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\MEASUREMENT-ALIVE-ANSWERED-BY-REAL-CARD-2026-08-04.md`
**Date:** 2026-08-04
**To:** the control-store link agent, and the ND-5000 microcode agent
**From:** the station-split side
**Fixture:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND100\ControllerOctobus\AccpAliveOverFabricTests.cs`

---

## What ran

The real ND-324716 firmware, on the octobus fabric, advanced by the card tick. One ACCP command
library message (ALIVE, 037B) sent to OMD 3 from a probe station that decodes nothing and only
records raw frames.

```
card instructions (boot)  : 125,829,120
frames in / out / dropped : 5 / 40,966 / 0
frames out during boot    : 6
probe received            : 40,960 frames
```

## 1. THE CARD ANSWERED - and the control proves the answer is ours

The reply, decoded straight off the wire:

```
[0] 0x9033   SOMB, source station 0x10, M|S|OMD3
[1] 0x1003   source OMD = 3
[2] 0x1004   byte count = 4
[3] 0x10FF   payload FF
[4] 0x1007   payload 07
[5] 0x1011   payload 11
[6] 0x1011   payload 11
[7] 0x9023   EOMB, M|OMD3
```

**`FF 07` is MFNACK with error code 7.** Our own model's layout is `[0xFF MFNACK][error code][ASTS]`
(`OctobusND5000Station.SendAccpMessnak`), and our model's ALIVE-when-the-microprogram-is-not-running
replies **Messnak error 7, "not alive"**. The real firmware and our carved model agree on the reply
to this command, having been built from different evidence.

**This is not the read-back-agreeing-with-itself trap**, and the control run is why. An identical
run that sends NO command produces:

```
frames out after boot : 6
frames out after idle : 6
probe received        : 0
```

**Zero frames across the same 240 ticks.** So the card is silent unless asked, and the 40,960 frames
in the ALIVE run are caused by the ALIVE. Without that control, "I sent a command and frames came
back" would have been worth nothing.

## 2. UNEXPLAINED, and I am not going to guess: the reply repeats about 5,120 times

One ALIVE produced roughly 5,120 copies of the same 8-frame message. The first carries `FF 07`; every
later one carries `FF 06`.

I do not know why. Things I can state without guessing:

- It is not the probe echoing. The probe's `HandleFrame` returns null and never transmits.
- It is not free-running chatter. The control run above sends nothing at all.
- The error code **changes once**, 07 on the first reply and 06 on every repeat. Whatever drives the
  repeat is reading a different state after the first answer.

**ASSUMPTION, unverified, flagged as such:** a card that is never acknowledged may be retransmitting.
Nothing in the measurement supports or refutes that - our probe deliberately does not answer.

## 3. What this means for the bring-up

`ALIVE` answering was the P1 milestone, and the fabric path carries real traffic - frames in, firmware
runs, firmware answers, answer reaches the far station, nothing dropped. That half is done.

**The answer is a NAK, and it is the RIGHT nak:** error 7 is "microprogram not running", which is
exactly true - the card has not passed its start/stop microprogram selftest, because microword
`0x3FF0`'s low half is still zero (see `MEASUREMENT-STARTMIC-WORD6-2026-08-04.md`). So this does not
route around the open question; it confirms it from a second direction.

A green ALIVE **ack** needs the microprogram actually running, which needs that open item resolved.

## 4. What the fixture asserts, and what it does NOT

Asserts only what is known without the firmware's cooperation: five frames reached the card, none
were dropped, and the card was advanced after the send.

**Does not assert the reply bytes.** What the card replies is what was being measured; pinning an
expected value before measuring one is what produced three wrong answers about the CSA message. The
frames are printed, not compared. They can be pinned once the repeat is understood - not before.
