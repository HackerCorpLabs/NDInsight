# Question for the GOD LLM - the secure-ACK Counter/channel rule across reconnects

## Where we are

Live reconnect testing against real 100 (retrocore both ends). Progress is real: with the
outgoing-Flags1 continuation fix and the "ACK base channel = 0xDE anchor" fix, sessions 1 and 2
(connect, login, commands, help, disconnect) now run end to end with NO crash, INCLUDING the
climbed reconnect (session 2's connect rode D9/epoch 1). But the THIRD connect (still no restart)
crashes 100 at PERF_CONNCT / XMSG error 24B, during the connect handshake.

We believe the remaining bug is our secure-ACK (subtype 0x03) Counter+channel model, and we want
you to CONFIRM the exact rule from the capture bytes before we implement - we are only "slightly
sure" of the derivation and were told not to guess.

## What the capture shows (multiple-connect-100-to102-and-then-reboot-and-connect-again.pcapng)

The 102-originated ACKs (class 0x0001, echoing the acknowledged data Flags1) form ONE continuous
sequence across session 1 AND session 2 (no reset at the session boundary). Counter + channel:

```
F1    class  ch  ctr
0014  0001   DE  0A
0015  0002   DE  08
0016  0001   DE  08
0017  0001   DE  07
0018  0001   DE  06
0019  0002   DE  04
001A  0001   DE  04
001B  0002   DE  02
001C  0001   DE  02
001D  0001   DE  01
001E  0001   DE  00
001F  0001   DD  FF      <- Counter wraps 0x00 -> 0xFF, channel steps DE -> DD
0020  0001   DD  FE
...
0028  0001   DD  E6
   [102 rebooted]
0000  0001   DE  1E      <- reset: F1 back to 0, ctr 0x1E, channel back to DE
0001  0001   DE  1D
```

Reading this as the standard envelope arithmetic (XmsgEnvelope) applied to the ECHOED data Flags1
with an ACK-specific seed S_ack:

- Counter = ComputeCounter(S_ack, echoed_F1, ack_flags2) = (BaseLow(S_ack, ack_flags2) - echoed_F1)
- channel = DeriveChannel(S_ack, echoed_F1, ack_flags2, 0x00010000) = 0xDE - epoch(echoed_F1)

With S_ack = 0x1F this reproduces every row:
- class 0x0001: BaseLow = 0x1F - 0x01 = 0x1E, so ctr = 0x1E - F1 (0x1E at F1 0, 0x00 at F1 0x1E,
  0xFF at F1 0x1F with channel stepping to DD). MATCHES.
- class 0x0002: BaseLow = 0x1F - 0x02 = 0x1D, so ctr = 0x1D - F1 (0x08 at F1 0x15). MATCHES.

So the ACK Counter/channel appear to be a PURE FUNCTION of the echoed data Flags1 and a constant
S_ack, with NO per-connect reset - the channel steps DE -> DD -> DC purely because Flags1 climbs
past the ACK baseLow.

## What our code does wrong

We RE-SEED the ACK counter every connect (SeedCounter = connect_Counter + 0x0A) and reset the
channel wrap to 0. This happens to be correct for the first ACK of each session (connect_Counter +
0x0A == 0x1E - F1_connect), which is why sessions 1 and 2 worked. But on session 3 the connect
arrived at F1 0x2A - already PAST the ACK baseLow 0x1E - so the ACK must ride DD (one wrap). Our
re-seed reset the wrap to 0 and we emitted:

```
our session-3 connect ACK:  F1 0x2A  class 0001  DE  F4     (channel DE, WRONG)
capture-implied correct:    F1 0x2A  class 0001  DD  F4     (channel DD)
```

100 crashed at PERF_CONNCT on that DE ACK. So our per-connect re-seed is the bug; the fix is the
continuous S_ack envelope model above.

## What we need you to CONFIRM or CORRECT (do not answer from first principles - mine the bytes)

1. Is the secure-ACK Counter+channel truly the envelope model applied to the ECHOED data Flags1
   with a single constant S_ack per conversation (no per-connect reset)? Confirm ctr =
   ComputeCounter(S_ack, echoed_F1, ack_flags2) and channel = DeriveChannel(...) across the corpus.

2. What is S_ack EXACTLY, and how is it derived?
   - We infer S_ack = link_seed + 0x0B (0x14 + 0x0B = 0x1F for 100<->102). Is +0x0B universal, or
     link-specific, or is it really "first-connect Counter + 0x0B" (which equals link_seed + 0x0B
     only because the first connect after a reset arrives at Flags1 0x0000)?
   - Would it be more robust to LEARN S_ack from 100's OWN ACK stream to us (the reverse direction)
     rather than compute it? Do both directions share one S_ack, or one each?

3. The class-0x0002 ACKs (the "batch" second-of-a-pair): can our responder ALWAYS emit class 0x0001
   (ctr = 0x1E - F1), or does 100 validate the exact 0x0002 Counter (ctr = 0x1D - F1) and require us
   to reproduce the 0x0001/0x0002 alternation? i.e. is emitting 0x0001 where the real 102 emitted
   0x0002 (ctr off by one) tolerated, or is it a crash risk?

4. Is the ACK sequence SHARED between directions (100's ACKs to us and our ACKs to 100 drawn from a
   single interleaved counter) or independent per direction? The capture rows interleave 100's and
   102's ACKs; we need to know whether our per-direction ComputeCounter(echoed_F1) is correct or
   whether the counter must account for the other direction's ACKs too.

5. Reset semantics: does S_ack (and the whole ACK sequence) reset ONLY on a peer restart
   (ReachabilityRequest -> connect at Flags1 0x0000, ctr 0x1E, DE), and never at a mere session
   disconnect/reconnect? The capture shows the reset only at the reboot; confirm.

6. If any of this cannot be resolved from the existing corpus, tell us EXACTLY what additional
   capture to take (e.g. "3+ connects without restart so Flags1 crosses 0x1E and 0x11E, capturing
   both the DE->DD and DD->DC ACK-channel steps").

## Available captures

All decoded .md alongside each .pcapng in E:\Dev\Ronny\X25Emulator\pcap. The decisive one here is
multiple-connect-100-to102-and-then-reboot-and-connect-again.pcapng (379 FCS-valid frames, 2
back-to-back sessions then a reboot). Our C# decoder round-trips all of them byte-identical, and
EnvelopeConformanceTests proves ComputeCounter/DeriveChannel reproduce every DATA frame's
Counter+channel (753 frames, 0 mismatch) - so the envelope arithmetic itself is trusted; the open
question is purely the ACK seed S_ack and the 0x0001/0x0002 + direction/reset details.

## What we will do with the answer

Replace SecureDatagramReceiver's per-connect decrementing counter with the continuous S_ack
envelope model for the TAD-session ACK path (leaving the reachability/list-route path unchanged),
and lock it with a test that feeds the capture's data-frame Flags1 sequence and asserts our ACKs
match the capture's 102 ACK Counter+channel byte-for-byte across the DE->DD wrap.
