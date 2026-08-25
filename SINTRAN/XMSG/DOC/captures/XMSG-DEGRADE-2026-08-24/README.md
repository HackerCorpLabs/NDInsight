# The peer does answer. D100 stops listening.

**Captured 2026-08-24 at the hub, every frame on the segment, in `segment.pcap`** (classic pcap,
link type Ethernet - `xmsghub --capture`). 993 frames, 175 seconds, from a cold boot of all three
machines through one complete failure.

## What was done

```
1. all three machines rebooted, boot files configure everything by themselves
2. D100:  X-C LIST-NAMES 102                    -> D102's whole table. WORKS.
3. D100:  COPY-FILE D102(SYSTEM).CHAT:PLNC,CHAT:PLNC
                                                -> hangs ~150 s, then
                                                   NO ANSWER FROM REMOTE SYSTEM;
                                                   FILE-ACCESS CONNECTION ABORTED
```

## What the frames say

The machines are `08:00:26:64:00:00` (D100 - `64` is 100) and `08:00:26:66:00:00` (D102). Every
frame is 802.3 with the ND LLC header `a8 a8 03`, then `0b 02 <opcode>` and a 16-bit sequence.

**Healthy traffic is a tight ladder, both directions at once:**

```
 55.794 D100->D102  0b 02 20  seq  17  650 bytes      data
 55.794 D100->D102  0b 02 20  seq  18  480 bytes      data
 55.796 D102->D100  0b 02 20  seq 100   64 bytes      data the other way
 55.798 D100->D102  0b 02 3f  seq 101   60 bytes      ACK, carrying next-expected
 55.800 D102->D100  0b 02 3f  seq  19   60 bytes      ACK
```

`0b 02 20` is data and `0b 02 3f` is the acknowledgement. Across the whole capture: 545 data
frames, 435 acks.

**Then D100 goes silent.** Its last transmission of the run is frame 950:

```
 55.955 D100->D102  0b 02 20  seq  19   60 bytes   <-- the last thing D100 says for 38.6 s
```

**D102 keeps answering, and nothing acknowledges it.** It retransmits the same three frames - link
sequences 101, 102, 103 - **six times over the next 1.3 seconds**, and D100 acks none of them:

```
 55.962  56.424  56.552  56.675  56.824  56.944  57.065   D102->D100  seq 101, 102, 103
```

Those three frames are small - 60, 64 and 74 bytes - so they are not file content. They carry the
XMSG system pair `00 64 00 66` (to 100, from 102): **replies from D102's file server to D100.**

**Then the higher layer retries three times, 40 seconds apart, and gets a DIFFERENT answer:**

```
 95.676 D100->D102  0b 02 20  seq 20  650 bytes    the push, resent
 95.677 D100->D102  0b 02 20  seq 21  480 bytes
 95.678 D102->D100  0b 02 6f  seq 16   60 bytes    <-- NOT the 0b 02 3f ack
 95.679 D102->D100  0b 02 6f  seq 17   60 bytes
 135.475 ... the same, sequences 22/23, answered 19/20
 175.286 ... the same, sequences 24/25, answered 20/16
```

`0b 02 6f` appears **six times in the whole capture and only here.** The retried data frames are
byte-identical except for the link sequence field at offset 21, so it is the same data with a
fresh sequence.

## The finding

**"NO ANSWER FROM REMOTE SYSTEM" is exactly backwards. D102 answers every single time.** It
answers the file-server reply six times over, and it answers each of the three retries. What
stops is **D100 acknowledging what it receives** - after 55.955 s it never sends another ack.

So the fault is on **D100's receive path**, not on the wire, not at D102, and not in routing. The
hub forwarded everything: it holds all three machines and its counters show no drop of any kind
(`dropped slow 0 / loop 0 / ttl 0`).

## NOT ESTABLISHED - do not turn these into causes without more decoding

 - **Why D100 stops acking.** Receive buffers full, the consuming task stalled, or the ENNS0
   receive path itself - none of these has been shown.
 - **What `0b 02 6f` means.** It replaces the `0b 02 3f` ack when the session is already broken, so
   it is probably a reject or an error, but the byte layout has not been decoded.
 - **Whether it is specific to this file.** The same push of `CHATSV:PLNC` (230583 bytes) had
   succeeded minutes earlier. `CHAT:PLNC` failed on the first attempt after a fresh boot.
 - One oddity worth a look: `X-C LIST-LINK` on D100 shows the Ethernet row with `Rcv 0` and
   `Xmit 0` while `TXData` climbs into the thousands. Nobody has checked what those two columns
   are counting.

## Method note

This is what [[analyse-captures-do-not-grep-them]] is for. Every earlier theory about this fault -
the Ethernet cards, the system table, the friend grants, the sequence drift - came from reading
tables and reasoning. Decoding one complete failure in order took minutes and produced the one
fact none of them contained: **the peer is answering.**

---

# Follow-up on the machine, same session

## `0b 02 6f` was ALREADY KNOWN. I decoded by hand what the registry had.

`DOC/protocols/sintran-wire.json`, bitfield `nd_link_frame_kind`:

```
0x0F  ConnectionRequest              MEASURED
0x1F  ConnectionConfirm              MEASURED (we send it; never received one)
0x20  Data                           MEASURED
0x3F  Acknowledge                    MEASURED
0x60  DisconnectRequest60            MEASURED
0x6F  DisconnectRequestByNetworkService   MEASURED, since 2026-08-11
```

**So D102 is not failing to acknowledge - it is DISCONNECTING.** Read the registry first; that is
what it is for.

## The capture kept running, and the later part is worse than the early part

The file now spans 2093 s. **After 175.5 s, D102 sent 23 frames and D100 sent exactly ONE.**

```
1255.077 .. 1256.485   D102 -> D100   ConnectionRequest  x11    D100 NEVER ANSWERS
1264.589 .. 1265.697   D102 -> D100   ConnectionRequest  x11    D100 NEVER ANSWERS
2093.632               D100 -> D102   Data                      (this was a LIST-NAMES 102 by hand)
2093.640               D102 -> D100   DisconnectByNetworkService
```

A healthy connection setup is in the same file at 29.747: D100 sends CR, D102 answers CC 3 ms
later. **D100 answers 22 ConnectionRequests with nothing at all.**

## It is NOT XMSG dying, and NOT the whole receive path

 - Nothing is printed on the console. XMSG has not crashed - this is not the XXHER fault.
 - `X-C LIST-NAMES 103` over **HDLC answers instantly** while `LIST-NAMES 102` over Ethernet
   refuses. XMSG on D100 is healthy; one Ethernet conversation is not.

## A theory that lasted two minutes, killed by a control run

`LIST-UTILIZATION` on D100 showed `Receive Frame table: 20 limit, 10 max used, 10 IN USE` and it
looked like an exhausted pool. **D102, which is healthy, shows 5 in use with 1 link. D100 has 2
links.** `List-Generation-Variables` says `X4NBF Default receive frames per link = 5`. So 5 per
link is simply what a working machine looks like, and the pool is fine on both.

## The one real difference found so far - the receive frame STATUS

`LIST-FRAMES` on both, Ethernet link 152164:

```
D100 (broken)   164457 152164 ----     0   100   102   111  ACK
                164512 152164 ----     0   100   102   113  * <->
                164545 152164 ----     0   100   102   112  * <->

D102 (healthy)  164457 152164 ----    41   100   102   115  * <->
                164512 152164 ----    41   100   102   114  * <->
                164545 152164 ----    41   100   102   114  ACK
```

**Same table, same link type, and the Status column reads 41 on the healthy machine and 0 on the
broken one.**

**NOT ESTABLISHED:** what the Status field means, whether 41 is "armed to receive" and 0 is "not
armed", and which side sets it. That is the next thing to find out - the XMSG NPL source and the
XMSG manual both describe the receive frame record, so this is a lookup, not a guess.

If 0 does turn out to mean "not armed", it would explain everything above in one line: a card that
is not armed to receive hears nothing, so 22 ConnectionRequests go unanswered while the machine
itself is perfectly healthy - and the suspicion would land on the C# Ethernet controller, which is
what arms and completes those buffers.

---

# The C# Ethernet controller is EXONERATED - exact frame-count match

`net status` on D100's RetroCore console, taken while D100 was in the broken state and moments
after D102 had sent eleven fresh ConnectionRequests:

```
Card MAC: 08:00:26:64:00:00 (00:00:.. until SINTRAN initializes the LANCE)
Network: ATTACHED - tcp:127.0.0.1:5010
TX: 496 packets, 80148 bytes
RX: 532 packets, 33018 bytes (enqueued to LANCE)
    accepted=531  filtered(wrong MAC)=1  missed(no buffer)=0  rx-off=0
```

Counted independently from this pcap, over the same span:

| | hub capture | card counter |
|---|---|---|
| frames addressed TO D100 | **531** | `accepted=531` |
| frames sent FROM D100 | **496** | `TX 496` |
| frames to some other address | **1** | `filtered(wrong MAC)=1` |

**Every number matches exactly.** The controller received, accepted and enqueued every frame the
segment carried, and transmitted every frame the segment saw from it. All 34 ConnectionRequests
addressed to D100 - including the eleven sent minutes before the reading - are inside that 531.

**So the bug is NOT the host-side networking, NOT the MAC filter, and NOT frame delivery into the
card.** The theory that D100's LANCE was filtering inbound frames is dead.

## And `missed(no buffer)=0` narrows it further

If the ND side had stopped giving the LANCE receive buffers, the ring would have filled and the
counter would show missed frames. It shows none, across 531 receives. **Something kept servicing
the receive ring the whole time**, which means the frames were not merely enqueued - they were
being taken.

## Where that leaves it

The frames arrive, are accepted, and are taken off the ring. D100 still answers nothing. So the
fault lies between the LANCE receive ring and XMSG's link layer:

 - the LANCE emulation's descriptor and interrupt handling (C#), or
 - the ENCOS 68000 firmware running on the emulated card, or
 - SINTRAN's network server / XMSG link layer above it.

**NOT ESTABLISHED: which of those three.** The next discriminating measurement is XMSG's own trace
class 11, "Link layer: frames received" - if XMSG's link layer logs the arrival, the first two are
cleared and the fault is in XMSG; if it logs nothing, the frame died below it. The trace commands
are not in the plain `X-C` command list and are probably behind `Set-Advanced-Mode` or
`Debug-Mode-On`; that has not been worked out yet.

Also still unexplained, and possibly the same thing: `LIST-FRAMES` shows D100's Ethernet receive
frames with **Status 0** where healthy D102 shows **Status 41**.

---

# Two hypotheses tested and killed, and one that the firmware RE predicted

## KILLED: "D100 only answers a ConnectionRequest whose sequence is zero"

The two answered requests in this capture both carry sequence 0 and every ignored one carried a
non-zero sequence, so this looked strong. **Tested directly:** `STOP-NETWORK-SERVER` then
`START-NETWORK-SERVER` on D102 plus a fresh `DEF-NETWORK-CONN` reset its tables (`LIST-SYSTEMS`
went from 6 entries to 4, the row for 100 back to sequence 0/0). D102 then sent **22 fresh
ConnectionRequests with sequence 00**, link ids 1B3F and 1B40:

```
3799.488 .. 3801.010   D102 -> D100  ConnectionRequest seq 00  ids 0000/1B3F  x11
3826.333 .. 3827.847   D102 -> D100  ConnectionRequest seq 00  ids 0000/1B40  x11
```

**D100 answered none of them.** The sequence number is not the discriminator.

Worth keeping from that experiment: **restarting the network server alone does NOT make a machine
retry** - D102 refused locally and put nothing on the wire until `DEF-NETWORK-CONN` was run again.
The server start and the connection definitions belong together, exactly as the boot file has them.

## KILLED: "ENNS0 has died or hung on D100"

`LI-RT-DES,ENNS0` on both machines:

```
D100 (broken)   PASSIVE ... RTWT   ACTUAL 101B   LU 2240B INPUT   P=030440 X=000044 ...
D102 (healthy)  PASSIVE ... RTWT   ACTUAL 113B   LU 2240B INPUT   P=030440 X=000044 ...
```

**Identical** - same wait, same registers, same start address. `RTWT` on an input wait from LU
2240B is simply what ENNS0 looks like between frames. It says nothing about the fault.

## RETRACTED: "the frames were being taken off the receive ring"

That was written here on the strength of `missed(no buffer)=0` in `net status`. **That counter is
`lance.RxMissed` - the LANCE's own, on the host side of the card.** The 68000 firmware has a
SEPARATE discard that `net status` cannot see. So whether the ND-100 ever gets the frames is NOT
established by that counter.

## THE HYPOTHESIS THE FIRMWARE RE ALREADY WROTE DOWN - "enabled but starved"

`Emulated.HW/ND/CPU/NDBUS/NDBusEthernetII.cs` in the RetroCore repo, from firmware reverse
engineering dated 2026-07-24, describes this failure before we met it:

 - The card's rx pool at `$188C6` is **NOT** pre-filled by firmware init. It is ENABLED by a host
   command, and FILLED only by the host POST-BUFFER command, opcode `0x12` (handler `0x6CEE`).
 - So gate3's "count != 0" test can PASS while the free list head is still NULL.
 - In that case RCVCOMPLETE branches to **`0x5ECA`: bump the miss counter and DISCARD the frame
   with NO SCIP** - no interrupt, so the ND-100 is never told anything arrived.
 - The comment calls this *"the exact enabled-but-starved failure mode to look for on a node that
   receives but never forwards."*

**That is a written description of D100.** And it fits the one difference already measured here:
`LIST-FRAMES` shows D100's Ethernet receive frames with **Status 0** where healthy D102 shows
**Status 41** - consistent with buffers posted to the card versus not posted. UNVERIFIED: what the
Status field actually means.

## The experiment, already built into the emulator

`NDBusEthernetII.cs` carries a `[68K-PC]` watch list written, in its own words, *"to see whether -
during ENNS0's LU-2240B input wait - the firmware ever reaches its message/superkick layer"*. It
logs at `Logger.LogLevel.Device`, which is level **2**.

**On D100's RetroCore console, while it is in the broken state:**

```
DebugLog 2
```

then make D102 send connection requests, and read the log for:

| line | what it means |
|---|---|
| `[68K-RX] NO-BUFFER DISCARD 0x5ECA` | pool enabled, free list empty - **the host stopped posting receive buffers** |
| `[68K-RXPOOL] POST-BUFFER 0x6CEE` | SINTRAN posting a buffer - if these have gone quiet, that is the fault |
| `0x6020` RX DELIVERED | the frame was handed up and the ND-100 got its SCIP |
| `0x6034` / `0x5EF4` | discarded for some other reason - which gate failed is one line past the last PASSED |

If `0x5ECA` fires while `0x6CEE` has gone quiet, the host stopped replenishing. If `0x6CEE` never
fires at all, the post-buffer command is being lost on the way in.

`DebugLog 2` is Device level and may be noisy - turn it off again with `NoLog`.

---

# THE CARD'S 68000 NEVER RUNS ITS RECEIVE PATH

Measured 2026-08-24 with `DebugTrace 2 4` on D100 - level 2 (Device) written to
`%LOCALAPPDATA%\trace\file-trace.txt` instead of the console, so the machine's own window stays
readable. **Use the file destination, never `DebugLog 2`** - Device level on a running machine
buries the console.

While the log ran, D102 was made to send another burst of ConnectionRequests, confirmed on the
wire at the hub (the capture grew 836 bytes = 11 more frames).

**Across the WHOLE trace - 27830 lines, 26 PC samples - every 68000 PC is in one small loop:**

```
0x02CEE  x13     0x02CCC x3     0x02CC4 x2     0x02CC6 x2
0x02CBC  x2      0x02CE0 x2     0x02CD2 x1     0x0356A x1
                                            halt=False  reset=False
```

**And not one receive-path PC appears anywhere in the file:**

| PC | what it is | seen |
|---|---|---|
| `0x5C42` | RCVCOMPLETE, LANCE receive complete | **never** |
| `0x5ECA` | no-buffer discard, pool enabled and starved | **never** |
| `0x6CEE` | POST-BUFFER, the host opcode 0x12 handler | **never** |
| `0x6020` / `0x6034` / `0x5EF4` | delivered / discarded / gate failed | **never** |

## What that settles

Both earlier candidates are dead:

 - **NOT gate1 dropping on a wrong firmware station MAC** - gate1 lives inside RCVCOMPLETE, which
   never runs.
 - **NOT "enabled but starved" at gate3** - same reason. The rx pool is never even consulted.

The 68000 is running (`halt=False`, `reset=False`) and doing something in a six-instruction loop
around `0x02CEE`, and it never services a received frame. The frames are on the wire, the host
side delivers them, the LANCE accepts them - and the firmware is somewhere else entirely.

## NOT ESTABLISHED, and the next step

**What `0x02CBC`..`0x02CEE` IS.** It could be a normal idle loop that is simply never interrupted,
or a fault handler it has fallen into and cannot leave. Those two have completely different
causes, and nothing here distinguishes them.

**The control run that would:** the same `DebugTrace 2 4` on D102, whose card is healthy. Both
machines write to the same file and each line carries its own `[ID:nnn]` tag, so they are easy to
tell apart. If D102's idle PC sits in the same loop, the loop is normal and the missing piece is
the interrupt that should break it. If D102's is elsewhere, D100's firmware has gone somewhere it
should not be.

That control is cheap and must come before any claim about the loop - two hypotheses have already
died here for want of one.

---

# THE CONTROL RUN: the idle loop is NORMAL. D100 is never woken from it.

**Correction to the section above.** I wrote that D100's 68000 was "spinning in a six-instruction
loop" and let that imply it was trapped. **It is not.** The healthy machine sits in exactly the
same loop.

Measured with `DebugTrace 2 5` on D102 - **destination 5 is NATS**, which is how you trace more
than one machine at once. Traces land in `C:\Users\ronny\AppData\Local\trace\trace.txt`.

**DO NOT USE DESTINATION 4 (File) ON TWO MACHINES.** The path is a fixed `file-trace.txt` with no
machine id and it is opened `FileMode.Append` without sharing, so the second machine to start
logging dies on a sharing violation. It killed D102 while we were doing this.

## D102, healthy, receiving a frame that D100 sent

```
[ID:102] [68K-RX] RCVCOMPLETE 0x5C42: pool $188C6 count=1 freeHead=0x01B22E
                  | stationMAC@1885E=08:00:26:66:00:00 promisc@18888=0x0001 mode8023@1888A=0x0001
[ID:102] [68K-RX] gate1 PASSED (dst MAC == station MAC 0x1885E) -> testing gate2 (802.3)
[ID:102] [68K-RX] gate2 PASSED (802.3 length-framed) -> testing gate3 (rx pool)
[ID:102] [68K-RX] DELIVERED: RCVCOMPLETE built a message -> jsr 0x134E6 (ND-100 ready-ring + SCIP)
[ID:102] [68K-RXPOOL] POST-BUFFER 0x6CEE entered (host opcode 0x12, #1): pool count=1 freeHead=0x01B268
[ID:102] [68K-RX] RCVCOMPLETE 0x5C42: pool count=1 freeHead=0x01B268 | stationMAC ...
```

Receive, pass both gates, deliver with a SCIP to the ND-100, host reposts a buffer, round again.

## The two machines side by side

| | D100 (broken) | D102 (healthy) |
|---|---|---|
| idle PCs sampled | `02CBC 02CC4 02CC6 02CCC 02CD2 02CE0 02CEE` | **the same set** |
| `[68K-RX]` lines | **none** | 5 |
| `[68K-RXPOOL]` lines | **none** | 1 |
| RCVCOMPLETE `0x5C42` entered | **never** | yes |

**The idle loop is identical. The difference is that D102 gets PULLED OUT of it to service a
received frame and D100 never does.**

## Where the fault is now

The frames are on the wire (hub capture), the host side accepts every one (`net status`
531 accepted of 531 addressed to D100, exact), and the card's 68000 is alive and idling
normally - **and it is never invoked to service a receive.**

So the break is in the step between the LANCE having a frame and the 68000 running RCVCOMPLETE:
the receive-complete signalling inside `NDBusEthernetII`. That is C# in the RetroCore repo, and it
works on D102's instance of the same code, so it is a state a card instance can get into rather
than a plain coding error on the path.

**NOT ESTABLISHED:** what state that is, or what puts a card into it. The next step is to read what
`NDBusEthernetII` does after it enqueues a received frame - what it sets or raises to make the
68000 leave the idle loop - and find what could stop that happening while `RxAccepted` still
climbs.

---

# ROOT CAUSE LOCALISED: the LANCE decides to interrupt and the CPU is never told

Both machines traced with `DebugTrace 2 5`, D100 broken and D102 healthy, same instruments.

## The counts that settle it

| line, logged by the emulator itself | D100 (broken) | D102 (healthy) |
|---|---|---|
| `LANCE Interrupt Generated` | **13** | 3 |
| `LANCE Interrupt Asserted (level 2)` | **0** | 2 |
| `LANCE Interrupt Deasserted (level 2)` | **0** | 2 |

**D100's LANCE decides an interrupt is due thirteen times and asserts the CPU line zero times.**

## The two cycles, side by side

**D102, healthy - the whole handshake completes:**

```
LANCE RX accepted: 60 bytes dst=08:00:26:66:00:00 src=08:00:26:64:00:00
LANCE RX: ring[1] addr=0x018010 rmd1=0x8003 OWN=True
LANCE RX complete: result=60 ring[1] rmd1=0x0303
LANCE Interrupt Generated
LANCE Interrupt Asserted (level 2)                 <-- the CPU is told
LANCE Interrupt Acknowledged (level held until CSR0 write / LANRESET)
LANCE RDP Read CSR0 = 0x04F3                       <-- firmware services it
LANCE Interrupt Deasserted (level 2)
LANCE RDP Write CSR0 = 0x04F3                      <-- firmware clears the cause
```

**D100, broken - it stops dead after "Generated", every single time:**

```
LANCE RX: ring[41] addr=0x018150 rmd1=0x8004 OWN=True
LANCE RX complete: result=60 ring[41] rmd1=0x0304
LANCE Interrupt Generated
LANCE RX: ring[42] ...  RX complete ...  Interrupt Generated
LANCE RX: ring[43] ...  RX complete ...  Interrupt Generated
LANCE RX: ring[44] ...
```

The ring advances 41, 42, 43, 44 - the frames really are being written into card memory - and
the CPU line is never touched.

## Where it is, exactly

`Am2990Lance.CheckInterrupts()` logs "Interrupt Generated" and then:

```csharp
if (shouldInterrupt != _irqAsserted)
{
    _irqAsserted = shouldInterrupt;
    OnLANCEIRQ?.Invoke();          // -> Lance_OnIRQ -> InterruptControllerSetInterrupt(2, active)
}
```

`Lance_OnIRQ` is the only place that logs Asserted/Deasserted, and it never ran on D100. **So
`_irqAsserted` is stuck `true` while the 68000 is not servicing anything.** Every later receive
sets RINT, `shouldInterrupt` is already true, the guard sees no transition, and the notification is
swallowed.

It is self-sustaining once entered. The design is deliberate and matches the hardware - LANINTR is
a LEVEL that only a firmware CSR0 write or LANRESET releases, and IACK does not clear it. But the
firmware only writes CSR0 when it services the interrupt, and it never gets one. Nothing in that
loop can break itself.

## What is PROVED and what is NOT

**PROVED, by the emulator's own log on two machines running the same code:** frames are DMA'd into
the receive ring, receive completes, the LANCE decides an interrupt is due, and the CPU is never
told. The 68000 therefore never leaves its idle loop and never runs RCVCOMPLETE.

**NOT ESTABLISHED: what first put `_irqAsserted` out of step.** The trace was started long after
the machine broke, so the transition that stranded it is not in it. Candidates, none tested:
the line was asserted once while the 68000 could not take it; something reset the CPU-side
interrupt without the LANCE knowing; or a path clears the CPU level without going through
`Lance_OnIRQ`.

**The next measurement is the one that catches it happening.** Trace with `DebugTrace 2 5` from
BOOT, on both machines, then run the COPY-FILE that breaks it. The last `Asserted` with no matching
`Deasserted` is the moment, and everything around it is the cause.

## An instrument worth adding either way

`net status` prints the RX counters but nothing about interrupt state. Adding CSR0 (INEA, RINT,
INTR), CSR3's masks and `_irqAsserted` to it would turn this from a trace-and-compare job into one
command - and would have answered the whole question here in a second.

---

# THE BUG, in three code sites that each behave reasonably alone

Found by reading, after the trace localised it. All three are in the RetroCore repo.

## 1. The CPU clears the pending bit when it takes the interrupt

`Emulated.HW/Motorola/CPU/MC68K/Cpu68K.Interrupts.cs`, `CheckAndHandleInterrupts`:

```csharp
// Clear the pending interrupt BEFORE the IACK callback.
// MC68000 uses level-triggered interrupts: the device's IACK handler
// may re-assert the interrupt ... Clearing first ensures the re-assertion isn't lost.
ClearPendingInterrupt(pendingInterruptLevel);
```

**The contract is explicit: the CPU drops the level, and the DEVICE re-asserts it if still held.**

## 2. The LANCE's IACK handler re-asserts nothing

`NDBusEthernetII.HandleLANCEInterruptAck` sets autovector, marks handled, and logs
*"level held until CSR0 write / LANRESET"*. It never calls
`InterruptControllerSetInterrupt(2, ...)`. **Nothing is holding the level.**

## 3. The LANCE core will not raise it again

`Am2990Lance.CheckInterrupts`:

```csharp
if (shouldInterrupt != _irqAsserted)      // already true -> no transition
{ _irqAsserted = shouldInterrupt; OnLANCEIRQ?.Invoke(); }
```

## Put together

```
frame -> RINT -> CheckInterrupts -> _irqAsserted false->true -> SetPendingInterrupt(2)
68000 takes it  -> ClearPendingInterrupt(2)          the level is now GONE at the CPU
                -> IACK handler re-asserts NOTHING
                -> _irqAsserted is still TRUE
firmware does not write CSR0 this time round
next frame -> RINT -> shouldInterrupt true == _irqAsserted true -> NO NOTIFY, ever again
```

**One ISR entry where the firmware does not clear the cause is enough to deadlock the card for
good.** It matches every measurement: frames DMA into the ring, `RxAccepted` climbs, the 68000
idles, and the emulator logs "Interrupt Generated" over and over with no assertion.

D102 stays healthy because its firmware promptly reads and writes CSR0, which drops
`_irqAsserted` back to false and restores the ability to notify.

## The fix, at the site the CPU already expects it

`HandleLANCEInterruptAck` should re-assert while the LANCE still holds its line:

```csharp
// The CPU cleared the pending bit before calling us. LANINTR is a LEVEL, so if the
// LANCE still has an unmasked cause with INEA set, put it straight back - that is
// what "the device's IACK handler may re-assert" in Cpu68K.CheckAndHandleInterrupts
// means. Without this the level is lost the moment the CPU takes it.
if (lance.IsInterruptActive)
{
    InterruptControllerSetInterrupt(2, true);
}
```

**UNVERIFIED - this fix has not been written or tested.** It is the smallest change that satisfies
the contract the CPU documents, but a second candidate is to make `CheckInterrupts` notify on every
evaluation while asserted rather than only on a transition. Which is right depends on what other
devices on this CPU rely on, and that has not been checked.

## A second, separate defect found on the way

`WriteCSR0`'s STOP branch calls `Reset()` and returns without `CheckInterrupts()`, and `Reset()`
sets `_irqAsserted = false` without firing `OnLANCEIRQ`. `LANRESET` goes the same way
(`ethIoMem.OnLANReset = () => lance.Reset()`). So a stop or a hardware reset while the line is
asserted leaves the CPU level asserted with the LANCE believing it is clear - the opposite
desync. The comment at `NDBusEthernetII.cs:3202` claims LANRESET makes "the Am7990 core fire
OnLANCEIRQ", and it does not.

**This is NOT the fault we chased** - it strands the flag FALSE and D100's is stuck TRUE - but it
is real and worth fixing while the area is open.

---

# PROVED ON THE MACHINES: the fix holds

RetroCore rebuilt with both commits and deployed to all three machines, hub capturing to a fresh
`after-fix.pcap`, all three booted from their own boot files with nothing typed.

## The test that used to kill it

```
X-C LIST-NAMES 102                                  -> D102's full table. Baseline good.
@COPY-FILE D102(SYSTEM).CHAT:PLNC,CHAT:PLNC         -> DONE IN 13.5 s
X-C LIST-NAMES 102                                  -> STILL ANSWERS
```

Before the fix that exact command hung for about 150 seconds and returned `NO ANSWER FROM REMOTE
SYSTEM`, and D100 was deaf from then on.

**Byte-count verified, not just error-free:** D100's `CHAT:PLNC` is 84 pages / 170712 bytes and
D102's copy came out 84 pages / 170712 bytes, written 12.30.51 on 24 August.

## Then it was stressed

Four transfers back to back - 170712 bytes, 230583 bytes, 42572 bytes, 170712 bytes again - all
completed, and `LIST-NAMES 102` still answered afterwards. The old failure needed only one.

## The wire, before against after

| | before the fix | after the fix |
|---|---|---|
| frames captured | 1074 over **6926 s** | **11046 over 248 s** |
| Data | 547 | **5914** |
| Acknowledge | 435 | **5125** |
| ConnectionRequest | **79** | **2** |
| DisconnectByNetworkService | **7** | **0** |
| ConnectionConfirm | 1 | 1 |

Before, D102 sent 79 connection requests into a machine that never answered and gave up seven
times. After, two requests set the link up and it simply worked: about 24 data frames a second
against 0.08 before, and **not one disconnect**.

## The fix

RetroCore branch `ethernet-ii-controller-fixes`:

 - **`47a52bd31`** - `HandleLANCEInterruptAck` re-asserts CPU level 2 while `IsInterruptActive`.
   This is the fault. `Cpu68K.CheckAndHandleInterrupts` clears the pending bit before the callback
   precisely so the device can put the level back, and nothing was putting it back.
 - **`e8e706354`** - `Am2990Lance.Reset()` now fires `OnLANCEIRQ` when it drops an asserted line,
   so STOP or LANRESET cannot strand the level the other way. A separate defect found on the way.
 - **`671411c24`** - a test pinning the reset half, proved to fail when the fix is reverted.

**158 ND Ethernet II tests pass.** The one red in the LANCE chip suite,
`Test_SunOS_RxAndInterrupt` on MCNT, fails identically with the fix reverted and is unrelated.

**Still not covered by a test: the IACK re-assert itself** - the path has no public seam and one
was not added to production code just to reach it. That is the half that mattered, so it is worth
doing.
