# XMSG connect-to — reconnect crashes 100's XMSG (epoch ≥ 1). Validation handoff.

**For a reviewer with the pcaps and our capture decode.** A C# node (node 102) that serves an
interactive SINTRAN terminal to machine 100 **works fully when 100's XMSG is freshly restarted**
(login MOTD renders, menu commands respond), but **crashes 100's XMSG (`XXPER`, error 24B, physical
address 134265B) when we reconnect without restarting 100.** All bugs here are in OUR node's frame
generation — the task is to find which frame we emit that 100 cannot process, by validating our
frames against the captured real-responder frames.

Everything marked VERIFIED is from live logs against machine 100. UNKNOWN / HYPOTHESIS are marked.

---

## 1. What WORKS (VERIFIED live) — epoch 0 only

With 100's XMSG freshly (re)started, a full session runs end to end:
- connect handshake → `=== CONNECTION ESTABLISHED / TAD LOGICAL UNIT NO: 768 ===`
- the RETROCORE MOTD banner renders on 100's console
- `ENTER` → `#` prompt, and menu commands reply live: `1`→time, `2`→date, `3`→echo
- SINTRAN ends it after 1 minute ("TAD not logged in") with a `DCON`, received cleanly

Every one of those successes had 100's connect at a **LOW** datagram sequence — `Flags1` `0x0000`–
`0x0009`, `Counter` `~0x14`, connect channel **DA**. In the envelope model (below) that is **epoch 0**
(`Base = Flags1 + Counter < 0x0100`).

## 1a. The LAPB (link) layer — full detail, and where it is NON-STANDARD

The crash sometimes follows `XROUT: Link restarted, LUN 1360B` (a link-management restart on 100),
so the LAPB layer is a prime suspect on reconnect. Our implementation
(`Xmsg.Live/LapbLink.cs`, `Xmsg.Live/Seam/LapbLinkAdapter.cs`):

- **Modulo-8 ABM.** Control encodings: SABM `0x3F`, UA `0x73`, RR `0x01 | (N(R)<<5)`, I-frame
  `(N(R)<<5) | (N(S)<<1)`. Link-setup address `0x01` (SABM/UA), data address `0x09` (RR/I). Each
  link-management frame carries the sender's node number as its 2-byte info field (`00 64` = 100,
  `00 66` = 102).
- **Balanced link, both stations SABM (VERIFIED from raw captures).** On receiving a peer SABM while
  not yet synced we reset V(S)=V(R)=0, send OUR OWN SABM **once per establishment episode**
  (`_ownSabmSent` guard), then UA, then an RR. Re-emitting our SABM per received SABM caused a
  restart flood, hence the once-only guard. A SABM that arrives AFTER we are synced is treated as a
  genuine restart (peer HDLC reset) and re-arms the guard.
- **NON-STANDARD sequence adoption (this is the likely bug surface).** On the FIRST I-frame after
  (re)establishing, instead of using our own V(S)/V(R)=0, we **adopt the peer's**:
  `V(R) = frame.N(S)`, `V(S) = frame.N(R)`. Rationale in the code: *"the kernel keeps its V(S)/V(R)
  across our TCP reconnect, so a fresh SABM does not reset its sequence."* This is NOT standard LAPB
  (a SABM should reset both sides to 0). If that assumption is wrong — or wrong specifically on a
  reconnect where 100's link is still up and its V(S)/V(R) are mid-window — we would ack/deliver at
  the wrong sequence, which is exactly a sliding-window desync.
- **Acknowledgement policy:** we send an RR carrying the current V(R) after every received I-frame
  (one RR per I-frame). We do **NOT** implement REJ or RNR — only RR. We do not pipeline; each of our
  I-frames is followed by an RR. **We never NAK.** A lost/duplicate/out-of-window frame from 100 is
  simply not delivered and gets an RR with the unchanged V(R) — we rely on 100 to retransmit. If
  100 uses REJ/RNR or a window > our handling, we may mishandle it.
- **Retransmit:** timer-based (`Tick`), but the live runner passes NO keepalive/tick, so we never
  retransmit on a timer. **VERIFIED FAILURE + FIX:** terminal replies (echo for `3`, the `help` menu)
  reliably stalled — we sent the reply I-frame (e.g. `ns=0`), but 100 kept answering
  `RR nr=0` (asking for it) and we never resent → deadlock. Fixed 2026-07-03: on an RR whose `N(R)`
  is behind `V(S)` **and repeats** (100 genuinely stuck), retransmit the last unacked frame. Whether
  the *root cause* of 100 not receiving the frame is a genuine loss, an FCS/stuffing error on the
  larger reply frames, or a mod-8 window edge, is still UNKNOWN — the retransmit only *recovers* it.
  **Open: why is 100 dropping our reply I-frame in the first place, on TCP (which is lossless)?**

**Reconnect-specific LAPB symptom (VERIFIED):** when our process restarts while 100's link is still
up, 100 re-sends the previous session's pending I-frame (a `DCON`) and/or repeats SABM; we saw a
**991-SABM storm** before traffic resumed, and sometimes `XROUT: Link restarted`. So the reconnect
path clearly stresses the link layer. Whether the storm merely delays, or actually corrupts state
that later crashes XMSG, is UNKNOWN.

**Two LAPB questions for the reviewer:**
1. Is the **peer-sequence adoption** correct, and correct *on a reconnect* where 100's link never
   went down (its V(S)/V(R) are at arbitrary mid-window values, not 0)? Should a fresh SABM from us
   force 100 to reset to 0, and if so are we failing to make it do that?
2. Does 100 use **REJ/RNR / a real sliding window** that our RR-only, no-retransmit, no-NAK
   implementation mishandles — such that on reconnect it declares the link restarted?

## 2. The verified envelope/sequence model (context)

Per link one **seed** byte (`0x14` for 100↔102, learned as `(Counter + Flags1 + (Flags2 & 0xFF)) &
0xFF`). Per direction one **Flags1** (datagram sequence, +1 per Data frame). Then, for every frame:

```
baseLow = (seed − (Flags2 & 0xFF)) & 0xFF        ; Flags2 == XMCSM >> 16
Counter = (baseLow − Flags1) & 0xFF
epoch   = (Flags1 − baseLow + 0xFF) >> 8          ; # of counter wraps; 0 while Flags1 ≤ baseLow
Channel = 0xDE − (XMCSM >> 24) − epoch
```

VERIFIED against all 601 captured Data frames. So the **channel a frame rides depends on the epoch**:
for terminal data (`XMCSM 0x01080000`) it is DD at epoch 0, DC at epoch 1, DB at epoch 2.

- Our outgoing `Flags1` (102→100) persists per node across our restarts, advanced only on 100's
  delivery ACK (so it never runs ahead of what 100 received). On a ReachabilityRequest from 100
  (its XMSG-restart signal) we reset it to 0x0000.
- 100's expected-from-us and 100's own send sequence are **independent** counters (VERIFIED); a
  behind-sequence frame is silently dropped, an ahead-sequence frame gets a recoverable XENSE
  (`0x07`, Flags2 `0xFFDE`).

## 3. The FAILURE (VERIFIED symptom; cause UNKNOWN)

Reconnecting while 100's XMSG stays up (its sequence has climbed): 100's connect now arrives at a
**HIGH** sequence, e.g. (real log):

```
100 connect: Flags1=0x0015  Counter=0xFF  Flags2=0x0400  proto=D9  XMCSM=0x04000041  port=0x02B5
  -> Base = 0x0015 + 0xFF = 0x0114  => EPOCH 1, connect channel D9
```

Our node answers (real log, seed model):
```
secure ACK : 2113000300640066 0015 0001 DD 09          (channel DD = connect D9 + 4)
accept     : 2113000E00640066 0015 0400 D9 FF 2100 86 40 0064 02B5 0066 0156 04000041 00 08 01020000 0202000A
             -> Flags1=0x0015, Counter=0xFF, channel D9 (epoch 1)
```

Shortly after, 100 prints `SYSTEM MALFUNCTION / PERF_CONNCT:3`, then `XMSG fatal error … code 24B …
Physical address 134265B`, and `XMFIDO: ABORTS, XMSG not running`. (The `XROUT: Link restarted`
line sometimes precedes it.) **We have not captured the exact frame at the moment of the crash — the
runner log was cut off — so which of our epoch-1 frames it dies on is not yet pinned.**

**The one hard fact:** every success was **epoch 0**; every crash-on-reconnect is **epoch ≥ 1**. We
have never validated our epoch-1 frame generation against the live machine. Our code produces the
epoch-1 channels/counters purely from the (verified-on-captures) formula, e.g. the terminal-data
burst would move DD→DC and the counters into the `0xF*` wrap range.

## 4. The question to validate against the captures

**Does our node generate byte-correct frames when 100 is at epoch ≥ 1, or does the crash mean our
epoch/counter/channel handling is wrong somewhere above epoch 0?**

Concretely, using the capture `conn-to-d102-from-100.pcapng` (decode `SRC/pcap-decode-report.txt`,
lines 2012–2929), which contains **real** responder frames at epoch 2 (102→100, `Base 0x0214`/`0x020C`)
and 100's frames at epoch 1 (`Base 0x010C`):

1. **Accept at epoch 1.** In the capture the connect is on D9 and the responder's accept is on **D8**
   (`Base 0x0214`, epoch 2). Our epoch-1 accept comes out on **D9** (`Base 0x0114`). **Is an accept
   expected on the connect's own channel (D9), or on `connect−1` (D8), or strictly on the formula's
   epoch-derived channel? Does a `Counter=0xFF` accept (epoch-1 wrap) crash a real 100?** We only
   ever validated an epoch-0 accept (channel DA, `Counter 0x14`).

2. **Do the terminal-data channels at epoch 1 (DC) and their `0xF*` counters match** what a real
   responder sends when its Base is in `0x01xx`? The capture's 100→102 data at epoch 1 is on DC with
   `Counter 0x12`-ish — compare our computed epoch-1 values.

3. **Is there an off-by-one at the epoch boundary** (`Flags1` crossing `baseLow`, where `Counter`
   wraps `0x00→0xFF` and epoch steps 0→1)? Our formula puts `Flags1=0x0014` at epoch 0 (`Counter 0x00`)
   and `Flags1=0x0015` at epoch 1 (`Counter 0xFF`). Verify that boundary against the three live
   counter-wrap events the captures contain.

4. **Secondary:** on reconnect, 100 re-sends the previous session's `DCON` and our fresh `SABM`
   collides with 100's still-live link, producing a SABM storm and sometimes `XROUT: Link restarted`.
   Is our LAPB re-establishment mishandling a peer whose link is already up? (Believed secondary to
   the epoch issue, but noted.)

## 5. Data available

- Full working (epoch 0) session log and the failing (epoch 1) reconnect log (both in this
  conversation; can be pasted into the repo on request).
- `SRC/pcap-decode-report.txt` — all 13 captures decoded; the epoch-0/1/2 responder frames.
- The model + implementation: `Xmsg.Protocol/Packet/XmsgEnvelope.cs`,
  `Xmsg.Live/Tad/TadTerminalResponder.cs` (`BuildResponderFrame`), and the analysis docs
  `XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md`, `XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md`.

**What we need:** confirmation of the exact accept channel/counter rule at epoch ≥ 1, and whether
any of our epoch-1 frame values deviate from the captured real-responder frames — that deviation is
the bug that crashes 100.

---

*Written 2026-07-03. The crash is caused by a frame OUR node emits; this doc exists to find which one
by comparing our epoch-≥1 output to the captured real-responder frames. No blame on XMSG/XROUT — the
defect is ours.*
