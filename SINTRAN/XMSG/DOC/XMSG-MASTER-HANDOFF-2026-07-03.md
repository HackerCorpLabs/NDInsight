# XMSG / TAD connect-to — MASTER handoff for an expert reviewer

**Goal:** a C# node ("node 102") connects to a real/emulated Norsk Data SINTRAN III
machine ("node 100") over an `nd100x --hdlc` TCP bridge and serves an interactive
SINTRAN terminal when a user on 100 types `@connect-to d102` — banner, prompt, menu,
and (goal) a login. It **works fully when 100's XMSG is freshly restarted**, but two
failures remain that we have **not** been able to root-cause ourselves. This document
gathers everything we have verified and states the exact open questions.

Every claim is tagged **VERIFIED** (observed live against machine 100, or in a capture),
**HYPOTHESIS** (a lead, not confirmed), or **UNKNOWN**. Where we don't know, we say so.

> **These are OUR node's bugs.** The task is NOT to fix SINTRAN, XROUT or XMSG — those
> are the ground truth. The task is to find which frame OUR node emits (or fails to
> emit / retransmit) that the real machine cannot process, by comparing our output to the
> captured **real-responder** frames.

---

## 0. Environment & artifacts

- **Link:** `nd100x --hdlc` presents machine 100 as a raw HDLC-over-TCP stream. Our node
  is a TCP client to `127.0.0.1:10362` (the bridge). Lossless, ordered byte stream.
- **Decoded captures:** `SINTRAN/XMSG/SRC/pcap-decode-report.txt` — 13 real captures
  fully decoded (SINTRAN header, XMSG sub-header, TAD/XROUT/ROUTING chains, raw hex).
  The raw `.pcapng` files are at `E:\Dev\Ronny\X25Emulator\pcap` (outside this repo).
- **Our code:** `SINTRAN/XMSG/SRC/`
  - `Xmsg.Protocol/Packet/XmsgEnvelope.cs` — the seed/epoch/channel model.
  - `Xmsg.Live/Tad/TadTerminalResponder.cs` — builds every reply frame (`BuildResponderFrame`).
  - `Xmsg.Live/LapbLink.cs` — the LAPB (link) layer.
  - `Xmsg.Live/XmsgNode.cs` — frame dispatch.
  - `Xmsg.Live.Runner/Program.cs` — the runnable node (timestamped logs).
  - 110 unit tests green (64 protocol + 46 live).
- **Companion docs (context):** `XMSG-PROTOCOL.md` (wire format, section 18.5 = seed model),
  `XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md`, `XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md`,
  `XMSG-RECONNECT-CRASH-QUESTION-2026-07-03.md`, `TAD-CONNECT-FIELD-ANALYSIS.md`.

---

## 1. Frame format (VERIFIED from all 13 captures)

Three stacked layers. Byte offsets below are into the SINTRAN payload (after LAPB
de-stuffing / FCS strip).

### 1.1 LAPB (link) layer
- **Modulo-8 ABM, balanced** (both stations send SABM). Byte-stuffed `0x7E` framing,
  FCS-16 poly `0x8408`.
- Control: SABM `0x3F`, UA `0x73`, RR `0x01 | (N(R)<<5)`, I-frame `(N(R)<<5) | (N(S)<<1)`.
- Address: `0x01` for link setup (SABM/UA), `0x09` for data (RR/I).
- **Each link-management frame carries the sender's node number as a 2-byte info field:**
  `00 64` = 100, `00 66` = 102, `00 67` = 103. This is the **physical neighbour** on the
  link — always 100↔102 on our link, *even when the logical traffic is for node 103 relayed
  through 100* (see 1.3).

### 1.2 SINTRAN header (12 bytes)
```
off 0-1 : markers        0x21 0x13   (0x12 relay variant seen; 0x13 normal)
off 2   : packetType     0x00
off 3   : (length/aux)   0x0E on Data
off 4-5 : DEST node      big-endian word   <-- 102 = 0x0066
off 6-7 : SRC  node      big-endian word   <-- 100 = 0x0064
off 8-9 : Flags1         datagram sequence (per direction, +1 per Data frame)
off 10-11: Flags2        = XMCSM >> 16   (0x0400 on connect-data, 0x0001 on ACK, 0x0108 on TAD)
off 12  : proto/channel  D9..DE   (the "channel", epoch-dependent — see section 2)
```
**Header order is DEST first, SRC second.** VERIFIED identical across every capture.

### 1.3 Node IDs — the two-node distinction (VERIFIED)
A node ID is just the **decimal node number as a 16-bit big-endian word**
(100=`0x0064`, 102=`0x0066`, 103=`0x0067`). It appears **four times** per Data frame:
SINTRAN header dst (off 4-5) + src (off 6-7), and the XMSG sub-header `dstNode:dstPort`
+ `srcNode:srcPort`.

- **Direct** (`conn-to-d102-from-100.pcapng`, report line 2012): header `100 -> 102`,
  sub-header `src 100:683 -> dst 102:0`.
- **Relayed** (`conn-to-102-from103-via100.pcapng`, report line 6): header `103 -> 102`,
  sub-header `src 103:581 -> dst 102:0`. **The source is the logical originator (103),
  NOT the relay (100).** On the LAPB link it is still 100↔102.

| Layer | Whose ID | 103-via-100 shows |
|---|---|---|
| LAPB info field | physical neighbour | **100** |
| SINTRAN + XMSG endpoints | logical endpoints | **103** ↔ 102 |

Our responder mirrors this correctly (`TadTerminalResponder.cs:525`):
`DestinationNode = request.Header.SourceNode` (echoes 100 or 103),
`SourceNode = _nodeNumber` (102). **VERIFIED correct** — not hardcoded to 100.

### 1.4 XMSG sub-header (Data frames), after the proto byte
```
counter (1)  frameFlags (1)  role (1)  dstNode(2) dstPort(2)  srcNode(2) srcPort(2)
XMCSM (4)  pad (1)  XMLEN (1)  <payload: TAD / XROUT / ROUTING chain>
```
`XMCSM` is the 32-bit control word. `Flags2 == XMCSM>>16`. The high two bytes drive the
channel (section 2).

---

## 2. The seed / epoch / channel model (VERIFIED against all 601 captured Data frames)

Per link there is one **seed** byte, learned from 100's connect frame:
```
seed = (Counter + Flags1 + (Flags2 & 0xFF)) & 0xFF          ; 100<->102 = 0x14, 100<->103 = 0x13
```
Then for EVERY frame on that link:
```
baseLow = (seed - (Flags2 & 0xFF)) & 0xFF                   ; Flags2 == XMCSM >> 16
Counter = (baseLow - Flags1) & 0xFF
epoch   = (Flags1 - baseLow + 0xFF) >> 8                    ; # of Counter wraps
Channel = 0xDE - (XMCSM >> 24) - epoch
```
So the channel a frame rides **depends on the epoch**. For terminal data
(`XMCSM 0x01080000`): channel **DD** at epoch 0, **DC** at epoch 1, **DB** at epoch 2.
A fresh responder (epoch 0) sends terminal data on **DD** (an earlier DB reading was an
epoch-2 capture artifact).

**HYPOTHESIS (2 data points):** the seed encodes the node number —
`seed = 122 − responderNode` (122−102=0x14, 122−103=0x13). Unconfirmed; we currently
*learn* the seed live so correctness doesn't depend on it. A `100↔104` capture would
confirm or kill this.

### Sequence persistence (VERIFIED)
- Our outgoing `Flags1` (102→100) is one counter, **+1 per Data frame**, and persists
  per remote node **across our process restarts**. It advances **only on 100's delivery
  ACK** (subtype 0x03 echoing the Flags1 it received) — never on send — so it can never
  run ahead of what 100 actually received. File: `FileResponderSequenceStore`.
- 100's expected-from-us (**XSRSQ**) is persistent on 100's side and independent of 100's
  own send counter.
- **Behind** sequence ⇒ 100 silently drops the frame. **Ahead** ⇒ recoverable **XENSE**
  (subtype 0x07, Flags2 `0xFFDE` = −34). We recover by stepping our accept Flags1 down one
  per XENSE until 100 accepts (`ResyncAcceptDown`).
- A **ReachabilityRequest** from 100 (subtype 0x19, Flags1 `0xFFFF`) = 100's XMSG-restart
  signal ⇒ its sequences zero ⇒ we reset our stored Flags1 to 0. A bare HDLC link restart
  does NOT send one.

---

## 3. What WORKS — epoch 0, VERIFIED live end-to-end

With 100's XMSG freshly (re)started, a full session runs:
1. `@connect-to d102` → connect handshake → `=== CONNECTION ESTABLISHED / TAD LOGICAL
   UNIT NO: 768 ===`.
2. Our accept (proto D8/DA per epoch, role 0x40) is accepted; we send session-setup
   (port-assign) + **DUMM**; 100 then drives TMOD/TTYP.
3. We answer the terminal-setup with the MOTD burst (ctrl 0x20, RESE, RESE, MOTD) →
   the **RETROCORE MOTD banner renders on 100's console**.
4. `ENTER` → `#` prompt; menu commands reply live: `1`→time, `2`→date, `3`→echo. Each
   reply appends an **RFI** (ready-for-input, TAD 0x02) flow-control credit; without it
   100 sits idle after one line.
5. After ~1 minute SINTRAN ends it ("TAD not logged in") with a **DCON** (TAD 0x09),
   which we receive and close cleanly.

Every success had 100's connect at a **LOW** sequence (`Flags1 0x0000–0x0009`,
`Counter ~0x14`, connect channel DA) = **epoch 0**.

---

## 4. OPEN PROBLEM A — terminal reply stalls (LAPB), recovered but root cause UNKNOWN

**VERIFIED symptom:** during an epoch-0 session, some terminal replies (reliably the
`help` menu and the echo for command `3`) stalled. Trace:
```
[TX]  a=0x09 I ns=0 nr=0  body=<help menu>      <- we send the reply
[rx-raw] a=0x09 RR nr=0                          <- 100 asks for ns=0 again
[rx-raw] a=0x09 RR nr=0                          <- ...and again (stuck)
(we never retransmitted)                          <- DEADLOCK
```
100 is doing the correct LAPB thing: repeating `RR N(R)=0` to request the frame it did
not accept. Our LAPB had **no retransmit** (RR-only; no REJ/RNR; timer-based retransmit
but the runner passes no tick), so we never resent → both sides wait forever.

**FIX applied (commit b832cdd):** in `LapbLink.HandleSupervisory`, when an RR's `N(R)`
is behind `V(S)` **and repeats** (peer genuinely stuck, `_lastBehindNr` guard), retransmit
`_lastUnackedBody`. This **RECOVERS** the deadlock (VERIFIED locally by test; awaiting a
live re-run).

**OPEN QUESTION A — the root cause we do NOT know:**
> Why does 100 fail to accept our reply I-frame **the first time**, on a lossless ordered
> TCP stream? Candidates we cannot distinguish: (a) an FCS/byte-stuffing error our encoder
> makes specifically on the **larger** reply frames (menu > single echo); (b) a mod-8
> **window edge** (do we violate a k=window limit / send while 100's window is closed?);
> (c) 100 legitimately REJs and our RR-only handler misreads it. The retransmit masks the
> symptom; it does not explain the loss. **We need the frame that 100 rejects decoded and
> compared to a captured real-responder frame of the same class.**

Our LAPB is also **NON-STANDARD** in one place (prime suspect): on the FIRST I-frame after
(re)establishing we **adopt the peer's** sequence (`V(R)=frame.N(S)`, `V(S)=frame.N(R)`)
instead of resetting to 0, on the theory that 100 keeps its V(S)/V(R) across our TCP
reconnect. Standard LAPB resets both sides to 0 on SABM. If that assumption is wrong, we
ack/deliver at the wrong sequence — a sliding-window desync.

---

## 5. OPEN PROBLEM B — reconnect crashes 100's XMSG at epoch ≥ 1 (cause UNKNOWN)

**VERIFIED symptom:** reconnecting **without restarting 100** (so 100's sequence has
climbed) makes 100's connect arrive at a **HIGH** sequence:
```
100 connect: Flags1=0x0015  Counter=0xFF  Flags2=0x0400  proto=D9  XMCSM=0x04000041  port=0x02B5
  -> baseLow low, Flags1 > baseLow  => EPOCH 1, connect channel D9
```
Our node answers (seed model):
```
secure ACK : 2113000300640066 0015 0001 DD 09          (channel DD = connect D9 + 4)
accept     : 2113000E00640066 0015 0400 D9 FF 2100 8640 0064 02B5 0066 0156 04000041 00 08 01020000 0202000A
```
Shortly after, 100 prints `SYSTEM MALFUNCTION / PERF_CONNCT:3`, then
`XMSG fatal error … code 24B … Physical address 134265B`, `XMFIDO: ABORTS, XMSG not
running`. Sometimes preceded by `XROUT: Link restarted, LUN 1360B`.

**The one hard fact:** every SUCCESS was **epoch 0**; every crash-on-reconnect is
**epoch ≥ 1**. We have **never** validated our epoch-≥1 frame generation against the live
machine — we produce those channels/counters purely from the (capture-verified) formula.

**Reference for validation:** `conn-to-d102-from-100.pcapng` (report lines 2012–2929)
contains **real responder** frames at epoch 2 (102→100, Base `0x0214`/`0x020C`) and 100's
frames at epoch 1 (Base `0x010C`). Compare our epoch-1 output to these.

**OPEN QUESTIONS B:**
1. **Accept channel at epoch ≥ 1.** In the capture the connect is D9 and the real
   responder's accept is on **D8** (Base `0x0214`, epoch 2). Our epoch-1 accept comes out
   on **D9** (Base `0x0114`). Is the accept expected on the connect's own channel, on
   `connect−1`, or strictly on the formula's epoch-derived channel? Does a `Counter=0xFF`
   (epoch-1 wrap) accept crash a real 100?
2. **Terminal-data channels/counters at epoch 1** (DC, `0xF*` counters) — do they match
   what a real responder sends when its Base is `0x01xx`?
3. **Off-by-one at the epoch boundary** — our formula puts `Flags1=0x0014`→epoch 0
   (`Counter 0x00`) and `Flags1=0x0015`→epoch 1 (`Counter 0xFF`). Is the boundary correct
   against the three live counter-wrap events in the captures?

---

## 6. OPEN PROBLEM C — login flow NOT built (needed to beat the 1-minute timeout)

SINTRAN policy: **"a TAD not logged in cannot be held for more than 1 minute"** → it
DCONs us. To keep the session we must complete the login handshake so SINTRAN considers
the session logged in. We have the opcodes (from captures / `TAD-CONNECT-FIELD-ANALYSIS.md`)
but have **not built the state machine**:
- **SYCN (0x13)** sync steps `0002 → 0003 → 0006 → 000A` (`000A` = logged in).
- **CESC (0x0E)** `00 → 01`.
- **ECKM (0x03)** echo control: `FF` = echo off (password entry) then `01`.
- **RFI (0x02)** = ready-for-input credit — already appended to every input-expecting reply.

**OPEN QUESTION C:** the exact ordering/interleave of SYCN/CESC/ECKM with the PASSWORD
prompt, and which side initiates each step. Needs validation against a login capture
(`li-syst-tad-103.pcapng` may contain a login sequence — report line ~7771).

---

## 7. What we need from the reviewer (priorities)

1. **Problem A root cause** — decode the reply I-frame 100 rejects; is it FCS/stuffing on
   larger frames, a window violation, or a misread REJ? Is the peer-sequence **adoption**
   (section 4) correct on a reconnect where 100's link never went down?
2. **Problem B** — the exact accept channel/counter rule at epoch ≥ 1, and any deviation of
   our epoch-1 frames from the captured real-responder frames (that deviation is the crash).
3. **Problem C** — the login handshake ordering.

All raw evidence is in `SRC/pcap-decode-report.txt`; our exact frame generation is in
`XmsgEnvelope.cs` + `TadTerminalResponder.BuildResponderFrame` + `LapbLink.cs`.

---

*Written 2026-07-03. Consolidates: XMSG-RECONNECT-CRASH-QUESTION, XMSG-CHANNEL-SEQUENCE-
ANALYSIS, XMSG-SEQUENCE-RESTART-ANSWER, TAD-CONNECT-FIELD-ANALYSIS, XMSG-PROTOCOL section 18.5.
Every failure documented here is a defect in OUR node, to be found by comparing our output
to the captured real-responder frames — not by changing SINTRAN/XROUT/XMSG.*
