# XMSG TAD terminal bring-up (MOTD) — the unsolved channel/sequence problem

**Purpose of this document.** A self-contained, precise problem statement for a fresh analyst
(human or LLM). It states what is **VERIFIED**, what is **ASSUMED**, and what is **UNKNOWN**, with
exact byte values from the packet captures and from live tests against a real SINTRAN III machine.
The goal is to determine the rule that decides the **sub-protocol channel (Protocol ID)** and the
**datagram sequence (Flags 1)** for the terminal-data phase of a SINTRAN `connect-to` session, so a
freshly-started responder node can send the MOTD/login without machine 100 rejecting or crashing.

Everything marked **ASSUMPTION** or **IDEA** is *not* established fact — treat with suspicion.

---

## 1. Context and goal

- **System:** Norsk Data SINTRAN III, the XMSG inter-node message system, over HDLC/LAPB.
- **We are building** a C# node (call it **node 102**) that connects to a real/emulated ND
  machine (**node 100**) over a `nd100x --hdlc` TCP bridge, and participates in XMSG well enough
  that a user on 100 can type `@connect-to d102` and get a terminal session **hosted by our node**
  (MOTD banner, `PASSWORD:` prompt, a menu). Node 102 is the *responder/host*; node 100 is the
  *terminal/client*.
- **What already works (VERIFIED live against 100, no crash):**
  1. LAPB link establishment (balanced, both sides SABM).
  2. Reachability request/reply.
  3. `list-route` (XSGSY) replies.
  4. The `connect-to` **handshake**: 100 sends a connect, we reply with a **connect-accept**; 100
     sends a **session-setup**, we reply with a **port-assign**. 100 delivery-ACKs both of our
     replies. No crash.
- **What is BLOCKED:** the very next step — the responder's **terminal-data burst** (a `DUMM`
  no-op, then `RESE`/control frames, then the MOTD as `BDAT` text). Every attempt to send the first
  terminal-data frame is either sequence-rejected (recoverable) or **crashes 100's XMSG** (fatal).

---

## 2. Wire format essentials (VERIFIED)

Reference: `SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md`, and the FCS-verified peer spec
`SINTRAN/XMSG/OLD/xmsg-hdlc-protocol.md`.

### 2.1 SINTRAN header (13 bytes) — every XMSG information field starts with it

| Offset | Field | Notes |
|---|---|---|
| 0 | Marker 1 = `0x21` | |
| 1 | Marker 2 = `0x13` (normal) / `0x12` (relay) | |
| 2 | Packet type = `0x00` | |
| 3 | **Subtype** | `0x0E` Data, `0x03` Ack, `0x13` ReachReply, `0x19` ReachReq, **`0x07` NetworkError** |
| 4-5 | Destination node (big-endian) | e.g. `0x0064` = 100, `0x0066` = 102 |
| 6-7 | Source node | |
| 8-9 | **Flags 1** | the per-direction **datagram sequence**, or `0xFFFF` on broadcast |
| 10-11 | **Flags 2** | frame-class word; for a NetworkError it is a **negative XE\* code** |
| 12 | **Protocol ID (channel)** | `0xD8`..`0xDE` — the crux of this problem |

### 2.2 XMSG sub-header (19 bytes) — present on Data (0x0E) frames, right after the header

| Offset (from sub-header start) | Field | Notes |
|---|---|---|
| 0 | **Counter** | per-direction byte, generally decrements |
| 1-2 | Marker = `0x21 0x00` | constant |
| 3 | Frame flags | e.g. `0x86` setup, `0x92`/`0x96` data |
| 4 | Role | `0x40` responder setup, `0x00` data-phase, `0xE4`/`0x84` asker |
| 5-6 | XMDSY dest system | |
| 7-8 | XMDPT dest port | |
| 9-10 | XMSSY src system | |
| 11-12 | XMSPT src port | port = `(logical << 7) | low7` (magic-number encoding) |
| 13-16 | **XMCSM** control/service word (32-bit) | its **top byte** classifies the frame |
| 17 | Pad = `0x00` | |
| 18 | XMLEN user-data length (low byte) | |

Then the payload (a TAD message chain, or XROUT letter).

### 2.3 XMCSM values seen (VERIFIED)

- `0x04000041` — SYSTEM-TAD connect (the `*TADADM` letter).
- `0x04000000` — session-setup / port-negotiation.
- `0x01080000` — **terminal data** (BDAT, DUMM, RESE, MOTD…). *This is the one that must land on DB.*
- `0x00080000` — a terminal control class (the `0x20`/`ESCA` frames).

### 2.4 Protocol IDs (channels) observed

`0xDE` ROUTING, `0xDD` TAD, `0xDC` DC (terminal data), `0xDB` DB (terminal data variant),
`0xDA` PAD, `0xD9`, `0xD8`. In a session these are used as a *pool* of per-stream channels.

### 2.5 Error codes (VERIFIED from the peer spec + memory)

Subtype `0x07` = network error/reject. Flags 2 = a negative 16-bit XE\* code:
- `0xFFED` = **XEIMA** −19 (invalid magic number)
- `0xFFDE` = **XENSE** −34 (**sequence error**) ← the one we keep hitting

A `0x07` reject is **recoverable** (100's XMSG stays up). A separate failure mode, **XXPER**
(`XMSG error 24B, physical address 134265B`), is **fatal** — XMSG dies ("XMSG not running", XMFIDO
aborts) and must be restarted on the ND.

---

## 3. The envelope model (VERIFIED to *fit* captures; does NOT predict fresh-node behaviour)

Reference: `XMSG-PROTOCOL.md` section 18.5; implemented in
`SINTRAN/XMSG/SRC/Xmsg.Protocol/Packet/XmsgEnvelope.cs`.

```
Base    = Flags1 + Counter                     (16-bit)
Channel = 0xDE − (XMCSM >> 24) − (Base >> 8)
```

Within one stream, **Flags1 increments and Counter decrements in lockstep, holding Base (and
therefore the channel) constant.**

**This formula reproduces EVERY frame in the captures** — see section 5. It is verified by
`Xmsg.Protocol.Tests/EnvelopeChannelTests.cs`. **BUT** section 6 shows it does not predict what a
real 100 accepts from our *freshly-started* node, because the channel depends on the *absolute*
sequence value, which a fresh node cannot set the way a long-running ND can.

---

## 4. Reference material — where to look

| Artifact | Path | What it contains |
|---|---|---|
| Main protocol spec | `SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md` | wire format, envelope model (18.5) |
| Peer/friend spec (FCS-verified) | `SINTRAN/XMSG/OLD/xmsg-hdlc-protocol.md` | independent decode, XE codes, port<<7 |
| **Decode report** | `SINTRAN/XMSG/SRC/pcap-decode-report.txt` | every captured frame decoded (line refs below) |
| The seam plan + live findings | `SINTRAN/XMSG/DOC/XMSG-TRANSPORT-SEAM-PLAN.md` | phase history, "Phase 6 live findings" |
| pcap files | `E:\Dev\Ronny\X25Emulator\pcap\*.pcapng` | raw captures |
| TAD message formats | `SINTRAN/XMSG/TAD/` and `SubProtocol/TadOpcodes.cs` | TAD opcode table |

**Key capture:** `conn-to-d102-from-100.pcapng` — a real `connect-to d102` where **100 is the
client and 102 is the host**, i.e. exactly our roles. Decode report lines **2012–2929**:
- lines 2012–2456 = 100→102 direction (frames 1–49)
- lines 2457–2929 = **102→100 direction (frames 50–98)** ← the responder side we must reproduce

---

## 5. The captured ground truth (VERIFIED byte values)

All from `conn-to-d102-from-100.pcapng`. In this capture the connect channel is **`0xD9`** and both
machines had been running (their sequences are already "high").

### 5.1 The responder (102→100) frames — what we must reproduce

| Frame | Subtype | Proto | Flags1 | Counter | Base | XMCSM | Role | Meaning |
|---|---|---|---|---|---|---|---|---|
| 50 | Ack 0x03 | DD | 0x00F8 | (trailing 0x26) | — | — | — | ACK of connect |
| 51 | Data | **D8** | 0x012F | 0xE5 | **0x0214** | 04000041 | 40 | connect-accept |
| 53 | Data | **D8** | 0x0130 | 0xE4 | **0x0214** | 04000000 | 40 | port-assign |
| 54 | Data | **DB** | 0x0131 | 0xDB | **0x020C** | 01080000 | 00 | **DUMM** (first terminal-data) |
| 57 | Data | **DC** | 0x0132 | 0xDA | **0x020C** | 00080000 | 00 | control `0x20` |
| 58 | Data | **DB** | 0x0133 | 0xD9 | **0x020C** | 01080000 | 00 | RESE |
| 62 | Data | **DB** | 0x0135 | 0xD7 | **0x020C** | 01080000 | 00 | **MOTD** (BDAT banner) |
| 66 | Data | **DB** | 0x0137 | 0xD5 | **0x020C** | 01080000 | 00 | PASSWORD prompt |

Exact bytes (frame 54, DUMM): `2113000E0064006601310108DBDB21009200006402AB006604C20108000000021800`
Exact bytes (frame 62, MOTD): begins
`2113000E0064006601350108DBD721009600006402AB006604C201080000007C0004030100000003010101600D0A...`
and the BDAT payload decodes to the ASCII banner
`"…22.27.22 8 APRIL 1998 / SINTRAN III - VSX/500 L / --- RETROCORE EMULATED L ID:102 --- / ENTER "`.

**Check the envelope formula on these:**
- Frame 51: Base 0x012F+0xE5 = **0x0214**; `0xDE − 0x04 − 0x02 = 0xD8` ✓
- Frame 54: Base 0x0131+0xDB = **0x020C**; `0xDE − 0x01 − 0x02 = 0xDB` ✓
- Frame 57: Base 0x0132+0xDA = **0x020C**; `0xDE − 0x00 − 0x02 = 0xDC` ✓

**KEY:** Flags1 runs **continuously** 0x012F→0x0130→0x0131→0x0132… across accept, port-assign, and
data, and the **Counter jumps** (0xE4→0xDB) between the control stream (Base 0x0214) and the data
stream (Base 0x020C). So the responder maintains **one monotonic Flags1 sequence** but **two Base
values** (0x0214 control, 0x020C data). The channel is fully determined by Base and XMCSM.

### 5.2 The client (100→102) data frames — 100's own data channel

| Frame | Proto | Flags1 | Counter | Base | XMCSM | Meaning |
|---|---|---|---|---|---|---|
| 6 | **DC** | 0x00FA | 0x12 | **0x010C** | 01080000 | TMOD/TTYP/DESC/OPSV |
| 7 | **DD** | 0x00FB | 0x11 | **0x010C** | 00080000 | ESCA |
| 10 | **DC** | 0x00FC | 0x10 | **0x010C** | 01080000 | RECO |

- Frame 6: Base 0x00FA+0x12 = **0x010C**; `0xDE − 0x01 − 0x01 = 0xDC` ✓
- Frame 7: Base 0x00FB+0x11 = **0x010C**; `0xDE − 0x00 − 0x01 = 0xDD` ✓

**Observation (VERIFIED):** the two directions differ by Base high byte — 100→102 data has
`Base >> 8 = 1` (→ DC/DD), 102→100 data has `Base >> 8 = 2` (→ DB/DC). Because 100's Flags1 was
`~0x00FA` and 102's was `~0x0131`, i.e. **both sides simply had high running sequences**, and the
different high bytes fall out of `Base = Flags1 + Counter`.

---

## 6. What we tried LIVE and what machine 100 answered (VERIFIED)

Our live node's sequence starts **fresh (~0x0000)** because it has no prior XMSG traffic. In each
live connect, 100's connect Flags1 was `0x0000`, its counter `~0x14`; 100's port varied per connect
(`0x02BC`, `0x02DD`, `0x02F7`, …). We assign our session port `0x0211`.

The **echo** accept/port-assign (Flags1 echoes the connect's `0x0000`/`0x0001`, channel echoes the
connect's DA) is **accepted** by 100 — it ACKs them and the session advances to the terminal phase.
The problem is purely the **first terminal-data frame** (the DUMM).

| # | DUMM Flags1 | Counter | Base | Derived channel | 100's response |
|---|---|---|---|---|---|
| 1 | 0x0131 (capture-absolute, a **jump**) | 0xDB | 0x020C | **DB** | **XENSE** `0xFFDE` (recoverable). Reject frame: `2113000700660064` `0131` `FFDE` `DD` `0B` — subtype 0x07, echoes our Flags1, on channel DD, trailing 0x0B. **No crash.** |
| 2 | 0x000C (continuous, low) | 0x08 | 0x0014 | **DD** | **XXPER fatal crash** (XMSG dies) |
| 3 | 0x000C (continuous) | 0xFF | 0x0101 | **DC** | **XXPER fatal crash** |
| 4 | *(accept itself)* 0x012F on **D8** | 0xE5 | 0x0214 | D8 | **XENSE** on the **accept**: `2113000700660064` `012F` `FFDE` `DD` `0D`. 100 rejected our high-sequence accept. **No crash**, but the session never advanced. |

### 6.1 What these four results establish (VERIFIED)

1. **Terminal-data must be on channel DB.** DC (probe 3) and DD (probe 2) both cause the *fatal*
   XXPER. DB (probe 1) produced only a *recoverable* XENSE — i.e. 100 parsed the DB frame as a
   valid terminal-data frame and rejected only its sequence. So **DB is structurally correct; DC/DD
   are structurally invalid for a 102→100 terminal-data frame and crash the kernel.**
2. **Flags1 must be continuous** with our own prior frames (probe 1's jump → XENSE).
3. **100 forces our sequence low.** Probe 4 tried to start our whole sequence high (`0x012F`) so DB
   would be reachable; 100 rejected the **accept** with XENSE. So we cannot simply adopt the
   capture's high sequence — 100 requires our datagram sequence to follow the low connect it sent.

### 6.2 The contradiction (the actual problem)

- To land on **DB** with `XMCSM 0x01080000`: `0xDE − 1 − (Base>>8) = 0xDB` ⟹ **`Base>>8 = 2`** ⟹
  `Base ≈ 0x02xx` ⟹ **Flags1 must be high** (a one-byte Counter, max `0xFF`, cannot lift a low
  Flags1 to `0x02xx` — e.g. `0x0002 + 0xFF = 0x0101`, only `Base>>8 = 1` → DC).
- But 100 **forces Flags1 low** (must be in-sequence with the connect's `0x0000…`; a high start is
  XENSE-rejected).

A fresh node cannot satisfy both. In the capture there was no conflict because **102 already had a
high running sequence** (`0x0131`), so its `0x01080000` frames landed on DB *for free*. We start at
zero and 100 will not let us jump. **Therefore the envelope formula, though it reproduces the
captures exactly, does not by itself tell us how a fresh responder reaches DB.**

---

## 7. Code modules (for reference)

All under `SINTRAN/XMSG/SRC/`. C# / .NET 9. No LINQ, no `foreach`, named-delegate events.

| Module | Responsibility |
|---|---|
| `Xmsg.Protocol/Wire/SintranHeader.cs`, `XmsgSubHeader.cs`, `XmsgFrame.cs` | parse/serialise the wire frame |
| `Xmsg.Protocol/Packet/XmsgEnvelope.cs` | **`DeriveChannel(flags1, counter, xmcsm)`** — the formula in §3 |
| `Xmsg.Protocol/Packet/XmsgPacketBuilder.cs` | `CreateData`, `CreateAck`, **`CreateSessionData`** (derives the channel) |
| `Xmsg.Protocol/StateMachine/SecureDatagramReceiver.cs` | the subtype-0x03 secure ACK (channel = connect+4, trailing = connect-counter+0x0A) — VERIFIED working |
| `Xmsg.Live/Tad/TadTerminalResponder.cs` | the responder: `OnConnect` (accept), `OnSessionSetup` (port-assign + the disabled DUMM), **`BuildResponderFrame`** |
| `Xmsg.Live/Seam/XmsgLayer.cs` | L3 dispatch above the codec |
| `Xmsg.Live.Runner/Program.cs` | live composition; `SendTerminalBringup` is currently **false** (bring-up disabled to avoid the crash) |
| `Xmsg.Protocol.Tests/EnvelopeChannelTests.cs` | proves the formula reproduces the captured DUMM/MOTD byte-for-byte |
| `Xmsg.Live.Tests/TadConnectAcceptTests.cs` | proves accept/port-assign/DUMM byte values |

**The secure ACK (subtype 0x03) is fully solved and VERIFIED live:** channel = connect-channel + 4
(DA→DE, D9→DD); trailing byte seeded at `connect-counter + 0x0A`, decrementing. This is the model
that lets 100 accept our accept/port-assign — see `SecureDatagramReceiver.cs`. It is *not* the
blocker; the terminal-data channel/sequence is.

---

## 8. What we KNOW vs ASSUME vs DON'T KNOW

### VERIFIED (KNOW)
- The wire format (§2), the envelope formula fits all captures (§3, §5).
- The handshake works live (accept/port-assign echo + secure ACK on connect+4).
- Terminal-data 102→100 must be on **DB**; DC/DD crash 100 fatally (§6.1).
- Flags1 must be continuous with our prior frames; 100 rejects out-of-order with XENSE.
- 100 rejects a high-sequence accept (we cannot just start high).
- In the working captures, both machines had **high** running datagram sequences.

### ASSUMPTION (marked — NOT established)
- **ASSUMPTION A:** The datagram sequence (Flags1) is tracked **per node-pair** (all 102→100
  traffic shares one sequence), not per-port-pair. *Evidence:* in the capture Flags1 is continuous
  across the TADADM port and the session port. *Risk:* if it is actually **per-connection**, the
  session-port data stream could have its own independent sequence that we could start where we like
  — which might dissolve the contradiction. **This is the single most important thing to verify.**
- **ASSUMPTION B:** The channel is *derived* from `Base` (not independently allocated and then
  merely consistent-with-Base in the captures). *Evidence:* the formula fits. *Risk:* if the channel
  is actually **allocated** by XMSG during setup and *announced* to 100, then we must send on the
  allocated channel and our Base must be made to match it — different mental model.
- **ASSUMPTION C:** 100 validates the incoming channel by recomputing the formula and comparing;
  a mismatch (or an out-of-family channel like DD-for-terminal-data) triggers XXPER. *Unverified.*

### UNKNOWN (DON'T KNOW)
- **The rule XMSG uses to pick the terminal-data channel and the expected Flags1 for a responder
  whose sequence starts near zero.** This is the crux. The captures only show responders with high
  sequences.
- Whether a fresh responder is even *expected* to start at zero, or whether real XMSG seeds its
  outgoing datagram sequence to some non-zero value at startup (which would explain the captures'
  `0x012F`).
- The exact meaning of the XXPER at physical address `134265B` (which XMSG routine, which check).
- Whether the session port value we assign (`0x0211`, logical 4) matters — captures used `0x04C2`
  (logical 9). Not yet isolated.
- What the counter's semantics are beyond feeding `Base` (does 100 track/validate it independently?).

---

## 9. IDEAS to test (clearly marked as UNVERIFIED hypotheses)

- **IDEA 1 (test ASSUMPTION A):** capture a `connect-to` to a **freshly (re)started** ND responder
  whose XMSG sequence is genuinely near zero. If such a responder *still* sends terminal-data on DB
  with a *low* Flags1, then the channel is **not** `Base`-derived (ASSUMPTION B is false) and is
  allocated some other way — decisive. If instead a fresh responder's sequence is *not* near zero
  (XMSG seeds it high at startup), then our node should **seed its outgoing datagram sequence high
  at startup** and the contradiction dissolves — but we must find what 100 expects that seed to be.
- **IDEA 2:** the datagram sequence may be **per-connection (per XMSPT/XMDPT port-pair)**. The
  accept/port-assign ride the TADADM port; the terminal-data rides the session port `0x0211`. If
  100 tracks a *separate* sequence for the session-port connection, that stream can start where we
  choose — possibly high enough for DB — without violating the TADADM stream's continuity. **Test:**
  send the DUMM on the session port with a Flags1 that is the *first* frame of that port-pair
  (e.g. matching whatever 100 used when it opened the session), and see if XENSE disappears.
- **IDEA 3:** maybe the terminal-data channel is **allocated relative to the connect channel**, not
  from Base. Capture: connect D9 → data DB (D9+2) and DC (D9+3). Our connect DA → data would be DC
  (DA+2)/DD (DA+3) — but we *tried* DC/DD and they crashed. So this simple offset is likely wrong,
  **or** the offset is anchored differently (e.g. always DB regardless of connect channel). Worth
  re-deriving the offset across *all* captures with different connect channels.
- **IDEA 4:** read the XMSG kernel source (XMSG/XROUT NPL symbols) for the routine at the XXPER
  address and for where it computes/looks up the Protocol ID of an outbound terminal-data letter.
  The repo has NPL sources under `SINTRAN/NPL-SOURCE/` — the channel-allocation routine may be there.

---

## 10. The precise question for the analyst

> For a SINTRAN III XMSG responder (node 102) whose XMSG has **just started** (outgoing datagram
> sequence near zero), answering a `connect-to` from node 100: **on which Protocol-ID channel, and
> with which Flags1 (datagram sequence) and sub-header Counter, must the first terminal-data frame
> (`XMCSM 0x01080000`, e.g. a DUMM) be sent so that 100 accepts it** — given that (a) terminal-data
> must be on DB, (b) DB via the envelope formula needs `Base = Flags1 + Counter ≈ 0x02xx`, and
> (c) 100 XENSE-rejects any Flags1 that is not in-sequence with the low connect it sent?
>
> Equivalently: **is the datagram sequence per-node-pair or per-connection, and does XMSG seed a
> responder's outgoing sequence to a non-zero value at startup?** Resolving that resolves the MOTD.

---

*Written 2026-07-03. All hex values are from `conn-to-d102-from-100.pcapng` (decode report lines
2012–2929) and from live console logs against machine 100. Verified/assumed/unknown are marked
throughout; do not trust the ASSUMPTION/IDEA items without independent confirmation.*
