# SINTRAN III XMSG over HDLC — Protocol Specification

This document specifies the inter-system messaging protocol used by SINTRAN III's
**XMSG** (eXchange MeSsaGe) subsystem — part of ND COSMOS / NORD-NET networking —
when it carries messages between two systems over an **HDLC serial link**. It is
written so an application on any platform can implement a peer that exchanges
messages with an ND-100 / SINTRAN III host.

The field names, symbols, and status codes used throughout are the **official ND
terms**, taken from the ND L07 symbol tables, the COSMOS Programmer Guide
(ND-60.164), and the NDInsight LAPB/SINTRAN Wireshark dissector. The byte-level
formats were confirmed against FCS-validated link traffic between two live SINTRAN
III systems (nodes 100 and 102, VSX/500 L).

A short **Provenance & confidence** section (§14) states, per field, whether a
detail is documented in those ND sources, observed in this capture work, or still
uncertain — read it before relying on any single field for a production decoder.

### Conventions

- Multi-byte integers are **big-endian** unless stated otherwise.
- Wire bytes are shown in hexadecimal; ND symbol values are given in octal (ND's
  native radix) with hex/decimal where useful.
- **System number** (a.k.a. **node number**) is a host's network identity (e.g.
  `100`, `102`).
- **Port** is a task-owned message endpoint on a host, identified by a small
  number locally and by a **magic number** remotely (§8).

---

## 1. Layer model

```
 ┌──────────────────────────────────────────────────────────────────┐
 │ HDLC frame      7E | byte-stuffed( LAPB frame | FCS ) | 7E         │  §2
 │   LAPB frame    address | control | information                   │  §3
 │     information ┌────────────────────────────────────────────┐    │
 │                 │ SINTRAN header (13 bytes)                   │    │  §4
 │                 │   Markers, Packet Type/Subtype, Dest/Src    │    │
 │                 │   Node, Flags, Protocol ID                  │    │
 │                 ├────────────────────────────────────────────┤    │
 │                 │ Sub-protocol payload                        │    │  §5–§7
 │                 │   ROUTING / TAD / DC / PAD                   │    │
 │                 │   (XMSG sub-header + user data)             │    │
 │                 └────────────────────────────────────────────┘    │
 └──────────────────────────────────────────────────────────────────┘
```

| Layer | Role | Section |
|-------|------|---------|
| HDLC framing | delimit frames on the byte stream, FCS error detection | §2 |
| LAPB | balanced data link: establish, sequence, acknowledge | §3 |
| SINTRAN header | route a frame between nodes; identify the sub-protocol | §4 |
| Sub-protocol / XMSG | message endpoints (ports), addressing, payload | §5–§8 |

The `--hdlc` TCP bridge exposed by the `nd100x` emulator is a transparent byte
pipe; the HDLC bit framing is performed by the emulated COM5025/DMA layer, so over
the bridge the async byte stream described in §2 is what appears.

---

## 2. HDLC framing

Frames use asynchronous HDLC framing (RFC 1662 / PPP-async style):

```
   7E | byte-stuff( LAPB-frame | FCS_low | FCS_high ) | 7E
```

| Symbol | Value | Meaning |
|--------|-------|---------|
| `FLAG` | `0x7E` | frame delimiter (start and end) |
| `ESCAPE` | `0x7D` | escape prefix |
| mask | `0x20` | escaped byte = `original XOR 0x20` |

**Byte-stuffing.** Within `LAPB-frame | FCS`, every `0x7E` and `0x7D` is
transmitted as `0x7D` then `(byte XOR 0x20)`. The receiver reverses it.

**Frame Check Sequence (FCS-16).** A 16-bit CRC over the unstuffed LAPB frame:

- CRC-CCITT, polynomial `0x1021` (reflected/table form `0x8408`).
- Initial value `0xFFFF`; **one's-complement** the result before transmission.
- Sent **low byte first**, then high byte.
- Receiver folds the CRC (init `0xFFFF`) over `LAPB-frame | FCS_low | FCS_high`;
  a valid frame yields the constant **good residue `0xF0B8`**.

**Anchor** (a real captured SABM link-setup frame from node 100 — `ctrl 0x3F`, §3.4):

```
LAPB-frame = 01 3F 00 64
FCS        = 0x092E      → on the wire low-first: 2E 09
frame      = 7E 01 3F 00 64 2E 09 7E
```

---

## 3. LAPB data link

After de-framing, the LAPB frame is:

```
   address(1) | control(1) | information(0..n)
```

### 3.1 Address

| Address | Used on |
|---------|---------|
| `0x01` | link-establishment frames (SABM, UA) |
| `0x09` | data-transfer frames (RR, I-frames) |

### 3.2 Control byte (modulo-8)

| Frame type | Bit pattern | Fields |
|------------|-------------|--------|
| **I** (information) | `bit0 = 0` | `control = (N(R) << 5) \| (P/F << 4) \| (N(S) << 1)` |
| **S** (supervisory) | `bits1..0 = 01` | `control = (N(R) << 5) \| (P/F << 4) \| (type << 2) \| 01`; type RR=0, RNR=1, REJ=2 |
| **U** (unnumbered) | `bits1..0 = 11` | fixed codes below |

Decoders: `N(S) = (control >> 1) & 7`, `N(R) = (control >> 5) & 7`,
`P/F = (control >> 4) & 1`.

| Unnumbered frame | Control |
|------------------|---------|
| SABM (set async balanced mode) | `0x3F` |
| UA (unnumbered acknowledge) | `0x73` |
| DISC (disconnect) | `0x43` |
| DM (disconnected mode) | `0x0F` |
| FRMR (frame reject) | `0x87` |
| RR (receive ready) | `0x01 \| (N(R) << 5)` |

> SABM (`0x3F`) and UA (`0x73`) are shown in their **observed wire form with the
> poll/final bit set** (base codes `0x2F`/`0x63` `\| 0x10`). DISC/DM/FRMR are the
> base codes with P/F = 0; their exact wire form was not observed in capture (only
> SABM/UA/RR/REJ/I appear) and rests on standard LAPB.

### 3.3 Node identity on link-management frames

SABM, UA, and RR carry the **sending node number** as their 2-byte information
field — this is how each end announces and confirms its identity (`00 64` = node
100, `00 66` = node 102).

### 3.4 Establishing the link

The link is balanced; either side may initiate:

```
   →  SABM   addr 01  ctrl 3F   info = own node number
   ←  UA     addr 01  ctrl 73   info = peer node number
   ←  SABM   addr 01  ctrl 3F   info = peer node number     (peer also initiates)
   →  UA     addr 01  ctrl 73   info = own node number
   ── link established; V(S) = V(R) = 0 ──
   ↔  RR     addr 09  ctrl 01   info = node number          (periodic keepalive, ~1 s)
```

### 3.5 Sequencing

- Each I-frame sent carries `N(S) = V(S)`; increment `V(S)` (mod 8) after sending.
- An in-order I-frame (`N(S) == V(R)`) increments `V(R)` (mod 8).
- I-frames and RRs carry `N(R) = V(R)`, acknowledging peer frames up to `N(R) − 1`.
  An I-frame therefore doubles as an acknowledgment.
- A duplicate / out-of-order I-frame is answered with an RR carrying the current
  `N(R)`.

---

## 4. SINTRAN header

Every LAPB I-frame carries a fixed **13-byte SINTRAN header** immediately after
the address and control bytes:

```
Offset  Size  Field          Notes
──────  ────  ─────────────  ───────────────────────────────────────────────
  0      1    Marker 1       Always 0x21
  1      1    Marker 2       0x13 = normal frame, 0x12 = relay frame
  2      1    Packet Type    Frame-type indicator (0x00 in observed XMSG traffic)
  3      1    Packet Subtype Message kind (see §4.2)
  4      2    Dest Node      Destination node number (big-endian)
  6      2    Src Node       Source node number (big-endian)
  8      2    Flags 1        Broadcast marker / datagram sequence (see §4.3)
 10      2    Flags 2        Version-type / length / status (see §4.3)
 12      1    Protocol ID    Sub-protocol carried in the payload (see §4.4)
```

`Marker 1 (0x21)` and `Marker 2 (0x13)` serve as the SINTRAN frame fingerprint;
their values are reminiscent of the X.25 Packet-Layer GFI and LCN bytes (logical
channel 19), but ND sources treat them simply as fixed marker bytes — the X.25
reading is an observation, not a cited ND fact. `Marker 2 = 0x12` marks a **relay
frame** (a node forwarding between two others; the outer Dest/Src are the relay
nodes, the endpoints sit inside the payload).

### 4.2 Packet Subtype

The Packet Subtype byte (offset 3) distinguishes message kinds. Values observed
in inter-node XMSG/ROUTING traffic:

| Subtype | Message kind | Section |
|---------|--------------|---------|
| `0x19` | Reachability request | §6 |
| `0x13` | Reachability reply | §6 |
| `0x0E` | Data message | §7 |
| `0x07` | Delivery acknowledgment | §9 |

> The NDInsight dissector's Lua field names offset 3 *Packet Subtype*, but its
> README labels the same byte *Packet Length*. The captures refute a length
> reading — the value is independent of frame length (a 14-byte reachability
> request carries `0x19` = 25; a 14-byte ack carries `0x07`) — so this spec treats
> it as a subtype/message-kind code.

### 4.3 Flags 1 / Flags 2 (content varies by Packet Subtype)

These two words are general-purpose in the SINTRAN header; their content depends
on the message kind:

| | Flags 1 (offset 8) | Flags 2 (offset 10) |
|---|---|---|
| Reachability request/reply | `0xFFFF` (broadcast marker) | `0x0001` |
| Data message | **datagram sequence number** (increments +1 per message) | message length/size |
| Delivery acknowledgment | **echoed datagram sequence** of the acked message | **status word** (see §9) |

The "datagram sequence number" is ND's own term: the COSMOS Programmer Guide
defines status `XENSE` (−34) as *"the received datagram identifier (sequence
number) is not equal to the expected sequence number."* The in-kernel header
symbol for it is `XMSEQ`.

### 4.4 Protocol ID

| Protocol ID | Name | Description |
|-------------|------|-------------|
| `0xDE` | ROUTING | network routing / inter-node control (carries XMSG) |
| `0xDD` | TAD | Terminal Access and Directory (terminal sessions) |
| `0xDC` | DC | terminal data |
| `0xDA` | PAD | X.25 PAD virtual-circuit data |

> **Caveat for generic XMSG traffic.** In captured point-to-point XMSG
> `Send-Message` traffic, offsets **12–13 behaved as a single 16-bit counter
> decrementing once per message** (`de04 → de03 → … → de00 → ddff`; the high byte
> borrowed `de → dd`). For that traffic class, offset 12 is the high byte of a
> per-message identifier rather than a fixed Protocol ID. The fixed-Protocol-ID
> reading holds for the ROUTING/TAD/DC/PAD sub-protocol traffic. Either generic
> XMSG and sub-protocol traffic differ here, or the field is a counter whose high
> byte normally sits in the `0xDx` range — see §14.

---

## 5. XMSG sub-header

ROUTING / TAD / DC / PAD payloads share the same **XMSG sub-header**, beginning
immediately after the SINTRAN header:

```
Offset*  Size  Symbol   Field             Notes
───────  ────  ───────  ────────────────  ──────────────────────────────────
  0       1    —        Counter           per-direction message counter
  1       2    —        Marker            always 0x21 0x00
  3       1    —        Frame Flags       not constant (0x82 / 0x86 / 0x96 seen)
  4       1    —        Role              asker/responder hint (see table)
  5       2    XMDSY    Destination system  node number (big-endian)
  7       2    XMDPT    Destination port    port on dest node (big-endian)
  9       2    XMSSY    Source system       node number (big-endian)
 11       2    XMSPT    Source port         port on src node (big-endian)
 13       4    XMCSM    Control / service   service-id for ROUTING/TAD services
 17       1    —        Pad                 0x00
 18       1    XMLEN    User data length    low byte of length
 19       N    —        User data           format depends on the service
```

`*` Offsets are relative to the start of the sub-header (i.e. SINTRAN-header
offset 13 onward). `XMDSY/XMDPT/XMSSY/XMSPT/XMCSM/XMLEN` are the application-
relevant fields of the in-kernel **XM5** message header (§10); kernel-only fields
(data bank/offset, timestamps, allocation flags) are dropped before transmission.

**Role byte (offset 4)** — low nibble `4` = asker, `0` = responder:

| Value | Meaning |
|-------|---------|
| `0xC4` | Asker for LI ROUTING |
| `0x60` | Responder for LI ROUTING |
| `0xE4` | Asker for LI SYSTEM-TAD |
| `0x40` | Responder for LI SYSTEM-TAD |
| `0x00` | Generic data frame (no role) |

Short messages (e.g. an empty buffer) carry a truncated trailer — the
`XMCSM/Pad/XMLEN` block is only present for service requests that need it.

---

## 6. Reachability handshake

Before a node will route messages to a remote, it tests reachability: it sends a
request (Packet Subtype `0x19`); the remote answers with a reply (Packet Subtype
`0x13`). Until the reply arrives, the remote is treated as **not accessible** and
messages to it are withheld.

```
   →  21 13 00 19  00 66 00 64  FF FF 00 01 DE 08      request  (100 → 102)
   ←  21 13 00 13  00 64 00 66  FF FF 00 01 DE 0E      reply    (102 → 100)
```

The reply swaps Dest/Src Node. Flags 1 = `FFFF` (broadcast marker), Flags 2 =
`0001`, Protocol ID = `DE` (ROUTING), followed by the per-direction Counter.

---

## 7. Data message

A data message (Packet Subtype `0x0E`) carries the SINTRAN header followed by the
XMSG sub-header and any user data:

```
21 13 00 0E  00 66 00 64                     ← SINTRAN header: subtype 0E, dest 102, src 100
00 00                                        ← Flags 1 = datagram sequence (0)
00 10                                        ← Flags 2 = length/size (16)
DE                                           ← Protocol ID (ROUTING)
04                                           ← Counter
21 00                                        ← XMSG Marker
82                                           ← Frame Flags
C4                                           ← Role (asker)
00 66                                        ← XMDSY = dest system 102
03 80                                        ← XMDPT = dest port 0x380  (= logical port 7 << 7)
00 64                                        ← XMSSY = src system 100
02 AD                                        ← XMSPT = src port 0x2AD
00 10                                        ← trailing size word = 16 (see note)
```

In this short empty-buffer message the `XMCSM / Pad / XMLEN` block (§5) is
truncated: the trailing `00 10` sits where `XMCSM` would begin (sub-header offset
13), not at `XMLEN`'s offset 18, and its value mirrors the Flags 2 length. A
message carrying user data has the full trailer and the data after it.

The datagram sequence number (Flags 1) increments by 1 for each message sent on a
port. It is the value the delivery acknowledgment echoes (§9).

---

## 8. Addressing model

A host has a **node number** (100, 102, …) and runs service tasks (XROUT, TADAD,
XMFIDO, …). Each task owns one or more **ports**. A remote reference is a 32-bit
**magic number** = port + system + a random component (so stale references can't
be reused).

On the wire the destination is expressed as the `XMDSY` (system) and `XMDPT`
(port) fields. The port value carried in `XMDPT` is the **logical port number
shifted left by 7** (logical port 7 → `0x0380`). To address a message to a known
`(system, port)`:

```
   high word = system number
   low  word = port << 7
```

XMSG provides `XFP2M` (port → magic) to build a magic number, and `XFRTN`
("return message" — send a reply back to the port the message came from) to reply
to a received message without minting a new address.

---

## 9. Delivery acknowledgment

Routed messages are delivered **securely** (the `XFSEC` "secure message" option):
the receiver **must acknowledge** each data message, or the sending system
retransmits it (~3×). The acknowledgment is a SINTRAN frame of Packet Subtype
`0x07`, sent immediately after the data message is accepted:

```
21 13 00 07 | Dest Node(2) | Src Node(2) | Flags1(2) | Flags2(2) | DE | Counter
```

| Field | Value / meaning |
|-------|-----------------|
| Dest Node / Src Node | the data message's nodes, **swapped** (ack returns to the sender) |
| Flags 1 | **the datagram sequence number copied verbatim** from the acknowledged data message — binds the ack to that message |
| Flags 2 | **status word** (see below) |
| Protocol ID | `DE` (ROUTING) — but see §4.4/§14: in generic XMSG traffic offsets 12–13 act as one 16-bit value decrementing per message (`de2d`→`de28` across this burst) |
| Counter | the acknowledging side's per-direction counter (low byte of the above) |

The `Flags 1` echo is mandatory: each outstanding message is matched by the ack
carrying its sequence number. Acknowledging with any other value leaves the
message unacknowledged and the sender keeps retransmitting it.

**Status word (Flags 2).** Observed as the constant `0xFFED` = −19. ND status −19
is **`XEIMA`** ("Invalid magic number" / "remote end has terminated", COSMOS
Programmer Guide Appendix D). Its appearance in a *successful* delivery ack is not
fully explained — treat the `XEIMA` reading as a value match, not a confirmed
semantic (§14).

**Example** — six data messages, each acknowledged with its echoed sequence:

| Data message: datagram seq | Acknowledgment (Flags1, Flags2, Counter) |
|----------------------------|------------------------------------------|
| `00 00` | `00 00  FF ED  DE 2D` |
| `00 01` | `00 01  FF ED  DE 2C` |
| `00 02` | `00 02  FF ED  DE 2B` |
| `00 03` | `00 03  FF ED  DE 2A` |
| `00 04` | `00 04  FF ED  DE 29` |
| `00 05` | `00 05  FF ED  DE 28` |

The datagram sequence (Flags 1) increments per message; the Counter decrements
per ack (the receiver's own per-direction counter). The two are independent.

---

## 10. XM5 message header (in-kernel reference)

The wire sub-header (§5) is a serialised subset of the in-kernel **XM5** message
header. The full symbol set (from the L07 symbol tables) is useful when matching
wire fields to ND documentation:

| Symbol | Octal | Field |
|--------|------:|-------|
| XMSTA | 000135 | Status / return code |
| XMDSY | 000136 | Destination system |
| XMDPT | 000137 | Destination port |
| XMSSY | 000140 | Source system |
| XMSPT | 000141 | Source port |
| XMCSM | 000142 | Control / session-management word |
| XMLIX | 000143 | Current-position index |
| XMSIZ | 000144 | Total buffer size (allocated) |
| XMLEN | 000147 | User data length (bytes) |
| XMSEQ | 000154 | Sequence number (overloads XMPRT/XMBLN) |

Kernel-only fields (`XMDAB/XMDAW` data bank/offset, `XMTIM/XMTPT` timestamps,
`XMALL/XMSIZ` allocation) are not serialised onto HDLC.

---

## 11. Function codes (XF\*) and the send/receive sequence

XMSG operations are invoked through monitor call **200B** with a function code:

| Symbol | Octal | Hex | Purpose |
|--------|------:|----:|---------|
| XFGET | 000002 | 0x02 | Get (allocate) message buffer |
| XFREL | 000003 | 0x03 | Release message buffer |
| XFRHD | 000004 | 0x04 | Read header |
| XFWHD | 000005 | 0x05 | Write header (XMDSY/XMDPT/XMSSY/XMSPT) |
| XFREA | 000006 | 0x06 | Read user data |
| XFWRI | 000007 | 0x07 | Write user data |
| XFOPN | 000012 | 0x0A | Open port |
| XFCLS | 000013 | 0x0B | Close port |
| XFSND | 000014 | 0x0C | Send message |
| XFRCV | 000015 | 0x0D | Receive message |
| XFP2M | 000027 | 0x17 | Port → magic number |
| XFPRV | 000036 | 0x1E | Make privileged (needed for privileged operations, e.g. routing-table updates) |
| XFRTN | 000037 | 0x1F | Return message (send it back to the port it came from) |

Typical send/receive:

```
  sender:    XFGET(size) → XFWHD(dst sys/port, src sys/port) → XFWRI(data) → XFSND
  receiver:  XFRCV(port) → XFRHD → XFREA(data) → XFREL
```

---

## 12. Status codes (XR\* / XE\*)

`XR*` codes (returned in `XMSTA`, small non-negative values) and the negative
`XE*` codes (COSMOS Programmer Guide Appendix D) most relevant to this protocol:

| Symbol | Value | Meaning |
|--------|------:|---------|
| XRSOK | 0 | OK / success |
| XRNSP | 4 | No such port |
| XRMTL | 8 | Message too large |
| XRPRV | 10 | Privilege violation |
| XRNRO | 12 | No route |
| XEIMA | −19 | Invalid magic number / remote end terminated |
| XENSE | −34 | Received datagram identifier (sequence number) ≠ expected |

---

## 13. Worked example — one full exchange

On an established link (all frames address `0x09`):

```
   100 → 102   I  N(S)=0 N(R)=0   21 13 00 19 00 66 00 64 FF FF 00 01 DE 08    reachability request
   102 → 100   I  N(S)=0 N(R)=1   21 13 00 13 00 64 00 66 FF FF 00 01 DE 0E    reachability reply
   100 → 102   I  N(S)=1 N(R)=1   21 13 00 0E 00 66 00 64
                                  00 00 00 10 DE 04 21 00 82 C4
                                  00 66 03 80 00 64 02 AD 00 10                data message, datagram seq 0
   102 → 100   I  N(S)=1 N(R)=2   21 13 00 07 00 64 00 66 00 00 FF ED DE 2D    delivery ack, echoes seq 0
```

---

## 14. Provenance & confidence

The byte-level formats here were confirmed against FCS-validated captures between
two live SINTRAN III systems. The terminology and several field meanings are
cross-referenced to ND sources. Per area:

**Documented in ND primary sources (high confidence)** — ND L07 symbol tables and the COSMOS Programmer Guide
- HDLC framing, LAPB control encoding, FCS-16 — standard, and matched on the wire.
- The XMSG sub-header fields `XMDSY/XMDPT/XMSSY/XMSPT`, the `XF*` function codes,
  the XM5 header symbols, the magic-number addressing model, and `XFP2M`/`XFRTN`.
- Status codes `XEIMA` (−19) and `XENSE` (−34, "datagram identifier (sequence
  number)") — COSMOS Programmer Guide Appendix D.

**Documented in the NDInsight Wireshark dissector (an RE artifact, not ND primary docs)**
- The 13-byte SINTRAN header layout, Markers `21`/`13`, the relay marker `0x12`,
  the Protocol ID values (`0xDE/0xDD/0xDC/0xDA`), the XMSG sub-header `Marker 21 00`,
  and the **Role byte**. The Role high-nibble meaning is partially inferred, and the
  dissector itself flags relay routing as unverified.

**Observed in this capture work (extends the above)**
- **Packet Subtype enumeration** `0x19` / `0x13` / `0x0E` / `0x07` for
  reachability request / reply / data / delivery-ack. The dissector's Lua field
  names offset 3 "Packet Subtype" but does not enumerate values (its README labels
  the byte "Packet Length"; the captures refute a length reading — see §4.2).
- **Delivery acknowledgment (Packet Subtype `0x07`)**: that it exists at this
  layer, that it echoes the datagram sequence in Flags 1, and that its absence
  triggers retransmission. The dissector has no `0x07` frame type at the SINTRAN
  layer. (Distinct from the ROUTING sub-protocol *command* `0x07` = Bootstrap-
  Request, which lives in the payload, not the SINTRAN Packet Subtype byte.)
- **Flags 1 carries the datagram sequence** on point-to-point data messages (it
  is `0xFFFF` only on broadcast/reachability frames).
- **Frame Flags byte value `0x82`** on point-to-point data frames, extending the
  dissector's documented `0x86`/`0x96` set.
- `XMDPT` on the wire = logical **port ≪ 7** (captured `0x0380` = port 7).

**Uncertain / flagged for confirmation**
- **`0xFFED` (−19) status in the ack.** The value matches `XEIMA` ("invalid magic
  number"), but that meaning is odd for a successful delivery — semantic unconfirmed.
- **Offset 12 (Protocol ID) vs a 16-bit counter.** Generic XMSG `Send-Message`
  traffic showed offsets 12–13 decrementing as one 16-bit counter
  (`de04 → ddff`), unlike the fixed `0xDE` Protocol ID seen in ROUTING/TAD
  traffic. Needs a second capture to settle whether the two traffic classes
  differ or the field is a counter throughout.
- **Packet Subtype ↔ symbol mapping.** The subtype values `7/14/19/25` numerically
  match the `5*` network-layer symbols in the L07 symbol table
  (`5SRPI`=0o16=14, `5DPIT`=0o23=19, `5SSPD`=0o31=25), but those symbols carry no
  descriptive comments — the mapping is a numeric lead only, unconfirmed without
  the segment-5 NPL source.

---

## 15. Implementation checklist

To exchange messages with a SINTRAN III system over HDLC, an implementation must:

1. Frame/de-frame the byte stream (§2), validating FCS on receive.
2. Establish the LAPB link (§3.4) and maintain `V(S)`/`V(R)`; send periodic RR
   keepalives.
3. Answer reachability requests (Packet Subtype `0x19`) with replies (`0x13`) so
   the peer considers it accessible (§6).
4. Decode incoming data messages (Packet Subtype `0x0E`, §7).
5. **Acknowledge every data message** with a delivery ack (Packet Subtype `0x07`)
   that echoes the message's datagram sequence in Flags 1 (§9) — otherwise the
   peer retransmits.
6. To send: address the destination as `XMDSY` = system, `XMDPT` = port ≪ 7 (§8);
   to reply, swap source/destination (the `XFRTN` pattern) and fill the buffer to
   its declared size.

---

## 16. Constants

| Constant | Value |
|----------|-------|
| HDLC flag / escape / mask | `0x7E` / `0x7D` / `0x20` |
| FCS polynomial / init / good residue | `0x1021` (reflected `0x8408`) / `0xFFFF` / `0xF0B8` |
| LAPB address — link setup / data | `0x01` / `0x09` |
| SABM / UA / DISC / DM / FRMR | `0x3F` / `0x73` / `0x43` / `0x0F` / `0x87` |
| SINTRAN Marker 1 / Marker 2 (normal/relay) | `0x21` / `0x13` (normal), `0x12` (relay) |
| Packet Subtype — request / reply / data / ack | `0x19` / `0x13` / `0x0E` / `0x07` |
| Protocol ID — ROUTING / TAD / DC / PAD | `0xDE` / `0xDD` / `0xDC` / `0xDA` |
| XMSG Marker | `0x21 0x00` |
| Role — asker / responder (LI ROUTING) | `0xC4` / `0x60` |
| Delivery-ack status word | `0xFFED` (−19, XEIMA) |
| Port encoding on the wire | `port << 7` |
| Send addressing | `XMDSY` = system, `XMDPT` = port ≪ 7 |
