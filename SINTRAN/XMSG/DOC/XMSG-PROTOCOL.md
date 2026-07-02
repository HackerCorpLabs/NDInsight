# SINTRAN III XMSG-over-HDLC — Protocol Reference

**One authoritative description of the XMSG (eXchange MeSsaGe) wire format** used
by SINTRAN III / COSMOS / NORD-NET to carry inter-node messages (routing, terminal
/ TAD, PAD, file transfer, mail) over an HDLC serial link.

This document supersedes and merges three earlier notes — the peer protocol
spec, its pcap validation, and the NPL-symbol analysis — plus the format content
that used to live in the Wireshark dissector README. Those are archived under [OLD/](../OLD/).

## How to read this document

Every claim carries a confidence tag:

- **[VERIFIED]** — reproduced from FCS-valid captured traffic *and/or* read
  directly from NPL source / ND symbol tables.
- **[SYMBOLS]** — a symbol value or name from the ND symbol tables; on-wire
  serialisation not independently confirmed.
- **[INFERRED]** — a reasoned conclusion, not directly confirmed.
- **[CAPTURE-SPECIFIC]** — observed only in a traffic class **not** present in the
  13-capture corpus (the peer's `XFSND` send-message capture); plausible but
  unconfirmed here.

**Source-precedence rule** (applied wherever sources disagreed): FCS-verified
captures  >  the Wireshark dissector (a reverse-engineering artifact)  >  the ND
symbol tables  >  the peer's prose spec. Section 13 records exactly what this
rule changed.

**Evidence base:** 13 `.pcapng` captures held in the separate sibling
**X25Emulator** repository (its `pcap/` directory) — nodes 100/102/103, several
relayed 103↔102 via 100; 6379 raw de-framed frames, of which **1947 passed FCS**
and form the statistics cited below.

**Related documents (kept separate by scope):**
- [XMSG-API.md](XMSG-API.md) — the **programming / API** side: MON 200B calling
  convention, option bits, the XROUT letter/standard-message format, and the
  complete `XF*`/`XS*`/`XE*`/`XR*` constant catalogs (machine-readable copy in
  [XMSG/xmsg-constants.json](../xmsg-constants.json)).
- [HDLC-Frame-Format-Reference.md](../../Devices/HDLC/HDLC-Frame-Format-Reference.md) — the HDLC
  hardware / COM5025 / DMA / driver layer beneath XMSG.
- [XMSG-COMMAND-REFERENCE.md](XMSG-COMMAND-REFERENCE.md) — the `XMSG-COMMAND`
  operator utility (commands, not wire format).
- [TAD/TAD-Message-Formats.md](../../TAD/TAD-Message-Formats.md) — the full TAD
  terminal opcode chain layered on top of XMSG (see Section 9.2).
- [Devices/HDLC/WireShark/README.md](../../Devices/HDLC/WireShark/README.md) — the
  Wireshark dissector that implements this document.

---

## 1. Layer model  [VERIFIED]

```
 ┌──────────────────────────────────────────────────────────────────┐
 │ HDLC frame      7E | byte-stuffed( LAPB frame | FCS ) | 7E         │  sec 2
 │   LAPB frame    address | control | information                   │  sec 3
 │     information ┌────────────────────────────────────────────┐    │
 │                 │ SINTRAN header (13 bytes)                   │    │  sec 4
 │                 │   Markers, Packet Type, Subtype, Dest/Src,  │    │
 │                 │   Flags1, Flags2, Protocol ID               │    │
 │                 ├────────────────────────────────────────────┤    │
 │                 │ Sub-protocol payload                        │    │  sec 7-9
 │                 │   ROUTING / TAD / DC / DB / PAD              │    │
 │                 │   (XMSG sub-header + user data)             │    │
 │                 └────────────────────────────────────────────┘    │
 └──────────────────────────────────────────────────────────────────┘
```

| Layer | Role | Section |
|-------|------|---------|
| HDLC framing | delimit frames, FCS error detection | 2 |
| LAPB | balanced data link: establish, sequence, acknowledge | 3 |
| SINTRAN header | route a frame between nodes; identify subtype + sub-protocol | 4 |
| Sub-protocol / XMSG | message endpoints (ports), addressing, payload | 5–9 |

The `nd100x --hdlc` TCP bridge is a transparent byte pipe: the emulated
COM5025/DMA layer performs the HDLC bit framing, so over the bridge the async
byte stream in Section 2 is what appears on TCP. Conventions: multi-byte integers
are **big-endian** unless stated; wire bytes in hex, ND symbol values in octal
(ND's native radix) with hex/decimal where useful.

---

## 2. HDLC framing  [VERIFIED]

Asynchronous HDLC framing (RFC 1662 / PPP-async style):

```
   7E | byte-stuff( LAPB-frame | FCS_low | FCS_high ) | 7E
```

| Symbol | Value | Meaning |
|--------|-------|---------|
| `FLAG` | `0x7E` | frame delimiter (start and end) |
| `ESCAPE` | `0x7D` | escape prefix |
| mask | `0x20` | escaped byte = `original XOR 0x20` |

**Byte-stuffing.** Within `LAPB-frame | FCS`, every `0x7E` and `0x7D` is sent as
`0x7D` then `(byte XOR 0x20)`; the receiver reverses it.

**Frame Check Sequence (FCS-16).** 16-bit CRC over the unstuffed LAPB frame:
- CRC-CCITT, polynomial `0x1021` (reflected/table form `0x8408`).
- Init `0xFFFF`; **one's-complement** the result before transmission.
- Sent **low byte first**, then high byte.
- Receiver folds the CRC (init `0xFFFF`) over `LAPB-frame | FCS_low | FCS_high`;
  a valid frame yields the constant **good residue `0xF0B8`**.

**Anchor** (a real captured SABM link-setup frame from node 100, `ctrl 0x3F`):

```
LAPB-frame = 01 3F 00 64
FCS        = 0x092E      → on the wire low-first: 2E 09
frame      = 01 3F 00 64 2E 09        (CRC folded over frame|FCS = 0xF0B8)
```

All 1947 FCS-valid frames fold to `0xF0B8`.

---

## 3. LAPB data link  [VERIFIED]

After de-framing: `address(1) | control(1) | information(0..n)`.

### 3.1 Address

| Address | Used on |
|---------|---------|
| `0x01` | link-establishment frames (SABM, UA) |
| `0x09` | data-transfer frames (RR, I-frames) |

### 3.2 Control byte (modulo-8)

| Frame type | Bit pattern | Fields |
|------------|-------------|--------|
| **I** (information) | `bit0 = 0` | `control = (N(R) << 5) \| (P/F << 4) \| (N(S) << 1)` |
| **S** (supervisory) | `bits1..0 = 01` | `control = (N(R) << 5) \| (P/F << 4) \| (type << 2) \| 01`; RR=0, RNR=1, REJ=2 |
| **U** (unnumbered) | `bits1..0 = 11` | fixed codes below |

Decoders: `N(S) = (control >> 1) & 7`, `N(R) = (control >> 5) & 7`,
`P/F = (control >> 4) & 1`.

| Unnumbered frame | Control | Confidence |
|------------------|---------|------------|
| SABM | `0x3F` (base `0x2F`\|P) | [VERIFIED] |
| UA | `0x73` (base `0x63`\|P) | [VERIFIED] |
| RR | `0x01 \| (N(R) << 5)` | [VERIFIED] |
| REJ | `0x09 \| (N(R) << 5)` | [VERIFIED] |
| DISC | `0x43` | [INFERRED — standard LAPB, not seen in corpus] |
| DM | `0x0F` | [INFERRED] |
| FRMR | `0x87` | [INFERRED] |

### 3.3 Node identity on link-management frames  [VERIFIED]

SABM, UA and RR carry the **sending node number** as their 2-byte information
field: `00 64` = node 100, `00 66` = node 102, `00 67` = node 103.

### 3.4 Establishing the link  [VERIFIED]

Balanced link; either side may initiate:

```
   →  SABM   addr 01  ctrl 3F   info = own node number
   ←  UA     addr 01  ctrl 73   info = peer node number
   ←  SABM   addr 01  ctrl 3F   info = peer node number     (peer also initiates)
   →  UA     addr 01  ctrl 73   info = own node number
   ── link established; V(S) = V(R) = 0 ──
   ↔  RR     addr 09  ctrl 01   info = node number          (periodic keepalive)
```

### 3.5 Sequencing  [VERIFIED]

- Each I-frame carries `N(S) = V(S)`; increment `V(S)` (mod 8) after sending.
- An in-order I-frame (`N(S) == V(R)`) increments `V(R)` (mod 8).
- I-frames and RRs carry `N(R) = V(R)`, acknowledging peer frames up to `N(R)−1`
  (an I-frame doubles as a link-level ack). Note this LAPB `N(R)` acknowledgment
  is **distinct** from the XMSG datagram-level ACK in Section 6.
- A duplicate/out-of-order I-frame is answered with an RR carrying current `N(R)`.

---

## 4. SINTRAN header  [VERIFIED]

Every LAPB I-frame carries a fixed **13-byte SINTRAN header** immediately after
address and control:

```
Offset  Size  Field          Notes
──────  ────  ─────────────  ───────────────────────────────────────────────
  0      1    Marker 1       Always 0x21
  1      1    Marker 2       0x13 = normal frame, 0x12 = relay frame
  2      1    Packet Type    0x00 in all observed XMSG traffic
  3      1    Packet Subtype Message kind (see 4.1) — NOT a length
  4      2    Dest Node      Destination node number (big-endian)
  6      2    Src Node       Source node number (big-endian)
  8      2    Flags 1        Datagram sequence / broadcast marker (see 4.2)
 10      2    Flags 2        Frame-class word (see 4.2)
 12      1    Protocol ID    Sub-protocol carried in the payload (see 4.3)
```

`Marker 1 (0x21)` + `Marker 2 (0x13)` are the SINTRAN frame fingerprint. Their
values resemble the X.25 Packet-Layer GFI/LCN bytes, but ND treats them as fixed
markers — the X.25 reading is an observation, not a cited ND fact. **Marker 2 =
`0x12`** marks a **relay frame**: a node forwarding between two others. The outer
Dest/Src are the relay nodes; the endpoints sit inside the payload. In relayed
captures the *same* logical frame appears on two links with the per-link counter
re-stamped by +1.

### 4.1 Packet Subtype (offset 3)  [VERIFIED]

**This byte is a message-kind code, not a length.** (The old dissector README
labelled it "Packet Length"; that was wrong.) Proof from the corpus: subtype
`0x0E` appears on frames from **34 to 292 bytes**, and three distinct subtypes
(`0x03`, `0x13`, `0x19`) all occur at the *same* 14-byte length. Only four values
ever occur across all 1947 frames:

| Subtype | Meaning | Flags 1 | Flags 2 | Length | Count |
|---------|---------|---------|---------|--------|------:|
| `0x03` | **ACK / flow-control** (Section 6) | echoed acked datagram seq | `0x0001` | 14 B | 602 |
| `0x0E` | **Data message** (Section 5) | own datagram seq | `0x0400 / 0x0108 / 0x0008` | 34–292 B | 601 |
| `0x13` | **Reachability reply** (Section 5.1) | `0xFFFF` broadcast | `0x0001` | 14 B | 4 |
| `0x19` | **Reachability request** (Section 5.1) | `0xFFFF` broadcast | `0x0001` | 14 B | 8 |

> [CAPTURE-SPECIFIC] The peer's send-message capture reports a fifth subtype
> `0x07` for the delivery ACK with a `0xFFED` status word. **No subtype-`0x07`
> frame, and no `FF ED` byte pair, occurs anywhere in this corpus** (0 of 6379
> raw frames). In this corpus the ACK role is filled by subtype `0x03`
> (Section 6). Treat `0x07`/`0xFFED` as belonging to a different traffic class
> until confirmed.

### 4.2 Flags 1 / Flags 2  [VERIFIED]

`Flags 2` cleanly separates the two frame classes: `0x0001` on all short/control
frames (`0x03/0x13/0x19`); `0x0400 / 0x0108 / 0x0008` on data (`0x0E`), where it
appears to encode a per-sub-protocol class rather than a raw length.

`Flags 1` is the **datagram sequence number** (ND term; see status `XENSE` /
symbol `XMSEQ`): it increments per data message on a direction, is `0xFFFF` on
broadcast/reachability frames, and is **echoed verbatim by the ACK** (Section 6).

### 4.3 Protocol ID (offset 12)  [VERIFIED]

A stable sub-protocol selector — **not** a counter. (The peer spec's "offset
12–13 decrements as a 16-bit counter" was a decode-boundary misread: offset 12 is
this fixed ID, and the decrementing value is payload byte 0, the sub-protocol's
own per-direction counter.)

| Protocol ID | Name | Description | Section |
|-------------|------|-------------|---------|
| `0xDE` | ROUTING | network routing / inter-node control | 9.1 |
| `0xDD` | TAD | Terminal Access and Directory (terminal sessions) | 9.2 |
| `0xDC` | DC | terminal data forwarding | 9.3 |
| `0xDB` | DB | terminal data forwarding (DC variant) | 9.3 |
| `0xDA` | PAD | X.25 PAD virtual-circuit data | 9.4 |
| `0xD9` / `0xD8` | D9 / D8 | DC variants (observed; semantics inferred) | 9.3 |

---

## 5. Data message (subtype `0x0E`) and the XMSG sub-header  [VERIFIED structure]

A data message carries the 13-byte SINTRAN header, then the **XMSG sub-header**,
then user data. Sub-header layout (offsets relative to the start of the payload,
i.e. SINTRAN offset 13 onward):

```
Offset  Size  Symbol   Field             Notes
──────  ────  ───────  ────────────────  ──────────────────────────────────
  0      1    —        Counter           per-direction counter (decrements)
  1      2    —        Marker            always 0x21 0x00
  3      1    —        Frame Flags       not constant (0x82 / 0x84 / 0x86 / 0x96 seen)
  4      1    —        Role              asker/responder hint (see 5.2)
  5      2    XMDSY    Destination system  node number (big-endian)
  7      2    XMDPT    Destination port    port on dest node (big-endian; see note)
  9      2    XMSSY    Source system       node number (big-endian)
 11      2    XMSPT    Source port         port on src node (big-endian)
 13      4    XMCSM    Control / service   service-id (dispatch key for ROUTING/TAD)
 17      1    —        Pad                 0x00
 18      1    XMLEN    User data length    low byte of length
 19      N    —        User data           format depends on the service
```

`XMDSY/XMDPT/XMSSY/XMSPT/XMCSM/XMLEN` are the application-relevant fields of the
in-kernel **XM5** header (Section 10); kernel-only fields (data bank/offset,
timestamps, allocation) are dropped before transmission. Short messages carry a
truncated trailer — the `XMCSM/Pad/XMLEN` block is present only for services that
need it.

> **Port encoding note.** [REFUTED as stated by peer] The peer spec's
> `XMDPT = logical port ≪ 7` does **not** hold in this corpus: `XMDPT & 0x7F` is
> non-zero on most frames (common values `0x02C1, 0x0245, 0x02AB, 0x02E4`). The
> exact encoding of the logical port into `XMDPT` is **unconfirmed** — do not
> assume `≪ 7`.

### 5.1 Reachability handshake  [VERIFIED]

Before routing to a remote, a node tests reachability with a request
(subtype `0x19`); the remote answers with a reply (subtype `0x13`). Until the
reply arrives the remote is treated as **not accessible**.

```
   →  21 13 00 19  00 66 00 64  FF FF 00 01  DE 08      request  (100 → 102)
   ←  21 13 00 13  00 64 00 66  FF FF 00 01  DE 0E      reply    (102 → 100)
```

The reply swaps Dest/Src Node; `Flags 1 = 0xFFFF` (broadcast marker),
`Flags 2 = 0x0001`, Protocol ID `0xDE` (ROUTING), followed by the per-direction
counter byte.

### 5.2 Role byte (sub-header offset 4)  [VERIFIED low nibble; INFERRED high nibble]

Low nibble `4` = asker, `0` = responder. High-nibble class is partially inferred:

| Value | Meaning |
|-------|---------|
| `0xC4` | Asker for LI ROUTING |
| `0x60` | Responder for LI ROUTING |
| `0xE4` | Asker for LI SYSTEM-TAD |
| `0x40` | Responder for LI SYSTEM-TAD |
| `0x94` | Asker (connection setup) |
| `0x84` | Asker (legacy) |
| `0x54` | Asker variant |
| `0x00` | Generic data frame (no role) |

### 5.3 Peer data-message example  [CAPTURE-SPECIFIC — peer's send-message capture]

An empty-buffer data message from the peer's 100→102 send-message capture. **This
exact frame is not present in the 13-capture corpus**, and its `XMDPT = port ≪ 7`
reading is refuted here (see the port-encoding note above) — retained verbatim
from the peer spec so no detail is lost:

```
21 13 00 0E  00 66 00 64     SINTRAN header: subtype 0E, dest 102, src 100
00 00                        Flags 1 = datagram sequence (0)
00 10                        Flags 2 = length/size (16)
DE 04                        Protocol ID (ROUTING) + counter
21 00 82 C4                  XMSG marker, frame-flags 0x82, role 0xC4 (asker)
00 66 03 80                  XMDSY = 102, XMDPT = 0x0380  (peer reads as port 7 ≪ 7)
00 64 02 AD                  XMSSY = 100, XMSPT = 0x02AD
00 10                        trailing size word = 16
```

In this short empty-buffer message the `XMCSM/Pad/XMLEN` block is truncated: the
trailing `00 10` sits where `XMCSM` would begin and mirrors the Flags 2 length.

---

## 6. Delivery acknowledgment (subtype `0x03`)  [VERIFIED]

Routed data messages are acknowledged. In this corpus the ACK is a **short
subtype-`0x03` frame** (14 bytes: 13-byte header + 1 payload byte), sent in the
**opposite** direction to the data frame it acknowledges.

**Mechanism (verified by correlation):**
- **`Flags 1` of the `0x03` frame = the echoed datagram-sequence of the `0x0E`
  data frame it acknowledges.** Measured echo rate on the two direct 100↔102
  captures: 53/58 and 40/49 against the opposite-direction data frame, vs ~0%
  (0/48) against the same direction — the residual misses are off-by-one
  pipelining (two data frames in flight). Same-direction correlation being ~0
  rules out a same-side companion.
- **`Flags 2` = `0x0001`** on every ACK.
- **The single payload byte = the acking side's own per-direction counter**
  (decrements), interpreted per sub-protocol: proto `0xDE` → routing / connection-
  step command byte (Section 9.1), `0xDD` → TAD control byte, `0xDC` → DC
  flow-control counter.

**A `0x03` byte cannot be decoded in isolation** — its meaning is fixed by the
data frame it answers (`Flags 1` identifies which datagram).

> [CAPTURE-SPECIFIC] The peer's send-message capture describes the same ACK
> mechanism (Flags 1 echoes the datagram sequence; a per-direction counter in the
> body) under subtype **`0x07`** with a `0xFFED` status word. The *mechanism* is
> confirmed here; only the subtype value and status word differ by traffic class.

### 6.1 Secure delivery and retransmission  [VERIFIED ack exists; INFERRED retransmit]

Routed data messages are sent with the `XFSEC` "secure message" option: the
receiver **must acknowledge** each data message, or the sending system
**retransmits** it (the peer spec reports ~3× before giving up). The ACK binds to
its data message via the echoed datagram sequence in `Flags 1` — acknowledging
with any other value leaves the message outstanding and the sender keeps
retransmitting.

> No message loss or retransmission occurs in the 13-capture corpus, so the ~3×
> retransmit count and the `XFSEC` option name are carried over from the peer
> spec and unconfirmed here; the ACK itself (subtype `0x03`) is verified.

### 6.2 Peer ACK example  [CAPTURE-SPECIFIC — peer's send-message capture]

Six data messages, each acknowledged with its echoed sequence (subtype `0x07`,
status `0xFFED` = `XEIMA`; not reproduced in this corpus — see the Section 6
caveat):

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

### 6.3 Worked example — one full exchange  [CAPTURE-SPECIFIC ack line]

On an established link (all frames address `0x09`), from the peer spec. The final
ack line uses the peer's subtype `0x07`; in this corpus that ack is subtype `0x03`
(Section 6):

```
   100 → 102   I  N(S)=0 N(R)=0   21 13 00 19 00 66 00 64 FF FF 00 01 DE 08    reachability request
   102 → 100   I  N(S)=0 N(R)=1   21 13 00 13 00 64 00 66 FF FF 00 01 DE 0E    reachability reply
   100 → 102   I  N(S)=1 N(R)=1   21 13 00 0E 00 66 00 64
                                  00 00 00 10 DE 04 21 00 82 C4
                                  00 66 03 80 00 64 02 AD 00 10                data message, datagram seq 0
   102 → 100   I  N(S)=1 N(R)=2   21 13 00 07 00 64 00 66 00 00 FF ED DE 2D    delivery ack, echoes seq 0
```

---

## 7. Addressing model  [SYMBOLS + INFERRED]

A host has a **node number** (100, 102, …) and runs service tasks (XROUT, TADAD,
XMFIDO, BAK01, …), each owning one or more **ports**. A remote reference is a
32-bit **magic number** = port + system + a random component, so stale references
cannot be reused. On the wire the destination is `XMDSY` (system) + `XMDPT`
(port). XMSG provides `XFP2M` (port → magic) / `XFM2P` (magic → port) to build
and resolve addresses, and `XFRTN` ("return message") to reply to a received
message without minting a new address (it swaps src/dst).

**Frame sizing** [VERIFIED from symbols]: `X4FSO = 312` (max RX frame bytes),
`X4FRM = 20` (frame overhead), `X4LTO = 10` (link timeout XTUs). So the largest
HDLC information field is 312 bytes and the max user payload is ~**292 bytes** —
which matches the observed 292-byte ceiling on subtype-`0x0E` frames. Larger XMSG
buffers are fragmented across multiple frames (DCB chaining, Section 12).

---

## 8. Function codes (XF\*) and the send/receive sequence  [VERIFIED — symbols]

XMSG operations are invoked through **monitor call 200B** with a function code
(from `NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT`):

| Symbol | Octal | Hex | Purpose |
|--------|------:|----:|---------|
| XFDUM | 000000 | 0x00 | Dummy / no-op |
| XFDCT | 000001 | 0x01 | Disconnect |
| XFGET | 000002 | 0x02 | Get (allocate) message buffer |
| XFREL | 000003 | 0x03 | Release message buffer |
| XFRHD | 000004 | 0x04 | Read header |
| XFWHD | 000005 | 0x05 | Write header (XMDSY/XMDPT/XMSSY/XMSPT) |
| XFREA | 000006 | 0x06 | Read user data (advances XMLIX) |
| XFWRI | 000007 | 0x07 | Write user data (advances XMLIX) |
| XFMST | 000011 | 0x09 | Message status |
| XFOPN | 000012 | 0x0A | Open port |
| XFCLS | 000013 | 0x0B | Close port |
| XFSND | 000014 | 0x0C | Send message |
| XFRCV | 000015 | 0x0D | Receive message |
| XFPST | 000016 | 0x0E | Port status |
| XFGST | 000017 | 0x0F | General status |
| XFM2P | 000026 | 0x16 | Magic → port |
| XFP2M | 000027 | 0x17 | Port → magic |
| XFPRV | 000036 | 0x1E | Make privileged (e.g. routing-table updates) |
| XFRTN | 000037 | 0x1F | Return message (swap src/dst) |
| XFRRH | 000040 | 0x20 | Receive + read header |
| XFRRE | 000051 | 0x29 | Receive + read entire message |

Typical send/receive:

```
  sender:    XFGET(size) → XFWHD(dst sys/port, src sys/port) → XFWRI(data) → XFSND
  receiver:  XFRCV(port) → XFRHD → XFREA(data) → XFREL
```

---

## 9. Sub-protocol payloads

**All data (subtype `0x0E`) frames share one envelope** [VERIFIED]. Across the
whole corpus **100%** of data frames — for *every* Protocol ID `0xD8`…`0xDE` —
carry the identical XMSG sub-header of Section 5 (counter, `21 00` marker,
frame-flags, role, XMDSY/XMDPT/XMSSY/XMSPT, XMCSM, pad, XMLEN). **The Protocol ID
byte is a channel tag, not a different frame layout** — TAD, PAD, DC and DB are
the *same* structure; they differ only in the logical channel they name and in
what the trailer carries. Decode any data frame as:

```
SINTRAN header (13B)  →  XMSG sub-header  →  trailer (XMLEN bytes)
```

then decode the **trailer by content**, dispatched on `XMCSM`:
- `XMCSM 0x0100014B / 0x01000100` → LI ROUTING (`XSGSY`) parameter blocks (9.1)
- `XMCSM` low byte `0x41` = `XSLET` → an XROUT letter (TLV; e.g. to service `TADADM`)
- otherwise (e.g. `XMCSM 0x01080000`) → a TAD message chain (9.2)
- anything not recognised → raw bytes (retained verbatim)

| Proto ID | Channel | Typical trailer content |
|----------|---------|-------------------------|
| `0xDE` | ROUTING | LI ROUTING (`XSGSY`) params; 1-byte routing commands |
| `0xDD` | TAD | TAD message chain (terminal access) |
| `0xDC` / `0xDB` | DC / DB | TAD chain / terminal data forwarding |
| `0xDA` | PAD | X.25 PAD channel — in this corpus, LI SYSTEM-TAD (`TADAD`) letters |
| `0xD9` / `0xD8` | DC variants | as DC |

### 9.1 ROUTING trailer / `XSGSY` (`0xDE`)  [VERIFIED tables; INFERRED anomaly]

A ROUTING payload begins with a **1-byte command** (the byte decoded per
Section 6 when it is a short frame). Command values (from the dissector, verified
against LI ROUTING,TREE output):

| Cmd | Meaning | Cmd | Meaning |
|-----|---------|-----|---------|
| `0x00`–`0x04` | TermParam-Step 4…0 | `0x0D` | Bootstrap-Response |
| `0x05` | Propagate-Request | `0x0E` | Sync-Response |
| `0x07` | Bootstrap-Request | `0x11`/`0x12` | PAD-Resp |
| `0x08` | Sync-Request | `0x13`–`0x1E` | ConnStep-ACK (= step + 0x0A) |
| `0x0B` | Propagate-Response | | |
| `0x0C` | RouteInfo-Exchange | | |

**LI ROUTING records = XROUT `XSGSY` reply parameters** [VERIFIED]: the routing
trailer is the reply of XROUT service `XSGSY` ("get routing info for system N"),
formatted as ND standard-message parameter blocks — a sequence of 4-byte records
`[param-number][length=2][value16]`. A response carries four parameters
describing one routing-table entry; a request carries one (the system number to
look up). See [XMSG-API.md](XMSG-API.md) section 4 for the standard-message format.

| Param | Meaning (from COSMOS Guide ND-60.164, `XSGSY`) |
|------:|-----------------------------------------------|
| 1 | **System number** (first ≥ the requested one; `0` = none) |
| 2 | **Connection type** enum: `0`=Unavailable, `1`=Neighbour, `2`=Via (relay), `3`=Via network server, `4`=Local |
| 3 | **Extra info** (link index / relay system / subaddress — depends on param 2) |
| 4 | **Network info**: value ≤ `0377₈` → hop count in the low byte; ≥ `0400₈` → #WANs in the high byte, #hops in the low byte |

> **Correction.** Earlier revisions (and the dissector) labelled these records
> `XMTNO/XMROU/XMTHI/XMTRE` and decoded params 2/4 as bitfields. That was a
> **misattribution**: `XMTNO..XMTRE` are the `XFRCV` *message-type* return codes
> (Normal/Routed/High-priority/Return), which merely share the values 1–4 — they
> are unrelated to these routing records. Param 2 is a 0–4 **enum**, not a
> bitfield. Corrected here and in the dissector.

**XMCSM dispatch values** [VERIFIED]: `0x0100014B` = LI ROUTING request,
`0x01000100` = LI ROUTING response, `0x04000041` = LI SYSTEM-TAD. **The low byte
is the XROUT service code** (`0x4B` = 75 = `XSGSY` "get routing info"; `0x41` = 65
= `XSLET` "send a letter"); see the `XS*` catalog in
[XMSG-API.md](XMSG-API.md) section 6.4.

> **Stateless-RPC response anomaly** [INFERRED]: in LI ROUTING responses the
> responder fills `XMSSY/XMSPT` with the **originator's** address, not its own —
> i.e. it echoes the asker's identity as a transaction id rather than allocating a
> port. The dissector flags this on every such frame.

### 9.2 TAD trailer (`0xDD`)  [VERIFIED]

After the common XMSG sub-header, a TAD frame's trailer is a chain of
`[opcode][count][data…]` messages. The full opcode table (BDAT, RFI, ECKM, OPSV,
ISRS, CPCO, …, ~30 opcodes, verified against the K03/L07/M06 symbol tables) is
documented in [TAD/TAD-Message-Formats.md](../../TAD/TAD-Message-Formats.md) and
implemented in the dissector; this document does not duplicate it.

> **Correction.** `0xDD` frames were previously decoded as a bare TAD chain
> starting at the SINTRAN header, which mis-read the sub-header's `counter + 21 00`
> prefix as a fake "TAD opcode + count `0x21` → 33-byte Block-33". There is no
> such block — that is the XMSG sub-header. Parse the sub-header first (as for DC),
> then the TAD chain in the trailer.

### 9.3 DC / DB (`0xDC` / `0xDB`, and variants `0xD9` / `0xD8`)  [VERIFIED layout]

Terminal-data forwarding — the reference implementation of the common envelope:
XMSG sub-header, then a TAD message chain (or an XROUT letter) in the trailer. A
single-byte DC frame is the flow-control counter (see Section 6).

### 9.4 PAD (`0xDA`)  [VERIFIED envelope]

The X.25 PAD channel. **PAD frames use the identical XMSG sub-header** (not opaque
data) — verified on 100% of PAD frames in the corpus. In these captures the PAD
channel carries **LI SYSTEM-TAD** traffic: `XMCSM = 0x04000041` whose low byte
`0x41` = `XSLET` (send letter) to service **`TADADM`** — i.e. TAD *directory*
queries. So PAD here is the same envelope carrying TAD-service content. (PAD-Resp
routing commands `0x11`/`0x12` accompany virtual-circuit setup on the 103 links.)
Any genuinely opaque PAD virtual-circuit payload is retained verbatim.

---

## 10. XM5 in-kernel message header  [SYMBOLS]

The wire sub-header (Section 5) is a serialised subset of the in-kernel **XM5**
header. Full symbol set (from `XMSG-SYMBOL-LIST.SYMB.TXT`); values are word
indices within the control block, not necessarily wire byte offsets:

| Symbol | Octal | Field |
|--------|------:|-------|
| XMSTA | 000135 | Status / return code |
| XMDSY | 000136 | Destination system |
| XMDPT | 000137 | Destination port |
| XMSSY | 000140 | Source system |
| XMSPT | 000141 | Source port |
| XMCSM | 000142 | Control / session-management word |
| XMLIX | 000143 | Link / current-position index |
| XMSIZ | 000144 | Total buffer size (allocated) |
| XMDAB / XMDAW | 000145 / 000146 | Data address — bank / word offset |
| XMLEN | 000147 | User data length (bytes) |
| XMSCR | 000150 | Scramble / checksum |
| XMTIM / XMTPT | 000151 / 000152 | Timestamp / timeout |
| XMALL | 000153 | Allocation flags |
| XMPRT / XMSEQ / XMBLN | 000154 | Priority / sequence / block (union overload) |

`XMPRT`, `XMSEQ` and `XMBLN` share offset `000154` — a union whose meaning depends
on message class. The datagram sequence in `Flags 1` (Section 4.2) corresponds to
`XMSEQ`. Pool-management symbols: `XMFRE` (free count), `XMUSE` (in-use), `XMMAX`
(max), `XMLIM` (limit), `XMCUR` (current handle).

---

## 11. Status codes (XR\* / XE\*)  [SYMBOLS]

`XR*` codes returned in `XMSTA` (small non-negative); negative `XE*` codes from
the COSMOS Programmer Guide Appendix D:

| Symbol | Value | Meaning |
|--------|------:|---------|
| XRSOK | 0 | OK / success |
| XRISN | 1 | Invalid system number |
| XRUNN | 2 | Unknown node name |
| XRDDF | 3 | Destination not defined |
| XRNSP | 4 | No such port |
| XRMTL | 8 | Message too large |
| XRSMF | 9 | System message format error |
| XRPRV | 10 | Privilege violation |
| XRNRO | 12 | No route |
| XEIMA | −19 | Invalid magic number / remote end terminated |
| XENSE | −34 | Network sequencing error (purely internal significance) — a received datagram's sequence number ≠ expected |

`XENSE` confirms the ND term "datagram identifier (sequence number)" for the
`Flags 1` field.

---

## 12. XMSG ↔ HDLC driver mapping  [PARTIAL — from NPL]

Each XMSG message on the wire is described by a **DCB (Data Control Block)** the
HDLC controller DMAs from (`MP-P2-HDLC-DRIV.NPL`): `LBYTC` (byte count), `LMEM1`
(bank bits, from `MASTB`), `LMEM2` (buffer address, `OMSG + CHEAD`), `LKEY`
(continuation / list-end, `FSERM`). Large buffers split across multiple DCBs: the
first carries RSOM (start-of-message), the last REOM (end-of-message) — the
SDLC/HDLC convention. See [HDLC-Frame-Format-Reference.md](../../Devices/HDLC/HDLC-Frame-Format-Reference.md)
for the hardware/DMA detail.

---

## 13. Corrections applied during consolidation

What the source-precedence rule changed relative to the peer's original spec and
the old dissector README:

| Claim | Was | Now | Basis |
|-------|-----|-----|-------|
| SINTRAN offset 3 | "Packet Length" (README) | Packet **Subtype** (message kind) | Subtype `0x0E` spans 34–292 B; three subtypes share 14 B |
| Delivery ACK | subtype `0x07`, status `0xFFED` | subtype **`0x03`**, `Flags2 = 0x0001` (this corpus) | 0 `0x07` frames, 0 `FF ED` bytes in 6379 raw frames |
| `XMDPT` encoding | `port ≪ 7` | encoding unconfirmed (`≪ 7` refuted) | `XMDPT & 0x7F` non-zero on most frames |
| Offset 12–13 | one decrementing 16-bit counter | offset 12 = fixed Protocol ID; counter is payload byte 0 | 7 distinct stable proto IDs observed |
| Subtypes ↔ `5SRPI/5DPIT/5SSPD` | numeric lead to segment-5 symbols | rejected | those are kernel memory-segment IDs (`5DPIT=023` is the DPIT data segment), not packet types |
| Subtype `0x03` | undocumented | the ACK / flow-control frame | correlation analysis (Section 6) |

---

## 14. Open questions

1. Does real `XFSND` user-data traffic use subtype `0x07` (not `0x03`) for the
   ACK, or was `0x07` a mis-read? One frame from the peer's capture settles it.
2. Is `0xFFED` a genuine status word (`XEIMA`?) or a different field? Absent here.
3. The true encoding of the logical port into `XMDPT` (`≪ 7` is refuted).
4. Exact byte serialisation of the XM5 header on the wire (word-for-word vs
   repacked) — needs the `XFSND` code path in `MP-P2-HDLC-DRIV.NPL`.
5. Routing-message body serialiser (`XMTNO/XMROU/XMTHI/XMTRE`) — likely in an
   `XC-P2-*.NPL` / `RP-P2-ROUT*.NPL` file.
6. File-transfer and mail body framing (opcode values) — not yet located in NPL.

---

## 15. Constants quick reference

| Constant | Value |
|----------|-------|
| HDLC flag / escape / mask | `0x7E` / `0x7D` / `0x20` |
| FCS polynomial / init / good residue | `0x1021` (reflected `0x8408`) / `0xFFFF` / `0xF0B8` |
| LAPB address — link setup / data | `0x01` / `0x09` |
| SABM / UA / RR | `0x3F` / `0x73` / `0x01\|(N(R)<<5)` |
| SINTRAN Marker 1 / Marker 2 (normal / relay) | `0x21` / `0x13` / `0x12` |
| Packet Subtype — ACK / data / reach-reply / reach-request | `0x03` / `0x0E` / `0x13` / `0x19` |
| Flags 2 — short/control / data | `0x0001` / `0x0400,0x0108,0x0008` |
| Protocol ID — ROUTING / TAD / DC / DB / PAD | `0xDE` / `0xDD` / `0xDC` / `0xDB` / `0xDA` |
| XMSG sub-header marker | `0x21 0x00` |
| Max HDLC info / max user payload | 312 B (`X4FSO`) / ~292 B |
| Monitor call | 200B, function code in `XF*` range |

---

## 16. Reproduction

The corpus statistics were produced by an independent validator: extract raw TCP
payloads with `tshark -T fields -e tcp.payload`, reassemble each TCP stream per
direction by `tcp.seq`, de-frame HDLC (`0x7E` split, `0x7D` unstuff), keep only
frames passing the FCS-16 check (Section 2) — which rejects the terminal and DAP
telemetry noise present in several captures — then parse and tabulate the fields
above. Captures: the 13 `.pcapng` files in the separate sibling **X25Emulator**
repository (`pcap/` directory).

---

## 17. Implementation checklist  [from peer spec, corrected]

To exchange messages with a SINTRAN III system over HDLC, an implementation must:

1. Frame / de-frame the byte stream (Section 2), validating FCS on receive.
2. Establish the LAPB link (Section 3.4) and maintain `V(S)`/`V(R)`; send periodic
   RR keepalives carrying the node number.
3. Answer reachability requests (subtype `0x19`) with replies (`0x13`) so the peer
   considers it accessible (Section 5.1).
4. Decode incoming data messages (subtype `0x0E`, Section 5).
5. **Acknowledge every data message** with a delivery ACK that echoes the
   message's datagram sequence in `Flags 1` (Section 6) — otherwise the peer
   retransmits. In this corpus the ACK is subtype `0x03` (`Flags 2 = 0x0001`); the
   peer's send-message capture uses subtype `0x07` (status `0xFFED`).
6. To send: address the destination as `XMDSY` = system, `XMDPT` = port (exact
   port encoding unconfirmed — the peer's `port ≪ 7` is refuted in this corpus,
   Section 5); to reply, swap source/destination (the `XFRTN` pattern) and fill
   the buffer to its declared size.

---

**Document path:** `SINTRAN/XMSG-PROTOCOL.md`
**Supersedes:** `xmsg-hdlc-protocol.md`, `xmsg-hdlc-protocol-VALIDATION.md`,
`XMSG-Protocol-Analysis.md`, and the format sections of the Wireshark README
(all archived).
