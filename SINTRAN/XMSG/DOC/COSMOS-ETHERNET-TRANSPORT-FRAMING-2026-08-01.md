# COSMOS/XMSG over Ethernet - the transport framing (2026-08-01)

Until now every XMSG capture in the corpus arrived over HDLC, so the whole transport analysis
(LAPB addressing, sequence numbers, the T1 retransmit ladder) was written against a link that
COSMOS does not have to use. Both live machines were reconfigured onto Ethernet on 2026-08-01,
which lets the Ethernet path be decoded directly instead of inferred.

This documents what the frames look like on Ethernet, what carries over from the HDLC work, and
what does not.

Claims are marked **VERIFIED** (seen on the wire or stated in a vendor manual), **INFERRED**, or
**UNKNOWN**.

---

## 1. There is no *LAPB* on Ethernet - but there IS a link layer [VERIFIED]

> **CORRECTION 2026-08-01, later the same day.** An earlier revision of this document claimed
> that COSMOS on Ethernet has no link layer at all and that reliability moves up into the
> SINTRAN header. **That was wrong**, and it was written from the 2026-07-24 captures of a link
> that never came up - which therefore contained only retransmissions of a single frame and no
> acknowledgements to notice. A live capture shows an 11-byte ND link header with its own
> sequence numbers and acknowledgements sitting between the LLC header and the SINTRAN header.
> Section 2a documents it. Do not build a node on the earlier claim.

The IEEE apparatus in `LAPB-REQUIREMENTS.md` - SABM/UA, the address parity bit, V(S)/V(A)/V(R),
REJ go-back-N, RNR, T1/N2 - **is HDLC-only**, and none of it appears here. The 802.3 frame uses
plain LLC1 UI, which is itself connectionless and unsequenced.

But COSMOS does not rely on that. It carries its own link protocol **inside** the LLC payload,
with a send sequence number and an explicit acknowledgement frame. So the responsibility has not
moved up into the SINTRAN header; it has moved sideways, into an ND-specific header that does the
same job LAPB does on HDLC.

Practical consequence: a node implementation cannot reuse the LAPB state machine here, and it
cannot omit a link layer either. It needs this one.

---

## 2. Frame layout [VERIFIED]

Captured from `ethernet-conn-to-100-102.pcapng`, a frame from node 102 to node 100:

```
offset  bytes                 meaning
------  --------------------  --------------------------------------------------
00-05   08 00 26 64 00 00     destination MAC - node 100
06-11   08 00 26 66 00 00     source MAC      - node 102
12-13   00 0e                 802.3 LENGTH field (14), NOT an EtherType
14      a8                    LLC DSAP - the ND/COSMOS service access point
15      a8                    LLC SSAP
16      03                    LLC control = UI (unnumbered information)
17..    ...                   COSMOS payload, length = (802.3 length) - 3
```

Points that are easy to get wrong:

- **Bytes 12-13 are a LENGTH, not a type.** The value is <= 1500, which is what tells a receiver
  to parse an 802.3/LLC frame rather than a DIX/EtherType one. A decoder that reads these two
  bytes as an EtherType will look up `0x000E` and find nothing.
- **There is no SNAP header.** DSAP/SSAP are `0xA8`/`0xA8` directly; there is no `AA AA 03`
  escape and no OUI/type after the control byte.
- **The declared length covers the LLC header.** Payload length is the 802.3 length minus the
  3 LLC bytes, so the 14 above means 11 bytes of COSMOS payload.
- **Short frames are padded to 60 bytes** and the padding is NOT part of the message. Trust the
  declared length, not the frame size. In the captured frame there are four non-zero bytes
  immediately past the declared end (`00 01 cf 32`) before the zero padding begins - their origin
  is **UNKNOWN** (stale transmit-buffer contents is one possibility, not established). Do not
  read them as protocol.

---

## 2a. The ND link header - 11 bytes [VERIFIED]

Captured 2026-08-01 from a working 100 <-> 102 link (`CONNECT-TO D100` from node 102), 96 frames.
Immediately after the three LLC bytes:

```
offset  field   example       meaning
------  -----   -----------   ---------------------------------------------------
+0      b0      0b            the HEADER LENGTH, 11 - see the correction below
+1      b1      02            constant on every frame observed, meaning unknown
+2      b2      20 | 3f       frame kind - see the correction below, the HIGH
                              NIBBLE is the NPDU type
+3      b3      00            constant on every frame observed
+4      b4      43            sequence number (see below)
+5..6   id1     5062          SENDER's link id
+7..8   id2     59c1          RECEIVER's link id
+9..10  plen    0030          payload length ON DATA FRAMES ONLY - see the
                              correction below
```

**The length relation holds on every captured DATA or ACK frame**, in both directions:

```
802.3 length = 3 (LLC) + 11 (this header) + plen
```

so on those two kinds `plen` is the authoritative payload length and the 802.3 length is derivable
from it. An ACK carries `plen = 0` and the frame is then padded out to the 60-byte minimum.

**The link ids are per-node and swap with direction.** Node 102 is `0x5062` and node 100 is
`0x59c1` throughout this capture; a frame from 102 to 100 carries `id1=5062 id2=59c1`, and the
reply carries them the other way round. These are **not** the node numbers (100/102) and not the
MAC-embedded system numbers - where they come from is **UNKNOWN**. They are stable for the life of
the link, but not across sessions: a both-ends capture on 2026-08-03 saw D100 = `0x2F48` and
D102 = `0x28A4`. Both pairs are real observations; keep both in mind before treating any of these
four values as a constant.

**b4 is a send sequence with explicit acknowledgement.** Data frames in each direction carry an
increasing b4 (observed `0x43, 0x44, 0x45, ...`), and the peer answers with a `0x3f` frame whose
b4 is **the data frame's b4 plus one** - i.e. the next expected sequence, the usual convention.
Each direction runs its own counter.

Worked example, the first four frames of the capture:

```
102->100  b2=20 b4=43 plen=0030   data: the XSLET letter to *TADADM
100->102  b2=3f b4=44 plen=0000   ack  (43 + 1)
100->102  b2=20 b4=43 plen=000e   data: the SINTRAN subtype 0x03 ACK
102->100  b2=3f b4=44 plen=0000   ack  (43 + 1)
```

Note that the third frame is a **SINTRAN-level** ACK (header subtype `0x03`) travelling as
**link-level DATA**, and it is itself link-acknowledged by the fourth. There are two independent
acknowledgement layers and they must not be conflated.

**UNKNOWN as of 2026-08-01**: whether b2 takes values other than `0x20` and `0x3f` (no others were
observed in this capture); whether there is a negative acknowledgement / reject; the window size;
the retransmit timer. The first of those was answered two days later - see the corrections below.

### CORRECTIONS 2026-08-03

Three things above were written from a single capture of one healthy link and are now known to be
incomplete or wrong. What was believed, and why it changed, is kept on purpose.

**1. b0 is the header LENGTH, not a magic constant. [VERIFIED]**
The table used to read "+0 b0-b1 `0b 02` constant on every frame observed". That is true but it
hides the meaning: `0x0B` is 11, the length of this header, and the 802.3 length is always
`3 (LLC) + 11 + payload`. The sender computes it. This is why every search for a literal `0B02`
signature in an ND binary came up empty - there is no such constant in the code to find. `b1 = 0x02`
is still unexplained.

**2. b2 has at least four values, and its high nibble is the NPDU type. [VERIFIED]**
Two more kinds were captured on 2026-08-03, and the ENCOS monitor `encos-mon-ii-b01.prog` was
carved to find out what they are. Its trace decoder dispatches at `ram:2556` on an internal type
index bounded by 7 (`ram:26ad`) through an eight-entry table at `ram:26ae`. The index names are ISO
transport NPDU types:

| index | type | meaning | wire byte |
|-------|------|---------|-----------|
| 0 | CR | connection request | `0x0F` CONFIRMED |
| 1 | CC | connection confirm | UNKNOWN, never captured |
| 2 | DT | data | `0x20` CONFIRMED |
| 3 | AK | acknowledge | `0x3F` CONFIRMED |
| 4 | WO | window | UNKNOWN, never captured |
| 5 | DR | disconnect request, by user | UNKNOWN, never captured |
| 6 | DR | disconnect request, by network service | `0x6F` CONFIRMED |
| 7 | DC | disconnect confirm | UNKNOWN, never captured |

Indices 0 and 1 were read directly off the code the table branches into. Indices 2-7 follow the
order of the format-string blob at `ram:2642`-`ram:26a0` and are STRONGLY INDICATED but were not
individually read.

The wire byte's HIGH NIBBLE is the index. That is not circular: `0x20` = data and `0x3F` =
acknowledge had been pinned from behaviour on the wire before the table was found, and the table,
found later and separately, puts DT at 2 and AK at 3. Two routes, same answer.

The LOW NIBBLE is **NOT explained**. `0x0F`, `0x3F` and `0x6F` all end in `F` while `0x20` ends in
`0`. Do not invent a meaning for it. For the four types never seen on the wire, no byte is claimed -
the high nibble presumably equals the index, but the low nibble would be a guess.

**3. `plen` is NOT always a payload length. [VERIFIED]**
This is the one that could bite an implementation. Offsets `+9..+10` mean different things per
kind:

| kind | field | meaning |
|------|-------|---------|
| `0x20` DT | 14 .. 140 | the real payload length |
| `0x3F` AK | 0 | no payload |
| `0x0F` CR | `0x0066` | the SENDER'S OWN SYSTEM NUMBER (102), payload length is ZERO |
| `0x6F` DR | `0x0101` | UNKNOWN, payload length is ZERO |

A `0x0F` or `0x6F` frame carries **no payload at all**: its 802.3 length is `0x000E` = 3 + 11 + 0,
and every byte after the eleventh is Ethernet padding to the 60-byte minimum. Earlier readings that
assigned meaning to that tail were reading padding. **The 802.3 length field is the authority on how
many payload bytes are present.**

Also on a CR frame the SENDER link id is `0x0000` - there is no link yet.

In the C# library `NdLinkHeader.PayloadLength` keeps its name and behaviour (callers and tests
depend on it) but is now documented as valid only when `IsData`, and `NdLinkHeader.TrailingField`
reads the same two bytes without claiming they are a length.

Full working, frame by frame:
[COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md](COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md),
sections 2b, 2c and 2d.

### The SINTRAN header is carried verbatim [VERIFIED]

This is the finding that matters most for the rest of the project. The bytes after the link
header are the **same 7-word SINTRAN header documented for HDLC**, unchanged:

```
2113 000e 0064 0066 0021 0400 d9f3
 |    |    |    |    |    |    |
 |    |    |    |    |    |    +-- word 6, the checksum
 |    |    |    |    |    +------- Flags 2
 |    |    |    |    +------------ Flags 1
 |    |    |    +----------------- source node 102
 |    |    +---------------------- destination node 100
 |    +--------------------------- type:subtype (Data)
 +-------------------------------- markers
```

**The word-6 checksum verifies on Ethernet.** Hand-checked on four frames spanning both
directions and both the `0x0e` (data) and `0x03` (ACK) subtypes. For the header above, the
ones-complement sum with end-around carry of words 0-5 is `0x260C`, and `~0x260C = 0xD9F3`, which
is the word on the wire. Three further frames:

| words 0-5 | sum | expected | on wire |
|---|---|---|---|
| `2113 0003 0066 0064 0021 0001` | `0x2202` | `0xDDFD` | `0xDDFD` |
| `2113 000e 0064 0066 0022 0400` | `0x260D` | `0xD9F2` | `0xD9F2` |
| `2113 000e 0066 0064 0023 0108` | `0x2316` | `0xDCE9` | `0xDCE9` |

So the checksum rule in `XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md`, derived entirely from
HDLC captures and the kernel carve, is **transport-independent**. Every header-level formula -
the checksum, the Flags 1 datagram sequence, the subtype set - transfers to Ethernet unchanged,
and the C# `ComputeHeaderChecksum` needs no per-transport variant.

---

## 2b. Route-through, captured across two different transports [VERIFIED]

Captured 2026-08-01. Three nodes: 103 reaches 100 over **HDLC**, 102 reaches 100 over
**Ethernet**, and `CONNECT-TO D102` from node 103 makes 100 relay between the two. Both links were
captured in one file, so the same message is visible entering and leaving the relay.

The XSLET letter to `*TADADM`, on both sides of node 100:

```
103 --HDLC--> 100     2113 000e 0066 0067 000d 0400 da04   + body
100 --ETH-->  102     2112 000e 0066 0067 000d 0400 da05   + body, byte-identical
```

The relay changes **exactly two things**:

- **Word 0: `0x2113` -> `0x2112`** - the relayed-hop marker.
- **Word 6: recomputed.** `0xda04` -> `0xda05`.

and changes **nothing else**. Destination `0066`, source `0067`, Flags 1 `000d`, Flags 2 `0400`
and the entire message body all pass through untouched. Verified in both directions and on three
separate message pairs in the capture:

| direction | entering relay | leaving relay |
|---|---|---|
| 103 -> 102 | `2113 000e 0066 0067 000d 0400 da04` | `2112 ... da05` |
| 102 -> 103 | `2113 0003 0067 0066 000d 0001 de0e` | `2112 ... de0f` |
| 102 -> 103 | `2113 000e 0067 0066 000a 0108 dcff` | `2112 ... dd00` |

So **the SINTRAN header carries the ORIGINAL endpoints end to end**. It is not rewritten
hop-by-hop, and a relay must not put its own node number in it.

### This retro-explains the old "counter re-stamp"

Earlier notes recorded the relayed hop as *"`0x12` relayed hop (Counter re-stamped +1 by the
relay)"*. There is no counter. Word 0 decreases by exactly 1, so the ones-complement sum decreases
by 1 and its complement increases by exactly 1 - including across the byte boundary
(`dcff` -> `dd00`). The observation was always the checksum moving, and it is independent
confirmation of
[XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md](XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md)
from a direction that was never tested when that document was written.

### Bulk validation on a working relayed session [VERIFIED]

`CONNECT-TO D103` from node 102 relayed through 100 succeeds (`CONNECTION ESTABLISHED`, TAD 770)
and gives a full session rather than a failed handshake. Capturing both links and recomputing
word 6 for every SINTRAN header found in it:

```
SINTRAN headers with a VALID word-6 checksum : 128
   of which marker 0x2113 (normal)           :  72
   of which marker 0x2112 (relayed)          :  56
candidate positions that did not validate    :   0
```

So the checksum is correct on **every** frame, relayed and direct, across HDLC and Ethernet, with
no exceptions. Capture:
`E:\Dev\Ronny\X25Emulator\pcap\ethernet-hdlc-ROUTE-THROUGH-WORKING-102-via-100-to-103-2026-08-01.pcapng`

### Acknowledgements are end-to-end, not per-relay [VERIFIED]

The subtype `0x03` ACK for the relayed letter was sent by **node 102, the real destination**, and
was itself relayed back through 100 to 103 (marker flipped, checksum adjusted, like any other
datagram). Node 100 did not generate an ACK on 102's behalf. A relay implementation that
acknowledges locally would break the end-to-end guarantee.

### What this requires of the C# implementation [STATUS: BUILT AND TESTED, NOT WIRED IN]

**CORRECTED 2026-08-06.** The paragraph below said the node "cannot relay" and that
`Marker2Relay` is "never assigned". That was true when written and is now WRONG - it was
repeated as fact by a later audit, so it is corrected here rather than quietly deleted.

What actually exists:

 - `SRC\Xmsg.Node\Seam\SintranDatagramRelay.cs` - the stamping rules. `MakeRelayed` /
   `ToRelayed` set `Marker2Relay` and recompute word 6; `IsRelayed`, `HasValidChecksum`,
   `GetDestinationNode` and `GetSourceNode` round it out.
 - `SRC\Xmsg.Node\Seam\DatagramRelay.cs` - the routing decision. `Route(fromLink, datagram,
   length)` looks the destination up in a per-node route table, refuses to send a datagram back
   out of the link it arrived on, stamps it, and forwards. It originates no acknowledgement, so
   the end-to-end guarantee described above is preserved.
 - `SRC\Xmsg.Node.Tests\DatagramRelayTests.cs` - covers the stamping, the checksum, the
   no-route and wrong-marker refusals, and the loop-back-out-the-same-link refusal.

**The real gap is that nothing outside the tests ever constructs a `DatagramRelay`.** It is a
complete, tested component with no production caller. Wiring it up needs a node holding TWO
links and a route table populated from `topology.json` - which is a genuine feature, not a
missing line. Steps 1-5 below still describe what it does, and are all implemented.

To support route-through:

1. On receive, compare `Header.DestinationNode` with this node's number. If it differs and a route
   exists, forward instead of processing locally.
2. When forwarding, change **only** `Marker2` to `SintranHeader.Marker2Relay` and recompute word 6
   with the existing `XmsgEnvelope.ComputeHeaderChecksum`. Copy every other header field and the
   whole body verbatim.
3. Re-frame for the outgoing link. Link-layer sequence numbers and link ids (section 2a) are
   **per-hop** and must be generated fresh for the outgoing link; the SINTRAN header is not.
4. Do **not** originate acknowledgements for traffic being relayed.
5. A relay between transports must translate framing (HDLC LAPB on one side, ND link header over
   LLC1 on the other) while leaving the SINTRAN header alone apart from step 2.

---

## 3. The MAC address encodes the ND system number [VERIFIED]

Per `ND-60.197.01 EN Ethernet Basic Software Programmer Guide` section 2.4, and fully documented
in [MAC-ADDRESS-ASSIGNMENT-MULTI-CARD.md](COSMOS-RE/MAC-ADDRESS-ASSIGNMENT-MULTI-CARD.md):

| Bytes | Contents |
|---|---|
| 0-2 | `08 00 26` - the Xerox/ND vendor prefix |
| 3-4 | **ND system number, 16 bits, stored in REVERSED byte order** |
| 5 | physical user code |

The manual: *"ND system numbers, in the range 0 - 177777B, are contained (in reversed order) in
characters 4-5 of the address string. Physical user numbers are contained in character 6."*

So `08 00 26 64 00 00` is system `0x0064` = 100, physical user 0.

**The byte order is opposite to the HDLC link.** In the SINTRAN header the node number is a
big-endian 16-bit word (node 100 = `00 64`); in the MAC the same number is little-endian
(`64 00`). Both are 16-bit and cover the full 0-65535 range - the field width is the same, only
the storage order differs. Code that moves a node number between the two representations must
swap. Two nodes numbered under 256 make an 8-bit reading look plausible; it is wrong.

The card has no address of its own - no address PROM, no EPROM. SINTRAN writes the six bytes
into card DRAM at `LNMAPHYSIC` (`0x1885E`) with host command 0 at bring-up. The MAC is entirely
a software construction.

### The physical user byte selects a protocol family

Same manual section: the hardware **does not check bits 7-6 of the destination address**, so each
interface answers to four addresses:

| bits 7-6 | family |
|---|---|
| 00 | IEEE |
| 01 | DIX |
| 10 | user-written protocol |
| 11 | ND (COSMOS) |

DIX (TCP/IP) and COSMOS traffic were designed to arrive at the same physical interface under
different physical user addresses.

**UNKNOWN**: the captured COSMOS frames carry physical user `0x00` (bits 7-6 = `00` = IEEE), not
the `11` that the table would suggest for COSMOS. Since the hardware ignores those bits on the
receive match, both work, so the capture cannot distinguish "COSMOS does not use the encoding" from
"COSMOS uses it only when more than one family is attached". Not resolved - do not assume either.

---

## 4. What the existing Ethernet captures are, and are not

`ethernet-conn-to-100-102.pcapng` and `ethernet-list-routing.pcapng` (both 2026-07-24) were taken
with the card in raw-NIC mode (`--net=select:192.168.56`), so the ND frames appear as ordinary
Ethernet frames alongside the host's own traffic.

**They capture a link that never came up.** The ND traffic in `ethernet-conn-to-100-102.pcapng` is
node 102 sending the same 11-byte frame nine times at roughly 100 ms intervals with no reply from
100 - a retransmit ladder, not a conversation. They are good enough to establish the framing above
and nothing more. Do not derive field semantics from them.

---

## 5. Capturing the traffic

The emulator offers three Ethernet backends, and the choice determines whether the traffic can be
captured at all:

| `--net=` | transport | capturable on this host? |
|---|---|---|
| `udp` | multicast `239.3.9.4:3094` | **NO** - see below |
| `select:<prefix>` | raw injection on a real NIC | yes, on that NIC |
| `listen:<port>` / `tcp:<host>:<port>` | point-to-point TCP | yes, on the loopback adapter |

**VERIFIED 2026-08-01**: with both emulators on the same host and `--net=udp`, Windows loops the
multicast back inside the kernel and it reaches no capturable adapter. Confirmed by capturing with
traffic demonstrably flowing (a `CONNECT-TO D100` that reported `CONNECTION ESTABLISHED`) on the
loopback adapter and on all three interfaces carrying a `224.0.0.0/4` route: zero packets on every
one. This is a property of the host network stack, not a filter or permissions mistake - do not
retry it.

For a capturable two-node setup on one host, use the point-to-point TCP backend:

```
HDLC1 (node 100):  device add ETH 0 --net=listen:5010
HDLC2 (node 102):  device add ETH 0 --net=tcp:127.0.0.1:5010
```

then

```
tshark -i \Device\NPF_Loopback -f "tcp port 5010" -w out.pcapng
```

This is the same shape as the HDLC capture setup (loopback TCP on 10362), which is why that one
always worked.

**Frames inside this TCP link are length-prefixed** [VERIFIED]: two bytes, big-endian, giving the
length of the Ethernet frame that follows, then the raw frame starting at the destination MAC.
The emulator sends the prefix as its own small write, so in a capture the prefix and the frame
usually arrive as two separate TCP segments (a 2-byte one then an N-byte one). A decoder must
reassemble the stream rather than assume one frame per segment. This prefix is a property of the
*emulator's* transport, not of COSMOS - it does not exist on real hardware.

### Driving the machines

Node 100's terminal is TCP 9010, node 102's is 9102. The system name is `D100` / `D102` - the bare
number is rejected with `Unknown remote system name 100`. The operational traps from
`XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md` section 7 all still apply: one connection for the
whole sequence, ESC first, ESC again to abort a running program and get back to the prompt.

---

## 6. Open items on this transport

Both of the questions this section originally listed were **answered** by the 2026-08-01 live
capture and are now sections 2a and 2a's closing subsection: the 11 bytes are the ND link header,
and the SINTRAN header is carried verbatim with a verifying checksum. What remains:

- **Where the link ids (`0x5062`, `0x59c1`) come from.** They are stable per node but are neither
  the node number nor the MAC system number.
- **Whether b2 has values beyond `0x20` and `0x3f`** - in particular a reject/negative
  acknowledgement, and what happens on loss. Only a link with induced errors will show this.
- **The window size and retransmit timer** of the link layer.
- **Route-through**: what a relayed frame looks like on this transport, and whether the `0x12`
  relayed marker and the relay's counter re-stamping behave as they do on HDLC. A three-node
  capture (103 on HDLC to 100, 102 on Ethernet to 100) is the experiment.
- The physical user code discrepancy in section 3.
- The four non-zero bytes past the declared length in section 2. Now less mysterious: `plen`
  bounds the payload precisely, so anything past it is padding or stale buffer regardless.

---

## Related

- [MAC-ADDRESS-ASSIGNMENT-MULTI-CARD.md](COSMOS-RE/MAC-ADDRESS-ASSIGNMENT-MULTI-CARD.md) - the
  address format, and the unresolved multi-card question
- [COSMOS-MULTI-NODE-NETWORK-2026-07-25.md](COSMOS-RE/COSMOS-MULTI-NODE-NETWORK-2026-07-25.md) -
  multi-transport routing, `LIST-ROUTING-INFO`, and the LAN/WAN distinction
- [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md) - the SINTRAN header and envelope, written against HDLC
- `LAPB-REQUIREMENTS.md` - the HDLC link layer, which section 1 says does not apply here
