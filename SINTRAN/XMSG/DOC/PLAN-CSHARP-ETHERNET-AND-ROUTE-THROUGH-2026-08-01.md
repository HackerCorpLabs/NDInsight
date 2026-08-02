# Plan: Ethernet transport and route-through in the C# node (2026-08-01)

Written after the 2026-08-01 live captures decoded the COSMOS Ethernet transport and route-through
(see [COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md](COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md)).
Two capabilities are missing from `SINTRAN\XMSG\SRC` and both are now fully specified by capture.

Nothing here is speculative design: every wire fact cited is VERIFIED against a capture, and the
open questions are called out as such rather than designed around.

---

## 0. Why this is cheap - the existing seam is already right

`Xmsg.Node\Seam\ILink.cs` documents itself as:

> *"The bottom seam of the stack: an established data link that delivers opaque L3 payloads up as
> events and accepts opaque L3 payloads down as method calls. Nothing above this interface knows
> about HDLC framing, byte-stuffing, FCS, or LAPB; the link knows nothing about which L3 protocol
> it carries."*

That is exactly the boundary Ethernet needs. `LapbLayer` + `LapbLayerAdapter` implement `ILink`
today; an Ethernet implementation of the same interface gives the whole stack - `XmsgLayer`,
`XmsgNode`, TAD, the servers - Ethernet support with **no changes above the seam**.

The one wrinkle: `ILink`'s XML doc mentions LAPB in three member descriptions ("initiates LAPB
establishment", "as a LAPB I-frame"). Those become transport-specific lies once a second
implementation exists and must be reworded to the neutral contract the interface summary already
states. That is a doc fix, not a signature change.

---

## 1. Ethernet transport - new project `Xmsg.Ethernet`

Mirrors the existing `Xmsg.Hdlc` project. Five types, bottom up.

### 1.1 `NdMacAddress`

The 48-bit station address, VERIFIED against `ND-60.197.01` section 2.4:

```
bytes 0-2   08 00 26                    ND/Xerox OUI
bytes 3-4   ND system number, 16-bit, BYTE-REVERSED (little-endian)
byte  5     physical user code
```

API: `FromSystemNumber(ushort system, byte physicalUser = 0)` and
`TryGetSystemNumber(out ushort system)`.

**The byte reversal is the whole point of this type.** In the SINTRAN header a node number is
big-endian (`00 64`); in the MAC it is little-endian (`64 00`). Two nodes numbered under 256 make an
8-bit reading look correct, so the conversion must be a named, tested type rather than inline
shifts. One test with a system number above 255 (e.g. 300 -> `2c 01`) locks it.

The physical user code's top two bits select protocol family (`00` IEEE, `01` DIX, `10` user,
`11` ND/COSMOS). Model it as a `[Flags]`-free small enum plus the 6-bit remainder; default 0,
because that is what the live capture shows COSMOS actually using.

### 1.2 `Ieee8023Frame`

Parse/build `dstMac | srcMac | length | DSAP | SSAP | control | payload`, with:

- `DSAP = SSAP = 0xA8`, `control = 0x03` (UI). Constants, named `NdCosmosSap` and `LlcUnnumberedInformation`.
- Bytes 12-13 are a **LENGTH, not an EtherType**. Guard: reject a parse where the value is > 1500,
  because that is a DIX frame and not ours.
- `length = 3 + linkHeaderLength + plen`. Verified on every captured frame.
- Padding to the 60-byte minimum on transmit; on receive, **trust the declared length**, never the
  buffer length.

### 1.3 `NdLinkHeader` - 11 bytes

```
+0..1  0x0b 0x02      constant
+2     kind           0x20 = data, 0x3f = acknowledgement
+3     0x00           constant
+4     sequence
+5..6  senderLinkId
+7..8  receiverLinkId
+9..10 payloadLength
```

Straight struct with `TryParse`/`Write`. `kind` becomes an enum `NdLinkFrameKind { Data = 0x20,
Acknowledge = 0x3f }`.

**UNKNOWN and must be handled defensively, not invented:** whether other `kind` values exist
(a reject / negative acknowledgement was never observed), the window size, and the retransmit
timer. The parser must accept an unknown `kind` and surface it rather than throw, so an unexpected
value shows up in a log instead of taking the link down.

### 1.4 `NdLinkLayer`

The sequence/acknowledgement state machine - the Ethernet counterpart of `LapbLayer`.

VERIFIED behaviour to implement:
- Each direction runs its own send sequence, observed stepping `0x43, 0x44, 0x45, ...`.
- On receiving a `Data` frame, reply with an `Acknowledge` frame carrying **the received sequence
  plus one** and `payloadLength = 0`.
- Link ids are per-node and stable for the life of the link; they swap with direction
  (`id1` = sender, `id2` = receiver).

**UNKNOWN: where the link ids come from.** Node 102 used `0x5062` and node 100 `0x59c1`; these are
neither the node number nor the MAC system number. Until that is carved, the layer must **learn the
peer's id from the first frame received** and take its own from configuration. Do not synthesise
one from the node number - that would be a fabricated constant of exactly the kind
`no-hardcoding-derive-like-the-microcode` warns against.

### 1.5 `EthernetLink : ILink` and the framed transport

`IByteDuplex` is byte-oriented and wrong for Ethernet, which is frame-oriented. Add:

```csharp
public interface IFrameDuplex
{
    Task<int> ReadFrameAsync(byte[] buffer, CancellationToken ct);   // 0 = end of stream
    Task WriteFrameAsync(ReadOnlyMemory<byte> frame, CancellationToken ct);
}
```

**All three RetroCore transports must be supported**, mirroring
`Emulated.HW\Common\Network\EthernetBackendFactory.cs` and selected by the same spec strings:

| spec | transport | notes |
|---|---|---|
| `udp` / `udp:<group>:<port>` | multicast, default `239.3.9.4:3094` | any number of peers; NOT capturable same-host |
| `tcp:<host>:<port>` / `listen:<port>` | point-to-point, default port 3094 | two parties only |
| `pcap:<interface>` | real host adapter via SharpPcap/npcap | needs the SharpPcap dependency |

**CORRECTED 2026-08-01 - the TCP wire format is not just a length prefix.** It is a one-time
**5-byte handshake `['R','E','T','H', version]`** (`52 45 54 48 <ver>`, magic = "RETH", RetroCore
ETHernet bridge), sent by BOTH ends and validated on the four magic bytes, and only then the
`[u16 big-endian length][frame]` stream. An earlier revision of this plan described only the length
prefix, because every capture we had was taken mid-session and never contained a connect. A client
that skips the handshake will be dropped.

### The multi-client hub ALREADY EXISTS - do not build one

`Emulated.HW\Common\Network\TcpEthernetRelay.cs` is a central Ethernet **hub over TCP**: it accepts
many client connections and forwards each frame to every other client, so any number of emulators -
and, in its own words, *"a future COSMOS application"* - form one virtual Ethernet segment across
hosts or the internet. Clients dial OUT (`--net=tcp:<relay-host>:<port>`), so it works through NAT
with no per-client port forwarding. It is a dumb hub: no MAC learning, never echoes to the sender.

So the 10-to-100-node "over the internet" topology needs **no new server** from us. What we build is
a **client** that speaks the `TcpEthernetBackend` format above and dials into that relay like any
other node.

Also add `InMemoryFrameDuplex` for deterministic tests, mirroring the existing `InMemoryDuplex`.

Note in the XML docs that the handshake and length prefix are properties of the **emulator's**
transport, not of COSMOS - neither exists on real hardware. Anyone reading the class later must not
mistake them for protocol.

---

## 2. Route-through - the capability the node does not have

**Current state, verified by grep**: `SintranHeader.Marker2Relay` (`0x12`) is declared in
`Xmsg.Protocol\Wire\SintranHeader.cs` and **never assigned anywhere**. All fourteen send sites in
the tree set `Marker2Normal`. No code inspects whether a received datagram is addressed elsewhere.
The node cannot relay.

### 2.1 What the capture says a relay does

Between two different transports (103 over HDLC -> 100 -> 102 over Ethernet), node 100 changes
**exactly two fields** and nothing else:

| field | action |
|---|---|
| word 0 (markers) | `0x2113` -> `0x2112` |
| word 6 (checksum) | recomputed |
| destination, source, Flags 1, Flags 2 | **unchanged** |
| entire message body | **unchanged, byte for byte** |

The SINTRAN header therefore carries the **original endpoints end to end**. A relay must never put
its own node number in it.

Acknowledgements are **end-to-end**: the subtype `0x03` ACK came from node 102, the real
destination, and was relayed back like any other datagram. A relay must not acknowledge on the
destination's behalf.

### 2.2 New code

`Xmsg.Protocol\Relay\`:

- `SintranHeaderRelayExtensions.MakeRelayed(ref SintranHeader)` - sets `Marker2 = Marker2Relay` and
  recomputes word 6 via the existing `XmsgEnvelope.ComputeHeaderChecksum`. Deliberately the only
  place those two edits happen together, so they cannot drift apart.
- `IRouteTable` - `bool TryGetOutgoingLink(ushort destinationNode, out ILink link)`. The runner
  already models routes in `Xmsg.Live.Runner\Topology.cs` (local / neighbour / via with a relay
  chain); this interface should be backed by that rather than a second source of truth.
- `DatagramRelay` - given a decoded frame and the link it arrived on: if
  `header.DestinationNode == localNode`, return `false` (handle locally); otherwise look up the
  route, `MakeRelayed`, send on the outgoing link, return `true`.

### 2.3 Wiring

Hook `DatagramRelay` into the receive path in `XmsgNode` / `XmsgLayer` **before** local dispatch and
**before** the secure-delivery ACK logic in `SecureDatagramReceiver`. Getting this order wrong is
the single most likely bug: acknowledging a frame that was only passing through would corrupt the
end-to-end sequence.

The outgoing link re-frames from scratch - new link-layer sequence, new link ids (LAPB N(S)/N(R) on
HDLC, `NdLinkHeader` sequence on Ethernet). Those are **per-hop**. The SINTRAN header is not.

### 2.4 Cross-transport is the interesting case

A relay between HDLC and Ethernet must translate framing while leaving the SINTRAN header alone
apart from section 2.1's two edits. Because both sides are already `ILink`, the relay itself is
transport-blind - which is the payoff of doing section 1 through the existing seam.

---

## 3. Existing debt this touches

Do not bundle these into the above; they are listed so the ordering decision is informed.

- **The 32-bit `ControlService` field is a misnomer spanning a field boundary** (`XmsgDataFields.cs`
  documents this at length). It is really `(XMCSM << 16) | firstBodyWord`. Splitting it correctly
  touches ~108 call sites across ~34 files. Large, mechanical, and orthogonal to relay/Ethernet.
- **`SintranProtocolId` is a misnomer.** Now that word 6 is known to be a checksum, the "Protocol
  ID" byte at offset 12 and the "Counter" at offset 13 are simply its high and low bytes. The type
  and its uses encode a model that has been disproved. **Needs a naming decision** before the
  rename.
- **Superseded `XmsgEnvelope` members** - `LearnSeed`, `BaseLow`, `BaseLowSigned`, `ComputeCounter`,
  `ComputeEpoch`, `DeriveChannel`, `ChannelAnchor` - are retained only so the tree compiles. They
  implement the seed/epoch model that the checksum finding replaced. Delete once nothing calls them.

---

## 4. Suggested order

1. **`MakeRelayed` + `DatagramRelay` + wiring** (section 2). Smallest, highest value, explicitly
   asked for, and fully specified by capture.
2. **`Xmsg.Ethernet`** (section 1). Larger but self-contained and additive; nothing above `ILink`
   changes.
3. **Delete the superseded `XmsgEnvelope` members** (section 3). Trivial once nothing references
   them; removes a disproved model from the codebase.
4. **`ControlService` split** and the `SintranProtocolId` rename (section 3). Big churn, needs a
   naming decision, and benefits from being done after the above so it is done once.

---

## 5. Testing - use the captures as golden vectors

Three captures now exist in `E:\Dev\Ronny\X25Emulator\pcap\`:

| file | what it proves |
|---|---|
| `ethernet-conn-to-D100-from-102-WORKING-2026-08-01.pcapng` | Ethernet framing, link header, sequence/ack |
| `ethernet-hdlc-ROUTE-THROUGH-WORKING-102-via-100-to-103-2026-08-01.pcapng` | relay, 592 packets, 128/128 valid checksums |
| `ethernet-hdlc-ROUTE-THROUGH-103-via-100-to-102-2026-08-01.pcapng` | relay, the failing direction |

Tests to write, in the style of the existing `HeaderChecksumTests`:

- **Round-trip**: parse every ND frame in the Ethernet capture and re-encode it; assert byte
  equality. This catches the length relation, the padding rule, and the MAC byte reversal at once.
- **Relay vector**: feed the captured `103 -> 100` frame through `MakeRelayed` and assert the output
  equals the captured `100 -> 102` frame **exactly**, including word 6 changing `0xda04` -> `0xda05`
  and every other field being untouched. This is the single strongest test in the set: it is a real
  ND machine's own relay output as the expected value.
- **Do-not-ack-relayed**: a relayed frame must produce no subtype `0x03` ACK from the relay.
- **Checksum sweep**: recompute word 6 for every SINTRAN header in all three captures and assert
  100% agreement (currently 128/128 on the working relay capture, 0 failures).

Per repo policy: no LINQ, no FluentAssertions, no standalone test programs - these go in the
existing `Xmsg.Protocol.Tests` / a new `Xmsg.Ethernet.Tests`.

---

## 6. What is NOT specified and must not be guessed

- The origin of the link ids (section 1.4). Learn from the peer; do not synthesise.
- `NdLinkHeader.kind` values beyond `0x20` / `0x3f`, and loss/retransmit behaviour. No capture
  exercises them. Accept-and-log rather than assume.
- Whether COSMOS ever uses a non-zero physical user code. Observed value is always `0x00`, which
  does not match the `11` = ND/COSMOS the manual's table would suggest, and the hardware ignores
  those bits on receive - so the capture cannot distinguish the two readings.
- Window size on the ND link layer.
