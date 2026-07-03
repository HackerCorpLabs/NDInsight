# XMSG transport seam — implementation plan

Restructures the working XMSG live node into the **events-up / interfaces-down** seam
pattern from `docs/COSMOS_XMSG_SEAM_ARCHITECTURE.md` (in the X25Emulator repo), so the
code later drops into `X25Emulator` (as the SINTRAN/XMSG sibling of the X.25 stack) with
minimal refactor. This plan is the contract; each phase has a byte-exact test gate.

> **Source of truth for the wire format:** `SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md`
> (especially the universal envelope model, Section 18.5). The seam only *packages* that
> model into layers — it does not change any wire behaviour.

---

## Status (implementation)

| Phase | State | Gate |
|-------|-------|------|
| 1 — Packet layer | **Done** | 11 tests: parse↔re-encode byte-identical; `CreateData` reproduces accept + port-assign; `CreateAck`/`CreateReachabilityReply`/`CreateNetworkError` byte-exact. |
| 2 — Codec seam | **Done** | 4 tests: bytes-in raise `PacketReceived` (link-id first); `SendPacket` writes exact bytes; round-trip identical. |
| 3 — Link seam | **Done** | 2 tests: payload round-trips Layer↔wire through `LapbLinkAdapter` over in-memory duplex; XMSG-bound link rejects `SendX25Packet`. |
| 4 — XmsgLayer | **Done** | 4 tests: driven through the codec, the layer emits byte-exact reachability / TAD ACK+accept / port-assign, matches the proven node's XSGSY reply, and raises `SessionOpened`. |
| 5 — Compose + parity | **Done (offline + live)** | Offline: `SeamParityTests` proves the seam composition writes byte-identical wire output to the legacy path. **Live CONFIRMED against machine 100:** `connect-to d102` accept + port-assign both delivery-ACKed (DE 1A / DE 19), correct derived channels/counters, no crash, no SABM flood. (Runner uses NO periodic keepalive — a 1s keepalive floods RR; the per-RX `RR nr=N` is correct LAPB.) |
| 6 — TAD terminal bring-up | **Mechanism done; live bring-up pending** | Channel model cracked (below) + `CreateSessionData` gated to reproduce captured DUMM/MOTD byte-exact. Remaining: emit the post-port-assign burst (DUMM/ctrl-0x20/RESE) live — 100 waits for it before driving TMOD/TTYP → MOTD. One live unknown: absolute starting Base for our data stream (probe frame-by-frame). |

Test totals after Phase 6 mechanism: **54 Xmsg.Protocol.Tests + 42 Xmsg.Live.Tests = 96** (was 65).

### Envelope channel model (VERIFIED — resolves the old per-session channel-allocator unknown)

The sub-protocol channel (Protocol ID) is **derived, not allocated**:
`Base = Flags1 + Counter` (16-bit); `Channel = 0xDE − (XMCSM≫24) − (Base≫8)`. Within a stream
Flags1 counts up while Counter counts down, holding Base (and the channel) constant. Verified
against every captured `102→100` responder frame and our own live-accepted accept. Implemented as
`XmsgEnvelope.DeriveChannel` / `XmsgPacketBuilder.CreateSessionData`. This is why replaying a canned
session's channels crashed 100 (wrong Base → XXPER) and computing them does not.

### Live operational notes

**The `[TX] RR nr=N` frames are LAPB, not a keepalive and not a bug.** LAPB requires exactly one
Receiver-Ready acknowledgment per *received* I-frame. During a connect-to, machine 100 sends us four
I-frames (its connect, its delivery-ACK of our accept, the session-setup, its delivery-ACK of our
port-assign), so V(R) climbs 1→2→3→4 and each step emits one RR. Suppressing them would make 100
treat the frame as lost and retransmit. This is distinct from the RR *keepalive* (an idle-timer RR
once per second) — the runner sends **no** keepalive on the seam path (reactive RR only), matching
the validated legacy runner. A run of identical `RR nr=0` was that keepalive bug and is fixed.

**Phase 6 live-probe procedure (DUMM-first).** After the port-assign, 100 beeps waiting for our
session-data burst before it drives `TMOD/TTYP` → MOTD. `TadTerminalResponder` (behind
`SendTerminalBringup`, enabled in the runner's seam path) emits the DUMM as the next frame of the
responder's own sequence. Read 100's reaction (see the live findings below for the two error kinds).

### Phase 6 live findings (VERIFIED against machine 100)

Three live probes of the first terminal-data (DUMM) frame pinned down two independent, hard
constraints — each probe broke exactly one and 100 named it:

| Probe | DUMM flags1 | Derived channel | 100's reaction |
|-------|-------------|-----------------|----------------|
| capture-absolute | `0x0131` (a *jump*) | **DB** | clean **XENSE** — frame well-formed, only the sequence rejected (recoverable) |
| continuous, low Base | `0x000C` | **DD** | **XXPER** fatal crash |
| continuous, counter=0xFF | `0x000C` | **DC** | **XXPER** fatal crash |

1. **Only DB is a well-formed terminal-data channel.** DC and DD both crash 100 fatally (XXPER 24B
   @ physical 134265B — the same signature the old canned replay produced). DD is the TAD *control*
   channel; DC is 100's own send channel; a terminal-data frame (`XMCSM 0x01080000`) belongs on
   **DB**. Crucially, the DB probe did **not** crash — it returned a clean, recoverable network error
   (subtype `0x07`, Flags2 `0xFFDE` = XENSE −34, sequence error). So DB is correct and the *only*
   thing wrong with the DB probe was the sequence.
2. **flags1 must be continuous** with our own prior frames. A jump → XENSE.

These two constraints are *coupled* through the envelope identity (`Channel = 0xDE − (XMCSM≫24) −
(Base≫8)`, `Base = Flags1 + Counter`): reaching DB needs `Base≫8 = 2`, i.e. `Base ≈ 0x02xx`, i.e. a
**high** flags1 — but a jump from a low echoed sequence is what triggers XENSE. The resolution is
that the **whole responder runs one continuous high sequence**: the accept, port-assign, and every
terminal-data frame advance a single datagram sequence seeded at `0x012F` (the conn-to-d102
responder's own start). Control frames use Base `0x0214` → channel **D8**; terminal-data frames use
Base `0x020C` → channel **DB**; flags1 increments `0x012F → 0x0130 → 0x0131 …` across all of them, so
the DUMM at `0x0131` is both **in-order** and **on DB**. We no longer echo 100's low sequence for the
accept (that worked for the accept alone but boxed the data frames into the fatal low Base).

**The high-sequence accept was REFUTED (the plan above does not work).** A fourth probe made the
whole responder run one high sequence (accept `0x012F` on D8). Machine 100 rejected the **accept
itself** with subtype `0x07`, Flags2 `0xFFDE` = XENSE − 34: **100 requires our datagram sequence to
be in-sequence with the connect it sent (low, echoed), not a fresh high one.** This closes the loop
into a genuine contradiction for a fresh-sequence node:

- 100 forces our sequence **low** (echo the connect's `0x0000…`; a high start → XENSE), **and**
- terminal-data must ride **DB**, which needs `Base≫8 = 2` i.e. `Base ≈ 0x02xx` i.e. a **high**
  Flags1 — and a byte-sized counter cannot lift a low Flags1 to `0x02xx`.

Both cannot hold at once. In the captures this was never a problem because those machines had
**high** datagram sequences from prior traffic (102 was at `0x0131`), so their low-XMCSM terminal
frames landed on DB naturally. Our C# node starts **fresh** (sequence `~0x0000`), and 100 will not
let it jump. The envelope channel formula *fits* the captures but does **not** predict what 100
accepts from a fresh-sequence responder — the data-phase channel depends on the absolute sequence,
which we cannot set freely.

**Status: MOTD blocked; connect handshake stable.** The bring-up (`SendTerminalBringup`) is disabled;
the runner sends only the echoed accept + port-assign, which 100 ACKs with no crash (it then beeps,
waiting for a terminal we cannot yet form). Resolving the MOTD needs one of: (a) the XMSG channel/
sequence-allocation rule from the kernel source (not in this repo), or (b) a capture of a **fresh**
responder session (sequence starting near zero) to see what channel 100 actually expects then.
Blind live probing is a dead end here — every wrong data-phase channel (DC/DD) is a fatal XXPER that
kills 100's XMSG and forces a manual restart.

**Two error kinds and what they mean operationally:**
- **XENSE** (subtype `0x07`, Flags2 `0xFFDE`) — a *recoverable* reject; 100's XMSG stays up. Means
  the frame was well-formed but its datagram sequence was out of order. Cheap to iterate.
- **XXPER** (`XMSG error 24B @ 134265B`) — a *fatal* internal error; 100's XMSG **dies** ("XMSG not
  running", XMFIDO aborts) and must be **restarted on the ND** before any further test. A wrong
  channel (DC/DD for terminal-data) causes this. A dead XMSG shows on our side as an endless
  bare-link `SABM/UA/RR nr=0` storm (`[rx-raw]` SABM with no data frames) — that is the link layer
  with no XMSG behind it, **not** a bug in our node.

**Deviation (recorded):** `XmsgLayer` is placed in **`Xmsg.Live`**, not `Xmsg.Protocol` as the
table in Section 3 proposed. Reason: the TAD session service (`TadTerminalResponder`, its menu and
frame context) and the byte-verified multi-frame orchestration (`XmsgNode.HandleFrames`) live in
`Xmsg.Live`, and `Xmsg.Protocol` cannot depend on `Xmsg.Live`. Relocating all of TAD into
`Xmsg.Protocol` would be a large, risky churn against the locked "façade over proven internals"
decision. The reachability / XSGSY / secure-ACK services the layer drives remain pure in
`Xmsg.Protocol` (`ListRoutingServer`, `SecureDatagramReceiver`). At migration, `XmsgLayer` moves
together with the TAD service into the X25Emulator XMSG sibling — mechanical (namespace root only).

---

## 1. Decisions (locked)

| # | Decision | Choice |
|---|----------|--------|
| Naming | class names | **XMSG prefix throughout** — `XmsgLayer`, `IXmsgCodec`, `XmsgCodecBase`, `IXmsgTransport`, `XmsgPacket`, `XmsgPacketInfo`, `XmsgPacketParser`, `XmsgPacketBuilder`, `XmsgPacketType`. (We implement XMSG, not all of COSMOS; matches the spec's own "protocol + Layer" rule and the existing `…Sintran.Xmsg` namespace.) |
| Namespace root | now vs. migration | Keep `NDInsight.Sintran.Xmsg` now; the root `NDInsight → X25Emulator` rename is a mechanical find-replace at migration. |
| Refactor scope | risk | **Façade over proven internals** — build the seam fresh; reuse `LapbLink`, the `XmsgFrame` parse/serialize, and the `SecureDatagramReceiver` logic underneath. The live node stays runnable at every step; the runner is re-pointed last. |
| `XmsgPacketType` | granularity | **SINTRAN subtype** (`Data 0x0E`, `Ack 0x03`, `ReachabilityRequest 0x19`, `ReachabilityReply 0x13`, `NetworkError 0x07`) — the transport-level packet type, matching `X25PacketType`. The XMCSM **service class** (connect / terminal-data / control / routing) is dispatched *inside* `XmsgLayer`, not in the enum. |
| Reliability | where secure-delivery lives | In **`XmsgLayer`** (datagram-seq + `0x03` ACK + retransmit), per spec §5 "session/connection state lives in the layer, not the codec". The codec is pure parse/encode. |
| Protocol detector | now vs. later | **Per-link binding** is the real mechanism. The HDLC/LAPB transport is **common**; a SINTRAN HDLC link carries **either X.25 or XMSG at L3, depending on the software installed on the ND machine** — so you cannot tell from the link *type*. Each link is bound to **`X25` or `XMSG`** at composition time (not by sniffing). Build only a `ProtocolDetector` **seam** (interface + a bound-to-`XMSG` stub here); defer the multi-field heuristic. See Section 5. |

**House rules (RetroCore, enforced every phase):** named delegates only (no `EventHandler<T>`,
no `EventArgs`, no bare `Action`/`Func`), sender/id first parameter; no LINQ; no `foreach`
(index `for`); block-scoped namespaces; full `///` XML docs on public members; keep comments /
citations. No reference to any external reference implementation in code or comments.

---

## 2. Layer stack (target)

```
        up-events: MessageReceived · SessionOpened · TerminalDataReceived · ...
                                   ▲
   XmsgLayer   (reachability · list-route/XSGSY · TAD sessions · secure-ACK · datagram-seq)
                                   ▲  PacketReceived(linkId, XmsgPacketInfo)
                                   │           ── IXmsgCodec.SendPacket(XmsgPacket) ──►
   XmsgCodec : XmsgCodecBase       │           (ProcessBytes → parse → raise;  SendPacket → build → send)
                                   ▲
                                   │           ── IXmsgTransport.Send(bytes) ──►
   ILink adapter (HDLC link, bound  │           PayloadReceived(link, payload, len)  /  SendSintranFrame
     to XMSG here; X25 in the       │
     X.25 case)                     ▲
   LapbLink · HdlcEncoder · TcpBridgeTransport      (UNCHANGED — proven, live-tested)
                                    (the HDLC/LAPB transport is common to X.25 and XMSG)
```

Dependency graph is strictly downward: `XmsgLayer → IXmsgCodec → IXmsgTransport → ILink`.
Nothing lower references anything upper; up is always a named-delegate event.

---

## 3. Project placement

| Type(s) | Project | Rationale |
|---------|---------|-----------|
| `XmsgPacket`, `XmsgPacketInfo`, `XmsgPacketType`, `XmsgPacketParser`, `XmsgPacketBuilder` | `Xmsg.Protocol` | pure protocol, no link dependency |
| `IXmsgTransport`, `IXmsgCodec`, `XmsgCodecBase`, `XmsgCodec` | `Xmsg.Protocol` | seam is pure; testable with an in-memory transport |
| `XmsgLayer` + service handlers (reachability, XSGSY, TAD session) | `Xmsg.Protocol` | reuses `ListRoutingServer`, ports the `TadTerminalResponder` logic; testable with a fake `IXmsgCodec` |
| `ILink`, `LapbLinkAdapter`, `LinkXmsgTransport`, `ProtocolDetector` (stub) | `Xmsg.Live` | link-facing; wraps `LapbLink`/`LiveNode`/`TcpBridgeTransport` |
| composition (Transport→Link→Detector→Codec→Layer) | `Xmsg.Live.Runner` | the composition root |

The old `XmsgNode` / `TadTerminalResponder` stay in place and runnable until Phase 5 proves
parity; they are removed only once the new path passes the same live test.

---

## 4. Phases (each ends at a green test gate)

### Phase 1 — Packet layer  `[Xmsg.Protocol]`
- `XmsgPacketType` (subtype enum, above).
- `XmsgPacketInfo` — read-only decoded fields: markers, subtype, dest/src node, Flags1, Flags2,
  Protocol ID; for data frames the sub-header (Counter, FrameFlags, Role, XMDSY/XMDPT/XMSSY/XMSPT,
  XMCSM, XMLEN) and the payload/TAD span. Convenience derived: `Base = Flags1 + Counter`.
- `XmsgPacketParser.ParsePacket(ReadOnlySpan<byte>) → XmsgPacketInfo` (wraps `XmsgFrame.Parse`).
- `XmsgPacket` — outgoing model; `XmsgPacketBuilder.Create*` (`CreateData`, `CreateAck`,
  `CreateReachabilityReply`, `CreateNetworkError`) encode via the envelope model (channel derived
  from `base`, ACK seed = connect-counter + 0x0A, etc.).
- **Gate:** parse→re-encode is byte-identical for a sample across ALL captures; `Create*`
  reproduces the captured `conn-to-d102` accept / port-assign / MOTD frames byte-for-byte.

### Phase 2 — Codec seam  `[Xmsg.Protocol]`
- `IXmsgTransport { void Send(ReadOnlySpan<byte> bytes); }`
- `IXmsgCodec { void SendPacket(XmsgPacket packet); void ProcessBytes(ReadOnlySpan<byte> data); void Reset(); }`
- `XmsgCodecBase : IXmsgCodec` — owns `delegate void XmsgPacketReceived(string linkId, XmsgPacketInfo packet)`,
  the `PacketReceived` event, `RaisePacketReceived`, and the held `IXmsgTransport` + `LinkId`.
- `XmsgCodec : XmsgCodecBase` — `ProcessBytes` → `ParsePacket` → `RaisePacketReceived`;
  `SendPacket` → `XmsgPacketBuilder` → `Transport.Send`.
- **Gate:** bytes-in raises `PacketReceived` with the right `XmsgPacketInfo`; `SendPacket` writes
  the right bytes to a fake transport. Named delegate, sender-first.

### Phase 3 — Link seam  `[Xmsg.Live]`
- `ILink` (per spec: `Start/Stop`, `SendSintranFrame`, `SendX25Packet` [throws — SINTRAN link],
  `PayloadReceived`, `StatusChanged`).
- `LapbLinkAdapter : ILink` — wraps `TcpBridgeTransport`+`LapbLink`+`LiveNode`; `PayloadReceived`
  from the LAPB information field; `SendSintranFrame` → LAPB I-frame. **Per-link binding:** the
  HDLC/LAPB transport is common to X.25 and XMSG; this link instance is *bound* to `XMSG` at L3
  (an X.25 machine would bind the same adapter to `X25`). The binding is config, not detection.
- `LinkXmsgTransport : IXmsgTransport` — adapts `Send` → `ILink.SendSintranFrame`.
- `ProtocolDetector` seam — SINTRAN-only stub (Section 5).
- **Gate:** a frame round-trips Layer↔wire through the adapter over an in-memory duplex.

### Phase 4 — XmsgLayer (L3)  `[Xmsg.Protocol]`
- `XmsgLayer` holds `IXmsgCodec`, subscribes `PacketReceived`. Contains:
  - **Secure-delivery sublayer** — datagram-seq tracking + `0x03` ACK on the envelope channel with
    the seeded counter (reuse `SecureDatagramReceiver` logic).
  - **Service dispatch** by subtype + XMCSM class → reachability service, list-route (`XSGSY`)
    service (reuse `ListRoutingServer`), TAD session service (port `TadTerminalResponder`).
  - **Up-events** (named delegates): `MessageReceived`, `SessionOpened`, `TerminalDataReceived`, …
- **Gate:** driven with captured incoming packets, `XmsgLayer` emits byte-exact reachability
  replies, list-route replies, and connect accept / port-assign (the current live-verified set).

### Phase 5 — Compose + live parity
- New composition in the runner: `TcpBridgeTransport → LapbLinkAdapter(ILink) → ProtocolDetector →
  XmsgCodec → XmsgLayer`. Old runner kept until parity.
- **Gate:** live `connect-to d102` reaches the same clean handshake (accept + port-assign ACKed by
  100, no crash, no SABM flood) as the current node. Then retire `XmsgNode`.

### Phase 6 — TAD terminal bring-up (the remaining goal)
- In the TAD session service, reproduce the `conn-to-d102` responder frames (MOTD / prompt / menu)
  with `XmsgPacketBuilder` + the envelope model: channels **derived** from our session base,
  addressing patched to the live session. (This is the piece that previously crashed only because
  a *canned* session's channels were replayed; now they are computed.)
- **Gate:** byte-exact against the captured responder frames offline; then live — 100 drives the
  terminal instead of hanging.

---

## 5. Protocol detector — deferred design (per-link binding now)

**The transport does not tell you the L3 protocol.** A SINTRAN HDLC link runs LAPB over HDLC and
carries **either X.25 or XMSG at L3, decided by the software installed on the ND machine** — the
same physical link type does both. And byte-level sniffing is unreliable: `0x21 0x13` is also a
valid X.25 GFI/LCN ("logical channel 19"), which `XMSG-PROTOCOL.md` and the peer/friend spec both
note. Therefore the classification is **X25 vs XMSG**, and:

- **Now (real mechanism): per-link binding.** Each HDLC link instance is *configured* to carry
  `X25` or `XMSG` (matching the ND machine's installed SW) and the matching L3 layer subscribes to
  its `PayloadReceived`. No per-packet sniffing. Our bridge is bound to `XMSG`.
- **Seam only:** a `ProtocolDetector` interface with a bound-to-`XMSG` stub, so a heuristic can drop
  in later without restructuring.
- **Later (heuristic, only if a single link must auto-classify X25 vs XMSG):** require several
  signals, not just byte 0 — `byte0==0x21` AND `byte1∈{0x12,0x13}` AND `byte3` a known XMSG subtype
  AND (for data) the sub-header marker `0x21 0x00` at its expected offset (the strongest secondary
  tell). Add length/structure sanity as a tiebreak. Note the X.25 GFI/LCN collision above is exactly
  why a single-byte test is insufficient.

---

## 6. Migration to X25Emulator (what this buys us)

- `ILink` here is shaped to the target `ILink` (`SendSintranFrame` / `PayloadReceived`); the
  `LapbLinkAdapter` is replaced by X25Emulator's real `ILink` implementation.
- `XmsgLayer` becomes the sibling of `X25Layer` under the composition root; the root binds each
  HDLC link to `X25` or `XMSG` (per the ND machine's installed SW) and attaches the matching layer
  to that link's `PayloadReceived`.
- Only the namespace root (`NDInsight → X25Emulator`) and the `LapbLinkAdapter` swap are mechanical;
  the packet layer, codec, layer, and services move unchanged.
