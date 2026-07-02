# Xmsg.Protocol — C# XMSG protocol library

A standalone C# model of the SINTRAN III **XMSG** protocol: strongly-typed enums
for every protocol constant, a layered decoder/encoder for XMSG messages, a
builder for constructing them, and a state machine for the secure-datagram
delivery handshake.

- **Target:** `net9.0`, **BCL-only** (no external or emulator dependencies).
- **Namespace root:** `NDInsight.Sintran.Xmsg`.
- **Style:** follows the `retrocore-csharp` and `csharp-xml-comments` conventions
  and the repo rules — **no LINQ, no `foreach`** (indexed `for` only), **no
  FluentAssertions**, `Span`/`ReadOnlySpan`-based parsing, big-endian helpers, XML
  doc comments on every public member, `TreatWarningsAsErrors` + doc-file on
  (builds with 0 warnings).

## Build and test

```
dotnet build   XMSG/SRC/Xmsg.Protocol.slnx
dotnet test    XMSG/SRC/Xmsg.Protocol.slnx
```

(Run from the repo; the SDK emits a `.slnx` solution.) Current status: **20/20
tests green** in Debug and Release.

## Project layout

```
SRC/
  Xmsg.Protocol.slnx              solution
  Xmsg.Protocol/                  the library
    Enums/                        one enum per protocol constant group
    Wire/                         byte <-> object for the wire layers
    Xrout/                        the XROUT "letter" / standard-message body
    Builders/                     fluent construction of messages/frames
    StateMachine/                 secure-datagram send/receive/timeout/ack
  Xmsg.Protocol.Tests/            xUnit tests
  tools/gen-xmsg-enums.py         regenerates the Enums/ files from the JSON
```

## Architecture

### `Enums/` — the protocol vocabulary
One enum per constant group, each member carrying a `/// <summary>` copied
verbatim from the official ND comment text:
- **Generated** (from [../xmsg-constants.json](../xmsg-constants.json)):
  `XmsgFunction` (XF*), `XmsgOption` (`[Flags]`; the mask is `1 << bit`, and each
  member's XML notes the bit number), `XmsgDriverFunction` (XD*),
  `XmsgMessageType` (XM*), `XmsgError` (XE*), `XroutService` (XS*),
  `XroutError` (XR*), `XmfidoStatus`, `XmsgCrashCode` (XX*), and the three parent-
  scoped sub-service enums (`XroutSetCrashInfoSubservice` / `…GetAttribute…` /
  `…DefineAttribute…`).
- **Hand-written** (from the wire analysis): `XroutConnectionType` (the `XSGSY`
  reply enum), `SintranPacketSubtype` (`0x03`/`0x0E`/`0x13`/`0x19`),
  `SintranProtocolId` (`0xDE`…`0xD8`), `XmsgLinkState` (the `XSLKI` lifecycle).

### `Wire/` — the on-wire layers
Decode a `ReadOnlySpan<byte>` (an HDLC **information field**) into a composable
object graph, and re-serialize byte-identically:
- `SintranHeader` — the 13-byte header (markers, packet type/subtype, dest/src
  node, Flags1/Flags2, protocol id).
- `XmsgSubHeader` — the DC/XMSG sub-header (counter, `21 00` marker, frame-flags,
  role, XMDSY/XMDPT/XMSSY/XMSPT, XMCSM, XMLEN).
- `XmsgFrame` — the top-level composer: data frames (subtype `0x0E`) decode into
  header + sub-header + optional `XroutMessage`; short/control frames keep their
  single trailing counter/command byte in `TrailingBytes` (so ACK / reachability
  frames round-trip exactly).
- `BigEndian` — allocation-free 16/32-bit big-endian read/write helpers.

### `Xrout/` — the XROUT "letter"
`XroutMessage` + `XroutParameter` implement the ND **standard-message format**:
a 4-byte header (serial, service, length) followed by even-aligned TLV parameter
blocks, with the signed type-byte convention (positive = integer, negative =
two's-complement of the parameter number = string).

### `Builders/`
`XroutMessageBuilder` (set serial/service, append integer/string parameters with
correct signing + alignment) and `XmsgFrameBuilder` (assemble a full frame),
mirroring the XMSG call sequence (XFGET/XFWHD/XFWRI/XFSND) as an in-memory builder.

### `StateMachine/` — secure-datagram delivery
`SecureDatagramSession` (sender) and `SecureDatagramReceiver` model the `XFSEC`
secure path: send → await the `0x03` ACK whose Flags1 echoes the datagram
sequence → `Delivered`; on `Timeout` resend (default ×3) → on exhaustion
`Returned` (as message type `XMTRE`, reason `XENTO`); a NACK path returns
`XEREJ`. **Time is injected** (a tick count, 1 tick = 1 XTU = 0.1 s) — no
wall-clock reads — so transitions are deterministic and unit-testable. No threads
or timers inside; drive it with `Tick(currentTicks)` and event methods.

## Where the formats came from

| Component | Format source | Ultimate provenance |
|-----------|---------------|---------------------|
| `Enums/` values + descriptions | [../xmsg-constants.json](../xmsg-constants.json) | official [../XMSG-PL-VALUES-M.INCL](../XMSG-PL-VALUES-M.INCL) (version M) |
| `Wire/` (SINTRAN header, sub-header, subtypes) | [../DOC/XMSG-PROTOCOL.md](../DOC/XMSG-PROTOCOL.md) | FCS-validated packet captures (X25Emulator `pcap/`) + the Wireshark dissector |
| `Xrout/` (letter TLV format) | [../DOC/XMSG-API.md](../DOC/XMSG-API.md) section 4 | COSMOS Programmer Guide ND-60.164, Appendix B |
| `XroutConnectionType` (`XSGSY` reply) | [../DOC/XMSG-API.md](../DOC/XMSG-API.md) section 4.3 | COSMOS guide, `XSGSY` service |
| `StateMachine/` (secure datagram) | [../DOC/XMSG-API.md](../DOC/XMSG-API.md) sections 5–6 | `XFSEC` semantics + the `0x03` ACK from the captures |

## Regenerating the enums

The `Enums/` files are auto-generated; edit the JSON (or the `.INCL` it comes
from), then:

```
python XMSG/SRC/tools/gen-xmsg-enums.py
```

The tool resolves its paths relative to itself (reads `../../xmsg-constants.json`,
writes `../Xmsg.Protocol/Enums/`). Do not hand-edit the generated files.

## Design decisions worth knowing

- **Sub-service enums are split by parent service.** The XROUT sub-service values
  genuinely collide (many reuse 1–5 across `XSSCI`/`XSGAT`/`XSDAT`), so a single
  enum is impossible; they are three enums, matching the doc grouping.
- **`XMDPT` is stored verbatim** (raw `ushort`). The `port ≪ 7` encoding is
  refuted in the capture corpus and the true encoding is unconfirmed, so the
  library imposes no transform.
- **State-machine return reasons:** timeout-exhaustion → `XENTO` (network
  timeout); NACK-exhaustion → `XEREJ` (network remote reject); both surface as
  message type `XMTRE`.
- **Duplicate-valued symbols** (e.g. `XSDMC`=`XSDSY`, and option bits reused per
  function) compile as C# enum aliases — faithful to the source.

## Scope boundaries

HDLC framing, byte-stuffing, FCS, and LAPB are **out of scope** — decoding starts
at the SINTRAN header (the information field). For the framing layer see
[../../HDLC-Frame-Format-Reference.md](../../Devices/HDLC/HDLC-Frame-Format-Reference.md) and
the Wireshark dissector.

## Tests (`Xmsg.Protocol.Tests/`)

20 xUnit tests (Assert only, no FluentAssertions): enum spot-checks against the
JSON; the three captured fixtures (reachability request/reply, `0x03` ACK) with
subtype/dest/src/Flags1/protocol asserts; byte-identical round-trips; the XROUT
TLV worked example (signed-type string decoding); and every state-machine path
(send→timeout→resend×3→Returned; send→matching-ACK→Delivered; NACK exhaustion;
receiver emits the ACK echoing Flags1).
