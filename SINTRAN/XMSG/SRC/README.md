# The C# XMSG / COSMOS library

A working implementation of the SINTRAN III **XMSG** message system and the **COSMOS**
services built on it - the wire formats, the MON 200B programming model, a file-access
server and a TAD terminal server - able to talk to a real ND-100 over HDLC.

- **Target:** `net9.0`, **BCL only**. No external packages, no emulator dependency.
- **Namespace root:** `NDInsight.Sintran.Xmsg`.
- **Style:** no LINQ, no `foreach` (indexed `for` only), no FluentAssertions,
  `Span`/`ReadOnlySpan` parsing, XML docs on every public member,
  `TreatWarningsAsErrors` with the doc file on, and no `<NoWarn>` or `#pragma` suppressions
  anywhere.
- **Warnings:** production builds clean. The test projects raise ~91 `CS0618`, every one a
  deliberate use of a member we marked obsolete (`SintranHeader.ProtocolId` and friends) by
  tests that exist precisely to pin the old behaviour. Any warning that is NOT `CS0618` is a
  regression.

```
dotnet build   XMSG/SRC
dotnet test    XMSG/SRC
```

**1001 tests green** as of 2026-08-11.

---

## Size and shape

90,075 lines across 401 files, in 26 projects. The per-project table below is older than
that total and is indicative rather than exact - the total is the number that gets re-counted.

| | Files | Total lines | Code |
|---|---:|---:|---:|
| Production | 209 | 48,731 | 19,527 |
| Tests | 98 | 24,135 | 13,494 |

Tests are **41% of the code**, and that is deliberate. Almost every defect found against a
real machine has been caught by a test built from captured bytes rather than by a
behavioural assertion - see "How this library is tested" below.

### By project

| Project | Total | Code | What it holds |
|---|---:|---:|---|
| **Xmsg.Protocol** | 12,654 | 4,100 | Wire formats: SINTRAN header, XMSG sub-header, QFORM, FA codecs, fragmentation, every protocol enum |
| **Xmsg.Protocol.Tests** | 9,148 | 5,068 | The largest test project - byte-level wire tests |
| **Xmsg.Node** | 8,081 | 3,245 | Node behaviour: frame dispatch, TAD sessions, datagram relay, server hosting |
| **Xmsg.Api** | 7,764 | 3,154 | The MON 200B kernel model and the XROUT client |
| **Xmsg.Node.Tests** | 6,443 | 3,442 | Server wiring and listing-golden tests |
| **Xmsg.Servers** | 5,283 | 2,097 | The COSMOS FA file server and the TAD server |
| **Xmsg.Live** | 3,162 | 1,499 | LAPB and the replaceable transport half |
| **Xmsg.Ethernet** | 2,820 | 1,171 | 802.3, LLC1, the ND link header |
| **Xmsg.Live.Tests** | 2,674 | 1,577 | Link-layer tests |
| **Xmsg.Api.Tests** | 2,506 | 1,517 | Kernel-function tests |
| **Xmsg.Live.Runner** | 2,132 | 1,119 | The harness that talks to a real D100 |
| **Xmsg.Ndfs** | 1,808 | 646 | The file store the FA server serves from |
| **Xmsg.Hdlc** | 1,711 | 736 | HDLC framing, FCS-16, LAPB frame parsing and control constants |
| Xmsg.Hub, Xmsg.Chat, Xmsg.Diagnostics, Xmsg.Api.Node, and their tests | ~4,600 | ~2,600 | Hub mode, the chat system (rooms, aliases, channels), the frame decoder, a node-level API facade |

`Xmsg.Protocol` is the striking one: 12,654 lines carrying only 4,100 of code, a 3:1
comment ratio. That is the carving work. Every constant records where it was measured,
which capture proved it, and what was believed before - `FaOperation.cs` reproduces ND's
own handler-address table from the server binary.

---

## What actually works

### COSMOS file access - verified against a real ND-100

Every operation below has been driven from a live D100 running SINTRAN III K:

| Operation | Code | State |
|---|---|---|
| Reserve / release file entry | `0x02` / `0x03` | live |
| Open / close file | `0x05` / `0x06` | live |
| Set block size | `0x07` | live |
| **Read file** | `0x08` | live, byte-identical over 7 blocks |
| **Write file** | `0x09` | live, byte-identical |
| Create file | `0x0A` | from capture; not yet driven live |
| Delete file | `0x0B` | live |
| SiiiSpecial - file info, set-EOF, and three listing walks | `0x0C` | live |

`LIST-FILES` and `FILE-STATISTICS` both work from a real terminal. A 12,690-byte file has
been read off this server and written back to it byte-for-byte.

**Partial transfers** follow the monitor call the operation is named after: `RFILE` reads
"any number of bytes" but "must start at the beginning of a block". So a transfer is
block-aligned at the start and arbitrary in length; fine granularity comes from setting a
smaller block size, not from a byte offset.

Three FA operations are **not** served - `0x01`, `0x04` and `0x0D`. They have never been
captured and no manual we hold documents the FA wire protocol, so they are refused rather
than guessed.

### The XMSG kernel - all 48 function codes accounted for

`IXmsgKernel` is the MON 200B surface as a typed C# API. Every code ND declares falls into
exactly one group, and `XmsgFunctionCoverageTests` fails if that ever stops being true:

 - **27 implemented** - the port, buffer, send/receive, currency, conversion and privilege
   functions a task actually uses.
 - **19 deliberately absent** - driver-only, privileged, physical-memory, or marked obsolete
   in ND's own include. Plus `XFSMC`, whose entire purpose is batching register sets to save
   monitor calls; there is no monitor-call boundary here, so it would only add a second
   dispatch model beside the typed one.
 - **2 blocked on evidence** - `XFWRT` and `XFGSM` are named in `XMSG-PL-VALUES-M.INCL` and
   have **no section in Appendix A**, so their parameters are unknown. They wait on a
   capture or a carve, not on effort.

### Transport

HDLC framing, LAPB, and the COSMOS Ethernet link header are all implemented -
`Xmsg.Hdlc` and `Xmsg.Live` for the serial path, `Xmsg.Ethernet` for 802.3/LLC1.
Message fragmentation and reassembly work in both directions, which is what a file
transfer needs: the LAPB information field carries up to 622 bytes, derived from the
fragment constants rather than picked.

---

## How this library is tested

**The rule that matters: a test that builds its own frame only ever agrees with our idea
of the format.** Four separate defects survived a fully green suite because every test
constructed its inputs. Where a capture exists, the test takes the captured hex verbatim,
parses it with the real parser, and asserts against that - with a guard that the constant
is the length the frame declared, so a mistyped byte fails as itself.

`FaLiveRequestRegressionTests` and `SintranFragmentCaptureTests` are the pattern.

Goldens (`FaListingRegressionTests`) compare every emitted frame byte for byte. They are
strong on what we **emit** and structurally blind to what we **parse** - which is why both
kinds of test exist. Never update a golden silently; do it deliberately and say why in the
commit.

---

## Architecture

Three layers, and the boundary is enforced by project references rather than convention:

```
Xmsg.Protocol   wire formats and enums   - knows nothing about links or sockets
Xmsg.Hdlc       framing and LAPB frames  - below both, referenced by Live
Xmsg.Live       LAPB state machine, HDLC encoder, the replaceable transport half
Xmsg.Node       node behaviour and dispatch
Xmsg.Servers    the FA and TAD servers
Xmsg.Api        the MON 200B kernel model, for code that wants the SINTRAN API shape
```

`Xmsg.Protocol` has no transport dependency at all. That is what let the LAPB control
constants move down into `Xmsg.Hdlc` so the frame builder and the frame parser stopped
holding separate copies of the same wire numbers.

### Where the formats came from

| Component | Source |
|---|---|
| Enums (values and descriptions) | [../xmsg-constants.json](../xmsg-constants.json), generated from the official [../XMSG-PL-VALUES-M.INCL](../XMSG-PL-VALUES-M.INCL) |
| SINTRAN header, sub-header, subtypes | [../DOC/XMSG-PROTOCOL.md](../DOC/XMSG-PROTOCOL.md), from FCS-validated captures |
| The XROUT letter TLV format | COSMOS Programmer Guide ND-60.164 Appendix B |
| The MON 200B function semantics | COSMOS Programmer Guide ND-60.164 Appendix A |
| **The whole COSMOS FA protocol** | **Captures only.** No manual we hold documents it. The operation NAMES come from a command table inside `COS-FA-SERV-E04:PROG` |

---

## Regenerating the enums

`Enums/` is generated. Edit the JSON (or the `.INCL` behind it), then:

```
python XMSG/SRC/tools/gen-xmsg-enums.py
```

Do not hand-edit the generated files - a regeneration silently discards the edit. Anything
that must live alongside an enum but is not in the JSON belongs on the consuming type
instead; the function-coverage accounting sits on `IXmsgKernel` for exactly that reason.

---

## Design decisions worth knowing

- **Header word 6 is a checksum**, a ones-complement sum over words 0-5, confirmed on
  3595/3595 captured frames. It is never carried through from anything and never invented -
  a wrong one kills a real D100 with `XMSG ERROR CODE 24`. `SintranHeader.ProtocolId` and
  `.Counter` are compatibility views over its two bytes and are marked obsolete.
- **One message counter per conversation.** Replies and data messages draw from the same
  counter, +1 per message sent. Two counters looked right until a real multi-block transfer
  proved otherwise.
- **Sub-service enums are split by parent service** - XROUT sub-service values genuinely
  collide across `XSSCI`/`XSGAT`/`XSDAT`, so one enum is impossible.
- **`XMDPT` is stored verbatim.** The `port << 7` encoding is refuted by the capture corpus
  and the true one is unconfirmed, so no transform is imposed.
- **Duplicate-valued symbols compile as aliases**, faithful to the source. Role bit 5 is
  both `HighPriority` and `RemoteXrout` - which of the two depends on whether the routed bit
  is set, so use `IsHighPriority()` / `IsRemoteXrout()` rather than testing the bit.
- **Uncaptured things are refused, not guessed.** A refusal is a correct answer; an invented
  layout is a defect that hides until a real machine disagrees.

---

## Talking to a real machine

`Xmsg.Live.Runner` connects to a RetroCore ND-100 over its HDLC port and serves a folder as
a COSMOS file system:

```
Xmsg.Live.Runner.exe --config <path>\topology-d103-hdlc.json
```

The config names the node, the link, and the folder to serve. `DOC/PLAN-FA-FILE-SERVER-2026-08-06.md`
carries the live-test procedure and the traps - the short version being: one terminal
connection, restart XMSG on the peer if `XENSE` errors flood, and stop the runner before
rebuilding or the old binary stays loaded and the tests lie.
