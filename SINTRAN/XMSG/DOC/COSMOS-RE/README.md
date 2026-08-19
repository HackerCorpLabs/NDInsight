# COSMOS-RE — reverse-engineering of the COSMOS ND-100 communication programs

This folder holds the complete reverse-engineering of four **COSMOS** ND-100 programs (SINTRAN-III
`:PROG`, PLANC-compiled), decoded in Ghidra to recover **how they send/receive XMSG messages and the
exact wire formats**. It is the app-layer companion to the transport-level XMSG spec (see *Related*).

The work was produced by two parallel sessions and merged here:
- **cos-conn-to-e02** + **cos-fa-serv-e04** (the TAD client + the file-access server)
- **cos-file-tra-e02** + **cos-xftra-e02** (the file-transfer app + the transport exerciser)

## Folder structure

| Folder | Contents | Put here |
|--------|----------|----------|
| **[`ProtoCode/`](ProtoCode/README.md)** | Behavioural **C#** reconstructions — readable "what the code does", *not* runnable ND-100 emulators. Compile clean on net8.0. | new `.cs` |
| **[`Analysis/`](Analysis/README.md)** | Per-program analysis docs + the cross-program synthesis. The authoritative write-ups. | new analysis `.md` |
| **[`Planning/`](Planning/README.md)** | Coordination, corrections, and the capture-validation plan. Process — not findings. | briefs, validation plans, inter-session notes |

### `ProtoCode/`
| File | Program |
|------|---------|
| `CosConnToE02.cs`  | CONNECT-TO client — TAD-over-XMSG, receive dispatch table, startup, disconnect |
| `CosFaServerE04.cs`| File-access server — data-driven typed-param protocol, file-entry state machine |
| `CosFileTraE02.cs` | File-transfer app — XMSG transport + QFORM formatting |
| `CosXftraE02.cs`   | XMSG transport exerciser — client/server loopback tester |

### `Analysis/`
| File | Scope |
|------|-------|
| `COS-CONN-TO-E02-Analysis.md`      | TAD client: transport, XMSG envelope, TAD opcode table, startup, receive, disconnect |
| `COS-FA-SERV-E04-Analysis.md`      | File server: 13 ops, QFORM tags, entry descriptor, data-driven model, naming completeness |
| `COS-FILE-TRA-E02-XMSG-Analysis.md`| File-transfer: XMSG layer, QFORM formatter, page unit |
| `COS-XFTRA-E02-Analysis.md`        | Transport exerciser: XMSG wrapper lib, letter-indexed param types |
| `COSMOS-XMSG-Synthesis.md`         | **Start here for the big picture** — cross-program synthesis (shared transport, per-program wire proofs, the FA layer) |

### `Planning/`
| File | Purpose |
|------|---------|
| `README-CAPTURE-VALIDATION.md`       | **The validation plan** — 7 capture scenarios (S1–S7) → which `[INF]`/`[CANDIDATE]`/`[UNKNOWN]` claim each confirms, and which need a debugger trace vs a pcap |
| `REVIEW-CORRECTIONS-BRIEF.md`        | The methodology brief (decode bytes, don't infer) that drove the corrections pass |
| `REPLY-TO-FILE-TRA-XFTRA-SESSION.md` | The FA op catalog + the data-driven finding + the bit-7 rule, handed between sessions |
| `FOLDER-REORG-NOTICE.md`             | The notice that established this layout |

### Root-level docs (Ethernet II card / ENNS0 network server)

These sit at the folder root rather than in `Analysis/` because they cut across the ND-100 programs,
the ENCOS 68000 firmware and the live network.

| File | Scope |
|------|-------|
| [`ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md`](ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md) | The card's host contract: mailbox, command ring, DRAM map, node layout, SCIP |
| [`ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md`](ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md) | **Can this card carry TCP/IP?** The `0x1888A` mode word decoded; raw pass-through mode; ARP/broadcast; COSMOS coexistence. Read its correction banner first |
| [`HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md`](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md) | **How ND actually did it** - the 211185 Gateway (Ethernet II) vs 211327 Basic Module (Ethernet III), TCP-on-host / IP-on-card split, AIP = ARPA Internet Protocol, per-protocol controller images, and what media to hunt |
| [`ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md`](ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md) | Why a received frame never became an XMSG message (FCS double-count) |
| [`ENNS0-RXPOOL-PRODUCER-RE-2026-07-24.md`](ENNS0-RXPOOL-PRODUCER-RE-2026-07-24.md) | Who fills the card's receive-buffer pool (CARD side: ENABLE + POST-BUFFER opcode 0x12 are the only producers) |
| [`CARVE-ANSWER-COSMOS-ETHERNET-ENCOS-ERR-II-STATS-AND-MAILBOX-2026-08-09.md`](CARVE-ANSWER-COSMOS-ETHERNET-ENCOS-ERR-II-STATS-AND-MAILBOX-2026-08-09.md) | HOST side (`encos-err-ii-b01.brf` = ENNS0 supervisor): module map, the MA-statistics record order (live-verified via `(UTILITY)ENCOS-MON-II-B01` on D100 — reads the card's drop counters from SINTRAN), the MON 255B wrapper family, a request-block builder's opcode set, and the still-OPEN host re-post policy with next probes |
| [`CARVE-ANSWER-FA-SEAT-RETURN-2026-08-18.md`](CARVE-ANSWER-FA-SEAT-RETURN-2026-08-18.md) | **When does `*FA-SERVER` give a connection seat back? It never does.** The only `XMPINFC`/`XSNSP` call in `cos-fa-serv-e04.prog` is on the init path - proven by `SAA 0x51` occurring exactly once in 231KB, plus the full call chain and the exhaustive xref negative. Also: why segment 73B was the wrong artifact, and a phantom-split trap at 0x7dcf that would have given the opposite answer |
| [`XROUT-LAN-NEIGHBOUR-ROUTING-RE-2026-07-24.md`](XROUT-LAN-NEIGHBOUR-ROUTING-RE-2026-07-24.md) | XROUT LAN neighbour routing |
| [`COSMOS-MULTI-NODE-NETWORK-2026-07-25.md`](COSMOS-MULTI-NODE-NETWORK-2026-07-25.md) | The working two-node COSMOS network write-up |
| [`ENNS0-Startup-RE-2026-07-23/`](ENNS0-Startup-RE-2026-07-23/README.md) | The ENNS0 startup reverse-engineering session (disassembly, PIOCM, LU 2240B, PRKEY) |

## How to read the findings

**Confidence tags** appear throughout every doc, C# comment, and Ghidra annotation:

| Tag | Meaning |
|-----|---------|
| `[BIN-VERIFIED]` | read directly from the binary (an actual instruction / byte) |
| `[SYM]` | from the ND kernel symbol tables |
| `[DOC-VERIFIED]` | confirmed against a real pcap / the TAD spec |
| `[INFERRED]` | a reasoned deduction, not yet byte- or wire-confirmed |
| `[CANDIDATE]` | a name/value seen but not in the symbol tables |
| `[UNKNOWN]` | not determinable from the traced code — needs a capture |

**Two rules the whole corpus obeys** (learned the hard way — see the corrections brief):
1. **Decode the byte, don't infer it.** Every wire field traces to a real `LBYT`/`SBYT`/`SAA`; where
   it doesn't, it says `UNKNOWN` rather than inventing a plausible value.
2. **Layer boundary.** These are *application* binaries, above `MON 200B`. The kernel transport
   (envelope, secure-ACK, LAPB, sequence/channel) is invisible here — the C# explains intent, it is
   **never** a transport build-spec. That layer lives in the XMSG spec below.

**Nothing here is wire-verified yet** — there is no live capture (no running binary). `Planning/
README-CAPTURE-VALIDATION.md` is the plan for turning `[BIN]`/`[INF]`/`[CANDIDATE]` into `[VERIFIED]`
once a machine runs. Every claim confirmed by a future capture should have its tag upgraded here and in
`COSMOS-XMSG-Synthesis.md`.

## Key results (one-paragraph summary)

All four programs ride the **`MON 200B` XMSG** transport with an `[op][len][body]` message shape.
**cos-conn-to** speaks **TAD** with an opcode-indexed receive dispatch table. **cos-fa-serv** has **no
numeric opcode** — it is a **data-driven** typed-parameter protocol (`[BIN-VERIFIED]` from the engine):
the operation is resolved from which params are present + the target file-entry's type/state, and file
data rides the **entry-type-`0x10`** path in 0x800-byte pages. A **family-wide rule**: **bit 7 (`0x80`)
of a param byte is the "typed-param present" marker** (fa-serv uses tags `0x92/0x94/0xA2/0xF2`; the
xftra/file-tra side uses letter-indexed `A–F`). Function naming is complete for all
protocol-significant code; the mechanical `_v2`-duplicate/fragment tail is documented and deliberately
left (see the fa-serv doc §6).

## Related (outside this folder — not moved)
- `..\..\..\TAD\TAD-Message-Formats.md` — the TAD wire spec (opcodes, login ladder, connect session).
- `..\XMSG-PROTOCOL.md` — the transport spec (framing, LAPB, envelope, secure-ACK, ports).
- The annotated Ghidra programs themselves live in
  `Installation/Communication/COSMOS Basic/x/`.
