# XMSG Documentation

**The XMSG documentation set: the wire-protocol and API specs, the LAPB layer-2 requirements, the TAD terminal protocol work, the COSMOS application-layer reverse engineering, and the dated question/answer/handoff log of the live protocol investigation.**

See [../README.md](../README.md) for the XMSG overview, provenance rules, and the
official constant files.

---

## Core specifications (start here)

| Document | Scope |
|----------|-------|
| [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md) | **The wire format** - HDLC framing, LAPB, the 13-byte SINTRAN header, packet subtypes, the XMSG sub-header/envelope, and the ROUTING/TAD/DC/PAD sub-protocols |
| [XMSG-API.md](XMSG-API.md) | **Programming/API** - the MON 200B calling convention, T-register option bits, XROUT letters/standard messages, magic numbers/ports/names, and the complete constant catalog |
| [XMSG-COMMAND-REFERENCE.md](XMSG-COMMAND-REFERENCE.md) | The `XMSG-COMMAND` operator utility (network management, tracing, dumps) |

## Layer 2 (LAPB)

| Document | Scope |
|----------|-------|
| [LAPB-REQUIREMENTS.md](LAPB-REQUIREMENTS.md) | Requirements on the LAPB implementation |
| [LAPB-CONFORMANCE.md](LAPB-CONFORMANCE.md) | Conformance notes for the ND LAPB dialect |

## TAD (terminal access) protocol work

| Document | Scope |
|----------|-------|
| [TAD-CONNECT-FIELD-ANALYSIS.md](TAD-CONNECT-FIELD-ANALYSIS.md) | Field-by-field analysis of the TAD connect exchange |
| [TAD-MISSING.md](TAD-MISSING.md) | Known gaps in the TAD understanding |
| [XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md](XMSG-TAD-REAL-SETUP-REFERENCE-2026-07-07.md) | Reference for the real-machine XMSG/TAD setup used in the live experiments |

The TAD wire spec itself is [../../TAD/TAD-Message-Formats.md](../../TAD/TAD-Message-Formats.md).

## Application layer / reverse engineering

| Item | Scope |
|------|-------|
| [COSMOS-RE/](COSMOS-RE/README.md) | **The COSMOS application RE** - four COSMOS `:PROG` binaries decoded in Ghidra (analyses, C# reconstructions, validation plan) |
| [ENNS0-XROUT-DISASSEMBLY-HANDOFF.md](ENNS0-XROUT-DISASSEMBLY-HANDOFF.md) | Handoff for disassembling the ENNS0 Ethernet server / XROUT interaction |
| [ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md](ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md) | Findings on the ENNS0 getMagic strings (a trace-log-viewer label table, not the request builder) |

## Plans, open items, methodology

| Document | Scope |
|----------|-------|
| [XMSG-TRANSPORT-SEAM-PLAN.md](XMSG-TRANSPORT-SEAM-PLAN.md) | The transport "seam" implementation plan (phases for the C# node) |
| [XMSG-OPEN-ITEMS-2026-07-06.md](XMSG-OPEN-ITEMS-2026-07-06.md) | Open protocol questions as of 2026-07-06 |
| [CAPTURE-SPEC-CLIMBED-RECONNECT-2026-07-05.md](CAPTURE-SPEC-CLIMBED-RECONNECT-2026-07-05.md) | Capture specification for the climbed-Flags1 reconnect scenario |
| [LEARNING-A-NEW-PROTOCOL.md](LEARNING-A-NEW-PROTOCOL.md) | Methodology notes: how this protocol was learned from captures and binaries |

## Investigation log (dated questions / answers / handoffs)

Chronological Q&A exchanged with the protocol-expert review during the live bring-up.
Each file is self-describing; superseded conclusions are corrected in later files and
in the core specs, so treat the specs as authoritative.

| Document |
|----------|
| [XMSG-SEQUENCE-RESTART-QUESTION.md](XMSG-SEQUENCE-RESTART-QUESTION.md) / [XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md](XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md) |
| [XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md](XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md) |
| [XMSG-RECONNECT-CRASH-QUESTION-2026-07-03.md](XMSG-RECONNECT-CRASH-QUESTION-2026-07-03.md) |
| [XMSG-MOTD-REMAINING-QUESTION-2026-07-03.md](XMSG-MOTD-REMAINING-QUESTION-2026-07-03.md) / [XMSG-TAD-MOTD-CHALLENGE.md](XMSG-TAD-MOTD-CHALLENGE.md) |
| [XMSG-MASTER-HANDOFF-2026-07-03.md](XMSG-MASTER-HANDOFF-2026-07-03.md) / [XMSG-MASTER-REVIEW-ANSWERS-2026-07-03.md](XMSG-MASTER-REVIEW-ANSWERS-2026-07-03.md) |
| [XMSG-CLIMBED-RECONNECT-FLAGS1-QUESTION-2026-07-04.md](XMSG-CLIMBED-RECONNECT-FLAGS1-QUESTION-2026-07-04.md) |
| [XMSG-DISCONNECT-TEARDOWN-QUESTION-2026-07-04.md](XMSG-DISCONNECT-TEARDOWN-QUESTION-2026-07-04.md) |
| [XMSG-LOGIN-NS6-REJECT-QUESTION-2026-07-04.md](XMSG-LOGIN-NS6-REJECT-QUESTION-2026-07-04.md) |
| [XMSG-RUNNING-NUMBERS-DEEP-QUESTION-2026-07-04.md](XMSG-RUNNING-NUMBERS-DEEP-QUESTION-2026-07-04.md) |
| [XMSG-WIRE-BYTE-NAMING-QUESTION-2026-07-04.md](XMSG-WIRE-BYTE-NAMING-QUESTION-2026-07-04.md) |
| [XMSG-XENSE-STALL-QUESTION-2026-07-04.md](XMSG-XENSE-STALL-QUESTION-2026-07-04.md) |
| [XMSG-ACK-ENVELOPE-QUESTION-2026-07-05.md](XMSG-ACK-ENVELOPE-QUESTION-2026-07-05.md) |
| [XMSG-TAD-OUTPUT-LENGTH-QUESTION-2026-07-06.md](XMSG-TAD-OUTPUT-LENGTH-QUESTION-2026-07-06.md) |
| [XMSG-TAD-LONG-TERMINAL-OUTPUT-QUESTION-2026-07-07.md](XMSG-TAD-LONG-TERMINAL-OUTPUT-QUESTION-2026-07-07.md) |
| [XMSG-TAD-MULTICHUNK-46MS-GAP-DISPROVEN-2026-07-07.md](XMSG-TAD-MULTICHUNK-46MS-GAP-DISPROVEN-2026-07-07.md) |
| [XMSG-TAD-MULTICHUNK-DISPROVES-DUMM-2026-07-07.md](XMSG-TAD-MULTICHUNK-DISPROVES-DUMM-2026-07-07.md) |
| [XMSG-TAD-MULTICHUNK-HANDOFF-2026-07-08.md](XMSG-TAD-MULTICHUNK-HANDOFF-2026-07-08.md) |

Note: the `ProtoCode/` folder directly under DOC is an empty leftover from the
reorganization that moved its contents to [COSMOS-RE/ProtoCode/](COSMOS-RE/ProtoCode/README.md)
(see [COSMOS-RE/Planning/FOLDER-REORG-NOTICE.md](COSMOS-RE/Planning/FOLDER-REORG-NOTICE.md)).

---

**Parent:** [../README.md](../README.md)
