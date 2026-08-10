# XMSG Documentation

**The XMSG documentation set: the wire-protocol and API specs, the LAPB layer-2 requirements, the TAD terminal protocol work, the COSMOS application-layer reverse engineering, and the dated question/answer/handoff log of the live protocol investigation.**

See [../README.md](../README.md) for the XMSG overview, provenance rules, and the
official constant files.

---

## Core specifications (start here)

| Document | Scope |
|----------|-------|
| [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md) | **The wire format** - HDLC framing, LAPB, the 14-byte SINTRAN header, packet subtypes, the XMSG sub-header/envelope, and the ROUTING/TAD/DC/PAD sub-protocols |
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

## Kernel-carved findings and session walkthroughs (2026-07-26)

| Document | Scope |
|----------|-------|
| [XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md](XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md) | **The magic-number bit layout**, carved from the XMSG L03 kernel (`ZCRMG`, `ZRAND`, `MFM2P`): `system << 16 \| port << 7 \| random`, and the proof that the "random" part is a linear congruential generator |
| [XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md](XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md) | Proof that the wire `XMDPT`/`XMSPT` fields ARE the magic number's low word |
| [XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md](XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md) | **XROUT service calls captured from guest memory** - the `XSNAM` registration (which never crosses a wire), proof that the message-buffer form carries the 4-byte header the wire form omits, and the magic-number layout confirmed from a running kernel |
| [XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md) | **Connection ports captured** - the `XSCRS` registrations of `*XFTRA` / `*FA-FSA` / `*FA-SERVER`, and the correction that the free-SP count is built by repeated `XSNSP` +1 rather than set at registration |
| [XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md) | **Remote file access decoded** - the `XSLET` letter to `*FA-SERVER`, whose payload is RAW bytes after the parameter block (unlike `*XFTRA`), built with an append write. Captured on the SINTRAN K image, which is the only pack where the File User runs |
| [XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md) | **First COSMOS file-server traffic ever decoded** - the `XSLET` letter to `*XFTRA` carrying the whole transfer specification, and the unroutable reply that returns the body with the status in the service byte |
| [XMSG-FA-ACCESS-PASSWORD-ON-THE-WIRE-2026-07-29.md](XMSG-FA-ACCESS-PASSWORD-ON-THE-WIRE-2026-07-29.md) | **File access WORKING, and the password on the wire** - 102 lists a real user's files on 100 over HDLC. Correct vs wrong password requests differ in exactly ONE byte, which both pins the password field and confirms the carved fold algorithm live. The plaintext never leaves the machine |
| [FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md](FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md) | **What an FA request and reply CONTAIN** - READ and WRITE decoded from live D100/D102 captures, the file-data envelope, the partial last block, the error path, the field tags. Also the proof that the SINTRAN header is **14** bytes and the sub-header 14 more, putting an FA body at absolute 28 |
| [FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md](FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md) | **How an FA conversation is SEQUENCED** - a request is answered by a ShortAck at its Flags 1 and the reply is a NEW exchange one higher; get it wrong and the peer rejects with XENSE. Also XMCSM is the frame's own body length, the directory cursor is "start over / next" rather than an index (the SERVER holds the position), and `0x078x` means "finished" and wants a Close |
| [PLAN-FA-FILE-SERVER-2026-08-06.md](PLAN-FA-FILE-SERVER-2026-08-06.md) | **START HERE for the FA file server** - the forward plan. P1 is proving the read/write path against a real machine, because everything about reading and writing a file is verified against captures only and no real client has ever read a file from us. Then the remaining operations, then the `CS0618` sweep. Also the traps that cost a session each |
| [NDIX-XMSG-CROSS-CHECK-2026-08-05.md](NDIX-XMSG-CROSS-CHECK-2026-08-05.md) | **A second, independent XMSG client read against ours** - the NDIX-C ND-500 Unix port. Confirms the `*XFTRA` parameter order and that the file type is always sent; CORRECTS two guesses in our capture doc (parameter 11 is the operation, parameter 13 is the folded password); and shows why function 48 must NOT be added - ND's own include calls it the table END MARKER. Also the ND-500 `SHR` trap: it means ROTATE, positive = LEFT, so reading it with ND-100 habits gives the wrong answer |
| [XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md](XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md) | **What a letter that SUCCEEDS looks like** - the `connect-to` accept, found in the pcap corpus: the service byte survives, the body is replaced by a fixed 8-byte answer, and the reply comes from the server's own port rather than from XROUT. The exact opposite convention to the `XRNRO` refusal, which preserves the body and overwrites the service |
| [XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md](XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md) | **The `*XFTRA` letter read field by field** - the same transfer driven six times on the K image changing one input per run, proving which parameter is the remote system and which the remote user, that the SOURCE file is absent from the request, that strings are word-aligned with a pad byte, and that this letter carries no per-request counter |
| [PLAN-FILE-SERVER-CAPTURE-2026-07-28.md](PLAN-FILE-SERVER-CAPTURE-2026-07-28.md) | **NEXT UP** - how to capture `*XFTRA` and the `*FA-*` family: which method answers which question, what to drive, and what "done" looks like |
| [XMSG-HANDOFF-2026-07-28.md](XMSG-HANDOFF-2026-07-28.md) | **START HERE** - both file servers decoded, the successful-answer form found, the single remaining blocker, which image to use, and the traps |
| [XMSG-HANDOFF-2026-07-27.md](XMSG-HANDOFF-2026-07-27.md) | SUPERSEDED by the 07-28 handoff. Kept for the library inventory and the open items it still describes accurately |
| [XMSG-COMMAND-MON200-CARVE-2026-07-27.md](XMSG-COMMAND-MON200-CARVE-2026-07-27.md) | **Where XMSG-COMMAND's MON 200 calls are** - only three, two hardcoded, one general wrapper taking its function code from the caller's T; narrows but does not close the "can it issue XFWRI?" question |
| [XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md](XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md) | **Name lookup captured** - `XSGIN` for a system name, a port name and an unknown name, which command emits which service, and why `XSGMG` cannot be reached from the XMSG command program |
| [XMSG-SERVER-NAMES-AND-LETTERS.md](XMSG-SERVER-NAMES-AND-LETTERS.md) | **How named servers work** (`*TADADM` and friends) - XROUT as a letterbox, `XSNAM` vs `XSCRS`, and the checklist for building a server |
| [XMSG-CONNECT-TO-LOGIN-WALKTHROUGH.md](XMSG-CONNECT-TO-LOGIN-WALKTHROUGH.md) | **The whole connect-to + login session**, frame by frame in true time order. Interactive companion page (packet ladder + all 98 frames decoded): <https://claude.ai/code/artifact/2fea47cb-2947-48da-981a-bfe7846a8ab6> |

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
| [RETROCORE-TO-XMSG-HLE-STARTNET-REPLY-REQUEST-2026-08-09.md](RETROCORE-TO-XMSG-HLE-STARTNET-REPLY-REQUEST-2026-08-09.md) / [XMSG-TO-RETROCORE-HLE-STARTNET-REPLY-2026-08-09.md](XMSG-TO-RETROCORE-HLE-STARTNET-REPLY-2026-08-09.md) - ENNS0 start-net-server reply bytes (still open) and XFMST task-current semantics (answered from the manual) |
| [RETROCORE-TO-XMSG-POCSPROCES-REPLY-FIELDS-2026-08-09.md](RETROCORE-TO-XMSG-POCSPROCES-REPLY-FIELDS-2026-08-09.md) - follow-up: start-net decoded as a multi-round TLV directory conversation; asks POCSPROCES field derivations (what is 0x45B8, per-record reply selection, which tags derive from identity) to build the HLE full responder |
| [XMSG-TO-RETROCORE-POCSPROCES-REPLY-FIELDS-2026-08-09.md](XMSG-TO-RETROCORE-POCSPROCES-REPLY-FIELDS-2026-08-09.md) - the answer: identity comes from runtime global 0x1E21A (settled); 0x45B8 is a system-number-class value with a one-grep test to decide echo vs derive; reply selection NOT known |

Note: the `ProtoCode/` folder directly under DOC is an empty leftover from the
reorganization that moved its contents to [COSMOS-RE/ProtoCode/](COSMOS-RE/ProtoCode/README.md)
(see [COSMOS-RE/Planning/FOLDER-REORG-NOTICE.md](COSMOS-RE/Planning/FOLDER-REORG-NOTICE.md)).

---

**Parent:** [../README.md](../README.md)
