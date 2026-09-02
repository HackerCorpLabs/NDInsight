# XMSG / COSMOS — resume point after reboot

**Written:** 2026-08-02, immediately before a Windows reboot.
**Everything below is committed and pushed** (`NDInsight` `5000x`, head `6bacc62`).
Nothing is only-on-disk. Nothing is only-in-a-running-process.

---

## 0. Machine state — all of this dies on reboot

| Machine | Where | Ports | Purpose |
|---|---|---|---|
| **BIG** (id 102) | `F:\RC\RonnyTest\BIG\RetroCore.ini` | telnet **9202**, DAP **4202** | the 201-user NDFS pack. Its pack is already captured as a fixture; nothing further is needed from it. |
| d100 / d102 / d103 | (previous sessions) | see their `RetroCore.ini` | the COSMOS/XMSG network. **These are what you need back for XMSG.** |

The `RetroCore.ini` files tell you the ports: `tcp start <telnet>`,
`device add TERM n --port=<telnet>`, `DAPSTART <dap>`.

**Driving a machine headless** is documented in the `sintran-console-driver` memory note. The
essentials: connect, **send ESC first** (a fresh connection shows only a RetroCore banner; ESC
produces the SINTRAN banner and `ENTER`), log in, then answer prompts **one field at a time** —
do not pass comma-separated arguments. Only one connection at a time; reconnecting mid-program
hangs the line and ESC is what recovers it.

For the XMSG machines specifically, the transport is set in the ini:
`device add ETH 0 --net=udp` (multicast 239.3.9.4:3094, **not capturable on the same host**),
or `--net=tcp`/`listen:`, or `pcap:`. HDLC is `device add HDLC 1 --connect=localhost:10364`.

---

## 1. Where XMSG stands

### 1.1 Settled and load-bearing

**Header word 6 is a ones-complement checksum**, not a channel/epoch/seed. Carved from the XMSG
kernel routine at `137314` and verified on **3595 of 3595** frames — every subtype, both
directions, every link:

```
w0 markers | w1 type:subtype | w2 dest | w3 src | w4 Flags1 | w5 Flags2 | w6 checksum
w6 == ~ones_complement_sum(w0..w5, 0)      ; 16-bit, END-AROUND carry
```

So the SINTRAN header is **14 bytes (7 words), not 13**, and the XMSG sub-header starts at
offset **14**. What was long called the "Protocol ID" at offset 12 is the checksum HIGH byte;
what was called the "Counter" at offset 13 is its LOW byte.

`XMSG-PROTOCOL.md` §5 now carries that correction, and
`XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md` carries a SUPERSEDED banner. **Sections 4.3,
9.1.1 and 18.2 of `XMSG-PROTOCOL.md` still carry the old off-by-one and the superseded
channel/epoch model** — treat **§18.5 as authoritative** wherever they disagree. Fixing those
three is a good first task.

> Why the old model survived: "zero formula failures across 601 data + 602 ACK frames" tested
> an algebraic restatement, not a prediction. The seed was *defined* as
> `(Counter + Flags1 + Flags2low) & 0xFF` and then learned per frame from that same identity,
> and the header words that never vary within one link were absorbed into it silently. The
> check could not have failed however many frames it ran over. `XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md`
> says this well: *"the 'seed' was the contribution of the fields nobody was varying."*

**COSMOS over Ethernet** (committed `6bacc62`): 802.3 with a LENGTH field (<= 1500, not
EtherType) + LLC1 UI, DSAP = SSAP = `0xA8`, control `0x03`, no SNAP. Then an 11-byte ND link
header:

```
0b 02 | kind | 00 | seq | senderLinkId(2) | receiverLinkId(2) | plen(2)
kind 0x20 = data, 0x3f = ack; ack carries seq+1; 802.3 length = 3 + 11 + plen
```

MAC = `08 00 26` + ND system number as a 16-bit value **byte-reversed** + physical user code;
top two bits of byte 5 are the protocol family (`11` = ND/COSMOS). The node number is 16-bit —
`ND-60.197.01` §2.4.

The SINTRAN header and its checksum are **transport-independent** (128/128 including relayed),
so nothing above the link layer changes between HDLC and Ethernet.

**Route-through:** a relay rewrites **only** word 0 (`0x2113` -> `0x2112`) and word 6. Endpoints,
Flags1, Flags2 and body untouched; ACKs are end-to-end.

### 1.2 What exists in code

`SINTRAN/XMSG/SRC/` — solution `Xmsg.Protocol.slnx`:

| Project | State |
|---|---|
| `Xmsg.Protocol` | frame/packet decode |
| `Xmsg.Api` / `Xmsg.Api.Node` | the user-facing library — **write against this, never raw frames**. `XmsgKernel` (appendix A function set), typed `XroutRequests` builders for every XS* service, RR-LIB client/server |
| `Xmsg.Node` | node model |
| `Xmsg.Hdlc` | HDLC/LAPB transport |
| `Xmsg.Live` / `Xmsg.Live.Runner` | the live seam; runner reads `topology.json` |
| `Xmsg.Ethernet` | **NEW, committed `6bacc62`, 27 tests.** `NdMacAddress`, `NdLinkHeader`, `Ieee8023Frame`, `NdLinkLayer`, and UDP / TCP (RETH handshake) / loopback backends |
| `Xmsg.Servers`, `Xmsg.Chat`, `Xmsg.Ndfs`, `Xmsg.Diagnostics` | supporting |

---

## 2. Next task — the D9999 C# node

**Decided with the user:** build a C# node as **D9999** on the Ethernet segment, going *all the
way to serving files*, supporting **all three RetroCore transports** (udp, tcp, raw network)
**plus HDLC**, and with a **multi-client TCP server variant** so 10-100 ND machines can use one
central bridge ("typically how we would do it over the internet").

Plans, both committed:
- `PLAN-CSHARP-ETHERNET-AND-ROUTE-THROUGH-2026-08-01.md`
- `PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md`

### 2.1 Immediate next steps

1. **`EthernetLink : ILink`** so D9999 can actually join the segment. `Xmsg.Ethernet` has the
   framing and backends; what is missing is the adapter onto the existing link abstraction.
2. **Relay support** — `MakeRelayed` + `DatagramRelay`. Our C# node currently **cannot relay**.
   The rule is small and known (word 0 and word 6 only), so this is mostly plumbing.
3. **Multi-client TCP hub.** `TcpEthernetRelay.cs` already exists in RetroCore as a working
   multi-client hub — read it before writing a new one.
4. **File server stages 2-7** of the file-server plan, then the client test harness, then the
   Windows folder-sync app the user asked for ("sync all files in a Windows folder to a SINTRAN
   user").

### 2.2 Design points already settled with the user

- **Access rights**: define them in the file server, and integrate with the RetroFS access
  model — but **only after the move**, not before. It is in the plan as a post-move item.
- **File numbers are stable identifiers.** They persist for the life of a file and deletion
  leaves a hole that is never compacted. A sync tool may cache them. See §4 below — the rule
  changed on 2026-08-02.

---

## 3. Traps that have already cost time — do not rediscover these

- **Diff the machine config before any protocol theory.** Hours went into an XMSG/TADADM/lock
  theory for an FSA hang on d102; the cause was `--cpu=ND120CX` in its ini. That line had been
  read at session start and not connected.
- **`SET-ADVANCED` is the gate for XMSG tracing.** Every trace command prompts `XROUT system?`
  first and swallows the next line; `ENABLE-TRACE` prompts rather than taking the number inline.
  The comma form `ena-trac,,9` works. Enable events **8 AND 9** — a session where only 8 was on
  was reported as "both enabled" and wasn't; the tell was that task 152616 made 6 sequential
  calls with no returns traced.
- **`--net=udp` is not capturable on the same host** (multicast 239.3.9.4:3094). Use tcp or pcap
  when you need a capture.
- **An XRNRO never crosses a wire.** The local XROUT generates it and hands it straight back
  through MON 200, so it appears in a MON 200 trace and never in a pcap.
- **Ports are not well-known.** They are the kernel's port-table index for whatever port a
  server happened to open, so they move with load order. Two boots gave `*XM-FIDO` 3 and 4.
  Resolve names at run time (XROUT letter, or `XSGIN`) and learn ports from frames. Only port 0
  (XROUT) is known a priori. An earlier version of the decode skill listed a fixed table; it was
  wrong. Same for **port 342** — `TAD-Message-Formats.md` explicitly corrects "always 342".
- **Odd-length I-frames need LAPB address `0x89`, not `0x09`.** A real ND silently discards the
  frame otherwise; V(R) freezes, the next frame draws REJ, and the login stalls at `PASSWORD:`.
- **Flags1 is one sequence per direction per link**, shared by all sessions. Continue it across
  sessions, DCON, LAPB re-SABM and process restarts. Never reset per connect (lands behind a
  climbed peer -> silent drop); never echo the peer's (the historical crash).
- **Strings are word-aligned with a pad byte and the declared length counts the pad.** Every
  string captured before 2026-07-28 happened to be even-length, so this went unexercised for
  months; a parser advancing by the parameter length alone desynchronises on the first odd one.
- **`*FA-SERVER` builds its request in TWO `XFWRI` writes**, the second with displacement -1 to
  append. A reader assuming one write per message truncates it.
- **COSMOS File User is gated at revision F.** Only E media exists here, so remote file ACCESS
  cannot run on the L or M images at all. It works on **SINTRAN K**.

---

## 4. Cross-cutting change from this session that touches XMSG work

The NDFS libraries were corrected on 2026-08-02 and **`Xmsg.Ndfs` may depend on the old
behaviour**. Check it before trusting file numbers:

- The allocation bitmap is **16-bit word addressed** (page N = bit N%16 of word N/16), not
  byte-addressed. Four libraries had it byte-swapped; writing to a real pack could corrupt it.
- **The file number is the ordinal RANK** of an entry's index-block group among the groups that
  user occupies — **not** the physical group. SINTRAN relocates a user's overflow object block
  when another user needs that group (watched moving twice, live). So file numbering is no
  longer a pure function of position; it is a post-load pass.
- Object entry **bytes 32-33 are LAST RESERVING USER**, not a file-type code. The type is the
  4 ASCII characters at bytes 18-21.

Full detail: `norskdata-ndfs/docs/NDFS-OBJECT-BLOCKS-SPEC.md` §6, and `norskdata-ndfs/CHANGELOG.md`.

---

## 5. Housekeeping backlog (low priority, listed so it is not lost)

- Retire the 32-bit `ControlService` — **108 call sites across 34 files**.
- Rename the `SintranProtocolId` misnomers (it is the checksum high byte, not a protocol id).
- Delete the superseded `XmsgEnvelope` members.
- `XmsgDataFields.ControlService` reads wire bytes 26-29 as a 32-bit XMCSM. There is an open
  question (`XMSG-FIELD-INVENTORY-2026-07-31.md`) whether XMCSM is **one word** at 26-27 with
  `0x0141` being the first BODY word. Both parses give the same answer on every captured frame,
  so the corpus cannot decide it — this needs a frame where they differ.
- **User action, still outstanding:** install the fixed `hdlc_tcp.lua` into
  `C:\Program Files\Wireshark\plugins`.
- `XMSG-PROTOCOL.md` §4.3 / §9.1.1 / §18.2 still carry the superseded channel model (see §1.1).

---

## 6. Where the authority is

| For | Read |
|---|---|
| Wire format, worked scenarios | `SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md` (**§18.5 wins on conflict**) |
| The XF*/XS* semantics behind the wire | `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` |
| TAD opcodes, login ladder, connect-to recipes | `SINTRAN/TAD/TAD-Message-Formats.md` |
| Multi-transport / relay bring-up | `SINTRAN/XMSG/DOC/COSMOS-RE/COSMOS-MULTI-NODE-NETWORK-2026-07-25.md` |
| Named servers, XROUT as a letterbox | `SINTRAN/XMSG/DOC/XMSG-SERVER-NAMES-AND-LETTERS.md` |
| Ethernet transport | `SINTRAN/XMSG/DOC/COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md` |
| What is still open | `SINTRAN/XMSG/DOC/XMSG-OPEN-ITEMS-2026-07-06.md` |
| Captures | `E:\Dev\Ronny\X25Emulator\pcap\*.pcapng`, master decode `SRC/pcap-decode-report.txt` |

Use the **decode report**, not the per-capture `.md` summaries — those truncate every payload to
the first 16 bytes.

The `xmsg-decode` skill carries the formulas and framing in condensed form. Note it still
describes offset 13 as "Counter"; §1.1 above supersedes that.

---

## 7. Not this thread

The VERIFIED-audit remainder has its own handoff:
`SINTRAN/Filesystem/HANDOFF-VERIFIED-AUDIT-REMAINDER-2026-08-02.md`. Three unresolved claims and
eleven LOW items. Different person, different thread — do not merge the two.
