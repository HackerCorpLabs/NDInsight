# Learning a new XMSG sub-protocol — methodology

How to reverse-engineer a NEW service on top of XMSG (the file server `*XM-FIDO`,
app-to-app user traffic, mail, SINTRAN's own inter-node services, …) from captured
traffic, and fold it into the specs and the dissector. This is the repeatable process
distilled from the TAD terminal-protocol work — TAD is used throughout as the worked
example of what each step produces.

## The core thesis: the transport is protocol-AGNOSTIC

Everything below the payload is already solved and is IDENTICAL for every service:
HDLC framing + FCS, the ND LAPB dialect (including the odd-length address bit), the
13-byte SINTRAN header, the datagram-sequence/Counter/**channel envelope** (the seed
model), the **secure ACK**, **reachability/resync**, the **odd-length address** rule,
and the **port** scheme. See `XMSG-PROTOCOL.md`.

So when a new sub-protocol appears, only **three** things are new:

1. the **port-0 dispatch identity** — a new SERVER name (XSLET letter) or a new numbered
   SERVICE (XMCSM low byte), see `XMSG-PROTOCOL.md` §7 / §18.8 S10;
2. the **XMCSM class words** its data frames use (each new `Flags2 = XMCSM>>16` is a new
   channel lane / `baseLow` value);
3. the **trailer vocabulary** — the opcode/message format inside the frames.

Everything else you already know how to decode. That is why this process is fast:
you spend your effort only on those three, and you *prove* the rest still holds with two
automated scans before touching anything.

## Step 0 — capture the right traffic

Exercise the new service with REAL traffic between two machines and capture it on the
`nd100x --hdlc` TCP bridge. Save the `.pcapng` in `E:\Dev\Ronny\X25Emulator\pcap\`.
Good first targets and how to elicit them:

- **File server family:** the live `list-servers` registry (XMSG-PROTOCOL.md §7.1) shows
  a whole family of file/COSMOS servers not yet captured — `*XM-FIDO` (port 4),
  `*COSPO` (5), `*FA-FSA` (7), `*XFTRA` (8), `*FA-SERVER` (11). Do any COSMOS
  remote-file operation between the machines (remote file access / copy / a File-Server
  command); the first XSLET letter naming one of these opens that sub-protocol. Census
  the names (step 2) to see WHICH server the operation actually drives — a file transfer
  may touch several.
- **App-to-app XMSG:** run a user program on each node that opens a port (`XFOPN`) and
  sends/receives via the XMSG API (`XFSND`/`XFRCV`).
- Capture BOTH directions and let the session run to completion (setup → work → teardown)
  so the full shape is present. For sequencing questions, capture with timestamps and,
  where relevant, several back-to-back sessions without restart.

The `XMSG-OPEN-ITEMS-2026-07-06.md` "combo capture" script lists concrete operator steps.

## Step 1 — prove the transport still holds (two conformance scans)

Before decoding anything new, run the two scans that instantly confirm the new traffic
obeys the known transport. If they pass, you KNOW only the application layer is new and
you can trust every envelope/ACK byte.

- **Envelope scan** — for every Data (0x0E) frame recompute
  `Counter = (seed − Flags2low − Flags1) & 0xFF` and
  `Channel = 0xDE − (XMCSM>>24) − epoch`, and compare to the wire bytes. Expect **0
  mismatches** (VERIFIED 753/753 on the current corpus). A mismatch means either a new
  per-link seed or — more interesting — a genuinely new envelope behaviour to chase.
- **ACK scan** — for every subtype-0x03 frame check
  `trailing = (S_ack − Flags2low − ackedFlags1) & 0xFF`, `S_ack = seed + 0x0B`,
  `channel = 0xDE − epoch(ackedFlags1)`. Expect 0 mismatches (VERIFIED 904/904).

The `hdlc_tcp.lua` dissector performs both live (mismatches show as expert warnings), so
`tshark -r cap.pcapng -Y hdlc_lapb -V` and grep for warnings is the fastest scan; a
standalone Python scan over `raw=` hex lines is the reproducible one.

*TAD example:* these scans are exactly how the envelope model was proven and how the
seed-per-responder hypothesis was refuted — the arithmetic held across every capture, so
all TAD work could assume a correct envelope and focus purely on the payload.

## Step 2 — identify the service (name census / dispatch)

Every request arrives at **port 0** and forks on the XMCSM low byte.

- **XSLET letters (low byte 0x41):** scan every letter's `FF <len> 2A <name>` for the
  SERVER name. This is an exhaustive census — extract the name from every frame carrying
  the `FF ll 2A` signature. Compare the names to the live **`list-servers`** registry
  (name → logical port → free session slots).
- **Numbered services (other low bytes):** e.g. `0x4B` = XSGSY routing; these carry NO
  name — the code IS the selector.

*TAD example:* the census found `TADADM` as the only name on the wire (18/18 letters);
`list-servers` additionally showed `*XM-FIDO` at logical port 4, unaddressed in any
capture — i.e. the file server is the obvious next name to expect. Terminology
(server vs service vs function) is fixed in `XMSG-PROTOCOL.md` §7.

## Step 3 — map the new XMCSM class words

List the distinct `XMCSM` values (and therefore `Flags2 = XMCSM>>16` class words) the new
service uses. For each, record `baseLow = (seed − Flags2low) & 0xFF` and which channels
it rides per epoch. Each new class is a new lane the envelope already knows how to place;
you are just cataloguing which the service uses.

*TAD example:* four classes — `0x0400` (letters), `0x0108` (terminal data), `0x0008`
(control: ESCA/`0x20`/DCON), `0x0006` (the `0xFD` notification). A file server will
likely introduce its own class word(s) — record them the same way.

## Step 4 — decode the trailer vocabulary

The trailer is the genuinely new part. First pass: try the TAD-style
`[opcode][count][data…]` chain walker (skip `0x00` bytes between messages — they are
word-align pads or the 16-bit-opcode prefix of certain opcodes). Catalog every opcode:
its direction on the wire, data length, where it appears, and any payload structure.

Then NAME the unknown opcodes by matching their byte values against the kernel symbol
tables — this is the highest-leverage technique and it works because the missing kernel
SOURCE left the symbol NAMES behind:

- Symbol lists: `SINTRAN\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\*-SYMBOL-LIST.SYMB.TXT` and
  `SINTRAN\XMSG\XMSG-VALUES-M.SYMB` — **values are OCTAL**; small values = opcodes/offsets,
  large = code/data addresses.
- Look for a coherent FAMILY of related names at consecutive values (that is an enum).
- Confirm usage in the NPL sources `SINTRAN\NPL-SOURCE\NPL\*.NPL` (`MP-*`, `RP-*` for the
  process split; message headers pack the opcode as `value<<8`).

*TAD example:* the `7*` family named the wire opcodes — `0x0B = 7LUN` (which independently
confirmed the measured LU=768+XX), `0x20 = 7ESRS` escape response, `0xFD = 7POLL`,
`0x06/0x07 = 7CORQ/7CORS` connect request/response, etc. — see TAD doc §2.1. A file
server will have its own family (look for FILE/FS/FIDO/`XF*`-adjacent names); the same
octal-matching method applies. The symbol-hunt agent prompt used for TAD is a reusable
template.

## Step 5 — record the session shape and port lifecycle

Classify the exchange against the three known shapes (`XMSG-PROTOCOL.md` §18.8 S10):

- **session-opening** (accept → port-assign → session, like connect-to),
- **letter-only** (query + reply, no session, like list-systems),
- **request/reply service** (like list-route/XSGSY).

Note who allocates which port and when (§7.1): the client's own port (`XFOPN` on the
originating node), any well-known server wire port, and any per-session port the server
mints. Record the ACK discipline (does every data frame get its 0x03 ACK, as TAD does?)
and the teardown.

## Step 6 — write it up (with discipline)

Document the transport-level findings as a new scenario/section in `XMSG-PROTOCOL.md`
(so the transport stays one source of truth) and the application layer either there or in
a companion doc (as TAD has its own `TAD-Message-Formats.md`). Non-negotiable discipline,
the reason these docs are trustworthy:

- **Tag every claim** VERIFIED (byte-for-byte in a capture, cite frame/line) / INFERRED
  (reasoned, state the reasoning) / ASSUMPTION / UNKNOWN. Never present a guess as a fact.
- Prefer a MEASURED scan (counts like "18/18", "753/753") over an eyeballed sample.
- When a rule can't be settled from the corpus, say so and state the exact capture or
  experiment that would settle it (feed it into `XMSG-OPEN-ITEMS-*.md`).
- Add worked scenarios with real bytes; implementers read those first.

## Step 7 — extend the dissector, then re-verify

Add the new opcodes / class handling / letter names / validators to
`SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua`, mirroring the spec. Verify with
`tshark -r <cap> -Y hdlc_lapb -V` over several captures: zero Lua errors, zero NEW false
expert-warnings on known-good traffic. Install to ONE Wireshark plugin folder and reload.
Details and guardrails: the folder's `README.md` and the `xmsg-decode` skill's
"Upgrading the dissector" section.

## Step 8 — (optional) build it into the C# node

The reference node under `SINTRAN\XMSG\SRC\` is structured for this: the transport
(`Xmsg.Protocol` envelope/ACK) is shared, and a new service is a new handler alongside
`TadTerminalResponder`. Per the server-vs-service terminology, the pluggable named
services (TAD, and a coming file server) are `IXmsgServer`-shaped; reserve "service" for
the port-0 router dispatch.

---

## Quick checklist

```
[ ] 0. Capture real new-service traffic (both directions, full session), save to pcap/
[ ] 1. Envelope scan + ACK scan → 0 mismatches (transport confirmed)
[ ] 2. Name census (FF ll 2A) + compare to list-servers → the new server/service
[ ] 3. Distinct XMCSM class words → baseLow lanes
[ ] 4. Trailer opcode catalog → name them via the octal symbol tables + NPL usage
[ ] 5. Session shape (session / letter-only / request-reply) + port lifecycle + ACK rule
[ ] 6. Write up in XMSG-PROTOCOL.md (+ companion), VERIFIED/INFERRED/UNKNOWN tagged
[ ] 7. Extend hdlc_tcp.lua, tshark-verify (0 errors/0 new warnings), install+reload
[ ] 8. (optional) add the IXmsgServer handler in the C# node
```

## References

- `XMSG-PROTOCOL.md` — the transport (envelope §18.5, ACK §6, ports §7.1, dispatch §7/§18.8 S10, scenarios).
- `TAD-Message-Formats.md` — the worked application example (opcode census §2.1, session §22).
- `XMSG-OPEN-ITEMS-2026-07-06.md` — the capture/experiment plan, including the `*XM-FIDO` opener.
- `Devices/HDLC/WireShark/hdlc_tcp.lua` + `README.md` — the dissector and how to run/upgrade it.
- `xmsg-decode` skill — the condensed operational version of this method.
