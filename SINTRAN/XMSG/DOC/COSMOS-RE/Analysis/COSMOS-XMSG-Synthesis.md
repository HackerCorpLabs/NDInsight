# COSMOS programs over XMSG — cross-program synthesis

**Scope:** how the COSMOS Communication programs use the SINTRAN-III XMSG transport
(`MON 200B`), what is shared, and what each program contributes to the message-format
picture. **Authoritative for the two programs reverse-engineered in this session**
(`cos-file-tra-e02`, `cos-xftra-e02`); the `cos-conn-to-e02` / `cos-fa-serv-e04` columns
summarise the sibling session's findings and are cited as such.

**Symbol authority:** `F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\*`.
**Companion analyses:** `COS-FILE-TRA-E02-XMSG-Analysis.md`, `COS-XFTRA-E02-Analysis.md`.
Tags: **VERIFIED** (decoded byte / symbol file), **INFERRED**, **CANDIDATE**, **UNKNOWN**.

---

## ⚠ LAYER-BOUNDARY CAVEAT (applies to ALL of this document)

Every COSMOS program here is **application-level, ABOVE the `MON 200B` (XMSG) kernel
call.** The transport **envelope** (per-link seed, Counter, epoch, channel byte), the
stateless **secure-ACK** closed form, the **odd-length LAPB address** rule, and the
**≤2-datagram flow-control window** live in the KERNEL and are **invisible** in these
binaries. This document (and the per-program docs/C#) describe application **intent**.
None of it is a wire build-spec: a node built from the app layer alone crashes the real
machine unless the kernel envelope from `…\SINTRAN\XMSG\DOC\XMSG-PROTOCOL.md` is layered
underneath. The kernel-visible boundary is exactly the `MON 200B` register interface below.

---

## 1. The one shared thing: the `MON 200B` XMSG interface

Every COSMOS program reaches XMSG through the **same monitor call** — `MON 200B`
(octal 200 = `0x80`, opcode `0xD680`). [VERIFIED in all four binaries.]

```
 Register interface (the true kernel boundary):
   T = XMSG function code (low byte)  |  option bits (high bits)
   A / D = parameters (handle, byte count, magic number, displacement, sysno)
   X = buffer / port pointer / message handle
 Return:
   T = status   (0 ok-or-pending, <0 = XE* error code, >0 = message TYPE on XFRCV)
   A / D = result value ; X = returned pointer
```

Function codes (decimal, `XMSG-PL-VALUES-L.INCL`) and option bits are identical across
programs. **GOTCHA (verified error class):** a `SAT n` immediately before the MON / a
wrapper call sets **T = function code n** — it is NOT a message-type `== n` comparison.
Confusing the two produced real mistakes (e.g. `SAT 3/4` = XFREL/XFRHD, misread as
XMTHI/XMTRE) that were corrected 2026-07-07.

---

## 2. Two wrapper-library shapes

| Program | Wrapper shape |
|---|---|
| `cos-file-tra` | **Single gateway** `xmsg_mon_call` @7b8f — all traffic funnels through one MON site. [VERIFIED] |
| `cos-xftra` | **Inline per-function library** @2fa1 (11 thunks) **plus a second copy** @6ba1 (5 thunks) — two linked segments. [VERIFIED] |
| `cos-conn-to` | Per-function library @a0b1 (`xmsg_XF*`) — sibling session. |
| `cos-fa-serv` | Per-function library @a0c3 — sibling session. |

All thunks share the idiom: `SAA <fncode>; ORA <options>; ...; MON 200B`.

---

## 3. The application message envelope (above the kernel)

Both of this session's programs build the outgoing message the same way — a **descriptor
`[opcode][sublength][body]`** handed to `XFWRI`, then shipped with `XFSND`. [VERIFIED]

```
 message descriptor (at a frame-local buffer)
 +-----------------+-----------------+-------------------------------+
 | word0 = opcode  | word1 = sublen  | body (name / params / data)   |
 +-----------------+-----------------+-------------------------------+
   XFWRI byte count = sublength + 4
   file-tra: opcode is caller-supplied (baked 0x0845 for the one query)
   xftra:    opcode = RORA(source & 0xFF00)  [BIN-VERIFIED mask 0xFF00, ram:5ec6]
```

For **XROUT-routed letters**, the letter's **byte 1** carries the XROUT service code with
bit 6 set (`XMSG-PL-VALUES` header: "Values in byte 1 of message. Bit 6 set => service
request"). [SYM-VERIFIED]

---

## 4. What each program proves about the wire

| Program | On-wire message(s) proven | Confidence |
|---|---|---|
| **cos-file-tra** | One send: XROUT query, opcode word `0x0845` → bytes `08 45`, byte1 `0x45` = **XSGNI** (get-name). Reply payload **UNKNOWN**. File-transfer *data* commands: not emitted in this segment — **resolved via the `*FA-SERVER`, see §8**. | VERIFIED / (data path via FA §8) |
| **cos-xftra** | Test messages via XFSND (+XFFWD) and XFSND (+XFSEC, 70-byte secure). Typed params use a **letter-indexed** type system (`(byte & 0x7F) − 0x41`, codes `0x41..0x46`). Exact per-letter layout **CANDIDATE**. | VERIFIED / CANDIDATE |
| cos-conn-to | TAD terminal opcode chain `[opcode][count][data]` (sibling). | sibling |
| cos-fa-serv | FA param tags **0x92**=INT16, **0x94**=INT32, **0xA2**=classA, **0xF2**=classF; encoding `(class<<4)|len` (sibling, INFERRED). | sibling |

**Key cross-program GOTCHA:** the numeric param tags `0x92/0x94/0xA2/0xF2` are a
**cos-fa-serv** finding. They do **NOT** appear in cos-file-tra or cos-xftra (`SAA
0x92/0x94` = 0 hits in both). cos-xftra's params are letter-indexed instead. Do not
cross-paste one program's tag table onto another — each application layer differs even
though the transport is shared.

---

## 5. Lifecycle verbs used (per program)

| Verb (XF*) | file-tra | xftra |
|---|---|---|
| XFOPN 10 open port | ✔ | ✔ |
| XFGET 2 / XFREL 3 | ✔ | ✔ |
| XFWRI 7 / XFREA 6 | ✔ | ✔ |
| XFRHD 4 read header | — | ✔ (server recv) |
| XFSND 12 send | ✔ (+SEC/ROU/RRO) | ✔ (+FFWD / +SEC) |
| XFRCV 13 (+WAK) | ✔ | ✔ |
| XFGST 15 status/wait | ✔ | — |
| XFMST 9 / XFSCM 8 | — | ✔ (wrapper lib) |
| XFSIN 16 service-init | — | ✔ (server registers name) |
| XFCLS 11 / XFDCT 1 | ✔ (XFDCT ×3 variants) | ✔ |

[All BIN-VERIFIED from the `SAT <n>` before each MON.] Note file-tra has **three** XFDCT
teardown paths (plain @7c4f, +LEAVE @6f81, +clear @5edb) — found in the full carve.

---

## 6. Roles at a glance

- **cos-file-tra** — COSMOS File Transfer client. Resolves a remote system/server name via
  the XROUT XSGNI query; the actual file-access protocol is QFORM-template-driven and not
  provably emitted in this segment.
- **cos-xftra** — XMSG transport EXERCISER (Client/Server loopback tester for
  `*ae-transport`). Sends/echoes configurable typed test messages; reports throughput.
- **cos-conn-to** — TAD terminal CONNECT-TO client (sibling).
- **cos-fa-serv** — file-access SERVER; receives + dispatches the FA application opcodes
  (sibling) — the place the FA param tags and request-ops are ground-truth.

---

## 7. Open items / how to close them (static-only, no live capture)

1. **file-tra per-page message — RESOLVED as a dead end [BIN-VERIFIED 2026-07-07].**
   `7920` (renamed `file_transfer_progress_setup`, was mis-named "transfer_msg_builder")
   turned out to be the **progress-display** routine, not a message builder: it computes
   the page count (size ÷ `0x800`), times it, and formats the "Current page index:" line
   via QFORM (`[7987]` is the string pointer; `[7985]=0x048e`/`[7986]=0x008e` are QFORM
   field descriptors, NOT wire bytes). **The per-page file-transfer network wire format is
   confirmed absent from this segment** — it is not emitted anywhere reachable here, so it
   stays `UNKNOWN` (likely a `:NEXT` overlay or handled by the `*FA-SERVER`). No opcode
   was invented.
2. **xftra param letters** — decode the 6 jump targets @6418 to resolve the exact
   letter→type mapping (CANDIDATE → VERIFIED).
3. **Reconcile with the sibling** — confirm whether the FA numeric tags or the TAD opcode
   chain appear anywhere in file-tra/xftra (so far: no).
4. Everything genuinely kernel-level (envelope/ACK/flow-window) stays out of scope here by
   the §caveat — it comes from `XMSG-PROTOCOL.md`, not these binaries.

---

## 8. FA application layer — the file-access protocol (from the cos-fa-serv session)

Contributed by the `cos-conn-to` / `cos-fa-serv` session (their `[BIN]`, from the annotated
`cos-fa-serv-e04` DB). This **closes the file-transfer data path** that is `UNKNOWN` in
cos-file-tra. Source: `REPLY-TO-FILE-TRA-XFTRA-SESSION.md`.

**Receive path (server):** `fa_recv_request_wait` (0x8c5d, XFRCV+XFWTF) →
`fa_request_engine_process` (0x8c99) → `fa_dispatch_by_type3bits` (0x08b1).
**Dispatch is DECENTRALIZED** — no single opcode→handler table. An op is distinguished by
**(3-bit category) + (entry-type `entry[+1]`) + (typed-param content)**, and handlers are
reached from multiple indirect COMPUTED_CALL sites. [BIN, sibling]

**Op → handler → entry-type discriminator (13 ops):** [BIN, sibling]

| Op | Handler | Discriminator |
|---|---|---|
| Reserve-file-entry | `fa_reserve_file_entry` 0x2ca5 | sets reservation bits `entry[+0xa]` |
| Release-file-entry | `fa_release_file_entry_op` 0x34cd | entry-type **2** |
| Change-file-entry-id | `fa_change_file_entry_id` 0x2e12 | entry-type **8** |
| Open-file | `fa_open_file_op` 0x2eae | lock bit15 + size check |
| Close-file | `fa_close_file_decrement_ref` 0x2f2d | refcount → free at 0 |
| **Read/Write-file (DATA)** | **`fa_file_data_transfer` 0x315b** | **entry-type 0x10** |
| Create-file | `fa_create_file_entry` 0x3294 / `fa_process_named_file_entry` 0x3332 | entry-type **0x80** = named |
| Delete-file | `fa_delete_file_entry_op` 0x34f8 | entry-type **1** |
| Set-block-size | `fa_blocksize_config_op` 0x33d6 | returns `g_fa_blocksize_9020` |
| File-entry-disconnect | `fa_release_all_session_entries` 0x27f4 | session cleanup |

**Typed-param wire format (fa-serv only):** body = list of `[tag][value]`; tags
**`0x92`=INT16(2B) / `0x94`=INT32(4B) / `0xA2`=classA(2B) / `0xF2`=classF/string(2B)**;
encoding `(class<<4)|len_bytes` [INFERRED]. Parse: `fa_parse_request_params` (0x29c0) →
`fa_process_params_dispatch` (0x35da tbl 0x9039) / `_v2` (0x3b34 tbl 0x9044). Reply
serializers: `fa_build_full_entry_reply` (0x393a), `fa_build_typed_reply_dispatch`
(0x3808 tbl 0x903d). **Reminder:** these numeric tags are FA-ONLY — cos-xftra/file-tra use
the letter-indexed scheme (§4).

**The file-transfer DATA wire format (answers cos-file-tra's UNKNOWN):** [BIN, sibling]
- It is the **entry-type-0x10** path, `fa_file_data_transfer` (0x315b).
- The transferred bytes live in the file-entry's **~0x800-byte (2048 B = 1 ND page) data
  buffer** at far offset `entry[+~0x7ba]`.
- The request/reply carry **position + count** as typed params (`0x92`/`0x94`).
- The valid/lock bit is `entry[+~0x7bf]` bit15.

So cos-file-tra's per-page transfer (page size `0x800`, confirmed on my side at
`file_transfer_progress_setup`) maps onto the FA server writing/reading that 0x800-byte
entry buffer. The two sides now agree on the 2048-byte page unit. [cross-VERIFIED]

**Still open [CANDIDATE]:** the exact numeric op-selector per op — needs the 3-bit-category
decode of `BANK2::8477` correlated to each handler, or a live FA-session capture. The
sibling owns this and will refine it.

### Corrections the sibling handed back (recorded for honesty)
- **`FUN_ram_0517` is NOT the receive dispatcher** — it is a letter/header builder
  (`SBYT byte0=1, byte1=0x45` service, routing bytes), the XROUT registration/reply path.
  My hand-off named it as the dispatcher; that was wrong. The real receive entry is
  `fa_recv_request_wait` @0x8c5d.
- **fa-serv `FUN_ram_*` counts overstate what's undecoded** — it is protocol-complete
  (~179 named); the remainder is a duplicate `_v2` registry (0x42xx mirrors 0x26xx),
  BANK2 utilities, and Ghidra fragment inflation. Don't read "still FUN_ram_*" as "undecoded".

---

## 9. Capture-validation scenarios — how to promote CANDIDATE / INFERRED / UNKNOWN → VERIFIED

**Status:** we do **not** yet have runnable binaries, so nothing below has been captured.
This section is the *ready-to-run playbook* for the day a working COSMOS image boots:
every open tag in this family maps to one concrete capture that settles it. Do the captures
in the numbered order — early ones also sanity-check the transport before you trust the
application-layer reads.

### 9.0 Capture rig (same for every scenario)

Two `nd100x --hdlc` instances bridged over TCP (default ports 10362 / 10364), each a raw
HDLC-over-TCP byte stream. Decode with the Wireshark dissector:

```
tshark -r <cap>.pcapng -Y hdlc_lapb -V           # full decode; validators flag mismatches
tshark -r <cap>.pcapng -Y "tcp.port==10362" ...  # one direction
```

**Run the two conformance scans on EVERY capture first** (they prove the transport before
you read the app layer — if these fail, the seed/class/ACK is new and the app reads are
untrustworthy):
- **Envelope scan** — recompute `Counter` and `Channel` from the §formulas; expect **0
  mismatches** over all Data frames.
- **Secure-ACK scan** — `S_ack = seed + 0x0B`; expect **0 mismatches**.

Node numbers: 100=`0x0064`, 102=`0x0066`, 103=`0x0067`. Per-link seeds: 100↔102=`0x14`,
100↔103=`0x13`, 102↔103=`0x11`. (All from `XMSG-PROTOCOL.md`.)

### 9.1 Scenario A — cos-xftra letter→type mapping  (promotes §5.2 `CANDIDATE` → VERIFIED)

- **What's open:** which of the 6 param-type letters (`0x41..0x46` = A–F) is INT16 / INT32 /
  STRING, and the exact per-letter field width.
- **Run:** cos-xftra **Client** → **Server** loopback with the menu set to send **one message
  per parameter type** — i.e. one INT16, one INT32, one STRING (the "Automatic pattern" off,
  a known Start pattern so the data bytes are recognisable).
- **Decode & look for:** in the XFWRI'd message body, the first byte of each param is the
  **type byte** (`0x41..0x46`, maybe with bit7 set for parity). Correlate: the param the menu
  labelled INT16 carries **2** data bytes, INT32 carries **4**, STRING carries a length +
  variable bytes. Read the type byte in front of each → that fixes letter→kind and the width.
- **Verified when:** each display name (INT16/INT32/STRING) is pinned to a specific letter and
  byte count, matching the 6-way jump at `decode_param_value` (0x640a).

### 9.2 Scenario B — cos-file-tra XSGNI reply payload  (promotes §7c `UNKNOWN` → VERIFIED)

- **What's open:** the reply body of the `0x0845` / XSGNI (get-name) XROUT query.
- **Run:** cos-file-tra command that resolves a remote system/server name (e.g.
  `Get-default-remote-system` or `List-names <known system>`), against a node whose name is
  known.
- **Decode & look for:** the **outbound** letter — confirm byte1 `0x45` = XSGNI and the name
  QSTRING trailer. Then the **inbound reply** on port 0 — dump its trailer bytes: the magic
  number / name / status the server returns. That is the currently-`UNKNOWN` reply payload.
- **Verified when:** the reply trailer fields are decoded and match the fields
  `xmsg_read_message_bytes` (0x6b6d) consumes (word0 masked `0x00FF` = returned service/status).

### 9.3 Scenario C — FA op numeric selector + typed-tag encoding  (promotes §8 `CANDIDATE`/`INFERRED`)

- **What's open:** the exact numeric op-selector per FA op (`BANK2::8477` 3-bit category), and
  the `(class<<4)|len` tag encoding.
- **Run:** a **real file operation between two nodes** that exercises the `*FA-SERVER` — e.g. a
  remote OPEN then a page READ/WRITE then CLOSE (cos-file-tra `Transfer-file` to/from a remote
  system). Capture the whole FA session.
- **Decode & look for:** for each request letter to `*FA-SERVER`, read the leading bytes → the
  **3-bit category** + the **entry-type** (`entry[+1]`: e.g. `0x10` for DATA, `0x80` named,
  `2`/`8`/`1` for release/change/delete). Then the **typed params**: each `[tag][value]` where
  tag ∈ `{0x92,0x94,0xA2,0xF2}` — confirm `0x92`→2 bytes, `0x94`→4 bytes (validates the
  `(class<<4)|len` rule). For the DATA op, confirm **position + count** params precede the
  page payload and the payload is ≤ `0x800` bytes.
- **Verified when:** each of the 13 ops (§8 table) shows a distinct, repeatable leading-byte
  signature on the wire, and the tag→width table holds across all params. **Owner: sibling.**

### 9.4 Scenario D — the XMTHI=3 → XFHIP inference  (promotes conn-to `INFERRED`)

- **What's open:** whether the `SAT 3; SKP` compare in conn-to means the XFRCV return is the
  **msg-type** (so `3 = XMTHI` high-priority, and TAD traffic is therefore sent with `XFHIP`).
- **Run:** a cos-conn-to **connect-to terminal session** to a remote host (login → a few
  keystrokes → logout).
- **Decode & look for:** in the SINTRAN header, **off 16 `frameFlags`** and the XMCSM class
  word — check whether the TAD Data frames carry the high-priority class bit (`5MHIP`) that
  `XFHIP` sets. If the sent frames are high-priority, the `==3` receive filter = XMTHI is
  confirmed; if not, the inference is wrong and the compared value is something else.
- **Verified when:** the on-wire priority class of TAD frames is read and matches (or refutes)
  the XFHIP inference. **Owner: sibling.**

### 9.5 Scenario E — the file-transfer DATA path end-to-end  (cross-checks §8 `[BIN]`)

- **What's open:** confirm the reverse-engineered DATA model (page in a 0x800 buffer, position
  + count typed params) actually appears on the wire.
- **Run:** cos-file-tra `Transfer-file` of a **multi-page file** (> 2048 bytes) to a remote
  system, so several DATA ops flow.
- **Decode & look for:** a sequence of entry-type-`0x10` requests, each carrying a **position**
  that advances by `0x800` and a **count** ≤ `0x800`, followed by (or carrying) the page bytes;
  the last page's count < `0x800`. Cross-check the page count against the file size ÷ 2048 that
  `file_transfer_progress_setup` computes.
- **Verified when:** the observed per-page position/count sequence matches the page arithmetic
  on both the client (cos-file-tra) and server (`fa_file_data_transfer`) sides.

### 9.6 What a capture can NEVER promote (stays out of scope)

The kernel-level items in the §caveat — the envelope seed/Counter/channel derivation, the
stateless secure-ACK closed form, the odd-length LAPB address rule, the ≤2-datagram flow
window — are **already VERIFIED** in `XMSG-PROTOCOL.md` from prior captures; they are *inputs*
to the scans in §9.0, not things these application binaries can confirm or deny. If a §9.0
scan **fails**, that means a new *transport* seed/class was introduced, not an app-layer bug.
