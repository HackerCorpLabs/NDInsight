# COSMOS RE — Capture Validation Plan

**Purpose:** the COSMOS reverse-engineering (`cos-conn-to`, `cos-fa-serv`, `cos-file-tra`,
`cos-xftra`) is decoded from the **application binaries only** — i.e. *above* `MON 200B`. Many facts
are therefore tagged `[BIN]` (read from the binary), `[INFERRED]`, `[CANDIDATE]`, or `[UNKNOWN]`, and
have **never been confirmed on the wire**. There is no live capture yet (no working binaries running).
This document is the forward plan: **when a machine runs, which capture validates which claim, and
how.** It also states honestly which claims a pcap *cannot* confirm.

## How a capture upgrades a tag
- `[DOC-VERIFIED]` → already seen in an earlier pcap (re-confirm, don't re-derive).
- `[BIN]` / `[INFERRED]` / `[CANDIDATE]` → **needs a wire signature to become `[VERIFIED]`.**
- `[UNKNOWN]` → the wire is the *only* source (never emitted in the traced code path).

## What a pcap can vs. cannot show (read first)
- **CAN:** anything in the HDLC/LAPB frame — the SINTRAN header, the XMSG sub-header, and the
  application payload (`[type/tag][len][value]` chains, opcodes, port assignments, letter names).
- **CANNOT (needs a kernel/debugger trace, not a pcap):** call-time `MON 200B` register bits and the
  msg-type *return code*. Per `XMSG-PROTOCOL.md` U1, the `XF*` option bits (XFHIP/XFSEC/…) are
  T-register bits at call time, **not** a wire byte, and there is no proven mapping to the frame's
  frame-flags byte. So e.g. "TAD = XMTHI/XFHIP" is a **kernel-local** fact — validate it with an
  ND-100 debugger/register trace or an emulator hook, **not** a pcap (unless the frame-flags byte is
  later proven to encode priority).

---

## Capture scenarios

Each scenario = operator steps → what to look for on the wire → which tagged claim it validates.

### S1 — connect-to login session (the core TAD case)
**Steps:** from machine A, `@CONNECT-TO <systemB>`; log in with a valid user/password; type a command;
log out.
**Look for (in the TAD trailer of the XMSG data frames):**
- the session-setup chain to the **system port** (342/358) carrying `OPSV`, `TMOD`, `TTYP`, … as **one
  chained message** (not one datagram per opcode).
- the host's `7CORS` (assigned terminal port) and `7LUN` (LU = 768 + value).
- the `7SYCN` login ladder values (`0002` WaitUser → `0006` PassOK → `000A` LoggedIn).
**Validates:**
- `[BIN→VERIFIED]` the append-then-flush **chained-setup-to-system-port** model (conn-to).
- `[UNKNOWN→VERIFIED]` the **`7TTYP` and `7TMOD` payload bytes** (currently left UNKNOWN in
  `CosConnToE02.cs` — read them here; e.g. TTYP should carry the decimal terminal-type, *not* `01 08`).
- `[DOC→re-confirm]` `7CORS` layout `00 00 <node> <port_hi> <port_lo>` (count 5, port = bytes 3..4).

### S2 — connect-to with a WRONG password
**Steps:** as S1 but enter a bad password; observe the failure.
**Look for:** the `7SYCN` value on the failure path.
**Validates:** `[BIN CANDIDATE→VERIFIED or REFUTED]` **`7SYCN 000C = error state`**. The binary's SYCN
handler treats `000C` as an error, but every pcap so far showed a wrong password as a *silent reset to
`0002`*. This capture decides whether `000C` is ever actually sent.

### S3 — remote file READ / WRITE (the FA data path — highest value)
**Steps:** from machine A, open/read (and separately write) a file that lives on machine B, so the
`*FA-SERVER` on B services it.
**Look for (in the FA request/reply payload):**
- **no leading opcode byte** — a bare **typed-param stream** (`[tag][value]` with tag **bit 7 set**).
- the fa-serv param tags **`0x92`=INT16 / `0x94`=INT32 / `0xA2` / `0xF2`**.
- **position + count** typed params, and data in **0x800-byte** chunks (the entry data-buffer unit).
**Validates:**
- `[BIN→VERIFIED]` **FA is data-driven (no numeric opcode)** — confirm the request has no opcode field.
- `[BIN→VERIFIED]` the **bit-7 typed-param marker** and the **`0x92/0x94/0xA2/0xF2` tag bytes**.
- `[INF→VERIFIED]` the **entry-type-`0x10` data path**: position/count params over the 0x800-byte page
  (this also confirms the 0x800 page unit the `cos-file-tra` side found independently).
- `[UNKNOWN→VERIFIED]` the **cos-file-tra data-command wire format** (never emitted in that binary —
  the bytes only exist here, in the FA request B receives).

### S4 — remote file RESERVE / CHANGE-ID / CREATE / DELETE
**Steps:** exercise each file-entry lifecycle op against a remote file (reserve an entry, change its id,
create a new file, delete a file).
**Look for:** which typed params accompany each op, and the reply status codes.
**Validates:**
- `[INF→VERIFIED]` the **entry-type discriminator** `entry[+1] ∈ {1 delete, 2 reserved, 8 change,
  0x10 data, 0x80 named}` and the param-set → operation mapping in `ClassifyOperation` (currently
  `[APPROX]`).
- `[INF→VERIFIED]` the **FA param class meanings** (`9/A/F` in the tags; the `0x4225/0x423f/0x433f`
  type/status words the engine checks) — correlate the class code to the observed value type.
- `[BIN→VERIFIED]` the status codes `0x28`=reserve / `0x29`=release / `3`=bad-type / `5`=full /
  `0x0D`=not-reserved appearing in replies.

### S5 — list-systems / list-route (XROUT service)
**Steps:** `@LIST-SYSTEMS` / a list-route query from the XMSG command program or an app.
**Look for:** the XROUT letter (`FF <len> 2A <name>`) and the reply.
**Validates:**
- `[BIN→VERIFIED]` the **`*FA-SERVER`(11) / `*FA-FSA`(7)** name registration (byte1 `0x45` service).
- `[UNKNOWN→VERIFIED]` the **XSGNI `0x0845` reply payload** (cos-file-tra's only proven send; its
  *reply* was never captured).

### S6 — COSMOS file-transfer between machines
**Steps:** run `XFTRAD` file-transfer of a real file A→B.
**Look for:** the per-page request/reply to `*FA-SERVER` and the letter-indexed param types.
**Validates:**
- `[CANDIDATE→VERIFIED]` the **cos-xftra / cos-file-tra letter-indexed param types** (`A–F` /
  `0x41..0x46`, exact letter→type currently CANDIDATE — the handlers are garbled in the binary).
- `[UNKNOWN→VERIFIED]` the per-page **file-transfer data wire format** (matches S3's FA data path).

### S7 — (debugger/emulator hook, NOT a pcap) msg priority
**Steps:** on the emulator, hook `MON 200B` / the `XFRCV` return and log the T-register / msg-type for
TAD terminal traffic.
**Validates:** `[INF/kernel-local]` **TAD traffic = msg-type 3 = `XMTHI` (high-priority) → sent
`XFHIP`**. Cannot be done from a pcap (kernel-local); needs a register/kernel trace.

---

## Claim → scenario index

| Claim | Tag now | Validated by | Method |
|---|---|---|---|
| Chained setup to system port | `[BIN]` | S1 | pcap |
| 7TTYP / 7TMOD payloads | `[UNKNOWN]` | S1 | pcap |
| 7CORS layout (port = bytes 3..4) | `[DOC]` | S1 | pcap (re-confirm) |
| 7SYCN `000C` = error | `[BIN CANDIDATE]` | S2 | pcap |
| FA is data-driven (no opcode) | `[BIN]` | S3 | pcap |
| bit-7 typed-param marker | `[BIN]` | S3 | pcap |
| FA tags `0x92/0x94/0xA2/0xF2` | `[BIN]` | S3 | pcap |
| FA entry-type-0x10 data path / 0x800 page | `[INF]` | S3, S6 | pcap |
| file-tra data-command wire format | `[UNKNOWN]` | S3, S6 | pcap |
| entry-type discriminator + param→op map | `[INF/APPROX]` | S4 | pcap |
| FA param class 9/A/F meanings | `[INF]` | S4 | pcap |
| `*FA-SERVER`/`*FA-FSA` registration | `[BIN]` | S5 | pcap |
| XSGNI `0x0845` reply payload | `[UNKNOWN]` | S5 | pcap |
| xftra/file-tra letter param types A–F | `[CANDIDATE]` | S6 | pcap |
| TAD = XMTHI / XFHIP (high-priority) | `[INF, kernel-local]` | **S7** | debugger/register trace (NOT pcap) |

## Tooling
Decode captures with the Wireshark dissector `…\SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua`
(`tshark -r cap.pcapng -Y hdlc_lapb -V`) — it already validates the LAPB/envelope/ACK invariants and
flags mismatches. Save new captures in `E:\Dev\Ronny\X25Emulator\pcap\`. Every claim confirmed here
should have its tag upgraded to `[VERIFIED]` (with the capture line ref) in the relevant analysis doc
and in `COSMOS-XMSG-Synthesis.md`.
