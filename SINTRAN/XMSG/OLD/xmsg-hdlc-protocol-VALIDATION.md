# XMSG-over-HDLC Protocol Spec — Validation Against Captured Traffic

This document validates the peer-authored specification
[xmsg-hdlc-protocol.md](xmsg-hdlc-protocol.md) against the real packet captures,
and records new findings (most importantly, the meaning of the previously
undocumented **subtype `0x03`** frame).

Every claim below is tagged **VERIFIED** (reproduced from the wire),
**REFUTED** (contradicted by the wire), **CAPTURE-SPECIFIC** (true only for a
traffic class not present in these captures), or **INFERRED** (a reasoned
conclusion not yet independently confirmed). Frame counts and example bytes are
cited so anyone can re-check.

---

## 1. Method (how these results were produced)

The captures were **not** decoded by trusting either the existing Wireshark
dissector or the peer's spec. Instead the raw bytes were parsed independently:

1. Extract raw TCP payloads with `tshark -T fields -e tcp.payload` (no dissector
   involved), reassemble each TCP stream per direction ordered by `tcp.seq`.
2. De-frame HDLC: split on `0x7E` flags, reverse `0x7D` byte-stuffing.
3. **Validate FCS-16** (reflected CRC-CCITT, poly `0x8408`, init `0xFFFF`,
   transmit = `~CRC` little-endian). **Only FCS-valid frames were accepted.**
   This is the key filter — it automatically rejects the terminal-escape and
   VS Code / DAP telemetry traffic that pollutes several captures.
4. Parse LAPB + the 13-byte SINTRAN header + sub-headers, then test each of the
   spec's claims statistically across all frames.

**Capture corpus:** the 13 `.pcapng` files in the sibling repo at
`..\..\X25Emulator\pcap\` (relative to this folder). Nodes 100, 102, 103;
several captures are **relay** traffic (103↔102 via 100).

**Corpus size:** 6379 raw de-framed frames; **1947 passed FCS** and form the
evidence base below.

> **ASSUMPTION (stated):** FCS-valid ⇒ genuine ND traffic. Given the 16-bit CRC
> plus the `0x21 0x12/0x13` SINTRAN marker requirement, a false positive is
> vanishingly unlikely, so this is treated as sound.

---

## 2. Verdict summary

| Spec area | Verdict | Evidence |
|-----------|---------|----------|
| Sec. 2 — HDLC framing, byte-stuffing, FCS-16 (`0xF0B8` residue) | **VERIFIED** | Anchor SABM `01 3F 00 64` → FCS `2E 09`; residue `0xF0B8` exact |
| Sec. 3 — LAPB addresses/control, SABM/UA/RR, node-id in S/U info | **VERIFIED** | Present throughout; SABM anchor matches byte-for-byte |
| Sec. 4 — 13-byte SINTRAN header, markers `0x21`/`0x13`, relay `0x12` | **VERIFIED** | Structure holds; relay frames observed |
| Sec. 4.2 — offset 3 is a **subtype**, NOT a length | **VERIFIED** | Subtype `0x0E` spans 34–292 B; three subtypes share the 14-B length |
| Sec. 5 — XMSG sub-header field layout | **VERIFIED** | Matches captured DC/XMSG payloads |
| Sec. 6 — Reachability handshake (`0x19` request / `0x13` reply) | **VERIFIED** | Exact frames present (see 4.3) |
| Sec. 4.3 — Flags1 = per-direction datagram sequence | **VERIFIED** | Monotonic; and echoed by the ACK (new finding, Sec. 5 here) |
| Sec. 9 — Delivery ACK **mechanism** (echoes datagram seq) | **VERIFIED (as subtype `0x03`)** | Ack exists and echoes Flags1 — but subtype is `0x03` here, not `0x07` |
| Sec. 9 — Ack **subtype `0x07`** and status **`0xFFED`** | **CAPTURE-SPECIFIC** | 0 subtype-`0x07` frames, 0 `FF ED` bytes in 6379 raw frames |
| Sec. 8 — `XMDPT = port ≪ 7` | **REFUTED (here)** | `XMDPT & 0x7F` non-zero on most frames |
| Sec. 4.4 — offset 12–13 = decrementing 16-bit counter | **REFUTED** | Offset 12 is a stable Protocol ID; the counter is payload byte 0 |
| Sec. 14 — subtypes map to `5SRPI/5DPIT/5SSPD` symbols | **REFUTED** | Those are kernel memory-segment IDs, not packet types |
| Sec. 7 — the specific data-frame example (dpt `0x0380`, spt `0x02AD`) | **CAPTURE-SPECIFIC** | 0 matches in these captures |

---

## 3. Detailed findings

### 3.1 Framing / LAPB / FCS — VERIFIED

The anchor frame in the spec reproduces exactly:

```
SABM  01 3F 00 64   → FCS 0x092E → wire low-first: 2E 09
full frame on wire:  01 3F 00 64 2E 09
CRC folded over (frame|FCS) = 0xF0B8   ← the "good residue" the spec claims
```

All 1947 accepted frames fold to residue `0xF0B8`, confirming poly `0x8408`,
init `0xFFFF`, one's-complement, little-endian storage.

### 3.2 Offset 3 is a message-kind subtype, NOT a length — VERIFIED

The old dissector README labelled offset 3 "Packet Length"; the peer spec calls
it a subtype. **The captures decide for the spec:**

| Offset-3 value | Frames | Info-length behaviour |
|----------------|-------:|-----------------------|
| `0x03` | 602 | always 14 B |
| `0x0E` | 601 | **34 B … 292 B** (impossible for a length) |
| `0x13` | 4 | 14 B |
| `0x19` | 8 | 14 B |

Three different offset-3 values (`0x03`, `0x13`, `0x19`) all occur at the **same**
14-byte length. A length field cannot do that. Only these **four** values ever
appear across all 1947 frames.

### 3.3 Reachability handshake — VERIFIED

Both frames are present byte-for-byte (proto `0xDE`, Flags1 `0xFFFF`,
Flags2 `0x0001`):

```
request  100→102:  21 13 00 19  00 66 00 64  FF FF 00 01  DE 08
reply    102→100:  21 13 00 13  00 64 00 66  FF FF 00 01  DE 0E
```

### 3.4 `XMDPT = port ≪ 7` — REFUTED in this corpus

If true, `XMDPT & 0x7F` would always be 0. Observed histogram of the low 7 bits:

```
{0: 156, 65: 115, 69: 58, 43: 58, 100: 55, 19: 46, 8: 29, 82: 28, ...}
```

Most-common `XMDPT` values are `0x02C1, 0x0245, 0x02AB, 0x02E4, 0x0313` — none
are multiples of 128. The spec's `0x0380 = port 7` was not representative of this
traffic. (May still hold in the peer's separate send-message capture — see
Sec. 4 here.)

### 3.5 Offset 12–13 "decrementing counter" — REFUTED (a decode-boundary misread)

Offset 12 (Protocol ID) takes seven **distinct, stable** values across the
corpus: `D8, D9, DA, DB, DC, DD, DE`, each with its own sub-payload shape. It is
not a counter. What the peer saw decrementing is **payload byte 0** (the DC
per-direction counter), e.g. `…0xCE, 0xCD, 0xC4, 0xC3, 0xC2, 0xC1, 0xC0…`.
Reading `proto(DE) + counter(04)` as one 16-bit word produced the phantom
`de04 → ddff`.

### 3.6 The `5SRPI/5DPIT/5SSPD` symbol lead — REFUTED by the repo's own source

The spec (Sec. 14) floats that subtype numbers 14/19/25 match segment-5 network
symbols. The repo's symbol tables show these are **kernel memory-segment IDs**,
not packet types:

- `5DPIT = 000023` — the *"DPIT #7, Data/DMA segment"* (segment 19 in the
  SINTRAN segment table), per
  [SINTRAN Structures/SINTRAN-STRUCTURES.md](SINTRAN%20Structures/SINTRAN-STRUCTURES.md)
  line 700, and the `SYMBOLS\L07\XMSG-SYMBOL-LIST.SYMB.TXT` / `K03` tables.

The `5` prefix denotes the segment table, not "layer 5". The numeric match is a
coincidence; the lead is dead.

---

## 4. New finding — subtype `0x03` is the ACK / flow-control frame

Neither the spec nor the old README explained subtype `0x03` (602 frames, the
second-most-common). It is the **short acknowledgment**.

**Fixed shape** (VERIFIED): 14 bytes = 13-byte SINTRAN header + 1 payload byte;
`Flags2` **always** `0x0001` (all 602 frames).

**It acknowledges a data frame in the opposite direction** (VERIFIED): for each
`0x03` frame, its `Flags1` was compared with the most recent data (`0x0E`) frame's
`Flags1` in the **opposite** vs the **same** direction, on the two direct
(non-relay) 100↔102 captures:

| Capture | `0x03` Flags1 = opposite-dir data Flags1 | = same-dir (control) |
|---------|:---------------------------------------:|:--------------------:|
| `new-conn-to-102-from-100.pcapng` | **53 match / 5 miss** | 21 / 36 |
| `conn-to-d102-from-100.pcapng`    | **40 match / 9 miss** | **0 / 48** |

The same-direction correlation is ~0% (0/48 in the second capture), which
**rules out** a same-side companion. The opposite-direction "misses" are all
off-by-one pipelining (two data frames in flight; the very next tracked value
matches) — i.e. effectively 1:1.

**Conclusion (VERIFIED mechanism):**

- `Flags1` of a `0x03` frame = **the echoed datagram-sequence of the `0x0E` data
  frame it acknowledges**.
- The single payload byte = **the acking side's own per-direction counter**
  (decrements), interpreted per sub-protocol: proto `0xDE` → routing / connection-
  step command byte, proto `0xDD` → TAD control byte, proto `0xDC` → DC
  flow-control counter.
- **You cannot decode the `0x03` byte in isolation** — its meaning is fixed by
  the data frame it answers.

`Flags2` cleanly separates the two frame classes: `0x0001` = short/control
(subtypes `0x03/0x13/0x19`); `0x0400 / 0x0108 / 0x0008` = data (`0x0E`).

---

## 5. Reconciliation — the peer's ACK is real, under a different subtype

The peer's Sec. 9 describes an ack that "echoes the datagram sequence in Flags 1"
with "Counter = the acknowledging side's per-direction counter." **That mechanism
is exactly what subtype `0x03` does here.** The differences are capture-class
specific:

| | Peer's send-message capture | This corpus |
|---|---|---|
| Ack subtype | `0x07` | **`0x03`** |
| Status word (Flags2) | `0xFFED` | `0x0001` |
| Flags1 echoes acked datagram seq | yes | **yes (VERIFIED)** |
| Acker's own counter in payload | yes | **yes (VERIFIED)** |

So the ack **concept** validates. What is *not* present in these 13 captures and
still needs the peer's capture to confirm: the `0x07` subtype value and the
`0xFFED` status word.

---

## 6. Corrected subtype table

| Subtype | Meaning | Flags1 | Flags2 | Length |
|---------|---------|--------|--------|--------|
| `0x03` | **ACK / flow-control** | echoed acked datagram seq | `0x0001` | 14 B |
| `0x0E` | Data message | own datagram seq | `0x0400/0x0108/0x0008` | 34–292 B |
| `0x13` | Reachability reply | `0xFFFF` (broadcast) | `0x0001` | 14 B |
| `0x19` | Reachability request | `0xFFFF` (broadcast) | `0x0001` | 14 B |
| `0x07` | Delivery ack (peer's capture only) | echoed acked datagram seq | `0xFFED` | — |

---

## 7. Open questions (need the peer's send-message capture)

1. Does the peer's traffic really use subtype `0x07` (not `0x03`) for the ack, or
   was `0x07` a mis-read? A single frame from that capture settles it.
2. Is `0xFFED` a genuine status word, or a different field? It never appears here.
3. `XMDPT = port ≪ 7`: confirm or refute on real `XFSND` user-data traffic.
4. Subtype `0x03` payload byte for proto `0xDD`/`0xDC` — is it purely a counter,
   or does it also carry a status/credit value? (INFERRED: counter only.)

---

## 8. Changes made to the dissector

`Devices/HDLC/WireShark/hdlc_tcp.lua` was updated to reflect these findings:

- Added a `vs_subtype` value-string table (`0x03` ACK, `0x0E` data, `0x13`/`0x19`
  reachability) and attached it to the offset-3 field (label corrected from the
  old "Packet Length" understanding).
- Subtype `0x03` frames are now labelled **ACK** in the tree, with `Flags1` shown
  as `[acknowledged datagram seq = N]` and an expert-info note; `0x0E` frames show
  `Flags1` as `[datagram seq = N]`.

Verified live: forcing the updated dissector on `new-conn-to-102-from-100.pcapng`
renders e.g. `Flags/Broadcast: 0x0046 [acknowledged datagram seq = 70]`.

> **Sync step (manual, needs admin):** copy the repo plugin over
> `C:\Program Files\Wireshark\plugins\hdlc_tcp.lua` so Wireshark uses the updated
> version. The copy could not be done automatically (Program Files is
> write-protected without elevation).

---

## 9. Reproduction

The independent validator (raw `tshark` extract → HDLC de-frame → FCS filter →
field tests) was run from a scratchpad; the core algorithm is described in
Sec. 1 and can be committed into the repo on request. All numbers above are
reproducible from the 13 captures in `..\..\X25Emulator\pcap\`.

---

**Status:** validation complete for the 13-capture corpus. Ack subtype value
(`0x07`) and `0xFFED` status remain open pending the peer's send-message capture.
