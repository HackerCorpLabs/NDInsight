# TAD connect-to — field analysis (what is solved vs. uncertain)

Analysis of every `connect-to` capture (decoded to fields) to separate the fields whose
values we can reproduce from the ones whose derivation is still unknown. Goal: a live node
that answers `connect-to d103` with a terminal, without crashing 100's XMSG.

Captures analysed (all under `E:\Dev\Ronny\X25Emulator\pcap`, decoded in
`SINTRAN/XMSG/SRC/pcap-decode-report.txt`):

- `conn-to-102-from103-via100.pcapng` — 103 connects to 102 (connect channel **DA**), 102 responds. 102 frames each way.
- `conn-to-d102-from-100.pcapng` — 100 connects to 102 (channel **D9**), 102 responds.
- `new-conn-to-102-from-100.pcapng` — 100 connects to 102 (channel **D9**), **58 frames each direction** (a full session).
- `li-syst-tad-103.pcapng` — 103 connecting outward (asker side only).

---

## 1. SOLVED — the responder MIRRORS the sender  [VERIFIED across captures]

For every data frame the asker sends, the responder replies with a frame that **echoes the
sender's transport envelope** and fills in its own payload. Verified frame-for-frame in
`conn-to-102-from103` (asker 103→102 vs responder 102→103, aligned by Flags1):

| asker Flags1 | asker proto | asker counter | responder proto | responder counter |
|---|---|---|---|---|
| 0x0004 | DA | 0x0D | **DA** | **0x0D** |
| 0x0005 | DA | 0x0C | **DA** | **0x0C** |
| 0x0006 | DD | 0x03 | **DD** | **0x03** |
| 0x0007 | DE | 0x02 | **DE** | **0x02** |
| 0x0008 | DD | 0x01 | **DD** | **0x01** |
| 0x000A | DC | 0xFF | **DC** | **0xFF** |

So these fields are **not chosen by the responder** — they are copied from the sender:

| Field | Rule | Confidence |
|-------|------|-----------|
| Flags1 (datagram seq) | = sender's Flags1 | VERIFIED |
| Protocol ID (channel) | = sender's Protocol ID | VERIFIED |
| Sub-header Counter | = sender's Counter | VERIFIED |
| Dest/Src node, XMDSY/XMDPT/XMSSY/XMSPT | swapped (reply to sender), our port in XMSPT | VERIFIED |
| Role byte | `0x40` in the XROUT/setup phase (accept, port-assign); `0x00` in the data phase (session frames) | VERIFIED |
| XMCSM | `0x04000041` connect, `0x04000000` port-assign/setup, `0x01080000` terminal data, `0x00080000` control | VERIFIED |
| Connect-accept params | `01 02 0000  02 02 000A` — **constant** in all captures (the `000A` is fixed, not derived) | VERIFIED |

**Consequence:** we cannot *initiate* session frames with invented channels/counters — that is
exactly what crashed 100 (XXPER). We must **mirror** each frame 100 sends. And 100 *does* drive
a full session when talking to a real 102 (`new-conn` = 58 frames each way); it stalled for us
only because **we stopped ACKing its frames** (it retransmits the connect/setup instead of
advancing).

---

## 2. UNCERTAIN — fields whose derivation I cannot yet prove

These are the values I am NOT sure how to compute. Observed across captures so a pattern can be
reasoned about.

### 2.1 Frame-flags byte (sub-header offset 3)
Response value sometimes equals the sender's, sometimes not — so it is **not** a pure echo:

| Flags1 | asker frame-flags | responder frame-flags |
|---|---|---|
| 0x0004 | 0x86 | 0x86 |
| 0x0005 | 0x86 | 0x86 |
| 0x0006 | 0x86 | **0x92** |
| 0x0007 | 0x82 | **0x86** |
| 0x0008 | 0x96 | 0x96 |
| 0x0009 | 0x96 | **0x92** |
| 0x000A | 0x96 | 0x96 |

Observed set: `0x82, 0x86, 0x92, 0x96`. Bit view (bit7 always set, bit1 always set):

| value | b7 | b4(0x10) | b2(0x04) | b1(0x02) |
|-------|----|----|----|----|
| 0x82 | 1 | 0 | 0 | 1 |
| 0x86 | 1 | 0 | 1 | 1 |
| 0x92 | 1 | 1 | 0 | 1 |
| 0x96 | 1 | 1 | 1 | 1 |

**Open question:** what do bit 4 (`0x10`) and bit 2 (`0x04`) encode? (message class? a
per-frame toggle? first/last-of-a-group?) This is the field I am least sure of.

### 2.2 Role byte — high nibble
Low nibble is known (4 = asker, 0 = responder). High nibble varies and is unexplained:

| Seen (asker) | Seen (responder) |
|---|---|
| `0xC4`, `0xE4`, `0x84`, `0x94` | `0x40`, `0x00` |

**Open question:** meaning of the high nibble (`C/E/8/9` on the asker, `4/0` on the responder)?
Does it encode the message/opcode class? Our node uses `0x40`/`0x00` and that has been accepted,
so this may not be blocking — but it is not understood.

### 2.3 Session port the responder assigns (port-assign TAD `0x07`)
The responder allocates a session port and advertises it in `07 05 00 00 <sys> <portHi> <portLo>`.
Observed:

| Capture | asker | responder session port | logical (>>7) | low-7 (random?) |
|---|---|---|---|---|
| conn-to-102-from103 | 103 | 0x0313 | 6 | 0x13 |
| conn-to-d102-from-100 | 100 | 0x04C2 | 9 | 0x42 |
| new-conn-to-102-from-100 | 100 | 0x0341 | 6 | 0x41 |

Same asker (100) yields low-7 `0x42` then `0x41` in two sessions → looks per-session (an
incarnation / the magic-number "random part"). **Open question (your hypothesis):** is the
low-7 actually derived from the sender's info (its port/system/magic) rather than random? If so,
what is the formula? (This matters: an invented session port may be why 100 never validated us.)

### 2.4 Port-assign `0x0B` option — second data byte
The port-assign trailer is identical across captures EXCEPT one byte in the `0B 02 03 ??` option:

| Capture | asker | `0x0B` data | session port |
|---|---|---|---|
| conn-to-102-from103 | 103 | `03 00` | 0x0313 |
| conn-to-d102-from-100 | 100 | `03 04` | 0x04C2 |
| new-conn-to-102-from-100 | 100 | `03 02` | 0x0341 |

**Open question:** the second byte is `00 / 04 / 02` — what does it encode? (It does not obviously
track the asker system or port; may track the session port / a link index / a count.)

### 2.5 The counter's absolute base (secondary — we echo, so not blocking)
The sender's own counter runs two linear regimes: XROUT phase `ctr = 0x11 - Flags1`
(f1=4→0x0D, f1=5→0x0C), then session phase `ctr = 0x09 - Flags1` (f1=6→0x03, f1=7→0x02 …), a
`-8` base drop (a `-9` jump with the f1 increment). Because the responder **echoes** the counter
we do not need to originate it — but the base values (`0x11`, `0x09`) are unexplained.
**Open question:** where do the per-session base values come from?

---

## 3. The one flow question that actually blocks us

When 100 connects to a **real** 102 it drives a full session (`new-conn` = 58 frames). When it
connects to **our** node it sends only connect + session-setup, then stalls ("waiting for
feedback from 103" / "Unable to communicate with this TAD"). The most likely cause is that we
**stopped sending the `0x03` delivery ACKs** (so 100 retransmits instead of advancing). Next
experiment (no guessing at content): ACK every 100 frame (subtype `0x03`, echo its Flags1,
Flags2=`0x0001`, echo its Protocol ID, our decrementing counter) and MIRROR each data frame it
sends, and see whether 100 then drives the session the way it does with a real 102.

**Fields to keep in mind while reasoning:** 2.1 frame-flags bits, 2.3 session-port low-7, 2.4 the
`0x0B` byte. If any of these must be derived from the sender (your hypothesis) rather than
free/echoed, that is what to work out.
