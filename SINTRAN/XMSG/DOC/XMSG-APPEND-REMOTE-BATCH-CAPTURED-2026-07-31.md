# APPEND-REMOTE-BATCH captured — it is `*XFTRA`, and parameter 11 is the operation code

**Date:** 2026-07-31
**Capture:** `E:\Dev\Ronny\X25Emulator\pcap\append-remote-batch-102-to-100-2026-07-31.pcapng`
**Machines:** two live RetroCore SINTRAN III K images, node 102 (`D102`) and node 100 (`D100`),
linked by HDLC over TCP (`127.0.0.1:10362`). Both packs are `BIGDISK0-K-*.IMG`.
**Command driven:** COSMOS File Transfer E02 on 102, `APPEND-REMOTE-BATCH` with
`D100(SYSTEM)` / `ARBTEST:SYMB` / `ARBOUT:SYMB`.

This closes the `APPEND-REMOTE-BATCH` item in `XMSG-OPEN-ITEMS-2026-07-06.md`.

---

## 1. Headline result

`APPEND-REMOTE-BATCH` does **not** introduce a new COSMOS server. It is sent to
**`*XFTRA`** — the same server as `TRANSFER-FILE` — as a **single XSLET letter**, using the
same tagged-parameter vocabulary. The only structural differences are the value of
parameter 11 and the presence of a parameter we had never seen.

That makes **parameter 11 the operation selector**, which the earlier `*XFTRA` write-up
recorded as UNKNOWN with the constant value 2:

| Command | p11 |
|---|---|
| `TRANSFER-FILE` | 2 (from `XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md`) |
| `APPEND-REMOTE-BATCH` | **3** (this capture) |

INFERRED (two data points, one per command, each stable): p11 selects which `*XFTRA`
operation the letter is asking for. It is NOT a constant. Anyone building an `*XFTRA`
client must set it per operation rather than copying the 2 from the transfer capture.

---

## 2. The request frame, byte for byte

Frame 1, node 102 to node 100, 106 bytes of TCP payload. Hand-decoded and then confirmed
field-for-field by the project dissector (`SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua`),
which is already installed in the global Wireshark plugin folder on this box.

```
7e 09 ea
   21 13 00 0e 0064 0066 03d3 0080 d9        <- SINTRAN header (13 bytes)
   c1 2100 86 e4 0064 0000 0066 0608
   00800141 0044                             <- XMSG sub-header
   ff 06 2a 58 46 54 52 41                   <- "*XFTRA"
   fe 04 44 31 30 30                         <- "D100"
   f4 06 53 59 53 54 45 4d                   <- "SYSTEM"
   0d 02 0000
   f8 0c 41 52 42 54 45 53 54 3a 53 59 4d 42 <- "ARBTEST:SYMB"
   f7 04 53 59 4d 42                         <- "SYMB"
   0a 02 0400
   0b 02 0003
   f0 0b 41 52 42 4f 55 54 3a 53 59 4d 42 00 <- "ARBOUT:SYMB" + PAD
   80 27 7e                                  <- FCS 0x2780 (correct)
```

Transport fields, all VERIFIED against the formulas in the `xmsg-decode` skill:

| Field | Value | Note |
|---|---|---|
| LAPB address | `0x09` | data, EVEN info length |
| LAPB control | `0xEA` | I-frame, N(S)=5, N(R)=7 |
| Subtype | `0x0E` | Data |
| Dest / Src node | 100 / 102 | |
| Flags1 | `0x03D3` (979) | datagram sequence |
| Flags2 | `0x0080` | `== XMCSM >> 16` |
| Counter | `0xC1` | |
| Frame flags | `0x86` | setup / letters / first-use |
| Role | `0xE4` | XFROU + XFHIP + XFWAK + XFWTF |
| XMDPT | 0 | port 0 = XROUT, i.e. a letter |
| XMSPT | 1544 | port 12, random 8 |
| XMCSM | `0x00800141` | low byte `0x41` = XSLET |
| XMLEN | 68 | |

Seed check passes exactly:
`(Counter + Flags1low + Flags2low) & 0xFF = (0xC1 + 0xD3 + 0x80) & 0xFF = 0x14`,
the known 100-102 link seed.

---

## 3. Parameters

Tag rule (already established): an **integer** parameter `n` is tagged `n`; a **string**
parameter `n` is tagged `256 - n`.

| Tag | Param | Type | Value | Meaning |
|---|---|---|---|---|
| `FF` | 1 | string | `*XFTRA` | server name |
| `FE` | 2 | string | `D100` | remote system name |
| `F4` | 12 | string | `SYSTEM` | remote user name |
| `0D` | 13 | integer | `0x0000` | password — see below |
| `F8` | 8 | string | `ARBTEST:SYMB` | **input** (batch job) file |
| `F7` | 9 | string | `SYMB` | constant, meaning still UNKNOWN |
| `0A` | 10 | integer | `1024` | constant, meaning still UNKNOWN |
| `0B` | 11 | integer | **`3`** | **operation = append-remote-batch** |
| `F0` | 16 | string | `ARBOUT:SYMB` | **output file — parameter never seen before** |

Notes:

- **Parameter 16 is new.** The transfer capture had no parameter above 13. It carries the
  batch output (listing) file. This is the only genuinely new field in the message.
- **Parameter 8 is "the file", not "the destination file".** The transfer write-up called
  p8 the destination file spec; here the same tag carries the batch *input* file. Its role
  is set by the operation in p11, so a decoder should not hard-code the word
  "destination".
- **p13 arrived here as an INTEGER 0**, where the transfer capture had it as a *string*
  password. I gave no password (`D100(SYSTEM)` with the password field omitted), so the
  program appears to emit integer 0 for an absent password rather than an empty string.
  INFERRED — one observation, and I have not tested a non-empty password on this path.
- p9 (`"SYMB"`) and p10 (1024) are unchanged from the transfer request and remain
  UNKNOWN. The transfer work already proved p9 is NOT the file type.

---

## 4. The word-alignment pad rule, exercised

`ARBOUT:SYMB` is **11 bytes — odd**, and it is the **final** parameter. It is followed by a
single `0x00` pad, and the declared length stays 11 while the message grows by two.

This is the first capture in the corpus where the padding rule fires on a final odd string
in an `*XFTRA` request. A parser that advances by the declared length alone still lands
correctly here because it is the last parameter, but one that computes the message length
from the parameters will come up two bytes short.

---

## 5. What node 100 answered

The exchange is short. After the letter (frame 1) and its ACK (frame 3), node 100 sends
back a reply carrying the ASCII `USER` followed by a long zero run (frame 13), and node 102
then sends a **second letter naming `*FA-USER`** (frame 19).

On the terminal this surfaced as:

```
*** Error in accessing: D100(SYSTEM).ARBTEST:SYMB
Sintran file system error:
NO SUCH FILE NAME
```

So `*XFTRA` on the remote side hands the job to the remote-file-access path (`*FA-USER`)
to open the batch input file, and that is what failed — the file genuinely does not exist
on D100. **The failure is remote, not local**, which is what makes this capture valid: the
request was fully built and fully delivered.

I have NOT decoded frames 13 and 19 beyond their headers. Frame 19's sub-header parses
with `XMDSY == XMSSY == 100`, which I cannot explain and have not chased. Do not treat my
partial read of those two frames as established.

---

## 6. Open: a channel-byte mismatch on this frame

The dissector raises, and my own arithmetic independently reproduces:

```
Protocol ID 0xD9 but the seed model expects 0xDA
```

Working: `baseLow = (seed - Flags2low) & 0xFF = (0x14 - 0x80) & 0xFF = 0x94`;
`epoch = (0x03D3 - 0x94 + 0xFF) >> 8 = 4`; `Channel = 0xDE - 0 - 4 = 0xDA`. The wire says
`0xD9`.

The seed check itself passes exactly, so the frame is not malformed and the parse is not
wrong. Either the epoch expression is off by one near a wrap, or `Flags2 = 0x0080` is a
class this link had not carried before. **UNKNOWN — flagged, not explained.** It is worth
running the envelope conformance scan over this capture plus the older ones before
touching the formula, since the formula currently holds at 753/753 on the existing corpus.

---

## 7. How to reproduce

1. Both machines up (`F:\RC\RonnyTest\HDLC1` = node 100, `HDLC2` = node 102). Node 100
   listens on `10362`; node 102 dials it.
2. Capture: `tshark -i \Device\NPF_Loopback -f "tcp port 10362" -w out.pcapng`.
3. On node 102's terminal (TCP port 9102): ESC, log in `system` with an empty password,
   then `TRANSFER-FILE`, `APPEND-REMOTE-BATCH`, and answer the three prompts.

Two operational traps, both hit while producing this capture:

- **RetroCore hands every new TCP connection to TERMINAL 8 and the SINTRAN session
  survives the disconnect.** Reconnecting mid-program breaks the terminal line: the
  program floods `NO FILE OPEN WITH THIS NUMBER` / `TERMINAL LINE IS NOT CONNECTED`, and
  the line then stops echoing entirely. Drive the whole sequence over ONE connection.
- **After a reconnect the line is dead until an ESC**, and ESC also aborts whatever is
  running (`USER BREAK AT ...`) and returns to the login prompt. So the reliable opening
  move is always: connect, ESC, log in, then do everything in that one connection.

---

## 8. What this changes elsewhere

- `XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md` — p11's "constant 2, meaning
  unknown" should now point here: it is the operation code.
- The `xmsg-decode` skill's `*XFTRA` bullet says the whole specification rides in
  "tagged XROUT parameters 8-13". That upper bound is wrong; parameter 16 exists.
- Any `*XFTRA` client in `Xmsg.Api` must set p11 per operation and must be able to emit
  p16. Not yet checked whether the library models these at all.
