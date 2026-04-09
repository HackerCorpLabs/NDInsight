# LAPB/SINTRAN Wireshark Dissector

Lua plugin for Wireshark that decodes HDLC/LAPB frames carrying SINTRAN III
protocol over TCP. Handles byte-stuffing, CRC-16-CCITT FCS verification, and
full sub-protocol dissection for ROUTING, TAD, DC/DB, PAD and relay frames.

---

## Requirements

- Wireshark 4.x or later (uses Lua 5.4 bitwise operators — `bit32` not required)
- Tested on Windows; works on any platform Wireshark supports

---

## Installation

### Step 1 — Find your personal Lua plugins folder

Open Wireshark, go to **Help → About Wireshark → Folders tab**.  
Look for **Personal Lua Plugins** — it will show the full path, typically:

| OS      | Default path                                               |
|---------|------------------------------------------------------------|
| Windows | `%APPDATA%\Wireshark\plugins\`  (e.g. `C:\Users\<you>\AppData\Roaming\Wireshark\plugins\`) |
| Linux   | `~/.local/lib/wireshark/plugins/`                          |
| macOS   | `~/.local/lib/wireshark/plugins/`                          |

Alternatively, you can install system-wide into Wireshark's global plugins folder
(shown as **Global Lua Plugins** in the same Folders tab), but that requires admin
rights and will be overwritten on Wireshark upgrades.

### Step 2 — Copy the plugin

Copy `hdlc_tcp.lua` into the plugins folder found above.

```
copy hdlc_tcp.lua "%APPDATA%\Wireshark\plugins\"
```

### Step 3 — Enable Lua scripting

Lua is enabled by default. If it has been disabled:

1. Go to **Edit → Preferences → Advanced**
2. Search for `gui.enable_lua`
3. Set to `TRUE`
4. Restart Wireshark

### Step 4 — Load / reload the plugin

After placing the file:

- **Restart Wireshark**, or
- Use **Ctrl+Shift+L** (Analyze → Reload Lua Plugins) to hot-reload without restart

If the plugin loaded successfully you will see `LAPB/SINTRAN over TCP` listed
under **Help → About Wireshark → Plugins tab**.

---

## How the dissector activates

### Automatic — heuristic detection (recommended)

The dissector registers a TCP heuristic. It will **automatically** claim any TCP
stream whose first LAPB frame passes all three checks:

1. Valid HDLC framing (`0x7E` flags, byte-unstuffing succeeds)
2. CRC-16-CCITT FCS is correct
3. SINTRAN marker bytes `0x21 0x12` or `0x21 0x13` present at info offset

No port configuration required. Works regardless of which TCP ports the emulator
uses.

### Manual — Decode As

If the heuristic does not fire (e.g. only ACK/SYN packets visible in the first
segment, or capture starts mid-stream):

1. Right-click any TCP packet in the target stream
2. **Decode As…**
3. In the Current column, select `hdlc_lapb`
4. Click **OK**

To make the mapping permanent for a specific port: in the Decode As dialog, set
the field to **TCP port**, enter the port number, choose `hdlc_lapb`, and save.
Wireshark stores this in your profile.

### Port binding (fallback — currently commented out in the script)

The script contains commented-out static port registrations:

```lua
-- tcp_table:add(10362, lapb_proto)
-- tcp_table:add(10364, lapb_proto)
-- tcp_table:add(24182, lapb_proto)
-- tcp_table:add(17230, lapb_proto)
-- tcp_table:add(17237, lapb_proto)
```

If you want static port binding instead of (or in addition to) the heuristic,
uncomment those lines and reload the plugin.

---

## Filtering

### Show only LAPB/SINTRAN traffic

```
hdlc_lapb
```

> **Note:** Use `hdlc_lapb` — not `hdlc`. The dissector registers under the name
> `hdlc_lapb`. Wireshark's built-in `hdlc` dissector is not active on these TCP
> streams, so filtering on `hdlc` alone will return no results.

### Filter by SINTRAN sub-protocol

```
sintran.proto == 0xDD          -- TAD only
sintran.proto == 0xDE          -- ROUTING only
sintran.proto == 0xDC          -- DC (terminal data)
sintran.proto == 0xDA          -- PAD
```

### Filter by node

```
sintran.src == 100             -- frames from node 100
sintran.dest == 102            -- frames to node 102
sintran.src == 103 && sintran.dest == 100
```

### Filter by LAPB frame type

```
lapb.ctrl == 0x00              -- I-frame N(S)=0
lapb.addr == 0x09              -- specific LAPB address
```

### Filter by routing command

```
routing.cmd == 0x05            -- Propagate-Request
routing.cmd == 0x0C            -- RouteInfo-Exchange
```

### Filter by TAD message type

```
tad.type == 0x01               -- BDAT
tad.type == 0x02               -- RFI
tad.type == 0x14               -- ConnSetup-Step4
```

### Exclude LAPB supervisory frames (show data only)

```
hdlc_lapb && !(lapb.ctrl & 0x01)
```

### Combine with TCP port (if known)

```
hdlc_lapb && tcp.port == 10362
```

---

## Packet list columns (Info field)

The dissector writes a summary line per TCP segment into the **Info** column.
Multiple LAPB frames in one TCP segment are separated by ` | `.

Example Info values:

| Info string | Meaning |
|---|---|
| `I N(S)=0 N(R)=1  100→102  TAD` | I-frame, seq 0, ack 1, SINTRAN node 100→102, TAD sub-protocol |
| `S RR N(R)=2` | Supervisory RR frame, acknowledging up to seq 2 |
| `U SABM  Node 100` | Unnumbered SABM (link setup), SINTRAN node 100 |
| `U UA` | Unnumbered Acknowledgement |
| `100→102 ROUTING` | SINTRAN I-frame carrying ROUTING protocol |
| `SINTRAN Relay  [103 → 102  PAD]` | Relay frame (mark2=0x12), node 100 forwarding PAD between 103 and 102 |

---

## Decode tree structure

Each captured TCP segment expands as:

```
LAPB/SINTRAN over TCP
  └─ LAPB Frame
       ├─ Raw Frame (stuffed)          -- original bytes including 0x7E flags
       ├─ Address         0x09
       ├─ Control         0x00
       │    ├─ N(S) Send Seq  0
       │    ├─ Poll/Final     False
       │    └─ N(R) Recv Seq  1
       ├─ SINTRAN  [100 → 102  TAD]   -- or "SINTRAN Relay" for mark2=0x12
       │    ├─ Marker 1    0x21
       │    ├─ Marker 2    0x13
       │    ├─ Packet Type 0x00
       │    ├─ Packet Subtype
       │    ├─ Dest Node   102
       │    ├─ Src Node    100
       │    ├─ Flags/Broadcast
       │    ├─ Version/Type
       │    └─ Protocol ID  TAD (0xDD)
       ├─ TAD Message(s)
       │    ├─ Message Type  BDAT (0x01)
       │    ├─ Data Count    33
       │    └─ ...
       └─ FCS  0x1234  [correct]
```

---

## SINTRAN header

Every LAPB I-frame carries a 13-byte SINTRAN header immediately after the LAPB
address and control bytes:

```
Offset  Size  Field            Notes
──────  ────  ───────────────  ──────────────────────────────────────────────
  0      1    Marker 1         Always 0x21
  1      1    Marker 2         0x13 = normal frame, 0x12 = relay frame
  2      1    Packet Type      Frame type / sequence indicator
  3      1    Packet Length    Length of remaining info (after this byte)
  4      2    Dest Node        Big-endian destination SINTRAN node number
  6      2    Src Node         Big-endian source SINTRAN node number
  8      2    Flags 1          Broadcast / routing flags
 10      2    Flags 2          Version / type flags
 12      1    Protocol ID      Sub-protocol carried in the payload (see below)
```

The 13 bytes are followed by the sub-protocol payload.

**Relay frames (Marker 2 = 0x12):** Node 100 acting as a forwarder inserts an
extra relay header — the Src/Dest in the outer SINTRAN header are the relay
nodes, while the actual endpoint nodes appear inside the relay header payload.
The Info column shows both: `SINTRAN Relay  [103 → 102  PAD]`.

---

## Sub-protocols decoded

| Proto ID | Name    | Description |
|----------|---------|-------------|
| `0xD8`   | D8      | Variant of DC (observed; semantics inferred from pcap) |
| `0xD9`   | D9      | Variant of DC (observed; semantics inferred from pcap) |
| `0xDA`   | PAD     | X.25 PAD virtual circuit data forwarding |
| `0xDB`   | DB      | Terminal data variant (close to DC) |
| `0xDC`   | DC      | Terminal data — carries a 17-byte sub-header (speed, flags, local node/channel, remote node/channel) followed by TAD messages |
| `0xDD`   | TAD     | Terminal Access and Directory — primary session protocol; carries chained TAD messages |
| `0xDE`   | ROUTING | Network routing — propagation, bootstrap, route-info exchange |

### DC sub-header (proto 0xDC)

DC frames embed a 17-byte connection sub-header before the TAD message chain:

```
Offset  Size  Field         Notes
──────  ────  ────────────  ────────────────────────────────────────────────
  0      1    Control 1     Frame control byte
  1      1    Speed         Line speed code
  2      1    Flags         Connection flags
  3      2    Local Node    Big-endian node number of local endpoint
  5      2    Local Chan    Big-endian channel number on local node
  7      2    Remote Node   Big-endian node number of remote endpoint
  9      2    Remote Chan   Big-endian channel number on remote node
 11      6    Extra         Additional connection parameters
```

After the sub-header, the DC payload contains a TAD message chain (same format
as proto 0xDD).

---

## LAPB frame types

| ctrl bits     | Type | Description |
|---------------|------|-------------|
| bit 0 = 0     | I    | Information frame — carries SINTRAN data |
| bits 1:0 = 01 | S    | Supervisory — RR / RNR / REJ |
| bits 1:0 = 11 | U    | Unnumbered — SABM / UA / DM / DISC / FRMR / XID |

**I-frame control byte:** bits 1–3 = N(S) send sequence, bit 4 = P/F,
bits 5–7 = N(R) receive sequence.

**S-frame subtypes** (bits 2–3):

| Value | Name | Description |
|-------|------|-------------|
| 0     | RR   | Receive Ready — acknowledges frames up to N(R) |
| 1     | RNR  | Receive Not Ready — flow control hold |
| 2     | REJ  | Reject — retransmit from N(R) |

**U-frame types:**

| ctrl (masked) | Name  | Description |
|---------------|-------|-------------|
| 0x2F          | SABM  | Set Asynchronous Balanced Mode — link setup |
| 0x63          | UA    | Unnumbered Acknowledgement — confirms SABM/DISC |
| 0x43          | DISC  | Disconnect |
| 0x0F          | DM    | Disconnected Mode |
| 0x87          | FRMR  | Frame Reject — protocol error |
| 0x03          | UI    | Unnumbered Information |
| 0xAF          | XID   | Exchange ID |

---

## TAD protocol

TAD (Terminal Access and Directory) is the primary application-layer protocol
for terminal sessions between SINTRAN nodes. It is carried in SINTRAN frames
with Protocol ID 0xDD, or embedded after the sub-header in DC frames (0xDC).

### TAD message structure

A TAD payload is a chain of back-to-back messages. Each message has a 2-byte
header:

```
Offset  Size  Field   Notes
──────  ────  ──────  ──────────────────────────────────────────────────────
  -1     0–1  Pad     0x00 inserted only if message would land on an odd
                      byte boundary — skip silently, not a message type
   0      1   Type    Opcode (see table below)
   1      1   Count   Payload byte count (0–255); does not include type/count
   2      N   Data    Payload; format depends on opcode
```

The dissector skips 0x00 pad bytes automatically before reading each type byte.

### TAD opcode table

Opcodes verified against SINTRAN III symbol tables K03 / L07 / M06 and NPL source.
Direction: C = Client (terminal / Remote Process), S = Server (Master Process).

| Hex  | Symbol | Dir | Count | Description |
|------|--------|-----|-------|-------------|
| 0x01 | BDAT   | C↔S | 0–255 | Terminal data block. Raw bytes sent to/from the terminal. In 7-bit mode bit 7 is cleared; in 8-bit mode (after 78MOD) all 8 bits are significant. Break on last byte is signalled via REMBYT=-1. |
| 0x02 | RFI    | C→S | 0     | Ready For Input. Flow-control credit — "I have a fresh input buffer; you may send." Sent when input buffer is empty, after rejecting data, or in nowait mode when no data is available. |
| 0x03 | ECKM   | C→S | 1 or 21 | Echo strategy. Byte 0 = strategy code (1–6 predefined, 7 = custom). If strategy = 7, followed by a 20-byte (8-word) echo-character bitmap table where each bit represents one character. |
| 0x04 | BMMX   | C→S | 3 or 23 | Break strategy + max-break. Byte 0 = strategy (1–6, 7=custom, 8–9, 11=user BRKTAB). Bytes 1–2 = 16-bit MaxBreak level. If strategy = 7, followed by a 20-byte break-character bitmap table. |
| 0x08 | ESCA   | S→C | 0     | Escape character received. Server signals that the configured escape character was seen. Triggers an escape response (CERS). |
| 0x09 | DCON   | S→C | 0     | Disconnect indication. Server forces disconnect and cleanup (DSTOTA). |
| 0x0C | TMOD   | C→S | 1     | Terminal mode flags (1 byte). Bit 0=CAPITAL (force uppercase), bit 1=CRDLY (CR delay), bit 2=page-stop, bit 3=LBLOG (logout on carrier loss), bit 4=IESC (inhibit escape), bit 5=8BIT, bit 6=UMOD. |
| 0x0D | TTYP   | C→S | 2     | Terminal type ID. 2-byte identifier stored in CTTYP. |
| 0x0E | CESC   | C→S | 1     | Enable/disable escape processing. 0 = disable, non-zero = enable. Always followed by a CERS (0x21) response from the server. |
| 0x0F | DESC   | C→S | 1     | Define escape character. The byte value becomes the new escape character (stored in CESCP). |
| 0x13 | SYCN   | C↔S | 2    | System control command. 2-byte command word. Auto-sent for command codes 1, 13 (CR), 17 (DC1). |
| 0x14 | USCN   | C↔S | 2    | User control command. 2-byte command word. Always sends and waits for a 7ERRS response. |
| 0x16 | RESE   | C→S | 0     | Reset connection. Resets to initial state; server must respond with RECO. |
| 0x17 | RECO   | S→C | 0     | Reset confirm. Response to RESE. |
| 0x18 | DUMM   | any | 0     | Dummy/filler. No content; used for alignment. Ignored by receiver. |
| 0x1F | OPSV   | C↔S | 3    | OS and TAD protocol version handshake. Byte 0 = SINTRAN OS version, byte 1 = OS sub-version, byte 2 = TAD protocol version. Must be exchanged before optional-feature messages (78MOD, UMOD) are legal. |
| 0x21 | CERS   | S→C | 0     | Escape-control response. Server acknowledges CESC enable/disable. |
| 0x22 | ISRQ   | C→S | 0     | Input size request. Client asks how many characters are available in the server's input buffer. |
| 0x23 | ISRS   | S→C | 2     | Input size response. 2-byte count; bit 15 (0x8000) set if a break is present in the data. |
| 0x24 | NOWT   | C↔S | 1    | Nowait status. 1-byte status flag. |
| 0x25 | TNOW   | C↔S | 1    | Terminate nowait. 1-byte flag. |
| 0x26 | NWRE   | S→C | 0     | Nowait restart. |
| 0x27 | RLOC   | S→C | 0     | Remote/local mode toggle. |
| 0x2A | TREP   | S→C | 2     | Terminal status report. 2-byte status word. |
| 0x2B | UMOD   | C→S | 2     | UMOD strategy word (protocol version ≥ 4 only). Requires both sides to have advertised protocol ≥ 4 via OPSV. |
| 0x2C | 78MOD  | C→S | 2     | 8-bit mode set. Non-zero enables 8-bit data path (sets 58BIT flag). Zero = revert to 7-bit strip. |
| 0xFA | CPCO   | S→C | 4     | Completion code. 4-byte (2-word) completion code: CPC1 high, CPC2 low. |
| 0xFB | ERRS   | S→C | 2     | Error response. 2-byte SINTRAN error code (passed to MFERSP). |
| 0xFE | REJE   | S→C | 1     | Reject. Echoes back the opcode of the message that was rejected. If the rejected message was BDAT, also sends an RFI to unblock the sender. |

> **Observed in pcap but not in symbol tables:** 0x10, 0x11, 0x12, 0xFD — shown
> as `?0x10` etc. in the decode tree. Meaning unknown.

---

## ROUTING protocol

ROUTING frames (Protocol ID 0xDE) carry network routing control between SINTRAN
nodes. They do not contain TAD messages — the payload starts with a 1-byte
command code.

### ROUTING commands

| Cmd  | Name               | Description |
|------|--------------------|-------------|
| 0x00 | TermParam-Step4    | Terminal parameter negotiation — step 4 of 5 (countdown) |
| 0x01 | TermParam-Step3    | Step 3 |
| 0x02 | TermParam-Step2    | Step 2 |
| 0x03 | TermParam-Step1    | Step 1 |
| 0x04 | TermParam-Step0    | Step 0 — final step; connection setup completes |
| 0x05 | Propagate-Request  | Push local routing table to a neighbouring node |
| 0x07 | Bootstrap-Request  | Request routing bootstrap data from neighbour |
| 0x08 | Sync-Request       | Routing synchronisation request |
| 0x0B | Propagate-Response | Acknowledgement of a Propagate-Request |
| 0x0C | RouteInfo-Exchange | Route-info query / response (echo-ACK pattern between neighbours) |
| 0x0D | Bootstrap-Response | Response to Bootstrap-Request — provides initial routing table |
| 0x0E | Sync-Response      | Response to Sync-Request |
| 0x11 | PAD-Resp           | PAD virtual-circuit setup response |
| 0x12 | PAD-Resp           | PAD virtual-circuit setup response (variant) |

---

## FCS errors

If the FCS check fails, the FCS field in the decode tree is marked:

```
FCS  0xABCD  [BAD — expected 0x1234]
```

And an expert info entry (red) is added to the frame. This is useful for
diagnosing TCP segmentation reassembly issues or genuine transmission errors.

---

## TCP segmentation

The dissector handles HDLC frames split across TCP segments using Wireshark's
`desegment_len = DESEGMENT_ONE_MORE_SEGMENT` mechanism. Wireshark will
automatically reassemble segments before calling the dissector when needed.

---

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| Plugin not listed in About → Plugins | Check file is in the correct plugins folder; check Lua is enabled |
| Reload shortcut not working | Full restart of Wireshark |
| Traffic not auto-decoded | Stream may have started before capture — use **Decode As** manually |
| FCS BAD on all frames | Capture may have TCP checksum offloading active — enable **Edit → Preferences → Protocols → TCP → Allow subdissector to reassemble TCP streams** and disable checksum validation |
| Heuristic firing on wrong traffic | Very unlikely (FCS + SINTRAN marker is a very tight fingerprint) — if it happens, re-enable port binding and disable heuristic |

---

## Re-enabling port binding

Edit `hdlc_tcp.lua`, find the section near the bottom and uncomment the lines:

```lua
local tcp_table = DissectorTable.get("tcp.port")
tcp_table:add(10362, lapb_proto)
-- add further ports as needed
```

Reload with Ctrl+Shift+L.
