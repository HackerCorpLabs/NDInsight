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

## Sub-protocols decoded

| Proto ID | Name    | Description |
|----------|---------|-------------|
| `0xD8`   | D8      | Variant of DC (observed, semantics inferred) |
| `0xD9`   | D9      | Variant of DC (observed, semantics inferred) |
| `0xDA`   | PAD     | X.25 PAD virtual circuit forwarding |
| `0xDB`   | DB      | Terminal data variant |
| `0xDC`   | DC      | Terminal data with connection sub-header (speed, flags, node/channel) |
| `0xDD`   | TAD     | Terminal Access and Directory — session setup, routing, data |
| `0xDE`   | ROUTING | Network routing — propagation, bootstrap, route-info exchange |

### LAPB frame types

| ctrl bits | Type | Description |
|-----------|------|-------------|
| bit 0 = 0 | I    | Information frame — carries SINTRAN data |
| bits 1:0 = 01 | S | Supervisory — RR / RNR / REJ |
| bits 1:0 = 11 | U | Unnumbered — SABM / UA / DM / DISC / FRMR / XID |

### TAD message types (partial)

| Type | Name | Description |
|------|------|-------------|
| 0x00 | DUMM | Dummy / padding |
| 0x01 | BDAT | Binary data / node identity |
| 0x02 | RFI  | Route/facility information |
| 0x03 | TMOD | Terminal mode |
| 0x04 | TTYP | Terminal type |
| 0x0B | DCON | Disconnect |
| 0x10–0x14 | ConnSetup-Step0–4 | Connection handshake steps |

Any TAD message with `count == 0x21` (33 bytes) carries a full connection/routing
block: speed, flags, local node/channel, remote node/channel, and session parameters.

### ROUTING commands (partial)

| Cmd  | Name | Description |
|------|------|-------------|
| 0x00–0x04 | TermParam-Step4–0 | Terminal parameter negotiation countdown |
| 0x05 | Propagate-Request | Push routing table to neighbour |
| 0x0B | Propagate-Response | ACK of propagation |
| 0x0C | RouteInfo-Exchange | Route-info query/response (echo-ACK pattern) |
| 0x0D | Bootstrap-Response | Routing bootstrap reply |

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
