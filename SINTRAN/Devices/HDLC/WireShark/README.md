# LAPB / SINTRAN Wireshark dissector (`hdlc_tcp.lua`)

A Wireshark/tshark Lua dissector for SINTRAN III traffic on **both** COSMOS
transports. One file, two protocols:

- **`hdlc_lapb`** — HDLC carried over the `nd100x --hdlc` TCP bridge (ports 10362 /
  10364 and friends). It de-frames byte-stuffed HDLC, checks the FCS, and decodes
  LAPB, the SINTRAN header, the XMSG envelope (seed/epoch/channel with built-in
  validation), the secure ACK, reachability/resync, XROUT routing letters, and the
  full TAD terminal protocol (login ladder, opcodes, ports).
- **`ndlink`** — the COSMOS **Ethernet hub** on TCP **5010** (added 2026-08-11).
  COSMOS does not use LLC1's connectionless service on Ethernet: it carries its own
  sequenced, acknowledged link protocol, and that is what `ndlink` decodes, along
  with the hub's length prefix, the 802.3 header and the LLC header above it. The
  SINTRAN datagram inside is handed to exactly the same code the HDLC path uses, so
  everything above the link layer decodes identically on both transports.

The two link layers never appear in the same stream. LAPB is HDLC only; the ND link
header is Ethernet only.

> **Protocol details live in one place.** This README covers only how to *run*
> the dissector. The wire format it implements is documented in
> **[../../../XMSG/DOC/XMSG-PROTOCOL.md](../../../XMSG/DOC/XMSG-PROTOCOL.md)**
> (framing, LAPB, header, envelope §18.5, ACK §6, ports §7.1, routing §9.1,
> scenarios §18.8) and **[../../../TAD/TAD-Message-Formats.md](../../../TAD/TAD-Message-Formats.md)**
> (TAD opcodes, login ladder, connect-to session). The HDLC hardware/framing
> layer beneath it is in
> [../HDLC-Frame-Format-Reference.md](../HDLC-Frame-Format-Reference.md).

---

## Install — where to copy the file

1. Find your Wireshark Lua plugin directory:
   **Help → About Wireshark → Folders**, then use either:
   - **"Personal Lua Plugins"** — e.g. `C:\Users\<you>\AppData\Roaming\Wireshark\plugins`
     (per-user, no admin needed — RECOMMENDED), or
   - **"Global Lua Plugins"** — e.g. `C:\Program Files\Wireshark\plugins`
     (all users; needs Administrator to write here).

   > Use **only one** of these. If a copy exists in BOTH, Wireshark loads both and
   > the duplicate `Proto` registration aborts the plugin — delete the one you are
   > not using.

2. Copy `hdlc_tcp.lua` (from this directory) into the chosen folder.

3. Load it:
   - If Wireshark is already open: **Analyze → Reload Lua Plugins** (`Ctrl+Shift+L`).
   - Or just **restart Wireshark**.
   - Copying the file while Wireshark is open does **nothing** until you reload —
     Wireshark reads plugins once at startup.

Keep the installed copy in sync with this repo copy whenever the dissector is
updated — both must match. (Quick check: right sizes/hashes; or re-copy after
every `git pull` that touched this file.)

---

## Verify a repo edit WITHOUT installing it (no admin needed)

Loading the repo copy with `-X lua_script:` while an installed copy exists aborts with
`Proto_new: there cannot be two protocols with the same description`. Overriding the
plugin directory to an empty folder stops the installed copy loading, so the repo copy
is the only one and can be tested directly:

```bash
mkdir -p /tmp/noplugins
WIRESHARK_PLUGIN_DIR=/tmp/noplugins tshark \
  -X lua_script:"<repo path>/hdlc_tcp.lua" -r capture.pcapng -Y hdlc_lapb -V
```

(`WIRESHARK_CONFIG_DIR` does NOT work for this - it only moves the personal config
directory, and the installed copy lives in the global one.)

The acceptance check before installing: run it over several captures and require
**zero Lua errors** and **no NEW expert warnings** versus the installed copy. Count
them with `grep -c "Expert Info"` on both and compare - the numbers must match.

Two things make that comparison lie, so check them first:

- **The installed copy may be old.** Compare against `git show HEAD:<path>` rather
  than against whatever is in the plugins folder - the installed copy here was found
  to be several revisions behind, which made every count look like a regression.
- **Compare like for like when a change adds a transport.** Adding the `ndlink` path
  makes frames decode that previously decoded as nothing, so their expert items are
  new by definition. Filter them out to check the HDLC path alone:
  `-Y "_ws.expert && hdlc_lapb && !ndlink"`.

## Use — the display filter you type at the top of the window

The `nd100x --hdlc` bridge uses a **configurable TCP port**, and Wireshark
captures pick up unrelated background traffic (mDNS, SSDP, NBNS, your own SSH,
etc.) that clutters the list. So after opening a capture, type a **display
filter** in the bar at the top of the Wireshark window and press Enter:

```
tcp.port == 10362
```

If your ND-100 emulator is configured with a second HDLC line (or you run two
ports), OR them together:

```
tcp.port == 10362 || tcp.port == 10364
```

- **`10362` / `10364` are the DEFAULT HDLC ports** — change the numbers to
  whatever you configured `nd100x --hdlc` to listen on. These two ports (plus a
  few others) are also pre-bound in the dissector, so their streams auto-decode
  without needing "Decode As".
- To see only the decoded protocol and hide the raw TCP acks, filter on the
  protocol name instead:

  ```
  hdlc_lapb
  ```

  (that is the dissector's protocol name — use it in the filter bar or in
  `tshark -Y hdlc_lapb`).
- Useful field filters once decoded: `hdlc_lapb` (all frames),
  `xmsg` / `tad` / `routing` sub-trees, or specific fields such as
  `tad.sycn`, `sintran.flags1`, `lapb.addr`.

### Command-line (tshark)

```
tshark -r capture.pcapng -Y hdlc_lapb
tshark -r capture.pcapng -Y "tcp.port == 10362" -V
```

If a capture uses a port the dissector doesn't recognise and the stream is not
auto-decoded, force it: `tshark -r cap.pcapng -d tcp.port==<port>,hdlc_lapb`.

---

## Troubleshooting

- **"Nothing is decoded / frames show as TCP."** In order:
  1. **Reload the plugin** (`Ctrl+Shift+L`) or restart Wireshark — the most common
     cause is an edit/copy that Wireshark hasn't picked up yet.
  2. Check the plugin is enabled: **Analyze → Enabled Protocols** (`Ctrl+Shift+E`),
     type `hdlc`, ensure **HDLC_LAPB** is ticked.
  3. Add the display filter `tcp.port == <your HDLC port>` so the HDLC stream
     isn't lost among background traffic.
  4. If one specific stream still won't decode (capture started mid-frame), right-
     click a packet in it → **Decode As…** → set the TCP port to
     **HDLC_LAPB**.
- **A red error bar on reload** means a Lua error — copy the exact text; it names
  the line.
- **Every frame shows FCS BAD:** **Edit → Preferences → Protocols → TCP** →
  enable "Allow subdissector to reassemble TCP streams" and disable TCP checksum
  validation.

## Reading a COSMOS Ethernet hub capture (TCP 5010)

All the lab machines join one hub, so a single loopback capture sees every node:

```powershell
& "C:\Program Files\Wireshark\tshark.exe" -i "\Device\NPF_Loopback" -f "tcp port 5010" -a duration:120 -w out.pcapng
```

Then filter on `ndlink`. Wireshark's own table claims 5010 as IPSICTL, so without
this dissector a hub capture shows nothing useful.

> **Do NOT instead force `-d tcp.port==5010,hdlc_lapb`.** It looks like it works —
> thousands of frames turn into LAPB — but the decode is misaligned and the output
> is noise that reads like data: node numbers come out as `53306 → 1` instead of
> 100/102/103.

Useful fields: `ndlink.kind`, `ndlink.seq`, `ndlink.backlog`, `ndlink.dupof`,
`ndlink.srclink` / `ndlink.dstlink`, `ndlink.sysno`.

**The hub is a BROADCAST hub**, so one frame sent once is forwarded to every other
member and the same bytes appear on several TCP streams. The dissector counts each
frame exactly once by binding every MAC-to-MAC direction to the first TCP stream it
was seen on; the other copies are marked `hub fan-out copy` and carry no backlog
line. Do not count all the copies by hand — that is what made one earlier set of
block-size numbers unusable.

## Validation the dissector performs

**On the HDLC path**, for each data frame it derives the link seed, epoch and
expected channel (spec §18.5) and flags a Wireshark **expert warning** when the
on-wire Protocol-ID, `Flags2`, secure-ACK channel, or the LAPB odd-length address
bit disagree with the model — so a malformed or mis-sequenced frame stands out in
the packet list. A clean capture shows zero expert warnings.

**On the SINTRAN header**, on both transports, it recomputes word 6 (the
ones-complement checksum over the other six words) and warns when it disagrees.

**On the ND link layer** it carries the three checks that came out of the
2026-08-11 investigation, where two nights went into the file-access protocol for a
fault one layer below it. All three are the cheap tells that would have found it in
minutes:

| Check | Warns when | Why it matters |
|---|---|---|
| seven-bit sequence | a data frame's sequence has bit 7 set | the sequence is 7 bits; the highest value in three real captures is `0x7F` and the wrap `0x7F → 0x00` is visible. No real ND emits a `0x80`. |
| send window | more than **4** frames are unacknowledged from one sender | a real ND sends at most four before waiting. A backlog that climbs and never comes down is THE symptom: the peer starts retransmitting everything, and that reads as a pile of application defects one layer up. |
| retransmission | a sender repeats a sequence still unacknowledged | a peer repeating a data frame is never normal — it means it has not seen an acknowledgement. The warning says whether the bytes were byte-for-byte identical. |

The acknowledgement carries the **next expected** sequence, not the one being
acknowledged, and its trailing word is `0000` on every captured acknowledgement from
both machines — there is no credit field, so the window cannot be negotiated. The
two constants live in `hdlc_tcp.lua` as `ND_SEQ_MODULUS` and `ND_SEND_WINDOW`, next
to the measurement that produced them; the same two numbers are pinned in the C#
reference implementation (`SINTRAN/XMSG/SRC/Xmsg.Ethernet/`) and held to the
captures by `NdLinkCaptureConformanceTests`.

**Measure the window on a READ capture, never a listing.** A listing tops out at two
frames — the short acknowledgement and the reply — because it never sends a content
message, and reading only a listing gives a window half the real size.

What this looks like in practice, measured over the shipped captures: every capture
of two real ND machines stays at a backlog of 1–4 with zero retransmissions, while
every capture of a run made before the send window existed shows a backlog of 17 to
125 and dozens of retransmissions. The two are told apart at a glance by one column.

## Field reference

Filter fields are namespaced `lapb.*`, `sintran.*`, `xmsg.*`, `tad.*`,
`routing.*`, `xm.*`, `env.*`, `ack.*`, `reach.*` for the HDLC path and `ndlink.*`
for the Ethernet hub path. See the `ProtoField` definitions at the top of
`hdlc_tcp.lua` (HDLC) and in the ND link section near the bottom; field meanings are
explained in the two protocol documents linked above, and the ND link header field
by field in `../../../XMSG/SRC/Xmsg.Ethernet/NdLinkHeader.cs`.

Test captures: the separate sibling **X25Emulator** repository (`pcap/` directory)
holds both HDLC and hub captures, and `../../../XMSG/DOC/captures/` holds the
machine-to-machine hub captures.

> The captures in `../../../XMSG/DOC/captures/FA-READ-WRITE-2026-08-04/` are **text
> hex dumps, not pcap** — one frame per line as `time length hex`, raw 802.3 frames
> with no hub length prefix. tshark cannot open them directly. To run the dissector
> over them, prefix each frame with its own 2-byte big-endian length and feed the
> result to `text2pcap -t "%H:%M:%S." -T <anyport>,5010`. Doing that reproduces both
> measured constants from the dissector's own output: the highest sequence in all
> three is 127, and the backlog peaks at 2 for `capture-list-files`, **4** for
> `capture-read` and 2 for `capture-write` — the read capture being the only one
> that shows the real window.
