# LAPB / SINTRAN Wireshark dissector (`hdlc_tcp.lua`)

A Wireshark/tshark Lua dissector for SINTRAN III HDLC traffic carried over the
`nd100x --hdlc` TCP bridge. It de-frames byte-stuffed HDLC, checks the FCS, and
decodes LAPB, the SINTRAN header, the XMSG envelope (seed/epoch/channel with
built-in validation), the secure ACK, reachability/resync, XROUT routing
letters, and the full TAD terminal protocol (login ladder, opcodes, ports).

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

## Validation the dissector performs

For each data frame it derives the link seed, epoch and expected channel
(spec §18.5) and flags a Wireshark **expert warning** when the on-wire
Protocol-ID, `Flags2`, secure-ACK channel, or the LAPB odd-length address bit
disagree with the model — so a malformed or mis-sequenced frame stands out in
the packet list. A clean capture shows zero expert warnings.

## Field reference

Filter fields are namespaced `lapb.*`, `sintran.*`, `xmsg.*`, `tad.*`,
`routing.*`, `xm.*`, `env.*`, `ack.*`, `reach.*`. See the `ProtoField`
definitions at the top of `hdlc_tcp.lua`; field meanings are explained in the two
protocol documents linked above. Test captures live in the separate sibling
**X25Emulator** repository (`pcap/` directory).
