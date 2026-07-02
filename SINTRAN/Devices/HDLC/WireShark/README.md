# LAPB / SINTRAN Wireshark dissector (`hdlc_tcp.lua`)

A Wireshark/tshark Lua dissector for SINTRAN III HDLC traffic carried over the
`nd100x --hdlc` TCP bridge. It de-frames byte-stuffed HDLC, checks the FCS, and
decodes LAPB, the SINTRAN header, and the XMSG sub-protocols (ROUTING / TAD /
DC / DB / PAD).

> **Protocol details live in one place.** This README covers only how to *run*
> the dissector. The wire format it implements — framing, LAPB, the SINTRAN
> header, packet subtypes (including the `0x03` ACK and `0x0E` data frames), the
> XMSG sub-header, addressing, and every sub-protocol — is documented in
> **[../../../XMSG-PROTOCOL.md](../../../XMSG/DOC/XMSG-PROTOCOL.md)**. The HDLC hardware /
> framing layer beneath it is in
> [../../../HDLC-Frame-Format-Reference.md](../HDLC-Frame-Format-Reference.md).

## Install

1. Find your Wireshark Lua plugin directory: **Help → About Wireshark → Folders →
   "Personal Lua Plugins"** (or the global plugins folder).
2. Copy `hdlc_tcp.lua` (this directory) into it.
3. Reload plugins: **Analyze → Reload Lua Plugins** (or `Ctrl+Shift+L`), or
   restart Wireshark.

Keep the installed copy in sync with the repo copy in this directory whenever the
dissector is updated — both must match.

## Usage notes

- The dissector auto-attaches by a heuristic (0x7E flag + valid FCS + SINTRAN
  `0x21 0x12/0x13` marker) and by port binding. If a stream is **not** auto-
  decoded (e.g. capture started mid-frame), right-click a packet → **Decode
  As…** → set the TCP port to `LAPB/SINTRAN over TCP`.
- If every frame shows **FCS BAD**, TCP checksum offload may be reassembling
  payloads oddly: **Edit → Preferences → Protocols → TCP** → enable "Allow
  subdissector to reassemble TCP streams" and disable checksum validation.
- Test captures live in the separate sibling **X25Emulator** repository
  (`pcap/` directory).

## Field reference

Filter fields are namespaced `lapb.*`, `sintran.*`, `xmsg.*`, `tad.*`,
`routing.*`, `xm.*`. See the `ProtoField` definitions at the top of
`hdlc_tcp.lua`; field meanings are explained in
[../../../XMSG-PROTOCOL.md](../../../XMSG/DOC/XMSG-PROTOCOL.md).
