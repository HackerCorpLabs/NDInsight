-- LAPB / SINTRAN over TCP dissector
-- Handles byte-stuffed HDLC, full LAPB frame parsing, SINTRAN routing/TAD/PAD.
-- Frame format (between 0x7E flags, after unstuffing):
--   addr(1) + ctrl(1) + [info...] + FCS_lo(1) + FCS_hi(1)
--
-- ── Upgraded 2026-07-06 ──────────────────────────────────────────────────────
-- Source-of-truth specs (authoritative for everything decoded here):
--   • SINTRAN/XMSG/DOC/XMSG-PROTOCOL.md   — LAPB (§3), SINTRAN header (§4),
--     reachability (§5.1), ACK closed form (§6), ports (§7.1), XSGSY (§9.1),
--     role/frameFlags (§18.4), UNIFIED ENVELOPE seed model (§18.5), worked
--     scenarios incl. S10 letter shapes and S11 relay (§18.8)
--   • SINTRAN/TAD/TAD-Message-Formats.md  — TAD opcode tables (§2 + §2.1
--     capture-only opcodes), login SYCN ladder (§21), connect-to session (§22)
--
-- Verified model highlights implemented below:
--   • LAPB address bit 0x80 = ODD-info-length marker (0x89 odd / 0x09 even);
--     a mismatch is a real-machine frame-discard bug signature (expert WARN).
--     Address 0x07 observed on some ACK I-frames — meaning unknown, tolerated.
--   • The "Packet Subtype" byte at header offset 3 is a MESSAGE KIND, never a
--     length (the old README "Packet Length" label was wrong).
--   • Every subtype-0x0E data frame carries the seed-model envelope:
--       seed    = (Counter + Flags1 + (Flags2 & 0xFF)) & 0xFF   (per-link const)
--     SUPERSEDED 2026-07-31 — see sintran_hdr_checksum below. The header is
--     SEVEN WORDS and word 6 is a ones-complement checksum over the other six;
--     the "channel" is its high byte and the "Counter" its low byte. There is no
--     channel, no epoch and no seed. The whole baseLow/epoch construction was a
--     curve fit to that checksum, which is why it kept needing scope caveats.
--       w6 == ~ones_complement_sum(w0..w5, 0)
--     VERIFIED 3595/3595 frames offline, every subtype; this dissector validates
--     3591 of them (it skips the 0xFD/0xFE marker family by design).
--   • ACK (0x03) trailing byte: S_ack = (trailing + Flags1 + F2low) & 0xFF
--     = link seed + 0x0B; ack channel = 0xDE − epoch(echoed Flags1).
--   • Reachability trailing byte closed form: request = ((seed−0x0C) − F1adj),
--     reply = request + 6; Flags2 = hop count. Link-start (Flags1 0xFFFF) vs
--     RESYNC (Flags1 echoes an out-of-sequence datagram).
--   • XMLEN is 16-bit: sub-header offset 17 (previously "pad") is the HIGH
--     byte of the user-data length (255-byte chunks prove 0x0101 = 257).
--   • Ports decode as (logical slot << 7) | incarnation; port 0 = XROUT.
--   • Role byte = high byte of the XMSG send-option word (XF* bitfield) —
--     the old fixed asker/responder role-nibble labels are RETIRED (§18.4:
--     bit 0x04 is XFROU "routed", NOT an asker marker).
--   • XSGSY reply records are parameters #1..#4 (the old XMTNO/XMROU/XMTHI/
--     XMTRE labels were a misattribution and stay retired).

local lapb_proto = Proto("hdlc_lapb", "LAPB/SINTRAN over TCP")

-- ── Lua 5.4 bitwise helpers (Wireshark 4.x dropped bit32) ────────────────────
local function band(a, b)   return a & b  end
local function bxor(a, b)   return a ~ b  end
local function lshift(a, n) return a << n end
local function rshift(a, n) return a >> n end

-- @@BEGIN GENERATED FROM DOC/protocols BY generate_lua.py - DO NOT EDIT @@
-- Constant tables read straight out of DOC/protocols. Regenerate with:
--     python DOC/protocols/generate_lua.py
-- and check for staleness with --check. Editing between the markers by
-- hand is pointless - the next run overwrites it.
--
-- Each block gives REG.<name> (the full record, with the status and the
-- plain-English meaning) and REG.<name>_vs (a Wireshark value string, with
-- the status appended whenever it is not MEASURED).
local REG = {}

-- nd_link_frame_kind -- from sintran-wire.json bitfields/nd_link_frame_kind. 6 values.
REG.nd_link_frame_kind = {
    [0xF] = { name = "ConnectionRequest", status = "MEASURED" },
    [0x20] = { name = "Data", status = "MEASURED" },
    [0x3F] = { name = "Acknowledge", status = "MEASURED" },
    [0x60] = { name = "DisconnectRequest60", status = "MEASURED", meaning = "the other disconnect - D100 sends this one repeatedly once it gives up on a conversation" },
    [0x6F] = { name = "DisconnectRequestByNetworkService", status = "MEASURED" },
    [0x70] = { name = "DisconnectConfirm", status = "INFERRED", meaning = "The confirmation of a disconnect request. Read from its position in the exchange - a connection request is ..." },
}
REG.nd_link_frame_kind_vs = {
    [0xF] = "ConnectionRequest",
    [0x20] = "Data",
    [0x3F] = "Acknowledge",
    [0x60] = "DisconnectRequest60",
    [0x6F] = "DisconnectRequestByNetworkService",
    [0x70] = "DisconnectConfirm [INFERRED]",
}

-- xrout_service -- from xrout-services.json services. 33 values.
REG.xrout_service = {
    [0x40] = { name = "XSNUL", status = "INFERRED", meaning = "do nothing - the no-op" },
    [0x41] = { name = "XSLET", status = "MEASURED", meaning = "deliver this letter to a service I only know by NAME. XROUT looks the name up, spends one of that service's..." },
    [0x42] = { name = "XSNAM", status = "MEASURED", meaning = "claim a name for my port, so others can find me by asking for it instead of needing my address" },
    [0x43] = { name = "XSCNM", status = "MEASURED", meaning = "give my name back, so nobody is sent to a port that has closed" },
    [0x44] = { name = "XSGNM", status = "INFERRED", meaning = "tell me the name registered for this address" },
    [0x45] = { name = "XSGNI", status = "INFERRED", meaning = "tell me more about a registered name - who holds it and how much room it has" },
    [0x46] = { name = "XSRME", status = "INFERRED", meaning = "find me the address of a named port on ANOTHER machine. Asked of our own XROUT, which does the asking onwards" },
    [0x47] = { name = "XSGMG", status = "INFERRED", meaning = "give me the address behind this name" },
    [0x48] = { name = "XSCMG", status = "INFERRED", meaning = "forget that address - one XROUT telling another that a port is gone. Privileged, and not something a normal..." },
    [0x49] = { name = "XSDRN", status = "MEASURED", meaning = "record that a remote system exists and what number it has. This is what the DEF-REMOTE operator command doe..." },
    [0x4A] = { name = "XSDMC/XSDSY", status = "INFERRED", meaning = "record how to reach another machine - which link, and how far away it is" },
    [0x4B] = { name = "XSGMC/XSGSY", status = "INFERRED", meaning = "tell me what you know about that machine - can you reach it, and how" },
    [0x4C] = { name = "XSLKI", status = "INFERRED", meaning = "tell me about the links this machine has - what is connected and in what state" },
    [0x4D] = { name = "XSTIN", status = "INFERRED", meaning = "tell me the time, as this machine has it" },
    [0x4E] = { name = "XSTCL", status = "INFERRED", meaning = "Close tracing (privileged)" },
    [0x4F] = { name = "XSTDC", status = "INFERRED", meaning = "Define tracing conditions (privileged)" },
    [0x50] = { name = "XSCRS", status = "INFERRED", meaning = "Create service (name, init no of SP's)" },
    [0x51] = { name = "XSNSP", status = "INFERRED", meaning = "New service point(s) (increment/decrement in SP's)" },
    [0x52] = { name = "XSGIN", status = "INFERRED", meaning = "Get information about name" },
    [0x53] = { name = "XSDLO", status = "INFERRED", meaning = "Define local system (privileged)" },
    [0x54] = { name = "XSLEK", status = "INFERRED", meaning = "Send letter and kick if unavailable (privileged)" },
    [0x55] = { name = "XSNET", status = "INFERRED", meaning = "Start/stop gateway (network server) (privileged)" },
    [0x56] = { name = "XSSCI", status = "INFERRED", meaning = "Set crash information (privileged)" },
    [0x57] = { name = "XSGAT", status = "INFERRED", meaning = "Get/check attribute" },
    [0x58] = { name = "XSDAT", status = "INFERRED", meaning = "Define/remove attribute (privileged)" },
    [0x59] = { name = "XSNSI", status = "INFERRED", meaning = "Get network server information (privileged)" },
    [0x5A] = { name = "XSLIN", status = "INFERRED", meaning = "Get information about a link (privileged)" },
    [0x5B] = { name = "XSPIN", status = "INFERRED", meaning = "Get information about named ports" },
    [0x5C] = { name = "XSLSY", status = "INFERRED", meaning = "Get information about a system (privileged)" },
    [0x5D] = { name = "XSGSU", status = "INFERRED", meaning = "Get info about system utilization (privileged)" },
    [0x5E] = { name = "XSCRM", status = "INFERRED", meaning = "Start/stop Cosmos routing manager (priv - COSROUT only!)" },
    [0x5F] = { name = "XSGLI", status = "INFERRED", meaning = "Get information from link tables (priv - COSROUT only!)" },
    [0x60] = { name = "XSGSG/XSMAX", status = "INFERRED", meaning = "Get info about system generation variables (privileged)" },
}
REG.xrout_service_vs = {
    [0x40] = "XSNUL [INFERRED]",
    [0x41] = "XSLET",
    [0x42] = "XSNAM",
    [0x43] = "XSCNM",
    [0x44] = "XSGNM [INFERRED]",
    [0x45] = "XSGNI [INFERRED]",
    [0x46] = "XSRME [INFERRED]",
    [0x47] = "XSGMG [INFERRED]",
    [0x48] = "XSCMG [INFERRED]",
    [0x49] = "XSDRN",
    [0x4A] = "XSDMC/XSDSY [INFERRED]",
    [0x4B] = "XSGMC/XSGSY [INFERRED]",
    [0x4C] = "XSLKI [INFERRED]",
    [0x4D] = "XSTIN [INFERRED]",
    [0x4E] = "XSTCL [INFERRED]",
    [0x4F] = "XSTDC [INFERRED]",
    [0x50] = "XSCRS [INFERRED]",
    [0x51] = "XSNSP [INFERRED]",
    [0x52] = "XSGIN [INFERRED]",
    [0x53] = "XSDLO [INFERRED]",
    [0x54] = "XSLEK [INFERRED]",
    [0x55] = "XSNET [INFERRED]",
    [0x56] = "XSSCI [INFERRED]",
    [0x57] = "XSGAT [INFERRED]",
    [0x58] = "XSDAT [INFERRED]",
    [0x59] = "XSNSI [INFERRED]",
    [0x5A] = "XSLIN [INFERRED]",
    [0x5B] = "XSPIN [INFERRED]",
    [0x5C] = "XSLSY [INFERRED]",
    [0x5D] = "XSGSU [INFERRED]",
    [0x5E] = "XSCRM [INFERRED]",
    [0x5F] = "XSGLI [INFERRED]",
    [0x60] = "XSGSG/XSMAX [INFERRED]",
}

-- xrout_error -- from xrout-services.json errors. 57 values.
REG.xrout_error = {
    [0x0] = { name = "XRSOK", status = "MEASURED", meaning = "it worked" },
    [0x1] = { name = "XRISN", status = "INFERRED", meaning = "Illegal service number" },
    [0x2] = { name = "XRUNN", status = "MEASURED", meaning = "nobody here has registered that name - either the service is not running, or the name is spelled differently" },
    [0x3] = { name = "XRDDF", status = "INFERRED", meaning = "Another port already has this name" },
    [0x4] = { name = "XRNSP", status = "MEASURED", meaning = "that service is full. Its free-slot count is zero, so XROUT will not forward another letter to it until som..." },
    [0x5] = { name = "XRIPT", status = "INFERRED", meaning = "Illegal parameter type" },
    [0x6] = { name = "XRMMP", status = "INFERRED", meaning = "Missing mandatory parameter" },
    [0x7] = { name = "XRUNM", status = "MEASURED", meaning = "that address means nothing here - the port it named has closed" },
    [0x8] = { name = "XRMTL", status = "INFERRED", meaning = "Too short message or resulting message too long" },
    [0x9] = { name = "XRSMF", status = "INFERRED", meaning = "Standard message format not handled" },
    [0xA] = { name = "XRPRV", status = "INFERRED", meaning = "Caller was not privileged" },
    [0xB] = { name = "XRISY", status = "INFERRED", meaning = "Illegal system number parameter" },
    [0xC] = { name = "XRNRO", status = "INFERRED", meaning = "No access to remote system" },
    [0xD] = { name = "XRIIV", status = "INFERRED", meaning = "Illegal integer value" },
    [0xE] = { name = "XRNEI", status = "INFERRED", meaning = "Cannot define route to a neighbour" },
    [0xF] = { name = "XRNXM", status = "INFERRED", meaning = "Invalid service request - not available to current caller" },
    [0x10] = { name = "XRILN", status = "INFERRED", meaning = "Illegal/Reserved Logical Unit Number (LUN) for link" },
    [0x11] = { name = "XRNXL", status = "INFERRED", meaning = "No more Link Descriptors (XL-blocks) for start-link/netserver" },
    [0x12] = { name = "XRNXD", status = "INFERRED", meaning = "Not enough resources (XD/XF/XM-Blocks) for start-link/netserver" },
    [0x13] = { name = "XRNTR", status = "INFERRED", meaning = "No trace generated (no trace buffer available)'" },
    [0x14] = { name = "XRTRA", status = "INFERRED", meaning = "Trace already active" },
    [0x15] = { name = "XRTRP", status = "INFERRED", meaning = "Trace passive" },
    [0x16] = { name = "XRTFE", status = "INFERRED", meaning = "Trace/dump file open error (see parameter 1)" },
    [0x17] = { name = "XRTRT", status = "INFERRED", meaning = "Trace RT-prog (XTRACE) not found" },
    [0x18] = { name = "XRTIS", status = "INFERRED", meaning = "Illegal trace system number" },
    [0x19] = { name = "XRBLK", status = "INFERRED", meaning = "Bad link - open unsuccessful" },
    [0x1A] = { name = "XRSYD", status = "INFERRED", meaning = "Attempt to redefine local system no" },
    [0x1B] = { name = "XRNLS", status = "INFERRED", meaning = "No local system number defined" },
    [0x1C] = { name = "XRTRE", status = "INFERRED", meaning = "Too many remote names to this system" },
    [0x1D] = { name = "XRRNA", status = "INFERRED", meaning = "Old service calls (below 64) cannot go remote" },
    [0x1E] = { name = "XRBUS", status = "INFERRED", meaning = "Service points busy" },
    [0x1F] = { name = "XRNSE", status = "INFERRED", meaning = "This is not a service port" },
    [0x20] = { name = "XRRPN", status = "INFERRED", meaning = "Remote port statically declared" },
    [0x21] = { name = "XRUKS", status = "INFERRED", meaning = "Unknown remote system name or number" },
    [0x22] = { name = "XRMFL", status = "INFERRED", meaning = "Remote system message table space full" },
    [0x23] = { name = "XRROV", status = "INFERRED", meaning = "Remote task message space used up" },
    [0x24] = { name = "XRRFU", status = "INFERRED", meaning = "Routing table full (too many systems)" },
    [0x25] = { name = "XRNRB", status = "INFERRED", meaning = "No remote batch service available" },
    [0x26] = { name = "XRURT", status = "INFERRED", meaning = "Unknown RT name" },
    [0x27] = { name = "XRSNR", status = "INFERRED", meaning = "This server is not running" },
    [0x28] = { name = "XRRND", status = "INFERRED", meaning = "Netserver: remote system is not defined" },
    [0x29] = { name = "XRNNA", status = "INFERRED", meaning = "Netserver: network not available" },
    [0x2A] = { name = "XRISE", status = "INFERRED", meaning = "Netserver: internal server error" },
    [0x2B] = { name = "XRIRQ", status = "INFERRED", meaning = "Netserver: invalid request" },
    [0x2C] = { name = "XRNGA", status = "INFERRED", meaning = "XMSG not congfigurated with gateway code" },
    [0x2D] = { name = "XRRNL", status = "INFERRED", meaning = "Remote system not on same LAN" },
    [0x2E] = { name = "XRNCO", status = "MEASURED", meaning = "there is no connection to work with" },
    [0x2F] = { name = "XRAMB", status = "INFERRED", meaning = "Ambiguous name" },
    [0x30] = { name = "XRFFU", status = "INFERRED", meaning = "Friend system table full (too many friends)" },
    [0x31] = { name = "XRNTA", status = "INFERRED", meaning = "Netserver: network temporarily not available" },
    [0x32] = { name = "XRMTO", status = "INFERRED", meaning = "Netserver: message too old" },
    [0x33] = { name = "XRCNR", status = "INFERRED", meaning = "COSMOS routing manager is not running" },
    [0x34] = { name = "XRICR", status = "INFERRED", meaning = "COSMOS routing manager is already running" },
    [0x35] = { name = "XRRID", status = "INFERRED", meaning = "Routing information defined (start/stop inhibited)" },
    [0x36] = { name = "XRIRR", status = "INFERRED", meaning = "COSMOS routing manager: invalid request" },
    [0x37] = { name = "XRILX", status = "INFERRED", meaning = "XRILX - Illegal link number parameter. SOURCE DISCREPANCY: present in XMSG-PL-VALUES-L.INCL (\"CONSTANT XRIL..." },
    [0x4240] = { name = "XRXXX", status = "INFERRED", meaning = "Base for XROUT errors: 41100B" },
}
REG.xrout_error_vs = {
    [0x0] = "XRSOK",
    [0x1] = "XRISN [INFERRED]",
    [0x2] = "XRUNN",
    [0x3] = "XRDDF [INFERRED]",
    [0x4] = "XRNSP",
    [0x5] = "XRIPT [INFERRED]",
    [0x6] = "XRMMP [INFERRED]",
    [0x7] = "XRUNM",
    [0x8] = "XRMTL [INFERRED]",
    [0x9] = "XRSMF [INFERRED]",
    [0xA] = "XRPRV [INFERRED]",
    [0xB] = "XRISY [INFERRED]",
    [0xC] = "XRNRO [INFERRED]",
    [0xD] = "XRIIV [INFERRED]",
    [0xE] = "XRNEI [INFERRED]",
    [0xF] = "XRNXM [INFERRED]",
    [0x10] = "XRILN [INFERRED]",
    [0x11] = "XRNXL [INFERRED]",
    [0x12] = "XRNXD [INFERRED]",
    [0x13] = "XRNTR [INFERRED]",
    [0x14] = "XRTRA [INFERRED]",
    [0x15] = "XRTRP [INFERRED]",
    [0x16] = "XRTFE [INFERRED]",
    [0x17] = "XRTRT [INFERRED]",
    [0x18] = "XRTIS [INFERRED]",
    [0x19] = "XRBLK [INFERRED]",
    [0x1A] = "XRSYD [INFERRED]",
    [0x1B] = "XRNLS [INFERRED]",
    [0x1C] = "XRTRE [INFERRED]",
    [0x1D] = "XRRNA [INFERRED]",
    [0x1E] = "XRBUS [INFERRED]",
    [0x1F] = "XRNSE [INFERRED]",
    [0x20] = "XRRPN [INFERRED]",
    [0x21] = "XRUKS [INFERRED]",
    [0x22] = "XRMFL [INFERRED]",
    [0x23] = "XRROV [INFERRED]",
    [0x24] = "XRRFU [INFERRED]",
    [0x25] = "XRNRB [INFERRED]",
    [0x26] = "XRURT [INFERRED]",
    [0x27] = "XRSNR [INFERRED]",
    [0x28] = "XRRND [INFERRED]",
    [0x29] = "XRNNA [INFERRED]",
    [0x2A] = "XRISE [INFERRED]",
    [0x2B] = "XRIRQ [INFERRED]",
    [0x2C] = "XRNGA [INFERRED]",
    [0x2D] = "XRRNL [INFERRED]",
    [0x2E] = "XRNCO",
    [0x2F] = "XRAMB [INFERRED]",
    [0x30] = "XRFFU [INFERRED]",
    [0x31] = "XRNTA [INFERRED]",
    [0x32] = "XRMTO [INFERRED]",
    [0x33] = "XRCNR [INFERRED]",
    [0x34] = "XRICR [INFERRED]",
    [0x35] = "XRRID [INFERRED]",
    [0x36] = "XRIRR [INFERRED]",
    [0x37] = "XRILX [INFERRED]",
    [0x4240] = "XRXXX [INFERRED]",
}

-- xrout_connection_type -- from xrout-services.json connection_types. 5 values.
REG.xrout_connection_type = {
    [0x0] = { name = "Unavailable", status = "MEASURED" },
    [0x1] = { name = "Neighbour", status = "MEASURED", meaning = "the machine is on the same wire - we can talk to it without being told a path" },
    [0x2] = { name = "Via", status = "MEASURED", meaning = "the machine is further away, and another machine passes traffic along for us" },
    [0x3] = { name = "ViaNetworkServer", status = "UNKNOWN" },
    [0x4] = { name = "Local", status = "MEASURED" },
}
REG.xrout_connection_type_vs = {
    [0x0] = "Unavailable",
    [0x1] = "Neighbour",
    [0x2] = "Via",
    [0x3] = "ViaNetworkServer [UNKNOWN]",
    [0x4] = "Local",
}

-- fa_operation -- from fa-qform.json operations. 13 values.
REG.fa_operation = {
    [0x1] = { name = "FileEntryDisconnect", status = "MEASURED", meaning = "let go of a file slot" },
    [0x2] = { name = "ReserveFileEntry", status = "MEASURED", meaning = "claim one of the far machine file slots before doing anything with a file" },
    [0x3] = { name = "ReleaseFileEntry", status = "MEASURED", meaning = "give that slot back when finished" },
    [0x4] = { name = "ChangeFileEntryId", status = "INFERRED", meaning = "renumber a slot" },
    [0x5] = { name = "OpenFile", status = "MEASURED", meaning = "open a file by name, for reading or for writing" },
    [0x6] = { name = "CloseFile", status = "MEASURED", meaning = "close it" },
    [0x7] = { name = "SetBlockSize", status = "MEASURED", meaning = "agree how big each chunk of data will be" },
    [0x8] = { name = "ReadFile", status = "MEASURED", meaning = "send me the contents" },
    [0x9] = { name = "WriteFile", status = "MEASURED", meaning = "here are the contents to store" },
    [0xA] = { name = "CreateFile", status = "MEASURED", meaning = "make a new file with this name and this many pages" },
    [0xB] = { name = "DeleteFile", status = "MEASURED", meaning = "delete a file" },
    [0xC] = { name = "SiiiSpecial", status = "INFERRED", meaning = "a SINTRAN-specific request" },
    [0xD] = { name = "DeviceFunction", status = "UNKNOWN", meaning = "operate on a device rather than a file" },
}
REG.fa_operation_vs = {
    [0x1] = "FileEntryDisconnect",
    [0x2] = "ReserveFileEntry",
    [0x3] = "ReleaseFileEntry",
    [0x4] = "ChangeFileEntryId [INFERRED]",
    [0x5] = "OpenFile",
    [0x6] = "CloseFile",
    [0x7] = "SetBlockSize",
    [0x8] = "ReadFile",
    [0x9] = "WriteFile",
    [0xA] = "CreateFile",
    [0xB] = "DeleteFile",
    [0xC] = "SiiiSpecial [INFERRED]",
    [0xD] = "DeviceFunction [UNKNOWN]",
}

-- fa_status -- from fa-qform.json status_codes. 7 values.
REG.fa_status = {
    [0x0] = { name = "Ok", status = "MEASURED", meaning = "it worked" },
    [0x2E] = { name = "NoSuchFile", status = "MEASURED", meaning = "no file of that name on that machine" },
    [0x30] = { name = "WrongPassword", status = "MEASURED", meaning = "the file is protected and the password did not match" },
    [0x61] = { name = "StoreError", status = "INFERRED", meaning = "the machine could not write it - out of room, or the disc complained" },
    [0x81] = { name = "NotSupported", status = "INFERRED", meaning = "that machine does not offer this operation" },
    [0xC5] = { name = "EndOfDirectory", status = "MEASURED", meaning = "no more files - the end of a listing, not a fault" },
    [0xD3] = { name = "BadRequest", status = "INFERRED", meaning = "the request did not make sense to the far end" },
}
REG.fa_status_vs = {
    [0x0] = "Ok",
    [0x2E] = "NoSuchFile",
    [0x30] = "WrongPassword",
    [0x61] = "StoreError [INFERRED]",
    [0x81] = "NotSupported [INFERRED]",
    [0xC5] = "EndOfDirectory",
    [0xD3] = "BadRequest [INFERRED]",
}

-- fa_message_type -- from fa-qform.json message_types. 6 values.
REG.fa_message_type = {
    [0x781] = { name = "SessionFinishedAlternate", status = "MEASURED" },
    [0x782] = { name = "SessionFinished", status = "MEASURED" },
    [0x7A2] = { name = "ShortAck", status = "MEASURED" },
    [0x7C0] = { name = "Close", status = "MEASURED" },
    [0x7D2] = { name = "ConnectionConfirm", status = "MEASURED" },
    [0x7F0] = { name = "Request", status = "MEASURED" },
}
REG.fa_message_type_vs = {
    [0x781] = "SessionFinishedAlternate",
    [0x782] = "SessionFinished",
    [0x7A2] = "ShortAck",
    [0x7C0] = "Close",
    [0x7D2] = "ConnectionConfirm",
    [0x7F0] = "Request",
}

-- qform_class -- from fa-qform.json qform/classes. 8 values.
REG.qform_class = {
    [0x0] = { name = "Constructed", status = "MEASURED", meaning = "length-delimited, content itself tagged" },
    [0x1] = { name = "Integer", status = "MEASURED" },
    [0x2] = { name = "TypedInteger", status = "MEASURED", meaning = "carries the SINTRAN error number in a rejection - 0x0030 = 48 = wrong password" },
    [0x3] = { name = "ByteString", status = "MEASURED" },
    [0x4] = { name = "Class4", status = "UNKNOWN" },
    [0x5] = { name = "Class5", status = "UNKNOWN" },
    [0x6] = { name = "Class6Unknown", status = "UNKNOWN" },
    [0x7] = { name = "Selector", status = "MEASURED", meaning = "names the field whose value follows" },
}
REG.qform_class_vs = {
    [0x0] = "Constructed",
    [0x1] = "Integer",
    [0x2] = "TypedInteger",
    [0x3] = "ByteString",
    [0x4] = "Class4 [UNKNOWN]",
    [0x5] = "Class5 [UNKNOWN]",
    [0x6] = "Class6Unknown [UNKNOWN]",
    [0x7] = "Selector",
}

-- xmcsm_service -- from tad-wire.json control_services. 7 values.
REG.xmcsm_service = {
    [0x60000] = { name = "SessionNotify", status = "INFERRED", meaning = "tell the other side something about the session itself, rather than carrying data" },
    [0x80000] = { name = "BareTadControl", status = "MEASURED", meaning = "bare TAD control - ESCA, DCON, the 0xFD notify" },
    [0x1000100] = { name = "XsgsyReply", status = "MEASURED", meaning = "the answer to that question" },
    [0x100014B] = { name = "XsgsyRequest", status = "MEASURED", meaning = "ask another machine what it knows about a system" },
    [0x1080000] = { name = "TerminalData", status = "MEASURED", meaning = "terminal data phase" },
    [0x4000000] = { name = "SessionSetup", status = "MEASURED", meaning = "session setup control word" },
    [0x4000041] = { name = "XsletLetter", status = "MEASURED", meaning = "XROUT connect letter / setup. Low byte 0x41 is the XROUT service code." },
}
REG.xmcsm_service_vs = {
    [0x60000] = "SessionNotify [INFERRED]",
    [0x80000] = "BareTadControl",
    [0x1000100] = "XsgsyReply",
    [0x100014B] = "XsgsyRequest",
    [0x1080000] = "TerminalData",
    [0x4000000] = "SessionSetup",
    [0x4000041] = "XsletLetter",
}

-- tad_op -- from tad-wire.json operations. 33 values.
REG.tad_op = {
    [0x1] = { name = "Bdat", status = "MEASURED", meaning = "here is some text - the characters going to or from the screen. The workhorse: almost all terminal traffic ..." },
    [0x2] = { name = "Rfi", status = "MEASURED", meaning = "your turn to type - the far end is waiting for input" },
    [0x3] = { name = "Eckm", status = "INFERRED", meaning = "decide who echoes what you type - the terminal itself, or the machine at the far end" },
    [0x4] = { name = "Bmmx", status = "MEASURED", meaning = "BREAK parameters: a break-strategy byte, a 16-bit BRKMAX word, and for strategy 7 a 16-byte break table. It..." },
    [0x8] = { name = "Esca", status = "MEASURED", meaning = "the user pressed the escape key - break out of whatever is running" },
    [0x9] = { name = "Dcon", status = "MEASURED", meaning = "end the session and hang up" },
    [0xB] = { name = "Lun", status = "MEASURED", meaning = "the TAD logical-unit index in the port assignment; the unit the user sees is 768 + this value" },
    [0xC] = { name = "Tmod", status = "MEASURED", meaning = "how the terminal should behave - a line at a time or a character at a time, and similar settings" },
    [0xD] = { name = "Ttyp", status = "MEASURED", meaning = "what kind of terminal this is, so the far end knows what it can draw" },
    [0xE] = { name = "Cesc", status = "MEASURED", meaning = "enable (payload 1) or disable (payload 0) the escape function for this session. Choosing WHICH key means es..." },
    [0xF] = { name = "Desc", status = "INFERRED", meaning = "define what the escape key does" },
    [0x13] = { name = "Sycn", status = "MEASURED", meaning = "the SYSTEM CONTROL word - one 16-bit payload word on a general control channel. Its user-control twin is US..." },
    [0x14] = { name = "Uscn", status = "INFERRED", meaning = "the USER CONTROL word - one 16-bit payload word; the sender then waits for an ERRS response" },
    [0x15] = { name = "Fbsi", status = "INFERRED", meaning = "field/buffer size tag in the port assignment; we emit the two-byte value 01 08 copied from a real capture" },
    [0x16] = { name = "Rese", status = "MEASURED", meaning = "start clean. Sent twice while a session is being set up, which is why it appears in the accept ladder" },
    [0x17] = { name = "Reco", status = "MEASURED", meaning = "reset confirm - the answer to a RESE. Not 'pick up a session again after it was interrupted', which this en..." },
    [0x18] = { name = "Dumm", status = "MEASURED", meaning = "a placeholder that carries nothing. Real machines send one during setup, so we do too" },
    [0x1F] = { name = "Opsv", status = "INFERRED", meaning = "OPSV - OS / protocol version handshake (0x1F)" },
    [0x20] = { name = "Esrs", status = "MEASURED", meaning = "the host answering the terminal ESCA - sent with Rese as the pair that lets the login prompt follow" },
    [0x21] = { name = "Cers", status = "INFERRED", meaning = "CERS - escape / CESC response (0x21). Asker-sent after each host burst / CESC change" },
    [0x22] = { name = "Isrq", status = "INFERRED", meaning = "remote ISIZE request - empty I-field. Asks the far end how many input characters are waiting" },
    [0x23] = { name = "Isrs", status = "INFERRED", meaning = "remote ISIZE response - two data bytes, big-endian character count" },
    [0x24] = { name = "Nowt", status = "INFERRED", meaning = "nowait status - one status byte; the variant chosen when the entry status is zero" },
    [0x25] = { name = "Tnow", status = "INFERRED", meaning = "nowait status - one status byte; the variant chosen when the entry status is non-zero" },
    [0x26] = { name = "Nwre", status = "INFERRED", meaning = "nowait restart - high priority, empty. The receiver bounces it straight back and then restarts its suspende..." },
    [0x27] = { name = "Rloc", status = "INFERRED", meaning = "remote local / rubout for NORD-NET - high priority, empty. Handled in the same branch as ESCA" },
    [0x29] = { name = "Edrs", status = "INFERRED", meaning = "escape response sent when the escape function is DISABLED - high priority, empty. This is the answer to ESC..." },
    [0x2A] = { name = "Trep", status = "INFERRED", meaning = "terminal report status - two data bytes, big-endian. Bit 2 buffer overrun, bit 3 parity error, bit 4 framin..." },
    [0x2B] = { name = "Umod", status = "INFERRED", meaning = "a mode message that appears alongside 78MOD from Release L onwards; meaning not established" },
    [0x2C] = { name = "Mod8", status = "INFERRED", meaning = "8-bit mode negotiation for the terminal line; we neither send nor handle it" },
    [0xFA] = { name = "Cpco", status = "INFERRED", meaning = "completion code - four data bytes, two 16-bit words" },
    [0xFB] = { name = "Errs", status = "INFERRED", meaning = "error response - two data bytes, big-endian; the answer to USCN" },
    [0xFE] = { name = "Reje", status = "INFERRED", meaning = "reject - one data byte, the type of the message being rejected. Three bytes on the wire: FE 01 type" },
}
REG.tad_op_vs = {
    [0x1] = "Bdat",
    [0x2] = "Rfi",
    [0x3] = "Eckm [INFERRED]",
    [0x4] = "Bmmx",
    [0x8] = "Esca",
    [0x9] = "Dcon",
    [0xB] = "Lun",
    [0xC] = "Tmod",
    [0xD] = "Ttyp",
    [0xE] = "Cesc",
    [0xF] = "Desc [INFERRED]",
    [0x13] = "Sycn",
    [0x14] = "Uscn [INFERRED]",
    [0x15] = "Fbsi [INFERRED]",
    [0x16] = "Rese",
    [0x17] = "Reco",
    [0x18] = "Dumm",
    [0x1F] = "Opsv [INFERRED]",
    [0x20] = "Esrs",
    [0x21] = "Cers [INFERRED]",
    [0x22] = "Isrq [INFERRED]",
    [0x23] = "Isrs [INFERRED]",
    [0x24] = "Nowt [INFERRED]",
    [0x25] = "Tnow [INFERRED]",
    [0x26] = "Nwre [INFERRED]",
    [0x27] = "Rloc [INFERRED]",
    [0x29] = "Edrs [INFERRED]",
    [0x2A] = "Trep [INFERRED]",
    [0x2B] = "Umod [INFERRED]",
    [0x2C] = "Mod8 [INFERRED]",
    [0xFA] = "Cpco [INFERRED]",
    [0xFB] = "Errs [INFERRED]",
    [0xFE] = "Reje [INFERRED]",
}

-- tad_error -- from tad-wire.json error_codes. 4 values.
REG.tad_error = {
    [0xCC] = { name = "TER00", status = "INFERRED", meaning = "input completed while a delayed escape action was still pending" },
    [0xCD] = { name = "TER01", status = "INFERRED", meaning = "the message was rejected - the driver sent a REJE and failed its caller with this" },
    [0xCE] = { name = "TER02", status = "INFERRED", meaning = "the TAD is not connected - its port number is zero" },
    [0x4200] = { name = "XKXXX", status = "MEASURED", meaning = "the base a negated XMSG error code is OR-ed onto to make a SINTRAN error number" },
}
REG.tad_error_vs = {
    [0xCC] = "TER00 [INFERRED]",
    [0xCD] = "TER01 [INFERRED]",
    [0xCE] = "TER02 [INFERRED]",
    [0x4200] = "XKXXX",
}

-- chat_kind -- from chat-wire.json message_kinds. 27 values.
REG.chat_kind = {
    [0x0] = { name = "None", status = "MEASURED", meaning = "not a valid message - guards against a zeroed buffer being read as a real one" },
    [0x1] = { name = "Join", status = "MEASURED", meaning = "client to server: let me into the room, and know me by this nickname" },
    [0x2] = { name = "Welcome", status = "MEASURED", meaning = "server to client: you are in. Sent to the caller's own address, learned from the arrived join, so everythin..." },
    [0x3] = { name = "Reject", status = "MEASURED", meaning = "server to client: refused, with a reason" },
    [0x4] = { name = "Say", status = "MEASURED", meaning = "client to server: say this to the room" },
    [0x5] = { name = "Said", status = "MEASURED", meaning = "server to clients: somebody said something" },
    [0x6] = { name = "Leave", status = "MEASURED", meaning = "client to server: I am going" },
    [0x7] = { name = "Joined", status = "MEASURED", meaning = "server to clients: somebody entered the room" },
    [0x8] = { name = "Left", status = "MEASURED", meaning = "server to clients: somebody left the room" },
    [0x9] = { name = "Rename", status = "MEASURED", meaning = "member to server: know me by a different name from now on. A REQUEST, not a statement - it can be refused f..." },
    [0xA] = { name = "Renamed", status = "MEASURED", meaning = "server to room: somebody is now known by a different name. Carries BOTH names, because a client showing a t..." },
    [0xB] = { name = "Who", status = "MEASURED", meaning = "client asks who is in the room; server answers the asker alone with the member names" },
    [0xC] = { name = "Map", status = "PROVED LIVE ON D100", meaning = "a member asks to see the network, and the server's answer. ONE KIND BOTH DIRECTIONS, like Who: the client s..." },
    [0xD] = { name = "Rooms", status = "PROVED LIVE ON D100", meaning = "a member asks which rooms exist, and the server's answer. ONE KIND BOTH DIRECTIONS like Who and Map: the cl..." },
    [0xE] = { name = "Topic", status = "PROVED LIVE ON D100", meaning = "set a room's topic, and the room being told it changed. A ROOM kind - it changes what a room IS called abou..." },
    [0xF] = { name = "AllWho", status = "PROVED LIVE ON D100", meaning = "SERVER TO CLIENT ONLY - the answer to Who when the asker holds NO SEAT. Text is every room and the people i..." },
    [0x10] = { name = "History", status = "PROVED ON D100", meaning = "SERVER TO CLIENT ONLY - one per remembered message, replayed to a joiner alone, oldest first, AFTER the wel..." },
    [0x20] = { name = "AdminStatus", status = "INFERRED", meaning = "CHAT-MON to server: report what you are doing. Arrives on the COMMAND port, not CHAT-LOBBY, so it spends no..." },
    [0x21] = { name = "AdminStatusReply", status = "INFERRED", meaning = "server to CHAT-MON: the answer, as readable text in the text field. Text rather than a struct because the s..." },
    [0x22] = { name = "AdminStop", status = "INFERRED", meaning = "CHAT-MON to server: shut down cleanly. The point is a stop that does not need STOP-TERMINAL, NOT releasing ..." },
    [0x23] = { name = "AdminStartTrunk", status = "BUILT, NOT YET OBSERVED ON A WIRE", meaning = "CHAT-MON to server: trunk to the system named in the TEXT, as decimal digits. The server adds a peer row, g..." },
    [0x24] = { name = "AdminStopTrunk", status = "BUILT, NOT YET OBSERVED ON A WIRE", meaning = "CHAT-MON to server: forget the peer named in the TEXT. Answers AdminStatusReply. Removing a trunk does not ..." },
    [0x25] = { name = "AdminListTrunks", status = "BUILT, NOT YET OBSERVED ON A WIRE", meaning = "CHAT-MON to server: every configured peer and what we believe about it, answered as AdminStatusReply text -..." },
    [0x30] = { name = "TrunkHello", status = "PROVED LIVE ON D100 AND D102", meaning = "SERVER TO SERVER, on the CHAT-TRUNK port. ONE KIND BOTH DIRECTIONS, and the TEXT'S FIRST BYTE says which wa..." },
    [0x31] = { name = "TrunkWho", status = "PROVED LIVE ON D100 AND D102", meaning = "SERVER TO SERVER: who is on you? Empty text - the kind IS the question. Sent on EVERY hello, not only the f..." },
    [0x32] = { name = "TrunkMembers", status = "PROVED LIVE ON D100 AND D102", meaning = "SERVER TO SERVER: the answer to TrunkWho - 'NAME/ROOM NAME/ROOM ...', space between people, slash between a..." },
    [0x33] = { name = "TrunkSaid", status = "PROVED LIVE ON D100 AND D102", meaning = "SERVER TO SERVER: one of my members spoke. NAME field is the speaker as their own machine knows them, unqua..." },
}
REG.chat_kind_vs = {
    [0x0] = "None",
    [0x1] = "Join",
    [0x2] = "Welcome",
    [0x3] = "Reject",
    [0x4] = "Say",
    [0x5] = "Said",
    [0x6] = "Leave",
    [0x7] = "Joined",
    [0x8] = "Left",
    [0x9] = "Rename",
    [0xA] = "Renamed",
    [0xB] = "Who",
    [0xC] = "Map [PROVED LIVE ON D100]",
    [0xD] = "Rooms [PROVED LIVE ON D100]",
    [0xE] = "Topic [PROVED LIVE ON D100]",
    [0xF] = "AllWho [PROVED LIVE ON D100]",
    [0x10] = "History [PROVED ON D100]",
    [0x20] = "AdminStatus [INFERRED]",
    [0x21] = "AdminStatusReply [INFERRED]",
    [0x22] = "AdminStop [INFERRED]",
    [0x23] = "AdminStartTrunk [BUILT, NOT YET OBSERVED ON A WIRE]",
    [0x24] = "AdminStopTrunk [BUILT, NOT YET OBSERVED ON A WIRE]",
    [0x25] = "AdminListTrunks [BUILT, NOT YET OBSERVED ON A WIRE]",
    [0x30] = "TrunkHello [PROVED LIVE ON D100 AND D102]",
    [0x31] = "TrunkWho [PROVED LIVE ON D100 AND D102]",
    [0x32] = "TrunkMembers [PROVED LIVE ON D100 AND D102]",
    [0x33] = "TrunkSaid [PROVED LIVE ON D100 AND D102]",
}

-- Byte values the registry lists as CONSTANTS beside the kind enum
-- (sintran-wire.json nd_link_frame_kind/constants). They are real wire
-- bytes, so a decoder needs them even though the C# enum has no member.
REG.nd_link_frame_kind[0x1F] = { name = "ConnectionConfirm", status = "MEASURED" }
REG.nd_link_frame_kind_vs[0x1F] = "ConnectionConfirm"

-- @@END GENERATED@@

-- ── Value strings ─────────────────────────────────────────────────────────────

-- LAPB address byte (XMSG-PROTOCOL.md §3.1). Bit 0x80 = odd-info-length marker:
-- a real ND machine SILENTLY DISCARDS an odd-length I-frame sent with 0x09
-- (before sequence processing → V(R) freezes, REJ deadlock). 0x07 is observed
-- on a few ACK-carrying I-frames (first of a back-to-back pair) — meaning
-- UNKNOWN; receivers must tolerate it as a data-transfer address.
local vs_addr = {
    [0x01] = "link management (SABM/UA)",
    [0x09] = "data (even info length)",
    [0x89] = "data (ODD info length)",
    [0x07] = "ACK variant (meaning unknown - tolerated)",
}

local vs_stype = {
    [0] = "RR  (Receive Ready)",
    [1] = "RNR (Receive Not Ready)",
    [2] = "REJ (Reject)",
    [3] = "Reserved",
}

local vs_utype = {
    [0x03] = "UI",
    [0x0F] = "DM",
    [0x2F] = "SABM",
    [0x43] = "DISC",
    [0x63] = "UA",
    [0x6F] = "SABME",
    [0x87] = "FRMR",
    [0xAF] = "XID",
}

-- Protocol ID (SINTRAN header offset 12). NOTE (XMSG-PROTOCOL.md §4.3/§18.5):
-- this byte is NOT an independent constant — it is the DERIVED channel
-- 0xDE − (XMCSM class) − epoch. The names below are the practical selector
-- labels; the frame layout is IDENTICAL for all of them (one envelope).
local vs_proto = {
    [0xD8] = "D8",
    [0xD9] = "D9",
    [0xDA] = "PAD",
    [0xDB] = "DB",
    [0xDC] = "DC",
    [0xDD] = "TAD",
    [0xDE] = "ROUTING",
}

-- SINTRAN header offset-3 "Packet Subtype" (message-kind) values.
-- VERIFIED against all X25Emulator pcaps: these values are INDEPENDENT of frame
-- length (subtype 0x0E appears on frames from 34 to 292 bytes) — so offset 3 is
-- a message kind, NOT a length. (The old README label "Packet Length" was
-- wrong; corrected here.)  Subtype 0x07 does not occur in the capture corpus
-- but is emitted live as a network-error/reject notification (spec §4.1.1).
local vs_subtype = {
    [0x03] = "Ack (delivery acknowledgment)",
    [0x0E] = "Data message",
    [0x13] = "ReachReply",
    [0x19] = "ReachRequest",
    [0x07] = "NetworkError (reject notification)",
}

-- Subtype-0x07 Flags2 = a SIGNED 16-bit XE* network-layer error code
-- (XMSG-VALUES-M.SYMB via spec §4.1.1 / §11).
local vs_xe_error = {
    [0xFFDE] = "XENSE (-34, network sequencing error)",
    [0xFFED] = "XEIMA (-19, invalid magic number)",
}

-- TAD opcodes: NPL-table names verified against SINTRAN III symbol tables
-- K03/L07/M06 plus the capture-only opcodes named in the 2026-07-06 symbol-table
-- hunt (TAD-Message-Formats.md §2 and §2.1; candidates marked "cand").
local vs_tad_type = {
    [0x01] = "BDAT",   -- Terminal data block (both directions)
    [0x02] = "RFI",    -- Ready For Input — flow-control credit (host-only on wire)
    [0x03] = "ECKM",   -- Echo strategy: 01 echo-on / FF echo-off(password) / 00 teardown
    [0x04] = "BMMX",   -- Break strategy + max-break [+ optional table]
    [0x06] = "7CORQ",  -- Connect request marker (cand; session-setup chain)
    [0x07] = "7CORS",  -- Connect response: 5-byte payload 00 00 node port16 (assigned terminal port)
    [0x08] = "ESCA",   -- Escape character received (asker→host)
    [0x09] = "DCON",   -- Disconnect indication (both dirs since 2026-07-06 live check)
    [0x0B] = "7LUN",   -- TAD logical unit: LU = 768 + value
    [0x0C] = "TMOD",   -- Terminal mode flags (1 byte)
    [0x0D] = "TTYP",   -- Terminal type ID (16-bit, decimal)
    [0x0E] = "CESC",   -- Enable/disable escape (1 byte)
    [0x0F] = "DESC",   -- Define escape character (1 byte)
    [0x11] = "7PASS",  -- (symbol-table member; not yet seen on this wire)
    [0x13] = "SYCN",   -- System control: login-ladder state (see vs_sycn)
    [0x14] = "USCN",   -- User control command (2 bytes)
    [0x15] = "7FBSI",  -- cand; port-assign data 0108 = terminal class advertisement
    [0x16] = "RESE",   -- Reset connection (host-only on wire)
    [0x17] = "RECO",   -- Reset confirm (asker)
    [0x18] = "DUMM",   -- Dummy/padding filler / keepalive
    [0x19] = "7STRQ",  -- (symbol-table member; not yet seen)
    [0x1A] = "7STRS",  -- (symbol-table member; not yet seen)
    [0x1B] = "7KEYI",  -- cand; session-setup chain, semantics unknown
    [0x1C] = "7BADT",  -- cand; session-setup chain, semantics unknown
    [0x1F] = "OPSV",   -- OS version + TAD protocol version (3 bytes)
    [0x20] = "7ESRS",  -- Escape response (host answer to ESCA)
    [0x21] = "CERS",   -- Escape-control response (asker, after each CESC)
    [0x22] = "ISRQ",   -- Input size request (count=0)
    [0x23] = "ISRS",   -- Input size response (2 bytes; bit15=break)
    [0x24] = "NOWT",   -- Nowait status (1 byte)
    [0x25] = "TNOW",   -- Terminate nowait (1 byte)
    [0x26] = "NWRE",   -- Nowait restart (count=0)
    [0x27] = "RLOC",   -- Remote/local mode toggle (count=0)
    [0x28] = "7IAM",   -- (symbol-table member; not yet seen)
    [0x29] = "7EDRS",  -- Escape response, escape-disabled variant (not yet seen)
    [0x2A] = "TREP",   -- Terminal status report (2 bytes)
    [0x2B] = "UMOD",   -- UMOD strategy (2 bytes, protocol v4+)
    [0x2C] = "78MOD",  -- 8-bit mode set (2 bytes)
    [0xFA] = "CPCO",   -- Completion code (4 bytes)
    [0xFB] = "ERRS",   -- Error response (2-byte SINTRAN error code)
    [0xFC] = "7WHO",   -- (symbol-table member; not yet seen)
    [0xFD] = "7POLL",  -- Poll from the host's server task (0x0006 class)
    [0xFE] = "REJE",   -- Reject — echoes the rejected opcode (1 byte)
    [0xFF] = "EOP",    -- 7EOP chain terminator (FF 00 ends setup-phase chains)
}

-- SYCN login-ladder state values (TAD-Message-Formats.md §21, verified from
-- three captured logins).
local vs_sycn = {
    [0x0002] = "WaitUsername",
    [0x0003] = "UsernameAccepted",
    [0x0006] = "PasswordOK",
    [0x000A] = "LOGGED-IN",
    [0x000B] = "LoggedOut",
    [0x000C] = "ErrorText",
}

-- XMCSM (4-byte control/service word). Structure (spec §9/§18.5):
--   • high 16 bits = the message CLASS word — ALWAYS equal to header Flags2
--   • low byte     = XROUT service code on a request (bit 6 set), or the
--                    XR* return status on a reply (bit 6 clear; 0x00 = XRSOK)
-- The high class byte also anchors the derived channel: 0xDE − (XMCSM>>24) − epoch.
local vs_dc_cmd = {
    [0x0100014B] = "XSGSY request (get routing info)",
    [0x01000100] = "XSGSY reply (XRSOK)",
    [0x04000041] = "XSLET letter (to named server, e.g. *TADADM)",
    [0x04000000] = "session-setup / port-assign (0x0400 class reply)",
    [0x01080000] = "terminal data (0x0108 class)",
    [0x00080000] = "out-of-band control (0x0008 class)",
    [0x00060000] = "host notification (0x0006 class)",
}

-- Role byte (sub-header offset 4) = HIGH byte of the XMSG send-option word
-- (spec §18.4 U2, SOLVED). Decode as flags, not as fixed role labels.
-- ⚠️ Do NOT use bit 0x04 as an "is asker" test: it is XFROU "routed via XROUT",
-- which usually correlates with the asker but the host's 7POLL notification
-- carries role 0x54 (XFROU set). Observed combinations: 0x00 (responder/none),
-- 0x40, 0x54, 0x60, 0x84, 0x94, 0xC4, 0xE4.
local role_bits = {
    { 0x01, "XFTCM" },
    { 0x02, "XFSEC" },
    { 0x04, "XFROU(routed)" },
    { 0x08, "XFFWD" },
    { 0x10, "XFBNC(bounce)" },
    { 0x20, "XFHIP(high-pri)" },
    { 0x40, "XFWAK(wake)" },
    { 0x80, "XFWTF(wait-for-transfer)" },
}

-- Frame-flags byte (sub-header offset 3): raw value shown plus known bits
-- (spec §18.4 U1, mostly solved).
local frameflags_bits = {
    { 0x80, "XFSYS/system-mode(always)" },
    { 0x10, "data-phase" },
    { 0x04, "data-letter(discriminator, partially understood)" },
    { 0x02, "always-set" },
}

-- LI ROUTING trailer records = the reply of XROUT service XSGSY ("get routing
-- info for system N"), in the ND standard-message parameter-block format:
-- each 4-byte record is [param-number][length=2][value-hi][value-lo].
--
-- CORRECTION (2026-07): earlier revisions labelled these records
-- XMTNO/XMROU/XMTHI/XMTRE and decoded params 2 and 4 as bitfields. That was a
-- MISATTRIBUTION. XMTNO..XMTRE are actually the XFRCV *message-type* return
-- codes (1=Normal, 2=Routed, 3=High-priority, 4=Return message) and have nothing
-- to do with these routing records — they merely share the values 1..4. The
-- records are XSGSY reply parameters #1..#4, and parameter 2 (connection type) is
-- a 0..4 ENUM, not a bitfield. Source: COSMOS Programmer Guide ND-60.164 (XSGSY)
-- and XMSG-API.md section 4.3 / XMSG-PROTOCOL.md §9.1.
local vs_xsgsy_param = {
    [1] = "System number (first >= requested; 0 = none)",
    [2] = "Connection type",
    [3] = "Extra info (link idx / system / subaddr)",
    [4] = "Network info (hops + WANs)",
}

-- XSGSY reply parameter 2 — connection-type ENUM (verified: not a bitfield).
local vs_conn_type = {
    [0] = "Unavailable",
    [1] = "Neighbour",
    [2] = "Via-relay",
    [3] = "Via-server",
    [4] = "Local",
}

-- XROUT service codes (XS*), values from the official XMSG constant files
-- xmsg-pl-values-l.incl (version L, 87.01.05) and XMSG-PL-VALUES-M.INCL
-- (version M) — the two agree exactly. The L file's section header states
-- verbatim: "Values in byte 1 of message. Bit 6 is set => service request".
-- On the wire the LOW byte of the 4-byte XMCSM field carries the XROUT service
-- code on a REQUEST (bit 6 = 0x40 set); on a REPLY XROUT overwrites that byte
-- with the return status with bit 6 reset (0x00 = XRSOK, else an XR* error,
-- "Error values returned in byte 1 of return message (Bit 6 reset)"). See
-- XMSG-PROTOCOL.md §9.1 and XMSG-API.md sections 4 / 6.4.
local vs_xs_service = {
    [64] = "XSNUL (null)",              [81] = "XSNSP (new service point)",
    [65] = "XSLET (send letter)",       [82] = "XSGIN (get name info)",
    [66] = "XSNAM (name port)",         [83] = "XSDLO (define local system)",
    [67] = "XSCNM (clear name)",        [84] = "XSLEK (send letter + kick)",
    [68] = "XSGNM (get name by magic)", [85] = "XSNET (start/stop netserver)",
    [69] = "XSGNI (get name)",          [86] = "XSSCI (set crash info)",
    [71] = "XSGMG (magic from name)",   [87] = "XSGAT (get attribute)",
    [73] = "XSDRN (define remote name)",[88] = "XSDAT (define attribute)",
    [74] = "XSDSY (define routing)",    [89] = "XSNSI (netserver info)",
    [75] = "XSGSY (get routing info)",  [90] = "XSLIN (link info)",
    [76] = "XSLKI (start link)",        [91] = "XSPIN (named-port info)",
    [77] = "XSTIN (init tracing)",      [92] = "XSLSY (system info)",
    [78] = "XSTCL (close tracing)",     [93] = "XSGSU (system utilization)",
    [79] = "XSTDC (tracing conditions)",[94] = "XSCRM (routing manager)",
    [80] = "XSCRS (create service)",    [95] = "XSGLI (link-table info)",
                                        [96] = "XSGSG (sysgen variables)",
}

-- Build a "FOO+BAR" label for a bitfield value, or "" if no bits match.
local function bitfield_label(value, bits)
    local parts = {}
    for _, b in ipairs(bits) do
        if (value & b[1]) ~= 0 then parts[#parts + 1] = b[2] end
    end
    if #parts == 0 then return "" end
    return table.concat(parts, "+")
end

-- Port display. VERIFIED 2026-07-26: a wire port field is the LOW WORD of the XMSG
-- magic number (MAGNO), so it is (port number << 7) | random, with the port number
-- 9 bits and 1-based and the random part 7 bits. The matching system field is the
-- magic's high word, so system+port together reassemble a whole 32-bit MAGNO.
-- Carved from the XMSG L03 kernel (ZCRMG at 131055, MFM2P at 126774); see
-- NDInsight SINTRAN/XMSG/DOC/XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md and
-- XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md.
--
-- The random part is drawn by the kernel routine ZRAND, which is NOT random: it is a
-- linear congruential generator whose low 7 bits step as r' = (53*r + 25) mod 128 and
-- cycle through all 128 values. ZRAND redraws 0 and 127, so a minted port never
-- carries either -- a port field showing one of those is a reserved address, not an
-- allocated port. Port 0 is the XROUT well-known sink for letters and direct services.
local ZRAND_A7, ZRAND_C7 = 53, 25

-- Next value the kernel's randomiser actually yields after r (low 7 bits only).
-- ZRAND redraws 0 and 127, so a raw step onto either is skipped: the accepted range
-- is 1..126 and a prediction must not display a value the kernel would never mint.
local function zrand_step(r)
    local n = band(r * ZRAND_A7 + ZRAND_C7, 0x7F)
    while n == 0 or n == 0x7F do
        n = band(n * ZRAND_A7 + ZRAND_C7, 0x7F)
    end
    return n
end

local function port_label(p)
    if p == 0 then return "0 = XROUT (letters/services)" end
    local port, rnd = rshift(p, 7), band(p, 0x7F)
    local note = ""
    if rnd == 0 or rnd == 0x7F then
        note = ", random NOT kernel-mintable"
    end
    return string.format("%d (port %d, random %d, next %d%s)",
                         p, port, rnd, zrand_step(rnd), note)
end

-- ── SINTRAN header checksum ───────────────────────────────────────────────────
-- CARVED 2026-07-31 from the XMSG kernel routine at 137314 (reached from XSDGM
-- via the pointer at 137744; the caller stores the result at 137675 STA ,X 32,
-- which is header word 6 itself):
--
--     T := X + 0o32 ; A := 0 ; X += 0o24        -- 0o24..0o32 = SEVEN words
--     while X <= T: A += mem[X]; A += carry; X += 1     -- end-around carry
--     if A ~= 0: A := ~A                                -- ones complement
--
-- So the SINTRAN header is SEVEN WORDS and word 6 is a ones-complement checksum
-- over the other six, with the checksum field itself counted as zero. On the
-- wire that word is read as two bytes:
--
--     offset 12  = checksum HIGH byte  (long mislabelled "Protocol ID"/"channel")
--     offset 13  = checksum LOW  byte  (long mislabelled "Counter")
--
-- There is NO channel, NO epoch and NO per-link seed. The old
-- 0xDE − class − epoch model was a curve fit to this arithmetic: the "seed" was
-- the contribution of header fields nobody varied, the "epoch" was carry
-- propagation, and a peer "crashing on a wrong channel" is simply rejecting a
-- corrupt header checksum.
--
-- VERIFIED on 3595/3595 frames across the whole capture corpus - every subtype
-- (Ack 1671, Data 1449, transfer 0x0A 226 / 0x0C 226, ReachRequest 10,
-- ReachReply 6, NetworkError 3, and the 0xFD/0xFE family 4), both directions,
-- every link, with no per-subtype special cases.
-- Doc: NDInsight SINTRAN/XMSG/DOC/XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md
local function sintran_hdr_checksum(w0, w1, w2, w3, w4, w5)
    local sum = 0
    local function add(w)
        sum = sum + w
        if sum > 0xFFFF then
            sum = band(sum, 0xFFFF) + 1     -- end-around carry (RADD ADC)
        end
    end
    add(w0); add(w1); add(w2); add(w3); add(w4); add(w5)
    -- The kernel skips the complement when the sum is zero (JAZ over RADD CM1).
    -- No corpus frame hits that, but mirror the code rather than the corpus.
    if sum == 0 then return 0 end
    return band(~sum, 0xFFFF)       -- unary ~ is bitwise NOT (Lua 5.3+), as band/rshift above
end

-- ── ProtoFields ───────────────────────────────────────────────────────────────

local pf = {}

-- LAPB
pf.frame_raw  = ProtoField.bytes ("hdlc.raw",       "Raw Frame (stuffed)")
pf.addr       = ProtoField.uint8 ("lapb.addr",      "Address",           base.HEX, vs_addr)
pf.ctrl       = ProtoField.uint8 ("lapb.ctrl",      "Control",           base.HEX)
pf.ns         = ProtoField.uint8 ("lapb.ns",        "N(S) Send Seq",     base.DEC, nil, 0x0E)
pf.pf_bit     = ProtoField.bool  ("lapb.pf",        "Poll/Final",        8, nil,   0x10)
pf.nr         = ProtoField.uint8 ("lapb.nr",        "N(R) Recv Seq",     base.DEC, nil, 0xE0)
pf.stype      = ProtoField.uint8 ("lapb.stype",     "Supervisory",       base.DEC, vs_stype, 0x0C)
pf.fcs        = ProtoField.uint16("lapb.fcs",       "FCS",               base.HEX)

-- SINTRAN extension on S/U frames: 2-byte big-endian sender node number
-- (SABM/UA/RR carry it — spec §3.3).
pf.node_id    = ProtoField.uint16("lapb.node_id",   "Node ID",           base.DEC)

-- SINTRAN routing header (inside LAPB I-frame info)
pf.snt_mark1  = ProtoField.uint8 ("sintran.mark1",  "Marker 1",          base.HEX)
pf.snt_mark2  = ProtoField.uint8 ("sintran.mark2",  "Marker 2",          base.HEX)
pf.snt_pkt    = ProtoField.uint8 ("sintran.pkt",    "Packet Type",       base.HEX)
-- Abbrev kept as "sintran.len" for backward compatibility with existing filters,
-- but the field is a message-kind subtype (see vs_subtype), not a length.
pf.snt_len    = ProtoField.uint8 ("sintran.len",    "Packet Subtype",    base.HEX, vs_subtype)
pf.snt_dest   = ProtoField.uint16("sintran.dest",   "Dest Node",         base.DEC)
pf.snt_src    = ProtoField.uint16("sintran.src",    "Src Node",          base.DEC)
pf.snt_flags1 = ProtoField.uint16("sintran.flags1", "Flags 1 (datagram seq)", base.HEX)
pf.snt_flags2 = ProtoField.uint16("sintran.flags2", "Flags 2 (class word)",   base.HEX)
pf.snt_proto  = ProtoField.uint8 ("sintran.proto",  "Protocol ID (derived channel)", base.HEX, vs_proto)

-- Trailing bytes, shown as the bytes they are.
--
-- The "derived link seed", "epoch", "expected channel" and "S_ack" fields that
-- used to sit here went with the ACK check on 2026-08-11 - see the note in the
-- subtype-0x03 branch. They were the seed/epoch/channel model, which this file
-- already records as disproved at the top: offset 12 is the checksum high byte,
-- and there is no channel, no epoch and no per-link seed. A field labelled
-- "Expected channel" states that model as fact every time a frame is opened.
pf.ack_trail  = ProtoField.uint8 ("xmsg.acktrail",  "ACK trailing byte", base.HEX)
pf.reach_trail= ProtoField.uint8 ("xmsg.reachtrail","Reachability trailing byte", base.HEX)

-- TAD
pf.tad_type   = ProtoField.uint8 ("tad.type",       "Message Type",      base.HEX, vs_tad_type)
pf.tad_count  = ProtoField.uint8 ("tad.count",       "Data Count",       base.DEC)
pf.tad_data   = ProtoField.bytes ("tad.data",        "Data")
pf.tad_text   = ProtoField.string("tad.text",        "Text")
pf.tad_ctrl   = ProtoField.uint8 ("tad.ctrl",        "Control Byte",     base.HEX)

-- TAD structured fields (per-type decoding)
pf.tad_opsv_osver  = ProtoField.uint8 ("tad.opsv.osver",  "OS Version",       base.DEC)
pf.tad_opsv_ossub  = ProtoField.uint8 ("tad.opsv.ossub",  "OS Sub-Version",   base.DEC)
pf.tad_opsv_proto  = ProtoField.uint8 ("tad.opsv.proto",  "TAD Protocol Ver", base.DEC)
pf.tad_ttyp_id     = ProtoField.uint16("tad.ttyp.id",     "Terminal Type",    base.DEC)
pf.tad_tmod_flags  = ProtoField.uint8 ("tad.tmod.flags",  "Mode Flags",       base.HEX)
pf.tad_cmd_word    = ProtoField.uint16("tad.cmd",          "Command Word",     base.HEX)
pf.tad_sycn        = ProtoField.uint16("tad.sycn",         "SYCN State",       base.HEX, vs_sycn)
pf.tad_errcode     = ProtoField.uint16("tad.errcode",      "Error Code",       base.HEX)
pf.tad_isrs_size   = ProtoField.uint16("tad.isrs.size",    "Input Size",       base.DEC)
pf.tad_trep_status = ProtoField.uint16("tad.trep.status",  "Status",           base.HEX)
pf.tad_lun         = ProtoField.uint8 ("tad.lun",          "TAD Logical Unit index", base.DEC)
pf.tad_cors_port   = ProtoField.uint16("tad.cors.port",    "Assigned Terminal Port", base.DEC)

-- XMSG (XM5) on-wire fields — names and meanings cross-referenced with
-- XMSG-PROTOCOL.md §5/§10 and the L07 XMSG-SYMBOL-LIST symbol table.
-- The wire format is a repacked subset of the 17-word in-kernel XM5 header:
-- only the application-relevant fields (XMDSY/XMDPT/XMSSY/XMSPT/XMCSM/XMLEN
-- + user data) are serialised; kernel-only fields (XMDAB/XMDAW/XMTIM/XMTPT/
-- XMALL/XMSIZ etc.) are dropped before transmission.
pf.dc_ctr1       = ProtoField.uint8 ("dc.ctr1",        "Counter",          base.HEX)
pf.dc_sub_type   = ProtoField.uint8 ("dc.sub_type",    "Sub-Type",         base.HEX)
pf.dc_flags86    = ProtoField.uint8 ("xmsg.flags86", "Frame Flags",        base.HEX)
pf.dc_role       = ProtoField.uint8 ("xmsg.role",   "Role (send-options)", base.HEX)
-- RELABELLED 2026-08-24, same bytes and same filter name. These four bytes at
-- absolute 26-29 are NOT one field: the carved sub-header is 14 bytes, so 26-27 is
-- the whole 16-bit XMCSM (which always equals Flags 2) and 28-29 is the first word
-- of the message BODY. That is why the "service byte" here works - it is the body
-- first word low byte. See the note beside dissect_xmsg_body. The abbrev stays
-- xmsg.xmcsm so existing capture filters keep working.
pf.dc_cmd        = ProtoField.uint32("xmsg.xmcsm",  "XMCSM + body word 0 (26-29)", base.HEX, vs_dc_cmd)
-- RELABELLED 2026-08-24. This word is at absolute 30-31, which is body word 1, and
-- its meaning belongs to whichever application owns the body: a length on an XROUT
-- letter, the CONVERSATION NUMBER on a file-server message. It is not a general
-- XMSG user-data length.
--
-- The comment it replaces read: "XMLEN is 16-bit: sub-header offset 17 (formerly
-- decoded as a pad byte) is the HIGH byte of the user-data length. Proven by
-- 255-byte output chunks: bytes 01 01 = 0x0101 = 257 = 2-byte BDAT header + 255
-- data (TAD spec 22.3)." That reading holds for the TAD family, where body word 1
-- really is the chain length, and it is why the mistake survived - it is kept here
-- because it says what the field means on the traffic it was measured on.
pf.dc_tlen       = ProtoField.uint16("xmsg.xmlen",  "Body word 1 (30-31; a length on TAD/XROUT, the conversation on FA)", base.DEC)
pf.dc_trailer    = ProtoField.bytes ("xmsg.userdata","User Data")
pf.xmsg_dsy      = ProtoField.uint16("xmsg.xmdsy",  "XMDSY (dest system)",  base.DEC)
pf.xmsg_dpt      = ProtoField.uint16("xmsg.xmdpt",  "XMDPT (dest port)",    base.DEC)
pf.xmsg_ssy      = ProtoField.uint16("xmsg.xmssy",  "XMSSY (src system)",   base.DEC)
pf.xmsg_spt      = ProtoField.uint16("xmsg.xmspt",  "XMSPT (src port)",     base.DEC)

-- XROUT letter TLVs (XMCSM 0x04000041 = XSLET; spec §18.8 S10a/S10b)
pf.letter_name   = ProtoField.string("xmsg.letter.name",   "Server Name")
pf.letter_target = ProtoField.string("xmsg.letter.target", "Target System Name")

-- ── The message body at absolute 28 ──────────────────────────────────────────
-- Added 2026-08-24 with the body decoders. The value strings come from the
-- generated REG tables, so a name here is whatever the registry says today and a
-- status that is not MEASURED is shown beside it.
pf.xmsg_body     = ProtoField.bytes ("xmsg.body",       "Message body (absolute 28)")

-- File server (FA). DOC/protocols/fa-qform.json.
pf.fa_body       = ProtoField.bytes ("fa.body",         "File server message")
pf.fa_msgtype    = ProtoField.uint16("fa.msgtype",      "FA message type",  base.HEX, REG.fa_message_type_vs)
pf.fa_conversation = ProtoField.uint16("fa.conversation", "Conversation number", base.DEC)
pf.fa_session    = ProtoField.bytes ("fa.session",      "Session header (counter, zero, token)")
pf.fa_qform      = ProtoField.bytes ("fa.qform",        "QFORM body")
-- Read from the C# codec, not from the registry - see the note beside
-- dissect_fa_body. The field is marked generated so it is plainly a derived value.
pf.fa_operation  = ProtoField.uint16("fa.operation",    "FA operation (from the C# codec, not the registry)",
                                     base.HEX, REG.fa_operation_vs)

-- QFORM items.
pf.qform_item    = ProtoField.bytes ("qform.item",      "QFORM item")
pf.qform_tag     = ProtoField.uint8 ("qform.tag",       "Tag byte",         base.HEX)
pf.qform_class   = ProtoField.uint8 ("qform.class",     "Class",            base.DEC, REG.qform_class_vs)
pf.qform_len     = ProtoField.uint16("qform.len",       "Value length",     base.DEC)
pf.qform_value   = ProtoField.bytes ("qform.value",     "Value")
pf.qform_text    = ProtoField.string("qform.text",      "Text")
pf.qform_end     = ProtoField.uint8 ("qform.end",       "End of stream",    base.HEX)
pf.qform_pad     = ProtoField.uint8 ("qform.pad",       "Even-length pad",  base.HEX)

-- CHAT. DOC/protocols/chat-wire.json. The one protocol here Norsk Data never
-- shipped, so every name is ours.
pf.chat_body     = ProtoField.bytes ("chat.body",       "CHAT message")
pf.chat_kind     = ProtoField.uint8 ("chat.kind",       "Kind",             base.DEC, REG.chat_kind_vs)
pf.chat_namelen  = ProtoField.uint8 ("chat.namelen",    "Nickname length",  base.DEC)
pf.chat_name     = ProtoField.string("chat.name",       "Nickname")
pf.chat_textlen  = ProtoField.uint16("chat.textlen",    "Text length",      base.DEC)
pf.chat_text     = ProtoField.string("chat.text",       "Text")

-- XROUT named from the registry rather than from the hand table further up.
pf.xrout_body    = ProtoField.bytes ("xrout.body",      "XROUT message")
pf.xrout_service = ProtoField.uint8 ("xrout.service",   "XROUT service",    base.HEX, REG.xrout_service_vs)
pf.xrout_status  = ProtoField.uint8 ("xrout.status",    "XROUT return status", base.HEX, REG.xrout_error_vs)

-- XSGSY routing-reply parameter blocks (4 bytes each: param-number + length + value)
pf.xm_param      = ProtoField.uint8 ("xm.param",       "XSGSY Parameter",  base.DEC, vs_xsgsy_param)
pf.xm_value_raw  = ProtoField.uint16("xm.value",       "Value (raw)",      base.HEX)
pf.xm_sysno      = ProtoField.uint16("xm.sysno",       "System Number",    base.DEC)
pf.xm_conntype   = ProtoField.uint16("xm.conntype",    "Connection Type",  base.DEC, vs_conn_type)
pf.xm_extrainfo  = ProtoField.uint16("xm.extrainfo",   "Extra Info",       base.DEC)
pf.xm_netinfo    = ProtoField.uint16("xm.netinfo",     "Network Info (hops+WANs)", base.HEX)

lapb_proto.fields = {
    pf.frame_raw, pf.addr, pf.ctrl,
    pf.ns, pf.pf_bit, pf.nr, pf.stype,
    pf.fcs, pf.node_id,
    pf.snt_mark1, pf.snt_mark2, pf.snt_pkt, pf.snt_len,
    pf.snt_dest, pf.snt_src,
    pf.snt_flags1, pf.snt_flags2, pf.snt_proto,
    pf.ack_trail, pf.reach_trail,
    pf.tad_type, pf.tad_count, pf.tad_data, pf.tad_text, pf.tad_ctrl,
    pf.tad_opsv_osver, pf.tad_opsv_ossub, pf.tad_opsv_proto,
    pf.tad_ttyp_id, pf.tad_tmod_flags, pf.tad_cmd_word, pf.tad_sycn,
    pf.tad_errcode, pf.tad_isrs_size, pf.tad_trep_status,
    pf.tad_lun, pf.tad_cors_port,
    pf.dc_ctr1, pf.dc_sub_type,
    pf.dc_flags86, pf.dc_role, pf.dc_cmd, pf.dc_tlen, pf.dc_trailer,
    pf.xmsg_dsy, pf.xmsg_dpt, pf.xmsg_ssy, pf.xmsg_spt,
    pf.letter_name, pf.letter_target,
    pf.xm_param, pf.xm_value_raw,
    pf.xm_sysno, pf.xm_conntype, pf.xm_extrainfo, pf.xm_netinfo,
    -- The body decoders added 2026-08-24.
    pf.xmsg_body,
    pf.fa_body, pf.fa_msgtype, pf.fa_conversation, pf.fa_session,
    pf.fa_qform, pf.fa_operation,
    pf.qform_item, pf.qform_tag, pf.qform_class, pf.qform_len,
    pf.qform_value, pf.qform_text, pf.qform_end, pf.qform_pad,
    pf.chat_body, pf.chat_kind, pf.chat_namelen, pf.chat_name,
    pf.chat_textlen, pf.chat_text,
    pf.xrout_body, pf.xrout_service, pf.xrout_status,
}

-- ── CRC-16-CCITT ─────────────────────────────────────────────────────────────
-- Reflected CRC-16-CCITT: polynomial 0x8408 (bit-reversal of 0x1021).
-- HDLC feeds bits LSB-first, so the reflected variant must be used.
-- FCS transmitted = ~CRC, stored little-endian (lo byte first).
local function crc16_ccitt(ba, nbytes)
    local crc = 0xFFFF
    for i = 0, nbytes - 1 do
        crc = band(bxor(crc, ba:get_index(i)), 0xFFFF)
        for _ = 1, 8 do
            if band(crc, 0x0001) ~= 0 then
                crc = band(bxor(rshift(crc, 1), 0x8408), 0xFFFF)
            else
                crc = band(rshift(crc, 1), 0xFFFF)
            end
        end
    end
    return crc
end

-- ── Byte unstuffing ───────────────────────────────────────────────────────────
-- 0x7D XX  →  XX ^ 0x20

local function unstuff(tvb)
    local len = tvb:len()
    local out = ByteArray.new()
    out:set_size(len)
    local n, i = 0, 0
    while i < len do
        local b = tvb(i, 1):uint()
        if b == 0x7D then
            i = i + 1
            if i < len then
                out:set_index(n, bxor(tvb(i, 1):uint(), 0x20))
                n = n + 1
            end
        else
            out:set_index(n, b)
            n = n + 1
        end
        i = i + 1
    end
    out:set_size(n)
    return out
end

-- ── SINTRAN header size: 13 bytes ────────────────────────────────────────────
-- mark1(1)+mark2(1)+pkt(1)+subtype(1)+dest_BE(2)+src_BE(2)+flags1(2)+flags2(2)+proto(1)
local SINTRAN_HDR = 13

-- ── TAD dissector ─────────────────────────────────────────────────────────────
-- Parses one or more chained TAD messages starting at tvb offset off.
-- Opcode table verified against SINTRAN K03/L07/M06 symbol tables + the
-- capture-only opcodes of TAD-Message-Formats.md §2.1.
--
-- NOTE the "Block-33" special case of earlier revisions is REMOVED: it was an
-- artifact of feeding the XMSG sub-header (counter + 21 00 marker) into the TAD
-- parser (XMSG-PROTOCOL.md §9.2 correction). Trailers reaching this function
-- are pure TAD chains.

-- Build a printable preview from BDAT data. Asker keystrokes carry EVEN PARITY
-- in bit 7 (e.g. F3 F9 F3 8D = "sys"+CR, but mixed clean/parity bytes occur:
-- 6C 69 2D 66 69 AC AC AC 8D) — so strip bit 7 per byte for display
-- (TAD spec §22.6: hosts MUST strip bit 7).
local function bdat_preview(tvb, off, n)
    local chars = {}
    for i = 0, n - 1 do
        local c = band(tvb(off + i, 1):uint(), 0x7F)
        if c >= 0x20 and c < 0x7F then
            chars[#chars + 1] = string.char(c)
        else
            chars[#chars + 1] = "."
        end
    end
    return table.concat(chars)
end

local function dissect_tad(tvb, pinfo, tree, off)
    local rem = tvb:len() - off

    -- Single leftover byte (malformed or padding)
    if rem == 1 then
        local t = tree:add(lapb_proto, tvb(off, 1), "TAD Control")
        t:add(pf.tad_ctrl, tvb(off, 1))
        pinfo.cols.info:append(string.format(" TAD:ctrl=0x%02X", tvb(off, 1):uint()))
        return
    end

    if rem < 2 then return end

    -- ── Parse chained TAD messages ────────────────────────────────────────────
    local pos = off
    while pos + 1 <= tvb:len() - 1 do
        -- Skip 0x00 bytes between messages. Two distinct things look identical
        -- here (TAD spec §2.1, measured over 753 data frames):
        --   • true word-alignment pads (before RFI/SYCN/CESC after an odd-length
        --     message), and
        --   • the leading zero of the effectively 16-BIT opcodes 0x0003 ECKM /
        --     0x0004 BMMX / 0x0007 7CORS (and possibly 0x000B 7LUN) — those get a
        --     0x00 prefix even when the previous message ended EVEN.
        -- A decoder can safely skip all of them and read the next non-zero byte
        -- as the opcode.
        while pos + 1 <= tvb:len() - 1 and tvb(pos, 1):uint() == 0x00 do
            pos = pos + 1
        end
        if pos + 1 > tvb:len() - 1 then break end

        local mtype  = tvb(pos, 1):uint()
        local mcount = tvb(pos + 1, 1):uint()
        local name   = vs_tad_type[mtype] or string.format("0x%02X", mtype)
        local avail  = math.min(mcount, tvb:len() - (pos + 2))
        local t      = tree:add(lapb_proto, tvb(pos, 2 + avail),
                           string.format("TAD  [%s]", name))
        t:add(pf.tad_type,  tvb(pos,     1))
        t:add(pf.tad_count, tvb(pos + 1, 1))

        -- 0xFF EOP terminates setup-phase chains (FF 00) — stop walking; any
        -- bytes after it are padding.
        if mtype == 0xFF then
            t:append_text("  (chain terminator)")
            pinfo.cols.info:append(" TAD:EOP")
            break
        end

        -- Direction hints per opcode — CAPTURE-VERIFIED wire directions
        -- (TAD spec §22.2), NOT the NPL C/S provenance column (which is
        -- misleading on the wire: RFI/ECKM/BMMX/SYCN/CESC/RESE come from the
        -- HOST; TMOD/TTYP/DESC/ESCA/RECO/CERS from the ASKER).
        local dir = ""
        if     mtype == 0x02 or mtype == 0x03 or mtype == 0x04
            or mtype == 0x07 or mtype == 0x0B or mtype == 0x0E
            or mtype == 0x13 or mtype == 0x15 or mtype == 0x16
            or mtype == 0x20 or mtype == 0xFD then
            dir = " host->asker"
        elseif mtype == 0x06 or mtype == 0x08 or mtype == 0x0C
            or mtype == 0x0D or mtype == 0x0F or mtype == 0x17
            or mtype == 0x1B or mtype == 0x1C or mtype == 0x21 then
            dir = " asker->host"
        end
        t:append_text(dir)

        if avail > 0 then
            local d = pos + 2
            -- ── Per-type structured decoding ──────────────────────────────

            if mtype == 0x01 then                        -- BDAT: terminal data
                local ti = t:add(pf.tad_text, tvb(d, avail))
                -- Printable ASCII preview, bit 7 stripped (even-parity keystrokes)
                local preview = bdat_preview(tvb, d, math.min(avail, 40))
                ti:append_text(string.format('  "%s"%s',
                    preview, avail > 40 and "..." or ""))

            elseif mtype == 0x03 and avail >= 1 then     -- ECKM: echo strategy
                local st = tvb(d, 1):uint()
                local eckm_nm =
                    (st == 0x01 and " (echo ON)") or
                    (st == 0xFF and " (echo OFF - password entry)") or
                    (st == 0x00 and " (echo off / discipline teardown)") or
                    (st == 0x07 and " (custom table follows)") or ""
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [strategy=0x%02X%s]", st, eckm_nm))
                if avail == 21 then
                    t:add(pf.tad_data, tvb(d + 1, 20)):append_text("  [echo table 20B]")
                end

            elseif mtype == 0x04 and avail >= 3 then     -- BMMX: break strategy
                local st  = tvb(d,     1):uint()
                local mx  = tvb(d + 1, 2):uint()
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [strategy=%d%s]", st,
                        st == 7 and " (custom table follows)" or ""))
                t:add(pf.tad_data, tvb(d + 1, 2)):append_text(
                    string.format("  [maxbreak=%d]", mx))
                if avail == 23 then
                    t:add(pf.tad_data, tvb(d + 3, 20)):append_text("  [break table 20B]")
                end

            elseif mtype == 0x07 and avail >= 5 then     -- 7CORS: connect response
                -- Payload = 00 00 <node> <port16 BE>: the ASSIGNED TERMINAL PORT
                -- the host minted for this session (port-assign frame, spec §18.7).
                local node = tvb(d + 2, 1):uint()
                local port = tvb(d + 3, 2):uint()
                t:add(pf.tad_cors_port, tvb(d + 3, 2)):append_text(
                    string.format("  [node %d, port %s]", node, port_label(port)))
                pinfo.cols.info:append(string.format(" 7CORS(port=%d)", port))

            elseif mtype == 0x0B and avail >= 2 then     -- 7LUN: TAD logical unit
                -- data = 03 XX; LU = 768 + XX (verified: XX=01 → who shows 769)
                local lu = tvb(d + 1, 1):uint()
                t:add(pf.tad_lun, tvb(d + 1, 1)):append_text(
                    string.format("  [TAD logical unit = %d (768 + %d)]", 768 + lu, lu))

            elseif mtype == 0x0E and avail >= 1 then     -- CESC: enable/disable escape
                local en = tvb(d, 1):uint()
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    en ~= 0 and "  [escape ENABLED]" or "  [escape DISABLED]")

            elseif mtype == 0x0F and avail >= 1 then     -- DESC: define escape char
                local ch = tvb(d, 1):uint()
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [escape char = 0x%02X%s]", ch,
                        ch < 0x20 and string.format(" (Ctrl-%c)", ch + 0x40) or ""))

            elseif mtype == 0x0C and avail >= 1 then     -- TMOD: terminal mode flags
                local fl = tvb(d, 1):uint()
                local fi = t:add(pf.tad_tmod_flags, tvb(d, 1))
                fi:append_text(string.format("  [%s%s%s%s%s%s%s]",
                    (fl & 0x01) ~= 0 and "CAPITAL " or "",
                    (fl & 0x02) ~= 0 and "CRDLY "   or "",
                    (fl & 0x04) ~= 0 and "SCREEN "  or "",
                    (fl & 0x08) ~= 0 and "LBLOG "   or "",
                    (fl & 0x10) ~= 0 and "IESC "    or "",
                    (fl & 0x20) ~= 0 and "8BIT "    or "",
                    (fl & 0x40) ~= 0 and "UMOD "    or ""))

            elseif mtype == 0x0D and avail >= 2 then     -- TTYP: 16-bit terminal type
                -- Decimal display: wire 0x0068 = terminal type 104; 0 = "not set"
                t:add(pf.tad_ttyp_id, tvb(d, 2))

            elseif mtype == 0x13 and avail >= 2 then     -- SYCN: login-ladder state
                local sv = tvb(d, 2):uint()
                t:add(pf.tad_sycn, tvb(d, 2))
                local nm = vs_sycn[sv]
                if nm then
                    t:append_text("  " .. nm)
                    pinfo.cols.info:append(string.format(" SYCN=%s", nm))
                else
                    pinfo.cols.info:append(string.format(" SYCN=0x%04X", sv))
                end

            elseif mtype == 0x14 and avail >= 2 then     -- USCN: user control
                t:add(pf.tad_cmd_word, tvb(d, 2))

            elseif mtype == 0x15 and avail >= 2 then     -- 7FBSI (candidate)
                local cls = tvb(d, 2):uint()
                t:add(pf.tad_cmd_word, tvb(d, 2)):append_text(
                    cls == 0x0108 and "  [terminal class 0x0108]" or "")

            elseif mtype == 0x1F and avail >= 3 then     -- OPSV: OS + proto version
                t:add(pf.tad_opsv_osver,  tvb(d,     1))
                t:add(pf.tad_opsv_ossub,  tvb(d + 1, 1))
                t:add(pf.tad_opsv_proto,  tvb(d + 2, 1))
                pinfo.cols.info:append(string.format(
                    " OPSV(os=%d.%d proto=%d)",
                    tvb(d,1):uint(), tvb(d+1,1):uint(), tvb(d+2,1):uint()))

            elseif mtype == 0x23 and avail >= 2 then     -- ISRS: input size response
                local sz = tvb(d, 2):uint()
                local si = t:add(pf.tad_isrs_size, tvb(d, 2))
                local brk = (sz & 0x8000) ~= 0
                si:append_text(string.format("  [%d chars%s]",
                    sz & 0x7FFF, brk and ", BREAK present" or ""))

            elseif mtype == 0x24 and avail >= 1 then     -- NOWT: nowait status
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [nowait flag = 0x%02X]", tvb(d, 1):uint()))

            elseif mtype == 0x25 and avail >= 1 then     -- TNOW: terminate nowait
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [terminate flag = 0x%02X]", tvb(d, 1):uint()))

            elseif mtype == 0x2A and avail >= 2 then     -- TREP: terminal status
                local st = tvb(d, 2):uint()
                local si = t:add(pf.tad_trep_status, tvb(d, 2))
                si:append_text(string.format("  [%s%s%s]",
                    (st & 0x04) ~= 0 and "BFUL " or "",
                    (st & 0x08) ~= 0 and "PAER " or "",
                    (st & 0x10) ~= 0 and "FRER " or ""))

            elseif mtype == 0x2B and avail >= 2 then     -- UMOD: UMOD strategy (v4+)
                t:add(pf.tad_cmd_word, tvb(d, 2)):append_text("  [UMOD strategy]")

            elseif mtype == 0x2C and avail >= 2 then     -- 78MOD: 8-bit mode set
                local val = tvb(d, 2):uint()
                t:add(pf.tad_cmd_word, tvb(d, 2)):append_text(
                    val ~= 0 and "  [8-bit mode ON]" or "  [7-bit strip]")

            elseif mtype == 0xFA and avail >= 4 then     -- CPCO: completion code
                local hi = tvb(d, 2):uint()
                local lo = tvb(d + 2, 2):uint()
                t:add(pf.tad_data, tvb(d, 4)):append_text(
                    string.format("  [CPC1=0x%04X CPC2=0x%04X]", hi, lo))

            elseif mtype == 0xFB and avail >= 2 then     -- ERRS: SINTRAN error code
                t:add(pf.tad_errcode, tvb(d, 2))

            elseif mtype == 0xFE and avail >= 1 then     -- REJE: rejected opcode
                local bad = tvb(d, 1):uint()
                local bad_nm = vs_tad_type[bad] or string.format("0x%02X", bad)
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [rejected: %s]", bad_nm))

            else
                t:add(pf.tad_data, tvb(d, avail))
            end

        -- Zero-count messages: show purpose in tree label even without data
        else
            if     mtype == 0x02 then t:append_text("  (flow-control credit)")
            elseif mtype == 0x06 then t:append_text("  (connect request marker)")
            elseif mtype == 0x08 then t:append_text("  (escape char received)")
            elseif mtype == 0x09 then t:append_text("  (disconnect)")
            elseif mtype == 0x16 then t:append_text("  (reset request)")
            elseif mtype == 0x17 then t:append_text("  (reset confirm)")
            elseif mtype == 0x18 then t:append_text("  (dummy / filler)")
            elseif mtype == 0x1B then t:append_text("  (session setup, semantics unknown)")
            elseif mtype == 0x20 then t:append_text("  (escape response to ESCA)")
            elseif mtype == 0x21 then t:append_text("  (escape-control ACK)")
            elseif mtype == 0x22 then t:append_text("  (input size request)")
            elseif mtype == 0x26 then t:append_text("  (nowait restart)")
            elseif mtype == 0x27 then t:append_text("  (remote/local toggle)")
            elseif mtype == 0xFD then t:append_text("  (poll from host server task)")
            end
        end

        pinfo.cols.info:append(string.format(" TAD:%s", name))
        pos = pos + 2 + mcount
    end
end

-- ── XSGSY routing-reply parameter decoder ────────────────────────────────────
-- The LI ROUTING trailer is the reply to XROUT service XSGSY ("get routing info
-- for system N"), formatted as ND standard-message parameter blocks. Each block
-- is 4 bytes: [param-number] [length=2] [value-hi] [value-lo] (16-bit BE value).
--
-- Request  = 1 parameter  (param 1 = the system number being queried).
-- Response = 4 parameters (COSMOS Programmer Guide ND-60.164, XSGSY):
--   #1 system number (first >= requested; 0 = none)
--   #2 connection type ENUM: 0=Unavailable 1=Neighbour 2=Via-relay 3=Via-server 4=Local
--   #3 extra info (link index / relay system / subaddress — depends on #2)
--   #4 network info: value <= 0377B -> hop count in the low byte;
--                    value >= 0400B -> #WANs in the high byte, #hops in low byte.
-- (Previously mislabelled XMTNO/XMROU/XMTHI/XMTRE with bitfield decodes — those
-- symbols are XFRCV message-type codes, unrelated; corrected 2026-07.)

local function dissect_li_routing_trailer(tvb, pinfo, tree, off, tlen, is_response)
    if tlen < 4 then return end
    if tlen % 4 ~= 0 then return end

    local n_records = tlen / 4
    local label = is_response and "LI ROUTING Response (XSGSY)" or "LI ROUTING Request (XSGSY)"
    local t = tree:add(lapb_proto, tvb(off, tlen),
                  string.format("%s  [%d parameter%s]",
                      label, n_records, n_records == 1 and "" or "s"))

    local p_sysno, p_conn, p_net = nil, nil, nil

    for i = 0, n_records - 1 do
        local rec_off = off + i * 4
        local pnum    = tvb(rec_off,     1):uint()
        local value16 = tvb(rec_off + 2, 2):uint()
        local pnm     = vs_xsgsy_param[pnum] or string.format("param?(0x%02X)", pnum)

        local rt = t:add(lapb_proto, tvb(rec_off, 4),
                       string.format("%s = %d (0x%04X)", pnm, value16, value16))
        rt:add(pf.xm_param,     tvb(rec_off,     1))
        rt:add(pf.xm_value_raw, tvb(rec_off + 2, 2))

        if pnum == 1 then            -- system number
            rt:add(pf.xm_sysno, tvb(rec_off + 2, 2))
            p_sysno = value16
        elseif pnum == 2 then        -- connection type (enum)
            local ri = rt:add(pf.xm_conntype, tvb(rec_off + 2, 2))
            local nm = vs_conn_type[value16]
            if nm then ri:append_text("  [" .. nm .. "]") end
            p_conn = value16
        elseif pnum == 3 then        -- extra info
            rt:add(pf.xm_extrainfo, tvb(rec_off + 2, 2))
        elseif pnum == 4 then        -- network info: hops (low byte) + WANs (high byte)
            local si = rt:add(pf.xm_netinfo, tvb(rec_off + 2, 2))
            local hops = value16 & 0xFF
            local wans = (value16 >> 8) & 0xFF
            si:append_text(string.format("  [%d hop%s%s]", hops,
                hops == 1 and "" or "s",
                wans > 0 and string.format(", %d WAN%s", wans, wans == 1 and "" or "s") or ""))
            p_net = value16
        end
    end

    -- Build a one-line summary for the tree label and Info column
    if is_response and n_records == 4 then
        local sysno = p_sysno or 0
        local conn  = p_conn or 0
        local net   = p_net or 0
        local summary
        if sysno == 0 and conn == 0 then
            summary = "end-of-table"
        else
            local hops = net & 0xFF
            local wans = (net >> 8) & 0xFF
            summary = string.format("system %d: %s, %d hop%s%s", sysno,
                vs_conn_type[conn] or string.format("conn?%d", conn),
                hops, hops == 1 and "" or "s",
                wans > 0 and string.format(" +%d WAN", wans) or "")
        end
        t:append_text("  — " .. summary)
        pinfo.cols.info:append("  LI-ROUT: " .. summary)
    elseif n_records == 1 then
        pinfo.cols.info:append(string.format("  LI-ROUT? sysno=%d", p_sysno or 0))
    end
end

-- ── XROUT letter TLV decoder (XMCSM low byte 0x41 = XSLET) ───────────────────
-- Letter format (spec §18.8 S10a/S10b, §22.4):
--   FF <len> 2A <name>   the registered server name — the 0x2A IS the literal
--                        '*' of the name as the registry displays it (*TADADM)
--   FE <len> <text>      the target system name the user typed (e.g. "D102")
--   04 02 00 01          the list-systems / directory-query marker TLV
-- A 0x00 between TLVs is padding.
--
-- These tags are XROUT PARAMETER BLOCKS in the sense of the COSMOS Programmer
-- Guide appendix B section 2: the tag byte is the parameter number, negative
-- (two's complement) for a string. So 0xFF is string parameter 1 and 0xFE is
-- string parameter 2 — which for XSLET (appendix B section 3.4) are exactly
-- "port or connection name" and "system name". The 0x00 padding is the manual's
-- even-boundary fill.
--
-- NOTE: there is deliberately NO 4-byte serial/service/length header parsed here.
-- That header is the MESSAGE BUFFER form; it is not carried in an XMSG data
-- frame, where the service travels in XMCSM instead. This dissector had it right
-- from the start — the C# library did not, and was corrected in 2026-07 (see
-- XroutMessageFraming and DOC/XMSG-SERVER-NAMES-AND-LETTERS.md section 5). Non-letter-shaped payloads (e.g. the constant
-- accept letter 01 02 0000 0202 000A) fall through as generic TLVs.

local function dissect_xrout_letter(tvb, pinfo, tree, off, tlen)
    local t = tree:add(lapb_proto, tvb(off, tlen), "XROUT Letter")
    local pos = off
    local endp = off + tlen
    local got_name, got_target, got_query = nil, nil, false

    while pos + 1 < endp do
        local tag = tvb(pos, 1):uint()
        if tag == 0x00 then pos = pos + 1 goto continue end
        do
            local tl  = tvb(pos + 1, 1):uint()
            local avail = math.min(tl, endp - (pos + 2))
            if tag == 0xFF and avail >= 1 and tvb(pos + 2, 1):uint() == 0x2A then
                -- Server name, asterisk included (registry style: *TADADM). Known
                -- registered servers from a live COSMOS list-servers (logical port):
                --   *TADADM 2  *XM-FIDO 4  *COSPO 5  *FA-FSA 7  *XFTRA 8  *FA-SERVER 11
                local name = tvb(pos + 2, avail):string()
                local ni = t:add(pf.letter_name, tvb(pos + 2, avail))
                ni:append_text("  [registered server]")
                got_name = name
            elseif tag == 0xFE and avail >= 1 then
                local target = tvb(pos + 2, avail):string()
                t:add(pf.letter_target, tvb(pos + 2, avail))
                got_target = target
            elseif tag == 0x04 and tl == 2 and avail >= 2
                and tvb(pos + 2, 2):uint() == 0x0001 then
                t:add(lapb_proto, tvb(pos, 4),
                    "Query TLV 04 02 0001  [list-systems / directory query]")
                got_query = true
            else
                t:add(lapb_proto, tvb(pos, 2 + avail),
                    string.format("TLV tag=0x%02X len=%d", tag, tl))
            end
            pos = pos + 2 + avail
        end
        ::continue::
    end

    if got_name then
        t:append_text(string.format("  [%s%s%s]", got_name,
            got_target and (" -> " .. got_target) or "",
            got_query and ", list-systems query" or ""))
        pinfo.cols.info:append(string.format(" LETTER:%s%s%s", got_name,
            got_target and ("->" .. got_target) or "",
            got_query and " (list-systems)" or ""))
    end
end

-- =============================================================================
-- THE XMSG MESSAGE BODY - absolute offset 28                    [added 2026-08-24]
-- =============================================================================
--
-- WHERE THE BODY STARTS, AND WHY IT IS NOT WHERE THIS FILE USED TO PUT IT.
--
-- The XMSG sub-header is FOURTEEN bytes and it starts at absolute wire offset 14,
-- straight after the seven-word SINTRAN header. So the message body begins at
-- absolute 28. That is not a reading of ours - it is pinned by the C# reference
-- implementation and by a test written to fail if it moves:
--
--   SINTRAN/XMSG/SRC/Xmsg.Protocol/Wire/XmsgSubHeader.cs   (corrected 2026-08-04)
--   SINTRAN/XMSG/SRC/Xmsg.Protocol.Tests/FaBodyOffsetTests.cs
--       - parses a REAL captured datagram at 28, and deliberately checks that 32
--         does NOT parse, which is what makes 28 a measurement and not a preference
--   DOC/protocols/fa-qform.json  message_prefix  (message type at body byte 0)
--
-- The decode further down this file still reads a 32-bit "XMCSM" at absolute 26
-- and an "XMLEN" at absolute 30. Under the carved 14-byte sub-header those are:
--
--   absolute 26-27   XMCSM, ONE word, and it always equals the header Flags 2
--   absolute 28-29   body word 0 - the APPLICATION first word
--   absolute 30-31   body word 1 - also the application
--
-- which is why the old "XMCSM low byte" happens to carry the XROUT service code
-- (it is really body word 0 low byte) and why "XMLEN" reads as a length on an
-- XROUT letter but as a CONVERSATION NUMBER on a file-server message. The old
-- fields are LEFT ALONE here on purpose - existing capture filters use them and
-- the TAD, XSGSY and letter decoders they feed are correct for those families.
-- What is added is a second, correctly placed view of the same bytes.

-- QFORM, the tag encoding the file server writes its fields in.
-- DOC/protocols/fa-qform.json, block "qform". Every rule below is MEASURED there.
--
--   bit 7 CLEAR        the stream ends here
--   class              (tag AND 0x70) >> 4
--   length             tag AND 0x0F, for classes 1..7
--   subtype            tag AND 0x17, for class 0 (constructed, length ALWAYS escaped)
--   length escape      a length nibble of 0 means the real length is the NEXT byte
--   escape marker      an escaped length byte of 0x80 is a MARKER, not a length;
--                      the length is the byte after it
--
-- Two traps the registry records, both honoured here:
--   - a FLAT walk descends into a constructed value and reads its bytes as
--     top-level tags, so class 0 recurses instead
--   - SINTRAN pads a body to an even length, and the pad byte can look like the
--     start of a field, so a single trailing byte is called padding and not parsed
local QFORM_MAX_DEPTH = 8

local function qform_class_label(cls)
    return REG.qform_class_vs[cls] or string.format("class %d", cls)
end

-- Decode one run of QFORM items. Returns the offset just past the run.
local function dissect_qform(tvb, pinfo, tree, off, len, depth)
    local stop = off + len
    depth = depth or 0

    while off < stop do
        -- The even-length pad. One byte left over is padding, never a field.
        if stop - off == 1 then
            local pad = tree:add(pf.qform_pad, tvb(off, 1))
            pad:append_text("  [SINTRAN pads a body to an even length]")
            return stop
        end

        local tag = tvb(off, 1):uint()
        if (tag & 0x80) == 0 then
            tree:add(pf.qform_end, tvb(off, 1)):append_text(
                "  [bit 7 clear - the QFORM stream ends here]")
            return off + 1
        end

        local cls = (tag & 0x70) >> 4
        local hdr = 1
        local vlen
        if cls == 0 then
            -- Constructed: the length is ALWAYS escaped, never in the nibble.
            vlen = tvb(off + 1, 1):uint()
            hdr = 2
            if vlen == 0x80 then
                vlen = tvb(off + 2, 1):uint()
                hdr = 3
            end
        else
            vlen = tag & 0x0F
            if vlen == 0 then
                vlen = tvb(off + 1, 1):uint()
                hdr = 2
                if vlen == 0x80 then
                    vlen = tvb(off + 2, 1):uint()
                    hdr = 3
                end
            end
        end

        if off + hdr + vlen > stop then
            local bad = tree:add(pf.qform_tag, tvb(off, 1))
            bad:add_expert_info(PI_MALFORMED, PI_WARN, string.format(
                "QFORM item of class %d claims %d bytes but only %d are left in the body. " ..
                "Stopping rather than reading past the end.", cls, vlen, stop - off - hdr))
            return stop
        end

        local item = tree:add(pf.qform_item, tvb(off, hdr + vlen))
        item:set_text(string.format("QFORM  %s  %d byte%s",
            qform_class_label(cls), vlen, vlen == 1 and "" or "s"))
        item:add(pf.qform_tag, tvb(off, 1))
        local ci = item:add(pf.qform_class, tvb(off, 1), cls)
        ci:set_generated()
        local li = item:add(pf.qform_len, tvb(off, hdr), vlen)
        li:set_generated()

        if cls == 0 then
            item:append_text(string.format("  [constructed, subtype 0x%02X]", tag & 0x17))
            if depth < QFORM_MAX_DEPTH then
                dissect_qform(tvb, pinfo, item, off + hdr, vlen, depth + 1)
            else
                item:append_text("  [nested too deep - not walked further]")
            end
        elseif vlen > 0 then
            item:add(pf.qform_value, tvb(off + hdr, vlen))
            if (cls == 1 or cls == 2 or cls == 7) and vlen <= 4 then
                local n = tvb(off + hdr, vlen):uint()
                item:append_text(string.format("  = %d (0x%X)", n, n))
                if cls == 2 then
                    -- CAREFUL. The registry says a TypedInteger carries the SINTRAN
                    -- error number IN A REJECTION - it does not say what the class
                    -- means anywhere else, and this capture is full of TypedIntegers
                    -- holding 0, 1, 2, 64, 2000 and 2048 on messages that are not
                    -- rejections. Looking every one of them up in the status table
                    -- was tried and printed "= Ok" beside a block number, which is
                    -- exactly the confident-and-wrong output this file must not
                    -- produce. So the note says what is known and stops there.
                    item:append_text("  [class 2. The registry records this class carrying " ..
                        "the SINTRAN error number in a REJECTION; on any other message its " ..
                        "meaning is NOT recorded]")
                elseif cls == 7 and n == 0x00FF then
                    item:append_text("  [selector 0x00FF - the end marker]")
                end
            elseif cls == 3 then
                item:add(pf.qform_text, tvb(off + hdr, vlen))
            end
        end

        off = off + hdr + vlen
    end
    return off
end

-- The file-server (FA) message body.
-- DOC/protocols/fa-qform.json, message_prefix: eight bytes before the QFORM.
--
--   0-1  message type    REG.fa_message_type
--   2-3  conversation number
--   4-7  session header  a counter byte, a zero, then a 16-bit token
--   8+   the QFORM body
--
-- The operation is NOT in the registry, and that is a gap worth naming: the C#
-- codec reads it, so the knowledge exists, it is just not written down where the
-- generators can see it. FaExchangeCodec.TryReadOperation says the QFORM opens
-- with TWO class-1 two-byte integers, tag 0x92 - the first is the operation and
-- the second is the exchange number, counting from one. That is what is annotated
-- below, and it is labelled as coming from the code rather than the registry.
local function dissect_fa_body(tvb, pinfo, tree, off, len)
    local mtype = tvb(off, 2):uint()
    local ft = tree:add(pf.fa_body, tvb(off, len))
    ft:set_text(string.format("File server (FA) message  [%s]",
        REG.fa_message_type_vs[mtype] or string.format("type 0x%04X UNKNOWN", mtype)))

    ft:add(pf.fa_msgtype, tvb(off, 2))
    ft:add(pf.fa_conversation, tvb(off + 2, 2))

    if len >= 8 then
        local sh = ft:add(pf.fa_session, tvb(off + 4, 4))
        sh:append_text(string.format("  [counter 0x%02X, then a zero, then token 0x%04X]",
            tvb(off + 4, 1):uint(), tvb(off + 6, 2):uint()))
    end

    -- Only a request or a reply carries a QFORM. A short acknowledgement carries
    -- nothing after the envelope and file content is raw bytes, not tagged.
    if len > 8 then
        local qlen = len - 8
        local qt = ft:add(pf.fa_qform, tvb(off + 8, qlen))
        qt:set_text(string.format("QFORM body  (%d bytes)", qlen))

        -- The operation annotation, from the C# codec (see the note above).
        if qlen >= 6
            and tvb(off + 8, 1):uint() == 0x92
            and tvb(off + 11, 1):uint() == 0x92 then
            local op = tvb(off + 9, 2):uint()
            local seq = tvb(off + 12, 2):uint()
            local oi = qt:add(pf.fa_operation, tvb(off + 9, 2), op)
            oi:set_generated()
            oi:append_text(string.format("  [exchange %d.  Read the way FaExchangeCodec." ..
                "TryReadOperation reads it - two class-1 integers, tag 0x92. NOT in the " ..
                "protocol registry]", seq))
        end

        dissect_qform(tvb, pinfo, qt, off + 8, qlen, 0)
    end

    local nm = REG.fa_message_type[mtype]
    return "FA " .. ((nm and nm.name) or string.format("0x%04X", mtype))
end

-- The chat message body.
-- DOC/protocols/chat-wire.json, message_prefix. This is the one protocol on this
-- wire that Norsk Data never shipped, so nothing about it is a rediscovery:
--
--   0    kind             REG.chat_kind
--   1    nickname length  ASCII bytes that follow; zero is legal
--   2+   the nickname, then a TWO-byte big-endian text length, then the text
--
-- HOW A CHAT MESSAGE IS RECOGNISED, AND WHY THAT IS A GUESS. There is no tag on
-- the wire that says "this is chat" - it is ordinary XMSG user data sent to a port
-- the server registered by name. A dissector has no way to know which port that
-- is, so the only handle is the SHAPE: a known kind byte, and a walk of the two
-- length fields that lands EXACTLY on the end of the body, give or take the one
-- pad byte SINTRAN adds to make the length even. That is a strong test but it is
-- still a test on shape, so the tree says so and this decode runs LAST, only when
-- nothing better has claimed the body.
local function chat_body_fits(tvb, off, len)
    if len < 4 then return false end
    local kind = tvb(off, 1):uint()
    if REG.chat_kind[kind] == nil or kind == 0 then return false end
    local namelen = tvb(off + 1, 1):uint()
    if 2 + namelen + 2 > len then return false end
    local textlen = tvb(off + 2 + namelen, 2):uint()
    local total = 2 + namelen + 2 + textlen
    -- An exact fit, or one pad byte short of the even length SINTRAN writes.
    return total == len or total == len - 1
end

local function dissect_chat_body(tvb, pinfo, tree, off, len)
    local kind = tvb(off, 1):uint()
    local namelen = tvb(off + 1, 1):uint()
    local textlen = tvb(off + 2 + namelen, 2):uint()

    local ct = tree:add(pf.chat_body, tvb(off, len))
    ct:set_text(string.format("CHAT message  [%s]",
        REG.chat_kind_vs[kind] or string.format("kind %d UNKNOWN", kind)))
    ct:append_text("  [recognised by SHAPE, not by any tag on the wire]")

    ct:add(pf.chat_kind, tvb(off, 1))
    ct:add(pf.chat_namelen, tvb(off + 1, 1))
    if namelen > 0 then
        ct:add(pf.chat_name, tvb(off + 2, namelen))
    end
    ct:add(pf.chat_textlen, tvb(off + 2 + namelen, 2))
    if textlen > 0 then
        ct:add(pf.chat_text, tvb(off + 4 + namelen, textlen))
    end

    local nm = REG.chat_kind[kind]
    return "CHAT " .. ((nm and nm.name) or tostring(kind))
end

-- The body dispatcher.
-- Runs on every data message, beside the older trailer decode rather than instead
-- of it. It says what it recognises and stays quiet about what it does not - an
-- unrecognised body is shown as bytes with its first word called out, because the
-- first word is what every one of these families dispatches on.
local function dissect_xmsg_body(tvb, pinfo, tree, off, len, is_xrout)
    if len < 2 or off + len > tvb:len() then return nil end

    local word0 = tvb(off, 2):uint()

    if REG.fa_message_type[word0] ~= nil and len >= 8 then
        return dissect_fa_body(tvb, pinfo, tree, off, len)
    end

    -- An XROUT request or reply: body word 0 low byte is the service code on a
    -- request (bit 6 set) or the XR* status on a reply (bit 6 clear). The letter
    -- and XSGSY trailers are already decoded by the older path below, so this only
    -- names what it is.
    --
    -- GATED ON THE PORT, and it has to be. The first version of this test read the
    -- low byte on EVERY message, and a status of 0x00 is XRSOK - so every TAD
    -- terminal-data frame in conn-to-d102-from-100.pcapng came out labelled
    -- "XROUT reply [XRSOK]". Wrong, and confidently so. Port 0 is the XROUT
    -- well-known address (see port_label), so a message is only read as XROUT when
    -- one end of it IS XROUT.
    local low = word0 & 0xFF
    local svc = REG.xrout_service_vs[low]
    if is_xrout and svc ~= nil and (low & 0x40) ~= 0 then
        local xt = tree:add(pf.xrout_body, tvb(off, len))
        xt:set_text(string.format("XROUT request  [service 0x%02X = %s]", low, svc))
        local si = xt:add(pf.xrout_service, tvb(off + 1, 1), low)
        si:set_generated()
        xt:append_text(string.format("  [body word 0 high byte 0x%02X]", word0 >> 8))
        local nm = REG.xrout_service[low]
        return "XROUT " .. ((nm and nm.name) or "")
    end
    if is_xrout and (low & 0x40) == 0 and REG.xrout_error[low] ~= nil then
        local xt = tree:add(pf.xrout_body, tvb(off, len))
        xt:set_text(string.format("XROUT reply  [status 0x%02X = %s]",
            low, REG.xrout_error_vs[low]))
        local ei = xt:add(pf.xrout_status, tvb(off + 1, 1), low)
        ei:set_generated()
        local nm = REG.xrout_error[low]
        return "XROUT " .. ((nm and nm.name) or "")
    end

    if chat_body_fits(tvb, off, len) then
        return dissect_chat_body(tvb, pinfo, tree, off, len)
    end

    local raw = tree:add(pf.xmsg_body, tvb(off, len))
    raw:set_text(string.format("Message body  (%d bytes, first word 0x%04X - not recognised)",
        len, word0))
    return nil
end

-- ── XMSG data-frame dissector (all subtype-0x0E frames, every channel) ───────
-- VERIFIED across all captures: every data (subtype 0x0E) frame carries the SAME
-- XMSG sub-header (counter, 0x21 0x00 marker, frame-flags, role, XMDSY/XMDPT/
-- XMSSY/XMSPT, XMCSM, 16-bit XMLEN) regardless of Protocol ID — 100% of frames
-- for D8/D9/DA/DB/DC/DD/DE. The Protocol ID is a DERIVED CHANNEL TAG, not a
-- different frame layout, so ALL channels go through this common parser.
-- The trailer is then decoded by content, dispatched on XMCSM.
--
-- Sub-header layout (offsets from the counter byte at 'off'):
--   +0   Counter          per-direction counter: (baseLow − Flags1) & 0xFF
--   +1-2 Marker           0x21 0x00
--   +3   Frame Flags      status byte (see frameflags_bits)
--   +4   Role             high byte of the XMSG send-option word (XF* flags)
--   +5-6  XMDSY           destination system (BE)
--   +7-8  XMDPT           destination port (BE) — MAGNO low word: (port<<7)|random
--   +9-10 XMSSY           source system (BE)
--   +11-12 XMSPT          source port (BE)
--   +13-16 XMCSM          class word (hi 16 = Flags2) + service/status byte
--   +17-18 XMLEN          user data length, 16-BIT big-endian (offset 17 is
--                         the high byte — formerly misread as a pad byte)
--   +19+  user data

local DC_SUBHDR = 17

local function dissect_dc(tvb, pinfo, tree, off, proto_label, ctx)
    local rem = tvb:len() - off
    proto_label = proto_label or "DC"

    -- Single control byte (rare data frame with 1-byte payload)
    if rem == 1 then
        local t = tree:add(lapb_proto, tvb(off, 1), proto_label .. " Control")
        t:add(pf.tad_ctrl, tvb(off, 1))
        pinfo.cols.info:append(string.format(" %s:ctrl=0x%02X", proto_label, tvb(off, 1):uint()))
        return
    end

    if rem < DC_SUBHDR + 2 then
        tree:add(lapb_proto, tvb(off), string.format("[%s: too short %dB]", proto_label, rem))
        return
    end

    local ctr1 = tvb(off, 1):uint()
    tree:add(pf.dc_ctr1, tvb(off, 1))

    -- Response anomaly observed in LI ROUTING captures: the responder fills
    -- XMSSY/XMSPT with the *originator's* address (the asker's), not its own.
    -- This looks like a stateless-RPC convention where the responder doesn't
    -- allocate a port and instead echoes the asker's identity as a transaction
    -- id. The doc references XFRTN as a "swap src/dst" kernel call — LI ROUTING
    -- responders may be skipping it for stateless replies.

    local frameflags = tvb(off + 3,  1):uint()
    local role     = tvb(off + 4,  1):uint()
    local dsy      = tvb(off + 5,  2):uint()
    local dpt      = tvb(off + 7,  2):uint()
    local ssy      = tvb(off + 9,  2):uint()
    local spt      = tvb(off + 11, 2):uint()
    local cmd_word = tvb(off + 13, 4):uint()
    -- XMLEN is 16-bit: offset 17 = HIGH byte, offset 18 = low byte.
    -- (Proven by 255-byte output chunks: 0x0101 = 257.)
    local tlen     = tvb(off + 17, 2):uint()

    local cmd_name = vs_dc_cmd[cmd_word] or string.format("0x%08X", cmd_word)

    -- Sub-header tree spans marker..XMLEN (off+1 .. off+18 inclusive = 18 bytes)
    local sub = tree:add(lapb_proto, tvb(off + 1, DC_SUBHDR + 1),
                    string.format("%s  [%s, %d:%d → %d:%d]",
                        proto_label, cmd_name, ssy, spt, dsy, dpt))
    sub:add(pf.dc_sub_type,  tvb(off + 1,  1))  -- 0x21 marker

    -- Frame Flags: raw value + known bits (spec §18.4 U1)
    local ff_item = sub:add(pf.dc_flags86, tvb(off + 3, 1))
    local ff_lbl  = bitfield_label(frameflags, frameflags_bits)
    if ff_lbl ~= "" then ff_item:append_text("  [" .. ff_lbl .. "]") end

    -- Role byte = send-option bitfield (spec §18.4 U2). NOT a fixed
    -- asker/responder label — see the role_bits comment above.
    local role_item = sub:add(pf.dc_role, tvb(off + 4, 1))
    local role_lbl  = bitfield_label(role, role_bits)
    role_item:append_text(role_lbl ~= "" and ("  [" .. role_lbl .. "]")
                                          or  "  [no options]")

    sub:add(pf.xmsg_dsy,     tvb(off + 5,  2))
    sub:add(pf.xmsg_dpt,     tvb(off + 7,  2)):append_text("  [" .. port_label(dpt) .. "]")
    local ssy_item = sub:add(pf.xmsg_ssy, tvb(off + 9,  2))
    local spt_item = sub:add(pf.xmsg_spt, tvb(off + 11, 2))
    spt_item:append_text("  [" .. port_label(spt) .. "]")

    -- Annotate the XMCSM low byte: XROUT service code (bit 6 set = request) on
    -- requests; XR* status (bit 6 clear, 0x00 = XRSOK) on replies.
    local cmd_item  = sub:add(pf.dc_cmd, tvb(off + 13, 4))
    local svc_byte  = cmd_word & 0xFF
    local svc_name  = vs_xs_service[svc_byte]
    if svc_name then
        cmd_item:append_text(string.format("  [service 0x%02X = %s, request]", svc_byte, svc_name))
    elseif svc_byte == 0 then
        cmd_item:append_text("  [status 0x00 = XRSOK, reply]")
    elseif svc_byte < 0x40 then
        cmd_item:append_text(string.format("  [status 0x%02X = XR* error, reply]", svc_byte))
    end

    sub:add(pf.dc_tlen, tvb(off + 17, 2))

    -- Stateless-RPC response anomaly detection: in LI ROUTING responses (and
    -- possibly other XROUT-style services), the responder fills XMSSY/XMSPT
    -- with the originator's address rather than its own. Flag it explicitly so
    -- the anomaly stays visible on every capture going forward — and so any
    -- frame where the anomaly does NOT happen stands out as worth investigating.
    if dsy == ssy and dpt == spt and dsy ~= 0 then
        ssy_item:append_text("  [== XMDSY: stateless-RPC response anomaly]")
        spt_item:append_text("  [== XMDPT: stateless-RPC response anomaly]")
        ssy_item:add_expert_info(PI_PROTOCOL, PI_NOTE,
            "XMSSY/XMSPT echo originator address (stateless-RPC convention)")
    end

    -- ── Envelope validation (spec §18.5 seed model) ──────────────────────────
    -- seed    = (Counter + Flags1 + F2low) & 0xFF   (per-link constant)
    -- baseLow = seed − F2low                        (SIGNED — see below)
    -- epoch   = (Flags1 − baseLow + 0xFF) >> 8
    -- channel = 0xDE − (XMCSM >> 24) − epoch  == the header Protocol-ID byte
    -- Also: Flags2 == XMCSM >> 16 on every data frame.
    -- On a relayed hop (marker 0x12) the relay re-stamps Counter +1, so the
    -- derived seed reads seed+1 (the "relay lane") — the arithmetic still
    -- self-checks because seed is derived from the same frame.
    --
    -- CORRECTED 2026-07-31: baseLow must NOT be masked to 0..255. When
    -- F2low > seed the true baseLow is NEGATIVE; masking turned it into a large
    -- positive, which moved the 256-boundary in the epoch shift and lost a
    -- borrow — the epoch came out one too low and the predicted channel one too
    -- HIGH. That is invisible on TAD/routing traffic (5 distinct Flags2 values,
    -- all <= seed) and fires constantly on file-server traffic (11-15 distinct
    -- Flags2, many > seed), which is why the old "VERIFIED 753/753" held: it was
    -- measured only on the family that cannot exercise the bug.
    -- With the mask removed the model is exact on the WHOLE corpus:
    -- 1449/1449 data frames across every capture and every link (seeds 0x11,
    -- 0x13, 0x14 and the relayed +1 lanes), vs 1267/1449 masked.
    -- See NDInsight SINTRAN/XMSG/DOC/
    -- XMSG-CHANNEL-FORMULA-DIVERGES-ON-FILE-SERVER-TRAFFIC-2026-07-31.md.
    if ctx then
        -- The channel/Counter check that used to live here is GONE: those two bytes
        -- are the halves of the header checksum, now validated once for every
        -- subtype in the header dissector (see sintran_hdr_checksum). Re-deriving a
        -- "seed" and "epoch" here would just be the old curve fit.
        --
        -- The Flags2 == XMCSM check below is KEPT and is now better motivated: both
        -- are checksum inputs, so a mismatch corrupts word 6. Note that under the
        -- carved 16-bit XMCSM the rule is a plain equality (Flags2 == XMCSM); the
        -- ">> 16" here is an artifact of this dissector still reading XMCSM as 32
        -- bits, which straddles into the message body.
        if ctx.flags2 ~= band(rshift(cmd_word, 16), 0xFFFF) then
            cmd_item:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                "Flags2 0x%04X != XMCSM class word 0x%04X (rule: Flags2 == XMCSM>>16)",
                ctx.flags2, band(rshift(cmd_word, 16), 0xFFFF)))
        end
    end

    -- ── The message body, at its measured offset ─────────────────────────────
    -- Absolute 28 = off + 15, because dissect_dc is called with off = absolute 13.
    -- See the long note beside dissect_xmsg_body for why 28 and not 32. This runs
    -- BESIDE the older trailer decode below, not instead of it: the trailer path
    -- is correct for TAD, XSGSY and letters and is what existing filters use,
    -- while this one is what reads a file-server or a chat message.
    local body_off = off + 15
    local body_len = tvb:len() - body_off
    local body_name = nil
    if body_len >= 2 then
        -- Port 0 is XROUT. A message with XROUT at either end is the only kind
        -- whose body word 0 may be read as a service code or a return status.
        local is_xrout = (dpt == 0 or spt == 0)
        body_name = dissect_xmsg_body(tvb, pinfo, tree, body_off, body_len, is_xrout)
    end

    -- ── Trailer, dispatched on XMCSM (spec §9) ───────────────────────────────
    local trailer_off = off + 19
    if tlen > 0 and trailer_off + tlen <= tvb:len() then
        if cmd_word == 0x0100014B then
            -- XSGSY request: 1 parameter record (the system number)
            dissect_li_routing_trailer(tvb, pinfo, tree, trailer_off, tlen, false)
        elseif cmd_word == 0x01000100 then
            -- XSGSY reply: parameters #1..#4
            dissect_li_routing_trailer(tvb, pinfo, tree, trailer_off, tlen, true)
        elseif svc_byte == 0x41 then
            -- XSLET letter (e.g. XMCSM 0x04000041): parse the letter TLVs
            dissect_xrout_letter(tvb, pinfo, tree, trailer_off, tlen)
        elseif cmd_word == 0x04000000 or cmd_word == 0x01080000
            or cmd_word == 0x00080000 or cmd_word == 0x00060000 then
            -- Known TAD-session classes: trailer is a TAD message chain
            dissect_tad(tvb, pinfo, tree, trailer_off)
        elseif body_name ~= nil then
            -- The body dispatcher recognised this message, so the TAD fallback is
            -- switched off for it. ADDED 2026-08-24 after watching the fallback
            -- turn a file-server QFORM into invented TAD opcodes 0x80 and 0x90 -
            -- output that reads like data and is not.
            local raw = tree:add(pf.dc_trailer, tvb(trailer_off, tlen))
            raw:append_text(string.format(
                "  [inside the %s body decoded above - no TAD fallback]", body_name))
        else
            -- Unknown command — show raw trailer, then try TAD chain.
            -- Label both branches so it's obvious which decode path was taken.
            local raw = tree:add(pf.dc_trailer, tvb(trailer_off, tlen))
            raw:append_text("  [unknown XMCSM, trying TAD chain fallback]")
            dissect_tad(tvb, pinfo, tree, trailer_off)
        end
    end

    if body_name then
        pinfo.cols.info:append(string.format(" %s[%s]", proto_label, body_name))
    else
        pinfo.cols.info:append(string.format(" %s[%s ctr=0x%02X]", proto_label, cmd_name, ctr1))
    end

    -- Handed back so the caller can put it in the Info column. It has to travel up
    -- the return chain rather than being appended here and left: the ND link
    -- decoders finish by REPLACING the Info column with their own summary, so
    -- anything appended lower down is wiped. That is why the file-server name never
    -- reached the Info column on a hub or segment capture.
    return body_name
end

-- ── SINTRAN info dissector ────────────────────────────────────────────────────
-- tvb covers the LAPB info bytes (addr+ctrl stripped, FCS already stripped).

local function dissect_sintran_info(tvb, pinfo, frame_tree)
    local len = tvb:len()

    if len < SINTRAN_HDR then
        frame_tree:add(lapb_proto, tvb(0), string.format("[Info too short for SINTRAN: %d bytes]", len))
        return nil
    end

    -- What the body decoder made of this message, if anything. Declared here so it
    -- is a LOCAL: assigning it further down without this line would quietly make a
    -- global that survives into the next frame.
    local body_name = nil

    local mark2 = tvb(1, 1):uint()
    if tvb(0, 1):uint() ~= 0x21 or (mark2 ~= 0x13 and mark2 ~= 0x12) then
        -- NOTE 2026-07-31: a fourth family exists with Marker2 = 0xFD / 0xFE
        -- (four frames in li-rout-102-tree.pcapng, from node 103, Flags1 0xFFFF
        -- and Flags2 0xFFFD). Their header checksum DOES validate under the same
        -- rule, so the 7-word layout evidently applies to them - but what their
        -- offset-3 field means is unknown, so they are deliberately NOT dissected
        -- as ordinary SINTRAN frames here. That is why a corpus sweep shows this
        -- dissector validating 3591 checksums where an offline scan finds 3595.
        frame_tree:add(lapb_proto, tvb(0), "[Non-SINTRAN info (no 0x21 0x12/0x13 marker)]")
        return nil
    end

    local subtype  = tvb(3, 1):uint()
    local dest     = tvb(4, 2):uint()
    local src      = tvb(6, 2):uint()
    local flags1   = tvb(8, 2):uint()
    local flags2   = tvb(10, 2):uint()
    local proto_id = tvb(12, 1):uint()
    local proto_nm = vs_proto[proto_id] or string.format("0x%02X", proto_id)
    local is_relay = (mark2 == 0x12)
    local label    = is_relay and "SINTRAN Relay" or "SINTRAN"

    local hdr = frame_tree:add(lapb_proto, tvb(0, SINTRAN_HDR),
                    string.format("%s  [%d → %d  %s]", label, src, dest, proto_nm))

    -- ── Header checksum (word 6) ─────────────────────────────────────────────
    -- Validate here rather than inside the data-frame path, because the checksum
    -- holds for EVERY subtype (3595/3595 across the corpus), not just 0x0E. The
    -- old envelope validator only ran for Data frames and so never checked ACKs,
    -- the bulk-transfer subtypes, or the reachability frames.
    if tvb:len() > SINTRAN_HDR then
        local w6_actual = proto_id * 0x100 + tvb(13, 1):uint()
        local w6_expect = sintran_hdr_checksum(
            tvb(0, 2):uint(),     -- w0  markers
            tvb(2, 2):uint(),     -- w1  packet type : subtype
            dest,                 -- w2
            src,                  -- w3
            flags1,               -- w4
            flags2)               -- w5  (== the 16-bit XMCSM)

        local ck = hdr:add(lapb_proto, tvb(12, 2), string.format(
            "Header checksum: 0x%04X  [ProtoID 0x%02X : Counter 0x%02X]",
            w6_actual, proto_id, tvb(13, 1):uint()))
        ck:set_generated()
        if w6_actual == w6_expect then
            ck:append_text("  [OK]")
        else
            ck:append_text(string.format("  [BAD - expected 0x%04X]", w6_expect))
            ck:add_expert_info(PI_CHECKSUM, PI_WARN, string.format(
                "SINTRAN header checksum 0x%04X, expected 0x%04X " ..
                "(ones-complement sum over header words 0-5)", w6_actual, w6_expect))
        end
    end

    hdr:add(pf.snt_mark1,  tvb(0,  1))
    local m2_item = hdr:add(pf.snt_mark2,  tvb(1,  1))
    if is_relay then
        -- Marker 2 = 0x12 marks the RELAYED HOP: a node forwarding between two
        -- others re-stamps the sub-header Counter +1; the SINTRAN endpoints stay
        -- the LOGICAL endpoints (the relay never appears in this header — only
        -- in the LAPB SABM/UA/RR info field). Spec §4 / §18.8 S11.
        m2_item:append_text("  [relayed hop — Counter re-stamped +1 by relay]")
    end
    hdr:add(pf.snt_pkt,    tvb(2,  1))
    local sub_item = hdr:add(pf.snt_len,    tvb(3,  1))
    hdr:add(pf.snt_dest,   tvb(4,  2))   -- big-endian: 0x0066 = node 102
    hdr:add(pf.snt_src,    tvb(6,  2))   -- big-endian: 0x0064 = node 100
    local f1_item  = hdr:add(pf.snt_flags1, tvb(8,  2))
    local f2_item  = hdr:add(pf.snt_flags2, tvb(10, 2))
    hdr:add(pf.snt_proto,  tvb(12, 1))

    -- ── Subtype semantics (VERIFIED against the X25Emulator pcaps) ────────────

    if subtype == 0x03 then
        -- ── ACK (delivery acknowledgment) ─────────────────────────────────────
        -- Sent OPPOSITE to the data frame it acknowledges; Flags1 ECHOES that
        -- frame's datagram sequence.
        -- Flags2 may be 0x0001 or 0x0002 (both valid; receivers accept both).
        --
        -- The seed / epoch / channel closed form that used to be worked out here
        -- IS GONE, removed 2026-08-11. This file already says why at the top:
        -- offset 12 is the checksum HIGH byte, and there is NO channel, NO epoch
        -- and NO per-link seed - the whole baseLow/epoch construction was fitted
        -- to a corpus and does not describe what the machines do.
        --
        -- It ended with an expert WARN, "ACK channel mismatch ... peer validates
        -- this". That warning stayed quiet on real ND-to-ND traffic only because
        -- none of it decoded this far. Once the ND link layer was added on
        -- 2026-08-11 our own captures started decoding, and it fired several
        -- hundred times per capture against a model known to be wrong - burying
        -- the link-layer expert items that ARE measured. A check for a rule we
        -- have disproved is worse than no check.
        --
        -- The trailing byte itself is still shown, plainly, as the byte it is.
        sub_item:append_text("  [ACK / delivery acknowledgment]")
        f1_item:append_text(string.format("  [acknowledged datagram seq = %d]", flags1))
        hdr:append_text(string.format("  ACK seq=%d", flags1))
        if flags2 ~= 0x0001 and flags2 ~= 0x0002 then
            f2_item:add_expert_info(PI_PROTOCOL, PI_NOTE,
                "Unusual ACK Flags2 (corpus shows only 0x0001 / 0x0002)")
        end
        if len >= SINTRAN_HDR + 1 then
            -- Shown as the byte on the wire. What it MEANS is not established -
            -- see the note above for the model that was removed and why.
            frame_tree:add(pf.ack_trail, tvb(SINTRAN_HDR, 1))
        end

    elseif subtype == 0x19 or subtype == 0x13 then
        -- ── Reachability handshake, spec §5.1 ─────────────────────────────────
        -- TWO request forms: link-start (Flags1 = 0xFFFF broadcast) and RESYNC
        -- (Flags1 echoes an out-of-sequence datagram — sent by a node with no
        -- pair state; the peer then zeroes both directions and replays).
        -- Trailing-byte closed form (VERIFIED all reach frames in the corpus):
        --   request = ((seed − 0x0C) − F1adj) & 0xFF, reply = request + 6
        --   (F1adj = 0 for link-start, = echoed Flags1 for resync)
        -- Flags2 = HOP COUNT (0x0001 direct, 0x0002 on the relayed leg).
        local is_req  = (subtype == 0x19)
        local is_resync = (flags1 ~= 0xFFFF)
        if is_resync then
            f1_item:append_text(string.format(
                "  [RESYNC form — echoes out-of-sequence datagram seq %d]", flags1))
            hdr:append_text("  RESYNC")
        else
            f1_item:append_text("  [link-start form — broadcast marker]")
        end
        f2_item:append_text(string.format("  [hop count = %d]", flags2))
        if len >= SINTRAN_HDR + 1 then
            local trailing = tvb(SINTRAN_HDR, 1):uint()
            local f1adj = is_resync and flags1 or 0
            -- Invert the closed form to display the implied link seed:
            -- request: trailing = seed − 0x0C − F1adj  →  seed = trailing + 0x0C + F1adj
            -- reply:   trailing = request-form + 6     →  subtract the 6 first
            local seed = band(trailing + 0x0C + f1adj - (is_req and 0 or 6), 0xFF)
            local rt = frame_tree:add(pf.reach_trail, tvb(SINTRAN_HDR, 1))
            rt:append_text(string.format(
                "  [closed form: %s ⇒ implied link seed 0x%02X]",
                is_req and "trailing = (seed−0x0C) − F1adj"
                        or "trailing = (seed−0x0C) − F1adj + 6",
                seed))
        end

    elseif subtype == 0x07 then
        -- ── Network error / reject notification, spec §4.1.1 ──────────────────
        -- Flags1 = datagram seq of the rejected message; Flags2 = a SIGNED
        -- negative XE* error code (XENSE −34 sequence error, XEIMA −19 invalid
        -- magic number).
        local err_nm = vs_xe_error[flags2]
        sub_item:append_text("  [network error / reject]")
        f1_item:append_text(string.format("  [rejected datagram seq = %d]", flags1))
        if err_nm then
            f2_item:append_text("  [" .. err_nm .. "]")
        else
            local signed = flags2 >= 0x8000 and (flags2 - 0x10000) or flags2
            f2_item:append_text(string.format("  [XE* error code %d]", signed))
        end
        f2_item:add_expert_info(PI_PROTOCOL, PI_WARN,
            "NetworkError frame: " .. (err_nm or string.format("XE* code 0x%04X", flags2)))
        hdr:append_text("  " .. (err_nm and err_nm:match("^(%S+)") or "NetworkError"))

    elseif subtype == 0x0E then
        -- Data frame: Flags1 is this message's own datagram-sequence number.
        f1_item:append_text(string.format("  [datagram seq = %d]", flags1))

        -- Sub-protocol dispatch.
        -- VERIFIED across all captures: every data (subtype 0x0E) frame carries
        -- the SAME XMSG sub-header regardless of Protocol ID — 100% of frames
        -- for D8/D9/DA/DB/DC/DD/DE. The Protocol ID is a DERIVED CHANNEL TAG
        -- (0xDE − class − epoch), not a different frame layout, so ALL channels
        -- go through the common sub-header parser (dissect_dc), which decodes
        -- the trailer by XMCSM (XSGSY params, XROUT letter TLVs, TAD chain).
        --
        -- (Earlier revisions routed 0xDD -> a bare-TAD parser, which mis-read the
        -- "counter + 21 00" sub-header prefix as a fake "TAD Block-33"; 0xDA ->
        -- a raw PAD dump; and 0xDE -> a 1-byte "routing command" parser. All
        -- three are corrected here: one envelope, one parser.)
        local ctx = { flags1 = flags1, flags2 = flags2,
                      proto_id = proto_id, relay = is_relay }
        body_name = dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, proto_nm, ctx)

    else
        -- Unknown subtype: show remaining bytes raw.
        if len > SINTRAN_HDR then
            frame_tree:add(lapb_proto, tvb(SINTRAN_HDR),
                string.format("[Unknown subtype 0x%02X payload]", subtype))
        end
    end

    if body_name then
        return string.format("%d→%d %s  %s", src, dest, proto_nm, body_name)
    end
    return string.format("%d→%d %s", src, dest, proto_nm)
end

-- ── LAPB frame dissector ──────────────────────────────────────────────────────
-- unstuffed: full frame bytes between flags, after unstuffing.
-- Format: addr(1) + ctrl(1) + [info...] + FCS_lo(1) + FCS_hi(1)

local function dissect_lapb_frame(unstuffed, pinfo, frame_tree)
    local len = unstuffed:len()

    -- Minimum: addr(1) + ctrl(1) + FCS(2) = 4 bytes
    if len < 4 then
        frame_tree:add("[Frame too short]")
        return nil
    end

    local tvb = unstuffed:tvb("LAPB")

    local addr_byte = unstuffed:get_index(0)
    local ctrl_byte = unstuffed:get_index(1)
    local pf_set    = band(ctrl_byte, 0x10) ~= 0

    -- ── FCS check ────────────────────────────────────────────────────────────
    -- FCS stored little-endian at end: fcs = lo | (hi << 8)
    local fcs_lo   = unstuffed:get_index(len - 2)
    local fcs_hi   = unstuffed:get_index(len - 1)
    local fcs_rx   = fcs_lo | (fcs_hi << 8)
    local fcs_calc = bxor(crc16_ccitt(unstuffed, len - 2), 0xFFFF)
    local fcs_ok   = (fcs_rx == fcs_calc)

    -- info_len = bytes between ctrl and FCS
    local info_len = len - 4

    -- ── Address ──────────────────────────────────────────────────────────────
    -- ND extension (spec §3.1): bit 0x80 = ODD-info-length marker on I-frames.
    -- 0x89 = data + odd info, 0x09 = data + even info, 0x01 = link management,
    -- 0x07 = observed on some ACK I-frames (meaning unknown — tolerated).
    local addr_item = frame_tree:add(pf.addr, tvb(0, 1))
    local is_iframe = band(ctrl_byte, 0x01) == 0

    if is_iframe and band(addr_byte, 0x7F) == 0x09 then
        local flag_odd   = band(addr_byte, 0x80) ~= 0
        local actual_odd = (info_len % 2) == 1
        if flag_odd ~= actual_odd then
            -- A real ND machine SILENTLY DISCARDS an odd-length I-frame sent
            -- with 0x09 (before sequence processing → V(R) freezes, REJ
            -- deadlock). This mismatch is the bug signature of two live stalls.
            addr_item:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                "Odd-length address bit contradicts info parity: addr 0x%02X but info length %d is %s — real ND receivers DISCARD this frame",
                addr_byte, info_len, actual_odd and "ODD" or "EVEN"))
        end
    end

    -- ── Control ──────────────────────────────────────────────────────────────
    local ctrl_tree = frame_tree:add(pf.ctrl, tvb(1, 1))
    local summary

    if is_iframe then
        -- ── I-frame ──────────────────────────────────────────────────────────
        local ns = band(rshift(ctrl_byte, 1), 0x07)
        local nr = band(rshift(ctrl_byte, 5), 0x07)

        ctrl_tree:add(pf.ns,     tvb(1, 1))
        ctrl_tree:add(pf.pf_bit, tvb(1, 1))
        ctrl_tree:add(pf.nr,     tvb(1, 1))

        summary = string.format("I  N(S)=%d N(R)=%d%s", ns, nr, pf_set and " P" or "")

        if info_len > 0 then
            local info_tvb = unstuffed:subset(2, info_len):tvb("SINTRAN")
            local snt = dissect_sintran_info(info_tvb, pinfo, frame_tree)
            if snt then
                summary = summary .. "  " .. snt
            end
        end

    elseif band(ctrl_byte, 0x03) == 0x01 then
        -- ── S-frame ──────────────────────────────────────────────────────────
        -- Subtype MUST be taken from control & 0x0F (RR 0x1 / RNR 0x5 / REJ
        -- 0x9) — the three are indistinguishable by control & 0x03 alone, and
        -- REJ's low nibble equals the ND data address byte 0x09 (a proven
        -- log/parse trap). (control >> 2) & 3 below is the same decode:
        -- 0x1→0 RR, 0x5→1 RNR, 0x9→2 REJ.
        local stype = band(rshift(ctrl_byte, 2), 0x03)
        local nr    = band(rshift(ctrl_byte, 5), 0x07)
        local sname = (vs_stype[stype] or "?"):match("^(%S+)")

        ctrl_tree:add(pf.stype,  tvb(1, 1))
        ctrl_tree:add(pf.pf_bit, tvb(1, 1))
        ctrl_tree:add(pf.nr,     tvb(1, 1))

        summary = string.format("S  %s N(R)=%d%s", sname, nr, pf_set and " P/F" or "")

        -- ND extension (spec §3.3): RR carries the SENDER'S NODE NUMBER as a
        -- 2-byte big-endian info field (00 64 = node 100).
        if info_len == 2 then
            local ext = frame_tree:add(lapb_proto, tvb(2, 2),
                string.format("SINTRAN Node  [%d]", tvb(2, 2):uint()))
            ext:add(pf.node_id, tvb(2, 2))
            summary = summary .. string.format("  Node %d", tvb(2, 2):uint())
        end

    else
        -- ── U-frame ──────────────────────────────────────────────────────────
        local ctrl_mf = band(ctrl_byte, 0xEF)
        local uname   = vs_utype[ctrl_mf] or string.format("U?0x%02X", ctrl_mf)

        ctrl_tree:add(pf.pf_bit, tvb(1, 1))
        ctrl_tree:append_text(string.format("  [%s]", uname))

        summary = string.format("U  %s%s", uname, pf_set and " P/F" or "")

        -- ND extension (spec §3.3): SABM and UA carry the SENDER'S NODE NUMBER
        -- as a 2-byte big-endian info field.
        if info_len == 2 then
            local ext = frame_tree:add(lapb_proto, tvb(2, 2),
                string.format("SINTRAN Node  [%d]", tvb(2, 2):uint()))
            ext:add(pf.node_id, tvb(2, 2))
            summary = summary .. string.format("  Node %d", tvb(2, 2):uint())
        elseif ctrl_mf == 0x87 and info_len >= 3 then
            -- Standard FRMR info (3 bytes)
            frame_tree:add(lapb_proto, tvb(2, info_len), "FRMR Info")
        end
    end

    -- ── FCS ──────────────────────────────────────────────────────────────────
    local fcs_item = frame_tree:add_le(pf.fcs, tvb(len - 2, 2))
    if fcs_ok then
        fcs_item:append_text("  [correct]")
    else
        fcs_item:append_text(string.format("  [BAD — expected 0x%04X]", fcs_calc))
        fcs_item:add_expert_info(PI_CHECKSUM, PI_ERROR, "Bad FCS")
    end

    frame_tree:append_text("  " .. summary)
    return summary
end

-- ═════════════════════════════════════════════════════════════════════════════
-- ND LINK LAYER over the COSMOS Ethernet hub (TCP 5010)      [added 2026-08-11]
-- ═════════════════════════════════════════════════════════════════════════════
--
-- SCOPE NOTE, so nobody looks for this in the wrong half of the file: everything
-- above is HDLC. On HDLC the link layer is LAPB (address + control + FCS). The ND
-- link layer decoded below is a DIFFERENT link layer that exists only on the
-- Ethernet path - COSMOS carries its own sequenced, acknowledged link protocol
-- inside the LLC payload rather than relying on LLC1. The two never appear in the
-- same stream, which is why this is a second Proto with its own port binding and
-- not an extension of the LAPB path.
--
-- Everything ABOVE the ND link header is identical on both transports, so the
-- SINTRAN datagram is handed straight to dissect_sintran_info - the same code that
-- decodes it out of a LAPB I-frame, including the word-6 checksum validator.
--
-- TCP payload layout (verified against the captures listed below, and against the
-- working Python decoder kept beside them,
-- SINTRAN/XMSG/DOC/captures/ND-TO-ND-WRITE-2026-08-10/decode_hub.py):
--
--     0-1     hub framing: 2-byte BE length of the Ethernet frame that follows
--     +0-5    802.3 destination MAC
--     +6-11   802.3 source MAC        08:00:26 | system number LE | physical user
--     +12-13  802.3 length            = 3 (LLC) + 11 (ND link) + payload
--     +14-16  LLC  A8 A8 03
--     +17-27  the 11-byte ND link header
--     +28..   the SINTRAN datagram, exactly as on HDLC
--
-- Wireshark claims TCP 5010 as IPSICTL, so without the binding at the bottom of
-- this file a hub capture shows nothing useful.
--
-- ND link header layout - the field-by-field provenance is in the C# reference
-- implementation SINTRAN/XMSG/SRC/Xmsg.Ethernet/NdLinkHeader.cs, which this
-- decode is written to agree with (VERIFIED there over 96 frames, both
-- directions), and in
-- SINTRAN/XMSG/DOC/COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md:
--
--     +0   0x0B = 11 = the LENGTH of this header, NOT a magic constant. Every
--          earlier hunt for a fixed 0B02 in an ND binary failed because the sender
--          computes it. Still used as a signature byte here because it is 11 on
--          every frame observed.
--     +1   0x02, constant on every observed frame, meaning UNKNOWN
--     +2   the frame kind; its HIGH NIBBLE is the NPDU type, its low nibble is
--          NOT explained (three known kinds end in 0xF, data ends in 0x0)
--     +3   0x00, constant on every observed frame
--     +4   the send sequence number
--     +5-6 the SENDER's link id   (0x0000 on a connection request - no link yet)
--     +7-8 the RECEIVER's link id
--     +9-10 MEANING DEPENDS ON THE KIND: the payload length on a 0x20 data frame,
--          the sender's own system number on a 0x0F connection request, 0x0101
--          (UNKNOWN) on a 0x6F disconnect, 0x0000 on every acknowledgement. The
--          802.3 length is the authority on how many payload bytes are present.
--
-- Link ids are per-session and are neither the node number nor the system number
-- in the MAC. Where they come from is UNKNOWN.

local ndlink_proto = Proto("ndlink", "COSMOS ND link over the XMSG Ethernet hub")

-- Frame kinds with a CONFIRMED wire byte. Four more NPDU types exist whose wire
-- byte has never been captured, so they are deliberately absent rather than
-- guessed - an unrecognised value is shown raw, never rejected.
--
-- CHANGED 2026-08-24: the values and the names now come from the registry table
-- REG.nd_link_frame_kind, generated out of DOC/protocols/sintran-wire.json by
-- generate_lua.py. This list used to be typed here by hand and had already fallen
-- one value behind - the registry's ConnectionConfirm 0x1F was missing, so every
-- connection confirm in a capture read as "never captured". The two-letter tags
-- below are the only part still written here, because they are a display
-- convenience for the Info column and not a protocol fact.
local vs_nd_kind_tag = {
    [0x0F] = "CR",   -- connection request (no payload; sender link id 0)
    [0x1F] = "CC",   -- connection confirm
    [0x20] = "DT",   -- data (carries the SINTRAN datagram)
    [0x3F] = "AK",   -- acknowledge (carries the NEXT EXPECTED sequence)
    [0x60] = "DR",   -- disconnect request (low nibble UNVERIFIED)
    [0x6F] = "DR",   -- disconnect request by the network service
}

local vs_nd_kind = {}
for value, label in pairs(REG.nd_link_frame_kind_vs) do
    local tag = vs_nd_kind_tag[value]
    vs_nd_kind[value] = tag and (tag .. "  " .. label) or label
end

-- The NPDU type index in the kind byte's high nibble. VERIFIED 2026-08-03 by
-- carving the ENCOS monitor encos-mon-ii-b01.prog: its trace decoder dispatches
-- through an eight-entry jump table at ram:26ae. Indices 0 and 1 were read off the
-- code; 2-7 follow the format-string blob order and are STRONGLY INDICATED but not
-- individually read.
local vs_nd_npdu = {
    [0] = "CR (connection request)",
    [1] = "CC (connection confirm) - wire byte never captured",
    [2] = "DT (data)",
    [3] = "AK (acknowledge)",
    [4] = "WO (window) - wire byte never captured",
    [5] = "DR (disconnect request, by user) - never captured",
    [6] = "DR (disconnect request, by network service)",
    [7] = "DC (disconnect confirm) - never captured",
}

-- ── The two measured link-layer constants ────────────────────────────────────
-- MEASURED 2026-08-11 over the three real machine-to-machine captures in
-- SINTRAN/XMSG/DOC/captures/FA-READ-WRITE-2026-08-04/ (capture-list-files.txt 887
-- frames, capture-read.txt, capture-write.txt). Pinned in code by
-- NdLinkHeader.SequenceModulus / NdLinkLayer.SendWindow and held to the captures
-- by NdLinkCaptureConformanceTests.
--
-- SEVEN BITS, not eight: the highest sequence anywhere in the three captures is
-- 0x7F and not one frame has bit 7 set. The wrap is visible in
-- capture-list-files.txt at 02:20:10.52, where D102 sends 0x7F and its next data
-- frame is 0x00.
local ND_SEQ_MODULUS = 128

-- AT MOST FOUR frames go out before a side waits for an acknowledgement. The
-- four-frame burst is one request's whole answer arriving at once
-- (capture-read.txt 02:29:50):
--     .792  D102 -> D100  seq 44   36 bytes  short acknowledgement
--     .803                seq 45   52 bytes  reply
--     .810                seq 46  622 bytes  content, fragment 1
--     .810                seq 47  452 bytes  content, fragment 2
--     .827  D100 -> D102  acknowledges up to 44
--     .844                acknowledges up to 47
--
-- SIX. The four-frame read above is the widest in the TEXT captures, not overall:
-- a real D100 sends six before waiting in
-- DOC\captures\ND-TO-ND-WRITE-2026-08-10\readback-proves-content.pcapng (D100 to
-- D102, no emulated node involved). That is the widest across every pcapng under
-- DOC\captures\ with both ends real, and this dissector is what found it -
-- confirmed independently by decode_hub.py in Python.
--
-- A CAPTURE CANNOT TELL YOU THIS NUMBER. It moved three times in one day: 2 from
-- capture-list-files.txt alone (a listing sends no content message, so it tops out
-- at the short acknowledgement and the reply), 4 from all three text captures, 5
-- from one hub capture, 6 from all of them. Each time the reasoning was "this is
-- the largest a real machine sends", and each time a wider capture disagreed.
-- A capture shows what the traffic NEEDED, never what the protocol ALLOWS. So this
-- is a FLOOR on the real limit, not the limit. The true limit is in the ENCOS
-- firmware, not in any capture.
--
-- A window below the real value is not incorrect, only slower, so everything still
-- works and the error stays hidden - which is exactly why it survived three
-- corrections. Sweep the whole corpus in one pass; do not read one capture and
-- conclude.
--
-- There is NO credit field: the acknowledgement's trailing word is 0000 on every
-- captured acknowledgement from both machines, so the window cannot be negotiated.
-- A window NPDU does exist (index 4 above) and has never been captured.
--
-- Keep this equal to NdLinkLayer.SendWindow in SINTRAN\XMSG\SRC\Xmsg.Ethernet.
local ND_SEND_WINDOW = 6

-- Hub framing constants (offsets within one length-prefixed Ethernet frame).
local ND_LLC_OFFSET     = 14    -- after the two MACs and the 802.3 length
local ND_LINK_OFFSET    = 17    -- after the 3-byte LLC header
local ND_LINK_LENGTH    = 11
local ND_PAYLOAD_OFFSET = ND_LINK_OFFSET + ND_LINK_LENGTH   -- = 28
local ND_HUB_PORT       = 5010

-- ── ND link ProtoFields ──────────────────────────────────────────────────────
local nf = {}
nf.hublen    = ProtoField.uint16("ndlink.hublen",   "Hub frame length",      base.DEC)
nf.dstmac    = ProtoField.ether ("ndlink.dstmac",   "Destination MAC")
nf.srcmac    = ProtoField.ether ("ndlink.srcmac",   "Source MAC")
nf.len8023   = ProtoField.uint16("ndlink.len8023",  "802.3 length",          base.DEC)
nf.llc       = ProtoField.bytes ("ndlink.llc",      "LLC header (A8 A8 03)")
nf.hdrlen    = ProtoField.uint8 ("ndlink.hdrlen",   "Header length",         base.DEC)
nf.sig1      = ProtoField.uint8 ("ndlink.sig1",     "Constant 0x02 (meaning unknown)", base.HEX)
nf.kind      = ProtoField.uint8 ("ndlink.kind",     "Frame kind",            base.HEX, vs_nd_kind)
nf.npdu      = ProtoField.uint8 ("ndlink.npdu",     "NPDU type (kind high nibble)", base.DEC, vs_nd_npdu)
nf.kindlow   = ProtoField.uint8 ("ndlink.kindlow",  "Kind low nibble (NOT explained)", base.HEX)
nf.pad3      = ProtoField.uint8 ("ndlink.pad3",     "Constant 0x00",         base.HEX)
nf.seq       = ProtoField.uint8 ("ndlink.seq",      "Send sequence (7-bit)", base.DEC)
nf.srclink   = ProtoField.uint16("ndlink.srclink",  "Sender link id",        base.HEX)
nf.dstlink   = ProtoField.uint16("ndlink.dstlink",  "Receiver link id",      base.HEX)
nf.trailing  = ProtoField.uint16("ndlink.trailing", "Trailing field (meaning depends on kind)", base.HEX)
nf.plen      = ProtoField.uint16("ndlink.plen",     "Payload length",        base.DEC)
nf.sysno     = ProtoField.uint16("ndlink.sysno",    "Sender system number (from MAC)", base.DEC)
nf.backlog   = ProtoField.uint16("ndlink.backlog",  "Unacknowledged frames from this sender", base.DEC)
nf.dupof     = ProtoField.framenum("ndlink.dupof",  "Retransmission of frame")

ndlink_proto.fields = {
    nf.hublen, nf.dstmac, nf.srcmac, nf.len8023, nf.llc,
    nf.hdrlen, nf.sig1, nf.kind, nf.npdu, nf.kindlow, nf.pad3, nf.seq,
    nf.srclink, nf.dstlink, nf.trailing, nf.plen, nf.sysno,
    nf.backlog, nf.dupof,
}

-- ── Per-direction link state ─────────────────────────────────────────────────
--
-- TWO-PASS SAFETY. Wireshark dissects every frame at least twice (a first scan,
-- then again to build the detail pane), and applying a display filter starts the
-- whole sequence over. Counting a frame twice would invent a backlog that never
-- existed, which is exactly the fault this decode is meant to FIND - so:
--
--   • ndlink_frame_result caches the computed answer under the Wireshark frame
--     number. The link state is advanced ONLY when there is no cached answer for
--     that frame number, so a re-visit displays the stored answer instead of
--     re-counting.
--   • ndlink_proto.init() clears both tables. Wireshark calls it once at the start
--     of a dissection sequence, so a reload or a filter change rebuilds the state
--     from frame 1 rather than continuing on top of the previous scan's numbers.
--
-- ONE COPY PER FRAME. The hub is a BROADCAST hub: a frame sent once is forwarded
-- to every other member, so the same bytes appear on several TCP streams. Counting
-- all the copies is what made the ETH-WRITE-2026-08-09 numbers unusable.
--
-- decode_hub.py de-duplicates by keeping only the machine->hub direction
-- (destination port 5010), which is the originator. THAT RULE IS NOT SAFE HERE and
-- was tried first: it silently drops a whole direction whenever the capture holds
-- only ONE member's connection. Measured on
-- X25Emulator/pcap/ALLTEST-fa-connectto-102-100-103-2026-08-01.pcapng, which has a
-- single hub connection (TCP 41107): node 102's frames are all machine->hub and
-- node 100's arrive only as hub->machine, so 100's acknowledgements were never
-- read, 102's backlog climbed to 280 and every sequence past 128 was reported as a
-- retransmission of the one 128 frames earlier. Both symptoms were fabricated by
-- the counting rule, not present on the wire.
--
-- The rule used instead: for each MAC-to-MAC direction, remember the FIRST TCP
-- stream that carried it and count that direction only on that stream. Copies of
-- the same frame on the other streams are then skipped whatever the capture holds -
-- with every member captured the first-seen stream is the originator's own, and
-- with a single member captured it is the only copy there is. Either way each frame
-- is counted exactly once. The skipped copies are still decoded and displayed; they
-- just carry no backlog line.
local ndlink_state = {}         -- direction key -> { outstanding = { ... }, stream = n }
local ndlink_frame_result = {}  -- Wireshark frame number -> list of per-frame results

-- Identity of the TCP connection a copy arrived on, built from the port pair.
-- The Field extractor "tcp.stream" was tried first and is NOT reliable here: under
-- tshark -2 it does not yield a value at the point this dissector runs, so every
-- direction bound to the same placeholder, nothing was recognised as a fan-out copy
-- and the two passes disagreed. The port pair is on pinfo, always present, and each
-- hub member holds its own connection - which is the only distinction needed.
local function nd_stream_key(pinfo)
    return string.format("%d:%d", pinfo.src_port, pinfo.dst_port)
end

function ndlink_proto.init()
    ndlink_state = {}
    ndlink_frame_result = {}
end

-- An acknowledgement carries the NEXT EXPECTED sequence, not the one being
-- acknowledged (VERIFIED on every data frame in the captures; the same rule is
-- NdLinkHeader.AcknowledgeFor). So everything strictly BEFORE that value is
-- acknowledged. "Before" in a 128-value space is the half-space behind it -
-- anything further away is read as still ahead rather than as a huge backlog.
local function nd_seq_is_acknowledged(seq, next_expected)
    local dist = (next_expected - seq) % ND_SEQ_MODULUS
    return dist >= 1 and dist <= (ND_SEQ_MODULUS / 2)
end

local function nd_direction_state(key)
    local st = ndlink_state[key]
    if st == nil then
        st = { outstanding = {}, stream = nil }
        ndlink_state[key] = st
    end
    return st
end

-- True when this stream is the one that owns the direction, i.e. the first stream
-- the direction was ever seen on. See the de-duplication note above.
local function nd_owns_direction(key, stream)
    local st = nd_direction_state(key)
    if st.stream == nil then
        st.stream = stream
    end
    return st.stream == stream
end

-- Short fingerprint of a frame's payload, so a repeat can be reported as
-- "identical bytes" rather than merely "same sequence". The skill's diagnosis
-- order is explicit that the raw bytes are what settles it: identical sequence AND
-- identical bytes is a retransmission, not a repeated request.
local function nd_fingerprint(tvb, off, len)
    if len <= 0 then return "" end
    local n = math.min(len, 24)
    if off + n > tvb:len() then return "" end
    return string.format("%d:%s", len, tostring(tvb(off, n):bytes()))
end

-- Advance the link state for ONE originator frame and return what to display.
local function nd_advance_state(dirkey, revkey, kind, seq, framenum, finger)
    local result = {}
    if kind == 0x20 then
        local st = nd_direction_state(dirkey)
        -- A repeat of a sequence still sitting in the backlog is never normal
        -- here: it means the sender has not seen an acknowledgement. This one
        -- distinction collapsed what looked like a pile of file-access defects
        -- into a single link-layer fault, and it cost two nights to find by hand.
        for i = 1, #st.outstanding do
            local e = st.outstanding[i]
            if e.seq == seq then
                result.dup_of = e.frame
                result.dup_identical = (e.finger == finger)
                break
            end
        end
        st.outstanding[#st.outstanding + 1] =
            { seq = seq, frame = framenum, finger = finger }
        result.backlog = #st.outstanding
    elseif kind == 0x3F then
        -- The acknowledgement travels in the OPPOSITE direction to the frames it
        -- clears, so it is the reverse key's backlog that shrinks.
        local st = nd_direction_state(revkey)
        local kept = {}
        for i = 1, #st.outstanding do
            local e = st.outstanding[i]
            if not nd_seq_is_acknowledged(e.seq, seq) then
                kept[#kept + 1] = e
            end
        end
        st.outstanding = kept
        result.peer_backlog = #kept
    end
    return result
end

-- Dissect the 11-byte ND link header and whatever it carries.
--
-- SPLIT OUT 2026-08-24. This used to be the tail of dissect_nd_hub_frame, which
-- could only be reached through the hub's own TCP framing. A pcap taken straight
-- off the Ethernet segment has no hub length prefix and Wireshark strips the MACs
-- and the LLC header for us, so the same eleven bytes arrive by a completely
-- different route. Splitting the function is what lets ONE decode serve both
-- transports rather than a second copy drifting away from this one.
--
--   tvb    the buffer the header sits in
--   h      byte offset of the header's first byte (the 0x0B length byte)
--   avail  how many bytes are left in THIS frame from h onwards, so the payload
--          can never be read past the end of the frame it belongs to
--   parent the tree item to hang the header under
--
-- Returns a short summary string for the Info column, or nil.
local function dissect_nd_link_pdu(tvb, pinfo, parent, h, avail, result)
    local ft = parent
    if avail < ND_LINK_LENGTH then
        ft:append_text(string.format("  [too short for an ND link header: %d bytes]", avail))
        return nil
    end

    local hdrlen   = tvb(h,     1):uint()
    local kind     = tvb(h + 2, 1):uint()
    local seq      = tvb(h + 4, 1):uint()
    local srclink  = tvb(h + 5, 2):uint()
    local dstlink  = tvb(h + 7, 2):uint()
    local trailing = tvb(h + 9, 2):uint()

    -- An unrecognised kind is reported as UNKNOWN and nothing is invented for it.
    -- segment.pcap of 2026-08-24 holds a 0x70 that no registry entry covers; the
    -- only thing said about it is its high nibble, which the NPDU table below
    -- reads with its own caveat.
    local kind_nm = vs_nd_kind[kind]
                    or string.format("?? kind 0x%02X UNKNOWN - not in the protocol registry", kind)
    local lt = ft:add(ndlink_proto, tvb(h, ND_LINK_LENGTH),
                  string.format("ND link header  [%s  seq %d]", kind_nm, seq))

    local hl_item = lt:add(nf.hdrlen, tvb(h, 1))
    hl_item:append_text("  [the header LENGTH, not a magic constant]")
    if hdrlen ~= ND_LINK_LENGTH then
        hl_item:add_expert_info(PI_MALFORMED, PI_WARN, string.format(
            "ND link header length byte is %d, expected %d", hdrlen, ND_LINK_LENGTH))
    end
    lt:add(nf.sig1,  tvb(h + 1, 1))
    lt:add(nf.kind,  tvb(h + 2, 1))
    local npdu_item = lt:add(nf.npdu, tvb(h + 2, 1), (kind >> 4) & 0x0F)
    npdu_item:set_generated()
    local low_item = lt:add(nf.kindlow, tvb(h + 2, 1), kind & 0x0F)
    low_item:set_generated()
    lt:add(nf.pad3,  tvb(h + 3, 1))

    local seq_item = lt:add(nf.seq, tvb(h + 4, 1))

    -- ── Check 1: the sequence is SEVEN bits ──────────────────────────────────
    -- No real ND puts bit 7 on the wire (see ND_SEQ_MODULUS above). The check is
    -- deliberately limited to DATA frames: those are the ones our own layer used to
    -- mint, and it wrapped at 256 until 2026-08-11 - the live run of 2026-08-10
    -- reached 124 and stopped four frames short of emitting a 0x80. Acknowledgement
    -- sequences were also all inside seven bits in the three captures, but an
    -- acknowledgement only ever echoes a number the peer chose, so flagging it
    -- would report the same fault twice.
    if (seq & 0x80) ~= 0 then
        seq_item:append_text("  [bit 7 SET]")
        if kind == 0x20 then
            seq_item:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                "ND link data sequence 0x%02X has bit 7 set - the sequence is SEVEN bits " ..
                "(measured: highest value in three real captures is 0x7F, wrap 0x7F -> 0x00). " ..
                "No real ND emits this.", seq))
        end
    end

    lt:add(nf.srclink, tvb(h + 5, 2))
    lt:add(nf.dstlink, tvb(h + 7, 2))

    -- Offsets +9..+10 are only a length on a data frame. On a connection request
    -- they carried the sender's own system number, on a 0x6F disconnect 0x0101, and
    -- 0x0000 on every acknowledgement - reading them as a length on a connection
    -- request would try to parse 102 bytes of Ethernet padding as a message.
    local plen = 0
    if kind == 0x20 then
        plen = trailing
        lt:add(nf.plen, tvb(h + 9, 2))
    else
        local tr_item = lt:add(nf.trailing, tvb(h + 9, 2))
        if kind == 0x3F then
            tr_item:append_text("  [always 0000 - there is NO credit field, the window cannot be negotiated]")
        elseif kind == 0x0F then
            tr_item:append_text("  [connection request: the sender's own system number, NOT a length]")
        else
            tr_item:append_text("  [meaning UNKNOWN for this kind - the 802.3 length is the authority]")
        end
    end

    -- ── Checks 2 and 3: backlog and retransmission ───────────────────────────
    if result ~= nil then
        if result.backlog ~= nil then
            local bl = lt:add(nf.backlog, tvb(h + 4, 1), result.backlog)
            bl:set_generated()
            bl:append_text(string.format("  [send window is %d]", ND_SEND_WINDOW))
            if result.backlog > ND_SEND_WINDOW then
                bl:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                    "%d frames unacknowledged from this sender - the widest any real ND has been " ..
                    "seen to send before waiting is %d. A backlog that CLIMBS and never comes " ..
                    "down is THE symptom: the peer starts retransmitting everything and it reads " ..
                    "as a pile of application defects one layer up.",
                    result.backlog, ND_SEND_WINDOW))
            end
        end
        if result.dup_of ~= nil then
            local du = lt:add(nf.dupof, tvb(h + 4, 1), result.dup_of)
            du:set_generated()
            du:add_expert_info(PI_SEQUENCE, PI_WARN, string.format(
                "ND link sequence %d was already sent in frame %d and is still unacknowledged%s. " ..
                "A peer repeating a data frame is never normal here - it means it has not seen " ..
                "an acknowledgement. Check the backlog before looking any higher up.",
                seq, result.dup_of,
                result.dup_identical and " (BYTE-FOR-BYTE IDENTICAL)"
                                      or " (same sequence, DIFFERENT bytes)"))
        end
        if result.hub_copy then
            lt:append_text("  [hub fan-out copy - counted on the stream that owns this direction]")
        end
    end

    -- ── Payload: the ordinary SINTRAN datagram ───────────────────────────────
    -- Only a data frame carries one. Control frames have 802.3 length 0x000E =
    -- 3 + 11 + 0 and every byte after the eleventh is Ethernet padding to the
    -- 60-byte minimum; earlier readings that assigned meaning to that tail were
    -- reading padding.
    local summary = string.format("%s seq=%d", kind_nm:match("^(%S+)") or "?", seq)
    if kind == 0x20 and plen > 0 then
        local take = math.min(plen, avail - ND_LINK_LENGTH)
        if take > 0 then
            local payload = tvb(h + ND_LINK_LENGTH, take):tvb("SINTRAN")
            local snt = dissect_sintran_info(payload, pinfo, ft)
            if snt then summary = summary .. "  " .. snt end
        end
    end
    return summary
end

-- Dissect ONE length-prefixed Ethernet frame out of the hub's TCP stream.
-- The hub framing and the 802.3 header are peeled here; the ND link header and
-- everything above it is the shared decode above.
-- Returns a short summary string for the Info column.
local function dissect_nd_hub_frame(tvb, pinfo, tree, off, flen, result)
    local ft = tree:add(ndlink_proto, tvb(off - 2, flen + 2), "ND hub frame")

    ft:add(nf.hublen, tvb(off - 2, 2))
    if flen < ND_PAYLOAD_OFFSET then
        ft:append_text(string.format("  [too short for an ND link frame: %d bytes]", flen))
        return nil
    end

    ft:add(nf.dstmac,  tvb(off,      6))
    local mac_item = ft:add(nf.srcmac, tvb(off + 6,  6))
    ft:add(nf.len8023, tvb(off + 12, 2))

    -- The MAC carries the sender's system number in bytes 3-4 in REVERSED byte
    -- order (ND-60.197.01 section 2.4), i.e. little-endian - the opposite order to
    -- the same number in the SINTRAN header two layers up.
    local sysno = tvb(off + 9, 1):uint() + tvb(off + 10, 1):uint() * 0x100
    local sys_item = ft:add(nf.sysno, tvb(off + 9, 2), sysno)
    sys_item:set_generated()
    mac_item:append_text(string.format("  [system %d]", sysno))

    if tvb(off + ND_LLC_OFFSET, 3):bytes():tohex() ~= "A8A803" then
        ft:append_text("  [not an ND/COSMOS LLC frame - no A8 A8 03]")
        return nil
    end
    ft:add(nf.llc, tvb(off + ND_LLC_OFFSET, 3))

    return dissect_nd_link_pdu(tvb, pinfo, ft,
               off + ND_LINK_OFFSET, flen - ND_LINK_OFFSET, result)
end

function ndlink_proto.dissector(buffer, pinfo, tree)
    pinfo.cols.protocol = "ND-LINK"

    local length = buffer:len()
    if length < 2 then return 0 end

    local root = tree:add(ndlink_proto, buffer(), "COSMOS ND link over the XMSG Ethernet hub")
    local offset = 0

    -- The link opens with a 5-byte greeting "RETH" + a version byte, which is NOT
    -- length-prefixed. A decoder that assumes frames from byte zero desynchronises
    -- and then reads a "frame length" out of the middle of a MAC address. It only
    -- shows on a connection that OPENS during the capture - machines already
    -- connected are joined mid-stream with no greeting in sight, so a decoder can
    -- look perfect and still be broken.
    if length >= 5 and buffer(0, 4):string() == "RETH" then
        root:add(ndlink_proto, buffer(0, 5),
            string.format("Hub greeting  [\"RETH\" version %d]", buffer(4, 1):uint()))
        offset = 5
    end

    local stream = nd_stream_key(pinfo)

    local results = ndlink_frame_result[pinfo.number]
    local computing = false
    if results == nil then
        results = {}
        ndlink_frame_result[pinfo.number] = results
        computing = true
    end

    local idx = 0
    local summaries = {}

    while offset + 2 <= length do
        local flen = buffer(offset, 2):uint()
        if flen == 0 or flen > 2000 then
            -- Not a length we can trust. Say so rather than silently resyncing on
            -- a guess - a wrong resync produces output that reads like data.
            root:add(ndlink_proto, buffer(offset),
                string.format("[Implausible hub frame length %d - stream out of step]", flen))
            break
        end
        if offset + 2 + flen > length then
            pinfo.desegment_len    = DESEGMENT_ONE_MORE_SEGMENT
            pinfo.desegment_offset = offset
            break
        end

        idx = idx + 1
        local result
        if computing then
            -- Read the fields the state machine needs before advancing it.
            local body = offset + 2
            if flen >= ND_PAYLOAD_OFFSET
                and buffer(body + ND_LLC_OFFSET, 3):bytes():tohex() == "A8A803" then
                local h    = body + ND_LINK_OFFSET
                local kind = buffer(h + 2, 1):uint()
                local seq  = buffer(h + 4, 1):uint()
                -- Direction key: the two MACs, so a frame is tracked by WHO sent it
                -- rather than by which TCP stream carried the copy.
                local dirkey = tostring(buffer(body + 6, 6):bytes()) .. ">" ..
                               tostring(buffer(body,     6):bytes())
                local revkey = tostring(buffer(body,     6):bytes()) .. ">" ..
                               tostring(buffer(body + 6, 6):bytes())
                if nd_owns_direction(dirkey, stream) then
                    local plen = (kind == 0x20) and buffer(h + 9, 2):uint() or 0
                    local finger = nd_fingerprint(buffer, body + ND_PAYLOAD_OFFSET,
                                       math.min(plen, flen - ND_PAYLOAD_OFFSET))
                    result = nd_advance_state(dirkey, revkey, kind, seq, pinfo.number, finger)
                else
                    result = { hub_copy = true }
                end
            else
                result = {}
            end
            results[idx] = result
        else
            result = results[idx]
        end

        local summary = dissect_nd_hub_frame(
            buffer, pinfo, root, offset + 2, flen, result)
        if summary then summaries[#summaries + 1] = summary end

        offset = offset + 2 + flen
    end

    if #summaries > 0 then
        pinfo.cols.info:set(table.concat(summaries, " | "))
    end
    return length
end

-- =============================================================================
-- ND LINK ON A RAW ETHERNET CAPTURE (classic pcap, link type 1)  [added 2026-08-24]
-- =============================================================================
--
-- WHY A SECOND ENTRY POINT. The block above reads the COSMOS hub's TCP stream:
-- every Ethernet frame arrives length-prefixed inside a TCP connection to port
-- 5010, so the decoder has to peel the hub framing, the two MACs and the LLC
-- header itself. A capture taken with "xmsghub --capture" is a completely
-- different thing: it is a plain pcap of the segment, link type 1, and Wireshark
-- decodes the 802.3 header and the LLC header on its own before handing us a
-- payload. Feeding such a file to the TCP decoder gets NOTHING - there is no TCP.
--
-- MEASURED on DOC/captures/XMSG-DEGRADE-2026-08-24/segment.pcap:
--
--     6 bytes  destination MAC
--     6 bytes  source MAC      08:00:26 | system number, low byte first | user
--     2 bytes  802.3 LENGTH (not an ethertype - it is below 0x0600)
--     3 bytes  LLC  A8 A8 03   DSAP 0xA8, SSAP 0xA8, control 0x03 (unnumbered
--                              information). Wireshark's own LLC dissector reads
--                              these three and dispatches on the DSAP.
--     n bytes  the ND link header and its payload - EXACTLY the bytes the hub
--              path sees at its offset 17
--
-- So the binding is one line at the bottom of this file: llc.dsap 0xA8 -> here.
-- Wireshark hands us a buffer that already starts at the 0x0B header-length byte
-- AND is already trimmed to the 802.3 length, so there is no Ethernet padding to
-- step over - checked on segment.pcap, where a control frame arrives as exactly
-- 11 bytes out of a 60-byte frame and a data frame as 11 + the payload length.
--
-- DSAP 0xA8 is not a registered IEEE SAP that Wireshark knows, so nothing else
-- claims it and the binding cannot steal another protocol's frames.

local ndeth_proto = Proto("ndlink_eth", "COSMOS ND link on 802.3 (raw segment capture)")

-- No fields of its own: every field is already registered on ndlink_proto and a
-- registered field can be added to any tree. Declaring them twice would be a
-- second set of filter names for the same bytes.
ndeth_proto.fields = {}

-- Both entry points share ndlink_state / ndlink_frame_result, so both need the
-- per-dissection reset. Wireshark calls init on every protocol before a
-- dissection sequence starts, so clearing the same two tables twice is harmless.
function ndeth_proto.init()
    ndlink_state = {}
    ndlink_frame_result = {}
end

-- "08:00:26:64:00:00" -> 100. The system number is MAC bytes 3 and 4 with the low
-- byte FIRST (ND-60.197.01 section 2.4), the opposite order to the same number in
-- the SINTRAN header two layers up. Returns nil when the string is not a MAC.
local function nd_sysno_from_mac(mac)
    local b3, b4 = mac:match("^%x%x:%x%x:%x%x:(%x%x):(%x%x):%x%x$")
    if b3 == nil then return nil end
    return tonumber(b3, 16) + tonumber(b4, 16) * 0x100
end

function ndeth_proto.dissector(buffer, pinfo, tree)
    local length = buffer:len()
    if length < ND_LINK_LENGTH then return 0 end

    pinfo.cols.protocol = "ND-LINK"

    local root = tree:add(ndeth_proto, buffer(),
                     "COSMOS ND link (raw 802.3 segment capture)")

    local src = tostring(pinfo.dl_src)
    local dst = tostring(pinfo.dl_dst)

    local sysno = nd_sysno_from_mac(src)
    if sysno ~= nil then
        local sys_item = root:add(nf.sysno, buffer(0, 1), sysno)
        sys_item:set_generated()
        sys_item:append_text("  [read out of the source MAC, low byte first]")
    end

    -- The same two-pass guard the hub path uses: Wireshark dissects every frame at
    -- least twice, and counting a frame twice would invent the very backlog this
    -- decode exists to find. See the long note beside ndlink_state.
    --
    -- The stream key is the constant "eth" because a segment capture has no TCP
    -- connection to tell copies apart - and it needs none: the hub writes each
    -- frame to the file once, so there are no fan-out copies to skip. (Checked on
    -- segment.pcap: no frame appears twice.)
    local results = ndlink_frame_result[pinfo.number]
    local result
    if results == nil then
        results = {}
        ndlink_frame_result[pinfo.number] = results

        local kind = buffer(2, 1):uint()
        local seq  = buffer(4, 1):uint()
        local dirkey = src .. ">" .. dst
        local revkey = dst .. ">" .. src
        if nd_owns_direction(dirkey, "eth") then
            local plen = (kind == 0x20) and buffer(9, 2):uint() or 0
            local finger = nd_fingerprint(buffer, ND_LINK_LENGTH,
                               math.min(plen, length - ND_LINK_LENGTH))
            result = nd_advance_state(dirkey, revkey, kind, seq, pinfo.number, finger)
        else
            result = { hub_copy = true }
        end
        results[1] = result
    else
        result = results[1]
    end

    local summary = dissect_nd_link_pdu(buffer, pinfo, root, 0, length, result)
    if summary then
        pinfo.cols.info:set(summary)
    end
    return length
end

-- ── TCP dissector ─────────────────────────────────────────────────────────────

function lapb_proto.dissector(buffer, pinfo, tree)
    pinfo.cols.protocol = "LAPB"

    local offset    = 0
    local length    = buffer:len()
    local root      = tree:add(lapb_proto, buffer(), "LAPB/SINTRAN over TCP")
    local summaries = {}

    while offset < length do
        -- Seek opening 0x7E flag
        while offset < length and buffer(offset, 1):uint() ~= 0x7E do
            offset = offset + 1
        end
        if offset >= length then break end

        local frame_start = offset
        offset = offset + 1

        -- Skip interframe fill (consecutive 0x7E flags are valid HDLC).
        -- Track content_start: the first non-flag byte, which is where the
        -- actual frame data begins (addr byte).  Using frame_start+1 here would
        -- wrongly include fill flags in the payload passed to unstuff/FCS.
        while offset < length and buffer(offset, 1):uint() == 0x7E do
            offset = offset + 1
        end
        if offset >= length then break end

        local content_start = offset  -- first real frame byte (addr)

        -- Seek closing 0x7E flag
        while offset < length and buffer(offset, 1):uint() ~= 0x7E do
            offset = offset + 1
        end

        if offset >= length then
            pinfo.desegment_len    = DESEGMENT_ONE_MORE_SEGMENT
            pinfo.desegment_offset = frame_start
            return
        end

        local frame_len  = offset - frame_start + 1
        local raw_frame  = buffer(frame_start, frame_len)
        local frame_tree = root:add(lapb_proto, raw_frame, "LAPB Frame")
        frame_tree:add(pf.frame_raw, raw_frame)

        local payload_len = offset - content_start  -- excludes closing 0x7E
        if payload_len > 0 then
            local payload   = buffer(content_start, payload_len)
            local unstuffed = unstuff(payload)
            local summary   = dissect_lapb_frame(unstuffed, pinfo, frame_tree)
            if summary then
                summaries[#summaries + 1] = summary
            end
        end


        offset = offset + 1
    end

    if #summaries > 0 then
        pinfo.cols.info:set(table.concat(summaries, " | "))
    end
end

-- ── Heuristic probe ──────────────────────────────────────────────────────────
-- Scans the first TCP segment for a plausible LAPB frame:
--   • 0x7E open flag
--   • unstuffed content passes CRC-16-CCITT
--   • info bytes start with 0x21 0x12 or 0x21 0x13  (SINTRAN marker)
-- This fingerprint is extremely specific — false positives are negligible.

local function lapb_heuristic(buffer, pinfo, tree)
    local len = buffer:len()
    if len < 6 then return false end  -- 0x7E + addr + ctrl + FCS(2) + 0x7E minimum

    -- Find opening 0x7E
    local i = 0
    while i < len and buffer(i, 1):uint() ~= 0x7E do i = i + 1 end
    if i >= len then return false end
    i = i + 1

    -- Skip consecutive 0x7E (interframe fill)
    while i < len and buffer(i, 1):uint() == 0x7E do i = i + 1 end
    if i >= len then return false end

    local content_start = i

    -- Find closing 0x7E
    while i < len and buffer(i, 1):uint() ~= 0x7E do i = i + 1 end
    if i >= len then return false end  -- no closing flag in this segment

    local content_len = i - content_start
    if content_len < 4 then return false end  -- need addr+ctrl+FCS(2) at minimum

    -- Unstuff the content between flags
    local payload_tvb = buffer(content_start, content_len)
    local unstuffed   = unstuff(payload_tvb)
    local ulen        = unstuffed:len()
    if ulen < 4 then return false end

    -- Verify FCS
    local fcs_rx   = unstuffed:get_index(ulen - 2) | (unstuffed:get_index(ulen - 1) << 8)
    local fcs_calc = crc16_ccitt(unstuffed, ulen - 2) ~ 0xFFFF
    if fcs_rx ~= fcs_calc then return false end

    -- Must be an I-frame (bit 0 of ctrl == 0) with SINTRAN info
    local ctrl = unstuffed:get_index(1)
    if (ctrl & 1) ~= 0 then return false end

    -- Check SINTRAN marker: info starts at byte 2, ends before FCS
    local info_len = ulen - 4  -- addr(1)+ctrl(1)+FCS(2)
    if info_len < 2 then return false end
    local mark1 = unstuffed:get_index(2)
    local mark2 = unstuffed:get_index(3)
    if mark1 ~= 0x21 then return false end
    if mark2 ~= 0x13 and mark2 ~= 0x12 then return false end

    -- Looks good — claim the stream and dissect
    lapb_proto.dissector(buffer, pinfo, tree)
    return true
end

lapb_proto:register_heuristic("tcp", lapb_heuristic)

-- ── Port binding ─────────────────────────────────────────────────────────────
-- Port binding guarantees the dissector is always called for known ports,
-- which means pinfo.desegment_len works reliably for TCP reassembly.
-- The heuristic alone is unreliable: if the very first TCP segment on a stream
-- starts with a large frame that has no closing 0x7E in that segment, the
-- heuristic probe returns false and the stream is never claimed.
-- Keep both: port binding for known ports, heuristic for unknown ports.

local tcp_table = DissectorTable.get("tcp.port")

-- The COSMOS Ethernet hub. Wireshark's own table claims 5010 as IPSICTL, so this
-- binding is what makes a hub capture readable at all. Do NOT instead force
-- "-d tcp.port==5010,hdlc_lapb": it looks like it works - thousands of frames turn
-- into LAPB - but the decode is misaligned and the output is noise that reads like
-- data (node numbers come out as 53306 -> 1 instead of 100/102/103).
tcp_table:add(ND_HUB_PORT, ndlink_proto)

-- A raw capture of the Ethernet segment (classic pcap, link type 1) has no TCP at
-- all. Wireshark reads the 802.3 header and the LLC header itself and then looks
-- the DSAP up in this table, so one entry is the whole binding. VERIFIED against
-- DOC/captures/XMSG-DEGRADE-2026-08-24/segment.pcap: the buffer handed over starts
-- at the ND link header's 0x0B length byte and is already trimmed to the 802.3
-- length. DSAP 0xA8 is not a SAP Wireshark has any other use for.
local llc_table = DissectorTable.get("llc.dsap")
llc_table:add(0xA8, ndeth_proto)

tcp_table:add(10362, lapb_proto)
tcp_table:add(10364, lapb_proto)
tcp_table:add(24182, lapb_proto)
tcp_table:add(17230, lapb_proto)
tcp_table:add(17237, lapb_proto)
