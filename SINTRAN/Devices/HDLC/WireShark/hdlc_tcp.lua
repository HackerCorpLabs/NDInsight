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
--       baseLow = (seed − F2low) & 0xFF
--       epoch   = (Flags1 − baseLow + 0xFF) >> 8
--       channel = 0xDE − (XMCSM >> 24) − epoch  == the Protocol-ID byte
--       Flags2  == XMCSM >> 16
--     VERIFIED 753/753 data frames — mismatches get expert-info.
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

-- Envelope arithmetic (derived from the seed model, spec §18.5 / §6 / §5.1)
pf.env_seed   = ProtoField.uint8 ("xmsg.seed",      "Derived link seed", base.HEX)
pf.env_epoch  = ProtoField.uint8 ("xmsg.epoch",     "Epoch (counter wraps)", base.DEC)
pf.env_chan   = ProtoField.uint8 ("xmsg.expchan",   "Expected channel",  base.HEX)
pf.ack_trail  = ProtoField.uint8 ("xmsg.acktrail",  "ACK trailing byte", base.HEX)
pf.ack_sack   = ProtoField.uint8 ("xmsg.sack",      "S_ack (= seed + 0x0B)", base.HEX)
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
pf.dc_cmd        = ProtoField.uint32("xmsg.xmcsm",  "XMCSM (class/service)",base.HEX, vs_dc_cmd)
-- XMLEN is 16-bit: sub-header offset 17 (formerly decoded as a "pad" byte) is
-- the HIGH byte of the user-data length. Proven by 255-byte output chunks:
-- bytes 01 01 → 0x0101 = 257 = 2-byte BDAT header + 255 data (TAD spec §22.3).
pf.dc_tlen       = ProtoField.uint16("xmsg.xmlen",  "XMLEN (user data len, 16-bit)", base.DEC)
pf.dc_trailer    = ProtoField.bytes ("xmsg.userdata","User Data")
pf.xmsg_dsy      = ProtoField.uint16("xmsg.xmdsy",  "XMDSY (dest system)",  base.DEC)
pf.xmsg_dpt      = ProtoField.uint16("xmsg.xmdpt",  "XMDPT (dest port)",    base.DEC)
pf.xmsg_ssy      = ProtoField.uint16("xmsg.xmssy",  "XMSSY (src system)",   base.DEC)
pf.xmsg_spt      = ProtoField.uint16("xmsg.xmspt",  "XMSPT (src port)",     base.DEC)

-- XROUT letter TLVs (XMCSM 0x04000041 = XSLET; spec §18.8 S10a/S10b)
pf.letter_name   = ProtoField.string("xmsg.letter.name",   "Server Name")
pf.letter_target = ProtoField.string("xmsg.letter.target", "Target System Name")

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
    pf.env_seed, pf.env_epoch, pf.env_chan,
    pf.ack_trail, pf.ack_sack, pf.reach_trail,
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

    -- ── Envelope validation (spec §18.5 seed model, VERIFIED 753/753) ─────────
    -- seed    = (Counter + Flags1 + F2low) & 0xFF   (per-link constant)
    -- baseLow = (seed − F2low) & 0xFF
    -- epoch   = (Flags1 − baseLow + 0xFF) >> 8
    -- channel = 0xDE − (XMCSM >> 24) − epoch  == the header Protocol-ID byte
    -- Also: Flags2 == XMCSM >> 16 on every data frame.
    -- On a relayed hop (marker 0x12) the relay re-stamps Counter +1, so the
    -- derived seed reads seed+1 (the "relay lane") — the arithmetic still
    -- self-checks because seed is derived from the same frame.
    if ctx then
        local f2low   = band(ctx.flags2, 0xFF)
        local seed    = band(ctr1 + ctx.flags1 + f2low, 0xFF)
        local baselow = band(seed - f2low, 0xFF)
        local epoch   = rshift(ctx.flags1 - baselow + 0xFF, 8)
        local class   = band(rshift(cmd_word, 24), 0xFF)
        local expchan = band(0xDE - class - epoch, 0xFF)

        local env = tree:add(lapb_proto, tvb(off, 1), "Envelope (derived, spec §18.5)")
        env:set_generated()
        env:add(pf.env_seed,  tvb(off, 1), seed):set_generated()
        env:add(pf.env_epoch, tvb(off, 1), epoch):set_generated()
        local ch_item = env:add(pf.env_chan, tvb(off, 1), expchan)
        ch_item:set_generated()
        env:append_text(string.format("  [seed=0x%02X epoch=%d expected-chan=0x%02X]",
            seed, epoch, expchan))

        if ctx.proto_id ~= expchan then
            ch_item:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                "Channel mismatch: Protocol ID 0x%02X but seed model expects 0x%02X",
                ctx.proto_id, expchan))
        end
        if ctx.flags2 ~= band(rshift(cmd_word, 16), 0xFFFF) then
            cmd_item:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                "Flags2 0x%04X != XMCSM class word 0x%04X (rule: Flags2 == XMCSM>>16)",
                ctx.flags2, band(rshift(cmd_word, 16), 0xFFFF)))
        end
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
        else
            -- Unknown command — show raw trailer, then try TAD chain.
            -- Label both branches so it's obvious which decode path was taken.
            local raw = tree:add(pf.dc_trailer, tvb(trailer_off, tlen))
            raw:append_text("  [unknown XMCSM, trying TAD chain fallback]")
            dissect_tad(tvb, pinfo, tree, trailer_off)
        end
    end

    pinfo.cols.info:append(string.format(" %s[%s ctr=0x%02X]", proto_label, cmd_name, ctr1))
end

-- ── SINTRAN info dissector ────────────────────────────────────────────────────
-- tvb covers the LAPB info bytes (addr+ctrl stripped, FCS already stripped).

local function dissect_sintran_info(tvb, pinfo, frame_tree)
    local len = tvb:len()

    if len < SINTRAN_HDR then
        frame_tree:add(lapb_proto, tvb(0), string.format("[Info too short for SINTRAN: %d bytes]", len))
        return nil
    end

    local mark2 = tvb(1, 1):uint()
    if tvb(0, 1):uint() ~= 0x21 or (mark2 ~= 0x13 and mark2 ~= 0x12) then
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
        -- ── ACK (delivery acknowledgment), spec §6 closed form ────────────────
        -- Sent OPPOSITE to the data frame it acknowledges; Flags1 ECHOES that
        -- frame's datagram sequence. The single trailing byte is derived state:
        --   S_ack    = (trailing + Flags1 + F2low) & 0xFF  == link seed + 0x0B
        --   baseLow  = (S_ack − F2low) & 0xFF
        --   epoch    = (Flags1 − baseLow + 0xFF) >> 8
        --   channel  = 0xDE − epoch     (the peer VALIDATES this channel!)
        -- Flags2 may be 0x0001 or 0x0002 (both valid; receivers accept both).
        sub_item:append_text("  [ACK / delivery acknowledgment]")
        f1_item:append_text(string.format("  [acknowledged datagram seq = %d]", flags1))
        hdr:append_text(string.format("  ACK seq=%d", flags1))
        if flags2 ~= 0x0001 and flags2 ~= 0x0002 then
            f2_item:add_expert_info(PI_PROTOCOL, PI_NOTE,
                "Unusual ACK Flags2 (corpus shows only 0x0001 / 0x0002)")
        end
        if len >= SINTRAN_HDR + 1 then
            local trailing = tvb(SINTRAN_HDR, 1):uint()
            local f2low   = band(flags2, 0xFF)
            local sack    = band(trailing + flags1 + f2low, 0xFF)
            local baselow = band(sack - f2low, 0xFF)
            local epoch   = rshift(flags1 - baselow + 0xFF, 8)
            local expchan = band(0xDE - epoch, 0xFF)
            local seed    = band(sack - 0x0B, 0xFF)

            local at = frame_tree:add(pf.ack_trail, tvb(SINTRAN_HDR, 1))
            at:append_text(string.format(
                "  [S_ack=0x%02X (link seed 0x%02X + 0x0B), epoch=%d, expected ack chan=0x%02X]",
                sack, seed, epoch, expchan))
            local sa = frame_tree:add(pf.ack_sack, tvb(SINTRAN_HDR, 1), sack)
            sa:set_generated()
            if proto_id ~= expchan then
                at:add_expert_info(PI_PROTOCOL, PI_WARN, string.format(
                    "ACK channel mismatch: Protocol ID 0x%02X but closed form expects 0x%02X (peer validates this)",
                    proto_id, expchan))
            end
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
        dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, proto_nm, ctx)

    else
        -- Unknown subtype: show remaining bytes raw.
        if len > SINTRAN_HDR then
            frame_tree:add(lapb_proto, tvb(SINTRAN_HDR),
                string.format("[Unknown subtype 0x%02X payload]", subtype))
        end
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
tcp_table:add(10362, lapb_proto)
tcp_table:add(10364, lapb_proto)
tcp_table:add(24182, lapb_proto)
tcp_table:add(17230, lapb_proto)
tcp_table:add(17237, lapb_proto)
