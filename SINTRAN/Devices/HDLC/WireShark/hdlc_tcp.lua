-- LAPB / SINTRAN over TCP dissector
-- Handles byte-stuffed HDLC, full LAPB frame parsing, SINTRAN routing/TAD/PAD.
-- Frame format (between 0x7E flags, after unstuffing):
--   addr(1) + ctrl(1) + [info...] + FCS_lo(1) + FCS_hi(1)

local lapb_proto = Proto("hdlc_lapb", "LAPB/SINTRAN over TCP")

-- ── Lua 5.4 bitwise helpers (Wireshark 4.x dropped bit32) ────────────────────
local function band(a, b)   return a & b  end
local function bxor(a, b)   return a ~ b  end
local function lshift(a, n) return a << n end
local function rshift(a, n) return a >> n end

-- ── Value strings ─────────────────────────────────────────────────────────────

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

local vs_proto = {
    [0xD8] = "D8",
    [0xD9] = "D9",
    [0xDA] = "PAD",
    [0xDB] = "DB",
    [0xDC] = "DC",
    [0xDD] = "TAD",
    [0xDE] = "ROUTING",
}

-- Opcode values verified against SINTRAN III symbol tables K03/L07/M06
-- Source: TAD-Message-Formats.md (NPL source + symbol cross-check)
local vs_tad_type = {
    [0x01] = "BDAT",   -- Terminal data block  (C↔S)
    [0x02] = "RFI",    -- Ready For Input — flow-control credit  (C→S, count=0)
    [0x03] = "ECKM",   -- Echo strategy [+ optional 20-byte table]  (C→S)
    [0x04] = "BMMX",   -- Break strategy + max-break [+ optional table]  (C→S)
    [0x08] = "ESCA",   -- Escape character received  (S→C, count=0)
    [0x09] = "DCON",   -- Disconnect indication  (S→C, count=0)
    [0x0C] = "TMOD",   -- Terminal mode flags (1 byte)  (C→S)
    [0x0D] = "TTYP",   -- Terminal type ID (2 bytes)  (C→S)
    [0x0E] = "CESC",   -- Enable/disable escape (1 byte)  (C→S)
    [0x0F] = "DESC",   -- Define escape character (1 byte)  (C→S)
    [0x13] = "SYCN",   -- System control command (2 bytes)  (C↔S)
    [0x14] = "USCN",   -- User control command (2 bytes)  (C↔S)
    [0x16] = "RESE",   -- Reset connection  (C→S, count=0)
    [0x17] = "RECO",   -- Reset confirm  (S→C, count=0)
    [0x18] = "DUMM",   -- Dummy/padding filler  (any, count=0)
    [0x1F] = "OPSV",   -- OS version + TAD protocol version (3 bytes)  (C↔S)
    [0x21] = "CERS",   -- Escape-control response  (S→C, count=0)
    [0x22] = "ISRQ",   -- Input size request  (C→S, count=0)
    [0x23] = "ISRS",   -- Input size response (2 bytes; bit15=break)  (S→C)
    [0x24] = "NOWT",   -- Nowait status (1 byte)  (C↔S)
    [0x25] = "TNOW",   -- Terminate nowait (1 byte)  (C↔S)
    [0x26] = "NWRE",   -- Nowait restart  (S→C, count=0)
    [0x27] = "RLOC",   -- Remote/local mode toggle  (S→C, count=0)
    [0x2A] = "TREP",   -- Terminal status report (2 bytes)  (S→C)
    [0x2B] = "UMOD",   -- UMOD strategy (2 bytes, protocol v4+)  (C→S)
    [0x2C] = "78MOD",  -- 8-bit mode set (2 bytes)  (C→S)
    [0xFA] = "CPCO",   -- Completion code (4 bytes)  (S→C)
    [0xFB] = "ERRS",   -- Error response (2-byte SINTRAN error code)  (S→C)
    [0xFE] = "REJE",   -- Reject — echoes the rejected opcode (1 byte)  (S→C)
    -- Observed in pcap during connection setup; not in symbol tables:
    [0x10] = "?0x10",
    [0x11] = "?0x11",
    [0x12] = "?0x12",
    [0xFD] = "?0xFD",
}

local vs_routing_cmd = {
    -- Proxy terminal-parameter negotiation (5-step countdown with DC frames)
    [0x00] = "TermParam-Step4",
    [0x01] = "TermParam-Step3",
    [0x02] = "TermParam-Step2",
    [0x03] = "TermParam-Step1",
    [0x04] = "TermParam-Step0",
    -- Routing table propagation
    [0x05] = "Propagate-Request",
    [0x07] = "Bootstrap-Request",
    [0x08] = "Sync-Request",
    [0x0B] = "Propagate-Response",
    [0x0C] = "RouteInfo-Exchange",
    [0x0D] = "Bootstrap-Response",
    [0x0E] = "Sync-Response",
    -- PAD virtual-circuit setup responses (observed on 103<->100 and 103<->102 links)
    [0x11] = "PAD-Resp",
    [0x12] = "PAD-Resp",
    -- TAD ConnSetup ACKs: formula = step + 0x0A
    [0x13] = "ConnStep-ACK(0x09)",
    [0x14] = "ConnStep-ACK(0x0A)",
    [0x15] = "ConnStep-ACK(0x0B)",
    [0x16] = "ConnStep-ACK(0x0C)",
    [0x17] = "ConnStep-ACK(0x0D)",
    [0x18] = "PAD-Resp",
    [0x19] = "PAD-Resp",
    [0x1A] = "ConnStep-ACK(0x10)",
    [0x1B] = "ConnStep-ACK(0x11)",
    [0x1C] = "ConnStep-ACK(0x12)",
    [0x1D] = "ConnStep-ACK(0x13)",
    [0x1E] = "ConnStep-ACK(0x14)",
}

-- ── ProtoFields ───────────────────────────────────────────────────────────────

local pf = {}

-- LAPB
pf.frame_raw  = ProtoField.bytes ("hdlc.raw",       "Raw Frame (stuffed)")
pf.addr       = ProtoField.uint8 ("lapb.addr",      "Address",           base.HEX)
pf.ctrl       = ProtoField.uint8 ("lapb.ctrl",      "Control",           base.HEX)
pf.ns         = ProtoField.uint8 ("lapb.ns",        "N(S) Send Seq",     base.DEC, nil, 0x0E)
pf.pf_bit     = ProtoField.bool  ("lapb.pf",        "Poll/Final",        8, nil,   0x10)
pf.nr         = ProtoField.uint8 ("lapb.nr",        "N(R) Recv Seq",     base.DEC, nil, 0xE0)
pf.stype      = ProtoField.uint8 ("lapb.stype",     "Supervisory",       base.DEC, vs_stype, 0x0C)
pf.fcs        = ProtoField.uint16("lapb.fcs",       "FCS",               base.HEX)

-- SINTRAN extension on S/U frames: 2-byte big-endian node ID
pf.node_id    = ProtoField.uint16("lapb.node_id",   "Node ID",           base.DEC)

-- SINTRAN routing header (inside LAPB I-frame info)
pf.snt_mark1  = ProtoField.uint8 ("sintran.mark1",  "Marker 1",          base.HEX)
pf.snt_mark2  = ProtoField.uint8 ("sintran.mark2",  "Marker 2",          base.HEX)
pf.snt_pkt    = ProtoField.uint8 ("sintran.pkt",    "Packet Type",       base.HEX)
pf.snt_len    = ProtoField.uint8 ("sintran.len",    "Packet Subtype",    base.DEC)
pf.snt_dest   = ProtoField.uint16("sintran.dest",   "Dest Node",         base.DEC)
pf.snt_src    = ProtoField.uint16("sintran.src",    "Src Node",          base.DEC)
pf.snt_flags1 = ProtoField.uint16("sintran.flags1", "Flags/Broadcast",   base.HEX)
pf.snt_flags2 = ProtoField.uint16("sintran.flags2", "Version/Type",      base.HEX)
pf.snt_proto  = ProtoField.uint8 ("sintran.proto",  "Protocol ID",       base.HEX, vs_proto)

-- TAD
pf.tad_type   = ProtoField.uint8 ("tad.type",       "Message Type",      base.HEX, vs_tad_type)
pf.tad_count  = ProtoField.uint8 ("tad.count",       "Data Count",       base.DEC)
pf.tad_data   = ProtoField.bytes ("tad.data",        "Data")
pf.tad_text   = ProtoField.string("tad.text",        "Text")
pf.tad_ctrl   = ProtoField.uint8 ("tad.ctrl",        "Control Byte",     base.HEX)

-- ROUTING
pf.rout_cmd   = ProtoField.uint8 ("routing.cmd",    "Command",           base.HEX, vs_routing_cmd)
pf.rout_data  = ProtoField.bytes ("routing.data",   "Routing Data")

-- PAD
pf.pad_data   = ProtoField.bytes ("pad.data",       "PAD Data")

-- TAD structured fields (per-type decoding)
pf.tad_opsv_osver  = ProtoField.uint8 ("tad.opsv.osver",  "OS Version",       base.DEC)
pf.tad_opsv_ossub  = ProtoField.uint8 ("tad.opsv.ossub",  "OS Sub-Version",   base.DEC)
pf.tad_opsv_proto  = ProtoField.uint8 ("tad.opsv.proto",  "TAD Protocol Ver", base.DEC)
pf.tad_ttyp_id     = ProtoField.uint16("tad.ttyp.id",     "Terminal Type ID", base.HEX)
pf.tad_tmod_flags  = ProtoField.uint8 ("tad.tmod.flags",  "Mode Flags",       base.HEX)
pf.tad_cmd_word    = ProtoField.uint16("tad.cmd",          "Command Word",     base.HEX)
pf.tad_errcode     = ProtoField.uint16("tad.errcode",      "Error Code",       base.HEX)
pf.tad_isrs_size   = ProtoField.uint16("tad.isrs.size",    "Input Size",       base.DEC)
pf.tad_trep_status = ProtoField.uint16("tad.trep.status",  "Status",           base.HEX)

-- DC (terminal data forwarding, proto=0xDC)
pf.dc_ctr1       = ProtoField.uint8 ("dc.ctr1",        "Counter 1",        base.HEX)
pf.dc_sub_type   = ProtoField.uint8 ("dc.sub_type",    "Sub-Type",         base.HEX)
pf.dc_speed      = ProtoField.uint8 ("dc.speed",       "Speed",            base.DEC)
pf.dc_flags      = ProtoField.uint8 ("dc.flags",       "Flags",            base.HEX)
pf.dc_loc_node   = ProtoField.uint16("dc.loc_node",    "Local Node",       base.DEC)
pf.dc_loc_chan   = ProtoField.uint16("dc.loc_chan",     "Local Channel",    base.HEX)
pf.dc_rem_node   = ProtoField.uint16("dc.rem_node",    "Remote Node",      base.DEC)
pf.dc_rem_chan   = ProtoField.uint16("dc.rem_chan",     "Remote Channel",   base.HEX)
pf.dc_ctr2       = ProtoField.uint8 ("dc.ctr2",        "Counter 2",        base.HEX)

lapb_proto.fields = {
    pf.frame_raw, pf.addr, pf.ctrl,
    pf.ns, pf.pf_bit, pf.nr, pf.stype,
    pf.fcs, pf.node_id,
    pf.snt_mark1, pf.snt_mark2, pf.snt_pkt, pf.snt_len,
    pf.snt_dest, pf.snt_src,
    pf.snt_flags1, pf.snt_flags2, pf.snt_proto,
    pf.tad_type, pf.tad_count, pf.tad_data, pf.tad_text, pf.tad_ctrl,
    pf.tad_opsv_osver, pf.tad_opsv_ossub, pf.tad_opsv_proto,
    pf.tad_ttyp_id, pf.tad_tmod_flags, pf.tad_cmd_word,
    pf.tad_errcode, pf.tad_isrs_size, pf.tad_trep_status,
    pf.rout_cmd, pf.rout_data,
    pf.pad_data,
    pf.dc_ctr1, pf.dc_sub_type, pf.dc_speed, pf.dc_flags,
    pf.dc_loc_node, pf.dc_loc_chan, pf.dc_rem_node, pf.dc_rem_chan, pf.dc_ctr2,
}

-- ── CRC-16-CCITT (polynomial 0x1021, init 0xFFFF, non-reflected) ─────────────
-- Transmit FCS = ~CRC, stored little-endian.

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
-- mark1(1)+mark2(1)+pkt(1)+len(1)+dest_BE(2)+src_BE(2)+flags1(2)+flags2(2)+proto(1)
local SINTRAN_HDR = 13

-- ── TAD dissector ─────────────────────────────────────────────────────────────
-- Parses one or more chained TAD messages starting at tvb offset off.
-- Opcode table verified against SINTRAN K03/L07/M06 symbol tables.
--
-- Special case: count=0x21 (decimal 33) signals a 33-byte connection/routing
-- block embedded as the data payload. This is a transport-layer construct and is
-- independent of opcode 0x21 (CERS), which always has count=0.

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

    local msg_type  = tvb(off, 1):uint()
    local msg_count = tvb(off + 1, 1):uint()

    -- count=0x21 → 33-byte connection/routing block (observed on BDAT, RFI,
    -- SYCN, USCN, and undocumented 0x10-0x12 types during session setup).
    -- Layout: pad(1) speed(1) flags(1) loc_node(2) loc_chan(2) rem_node(2)
    --         rem_chan(2) extra(5) ctr2(1) → followed by chained TAD messages.
    if msg_count == 0x21 then
        local type_nm = vs_tad_type[msg_type] or string.format("0x%02X", msg_type)
        local avail   = math.min(msg_count, tvb:len() - (off + 2))
        local t = tree:add(lapb_proto, tvb(off, 2 + avail),
                      string.format("TAD Block-33  [%s]", type_nm))
        t:add(pf.tad_type,  tvb(off,     1))
        t:add(pf.tad_count, tvb(off + 1, 1))
        local cb = off + 2
        if avail >= 11 then
            local cb_tree = t:add(lapb_proto, tvb(cb, avail), "Connection Block")
            cb_tree:add(pf.dc_speed,    tvb(cb + 1,  1))
            cb_tree:add(pf.dc_flags,    tvb(cb + 2,  1))
            cb_tree:add(pf.dc_loc_node, tvb(cb + 3,  2))
            cb_tree:add(pf.dc_loc_chan, tvb(cb + 5,  2))
            cb_tree:add(pf.dc_rem_node, tvb(cb + 7,  2))
            cb_tree:add(pf.dc_rem_chan, tvb(cb + 9,  2))
            if avail >= 17 then
                cb_tree:add(pf.dc_ctr2, tvb(cb + 16, 1))
            end
            if avail > 17 then
                dissect_tad(tvb, pinfo, t, cb + 17)
            end
        end
        pinfo.cols.info:append(string.format(" TAD:Block33[%s]", type_nm))
        return
    end

    -- ── Parse chained TAD messages ────────────────────────────────────────────
    local pos = off
    while pos + 1 <= tvb:len() - 1 do
        -- 0x00 is a TAD alignment pad byte — skip silently.
        -- The spec inserts it when a message would start on an odd byte boundary.
        -- It is NOT a message type.
        if tvb(pos, 1):uint() == 0x00 then
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

        if avail > 0 then
            local d = pos + 2
            -- ── Per-type structured decoding ──────────────────────────────
            if mtype == 0x01 then                        -- BDAT: raw terminal data
                t:add(pf.tad_text, tvb(d, avail))

            elseif mtype == 0x03 and avail >= 1 then     -- ECKM: echo strategy
                local st = tvb(d, 1):uint()
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [strategy=%d%s]", st,
                        st == 7 and " (custom table follows)" or ""))
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

            elseif mtype == 0x0C and avail >= 1 then     -- TMOD: mode flags byte
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

            elseif mtype == 0x0D and avail >= 2 then     -- TTYP: terminal type ID
                t:add(pf.tad_ttyp_id, tvb(d, 2))

            elseif mtype == 0x1F and avail >= 3 then     -- OPSV: OS + proto version
                t:add(pf.tad_opsv_osver,  tvb(d,     1))
                t:add(pf.tad_opsv_ossub,  tvb(d + 1, 1))
                t:add(pf.tad_opsv_proto,  tvb(d + 2, 1))
                pinfo.cols.info:append(string.format(
                    " OPSV(os=%d.%d proto=%d)",
                    tvb(d,1):uint(), tvb(d+1,1):uint(), tvb(d+2,1):uint()))

            elseif (mtype == 0x13 or mtype == 0x14) and avail >= 2 then  -- SYCN/USCN
                t:add(pf.tad_cmd_word, tvb(d, 2))

            elseif mtype == 0x23 and avail >= 2 then     -- ISRS: input size
                local sz = tvb(d, 2):uint()
                local si = t:add(pf.tad_isrs_size, tvb(d, 2))
                if (sz & 0x8000) ~= 0 then
                    si:append_text("  [break present]")
                end

            elseif mtype == 0x2A and avail >= 2 then     -- TREP: terminal status
                local st = tvb(d, 2):uint()
                local si = t:add(pf.tad_trep_status, tvb(d, 2))
                si:append_text(string.format("  [%s%s%s]",
                    (st & 0x04) ~= 0 and "BFUL " or "",
                    (st & 0x08) ~= 0 and "PAER " or "",
                    (st & 0x10) ~= 0 and "FRER " or ""))

            elseif mtype == 0xFB and avail >= 2 then     -- ERRS: error code
                t:add(pf.tad_errcode, tvb(d, 2))

            elseif mtype == 0xFE and avail >= 1 then     -- REJE: rejected opcode
                local bad = tvb(d, 1):uint()
                local bad_nm = vs_tad_type[bad] or string.format("0x%02X", bad)
                t:add(pf.tad_data, tvb(d, 1)):append_text(
                    string.format("  [rejected: %s]", bad_nm))

            else
                t:add(pf.tad_data, tvb(d, avail))
            end
        end

        pinfo.cols.info:append(string.format(" TAD:%s", name))
        pos = pos + 2 + mcount
    end
end

-- ── ROUTING dissector ─────────────────────────────────────────────────────────

local function dissect_routing(tvb, pinfo, tree, off)
    if tvb:len() - off < 1 then
        tree:add("[ROUTING: too short]")
        return
    end

    local cmd   = tvb(off, 1):uint()
    local cname = vs_routing_cmd[cmd] or string.format("0x%02X", cmd)
    local t     = tree:add(lapb_proto, tvb(off), string.format("ROUTING  [%s]", cname))

    t:add(pf.rout_cmd, tvb(off, 1))

    local rem = tvb:len() - off - 1
    if rem > 0 then
        t:add(pf.rout_data, tvb(off + 1, rem))
    end

    pinfo.cols.info:append(string.format(" ROUTING:%s", cname))
end

-- ── DC/DB dissector (proto=0xDC and 0xDB, terminal data forwarding) ──────────
-- Both share the same layout:
--   ctr1(1) + sub-hdr(17) + ctr2(1) + TAD-msgs...
-- sub-hdr(17):
--   sub_type(1) + 00(1) + speed(1) + flags(1)
--   + loc_node(2) + loc_chan(2) + rem_node(2) + rem_chan(2)
--   + extra(5)   [varies: 01 08 00 00 00  or  01 00 01 4b 00  etc.]
-- When only 1 byte follows SINTRAN header (len_field=3): flow-control counter.

local DC_SUBHDR = 17

local function dissect_dc(tvb, pinfo, tree, off, proto_label)
    local rem = tvb:len() - off
    proto_label = proto_label or "DC"

    -- Single control byte
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

    local sub = tree:add(lapb_proto, tvb(off + 1, DC_SUBHDR),
                    string.format("%s Connection  [loc=%d/0x%04X  rem=%d/0x%04X]",
                        proto_label,
                        tvb(off + 5, 2):uint(), tvb(off + 7, 2):uint(),
                        tvb(off + 9, 2):uint(), tvb(off + 11, 2):uint()))
    sub:add(pf.dc_sub_type, tvb(off + 1,  1))
    sub:add(pf.dc_speed,    tvb(off + 3,  1))
    sub:add(pf.dc_flags,    tvb(off + 4,  1))
    sub:add(pf.dc_loc_node, tvb(off + 5,  2))
    sub:add(pf.dc_loc_chan, tvb(off + 7,  2))
    sub:add(pf.dc_rem_node, tvb(off + 9,  2))
    sub:add(pf.dc_rem_chan, tvb(off + 11, 2))

    local ctr2_off = off + 1 + DC_SUBHDR
    local ctr2 = tvb(ctr2_off, 1):uint()
    tree:add(pf.dc_ctr2, tvb(ctr2_off, 1))

    local msg_off = ctr2_off + 1
    if msg_off < tvb:len() then
        dissect_tad(tvb, pinfo, tree, msg_off)
    end

    pinfo.cols.info:append(string.format(" %s[ctr1=0x%02X ctr2=0x%02X]", proto_label, ctr1, ctr2))
end

-- ── PAD dissector ─────────────────────────────────────────────────────────────

local function dissect_pad(tvb, pinfo, tree, off)
    local rem = tvb:len() - off
    if rem < 1 then
        tree:add("[PAD: too short]")
        return
    end

    local t = tree:add(lapb_proto, tvb(off), "PAD Data")
    t:add(pf.pad_data, tvb(off, rem))
    pinfo.cols.info:append(string.format(" PAD(0x%02X)", tvb(off, 1):uint()))
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

    local dest     = tvb(4, 2):uint()
    local src      = tvb(6, 2):uint()
    local proto_id = tvb(12, 1):uint()
    local proto_nm = vs_proto[proto_id] or string.format("0x%02X", proto_id)
    local label    = mark2 == 0x12 and "SINTRAN Relay" or "SINTRAN"

    local hdr = frame_tree:add(lapb_proto, tvb(0, SINTRAN_HDR),
                    string.format("%s  [%d → %d  %s]", label, src, dest, proto_nm))

    hdr:add(pf.snt_mark1,  tvb(0,  1))
    hdr:add(pf.snt_mark2,  tvb(1,  1))
    hdr:add(pf.snt_pkt,    tvb(2,  1))
    hdr:add(pf.snt_len,    tvb(3,  1))
    hdr:add(pf.snt_dest,   tvb(4,  2))   -- big-endian: 0x0066 = node 102
    hdr:add(pf.snt_src,    tvb(6,  2))   -- big-endian: 0x0064 = node 100
    hdr:add(pf.snt_flags1, tvb(8,  2))
    hdr:add(pf.snt_flags2, tvb(10, 2))
    hdr:add(pf.snt_proto,  tvb(12, 1))

    -- Sub-protocol dispatch (also handle single-byte control frames with len >= SINTRAN_HDR)
    if len >= SINTRAN_HDR then
        if proto_id == 0xDD then
            dissect_tad(tvb, pinfo, frame_tree, SINTRAN_HDR)
        elseif proto_id == 0xDC then
            dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, "DC")
        elseif proto_id == 0xDE then
            dissect_routing(tvb, pinfo, frame_tree, SINTRAN_HDR)
        elseif proto_id == 0xDB then
            dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, "DB")
        elseif proto_id == 0xDA then
            dissect_pad(tvb, pinfo, frame_tree, SINTRAN_HDR)
        elseif proto_id == 0xD9 then
            dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, "D9")
        elseif proto_id == 0xD8 then
            dissect_dc(tvb, pinfo, frame_tree, SINTRAN_HDR, "D8")
        else
            frame_tree:add(lapb_proto, tvb(SINTRAN_HDR),
                string.format("[Unknown protocol 0x%02X]", proto_id))
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

    -- ── Address ──────────────────────────────────────────────────────────────
    frame_tree:add(pf.addr, tvb(0, 1))

    -- ── Control ──────────────────────────────────────────────────────────────
    local ctrl_tree = frame_tree:add(pf.ctrl, tvb(1, 1))
    local summary

    -- info_len = bytes between ctrl and FCS
    local info_len = len - 4

    if band(ctrl_byte, 0x01) == 0 then
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
        local stype = band(rshift(ctrl_byte, 2), 0x03)
        local nr    = band(rshift(ctrl_byte, 5), 0x07)
        local sname = (vs_stype[stype] or "?"):match("^(%S+)")

        ctrl_tree:add(pf.stype,  tvb(1, 1))
        ctrl_tree:add(pf.pf_bit, tvb(1, 1))
        ctrl_tree:add(pf.nr,     tvb(1, 1))

        summary = string.format("S  %s N(R)=%d%s", sname, nr, pf_set and " P/F" or "")

        -- SINTRAN extension: 2-byte big-endian node ID before FCS
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

        -- SINTRAN extension: 2-byte big-endian node ID before FCS
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
