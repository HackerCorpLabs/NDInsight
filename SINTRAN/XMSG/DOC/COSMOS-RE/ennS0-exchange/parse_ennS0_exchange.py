#!/usr/bin/env python3
# Parse an ENNS0 start-net device log (oracle/LLE or HLE) into a structured JSON
# timeline of the XMSG exchange, so the two cards can be diffed cell-for-cell.
#
# Decodes:
#   - every XMSG call:      iFunc, PIL, the inline [..] args, and the MON-200 A/B/X/T regs
#   - every XFREA-CONTENT:  the XROUT record -> {serial, service(+name)/status, params[]}
#   - mailbox kicks:        REQUEST / SUBFUNCTION
#   - control-word writes:  [ETH(HLE)-CONTROL]
#   - SCIP / level-12 IDENT counts
#   - console output text (reassembled)
#
# XROUT record format (COSMOS Programmer Guide app. B, verified in
# XROUT-DIRECTORY-RECORD-TAGS-DECODE-2026-08-10.md):
#   word0 = byte0 serial | byte1 service(request, bit6 set)/status(reply, 0=XRSOK)
#   word1 = length of the rest in bytes
#   then parameter blocks: byte0 = param# AND sign (int>0, string=two's-complement<0),
#                          byte1 = length in bytes, then the data.
import sys, re, json

SERVICE = {64:"XSNUL",65:"XSLET",66:"XSNAM",67:"XSCNM",68:"XSGNM",69:"XSGNI",71:"XSGMG",
           73:"XSDRN",74:"XSDSY",75:"XSGSY",76:"XSLKI",77:"XSTIN",78:"XSTCL",79:"XSTDC",
           80:"XSCRS",81:"XSNSP",82:"XSGIN",83:"XSDLO",84:"XSLEK",85:"XSNET",86:"XSSCI",
           87:"XSGAT",88:"XSDAT",89:"XSNSI",90:"XSLIN",91:"XSPIN",92:"XSLSY",93:"XSGSU",
           94:"XSCRM",95:"XSGLI",96:"XSGSG"}

TS = re.compile(r"^\[(\d\d:\d\d:\d\d\.\d+)\]\s*(.*)$")
WORD = re.compile(r"\[0x([0-9A-Fa-f]{4})\]")

def decode_record(words):
    """words = list of 16-bit ints (the first N read from XFREA-CONTENT)."""
    if not words:
        return None
    w0 = words[0]
    serial = w0 >> 8
    lowb = w0 & 0xFF
    is_reply = lowb < 64            # service numbers are all >= 64; a reply carries a status 0..63
    rec = {"word0": f"0x{w0:04X}", "serial": serial}
    if is_reply:
        rec["kind"] = "reply"
        rec["status"] = lowb
        rec["status_name"] = "XRSOK" if lowb == 0 else f"XR#{lowb}"
    else:
        rec["kind"] = "request"
        rec["service"] = lowb
        rec["service_name"] = SERVICE.get(lowb, f"svc0x{lowb:02X}")
    if len(words) < 2:
        return rec
    rec["byte_len"] = words[1]
    # Walk parameter blocks starting at word index 2 (byte offset 4).
    params = []
    # Flatten to a byte stream for the param walk (big-endian words).
    data = []
    for w in words:
        data.append((w >> 8) & 0xFF)
        data.append(w & 0xFF)
    pos = 4                                    # skip word0 + word1
    end = min(4 + rec["byte_len"], len(data))
    guard = 0
    while pos + 1 < end and guard < 64:
        guard += 1
        pnum = data[pos]
        plen = data[pos+1]
        if pnum == 0 and plen == 0:
            pos += 2
            continue
        if pnum >= 0x80:                        # negative -> string param (two's complement)
            num = 256 - pnum
            raw = data[pos+2: pos+2+plen]
            txt = "".join(chr(b) if 32 <= b < 127 else "." for b in raw)
            params.append({"num": num, "type": "str", "len": plen, "text": txt})
            step = 2 + plen + (plen & 1)        # word-aligned
        else:
            num = pnum
            if plen == 2 and pos+3 < len(data):
                val = (data[pos+2] << 8) | data[pos+3]
                params.append({"num": num, "type": "int", "len": plen, "value": f"0x{val:04X}", "dec": val})
            else:
                raw = data[pos+2: pos+2+plen]
                params.append({"num": num, "type": "int", "len": plen,
                               "value": "".join(f"{b:02X}" for b in raw)})
            step = 2 + plen + (plen & 1)
        pos += step
    rec["params"] = params
    return rec

def reg(pat, s):
    m = re.search(pat, s)
    return m.group(1) if m else None

def parse(path):
    timeline = []
    counts = {"ident_lvl12": 0, "scip": 0, "xfrre_empty": 0}
    console = []
    last_call = None
    with open(path, "r", errors="replace") as f:
        for line in f:
            m = TS.match(line.rstrip("\n"))
            if not m:
                continue
            ts, body = m.group(1), m.group(2)

            if "IDENT called for level 12" in body:
                counts["ident_lvl12"] += 1
                continue
            if "SCIP" in body and "INT12" in body:
                counts["scip"] += 1
                continue

            mc = re.search(r"CONOUT '(.)' \(0x([0-9A-Fa-f]+)\)", body)
            if mc:
                console.append(mc.group(1))
                continue

            mrec = re.search(r"XFREA-CONTENT @0x([0-9A-Fa-f]+) NBYTES=(\d+).*?:\s*(.*)$", body)
            if mrec:
                words = [int(w, 16) for w in WORD.findall(mrec.group(3))]
                ev = {"ts": ts, "ev": "record", "buf": "0x" + mrec.group(1).upper(),
                      "nbytes": int(mrec.group(2)), "record": decode_record(words)}
                if last_call:
                    ev["by_pil"] = last_call
                timeline.append(ev)
                continue

            mcall = re.search(r"XMSG - PIL=(\d+) iFunc='([A-Z0-9]+)'.*?\[(.*)\]$", body)
            if mcall:
                pil, ifunc, args = mcall.group(1), mcall.group(2), mcall.group(3)
                last_call = pil
                if ifunc == "XFRRE" and "X=0xE97C" in args:
                    counts["xfrre_empty"] += 1
                ev = {"ts": ts, "ev": "call", "pil": int(pil), "func": ifunc}
                for key, pat in (("recv_port", r"Receiving port:\s*(0x[0-9A-Fa-f]+|\d+)"),
                                 ("send_port", r"Sending port:\s*(0x[0-9A-Fa-f]+|\d+)"),
                                 ("A", r"\bA=(0x[0-9A-Fa-f]+)"),
                                 ("D", r"\bD=(0x[0-9A-Fa-f]+)"),
                                 ("X", r"\bX=(0x[0-9A-Fa-f]+)"),
                                 ("nbytes", r"NBYTES=(\d+)"),
                                 ("addr", r"Address=(0x[0-9A-Fa-f]+)")):
                    v = reg(pat, args)
                    if v is not None:
                        ev[key] = v
                # keep the raw inline args too, they carry the odd cases
                ev["args"] = args
                timeline.append(ev)
                continue

            mmbx = re.search(r"REQUEST=0x([0-9A-Fa-f]+)\s+SUBFUNCTION=0x([0-9A-Fa-f]+)\s*\((\w+)", body)
            if mmbx:
                timeline.append({"ts": ts, "ev": "mailbox_kick",
                                 "request": "0x" + mmbx.group(1), "subfunction": "0x" + mmbx.group(2),
                                 "note": mmbx.group(3)})
                continue

            mctl = re.search(r"(?:ETHHLE|ETH)-CONTROL\]\s*(0x[0-9A-Fa-f]+)\s*(?:=|inten)(.*)$", body)
            if mctl:
                timeline.append({"ts": ts, "ev": "control", "value": mctl.group(1),
                                 "flags": mctl.group(2).strip(" =")})
                continue

            if "PRKEY" in body and ("posted" in body or "WRITTEN" in body):
                timeline.append({"ts": ts, "ev": "prkey", "text": body})
                continue

    return {"source": path, "counts": counts,
            "console": "".join(console),
            "n_events": len(timeline), "timeline": timeline}

if __name__ == "__main__":
    src, out = sys.argv[1], sys.argv[2]
    data = parse(src)
    with open(out, "w") as f:
        json.dump(data, f, indent=1)
    # A compact stdout summary: the ordered list of decoded records only.
    print(f"{src}")
    print(f"  events={data['n_events']} counts={data['counts']}")
    print("  RECORDS (serial.service/status  params):")
    for ev in data["timeline"]:
        if ev["ev"] != "record" or not ev.get("record"):
            continue
        r = ev["record"]
        tag = r.get("service_name") or r.get("status_name")
        ps = []
        for p in r.get("params", []):
            if p["type"] == "str":
                ps.append(f'p{p["num"]}="{p["text"]}"')
            else:
                ps.append(f'p{p["num"]}={p.get("value")}')
        print(f'    {ev["ts"]} {ev.get("buf","")} {r["word0"]} {r["kind"]:7} {tag:8} ' + " ".join(ps))
