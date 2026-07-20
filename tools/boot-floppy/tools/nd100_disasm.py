#!/usr/bin/env python3
"""
nd100_disasm.py -- a small, table-driven ND-100 disassembler.

Purpose
-------
Disassemble raw ND-100 machine code (big-endian 16-bit words) such as the
page-0 bootstraps found on installed Norsk Data mass-storage packs.  Written
for the disc-boot-sector anatomy study; it is deliberately dependency-light so
it can be run against any 2048-byte block dump.

Where the opcode table comes from
---------------------------------
NOTHING here is guessed.  The whole table is *extracted at run time* from the
machine-readable CPU definitions in

    E:\\Dev\\Ronny\\nd100-definitions\\specs\\

  * specs/instructions/*.yaml  -- one file per instruction, each carrying an
    octal `opcode:` and a bit `mask:` (or an `instruction_class:` naming a
    class in cpu.yaml from which the mask is inherited).
  * specs/cpu.yaml             -- `metadata.instruction_classes` (masks) and
    `metadata.registers` (register code numbers 1..7 = D,P,B,L,A,T,X).
  * specs/operand_types/addressing_modes.yaml -- the 3-bit ,B / I / ,X modes.
  * specs/instructions/tra.yaml -- the 4-bit internal-register enum used by
    TRA/TRR (PANS, STS, OPR, PSR, PVL, IIC, PID, PIE, CSR, ACTL, ALD, PES,
    PGC, PEA, CS).

Decoding rule: every entry is (opcode, mask); a word `w` matches when
`w & mask == opcode`.  Candidates are tried most-specific-first (highest
popcount of the mask), which is what separates e.g. 0146142 EXIT from the
0146000 RADD family and 0150405 PIOF from the 0150000 TRA family.

Displacement convention
-----------------------
P-relative effective address is rendered as `here + disp` (disp = signed
8-bit), i.e. the displacement is relative to the address of the instruction
ITSELF, not to the following word.  This matches
`specs/operand_types/addressing_modes.yaml` ("(P) + disp") and the MAC
assembler rule recorded in TSS/CLAUDE.md ("P-relative displacement is
target - here, where here is the instruction's own address").

Usage
-----
    python nd100_disasm.py <file.bin> [--start WORD] [--count WORDS]
                                      [--base WORD] [--octal-only]

    --start   first word index in the file to decode (default 0)
    --count   number of words to decode (default: to end of file)
    --base    word address to label the first decoded word with (default 0)

As a library:
    from nd100_disasm import disasm_word, load_table
"""

import os
import sys

SPEC_DIR = r"E:\Dev\Ronny\nd100-definitions\specs"


def _oct(n, width=6):
    """Format a non-negative int as zero-padded octal, ND house style."""
    return format(n & 0xFFFF, "0%do" % width)


def _mask_from_str(s):
    """'1111_1000_0000_0000' -> 0xF800."""
    return int(s.replace("_", ""), 2)


def load_table(spec_dir=SPEC_DIR):
    """Build the (opcode, mask, name, class) table from the YAML definitions.

    Returns a list sorted most-specific-mask-first, plus the auxiliary enums
    (addressing modes, internal registers) needed for operand rendering.
    """
    import yaml

    cpu = yaml.safe_load(open(os.path.join(spec_dir, "cpu.yaml"), encoding="utf-8"))
    classes = {}
    for c in cpu["metadata"]["instruction_classes"]:
        classes[c["name"]] = c

    entries = []
    idir = os.path.join(spec_dir, "instructions")
    for fn in sorted(os.listdir(idir)):
        if not fn.endswith(".yaml") or fn.startswith("_"):
            continue
        y = yaml.safe_load(open(os.path.join(idir, fn), encoding="utf-8"))
        i = y["instruction"]
        name = i.get("name")
        opstr = i.get("opcode")
        if name is None or opstr is None:
            continue
        opcode = int(str(opstr).strip(), 8)
        maskstr = i.get("mask")
        cls = i.get("instruction_class")
        if maskstr is None and cls and cls in classes and classes[cls].get("mask"):
            maskstr = classes[cls]["mask"]
        if maskstr is None:
            # bit_instructions / bit_instructions_with_condition carry no class
            # mask in cpu.yaml; every such instruction states its own mask, so
            # this branch is only a safety net.
            continue
        mask = _mask_from_str(maskstr)

        # BSET/BSKP fix-up. The YAML gives mask 1111_1111_1000_0000 (0xFF80),
        # which would only match the ZRO variant (174000 / 175000). The real
        # encoding, verified against ghidra-nd100/ND-100/data/languages/
        # nd100.slaspec:336 and :370 ("bits 15-11=BOP, bits 10-7=sub-instr,
        # bits 6-3=bn, bits 2-0=dr", op9=0x1F0..0x1F7), has a 9-bit opcode in
        # bits 15-7 of which the low 2 bits are the condition:
        #   BSET ZRO/ONE/BCM/BAC = 174000/174200/174400/174600
        #   BSKP ZRO/ONE/BCM/BAC = 175000/175200/175400/175600
        # so the discriminating mask is bits 15-9 = 0xFE00.
        if name in ("BSET", "BSKP"):
            mask = 0xFE00

        entries.append((opcode, mask, name, cls))

    # Most-specific first: more mask bits set == tighter match.
    entries.sort(key=lambda e: bin(e[1]).count("1"), reverse=True)

    # TRA/TRR internal register enum, straight out of tra.yaml.
    tra = yaml.safe_load(open(os.path.join(idir, "tra.yaml"), encoding="utf-8"))
    iregs = {}
    for op in tra["instruction"]["operands"]:
        if op.get("name") == "internal_register":
            for e in op["enum"]:
                iregs[int(e["value"], 8)] = e["name"]

    return entries, iregs


# Register code -> mnemonic. Codes are cpu.yaml `code:` values.
SRC = {0: "", 1: "SD", 2: "SP", 3: "SB", 4: "SL", 5: "SA", 6: "ST", 7: "SX"}
DST = {0: "", 1: "DD", 2: "DP", 3: "DB", 4: "DL", 5: "DA", 6: "DT", 7: "DX"}

# addressing_modes.yaml, value -> rendering template for the disp field.
MODES = {
    0: "%s",            # P relative
    1: "%s,B",          # B relative
    2: "I %s",          # P indirect
    3: "I %s,B",        # B indirect
    4: "%s,X",          # X relative
    5: "%s,B,X",        # B indexed
    6: ",X I %s",       # P indirect indexed
    7: ",X I %s,B",     # B indirect indexed
}

# skp.yaml bits 10-8.
SKP_COND = {0: "EQL", 1: "GEQ", 2: "GRE", 3: "MGRE",
            4: "UEQ", 5: "LSS", 6: "LST", 7: "MLST"}

# bset/bskp condition field (bits 5-3) and the STS bit names (dr == 0).
BIT_COND = {0: "ZRO", 1: "ONE", 2: "BCM", 3: "BAC"}

SHIFT_TYPE = {0: "", 1: "ROT", 2: "ZIN", 3: "LIN"}


def _s8(v):
    """Sign-extend the low 8 bits."""
    v &= 0xFF
    return v - 256 if v & 0x80 else v


def disasm_word(w, addr, entries, iregs):
    """Decode one 16-bit word. Returns (mnemonic_text, note) or (None, None)."""
    for opcode, mask, name, cls in entries:
        if (w & mask) != opcode:
            continue

        # ---- memory reference: 5-bit opcode, 3-bit mode, 8-bit signed disp ----
        if cls == "memory_transfer":
            mode = (w >> 8) & 7
            disp = _s8(w)
            if mode in (0, 2, 6):  # P-relative family -> resolve the target
                tgt = (addr + disp) & 0xFFFF
                txt = "%-6s %s" % (name, MODES[mode] % ("*%d" % disp if disp >= 0 else "*-%d" % -disp))
                return txt, "target=%s" % _oct(tgt)
            return "%-6s %s" % (name, MODES[mode] % str(disp)), None

        if cls == "jump_on_condition":
            disp = _s8(w)
            tgt = (addr + disp) & 0xFFFF
            return "%-6s *%s%d" % (name, "" if disp >= 0 else "-", abs(disp)), "target=%s" % _oct(tgt)

        # ---- I/O ----
        if name == "IOX":
            dev = w & 0x7FF
            return "%-6s %s" % (name, _oct(dev, 4)), "dev=%s%s" % (
                _oct(dev, 4), " (output)" if dev & 1 else " (input)")
        if name == "IOT":
            return "%-6s %s" % (name, _oct(w & 0x7FF, 4)), None

        # ---- internal register transfer ----
        if name in ("TRA", "TRR", "MCL", "MST"):
            r = w & 0xF
            return "%-6s %s" % (name, iregs.get(r, _oct(r, 2))), None

        # ---- register/register ----
        if cls in ("register_src_dst", "register_logical"):
            return "%-6s %s %s" % (name, SRC[(w >> 3) & 7], DST[w & 7]), None
        if cls == "register_arithmetic":
            subs = []
            if w & 0x0200:
                subs.append("ADC")
            if w & 0x0100:
                subs.append("AD1")
            if w & 0x0080:
                subs.append("CM1")
            if w & 0x0040:
                subs.append("CLD")
            return "%-6s %s" % (name, " ".join(subs + [SRC[(w >> 3) & 7], DST[w & 7]]).strip()), None
        if cls == "register_dst":
            return "%-6s %s" % (name, DST[w & 7]), None
        if cls == "register_block":
            return "%-6s level %d" % (name, (w >> 3) & 0xF), None

        # ---- skip ----
        if name == "SKP":
            return "%-6s %s %s %s" % (name, DST[w & 7], SKP_COND[(w >> 8) & 7],
                                      SRC[(w >> 3) & 7]), None

        # ---- bit instructions ----
        if name in ("BSET", "BSKP"):
            # cond = bits 8-7, bit number = bits 6-3, dr = bits 2-0.
            return "%-6s %s %d %s" % (name, BIT_COND[(w >> 7) & 3],
                                      (w >> 3) & 0xF, DST[w & 7] or "STS"), None
        if name in ("BSTC", "BSTA", "BLDC", "BLDA", "BANC", "BAND", "BORC", "BORA"):
            return "%-6s bit%d %s" % (name, (w >> 3) & 0xF, DST[w & 7]), None

        # ---- shifts ----
        if cls == "shift_instructions":
            cnt = w & 0x3F
            if cnt & 0x20:
                cnt -= 64
            return "%-6s %s %d" % (name, SHIFT_TYPE[(w >> 9) & 3], cnt), None

        # ---- 8-bit immediate argument instructions ----
        if name in ("SAA", "SAB", "SAT", "SAX", "AAA", "AAB", "AAT", "AAX",
                    "NLZ", "DNZ", "WAIT", "MON"):
            v = _s8(w)
            return "%-6s %d" % (name, v), ("=%s" % _oct(v, 6)) if v < 0 else None

        if name in ("IRW", "IRR"):
            return "%-6s %d %s" % (name, (w >> 3) & 0xF, DST[w & 7]), None

        # ---- everything else: no operands, or operands not modelled ----
        return name, None

    return None, None


def disasm(data, start_word=0, count=None, base=0, spec_dir=SPEC_DIR):
    """Yield (addr, word, text, note) for each decoded word."""
    entries, iregs = load_table(spec_dir)
    nwords = len(data) // 2
    if count is None:
        count = nwords - start_word
    for k in range(count):
        i = start_word + k
        if i >= nwords:
            break
        w = (data[i * 2] << 8) | data[i * 2 + 1]
        addr = base + k
        txt, note = disasm_word(w, addr, entries, iregs)
        yield addr, w, txt, note


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 1
    path = argv[1]
    start = 0
    count = None
    base = 0
    a = 2
    while a < len(argv):
        if argv[a] == "--start":
            start = int(argv[a + 1], 0); a += 2
        elif argv[a] == "--count":
            count = int(argv[a + 1], 0); a += 2
        elif argv[a] == "--base":
            base = int(argv[a + 1], 0); a += 2
        else:
            a += 1
    data = open(path, "rb").read()
    for addr, w, txt, note in disasm(data, start, count, base):
        line = "%s  %s  %s" % (_oct(addr), _oct(w), txt if txt else "??")
        if note:
            line += "   ; " + note
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
