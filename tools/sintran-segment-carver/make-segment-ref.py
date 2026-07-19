#!/usr/bin/env python3
"""
make-segment-ref.py - build a segments-ref/<SEG>/ bundle for a carved SINTRAN segment.

Produces the four canonical files (same layout as the existing 006-S3FS bundle):
    <SEG>.asm          whole-segment disassembly, load base applied, symbols inserted
    <SEG>.hex          addr / word / hi lo / byte-offset table
    <SEG>.symbols.txt  in-range symbols from the segment's own symbol table(s)
    <SEG>.meta.md      load base, size, sha256, byte-offset rule

Ground rules honoured here:
  - The carved .bin is big-endian, as carved. nd100-dis takes LITTLE-endian input,
    so we byte-swap into a temp file for the disassembler ONLY. The .hex and the
    sha256 are computed from the ORIGINAL big-endian bytes.
  - Symbols come from the version-matched symbol table named in the segment's
    .meta.json ("symbol_file"). Symbols are inserted as ">>> NAME(TABLE)" marker
    lines; they are NOT trusted as routine entries - see CARVING-HANDOFF section 3a.

Run under WSL (needs nd100-dis + python3):
    python3 make-segment-ref.py L-VSX-500 003-S3CP 044-S3IDPIT
    python3 make-segment-ref.py L-VSX-500 --all
"""
import json, os, re, subprocess, struct, hashlib, sys, glob, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SYMROOT = os.path.normpath(os.path.join(HERE, "..", "..", "SINTRAN", "NPL-SOURCE", "SYMBOLS"))
VER2SYM = {"K-VSX-500": "K03", "L-VSX-500": "L07", "M-VSX-500": "M06"}


def load_symbols(symdir, wanted):
    """addr -> [ 'NAME(TABLE)' ] for the named symbol table(s)."""
    out = {}
    for tab in wanted:
        p = os.path.join(symdir, tab + ".SYMB.TXT")
        if not os.path.exists(p):
            sys.stderr.write("  WARNING: no symbol file %s\n" % p)
            continue
        for line in open(p, errors="replace"):
            m = re.match(r"^([A-Z0-9#\-\.]+)=([0-7]+)\s*$", line.strip())
            if m:
                out.setdefault(int(m.group(2), 8), []).append("%s(%s)" % (m.group(1), tab))
    return out


def build(version, seg):
    vdir = os.path.join(HERE, "versions", version)
    meta = json.load(open(os.path.join(vdir, "segments", seg + ".meta.json")))
    binp = os.path.join(vdir, "segments", seg + ".bin")

    base = int(str(meta["load_address"]["oct"]), 8)
    raw = open(binp, "rb").read()          # big-endian, as carved
    nw = len(raw) // 2
    words = struct.unpack(">%dH" % nw, raw[:nw * 2])
    sha = hashlib.sha256(raw).hexdigest()

    symfiles = meta.get("symbol_file") or []
    if isinstance(symfiles, str):
        symfiles = [symfiles]
    syms = load_symbols(os.path.join(SYMROOT, VER2SYM[version]), symfiles)

    outdir = os.path.join(vdir, "re", "segments-ref", seg)
    os.makedirs(outdir, exist_ok=True)

    # ---- disassemble (byte-swap to LE for nd100-dis only) ----
    fd, le = tempfile.mkstemp(suffix=".le")
    os.close(fd)
    d = bytearray(raw)
    d[0::2], d[1::2] = d[1::2], d[0::2]
    open(le, "wb").write(d)
    dis = subprocess.run(["nd100-dis", "-a", "-o", "-b", str(base), le],
                         capture_output=True, text=True, check=True).stdout
    os.unlink(le)

    # ---- .asm : header + symbol markers ----
    role = meta.get("description", "").strip() or "(no description in meta.json)"
    hdr = [
        "; ==== %s.bin whole-segment disassembly (SINTRAN III VSX/500 %s) ====" % (seg, VER2SYM[version]),
        "; role: %s" % role,
        "; load base %oB ; %d words ; big-endian .bin, as carved ; addresses/values octal" % (base, nw),
        "; symbol table(s): %s (in-range labels inserted as >>> NAME)" % (", ".join(symfiles) or "none"),
        ";",
        "; NOTE: a symbol landing on an address does NOT prove it is a routine entry.",
        ";       See SINTRAN/CARVING-HANDOFF.md section 3a + gotcha 6.",
        ";",
        "; sha256(%s.bin) = %s" % (seg, sha),
        "",
    ]
    body = []
    for line in dis.splitlines():
        m = re.match(r"^([0-7]{5,6})\s", line)
        if m:
            a = int(m.group(1), 8)
            if a in syms:
                body.append(">>> " + " / ".join(syms[a]))
        body.append(line)
    open(os.path.join(outdir, seg + ".asm"), "w", newline="\n").write("\n".join(hdr + body) + "\n")

    # ---- .hex (from the ORIGINAL big-endian bytes) ----
    with open(os.path.join(outdir, seg + ".hex"), "w", newline="\n") as f:
        f.write("# addr(oct)  word(oct)  hi lo(oct)  byteoff(dec)  [ND-100 word=2 bytes big-endian]\n")
        for i, v in enumerate(words):
            f.write("%o  %06o  %03o %03o  %d\n" % (base + i, v, v >> 8, v & 0xFF, i * 2))

    # ---- .symbols.txt (in-range only) ----
    with open(os.path.join(outdir, seg + ".symbols.txt"), "w", newline="\n") as f:
        for a in sorted(syms):
            if base <= a < base + nw:
                f.write("%oB  %s\n" % (a, " / ".join(syms[a])))

    # ---- .meta.md ----
    open(os.path.join(outdir, seg + ".meta.md"), "w", newline="\n").write("""# %s - segment metadata

- **Canonical binary:** `../../../segments/%s.bin` (single source of truth; NOT duplicated here)
- **Load base (octal):** %oB
- **Words:** %d (%d bytes)
- **sha256:** `%s`
- **Symbol table(s):** %s
- **Disassembly:** `%s.asm` (whole-segment, load base applied)
- **Hex dump:** `%s.hex`  - **Symbols:** `%s.symbols.txt`

Byte offset of octal address A: `(A - %oB) in octal * 2` (decimal).
""" % (seg, seg, base, nw, nw * 2, sha,
       ", ".join(s + ".SYMB.TXT" for s in symfiles) or "none", seg, seg, seg, base))

    print("  %-14s base=%-8o words=%-6d symbols=%d  -> %s" %
          (seg, base, nw, sum(1 for a in syms if base <= a < base + nw), outdir))


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    version, args = sys.argv[1], sys.argv[2:]
    vdir = os.path.join(HERE, "versions", version)
    if args == ["--all"]:
        segs = sorted(os.path.basename(p)[:-10]
                      for p in glob.glob(os.path.join(vdir, "segments", "*.meta.json")))
    else:
        segs = args
    print("Building segments-ref bundles for %s:" % version)
    for s in segs:
        try:
            build(version, s)
        except Exception as e:
            print("  %-14s SKIPPED: %s" % (s, e))


if __name__ == "__main__":
    main()
