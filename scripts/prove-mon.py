#!/usr/bin/env python3
"""
Prove a MON call from GROUND TRUTH, so nothing has to be taken on faith.

For a given octal MON number it prints, step by step, with file offsets and raw
bytes you can re-check by hand:

  1. GOTAB[MON]  read directly from resident/SINTRAN-DATA_commoncode.bin
                 (base virtual 071233 octal, one word per MON number).
                 -> 000000 means "fall-through" (no direct handler); else = entry addr.
  2. The SYMBOL for that entry address, and the NEXT symbol, from the L07
     SYMBOL-2-LIST -> this is where the "how long is it" answer comes from.
  3. The first raw words at that entry address inside the overlay segment
     025-S3IRPIT.bin (load 32000 octal), so you can see it is real code, not zeros.

Everything is octal (ND-100 native). Cross-check the symbol names against
SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT yourself.

Usage:  python3 scripts/prove-mon.py 51 45 15 13 14 42
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
V = os.path.join(ROOT, "tools/sintran-segment-carver/versions/L-VSX-500")
COMMON = os.path.join(V, "resident/SINTRAN-DATA_commoncode.bin")
SYMS   = os.path.join(ROOT, "SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT")
S3RPIT = os.path.join(V, "segments/025-S3IRPIT.bin")
S3RPIT_LOAD = 0o32000
GOTAB = 0o71233

def word_be(buf, word_addr, load=0):
    off = (word_addr - load) * 2
    return (buf[off] << 8) | buf[off + 1], off

def load_symbols():
    syms = {}
    for m in re.finditer(r"([A-Z0-9]+)=([0-7]+)", open(SYMS, encoding="utf-8", errors="replace").read()):
        syms[int(m.group(2), 8)] = m.group(1)
    return syms

def main():
    mons = [int(x, 8) for x in sys.argv[1:]] or [0o51, 0o45, 0o15, 0o13, 0o14, 0o42]
    common = open(COMMON, "rb").read()
    rpit = open(S3RPIT, "rb").read()
    syms = load_symbols()
    saddrs = sorted(syms)
    print(f"GOTAB base = 071233 octal in {os.path.relpath(COMMON, ROOT)}")
    print(f"Overlay    = {os.path.relpath(S3RPIT, ROOT)} (load 32000 octal)")
    print(f"Symbols    = {os.path.relpath(SYMS, ROOT)}\n")
    for mon in mons:
        print(f"MON {mon:o}B:")
        # The ND-100 GOTAB only dispatches ND-100 level-14 calls. ND-500 calls
        # (410-427 octal, and the level-12 range 500-523 octal) are dispatched by
        # the ND-500 monitor tables (S3SM5 0x60 vector / the 5CMNO level-12 GOSW),
        # NOT by this GOTAB. Indexing GOTAB for them returns unrelated ND-100 data.
        if (0o410 <= mon <= 0o427) or (0o500 <= mon <= 0o523):
            print(f"  ND-500 call: NOT dispatched via the ND-100 GOTAB.")
            print(f"  410-427B -> S3SM5 0x60 vector table (030-S3SM5.bin); "
                  f"500-523B -> level-12 5CMNO GOSW. GOTAB[{mon:o}] is meaningless here.\n")
            continue
        entry, goff = word_be(common, GOTAB + mon)
        print(f"  GOTAB[{mon:o}] : file byte 0x{goff:x} of commoncode.bin, raw = "
              f"{common[goff]:02x} {common[goff+1]:02x}  -> {entry:06o} octal")
        if entry == 0:
            print(f"  = 000000 -> FALL-THROUGH (no direct handler; dispatched via MFELL/CALLPROC)\n")
            continue
        # symbol at exactly this address + next symbol => length
        name = syms.get(entry, "(no exact symbol)")
        nxt = next((a for a in saddrs if a > entry), None)
        if nxt:
            print(f"  symbol at {entry:o} = {name};  next symbol {syms[nxt]}={nxt:o}"
                  f"  => stub length {nxt-entry} words (to next symbol)")
        # raw words at the entry inside the overlay
        if S3RPIT_LOAD <= entry < S3RPIT_LOAD + len(rpit)//2:
            w0, off = word_be(rpit, entry, S3RPIT_LOAD)
            ws = [word_be(rpit, entry+i, S3RPIT_LOAD)[0] for i in range(6)]
            print(f"  bytes at {entry:o} in 025-S3IRPIT.bin (file byte 0x{off:x}): "
                  + " ".join(f"{w:06o}" for w in ws))
            print(f"  (first word {w0:06o} - if this is 000000 the overlay is wrong)")
        else:
            print(f"  {entry:o} is outside S3RPIT range; different overlay")
        print()

if __name__ == "__main__":
    main()
