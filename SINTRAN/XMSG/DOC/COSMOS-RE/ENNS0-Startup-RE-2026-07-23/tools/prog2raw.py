#!/usr/bin/env python
"""
prog2raw.py - flatten a loaded SINTRAN :PROG image to a raw 16-bit little-endian word file,
so it can be disassembled with nd100-dis.

Companion to bpun2raw.py; same reasoning. ND-100 disassembly is done with **nd100-dis**
(`/home/ronny/repos/nd100-tools/nd100-dis`, WSL). The ad-hoc `nd100dis.py` / `disprog.py`
that used to live here have been REMOVED - see bpun2raw.py for why.

USAGE
-----
    python prog2raw.py XMSG-COMMAND-L03.PROG command.bin
    nd100-dis -a -b <base> -s <offset> -n <count> command.bin

**`-b` and `-s` use `strtol(..., 0)`** - a LEADING ZERO means octal, bare digits mean
decimal; `-o` only affects OUTPUT formatting. `-b 0120000` and `-b 40960` are the same base;
`-b 120000` is decimal and wraps mod 2^16, silently shifting every address. This script
prints both forms; pass `--at <octal-address>` to get the matching `-s`.
"""

import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from prog_load import load_prog


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 1

    src, dst = sys.argv[1], sys.argv[2]
    loaded = load_prog(src)

    # load_prog returns either a dict with a 'mem' map or a flat (mem, base) pair depending
    # on the image; normalise both to an address->word map.
    mem = loaded["mem"] if isinstance(loaded, dict) and "mem" in loaded else loaded
    if not hasattr(mem, "keys"):
        print("prog2raw: unexpected load_prog result: %r" % type(loaded))
        return 2

    lo, hi = min(mem), max(mem)
    out = bytearray()
    for a in range(lo, hi + 1):
        w = mem.get(a, 0) & 0xFFFF
        out += bytes((w & 0xFF, (w >> 8) & 0xFF))       # little-endian words
    with open(dst, "wb") as f:
        f.write(out)

    print("words present : %d" % len(mem))
    print("span          : %o..%o octal (%d words emitted)" % (lo, hi, hi - lo + 1))
    print("")
    print("nd100-dis -a -b 0%o ...           # octal form (leading zero REQUIRED)" % lo)
    print("nd100-dis -a -b %d ...            # decimal form, same base" % lo)

    if "--at" in sys.argv:
        addr = int(sys.argv[sys.argv.index("--at") + 1], 8)
        off = addr - lo
        print("")
        print("nd100-dis -a -b 0%o -s 0%o -n 40 %s   # starts at %o"
              % (lo, off, dst, addr))
        print("nd100-dis -a -b %d -s %d -n 40 %s   # same, decimal"
              % (lo, off, dst, addr))
    return 0


if __name__ == "__main__":
    sys.exit(main())
