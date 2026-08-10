#!/usr/bin/env python
"""
bpun2raw.py - flatten a BPUN-loaded ND-100 image to a raw 16-bit little-endian word file,
so it can be disassembled with nd100-dis.

WHY THIS EXISTS
---------------
ND-100 disassembly is done with **nd100-dis**, the real disassembler:

    /home/ronny/repos/nd100-tools/nd100-dis        (WSL)

It has full instruction decode plus IO-device and MON-call tables. There used to be an
ad-hoc `nd100dis.py` in this folder; it has been REMOVED. It was a ~140-line script that
mis-decoded the entire register-operation group for months (its ROP test
`(w & 0o170000) == 0o144000` can never be true, so `146151` printed as `SKP IF DD LSS SD`
where it is really `COPY SA DD`). Do not reintroduce it. If a decode looks wrong, fix
nd100-dis, not a private copy.

nd100-dis does not recognise this BPUN variant - it falls back to treating the whole file as
raw and mis-frames it (reports 23833 words for XMSG-KERNEL-L03 where the loader places
23551). `bpun_load.py` does understand the container, so the split is: load here, decode
there.

USAGE
-----
    python bpun2raw.py XMSG-KERNEL-L03.BPUN kernel-l03.bin
    # prints the -b value to use

    nd100-dis -a -b <base> -s <offset> -n <count> kernel-l03.bin

**`-b` and `-s` are parsed with `strtol(..., 0)` - C-style.** A LEADING ZERO means octal,
`0x` means hex, and bare digits mean DECIMAL. The `-o` flag only affects OUTPUT formatting,
never input. So both of these are the same place, and both are correct:

    -b 0120000 -s 011055        # octal, note the leading zeros
    -b 40960   -s 4653          # decimal

but `-b 120000` (no leading zero) is decimal 120000, which wraps mod 2^16 to `0o152300` and
puts every address out by a fixed offset. That is silent - the listing looks perfectly
plausible. This script prints the correct `-b` in both forms, and `--at <octal-address>`
converts an address into the matching `-s`.

Verified end to end: both forms above put `ZCRMG` at `131055`, matching the hand decode
published in XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md.

A further trap worth knowing: **never locate code by scanning for a constant.** On this
architecture `020400` is both the `X5THD` marker and the encoding of `STD 0,B`, so a value
scan returns ordinary instructions. Navigate by SYMBOL.
"""

import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from bpun_load import load_bpun


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 1

    src, dst = sys.argv[1], sys.argv[2]
    r = load_bpun(src)
    mem = r["mem"]
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
    # Print both forms. nd100-dis uses strtol(...,0): a LEADING ZERO means octal, bare
    # digits mean decimal. Mixing them silently shifts every address.
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
