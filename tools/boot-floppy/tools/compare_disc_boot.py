#!/usr/bin/env python3
"""compare_disc_boot.py -- word-level comparison of ND-100 mass-storage boot pages.

Used to prove the SINTRAN model of the disc boot page (PH-P2-OPPSTART.NPL, PL011):

    page 0 = [ RELOA .. LDEND )   the fixed "LOAD PROGRAM" (LDEND-RELOA = 0300B = 192 words)
           + [ swap driver     )   SWDSI = 1350B = 744 words, chosen per disc type
                                   (ZBDIS = SMD, ZWDIS = Winchester, SCSWD = SCSI)

with a handful of parameter words patched in place before the page is written back
(NOBLK, KLHDE, KLIOX, DYBLS, XSWTP, KLRC1, LDRAD, ADR2B, KBLSZ, YSWTY).

So: the first 192 words should agree between two boot pages of DIFFERENT disc types
except at the patched words; the remaining ~744 words should differ wholesale.

Usage:  python compare_disc_boot.py a.bin b.bin [--limit N]
"""

import sys


def words(buf):
    return [(buf[i] << 8) | buf[i + 1] for i in range(0, len(buf) - 1, 2)]


def main(argv):
    limit = 192
    if "--limit" in argv:
        i = argv.index("--limit")
        limit = int(argv[i + 1])
        argv = argv[:i] + argv[i + 2:]
    a = words(open(argv[0], "rb").read())
    b = words(open(argv[1], "rb").read())
    n = min(len(a), len(b))
    diffs = [i for i in range(n) if a[i] != b[i]]
    head = [i for i in diffs if i < limit]
    print("file A: %s (%d words)" % (argv[0], len(a)))
    print("file B: %s (%d words)" % (argv[1], len(b)))
    print("differences in first %dB words (LOAD PROGRAM): %d of %d"
          % (limit, len(head), limit))
    for i in head:
        print("   %06o : A=%06o  B=%06o" % (i, a[i], b[i]))
    tail = [i for i in diffs if i >= limit]
    print("differences at/after word %dB (swap driver area): %d of %d"
          % (limit, len(tail), n - limit))


if __name__ == "__main__":
    main(sys.argv[1:])
