#!/usr/bin/env python3
"""decode_9read.py -- decode the BPUN load records embedded in a SINTRAN
distribution `SINTRAN*:DATA` MACM generation stream, into a flat memory image,
and search that image for the mass-storage boot program.

The `:DATA` stream is a MACM command file: ~7.4 KB of printable header (layout
parameters + patch macros), then a sequence of `)9READ` commands each followed by
a BPUN (binary punch) record:

    '!' | u16 load-address | u16 word-count | word*count | u16 checksum

all big-endian; the checksum is the plain 16-bit sum of the code words.

Searches performed on the reconstructed image (all octal):
  * RELOA prologue      150405 150001 ...            (PIOF, TRA STS)
  * the LOAD-PROGRAM tail  150206 x4 then IOX/---, 175025 (BSKP ZRO 20 DA)
  * any IOX in the SMD (1540-1557), Winchester (500-517) or drum (540-547) windows
  * IOXT (150415)

Usage:  python decode_9read.py <SINTRAN-x-n.DATA> [--dump out.bin]
"""

import sys

LAX = False  # --lax: accept records whose checksum does not verify (diagnostics)
REGION = None  # --region <record-index> <octal-addr> <count> <outfile>

PIOF = 0o150405
IOF = 0o150401
IOXT = 0o150415
MCLPID = 0o150206
BSKPZ20 = 0o175025


def parse_records(data):
    """Yield (offset, addr, count, words, cksum_ok) for every '!' record."""
    i = 0
    n = len(data)
    while True:
        j = data.find(b"!", i)
        if j < 0 or j + 5 > n:
            return
        addr = (data[j + 1] << 8) | data[j + 2]
        cnt = (data[j + 3] << 8) | data[j + 4]
        end = j + 5 + 2 * cnt + 2
        if cnt == 0 or end > n:
            i = j + 1
            continue
        w = [(data[j + 5 + 2 * k] << 8) | data[j + 6 + 2 * k] for k in range(cnt)]
        cks = (data[end - 2] << 8) | data[end - 1]
        ok = (sum(w) & 0xFFFF) == cks
        if not ok and not LAX:
            i = j + 1
            continue
        yield j, addr, cnt, w, ok
        i = end


def main(argv):
    global LAX, REGION
    if "--region" in argv:
        i = argv.index("--region")
        REGION = (int(argv[i + 1]), int(argv[i + 2], 8), int(argv[i + 3], 8),
                  argv[i + 4])
        argv = argv[:i] + argv[i + 5:]
    if "--lax" in argv:
        LAX = True
        argv = [a for a in argv if a != "--lax"]
    path = argv[0]
    dump = None
    if "--dump" in argv:
        dump = argv[argv.index("--dump") + 1]
    data = open(path, "rb").read()

    mem = {}
    recs = []
    for off, addr, cnt, w, ok in parse_records(data):
        recs.append((off, addr, cnt, ok))
        for k, x in enumerate(w):
            mem[(addr + k) & 0xFFFF] = x

    print("file %s (%d bytes)" % (path, len(data)))
    print("%d checksum-valid BPUN records" % len(recs))
    for off, addr, cnt, ok in recs:
        print("  @%-8d addr=%06o count=%06o (%d)  end=%06o" %
              (off, addr, cnt, cnt, (addr + cnt - 1) & 0xFFFF))

    if not mem:
        return
    lo, hi = min(mem), max(mem)
    print("image span %06o..%06o (%d words present)" % (lo, hi, len(mem)))

    # --- searches -------------------------------------------------------
    def w(a):
        return mem.get(a)

    hits = [a for a in sorted(mem) if w(a) == PIOF and w(a + 1) == 0o150001]
    print("PIOF+TRA-STS (RELOA prologue) at: %s" % ", ".join("%06o" % a for a in hits))

    hits = [a for a in sorted(mem)
            if all(w(a + k) == MCLPID for k in range(4)) and w(a + 5) == BSKPZ20]
    print("MCL PID x4 + BSKP ZRO 20 DA (LOAD-PROGRAM KLIOX site) at: %s"
          % ", ".join("%06o (KLIOX=%06o)" % (a, w(a + 4)) for a in hits))

    iox = {}
    for a in sorted(mem):
        x = w(a)
        if 0o164000 <= x <= 0o167777:
            d = x - 0o164000
            if 0o1540 <= d <= 0o1557 or 0o500 <= d <= 0o517 or 0o540 <= d <= 0o547:
                iox.setdefault(d, []).append(a)
    for d in sorted(iox):
        print("IOX %o : %d refs, first at %06o" % (d, len(iox[d]), iox[d][0]))
    nioxt = sum(1 for a in mem if w(a) == IOXT)
    print("IOXT (150415): %d words" % nioxt)

    if REGION:
        ridx, radr, rcnt, rout = REGION
        _, addr, cnt, w, _ = list(parse_records(data))[ridx]
        s = radr - addr
        sel = w[s:s + rcnt]
        with open(rout, "wb") as f:
            for x in sel:
                f.write(bytes((x >> 8, x & 0xFF)))
        print("record %d addr=%06o: extracted %d words from %06o to %s"
              % (ridx, addr, len(sel), radr, rout))
        for i, x in enumerate(sel):
            print("  %06o  %06o" % (radr + i, x))

    if dump:
        buf = bytearray(2 * (hi + 1))
        for a, x in mem.items():
            buf[2 * a] = x >> 8
            buf[2 * a + 1] = x & 0xFF
        open(dump, "wb").write(buf)
        print("dumped %d bytes to %s" % (len(buf), dump))


if __name__ == "__main__":
    main(sys.argv[1:])
