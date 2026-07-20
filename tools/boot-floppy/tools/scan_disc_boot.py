#!/usr/bin/env python3
"""scan_disc_boot.py -- survey ND-100 mass-storage boot pages (page 0) of real disk images.

The ND-100 firmware "mass storage load" reads 1 K words from mass-storage address 0
into memory address 0 and starts at 0 (ND-06.015.02 sec. 7.2.5.2). So page 0 of a
bootable SINTRAN hard disk is a raw ND-100 program. This script opens each image
READ-ONLY, reads the first 2048 bytes, classifies the boot program, and reports the
IOX/IOXT device numbers it references.

Opcodes used for classification (all octal):
    PIOF  = 150405   IOF = 150401   IOXT = 150415
    IOX n = 164000 + n   (n in 0..3777)

Usage:
    python scan_disc_boot.py <image> [<image> ...]
    python scan_disc_boot.py --extract <outdir> <image> [...]

Nothing is ever written to the images.
"""

import hashlib
import os
import sys

PAGE = 2048
BOOT_REGION = 2000  # bytes 0..1999 belong to the boot code; 2000+ is ext-info/label

# hard-disk controller device windows we care about (octal ranges, inclusive)
WINDOWS = [
    ("SMD/ECC  ", 0o1540, 0o1557),
    ("Winchester", 0o500, 0o517),
    ("Drum     ", 0o540, 0o547),
    ("Floppy   ", 0o1560, 0o1567),
    ("Console  ", 0o300, 0o307),
]


def words(buf):
    """Big-endian 16-bit words of buf."""
    return [(buf[i] << 8) | buf[i + 1] for i in range(0, len(buf) - 1, 2)]


def classify(w):
    """Return (kind, sorted iox device list, has_ioxt, ident_levels)."""
    iox = {}
    ioxt = 0
    ident = []
    for i, x in enumerate(w):
        if 0o164000 <= x <= 0o167777:
            iox[x - 0o164000] = iox.get(x - 0o164000, 0) + 1
        elif x == 0o150415:
            ioxt += 1
        elif 0o143600 <= x <= 0o143617:  # IDENT PLnn
            ident.append(x - 0o143600)
    if not w:
        return "empty", iox, ioxt, ident
    first = w[0]
    if first == 0o150405:
        kind = "RAW mass-storage bootstrap (PIOF)"
    elif first == 0o150401:
        kind = "RAW mass-storage bootstrap (IOF)"
    elif all(x == 0 for x in w[:16]):
        kind = "zero / not bootable"
    elif all(x == 0o40 for x in w[:16]):
        kind = "space-filled / not bootable"
    else:
        kind = "other / BPUN?"
    return kind, iox, ioxt, ident


def window_of(dev):
    for name, lo, hi in WINDOWS:
        if lo <= dev <= hi:
            return name.strip()
    return "?"


def main(argv):
    outdir = None
    if argv and argv[0] == "--extract":
        outdir = argv[1]
        argv = argv[2:]
        os.makedirs(outdir, exist_ok=True)

    seen = {}
    for path in argv:
        try:
            with open(path, "rb") as f:  # READ-ONLY
                page = f.read(PAGE)
        except OSError as e:
            print("%-60s ERROR %s" % (path, e))
            continue
        if len(page) < PAGE:
            print("%-60s too small" % path)
            continue
        boot = page[:BOOT_REGION]
        w = words(boot)
        kind, iox, ioxt, ident = classify(w)
        sha = hashlib.sha256(boot).hexdigest()[:8]
        devs = " ".join(
            "%o(%s)x%d" % (d, window_of(d), c) for d, c in sorted(iox.items())
        )
        print("%-58s %-8s %-34s IOXT=%d IDENT=%s %s"
              % (os.path.basename(path), sha, kind, ioxt,
                 ",".join(str(i) for i in sorted(set(ident))), devs))
        seen.setdefault(sha, (boot, []))[1].append(path)

    if outdir:
        import json
        for sha, (boot, paths) in seen.items():
            with open(os.path.join(outdir, "p0-%s.bin" % sha), "wb") as f:
                f.write(boot)
        with open(os.path.join(outdir, "provenance.json"), "w") as f:
            json.dump({s: p for s, (_, p) in seen.items()}, f, indent=1)
        print("\n%d unique boot regions written to %s" % (len(seen), outdir))


if __name__ == "__main__":
    main(sys.argv[1:])
