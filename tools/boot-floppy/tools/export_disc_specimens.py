#!/usr/bin/env python3
"""export_disc_specimens.py -- name, save and document ND-100 mass-storage boot pages.

Takes page-0 boot regions previously dumped by scan_disc_boot.py (2000-byte files),
identifies them from the structure of SINTRAN's LOAD PROGRAM, and writes
    disc-<device>-<sha8>.bin  +  disc-<device>-<sha8>.md
into the target directory.

Identification anchors (from PH-P2-OPPSTART.NPL, the LOAD PROGRAM source):

    NALOY, MCL PID; MCL PID; MCL PID; MCL PID
    KLIOX, IOX 4; BSKP ZRO 20 DA; JMP NALOY; LDA NALO5; JMP NALO4
    NALO2, IDENT PL11; JMP I (SINTR

so the byte pattern  150206 x4 , <KLIOX> , 175025  locates the one instruction word
that COLD-START patches with `IOX (HDEV+4)`. YSWTY (1 = SMD "big disc", 2 = Winchester,
3 = SCSI, 0 = other) sits 16 words before KLIOX in every specimen examined.

Usage: python export_disc_specimens.py <indir-of-p0-*.bin> <outdir> <provenance.json>

provenance.json maps sha8 -> list of source image paths (produced by hand or from the
scan_disc_boot.py listing).
"""

import hashlib
import json
import os
import sys

MCLPID = 0o150206
BSKP = 0o175025
YSWTY_NAME = {0: "other", 1: "SMD (big disc, ZBDIS)", 2: "Winchester (ZWDIS)",
              3: "SCSI (SCSWD)"}
DEVTAG = {1: "smd", 2: "winchester", 3: "scsi", 0: "unknown"}


def words(b):
    return [(b[i] << 8) | b[i + 1] for i in range(0, len(b) - 1, 2)]


def analyse(w):
    kliox = None
    for i in range(len(w) - 6):
        if all(w[i + k] == MCLPID for k in range(4)) and w[i + 5] == BSKP:
            kliox = i + 4
            break
    yswty = w[kliox - 16] if kliox is not None and kliox >= 16 else None
    hdev = (w[kliox] - 0o164000 - 4) if (kliox is not None
                                         and 0o164000 <= w[kliox] <= 0o167777) else None
    return kliox, yswty, hdev


def main(argv):
    indir, outdir, provfile = argv[0], argv[1], argv[2]
    prov = json.load(open(provfile)) if os.path.exists(provfile) else {}
    os.makedirs(outdir, exist_ok=True)
    for fn in sorted(os.listdir(indir)):
        if not fn.endswith(".bin"):
            continue
        b = open(os.path.join(indir, fn), "rb").read()
        sha = hashlib.sha256(b).hexdigest()
        s8 = sha[:8]
        w = words(b)
        if w[0] not in (0o150405, 0o150401):
            continue  # not a raw mass-storage bootstrap
        kliox, yswty, hdev = analyse(w)
        tag = DEVTAG.get(yswty, "unknown")
        base = "disc-%s-%s" % (tag, s8)
        open(os.path.join(outdir, base + ".bin"), "wb").write(b)
        src = prov.get(s8, ["(source image not recorded)"])
        with open(os.path.join(outdir, base + ".md"), "w") as f:
            f.write("# %s\n\n" % base)
            f.write("Real ND-100 **mass-storage boot page** - page 0, bytes 0..1999 "
                    "of an installed SINTRAN III system disc.\n\n")
            f.write("| | |\n|---|---|\n")
            f.write("| sha256 | `%s` |\n" % sha)
            f.write("| size | %d bytes (%d words) |\n" % (len(b), len(w)))
            f.write("| word 0 | `%06o` (%s) |\n"
                    % (w[0], "PIOF" if w[0] == 0o150405 else "IOF"))
            f.write("| KLIOX word index | %s |\n"
                    % ("%06oB" % kliox if kliox is not None else "not located"))
            f.write("| KLIOX value | %s |\n"
                    % ("%06o" % w[kliox] if kliox is not None else "-"))
            f.write("| implied HDEV | %s |\n"
                    % ("%o octal" % hdev if hdev is not None else
                       "n/a (not a literal IOX - SCSI uses IOXT)"))
            f.write("| YSWTY | %s = %s |\n"
                    % ("%o" % yswty if yswty is not None else "?",
                       YSWTY_NAME.get(yswty, "?")))
            f.write("\n**Extracted from (read-only):**\n\n")
            for p in src:
                f.write("- `%s`\n" % p)
            f.write("\n**How this page was produced on the real machine** - SINTRAN's "
                    "own COLD-START/RESTART-SYSTEM code (`PH-P2-OPPSTART.NPL`, PL011) "
                    "reads page 0, copies the fixed LOAD PROGRAM `RELOA..LDEND` "
                    "(0300B = 192 words) plus the disc-type-specific *swap driver* "
                    "(`SWDSI` = 1350B = 744 words) into it, patches the parameter "
                    "words (KLIOX = `IOX HDEV+4`, KLHDE = HDEV, YSWTY, NOBLK, DYBLS, "
                    "LDRAD, ADR2B, KLRC1, KBLSZ), and writes the page back.\n")
    print("done -> %s" % outdir)


if __name__ == "__main__":
    main(sys.argv[1:])
