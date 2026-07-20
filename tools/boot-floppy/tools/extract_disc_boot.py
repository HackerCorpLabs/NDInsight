#!/usr/bin/env python3
"""
extract_disc_boot.py -- decode the RetroFS embedded page-0 bootstraps and dump
page 0 of real installed Norsk Data hard-disk images.

Sources
-------
1. E:\\Dev\\Ronny\\RetroFS\\src\\RetroFS.NDFS\\Creation\\NdfsBootBlobs.cs
   Four base64 constants (SmdB64, WinchesterB64, ScsiB64, FloppyB64), each the
   full 2000-byte page-0 boot region lifted verbatim off a real drive.  This
   script parses the C# source directly so the blobs never have to be copied
   by hand.

2. Any raw disk image on disk.  Page 0 (bytes 0..2047) is read READ-ONLY; the
   script never opens an image for writing.

What it reports per blob/image
------------------------------
  * SHA-256 of the extracted region
  * length of meaningful content -- the offset just past the last non-zero,
    non-filler word (filler = 0x0000 or the 0xDB6D/0x2A / 0xAAAA style padding
    seen at the tail of the SMD and SCSI packs)
  * boot record format, using the same signatures as
    RetroFS.NDFS/Boot/NdfsBootLoader.cs:
        0150405 PIOF / 0150401 IOF  -> raw binary bootstrap
        '!' (0x21) BPUN record      -> BPUN / FLOMON
  * every IOX device address referenced (literal IOX, opcode 0164000 | dev)
    and every IOXT (0150415, device number taken from T at run time)

Usage
-----
    python extract_disc_boot.py blobs   <outdir>
    python extract_disc_boot.py image   <image path> <outdir>
    python extract_disc_boot.py scan    <image path> [...]   # summary only
"""

import base64
import hashlib
import os
import re
import sys

BLOBS_CS = r"E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Creation\NdfsBootBlobs.cs"
BOOT_REGION = 2000   # NdfsBootBlobs.BootRegionSize
PAGE = 2048          # NDFS page / block 0 size

OP_PIOF = 0o150405
OP_IOF = 0o150401
OP_IOXT = 0o150415
IOX_BASE = 0o164000
IOX_MASK = 0o174000   # top 5 bits (0xF800) -- IOX = 0164000 | 11-bit device address


def read_blobs(path=BLOBS_CS):
    """Return {'Smd': bytes, 'Winchester': bytes, 'Scsi': bytes, 'Floppy': bytes}."""
    src = open(path, encoding="utf-8").read()
    out = {}
    for m in re.finditer(r'const\s+string\s+(\w+)B64\s*=\s*\n?\s*"([A-Za-z0-9+/=]+)"', src):
        out[m.group(1)] = base64.b64decode(m.group(2))
    return out


def words(data):
    return [(data[i] << 8) | data[i + 1] for i in range(0, len(data) - 1, 2)]


def meaningful_len(data):
    """Byte offset just past the last word that is not trailing filler.

    Filler observed on real packs: 0x0000, and repeating byte patterns such as
    0xDB6D (SMD/Winchester tail) or 0xAAAA. We only strip a *trailing* run of
    words that all equal the same filler value, plus trailing zeros.
    """
    w = words(data)
    if not w:
        return 0
    i = len(w) - 1
    # strip trailing zeros
    while i >= 0 and w[i] == 0:
        i -= 1
    if i < 0:
        return 0
    # strip a trailing run of one repeated non-zero value (>= 8 words)
    fill = w[i]
    j = i
    while j >= 0 and w[j] == fill:
        j -= 1
    if i - j >= 8:
        i = j
        while i >= 0 and w[i] == 0:
            i -= 1
    return (i + 1) * 2


def classify(data):
    w0 = (data[0] << 8) | data[1]
    if w0 == OP_PIOF:
        return "raw-binary (starts PIOF 0150405)"
    if w0 == OP_IOF:
        return "raw-binary (starts IOF 0150401)"
    if 0x21 in data[:512]:
        return "BPUN/FLOMON candidate ('!' at byte %d)" % data.index(0x21)
    return "unknown"


def io_scan(data, limit=None):
    """Return (list of (wordindex, devaddr) for literal IOX, list of IOXT word indices)."""
    w = words(data if limit is None else data[:limit])
    iox = []
    ioxt = []
    for i, v in enumerate(w):
        if v == OP_IOXT:
            ioxt.append(i)
        elif (v & IOX_MASK) == IOX_BASE:
            iox.append((i, v & 0o3777))
    return iox, ioxt


def summarize(name, data):
    ml = meaningful_len(data)
    sha = hashlib.sha256(data).hexdigest()
    iox, ioxt = io_scan(data, ml)
    devs = sorted(set(d for _, d in iox))
    print("== %s ==" % name)
    print("  length          : %d bytes (%d words)" % (len(data), len(data) // 2))
    print("  sha256          : %s" % sha)
    print("  meaningful      : %d bytes (%d words)" % (ml, ml // 2))
    print("  format          : %s" % classify(data))
    print("  literal IOX     : %d instr, devices %s" %
          (len(iox), ", ".join(format(d, "04o") for d in devs) if devs else "none"))
    if iox:
        print("  first IOX at word %d" % iox[0][0])
    print("  IOXT (0150415)  : %d occurrences%s" %
          (len(ioxt), (", first at word %d" % ioxt[0]) if ioxt else ""))
    return sha, ml


def octal_dump(data, limit=None, per_line=8):
    w = words(data if limit is None else data[:limit])
    lines = []
    for i in range(0, len(w), per_line):
        chunk = w[i:i + per_line]
        lines.append("%06o: %s" % (i, " ".join(format(x, "06o") for x in chunk)))
    return "\n".join(lines)


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 1
    cmd = argv[1]
    if cmd == "blobs":
        outdir = argv[2] if len(argv) > 2 else "."
        os.makedirs(outdir, exist_ok=True)
        blobs = read_blobs()
        for name in ("Smd", "Winchester", "Scsi", "Floppy"):
            data = blobs[name]
            sha, ml = summarize(name, data)
            fn = os.path.join(outdir, "installed-%s-%s.bin" % (name.lower(), sha[:8]))
            with open(fn, "wb") as f:
                f.write(data)
            print("  written         : %s" % fn)
            print()
    elif cmd == "image":
        path = argv[2]
        outdir = argv[3] if len(argv) > 3 else "."
        with open(path, "rb") as f:           # READ-ONLY
            data = f.read(PAGE)
        summarize(os.path.basename(path), data)
    elif cmd == "scan":
        for path in argv[2:]:
            try:
                with open(path, "rb") as f:   # READ-ONLY
                    data = f.read(PAGE)
            except OSError as e:
                print("%s: %s" % (path, e))
                continue
            if len(data) < PAGE:
                continue
            w0 = (data[0] << 8) | data[1]
            ml = meaningful_len(data)
            iox, ioxt = io_scan(data, ml)
            devs = sorted(set(d for _, d in iox))
            print("%-55s w0=%06o mlen=%5d IOX=%-28s IOXT=%d sha=%s" % (
                path, w0, ml,
                ",".join(format(d, "04o") for d in devs)[:28] or "-",
                len(ioxt), hashlib.sha256(data[:BOOT_REGION]).hexdigest()[:12]))
    else:
        print(__doc__)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
