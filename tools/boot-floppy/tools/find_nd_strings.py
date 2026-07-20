#!/usr/bin/env python3
"""
find_nd_strings.py -- multi-encoding literal-string hunter for Norsk Data media.

WHY THIS EXISTS
---------------
ND (SINTRAN III / MACM / NPL) stores text in several different physical forms on
the same 16-bit big-endian machine, and a naive `grep` finds only one of them:

  1. plain 7-bit ASCII                       'T' 'A' 'N' ...
  2. "parity set" 8-bit ASCII                each byte OR 0x80  (0xD4 0xC1 0xCE ...)
  3. byte-swapped 16-bit words               'A' 'T' 'D' 'N' ...   (little-endian
     dump of big-endian words, or a word-order-reversed copy)
  4. byte-swapped AND parity set             combinations of 2 and 3
  5. an odd-offset ("shifted") variant of 3/4 -- the pair boundary can fall
     either side of the first character, so both phases are tried.

This tool searches a file for every needle in ALL of the above forms, case
insensitively, and prints file, decimal + octal byte offset, which encoding
matched, and a hexdump + printable rendering of the surrounding bytes.

USAGE
    python find_nd_strings.py [-c N] [-n NEEDLE]... PATH [PATH...]

    PATH may be a file or a directory (directories are walked recursively).
    -n / --needle   add a needle (repeatable). If none given, a built-in list
                    of SCSI vendor/product candidates is used.
    -c / --context  bytes of context to dump either side of a hit (default 48).

EXIT STATUS
    0 if at least one hit was found, 1 if none.

NOTE ON CASE INSENSITIVITY
    Matching is done by upper-casing both the needle and a copy of the data
    (ASCII only, and after masking bit 7 for the parity-set variants), so a hit
    is reported regardless of the case actually stored.
"""

import os
import sys
import argparse

# Default needles: the SCSI vendor / product identification strings claimed by
# the ndwiki "SCSI-TV" article, plus generic SCSI-ish words worth a sweep.
DEFAULT_NEEDLES = [
    b"NDMICROP", b"MICROP", b"TANDBERG", b"TDC", b"ARCHIVE", b"VIPER",
    b"NDCDC", b"EMD", b"NDSTK", b"97201", b"21247", b"2925", b"1375",
    b"LD 1200", b"TDC 3600", b"SCSI", b"INQUIRY", b"VENDOR", b"PRODUCT",
]


def _upper(b: bytes) -> bytes:
    """ASCII upper-case a byte string, leaving non-letters alone."""
    return bytes((c - 32) if 0x61 <= c <= 0x7A else c for c in b)


def _swap(b: bytes) -> bytes:
    """Swap every adjacent byte pair. Odd trailing byte is dropped."""
    n = len(b) & ~1
    out = bytearray(n)
    out[0::2] = b[1:n:2]
    out[1::2] = b[0:n:2]
    return bytes(out)


def variants(data: bytes):
    """
    Yield (name, transformed_data, offset_map) tuples.

    offset_map(i) converts an index in the transformed buffer back to a byte
    offset in the ORIGINAL file, so every reported offset is a real file offset.
    """
    # 1/2: parity is handled by masking the DATA, not by expanding the needle:
    #      masking bit 7 makes plain and parity-set text look identical.
    masked = bytes(c & 0x7F for c in data)
    yield ("ascii/parity", _upper(masked), lambda i: i)

    # 3/4: byte-swapped, both phases (even boundary and odd boundary).
    sw0 = _swap(masked)
    yield ("byteswap", _upper(sw0), lambda i: (i ^ 1) if (i ^ 1) < len(data) else i)

    sw1 = _swap(masked[1:])
    yield ("byteswap+1", _upper(sw1), lambda i: 1 + ((i ^ 1) if 1 + (i ^ 1) < len(data) else i))


def render(data: bytes, lo: int, hi: int) -> str:
    """Printable rendering: bit-7 stripped, non-printables shown as '.'"""
    out = []
    for c in data[lo:hi]:
        c &= 0x7F
        out.append(chr(c) if 32 <= c < 127 else ".")
    return "".join(out)


def scan_file(path: str, needles, ctx: int) -> int:
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError as e:
        print(f"!! cannot read {path}: {e}", file=sys.stderr)
        return 0

    hits = 0
    seen = set()
    for vname, buf, omap in variants(data):
        for needle in needles:
            n = _upper(needle)
            start = 0
            while True:
                i = buf.find(n, start)
                if i < 0:
                    break
                start = i + 1
                off = omap(i)
                key = (off, needle)
                if key in seen:
                    continue
                seen.add(key)
                hits += 1
                lo = max(0, off - ctx)
                hi = min(len(data), off + len(needle) + ctx)
                print(f"\n=== {path}")
                print(f"    needle={needle.decode('latin1')!r}  encoding={vname}"
                      f"  offset={off} (0o{off:o}, 0x{off:X})")
                print(f"    text : {render(data, lo, hi)}")
                print(f"    hex  : {data[lo:hi].hex(' ')}")
    return hits


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("paths", nargs="+")
    ap.add_argument("-n", "--needle", action="append", default=[])
    ap.add_argument("-c", "--context", type=int, default=48)
    a = ap.parse_args()

    needles = [s.encode("latin1") for s in a.needle] if a.needle else DEFAULT_NEEDLES

    total = 0
    for p in a.paths:
        if os.path.isdir(p):
            for root, _dirs, files in os.walk(p):
                for fn in files:
                    total += scan_file(os.path.join(root, fn), needles, a.context)
        else:
            total += scan_file(p, needles, a.context)

    print(f"\n--- {total} hit(s)")
    return 0 if total else 1


if __name__ == "__main__":
    sys.exit(main())
