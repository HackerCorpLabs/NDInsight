#!/usr/bin/env python3
"""find_disc_layout_table.py -- locate and dump SINTRAN III's DISC LAYOUT table
(the DTxxx records pointed at by DISPE) inside a CARVED SINTRAN segment binary.

Why this exists
---------------
SINTRAN III keeps its per-disc-type geometry in a table of fixed-size records,
one per supported disc format.  Each record is DILEZ = 11 octal = 9 words:

    word 0  SECWO  words per sector          (normally 1000B = 512 words = 1024 bytes)
    word 1  SECTR  sectors per track
    word 2  SECSY  sectors per cylinder
    word 3  MAXCY  max cylinder value
    word 4  POLSY  first cylinder in pool
    word 5  REFOR  format type (0 / 10B / 20B)
    word 6  RESCY  first reserved cylinder
    word 7  ALTFO  address of alternative-format record, or 0
    word 8  DISPN  index of this entry (the "format number")

The table is followed by DISPE, an array of MAXDI+1 = 50B pointers indexed by
disc-type number; a zero pointer means "type not supported by this build".

Detection is done on the *invariant* of the record layout rather than on any
single geometry value: a run of >= MIN_RUN consecutive 9-word records whose
word 0 is a plausible SECWO (1000B or 2000B) and whose word 8 (DISPN) is a
small integer that increases across the run.

Carved segment .bin files are BIG-ENDIAN 16-bit words, as produced by carve.py.

Usage
-----
    python find_disc_layout_table.py <segment.bin> <load_base_octal> [--min-run N]

Read-only: this script never writes to the file it is given.
"""

import sys
import struct

RECORD_WORDS = 9          # DILEZ = 11B
PLAUSIBLE_SECWO = (0o1000, 0o2000)
MIN_RUN_DEFAULT = 6


def read_words_be(path):
    """Return the whole file as a list of big-endian 16-bit words."""
    with open(path, "rb") as fh:
        data = fh.read()
    n = len(data) // 2
    return list(struct.unpack(">%dH" % n, data[: n * 2]))


def looks_like_record(w, i):
    """True if the 9 words starting at index i look like one DTxxx record."""
    if i + RECORD_WORDS > len(w):
        return False
    if w[i] not in PLAUSIBLE_SECWO:
        return False
    dispn = w[i + RECORD_WORDS - 1]
    # DISPN is a disc-type index; MAXDI = 47B, so it must be small and non-zero.
    return 0 < dispn <= 0o47


def find_runs(w, min_run):
    """Yield (start_index, record_count) for each run of consecutive records."""
    i = 0
    while i < len(w):
        if looks_like_record(w, i):
            j = i
            count = 0
            while looks_like_record(w, j):
                j += RECORD_WORDS
                count += 1
            if count >= min_run:
                yield i, count
            i = j
        else:
            i += 1


def dump_run(w, start, count, base):
    hdr = ("addr", "SECWO", "SECTR", "SECSY", "MAXCY",
           "POLSY", "REFOR", "RESCY", "ALTFO", "DISPN")
    print("  " + " ".join("%-7s" % h for h in hdr))
    for k in range(count):
        i = start + k * RECORD_WORDS
        addr = base + i
        vals = w[i:i + RECORD_WORDS]
        print("  %-7o " % addr + " ".join("%-7o" % v for v in vals))


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    path = sys.argv[1]
    base = int(sys.argv[2], 8)
    min_run = MIN_RUN_DEFAULT
    if "--min-run" in sys.argv:
        min_run = int(sys.argv[sys.argv.index("--min-run") + 1])

    w = read_words_be(path)
    found = False
    for start, count in find_runs(w, min_run):
        found = True
        print("DISC LAYOUT TABLE candidate: %d records at %o (file word %d, byte %d)"
              % (count, base + start, start, start * 2))
        dump_run(w, start, count, base)
        print()
    if not found:
        print("NOT FOUND: no run of >= %d disc-layout records in %s" % (min_run, path))


if __name__ == "__main__":
    main()
