#!/usr/bin/env python3
"""decode_start_vectors.py -- decode the ND-100 / SINTRAN low-memory START VECTOR
table from a carved resident image, and resolve each vector target against a
SINTRAN symbol list.

WHY THIS EXISTS
---------------
The MOPC console command ``NN!`` means "start the program in main memory at
octal address NN" (ND-06.015.02 sec 7.2.1, manual-sourced).  The SINTRAN
generation crib prints ``22!  => START SINTRAN``.  To prove *what* 22! starts we
must read the word actually sitting at address 22B in the resident image that is
in core when 22! is typed.

SINTRAN places a little vector table in the first ~0o40 words of the resident
image: pairs of ``JMP I 1`` (opcode 0o125001) followed by a data word holding the
real entry-point address.  So starting at an even vector address executes the
JMP-indirect and lands on the routine named by the following word.  This script
finds those ``125001`` entries and prints (vector, target, symbol).

Verified example (L-VSX-500 resident SINTRAN-DATA_commoncode.bin):
    0o16 -> 0o17=030500 MEMTO
    0o20 -> 0o21=033073 RESTA   (RESTART-SYSTEM)
    0o22 -> 0o23=042645 SINTR   (cold start -> writes disc page 0 via PL011)

USAGE
    python decode_start_vectors.py <resident.bin> [SYMBOL-2-LIST.SYMB.TXT] [--words 040]

All inputs are opened READ-ONLY.  Big-endian 16-bit words (ND-100 order).
"""
import sys

JMP_I_1 = 0o125001  # JMP I 1 : jump indirect through (P+1), i.e. the next word


def load_words(path, n):
    """Read the first n big-endian 16-bit words of a raw ND-100 image."""
    with open(path, "rb") as f:            # READ-ONLY
        buf = f.read(2 * n)
    return [(buf[2 * i] << 8) | buf[2 * i + 1] for i in range(len(buf) // 2)]


def load_symbols(path):
    """Parse a SINTRAN 'NAME=octal' symbol list into {value: name}. Best effort."""
    by_val = {}
    if not path:
        return by_val
    with open(path, "r", errors="replace") as f:  # READ-ONLY
        for line in f:
            line = line.strip()
            if "=" not in line:
                continue
            name, _, val = line.partition("=")
            name = name.strip()
            val = val.strip().split()[0] if val.strip() else ""
            try:
                v = int(val, 8)               # symbol values are octal
            except ValueError:
                continue
            # keep the first name seen for a value (definition order)
            by_val.setdefault(v, name)
    return by_val


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 1
    binpath = argv[1]
    sympath = argv[2] if len(argv) > 2 and not argv[2].startswith("--") else None
    nwords = 0o40
    if "--words" in argv:
        nwords = int(argv[argv.index("--words") + 1], 8)

    words = load_words(binpath, nwords)
    syms = load_symbols(sympath)

    print(f"; {binpath}: first {nwords:o}B words, big-endian")
    print(f"; symbol table: {sympath or '(none)'}")
    print(";  addr  value    meaning")
    for a in range(len(words) - 1):
        w = words[a]
        if w == JMP_I_1:
            tgt = words[a + 1]
            name = syms.get(tgt, "")
            print(f"  {a:04o}  {w:06o}  JMP I 1 -> {a+1:04o}={tgt:06o} {name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
