#!/usr/bin/env python3
"""Resolve JPL/JMP call targets inside a carved SINTRAN segment loaded at a base.

Same one-more-level rule as resolve.py: `JPL I <disp>` names a POINTER word at P+disp;
the routine is that word's contents. Within a segment the pointer lives in the same
segment, so both lookups are image-local.

Usage: segcalls.py <bigendian-segment.bin> <base-octal> <start-octal> <count-dec>
"""
import sys

IMG = open(sys.argv[1], 'rb').read()
BASE = int(sys.argv[2], 8)
start = int(sys.argv[3], 8)
count = int(sys.argv[4], 10)


def w(runtime_addr):
    """Big-endian word at a RUNTIME word address, or -1 if outside the image."""
    off = runtime_addr - BASE
    if off < 0 or off * 2 + 1 >= len(IMG):
        return -1
    return (IMG[off * 2] << 8) | IMG[off * 2 + 1]


targets = {}
for k in range(count):
    a = start + k
    v = w(a)
    if v < 0:
        break
    op = (v >> 11) << 2
    X, I, Bb = (v >> 10) & 1, (v >> 9) & 1, (v >> 8) & 1
    disp = v & 0o377
    if disp > 127:
        disp -= 256
    if op not in (0o124, 0o134) or X or Bb:   # JMP / JPL only, no index, not B-relative
        continue
    kind = 'JPL' if op == 0o134 else 'JMP'
    if I:
        ptr = a + disp
        tgt = w(ptr)
        print('%06o  %06o  %s I -> ptr @%06o -> %06o' % (a, v, kind, ptr, tgt))
    else:
        tgt = a + disp
        print('%06o  %06o  %s direct -> %06o' % (a, v, kind, tgt))
    targets.setdefault(tgt, 0)
    targets[tgt] += 1

print()
print('distinct targets:', ' '.join('%06o' % t for t in sorted(targets)))
