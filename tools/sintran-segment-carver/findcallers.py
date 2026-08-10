#!/usr/bin/env python3
"""Find every JPL/JMP in a carved segment whose resolved target falls in a range.

Usage: findcallers.py <segment.bin> <base-oct> <lo-oct> <hi-oct>
"""
import sys

IMG = open(sys.argv[1], 'rb').read()
BASE = int(sys.argv[2], 8)
LO = int(sys.argv[3], 8)
HI = int(sys.argv[4], 8)
NW = len(IMG) // 2


def w(a):
    off = a - BASE
    return (IMG[off * 2] << 8) | IMG[off * 2 + 1] if 0 <= off < NW else -1


hits = []
for off in range(NW):
    a = BASE + off
    v = w(a)
    op = (v >> 11) << 2
    X, I, Bb = (v >> 10) & 1, (v >> 9) & 1, (v >> 8) & 1
    disp = v & 0o377
    if disp > 127:
        disp -= 256
    if op not in (0o124, 0o134) or X or Bb:
        continue
    tgt = w(a + disp) if I else a + disp
    if LO <= tgt <= HI:
        hits.append((a, 'JPL' if op == 0o134 else 'JMP', 'I' if I else ' ', tgt))

print('call sites into %06o..%06o : %d' % (LO, HI, len(hits)))
for a, k, i, t in hits:
    print('  %06o  %s %s -> %06o' % (a, k, i, t))
