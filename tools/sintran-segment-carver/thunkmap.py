#!/usr/bin/env python3
"""Map every MON 60 thunk call site in ND-500-MON J04 bank 1.

The thunk table is 146310..147067, 123 entries x 3 words:
    +0  SAA <subfunction code>   (170400 | code)
    +1  JMP I 1                  (125001)
    +2  146244                   (-> the MON 60 gateway)

A caller reaches a thunk with `JPL I <disp>` through a pointer word, so the call
site's operand is NOT the thunk address - one more level has to be resolved.

Usage: thunkmap.py bank1.bin
"""
import sys

B1 = open(sys.argv[1], 'rb').read()
N = len(B1) // 2

THUNK_LO, THUNK_HI = 0o146310, 0o147070


def w(a):
    return (B1[a * 2] << 8) | B1[a * 2 + 1] if 0 <= a < N else -1


def thunk_code(t):
    """Subfunction code of the thunk starting at t, or None if t is not a thunk."""
    if not (THUNK_LO <= t < THUNK_HI):
        return None
    if (t - THUNK_LO) % 3 != 0:
        return None
    saa, jmp, gate = w(t), w(t + 1), w(t + 2)
    # Verify all three words, so a coincidental address cannot be mistaken for a thunk.
    if (saa & 0o177400) != 0o170400 or jmp != 0o125001 or gate != 0o146244:
        return None
    return saa & 0o377


sites = []
for a in range(N):
    v = w(a)
    op = (v >> 11) << 2
    X, I, Bb = (v >> 10) & 1, (v >> 9) & 1, (v >> 8) & 1
    disp = v & 0o377
    if disp > 127:
        disp -= 256
    if op != 0o134 or X or Bb:          # JPL only, no indexing, not B-relative
        continue
    tgt = w((a + disp) & 0xFFFF) if I else (a + disp) & 0xFFFF
    c = thunk_code(tgt)
    if c is not None:
        sites.append((a, tgt, c))

print('thunk call sites: %d' % len(sites))
by_code = {}
for a, t, c in sites:
    by_code.setdefault(c, []).append(a)
for c in sorted(by_code):
    print('  subfn %03o  thunk %06o  called from: %s'
          % (c, THUNK_LO + 0, ' '.join('%06o' % x for x in by_code[c])))
