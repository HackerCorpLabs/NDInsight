#!/usr/bin/env python3
"""Find `SAA <ascii>` immediates for the characters a template walker must handle.

The ND-500-MON trap printer walks a NUL-separated pool where '$' marks a field and
standalone 'I<n>'/'O<n>' entries give the radix. Any parser has to compare against
those characters, and `SAA c` (0o170400 | c) is a single distinctive 16-bit word -
far less noisy than scanning for the pool's ADDRESS, which collides with ordinary
instruction encodings (that scan returned 1217 hits and was useless).

Usage: findchars.py bank1.bin
"""
import sys

B = open(sys.argv[1], 'rb').read()
N = len(B) // 2

CHARS = {
    '$': 0o44,    # field marker in the template
    'I': 0o111,   # decimal radix code
    'O': 0o117,   # octal radix code
    'B': 0o102,   # octal suffix printed in the report ("10533B")
    'D': 0o104,   # decimal suffix
    ':': 0o72,    # separator seen in "logical 0:4"
}

for name, c in CHARS.items():
    want = 0o170400 | c
    hits = []
    for i in range(N):
        if ((B[i * 2] << 8) | B[i * 2 + 1]) == want:
            hits.append(i)
    print("SAA '%s' (%06o): %d hits  %s"
          % (name, want, len(hits), ' '.join('%06o' % h for h in hits[:24])))
