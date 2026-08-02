#!/usr/bin/env python3
# Byte-swap a big-endian ND segment carve to little-endian, which is the ONLY form
# nd100-dis accepts. The carved .bin is big-endian (for Ghidra); swap for nd100-dis only.
import sys

d = open(sys.argv[1], 'rb').read()
out = bytearray(len(d))
for i in range(0, len(d) - 1, 2):
    out[i] = d[i + 1]
    out[i + 1] = d[i]
open(sys.argv[2], 'wb').write(bytes(out))
print('swapped %d bytes -> %s' % (len(d), sys.argv[2]))
