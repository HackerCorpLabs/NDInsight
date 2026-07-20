#!/usr/bin/env python3
"""
nd_sixbit_decode.py -- decode Norsk Data 30-bit packed 6-bit symbol names.

WHY THIS EXISTS
---------------
MAC / MACM store symbol names (and MACM's SINTRAN "library marks") as a
30-bit field held in TWO consecutive 16-bit words:

    word0 = name bits 29..16   (the top two bits of word0 are FLAGS, not name)
    word1 = name bits 15..0

The 30 bits are five 6-bit character codes, most-significant group first,
so a name shorter than five characters is RIGHT-justified with zero groups
on the left.  This is why "MAC significance is the LAST 5 characters".

Character mapping (verified against MACM-1718L.BPUN):
    code < 0o40  ->  chr(code + 0o100)      e.g. 8 -> 'H', 18 -> 'R'
    code >= 0o40 ->  chr(code)              e.g. 0o62 -> '2', 0o70 -> '8'
    code == 0     ->  padding (rendered as '')

This is plain 7-bit ASCII with bit 6 dropped and re-derived, so both
letters and digits round-trip.  Digits matter: the mark "BD288" is
(2, 4, 0o62, 0o70, 0o70) and would decode as garbage under an A=1..Z=26 map.

VERIFICATION ANCHORS (MACM-1718L.BPUN, D:\\ND\\BPUN copy)
    ram:8760 = 0x0020,0x5394 -> "HENT"   (command, handler ram:9913)
    ram:8763 = 0x001c,0xa14d -> "GJEM"   (command, handler ram:990b)
    ram:872a = 0x1214,0x4146 -> "REDEF"  (command, handler ram:9257)
    ram:980f = 0x0213,0x2e38 -> "BD288"  (library mark)
    ram:9815 = 0x130c,0x14c9 -> "SCASI"  (library mark)

USAGE
    python nd_sixbit_decode.py 0213 2e38          # word pair, hex
    python nd_sixbit_decode.py 0x02132e38         # single 32-bit value
    python nd_sixbit_decode.py -e BD288           # encode instead
"""

import sys


def decode30(value):
    """Decode a 30-bit packed name (low 30 bits of `value`) to a string.

    The top two bits are ignored: in MAC's permanent symbol table they are
    entry flags, not part of the name.
    """
    value &= 0x3FFFFFFF
    out = []
    for shift in (24, 18, 12, 6, 0):          # MSB group = first character
        code = (value >> shift) & 0o77
        if code == 0:
            out.append('')                     # left padding for short names
        elif code < 0o40:
            out.append(chr(code + 0o100))      # letters / '@' / punctuation
        else:
            out.append(chr(code))              # digits and other ASCII
    return ''.join(out)


def encode30(name):
    """Encode up to 5 characters into a 30-bit packed name (right-justified)."""
    name = name.upper()[-5:]
    value = 0
    for ch in name.rjust(5, '\0'):
        code = 0 if ch == '\0' else (ord(ch) & 0o77)
        value = (value << 6) | code
    return value


def main(argv):
    if len(argv) >= 2 and argv[0] == '-e':
        v = encode30(argv[1])
        print("0x%08x  (words 0x%04x 0x%04x)" % (v, (v >> 16) & 0xFFFF, v & 0xFFFF))
        return 0
    if not argv:
        print(__doc__)
        return 1
    if len(argv) == 1:
        v = int(argv[0], 16) if not argv[0].startswith('0x') else int(argv[0], 0)
    else:
        # two words: high word first, as they appear in memory
        v = (int(argv[0], 16) << 16) | int(argv[1], 16)
    print("0x%08x -> '%s'" % (v, decode30(v)))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
