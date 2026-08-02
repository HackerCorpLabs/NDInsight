#!/usr/bin/env python3
# Generate the ND-500-MON J04 command -> handler map straight from the two bank images.
# Every column below is read from bytes; nothing here is inferred.
import sys

B1 = open(sys.argv[1], 'rb').read()   # nd-500-mon-j04-bank1.bin  (program bank, loads at 0)
B2 = open(sys.argv[2], 'rb').read()   # nd-500-mon-j04-bank2.bin  (data bank, loads at 0, PTM=1)


def w2(a):
    """One big-endian word of the DATA bank at word address a."""
    return (B2[a * 2] << 8) | B2[a * 2 + 1]


def txt(a, n):
    """n bytes of the data bank as printable text (non-printables -> '.')."""
    out = []
    for k in range(n):
        c = B2[a * 2 + k]
        out.append(chr(c) if 32 <= c < 127 else '.')
    return ''.join(out)


DESC = 0o011547     # base of the 3-word-per-command descriptor array
TABLE = 0o020671    # base of the 1-word-per-command handler-address table

# Walk the descriptor array. Each entry is (name_ptr, 0, byte_length).
# It terminates when the pointer leaves the string-table range or the middle
# word is non-zero - both checks, so a single bad word cannot run us off the end.
ents = []
a = DESC
while True:
    p, z, ln = w2(a), w2(a + 1), w2(a + 2)
    if not (0o012450 <= p <= 0o020060) or z != 0:
        break
    ents.append((a, p, ln))
    a += 3

print('| # | Command | Descriptor | Name ptr | Handler (bank 1) |')
print('|---|---|---|---|---|')
for i, (d, p, ln) in enumerate(ents):
    # The entry text is NAME + '\' + parameter descriptor; the name stops at the
    # first backslash. Commands with no parameters have no backslash at all.
    name = txt(p, ln).split('\\')[0]
    print('| %d | `%s` | `%06o` | `%06o` | `%06o` |' % (i, name, d, p, w2(TABLE + i)))

print()
print('commands: %d   descriptor array: 0o%o..0o%o   handler table: 0o%o..0o%o'
      % (len(ents), DESC, a - 1, TABLE, TABLE + len(ents) - 1))
print('word immediately after the handler table: 0o%06o' % w2(TABLE + len(ents)))
