#!/usr/bin/env python3
"""Print a bank-1 range of ND-500-MON J04 with INDIRECT call/jump targets resolved.

nd100-dis renders `JPL I 77` with `-> 030402`, but 030402 is the POINTER WORD, not
the routine. This resolves one more level (the trap the carving skill warns about:
"indirect jumps land on POINTER words, not code").

Usage: resolve.py bank1.bin bank2.bin <start-octal> <count>
"""
import sys

B1 = open(sys.argv[1], 'rb').read()
B2 = open(sys.argv[2], 'rb').read()
start = int(sys.argv[3], 8)
count = int(sys.argv[4], 10)


def w1(a):
    return (B1[a * 2] << 8) | B1[a * 2 + 1]


def w2(a):
    return (B2[a * 2] << 8) | B2[a * 2 + 1]


# Memory-reference opcodes we care about, by the top 5 bits (value << 11).
MEMREF = {
    0o000: 'STZ', 0o004: 'STA', 0o010: 'STT', 0o014: 'STX',
    0o020: 'STD', 0o024: 'LDD', 0o030: 'STF', 0o034: 'LDF',
    0o040: 'MIN', 0o044: 'LDA', 0o050: 'LDT', 0o054: 'LDX',
    0o060: 'ADD', 0o064: 'SUB', 0o070: 'AND', 0o074: 'ORA',
    0o100: 'FAD', 0o104: 'FSB', 0o110: 'FMU', 0o114: 'FDV',
    # 0o130 is the conditional-jump group (JAP/JAN/JAZ/JPC/...), NOT a memory
    # reference - deliberately absent. JPL is 0o134, not 0o130; getting that wrong
    # silently resolves nothing at all.
    0o120: 'MPY', 0o124: 'JMP', 0o134: 'JPL',
}

for k in range(count):
    a = start + k
    v = w1(a)
    op = (v >> 11) << 2          # opcode in the 0o000..0o130 numbering above
    X = (v >> 10) & 1
    I = (v >> 9) & 1
    Bb = (v >> 8) & 1
    disp = v & 0o377
    if disp > 127:
        disp -= 256
    note = ''
    name = MEMREF.get(op)
    if name in ('JMP', 'JPL') and I and not X and not Bb:
        # P-relative indirect: the pointer word is at P+disp, the routine is its contents.
        ptr = (a + disp) & 0xFFFF
        tgt = w1(ptr)
        note = '   ; %s -> pointer @%06o -> ROUTINE %06o' % (name, ptr, tgt)
    elif name in ('JMP', 'JPL') and not I and not X and not Bb:
        note = '   ; %s direct -> %06o' % (name, (a + disp) & 0xFFFF)
    elif name and I and not X and not Bb and name not in ('JMP', 'JPL'):
        ptr = (a + disp) & 0xFFFF
        note = '   ; %s via pointer @%06o = %06o (operand in bank2)' % (name, ptr, w1(ptr))
    print('%06o  %06o%s' % (a, v, note))
