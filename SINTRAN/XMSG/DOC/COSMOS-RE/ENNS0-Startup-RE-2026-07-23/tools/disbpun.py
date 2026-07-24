#!/usr/bin/env python
# Disassemble a range of a BPUN-loaded ND-100 image, annotated with the
# XMSG symbol table (NAME=octal lines, high-bit ASCII).
import sys
from bpun_load import load_bpun
import nd100dis


def load_syms(path):
    d = open(path, 'rb').read()
    s = ''.join(chr(b & 0x7f) for b in d).replace('\r', '').replace('\x00', '')
    syms = {}
    for line in s.split('\n'):
        if '=' in line:
            name, _, val = line.partition('=')
            name = name.strip()
            val = val.strip()
            if val and all(c in '01234567' for c in val):
                try:
                    syms[name] = int(val, 8)
                except ValueError:
                    pass
    return syms


class Img:
    def __init__(self, mem):
        self.mem = mem


if __name__ == '__main__':
    bpun = sys.argv[1]
    symf = sys.argv[2]
    start = int(sys.argv[3], 8)
    count = int(sys.argv[4]) if len(sys.argv) > 4 else 80
    r = load_bpun(bpun)
    mem = {}
    for a in range(0, max(r['mem']) + 1):
        mem[a] = r['mem'].get(a, 0)
    syms = load_syms(symf)
    img = Img(mem)
    print(nd100dis.dis_range(img, start, count, syms))
