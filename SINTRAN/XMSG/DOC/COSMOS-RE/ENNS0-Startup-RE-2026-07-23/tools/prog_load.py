#!/usr/bin/env python3
"""SINTRAN :PROG loader (host-side).

Turns a one-bank / two-bank :PROG file into a flat ND-100 word image + load info.
Header format VERIFIED from prog-fileformat.md (7 BE words at offset 0, bank1 at 0x200).

Usage:
    from prog_load import load_prog
    img, info = load_prog(path)      # img = dict addr->word (bank1), info = header dict

Returns bank1 image as a list indexed by memory word address (0..b1_last), padded 0.
"""
import sys

def rd_words_be(data, byteoff, nwords):
    out = []
    for i in range(nwords):
        o = byteoff + 2*i
        out.append((data[o] << 8) | data[o+1])
    return out

def load_prog(path):
    data = open(path, 'rb').read()
    hdr = rd_words_be(data, 0, 7)
    info = dict(start=hdr[0], restart=hdr[1],
                b1_first=hdr[2], b1_last=hdr[3],
                b2_first=hdr[4], b2_last=hdr[5], dbc_last=hdr[6],
                filelen=len(data))
    b1_words = (hdr[3] - hdr[2] + 1) & 0xFFFF
    if hdr[3] < hdr[2]:
        b1_words = hdr[3] - hdr[2] + 1
    # bank1 image starts at file offset 0x200
    base = hdr[2]
    mem = [0] * (base + b1_words)
    avail = (len(data) - 0x200) // 2
    n = min(b1_words, avail)
    w = rd_words_be(data, 0x200, n)
    for i in range(n):
        mem[base + i] = w[i]
    info['b1_words'] = b1_words
    return mem, info

class Img:
    """Adapter so nd100dis.dis_range(img,...) works (needs img.mem[addr])."""
    def __init__(self, mem):
        self.mem = mem

if __name__ == '__main__':
    mem, info = load_prog(sys.argv[1])
    print("header:", {k: (oct(v) if isinstance(v,int) else v) for k,v in info.items()})
    print("mem words:", len(mem))
