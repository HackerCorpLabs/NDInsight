#!/usr/bin/env python
"""
bpun_load.py - ND-100 BPUN (binary punch) absolute-load-format loader.

Format per ND-06.014.2A section 4.2.5.1 "Binary Format Load":

    A B C ! E F G H I

  A : arbitrary chars (comment / octal bootstrap text) NOT containing '!'
  B : (optional) octal number terminated with CR (LF ignored)
  C : (optional) octal number terminated with '!'  -> the program START address
  ! : ASCII 0x21 - marks start of the binary information
  E : block start (load) address        - 2 bytes, MSB first
  F : word count of this block           - 2 bytes, MSB first (E,F,H not counted)
  G : F data words                       - 2 bytes each, MSB first
  H : checksum = 16-bit arithmetic sum of all words in G, 2 bytes MSB first
  I : action code (1 byte). 0/blank = start at C ; nonzero = return to operator.

Multiple E-F-G-H blocks may be concatenated. A block with F==0 terminates.
High bit (0x80) is set on some ASCII chars in the header (paper-tape parity);
mask with 0x7f when reading the header text.

Returns a flat image dict {word_addr: value} plus load base / start address.

Read-only. Does not modify the input file.
"""
import sys


def load_bpun(path, verbose=False):
    d = open(path, 'rb').read()

    # ---- locate the '!' that starts binary information --------------------
    bang = d.find(0x21)
    if bang < 0:
        raise ValueError("no '!' (0x21) start-of-binary marker found")

    # ---- C: the octal number immediately before '!' = start address -------
    # walk backwards over octal digits (mask parity bit)
    j = bang - 1
    digits = []
    while j >= 0:
        c = d[j] & 0x7f
        if ord('0') <= c <= ord('7'):
            digits.append(chr(c))
            j -= 1
        else:
            break
    digits.reverse()
    start_addr = int(''.join(digits), 8) if digits else None

    # ---- parse the binary blocks after '!' --------------------------------
    p = bang + 1

    def rdword(i):
        return (d[i] << 8) | d[i + 1]

    mem = {}
    blocks = []
    load_base = None
    while p + 4 <= len(d):
        E = rdword(p)
        F = rdword(p + 2)
        if F == 0:
            # terminator block (E may carry action / start); stop
            blocks.append((E, 0, None, True))
            break
        gstart = p + 4
        gend = gstart + 2 * F
        if gend + 2 > len(d):
            if verbose:
                print("truncated block: E=%o F=%o needs to %d but len %d"
                      % (E, F, gend + 2, len(d)))
            break
        checksum = 0
        for w in range(F):
            val = rdword(gstart + 2 * w)
            mem[(E + w) & 0xffff] = val
            checksum = (checksum + val) & 0xffff
        H = rdword(gend)
        ok = (checksum == H)
        blocks.append((E, F, H, ok))
        if load_base is None:
            load_base = E
        if verbose:
            print("block: start=%06o(0x%04x) count=%o(%d) checksum file=%06o calc=%06o %s"
                  % (E, E, F, F, H, checksum, "OK" if ok else "BAD"))
        p = gend + 2  # advance past checksum; action byte handled implicitly

    return {
        'mem': mem,
        'load_base': load_base,
        'start_addr': start_addr,
        'blocks': blocks,
        'bang_offset': bang,
    }


if __name__ == '__main__':
    path = sys.argv[1]
    r = load_bpun(path, verbose=True)
    print("start_addr = %06o (0x%04x)" % (r['start_addr'], r['start_addr']))
    print("load_base  = %06o" % r['load_base'])
    print("num blocks = %d" % len(r['blocks']))
    addrs = sorted(r['mem'])
    print("mem span   = %06o .. %06o  (%d words)"
          % (addrs[0], addrs[-1], len(r['mem'])))
    bad = [b for b in r['blocks'] if not b[3]]
    print("bad checksums: %d" % len(bad))
