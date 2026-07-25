#!/usr/bin/env python3
"""Compare the two copies of MACM, and characterise SINTRAN-L-1:DATA.

WHAT THIS IS
    The tool for MACM-DIALOGUE.md's open question 7: "The floppy build.
    Everything here is the standalone D:\\ND\\BPUN\\MACM-1718L.BPUN. The
    L-floppy copy is 465 words larger and was NOT examined; the MSTYP
    tables may have moved or grown."

WHY IT MATTERS
    Every Ghidra finding in MACM-DIALOGUE.md and MSTYP-SWTYP-BRIDGE.md was
    made on the STANDALONE MACM binary. ND shipped a DIFFERENT build on the
    distribution floppy. If the shipped build moved its tables, those
    findings describe something ND never distributed. This script measures
    exactly how the two differ, so that risk can be judged instead of
    assumed.

WHAT IT DOES
    1. Parses each BPUN tape into the PROGRAM IT ACTUALLY LOADS - skips the
       NUL leader, finds the '!' start marker, reads the 16-bit big-endian
       base address and word count, then the words and the trailing
       checksum. Reports base, length, span and whether the checksum
       verifies, so a difference can be told apart from a corrupt tape.
    2. If both load at the same base with the same length, word-diffs them
       and prints the first differing words. If they do not, says so and
       stops - it does NOT attempt to align two differently-based images.
    3. Characterises SINTRAN-L-1:DATA: size in bytes/words/1K-pages, first
       32 bytes, whether a '!' start marker appears early (i.e. whether it
       is a BPUN-style tape or something else), leading NUL count, and the
       readable strings in the first 200 KB after parity-stripping.

MEASURED ON L07 (2026-07-25)
    floppy     : base 076203  19738 words  (076203-144634)  checksum OK
    standalone : base 077120  19273 words  (077120-144630)  checksum OK

    BOTH CHECKSUMS VERIFY, so both tapes are intact and the difference is
    real, not damage. They are different programs: the floppy build is 465
    words larger, and the arithmetic says where those words are -
        base  461 words LOWER, top 4 words HIGHER  (461 + 4 = 465).
    The floppy build is therefore extended almost entirely DOWNWARD, and
    the two images end at nearly the same address. Content near the top of
    the image sits at close to the same address in both builds; content
    near the bottom is displaced by ~461 words. That is the constraint on
    whether an address-specific finding from the standalone binary carries
    over. This script does not resolve which side the MSTYP tables fall on.

    SINTRAN-L-1:DATA is 1095538 bytes / 547769 words, ~51% printable after
    parity-stripping - i.e. a TEXT command stream with binary records
    embedded, not a raw image. Its plain-text header is parsed by
    extract_layout_params.py.

INPUT
    Both paths are hard-coded below and are WSL paths; there is no argument
    handling. Extract the floppy copy WITHOUT -p:
        ndtool -x -o D:\\ND\\extract\\VSXL1 D:\\ND\\S\\VSXL1.IMG
    (-p strips bit 7 and corrupts binaries, MACM-1718L:BPUN included.)

USAGE
    python3 analyze_vsxl1.py        # read-only; prints, writes nothing
"""
import sys


def parse_bpun(path):
    """Return (base, words) of the binary block, skipping leader+bootstrap."""
    d = open(path, 'rb').read()
    i = 0
    while i < len(d) and d[i] == 0:
        i += 1
    while i < len(d) and d[i] != ord('!'):
        i += 1
    i += 1
    if i + 4 > len(d):
        return None, [], 0
    base = (d[i] << 8) | d[i + 1]
    cnt = (d[i + 2] << 8) | d[i + 3]
    i += 4
    w = []
    for _ in range(cnt):
        if i + 1 >= len(d):
            break
        w.append((d[i] << 8) | d[i + 1])
        i += 2
    chk = (d[i] << 8) | d[i + 1] if i + 1 < len(d) else None
    return base, w, chk


A = '/mnt/d/ND/extract/VSXL1/MACM-1718L.BPUN'
B = '/mnt/d/ND/BPUN/MACM-1718L.BPUN'

print("=== MACM: floppy copy vs standalone copy, as LOADED PROGRAMS ===")
for tag, p in (("floppy    ", A), ("standalone", B)):
    base, w, chk = parse_bpun(p)
    s = sum(w) & 0xFFFF
    print(f"  {tag}: base {base:06o}  {len(w)} words  "
          f"({base:06o}-{base + len(w) - 1:06o})  "
          f"checksum {chk:06o} {'OK' if s == chk else 'MISMATCH'}")

ba, wa, _ = parse_bpun(A)
bb, wb, _ = parse_bpun(B)
if ba == bb and len(wa) == len(wb):
    diff = [i for i, (x, y) in enumerate(zip(wa, wb)) if x != y]
    print(f"  same load address and length; {len(diff)} differing words")
    for i in diff[:12]:
        print(f"    {ba + i:06o}: floppy {wa[i]:06o}  standalone {wb[i]:06o}")
    if not diff:
        print("  -> the loaded programs are IDENTICAL "
              "(only the tape wrapper differs)")
else:
    print(f"  DIFFERENT programs: base {ba:06o}/{bb:06o}  "
          f"len {len(wa)}/{len(wb)}")

print()
print("=== SINTRAN-L-1:DATA ===")
d = open('/mnt/d/ND/extract/VSXL1/SINTRAN-L-1.DATA', 'rb').read()
print(f"  {len(d)} bytes = {len(d)/1024:.0f} KB = {len(d)//2} words "
      f"= {len(d)/2048:.1f} pages of 1K words")
print(f"  first 32 bytes: {' '.join(f'{b:02X}' for b in d[:32])}")

# is it a BPUN-style tape, or a raw image?
head = d[:600]
print(f"  contains '!' start marker in first 600 bytes: "
      f"{b'!' in head}")
nul = 0
while nul < len(d) and d[nul] == 0:
    nul += 1
print(f"  leading NUL bytes: {nul}")

# printable runs -> is there readable text (symbol names, banners)?
runs, cur = [], b''
for b in d[:200000]:
    c = b & 0x7F
    if 32 <= c < 127:
        cur += bytes([c])
    else:
        if len(cur) >= 6:
            runs.append(cur.decode('ascii', 'replace'))
        cur = b''
print(f"  readable strings in first 200 KB: {len(runs)}")
for s in runs[:25]:
    print(f"    {s}")
