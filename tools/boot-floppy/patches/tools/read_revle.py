#!/usr/bin/env python3
# =============================================================================
#  read_revle.py
#
#  Read the SINTRAN III patch/revision level (REVLE) out of a carved system.
#
#  WHY THIS WORKS  (byte-verified, see ../README.md section "REVLE"):
#    * The patch floppies ship a one-line file SYMBOLS:FADM containing
#          33CPV=<octal patch level>
#      and PATCH-FILE:OUT states verbatim
#          "SYMBOL '33CPV' BELOW DEFINES REVISION LEVEL!"
#    * START-PATCH-FILE:MODE on the K floppies ends with
#          )CLOAD S3SDPIT
#          REVLE/ 33CPV
#      i.e. it stores that constant into the word named REVLE.
#    * REVLE = octal 4057 in the K03, L07 and M06 SYMBOL-1-LIST files
#      (identical across all three), and the DPIT segments load at octal 4000,
#      so REVLE is word 57 (octal) of S3SDPIT / S3IDPIT.
#
#  Result on the carved systems in this repo:
#      K-VSX-500  REVLE = 010200  -> patch level K-10200  (we HAVE that floppy)
#      L-VSX-500  REVLE = 000000  -> no patch level recorded
#      M-VSX-500  REVLE = 003200  -> patch level M-3200
#
#  Usage:
#     python3 read_revle.py <carved-version-dir-or-segments-dir> [...]
#     python3 read_revle.py --segment S3SDPIT.bin --base 4000
# =============================================================================

import os
import sys
import json
import struct
import argparse

REVLE_ADDR = 0o4057          # constant in K03 / L07 / M06 SYMBOL-1-LIST
SYSNO_ADDR = 0o4051
DPIT_NAMES = ('S3SDPIT', 'S3IDPIT')


def read_word(path, base, addr):
    d = open(path, 'rb').read()
    off = (addr - base) * 2
    if off < 0 or off + 2 > len(d):
        return None
    return struct.unpack('>H', d[off:off + 2])[0]


def scan_dir(segdir):
    out = []
    for fn in sorted(os.listdir(segdir)):
        if not fn.endswith('.bin'):
            continue
        mp = os.path.join(segdir, fn[:-4] + '.meta.json')
        name, base = None, None
        if os.path.exists(mp):
            try:
                j = json.load(open(mp, encoding='utf-8-sig'))
                name = j.get('name')
                base = int(j['load_address']['oct'], 8)
            except Exception:
                pass
        if name not in DPIT_NAMES:
            continue
        p = os.path.join(segdir, fn)
        out.append({
            'segment': name,
            'file': p,
            'base_oct': '%o' % base,
            'revle_addr_oct': '%o' % REVLE_ADDR,
            'revle_oct': '%06o' % (read_word(p, base, REVLE_ADDR) or 0),
            'sysno_oct': '%06o' % (read_word(p, base, SYSNO_ADDR) or 0),
        })
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('paths', nargs='*')
    ap.add_argument('--segment', help='a single S3?DPIT .bin file')
    ap.add_argument('--base', default='4000', help='octal load address')
    ap.add_argument('--json', action='store_true')
    args = ap.parse_args()

    results = []
    if args.segment:
        base = int(args.base, 8)
        results.append({
            'segment': os.path.basename(args.segment),
            'file': args.segment,
            'base_oct': '%o' % base,
            'revle_addr_oct': '%o' % REVLE_ADDR,
            'revle_oct': '%06o' % (read_word(args.segment, base, REVLE_ADDR) or 0),
            'sysno_oct': '%06o' % (read_word(args.segment, base, SYSNO_ADDR) or 0),
        })
    for p in args.paths:
        segdir = p if os.path.basename(p) == 'segments' \
            else os.path.join(p, 'segments')
        if not os.path.isdir(segdir):
            segdir = p
        if not os.path.isdir(segdir):
            print('not a directory: %s' % p, file=sys.stderr)
            continue
        for r in scan_dir(segdir):
            r['version_dir'] = p
            results.append(r)

    if args.json:
        json.dump(results, sys.stdout, indent=1)
        sys.stdout.write('\n')
        return

    print('%-24s %-9s %-8s %-8s' % ('system', 'segment', 'REVLE', 'SYSNO'))
    for r in results:
        print('%-24s %-9s %-8s %-8s'
              % (os.path.basename(r.get('version_dir') or r['file']),
                 r['segment'], r['revle_oct'], r['sysno_oct']))
    if not results:
        print('(no S3SDPIT / S3IDPIT segment found)')


if __name__ == '__main__':
    main()
