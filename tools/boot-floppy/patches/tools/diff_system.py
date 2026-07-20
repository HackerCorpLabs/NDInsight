#!/usr/bin/env python3
# =============================================================================
#  diff_system.py
#
#  Diff two SINTRAN III system images / carved segments and emit candidate
#  patch records in the SAME JSON shape that parse_patch.py produces, so the
#  two halves of the workflow can be cross-referenced.
#
#  This is the "identify what was patched" half:  take an as-shipped
#  distribution segment and the same segment carved from a site machine,
#  word-diff them, and every differing word becomes a candidate patch deposit.
#
#  ND-100 images are 16-bit WORD addressed and stored BIG-ENDIAN in the
#  carver's .bin files (see segments/*.meta.json -> content.byte_order).
#  Word N of a segment lives at load_address + N in the ND-100's address space.
#
#  Optional symbol resolution: point --symbols at a SINTRAN symbol list
#  (NPL-SOURCE/SYMBOLS/<ver>/SYMBOL-1-LIST.SYMB.TXT, lines "NAME=oooooo")
#  and each differing address is annotated with the nearest preceding symbol,
#  giving output directly comparable to the "SYMB+nn/" address expressions
#  in a real .PATC file.
#
#  Usage:
#     # single segment pair
#     python3 diff_system.py --base 4000 old/053-S3SDPIT.bin new/053-S3SDPIT.bin
#
#     # whole carved-segment directories (uses each *.meta.json for the base)
#     python3 diff_system.py --dirs old/segments new/segments
#
#     # with symbols
#     python3 diff_system.py --base 4000 --symbols SYMBOL-1-LIST.SYMB.TXT a.bin b.bin
# =============================================================================

import os
import re
import sys
import json
import struct
import argparse


# ---------------------------------------------------------------------------
# symbol table
# ---------------------------------------------------------------------------

RE_SYM = re.compile(r'^\s*([A-Z0-9$#][A-Z0-9$#]{0,4})\s*=\s*([0-7]+)\s*$')


def load_symbols(path):
    """Load 'NAME=oooooo' lines into a list sorted by address."""
    syms = []
    with open(path, 'rb') as fh:
        for raw in fh:
            line = ''.join(chr(b & 0x7F) for b in raw).rstrip('\r\n')
            m = RE_SYM.match(line)
            if m:
                syms.append((int(m.group(2), 8), m.group(1).strip()))
    syms.sort()
    return syms


def nearest_symbol(syms, addr, window=0o4000):
    """Return 'SYMB+nn' for the nearest preceding symbol, or None."""
    if not syms:
        return None
    lo, hi = 0, len(syms)
    while lo < hi:
        mid = (lo + hi) // 2
        if syms[mid][0] <= addr:
            lo = mid + 1
        else:
            hi = mid
    if lo == 0:
        return None
    base, name = syms[lo - 1]
    delta = addr - base
    if delta > window:
        return None
    return name if delta == 0 else '%s+%o' % (name, delta)


# ---------------------------------------------------------------------------
# image loading
# ---------------------------------------------------------------------------

def load_words(path, endian='>'):
    data = open(path, 'rb').read()
    if len(data) % 2:
        data += b'\x00'
    return struct.unpack('%s%dH' % (endian, len(data) // 2), data)


def meta_base(binpath):
    """Read load_address.oct from the carver's sibling .meta.json, if present."""
    meta = binpath[:-4] + '.meta.json' if binpath.endswith('.bin') else None
    if meta and os.path.exists(meta):
        try:
            j = json.load(open(meta, 'r', encoding='utf-8-sig'))
            return int(j['load_address']['oct'], 8), j.get('name')
        except Exception:
            return None, None
    return None, None


# ---------------------------------------------------------------------------
# diffing
# ---------------------------------------------------------------------------

def diff_words(a, b, base, segment, syms, group_gap=1):
    """Word-diff two images.  Returns (records, notes)."""
    notes = []
    n = min(len(a), len(b))
    if len(a) != len(b):
        notes.append('SIZE MISMATCH: %d vs %d words; compared first %d'
                     % (len(a), len(b), n))

    idx = [i for i in range(n) if a[i] != b[i]]

    # group consecutive (or near-consecutive) differing words into runs,
    # mirroring the "open location then sequential words" shape of a real patch
    runs = []
    for i in idx:
        if runs and i - runs[-1][-1] <= group_gap:
            runs[-1].append(i)
        else:
            runs.append([i])

    records = []
    for run in runs:
        start = run[0]
        addr = base + start
        rec = {
            'kind': 'binary-diff',
            'segment': segment,
            'base_address_oct': '%o' % base,
            'word_index': start,
            'address_oct': '%o' % addr,
            'address_expr': nearest_symbol(syms, addr) or ('%o' % addr),
            'length_words': run[-1] - run[0] + 1,
            'words': [],
        }
        for i in range(run[0], run[-1] + 1):
            rec['words'].append({
                'address_oct': '%o' % (base + i),
                'address_expr': nearest_symbol(syms, base + i)
                                or ('%o' % (base + i)),
                'old_octal': '%06o' % a[i],
                'new_octal': '%06o' % b[i],
                'changed': a[i] != b[i],
            })
        records.append(rec)
    return records, notes


# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(
        description='Diff two SINTRAN images/segments into candidate patch '
                    'records.')
    ap.add_argument('old', nargs='?', help='as-shipped / reference image')
    ap.add_argument('new', nargs='?', help='installed / patched image')
    ap.add_argument('--dirs', nargs=2, metavar=('OLDDIR', 'NEWDIR'),
                    help='compare two carved-segment directories by filename')
    ap.add_argument('--base', default=None,
                    help='octal load address (default: from .meta.json, else 0)')
    ap.add_argument('--segment', default=None, help='segment name label')
    ap.add_argument('--symbols', default=None, help='SYMBOL-n-LIST file')
    ap.add_argument('--endian', default='big', choices=['big', 'little'])
    ap.add_argument('--gap', type=int, default=1,
                    help='max gap in words to keep words in one run (default 1)')
    ap.add_argument('--summary', action='store_true')
    args = ap.parse_args()

    endian = '>' if args.endian == 'big' else '<'
    syms = load_symbols(args.symbols) if args.symbols else []

    pairs = []
    if args.dirs:
        olddir, newdir = args.dirs
        oldfiles = {f for f in os.listdir(olddir) if f.endswith('.bin')}
        newfiles = {f for f in os.listdir(newdir) if f.endswith('.bin')}
        for f in sorted(oldfiles & newfiles):
            pairs.append((os.path.join(olddir, f), os.path.join(newdir, f)))
        only_old = sorted(oldfiles - newfiles)
        only_new = sorted(newfiles - oldfiles)
    else:
        if not (args.old and args.new):
            ap.error('give two files, or --dirs OLDDIR NEWDIR')
        pairs.append((args.old, args.new))
        only_old = only_new = []

    all_records = []
    all_notes = []
    for oldp, newp in pairs:
        base, name = meta_base(newp)
        if args.base is not None:
            base = int(args.base, 8)
        if base is None:
            base = 0
            all_notes.append('%s: no load address known, using 0'
                             % os.path.basename(newp))
        seg = args.segment or name or os.path.basename(newp)
        a = load_words(oldp, endian)
        b = load_words(newp, endian)
        recs, notes = diff_words(a, b, base, seg, syms, args.gap)
        all_records.extend(recs)
        all_notes.extend('%s: %s' % (seg, n) for n in notes)

    if args.summary:
        print('pairs compared : %d' % len(pairs))
        if only_old:
            print('only in OLD    : %s' % ', '.join(only_old))
        if only_new:
            print('only in NEW    : %s' % ', '.join(only_new))
        bysec = {}
        for r in all_records:
            bysec.setdefault(r['segment'], [0, 0])
            bysec[r['segment']][0] += 1
            bysec[r['segment']][1] += r['length_words']
        print('%-16s %8s %8s' % ('segment', 'runs', 'words'))
        for s in sorted(bysec, key=lambda k: -bysec[k][1]):
            print('%-16s %8d %8d' % (s, bysec[s][0], bysec[s][1]))
        print('TOTAL runs=%d words=%d'
              % (sum(v[0] for v in bysec.values()),
                 sum(v[1] for v in bysec.values())))
        for n in all_notes:
            print('NOTE: %s' % n)
        return

    json.dump({'notes': all_notes,
               'only_in_old': only_old,
               'only_in_new': only_new,
               'records': all_records},
              sys.stdout, indent=1)
    sys.stdout.write('\n')


if __name__ == '__main__':
    main()
