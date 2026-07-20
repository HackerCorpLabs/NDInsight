#!/usr/bin/env python3
# =============================================================================
#  check_applied.py
#
#  PILOT / EXPERIMENTAL.  Test whether the deposits of a .PATC file are present
#  in a carved system, using the "% OLD:" values as the fingerprint.
#
#  This does NOT need an as-shipped baseline image - that is the whole point.
#  60-70% of patch deposits record their pre-patch word, so for each deposit we
#  can ask: does the target word still hold the OLD value (=> NOT APPLIED)?
#
#  HONEST LIMITATIONS (read WORKFLOW.md "A3 pilot" before believing output):
#   * It can prove NOT-APPLIED.  It can only prove APPLIED when the patch's new
#     value happens to be a bare octal constant - most are assembly mnemonics,
#     and this script deliberately does NOT assemble them.  Everything else is
#     reported as UNRESOLVED-NEW, not silently counted as applied.
#   * Symbol values are GENERATION-specific.  Using a symbol list from a
#     different generation of the same version letter produces garbage.  A high
#     MISMATCH count is the signature of the wrong symbol list.
#   * Only simple "SYMBOL" / "SYMBOL+octal" address expressions are resolved.
#
#  Usage:
#    python3 check_applied.py \
#        --patc  PATCHES-10200.PATC \
#        --segments  .../versions/K-VSX-500/segments \
#        --symbols   .../SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT \
#        --symbols   .../SYMBOLS/K03/SYMBOL-2-LIST.SYMB.TXT
# =============================================================================

import os
import re
import sys
import json
import struct
import argparse
import subprocess
import collections

HERE = os.path.dirname(os.path.abspath(__file__))

RE_SYM = re.compile(r'^([A-Z0-9$#][A-Z0-9$#]{0,4})\s*=\s*([0-7]+)$')
RE_ADDR = re.compile(r'^([A-Z0-9$#]{1,5})(?:\+([0-7]+))?$')
RE_OCTAL = re.compile(r'^[0-7]+$')


def load_symbols(paths):
    """First definition wins (SYMBOL-1-LIST is the primary list)."""
    sym = {}
    for p in paths:
        for raw in open(p, 'rb'):
            line = ''.join(chr(b & 0x7F) for b in raw).strip()
            m = RE_SYM.match(line)
            if m:
                sym.setdefault(m.group(1), int(m.group(2), 8))
    return sym


def load_segments(segdir):
    segs = {}
    for fn in sorted(os.listdir(segdir)):
        if not fn.endswith('.meta.json'):
            continue
        j = json.load(open(os.path.join(segdir, fn), encoding='utf-8-sig'))
        try:
            base = int(j['load_address']['oct'], 8)
        except Exception:
            continue
        b = os.path.join(segdir, fn.replace('.meta.json', '.bin'))
        if os.path.exists(b):
            segs[j['name']] = (base, open(b, 'rb').read())
    return segs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--patc', required=True)
    ap.add_argument('--segments', required=True)
    ap.add_argument('--symbols', action='append', required=True)
    ap.add_argument('--json', action='store_true')
    args = ap.parse_args()

    sym = load_symbols(args.symbols)
    segs = load_segments(args.segments)

    deposits = json.loads(subprocess.check_output(
        [sys.executable, os.path.join(HERE, 'parse_patch.py'),
         '--deposits-only', args.patc]).decode())

    counts = collections.Counter()
    rows = []
    for d in deposits:
        counts['total'] += 1
        if d.get('kind') != 'open':
            continue
        counts['open'] += 1
        if not d.get('old_octal'):
            counts['no-OLD'] += 1
            continue
        counts['has-OLD'] += 1
        seg = (d.get('context') or {}).get('coreload')
        if seg not in segs:
            counts['unknown-segment'] += 1
            continue
        m = RE_ADDR.match((d.get('address_expr') or '').replace(' ', ''))
        if not m:
            counts['complex-address'] += 1
            continue
        if m.group(1) not in sym:
            counts['unknown-symbol'] += 1
            continue
        addr = sym[m.group(1)] + (int(m.group(2), 8) if m.group(2) else 0)
        base, data = segs[seg]
        off = (addr - base) * 2
        if off < 0 or off + 2 > len(data):
            counts['out-of-segment'] += 1
            continue

        word = struct.unpack('>H', data[off:off + 2])[0]
        old = int(d['old_octal'], 8)
        ne = (d.get('new_expr') or '').strip()
        new = int(ne, 8) if RE_OCTAL.match(ne) else None

        if word == old:
            verdict = 'NOT-APPLIED'
        elif new is not None and word == new:
            verdict = 'APPLIED'
        elif new is None:
            verdict = 'UNRESOLVED-NEW'      # cannot decide without assembling
        else:
            verdict = 'MISMATCH'            # neither old nor new -> suspect

        counts[verdict] += 1
        rows.append({
            'report': d.get('report'),
            'revision': d.get('revision'),
            'segment': seg,
            'address_expr': d.get('address_expr'),
            'address_oct': '%o' % addr,
            'current_oct': '%06o' % word,
            'old_oct': '%06o' % old,
            'new_expr': ne or None,
            'verdict': verdict,
        })

    if args.json:
        json.dump({'counts': dict(counts), 'rows': rows}, sys.stdout, indent=1)
        sys.stdout.write('\n')
        return

    print('patch file : %s' % args.patc)
    print('segments   : %s (%d loaded)' % (args.segments, len(segs)))
    print('symbols    : %d' % len(sym))
    print()
    for k in ('total', 'open', 'has-OLD', 'unknown-segment', 'complex-address',
              'unknown-symbol', 'out-of-segment'):
        print('  %-18s %6d' % (k, counts[k]))
    print()
    for k in ('APPLIED', 'NOT-APPLIED', 'UNRESOLVED-NEW', 'MISMATCH'):
        print('  %-18s %6d' % (k, counts[k]))
    print()
    print('NOTE: UNRESOLVED-NEW means "new value is assembly source, not a')
    print('      number - this script will not guess".  A large MISMATCH count')
    print('      usually means the SYMBOL list does not match this generation.')


if __name__ == '__main__':
    main()
