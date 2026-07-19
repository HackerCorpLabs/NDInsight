#!/usr/bin/env python3
"""SINTRAN III system-segment carver.

Carves the individual system segments out of a SINTRAN SMD disk image so each
can be loaded into Ghidra (or nd100-dis) at its correct load address for
reverse engineering.

WHY a segment table from memory is needed
-----------------------------------------
SINTRAN keeps every system segment inside one big contiguous file
(SYSTEM)SEGFIL0:DATA. Where each segment sits *inside* that file (its MADR page
offset) and how long it is (SEGLE) live only in the in-memory Segment Table -
not in any fixed disk header. So the tool needs the Segment Table, obtained one
of two ways (see README):
  1. AUTO  : boot the image in nd100x + dump bank 3 / offset 0o124000 via DAP.
  2. MANUAL: you supply that dump as inputs/segment-table-bank3.bin.

Disk / page model (validated against the extracted SEGFIL0 + raw SMD image):
  1 SINTRAN page = 1024 words = 2048 bytes, big-endian (native ND-100).
  MADR and SEGLE in the Segment Table are BOTH in 2048-byte pages.
  Inside (SYSTEM)SEGFIL0:DATA a segment occupies:
      offset = (CBLST + MADR) * 2048 bytes, length = SEGLE * 2048 bytes.
  Segments are packed contiguously: segment N+1 begins exactly SEGLE pages
  after segment N, so a SEGLE-page read never overshoots the neighbour.

  Feed --smd the SEGFIL0 file extracted with ndtool (-x, big-endian) and keep
  --cblst 0 (MADR is then relative to the file start). The old raw-SMD path
  (--cblst 1670, 1024-byte "sectors") is WRONG on two counts - half the page
  size AND a mis-derived base that lands ~60 KB off the NDFS file location -
  and is retired.

Output is BIG-ENDIAN (native ND-100) so it drops straight into a big-endian
ND-100 Ghidra processor. Load each .bin at manifest[].load_address.
"""
import argparse, json, os

SECTOR = 2048          # bytes per SINTRAN page = 1024 words

def symbol_file_for(name):
    """Pick the ND symbol table (basename) that best labels a segment."""
    n = (name or '').upper()
    if 'FS' in n or 'S3FS' in n:               return 'FILSYS-SYMBOLS'
    if n.startswith('S3XM') or 'XMSG' in n:    return 'XMSG-SYMBOL-LIST'
    if 'RTL' in n or n.startswith('S3RT'):     return 'RTLO-SYMBOLS'
    if '5' in n or 'N500' in n or 'NM5' in n:  return 'N500-SYMBOLS'
    return 'SYMBOL-1-LIST'   # resident/kernel/command default

def carve(disk, cblst, madr, segle):
    buf = bytearray()
    for p in range(segle):
        off = (cblst + madr + p) * SECTOR
        if off + SECTOR > len(disk): break
        buf += disk[off:off+SECTOR]     # already big-endian on disk = native
    return bytes(buf)

def main():
    ap = argparse.ArgumentParser(description='Carve SINTRAN system segments from an SMD image.')
    ap.add_argument('--smd', required=True, help='(SYSTEM)SEGFIL0:DATA extracted with ndtool -x (big-endian)')
    ap.add_argument('--facts', required=True, help='segment-facts.json from reconcile.py')
    ap.add_argument('--out', required=True, help='output dir for .bin + manifest.json')
    ap.add_argument('--cblst', type=int, default=0, help='page offset added to MADR (0 for an extracted SEGFIL0)')
    args = ap.parse_args()

    disk = open(args.smd, 'rb').read()
    facts = json.load(open(args.facts))['segments']
    os.makedirs(args.out, exist_ok=True)

    manifest = []
    carved = 0
    for f in facts:
        sn = f['segnum']
        segfil = (int(f['flag_oct'], 8) >> 13) & 7 if f.get('flag_oct') else 0
        rec = dict(f)
        rec['symbol_file'] = symbol_file_for(f['name'])
        madr, segle = f.get('madr'), f.get('segle')
        # carve real SEGFIL-0 code/data segments only.
        # NOTE: madr==0 means "no distinct SEGFIL0 disk image" (memory-resident,
        # e.g. S3IMAGE/S3MPIT/S3RPIT) - NOT offset 0. Carving those from offset 0
        # yields duplicate/bogus content, so they are skipped. The resident MON
        # dispatch code (ENT14/GOTAB) is captured under 116-S3SERWD.bin instead
        # (verified by GOTAB content signature - see 23-MON-CALL-DISPATCH guide).
        if segfil == 0 and segle and madr and segle <= 512:
            data = carve(disk, args.cblst, madr, segle)
            fn = f'{sn:03o}-{f["name"]}.bin'
            open(os.path.join(args.out, fn), 'wb').write(data)
            # load address: octal in the facts -> decimal + hex for Ghidra.
            # ND-100 is WORD-addressed; the Ghidra base is this word address.
            la_oct = f.get('load_address_oct')
            try:
                la = int(str(la_oct), 8)
            except (TypeError, ValueError):
                la = None
            la_hex = ('0x%04X' % la) if la is not None else None
            rec['file'] = fn
            rec['bytes'] = len(data)
            rec['size_words'] = len(data) // 2
            rec['nonzero'] = any(data)
            rec['byte_order'] = 'big-endian'
            rec['load_address_hex'] = la_hex          # Ghidra base (hex)
            rec['load_address_dec'] = la
            # one self-contained metadata sidecar per carved segment
            meta = {
                'segment_number_oct': '%03o' % sn,
                'segment_number_dec': sn,
                'name': f['name'],
                'description': f.get('description') or f.get('desc'),
                'content': {'nonzero': rec['nonzero'], 'byte_order': 'big-endian'},
                'size': {'pages': segle, 'bytes': len(data), 'words': len(data) // 2},
                'segfil0': {'madr_page': madr, 'flag_oct': f.get('flag_oct'), 'segfil': segfil},
                'load_address': {'oct': la_oct, 'dec': la, 'hex': la_hex},
                'symbol_file': rec['symbol_file'],
                'confidence': f.get('confidence'),
                'file': fn,
                'ghidra': {
                    'processor': 'ND-100 big-endian 16-bit (word-addressed)',
                    'format': 'Raw Binary',
                    'base_address_hex': la_hex,
                    'base_address_oct': la_oct,
                    'note': 'Load the .bin as-is (big-endian) - do NOT byte-swap for Ghidra. '
                            'ND-100 is word-addressed: set the base in the word address space. '
                            'Byte-swap to little-endian ONLY for nd100-dis.',
                },
            }
            json.dump(meta, open(os.path.join(args.out, f'{sn:03o}-{f["name"]}.meta.json'), 'w'), indent=1)
            carved += 1
        manifest.append(rec)

    json.dump({'cblst': args.cblst, 'endianness': 'big', 'segments': manifest},
              open(os.path.join(args.out, 'manifest.json'), 'w'), indent=1)
    print(f'carved {carved} segments -> {args.out}')
    for m in manifest:
        if 'file' in m:
            la = m.get('load_address_oct') or '?'
            print(f"  {m['segnum']:>3o} {m['name']:<9} load={la:>7}B len={m['segle']:>3}p "
                  f"{m['bytes']:>7}B {'ZERO' if not m['nonzero'] else ''} "
                  f"conf={m['confidence']:<6} sym={m['symbol_file']}")

if __name__ == '__main__':
    main()
