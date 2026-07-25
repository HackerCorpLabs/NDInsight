#!/usr/bin/env python3
"""Extract the distribution layout-parameter block from SINTRAN-L-1:DATA.

WHAT THIS IS
    The generator of both files in
        tools/boot-floppy/versions/L-VSX-500-07/inputs/
    Those files are committed; this script is the only thing that produces
    them. If they are ever doubted, re-run this and diff.

WHY IT MATTERS
    SINTRAN-L-1:DATA is the MACM generation stream shipped on ND's own
    distribution floppy. It opens with a plain-text header that MACM reads:
    "NAME=octal" lines giving each SINTRAN system area's SEGFIL page number,
    plus a "%%" comment legend mapping patch-macro names to area names.

    That header is a THIRD, INDEPENDENT WITNESS to the segment page map -
    it is neither the live LIST-SEGMENT dump nor the OCR'd release-manual
    section 8.3. It is what allowed 28 of 32 layout parameters to be
    confirmed against carved "madr" values, and 30 segments whose confidence
    was only "medium" because of OCR damage to be promoted to "high".
    See ../versions/L-VSX-500-07/carve-crosscheck.md for the findings.

INPUT
    A SINTRAN*-1:DATA stream extracted from the distribution floppy. Extract
    it WITHOUT -p:
        ndtool -x -o D:\\ND\\extract\\VSXL1 D:\\ND\\S\\VSXL1.IMG
    (-p strips bit 7 and corrupts the binary half of the file. This script
    strips ND parity itself, in memory, which is safe.)

USAGE
    python3 extract_layout_params.py [SOURCE-STREAM] [OUTPUT-DIR]
    Defaults: /mnt/d/ND/extract/VSXL1/SINTRAN-L-1.DATA  and  the CWD.
    To regenerate the committed copies, pass
        ../versions/L-VSX-500-07/inputs  as OUTPUT-DIR.

OUTPUT
    distribution-layout-params.txt    the verbatim header text
    distribution-layout-params.json   parsed parameters + legend

HOW IT WORKS, AND THE ONE NON-OBVIOUS RULE
    The header runs from byte 0 to the first control byte (7457 bytes for
    L07); everything after that is binary and is ignored. Parameters are
    parsed as "NAME=expr" where expr is OCTAL. In an expression like
    "300-2", the leading additive terms are the PAGE ADDRESS and a trailing
    "-N" is a length/in-page offset that is NOT part of the address -
    reading it as arithmetic gives the wrong page. SEGFIL page numbers
    relate to the carver's "madr" as: madr = page - 0o200.

MEASURED ON L07 (2026-07-25, verified against the committed copies)
    header 7457 bytes; 34 layout parameters; 21 legend entries. Output is
    byte-identical to the committed files apart from line endings (those
    are CRLF in the repository, this writes LF).
"""
import json
import re
import sys

SRC = sys.argv[1] if len(sys.argv) > 1 \
    else '/mnt/d/ND/extract/VSXL1/SINTRAN-L-1.DATA'
OUTDIR = sys.argv[2] if len(sys.argv) > 2 else '.'

raw = open(SRC, 'rb').read()
txt = bytes(b & 0x7F for b in raw).decode('latin-1')   # strip ND parity

# The header runs from the start to the first control/binary byte; the agent
# located the first control byte at offset 7897.
end = 0
while end < len(txt) and (32 <= ord(txt[end]) < 127 or txt[end] in '\r\n\t'):
    end += 1
header = txt[:end]

with open(f'{OUTDIR}/distribution-layout-params.txt', 'w',
          newline='\n') as f:
    f.write(header.replace('\r\n', '\n').replace('\r', '\n'))

# --- parse "NAME=expr" layout parameters -------------------------------
params = {}
for m in re.finditer(r'(?m)^\s*([A-Z][A-Z0-9]{1,5})\s*=\s*([0-9+\-]+)\s*$',
                     header):
    name, expr = m.group(1), m.group(2)
    # leading additive terms are the page address; a trailing "-N" is a
    # length / in-page offset and is NOT part of the address
    terms = re.findall(r'([+-])?(\d+)', expr)
    addr, first = 0, True
    for sign, digits in terms:
        if sign == '-' and not first:
            break                      # stop at the trailing subtraction
        v = int(digits, 8)
        addr = v if first else addr + v
        first = False
    params[name] = {
        'raw': expr,
        'page_oct': f'{addr:o}',
        'page_dec': addr,
        'madr_oct': f'{addr - 0o200:o}' if addr >= 0o200 else None,
        'madr_dec': addr - 0o200 if addr >= 0o200 else None,
    }

# --- parse the macro-name -> area-name legend from the %% comments -----
legend = {}
for m in re.finditer(r'%%\s*-\s*([A-Z0-9]{3,6})\s{2,}(.+?)\s*$',
                     header, re.M):
    legend[m.group(1)] = m.group(2).strip()

out = {
    'source_image': 'D:/ND/S/VSXL1.IMG',
    'source_volume': '250305L07-XX-01D',
    'source_file': '(SYSTEM)SINTRAN-L-1:DATA',
    'note': ('Page numbers are SEGFIL page addresses, the same quantity as '
             '"madr" in segment-facts.json. The image base is 0o200: '
             'madr = page - 0o200. A trailing "-N" in the raw expression is '
             'a length/offset, not part of the address.'),
    'header_bytes': end,
    'layout_params': params,
    'macro_legend': legend,
}
with open(f'{OUTDIR}/distribution-layout-params.json', 'w',
          newline='\n') as f:
    json.dump(out, f, indent=2)
    f.write('\n')

print(f'header: {end} bytes')
print(f'layout parameters: {len(params)}')
print(f'legend entries: {len(legend)}')
for k, v in list(params.items())[:8]:
    print(f"  {k:<6} {v['raw']:<12} page {v['page_oct']:>6}  "
          f"madr {v['madr_oct']}")
