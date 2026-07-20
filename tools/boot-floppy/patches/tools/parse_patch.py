#!/usr/bin/env python3
# =============================================================================
#  parse_patch.py
#
#  Parse a Norsk Data SINTRAN III PATCH file (PATCHES:PATC / PATCH-FILE:PATC)
#  into structured JSON records.
#
#  The .PATC file is a plain 7-bit ASCII command stream fed to SINTRAN's
#  MAC-family assembler/patcher (FMAC on an image file for H/J, DMAC on
#  coreloads/segment save files for K).  See ../README.md for the format
#  write-up and the evidence behind it.
#
#  This parser is DELIBERATELY CONSERVATIVE.  It does NOT try to be a MACM
#  assembler.  It extracts:
#     * the patch *records* (delimited by "% ==> REPORT:" headers)
#     * the header metadata (system, report number, revision letter,
#       PROGRAM, REASON, free-text SUBJECT/SYMPTOM/DESCRIPTION)
#     * the *target context* in force (image file, coreload/segment,
#       conditional-generation guard)
#     * every "deposit" (open-location) operation, with its symbolic address
#       expression, the literal text deposited, and the "% OLD: nnnnnn"
#       octal value when the patch file records one.
#
#  Anything it cannot interpret is preserved verbatim in the record under
#  "unparsed_lines" and counted in the summary, so the output never pretends
#  to be more complete than it is.
#
#  Usage:
#     python3 parse_patch.py PATCHES.PATC            > patches.json
#     python3 parse_patch.py --summary PATCHES.PATC
#     python3 parse_patch.py --report 315 PATCHES.PATC
# =============================================================================

import sys
import re
import json
import argparse

# --- lexical helpers ---------------------------------------------------------

# ND text files carry odd parity in bit 7.  ndtool -p already strips it, but we
# strip again defensively so the parser works on raw carvings too.
def load_text(path):
    raw = open(path, 'rb').read()
    txt = ''.join(chr(b & 0x7F) for b in raw)
    return txt.replace('\r\n', '\n').replace('\r', '\n')


# "% ==> REPORT: SIN-K       451 C  PROGRAM: ALL              REASON: E"
# The revision letter and the PROGRAM/REASON fields are optional; the K-series
# files dropped PROGRAM/REASON entirely.
RE_REPORT = re.compile(
    r'^%\s*==>\s*REPORT:\s*(?P<system>SIN-[A-Z0-9]+)\s+'
    r'(?P<number>\d+)\s*'
    r'(?P<rev>[A-Z])?\b'
    r'(?:.*?PROGRAM:\s*(?P<program>\S+))?'
    r'(?:.*?REASON:\s*(?P<reason>\S+))?',
    re.IGNORECASE)

# A continuation comment line inside a header block: "% ==>   text"
RE_CONT = re.compile(r'^%\s*==>\s?(?P<text>.*)$')

# Pure banner line: "% ====================================="
RE_BANNER = re.compile(r'^%\s*=+\s*$')

# MACM/DMAC directive, e.g. ")CLOAD S3SSM5", ")FILL", ")KILL A B"
RE_DIRECTIVE = re.compile(r'^\)(?P<cmd>\w+)\s*(?P<args>.*)$')

# SINTRAN command line, e.g. "@DMAC", "@CONTINUE", "@FMAC"
RE_ATCMD = re.compile(r'^@(?P<cmd>\S*)\s*(?P<args>.*)$')

# Conditional-generation guard.  MACM uses a leading '"' followed by a sum of
# generation flags (8xxxx).  A bare '"' closes the block.
RE_GUARD_OPEN = re.compile(r'^"\s*(?P<expr>[-+0-9A-Z*/ ]*\S)\s*$')
RE_GUARD_CLOSE = re.compile(r'^"\s*$')

# Deposit / open-location line.  MACM syntax is:
#     <address-expression>/ <value expression>          % OLD: nnnnnn
# where the address expression may be a symbol, a symbol+offset, '*' (current
# location) or '*+n'.  A bare "*/ value" continues at the next word.
# A trailing ':' form ("SYMB:") is a symbol *reference/undefine*, not a deposit.
RE_DEPOSIT = re.compile(
    r'^(?P<addr>[A-Z0-9$*][A-Z0-9$*+\-. ]*?)\s*/'
    r'(?P<value>[^%]*?)\s*'
    r'(?:%\s*(?P<comment>.*))?$')

RE_OLD = re.compile(r'OLD:\s*(?P<old>[0-7]+)', re.IGNORECASE)

# Symbol assignment: "SG12S=26000", "XX=4000; YY=200"
RE_ASSIGN = re.compile(r'^\s*(?P<sym>[A-Z0-9$][A-Z0-9$]*)\s*=\s*(?P<val>[^;%]+)')


class Context(object):
    """Running assembler context: what the following deposits act upon."""

    def __init__(self):
        self.program = None       # DMAC / FMAC / CONTINUE
        self.image_file = None    # e.g. SINTRAN:DATA, MACM-AREA:DATA
        self.coreload = None      # )CLOAD argument: octal number or segment name
        self.guards = []          # stack of "8xxx" generation-flag expressions

    def snapshot(self):
        return {
            'program': self.program,
            'image_file': self.image_file,
            'coreload': self.coreload,
            'guard': '+'.join(self.guards) if self.guards else None,
        }


def parse(path):
    lines = load_text(path).split('\n')

    ctx = Context()
    records = []
    current = None
    expect_image_file = False   # next non-empty line answers the IMAGE-FILE prompt
    stats = {'lines': len(lines), 'deposits': 0, 'sequential': 0, 'unparsed': 0}

    # MACM/DMAC "open location" state.  After "SYMB/ value" the assembler stays
    # positioned and every following source line deposits into the NEXT word,
    # until a ')' directive or a new open-location line.  See README.md
    # "Sequential deposition" - inferred from the source layout, consistent
    # across all seven patch files.
    open_addr = None
    open_seq = 0

    def new_record(m, lineno):
        return {
            'system': m.group('system').upper(),
            'report': int(m.group('number')),
            'revision': m.group('rev'),
            'program': m.group('program'),
            'reason': m.group('reason'),
            'text': [],
            'line': lineno,
            'context': ctx.snapshot(),
            'directives': [],
            'symbols': [],
            'deposits': [],
            'unparsed_lines': [],
        }

    for lineno, raw in enumerate(lines, 1):
        line = raw.rstrip()
        stripped = line.strip()

        # ---- SINTRAN command level ----------------------------------------
        m = RE_ATCMD.match(stripped)
        if m and not stripped.startswith('@@'):
            open_addr = None
            cmd = m.group('cmd').upper()
            if cmd in ('DMAC', 'FMAC', 'MAC'):
                ctx.program = cmd
                expect_image_file = (cmd in ('FMAC', 'MAC'))
            elif cmd == 'CONTINUE':
                # @CONTINUE resumes the previously exited MAC/FMAC, which then
                # re-prompts for the image file on the following line.
                expect_image_file = True
            if current is not None:
                current['directives'].append({'line': lineno, 'text': stripped})
            continue

        if not stripped:
            continue

        # The line right after @FMAC / @CONTINUE answers "IMAGE-FILE :".
        if expect_image_file:
            if not stripped.startswith(('%', ')', '"', '@')):
                ctx.image_file = stripped
                expect_image_file = False
                continue
            # A comment/banner may intervene; keep waiting.
            if stripped.startswith('%'):
                pass
            else:
                expect_image_file = False

        # ---- comment / header block ---------------------------------------
        if stripped.startswith('%'):
            m = RE_REPORT.match(stripped)
            if m:
                current = new_record(m, lineno)
                records.append(current)
                open_addr = None
                continue
            if RE_BANNER.match(stripped):
                continue
            m = RE_CONT.match(stripped)
            if m and current is not None:
                t = m.group('text').rstrip()
                if t:
                    current['text'].append(t)
            continue

        # ---- conditional-generation guards --------------------------------
        if RE_GUARD_CLOSE.match(stripped):
            open_addr = None
            if ctx.guards:
                ctx.guards.pop()
            continue
        m = RE_GUARD_OPEN.match(stripped)
        if m:
            open_addr = None
            ctx.guards.append(m.group('expr'))
            if current is not None:
                current['context']['guard'] = '+'.join(ctx.guards)
            continue

        # ---- MACM/DMAC directives -----------------------------------------
        m = RE_DIRECTIVE.match(stripped)
        if m:
            cmd = m.group('cmd').upper()
            args = m.group('args').strip()
            if cmd == 'CLOAD':
                ctx.coreload = args.split('%')[0].strip()
                if current is not None:
                    current['context']['coreload'] = ctx.coreload
            if current is not None:
                current['directives'].append(
                    {'line': lineno, 'cmd': cmd, 'args': args})
            continue

        # ---- deposits ------------------------------------------------------
        if '/' in stripped:
            m = RE_DEPOSIT.match(stripped)
            if m:
                addr = m.group('addr').strip()
                value = (m.group('value') or '').strip()
                comment = (m.group('comment') or '').strip()
                old = None
                mo = RE_OLD.search(comment)
                if mo:
                    old = mo.group('old')
                dep = {
                    'line': lineno,
                    'address_expr': addr,
                    'new_expr': value if value else None,
                    'old_octal': old,
                    'comment': comment or None,
                    'context': ctx.snapshot(),
                    'raw': stripped,
                }
                dep['kind'] = 'open'
                open_addr = addr
                open_seq = 0
                stats['deposits'] += 1
                if current is not None:
                    current['deposits'].append(dep)
                else:
                    records.append({'system': None, 'report': None,
                                    'preamble': True, 'deposits': [dep],
                                    'text': [], 'directives': [],
                                    'symbols': [], 'unparsed_lines': []})
                continue

        # ---- symbol assignments -------------------------------------------
        m = RE_ASSIGN.match(stripped)
        if m and '=' in stripped and not stripped.startswith(('"', ')')):
            if current is not None:
                current['symbols'].append(
                    {'line': lineno, 'text': stripped})
            continue

        # ---- sequential deposition into an open location -------------------
        if open_addr is not None and current is not None:
            open_seq += 1
            label = None
            body = stripped
            ml = re.match(r'^([A-Z0-9$][A-Z0-9$]*)\s*,\s*(.*)$', stripped)
            if ml:
                label, body = ml.group(1), ml.group(2)
            comment = None
            if '%' in body:
                body, comment = body.split('%', 1)
                body, comment = body.strip(), comment.strip()
            current['deposits'].append({
                'line': lineno,
                'kind': 'sequential',
                'address_expr': '%s+%d' % (open_addr, open_seq),
                'base_expr': open_addr,
                'word_offset': open_seq,
                'label': label,
                'new_expr': body.strip() or None,
                'old_octal': None,
                'comment': comment,
                'context': ctx.snapshot(),
                'raw': stripped,
            })
            stats['sequential'] += 1
            continue

        # ---- everything else: )KILL operand lists, stray text --------------
        stats['unparsed'] += 1
        if current is not None:
            current['unparsed_lines'].append({'line': lineno, 'text': stripped})

    return records, stats


def main():
    ap = argparse.ArgumentParser(description='Parse a SINTRAN III PATCH file.')
    ap.add_argument('file')
    ap.add_argument('--summary', action='store_true',
                    help='print a human-readable summary instead of JSON')
    ap.add_argument('--report', type=int, default=None,
                    help='emit only this report number')
    ap.add_argument('--deposits-only', action='store_true',
                    help='emit a flat list of deposit records')
    args = ap.parse_args()

    records, stats = parse(args.file)
    if args.report is not None:
        records = [r for r in records if r.get('report') == args.report]

    if args.summary:
        print('file            : %s' % args.file)
        print('lines           : %d' % stats['lines'])
        print('patch records   : %d' % len([r for r in records
                                            if r.get('report') is not None]))
        print('open-location deposits : %d' % stats['deposits'])
        print('sequential deposits    : %d' % stats['sequential'])
        print('unparsed lines  : %d  ()KILL operand lists, stray text;'
              ' see "unparsed_lines" in the JSON)' % stats['unparsed'])
        withold = 0
        targets = {}
        for r in records:
            for d in r.get('deposits', []):
                if d['old_octal']:
                    withold += 1
                t = d['context'].get('coreload') or \
                    d['context'].get('image_file') or '?'
                targets[t] = targets.get(t, 0) + 1
        print('deposits with recorded OLD value : %d' % withold)
        print('targets (coreload / image file):')
        for t, n in sorted(targets.items(), key=lambda kv: -kv[1]):
            print('   %-16s %5d' % (t, n))
        return

    out = records
    if args.deposits_only:
        out = []
        for r in records:
            for d in r.get('deposits', []):
                e = dict(d)
                e['system'] = r.get('system')
                e['report'] = r.get('report')
                e['revision'] = r.get('revision')
                out.append(e)
    json.dump(out, sys.stdout, indent=1)
    sys.stdout.write('\n')


if __name__ == '__main__':
    main()
