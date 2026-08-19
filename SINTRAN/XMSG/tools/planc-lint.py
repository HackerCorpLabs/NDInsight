"""Check a PLANC source for the mistakes the ND compiler has actually caught here.

WHY THIS EXISTS. Every rule below cost a real build cycle on D100 - carry the file
across, compile, fetch the listing, read it - and several cost more than one because
the compiler's own message points somewhere else. A round trip is minutes; this is
instant, and it catches them before the file leaves Windows.

It is a LINTER, not a compiler. It reads text and knows nothing about scope or types,
so it will miss things and it can be wrong. Its job is the mistakes that repeat.

    python tools/planc-lint.py SINTRAN-CHAT/CHAT.PLNC SINTRAN-CHAT/CHATSV.PLNC

Exit code 1 if anything is reported.
"""

import io
import os
import re
import sys



def strip_comment(line):
    """Drop a trailing % comment, leaving quoted text alone.

    Needed because a bare RETURN with a comment after it - "RETURN  % not news" -
    otherwise looks like RETURN with an out-value, which is a different mistake.
    """
    out = []
    in_quote = False
    for ch in line:
        if ch == "'":
            in_quote = not in_quote
        if ch == '%' and not in_quote:
            break
        out.append(ch)
    return ''.join(out)


def check(path):
    """Report every problem found in one source. Returns a list of strings."""
    text = io.open(path, encoding='utf-8', errors='replace').read()
    lines = text.split('\n')
    out = []

    def code_lines():
        """Every line with its number, comments and blank lines skipped."""
        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped and not stripped.startswith('%'):
                yield i + 1, line, stripped

    # ---- the operator that does not exist -------------------------------------
    # "=<" gets ILLEGAL SYNTAX, but the compiler blames the line ABOVE first with
    # ILLEGAL DATA TYPE "AND", so the operator is the last thing anyone looks at.
    for n, line, _ in code_lines():
        if '=<' in line:
            out.append('%s:%d  "=<" is not PLANC - the operators are <= and >= '
                       '(ND-60.117.5 section 5.4)' % (path, n))

    # ---- array indexing is ROUND ----------------------------------------------
    # The COSMOS guide prints ADDR(buf[0]) and PLANC F rejects it:
    # EXPECTS ")" ILLEGAL SYNTAX "[", then INVALID PARAMETER LIST for the call.
    for n, line, _ in code_lines():
        if re.search(r'\w\s*\[', line):
            out.append('%s:%d  square brackets index nothing in PLANC - use round ones'
                       % (path, n))

    # ---- the out-value goes BEFORE the keyword --------------------------------
    # "RETURN at" gets ILLEGAL DATA TYPE "RETURN", which reads like RETURN itself
    # is the problem.
    for n, line, stripped in code_lines():
        if re.match(r'RETURN\s+\S', strip_comment(stripped).strip()):
            out.append('%s:%d  the out-value goes BEFORE RETURN - write "x RETURN"'
                       % (path, n))

    # ---- WHILE is a continue-test, not a loop header --------------------------
    for n, line, stripped in code_lines():
        if re.search(r'\bENDWHILE\b', stripped):
            out.append('%s:%d  there is no ENDWHILE - WHILE is a continue-test inside '
                       'DO ... ENDDO' % (path, n))
        if re.search(r'\bWHILE\b.*\bDO\s*$', stripped):
            out.append('%s:%d  "WHILE cond DO" is not a loop header - put WHILE inside '
                       'the DO body' % (path, n))

    # ---- an OUTPUT field is the FIELD, not a maximum ---------------------------
    # Too narrow and it prints the first n characters then a row of asterisks.
    for n, line, _ in code_lines():
        for m in re.finditer(r"OUTPUT\s*\(\s*\d+\s*,\s*'A[LR]?(\d+)'\s*,\s*'((?:[^']|'')*)'", line):
            width = int(m.group(1))
            literal = m.group(2).replace("''", "'")
            if len(literal) > width:
                out.append("%s:%d  'AL%d' is narrower than its %d-character string - "
                           'PLANC prints asterisks' % (path, n, width, len(literal)))

    # ---- a CASE selects on 0..255 ---------------------------------------------
    # Section 6.3: the expression must be an enumeration of at most 256 values or
    # "integers ranging between 0 and 255". A plain INTEGER draws OUT OF RANGE -
    # a WARNING, so the build "succeeds" and the CASE misbehaves at run time.
    ranged = set()
    for _, line, _ in code_lines():
        m = re.search(r'INTEGER\s+RANGE\s*\([^)]*\)\s*:\s*([\w,\s]+)', line, re.I)
        if m:
            for name in m.group(1).split(','):
                ranged.add(name.strip().upper())
    for n, _, stripped in code_lines():
        m = re.match(r'CASE\s+(\w+)\s*$', stripped, re.I)
        if m and m.group(1).upper() not in ranged:
            out.append('%s:%d  CASE selects on 0..255 - declare "%s" as '
                       'INTEGER RANGE (0:255) or it draws OUT OF RANGE'
                       % (path, n, m.group(1)))

    # ---- PLANC is single pass --------------------------------------------------
    # A routine called above its declaration gets ILLEGAL SYNTAX on the CALL,
    # naming a routine that plainly exists - which reads as nonsense.
    declared = {}
    for i, line in enumerate(lines):
        m = re.match(r'\s*(?:ROUTINE|PROGRAM)\b.*?:\s*(\w+)', line)
        if m:
            declared.setdefault(m.group(1).upper(), i + 1)
    for n, line, stripped in code_lines():
        if re.match(r'\s*(?:ROUTINE|PROGRAM)\b', line):
            continue
        for name, at in declared.items():
            if n < at and re.search(r'\b' + re.escape(name) + r'\b', stripped, re.I):
                out.append('%s:%d  uses %s but it is declared at line %d - PLANC is '
                           'single pass' % (path, n, name, at))

    # ---- do not redeclare a name the library already defines -------------------
    # Measured: CHATSV declared its own XFWTF, which XMP-B02:DEFS also defines. The
    # compiler said IDENTIFIER ALREADY SPECIFIED/DECLARED and then FELL OVER with
    # "ASSERT VIOLATION AT 136747B", taking the batch job with it. So this is not a
    # warning you can carry on past - it is a compiler crash waiting to happen.
    defs_path = os.path.join(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(path)))),
        'Installation', 'Software', 'ND-10609', 'files', 'XMP-B02-DEFS.readable.txt')
    if os.path.exists(defs_path):
        library = set()
        dt = io.open(defs_path, encoding='utf-8', errors='replace').read()
        for m in re.finditer(r'CONSTANT\s+(\w+)', dt, re.I):
            library.add(m.group(1).upper())
        if any(s.upper().startswith('$INCLUDE XMP-B02:DEFS') for _, _, s in code_lines()):
            for n, _, stripped in code_lines():
                m = re.match(r'(?:INTEGER4|INTEGER|BYTES|BOOLEAN)'
                             r'(?:\s+RANGE\s*\([^)]*\))?\s*:\s*([A-Za-z_]\w*)', stripped)
                if m and m.group(1).upper() in library:
                    out.append('%s:%d  "%s" is already defined by XMP-B02:DEFS - redeclaring '
                               'it crashes the compiler (ASSERT VIOLATION)'
                               % (path, n, m.group(1)))

    # ---- names are unique in their first TEN characters ------------------------
    # Measured on CHATSV: memberName and memberNameLen both start MEMBERNAME, so
    # the second became a redeclaration of the first - IDENTIFIER ALREADY
    # SPECIFIED/DECLARED - and every memberName(slot, i) then drew MORE SUBSCRIPTS
    # THAN IN THE ARRAY DECLARATION, because the name now meant the 1-D array.
    # Four errors from one cause, and not one of them mentions the length. The
    # listing is the tell: it prints the TRUNCATED names (BUILDRENAM, REASONNICK).
    declared_names = set()
    for _, _, stripped in code_lines():
        m = re.match(r'(?:INTEGER4|INTEGER|BYTES|BOOLEAN)(?:\s+ARRAY)?'
                     r'(?:\s+RANGE\s*\([^)]*\))?\s*:\s*([A-Za-z_]\w*)', stripped)
        if m:
            declared_names.add(m.group(1))
        m = re.match(r'(?:ROUTINE|PROGRAM).*?:\s*(\w+)', stripped)
        if m:
            declared_names.add(m.group(1))
    by_prefix = {}
    for name in sorted(declared_names):
        by_prefix.setdefault(name[:10].upper(), []).append(name)
    for prefix, names in sorted(by_prefix.items()):
        if len(names) > 1:
            out.append('%s  %s all start "%s" - PLANC names are unique in their FIRST TEN '
                       'characters, so these are one identifier'
                       % (path, ' and '.join(names), prefix))

    # ---- an OUTPUT field pads AFTER the newline --------------------------------
    # 'ALnn' wider than its string pads to nn AFTER the $ has emitted the CRLF, so
    # the spaces land at the START of the next line. A /help built from padded
    # fields came out as a staircase. Exact-width fields have no padding at all.
    for n, line, _ in code_lines():
        for m in re.finditer(r"OUTPUT\s*\(\s*\d+\s*,\s*'A[LR]?(\d+)'\s*,\s*'((?:[^']|'')*)'", line):
            width = int(m.group(1))
            literal = m.group(2).replace("''", "'")
            if len(literal) < width:
                out.append("%s:%d  'AL%d' is WIDER than its %d-character string - the padding "
                           'lands after the newline and indents the NEXT line'
                           % (path, n, width, len(literal)))

    # ---- $EOF belongs in an INCLUDE, never in the main source ------------------
    body = [s for _, _, s in code_lines()]
    if any(s.upper().startswith('$EOF') for s in body):
        if any(s.upper().startswith('MODULE') for s in body):
            out.append('%s  $EOF in a MAIN source ends the compiler SESSION - the '
                       'commands after COMPILE then go to SINTRAN' % path)

    # ---- the includes must be INSIDE the module --------------------------------
    module_at = None
    for n, _, stripped in code_lines():
        if re.match(r'MODULE\b', stripped, re.I):
            module_at = n
            break
    if module_at is not None:
        for n, _, stripped in code_lines():
            if stripped.upper().startswith('$INCLUDE') and n < module_at:
                out.append('%s:%d  $INCLUDE above MODULE lands in the outer scope - the '
                           'body cannot see it and the include draws no diagnostic'
                           % (path, n))

    # ---- a STRING literal cannot be stored into a BYTES element ----------------
    # "' ' =: outBuf(at)" reads perfectly and does not compile. 'x' is a STRING;
    # an element of a BYTES array is a BYTE. The compiler answers
    #
    #   1082   (422)/BUILDWHO  *** ERROR   - ILLEGAL DATA TYPE "OUTBUF"
    #
    # naming the ARRAY rather than the literal, which sends you looking at the
    # declaration - where nothing is wrong. Hold the character in a one-element
    # BYTES and copy element to element instead.
    for n, _, stripped in code_lines():
        m = re.search(r"'(?:[^']|'')*'\s*=:\s*([A-Za-z_]\w*)\s*\(", stripped)
        if m:
            out.append("%s:%d  a STRING literal cannot be stored into %s(...) - an "
                       "element of a BYTES array is a BYTE. Put the character in a "
                       "one-element BYTES and copy element to element. The compiler "
                       'blames the ARRAY ("ILLEGAL DATA TYPE"), not the literal'
                       % (path, n, m.group(1)))

    # ---- 'ALn' must be the length of the literal it formats --------------------
    # 'ALn' is a FIELD WIDTH, not a hint: too small and the tail of the line is
    # cut off, too large and OUTPUT runs past the end of the literal. The count
    # includes the trailing '$' when there is one, because that is a character in
    # the string like any other.
    #
    # Nothing in the compiler checks this - a wrong width builds clean and shows
    # up only as a truncated line on a terminal nobody may be watching. Five were
    # introduced in one sitting adding /who and the room name to CHAT.PLNC.
    for m in re.finditer(r"'AL(\d+)'\s*,\s*'((?:[^']|'')*)'", text):
        want = int(m.group(1))
        # '' is how a PLANC literal carries one quote, so it counts as one byte.
        got = len(m.group(2).replace("''", "'"))
        if want != got:
            n = text[:m.start()].count('\n') + 1
            out.append("%s:%d  'AL%d' but the literal is %d characters - the width "
                       "includes the trailing '$'" % (path, n, want, got))

    return out


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2

    total = 0
    for path in argv[1:]:
        problems = check(path)
        total += len(problems)
        if problems:
            for p in problems:
                print(p)
        else:
            print('%s: clean' % path)

    if total:
        print('\n%d problem(s). Every one of these has cost a build cycle on D100.' % total)
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
