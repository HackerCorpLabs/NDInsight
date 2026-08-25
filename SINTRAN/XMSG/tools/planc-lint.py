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
import json
import os
import re
import sys


# ---------------------------------------------------------------------------
# THE XMSG API, READ FROM DATA RATHER THAN HARD-CODED HERE.
#
# Developer/Languages/Application/xmp-api.json was transcribed from ND's OWN
# shipped declarations - XMP-B02:IMPT and XMP-B02:DEFS off the ND-10609 media -
# not from the guide, whose parameter tables have several typesetting slips.
# 54 routines and 397 constants. Keeping it as data means the same file serves
# the linter, a future code generator and anybody reading it, and it can be
# corrected in one place when the machine teaches us something.
#
# The linter still works without it: every XMSG rule below is skipped if the
# file is missing, because a linter that dies on a missing side-file is worse
# than one that checks less.
# ---------------------------------------------------------------------------
def load_xmp_api():
    here = os.path.dirname(os.path.abspath(__file__))
    for rel in (os.path.join('..', '..', '..', 'Developer', 'Languages', 'Application',
                             'xmp-api.json'),
                os.path.join('..', 'Developer', 'Languages', 'Application', 'xmp-api.json')):
        p = os.path.normpath(os.path.join(here, rel))
        if os.path.exists(p):
            try:
                with io.open(p, encoding='utf-8', errors='replace') as fh:
                    return json.load(fh)
            except (ValueError, IOError):
                return None
    return None


XMP_API = load_xmp_api()

# The three calls that RECEIVE. Everything in section 1 and 2 of the rules is about these.
RECEIVE_ROUTINES = ('xmpfrcv', 'xmpfrre', 'xmpfrrh')


def mask_strings(line):
    """Blank the inside of every 'quoted' run, keeping the line's length.

    Needed before looking for routine CALLS. A diagnostic string is very likely
    to name the routine it is about - showStatus('xmprout (the join)', st) - and
    a regex looking for "xmprout(" then matches the TEXT and reports a call with
    the wrong number of parameters, on a line that has no call at all. That was
    the first false alarm this rule produced.
    """
    out, in_quote = [], False
    for ch in line:
        if ch == "'":
            in_quote = not in_quote
            out.append(ch)
        else:
            out.append(' ' if in_quote else ch)
    return ''.join(out)


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


def include_names(source_path, inc_name):
    """Every name a local $INCLUDE'd file introduces, upper-cased.

    "$INCLUDE screen" brings in the whole PLANC-SCREEN-H interface - frame,
    bytdis, intacc and the rest. Without this, a screen program reports every
    one of them as undeclared: a page of false alarms that trains you to ignore
    the tool. The compiler's default file type is SYMB (appendix A section 0.8),
    so "screen" means SCREEN:SYMB; an explicit ":TYPE" is honoured too.

    ONLY files sitting NEXT TO the source are read. An include that lives on the
    ND and not here cannot be resolved, and then this returns nothing - the tool
    goes quiet rather than guessing at names it cannot see.
    """
    found = set()
    base = inc_name.split(':')[0].strip().strip("'\"")
    suffix = inc_name.split(':')[1].strip() if ':' in inc_name else None
    exts = [suffix] if suffix else ['SYMB', 'INCL', 'PLNC']
    here = os.path.dirname(os.path.abspath(source_path))
    for ext in exts:
        for cand in (base + '.' + ext, base.upper() + '.' + ext.upper(),
                     base.lower() + '.' + ext.lower()):
            full = os.path.join(here, cand)
            if not os.path.exists(full):
                continue
            try:
                raw = io.open(full, encoding='utf-8', errors='replace').read()
            except IOError:
                return found
            # Strip parity the way the rest of this repo does, so a file copied
            # straight off an ND floppy reads correctly.
            raw = ''.join(chr(ord(c) & 0x7F) for c in raw)
            for ln in raw.splitlines():
                ls = ln.strip()
                if ls.startswith('%'):
                    continue
                for mm in re.finditer(r':\s*([A-Za-z][A-Za-z0-9_]*)', ls):
                    found.add(mm.group(1).upper())
            return found
    return found


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

    # ---- assignment written the wrong way round -------------------------------
    # PLANC stores with "expression =: variable". Writing "variable := expression"
    # is the habit of every other language and it compiles as far as the ":=" then
    # answers ILLEGAL SYNTAX ":=" - preceded by a WARNING that the expression does
    # not store a value, which reads like a separate complaint about the line.
    #
    # ":=" IS legal in a DECLARATION as an initialiser, so only lines that are not
    # declarations are flagged. MEASURED: two of these in CHATSV's putNumber cost a
    # push-and-compile round on D100, 2026-08-20, and the linter passed the file.
    declaration = re.compile(
        r'^\s*(INTEGER|BYTE|BYTES|BOOLEAN|REAL|LABEL|POINTER|VOID|ENUMERATION)\b',
        re.IGNORECASE)
    for n, line, stripped in code_lines():
        if ':=' not in line:
            continue
        if declaration.match(line):
            continue
        # A store into a subscripted name, "x(i) := v", is wrong the same way.
        if re.search(r'^\s*[A-Za-z][A-Za-z0-9_]*\s*(\([^)]*\))?\s*:=', line):
            out.append('%s:%d  ":=" assigns the wrong way round - PLANC stores with '
                       '"expression =: variable". ":=" is only an initialiser in a '
                       'declaration.' % (path, n))

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
        # BLANK THE QUOTED TEXT FIRST. A routine called UPPER made this rule
        # report the word "Upper" inside the help screen's own sentence as a
        # forward call - a false alarm that sends you rearranging perfectly
        # correct declarations. The call-checking rules below already mask
        # strings; this one did not, and any routine whose name is an ordinary
        # English word walks straight into it.
        scan = mask_strings(stripped)
        for name, at in declared.items():
            if n < at and re.search(r'\b' + re.escape(name) + r'\b', scan, re.I):
                out.append('%s:%d  uses %s but it is declared at line %d - PLANC is '
                           'single pass' % (path, n, name, at))

    # ---- a guard that sets a flag NOBODY READS is a silencer, not a guard -------
    #
    # MEASURED 2026-08-21. The chat client's idleSleep did exactly this:
    #
    #     ON ROUTINEERROR DO
    #         TRUE =: sleepFailed
    #     ENDON
    #     FALSE =: sleepFailed
    #     MONITOR_CALL('TimeOut', sleepUnits, sleepKind, sleepWhy)
    #     ENDROUTINE          <- and that was the end of it
    #
    # A refused TimeOut therefore returned instantly and the main loop went round
    # again with NO SLEEP AT ALL - a spin against XMSG, invisible on the screen,
    # in the log and in the kernel tables alike.
    #
    # This is the same shape that hid GetUserName being refused for a whole build
    # cycle: guarded, refused, silent. The guard makes the program SURVIVE, which
    # is right, and then hides that anything happened, which is not. A swallowed
    # failure is a place the machine can be hurt with nothing saying so.
    #
    # The flag must be TESTED somewhere. What you do about it is your business -
    # print it, degrade, give up - but it may not simply vanish.
    handler_flags = {}
    in_handler = False
    for n, line, stripped in code_lines():
        s = mask_strings(stripped)
        if re.search(r'\bON\s+ROUTINEERROR\b', s, re.I):
            in_handler = True
            continue
        if re.search(r'\bENDON\b', s, re.I):
            in_handler = False
            continue
        if in_handler:
            m = re.search(r'\bTRUE\s*=:\s*(\w+)', s, re.I)
            if m:
                handler_flags.setdefault(m.group(1).upper(), n)

    for flag, at in handler_flags.items():
        tested = False
        for n, line, stripped in code_lines():
            s = mask_strings(stripped)
            # A test is any IF/WHILE that mentions it - NOT the assignments that
            # set or clear it, which is why the =: forms are excluded.
            if re.search(r'\b(IF|WHILE)\b.*\b' + re.escape(flag) + r'\b', s, re.I):
                tested = True
                break
        if not tested:
            out.append('%s:%d  %s is set by an ON ROUTINEERROR handler and NEVER TESTED - '
                       'the failure is swallowed in silence. A guard that sets a flag nobody '
                       'reads is a silencer, not a guard.' % (path, at, flag))

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
        m = re.match(r'(?:ROUTINE|PROGRAM)\b.*?:\s*(\w+)', stripped)
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

    # ---- a name that is STORED TO but never DECLARED ---------------------------
    # PLANC accepted "TRUE =: openFailed" with openFailed declared nowhere in the
    # file, and the compile reported 0 DIAGNOSTICS. The program then ran with the
    # flag permanently reading TRUE, which sent a config-file save down the wrong
    # branch on every single call and cost most of a day.
    #
    # This scans for names on the RECEIVING side of "=:" and checks each one is
    # declared somewhere in the file. It is deliberately loose - any declaration
    # anywhere counts, and subscripted targets are ignored - because the point is
    # to catch a name that exists NOWHERE, not to model PLANC's scoping.
    # NOTE the RANGE clause: "INTEGER RANGE (0:255) : xmxOffset" is a declaration
    # and an earlier version of this rule missed it, reporting a perfectly good
    # variable as undeclared. A lint rule that cries wolf is worse than none.
    declared = set()
    for m in re.finditer(r'^\s*(?:INTEGER4|INTEGER|BOOLEAN|BYTES|BYTE|REAL|LABEL|POINTER)'
                         r'(?:\s+ARRAY)*(?:\s+RANGE\s*\([^)]*\))?\s*:\s*([^%\n]+)', text, re.M):
        for name in re.findall(r'[A-Za-z_]\w*', m.group(1)):
            declared.add(name.upper())
    # routine names, parameters and IMPORTed routines are declarations too
    for m in re.finditer(r'^\s*(?:ROUTINE|PROGRAM|MODULE)\b[^:\n]*:\s*(\w+)\s*(\(([^)]*)\))?',
                         text, re.M):
        declared.add(m.group(1).upper())
        if m.group(3):
            for name in re.findall(r'[A-Za-z_]\w*', m.group(3)):
                declared.add(name.upper())
    for m in re.finditer(r'IMPORT\s*\(([^)]*)\)', text):
        for name in re.findall(r'[A-Za-z_]\w*', m.group(1)):
            declared.add(name.upper())
    # A $INCLUDE'd interface file declares names too - without this every
    # parameterless call into an included library (blankscreen, resetscreen)
    # is reported as a typo.
    for m in re.finditer(r'^\s*\$INCLUDE\s+(\S+)', text, re.M | re.I):
        declared |= include_names(path, m.group(1))

    seen_undeclared = set()
    for n, _, stripped in code_lines():
        for m in re.finditer(r'=:\s*([A-Za-z_]\w*)\s*(?![\w(])', stripped):
            name = m.group(1)
            if name.upper() in declared or name.upper() in seen_undeclared:
                continue
            seen_undeclared.add(name.upper())
            out.append('%s:%d  "%s" is stored to but never DECLARED anywhere in this file - '
                       'PLANC compiles that with 0 DIAGNOSTICS and the program then runs on a '
                       'value nothing sets' % (path, n, name))

    # ---- a bare name on a line of its own must BE a routine --------------------
    # THIS IS THE ONE THAT SHIPPED A BROKEN CLIENT FOR DAYS. CHAT.PLNC called
    #
    #     setMyName(at, argLen)
    #     showMyName            <- no such routine, anywhere in the file
    #     sendJoin
    #
    # and PLANC answered, on EVERY build,
    #
    #     2494  (1834)/HANDLECOMM  *** ERROR   - ILLEGAL SYNTAX "SHOWMYNAME"
    #
    # Nobody saw it. The compiler prints diagnostics as it goes and on a source
    # this long they scroll off a 24-line screen; the "0 DIAGNOSTICS" left on
    # screen belongs to the LINKER and sits happily under a compile that failed.
    # The listing is the only place a PLANC error survives, and CHAT:LIST had
    # never once been read. Builds went to the machine broken for days.
    #
    # A call with no parameters is just the name, so it is indistinguishable from
    # a typo by eye - which is exactly why it wants a machine to check it.
    #
    # A line ending in "&" continues onto the next, so the next line is an
    # ARGUMENT and not a statement: skipped, or every continued call is a
    # false alarm.
    STATEMENT_WORDS = {
        'RETURN', 'ENDIF', 'ENDDO', 'ENDFOR', 'ENDON', 'ENDROUTINE', 'ENDMODULE',
        'ENDPROGRAM', 'ENDRECORD', 'ENDCASE', 'ENDMACRO', 'ELSE', 'DO', 'THEN',
        'ENDVAL', 'ENDTYPE', 'GO', 'EXIT',
    }
    prev_continued = False
    for n, _, stripped in code_lines():
        if not stripped.strip():
            continue
        m = re.match(r'^\s*([A-Za-z_]\w*)\s*$', stripped)
        if m and not prev_continued:
            name = m.group(1)
            if name.upper() not in STATEMENT_WORDS and name.upper() not in declared:
                out.append('%s:%d  "%s" is CALLED on a line of its own but is declared '
                           'NOWHERE in this file - PLANC answers *** ERROR ILLEGAL SYNTAX '
                           'and that error only ever reaches the LISTING, which is why a '
                           'build like this can go to the machine looking fine'
                           % (path, n, name))
        prev_continued = stripped.rstrip().endswith('&')

    # =======================================================================
    # XMSG / XMP RULES - see Developer/Languages/Application/PLANC-XMSG-API-RULES.md
    # Skipped entirely when the API data file is not present.
    # =======================================================================
    if XMP_API:
        routines = XMP_API.get('routines', {})

        # ---- wrong ARGUMENT COUNT for a documented XMP routine -------------
        # PLANC will catch this, but only in the listing, and only after a
        # four-minute compile. Here it costs nothing. Continuation lines end
        # with "&", so a call is joined up before its arguments are counted.
        joined = []
        buf, start = '', 0
        for n, _, stripped in code_lines():
            s = mask_strings(stripped).rstrip()
            if not buf:
                start = n
            buf += ' ' + s[:-1] if s.endswith('&') else ' ' + s
            if not s.endswith('&'):
                joined.append((start, buf))
                buf = ''
        if buf:
            joined.append((start, buf))

        for n, text_line in joined:
            for m in re.finditer(r'\b(xmp\w+)\s*\(', text_line, re.I):
                name = m.group(1).lower()
                spec = routines.get(name)
                if not spec or not isinstance(spec.get('params'), list):
                    continue
                # Count arguments by scanning to the matching close paren, so a
                # nested call or a subarray "a(0:n-1)" does not split the count.
                i = m.end()
                depth, args, seen = 1, 1, False
                while i < len(text_line) and depth > 0:
                    ch = text_line[i]
                    if ch == '(':
                        depth += 1
                    elif ch == ')':
                        depth -= 1
                    elif ch == ',' and depth == 1:
                        args += 1
                    if depth == 1 and ch not in ' \t':
                        seen = True
                    i += 1
                if depth != 0:
                    continue                      # unbalanced - do not guess
                if not seen:
                    args = 0
                want = len(spec['params'])
                if args != want:
                    names = ', '.join(p.get('name', '?') for p in spec['params'])
                    out.append('%s:%d  %s takes %d parameter(s) but %d given - (%s)'
                               % (path, n, name, want, args, names))

        # WHICH VARIABLES CARRY A WAIT OR A WAKE-UP.
        #
        # THE REAL BUG PASSED A VARIABLE, WHICH IS WHY A LITERAL-ONLY RULE MISSED
        # IT. CHATSV said "2**XFWAK =: waitFlags" once, near the top of the
        # PROGRAM, and the receive four hundred lines later read
        # "xmpfrcv(waitFlags, myPort, ...)". Nothing on the call line mentions a
        # flag at all. A checker that only looks at the argument text sees
        # nothing wrong with the line that actually parked the server.
        tainted = set()
        for _, text_line in joined:
            for m in re.finditer(r'([^=]*?)=:\s*([A-Za-z_]\w*)\s*$', text_line):
                if re.search(r'\bXFWTF\b|\bXFWAK\b', m.group(1), re.I):
                    tainted.add(m.group(2).upper())
        for m in re.finditer(r'INTEGER\s*:\s*(\w+)\s*:=\s*([^%\n]*)', text, re.I):
            if re.search(r'\bXFWTF\b|\bXFWAK\b', m.group(2), re.I):
                tainted.add(m.group(1).upper())

        def flags_wait_or_wake(arg):
            """Does this flags argument ask to wait, or arm a wake-up?"""
            if re.search(r'\bXFWTF\b|\bXFWAK\b', arg, re.I):
                return True
            bare = arg.strip().upper()
            return bare in tainted

        # ---- a receive whose flags argument is a BIT POSITION ---------------
        # XFWTF is 15 and XFWAK is 14 - POSITIONS, not values. Passed raw they
        # are nonsense flags. The value is 2**XFWAK.
        for n, text_line in joined:
            m = re.search(r'\b(%s)\s*\(\s*(XF\w+)\s*,' % '|'.join(RECEIVE_ROUTINES),
                          text_line, re.I)
            if m:
                out.append('%s:%d  %s flags argument is "%s", which is a BIT POSITION, not a '
                           'value - write 2**%s, or 0 for no options'
                           % (path, n, m.group(1).lower(), m.group(2), m.group(2)))

        # ---- TWO PORTS AND A WAIT OR A WAKE-UP ------------------------------
        # THE ONE THAT PARKED THE SERVER FOR HALF A SESSION. XFRCV receives on
        # ONE port; there is no receive-on-any. Armed on two, the task sits on
        # one doorbell while a message waits on the other - measured on D100
        # with LIST-PORTS showing Qlen 1 WAK 0 on the room port and WAK 1 on
        # the admin port. Poll BOTH with flags 0 and sleep between passes.
        recv_ports, flag_lines = {}, []
        for n, text_line in joined:
            for m in re.finditer(r'\b(%s)\s*\(([^,]*),\s*([A-Za-z_]\w*)'
                                 % '|'.join(RECEIVE_ROUTINES), text_line, re.I):
                flags_arg, port_arg = m.group(2).strip(), m.group(3)
                recv_ports.setdefault(port_arg.upper(), n)
                if flags_wait_or_wake(flags_arg):
                    flag_lines.append(n)
        if len(recv_ports) > 1 and flag_lines:
            where = ', '.join(str(x) for x in sorted(set(flag_lines)))
            ports = ', '.join(sorted(recv_ports))
            out.append('%s:%d  receives on %d DIFFERENT ports (%s) with XFWTF or XFWAK set '
                       '(line(s) %s) - XFRCV waits on ONE port, so the task parks on one while '
                       'a message sits on the other. Poll both with flags 0 and sleep'
                       % (path, min(recv_ports.values()), len(recv_ports), ports, where))

        # ---- the two status tests that are both wrong -----------------------
        # "> 0" accepts XMXENTM (16896, an EMPTY port) and every error.
        # ">< XMOK" alone discards every message, because a receive may answer
        # a MESSAGE TYPE (XMTNO..XMTPS) as success. That one silently ate a
        # welcome and cost a day.
        # SCOPED TO THE LINES JUST AFTER A RECEIVE, and that scoping is the whole
        # difficulty. The same "returnStatus" variable is reused for xmpblet,
        # xmpopcn, xmpfsnd and the rest, where ">< XMOK" is exactly right - so
        # flagging every occurrence of the name reported 21 false alarms on
        # three good files. A lint rule that cries wolf is worse than none.
        recv_lines = []      # (line number, result variable)
        for n, text_line in joined:
            for m in re.finditer(r'\b(?:%s)\s*\([^)]*\)\s*&?\s*=:\s*([A-Za-z_]\w*)'
                                 % '|'.join(RECEIVE_ROUTINES), text_line, re.I):
                recv_lines.append((n, m.group(1).upper()))

        WINDOW = 6           # a receive and its status test are always close together
        by_line = dict((n, s) for n, _, s in code_lines())
        for rn, var in recv_lines:
            for n in range(rn, rn + WINDOW + 1):
                stripped = by_line.get(n)
                if stripped is None:
                    continue
                if re.search(r'\b%s\s*>\s*0' % re.escape(var), stripped, re.I):
                    out.append('%s:%d  "%s > 0" tests the result of the receive on line %d, and '
                               'accepts an EMPTY port - XMXENTM is 16896, and every error is '
                               'positive too. Accept XMOK OR XMTNO..XMTPS' % (path, n, var, rn))
                if re.search(r'\b%s\s*><\s*XMOK' % re.escape(var), stripped, re.I) \
                        and 'XMT' not in stripped.upper():
                    out.append('%s:%d  "%s >< XMOK" tests the result of the receive on line %d '
                               'and nothing else - that DISCARDS every message whose status is '
                               'a message TYPE (XMTNO..XMTPS), which is a successful receive too'
                               % (path, n, var, rn))

        # ---- no way out when XMSG dies underneath a POLLING loop ------------
        # XMXENRU (16933) is "XMSG not running". A poller that never looks for
        # it spins for ever on a dead kernel - which burned the machine once.
        # Only for a program that actually POLLS: the TimeOut sleep is the sign
        # of a loop meant to run for ever. A one-shot command program that
        # receives an answer and exits does not need this.
        if recv_lines and 'TIMEOUT' in text.upper() \
                and 'XMXENRU' not in text.upper() and 'XENRU' not in text.upper():
            out.append('%s  this program polls in a loop but never mentions XMXENRU (16933, '
                       '"XMSG not running") - when XMSG dies the receive stops blocking and the '
                       'loop has no way out' % path)

    # ---- a LOCAL filespec in a SINTRAN command must not be quoted --------------
    # The two routes to "make me a file" take OPPOSITE quoting:
    #
    #   MON50(file, ...)    quoting the name means CREATE IT IF ABSENT
    #   CREATE-FILE <name>  quoting the name is an ILLEGAL CHARACTER IN PARAMETER
    #
    # so a quoted name copied from the first into the second fails every time.
    # Measured on D100 2026-08-19: 'CREATE-FILE "CHAT:CNFG",1' handed to MON70
    # answered ILLEGAL CHARACTER IN PARAMETER, the file was never created, and
    # every nickname save had been failing silently since the day it was written.
    #
    # Quoting IS right for a REMOTE filespec, which goes to another machine's
    # command processor - so this only flags a name with no system prefix.
    for n, _, stripped in code_lines():
        m = re.search(r"'([^']*\b(?:CREATE-FILE|DELETE-FILE|RENAME-FILE)\s+\"[^\"]+\")",
                      stripped, re.I)
        if m and '(' not in m.group(1).split('"')[0][-3:]:
            out.append('%s:%d  a LOCAL filespec in a SINTRAN command must NOT be quoted - '
                       'it answers ILLEGAL CHARACTER IN PARAMETER. Quotes mean "create" to '
                       'MON50 and are illegal to CREATE-FILE, which is exactly how they get '
                       'copied from one to the other' % (path, n))

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

    # ---- a MESSAGE KIND that is declared and never handled ---------------------
    # THIS HAS NOW BITTEN THREE TIMES IN THE SAME PROGRAM, and every time it was
    # SILENT - which is what makes it worth a rule rather than a note.
    #
    #  1. kReject was declared and never tested, so a refusal fell through to the
    #     "<name>: <text>" printer and came out as
    #         OLAV: that nickname is taken
    #     - the server's words ABOUT a user, printed as that user speaking.
    #  2. kSaid, kJoined and kLeft appear NOWHERE outside their declarations. All
    #     three land on the same fall-through, and only kSaid is speech, so a join
    #     notice prints as a bare "SYSTEM:" line that reads as screen noise.
    #  3. writeCnfg hangs off kRenamed while the server answers kWelcome, so a
    #     nickname is confirmed on screen and never saved.
    #
    # A kind nobody handles produces NO error at all - not from the compiler, not
    # at run time - so it looks like a save failure, or a display glitch, or
    # nothing. The declaration is the only evidence that the case was intended.
    #
    # The rule: a constant whose name matches k<UpperCamel> and which appears
    # exactly once in the file (its own declaration) is almost certainly a case
    # nobody wrote. Names sent rather than received (kJoin, kSay, kLeave, kRename,
    # kWho) are still worth flagging - a kind this program never mentions again is
    # either dead or forgotten, and both want saying out loud.
    kind_decl = re.compile(r'^\s*(?:INTEGER|CONSTANT)\s*:\s*(k[A-Z][A-Za-z]*)\s*:=\s*(\d+)')
    declared = []
    for n, line, stripped in code_lines():
        m = kind_decl.match(stripped)
        if m:
            declared.append((n, m.group(1), m.group(2)))

    if declared:
        # Count uses across CODE only - a name mentioned in a comment is being
        # discussed, not handled, and that is exactly the state this rule exists
        # to catch.
        body = []
        for n, line, stripped in code_lines():
            body.append(mask_strings(strip_comment(line)))
        body_text = '\n'.join(body)
        for n, name, value in declared:
            uses = len(re.findall(r'\b%s\b' % re.escape(name), body_text))
            if uses <= 1:
                out.append("%s:%d  message kind %s (=%s) is declared and never used "
                           "again - an unhandled kind is SILENT: it falls through to "
                           "whatever the last branch is, or vanishes"
                           % (path, n, name, value))

    # ---- A NAME USED IN A ROUTINE THAT THE ROUTINE CANNOT SEE -----------------
    #
    # MEASURED 2026-08-22: showArrived in CHAT.PLNC used "nameAt", which exists
    # in that file ONLY as a parameter of setMyName. Six minutes into a compile
    # on D100 the answer came back as
    #
    #     2465 (1805)/SHOWARRIVE  *** ERROR   - ILLEGAL SYNTAX "NAMEAT"
    #
    # "ILLEGAL SYNTAX" is a misleading way to say "I have never heard of this
    # name" - the line is perfectly well formed. Worse, a file-wide "is it
    # declared anywhere" check would NOT have caught it, because the name is
    # declared, just not anywhere this routine can see. So the check has to know
    # about SCOPE: a routine sees its own parameters, its own locals, everything
    # declared at module level, the other routines, and the imports. Nothing
    # else.
    #
    # DELIBERATELY CONSERVATIVE. A linter that cries wolf gets ignored, and this
    # one is run before every build, so anything it cannot resolve confidently is
    # left alone rather than reported. It only ever looks at names that appear in
    # a position where a variable is being READ or WRITTEN.
    kw = set("""
        IF THEN ELSE ELSIF ENDIF DO ENDDO OD FOR IN BY ENDFOR WHILE UNTIL
        RETURN ROUTINE ENDROUTINE PROGRAM ENDPROGRAM MODULE ENDMODULE
        IMPORT EXPORT INTEGER INTEGER1 INTEGER2 INTEGER4 BYTE BYTES BOOLEAN
        REAL REAL4 REAL6 REAL8 VOID ARRAY RECORD ENDRECORD POINTER LABEL
        ENUMERATION CONSTANT TRUE FALSE AND OR NOT XOR ABS ADDR SIZE IND
        OUTPUT INPUT GO TO ON OFF ROUTINEERROR ENDON CASE ENDCASE OTHERWISE
        MOD SHIFT NIL PRECISION STANDARD ALIAS INLINE REFERENCE VALUE
        FORCE MONITOR_CALL
        ERRCODE ERRCLASS ERRPARAM
        INCASE RANGE ENDINCASE SELECT ENDSELECT GOTO EXITWHEN GLOBAL LOCAL
        GENERATE FREE NEW DISPOSE GENERALIZED PACKED GO_TO
        MAXINDEX MININDEX BIT_SIZE BLOCKSIZE FILESIZE
        """.split())

    # MAXINDEX and MININDEX are STANDARD ROUTINES the compiler provides - they
    # need no IMPORT and no declaration, so the undeclared-name check must not
    # report them. Added 2026-08-25 after MAXINDEX(text,1) - which is how a
    # routine asks a BYTES parameter how long it is instead of being handed a
    # hand-counted number - was reported as undeclared.
    #
    # ND-60.117.5 section 3.17 and page 153: MAXINDEX(array-identifier,
    # dimension-number) returns the DECLARED UPPER BOUND. One real restriction
    # is worth knowing and is NOT checked here: page 249 says MININDEX and
    # MAXINDEX for array parameters are NOT available in a STANDARD routine,
    # the FORTRAN/COBOL calling sequence. An ordinary PLANC routine is fine.

    # Every declared name in the file, with where it was declared. A module-level
    # declaration is one that is not inside a ROUTINE ... ENDROUTINE.
    # A DECLARATION IS "<type stuff> : <names>", and "type stuff" is richer than
    # the builtin list. Both of these are declarations and both were missed by a
    # narrower pattern, each producing a page of false alarms:
    #
    #     XMMSGIDENTIFIER : ident            a TYPE from the XMSG library
    #     INTEGER RANGE (0:255) : xmxOffset  a range-limited integer
    #
    # The ":" must not be the store operator "=:", hence the lookbehind.
    builtin_types = set("""
        INTEGER INTEGER1 INTEGER2 INTEGER4 BYTE BYTES BOOLEAN REAL REAL4 REAL6
        REAL8 POINTER LABEL ENUMERATION VOID
        """.split())
    decl_re = re.compile(
        r'^\s*([A-Za-z][A-Za-z0-9_]*)'          # the base type
        r'((?:\s+(?:ARRAY|RANGE|PACKED|POINTER))*'
        r'\s*(?:\([^)]*\))?)'                   # ARRAY / RANGE (0:255) / bounds
        r'\s*(?<![=:]):\s*(.+)$')

    def is_declaration(stripped):
        m = decl_re.match(stripped)
        if not m:
            return None
        base = m.group(1).upper()
        # A builtin, or one of the XMSG library's own types - those are all
        # spelled XM<something> in XMP-B02:DEFS.
        if base in builtin_types or re.match(r'^XM[A-Z0-9_]+$', base):
            return m.group(3)
        return None
    # "ROUTINE VOID, INTEGER (INTEGER, INTEGER) : name(a, b)" - possibly with the
    # ": name(...)" part on the next line after a trailing "&".
    routine_re = re.compile(r'^\s*ROUTINE\b(.*)$', re.IGNORECASE)

    def names_in(decl_tail):
        """The identifiers a declaration introduces.

        "i, j, at" -> i j at;  "mbrRoom(1:16, 1:16)" -> mbrRoom;
        "spaceByte := ' '" -> spaceByte. Anything inside brackets or after ":="
        is a SIZE or an INITIAL VALUE, not a new name.
        """
        found = []
        depth = 0
        current = ''
        for ch in decl_tail:
            if ch == '(':
                depth += 1
                continue
            if ch == ')':
                depth -= 1
                continue          # NOT appended - "outBuf(0:255)" must not
                                  # become "outBuf)", which matches no
                                  # identifier and silently drops the name.
                                  # That slip made the first run of this rule
                                  # report every module-level buffer in the
                                  # file as undeclared.
            if depth == 0 and ch == ',':
                found.append(current)
                current = ''
                continue
            if depth == 0:
                current += ch
        found.append(current)
        outn = []
        for piece in found:
            piece = piece.split(':=')[0].split('(')[0].strip()
            m = re.match(r'^([A-Za-z][A-Za-z0-9_]*)$', piece)
            if m:
                outn.append(m.group(1))
        return outn

    # Join continuation lines so a "&"-wrapped routine header reads as one line.
    joined = []          # (line_number_of_first_physical_line, text)
    pending_n, pending = None, ''
    for i, line in enumerate(lines):
        code = mask_strings(strip_comment(line)).rstrip()
        if pending_n is None:
            pending_n = i + 1
        if code.endswith('&'):
            pending += code[:-1] + ' '
            continue
        pending += code
        joined.append((pending_n, pending))
        pending_n, pending = None, ''

    module_names = set()
    # Module names declared WITH an initialiser - see the note where this is
    # filled. These are the ones a parameter may not reuse.
    module_consts = set()
    routine_names = set()
    import_names = set()

    # Names that arrive through a local $INCLUDE - see include_names().
    for _n, _code in joined:
        _m = re.match(r'^\$INCLUDE\s+(\S+)', _code.strip(), re.IGNORECASE)
        if _m:
            import_names |= include_names(path, _m.group(1))

    # (name, first_line, last_line, params, locals)
    routines = []
    cur = None
    in_import = False

    for n, code in joined:
        stripped = code.strip()
        if not stripped:
            continue

        if re.match(r'^\s*IMPORT\b', stripped, re.IGNORECASE):
            in_import = True
        if in_import:
            for m in re.finditer(r':\s*([A-Za-z][A-Za-z0-9_]*)', stripped):
                import_names.add(m.group(1).upper())
            if ')' in stripped:
                in_import = False
            continue

        rm = routine_re.match(stripped)
        if rm and not re.match(r'^\s*ENDROUTINE', stripped, re.IGNORECASE):
            # The declared name and its parameter list are after the LAST ":".
            tail = stripped.rsplit(':', 1)[-1].strip()
            nm = re.match(r'^([A-Za-z][A-Za-z0-9_]*)\s*(?:\(([^)]*)\))?', tail)
            if nm:
                params = []
                if nm.group(2):
                    for p in nm.group(2).split(','):
                        p = p.strip()
                        if re.match(r'^[A-Za-z][A-Za-z0-9_]*$', p):
                            params.append(p.upper())
                cur = {'name': nm.group(1).upper(), 'start': n, 'end': None,
                       'params': set(params), 'locals': set()}
                routine_names.add(nm.group(1).upper())
                routines.append(cur)
            continue

        if re.match(r'^\s*ENDROUTINE', stripped, re.IGNORECASE):
            if cur is not None:
                cur['end'] = n
                cur = None
            continue

        # CONSTANT IS A DECLARATION WITH NO COLON IN IT.
        #
        # "CONSTANT maxlen=100, pi=3.14" (manual 3.5 p.86) introduces names just
        # as "INTEGER : n" does, but the declaration pattern above looks for a
        # ":" and so never saw it. Every CONSTANT in a file was reported as
        # undeclared at its first use.
        cm = re.match(r'^CONSTANT\s+(.+)$', stripped, re.IGNORECASE)
        if cm is not None:
            for part in cm.group(1).split(','):
                nm4 = re.match(r'\s*([A-Za-z][A-Za-z0-9_]*)', part)
                if nm4:
                    if cur is not None:
                        cur['locals'].add(nm4.group(1).upper())
                    else:
                        module_names.add(nm4.group(1).upper())
                        module_consts.add(nm4.group(1).upper())
            continue

        dm = is_declaration(stripped)
        if dm is not None:
            initialised = ':=' in dm
            for nm2 in names_in(dm):
                if cur is not None:
                    cur['locals'].add(nm2.upper())
                else:
                    module_names.add(nm2.upper())
                # A PLANC LOCAL CANNOT CARRY AN INITIALISER - "INTEGER : x := 1"
                # inside a routine draws INITIAL VALUE ILLEGAL HERE. So anything
                # with a ":=" is certainly module level, whatever this parser
                # thinks the surrounding scope is. That makes it the one set of
                # names we can be SURE about, which is why the parameter rule
                # below tests against it and not against module_names.
                if initialised:
                    module_consts.add(nm2.upper())

    # PROGRAM/MODULE bodies are not routines; anything declared there is visible
    # to the code in them, and that code is not checked here.
    xmp_names = set()
    if XMP_API:
        for section in ('routines', 'constants'):
            entries = XMP_API.get(section) or []
            for e in entries:
                nm3 = e.get('name') if isinstance(e, dict) else e
                if isinstance(nm3, str):
                    xmp_names.add(nm3.upper())

    visible_everywhere = module_names | routine_names | import_names | xmp_names | kw

    # ---- TWO NAMES THAT ARE THE SAME IN THEIR FIRST TEN CHARACTERS ------------
    #
    # PLANC KEEPS TEN. "buildAdminNum" IS "buildAdmin", so declaring it beside an
    # existing buildAdmin is declaring the same routine twice:
    #
    #     950 (290) *** ERROR - IDENTIFIER ALREADY SPECIFIED/DECLARED "BUILDADMIN"
    #
    # and then TWENTY-FOUR more diagnostics, because the second declaration's
    # body is parsed as though it were loose statements - NOT PREVIOUSLY DECLARED
    # "KIND", MISPLACED STATEMENT, and so on down the routine. Only the FIRST
    # line means anything. Measured on D100, 2026-08-22.
    #
    # Checked across routine names AND module-level names together, since the
    # truncation does not care which kind of thing it is.
    seen_short = {}
    for n, code in joined:
        m = re.match(r'^\s*ROUTINE\b.*?:\s*([A-Za-z][A-Za-z0-9_]*)', code.strip(),
                     re.IGNORECASE)
        if m:
            full = m.group(1)
            short = full.upper()[:10]
            if short in seen_short and seen_short[short][0].upper() != full.upper():
                out.append('%s:%d  "%s" and "%s" (line %d) are THE SAME NAME to '
                           'PLANC - it keeps only the first ten characters. The '
                           'compiler answers IDENTIFIER ALREADY SPECIFIED/DECLARED '
                           'and then mis-parses the whole routine; only that first '
                           'line is real.'
                           % (path, n, full, seen_short[short][0], seen_short[short][1]))
            else:
                seen_short[short] = (full, n)

    # ---- ASSIGNING FROM A ROUTINE THAT RETURNS VOID ---------------------------
    #
    # "ROUTINE VOID, VOID (INTEGER) : addPeer" returns nothing, so
    #
    #     addPeer(taken) =: length
    #
    # is not a value to store. The compiler answers
    #
    #     3005 (2345)/HANDLEMESS *** ERROR - ILLEGAL DATA TYPE "ADDPEER"
    #
    # naming the ROUTINE, which reads like a problem with the argument rather
    # than with the assignment. Measured on D100 2026-08-22, twice in one
    # compile.
    #
    # This one is safe to check because both halves are unambiguous in the text:
    # the declaration says "VOID, VOID" and the call site has "=:" after it.
    void_routines = set()
    for n, code in joined:
        m = re.match(r'^\s*ROUTINE\s+VOID\s*,\s*VOID\b.*?:\s*([A-Za-z][A-Za-z0-9_]*)',
                     code.strip(), re.IGNORECASE)
        if m:
            void_routines.add(m.group(1).upper())
    for n, code in joined:
        stripped = mask_strings(strip_comment(code))
        for m in re.finditer(r'\b([A-Za-z][A-Za-z0-9_]*)\s*\([^()]*\)\s*=:', stripped):
            if m.group(1).upper() in void_routines:
                out.append('%s:%d  "%s" is declared "ROUTINE VOID, VOID" - it returns '
                           'nothing, so there is no value to store. The compiler '
                           'answers ILLEGAL DATA TYPE and names the ROUTINE, which '
                           'looks like an argument problem. Drop the "=:".'
                           % (path, n, m.group(1)))

    # ---- NOT A RULE: THE SUBARRAY-OF-AN-INITIALISED-BYTES TRAP ----------------
    #
    # There IS a real trap here and it has cost two compile cycles, but it is NOT
    # understood well enough to check, and a rule that flags working code is
    # worse than no rule at all.
    #
    # WHAT IS MEASURED:
    #   FAILS  CHATSV sendToName, slicing a BYTES FORMAL PARAMETER bound to
    #          trkName:  xmpblet(..., portName(0:nmLen - 1))
    #          -> 2512 (1852)/SENDTONAME *** ERROR - ILLEGAL DATA TYPE "XMPBLET"
    #   FAILS  CHATMON sendAdmin, per its own comment of 2026-08-20, slicing
    #          admName directly: xmpblet(..., admName(0:lenAdmName - 1))
    #          -> 830 (170)/SENDADMIN *** ERROR - ILLEGAL DATA TYPE "XMPBLET"
    #   WORKS  CHAT sendJoin, slicing servName directly:
    #          xmpblet(..., servName(0:lenServName - 1))          compiles clean
    #   WORKS  every xmpopnm here, slicing admName / trkName directly.
    #
    # The first draft of this rule flagged all four and would have condemned two
    # programs that build. Whatever separates the failing cases from the working
    # ones, it is not "initialised BYTES gets sliced" - so nothing is asserted.
    #
    # THE SAFE HABIT, until somebody carves it properly: pass the WHOLE array to
    # xmpblet when the BYTES was declared with an initialiser. That works in
    # every case above.

    # ---- A PARAMETER THAT REUSES A MODULE-LEVEL NAME --------------------------
    #
    # PARAMETERS ONLY. A routine LOCAL may shadow a module-level name perfectly
    # happily - "INTEGER : i" inside a routine while the module also has an "i"
    # is everywhere in these programs and has built for days. The first version
    # of this rule checked locals too and reported 51 problems across files that
    # compile clean, which would have made it useless: a linter that cries wolf
    # gets switched off, and this one runs before every build.
    #
    # A PARAMETER is different, and that difference is what cost the cycle:
    #
    #     2502 (1842)/SENDTONAME *** ERROR - IDENTIFIER ALREADY SPECIFIED/DECLARED "NAMELEN"
    #
    # and it does not stop there. The parameter list comes apart, so the next
    # three lines of the same routine each drew their own diagnostic - ILLEGAL
    # SYNTAX on a parameter that was perfectly good, then two ILLEGAL DATA TYPEs.
    # Reading the FIRST error is the only way through that; the others are noise.
    #
    # Measured on D100 2026-08-22: "sendToName(portName, nameLen, ...)" against a
    # module-level "INTEGER : nameLen := 16". One full compile cycle.
    for r in routines:
        if r['end'] is None:
            continue
        for nm5 in sorted(r['params']):
            if nm5 in module_consts:
                out.append('%s:%d  %s\'s PARAMETER "%s" reuses a module-level name '
                           'that has an INITIALISER - the compiler answers '
                           'IDENTIFIER ALREADY SPECIFIED/DECLARED and then '
                           'mis-parses the rest of the routine. Rename the '
                           'parameter.' % (path, r['start'], r['name'], nm5))

    # A name is only inspected when it is READ or WRITTEN in a way that cannot be
    # anything else: "x =: y", "x(i) =: y", or a bare use as a subscript or in a
    # comparison. Keeping the shapes narrow is what keeps this quiet.
    ident_re = re.compile(r'\b([A-Za-z][A-Za-z0-9_]*)\b')
    for r in routines:
        if r['end'] is None:
            continue
        seen_here = set()
        known = r['params'] | r['locals'] | visible_everywhere
        for n, code in joined:
            if n <= r['start'] or n >= r['end']:
                continue
            stripped = code.strip()
            if not stripped or is_declaration(stripped) is not None:
                continue
            for m in ident_re.finditer(stripped):
                nm4 = m.group(1)
                up = nm4.upper()
                if up in known or up in seen_here:
                    continue
                # An octal literal - 221B, 267B - is a NUMBER, not a name.
                if re.match(r'^[0-7]+B$', up):
                    continue
                # Everything the XMSG library exports is spelled XM<something>
                # in XMP-B02:DEFS - types like XMUSERADDRESS as well as the
                # routines and constants. xmp-api.json carries the routines and
                # constants but not the TYPES, so the shape is trusted rather
                # than the list. Nothing else in these programs is named XM*.
                if re.match(r'^XM[A-Z0-9_]+$', up):
                    continue
                # A field width inside an OUTPUT, 'AL38', is masked already, but
                # a bare ALnn elsewhere is not a variable either.
                if re.match(r'^A[LR]?\d+$', up):
                    continue
                seen_here.add(up)
                out.append('%s:%d  "%s" is used in %s but is not declared there, at '
                           'module level, or as a parameter - the compiler answers '
                           'ILLEGAL SYNTAX "%s", which reads like a punctuation '
                           'problem and is not one'
                           % (path, n, nm4, r['name'], up))

    # ---- A LOCAL MAY NOT CARRY AN INITIAL VALUE --------------------------------
    #
    #     PROGRAM : mainUi
    #         BOOLEAN : smallOpen := FALSE       <- ILLEGAL
    #
    # draws, on the compiler's FIRST pass,
    #
    #     166   (141)/MAINUI  *** ERROR   - INITIAL VALUE ILLEGAL HERE "SMALLOPEN"
    #
    # Only a MODULE-level declaration may be initialised. Inside a ROUTINE or a
    # PROGRAM the declaration must be bare and the value assigned as a statement.
    # The rest of this file already RELIES on that rule - the scope check treats
    # any ":=" as proof of module level - but nothing ever CHECKED it.
    #
    # MEASURED ON D100 2026-08-24, and it cost a build cycle plus a wrong
    # diagnosis. What made it expensive is that the program still LINKED and still
    # RAN: the two flags were simply never set, so two menu keys did nothing and
    # the fault was first blamed on PLANC's NOT operator. The compile errors were
    # on screen the whole time and were missed, because the "0 DIAGNOSTICS" that
    # stays visible belongs to the SECOND pass and sits directly under a COMPILE
    # that had two.
    depth = 0
    unit_name = None
    for n, _, stripped in code_lines():
        opens = re.match(r'^(ROUTINE|PROGRAM)\s', stripped, re.IGNORECASE)
        if opens:
            depth += 1
            nm = re.search(r':\s*([A-Za-z_]\w*)', stripped)
            if nm:
                unit_name = nm.group(1)
            continue

        if re.match(r'^ENDROUTINE', stripped, re.IGNORECASE):
            if depth > 0:
                depth -= 1
            continue

        if depth <= 0:
            continue

        # A LOCAL DECLARED "READ" MAY CARRY ONE. Rule R20 in
        # Developer/Languages/Application/PLANC-LANGUAGE-RULES.md, from RM5 8.6: a
        # local without READ access is allocated on the stack at run time, so it
        # has nowhere to put a compile-time value - but a READ one is constant and
        # does. "INTEGER READ : counter := 0" inside a routine is LEGAL.
        #
        # is_declaration happens not to match that shape today, so this guard is
        # belt and braces - but the rule is the reason, and leaving it implicit
        # would make the next person's tightening of the declaration pattern turn
        # this check into a false alarm.
        if re.search(r'\bREAD\b', stripped, re.IGNORECASE):
            continue

        # Same recogniser the scope check uses, so the two agree on what a
        # declaration is.
        tail = is_declaration(stripped)
        if tail is None or ':=' not in tail:
            continue

        # A ":=" inside brackets is a bound or an index, not an initial value.
        before = tail.split(':=')[0]
        if before.count('(') != before.count(')'):
            continue

        names = names_in(tail)
        if not names:
            continue

        where = (' in ' + unit_name) if unit_name else ''
        out.append(
            '%s:%d  "%s" is declared%s WITH an initial value, and a PLANC LOCAL may '
            'not carry one - the compiler answers *** ERROR INITIAL VALUE ILLEGAL '
            'HERE. Declare it bare and assign the value as a statement after '
            'INISTACK. The build still LINKS and RUNS with the name never set, so '
            'this shows up as behaviour that quietly does nothing'
            % (path, n, names[0], where))

    # ---- a ROUTINE must declare ONE TYPE PER PARAMETER ------------------------
    # MEASURED on D100, 2026-08-25: relayOnward was written with SIX INTEGER types
    # and SEVEN parameter names. The compiler answers
    #
    #     *** ERROR   - EXPECTS ")" ILLEGAL SYNTAX ","
    #
    # at the seventh name, then two more ILLEGAL SYNTAX "," as it stumbles over the
    # rest of the list. Every one of those reads like a BRACKET fault, so the eye
    # goes hunting for a missing parenthesis and never counts the two lists. It
    # cost a fifty-minute compile of a 5000-line source to find out.
    #
    # The header is often split across a continuation, so the logical line has to
    # be rebuilt before the two lists can be compared at all.
    joined = []
    buffer_text = ''
    buffer_line = 0
    for i, raw in enumerate(lines):
        body = strip_comment(raw).rstrip()
        if not body.strip():
            continue
        if not buffer_text:
            buffer_line = i + 1
        if body.endswith('&'):
            buffer_text += body[:-1]
            continue
        buffer_text += body
        joined.append((buffer_line, buffer_text))
        buffer_text = ''
    if buffer_text:
        joined.append((buffer_line, buffer_text))

    routine_header = re.compile(
        r'\bROUTINE\b[^(]*\((?P<types>[^()]*)\)\s*:\s*'
        r'(?P<name>[A-Za-z][A-Za-z0-9_]*)\s*\((?P<names>[^()]*)\)',
        re.IGNORECASE)
    for n, logical in joined:
        found = routine_header.search(mask_strings(logical))
        if found is None:
            continue

        def parts(text):
            """Comma-separated items, with an empty list for an empty bracket."""
            text = text.strip()
            if not text:
                return []
            return [one.strip() for one in text.split(',') if one.strip()]

        types = parts(found.group('types'))
        names = parts(found.group('names'))
        if types and names and len(types) != len(names):
            out.append(
                '%s:%d  routine "%s" declares %d parameter TYPE(S) but names %d '
                'parameter(S) - PLANC wants one type per name. The compiler will '
                'answer EXPECTS ")" ILLEGAL SYNTAX "," at the first name it has no '
                'type for, which looks like a bracket fault and is not'
                % (path, n, found.group('name'), len(types), len(names)))

    # ---- two names that become ONE after truncation ---------------------------
    # ND-60.117.5 keeps TEN characters of an identifier and PLANC does not warn:
    # it TRUNCATES and carries on. relayOnward compiled quite happily as
    # RELAYONWAR on 2026-08-25.
    #
    # A long name on its own is therefore HARMLESS - this server has nineteen of
    # them and every one works - so flagging length would be nothing but noise.
    # What is NOT harmless is two names sharing those first ten characters,
    # because they silently become the SAME name: one routine ends up calling the
    # other, the build is clean and the program does something nobody wrote.
    # That collision is what is reported here.
    declared_name = re.compile(
        r'ROUTINE[^:]*:\s*(?P<name>[A-Za-z][A-Za-z0-9_]*)', re.IGNORECASE)
    first_ten = {}
    for n, logical in joined:
        found = declared_name.search(mask_strings(logical))
        if found is None:
            continue
        name = found.group('name')
        key = name[:10].upper()
        if key in first_ten and first_ten[key][0] != name:
            other_name, other_line = first_ten[key]
            out.append(
                '%s:%d  routine "%s" and "%s" on line %d share their first TEN '
                'characters, and PLANC keeps only ten - both compile to "%s" and '
                'become ONE name with NO warning. Rename one of them'
                % (path, n, name, other_name, other_line, key))
        else:
            first_ten[key] = (name, n)

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
