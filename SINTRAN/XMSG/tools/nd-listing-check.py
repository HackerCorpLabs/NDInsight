#!/usr/bin/env python3
"""Judge a PLANC listing - BOTH halves, because checking one is how a bad build looks green.

A listing is green only when two separate things are true, and almost everyone checks the first
and forgets the second:

  1. No line carries the compiler's `*** ERROR` marker.

     THIS TEST WAS WRONG UNTIL 2026-08-26 AND PASSED EVERY BROKEN BUILD. It looked for a line
     BEGINNING with `***`, and the compiler does not emit one - a real diagnostic looks like

         1629   (944)/BUILDTELLT  *** ERROR   - NOT PREVIOUSLY DECLARED "WINLABLEN"

     with the marker in the MIDDLE, after the line numbers and the routine name. So the only
     half of this gate that ever worked was the truncation test below. Proved by injecting the
     line above into a good listing: the checker still said "clean and complete".

     The discriminator between a real diagnostic and a listing line that merely ECHOES a source
     comment quoting an old error - these sources are full of those on purpose - is what follows
     the line numbers. A diagnostic has `/ROUTINENAME`; echoed source does not.

  2. THE LISTING REACHES THE LAST SOURCE LINE. The compiler reports no fault at all for a part of
     the file it never read, so a source that ends early - or one a stalled transfer spliced a
     chunk out of - compiles perfectly and does the wrong thing at run time. On 2026-08-24 a
     truncated CHATSV.PLNC listed to source line 4857 where the good one reaches 4885. There was
     no error message anywhere, and it cost most of a day.

Do not read the "0 DIAGNOSTICS" the compiler prints on screen either. On a long source the real
count scrolls off the top, and the zero at the foot of the SECOND pass sits directly under a
compile that had three.

Usage:
    nd-listing-check.py <listing file> <expected source line count>

Exit code 0 if the build is good, 1 if it is not.
"""

import re
import sys
import os


# The compiler's own diagnostic line, and NOT a listing line that echoes a source comment
# quoting one. The difference is "/ROUTINENAME" straight after the line numbers.
DIAGNOSTIC = re.compile(r"^\s*\d+\s+\(\d+\)/\S+\s+\*\*\*\s*(ERROR|WARNING)")


def read_listing(path):
    """Read an ND listing, stripping the parity bit that ND text files may carry."""
    data = open(path, "rb").read()
    text = bytes(b & 0x7F for b in data).decode("ascii", "replace")
    return text.replace("\r\n", "\n").replace("\r", "\n").split("\n")


def last_source_line(lines):
    """The highest source line number the listing mentions.

    The listing puts the compiler's own line count on the left and the SOURCE line number in
    brackets after it, so the last bracketed number is how far the compiler actually read.
    """
    best = 0
    for line in lines:
        start = line.find("(")
        if start == -1:
            continue
        end = line.find(")", start)
        if end == -1:
            continue
        inner = line[start + 1:end].strip()
        if inner.isdigit():
            value = int(inner)
            if value > best:
                best = value
    return best


def self_test():
    """Prove the error test can actually FAIL before trusting it to pass.

    THIS EXISTS BECAUSE THE ERROR TEST WAS BLIND FOR WEEKS. It looked for a line beginning
    with `***`; the compiler puts the marker mid-line. Every broken build passed. A gate that
    cannot fail is not a gate, and nothing about the output said so - it printed
    "listing is clean and complete" over a compile with seven errors.

    Returns a list of problems. An empty list means the checker itself is sound.
    """
    problems = []

    must_fail = [
        '  1629   (944)/BUILDTELLT  *** ERROR   - NOT PREVIOUSLY DECLARED "WINLABLEN"',
        '  2494  (1834)/HANDLECOMM  *** ERROR   - ILLEGAL SYNTAX "SHOWMYNAME"',
    ]
    # A listing line that merely ECHOES a source comment quoting an old error. These sources
    # are full of them deliberately, and treating one as a fault fails a perfectly good build.
    must_pass = [
        '  2395  (1710)      %     2494  (1834)/HANDLECOMM  *** ERROR   - ILLEGAL SYNTAX',
        '  4868  (4183)      % drew "2465 (1805)/SHOWARRIVE *** ERROR - ILLEGAL SYNTAX", which is',
        '  1234   (567)  some ordinary source line',
    ]

    for line in must_fail:
        if not DIAGNOSTIC.match(line):
            problems.append("does NOT flag a real diagnostic: " + line.strip()[:60])
    for line in must_pass:
        if DIAGNOSTIC.match(line):
            problems.append("wrongly flags an echoed comment: " + line.strip()[:60])

    return problems


def main():
    broken = self_test()
    if broken:
        print("nd-listing-check IS BROKEN AND WOULD REPORT \"clean\" WRONGLY:")
        for b in broken:
            print("  - " + b)
        return 2

    if len(sys.argv) < 3:
        print("usage: nd-listing-check.py <listing> <expected source lines>")
        return 2

    path = sys.argv[1]
    expected = int(sys.argv[2])

    if not os.path.exists(path):
        print("no listing at %s - the compile did not run" % path)
        return 1

    lines = read_listing(path)
    # A REAL DIAGNOSTIC, not a source comment that quotes one.
    #
    #   "  1629   (944)/BUILDTELLT  *** ERROR   - ..."     <- the compiler talking
    #   "  2395  (1710)      %  ... *** ERROR ..."          <- our own comment, echoed
    #
    # Both contain the marker. Only the first has "/ROUTINE" straight after the line
    # numbers, so that is what is tested rather than the marker's position.
    errors = []
    warnings = []
    for l in lines:
        m = DIAGNOSTIC.match(l)
        if not m:
            continue
        if m.group(1) == "ERROR":
            errors.append(l.rstrip())
        else:
            warnings.append(l.rstrip())
    reached = last_source_line(lines)

    print("listing %d lines, reached source line %d of %d expected"
          % (len(lines), reached, expected))

    bad = False

    if errors:
        bad = True
        print("*** %d COMPILER ERRORS:" % len(errors))
        for e in errors[:12]:
            print("      %s" % e[:150])
        if len(errors) > 12:
            print("      ... and %d more" % (len(errors) - 12))

    # Allow a small slack: the listing's last bracketed number can trail the file's physical line
    # count by a line or two depending on how the source ends. A REAL truncation is far larger -
    # the one that cost a day was 28 lines.
    if reached < expected - 3:
        bad = True
        print("*** THE LISTING STOPS %d LINES SHORT." % (expected - reached))
        print("      The compiler found no fault in the part it never read, so this build is")
        print("      green and wrong. The usual cause is a stalled transfer that spliced a chunk")
        print("      out of the file on the machine. Run nd-verify.ps1 and deploy again.")

    if bad:
        return 1

    print("listing is clean and complete")
    return 0


if __name__ == "__main__":
    sys.exit(main())
