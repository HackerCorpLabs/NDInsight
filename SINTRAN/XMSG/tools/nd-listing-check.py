#!/usr/bin/env python3
"""Judge a PLANC listing - BOTH halves, because checking one is how a bad build looks green.

A listing is green only when two separate things are true, and almost everyone checks the first
and forgets the second:

  1. No line begins with `***`. That is the compiler's error marker.

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

import sys
import os


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


def main():
    if len(sys.argv) < 3:
        print("usage: nd-listing-check.py <listing> <expected source lines>")
        return 2

    path = sys.argv[1]
    expected = int(sys.argv[2])

    if not os.path.exists(path):
        print("no listing at %s - the compile did not run" % path)
        return 1

    lines = read_listing(path)
    errors = [l.rstrip() for l in lines if l.lstrip().startswith("***")]
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
