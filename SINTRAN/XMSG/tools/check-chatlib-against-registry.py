#!/usr/bin/env python3
"""Does CHATLIB agree with the protocol registry about the message kinds?

WHY THIS EXISTS. `DOC/protocols/chat-wire.json` is the registry - the recorded truth about
what goes on the wire - and there are generators beside it for C, C# and Lua. There is NO
generator for PLANC, so `SINTRAN-CHAT/CHATLIB.PLNC` is written by hand. A hand-written copy
of a registry drifts, and the drift is silent: both sides compile, both sides run, and the
two machines disagree only when one of them is old.

The chat client and server no longer declare the kinds themselves - they IMPORT them from
CHATLIB - so CHATLIB is now the single place a wrong number would enter the product. That
makes this check worth its few seconds.

WHAT IT DOES NOT DO. It does not check kinds the registry knows about and CHATLIB does not.
CHATLIB deliberately holds only the twenty kinds BOTH programs used to declare; the trunk
and admin kinds live in the server alone and have no business crossing a module boundary.
Absence is a decision here, not a fault. A number that DISAGREES is always a fault.

Usage:
    check-chatlib-against-registry.py [chatlib.plnc] [chat-wire.json]

Exit code 0 if they agree, 1 if they do not, 2 if a file is missing.
"""

import io
import json
import os
import re
import sys

# The registry names a kind "Renamed"; PLANC calls it kRenOk. The wire number is the same
# thing under two naming conventions, and this is the map between them. Two of these
# differ from the obvious "k" + registry name because PLANC names are unique in only SEVEN
# characters across a BRF boundary: kRename/kRenamed both read KRENAME, and
# kDirect/kDirected both read KDIRECT, so the second of each pair was renamed.
PLANC_NAME = {
    "Join": "kJoin",
    "Welcome": "kWelcome",
    "Reject": "kReject",
    "Say": "kSay",
    "Said": "kSaid",
    "Leave": "kLeave",
    "Joined": "kJoined",
    "Left": "kLeft",
    "Rename": "kRename",
    "Renamed": "kRenOk",        # kRenamed collided with kRename at seven characters
    "Who": "kWho",
    "Map": "kMap",
    "Rooms": "kRooms",
    "Topic": "kTopic",
    "AllWho": "kAllWho",
    # The next three are the registry's long names against PLANC's short ones. Nothing
    # forced these to be short - kHistory would have crossed a BRF boundary perfectly well -
    # but they were written short and the wire number is what matters, so they stay.
    "History": "kHist",
    "Direct": "kDirect",
    "Directed": "kDirGot",      # kDirected collided with kDirect at seven characters
    "DirectSent": "kDirSent",
    "DirectBad": "kDirBad",
}


def planc_kinds(path):
    """Every `INTEGER : kXxx := n` in the library, as {name: value}."""
    text = io.open(path, encoding="latin-1", newline="").read()
    found = {}
    for m in re.finditer(r"^\s*INTEGER\s*:\s*(k\w+)\s*:=\s*(\d+)", text, re.M):
        found[m.group(1)] = int(m.group(2))
    return found


def registry_kinds(path):
    """The registry's message_kinds, as {name: value}."""
    doc = json.load(io.open(path, encoding="utf-8"))
    out = {}
    for entry in doc.get("message_kinds", {}).get("values", []):
        try:
            out[entry["name"]] = int(entry["value"])
        except (KeyError, ValueError):
            continue
    return out


def main():
    lib = sys.argv[1] if len(sys.argv) > 1 else os.path.join(
        os.path.dirname(__file__), "..", "SINTRAN-CHAT", "CHATLIB.PLNC")
    reg = sys.argv[2] if len(sys.argv) > 2 else os.path.join(
        os.path.dirname(__file__), "..", "DOC", "protocols", "chat-wire.json")

    for p in (lib, reg):
        if not os.path.exists(p):
            print("no file at %s" % p)
            return 2

    have = planc_kinds(lib)
    want = registry_kinds(reg)

    problems = []
    checked = 0

    for reg_name, planc_name in sorted(PLANC_NAME.items(), key=lambda kv: want.get(kv[0], 0)):
        if reg_name not in want:
            problems.append("registry has no kind called %s - the map in this script is stale"
                            % reg_name)
            continue
        if planc_name not in have:
            problems.append("CHATLIB does not declare %s (registry %s = %d)"
                            % (planc_name, reg_name, want[reg_name]))
            continue
        checked += 1
        if have[planc_name] != want[reg_name]:
            problems.append("%s is %d in CHATLIB but %s is %d in the registry"
                            % (planc_name, have[planc_name], reg_name, want[reg_name]))

    # A number CHATLIB has that the registry does not know at all is worth saying, because
    # it means somebody invented a kind without recording it.
    mapped = set(PLANC_NAME.values())
    for name, value in sorted(have.items(), key=lambda kv: kv[1]):
        if name not in mapped:
            problems.append("CHATLIB declares %s = %d, which is in no registry entry this "
                            "script knows - record it in chat-wire.json" % (name, value))

    print("checked %d kinds against %s" % (checked, os.path.basename(reg)))

    if problems:
        print("*** CHATLIB AND THE REGISTRY DISAGREE:")
        for p in problems:
            print("  - " + p)
        return 1

    print("CHATLIB agrees with the registry")
    return 0


if __name__ == "__main__":
    sys.exit(main())
