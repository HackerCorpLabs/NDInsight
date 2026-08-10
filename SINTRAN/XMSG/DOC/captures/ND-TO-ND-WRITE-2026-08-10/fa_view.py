"""
Lay out the FA layer of a hub capture: one line per message, named fields.

The interesting question this was written for: what exactly does a REAL client put
in its short acknowledgement (0x07A2), which is the message our own push sends and
D100 refuses to accept.
"""
import sys
import decode_hub

FA_TYPES = {
    0x07F0: "Request/Reply",
    0x07A2: "ShortAck",
    0x07D2: "ConnectConfirm",
    0x07E2: "ConnectLetter",
    0x07C0: "Close",
    0x0782: "Release",
}


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def main(path):
    print("%-5s %-6s %-14s %-6s %-6s %-6s %s"
          % ("from", "f1", "kind", "conv", "word2", "word3", "rest"))
    for sender, p in decode_hub.frames(path):
        flags1 = be16(p, 8)
        body = p[14:]
        if len(body) < 14:
            continue
        # The XMSG sub-header is 14 bytes; the FA message starts after it.
        fa = body[14:]
        if len(fa) < 2:
            continue
        kind = be16(fa, 0)
        name = FA_TYPES.get(kind)
        if name is None:
            name = "data/%04x" % kind
        conv = be16(fa, 2) if len(fa) >= 4 else 0
        w2 = be16(fa, 4) if len(fa) >= 6 else 0
        w3 = be16(fa, 6) if len(fa) >= 8 else 0
        print("%-5s %04x   %-14s %04x   %04x   %04x   %s"
              % (sender, flags1, name, conv, w2, w3, fa[8:40].hex()))


if __name__ == "__main__":
    main(sys.argv[1])
