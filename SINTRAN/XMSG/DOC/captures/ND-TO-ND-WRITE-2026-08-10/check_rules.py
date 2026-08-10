"""
Check two claims against the ND-to-ND write capture:

  1. The short-acknowledgement counter: who counts, from what, and per what scope.
  2. Flags 1: is it each sender's OWN counter, or ONE sequence shared by the pair?

Both are checked by construction rather than by eye - the point of the exercise is
that our own push gets one of them wrong and eyeballing hex is how that was missed.
"""
import sys
import collections
import decode_hub

SHORT_ACK = 0x07A2


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def messages(path):
    for sender, p in decode_hub.frames(path):
        typesub = be16(p, 2)
        flags1 = be16(p, 8)
        body = p[14:]
        if len(body) < 14:
            continue
        sub = body[:14]
        fa = body[14:]
        yield sender, typesub, flags1, sub, fa


def main(path):
    print("=== 1. Short acknowledgements ===")
    print("%-5s %-6s %-6s %-6s %-6s %-6s %s"
          % ("from", "f1", "srcprt", "dstprt", "conv", "count", "trailer"))
    per_port = collections.OrderedDict()
    for sender, typesub, flags1, sub, fa in messages(path):
        if len(fa) < 8 or be16(fa, 0) != SHORT_ACK:
            continue
        srcport = be16(sub, 10)
        dstport = be16(sub, 6)
        conv = be16(fa, 2)
        count = be16(fa, 4)
        trailer = be16(fa, 6)
        print("%-5s %04x   %04x   %04x   %04x   %04x   %04x"
              % (sender, flags1, srcport, dstport, conv, count, trailer))
        per_port.setdefault((sender, srcport), []).append(count)

    print()
    print("counter runs, per (sender, source port):")
    for key in per_port:
        run = per_port[key]
        print("  %-5s port %04x : %s" % (key[0], key[1],
              " ".join("%04x" % v for v in run)))

    print()
    print("=== 2. Flags 1: shared sequence, or one per sender? ===")
    # An ORIGINATOR is anything that is not a short acknowledgement and not a
    # SINTRAN-level ACK (subtype 0x03). Those two ANSWER an exchange and echo its
    # number, which is already established. The open question is only what a side
    # picks when it STARTS one.
    print("%-5s %-6s %s" % ("from", "f1", "role"))
    last = None
    shared_ok = True
    for sender, typesub, flags1, sub, fa in messages(path):
        subtype = typesub & 0xFF
        kind = be16(fa, 0) if len(fa) >= 2 else 0
        if subtype == 0x03:
            continue                      # datagram-level ack, echoes
        if kind == SHORT_ACK:
            continue                      # FA short ack, echoes
        print("%-5s %04x   originates" % (sender, flags1))
        if last is not None and flags1 != ((last + 1) & 0xFFFF):
            shared_ok = False
            print("        ^ not last+1 (last originated %04x)" % last)
        last = flags1
    print()
    print("every origination is the previous origination + 1, "
          "REGARDLESS of which machine sent it: %s" % shared_ok)


if __name__ == "__main__":
    main(sys.argv[1])
