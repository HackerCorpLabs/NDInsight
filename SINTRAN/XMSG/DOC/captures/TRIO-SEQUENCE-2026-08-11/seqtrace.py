"""Follow the datagram sequence between real ND machines across repeated conversations.

The question this answers: when the SAME two machines talk again, and again, without
anything being restarted, does the sequence each side stamps CONTINUE, or restart? And
is it one counter per machine or one per PEER?

That is the thing no single capture can show. It needs several conversations in a row
with nothing reset between them, which is exactly what this capture is.

Reads the hub capture through tshark, keeping only the machine-to-hub direction so each
frame is counted once (the hub fans a frame out to every other member - counting the
copies multiplies everything by the member count).

Layout, outermost first:
    2-byte length | 802.3 | LLC A8 A8 03 | 11-byte ND link | 14-byte SINTRAN datagram
The SINTRAN datagram is: 2113 | subtype | dest | src | flags1 | flags2 | checksum
"""
import collections
import subprocess
import sys

TSHARK = r"C:\Program Files\Wireshark\tshark.exe"
HUB_PORT = 5010

# Subtypes worth naming; anything else prints as a number.
SUBTYPE = {
    0x03: "Ack", 0x07: "NetErr", 0x0A: "Frag1", 0x0C: "FragC",
    0x0E: "Data", 0x13: "ReachReply", 0x19: "ReachReq",
}


def read_segments(path):
    """Yield (srcport, dstport, payload) for every TCP segment carrying data."""
    out = subprocess.run(
        [TSHARK, "-r", path, "-Y", "tcp.len>0", "-T", "fields",
         "-e", "tcp.srcport", "-e", "tcp.dstport", "-e", "tcp.payload"],
        capture_output=True, text=True, check=True)
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) < 3 or not parts[2]:
            continue
        hexed = parts[2].replace(":", "").replace(",", "")
        yield int(parts[0]), int(parts[1]), bytes.fromhex(hexed)


def frames(path):
    """Yield each Ethernet frame once, in capture order."""
    buffers = collections.defaultdict(bytearray)
    greeted = set()
    for srcport, dstport, data in read_segments(path):
        if dstport != HUB_PORT:
            continue                      # hub->machine copy, already counted
        buf = buffers[srcport]
        buf += data

        # A connection that OPENS inside the capture starts with a 5-byte greeting,
        # "RETH" plus a version byte, and it is NOT length-prefixed. Skip it once per
        # stream or the very first length is read out of the middle of it and every
        # frame after is nonsense. A machine already connected when the capture began
        # is joined mid-stream with no greeting in sight, which is why this only bites
        # on a capture where something connects - it bit here because our own node did.
        if srcport not in greeted:
            if len(buf) < 5:
                continue
            greeted.add(srcport)
            if buf[:4] == b"RETH":
                del buf[:5]

        while len(buf) >= 2:
            length = (buf[0] << 8) | buf[1]
            if length == 0 or length > 2000:
                raise ValueError("bad frame length %d on port %d" % (length, srcport))
            if len(buf) < 2 + length:
                break
            yield bytes(buf[2:2 + length])
            del buf[:2 + length]


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def main(path):
    print("%-6s %-6s %-11s %-7s %-7s %s"
          % ("from", "to", "subtype", "flags1", "flags2", "note"))

    # Per ORDERED PAIR (sender, receiver): the last flags1 that sender used to it.
    last = {}
    # Per SENDER, ignoring who it was talking to.
    last_any = {}
    gaps = 0
    total = 0

    for f in frames(path):
        if len(f) < 28 or f[14:17] != b"\xa8\xa8\x03":
            continue
        link = f[17:28]
        if link[2] != 0x20:               # only data frames carry a datagram
            continue
        dg = f[28:]
        if len(dg) < 14 or be16(dg, 0) != 0x2113:
            continue

        subtype = be16(dg, 2)
        dest = be16(dg, 4)
        src = be16(dg, 6)
        flags1 = be16(dg, 8)
        flags2 = be16(dg, 10)
        total += 1

        note = ""
        if flags1 == 0xFFFF:
            note = "RESYNC FORM (no sequence claimed)"
        elif subtype not in (0x03,):
            # An acknowledgement ECHOES the other side's number, so it says nothing
            # about the sender's own counter - only originated frames are tracked.
            key = (src, dest)
            previous = last.get(key)
            if previous is not None:
                step = (flags1 - previous) & 0xFFFF
                if step != 1:
                    note = "step %+d from %04X   <-- NOT +1" % (step, previous)
                    gaps += 1
            last[key] = flags1

            any_previous = last_any.get(src)
            if any_previous is not None and any_previous != previous:
                note += "  [this sender last used %04X to ANY peer]" % any_previous
            last_any[src] = flags1

        print("%-6s %-6s %-11s %-7s %-7s %s"
              % (src, dest, SUBTYPE.get(subtype, "0x%02X" % subtype),
                 "%04X" % flags1, "%04X" % flags2, note))

    print()
    print("%d datagrams, %d places where a sender's number did not step by +1" % (total, gaps))
    print()
    print("Final number each sender used, per PEER:")
    for (src, dest), value in sorted(last.items()):
        print("   %s -> %s : %04X" % (src, dest, value))
    print()
    print("If a sender's number continues across DIFFERENT peers, the counter is per")
    print("MACHINE. If each pair has its own run, it is per PEER. That is the whole")
    print("point of running three machines instead of two.")


if __name__ == "__main__":
    main(sys.argv[1])
