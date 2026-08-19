"""How many datagrams will D100 take from us before it must accept one?

The push dies in a XENSE storm ("your sequence is ahead"). Our own logs showed the refused
datagram going out while an EARLIER one was still unacknowledged, which says the failure is
pacing rather than numbering - but a log cannot show whether a datagram actually left, nor the
order D100 saw things in. This reads the hub capture and answers, for every reject:

    at the moment D100 refused Flags 1 X, how many datagrams had we sent that it had not yet
    acknowledged?

The smallest such count over a whole run is an UPPER BOUND on what D100 tolerates; the largest
count that was never refused is a LOWER BOUND. Print both and let the numbers decide - do not
guess a window, which is exactly how the ND link send window was got wrong three times.

Frame layout, outermost first (same as seqtrace.py, which this borrows its reader from):
    2-byte length | 802.3 | LLC A8 A8 03 | 11-byte ND link | 14-byte SINTRAN datagram
The datagram is: 2113 | subtype | dest | src | flags1 | flags2 | checksum

Usage:  python window.py push-window.pcapng [our-node]
"""
import collections
import subprocess
import sys

TSHARK = r"C:\Program Files\Wireshark\tshark.exe"
HUB_PORT = 5010
OUR_NODE = 19999

SUBTYPE = {
    0x03: "Ack", 0x07: "NetErr", 0x0A: "Frag1", 0x0C: "FragC",
    0x0E: "Data", 0x13: "ReachReply", 0x19: "ReachReq",
}
XENSE = 0xFFDE


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
    """Yield each Ethernet frame once, in capture order (machine->hub direction only)."""
    buffers = collections.defaultdict(bytearray)
    greeted = set()
    for srcport, dstport, data in read_segments(path):
        if dstport != HUB_PORT:
            continue                      # the hub's fan-out copy, already counted
        buf = buffers[srcport]
        buf += data

        # A connection opening inside the capture starts with a 5-byte "RETH" greeting that
        # is NOT length-prefixed - skip it once per stream or every later length is garbage.
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


def main(path, ours):
    # Flags 1 values we have sent and that have not yet been acknowledged, in send order.
    outstanding = []
    # For each reject, how many were outstanding (counting the refused one itself).
    reject_depths = []
    # The largest outstanding count that was reached WITHOUT drawing a reject.
    deepest_accepted = 0
    sent = acked = rejected = 0

    print("%-5s %-7s %-11s %-7s %s" % ("dir", "flags1", "subtype", "flags2", "note"))

    for f in frames(path):
        if len(f) < 28 or f[14:17] != b"\xa8\xa8\x03":
            continue
        if f[19] != 0x20:                 # only ND link data frames carry a datagram
            continue
        dg = f[28:]
        if len(dg) < 14 or be16(dg, 0) != 0x2113:
            continue

        subtype = be16(dg, 2)
        dest = be16(dg, 4)
        src = be16(dg, 6)
        flags1 = be16(dg, 8)
        flags2 = be16(dg, 10)
        name = SUBTYPE.get(subtype, "0x%02X" % subtype)

        if src == ours and subtype not in (0x03,) and flags1 != 0xFFFF:
            # One of ours, originated. It is now in flight.
            outstanding.append(flags1)
            sent += 1
            if len(outstanding) > deepest_accepted:
                deepest_accepted = len(outstanding)
            print("%-5s %-7s %-11s %-7s %d in flight"
                  % ("we->", "%04X" % flags1, name, "%04X" % flags2, len(outstanding)))
            continue

        if dest == ours and subtype == 0x03:
            # D100 acknowledged one of ours: it and everything before it are taken.
            acked += 1
            if flags1 in outstanding:
                cut = outstanding.index(flags1) + 1
                del outstanding[:cut]
            print("%-5s %-7s %-11s %-7s %d still in flight"
                  % ("<-ack", "%04X" % flags1, name, "%04X" % flags2, len(outstanding)))
            continue

        if dest == ours and subtype == 0x07:
            rejected += 1
            depth = len(outstanding)
            note = "XENSE" if flags2 == XENSE else "error %04X" % flags2
            if flags1 in outstanding:
                # Everything from the refused one onward is dead; D100 took none of it.
                depth = outstanding.index(flags1) + 1
                del outstanding[outstanding.index(flags1):]
            reject_depths.append(depth)
            print("%-5s %-7s %-11s %-7s %s REFUSED - it was %d deep in flight"
                  % ("<-REJ", "%04X" % flags1, name, "%04X" % flags2, note, depth))
            continue

    print()
    print("%d datagrams sent by us, %d acknowledged, %d refused" % (sent, acked, rejected))
    if reject_depths:
        print("shallowest refusal: %d datagram(s) in flight" % min(reject_depths))
        print("deepest in flight at any point: %d" % deepest_accepted)
        print()
        print("So D100 tolerates at most %d unacknowledged datagram(s) from us before it"
              % (min(reject_depths) - 1))
        print("calls the next one ahead - IF every refusal has the same cause.")
    else:
        print("nothing was refused in this capture")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        raise SystemExit(2)
    main(sys.argv[1], int(sys.argv[2]) if len(sys.argv) > 2 else OUR_NODE)
