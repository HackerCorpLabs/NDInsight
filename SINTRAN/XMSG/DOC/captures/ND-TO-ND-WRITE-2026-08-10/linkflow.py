"""
Measure the ND LINK layer, not the SINTRAN layer.

The link tears down with kind 0x6F, so the question is what the link was doing at
that moment - how many of our DT frames were outstanding, and what the link
sequence numbers were.

ND link header (11 bytes, after LLC A8 A8 03):
    +0 length(11)  +1 const 0x02  +2 kind  +3 const 0  +4 send sequence
    +5..6 sender link id  +7..8 receiver link id  +9..10 plen / other
Kinds: 0x0F CR, 0x1F CC, 0x20 DT, 0x3F AK, 0x6F DR.
"""
import subprocess
import sys
import collections

TSHARK = r"C:\Program Files\Wireshark\tshark.exe"
HUB_PORT = 5010


def raw_frames(path):
    """Yield (sender_system, kind, linkseq, plen, frame) for every ND frame."""
    out = subprocess.run(
        [TSHARK, "-r", path, "-Y", "tcp.len>0", "-T", "fields",
         "-e", "tcp.srcport", "-e", "tcp.dstport", "-e", "tcp.payload"],
        capture_output=True, text=True, check=True)
    buffers = collections.defaultdict(bytearray)
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) < 3 or not parts[2]:
            continue
        if int(parts[1]) != HUB_PORT:
            continue
        buf = buffers[parts[0]]
        buf += bytes.fromhex(parts[2].replace(":", "").replace(",", ""))
        if len(buf) >= 5 and buf[:4] == b"RETH":
            del buf[:5]
        while len(buf) >= 2:
            length = (buf[0] << 8) | buf[1]
            if length == 0 or length > 2000 or len(buf) < 2 + length:
                break
            f = bytes(buf[2:2 + length])
            del buf[:2 + length]
            if len(f) < 28 or f[14:17] != b"\xa8\xa8\x03":
                continue
            sender = f[9] | (f[10] << 8)
            kind = f[19]
            linkseq = f[21]
            plen = (f[26] << 8) | f[27]
            yield sender, kind, linkseq, plen, f


def main(path, us):
    events = list(raw_frames(path))

    counts = collections.Counter()
    for sender, kind, seq, plen, f in events:
        counts[(sender, kind)] += 1
    print("frame kinds by sender:")
    for key in sorted(counts):
        name = {0x0F: "CR", 0x1F: "CC", 0x20: "DT", 0x3F: "AK", 0x6F: "DR"}.get(key[1], "?")
        print("   node %-6d kind 0x%02X %-3s %d" % (key[0], key[1], name, counts[key]))
    print()

    # Walk the link: our DT frames out, their AK frames back. An AK carries the NEXT
    # expected sequence, so it clears everything before it.
    outstanding = []
    peak = 0
    peak_at = None
    history = []
    stop_index = None
    for i in range(len(events)):
        sender, kind, seq, plen, f = events[i]
        if kind == 0x6F and stop_index is None:
            stop_index = i
        if sender == us and kind == 0x20:
            outstanding.append(seq)
            if len(outstanding) > peak:
                peak = len(outstanding)
                peak_at = i
        elif sender != us and kind == 0x3F:
            # AK's sequence is the next expected: drop everything up to it.
            outstanding = [s for s in outstanding if s >= seq]
        history.append((i, len(outstanding)))

    print("peak of OUR DT frames outstanding at the link layer: %d" % peak)
    if stop_index is not None:
        depth = dict(history).get(stop_index, 0)
        print("outstanding when the first DR (0x6F) arrived: %d" % depth)
    print("still outstanding at the end: %d" % len(outstanding))


if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]))
