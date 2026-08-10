"""
Decode an xmsghub (TCP 5010) capture into FA messages, one per line.

The hub is a BROADCAST hub: every frame is forwarded to every other member, so a
frame sent once appears on several TCP streams. To count each frame exactly once we
keep ONLY the machine->hub direction of each stream, which is the frame's originator.
That is what the source port tells us - each emulator holds its own connection.

Framing, per DOC/COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md:
    [2 bytes BE length][ethernet frame]
    frame: dstMAC(6) srcMAC(6) len802.3(2) LLC(3: A8 A8 03) ndlink(11) payload
    payload: SINTRAN header (14 bytes) [+ XMSG sub-header (14) + body]
"""
import subprocess
import sys
import collections

TSHARK = r"C:\Program Files\Wireshark\tshark.exe"

HUB_PORT = 5010


def read_segments(path):
    """Yield (srcport, dstport, payload_bytes) for every TCP segment carrying data."""
    out = subprocess.run(
        [TSHARK, "-r", path, "-Y", "tcp.len>0", "-T", "fields",
         "-e", "tcp.srcport", "-e", "tcp.dstport", "-e", "tcp.payload"],
        capture_output=True, text=True, check=True)
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) < 3 or not parts[2]:
            continue
        # tshark can emit several payload fields comma-joined when a frame holds
        # more than one TCP segment; join them back in order.
        hexed = parts[2].replace(":", "").replace(",", "")
        yield int(parts[0]), int(parts[1]), bytes.fromhex(hexed)


def reassemble(path):
    """Yield ethernet frames in capture order, each frame exactly once.

    Only the machine->hub direction is kept. That is the frame's ORIGINATOR; the
    copies the hub fans out to the other members are the same bytes again and
    would multiply every count by the member count.
    """
    buffers = collections.defaultdict(bytearray)
    for srcport, dstport, data in read_segments(path):
        if dstport != HUB_PORT:
            continue          # hub->machine direction: a copy, already counted
        buf = buffers[srcport]
        buf += data
        while len(buf) >= 2:
            length = (buf[0] << 8) | buf[1]
            if length == 0 or length > 2000:
                # Not a length we can trust - the stream is out of step. Say so
                # loudly rather than silently resyncing on a guess.
                raise ValueError("bad frame length %d on port %d" % (length, srcport))
            if len(buf) < 2 + length:
                break
            yield bytes(buf[2:2 + length])
            del buf[:2 + length]


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


LLC_OFFSET = 12          # after the two MACs
NDLINK_OFFSET = 17       # after 802.3 length (2) + LLC (3)
NDLINK_LENGTH = 11
PAYLOAD_OFFSET = NDLINK_OFFSET + NDLINK_LENGTH


def sender_of(frame):
    """Name the sending machine from its source MAC.

    Per ND-60.197.01 section 2.4 the MAC is 08 00 26 | system number in REVERSED
    byte order | physical user. So bytes 3-4 are the system number little-endian,
    which is the opposite order to the same number in the SINTRAN header.
    """
    system = frame[9] | (frame[10] << 8)
    return "D%d" % system


def frames(path):
    """Yield decoded data frames carrying a SINTRAN payload."""
    for f in reassemble(path):
        sender = sender_of(f)
        if len(f) < PAYLOAD_OFFSET:
            continue
        if f[14:17] != b"\xa8\xa8\x03":
            continue                      # not an ND/COSMOS LLC frame
        kind = f[NDLINK_OFFSET + 2]
        plen = be16(f, NDLINK_OFFSET + 9)
        if kind != 0x20 or plen == 0:
            continue                      # only DT frames carry a payload
        payload = f[PAYLOAD_OFFSET:PAYLOAD_OFFSET + plen]
        if len(payload) < 14:
            continue
        yield sender, payload


def main(path):
    n = 0
    for sender, p in frames(path):
        marker = be16(p, 0)
        typesub = be16(p, 2)
        dst = be16(p, 4)
        src = be16(p, 6)
        flags1 = be16(p, 8)
        flags2 = be16(p, 10)
        body = p[14:]
        print("%-5s %04x %04x dst=%-5d src=%-5d f1=%04x f2=%04x len=%-4d %s"
              % (sender, marker, typesub, dst, src, flags1, flags2, len(body),
                 body[:48].hex()))
        n += 1
    print("--- %d data frames ---" % n, file=sys.stderr)


if __name__ == "__main__":
    main(sys.argv[1])
