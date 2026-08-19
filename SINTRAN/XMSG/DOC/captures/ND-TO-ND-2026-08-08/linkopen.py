"""How does a real ND machine open a conversation with a peer it has not heard from?

THE QUESTION. Our node cannot originate to a silent peer: the XMSG layer learns the peer's link
id from an inbound frame and never derives it, so with nothing received there is no receiver id to
put in the header. That is what stops the folder-watch daemon running unattended - it waits for
the ND to speak first, and a daemon that needs somebody to type a command on the far machine is
not a daemon.

Two real machines talk to each other in these captures, and one of them must go first. So the
answer is on the wire, and this prints it: for every ND link frame, the kind, the sequence, and
BOTH link ids - and it calls out the frames where the sender clearly did not yet know the peer's
id (receiver 0) so the opening move stands out from the conversation that follows.

ND link header, 11 bytes, at offset 17 of the Ethernet frame (after the 802.3 header and the
A8 A8 03 LLC):
    +2 kind (high nibble is the type)   +4 sequence
    +5..6 sender link id                +7..8 receiver link id
    +9..10 payload length

Usage:  python linkopen.py nd-to-nd.pcapng
"""
import collections
import subprocess
import sys

TSHARK = r"C:\Program Files\Wireshark\tshark.exe"
HUB_PORT = 5010

KIND = {0x0F: "ConnectRequest", 0x20: "Data", 0x3F: "Acknowledge", 0x6F: "DisconnectRequest"}


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
    """Yield each Ethernet frame once, in capture order.

    Works on both rigs. On the HUB every frame is fanned out to every other member, so only the
    machine->hub direction is kept or each frame is counted once per member. A point-to-point pair
    (RetroCore listen:PORT against tcp:host:PORT) has no fan-out, so BOTH directions are real
    originators and both are kept - dropping one there would hide exactly the machine that spoke
    first, which is the whole question here.
    """
    hub = HUB_PORT in ports_in(path)

    buffers = collections.defaultdict(bytearray)
    greeted = set()
    for srcport, dstport, data in read_segments(path):
        if hub and dstport != HUB_PORT:
            continue
        buf = buffers[srcport]
        buf += data

        # The 5-byte "RETH" greeting on a connection that opens inside the capture is not
        # length-prefixed; skip it once per stream or every later length is garbage.
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
            yield srcport, bytes(buf[2:2 + length])
            del buf[:2 + length]


def ports_in(path):
    """The set of TCP ports in the capture, to tell a hub rig from a point-to-point pair."""
    out = subprocess.run(
        [TSHARK, "-r", path, "-T", "fields", "-e", "tcp.srcport", "-e", "tcp.dstport"],
        capture_output=True, text=True, check=True)
    found = set()
    for line in out.stdout.splitlines():
        for part in line.split("	"):
            if part.isdigit():
                found.add(int(part))
    return found


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def main(path):
    print("%-6s %-18s %-4s %-8s %-8s %s"
          % ("port", "kind", "seq", "sender", "receiver", "note"))

    # The link ids each port has been seen USING, so a frame that names one it could not yet have
    # known stands out.
    seen_from = collections.defaultdict(set)
    openers = 0

    for srcport, f in frames(path):
        if len(f) < 28 or f[14:17] != b"\xa8\xa8\x03":
            continue

        link = f[17:28]
        kind = link[2]
        seq = link[4]
        sender = be16(link, 5)
        receiver = be16(link, 7)

        note = ""
        if receiver == 0:
            # Addressed to nobody in particular: this is somebody going first.
            note = "<-- RECEIVER 0: the sender did not know the peer's id"
            openers += 1
        elif receiver not in seen_from[srcport]:
            note = "first use of receiver %04X by this port" % receiver

        seen_from[srcport].add(sender)

        print("%-6s %-18s %-4s %-8s %-8s %s"
              % (srcport, KIND.get(kind, "0x%02X" % kind), seq,
                 "%04X" % sender, "%04X" % receiver, note))

    print()
    print("%d frame(s) carried receiver id 0" % openers)
    print()
    print("Link ids each port used as SENDER:")
    for port, ids in sorted(seen_from.items()):
        print("   %s : %s" % (port, ", ".join("%04X" % i for i in sorted(ids))))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        raise SystemExit(2)
    main(sys.argv[1])
