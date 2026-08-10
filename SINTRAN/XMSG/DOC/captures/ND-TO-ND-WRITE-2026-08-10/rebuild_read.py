"""
Rebuild a file from an FA READ capture, using the carved fragment rule.

Proves the framing in DOC/FA-READ-FRAMING-CARVED-2026-08-10.md: a content message is TWO
fragments sharing one Flags 1, and the CONTINUATION has no XMSG sub-header.

    python rebuild_read.py readback-10-blocks.pcapng 100 102 > out.bin
"""
import sys
import decode_hub


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def rebuild(path, server, client):
    """Return the file content the server sent, in order."""
    out = bytearray()
    seen = set()
    for sender, pl in decode_hub.frames(path):
        if be16(pl, 6) != server or be16(pl, 4) != client:
            continue
        subtype = be16(pl, 2) & 0xFF
        key = (be16(pl, 8), subtype, len(pl))
        if key in seen:
            continue                      # a retransmission, already taken
        seen.add(key)

        if subtype == 0x0A:
            # SINTRAN header 14 + XMSG sub-header 14, then the 8-byte FA envelope.
            fa = pl[28:]
            if len(fa) < 8 or be16(fa, 0) != 0x07F0:
                continue
            out += fa[8:]
        elif subtype == 0x0C:
            # NO XMSG sub-header on a continuation - content starts right after the
            # SINTRAN header. Taking pl[28:] here loses 14 bytes of the FILE.
            out += pl[14:]
    return out


if __name__ == "__main__":
    data = rebuild(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]))
    sys.stdout.buffer.write(data)
