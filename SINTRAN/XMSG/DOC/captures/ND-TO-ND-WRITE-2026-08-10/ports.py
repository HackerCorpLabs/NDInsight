"""Print the port pair of every FA connect letter and confirmation in a capture.

The question: when a real server answers a SECOND connect letter from the same client
port, does it answer from the SAME source port as the first conversation, or a new one?
Ours uses a new one (0x0211 then 0x0213) and the client ignores the second confirm.

Sub-header layout, read off a live [tx] line and confirmed against the runner's own log:
    [2100][8284][dest system][dest port][src system][src port][fa length]
"""
import sys
import decode_hub


def be16(b, at):
    return (b[at] << 8) | b[at + 1]


def main(path):
    print("%-5s %-14s %-9s %-9s %s"
          % ("from", "kind", "src port", "dst port", "fa bytes"))
    for sender, p in decode_hub.frames(path):
        body = p[14:]
        if len(body) < 16:
            continue
        sub = body[:14]
        fa = body[14:]
        kind = be16(fa, 0)
        if kind not in (0x07E2, 0x07D2, 0x07C0, 0x0782):
            # Connect letters arrive as a data message whose FA word is 0x1B41,
            # so catch those too - the letter's own type word sits further in.
            if kind != 0x1B41:
                continue
        names = {0x07E2: "ConnectLetter", 0x07D2: "ConnectConfirm",
                 0x07C0: "Close", 0x0782: "Release", 0x1B41: "letter/1b41"}
        print("%-5s %-14s 0x%04x    0x%04x    %s"
              % (sender, names[kind], be16(sub, 10), be16(sub, 6), fa[:24].hex()))


if __name__ == "__main__":
    main(sys.argv[1])
