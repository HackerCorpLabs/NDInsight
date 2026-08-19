"""How many of OUR link frames were outstanding when D100 retransmitted?

Reads the runner log and, for every moment, tracks:
  - the highest data frame WE sent            (our send sequence)
  - the highest frame D100 has acknowledged   (its "next expected")
The difference is how much we had in flight. If D100 retransmits only while
that number is above some value, the link window is the root cause.

It also flags every frame D100 sends TWICE (identical link sequence), which is
what a retransmission looks like on this link.
"""
import re
import sys

TX_DATA = re.compile(r"^(\S+ \S+) \| \[tx\] node19999 -> node100  data seq=(\d+)")
RX_ACK = re.compile(r"^(\S+ \S+) \| \[sniff\] node100 -> node19999  ack seq=(\d+)")
RX_DATA = re.compile(r"^(\S+ \S+) \| \[sniff\] node100 -> node19999  data seq=(\d+)")


def main(path):
    ourhigh = -1
    theirack = 0
    seen = {}
    print("%-24s %-28s %s" % ("time", "event", "in flight (ours unacked)"))
    with open(path, "r", errors="replace") as fh:
        for line in fh:
            m = TX_DATA.match(line)
            if m:
                ourhigh = int(m.group(2))
                continue
            m = RX_ACK.match(line)
            if m:
                theirack = int(m.group(2))
                continue
            m = RX_DATA.match(line)
            if m:
                ts, seq = m.group(1), int(m.group(2))
                flight = ourhigh + 1 - theirack
                if seq in seen:
                    print("%-24s %-28s %d   (first seen %s)"
                          % (ts, "REPEAT of their seq %d" % seq, flight, seen[seq]))
                else:
                    seen[seq] = ts
                    print("%-24s %-28s %d" % (ts, "new their seq %d" % seq, flight))


if __name__ == "__main__":
    main(sys.argv[1])
