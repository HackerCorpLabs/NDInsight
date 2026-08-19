# What the datagram sequence does between two real ND machines

Full path: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\captures\TRIO-SEQUENCE-2026-08-11\FINDINGS.md`

Measured 2026-08-11. Three real SINTRAN machines were brought up (D100, D102, D103) and
several file listings were run back to back with **nothing restarted between them** - which is
the only way to see what a counter does across conversations.

Capture: `trio.pcapng`, 38410 frames, 2410 SINTRAN datagrams.
Reproduce: `python seqtrace.py trio.pcapng`

---

## 1. Flags 1 is PER SENDER. Counted, not eyeballed.

| | |
|---|---|
| Data frames D100 sent | **602** |
| Data frames D102 sent | **602** |
| Distinct Flags 1 values used | **602** (0x0000 .. 0x0259) |
| Total data frames on the wire | 1204 |

If the two machines shared one counter, 1204 frames would have consumed about 1204 values.
They consumed 602. So each machine ran its own counter 0..601 and sent exactly one frame per
value.

**They look shared and are not.** The exchange is perfectly symmetric - one data frame each
per exchange - so the two counters stay level for the whole capture:

```
100 -> 102  Data 0000      102 -> 100  Ack  0000
102 -> 100  Data 0000      100 -> 102  Ack  0000
100 -> 102  Data 0001      102 -> 100  Ack  0001
102 -> 100  Data 0001      100 -> 102  Ack  0001
```

Reading that ladder alone, "one shared counter that steps per exchange" fits every line. The
frame COUNT is what separates the two models, and it is the check
`xmsg-flags1-is-per-sender-not-shared` asked for. That memory is CONFIRMED, not overturned.

An acknowledgement carries the number of the data frame it answers, so acks say nothing about
the sender's own counter and are excluded from the count above.

## 2. Nothing resets it across conversations

Three separate `LIST-FILES` conversations ran in this capture. The counter climbed straight
through all of them:

| event | Flags 1 |
|---|---|
| link comes up, reachability exchange | `FFFF` both ways |
| first datagram after it | `0000` |
| end of conversation 1 | `0091` |
| end of conversation 2 | `0123` |
| end of conversation 3 | `01C7` |
| end of capture | `0259` |

No restart, no wrap, no gap. **A conversation ending does not reset anything.** The number is
a property of the LINK, not of the conversation.

## 3. It starts at zero only after a reachability exchange

The whole capture contains exactly **two** `FFFF` frames, both at the very start:

```
100 -> 102  ReachReq    FFFF
102 -> 100  ReachReply  FFFF
100 -> 102  Data        0000     <- counting starts here
```

`FFFF` is the "I am not claiming a sequence" form. The kernel carve agrees: the send path at
`ram:b3d6` picks between `XMSEQ = 0xFFFF` and `XMSEQ = XNSEQ | 0x8000` on a flag, and `XNSEQ`
has exactly one writer - an increment - and no reset anywhere in the image. So zeroing is not
something a message does; it is what a fresh link block starts as.

## 4. The only anomalies are the four teardown rejects

`seqtrace.py` flags every place a sender's number fails to step by +1. In 2410 datagrams there
are four, all identical in shape:

```
0091 FFED   0123 FFED   01C7 FFED   0259 FFED
```

One per conversation teardown - a subtype-07 network error carrying `0xFFED` = XEIMA. Already
established as NORMAL (see `FaServerConversation.BuildClose`): a real server's close gets the
same reject from a real client. They repeat the previous number rather than advancing because
an error frame answers the frame that offended it.

---

## What this means for D19999

Our node's problem was never the file-access protocol. D100 keeps a per-sender expectation for
each peer, it starts at zero only when the link block is created, and **nothing in normal
traffic ever puts it back**. When our runner restarts and opens at `0x0000`, D100 is still
counting from wherever the previous run left it, and refuses us - silently, or with XENSE once
we announce.

So there are only two honest ways forward, and this capture rules the middle ground out:

 - **Persist our counter** across restarts, which is exactly what `xmsg-sequence.state` is for.
   Deleting it - which the old live-test protocol says to do after an XMSG restart - is right
   ONLY when the peer really did reset. On Ethernet it does not, so deleting the file is what
   puts us out of step.
 - **Make the link block be created again**, which is what a reachability exchange accompanies.
   Sending `FFFF` alone did not do it (measured), so what triggers the rebuild is still open.

## 5. ANSWERED: the counter is PER PEER as well as per sender

`threeway.pcapng` - D100 listing D19999 and then D102, in one session, nothing restarted.
D100 ran two separate counters at the same time:

| D100 -> peer | first | last | data frames | check |
|---|---|---|---|---|
| D102 | `0x0410` | `0x04A1` | 146 | 145 steps + 1 = 146 |
| D19999 | `0x000E` | `0x0025` | 24 | 23 steps + 1 = 24 |

Both runs are exact and they are nowhere near each other. If the counter belonged to the
MACHINE, the run toward D19999 would have carried on from `0x04A1`, not started near zero.

So: **one counter per (sender, peer) pair**, and it survives everything short of the peer
block being rebuilt.

The `0x0410` also continues exactly from `allpairs.pcapng`, which ended at `0x040F` - across a
separate capture, a twenty-minute gap, six conversations and a live `DEF-NETWORK-CONN`. Three
captures in a row, one unbroken run:

```
trio.pcapng      0x0000 .. 0x0259     (602 frames)
allpairs.pcapng  0x025A .. 0x040F     (438 frames)
threeway.pcapng  0x0410 .. 0x04A1     (146 frames)
```

## 6. Confirmed on a THIRD machine and a SECOND transport

D103 was brought into the trio over HDLC through our relay
(`--relay-listen 10366 --relay-inbound-node 103`) and listed D19999's files.
`hdlc-trio.pcapng`:

```
19999 -> 103   FFFF                  reachability
103 -> 19999   FFFF
103 -> 19999   0x0000 .. 0x0017      its own run, from zero
19999 -> 103   0x0000 .. 0x0017      ours, from zero
```

Same law on a different wire: a reachability exchange at link bring-up, then each side counting
its own run from zero. D103's counter toward D19999 started at zero while D19999 was already
mid-run toward D100 - a third independent (sender, peer) counter, and on HDLC rather than
Ethernet. **So the rule is not an artefact of the Ethernet path.**

## Why D103 could not use the segment - it has no ENNS0

Two separate faults, found in order:

 1. **Its XMSG kernel table area was never initialised.** `LIST-SYSTEMS` in X-C printed
    `XMSG kernel table area initialised.` - it created the tables at that moment. Every
    `DEF-REMOTE` from the earlier bring-up had landed on a table that did not exist and did
    nothing, which is why XROUT answered "Unknown name (of server or system)". Afterwards all
    six `DEF-REMOTE` returned `Ok`.
 2. **ENNS0 is not installed on D103 at all.** `LIST-RT-DESCRIPTION ENNS0` returns
    `ILLEGAL PARAMETER`, where the same command on D100 returns a real descriptor (`PASSIVE`,
    segment `101B`, start address `32241B`). So `START-NET-SERVER,ENNS0` and every
    `DEF-NETWORK-CONN` fail with "Error in communicating with XROUT" - there is no Ethernet
    network server to communicate with.

An earlier draft of this document said D100 was "presumably routing D103 over HDLC". **That was
wrong and is withdrawn**: D100 was not routing it anywhere. D103 has no Ethernet software.

The HDLC path needed `START-LINK,1360,,,-1,,` on BOTH ends. The bring-up script starts LU `1362`
by default; LU `1360` is the one on controller 1, which is the port the relay uses.
`START-LINK,1360` on its own is refused - the command prompts for a timeout, so the whole
parameter list is required.

Also unexplained: the third `D100 -> D102` listing returned **one** file where the first two
returned 67, with no error. Same command, same pair, nothing restarted. Worth its own run.
