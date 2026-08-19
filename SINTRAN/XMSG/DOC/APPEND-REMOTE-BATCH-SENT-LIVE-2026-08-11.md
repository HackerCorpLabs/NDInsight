# APPEND-REMOTE-BATCH: one of ours went out, and D100 answered

Date: 2026-08-11. Our node D19999 to D100 over the COSMOS Ethernet segment (hub on TCP 5010).

---

## What was already true, and what was not

The request bytes have matched a live capture for a while: `XftraRequests.AppendRemoteBatch`
is built from the 2026-07-31 capture of node 102 to node 100, and a test compares our bytes to
the captured parameter area byte for byte.

What had never happened is one of OURS leaving a socket. A builder that matches a capture proves
the shape. It does not prove a real machine accepts the same bytes from us.

## What was sent

> **Do not copy this command line.** It carries `--announce-restart`, which was in every
> recipe at the time and is now known to make the peer refuse the conversation (`XRDDF`,
> "Another port already has this name"). The run below is kept as the record of what was
> actually run. Today the same run is made WITHOUT that flag.

```
Xmsg.Live.Runner --config topology-d19999.json --request-link --announce-restart --resync-hard \
                 --append-batch ARBTEST:SYMB --append-batch-out ARBOUT:SYMB 127.0.0.1 10364 19999 75
```

One XSLET letter to XROUT's well-known port 0, addressed to the name `*XFTRA`:

```
1B 41 0044 FF06 2A5846545241 FE04 44313030 F406 53595354454D
0D02 0000 F80C 41524254455354 3A53594D42 F704 53594D42
0A02 0400 0B02 0003 F00B 4152424F55543A53594D42 00
```

Reading it: serial `1B`, service `41` (XSLET), declared length `0044`, then the tagged parameters -
server name `*XFTRA`, system `D100`, user `SYSTEM`, p13 integer 0 (no password), p8 string
`ARBTEST:SYMB` (the batch INPUT file), p9 `SYMB`, p10 integer `0400`, **p11 integer 3 =
APPEND-REMOTE-BATCH**, p16 string `ARBOUT:SYMB` (the batch output file).

## What came back

MEASURED, from the trace with `--trace-frames`:

```
[arb] answer-ish frame from node 100: subtype=Ack  Flags1=0x0016 Flags2=0x0001
[RX] 100->19999 sub=Data f1=0x0000
     info=2113000E4E1F0064000000489013 2100 8600 4E1F0211 4E1F0211 0048 1B 16 0004 01 02 002E
```

Two separate things, and both matter:

 - **D100 ACKNOWLEDGED the letter**, echoing the Flags 1 we sent it on. The datagram was accepted.
 - **D100 then sent a Data frame back**, and its body **echoes our serial `1B`**. That is what
   makes it an answer to OUR request rather than traffic that happened to arrive.

The reply body is `1B 16 0004 01 02 002E`: serial `1B`, a reply service byte `16`, a declared
length of 4, and one tagged parameter - **integer parameter 1, value `0x002E` = 46**.

`XroutError` 46 is `XRNCO`, "No connection to this system".

## What this establishes, and what it does not

**ESTABLISHED:** our APPEND-REMOTE-BATCH letter is accepted by a real SINTRAN machine, routed by
its XROUT to a server, and ANSWERED, with our serial echoed so the answer can be matched to the
request. That is the whole path working, and it is the same class of outcome as the original
capture, where two real machines ended in `NO SUCH FILE NAME` from the far side. A named refusal
proves the letter arrived, was understood and was acted on. Silence is the failure to fear.

**NOT ESTABLISHED - do not write these down as facts:**

 - That the reply service byte `16` means what an XROUT reply service byte generally means. No
   capture in the corpus has been read for it. It is a number we observed once.
 - That parameter 1 in an `*XFTRA` reply is always the status. It carried a number that happens
   to match an `XroutError`, which is suggestive and is not proof.
 - WHY the answer is `XRNCO`. The letter names `D100` as the remote system while being sent TO
   D100, which is how the captured request was addressed too, but what D100 then tries to connect
   to has not been traced. Finding out means capturing D100's own outgoing traffic after it takes
   the letter, not reasoning about it here.

The reply also repeats - D100 re-sends it - which says our answer to it is not what it wants.
What the correct answer is has never been captured, and is not guessed at.
