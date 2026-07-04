# Question for the GOD LLM - XSGSY reply XENSE and connect-stall on a climbed 100

## Context

Our C# node is 102 (d102). Node 100 has NOT restarted XMSG across several failed sessions, so it
keeps its datagram sequence climbing (connects arrive at Flags1 0x0008/0x0009, no fresh
ReachabilityRequest, and at startup 100 is still retransmitting the previous session's DCON).
Two separate failures now appear. We suspect both are 100-expected-from-us sequence mismatches
caused by the climbed state, but we cannot confirm the correct reply-sequence model. Recently
fixed and VERIFIED separately: the LAPB odd-info-length address (0x89/0x09), the T1 retransmit
tick, and a T3 keepalive flood. Those are not in question here.

All bytes below are from the runner log (both directions).

## Failure 1 - list-route (XSGSY) reply is XENSE-rejected, li-rout hangs

100 sends an XSGSY request (XMCSM 0x0100014B):
```
rx 100->102  2113000E 0066 0064 0009 0100 DD 0B  210086 C4  0066 0000 0064 02A0  0100014B  0004  01020066
             (Flags1=0x0009 counter=0x0B proto=DD role=0xC4  query system=0x0066=102)
```
We reply (XMCSM 0x01000100), echoing the request Flags1 and counter:
```
tx 102->100  2113000E 0064 0066 0009 0100 DD 0B  210086 60  0064 02A0 0064 02A0  01000100  0010  01020066 02020004 03020066 04020000
             (Flags1=0x0009 counter=0x0B proto=DD role=0x60  4 param blocks)
```
100 rejects with subtype 0x07 (NetworkError):
```
rx 100->102  2113 0007 0066 0064 0009 FFDE DE 33
             (subtype=0x07  Flags1=0x0009  Flags2=0xFFDE = -34 = XENSE sequence error  proto=DE)
```
Then 100 re-sends the identical request ~20s later, we reply the same way, XENSE again - infinite
loop, so `li-rout` on 100 hangs.

Our reply echoes the request's Flags1 (0x0009) and counter (0x0B) - this echo was previously
VERIFIED live against a FRESH 100. Questions:
1. When 100 is at a climbed expected-from-us sequence, is echoing the request Flags1/counter still
   correct, or must the XSGSY reply carry OUR own outgoing sequence (100's expected-from-us)?
2. What Flags1/counter/channel does 100 actually expect for the XSGSY reply here? XENSE (-34) says
   the sequence is wrong; what is right?
3. Our reply rides proto DD (0xDD, derived by the seed model). The stateless ListRoutingServer
   default is DC (0xDC). Which channel is correct for a list-route reply, and does it depend on
   epoch/climbed sequence?
4. The doc notes list-route = TWO datagrams (route-table + liveness). We send ONE. Is the missing
   second datagram part of why 100 XENSEs, or unrelated?

## Failure 2 - connect accepted but 100 never drives session-setup

Fresh runner, 100 connects at a climbed Flags1 (0x0008), no ReachabilityRequest:
```
rx 100->102 connect letter  ...0008 0400 DA 0C  role=0xE4  XMCSM=0x04000041  ("*TADADM" / "D102")
tx 102->100 secure/route ack ...0008 0001 DE 16     (subtype 03, echoes Flags1 0x0008, counter 0x16 = request 0x0C + 0x0A)
tx 102->100 connect-accept   ...0000 0400 DA 14     (OUR Flags1 0x0000, counter 0x14 = seed, channel DA/epoch 0)
rx 100->102 RR nr=2                                  (LAPB acks both)
```
Then 100 goes silent - it never sends the datagram ACK for our accept and never drives the
session-setup (in a working FRESH run at Flags1 0x0000 it acked our accept and sent session-setup
immediately). Our accept is byte-identical to the working run's except the session-specific
Flags1/counter/ports. Questions:
5. When 100 reconnects at a climbed Flags1 (0x0008) without restarting XMSG, is our accept at OUR
   Flags1 0x0000 / counter 0x14 (seed, epoch 0) correct, or does 100 expect the accept to align
   with its climbed expected-from-us? We deliberately start our responder at Flags1 0x0000 per the
   epoch-1 crash fix.
6. 100 was still retransmitting the PREVIOUS session's DCON (Flags1 0x0007) at link bring-up, and
   we never acked it (arrived mid-SABM-storm). Does an un-acked prior-session DCON leave 100's TAD
   half-open so it ignores the new accept? Should our responder ACK a stray DCON for an unknown
   session to let 100 finish the teardown before the new connect?

## What we need back

The correct outgoing-sequence rule for a responder answering a 100 that has a CLIMBED
expected-from-us (both the XSGSY reply and the connect-accept), and whether Failures 1 and 2 are
(a) real bugs in our reply sequencing, or (b) artifacts that only a fresh XMSG restart on 100
clears. If (b), say so plainly so we stop chasing them. Mark inferred vs capture-verified.
