# We FRMR the INNAK — captured 2026-08-08

**Files:** `frmr-on-innak.pcapng` (loopback, TCP 10362/10364/10366), `relay-run.log`.
**Capture:** `tshark -i \Device\NPF_Loopback -f "tcp port 10362 or tcp port 10364 or tcp port 10366"`,
started BEFORE the machines were touched. Decoded with the project dissector
`SINTRAN/Devices/HDLC/WireShark/hdlc_tcp.lua` (`-Y hdlc_lapb`).

**Setup:** D100 and D103 both rebuilt with the section 2.5 sequence, XMSG restarted on D100 first to
clear the wedged XROUT. Relay: `topology-d19999-relay.json`, listening 10366 for D103, dialling
10364 for D100.

## The finding: the Frame Reject is OURS

```
frame 381   10364 -> 28910   D100 to us
            7e 09 20 | 21FE 0017 4E1F 0064 FFFF FFFD 8F69 | b271 7e
            LAPB  I  N(S)=0 N(R)=1
            dissector: [Non-SINTRAN info (no 0x21 0x12/0x13 marker)]

frame 383   28910 -> 10364   us to D100
            7e 01 97 | 20 00 08 | aef2 7e
            LAPB  U  FRMR P/F      address 0x01 (link management), control 0x97
```

Port 28910 is OUR relay's outbound socket; 10364 is D100's HDLC 1 listener. Every `FRMR` in the
capture goes 28910 to 10364 — six of them, frames 367, 383, 387, 391, 395, 399. **Our stack rejects
the peer's frame at the LAPB layer, and the frame it rejects is the INNAK** (`21FE 0017`).

That is why D100 retransmits the same two Data datagrams every 40 seconds forever, and why D103's
`LIST-FRAMES` never shows anything from 100. Nothing was ever wrong with the peer.

## What this KILLS

**The Marker 2 theory is dead as the explanation for the drop.** The earlier suspicion was that
rewriting Marker 2 from `0x13` to `0x12` made D103 discard forwarded datagrams. This capture shows
the traffic never gets far enough for that to matter: it is rejected below XMSG, on the D100 leg,
before any relaying decision. Whether `0x12` is right remains OPEN and UNTESTED - it is simply not
the cause of what we were chasing.

**"D103 silently discards our forwarded frames" was also wrong.** In this run the relay logged NO
`hdlc-out -> hdlc-in for node 103` lines at all. Reading the Data frames explains why: their SINTRAN
destination is `4E1F` = 19999, US, not 103. D100 is not asking us to relay to 103 - it is asking US
a question about 103, and `DatagramRelay` correctly counts those as `DatagramsForUs`. The single
frame we answer with is evidently not what it wants, so it asks again.

## FRMR info bytes

`20 00 08` - the rejected control byte `0x20` (I-frame, N(S)=0, N(R)=1) followed by two bytes whose
W/X/Y/Z reason bits are NOT yet decoded against the ND LAPB spec. **Do not guess which reason bit
this is** - read section 2.2 of the spec against these three bytes.

The suspicious detail to start from: at frame 365 D100 sends `RR N(R)=0` and, in the same TCP
segment, `I N(S)=0 N(R)=1`. An N(R) that moves 0 then 1 within one segment is what our layer appears
to object to. UNVERIFIED.

## Also visible, and probably a second defect

At 67.99 D100's SABM arrives SIX TIMES in one TCP segment (frame 337 decodes as six `SABM P/F` plus
a `UA`), and we answer every one with its own `UA` + `RR` - twelve frames back. Batched retries in a
single segment are normal on a TCP-carried link; answering each individually is a burst that no real
line would produce. NOT investigated.

## ROOT CAUSE (same day, from the spec - not guessed)

`20 00 08`: the rejected control byte `0x20`, then reason **Z = `0x08`**, "N(R) invalid (outside
`[V(A), V(S)]`)" - spec table at `lapb-nd-spec.md:204`, and our own `LapbFrmrReason` enum agrees.

The normative spec is
WSL `/home/ronny/repos/os/x25emu/docs/lapb-nd-spec.md`. Two rules decide this, and **we obey both**:

 - **3.2** - on EVERY SABM received, including mid-session while CONNECTED, hard-zero
   `V(S) = V(A) = V(R) = 0` and clear the retransmit queue. No sequence adoption. MUST.
 - **4.3** (line 276) - a received `N(R)` MUST lie within `[V(A), V(S)]`; outside that, answer
   FRMR reason Z. MUST.

Now the capture's own timeline:

```
335  us -> D100   SABM
337  D100 -> us   SIX SABM P/F frames + a UA, ALL IN ONE 100-byte TCP segment
339  us -> D100   UA                      (answering SABM #1)
341  us -> D100   RR N(R)=0
343  us -> D100   I N(S)=0 N(R)=0         our announce  -> V(S) = 1
345..363         UA + RR, five more pairs (answering SABM #2..#6)
                                          -> each one hard-zeroes V(S) back to 0
365  D100 -> us   RR N(R)=0 | I N(S)=0 N(R)=1   D100 acks our announce, correctly
367  us -> D100   FRMR(Z)                 N(R)=1 is outside [0,0]
```

**We transmitted an I-frame in the middle of a burst of resets.** The announce went out after
SABM #1, then SABM #2..#6 each reset `V(S)` to 0 as the spec demands. D100 - which never reset
its own view of our announce - then acknowledged it with `N(R)=1`, and by then `[V(A), V(S)]` was
`[0, 0]`, so the ack was out of range and 4.3 forced the FRMR. Every individual step is
conformant; the ORDER is the defect.

**Why six SABMs arrive at once.** D100's HDLC output is queued by RetroCore while no TCP peer is
attached, and flushed when one connects. Its T1 retries piled up during the seconds our listener
was down, then landed in a single segment. **LEADING EXPLANATION, NOT CONFIRMED** - it fits that
loopback TCP cannot carry anything before the connection exists, and that six retries cannot be
generated in the milliseconds the segment spans. Confirm before relying on it.

**Where the fix belongs.** Not in the FRMR path and not in the reset rule - both are required.
The candidate is: do not announce until the inbound backlog is drained, so no I-frame is emitted
between two resets. That is a change to when the runner announces, or to when the layer reports
Active. NOT YET IMPLEMENTED - it needs a decision and a regression test built from this capture.

## Next

1. Decide and implement the drain-before-announce fix; pin it with a test that replays this exact
   sequence (SABM, our I-frame, five more SABMs, peer I with N(R)=1) and asserts no FRMR.
2. Only then re-run and see what D100 actually asks about 103.

Related: `DOC/SUBTYPE-17-INIT-REJECT-2026-08-07.md` steps 7-10.
