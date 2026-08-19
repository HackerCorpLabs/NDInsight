# The reply that was never sent: a queued frame looks exactly like a delivered one

**Date:** 2026-08-17
**Found:** live, against D100 over the COSMOS Ethernet segment, trying to close #34
**Status:** #34 is NOT closed - the two-user run has not been made. Both items of the fix ARE done
and tested; the DR is explained. The next live attempt is the thing still owed.

---

## What was being attempted

The live two-user chat run. A terminal user on a real SINTRAN connects to our node with
`CONNECT-TO D19999`, logs in at our TAD prompt, and types `chat join`. Two of those in the same
room is what #34 has always meant.

It never got as far as the login prompt.

---

## What happens

`CONNECT-TO D19999` on D100 prints its banner and then sits there. After about forty seconds it
gives up and retries, and each retry opens a NEW session on our side - `tty1`, then `tty2`.

Our side looks healthy, which is the whole problem:

```
[tad] session opened: tty1 from node 100
[RX] 100->19999 sub=Data answered with 2 frame(s)
```

The connect was decoded, a session was opened, and two reply frames were built. D100 saw none of
them.

---

## The chain, each step from the trace

The frame D100 sends carries `2A 54 41 44 41 44 4D` (`*TADADM`) and `44 31 39 39 39 39` (`D19999`),
so the request itself is well formed and we read it correctly.

With `--trace-frames` on, the whole window is:

```
01:16:10.984  [sniff] node100 -> node19999  kind0x0F   (CR, connection request)
01:16:11.021  [tx]    node19999 -> node100  kind0x1F   (our connection confirm)
01:16:11.040  [sniff] node100 -> node19999  data seq=0 (the TADADM connect)
01:16:11.093  [tx]    node19999 -> node100  ack seq=1
01:16:11.150  [tad] session opened: tty1 from node 100
01:16:11.165  [RX] ... answered with 2 frame(s)
              <- nothing further is transmitted, ever
```

Our last transmission is the ack at `.093`. The tracer is demonstrably working - it logged our
`kind0x1F` and our `ack` in the same second - so the two reply frames genuinely never reached the
wire.

They were not refused either. The runner has a diagnostic for exactly that case,
`[TX] *** DROPPED *** ... refused a NNNB reply`, added because "a dropped reply hangs the remote
SINTRAN terminal". **It did not fire once.** Zero refusals in the whole run.

So the frames were built, handed down, accepted, and never sent. Reading down the path:

| Layer | What it did |
|---|---|
| `XmsgLayer` | called `_codec.SendPacket` for each response BEFORE logging the count, so both were handed down |
| `XmsgCodec.SendPacket` | serialised and called `Transport.Send` |
| `LinkXmsgTransport.Send` | called `_link.SendData`; a false would have raised `SendRefused`, which never fired |
| `EthernetLink.SendData` | logs and returns false when the link is not Active. No such line: the link WAS Active |
| `NdLinkLayer.SendDatagram` | **returned true without transmitting** |

---

## The defect

`SendDatagram` returns `true` for two completely different outcomes:

```csharp
if (OutstandingFrames < CurrentWindow)
{
    TransmitDatagram(payload);
    return true;          // sent
}
...
_waiting.Enqueue(payload.ToArray());
return true;              // NOT sent - parked, indefinitely
```

Every layer above sees one `true` and cannot tell which happened. That is why the log says
"answered with 2 frame(s)" while the terminal hangs, and it is why this took a live run and a frame
trace to see at all.

**Why the window was closed.** `CurrentWindow` is `SendWindow` (6) once the peer has told us where
our numbering is, and `UnpositionedWindow` (**1**) before that - deliberately one, so that a peer
carrying stale state can throw away exactly one frame and we can send it again. That single slot was
spent on the announce at start-up, and D100 answered it with a **DR, a disconnect request**
(`[link] disconnect request from the peer, ND frame kind 0x6F`) rather than an acknowledgement that
positions us. `OutstandingFrames` is computed from `_nextSequence - _peerNextExpected`, so it stays
at 1, `1 < 1` is false, and every datagram from then on is enqueued and reported as sent.

The queue holds `MaxQueuedDatagrams = 64` before it starts returning false, so a short session never
even reaches the one honest failure signal available.

---

## What to fix, in order

1. **Make "queued" distinguishable from "sent".** This is the real defect. Whatever the window is
   doing, a layer that reports success for a frame it has parked is lying to every diagnostic above
   it. The runner already separates "built no reply" from "the link refused it"; a third case,
   "the link accepted it and has not sent it", belongs beside them, with the queue depth in the
   line.
2. **Then ask why the announce is answered with a DR.** That is what closes the window, and it is a
   separate question from the reporting. Note it happened for BOTH systems 100 and 102.
3. Only then retry the two-user run.

---

## What this run DID settle

**Our connection-confirm byte is right.** `NdLinkLayer.ConnectionConfirmKindUnverified = 0x1F`
carries a comment saying the low nibble is a guess, that no CC has ever been captured, and how to
test it: *"after we answer a CR with this byte, a peer that accepts it should stop repeating the CR
and move on to sending DT. If it keeps repeating, the byte or the field layout is wrong."*

D100 sent a CR, we answered `0x1F`, and D100 **stopped and sent DT**. By the test the code sets
itself, the byte is correct. That question can be closed.

---

## Evidence

`DOC/captures/tad-connect-queued-2026-08-17.log` - the full traced run, including the CR/CC/DT
exchange and the two reply frames that never left.

Nothing on D100 was changed. `D19999` was already defined there (`LIST-SYSTEM` shows it at state 1,
link 152164), and the only commands sent were `X-C`/`LIST-SYSTEM`, `CONNECT-TO` and ESC.

---

## Item 1 is fixed - a queued frame now says so

`NdLinkLayer.SendDatagram` returned a bool that meant "I took it", covering both outcomes. It now
returns `NdSendOutcome`: **Transmitted**, **Queued** or **Refused**.

Changing the type rather than adding a side-channel was deliberate - the compiler then finds every
call site and makes it say which outcome it means, instead of leaving the distinction to whoever
remembers to read the remark. It found nine, and two of them were already describing behaviour the
old bool could not express:

- a loop asserting `SendDatagram` over `SendWindow + 3` datagrams, commented *"Accepted, not
  necessarily sent - the ones past the window are held"*. It now asserts Transmitted for the first
  six and Queued for the last three.
- *"Only ONE frame goes out before the peer has placed us"* - now Transmitted then Queued, which is
  precisely the live failure's shape.

`EthernetLink` raises `DatagramParked` when the layer holds a frame, and the runner logs it:

```
[TX] *** PARKED *** eth:d100 took a 74B frame but the send window is full;
     2 datagram(s) now waiting and NOTHING is leaving this node until the peer acknowledges us
```

That line is the one whose absence cost this investigation. The runner could already say "no reply
was built" and "the link refused it"; the third case, "the link took it and has not sent it", had
no voice at all.

Two tests pin it: one reproduces the live state exactly - peer known, peer has NOT positioned us,
window of one - and requires the second datagram to come back Queued rather than Transmitted. The
other requires the held frame to actually go out when the peer acknowledges, because a change that
returned Queued and then never transmitted would satisfy the first test while being worse than the
original bug.

**This does not make the chat work.** It makes the next attempt tell the truth about why.

---

## Item 2: the DR was never a mystery, and I caused it

The question was "why does D100 answer our announce with a disconnect request". The answer was
already in the codebase, measured six days earlier, in the remarks on
`NdLinkLayer.SendConnectionRequest`:

> **ANSWERED, and the answer is NO - MEASURED 2026-08-11.** [...] the answer is a DISCONNECT
> REQUEST [...] So this does not do what it was built for, and **it is left OFF**. [...] a real
> machine treats an unexpected connection request on an established link as a reason to tear it
> down.

I started the runner with `--request-link`, which calls exactly that. So the DR was not the peer
misbehaving - it was the documented, measured answer to a frame the code says not to send. The
recipe in the sync-daemon notes carries `--request-link`, and I copied it without reading what it
does here.

**So the live sequence was:**

1. `--request-link` sends a connection request. D100 answers DR, as documented.
2. We then send the announce - a data frame - into a link the peer has just torn down. It is never
   acknowledged.
3. `OutstandingFrames` is stuck at 1 against a dead connection. The unpositioned window is also 1.
4. Everything after that queues, silently, for ever.

### The defect underneath, which is real and is now fixed

Step 3 is not a usage error, it is a fault: **the send state belongs to the connection and has to
die with it.** Handling a disconnect counted it, raised an event and returned, leaving
`_nextSequence`, `_peerNextExpected`, `_havePeerPosition` and the kept-for-resend frame all pointing
at a link that no longer exists. The peer rebuilds the connection immediately - it sent a fresh CR
and we confirmed it - and that rebuilt link could never send anything, for ever, with no error
anywhere.

On a disconnect the layer now clears what was outstanding, forgets its position, drops the stale
resend frame, and drains. **The queue is kept**: those datagrams were accepted from callers who were
told `Queued` rather than `Refused`, so dropping them would make that answer a lie - one goes
immediately, the rest follow as the peer acknowledges.

Two tests, and both were **verified to fail with the fix backed out** before being kept:

```
ADisconnectClearsWhatWasOutstandingSoTheWindowReopens   [FAIL]
DatagramsHeldWhenThePeerDisconnectsAreStillSent         [FAIL]
```

### For the next live attempt

Do NOT pass `--request-link`. The link opens when the ND addresses us, which `CONNECT-TO` does by
itself.

**CORRECTED 2026-08-18 - do not pass `--announce-restart` either.** This section used to recommend
`--announce-restart --resync-hard`. Both are now known to harm: `--resync-hard` manufactures a
sequence drift, and `--announce-restart` makes the peer NAK the initialise with `XRDDF`, "Another
port already has this name", which kills the conversation before it starts. Pass neither.

---

## The second live attempt, and the wall #34 actually hits

Ran again with the fixes in and **without** `--request-link`. No disconnect this time - we did not
ask for one - and the runner sat waiting for D100 to address us, as it should.

`CONNECT-TO D19999` now fails FAST instead of hanging:

```
UNSUCCESSFUL CONNECT:
Remote system D19999 not available
```

Failing fast is better than hanging, but it is not success, and #34 is still not closed. Two things
came out of the run, and the second is the one that matters.

### D100 is not seeing our acknowledgements

```
[link] WARNING the peer RE-SENT data frame seq=7 (we were waiting for 8). It has not seen our
       acknowledgement - check the send window and our own receive latency BEFORE looking for a
       fault in XMSG or the file server.
[seq] node 100: ACK 0x0000 did NOT advance the store (still 0x0001)
```

Our own warning, written for exactly this, is pointing at the send window and our receive latency.
Not diagnosed further - it needs its own run.

### The ethernet seam serves ONE peer, so a cross-machine chat cannot work yet

This is structural, and it was invisible until two machines were on the segment at once. D102 sent us
connection requests, repeatedly, and we never answered one:

```
[sniff] node102 -> node19999  kind0x0F seq=104 ...   <== ADDRESSED TO US   (x4, unanswered)
```

Not a bug in the link - it is doing what it says. `EthernetLink` drops anything not sourced by
**its** peer, and the runner builds exactly one, from `topology.PrimaryEthernetPeer()`. The topology
lists 100 AND 102; the seam takes the first.

So the two-user run as it was imagined - somebody on D100 and somebody on D102 in the same room -
**cannot work today**, and no amount of fixing the TAD path changes that. The options are:

1. **Two terminals on the SAME machine.** Both `CONNECT-TO D19999` from D100. This needs nothing new
   and is the honest next attempt for #34 - the room does not care which machine a session came
   from, only that there are two.
2. **Make the seam serve every ethernet peer**, one `EthernetLink` each. That is the real fix and a
   bigger piece of work: it touches link selection, routing and the per-peer sequence store.

Option 1 is what to try next, and it should be tried before option 2 is built - if two sessions from
one machine work, the chat itself is proved and the remaining work is plumbing rather than protocol.

Evidence: `DOC/captures/tad-connect-second-attempt-2026-08-17.log`.

---

## Third and fourth attempts: the diagnostic works, and the real blocker is the ND-link acknowledgement

### The PARKED line earned its place immediately

First live outing, and it says the whole thing in three lines:

```
[TX] *** PARKED *** eth:d100 took a 14B frame ... 2 datagram(s) now waiting
[TX] *** PARKED *** eth:d100 took a 56B frame ... 3 datagram(s) now waiting
[TX] *** PARKED *** eth:d100 took a 34B frame ... 4 datagram(s) now waiting
```

A climbing queue with nothing leaving. That state was completely invisible before and cost a whole
session to find; now it is three lines at the top of the log.

### The announce alone does NOT make D100 reset - a recorded memory is wrong

Run WITHOUT `--request-link`:

```
[seq] node 100: RESET to 0x0000 (was 0x0001); link KEPT
[seq] node 100: ORIGINATE at Flags1 0x0000 (next becomes 0x0001)
[seq] node 100: ORIGINATE at Flags1 0x0001 (next becomes 0x0002)
[net] node 100 rejected our frame at Flags1 0x0001 with XENSE (our sequence is AHEAD)
```

We zeroed, announced at 0x0000, and D100 still expected 0x0000 for the NEXT one - so it never
accepted the announce and never reset. The note
`resync-hard-d100-resets-on-our-announce` says the announce does it; that was recorded with
`--request-link` in the same command line. **The connection request is what makes D100 forget**,
which is exactly what the flag's own log line claims for it. The DR is its COST, not its failure -
and now that a disconnect no longer poisons the send state, the cost is one it can afford.

That reframes the "left OFF" note on `SendConnectionRequest`: the frame does something useful, and
the 2026-08-11 conclusion was drawn when its side effect was fatal.

### The actual blocker: the XMSG datagram sequence, NOT the link

> **CORRECTION, same day.** This section first said the blocker was the ND-link acknowledgement -
> that D100 "never acknowledges it in a way that advances us", so "exactly one frame per connection
> can ever leave this node". **That is wrong**, and the correction is below. It was written from the
> line `[seq] node 100: ACK 0x0000 did NOT advance the store`, which is the **XMSG datagram (Flags1)
> store** and has nothing to do with the ND-link send window. Two different sequence layers, one
> careless reading.

With `--request-link` back on, both TAD connects still arrive and are answered, and both answers
park:

```
[runner] peer 100 heard after 0.0s; link is up
[RX] 100->19999 sub=Data answered with 2 frame(s)
[TX] *** PARKED *** ... 1 datagram(s) now waiting
[TX] *** PARKED *** ... 3 datagram(s) now waiting
```

So the request reaches us, the reply is built, and one frame goes out under the unpositioned window
of one - and **D100 never acknowledges it in a way that advances us**:

```
[seq] node 100: ACK 0x0000 did NOT advance the store (still 0x0002)
```

That is the root blocker, and it is a different fault from either of the two fixed here. Nothing
above the link layer can help: the ND-link acknowledgement is what opens the window, and until it
does, exactly one frame per connection can ever leave this node.

### Health warning on this evidence

By the fourth attempt D100 was answering `PERF_CONNCT : 17`, *"Error code not recognised by XMSG
error routine"*, having answered `: 7` before that. Repeated failed attempts are known to leave
COSMOS on D100 fouled, and back-to-back attempts are then **not independent samples** - the later
ones may be measuring the wreckage of the earlier ones.

**So the ACK finding above needs one confirming run on a freshly restarted D100 before it is
treated as settled.** I did not restart it: that is a change to a machine somebody else is using,
and STOP/START-XMSG has crashed one of these before.

---

## What the frame census actually shows

Counting frames by kind in the two TRACED runs settles it without touching a machine. This is the
cheap check that should have come first:

| Run | from D100 | from us |
|---|---|---|
| 1, with `--request-link`, before either fix | 5 data, 1 CR, **2 DR**, **0 ack** | 5 ack, **1 data** |
| 2, no `--request-link`, after fix 1 | **7 ack**, 12 data | 12 ack, **7 data** |

**Run 2 is the answer.** Our data went out at seq 0,1,2,3,4,5,6 - seven frames - and D100
acknowledged seq 1,2,3,4,5. There was exactly ONE park in the whole run and it cleared on the next
line. The link layer is healthy: the window opens, the peer acknowledges, our data flows.

So the ND link is not the problem, and the two faults fixed today were real but are not what is
stopping the connect now. What stops it in run 2 is one layer up:

```
[net] node 100 rejected our frame at Flags1 0x0001 with XENSE (our sequence is AHEAD of what the
      peer expects)
```

The XMSG **datagram** sequence is out of step with D100, and that is what `CONNECT-TO` reports as
"Remote system D19999 not available". The FLAGS 1 LAW is the relevant body of knowledge, not the
link window.

Run 1's picture - no acks at all from D100, one solitary data frame from us - is the disconnect
damage, and it is exactly what the fix for the DR path addresses.

### The lesson worth keeping

Two sequence layers, similar words, and I read a line about one as evidence about the other. That is
the same shape as the register conflation that keeps recurring in the ND-500 work. **Count the
frames before theorising** - a census of kinds took one command and overturned a conclusion I had
already committed.

---

## Narrowed again: D100 re-sends the connect, so the REPLY is what it will not take

Reading the traced run without `--request-link` properly, rather than skimming it for the answer I
expected:

```
[RX] sub=ReachabilityRequest f1=0xFFFF     answered with 1 frame(s)
[RX] sub=Data f1=0x0000  ...*TADADM...D19999
[tad] session opened: tty1 from node 100
[RX] sub=Data answered with 2 frame(s)
[RX] sub=Data f1=0x0000  ...*TADADM...D19999      <- byte for byte identical
[tad] session closed: tty1 from node 100
[tad] session opened: tty1 from node 100
[RX] sub=Data answered with 2 frame(s)
```

The reachability exchange works. The connect is decoded and a session opens. Then **D100 sends the
identical connect again**, byte for byte, and eventually gives up with "not available".

A peer repeats precisely because it has not seen a satisfactory answer. Our two frames DID leave -
the census shows seven of ours on the wire and seven acknowledgements coming back - so this is no
longer about the link, or the window, or the datagram sequence. **It is the content of our TAD
connect reply.** That is a much smaller target than anything chased today.

Note also that our own sequence handling is correct throughout: every reply is an ECHO of D100's
number, and our counter never advances, which is the FLAGS 1 LAW behaving exactly as written.

## A diagnostic that cried wolf, fixed

Three lines in every healthy run said:

```
[RX] 100->19999 sub=Ack *** NO REPLY BUILT *** (this hangs the caller)
[RX] 100->19999 sub=ReachabilityReply *** NO REPLY BUILT *** (this hangs the caller)
```

Nothing was hanging. An acknowledgement, a reachability reply and a network error are the END of an
exchange - answering one would be the fault. The alarm fired on all of them.

That is not just noise: those lines were read as evidence during today's hunt and helped push the
investigation a layer too low. A diagnostic that cries wolf on a healthy exchange is worse than
none. Both dispatch handlers now share one `ExpectsAReply` rule and print "(no reply expected)" for
those three, keeping the alarm for arrivals that genuinely wanted an answer.

The two handlers had already drifted - one printed a bare tag, the other a side-qualified one - so
the rule went in one place rather than two.
