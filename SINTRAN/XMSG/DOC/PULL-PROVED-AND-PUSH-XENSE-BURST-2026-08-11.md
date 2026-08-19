# The pull works end to end; the push now dies at the first data burst with a named XENSE

Date: 2026-08-11. Lab: D100 (system 100) over the COSMOS Ethernet segment on the TCP hub 5010,
our node D19999.

---

## 1. SOLVED: reading a file off a real ND, byte for byte

```
Xmsg.Live.Runner --config topology-d19999.json --pull BIGPSH3:TXT --pull-from 100 \
                 --pull-to pulled-verify.txt 127.0.0.1 10364 19999 180
```

Result: **20400 bytes**, and

```
original bigpush.txt      D523BDDC33C913F5E7C1CCDAA92B78DC2A329987BEA61EC4058B34758D5AEBDA
pulled  pulled-verify.txt D523BDDC33C913F5E7C1CCDAA92B78DC2A329987BEA61EC4058B34758D5AEBDA
```

Identical. The file was pushed to D100 earlier, stored by a real SINTRAN, and read back by us:
a complete round trip through someone else's file system. The runner's own "finished" line is
NOT the evidence - the hash comparison is, and it was run separately.

## 2. The two diagnostics that were never connected

Both found the same day, both hiding real answers in plain sight.

 - `XmsgServerHost.Log` was wired on the relay path and NOT on the Ethernet path, so every
   `[seq]` line - the value a link starts from, every Flags 1 we stamp, every store advance -
   was thrown away. An afternoon of sequence work ran on inference instead of on the numbers.
 - `XmsgLayer` wrapped `XmsgNode` privately and offered no way to reach its `Log`, and `XmsgNode`
   printed nothing at all for a network error it could not act on. A push that D100 was actively
   refusing therefore read as plain silence.

**A diagnostic that is not plugged in is not a diagnostic.** When a peer looks silent, check the
log wiring before believing the wire.

## 3. What the machine says now (this is the open item)

With the node's log connected, the push ladder runs a long way and then fails LOUDLY:

```
[push] SendConnectLetter / ReserveFileEntry / SendShortAck / OpenFile / SetBlockSize / WriteFile
[push] SendData: 4 frame(s) sent
[net]  node 100 rejected our frame at Flags1 0x0017 with XENSE (our sequence is AHEAD ...) FFDE
[net]  node 100 rejected our frame at Flags1 0x0018 with XENSE ...
```

Everything up to 0x0016 was acknowledged. The rejections begin at the FIRST multi-frame data
burst and continue in pairs from there.

MEASURED, not assumed:
 - the FA ladder itself is accepted: connect letter, ReserveFileEntry, our ShortAck, OpenFile,
   SetBlockSize and WriteFile all drew replies;
 - our file server answered D100's own LIST-FILES over the same link at the same time, correctly,
   with the two conversations sharing one Flags 1 counter - which is what the law says;
 - the failure is `XENSE` (-34, `0xFFDE`) = our number is AHEAD of what D100 expects. So D100
   did not receive one of our datagrams, or did not accept it.

NOT established: whether the missing datagram left our side at all. A link-layer ACK proves the
frame arrived; it does NOT prove the XMSG layer above accepted the datagram. Splitting those two
needs the wire or D100's own frame counters, not more reasoning.

## 3a. SOLVED - and section 3b below drew the WRONG conclusion from the same evidence

**The push completes.** 20400 bytes, all ten blocks, written to a file created by a quoted new
name, with no XENSE anywhere in the run:

> **Do not copy this command line.** It carries `--announce-restart`, which was in every
> recipe at the time and is now known to make the peer refuse the conversation (`XRDDF`,
> "Another port already has this name"). The run below is kept as the record of what was
> actually run. Today the same run is made WITHOUT that flag.

```
Xmsg.Live.Runner --config topology-d19999.json --announce-restart --resync-hard \
                 --push bigpush.txt --push-as SYNCTS2:TXT --push-to 100 127.0.0.1 10364 19999 150
...
[push] finished: 20400 bytes written to "SYNCTS4:TXT"
```

D100's OWN file server then reported the file back to us as **20400 bytes** when we opened it for
reading, which is confirmation from the machine rather than from our log.

**What actually cured it: `--resync-hard`.** It sends the reachability request (Flags 1 `0xFFFF`)
AND zeroes our own stored counter, and the next frame goes out at `0x0000`.

**That answers a question open since 2026-08-08: D100 DOES reset its expected-from-us when it
receives our reachability request.** D103 over HDLC did not - that measurement stands and is why
`AnnounceRestart` still keeps our counter by default. The two machines behave differently, so
this must stay an explicit choice and not become automatic on the strength of one peer.

**Section 3b is wrong and is kept only so the mistake is not made again.** It concluded that D100
refuses a datagram sent while the previous is still unaccepted - a pacing limit. The successful
run disproves that: `[push] SendData: 4 frame(s) sent` appears TEN times, four datagrams at a
time, every one accepted. The real fault was that we were AHEAD from the first frame; the
rejections that looked like a window were simply the drift being reported once per frame.

Still open, and NOT to be explained by the dead pacing theory: a later pull run drifted ahead
again mid-conversation, at `0x003F`, while our file server was answering D100's directory walk
over the same link. Whether two active halves can outrun the peer is UNMEASURED - the one thing
now known is that four datagrams in a row do not.

## 3b. WITHDRAWN - what the evidence looked like before the successful run

The runs above all started with a carried-over counter, which muddies the reading. A later run
began with a real reset - `[seq] node 100: RESET to 0x0000 (was 0x0021)` - so both sides were in
step at zero and every number below is fresh, not inherited drift.

```
18.102  ORIGINATE 0x0002 |
18.119  ORIGINATE 0x0003 |  all three ACKed at 18.186 / 18.204 / 18.223
18.171  ORIGINATE 0x0004 |
18.277  ORIGINATE 0x0005    push short-ack        ACKed, but not until 18.429
18.316  ORIGINATE 0x0006    our server's short-ack   XENSE at 18.362
18.332  ORIGINATE 0x0007    our server's reply       XENSE at 18.398
18.389  ORIGINATE 0x0008                             XENSE at 18.452
```

`0x0005` was ACCEPTED, and its acknowledgement arrived at 18.429 - **after** `0x0006` had already
been refused at 18.362. So when D100 saw `0x0006` it had not yet taken `0x0005`, and called the
next number ahead.

The rejected values track OUR originations one for one, including which half of our node sent
them - `0x0006` is our file server's short acknowledgement and `0x0007` its reply, 16 ms apart.
That is what identifies the Flags 1 in a subtype-0x07 frame as the REFUSED datagram's number
rather than D100's own counter; our own `CreateNetworkError` documents it the same way. Worth
stating because a symmetric exchange makes two counters look like one, and that trap has already
cost this project two wrong turns.

**So the failure is a PACING failure, not a numbering one.** Our two halves - the file server
answering D100, and the push client driving D100 - each originate independently, and together
they emit datagrams faster than D100 accepts them.

**NOT established: how many datagrams D100 will take before it must accept one.** It is not a
strict one-at-a-time rule - `0x0002`, `0x0003` and `0x0004` went out as a burst in the same run
and were all accepted. Something about rate or about the two halves interleaving decides it, and
guessing a window size here would be exactly the mistake the send-window number already caused
three times. Measure it before coding a limit.

The repair to try is to serialise our originations per peer: hold the next datagram for a node
until the previous one has been acknowledged. That is testable against this same run.

## 4. A defect this exposed, and it is ours

**A frame rejected with XENSE still consumes and persists its Flags 1.** Nothing steps back, so
every rejection pushes us one further ahead and the next attempt starts worse. Three runs drifted
0x003B -> 0x0040 that way with no traffic in between succeeding.

There is already a step-down recovery - `XmsgServerHost.ResyncAcceptDown` - but it only covers an
un-confirmed TAD accept. The general case (any originated frame refused with XENSE) has none.
Generalising it is the obvious repair and it is testable live: the XENSE numbers should walk down
and the conversation should start.

## 5. Recovering a diverged pair, until then

Restart XMSG on D100 (`tools/restart-xmsg-cosmos.ps1 -Port 9010 -WithEthernet`). It zeroes
D100's expectation and makes it send a ReachabilityRequest, which zeroes our store in step.
Verified twice today - the store went to `0x0000` and the numbers climbed together afterwards.

`--announce-restart` does NOT cure this direction: it tells the peer to reset while deliberately
keeping our own counter (see the memory `announce-must-not-reset-our-own-sequence`), which makes
an already-high counter worse, not better.

## 6. Two console traps worth writing down

 - `LIST-FILES D19999(SYSTEM)."",""` - the quoted `""` is the OUTPUT FILE parameter and SINTRAN
   reads the quotes as "create this file". The first run creates an empty-named file and every
   later run answers `FILE ALREADY EXISTS` **locally**, before any traffic leaves the machine.
   Use the prompt form: `LIST-FILES` then `D19999(SYSTEM).` then blank.
 - `FILE-ACCESS INTERNAL ERROR; CALL NOT VALID IN CURRENT STATE` can be stuck on ONE terminal of
   the pool rather than on the machine. Opening a fresh connection got a clean listing out of a
   machine that had answered that error four times in a row on the old line.

---

## 7. The sync daemon runs, and what actually stops it running unattended

The folder-watch daemon (`--sync <folder> --sync-user <user> --sync-to <node>`) was built and run
against D100 the same day. It scans, holds back a file still being written, plans, and carries one
transfer at a time.

**It works with nobody at a console.** With `--request-link` it learned the peer in 0.0 seconds
and drove a full push ladder by itself - connect letter, ReserveFileEntry, OpenFile, SetBlockSize,
WriteFile, SendData - before dying on the ahead-drift that `--resync-hard` cures.

Three defects were found by RUNNING it, none by reading it:

 - **One file was queued on every scan.** The ledger is written only when a transfer succeeds, so
   between queueing a file and finishing it nothing stopped the next scan queueing it again.
 - **A bad file name took the whole node down.** The plan names files as a person would type them,
   `D100(SYSTEM)."WATCH1:TXT"`; the wire carries neither machine nor user, and the open request
   refused 27 characters against a fifteen-byte field. The exception escaped the loop tick and
   stopped the runner - file server, TAD sessions and all, for one bad name.
 - **Our own resync dropped the in-memory link**, leaving us unable to address a peer we had been
   talking to a moment earlier. `ResetSequence` drops it correctly when the PEER announces; our own
   announce must keep it.

**The remaining blocker is not a bug, it is a gap in what we know.** `CanReach` is false until an
XMSG datagram arrives FROM the peer, because the layer learns its seed from an inbound datagram
and never derives it. So the daemon cannot open a conversation with a machine that has not spoken
since we started. `--request-link` brings up the ND LINK layer only: once that was enough to draw
a real conversation out of D100, and another time D100 answered it with a disconnect (frame kind
`0x6F`) and stayed silent.

A daemon that needs somebody to type `LIST-FILES` on the far machine is not a daemon. Closing this
is a PROTOCOL question - how a real COSMOS node opens a conversation with a peer that has not
spoken - so it wants a capture of a real machine doing it, not an invented seed.
