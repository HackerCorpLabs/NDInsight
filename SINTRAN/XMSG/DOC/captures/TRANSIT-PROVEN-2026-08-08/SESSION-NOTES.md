# The relay carries a full round trip - 2026-08-08

**Files:** `transit-proof.pcapng` (loopback, TCP 10362/10364/10366), `transit-run.log`.
**Result:** D100 reaches D103 THROUGH our C# node, in both directions, with zero drops and zero
Frame Rejects.

## The measurement

D100's `LIST-ROUTING-INFO,,,D103` - all three lines agree, including the ACTUAL measured path:

```
    103  L: *->19999->103
         T: *->19999->103
         A: *->19999->103
  19999  L: *->19999
         T: *->19999
         A: *->19999
```

`A:` is the path a live probe actually took, not a table lookup. Our node is a working COSMOS
route-through relay.

Our side, from `transit-run.log`:

```
[relay] hdlc-out:127.0.0.1:10364 -> hdlc-in:10366 for node 103     D100 to D103
[relay] hdlc-in:10366 -> hdlc-out:127.0.0.1:10364 for node 100     D103 to D100
...
[relay] relayed=10 forUs=7 dropped=0
```

**Ten datagrams relayed, both directions, none dropped.**

| | before the fix | after |
|---|---|---|
| FRMR frames | 6 | **0** |
| link flapping (`Active -> Starting`) | several | **0** |
| datagrams relayed | 0 | **10** |
| datagrams dropped | - | **0** |

## What made the difference

Two defects, both on the relay path only, both fixed in commit `751e3e8`:

1. **The announce was sent from inside the `StatusChanged` callback.** That fires part-way through
   a batch of received frames, so with D100's six-SABM burst the announce landed BETWEEN two
   sequence resets and the peer's correct acknowledgement then fell outside the window - forcing
   FRMR reason Z every time. Now armed in the callback and sent on the next loop tick, after the
   burst is drained. Full mechanism: `../FRMR-ON-INNAK-2026-08-08/SESSION-NOTES.md`.

2. **The relay ran its links with NO keepalive interval**, which the adapter documents as the
   in-memory test mode. `LoopTick` then fires only when a frame arrives - so fix 1 could never have
   been sent - and the LAPB T1/T3 timers never ticked on a live link at all. Found only because
   fix 1 was verified live rather than assumed. Both links now pass 20 ms.

Pinned by `LapbAnnounceOrderingTests` (replays the captured announce and INNAK bytes).

## Machine setup that this needed

Rebuilt on both peers per COSMOS Operator Guide ND-30.025.02 section 2.5 - and section 2.5.4 is the
rule that unblocked everything: **an ADJACENT system needs a name and NO route**; only NON-adjacent
systems get a `DEFINE-SYSTEM-ROUTE`.

```
D100:  DEFINE-REMOTE-NAME,,D100,100 / D19999,19999 / D103,103
       DEFINE-SYSTEM-ROUTE,,D103,D19999        (103 is NOT adjacent - it is behind us)
       START-LINK,1360,,,-1,,                   (no route to 19999 - it IS adjacent)
D103:  mirrored, with DEFINE-SYSTEM-ROUTE,,D100,D19999
```

D100's `RetroCore.ini` has its two HDLC listen ports SWAPPED so LU 1360's controller is the one only
we dial (10364). That costs D102 its line to D100; restore the commented lines to give it back.

## Still open

 - **`INNAK` is still never answered.** Both peers send one on every announce and the runner logs
   `*** NO REPLY BUILT ***`. It is demonstrably NOT fatal - this whole round trip happened while
   ignoring them - but what the correct reply is has never been captured.
 - **D100's XROUT degrades** after a while: `LIST-ROUTING-INFO` starts repeating one system's block
   forever and has to be broken with ESC, and `STOP-XMSG`/`START-XMSG` clears it. Whether our
   traffic provokes it is UNKNOWN.
 - **Marker 2 `0x12` on a relayed datagram is still unverified.** It was wrongly suspected as the
   cause of the drop; the relay now works WITH the rewrite in place, which is evidence it is at
   least accepted, but not that it is correct.
