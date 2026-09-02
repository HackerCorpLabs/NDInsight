# Plan: prove the conversation-word fix, then close the sequence question

**Target:** commit `d8e5f36` verified against a real machine, and the out-of-step failure either
fixed or pinned to the peer.

---

## Phase 1 - one clean run that proves the fix

First because it is 15 minutes of work and it either promotes `d8e5f36` from "well-evidenced" to
"proven" or tells us the fix is wrong. Everything else is worth less until this is answered.

**The rule that makes this cheap:** the FIRST conversation after D103 restarts is in step, because
its counter starts at zero and it announces. Later ones drift, because our announce zeroes us while
D103 keeps climbing. So one restart buys exactly one valid test - do not waste it.

1. Restart the D103 emulator - `F:\RC\RonnyTest\HDLC3\RetroCore.exe`. **Not `STOP-XMSG`** - that
   crashed the machine on 2026-08-08 and a plain restart gives the same reset in ten seconds.
2. Log in, then in `X-COMM`: `DEF-REMOTE,,D19999 19999` and `START-LINK,1360,,,-1,,`. The XROUT name
   does not reliably survive a reboot.
3. Start tshark on `tcp port 10366`, then start the relay runner. Listener before dialler.
4. Confirm from the `[seq]` line that this run really is in step - the link must be created from a
   frame carrying `0x0000`. **If it carries anything else, stop: the run is invalid, go back to 1.**
5. Drive `LI-FI D19999(SYSTEM).,,` from D103. Expect the full four-file listing.
6. If it lists: re-record the golden note in `FaListingRegressionTests` with "verified live", and
   close #29's first half.
7. If it still stalls after the ReserveFileEntry reply: the fix is wrong or incomplete. Diff our
   whole reply frame - sub-header included, not just the FA body - against the real one in
   `nd-to-nd.pcapng`, which is already on disk.

## Phase 2 - decide whether the peer honours our announce

Second because it is the actual blocker for everyday use, and because one measurement splits the
two possible fixes. Do NOT change code before this measurement - two attempts have already been
spent guessing at this fork.

1. With both sides up and a conversation already run (so both counters have climbed), restart ONLY
   our runner. That fires `AnnounceRestart`: it zeroes our stored sequence and sends the
   ReachabilityRequest.
2. Watch the wire for D103's NEXT Data frame and read its Flags 1.
   - **Drops to `0x0000`** - the peer honours our announce, both sides reset, and the out-of-step
     case is something else. Go to 3.
   - **Keeps climbing** - the peer ignores it. Our announce is then actively harmful: it zeroes us
     while the peer stays high. Go to 4.
3. If honoured: capture the failing case again with `[seq]` on and find what differs from the run
   that worked. Suspect ordering - the announce firing before the peer's link is ready to act on it.
4. If ignored: the fix is to stop resetting ourselves unilaterally. Options, cheapest first:
   - Keep our persisted counter across our own restarts and never zero it on our own announce -
     only on an INBOUND reachability request, which is the peer telling us it really did restart.
   - Seed from the highest Flags 1 seen from the peer when the store is behind it.
   - Both. Whichever is chosen, re-verify with Phase 1's recipe and expect the goldens to move.
5. `ResyncAcceptDown` exists to walk our number down one per XENSE. Once the counter model is
   correct, decide whether it is still needed or is now masking the real value - the code comment
   already flags it as a symptom rather than a feature.

## Phase 3 - confirm the close, then FA housekeeping

Third because each item is small and none of it blocks anything else.

1. Drive a full FA session to a close against D100 and check it answers normally rather than
   `XEIMA`. Likely already fixed by `d8e5f36`; this is confirmation, not investigation (#18).
2. Decode the TAD ending frame: what `8294` against `9694` means, and the body's `0900` against
   `1800`. Bytes are recorded verbatim in the ND-TO-ND `RIG.md`; no machines needed.
3. Decode the rename's two `B0` records field by field - the 60-byte existing entry and the 25-byte
   new-name record. Operation and sub-function are measured; the sub-fields are not.
4. Second `CreateFile` sample at a different name length, to separate a field from padding.

## Phase 4 - blocked, not actionable

Listed so they stay visible. **Nobody should spend time here until a trigger appears.**

1. **FA `0x0D` device-function** - blocked: no operator command is known to drive it, and the carve
   route is closed. Unblocks if a COSMOS program is found that emits it.
2. **FA `0x01` file-entry-disconnect** - blocked, same reason. Its table slot holds the padding
   address, so it may never be dispatched; **a capture showing it is never sent closes it as a
   result**, which is the realistic outcome.
3. **Whether anything ever sends `0x04`** - the binary's name table lists Change-file-entry-id
   there, but the operator route uses `0x000C`/`0x009A`. Needs a different client to settle.

---

## Two experiments not to run again

Both were measured and rejected; re-running them costs a session each.

 - **Echoing the letter's Flags 1 on the connection confirmation.** Tried 2026-08-04 and again
   2026-08-08. XENSE both times. `XmsgAnsweredFlags1.None` is correct.
 - **`STOP-XMSG` / `START-XMSG` on D103 to force a counter reset.** Hung `X-COMM`, then the
   emulator process died. Restart the emulator instead.
