# XMSG connect-to — what's proven, and what could still block the MOTD

**For a senior analyst.** We are very close: a fresh C# node (node 102) drives a real SINTRAN
machine (node 100) to `=== CONNECTION ESTABLISHED / TAD LOGICAL UNIT NO: 768 ===` and 100 begins
terminal negotiation. The remaining goal is to make the **MOTD/login screen** appear on 100's
console. This doc states what is VERIFIED, what the current code does, and the two or three things
that could still block the MOTD — so a smarter reviewer can check our reasoning or spot the gap.

Prior docs (all in `SINTRAN/XMSG/DOC/`): `XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md` (the seed
model), `XMSG-SEQUENCE-RESTART-ANSWER-2026-07-03.md` (the sequence-persistence answer),
`XMSG-PROTOCOL.md` §18.5 (updated wire model). Capture decode: `SRC/pcap-decode-report.txt`.

---

## 1. VERIFIED LIVE (against machine 100)

- **The envelope seed model is correct.** Per link one seed byte (0x14 for 100↔102, learned as
  `(Counter + Flags1 + (Flags2 & 0xFF)) & 0xFF`); per direction one Flags1 (+1 per Data frame).
  Then `Counter = (seed − (Flags2&0xFF) − Flags1) & 0xFF`, `epoch = (Flags1 − baseLow + 0xFF) >> 8`,
  `Channel = 0xDE − (XMCSM>>24) − epoch`. Verified over all 601 captured Data frames AND live.
- **A fresh responder (epoch 0) sends terminal data on channel 0xDD** (not DB — DB was an epoch-2
  artifact). The first terminal-data frame, a **DUMM** (`Flags1 0x0002, Counter 0x0A, channel DD,
  XMCSM 0x01080000`), was **accepted live** — 100 delivery-ACKed it and printed CONNECTION
  ESTABLISHED, then sent `TMOD/TTYP/DESC/OPSV`.
- **Datagram-sequence behaviour, both directions confirmed:**
  - Our accept `Flags1` **behind** 100's expected → **silently dropped** (only a LAPB RR, no `0x03`
    ACK, no session). Observed when we sent `0x0000`/`0x0001` while 100 expected ~`0x0003`.
  - Our accept `Flags1` **ahead** → recoverable **XENSE** (subtype `0x07`, Flags2 `0xFFDE` = −34),
    the reject echoing our offending Flags1. Observed when we sent `0x0002` while 100 expected `0x0000`.
  - **Exact match** → 100 ACKs and the session proceeds.
- **100's expected-from-us persists** across our process restarts and across bare HDLC link
  restarts; it is the count of Data frames 100 has received from us since its pair-state was created.
- **A ReachabilityRequest from 100 is its XMSG-restart signal.** Verified: restarting XMSG on 100
  makes 100 send a ReachabilityRequest (Flags1 `0xFFFF`, channel DE) and then connect at Flags1
  `0x0000` (fresh); a bare HDLC link stop/start sends NO reachability and 100 keeps climbing its
  sequence. So reachability ⇔ 100's sequences zeroed.

## 2. What the current code does (the fix under test)

- Learns the seed from 100's connect; builds accept/port-assign/DUMM and the MOTD burst from the
  seed model (`XmsgEnvelope`, `TadTerminalResponder.BuildResponderFrame`).
- **Persists our outgoing Flags1 per remote node to a file** and continues from it on restart
  (`FileResponderSequenceStore`), so a restart of OUR node stays in step with 100.
- **On a ReachabilityRequest from 100, resets our stored Flags1 for that node to 0x0000**
  (`TadResponder.ResetSequence`, wired in `XmsgNode`), so an XMSG restart on 100 auto-resyncs us.
- The MOTD burst (`TadTerminalResponder.OnTerminalSetup`) fires when 100 sends `TMOD` (its terminal
  negotiation): control `0x20` (channel DE, XMCSM `0x00080000`), RESE ×2 (DD, `0x01080000`), then
  the MOTD (DD) — each continuing our one Flags1 sequence. TAD payloads copied from capture frames
  57/58/60/62. Frame-flags copied per frame type: DUMM `0x92`, ctrl `0x86`, RESE `0x96`/`0x92`,
  MOTD `0x96`; role `0x00` on all data-phase frames.

At the time of writing, this build is **not yet confirmed to produce the MOTD** — the last failing
runs were sequence-desync (now addressed). The immediate expectation: with 100 fresh and our file
cleared, the accept matches, the session establishes, and the burst produces the banner.

## 3. What could STILL block the MOTD (the actual questions)

If, with sequences correctly in sync (accept ACKed, `CONNECTION ESTABLISHED`, 100 sends `TMOD`), the
MOTD burst does **not** render the login on 100's console, the suspects are:

1. **Burst timing / ordering.** In the capture the responder's frames 54–62 (DUMM, ctrl `0x20`,
   RESE×2, MOTD) have consecutive Flags1, but we cannot see from the one-directional decode whether
   the responder sent them as one burst or **interleaved with, and gated on, 100's `TMOD`/`RECO`
   acknowledgements**. We currently send DUMM after port-assign, then the rest as one burst on
   receipt of `TMOD`. **Q: must any of ctrl `0x20` / RESE / MOTD wait for a specific 100 frame
   (e.g. a `RECO` reset-confirm) rather than being sent back-to-back?** The capture shows 100
   sending `RECO` (0x17) frames in this phase.

2. **Sub-header frame-flags and role bytes.** These (`0x82`/`0x86`/`0x92`/`0x96` frame-flags; role
   `0x00`/`0x40`/…) are copied per-frame from the capture, not reduced to a rule. The DUMM worked
   with `0x92`. **Q: is there a rule for the frame-flags bits (esp. the `0x10` bit that flips
   `0x84`↔`0x94`, `0x86`↔`0x96` within a session), and do wrong values cause a silent no-render vs a
   fault?** We will not fuzz them against the live machine (XXPER kills XMSG).

3. **Does the responder owe 100 responses to the `TMOD/TTYP/DESC/OPSV` negotiation** beyond ACKing
   it — e.g. echoing chosen terminal parameters — before the MOTD is meaningful? The capture's
   responder does NOT obviously echo TTYP, but the ordering with `RECO` (above) suggests a small
   handshake we may be skipping.

4. **Disconnect hygiene.** When our node exits, we do not send a TAD disconnect (DCON, XMCSM
   `0x00080000`), so 100 may hold `TAD LOGICAL UNIT 768` allocated. **Q: does a lingering LU block
   or corrupt the next connect-to, and what is the correct teardown frame sequence?** (Believed
   non-blocking for the MOTD itself, but relevant to repeated runs.)

## 4. The single most useful thing a reviewer could confirm

**The exact time-ordering of the responder's terminal-setup phase in `conn-to-d102-from-100.pcapng`
— interleaving 100→102 and 102→100 by capture timestamp, not by direction** (the decode report
separates directions, hiding the interleave). Specifically: relative to 100's `TMOD/TTYP` (frame 6)
and `RECO` (frames 10/12), when exactly does the responder emit ctrl `0x20` (57), RESE (58/60), and
the MOTD (62)? That timeline resolves questions 1 and 3 at once.

Everything upstream (channels, counters, sequence sync) is solved and verified; the only remaining
risk is the micro-choreography of the four terminal-setup frames, which the one-directional decode
obscures.

---

*Written 2026-07-03. VERIFIED items are from live logs against machine 100 and the full capture
sweep. Section 3 lists genuinely open items, in priority order.*
