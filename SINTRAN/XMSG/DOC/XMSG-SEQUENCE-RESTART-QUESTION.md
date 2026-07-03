# XMSG — what Flags1 (datagram sequence) must a RESTARTED responder start at?

**For the analyst.** The channel/counter seed model (see `XMSG-CHANNEL-SEQUENCE-ANALYSIS-2026-07-03.md`
and `XMSG-PROTOCOL.md` section 18.5) is confirmed and works: our fresh C# node (node 102) drove a
real machine 100 to **`=== CONNECTION ESTABLISHED ===`** and 100 answered with `TMOD/TTYP`. But that
success is **not repeatable across our process restarts**. This document states the exact observations
and asks one question. **We are NOT guessing the answer — we need the rule.**

Everything below is VERIFIED from live console logs unless marked **DON'T KNOW**.

---

## 1. The setup

- Node 100 = real/emulated SINTRAN machine (the terminal/client). Node 102 = our C# responder (host).
- Our C# node is a **fresh OS process each run**: its outgoing datagram sequence (`Flags1`, 102→100)
  resets to `0x0000` on start. Node 100 is **long-running**: its XMSG state persists between our runs.
- Datagram sequence `Flags1` is one 16-bit counter **per direction per node-pair**, +1 per Data
  frame (VERIFIED). The link seed is `0x14` for 100↔102 (VERIFIED, learned from 100's connect as
  `(Counter + Flags1 + (Flags2 & 0xFF)) & 0xFF`).
- Per the seed model, given our chosen `Flags1` for a frame, the sub-header Counter and the channel
  are fully determined (`Counter = (seed − (Flags2&0xFF) − Flags1) & 0xFF`,
  `Channel = 0xDE − (XMCSM>>24) − epoch`). **That part is solved.** The ONLY free choice is the
  **starting value of our own `Flags1`.**

## 2. The two live runs (byte-exact)

Our connect-accept frame was **byte-identical in both** (only addressing differs): subtype 0x0E,
Flags1 `0x0000`, Flags2 `0x0400`, channel `DA`, sub-header Counter `0x14`, XMCSM `0x04000041`.

| Run | 100's connect (100→102) | our accept (102→100) | Result |
|---|---|---|---|
| A (earlier) | Flags1 **`0x0001`**, Counter `0x13` | Flags1 `0x0000`, Counter `0x14` | **SUCCESS** — 100 ACKed the accept, sent session-setup, ACKed our DUMM, sent `TMOD/TTYP`; `CONNECTION ESTABLISHED`. |
| B (now) | Flags1 **`0x0005`**, Counter `0x0F` | Flags1 `0x0000`, Counter `0x14` | **NO SESSION** — 100 sent only a LAPB `RR` for our accept; no XMSG `0x03` ACK, no session-setup. 100's console: no connect. |

Note: seed is `0x14` in both (`0x13+0x0001` = `0x0F+0x0005` = `0x14`). The only difference between
the runs is **100's connect Flags1 advanced from `0x0001` to `0x0005`** — i.e. 100's persistent
per-node-pair sequence had moved on (Run A had left it advanced), while our node reset to `0x0000`.

## 3. What we KNOW

- Our accept's Flags1 = `0x0000` **worked** when 100's connect Flags1 was `0x0001` (Run A).
- The **same** `0x0000` accept **did not** establish a session when 100's connect Flags1 was `0x0005`
  (Run B).
- Earlier, a *different* scheme where our accept **echoed** 100's connect Flags1 (accept Flags1 =
  connect Flags1) also worked across several runs with connect Flags1 = `0x0000` and `0x000A`
  (accept then `0x0000` and `0x000A` respectively). Those reached accept+port-assign ACKed.
- So we have **two schemes that each worked in some runs**: (i) accept Flags1 = `0x0000` [Run A,
  where connect was `0x0001`], and (ii) accept Flags1 = connect Flags1 [echo runs]. They give
  different bytes for the same connect, yet each succeeded in its run.

## 4. What we DON'T KNOW (the question)

**On a fresh responder process, what value must our outgoing `Flags1` (102→100) START at so that
machine 100 accepts our connect-accept, given that 100's per-node-pair expected-sequence-from-102
persists across our restarts and we cannot read our previous value from memory?**

Concretely, for Run B (100's connect Flags1 = `0x0005`, seed `0x14`): what should our accept's
Flags1 be — `0x0004`? `0x0005`? something derived from 100's connect Flags1 or Counter? — and does
the rule also reproduce Run A (`0x0000` when connect was `0x0001`)?

### Sub-questions that would pin it down

1. **Does 100 track "expected next Flags1 from 102" persistently (per node-pair), and does it reset
   that when a new `connect-to` begins, or only when 100's XMSG restarts?** (Run B suggests it does
   NOT reset per connect-to.)
2. **Is 100's expected-from-102 equal to 100's own outgoing Flags1** (i.e. should we mirror 100's
   connect Flags1)? Run A argues against a strict equality (we sent `0x0000`, 100's was `0x0001`,
   and it worked) — unless 100 tolerates an off-by-one on the *accept* specifically.
3. **Does any frame 100 sends reveal the expected value?** e.g. does an out-of-sequence accept get a
   subtype-0x07 `XENSE` (`0xFFDE`) reject whose fields carry the expected Flags1? (In Run B we did
   NOT capture a reject — the operator stopped the run right after 100's LAPB `RR`; we can re-run and
   let it play out to capture 100's actual response.)
4. **Could Run B's failure be a STALE session on 100** — the previous session (Run A allocated `TAD
   LOGICAL UNIT NO: 768`) not being torn down when our process exited — rather than a sequence
   mismatch? If so the fix is a proper disconnect on exit, not a sequence rule.
5. **How does a real ND responder handle its own restart** — does XMSG persist the send/receive
   sequence (`XSSSQ`/`XSRSQ`) in a way that survives a responder reboot, or does a reconnect
   re-synchronise the sequence via some handshake we are not performing?

## 5. What we can provide on request

- A full, uninterrupted Run B log (to capture 100's actual response to the `0x0000` accept — reject
  vs silent drop, and any XENSE fields).
- The exact bytes of every frame in Run A (the successful session) and Run B.
- The pcap decode report (`SINTRAN/XMSG/SRC/pcap-decode-report.txt`) and the XMSG symbol tables.

**Current code state:** `_respFlags1` is left at `0x0000` (the one confirmed-successful value) with
this question referenced in the source comment. No sequence-start rule is assumed.

---

*Written 2026-07-03. VERIFIED items are from live logs; the question in section 4 is genuinely open.*
