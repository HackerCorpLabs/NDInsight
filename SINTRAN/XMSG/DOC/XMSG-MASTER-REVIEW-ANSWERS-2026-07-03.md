# Master handoff — expert review answers (2026-07-03)

Answers to the three open problems in the MASTER handoff. Evidence: code review of
`SINTRAN/XMSG/SRC/` (file:line cites below), byte extraction from
`SINTRAN/XMSG/SRC/pcap-decode-report.txt` (line refs), and the COSMOS manuals.
CONFIRMED / RULED-OUT / INFERENCE / UNKNOWN marked throughout.

**TL;DR**
- **A:** "we fucked up the LAPB" is CONFIRMED at code level. The peer's repeated frame was
  never verified to be RR — the parser cannot distinguish RR/RNR/REJ and the logger prints
  "RR" for all three. Plus: no T1 tick in the live runner, depth-1 retransmit buffer with up
  to 4 frames outstanding, P/F bit ignored. FCS/stuffing is RULED OUT. **The existing stall
  logs already contain the answer** (raw hex is appended to each `[rx-raw]` line — read the
  control byte).
- **B:** our epoch-1 accept is **byte-identical to the captured real epoch-1 accept in every
  field except Flags1**. The envelope (chan D9, ctr 0xFF) is formula-correct and wrap-boundary
  frames are proven legitimate. Crash candidate #1 is the Flags1 value itself — ours equals the
  incoming connect's Flags1 (0x0015), an echo signature, where real responders use their own
  independent counter. Candidate #2: not following the accept with the port-assign frame.
- **C:** the complete login state machine is now extracted from three captured logins
  (including a failed-password retry). SYCN `000A` is the logged-in marker; all control
  opcodes (SYCN/ECKM/CESC/RFI/BMMX) are HOST-sent only. `li-syst-tad-103` was a false lead
  (zero TAD chains — PAD/XROUT only).

---

## Problem A — the LAPB stall

### Verdicts per candidate

**A-1. REJ/RNR misread as RR — CONFIRMED BUG (parse + handle + log).**
- `Xmsg.Hdlc/LapbFrame.cs:81-86`: S-frames are classified by `(Control & 0x03) == 0x01`
  and only N(R) is extracted. There is **no `Control & 0x0F` anywhere** — RR (0x01),
  RNR (0x05) and REJ (0x09) are indistinguishable downstream.
- `Xmsg.Live/LapbLink.cs:387-412` `HandleSupervisory`: every S-frame is treated as RR.
  A peer REJ (the standard, mandatory-retransmit NAK) was silently swallowed pre-b832cdd;
  RNR (peer busy) is likewise invisible — no busy state exists and `SendInformation`
  transmits into a busy peer.
- `Xmsg.Live.Runner/Program.cs:294` (RX) and `:220` (TX): the log string "RR" is hardcoded
  for any S-frame. **The handoff's "VERIFIED: peer repeats RR nr=0" is therefore NOT
  verified** — REJ nr=0 (`09 09`) and RNR nr=0 (`09 05`) print the identical line.
- Also CONFIRMED: `PollFinal` is parsed (`LapbFrame.cs:94`) but never read anywhere; our RR
  is always F=0 (`LapbLink.cs:377`). A peer in T1 recovery polling with RR P=1 never gets
  the required F=1 answer — this alone can present as a repeated-supervisory stall.

**Zero-cost decisive step:** `Program.cs:296` appends the raw frame hex to every `[rx-raw]`
line. In the existing stall logs, read the SECOND byte of the bracketed hex on the repeated
line: `01/21/41/61…` = genuine RR, `05/25/45/65…` = RNR, `09/29/49/69…` = REJ. That single
byte separates "peer never accepted a valid frame" (RR) from "peer got it out of sequence"
(REJ) from "peer was busy and legally discarded it" (RNR). Do this before any re-run.

**A-1b. No retransmit timer in the live pump — CONFIRMED BUG (turned one loss into deadlock).**
`LapbLink.Tick` implements T1, but `Program.cs:305` passes `keepaliveInterval: null` and the
null branch of `Seam/LapbLinkAdapter.cs:153-175` never calls `_link.Tick` (only the keepalive
branch does, lines 201/206). With no T1 and no REJ handling, any single first-transmission
loss deadlocks forever. This is the mechanism of the hang, independent of why the first copy
was lost.

**A-2. FCS / byte-stuffing on specific content — RULED OUT.**
`Xmsg.Live/HdlcEncoder.cs:52-99` + `Xmsg.Hdlc/Fcs16.cs`: FCS appended before stuffing and
both FCS bytes ARE stuffed (`HdlcEncoder.cs:72-73`); escape set exactly {0x7E,0x7D} XOR 0x20,
symmetric with `HdlcDeframer.Unstuff`; FCS over address+control+info only, matching the SABM
anchor (`01 3F 00 64` → `2E 09`). The four reply frames were rebuilt byte-for-byte
(scratchpad): help=129 B, echo-"3"=55 B, time=52 B, date=56 B — **no payload/header byte is
0x7E/0x7D in any of them**, and failing echo-"3" (55) is SHORTER than passing date (56), which
also kills the length theory. A stuffing bug cannot be per-command-deterministic here.

**A-3. Window / sequence adoption — CONFIRMED design defects; not provably THIS stall's trigger.**
- `LapbLink.cs:229`: `_lastUnackedBody = body;` — the retransmit buffer is depth 1, but the
  node routinely has up to FOUR I-frames outstanding (`TadTerminalResponder.OnTerminalSetup`
  lines 461-485 emits four back-to-back; connect and typed-line paths emit two,
  `XmsgNode.cs:248-256`, `:289-302`). The b832cdd heuristic therefore retransmits the WRONG
  frame whenever the peer's N(R) points at anything but the last.
- No window k enforcement; RNR never honored — transmitting into a busy peer, whose legal
  behavior is to DISCARD the I-frame, is the one mechanism by which a lossless TCP stream
  "loses" a frame whose identical retransmission later succeeds (INFERENCE for this stall;
  the log control byte will tell).
- Sequence adoption (`LapbLink.cs:420-431`): contradicts the code's own restart handling
  (`:303-318`, which assumes the peer resets on SABM). Desync scenario: first post-SABM
  I-frame is a retransmission with stale N(R) → we adopt garbage. In the logged stall the
  numbers happened to agree (ns=0 vs nr=0), so adoption is a standing risk, not the shown
  trigger.

**A-4. RX de-stuffing — RULED OUT.** `LapbLinkAdapter.FeedByte` (244-292) is correct;
every frame FCS-gated. (Nit: FCS-invalid frames are dropped silently — add a counter/log.)

### The fix (replaces, not extends, current handlers)

1. Decode S subtype in `LapbFrame` (`Control & 0x0F`) + P/F bit; fix both "RR" log sites.
2. `HandleSupervisory`: RR → cumulative ack to N(R)−1; **REJ → go-back-N retransmit from
   N(R)**; RNR → peer-busy (hold I-frames, probe with RR); answer P=1 with F=1.
   Delete the repeated-behind-RR heuristic.
3. Run T1: tick the link in the null-keepalive branch of `LapbLinkAdapter.RunAsync` too
   (or always run with an idle tick interval that sends nothing).
4. Real send window: queue of unacked bodies keyed by N(S), k ≤ 7, retransmit from
   requested N(R), trim on ack — replaces `_lastUnackedBody`.
5. Delete sequence adoption: SABM/UA resets both directions to 0 (the code's other branch
   already assumes the ND peer does this). Re-verify the original "kernel keeps V(S)/V(R)"
   observation AFTER fixing the logging — it was recorded through the everything-is-RR lens.

Root cause statement: the deadlock is fully explained by A-1 + A-1b (CONFIRMED). WHY the
peer refused the first copy remains open until the log control byte is read — the frames are
provably well-formed, so the refusal was state-based at the peer (busy discard or
out-of-sequence after an earlier frame of the same multi-frame burst), not content-based.
The apparent per-command determinism does not map to any frame property — UNKNOWN pending
the log check.

---

## Problem B — reconnect crash at epoch ≥ 1

### The field-by-field diff (ours vs the real epoch-1 accept)

The closest analog exists: `new-conn-to-102-from-100.pcapng` f60 (report lines 8678-8686) is a
REAL responder accept at epoch 1, channel D9:

```
ours: 2113000E 0064 0066 0015 0400 D9 FF 2100 86 40 0064 02B5 0066 0156 04000041 00 08 01020000 0202000A
real: 2113000E 0064 0066 0046 0400 D9 CE 2100 86 40 0064 0288 0066 0156 04000041 00 08 01020000 0202000A
```

Every field matches the captured pattern — frameFlags 0x86, role 0x40, dstPort = echo of the
connect's srcPort, srcPort 0x0156 (342, constant across ALL nine captured accepts), XMLEN 8,
payload `01020000 0202000A` **byte-identical in all nine accepts across all links, epochs and
responders** (via100 f104 L988, conn-to-d102 f51 L2464, li-syst-tad f10-f16 L7852-7914,
start-li-li f106 L10239, li-rout-102-tree f4 L3305). The accept payload carries NO per-session
data; the assigned data port travels in the following PORT-ASSIGN frame (XMCSM 0x04000000,
XMLEN 24 — f53 L2481, new-conn f62 L8695, via100 f106 L1006; per-session bytes = the assigned
port 0x04C2/0x0341/0x0313 and one 0x0B-message byte).

**The ONLY deviation is Flags1.** And ours (0x0015) exactly equals the incoming connect's
Flags1 — the signature of an ECHO, not of an independent counter. In the captures the
responder's Flags1 is its OWN monotonic per-direction count, unrelated to the connect's
(conn-to-d102: connect F1=0x00F8, accept F1=0x012F; new-conn's equal 0x0046/0x0046 is
coincidence of symmetric traffic).

### Answers to the B questions

1. **Accept channel at epoch ≥ 1:** the accept rides the channel derived from the
   RESPONDER'S OWN Flags1/epoch — never "connect channel", never "connect−1". The capture's
   D9-connect/D8-accept pair is explained entirely by the responder being at epoch 2.
   **Channel D9 with Counter 0xFF is the formula-correct envelope for an epoch-1 accept**
   (given the responder truly sent 0x15 prior frames), CONFIRMED by two real epoch-1 accepts
   on D9 (new-conn f60 ctr 0xCE; start-li-li f106 ctr 0x2B).
2. **Terminal channels at epoch 1 (DC, 0xF* counters):** legitimate; 100's own epoch-1
   terminal frames (Base 0x010C) demonstrate them throughout conn-to-d102 (L2097-2385).
3. **Epoch boundary:** CONFIRMED exactly right at all three captured wraps —
   via100 f12→f14 (L115-141), conn-to-d102 f42→f44 (L2385-2412: `...010D 0108 DB FF...`),
   li-route-d103-tree f1→f3 (L6186-6212). Real machines emit ctr=0xFF with the channel
   stepped down at every wrap. Counter 0xFF is NOT the crash cause per se — 100's own
   crashing-run connect arrived with ctr=0xFF.

### Crash candidates, ranked

1. **Flags1 discontinuity (echo instead of own counter).** Verify in
   `TadTerminalResponder.BuildResponderFrame` /`FileResponderSequenceStore` where the accept's
   Flags1 comes from. If it echoes `request.Header.Flags1` (or a resync path lands there),
   100 sees a 102→100 stream discontinuity: a value that is neither behind (silent drop) nor
   cleanly ahead (XENSE) of expected in the sense its state machine can classify — INFERENCE:
   XROUT/XMSG treats it as untreatable. Fix: the accept's Flags1 must be OUR persisted
   next-in-sequence, full stop.
2. **Port-assign missing after the accept.** Every real responder sends accept THEN
   port-assign (XMLEN 24, carrying the assigned data port) then DUMM. If the crash run never
   got past the accept, 100's CONNECT-TO is left half-open in its "perform connect" step
   (`PERF_CONNCT`). Check the live log for whether port-assign was sent before the crash.
3. (weak) An accept exactly on the wrap boundary — no capture shows one, but wrap frames in
   general are proven legitimate; only suspect if 1 and 2 are exonerated.

### The crash strings

- `PERF_CONNCT:3` matches the COSMOS CONNECT-TO "SYSTEM MALFUNCTION: <errname> : <nn>"
  format (COSMOS User Guide, `Operations/Cosmos/ND-60.163.4`, lines 2943-2959 show
  "CONNCT : 26" as the example). It is CONNECT-TO's perform-connect step failing with code 3;
  **what code 3 means is not documented in the tree — UNKNOWN.**
- **Crash code 24B:** two readings exist in the symbol file and they must not be conflated.
  `XMSG-VALUES-M.SYMB` is in DECIMAL: `XXPER=20` (line 349, "Protocol error in communications
  system") and `XXRO2=24` (line 353, "XROUT fatal error - see XROUT Basefield"). The console
  prints "code 24B" with an octal suffix: 24 octal = 20 decimal = **XXPER**. If the console
  actually printed decimal, it would be XXRO2. Given the B suffix, XXPER remains the primary
  reading (as in the earlier analysis); the XROUT-crash path (XXRO2) is consistent with the
  `XROUT: Link restarted, LUN 1360B` precursor and PERF_CONNCT, but is NOT established by the
  number alone. LUN 1360B is simply the standard XMSG HDLC link unit (START-LINK 1360,
  Operator Guide L938/L1093) — the link our node restarts by reconnecting; expected noise,
  not a fault code.
- One agent claim requires caution: it suggested the per-link seed is "renegotiated at link
  start" citing 102↔103 seed 0x11 vs 0x12 — but the earlier full sweep attributed 0x11/0x12
  to the DIRECT vs RELAYED legs of the same sessions (relay re-stamps Counter +1), not to
  different sessions. Seed-per-session is therefore NOT established; seed 0x14 for 100↔102
  held across five captures. Treat "seed changes at link restart" as UNKNOWN — but
  learn-from-peer per connect makes it moot for the responder path.

---

## Problem C — the login state machine (extracted from three captured logins)

Sources: via100 (L7-2011, complete login), conn-to-d102 (L2012-2929, complete login with a
Ctrl-A edit while typing "log"), new-conn (L8143-9235, **failed password then successful
retry**), test1 (L9321-11420, already-logged-in: error wrapper + ACCESS DENIED path).
`li-syst-tad-103` contains NO TAD chains at all (PAD/XROUT only) — false lead.

Direction rule (VERIFIED, all captures): SYCN/ECKM/CESC/RFI/BMMX are sent ONLY by the host
(the SINTRAN login side — i.e. US when 100 connects in). The client sends only BDAT
keystrokes (ASCII with high/parity bit set, e.g. `F3F9F38D` = "sys"+CR), CERS, DUMM
keepalives, the TMOD/TTYP/DESC/OPSV + ESCA + RECO setup, and final DCON.

```
 1. setup      client: TMOD 08 + TTYP 0000 + DESC 1B + OPSV 4C0104 ; ESCA ; RECO x2
               host:   RESE x2
 2. banner     host:   BMMX 010000 + ECKM 01 + BDAT(banner) + SYCN 0002 + BDAT("\r\nENTER ") + RFI
                       [SYCN 0002 = waiting for username]
 3. username   client: BDAT("sys"+CR)                      (high-parity ASCII)
 4.            host:   BDAT(0D0A) + SYCN 0003 + CESC 00    (client answers CERS)
 5. password   host:   BDAT("PASSWORD: ") + ECKM FF + RFI  (echo OFF)
 6.            client: BDAT(password+CR)                   (empty password = 8D alone)
 7a. WRONG     host:   BDAT(0D0A) + ECKM 01 + SYCN 0002 + BDAT("\r\nENTER ") + RFI
               -> silent reset to step 3. No error text, no OK.   (new-conn L8845-8854)
 7b. CORRECT   host:   BDAT(0D0A) + ECKM 01 + BDAT("OK") + SYCN 0006 + CESC 01   (client: CERS)
 8. logged in  host:   BDAT(0D0A) + SYCN 000A + BDAT(52 40) + RFI
                       [SYCN 000A = LOGGED IN - the state that beats the 1-minute DCON]
 9. steady     client BDAT command lines + DUMM keepalives;
               host: output BDATs (255-byte chunks), then SYCN 000A + 52 40 + RFI after
               EVERY completed command; errors: SYCN 000C + BDAT(error) + SYCN 000A + prompt + RFI
10. logout     host: BDAT(time/date) + CESC 00 ; BMMX 000000 + ECKM 00 + CESC 00 ;
               BDAT("\r\n--EXIT--\r\n") + SYCN 000B ; CESC 01     then client: DCON
```

Implementation rule for our host: after the password line, send exactly
`0D0A` + ECKM 01 + "OK" + SYCN 0006 + CESC 01, then `0D0A` + SYCN 000A + `52 40` + RFI, and
re-assert SYCN 000A + `52 40` + RFI after every completed command. RFI terminates every
host frame that expects input (ENTER prompt, PASSWORD, every # prompt).

UNKNOWNs (explicit): prompt bytes `52 40` ("R@") meaning; CERS trigger semantics
(correlates with CESC transitions); host opcode 0xFD (XMCSM 0x00060000) purpose; CPCO
payload `0004418B` on ACCESS DENIED; BMMX payload semantics (010000 with echo-on, 000000 at
teardown); the bad-USERNAME path (unseen — SINTRAN accepted "sys" in every capture);
whether SYCN 000A alone cancels the 1-minute timeout or the full 0002→0003→0006→000A ladder
is required (no capture shows the timeout itself — VALUES/ORDER above are from successful
logins only). Cross-direction ordering reconstructed from counters/causality, not packet
timestamps (the report has none).

---

## Priority actions

1. **Read the stall log's raw hex control byte** (A) — free, decides RR vs RNR vs REJ.
2. **Fix the LAPB layer** per A-fix 1-5 (one state machine per link, XMSG-agnostic;
   U/S frames never touch XMSG state).
3. **Audit the accept's Flags1 source** (B candidate 1) and confirm port-assign is sent
   (B candidate 2); only then re-test epoch-1 live.
4. **Implement the login ladder** (C) — it is fully specified above except the marked
   UNKNOWNs, none of which block implementation (mirror `52 40` verbatim).
