# HANDOFF - make ENNS0 DEF-NETWORK-CONN succeed (HLE Ethernet II card)

**Date:** 2026-08-10  **Status:** root cause characterised + golden reference built; the code fix
is NOT yet made (it needs registration-path RE, see below). Resume in a fresh context window.

## *** ORACLE ANSWER 2026-08-11 (full [0x0141] flow traced) ***

The net-server (ND-100 RT, P=0x74CD) does this ONCE, ~11 s after the card registers *XM-ENNS0:
build [0x0141] "*XM-ENNS0" op1 (28B) -> XFSND to XROUT(port0) from port4 -> XROUT routes it back
(XFRCV port1, 28B) -> net-server FORWARDS it to the CARD (0x006402FE = port5) from port1 and RINGS
the card (CONTROL 0x0005 ndint). That is the *XM-ENNS0 SERVER ACTIVATION. Oracle lines 5588-5605.
The card sends NOTHING to trigger it (only 1 PIL=12 XFSND all boot = the registration itself). So
[0x0141] is the net-server's OWN action, gated on: *XM-ENNS0 having JUST been registered DURING
start-net (oracle: card registers at 09:38:29 in RESPONSE to start-net-server ringing it at
09:38:27; [0x0141] at 09:38:40). The HLE registers *XM-ENNS0 at BOOT (proactively, burst-1), so by
start-net the net-server's activation window has passed -> it never sends [0x0141]. Ports/registration
content are IDENTICAL and NOT the cause (proven). The card must register IN RESPONSE to start-net
ringing it - and the blocker is that the idle HLE card never gets rung (start-net doesn't initiate an
unregistered HLE card the way it initiates the real card; the boot POSU state that makes SINTRAN
initiate it is not reproduced). That is defect B's remaining engineering piece. The net-server RT
program (the [0x0141] decision) is SINTRAN RT code, not in the loaded Ghidra programs.

## *** CORRECTION 2026-08-11 - the previous "CONFIRMED ROOT CAUSE" below is WRONG ***

A clean side-by-side of the SAME boot's device logs (oracle `oracle-startnet-device.txt` vs HLE
`retrocore-hle-dram/run-51188/hle-startnet-device.txt`, identical command ladder) disproves the
"XSNET bring-up diverges" story. VERIFIED, not inferred:

- The XSNET (start-net-server) record is BYTE-IDENTICAL in both: XFWRI/XFREA-CONTENT
  `[0x0255][0x0006][0x0104][0x0064][0x0271][ENNS0...]` (magic 0x00640271 = port 4 in BOTH).
- The XSNET REPLY is byte-identical: `[0x0200][0x0008][0x0102][0x0003][0x0202][0x0001]` = state 3
  (CONN), link 1. So start-net-server COMPLETES the same way and prints "ENNS0 started, sysid 9800"
  on both consoles. The XSNET call is NOT where they diverge.
- NEITHER card ever receives a real message on its XMSG port 4: EVERY XFRRE PIL=12 returns the
  empty marker X=0xE97C (oracle 6 total in the whole boot, HLE 2236). So DEF-NETWORK-CONN
  registration does NOT depend on a card->ND message on port 4. The old "the card must send a
  port-4 reply" idea is dead.

THE REAL, PRECISELY-LOCATED DIVERGENCE (net-server RT program, PIL=1, right after it sends the
XSNET reply and broadcasts on its own port 1):
```
oracle: XFWRI XFSCM XFRRE XFRRE XFWRI XFSND(port1->0xFFFFFFFF)  XFRCV port1  XFRCV port1  XFRCV port5  XFREA XFREL
                                                                 (X:0001)     (X:25B8 !!)   (X:0000)
HLE:    XFWRI XFSCM XFRRE XFRRE XFWRI XFSND(port1->0xFFFFFFFF)  XFRCV port1  ------------  XFRCV port5  XFREA XFREL
                                                                 (X:0001)                   (X:0000)
```
After broadcasting on port 1, the oracle net-server receives TWO messages on port 1 - the first
returns X:0001 (same on both), the SECOND returns a real message (X:0x25B8). The HLE receives only
the first, then bails straight to the port-5 receive. **That missing SECOND port-1 message is what
registers ENNS0 in the network-server table (LIST-NETWORK-SERVERS).** Confirmed symptom: oracle
LIST-NETWORK-SERVERS shows `ENNS0 9800 1 LAN 4 6 5 2`; HLE LIST-NETWORK-SERVERS is EMPTY -> XRUNN.

CAUTION - not yet pinned: in the oracle's 33 ms gap before the 2nd port-1 receive there is NO card
activity (only the MFP timer), so the 2nd message was ALREADY QUEUED, not generated on demand.
Origin of that 2nd port-1 message (card-queued earlier vs XROUT-internal) is the NEXT thing to
find. Do NOT edit card code until that origin is proven - editing on an unproven chain is the
repeated failure mode in this file.

SECOND, PROBABLY-RELATED DEFECT (verified counts): the HLE raises a SCIP level-12 storm - 2236
empty XFRRE vs the oracle's 6 - because `MboxhReplay`'s looping reactive drain
(`NDBusEthernetIIHle.cs` ~line 466, the `_loop` branch) re-posts an XFRRE AND raises SCIP on every
ND->card doorbell, and the ND-100 rings that doorbell after servicing each empty XFRRE. Self-feeding
loop. The real 68K raises SCIP only when it has genuine data. This may be the same underlying
bug (card never produces the real bring-up message, so it just spins) or a separate one.

### ARCHITECTURAL FINDING 2026-08-11 (CORRECTED counts + oracle-validated root cause)

FIRST, A CORRECTED MEASUREMENT (an earlier draft said "HLE = 0 ND->card doorbells" - WRONG, that
grepped for the 68K-only "GPIP I6" log line the HLE never emits). The real, apples-to-apples
counts over the whole boot (run-51188 vs oracle):

| signal                                   | oracle (real 68K) | HLE   |
|------------------------------------------|-------------------|-------|
| ND->card doorbell (ndint control writes) | 34                | 2354  |
| ND->card doorbell delivered to 68K (vec 0x4E) | 44           | n/a (HLE has no 68K) |
| card->ND SCIP                            | 80                | 4705  |
| XFRRE PIL=12 (ALL empty = 0xE97C)        | 6                 | 2236  |

The HLE does NOT ignore doorbells - the ND rings it 2354 times BECAUSE it is reacting to the
card's own 2236 self-posted XFRRE: card posts XFRRE(port4) on a timer -> SCIP -> ND services it
(empty) -> ND acks with a `0x0005` ndint write -> repeat. A self-feeding loop the CARD starts.

ROOT CAUSE (validated against oracle truth + firmware RE POCSPROCES-EVENT-DISPATCH-2026-08-10.md):
the real card's port-4 receive is EVENT-GATED. Its 68K posts an XFRRE(port4) mailbox element
(oracle line 15011, `NXFNC func=0x4029`) ONLY when POCSPROCES finds a pending-event bit set
(bitmask @0x1E1CA); that is why 44 doorbells yield only 6 XFRRE, all clustered in active windows
(4 during start-net, 2 later) with 80 s of TOTAL SILENCE between. The HLE's `DriveConnAcceptServer`
Recv (NDBusEthernetIIHle.cs ~1196) instead posts XFRRE(port4) on the `CA_SETTLE_TICKS`(=50000)
TIMER unconditionally (re-park at line 1314) - a poll, not an event response. THAT timer-poll is
"the HLE not doing the right thing." It runs from 41 s before start-net (first at 22:55:51, right
after burst-1) and never goes quiet.

DISPROVEN (experiment 2026-08-11, boot run-81412): throttling the empty re-park 40x cut the storm
2236->59 (pre-XSNET 426->16, near the oracle) but DEF-NETWORK-CONN STILL failed and the port-1
loop (PC 0x599) still ran once. So the storm volume is NOT what blocks registration - reverted the
throttle as a bandaid (user: fix the root cause, do not paper over it). The port-4 storm (A) and
the missing-2nd-port-1-message registration bug (B) are likely SEPARATE defects.

THE FIX (root cause of A) - DISCRIMINATOR RESOLVED 2026-08-11 (oracle correlation):
Every ND->card doorbell enters the 68K at 0x250E, but only ~1 in 6 proceeds to 0xBED8 and posts an
XFRRE(port4). The ND writes the IDENTICAL mailbox `REQUEST=0x0000 SUBFUNCTION=0x0005` before EVERY
doorbell (oracle lines 15000/15046/15097/.../15657) - so there is NO ND-observable discriminator.
The firmware decides INTERNALLY (POCSPROCES event state) whether a doorbell posts an XFRRE. The
decisive fact: the real card posts XFRRE ONLY while doorbells are arriving (active ND windows: 4
during start-net) and is SILENT for 80 s of idle because the ND rings it ZERO times then.

So the HLE's real defect is that its port-4 receive is driven by `OnClock` (self-timed, every
tick) instead of by the ND doorbell. It fires continuously even when the ND is idle, and each
self-post SCIPs the otherwise-idle ND awake -> ND services (empty) -> ND acks `0x0005` -> the
card's OnClock posts again. The 426 pre-XSNET storm XFRRE all happen during DEF-REMOTE typing when
the real card would be SILENT (no doorbells). FIX: drive `DriveConnAcceptServer`'s port-4 XFRRE
from `OnNdInterruptStrobe` (the ND doorbell), NOT from the `OnClock` tick loop (call site
NDBusEthernetIIHle.cs ~1800). Handle the self-ack: after an empty XFRRE do NOT re-post on the
immediate ack-doorbell it generated - only on a FRESH doorbell from new ND activity - so idle is
truly silent like the oracle. Start-net + conn-to still get their letters because those arrive AS
doorbells. NOTE: experiment already proved this will likely NOT fix DEF-NETWORK-CONN (defect B, the
missing 2nd port-1 message) - it fixes the storm (defect A), which is what the user asked for.

### FIX A IMPLEMENTED + VERIFIED 2026-08-11 (boot run-129064)

Made the HLE port-4 receive EDGE-DRIVEN instead of an OnClock self-poll. Changes in
`NDBusEthernetIIHle.cs`: two flags `_caReceiveArmed` / `_caAckPending`; `OnNdInterruptStrobe`
arms one receive per FRESH ND doorbell and SWALLOWS the single ack-doorbell that servicing our own
empty XFRRE raises; `DriveConnAcceptServer` Recv posts only when armed (else returns = silent); the
empty re-park no longer sets `CA_SETTLE_TICKS` (no timer re-post). Reset flags cleared in both card
reset paths.

RESULT (run-129064 vs before vs oracle):

| signal              | before | AFTER FIX | oracle |
|---------------------|--------|-----------|--------|
| XFRRE PIL=12 total  | 2236   | **2**     | 6      |
| card->ND SCIP       | 4705   | **30**    | 80     |
| XFRRE before XSNET  | 426    | **1**     | 2      |

The idle storm is GONE - the card is now silent like real hardware (even quieter). start-net still
completes ("Network server ENNS0 started, sysid 9800"); DEF-NETWORK-CONN D100 still correctly
refused (local). As predicted, DEF-NETWORK-CONN D102/etc STILL fail - defect A (storm) and defect B
(missing 2nd port-1 message) are independent; this fix is A only. All port-4 XFRRE are empty in
both cards, so posting fewer is harmless.

CONN-TO REGRESSION EVIDENCE: (1) single-node boot passes and start-net's accept/directory path
runs to completion; (2) the fast HLE accept-path unit tests pass 9/9 (Nd100EthIIHleStartNetReply +
Nd100EthIIHleConnAcceptDescriptor + Nd100EthernetIIHleHubJoin) - these verify the accept/reply
CONSTRUCTION, which my change does not touch (I gated only WHEN the Recv posts, not what an accept
does). The two-node Nd100TwoNodeEthernetIIHleHarnessTests CRASHED the test host - but that is the
DOCUMENTED two-boots-per-process limitation (skill nd-ethernet-ii sec 8: "a second ND100Machine
boot in the same process ... the test host crashes"), a hard process abort, NOT an assertion
failure my accept-path change could produce; it crashes at baseline too and cannot positively
verify conn-to in this harness either way. Rationale my change is conn-to-safe: conn-to letters
arrive WITH ND traffic that rings the card (doorbells), which arms the receive - and the real card
catches conn-to with only 6 doorbell-driven XFRRE, so doorbell-driven is provably sufficient.

STATUS: defect A (the storm) is FIXED, root-caused, oracle-validated, and verified. Defect B
(DEF-NETWORK-CONN still returns "Unknown name" = the missing 2nd port-1 message, PC 0x599) is
untouched and is the next target if the goal of a working DEF-NETWORK-CONN is to be reached.

### DEFECT B ROOT CAUSE FOUND 2026-08-11 (oracle-validated, same-scenario LAN ,,,N)

Method: added an XFRCV RETURN capture to the shared XMSG handler (MON_200_XMSG.cs, the same
`MonRetArmed` deferred-log mechanism XFMST already uses - UNCOMMITTED diagnostic, revert when done)
and booted BOTH the HLE (Boot_Login_EnnS0_DumpHleDram, run-15672) and the real 68K oracle on the
SAME LAN ,,,N recipe (Nd100EthernetIIOracleDramDumpTests.Boot_EnnS0_Lan_CaptureOracleBurst2). NOTE:
the DEFAULT oracle DumpOracleDram test runs WAN ,,,Y (D2XX) - do NOT compare it to the HLE; use the
Boot_EnnS0_Lan_CaptureOracleBurst2 LAN test for an apples-to-apples device log.

ROOT CAUSE: the HLE never sends the `[0x0141]` XSLET "*XM-ENNS0" op-1 SERVER-REGISTRATION letter.
- ORACLE: at PIL=1 it XFWRIs a 28-byte letter `[0x0141][0x0018][0xFF09]"*XM-ENNS0"[0xFD05]"ENNS0"
  [0x0A02][0x0001]...` and XFSNDs it FROM PORT 4 to XROUT (port 0) (oracle log lines 5588/5590).
  XROUT routes it to the net-server, whose port-1 XFRCV then RETURNS it (T=0x0001 A=0x2271 D=0xE385
  X=0x001C = 28 bytes); the net-server processes it -> ENNS0 enters the network-server table ->
  LIST-NETWORK-SERVERS shows `ENNS0 9800 1 LAN 4 6 5 2` -> DEF-NETWORK-CONN works.
- HLE: grep for `[0x0141]` = ZERO hits in the whole boot. The HLE DOES do XSNAM (names port 4
  "*XM-ENNS0", burst1 "registration sent") and DOES send `[0x0149]` XSDRN letters (the DEF-REMOTE
  definitions D100=0x0064 etc.), but NEVER the `[0x0141]` op-1 activation letter. So the net-server's
  port-1 XFRCV NEVER returns a real message (0 XFRCV(port 1) RET lines all boot; the same
  T=0x0001 A=0x2271 message shows up truncated to 2 bytes on PORT 5 instead) -> ENNS0 never registers
  -> LIST-NETWORK-SERVERS EMPTY -> "Unknown name". Storm fix (defect A) did NOT change this.

KEY NUANCE: the `[0x0141]` send is PIL=1 (the ENNS0 RT program, auto-started by start-net-server),
NOT PIL=12 (the card). The ND-100 ENNS0 RT code is IDENTICAL SINTRAN in both boots, so the RT
program's decision to SEND the `[0x0141]` letter must be gated on some CARD state/report the oracle
card provides and the HLE card does not. NEXT: find what card-provided state gates the ENNS0 RT
program's `[0x0141]` send (trace the ENNS0 RT program, or diff the card->ND reports the RT program
reads before that send, oracle vs HLE). The exact letter bytes to reproduce are captured above.

### DEFECT B - THE [0x0141] GATE NARROWED 2026-08-11 (net-server send-port divergence)

Diffing the net-server RT program's (P=0x74CD, same in both) PIL=1 XFSND sequence, oracle vs HLE:
- ORACLE: alternates `Sending port: 4` / `Sending port: 1` each poll cycle - the port-4 send is the
  `[0x0141]` "*XM-ENNS0" registration, re-sent every ~5-6 s.
- HLE: alternates `Sending port: 1` / `Sending port: 5` - it NEVER sends from port 4 (0 PIL=1
  port-4 sends all boot), and never emits `[0x0141]`. Its non-port-1 send is port 5, carrying the
  `[0x0149]` XSDRN DEF-REMOTE letters + a `[0x0100]` record.

So the net-server sends its server-registration/directory traffic FROM PORT 4 on the oracle but
FROM PORT 5 on the HLE, and only the port-4 path emits the `[0x0141]` activation letter. The gate
is this send-port assignment. NOTE this is the NET-SERVER's OWN sending port, NOT the card's
*XM-ENNS0 named port (both cards name *XM-ENNS0 on port 4 per LIST-SERVERS) - do not conflate with
the earlier "port 4-vs-5" red herring, which was about the card's port.

NEXT: find why the net-server RT program opens/uses port 4 on the oracle but port 5 on the HLE
(trace its XFOPN / port-allocation, or what magic/port the card hands it) - that allocation is what
gates the `[0x0141]` send. Likely tied to the ORDER/values of port allocation between the card's
XSNAM(*XM-ENNS0) and the net-server's own port open. The diagnostic (XFRCV-return capture in
MON_200_XMSG.cs) is still UNCOMMITTED - revert when defect B is closed.

### DEFECT B - CORRECTION 2026-08-11: the PORT SWAP is NOT the root cause (delay experiment disproved it)

Two burst-1-deferral experiments (both REVERTED, tree is back to the committed storm-fix state):

1. DOORBELL defer (run-48432): armed burst-1 on the first OnNdInterruptStrobe. That never fires for
   an idle card, so burst-1 NEVER RAN (card unregistered) - but the program took port 4 and SENT
   [0x0141] (1). start-net FAILED ("Server not yet started") because no server existed.
2. TICK-COUNTDOWN defer (run-91192, 20 M ticks): a countdown ALWAYS fires, so burst-1 ran LATE.
   RESULT: the port config now MATCHES THE ORACLE EXACTLY - program on port 4 (XFM2P magic 0x0271,
   was the mismatched 0x02FE), card on port 5, start-net "Ok" + "ENNS0 started, sysid 9800". BUT
   DEF-NETWORK-CONN STILL FAILED: 7 XRUNN [0x0102] replies, 0 [0x054A] route, 0 XFRCV(port 1)
   receives, and [0x0141] = 0.

CONCLUSION - the PORT-4-vs-5 SWAP IS NOT THE ROOT CAUSE of DEF-NETWORK-CONN failing. Aligning the
ports to the oracle did NOT fix it. (The earlier "burst-1 fires too early / port swap is the root
cause" sections below are DOWNGRADED to a real-but-secondary difference.)

WHAT THE TWO EXPERIMENTS JOINTLY PROVE about [0x0141]: the ND-100 program sends the [0x0141]
*XM-ENNS0 activation letter ONLY WHEN THE CARD HAS NOT ALREADY REGISTERED *XM-ENNS0.
- exp 1 (card did NOT register) -> program sent [0x0141].
- exp 2 (card DID register via burst-1) -> program did NOT send [0x0141].
On the ORACLE, BOTH happen: the card registers *XM-ENNS0 ([0x5342] from port 5) AND the program
sends [0x0141] (from port 4). So the oracle card's registration differs from ours in a way that
does NOT suppress the program's [0x0141]. THE REAL FRONTIER: what is different about the oracle
card's *XM-ENNS0 registration ([0x5342]) - its op/flags/type - such that the ND-100 ENNS0 RT
program still emits the [0x0141] activation afterwards. Decode the oracle [0x5342] letter fields
(op selector, the trailing params) vs the HLE burst-1 [0x5342], and/or trace the ENNS0 RT program's
decision to send [0x0141]. That is defect B's true root cause - NOT the port number.

FURTHER NARROWED (byte diff, no boot): the [0x5342] *XM-ENNS0 registration letter is BYTE-IDENTICAL
oracle vs HLE: `[0x5342][0x000C][0xFF09]"*XM-ENNS0"[0x3001][0xFD05]"ENNS0"[0x0A02][0x0001][0x8C80]...`.
So it is NOT the registration content and NOT the port. What remains: on the oracle the ENNS0 RT
program, ~11 s AFTER the identical registration, sends [0x0141] (it sits idle on a blocked receive,
then a card 2-byte [0x0000] XFREA coincides with the send); on the HLE it never does. This is the RT
program's OWN internal decision, gated on input not visible at XMSG-call granularity. TRUE NEXT STEP
(different approach - the device logs are EXHAUSTED for this): trace the ENNS0 RT program's PC around
the [0x0141] send point (CpuND100.cs has a DiagInstrTrace hook), OR find/disassemble the ENNS0 RT
program, to see what it waits for and why it emits [0x0141] on the oracle but not on us. All
burst-1-deferral code + test diagnostics are REVERTED; tree = committed storm-fix state (85b712787).
Uncommitted: the XFRCV-return diagnostic in MON_200_XMSG.cs (kept - useful for the RT-program trace).

### DEFECT B - DEFERRAL EXPERIMENT 2026-08-11 (proved the mechanism; reverted, needs the right trigger)

Tried deferring burst-1 (arm on first OnNdInterruptStrobe instead of at boot self-test). Boot
run-48432 RESULT - hugely informative, so keep it:
- PROVED THE MECHANISM: with burst-1 not grabbing port 4 at boot, the ND-100 program took PORT 4
  (24 PIL=1 XFSND from port 4, was 0) and the [0x0141] *XM-ENNS0 activation letter FIRED (1, was
  0). So the port-4 allocation IS what controls whether [0x0141] is sent. Confirmed.
- BUT REGRESSED start-net: burst-1 NEVER ARMED (the "burst1 ARMED" log fired 0 times; no card
  burst1 XFOPN; card nearly dead - 4 ETHHLE-CONTROL writes vs 32 in the working run). start-net-
  server printed "Server not yet started - will try to start him now (wait 10 sec!)" then failed
  "Error in communicating with XROUT / Unknown name". So the card never registered as a server.
- WHY THE TRIGGER FAILED: OnNdInterruptStrobe never fires for an IDLE card - a deferred card raises
  no SCIP, so the ND never acks/rings it. SINTRAN DOES engage the card at start-net (the 10-sec
  wait), but via a DIFFERENT signal than the ndint doorbell.
- ALSO LEARNED: [0x0141] alone is NOT sufficient - the card must ALSO register *XM-ENNS0 AND finish
  the start-net handshake. Both are needed: (1) program on port 4 (so [0x0141] fires) AND (2) card
  registers at start-net (so the server exists).

REVERTED to the committed storm-fix state (start-net works again, DEF-NETWORK-CONN still fails).

THE REAL REMAINING PIECE (found while chasing the trigger): SINTRAN engages the HLE card ONLY AT
BOOT. In BOTH HLE runs the only ND->card mailbox writes are at boot (0x408=SUBFUNCTION: 0x00 then
0x05 self-test, + the 0x0009 OPCOM); there is NO ND->card write during start-net-server at all. So
when burst-1 is deferred, nothing ever wakes the card -> it never registers -> start-net fails.
The ORACLE is different: its card is IDLE at boot and SINTRAN RINGS it at start-net (GPIP I6
doorbell at 09:38:27, card XFOPN+register at 09:38:29) because *XM-ENNS0 is NOT yet registered so
start-net-server initiates the server bring-up on the card. The HLE registers *XM-ENNS0 at boot, so
SINTRAN never needs to (and never does) engage the card at start-net - and the HLE does not model
SINTRAN's start-net card-initiation path.

So the true fix is bigger than a trigger tweak: the HLE card must behave like the oracle at BOOT
(POSU/self-test that makes SINTRAN see an Ethernet device but does NOT register *XM-ENNS0), so that
start-net-server ITSELF initiates the card (rings it), the card THEN opens its port (after the
program has port 4) and registers, [0x0141] fires, and the server exists. NEXT: RE how SINTRAN's
start-net-server decides to ring/initiate the card (the ENNS0 RT program + the *XM-ENNS0 lookup that
triggers the "will try to start him now" path), and reproduce the boot POSU state the oracle card
presents so SINTRAN takes that initiate path on the HLE too. This is the frontier for defect B.
PROVEN so far: program-on-port-4 -> [0x0141] fires (mechanism confirmed); the blocker is making the
card register at start-net (not boot) without losing SINTRAN's engagement.

### DEFECT B - CONFIRMED ROOT CAUSE 2026-08-11: burst-1 fires TOO EARLY (XFOPN ORDER swapped)

The port swap is an ORDERING bug, proven by the XFOPN timestamps (same LAN ,,,N boots):
```
ORACLE:  ND-100 program XFOPN @ 09:37:54 (line 388)  FIRST  -> program gets port 4
         card          XFOPN @ 09:38:29 (line 2708) 35s LATER -> card gets port 5
HLE:     card burst-1  XFOPN @ 09:11:59 (line 5829)  FIRST  -> card grabs port 4
         ND-100 program XFOPN @ 09:12:10 (line 6697) 11s LATER -> program pushed to port 5
```
The REAL card opens its port + registers *XM-ENNS0 ONLY DURING start-net-server (its single
PIL=12 XFOPN is at 09:38:29, ~12 s before the XSNET record and right before the [0x0141] send).
The HLE's burst-1 fires at BOOT (right after the card reports started), 30+ s before start-net,
so it grabs port 4 ahead of the ND-100 program. Program-on-port-5 breaks the *XM-ENNS0 activation
-> no [0x0141] -> no registration. So the HLE registers *XM-ENNS0 at the WRONG TIME.

FIX: defer the HLE card's burst-1 *XM-ENNS0 registration until start-net-server actually runs
(trigger it off the start-net delivery, not off _mboxhArmed at boot), so the ND-100 program opens
port 4 first and the card gets port 5 - matching the oracle. Then re-boot and grep the device log
for a [0x0141] XFWRI + the program's [0x0149] sends coming FROM PORT 4. Burst-1 arming is in
NDBusEthernetIIHle.cs OnClock (`if (_mboxhArmed && !_burst1Done) DriveBurst1Client();` ~line 1774)
and whatever sets _mboxhArmed. CAUTION: burst-1 currently also produces the working LIST-SERVERS
*XM-ENNS0 entry - deferring it must not lose that; verify LIST-SERVERS still shows *XM-ENNS0.

### DEFECT B - PORT-ALLOCATION SWAP FOUND 2026-08-11 (oracle-validated, LAN ,,,N) [earlier same-day note]

The [0x0141] gate is a card-vs-program PORT-ALLOCATION SWAP. Traced via XFM2P (magic->port) + the
PIL=12 vs PIL=1 XFOPN/XFSND origins, HLE run-15672 vs oracle Boot_EnnS0_Lan_CaptureOracleBurst2:

- ORACLE: the real card does EXACTLY ONE PIL=12 XFOPN/XFSND in the whole boot, and it registers
  its server from PORT 5 (line 2865: PIL=12 XFSND "Sending port: 5"). The ND-100 RT program (PIL=1)
  opens PORT 4 (line 388) and sends every [0x0149] DEF-REMOTE letter AND the [0x0141] *XM-ENNS0
  activation FROM PORT 4. XFM2P uses magic 0x0271 (port 4) consistently (29 hits; 0x02FE only 3).
- HLE: the card's BURST-1 opens PORT 4 EARLY (line 5831 "burst1 XFOPN -> port 4") and registers
  *XM-ENNS0 from port 4 (line 5978, PIL=12), so the ND-100 RT program is pushed to PORT 5 - every
  XFM2P uses magic 0x02FE (port 5). The card and program have SWAPPED ports vs the oracle.

Consequence: with the RT program off port 4, XROUT never delivers the activation trigger to it,
so it never sends the [0x0141] letter, so ENNS0 never registers. (The card's port-4 registration
is a dead end for that flow.) This is NOT the old per-boot random-port red herring: it is a
deterministic ALLOCATION-ORDER swap caused by the HLE card grabbing its port via burst-1 BEFORE
the ND-100 program opens its own, whereas the real card's single port-open lands AFTER (leaving
port 4 for the program).

FIX LEVER (candidate, needs an experiment): the HLE card's burst-1 XFOPN/registration timing/port.
The real card opens its port such that the ND-100 program gets port 4; the HLE burst-1 takes port 4
first. Options to try: (a) delay/reorder the burst-1 port-open so the ND-100 program opens first;
(b) have the card open the port the real card uses. VERIFY each by re-booting and checking that the
RT program's XFM2P/[0x0149] sends come FROM PORT 4 and a [0x0141] appears (grep the device log).
Do NOT assume a fix works without the boot + [0x0141]-present check. Diagnostic (XFRCV-return
capture in MON_200_XMSG.cs) still UNCOMMITTED.

Everything from here down is the SUPERSEDED analysis - kept for history. Read the correction above first.


## Goal (one line)

On a single-node boot, after `start-net-server,enns0,,,N` reports "ENNS0 started, sysid 9800",
`DEF-NETWORK-CONN D102 ENNS0,,0,0,0,0` must return **Ok** (route defined) instead of
`Error in communicating with Network Server / XMSG Routing/Naming error: Unknown name`.

## What is already known (do NOT re-derive - read these first)

- Memory: `def-network-conn-needs-directory-peer.md` (the full corrected story).
- Analysis: `SINTRAN\XMSG\DOC\COSMOS-RE\DEF-NETWORK-CONN-ORACLE-VS-HLE-2026-08-10.md`.
- **Golden reference + parser** (THE tool to verify any fix):
  `SINTRAN\XMSG\DOC\COSMOS-RE\ennS0-exchange\` - `lle-oracle-exchange.json` (real-68K card,
  WORKS), `hle-exchange.json` (our card, FAILS), `README.md` (the one-record diff),
  `parse_ennS0_exchange.py` (re-run on any boot's device log to re-diff).

Confirmed facts:
- The real-68K oracle card SUCCEEDS single-node with a FAKE remote (D2XX=17848) and no peer -
  so this is NOT a two-node/liveness problem. Our HLE card fails the identical command.
- DIRECTLY PROVEN 2026-08-10 (boot run-112620): running the oracle's EXACT command
  `DEFINE-NETWORK-CONNECTION D2XX,ENNS0` on the HLE ALSO returns XRUNN, in the same boot as
  `DEF-NETWORK-CONN D102`. So the WAN-vs-LAN-neighbour target-type theory is RULED OUT - the
  divergence is the CARD. The XSNAM "*XM-ENNS0" registration is byte-identical to the oracle
  (both PIL=1, [0x5342][0x000C]"*XM-ENNS0", same XFMST/reply) - so it is NOT the naming call.
- `,,,N` vs `,,,Y` is irrelevant (both fail identically). `DEF-NETWORK-CONN D100` is correctly
  refused ("local defined system" - D100 IS this machine).
- The failing record: HLE answers the `0x0441` XSLET directory query with `0x0102` =
  **XRUNN "Unknown name"** (XMSG-PL-VALUES-M.INCL:248); the oracle answers `0x0400` XRSOK with
  `p17 = own-sysid (0x2648=9800)`, after which XROUT issues `0x054A` XSDSY -> `0x0500` OK and the
  route exists. HLE issues XSDSY 0x; oracle 8x.
- The `0x0441 -> reply` exchange runs ENTIRELY on the ND-100 side (PIL=1, identical SINTRAN in
  both boots). The XRUNN is a SEND-SIDE name-resolution failure by real XROUT - it can't resolve
  where to deliver the `*XM-ENNS0` letter, so nothing is ever queued to the card's port (that is
  why the card's XFRRE on port 4 is always empty - NOT a receive-plumbing bug). So the fix is
  NOT a reply-builder or receive edit; it is whatever card-registration state makes real XROUT
  resolve `*XM-ENNS0` (or the remote route) differently.

## CONFIRMED ROOT CAUSE (2026-08-10, boots run-51188 HLE + bj3fp7deg oracle ,,,N)

`DEF-NETWORK-CONN D102 ENNS0` names **ENNS0 the NETWORK SERVER** as the route-via. XROUT can't
find it -> XRUNN. Proven with LIST-SERVERS + LIST-NETWORK-SERVERS run just before the command:

- Both cards: LIST-SERVERS shows `*XM-ENNS0` on PORT 4 (the port NAME is fine, identical).
- ORACLE LIST-NETWORK-SERVERS: `ENNS0  9800  Link 1  LAN  Xnser-port 4  Xgate 6  Rcv-buff 5  Xmit 2`
  -> then `DEFINE-NETWORK-CONNECTION D2XX,ENNS0` = **Ok**.
- HLE LIST-NETWORK-SERVERS: **EMPTY**. ENNS0 never enters the network-server table.

So the HLE never completes the **XSNET (start network server) bring-up**. XSNET INPUT is identical
(magic `0x00640271` = port 4 in BOTH - so port-4-vs-5 is a RED HERRING, just per-boot load order).
The post-XSNET bring-up call sequence is byte-identical in both until one point:
```
both: XFMST XFM2P XFOPN XFP2M (XFGET XFDUB)x5 XFGET XFWRI XFSCM XFRRE XFRRE XFWRI XFSND ...
oracle: ... XFSND -> XFRCV XFRCV XFRCV  XFREA XFREL  XFGST XFGET XFSCM XFWRI XFSND ... (COMPLETES)
HLE:    ... XFSND -> XFRCV XFRCV        XFREA XFREL  XFRRE XFRRE XFRRE... (idle storm, STALLS)
```
i.e. after XROUT `XFSND`s a frame to the card, the ORACLE card sends a RESPONSE that XROUT
receives (the 3rd XFRCV) which drives the rest of the bring-up (XFGST + follow-up XFWRI/XFSND that
finish the ENNS0 network-server entry). The HLE card NEVER sends that response, so bring-up stalls
and ENNS0 is never registered. This is single-node with NO peer (the oracle needs none), so it is
a LOCAL XROUT<->card handshake, not a network round-trip.

FIX: make the HLE card send the bring-up response XROUT expects after its XFSND, so ENNS0 enters
the network-server table. NEXT EXTRACTION (not yet done): the exact bytes of that missing card
response (the oracle's 3rd received message in the bring-up window, oracle device log
`retrocore-ethii/oracle-startnet-device.txt` after the 0x0255 XSNET record ~line 15393; find the
XFSND-to-card and the XFREA-CONTENT the card's reply produces).

ELIMINATED theories (do not revisit): two-node/peer needed; port 4-vs-5; *XM-ENNS0 name cleared;
WAN-vs-LAN target type; XSNAM registration (byte-identical to oracle).

## The verify loop (once a change is made)

1. Edit `NDBusEthernetIIHle.cs` (RetroCore C# standards - no LINQ/foreach, Span, etc.).
2. Build: `dotnet build Emulated.Tests -m:1 -p:UseSharedCompilation=false` (0 errors).
3. Boot: `dotnet test --filter Nd100EthernetIIHleDramDumpTests.Boot_Login_EnnS0_DumpHleDram`
   (~4 min). Console must show `DEF-NETWORK-CONN ... -> Ok` and `List-Routing-Info` must list the
   remote.
4. Parse the new device log with `parse_ennS0_exchange.py` and diff the fresh `hle-exchange.json`
   against `lle-oracle-exchange.json` - SUCCESS = the `0x0441 -> 0x0400{p17} -> 0x054A -> 0x0500`
   block appears.
5. `dotnet build-server shutdown` and confirm no leftover dotnet/testhost/MSBuild/VBCSCompiler.

## Do NOT

- Report success without the boot + JSON diff proving it (a green build is not proof).
- Add a card-side 0x0400 reply builder before confirming the card actually receives the letter -
  today it does not (XROUT bounces the send first).
- Re-adopt any "two-node peer required" theory - the oracle disproves it single-node.
