# ND Ethernet II HLE controller - behavioural scenarios (oracle-validated)

**Goal:** recreate the CONTROLLER's real behaviour in the HLE, scenario by scenario, each validated
against the real-68K oracle. NOT an echo/replay monster: every message received is processed per its
intention and answered correctly (if the oracle answers it); every Ethernet send/receive drives the
right ND-100 side effect (SCIP / doorbell / mailbox) only where the oracle shows one.

**Oracle source:** `retrocore-ethii/oracle-startnet-device.txt` (real 68K card, LAN ,,,N boot,
2026-08-11). Validate any HLE change by re-parsing its device log and diffing the per-scenario
sequence below.

---

## CORE MODEL (the single most important fact)

The controller is a STATE MACHINE driven STEP-PER-DOORBELL. Each ND->card ring (control word 0x0005
with the NdInterrupt bit -> GPIP I6 strobe -> 68K enters 0x250E, the "message/superkick path")
advances the card EXACTLY ONE step: it issues ONE XMSG call, then SCIPs the ND-100 back and waits
for the next ring. It does NOT self-poll and does NOT echo. Whole-boot card XMSG budget is TINY:
1x XFOPN, 1x XFGET, 1x XFSND, 1x XFRCV, 1x XFREA, 6x XFRRE, 2x XFDBK, 1x XFWDF, 1x XFDUM.

ND->card control words (what SINTRAN commands the card):
- 0x0030 = Reset | Halt        (once, at INIT)
- 0x0009 = EnableScipInt | StartOpcom   (once, triggers self-test)
- 0x0005 = EnableScipInt | NdInterrupt  (35x - THE DOORBELL that advances the state machine)
- 0x0001 = EnableScipInt        (36x - ack/re-enable after servicing a SCIP; NOT a doorbell)
- 0x0000 = none

ND->card mailbox is always REQUEST=0 SUBFUNCTION=5 (46x) - a generic "run your next step" kick, NOT
a per-step opcode. The STEP the card takes is decided by the CARD's own state, not by the mailbox
value. (One REQUEST=1 SUBFUNCTION=5 at the very first start-net kick.)

---

## SCENARIO 1 - INIT (boot + self-test)

Trigger sequence (oracle 09:38:24 - 09:38:27):
1. SINTRAN writes control 0x0030 (Reset|Halt) -> card enters reset.
2. control -> out of reset; card publishes the alive signature + boot report via mailbox + SCIP.
3. SINTRAN writes control 0x0009 (StartOpcom) -> card runs its uploaded self-test, then posts the
   self-test result mail (~340 ms later on real HW) + SCIP.
HLE today: MODELLED (OnResetExit / RunUploadedSelfTest). Keep. The card stays IDLE after this - it
does NOT register *XM-ENNS0 yet (see Scenario 3 timing).

## SCENARIO 2 - IDLE (boot -> start-net)

The card does NOTHING between self-test and start-net-server. Zero XMSG calls, zero SCIP. The FIRST
ND->card doorbell of the whole boot arrives only when start-net-server runs (~09:38:27, seconds/
minutes after boot). HLE DEFECT: the HLE registers *XM-ENNS0 at boot (burst-1), which is WRONG - it
must stay idle here so the ND-100 ENNS0 program opens its own port first. (See defect B in
HANDOFF-DEF-NETWORK-CONN-2026-08-10.md.)

## SCENARIO 3 - BRING-UP / *XM-ENNS0 REGISTRATION (during start-net, step-per-doorbell)

Kicked off by start-net-server. Each line = ONE doorbell -> ONE card XMSG call (oracle 09:38:29,
~40 ms total). Values are LIVE (use the reply handles/ports, never replayed literals):

| step | card XMSG call | oracle regs (A/X)        | intention |
|------|----------------|--------------------------|-----------|
| 1 | XFDBK (0x23)  | A=0x0010 X=0x0AC7        | debug/《define callback》 - card runtime setup |
| 2 | XFWDF (0x22)  | A=0x807B                 | 《define frame/window》 - card runtime setup |
| 3 | XFDUM (0x00)  | A=0 X=0                  | dummy/no-op sync step |
| 4 | XFOPN (0x0A)  | -> opens the card's port | open the server port (gets port 5 here - AFTER the ND program took port 4) |
| 5 | XFGET (0x02)  | -> message buffer handle | get a message buffer |
| 6 | XFDBK (0x23)  |                          | debug/callback |
| 7 | XFSND (0x0C)  | from port 5 -> 0x00000000 (XROUT) | SEND the *XM-ENNS0 registration ([0x5342] "*XM-ENNS0") |
| 8 | XFRCV (0x0D)  |                          | receive the reply |
| 9 | XFREA (0x06)  | @0xE9A8 (2B)             | read the reply/ack |

Then the card goes IDLE again (only the ND program's poll runs) until ACTIVATION.

## SCENARIO 4 - ACTIVATION (net-server sends [0x0141], ~11 s later)

The ND-100 net-server (NOT the card) sends the [0x0141] *XM-ENNS0 op1 activation, XROUT routes it,
the net-server forwards it to the card (port 5) and RINGS the card (control 0x0005). The card then
drains its port with XFRRE (0x29) x6 (oracle 09:38:40+), each on a doorbell. This is what promotes
ENNS0 into the network-server table (LIST-NETWORK-SERVERS) so DEF-NETWORK-CONN resolves it.
HLE DEFECT: because the HLE registered at boot (Scenario 2 wrong), the net-server never sends
[0x0141], so this scenario never runs and ENNS0 never registers -> DEF-NETWORK-CONN "Unknown name".

## SCENARIO 5 - ETHERNET SEND (host -> card -> wire)  [needs a real-traffic capture]

Not exercised single-node (no peer). Model from the transmit path already RE'd (skill nd-ethernet-ii
sec 2): host command node -> DATASERVIC subfn 16 -> LANCE TX ring; completion threads onto the ready
ring and raises ONE SCIP. VALIDATE against a two-node or file-transfer oracle capture (TODO).

## SCENARIO 6 - ETHERNET RECEIVE (wire -> card -> SINTRAN)  [needs a real-traffic capture]

Model from RCVCOMPLETE (skill sec 3b): frame delivered into a posted RX buffer, node threaded onto
host_ready_ring, ONE SCIP. Silent drop on pool-exhaust / size-gate / LANCE error (counters only, no
SCIP). VALIDATE against a real RX oracle capture (TODO).

---

## CRITICAL REFRAME 2026-08-11 (the CARD is already correctly reproduced)

Diffing the delay-experiment run-91192 card (PIL=12) XMSG sequence vs the oracle:
```
ORACLE:    XFDBK XFWDF XFDUM XFOPN XFGET XFDBK XFSND XFRCV XFREA | XFRRE x6
DELAY-HLE: XFDBK XFWDF XFDUM XFOPN XFGET XFDBK XFSND XFRCV XFREA | XFRRE XFREA XFRRE
```
The BRING-UP (Scenario 3) is IDENTICAL. The card is reproduced correctly - correct sequence,
correct port (5), *XM-ENNS0 registered, "ENNS0 started". So the CARD is NOT the blocker for
[0x0141]. The ACTIVATION tail differs only because the net-server never sent [0x0141], so the
card's XFRRE found nothing to drain.

[0x0141] does NOT fire in EITHER HLE run (boot-card OR delay-card, both real-systems ladder), so
it is INDEPENDENT of card timing/port. The net-server (ND-100 RT, identical SINTRAN) sends it on
the oracle ~11 s after registration, just BEFORE XSNET (oracle: register 09:38:29 -> [0x0141]
09:38:40 -> XSNET 09:38:41). Candidates for why the HLE net-server skips it, in order of
likelihood: (a) TIMING - the ~11 s activation window falls AFTER XSNET/"started" on the HLE so the
net-server already finished start-net without activating; (b) the command SCENARIO differs (oracle
= D2XX WAN 17848 + DEFINE-SYSTEM-ROUTE; HLE = real LAN D102/D200/D19999) - though [0x0141] is the
*XM-ENNS0 activation and precedes DEF-NETWORK-CONN, the DEF-REMOTE set differs and runs before
start-net. DECISIVE TEST (next): delay-fix (card correct) + the oracle's EXACT D2XX command ladder
-> does [0x0141] fire and DEF-NETWORK-CONN return Ok? If yes, the card fix is sufficient and the
real-LAN-systems failure is a separate config/peer issue; if no, it is net-server timing.

## DEFINITIVE ISOLATION 2026-08-11 (D2XX test - card/ports/scenario all RULED OUT)

Ran delay-fix (card bring-up identical to oracle, ports aligned: program port 4 magic 0x0271, card
port 5) + the ORACLE'S EXACT command `DEFINE-NETWORK-CONNECTION D2XX,ENNS0` (D2XX = WAN 17848, the
oracle's winning command). RESULT (run-31664): STILL FAILS - [0x0141]=0, 3x XRUNN, 0 [0x054A]
route, 0 XFRCV(port 1). So DEF-NETWORK-CONN failure is:
- NOT the card (bring-up sequence reproduced identically),
- NOT the ports (aligned to oracle),
- NOT the command scenario (oracle's exact D2XX tested).
All three RULED OUT by direct boot.

REMAINING = TIMING/READINESS: the oracle net-server sends [0x0141] ~11 s AFTER registration, which
is the card's CONN(state 3)->RUN(state 4) transition (XSNET reply is state 3 CONN; the card must
reach RUN). During that ~11 s the oracle card does its RTC-driven readiness action (the 2nd @0xE9A8
read). The HLE card registers (CONN) but never signals RUN/readiness, so the net-server never
activates it. This needs the card to reproduce the ~11 s post-registration CONN->RUN readiness
signal (firmware RTC behaviour) - the last piece. It is a SINTRAN-RT + firmware-RTC handshake, not
a card-sequence/port/scenario issue.

## VALIDATION METHOD

For each scenario, boot the HLE, parse its device log, and diff the ORDERED (trigger -> card XMSG
call -> SCIP/response) sequence against the oracle table above. A step that is missing, extra,
reordered, or uses a replayed literal instead of a live value is a defect. The HLE must match the
oracle's step-per-doorbell progression, not produce its own volume of calls.

## HLE GAP SUMMARY (what to change)

- Scenario 2/3 TIMING: defer *XM-ENNS0 bring-up from boot to the first start-net doorbell, and make
  it step-per-doorbell through the Scenario-3 table using LIVE handles/ports.
- The card must be genuinely IDLE (no SCIP) whenever it has no queued step - the storm fix
  (committed 85b712787) already made the port-4 receive edge-driven; extend that discipline to the
  whole card.
- OPEN: what makes SINTRAN ring the idle card at start-net (Scenario 3 trigger) - the boot POSU
  state the real card presents. Without it the deferred card never gets its first doorbell.

## THE TRIGGER, FOUND 2026-08-11 (control-0x0030 reset timing)

The controller's WHOLE bring-up (reset -> self-test -> *XM-ENNS0 register) is triggered by SINTRAN
writing control 0x0030 (Reset|Halt) AT START-NET, and SINTRAN issues that reset ONLY when the card
is not already a started server:
- ORACLE: 0x0030 appears EXACTLY ONCE, at 09:38:24 - AFTER the DEF-REMOTE [0x0149] phase
  (09:37:54-09:38:20, program already on port 4) and just before the card bring-up (09:38:29) and
  XSNET (09:38:40). So start-net-server RESETS + re-inits the card -> card opens port 5 (program
  already has port 4).
- HLE: 0x0030 appears ONCE, at BOOT (09:11:58), before DEF-REMOTE (09:12:10). The card brings up at
  boot and grabs port 4. At start-net SINTRAN does NOT write 0x0030 (verified in the doorbell-defer
  run-48432: the only control writes are the boot reset/opcom; NOTHING at start-net, even though the
  console printed "Server not yet started - will try to start him now, wait 10 sec").

So the HLE card must NOT bring up at boot. It must stay halted/idle until SINTRAN issues the
start-net 0x0030 reset, then run the Scenario-3 bring-up (getting port 5). REMAINING SINTRAN-side
question: on the HLE, start-net-server printed "will try to start him now" but issued NO 0x0030 and
NO card write - so SINTRAN decided not to (or its start path targets something the HLE boot state
does not present). Reproduce the exact boot POSU/device state the real card presents so SINTRAN's
start-net-server takes the "reset + start the card" path (the 0x0030 at start-net) instead of
believing the server is already up. That is the single remaining unknown for defect B; everything
else (the step-per-doorbell bring-up sequence, the port-5 result, the [0x0141] activation) is
oracle-mapped above.

## CORRECTION 2026-08-11 (reg-deferral alone is NOT the fix - run-48432 refutes it)

The section above implies: make the HLE not register *XM-ENNS0 at boot, and START-NETWORK-SERVER
will then take the reset path. RE-READING run-48432 (the doorbell-defer run) REFUTES that:
- In run-48432 the card DID defer, so *XM-ENNS0 was NOT registered, and the console DID print
  "Server not yet started - will try to start him now (wait 10 sec!)" = the XROUT name-query MISS.
- Yet SINTRAN STILL issued NO 0x0030 at start-net (only the boot reset/opcom writes appeared).

So the name-query miss (which reg-deferral produces) is necessary but NOT sufficient. The card
reset (PWCR=60B / 0x0030) is NOT issued by START-NETWORK-SERVER @0o50722 - that handler only
issues XSNET=85 to XROUT and prints the retry message. The reset is done by the ENNS0 RT program's
POSU (PISTA), and on the HLE that POSU is NOT being (re)triggered at start-net. So deferring
registration on the card side would just reproduce run-48432: query miss, "wait 10 sec", no reset,
hang.

## THE ACTUAL FRONTIER (confirmed by the START-NETWORK-SERVER carve 2026-08-11)

Two linked SINTRAN-RT questions, BOTH upstream of the card:
1. What (re)triggers the ENNS0 RT POSU to run PISTA (0x0030 reset + PRKEY poll + MPIOC start) at
   start-net time - i.e. what turns the XSNET=85 / "will try to start" into an actual card reset.
   On the oracle the 0x0030 fires at start-net; on the HLE it does not, even with the name unresolved.
2. What sends the [0x0141] *XM-ENNS0 op1 ACTIVATION ~11s after registration (it PRECEDES XSNET, so
   it is NOT caused by XSNET). This activation is what populates the network-server table that
   DEF-NETWORK-CONN reads. Earlier RE tied the ~11s to the card's RTC-driven 2nd @0xE9A8 read.

Both are SINTRAN-RT / XROUT / net-server side (ND-100), not the card sequence. The card bring-up,
port allocation, and *XM-ENNS0 registration are already reproduced identically to the oracle
(CRITICAL REFRAME above). Carving these two triggers is the next step; a card-side code change
without them will not move DEF-NETWORK-CONN.

## MECHANISM FULLY MAPPED 2026-08-11 (get-magic XSLEK/XSLET -> XSNET/QNSIN)

From the golden oracle log (ennS0-exchange/lle-oracle-exchange.json) + XROUT-XSNET decode:
- start-net sends [0x0154] XSLEK (svc 84, letter+KICK) to *XM-ENNS0 at the top of start-net.
- card names its port via XSNAM [0x5342] (registration).
- ~12.4s later (2 poll cycles of the measured ~5.4s RT poll) the net-server RT (PIL=1, P=0x74CD)
  sends [0x0141] XSLET (svc 65, letter no-kick) to *XM-ENNS0 = the GET-MAGIC letter; its reply
  [0x0100] carries the card's magic (0x0064/0x02FE = port 5).
- 48ms later start-net issues XSNET [0x0255] with that magic; RSNET @0o13753 -> QNSIN @0o14430
  INSERTS the net-server table row (state 3 CONN). LIST-NETWORK-SERVERS reads it via QNFND @0o15141;
  DEF-NETWORK-CONN resolves against the same table.

So: XSNET/QNSIN is the literal table writer; [0x0141] is the precondition (proves the server alive
and returns the magic). Without [0x0141] the entry is not usable/visible even if XSNET "OK".

## ROOT CAUSE (best-supported 2026-08-11): *XM-ENNS0 not resolvable at start-net (XRUNN)

The failing HLE reports XRUNN (Unknown name) x3 at start-net. Per XROUT-XSNET-XRUNN decode, XRUNN=2
means *XM-ENNS0 is NOT in the XROUT name table (CHNAM @0o20161 miss) at the moment start-net queries
it. On the oracle the name is registered FRESH at start-net (card reset at start-net -> bring-up ->
XSNAM on port 5). On the HLE the card registered at BOOT on port 4, and by start-net that boot
registration does not satisfy the net-server query -> XRUNN -> no [0x0141] -> no XSNET-usable magic
-> empty net-server table -> DEF-NETWORK-CONN "Unknown name".

## THE NEXT STEP IS A LIVE PC TRACE (not more doc-carving)

Both carves + every doc converge: the exact instruction/timer that gates [0x0141], and precisely why
the boot-time XSNAM does not resolve at start-net, are only answerable by a LIVE PC TRACE of the
ND-100 net-server RT program (P=0x74CD) across the start-net window - watch the XROUT name table
(CHNAM), the RT poll loop, and the card control writes. Candidates the trace must decide between:
(a) the boot XSNAM registration is stale/expired by start-net; (b) it is on the wrong port (4 vs 5)
so the net-server's magic/port expectation misses; (c) the net-server engages the card at start-net
via the PIOCM start doorbell (11B = 0x09, NOT the 0x0005 ndint) and the boot-registered card never
re-registers on the fresh port. Doc-carving cannot decide these; the trace can.

## LIVE-TRACE BREAKTHROUGH 2026-08-11 (run-38528, current committed state + receive fix)

Two coupled defects, one now fixed:

DEFECT #1 DELIVERY - FIXED. The committed storm-fix made the card's port-4 listen edge-driven on
the hardware ndint doorbell (0x0005). But a letter XROUT merely ROUTES to the card's port (the
DEF-NETWORK-CONN [0x0441] letter) is delivered via the XMSG SOFTWARE wake and rings NO doorbell,
so the card stayed silent after "ENNS0 started" (run-100564: last card activity right after XSNET,
then nothing; the [0x0441] letter routed to port 4 was never received; command XFRCV(port5) waited
forever = hang). FIX: added a gentle standing fallback re-poll to the Recv listen state
(CA_IDLE_REPOLL_TICKS=300000 ~0.4s, ~2-3 posts/s vs the ~1000/s storm) in NDBusEthernetIIHle.cs.
VALIDATED run-38528: the card now receives [0x0441] (nbytes 0x28), builds the accept, and the
requester receives it positively (XFRCV(port5) -> T=0x0001). No more hang.

DEFECT #2 WRONG REPLY - REMAINING. With #1 fixed the card answers [0x0441] with the GENERIC conn-to
accept {01 02 0000}{02 02 000A} sent to the requester. That is NOT what defines a route. Per
DEF-NETWORK-CONN-ORACLE-VS-HLE-2026-08-10.md the winning reply is an XSDSY:
  card XFWRI [0x054A][len]{0x0102:remote}{0x0202:own-sysid} -> XFSND to port 0 (XROUT, 0x00640000)
  -> XROUT replies [0x0500] OK -> X-C prints Ok, route defined.
run-38528 confirms the HLE emits ZERO [0x054A]/[0x0400]/[0x0500] - it never sends the XSDSY, so no
route -> "Unknown name". The target system is carried in the [0x0441] letter as INT param 0x0B
(0x0B02 0x0066 = 102 = D102); own sysid = 9800 (0x2648). The XSDSY goes to XROUT (port 0), NOT to
the requester (the generic accept goes to the requester). FIX: classify [0x0441] op2 as a
route-define request (not a conn-to letter), extract the target from param 0x0B, and emit
[0x054A]{target}{9800} to port 0. BuildStartNetDirectoryReply(0x054A,...) already builds the record;
the SnRead/SnMst/SnSend states exist but currently (a) don't accept the 0x0441 header and (b)
address the sender magic, not XROUT port 0 - both need adjusting for this path.

## DEFECT #2 - XSDSY NOW EMITTED, XROUT REJECTS IT AS UNPRIVILEGED (XRPRV) - run-62644

Implemented the route-define path: a [0x0441] XSLET op2 letter is now classified as a route-define,
read in full (SnRead), its target system pulled from INT param 0x0B (0x0066=102), and the card emits
XSDSY [0x054A]{0x0102:102}{0x0202:9800} to XROUT port 0. VALIDATED run-62644:
  - card: RECV route-define -> target=0x0066 op=2 -> XFMST -> XSDSY sent (ISTAT=0x0001).
  - XROUT INGESTS it: XFREA [0x054A][0x0008][0x0102][0x0066][0x0202][0x2648] = "define 102 via 9800".
  - "Unknown name" is GONE (was 3x XRUNN before this fix).
BUT XROUT replies [0x050A], NOT [0x0500] OK. Decoded: XR status 0x0A = XRPRV (XMSG-SYMBOL-LIST.SYMB
XRPRV=000012 octal = 10 = 0x0A), a PRIVILEGE error. XROUT understood the XSDSY but rejected it as
unprivileged, and XFSND'd the [0x050A] reply to 0xFFFFFFFF (broadcast).

ROOT of the privilege gap: the oracle builds its XSDSY from XFMST(A=0xFFFF) on the KERNEL-sent
[0x0400] directory query (privileged context). The HLE shortcut edits the [0x0441] command letter in
place (XFSCM(letter-handle) -> XFWRI -> XFSND), so the XSDSY inherits the COMMAND PROGRAM's context
(XFMST returned A=0x0064 D=0x0271 = port 4, the requester), which XROUT treats as unprivileged ->
XRPRV. So the [0x0400] kernel directory query is NOT optional - it is what gives the XSDSY its
privilege. REMAINING: either (a) reproduce the trigger that makes the kernel send the card the
[0x0400] directory query (privileged), and reply XSDSY in THAT context; or (b) find how the card can
assert server/kernel privilege on an XSDSY sent outside a kernel query (XFSND option / identity /
fresh message from the server port rather than editing the command letter). Card-side XSDSY compose
is proven correct; only the privilege/trigger context remains.

## DEFECT #2 - CARD NOW MATCHES ORACLE, BUT XFMST RETURNS EMPTY (run-122404)

Reworked [0x0441] handling to the ORACLE-VERIFIED shape: card does XFRRE -> XFMST-accept -> re-park,
sends NO reply (the privileged ND-100 command program builds the XSDSY itself). VALIDATED: the card
now does exactly XFMST-accept + re-park, and "Unknown name" is GONE (was XRUNN/XRPRV before).
BUT the route still isn't defined ([0x0400]=[0x054A]=[0x0500]=0). Root cause, from the device log:
  - ORACLE card XFMST(A=0xE385) -> T=0x0009 A=0xE385 D=0x0271 X=0x0028  (requester magic + nbytes)
    -> XROUT then delivers a [0x0400] ack to the command's port -> command builds XSDSY -> [0x0500] OK.
  - HLE   card XFMST(A=0xE385) -> T=0x0009 A=0xE385 D=0x0000 X=0x0000  (EMPTY)
    -> XROUT delivers NOTHING -> command XFRCV(its port) -> T=0x0000 -> never builds the XSDSY.
So the card's XFMST on the received [0x0441] letter returns empty magic/nbytes instead of the
requester's, so the kernel does not propagate the secure-letter ack ([0x0400]) to the requester.
Likely the handle/message metadata is not retrievable via XFMST after the gentle-poll XFRRE (the
handle 0xE385 may be stale, or the poll-driven receive leaves the message without the metadata XFMST
needs). NEXT: make the card's XFMST on the [0x0441] letter return the requester magic + nbytes like
the oracle (thread the LIVE handle from the XFRRE reply; ensure the poll-received message is fully
established before XFMST). PROGRESSION this session: hangs -> "Unknown name" -> XRPRV -> card matches
oracle (XFMST-accept), one XFMST-metadata issue from the command building the route.

## DEFECT #2 - XFMST NOW RETURNS MAGIC, BUT XROUT STILL WITHHOLDS THE [0x0400] ACK (run-125468)

Routed [0x0441] through SnRead(XFREA) -> SnMst(XFMST) -> drain/re-park (op2, no remote). VALIDATED:
the card now XFREA's the full 40-byte letter and its XFMST returns the requester magic 0x006402FE
(run-122404's bare XFMST returned EMPTY - so XFREA-before-XFMST fixed that). The card sends no reply
and re-parks, matching the oracle card. BUT [0x0400]=[0x054A]=[0x0500]=0 still: XROUT does NOT deliver
the [0x0400] directory ack to the command after the card's XFMST-accept, so the command never builds
the XSDSY -> no route.

So: card behaviour now matches the oracle (XFRRE/XFREA -> XFMST-accept -> re-park, XFMST returns the
magic), yet the kernel/XROUT does not return the secure letter to the requester as the [0x0400]
positive directory reply the oracle produces. The oracle command's XFRCV(its port) gets T=0x0001 + a
[0x0400] (describing server sysid 9800, op1); the HLE command's XFRCV gets T=0x0000. The [0x0400] is
XROUT-generated (it carries the SERVER's sysid, not the letter echoed), triggered by the server
accepting the secure letter. REMAINING FRONTIER: what in XROUT/the secure-letter path makes the
kernel emit the [0x0400] positive reply to the requester when the server XFMST-accepts - and why the
HLE poll-received letter does not trigger it (candidate: the MBOXH poll-XFRRE receives the letter in
a way that does not register the secure-accept/return context, so XFREA is needed for XFMST metadata
but the secure-return still never fires). This is a MBOXH-transport / XMSG secure-letter-semantics
issue, deeper than the card state machine. SESSION ARC: hangs -> "Unknown name" -> XRPRV ->
XFMST-empty -> XFMST-returns-magic; DEF-NETWORK-CONN still not "Ok" - the [0x0400] ack is the last piece.

## NEXT-ATTEMPT LEAD: card emits the [0x0400] directory reply (not generic accept, not XSDSY)

Evidence: run-38528 (generic accept) DID deliver a positive reply to the command (T=0x0001) but with
the WRONG content {01 02 0000}{02 02 000A} -> "Unknown name". The oracle command gets T=0x0001 with
[0x0400] DIRECTORY content instead. So the promising next attempt is: the card replies to [0x0441]
with a positive reply (same XFSMC accept transport: XFSCM(handle) -> XFWRI(content) -> XFSND to the
requester) but the CONTENT is the [0x0400] directory record, not the generic accept params.

EXACT oracle [0x0400] content to build (from oracle-startnet-device.txt run 2026-08-11, XFREA @0x1D30):
  [0x0400][0x0024][0x1102][0x2648][0x2702][0x0001][0x0302][0x0001][0x0402][0x0000][0x2753][0x3000][0x0A02][0x0001]
  = header 0x0400 (serial 4, status OK) | len 0x24 | param0x11=0x2648(9800 card sysid) | param0x27=1 |
    param0x03=1 | param0x04=0 | param0x27(str "0") | param0x0A=1(op1)
Then the command program takes that [0x0400], builds XSDSY [0x054A]{102}{9800}, sends to XROUT ->
[0x0500] OK -> "Ok". (On the oracle XROUT auto-generates this [0x0400] from the card's bare XFMST-
accept; on the HLE it does not, so the card must supply it explicitly - test whether a card-sent
[0x0400] is accepted the same as an XROUT-generated one.) This is the concrete next code change:
add a route-define reply builder that writes the [0x0400] record and XFSNDs it to the requester magic
(captured by SnMst's XFMST = 0x006402FE), analogous to BuildStartNetDirectoryReply/ReplyDescriptors.

## SOLVED 2026-08-11 - DEF-NETWORK-CONN RETURNS "Ok" ON THE HLE CARD (run-64680)

The card now answers a [0x0441] DEF-NETWORK-CONN letter with: XFRRE (gentle poll) -> XFREA (establish
the message) -> XFMST-accept (get requester magic) -> XFSND the [0x0400] DIRECTORY reply (reproduced
verbatim from the oracle, ownSysid stamped) back to the requester. The command program reads the
[0x0400], builds the XSDSY [0x054A]{target}{9800} to XROUT, and XROUT replies [0x0500] OK.

VALIDATED on a real SINTRAN boot (Boot_Login_EnnS0_DumpHleDram):
```
DEF-NETWORK-CONN D102 ENNS0   -> Ok
DEF-NETWORK-CONN D200 ENNS0   -> Ok
DEF-NETWORK-CONN D19999 ENNS0 -> Ok
DEF-NETWORK-CONN D100 ENNS0   -> "cannot make ... for the local defined system" (correct: D100 local)
List-Routing-Info: 102/200/19999 each  L: *->WAN?->*  (routes DEFINED; WAN? = unconfirmed, no peer)
```
Device log: [0x0400] x12 -> [0x054A] x6 -> [0x0500] x6. Fast descriptor unit tests 8/8 pass.

TWO defects fixed (both in NDBusEthernetIIHle.cs, uncommitted):
1. DELIVERY: the committed storm-fix made the port-4 listen edge-driven on the ndint doorbell, but an
   XROUT-routed letter uses the XMSG software wake (no doorbell). Added a gentle fallback re-poll
   (CA_IDLE_REPOLL_TICKS=300000, ~2-3/s, far below the ~1000/s storm) so routed letters are received.
2. ROUTE-DEFINE: [0x0441] is NOT a conn-to letter (generic accept -> "Unknown name") and the card must
   NOT build the XSDSY itself (unprivileged -> XRPRV). The card XFMST-accepts and XFSNDs the [0x0400]
   directory reply; the PRIVILEGED command program builds the XSDSY. Requires XFREA before XFMST (a
   bare XFMST on the poll-received letter returns empty magic).

Session arc: hangs -> "Unknown name" -> XRPRV -> XFMST-empty -> XFMST-returns-magic -> [0x0400] reply
-> "Ok". Remaining polish (optional): the routes show WAN? (unconfirmed) single-node, same as the
oracle; confirming a route would need a real peer.
