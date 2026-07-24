# The make-runnable trigger for ENNS0's first superkick - and the fix (2026-07-23)

Final link in the ND Ethernet II "ENNS0 hangs in INPUT wait on LU 2240B" investigation. This
doc answers ONE question the prior docs left `[OPEN]`: **what should generate the make-runnable
event that makes the 68K firmware post ENNS0's FIRST superkick, and is the gap (1) a missing
ND->68K follow-up kick, (2) network-RX-gated with no peer, or (3) something in the external
read routine 0146547B?**

Legend: `[V]`=VERIFIED (read the NPL bytes / read the emulator source / prior live trace on
record) - `[I]`=INFERRED - `[OPEN]`=not statically decidable, needs the RTCOMMON binary or DAP.

Sources read THIS pass:
- `SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL` (PIOCM / PISTA / PIKIC / PIWKI - the MON 255B driver).
- `SINTRAN\NPL-SOURCE\NPL\MP-P2-PIOC-DRIV.NPL` (PDRIV / PISUPER / PIWKF / DOIT / SPARK - the
  level-12 driver + superkick consumer + XMSG-box producer).
- `SINTRAN\NPL-SOURCE\SYMBOLS\{K03,L07,M06}\*.SYMB.TXT` (address neighborhood of 0146547B).
- Emulator `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`
  (control-word bit map + GPIP-I6 / OPCOM edge handling).
- Prior docs in this folder (their LIVE traces are taken as VERIFIED evidence, not re-derived):
  `ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md`,
  `FIRST-SUPERKICK-BRIDGE-DECODE-2026-07-23.md`,
  `FIRMWARE-SUPERKICK-TRIGGER-WHY-NO-INT12-2026-07-23.md`,
  `ENNS0-LU2240B-INPUT-PATH-FIX-2026-07-23.md`.

---

## ONE-LINE ANSWER

**Decision = (2) network-RX-gated.** `[V+I]` No ND->68K follow-up kick is supposed to be sent
during ENNS0's read (so nothing for the emulator to "drop" - (1) refuted at the ND-100 level),
and the firmware posts a superkick only when its message layer runs, which - since ENNS0 issues
no PIKIC/SEND_KI - can only be driven by a **LANCE RX frame**. The isolated boot harness has no
network peer and no LANCE loopback, so no inbound frame ever arrives, the firmware's
make-runnable dispatcher (0x2562 -> 0x259A) never fires, no superkick is produced, PDRIV
dequeues nothing, and ENNS0's LU-2240B receive waits forever. **Fix = give the harness a LANCE
RX stimulus (loopback or a canned/peer inbound frame), NOT another ND-side kick and NOT an
input path in NDBusEthernetII.cs.**

---

## The kick topology - who rings the firmware doorbell, and when (decoded this pass) `[V]`

The 68K is woken by the ND-100 only through the PWCR control word (`HDEV+3`). Two encodings
matter (emulator `NDBusEthernetII.cs` control-bit map, lines 1266-1317, `[V]`):

| PWCR bits written | emulator effect | firmware entry reached |
|-------------------|-----------------|------------------------|
| bit 3 (Start OPCOM); PISTA writes `A:=11`=bits 0+3 | level-6 autovector | **0x1B00** OPCOM/START handler |
| bit 2 (ND Interrupt) = **BNDC** | GPIP I6 level assert (line 1389 `GPIO_6 = !ND_interrupt`) | **0x250E** channel scanner -> 0x2562 dispatcher |

Every place the ND-100 rings the **BNDC** (bit 2 / GPIP-I6) doorbell in the whole PIOC driver
- and there are exactly these four `[V]`:

| routine | addr | what rings it | nature |
|---------|------|---------------|--------|
| **PISTA** | 115077B | `A:=11; T:=HDEV+3; *IOXT` (start, OPCOM bit) | one-shot bring-up START |
| **PIKIC** | 115227B | `A:=PWCR BONE BNDC; T:=HDEV+3; *IOXT` | explicit MON 255B kick (RT asks firmware to do work) |
| **PIWKF** | 100022B | `A:=PWCR BONE BNDC; ... *IOXT` after setting NXRTF bit2 (RTDONE) | **RESPONSE** to a firmware RTWAK request ("I completed your wake") |
| **DOIT/SPARK** | 100644B | `A:=PWCR BONE BNDC; ... *IOXT` after `NXFNC` bit1 (XMSG DONE) | **RESPONSE** to a firmware XMSG-call request |

**Key structural fact `[V]`:** there is **NO device-read code path** anywhere in RP-P2-PIOC.NPL
or MP-P2-PIOC-DRIV.NPL that rings the firmware to "start an input transfer." The PIOC input
model is not a byte-stream device model. It is an **RT-activation (superkick) model**: the
firmware autonomously enqueues a superkick into the RPTON ring (entry: `DEMPT` occupied,
`DLEVL`!=5 = RT, `DPROC` = the RT-description to wake) and SCIPs -> INT12; **PDRIV** (100765B) ->
**PISUPER** (077554B) walks the ring and does `JPL I (XRTEN` (077657B) to schedule the RT. The
ND-100 never "asks for input" - it only (a) explicitly kicks via PIKIC, or (b) *answers* a
firmware request via PIWKF / SPARK. `[V]`

**Consequence:** the only two make-runnable feeders on the firmware side (`FIRST-SUPERKICK-
BRIDGE` doc, `[V]`) - 0x2562 (inbound message-type event: GPIP-I6 channel doorbell OR LANCE RX)
and 0x2292 (timer sweep) - are driven, for the GPIP-I6 branch, ONLY by an actual ND->68K
BNDC/OPCOM doorbell. During ENNS0's read, per the four rows above, a BNDC doorbell is rung only
by PIKIC (explicit) or as a response (PIWKF/SPARK) to a firmware request that never comes. So
after the START handshake the GPIP-I6 branch is silent unless ENNS0 itself issues a PIKIC.

## ENNS0 issues no kick - VERIFIED at the ND-100 level (refutes (1)) `[V]`

- ENNS0's own image issues no SEND_KI/PIKIC (only MON 135 / 200x2 / 322 / 124 / 125) - prior
  opcode-exact decode, `[V]`.
- The LIVE MON-call trace on record (`ROOT-CAUSE...` lines 141-143) captured the ACTUAL monitor
  calls executed **including inside the external routine 0146547B up to the block point**: MON
  135 (RTWT), MON 200x2 (XFDUM+XFDCT), MON 322 (GSGNO), MON 124 (PRSRV), the read via 0146547B,
  then MON 125 (PRLS). **No MON 255B (PIOCM) / SEND_KI wrapper appears.** `[V]`

So neither ENNS0 nor the resident routine 0146547B rings a PIKIC/BNDC doorbell before blocking.
**There is no ND->68K follow-up kick for the emulator to be dropping.** The emulator's GPIP-I6
path is, moreover, correctly wired (`NDBusEthernetII.cs` line 1389, BNDC bit2 -> GPIP I6 level)
and the START/OPCOM level-6 handshake demonstrably completes (0x1B06-0x1C60 runs live). **=> (1)
is refuted.** (The `FIRST-SUPERKICK-BRIDGE` doc's earlier "(c) PRIMARY = emulator drops the
OPCOM IPL6 edge" was superseded by the later live PC-watch showing 0x1B00 DOES run; this pass
does not revive it.)

## Why (2) is the answer, and why it is not "unusual" for ENNS0 `[V for mechanism, I for the
network-server justification]`

Given ENNS0 sends no kick, the firmware's message layer (0xBFF8 -> 0xEAA6 producer) can be
entered only via 0x2562, and the only remaining 0x2562 feeder is a **LANCE RX-complete** event
(prior firmware decode, `[V]`). ENNS0 is the COSMOS Ethernet **network server** - the receiver
side. Its steady state, and its first post-bring-up action, is to post a receive and block until
an inbound network event arrives (`[I]`, consistent with: MON 124 PRSRV force-reserving the
INPUT part of the communication device, then an INPUT read on LU 2240B). In a real installation
that first event comes from the wire (a peer node, a broadcast, or the node's own outbound
traffic returning). In the isolated harness there is **no peer and no LANCE loopback**, so no RX
frame is ever generated, 0x2562/0x259A never fire, and the receive never completes. This is a
**missing test stimulus, not a defect** in the 68K, the MFP, the OPCOM path, the vector table,
PDRIV, or the emulator's doorbell wiring.

## Routine 0146547B - identity narrowed, body still OPEN `[V for "not ENNS0", OPEN for body]`

- `[V]` `0146547B` (= 0xCD67 = 52583 dec) is **outside the ENNS0 image** (span
  001756B..073116B); it is a resident/linked routine ENNS0 JPLs into (word@030452 = 146547).
- `[V]` Symbol-table check this pass: octal 146547 has **no entry** in the K03/L07/M06
  SYMBOL-1/2 lists (these are partial primary/secondary kernel maps, so absence is not proof of
  non-residence). The nearest K03 neighbors in that region are the **resident monitor byte-I/O
  primitives MINBT=146345B / MOUTB=146361B** - i.e. 0146547B sits in, or just above, the
  resident monitor I/O primitive neighborhood on a K-era layout. L07/M06 relocate that region
  entirely, so the absolute 146547 in the RUNNING L system cannot be pinned from the tables.
- `[OPEN]` Therefore 0146547B is one of: (a) a **resident SINTRAN monitor input primitive**
  (INBT/RSIO-class device read that queues on the LU-2240B datafield and blocks), or (b) a
  **COSMOS RTCOMMON** linked-segment input routine. The LIVE trace already rules out that it
  issues a PIOCM kick (no MON 255B executed), so under EITHER reading it is a **plain blocking
  device INPUT that waits to be woken by the level-12 driver** - it does **not** itself send a
  kick to the firmware. That is the load-bearing conclusion, and it holds regardless of (a)/(b).
  Its exact identity/body needs a DAP step-in or the RTCOMMON binary.

## What wakes ENNS0's first read in a WORKING COSMOS system `[V+I]`

```
[wire] inbound Ethernet frame for ENNS0's port
   -> LANCE RX-complete  ->  68K RX handler runs the ENCOS message layer
   -> 0x2562 dispatch (registered type-code + enabled mask) -> 0x259A make-runnable
   -> message-delivery coroutine -> 0xBFF8 -> 0xEAA6 producer:
        enqueue RPTON ring entry (DEMPT occupied, DLEVL=RT, DPROC=ENNS0 RT-desc), SCIP 0xEF0180
   -> ND-100 INT12 -> PDRIV (100765B) -> PISUPER (077554B) -> RTPR -> JPL I (XRTEN
   -> ENNS0's LU-2240B INPUT read (inside 0146547B) returns
```

`[V]` This is entirely firmware-initiated (superkick), NOT ND-initiated. `[I]` The firmware
learns ENNS0's RT-description / registers its RT-wakeup through the XMSG-box handshake (DOIT
line 100505 `A:="PIWKF"; T:=XFWDF; ...` = "DEFINE RT WAKE UP ADDRESS"), which is itself
bootstrapped by the message layer processing that first inbound frame. So in a working system
**the wire moves first**; the ND-100 does NOT ring a follow-up doorbell to provoke the first
receive.

## DECISION and the concrete FIX

| candidate | verdict | evidence |
|-----------|---------|----------|
| (1) missing ND->68K follow-up kick (emulator drops it, or kernel should send it) | **REFUTED `[V]`** | No PIOCM/BNDC kick is issued by ENNS0 or by 0146547B before the block (live MON trace); no device-read path in the NPL rings the firmware; emulator BNDC->GPIP-I6 path is wired and the OPCOM START handshake completes |
| (2) network-RX-gated, no peer/loopback | **LEADING / ANSWER `[V mechanism, I justification]`** | Only remaining 0x2562 feeder is LANCE RX; ENNS0 is the receiver-side network server that blocks on receive; harness has no peer and no loopback |
| (3) something in 0146547B | **BOUNDED, body OPEN `[OPEN]`** | 0146547B is a plain blocking device INPUT (monitor-I/O-primitive OR RTCOMMON); rules out that it kicks the firmware; exact identity needs DAP/RTCOMMON binary |

**THE FIX (harness/emulator, not the ND-100 side):** supply the LANCE RX stimulus that a real
network provides. In order of preference:

1. **LANCE internal loopback / TX->RX feedback** in `NDBusEthernetII.cs` (the LANCE model) so a
   frame ENNS0 transmits during startup returns as an RX-complete and drives the firmware's
   message layer. `[I]` - cleanest if ENNS0 emits any startup frame.
2. **A second emulated node / a canned inbound frame** injected into the LANCE RX path so the
   firmware's RCVCOMPLETE (68K 0x5C42) -> XMRECEIVER (0xBED8) -> message layer runs. `[I]`

**Do NOT** add a SINTRAN LU-2240B input completion or a byte/record input register to
`NDBusEthernetII.cs` - that surface does not exist on a PIOC-class card and the input completion
is manufactured by SINTRAN's PDRIV off INT12, not by the controller (prior `[V]`).

## Decisive confirmation - DAP breakpoints (one session settles all three)

68K (firmware):
- **0x250E** (GPIP-I6 scanner) and **0x1B00** (OPCOM) - not hit during the read = no ND->68K
  doorbell after bring-up (confirms (1) has nothing to deliver).
- **0x5C42** (RCVCOMPLETE) / **0xBED8** (XMRECEIVER) - not hit = no inbound LANCE frame (confirms
  (2)). Inject a loopback/peer frame and watch these fire.
- **0x2562** / **0x259A** - fire only after an RX/GPIP event = the make-runnable moment.
- **0xEAA6** producer / **0xECF8** SCIP 0xEF0180 - fire = superkick emitted.

ND-100 (RetroCore DAP):
- **PIKIC 115227B** - not hit during the read = ENNS0 issues no follow-up kick (confirms (1)).
- **030427** (PRSRV) / **030440** (resume-P), then **step into 0146547B** - names the input
  primitive (settles (3): resident monitor-I/O vs RTCOMMON) and shows it does NOT kick.
- **PDRIV 100765B** / **PISUPER 077554B** - fire only after the firmware SCIPs; if they fire
  after the injected RX frame and ENNS0 leaves RTWT, (2) is proven and the fix is confirmed.

## Honest OPEN items (no guessing)
- Exact identity/body of 0146547B (resident monitor input primitive vs COSMOS RTCOMMON). `[OPEN]`
  - narrowed to "a plain blocking device INPUT that does not kick"; needs DAP step-in or the
  RTCOMMON binary.
- Whether ENNS0 emits any startup TX frame (which would let simple LANCE loopback suffice) or
  strictly waits for an unsolicited peer frame. `[OPEN]` - decide with the 68K TX/RX breakpoints.
- Whether a single injected RX frame reaches 0x259A on the first try, or the firmware's
  message-type control block (0x0A8A[code]) / ENNS0 RT-wakeup registration must be bootstrapped
  first. `[OPEN]` - the loopback experiment above answers this directly.
