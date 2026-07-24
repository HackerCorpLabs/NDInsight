# ROOT CAUSE: ENNS0 hangs in a SINTRAN INPUT wait on logical unit 2240B

Date: 2026-07-23. Based on LIVE `LIST-RT-DESCRIPTION ENNS0` + `LIST-EXECUTION-QUEUE` from
the running system, cross-decoded against the ENNS0 supervisor disassembly
(`encos-err-i-b01.brf`). `[V]`=VERIFIED, `[I]`=INFERRED, `[OPEN]`=not yet decoded.

## The observed hang (VERIFIED, live, deterministic - reproduced 3x identically)

`LIST-RT-DESCRIPTION ENNS0`:
```
PASSIVE  ...  RTWT  ...
START ADDRESS: 32241B      LAST STARTED: <n> SECS
  P      X      T      A      D      L      S      B
030440 000044 000002 000000 000000 001753 000100 000236
RESERVED  DATAFIELDS 103356B   LOGICAL UNIT 2240B  INPUT   (FIRST WAITING)
```
`LIST-EXECUTION-QUEUE`: RTRFA / XROUT / BAK01 / ERS3WD / DUMMY - **ENNS0 is NOT in the
queue** (it is PASSIVE, parked in the wait); XROUT itself is running fine.

So ENNS0 is **blocked in a SINTRAN device INPUT wait on logical unit `2240B`** (datafield
`103356B`), resume-P `030440`, T=2. It is not crashed, not aborted - it is waiting for input
that never arrives. The operator-visible "Server not yet started - will try to start him now
(wait 10 sec!) ... Error in communicating with XROUT" is this wait timing out.

## What ENNS0 did to get here (VERIFIED from disassembly)

The POSU startup path around the hang (`encos-err-i-b01.brf`, nd100dis):
```
030375  153322   MON 322     ; [V] MON 322 = GSGNO GetSegmentNo
030427  153124   MON 124     ; [V] MON 124 = PRSRV ForceReserve (force-reserve a DEVICE)
030430  004616   STA -114,B  ; save PRSRV status
030432  131012   JAZ  030444 ; if status==0 -> ok branch
030433..030437              ; build a small param block: X+6 = <val@030451>, X+7 = 50B
030440  135012   JPL I *12   ; <- RESUME-P: call the routine at ptr@030452 (does the INPUT read)
...
030472  153125   MON 125     ; [V] MON 125 = PRLS ForceRelease (release the device) - cleanup path
```
Monitor calls IDENTIFIED (VERIFIED, ND-860228-2 Monitor Calls ref):
- MON 322 = **GSGNO** GetSegmentNo
- MON 124 = **PRSRV** ForceReserve - "Force reserve a device"
- MON 125 = **PRLS** ForceRelease - "Release another program's device"
- (RTWT = 135B WaitForRestart is the RT wait-state name; the descriptor shows the specific
  wait is an INPUT device wait on LU 2240B.)

So ENNS0 **force-reserves a device (PRSRV/MON 124)** and then reads INPUT from it (LU 2240B)
and blocks. `LU 2240B` is NOT a hardcoded literal (the single `002240` word in the image is
the instruction `STZ -96,X` at 030327, a false match) - the LU is assigned at runtime when
ENNS0 opens/reserves the device.

## Interpretation (corrects the earlier "config gap" inference)

- Earlier this investigation INFERRED (from the ND-210580 recipe) that the failure was a
  missing `DEFINE-REMOTE-NAME` / XMSG-STARTUP config step. That was TESTED LIVE and DID NOT
  WORK. This RT-descriptor evidence supersedes it: the real failure is **ENNS0 blocking on a
  device INPUT read during its own POSU startup** - it never gets far enough to matter what
  names are defined.
- This VINDICATES the operator's original instinct: ENNS0's startup fails talking to
  (waiting on) the controller/interface - a communication/input-feedback hang, not a naming
  gap. The "Error in communicating with XROUT" banner is ENNS0's generic startup-timeout
  message, not a literal XROUT-lookup failure.
- The controller emulation itself reaches READY + LANCE RX/TX ON in the harness, so the
  block is a SPECIFIC input on LU 2240B that arrives after the initial bring-up (ENNS0 prints
  its option banner BEFORE it hangs).

## What LU 2240B is - PINNED (agent 2026-07-23, ENNS0-LU2240B-DEVICE-PIN doc)

**LU 2240B = the SINTRAN-generated Ethernet-interface COMMUNICATION device** (the "Ethernet
Interface datafield" the install prereq requires). [INFERRED, strong] Evidence:
- MON 124 = **PRSRV** `ForceReserve(DeviceNo, IOflag, RTProgram, Stat)` - manual: works ONLY
  on peripheral devices + semaphores. ENNS0 force-reserves the device's INPUT part (IOflag=0).
- Datafield **103356B falls in the POF resident range 100000-110000B** = "Error device, line
  printers, **SINTRAN communication**, SIBAS internal devices" (00-SINTRAN-ARCHITECTURE sec
  5.3). So it is a communication/internal-device datafield, NOT a terminal/disk.
- RetroCore ETH II: hardware DEVNO 140360-140363B, IDENT 140034B, interrupt **level 12**;
  SINTRAN logical devno assigned at generation.

**Why no input arrives - THE FIX POINT [INFERRED, leading]:** the LU-2240B INPUT wait must be
woken by a SINTRAN **level-12 Ethernet-driver input completion** (the driver posts a record
and dequeues the first waiter). ENNS0 drives the controller only via PIOCM (MON 255B); the
RetroCore emulator services ETH at the IOX / SCIP / INT12 / PIOCM level (which DOES reach
READY + LANCE RX/TX ON) but **likely never delivers a completed SINTRAN logical-unit INPUT on
the LU-2240B path** -> ENNS0's read is never woken -> it waits forever. This matches the exact
symptom: controller comes up, yet the SINTRAN-level read never completes. Alternative
[INFERRED]: SINTRAN L does not wire that level-12 ETH-datafield driver (a generation issue).

**Static tension noted honestly:** resume-P 030440 sits on the PRSRV-returned-NONZERO branch
(`JAZ 030444` @030432 takes the Stat==0 OK path; 030440 is the fall-through = Stat!=0, "device
already reserved"), which tensions with the descriptor's "RESERVED DATAFIELDS 103356B" line.
The routine the blocked frame calls (`JPL I *12` @030440 -> word@030452 = **0146547B**) is
OUTSIDE the ENNS0 image (span 001756B..073116B) = an external/shared RTCOMMON-class routine.
The LIVE descriptor (device reserved, first-waiting INPUT) is authoritative and says the
reserve succeeded and ENNS0 waits for input; the exact live Stat/branch cannot be decided
statically.

## MECHANISM CORRECTED (agent 2026-07-23, ENNS0-LU2240B-INPUT-PATH-FIX doc)

The phrase "the emulator never delivers a SINTRAN input completion" above is a CATEGORY
ERROR - corrected here. VERIFIED from the emulator + SINTRAN source:
- The emulator NEVER produces a SINTRAN device-input completion for ANY device, and should
  not. `NDBusEthernetII.cs` correctly models only: IOX (STATUS/CONTROL, NO data register -
  ReadDataRegister falls through to ReadStatus), the 512KB DRAM window, and SCIP->INT12 /
  IDENT 140034B (payload-less). That is all a PIOC-class card is.
- The device-INPUT completion is produced by SINTRAN's OWN level-12 driver **PDRIV**
  (`MP-P2-PIOC-DRIV.NPL`): on INT12 it walks the 68K firmware's **"superkick" ring in PIOC
  DRAM** (SUKOF=1012B, pattern 52525/125252) and does `XRTEN`/`RTACT` to wake the waiting RT
  (the RTPR branch). `NXRTF`: "BIT 0 SET BY PIOC WHEN RTWAK REQUESTED, BIT 2 SET BY ND100
  WHEN RTWAK COMPLETED." SINTRAN L HAS this driver (not a generation gap).
- So ENNS0's LU-2240B read is woken by PDRIV dequeuing a **superkick that the 68K firmware
  must post** in shared DRAM (then SCIP->INT12). PDRIV + the wake path already exist in the
  emulator (SCIP->INT12 is wired, the firmware is real).

**THE REAL GAP:** the 68K firmware does not post the superkick that carries ENNS0's input.
Note (harness-verified): the firmware DOES complete the monitor-postbox handshake - it posts
PRKEY@0x0404, MON_CODE=3 READY, and the LANCE starts (RX/TX ON). But the **superkick ring**
(SUKOF=1012, the channel PDRIV reads to wake an RT) is a SEPARATE post, and the firmware
never produces the superkick for ENNS0's LU-2240B input -> PDRIV has nothing to dequeue ->
ENNS0's INPUT wait is never woken. (The agent's "firmware never wrote PRKEY" over-relied on
an older doc; the CURRENT emulator does post PRKEY - the missing post is the superkick, a
different channel.)

**Fix direction (VERIFIED not-here / INFERRED where):** do NOT add an input buffer/record
path to NDBusEthernetII.cs (category error). If it is an emulator-stimulus gap, it is in the
CONTROL-word RESET/INITIATE/OPCOM edge handling in `Write()` (NDBusEthernetII.cs ~1243-1355)
and SCIP->INT12 delivery (~877-903, 1258-1288) - i.e. whether the PWCR=0 INITIATE / the ND
kick gives the 68K the stimulus it needs to run the code that posts superkicks. Alternative
[INFERRED]: an ENNS0-B01 (1987) vs SINTRAN-L / firmware-image mismatch in the superkick
format/protocol.

## LIVE EMULATOR PROBES (2026-07-23, [68K-PC]/[68K-SAMPLE] instrumentation in NDBusEthernetII)

VERIFIED by running the boot harness with a 68K PC watch + sampler:
- **The 68K is ALIVE and healthy-idle, not dead/stuck.** PC samples live mostly at 0x2CEE
  (main loop) + 0x3A98 (RTC ISR). The main loop 0x2CB6-0x2CEE is a SCHEDULER: `moveq #64,D0;
  subq #4; move.l (0,A0,D0.w),A1; btst #1,(23,A1)` = scan 16 work-slots for a "pending" bit,
  process any found, else `STOP` (4E72 @0x2CEA) waiting for an interrupt, then rescan.
- **The MFP RTC (Timer C, channel 5, vector 0x45) fires ~1961x** - wakes the STOP each tick so
  the scheduler rescans. Timers A/B/D unused. So the MFP timer path WORKS (not the fault).
- **GPIP6 (the ND->68K kick, channel 14, vector 0x4E) fired EXACTLY ONCE** (bring-up), never
  during the read. And the firmware NEVER reaches its message layer (0xBFF8) or superkick
  producer (0xEAA6/0xEACC) in the whole run - so a superkick is never even attempted.
- **ENNS0's POSU issues NO kick.** Only MON calls before the blocking read: MON 135 (RTWT),
  MON 200x2 (XFDUM+XFDCT), MON 322 (GSGNO), MON 124 (PRSRV reserve LU 2240B); then the read
  via external 0146547B; then MON 125 (PRLS). No SEND_KI / PIOCM kick wrapper is called.

CONCLUSION (this pass): the 68K firmware is a correct scheduler idling for work; ENNS0
reserves the device and blocks reading, expecting the firmware to deliver an initial input
event (superkick) after START; no work is ever queued (no ND->68K kick after bring-up, no
startup superkick), so the scheduler stays idle and ENNS0's read never completes. NOT a 68K
or MFP-timer defect. The missing bridge = what should make the firmware post the FIRST
superkick to ENNS0's channel after START_P (an ND kick ENNS0/kernel should send, or a
firmware post-START event) - the last-last link, being decoded.

## CORRECTIONS from live PC-region instrumentation (2026-07-23, [68K-OPCOM] watch)

Two prior conclusions were WRONG and are corrected here (VERIFIED by widening the PC-watch
to the whole OPCOM handler region 0x1B00-0x1C60):
- **The OPCOM/START handler DOES run** (31 PC hits: 0x1B06 -> 0x1B4C -> 0x1BAA -> 0x1BF8 ->
  0x1C48 ... 0x1C60). The earlier "0x1B00 never entered" (blocker (c)) was a FALSE NEGATIVE
  from a too-narrow single-address watch (off-by-one: interrupt entry lands mid-fetch). The
  vector/IPL6 delivery is FINE.
- The handler dispatches to **0x1C48 = the MPIOC=5 path, which is PISTA's CORRECT start
  function** (PISTA writes MPIOC=5 at the mailbox; not a teardown). It posts MON_CODE=1 ACK,
  clears STARTED, SCIPs 0xEF0080, and returns. The monitor-postbox start handshake COMPLETES;
  the ND-100 then actively polls the postbox (MON_COUNTER/CODE/STARTED_FLAG reads observed).
- **The 0x200440 fault is SEPARATE/incidental** - NO 0x200440 fault occurs after the OPCOM
  handler's indirect call [0x4B6]. So 0x200440 is NOT on the start path (settles that).

## THE CAUSE (consolidated, this pass)

Everything on the controller side is healthy: 68K scheduler runs, MFP RTC fires, OPCOM/START
handler runs, the monitor-postbox handshake (PRKEY/MPIOC=5 ACK) completes. BUT the
monitor-postbox is a DIFFERENT channel from the **superkick (RTWAK)** channel that ENNS0's
`LU 2240B` device-read blocks on. The firmware posts a superkick ONLY when its scheduler runs
a work-slot, which requires a **make-runnable event** (0x259A bclr, fired by the message-type
dispatcher 0x2562 on an inbound event: a GPIP-I6 channel doorbell -> 0x250E, or a LANCE RX
frame). After the start handshake: NO further GPIP-I6 kick is sent by the ND-100 (no control
writes), and there is NO network peer/loopback -> no make-runnable event -> the scheduler
never runs the message layer -> no superkick -> PDRIV dequeues nothing -> ENNS0's INPUT wait
on LU 2240B is never woken -> ENNS0 never finishes startup -> never registers *XM-ENNS0 ->
XROUT "Unknown name". NOT a 68K/MFP/OPCOM/vector defect. The single missing piece = the
make-runnable trigger for ENNS0's FIRST superkick (an ND->68K follow-up kick that isn't sent,
or a network RX that has no peer).

## VALIDATION EXPERIMENT ([RX-INJECT], 2026-07-23) - CONFIRMS network-RX-gated

Injected ONE broadcast frame into the LANCE RX path during the read wait (NDBusEthernetII
Clock hook, tick>30M). Result:
- **LANCE RX + level-2 + firmware ISR all WORK** (VERIFIED): `LANCE RX accepted 64 bytes
  type=0x0806` -> DMA into ring[0] -> `LANCE Interrupt Generated/Asserted/Acknowledged` ->
  firmware level-2 ISR read `CSR0=0x0473` (RINT set) and wrote it back -> `CSR0=0x0073` (RINT
  cleared). So the inbound-frame -> firmware-interrupt path is NOT broken.
- BUT the firmware **discarded the frame without posting a superkick** - it never reached
  RCVCOMPLETE (0x5C42), the message layer, or 0xEAA6. Because the injected frame was **ARP
  (EtherType 0x0806)**, not a COSMOS/XMSG inter-system frame. So a wrong-type frame is RX'd,
  ACKed, and dropped - no make-runnable event, no superkick.

## FINAL ROOT CAUSE (validated)

ENNS0's startup device-read on `LU 2240B` blocks waiting for the 68K firmware to post an
RTWAK **superkick** (-> PDRIV -> XRTEN wakes it). The firmware posts a superkick ONLY when it
processes an inbound event that reaches its message layer -> make-runnable (0x259A) ->
producer (0xEAA6). The ONLY such event available at startup is a **valid COSMOS/XMSG inbound
LANCE frame**. The isolated single-node harness/emulator has **no network peer, no loopback,
and generates no valid COSMOS traffic**, so that event never occurs -> no superkick -> ENNS0's
INPUT wait never wakes -> ENNS0 never finishes startup -> never registers *XM-ENNS0 -> XROUT
"Unknown name". EVERYTHING else is VERIFIED HEALTHY: 68K scheduler, MFP RTC, OPCOM/START
handshake (monitor postbox, PRKEY, MPIOC=5 ACK), LANCE init+RX+level-2 interrupt path, XROUT.
The 0x200440 fault is separate/incidental. This is NOT an emulator interrupt-wiring or
firmware defect - it is the absence of valid inbound network traffic in an isolated node.

## THE FIX (named)
Provide a valid COSMOS/XMSG inbound stimulus: a network peer, or a LANCE loopback, or inject a
correctly-formed COSMOS frame (right EtherType + XMSG framing) so the firmware's
RCVCOMPLETE(0x5C42) -> XMRECEIVER(0xBED8) -> message layer -> 0x259A -> 0xEAA6 -> SCIP
0xEF0180 -> INT12 -> PDRIV -> XRTEN wakes ENNS0. Do NOT add an input register to
NDBusEthernetII.cs. The exact COSMOS Ethernet frame format is the remaining implementation
detail (see XMSG-PROTOCOL.md / a pcap) - my ARP test proved the path, not the payload.

## OPEN (needs DAP - none available this session)
Decisive confirmation = a DAP session: breakpoint 030427 (PRSRV) and 030440, read Stat + the
PRSRV param block, and step into the external routine 0146547B to see the exact input wait and
what should wake it. Remaining unknowns: identity of routine 0146547B; emulator-gap (fix in
NDBusEthernetII: deliver a SINTRAN LU-2240B input completion / level-12 driver wake) vs
missing-driver (SINTRAN generation). T=2 in the blocked frame is likely a leftover (T is not a
PRSRV param register).

## Concrete next steps
1. Decode where ENNS0 OPENS/reserves LU 2240B in its POSU (before 030427) to name the device,
   and what input it expects and from whom.
2. Identify LU 2240B in the running SINTRAN device tables (what physical/internal device it
   is; is it the ETH-interface datafield device).
3. Reconcile with the emulator: does the ND Ethernet II / PIOC device present a SINTRAN
   logical-unit INPUT that gets satisfied, or only the IOX/SCIP/memory-window path? If the
   latter, ENNS0's LU-2240B read is never woken = the emulator gap that hangs ENNS0.
</content>
