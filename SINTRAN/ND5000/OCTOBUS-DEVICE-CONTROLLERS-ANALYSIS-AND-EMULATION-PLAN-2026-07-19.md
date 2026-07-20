# Octobus Device Controllers - Analysis and Emulation Plan (2026-07-19)

Purpose: document ALL device types on the Octobus, how the ND-100 / ND-500 /
ND-5000 CPUs talk to each of them, assess the current RetroCore emulation,
and define (a) an implementation plan for device-controller emulation and
(b) a design for reusable octobus objects that the ND-100 card, the ND-5000
CPU station, and future device controllers (SCSI, Ethernet, MF controllers,
...) can all share.

Evidence marking (same convention as the rest of this folder):
[V] = byte/live/manual-verified, [NPL-V] = verified in NPL source,
[MC-V] = verified in ND-5800 microcode, [I] = inference,
[UNCERTAIN]/[OPEN] = explicitly open. Anything not marked [V]/[NPL-V]/[MC-V]
must not be treated as ground truth.

REVIEWED 2026-07-20: three independent critical reviews (RetroCore code,
SINTRAN carves, hardware manuals) were run against this document; all
corrections are applied below and section 8 now carries per-phase TODO task
lists. Full findings with evidence:
[OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md](OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md).
Finding IDs (DEV-n, SIN-Fn, HW-An) below refer to that file.

Sources:
- Manuals (this repo): [ND-05.020.01 ND-5000 Hardware Description](../../Reference-Manuals/500/ND-05.020.01%20EN%20ND-5000%20Hardware%20Description.md)
  (Appendix 2 = Octobus Protocol v5, chapter 5 = Access Module),
  [ND-05.017.01 ND-5000 Hardware Maintenance](../../Reference-Manuals/500/ND-05.017.01%20EN%20ND-5000%20HARDWARE%20MAINTENANCE.md)
  (chapter 3 octobus, Appendix A ACCP command tables),
  [ND-14001-1 DOMINO Standard Hardware Description](../../Reference-Manuals/500/ND-14001-1-EN%20DOMINO%20Standard%20Hardware%20Description.md)
  (chapter 4 = OBCON / OCTObus Adapter, chapter 3 = MFbus Adapter),
  [ND-820026-1c DOMINO and NUCLEUS Software Guide](../../Reference-Manuals/500/ND-820026-1c-EN%20DOMINO%20and%20NUCLEUS%20Software%20Guide.md)
  (DOMINOS, NUCLEUS, PROMAN boot, module table),
  [ND-820026.1 DOMINO and NUCLEUS Software Guide](../../Reference-Manuals/500/ND-820026.1%20EN%20DOMINO%20and%20NUCLEUS%20Software%20Guide.md)
  (larger edition: ERS event catalogs, selftest Appendix B, OPCOM LED
  patterns, DOMINO Monitor - the primary de-risking source for phases C/D;
  added per review HW-A20),
  [ND-814009 DOMINO SCSI Operator Guide](../../Reference-Manuals/500/ND-814009-1-EN%20DOMINO%20SCSI%20Operator%20Guide.md),
  [NEC-01 ND-500 course](../../Reference-Manuals/500/NEC-01%20-%20ND-500%20course.md)
  (3022/5015 classic interface - NOT octobus).
- Carves / RE (this folder): [OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md),
  [SINTRAN-OCTOBUS-MESSAGE-CATALOG.md](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md),
  [OCTOBUS-TEST-PROTOCOL-RE.md](OCTOBUS-TEST-PROTOCOL-RE.md),
  [ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md](ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md),
  [ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md](ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md),
  the CARVE-ANSWER-* files.
- Code: RetroCore repo (E: drive, Repos\Ronny\RetroCore), files named in
  section 7. Code paths are given as plain text because the repo is outside
  this documentation tree.

---

## 1. The two-bus model: Octobus signals, MFbus/MPM carries data

The single most important architectural fact for every controller type:

- The OCTOBUS is a serial SIGNALLING bus (short messages: idents, kicks,
  emergencies, small multibyte command messages). "The octobus is normally
  not used to transport data." [V] (verbatim twice in ND-05.020.01 ch 5;
  supporting: ND-05.017.01 Appendix A.4 "Octobus and memory are used for
  communication and synchronization". Citation tightened per HW-A19.)
- Bulk DATA moves over shared memory: the MFbus (an MPM-5-class multiport
  memory bus "with additional features for supporting the Octobus" [V],
  ND-05.020.01 introduction). Controllers are MFbus DMA masters; command
  blocks, message buffers and mailboxes live in shared MFbus/MPM memory.

Every host<->controller conversation therefore has the same shape:

1. Producer builds a command/message block in shared memory.
2. Producer sends a short octobus frame (kick or ident) to wake the consumer.
3. Consumer processes, DMAs data as needed, writes status back into shared
   memory.
4. Consumer sends a short octobus frame back (kick/ident/GIVEINT-style) to
   signal completion.

The ND-100 <-> ND-5000 mailbox (X5ACT / X5FIF, see section 4.2) and the
NUCLEUS kick-table model used by DOMINO controllers (section 4.4) are two
instances of this one pattern. This is the pattern the reusable objects in
section 8 must capture.

---

## 2. Station map and device catalog

### 2.1 Octobus station assignments [V]

(ND-05.020.01 Appendix 2 section 2.2 and ND-05.017.01 chapter 3; all octal.)

| Station (octal) | Device |
|---|---|
| 1 | ND-100 / ND-120 CPU (normally bus MASTER, lowest station) |
| 2-7 | MFbus controllers (crate masters; station = "Crate id") |
| 10-13 | SCSI controllers (disk) |
| 14-15 | Matra VME |
| 16-17 | Multifunction communication |
| 20 | Hyperchannel |
| 21-23 | FDDI (fibernet) |
| 24-27 | FPS-5000 |
| 30-33 | Graphic controller |
| 34-67 | Free for expansion |
| 70-76 | ND-5000 CPUs (SINTRAN uses 70B-73B for up to 4 CPUs [NPL-V]) |

Station 0 and 77B are not valid ASSIGNED stations, but neither is a hard
illegal destination: ND-14001 section 4.8.1 puts global devices in range
0-17B (0 nominally included) and the MFbus controller PROBES station 77B
(then downward) when hunting free numbers during crate configuration - so
an emulated fabric must treat 77B as "not present -> Ack 00", never as an
exception [V] (reworded per review HW-A2). Global (cabled) octobus devices
use stations 0-17B set by thumbwheel; local (in-crate) devices are assigned
77B downward by the MFbus controller at configuration time [V] (ND-14001
section 4.8.1). The apparent conflict with FIXED ND-5000 stations is
resolved by ND-05.017.01 chapter 8 (MFbus Test & Maintenance dialog, page
237): station number (70), POWER FAIL DESTINATION (default 1) and REC.
BROADCAST TYPE (default 0) are written per-slot into NON-VOLATILE MEMORY -
stations are pre-provisioned in NVRAM, not rediscovered at every boot [V]
(HW-A3). Those two defaults (PF dest=1, BT=0) are the power-up values an
emulated BADAP/WOI should carry. SINTRAN itself uses FIXED station
constants (5STATION = 070B + cpu index) and never discovers stations
dynamically [NPL-V] - so the emulator may pre-assign stations to match the
SINTRAN convention.

### 2.2 DOMINO controller (DIOC) module types [V]

(ND-820026 Table 1. "Module Number" is hardwired per card and read by the
MFbus controller over MFbus PIO; "Hardware-id" selects the boot image name
PMA-<hwid>-<basic-sw-id>:IMAG.)

| Module no (octal) | Hardware-id | Type |
|---|---|---|
| 5 | VMEI | VME-bus interface |
| 20 | IPI3 | IPI level III disk controller |
| 21 | SCSI | SMDE controller (SCSI disk/tape/optical/streamer) |
| 22 | ETH3 | Ethernet III (LAN) |
| 23 | FPS5 | FPS-5000 controller |
| 24 | TERM | Terminal controller |
| 25 | GRAP | Graphic controller |
| 26 | MFCC | Multifunction comms controller |
| 27 | VMEC | VME-bus controller |
| 30 | DMAC | MF-DMA controller |

Live examples [V] (ND-820026 LIST-CONFIGURATION; ND-814009): SCSI controller
at station 13B module 21B image PMA-SCSI-BDIO; Ethernet III at station 12B
module 22B image PMA-ETH3-TCP; DOMINO SCSI setup examples use stations
10B/11B. One hardware type can run different basic software (ETH3 runs
TCPI / COSM / SIBR images).

Module numbers 6B-76B that are NOT in Table 1 present a plain numeric
hardware-id ("006B" etc., ND-820026-1c page 30) - relevant if the emulator
ever presents a nonstandard module number (HW-A4).

The TPE OCTOBUS B00 "Get module type" (OMD-0 test protocol command 001AB
area) classifies stations as 1 = Domino controller, 2 = MFBus controller,
3 = ACCP [V - CARVE-derived, OCTOBUS-TEST-PROTOCOL-RE.md live+disassembly;
these type codes appear in NO manual (HW-A5), keep provenance as carve].

### 2.3 What every DIOC is made of [V]

(ND-14001 chapters 1-2.) All DOMINO controllers share ONE standard hardware
core; only the device-dependent circuitry differs:

- MC68020 CPU, local DRAM 1/2-8 MB + EPROM (OPCOM firmware, selftest) +
  EEROM.
- OBCON gate array = the OCTObus Adapter (transmit/receive, receive FIFO,
  hardware retry/priority, hardware-decoded emergency codes). The "16x16"
  FIFO depth figure is documented only on the ND-5000 CPU side
  (ND-05.017.01 ch 3.4); the DIOC OBCON FIFO depth is NOT documented -
  TPE OCTOBUS test 3 measures it on real hardware [I same chip family]
  (HW-A6). On the DIOC it sits at 68020 I/O addresses FF8080-FF80BF, input
  FIFO at FF80C0-FF80FF, OCINT7 (clear level-7 octobus interrupt) at
  FF810E. Also emulation-relevant if a DIOC 68020 is ever executed: DRAM
  parity/INVP and the breakpoint RAM (ND-14001 2.1.3).
- BADAP gate array = MFbus Adapter (MFA): PIO registers RMT/RMS/RECOL/RDS/
  WMT/WMC/WOI/WDC/WLIM the MFbus controller uses to identify, bound and
  start the card; holds the octobus init parameters (STANO station number,
  PF power-fail destination, BT broadcast type).
- MC68901 MFP (timers + console UART), protection tables, RTC, breakpoint HW.
- Software: OPCOM firmware (PROM) + DOMINOS kernel (enhanced PIOCOS:
  processes, events, buffers, PIRCreateDriver for device interrupt vectors)
  + NUCLEUS message-passing library + the per-type application image.

The ND-5000 side equivalent of OBCON is the OCTC gate array on the ACCP
baby card (MC68000 + OCTC) [V] (ND-05.020.01 ch 5.1.2/5.3). Functionally
the same transmitter/receiver/FIFO block. The ND-100 side card
(PCB 3109/3096, part 324118 "Multiport Line Driver with an Octobus
Controller gate array") carries the same OBCON family chip
[UNCERTAIN - these PCB/part numbers appear in NO manual in
Reference-Manuals\500 (HW-A6); source them from a parts list or keep this
tag].

This is the hardware justification for the reusable-objects design: the
manuals themselves define one generic core (68020 + OBCON + BADAP + DOMINOS
+ NUCLEUS) with a thin per-device personality on top.

---

## 3. Frame formats and message types (the shared vocabulary)

Established byte-verified facts, repeated here only as the contract every
reusable object must honor. Full detail: [OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md).

### 3.1 The one true 16-bit software frame [V]

```
bit:  15 14 | 13..8            | 7 6 5 4 | 3..0 (or 7..0)
      C  B  | DEST(send)/      | E K M S | number / data byte
            | SOURCE(receive)  |         |
```

Decode order: E=1 emergency; K=1 kick (number in low bits); C=1,M=1,S=1 SOMB
(OMD in bits 3:0); C=1,M=1,S=0 EOMB; C=1,E=K=M=0 ident; C=0 data byte.
Primary citation for the decode table: ND-05.017.01 section 3.3.1 (page 57)
- the ND-05.020.01 figure 64 OCR is column-garbled, do not decode from it
(HW-A7). The fabric/hardware rewrites bits 13:8 from destination to source
on delivery. Broadcast (B=1) is legal for emergency [V - explicit,
ND-05.020.01 2.6], FORBIDDEN for kicks and idents [V], and for multibyte
only [UNCERTAIN] - the sole support is a garbled figure column; section 2.9
is silent and the 017.01 table shows B=0 on every row; no carve shows it
used (HW-A8).

### 3.2 Wire frame (30 bits) [V]

Priority(4-5 bits, lost-access counter) | Dest(6) | C | B | Source(6) |
Information(8) | Parity(2) | Ack(2). Ack: 00 timeout/not present (15
retries), 01 OK, 10 busy (255 retries), 11 parity error / ambiguous
broadcast. Manual details added per HW-A9 (ND-14001 4.2-4.4, Figure 32
page 128): on a BROADCAST, ack 11 = ambiguous response with ZERO retries;
the retry counts are DEFAULTS programmable via the OCTObus Transmitter
Control Register; retransmission is flagged in the Transmit Status
Register; the priority (lost-access) counter resets after a successful
transmission and the LOWEST station number wins arbitration ties. All of
this is hardware; software never sees priority/parity/ack except as status
bits. The emulator models Ack results as status-register outcomes
(Error / NotPresent / Busy), which is sufficient.

### 3.3 Multibyte message envelope (protocol v5) [V]

SOMB(C,M,S, dest OMD) -> data(source OMD) -> data(byte count N) ->
N x data(payload) -> EOMB(C,M, dest OMD). OMD = "Octobus Message Device"
(ND-05.020.01 terminology). The byte count is ONE data frame, so the HARD
protocol limit is a 255-byte body - MultibyteAssembler must enforce this
(HW-A10). Mode tension to keep in mind: ND-05.020.01 5.3.48 states that in
test/init mode the ACCP ignores everything except multibyte to OMD 3, yet
the OMD-0 test responder demonstrably answers on live hardware
(OCTOBUS-TEST-PROTOCOL-RE.md) - either "ignored" excludes OMD 0 or the
responder sits behind ENABLE KICKS; noted, not silently assumed (HW-A10).
Known OMD allocations:

| OMD | Consumer | Verified where |
|---|---|---|
| 0 | Octobus Test Protocol responder (every station's firmware) | [V] OCTOBUS-TEST-PROTOCOL-RE.md |
| 3 | ACCP command library (OMDACCP, ND-5000 only) | [NPL-V] |
| 4 | MF-controller message channel (MFOMDNO); also SINTRAN J04 receive side for ACCP Messack/Messnak | [NPL-V] |
| 5OMDNO (dynamic) | SINTRAN's receive OMD, announced to each CPU in the CMSYSPAR body | [NPL-V] |
| other | delivered to the ND-5000 microprogram via AOB (on a CPU station) | [V] |

[UNCERTAIN] A global OMD allocation table (how many exist per station, who
assigns them) is NOT documented in any manual found so far. The DOMINO
software guide mentions OMD only in an OPCOM LED diagnostic ("error
connecting to OMD"). Treat OMDs as per-station mailbox numbers connected at
runtime by each station's software.

### 3.4 Kicks [V]

Kick numbers 1-6 (N100KICK=1 activate, 2 activate, CLRKICK=3, 4 clock,
NUCLEUS=5, IDLEKICK=6); 0 and 7-63 rejected by the ND-5800 microcode
(TRAP_NOTREC report 204B). IMPORTANT correction (SIN-F1): kick 5 =
"NUCLEUS" is the manual/NPL kick-NAME table (ND-05.020.01 2.7). The
BYTE-VERIFIED host-side NUCLEUS send path (NKSEND -> NKICK -> SKICK) emits
kick number 1 (NUCKI=000001; 044600 SAA 1 [V],
NUCLEUS-PRIMITIVES-CARVE.md section 5.2). The kick number a DIOC must send
TOWARD the ND-100 to wake DKICK is [OPEN] - it is bound by CONKI(A=14B,
T=1), body uncarved. Do NOT wire "kick 5" anywhere on the NUCLEUS path
without carving CONKI first.

### 3.5 Emergency codes - one open discrepancy [OPEN]

Byte-verified truth used by RetroCore today: EBIT(200B) OR-ed with the CM*
code -> 241B master clear, 242B continue ACCP, 244B terminate ACCP
[NPL-V, PH-P2-OPPSTART CH5CPUPRESENT]. Code-point sources: ND-14001
Figure 35 (page 130) + NPL. (Earlier citation "ND-05.020.01 ch 5.3.9" was
WRONG - 5.3.9 is ACCP TIMEOUT; the emergency commands are 5.3.50 Terminate
/ 5.3.51 ACCP Reset, behavioral text only, NO byte values anywhere in
ND-05.020.01. Fixed per HW-A12.)
The DOMINO hardware-decoded set 241 RESTART / 242 CONTINUE / 243 STOP /
244 INT7 / 245 RESCOUNT / 376 POWERUP / 377 POWERDOWN (ND-14001 Figure 35)
is anchored octal by Figure 33: the power-down info field is all ones =
0xFF = 377B. So DIOC hardware decodes the 0xA1-family.
The ND-05.017.01 Appendix A discrepancy is now ANALYZED (HW-A12), not an
OCR guess: 361B octal = 241 DECIMAL (362B=242, 363B=243, 364B=244), i.e.
the appendix self-consistently claims wire bytes 0xF1-0xF4 - a genuinely
different family, not a typo of 241B. ND-05.020.01 Figure 66's R-bit table
cannot arbitrate (0xFE/0xFF POWERUP/POWERDOWN already violate its R=1 "not
used"). Since SINTRAN's send side is byte-verified 0xA1-family and real
ND-5000 systems worked, the ACCP must accept 0xA1-family [I - strong].
VERDICT: keep 241B/242B/244B in the emulator; the Appendix-A 0xF1 family
is a firmware-listing anomaly; residual (low-priority) question = what the
AOCP hardware decoder actually matches - ONE live-trace byte of a
SINTRAN-issued master clear closes it permanently.
ARES timing (ND-05.020.01 5.3.51, added per HW review): on print-A/B ACCPs
RESET is LATCHED and requires a CONTINUE after >= 100 ms; print C+
self-pulses. This is WHY SINTRAN sends 242B after 241B, and it is a timing
requirement for AccpAccessModule emergency handling.

Power messages [V] (ND-14001 section 4.5): OBCON broadcasts power-down
(info=377B) / power-up (info=376B) automatically. Receiver disambiguates by
source station: 1-17B means power fail, 20B-76B means "fatal controller
hardware failure" (same frame, different meaning). Manual inconsistency to
log (HW-A13): ND-14001 Figure 34 draws the power-UP info field as ALL
ZEROS while Figure 35 says 376B; both cannot be right (all-zeros with C=1
would decode as an ident; 0xFE fits the hardware-decoded family) - likely
a drawing error, emulate 376B.

---

## 4. How each CPU talks to devices

### 4.1 ND-100 side (SINTRAN III) [NPL-V + carve]

Hardware: octobus interface card at IOX 100400 (+0 input FIFO read, +2 input
status incl. static own-station readback bits 13:8, +3 input control, +5
transmit frame write, +6 output status bit 3 = ready, +7 output control),
idents 40B (receive) / 41B (transmit) on level 13, EVENT-latched interrupt
FFs, IDENT clears request AND enable. Full maps:
[OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md) and
[CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md](CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md).

Driver structure (OCSTART, PH-P2-OPPSTART) [NPL-V]: per-interface CENTRY
tables - 16 OMD entries, 16 kick entries, per-source-station ident entries,
each an (OLINK, DLEVE, DFADD) triplet linking to level 10/11/12 drivers.
Receive dispatch (SOCTO / 5OMBREAD) routes by frame type + source station.
Send paths: SKICK (kick/ident single frames), MBSEND (multibyte builder from
an LMFIELD descriptor: MOCTSTATION, MOCTOMD, MBROADCAST, MMSGLENGTH,
MCOMMAND, MDP1...). Details:
[SINTRAN-OCTOBUS-MESSAGE-CATALOG.md](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md).

What SINTRAN actually sends to NON-CPU stations today [NPL-V + carve;
direction facts sharpened per SIN-F2/F3/F4/F8]:
- MFPREPARE: CMSYSPAR|N100IDENT multibyte to EVERY MF-controller station
  2-6 unconditionally at 5PIT bring-up, on OMD 4 (MFOMDNO = the MF
  controller's RECEIVE OMD; OMD 4 is the ND-100 -> MF direction ONLY).
  The body is 0E 01 <5OMDNO> - byte 2 announces SINTRAN's own
  runtime-allocated receive OMD (5OMDNO, NOT a constant; allocated by
  CONOMD, body uncarved). Absent controllers simply time out (Ack=00);
  carved 5OMBREAD DISCARDS an MF-source MFACK reply with no aliveness
  bookkeeping (SIN-F4) - SINTRAN cannot distinguish absent from silent MF
  stations. Precondition: if CONOMD fails at 5PIT, MFPREPARE never runs
  (WT12 stall, ODR 147273).
- MF -> host error records arrive at STATION 1 on OMD = 5OMDNO (never on
  OMD 4); SEC codes 20B/30B/31B/50B/51B/77B logged via 9FLER. The host's
  MFACK reply addressing is byte-anomalous on L07 (destination read from a
  cell holding the received byte count; only fires for lengths 2..6 -
  ODR divergence 12.2, [V bytes / OPEN reconciliation], SIN-F3): do not
  use the catalog's NPL prose as a test oracle for the ack until
  reconciled.
- Bring-up order (catalog 6.1-6.3): cold SINTR probes the octobus CARD by
  IOXT, then sends emergencies 241B/242B to SAMSON stations ONLY; SCSI
  stations 10B-13B receive NOTHING from SINTRAN at bring-up (SIN-F8).
- BDIO - the IN-KERNEL DOMINO block-device driver [NPL-V] (survey
  2026-07-19, corrects an earlier draft of this section): STRBDIO/REBDIO
  (MP-P2-DISK-START.NPL line ~315, addr 073633B) is a level-12 routine that
  builds a BDIO function-167B message (size 70B) and sends it via the
  NUCLEUS write primitive NKWRI to the DOMINO datafield DOMDF, then waits
  (WT12); REBDIO handles completion and HSTAT BDIO errors. The
  MTRANS-for-DOMINO path converts ND-100 addresses to DOMINO form (DMYAD,
  bit-31 flag). Disk-type code 5DSKC selects the path: 0 = SMD/SCSI-100
  (internal 3201 driver, IP-P2-SCSI-*), 1 = DOMINO (DP-P2-VARIABLES).
  So octobus-attached SCSI (stations 10-13B) has NO dedicated driver -
  it rides the generic BDIO + NUCLEUS + octobus-kick stack. NUCLEUS itself
  is partly in-image: server segments 104/105-NKSE + 106/107-NKNA, started
  by NUCST (PH-P2-OPPSTART); primitives NKSEND=042171B, NKGETINFO=043672B,
  NCALL=050407B, ENKIC=047526B (SYMBOL-1-LIST, L07). The terminal driver
  also emits NUCLEUS kicks (MP-P2-TERM-DRIV NKSEND call, MTAD level 2).
  What IS outside the kernel: the controller-side firmware (PROMAN boot
  images, BOPCOM server body, DOMINOS, PMA-*-* images) - absent from the
  repo and from F: (only manuals found). ND-814009: MAGTP explicitly
  unusable toward DOMINO SCSI tape [V].

### 4.2 ND-5000 side (SAMSON CPU + ACCP) [V/NPL-V/MC-V]

- ACCP baby card (MC68000 + OCTC). Octobus frames to a CPU station arrive
  at the ACCP; kicks, idents and multibyte to OMD != 0,3 are forwarded RAW
  to the microprogram via AOB (with ATRAP/OMESS flags); OMD 0 (test
  protocol) and OMD 3 (ACCP command library: selftest, CS load, LPARP/VPARP,
  start/stop micro, CMCPURES, ENABLE/DISABLE KICKS...) are consumed by the
  ACCP itself. Microprogram sends by writing AIB; with kicks enabled the
  word goes to the wire unmodified.
- Data path is the MPM mailbox, not the bus: SINTRAN activates the CPU by
  writing 0 to X5ACT (plain shared-memory write, no IOX, no MAR); the
  octobus kick 1 is only the preempt path. Answers come back through the
  X5FIF ring plus a GIVEINT frame built from the announced 5OMDNO. Full
  model: [CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION-2026-07-19.md](CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION-2026-07-19.md)
  and [CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md](CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md).
- Presence: a SAMSON is "alive" only after the ACCP MFACKs the CMSYSPAR
  multibyte (5OMBREAD sets 5ALIVE) [NPL-V].

### 4.3 Classic ND-500-II [V]

The ND-500-II CPU INTERFACE is not octobus: it uses the 3022 (ND-100) +
5015 (ND-500) parallel interface (RMAR/RSTA/RCON/MCLR/TERM/TAG registers;
NEC-01 chapter 3). BUT the blanket "not an octobus device" was wrong
(corrected per HW-A16): ND-14001 page 123 states the octobus cable "is
used to connect MFbus banks, ND-100 and ND-500 model II to the OCTObus" -
a model II ATTACHES to the octobus in DOMINO configurations. Its NUCLEUS
"fast calls" run in the ND-100 at level 12 instead of being microcoded
(ND-820026.1 section 6). Kept out of scope for the CPU interface itself.

### 4.4 DOMINO controllers (the generalized device model) [V]

Boot (PROMAN, an ND-100 SINTRAN RT program; ND-820026 section 2.6):
hard reset -> EchoTest -> IdentY (ident + selftest status) -> Stop ->
loop { SetBxP (set mailbox pointer), BxDoLd (download image block via the
MFbus-memory mailbox) } -> RegMod (set start PC) -> Go-On -> watchdog.
The boot BYTE protocol is absent from the manuals, but the OBSERVABLE
contract is documented and must anchor any stub DIOC (ND-820026.1, added
per HW-A14): the reset state machine has preboot (EPROM) and postboot
(standard + device) selftests plus "aborted"/WAITCONT states awaiting an
OPCOM command (2.6 flowchart; test numbers/names in Appendix B); SetBxP
has an ACK/NAK reply with error codes and a timeout-0 convention (ERS
105027B "Unable to set mailbox for controller ... Opcom NAK-error code
('0' means timeout)"); the boot event ladder is ERS 105042B-105045B
(booting started / rebooting / started / selftest status incl. CPU type,
sw-part versions, failed-test lists) plus DOMINOS fatal events
6000B-6007B; OPCOM LED flash patterns diagnose octobus-level failures
incl. "error connecting to OMD"; the DOMINO Monitor reaches OPCOM via
ASYL/SERVER/MAILBOX paths (USE-MAILBOX "MF page number for mailbox: 400B",
SET-DOPCOM-PARAMETERS).

Crate configuration (ND-14001 sections 4.8-4.8.1; expanded per HW-A15 -
this is fully specified at register/sequence level, no firmware needed):
phase I = global-node auto-init + master selection (lowest station);
phase II = the CONFIGURATOR (not necessarily the master) broadcasts
"Identify yourself"; the MFbus controller with the HIGHEST station number
configures its crate first. Per slot: read RMT module type (RMT=0 =
empty slot); probe station 77B then downward for a free number; assign
station / power-fail dest / broadcast type via TWO consecutive WOI writes
to slot address + 10B; start the node by writing 1 to MASTA bit 7 at
slot+4B, which GENERATES the OBRES reset signal - after which the node
answers (the manual's wording; not "releases reset"). Then "Finish", and
the next-highest MFbus controller proceeds. Slot addressing: slot*2 forms
the upper address digits (worked example ND-14001 page 136).

Runtime (NUCLEUS; ND-820026 sections 6-7): ports + messages + send
references living in shared MPM ("the NUCLEUS kernel": master block,
descriptor table, hash array, KICK TABLE indexed by octobus station number,
buffer area; TSET/MFbus-semaphore locking). nkSend to a port owned by a
remote processor queues the message in shared memory and emits an octobus
KICK to the port's KICK DEST station; the remote scheduler wakes the
owning process. KICK NUMBER (SIN-F1): the byte-verified host-side send is
kick 1 (NUCKI) [V]; the manual's kick-name table calls kick 5 "NUCLEUS";
the remote->host kick number that wakes DKICK is [OPEN] (CONKI uncarved).
Completion flows back the same way to the message's home port. On the
ND-5000 nkMove/nkSend/nkReceive/nkGetInfo are microcoded; slow calls
always run in the ND-100. Debug oracle (HW-A17): ND-820026's masterblock
display defines that "only descriptor array and kick table have meaning" -
use as a NucleusKernel cross-check.

SCSI specifically (ND-814009): SINTRAN file system -> pool (BDIO program)
-> PMA-SCSI-BDIO image on the DIOC -> SCSI bus. Device binding = octobus
station + SCSI device no + LUN in DDS-DEVICES:CNFG (DP-SERVICE program) -
HOW that binding reaches the DIOC/datafields is uncarved (SIN-F5a/b).
ND-814009 also carries the host-visible ERROR VOCABULARY for a future SCSI
DIOC (HW-A18): status tables tape 104701B-104777B, disk/DOMINO
104601B-104677B and 105301B-105377B, incl. 105314B "Missing DOMINO
heartbeat, controller aborted" and 105312B/105313B DOMINO init status.
Tape access library documents the async pattern: taOpen (NUCLEUS
connection), taReserve/read/write (send message, get OpId, return
immediately), tarStatus (collect completion from the returned message).
[OPEN] The BYTE-LEVEL request/reply record formats inside those NUCLEUS
messages (the actual SCSI command blocks) are NOT in any manual we have.
They must come from carving the PMA-SCSI-BDIO image, BDIO/DP-SERVICE on the
ND-100, or a live trace. Until then a SCSI DIOC emulation can be brought up
only to the discovery/boot/test-protocol level, not to real disk I/O.

---

## 5. Communication-mechanism matrix (summary)

| Mechanism | ND-100 <-> ND-5000 CPU | ND-100 <-> DIOC (SCSI etc.) | ND-5000 <-> DIOC | MF controller |
|---|---|---|---|---|
| Discovery | fixed stations, CMSYSPAR handshake, 5ALIVE [NPL-V] | PROMAN via MFbus RMT + IdentY [V] | via NUCLEUS ports [V] | crate master, station=crate id [V] |
| Doorbell in | X5ACT:=0 memory write; kick 1 preempt [NPL-V] | NUCLEUS kick: host->remote = kick 1 NUCKI [V]; remote->host number [OPEN] (SIN-F1) | NUCLEUS kick, microcoded fast calls [V] | CMSYSPAR/ident [NPL-V] |
| Command block | X5BEX chain + X5FIF ring in MPM [NPL-V/MC-V] | NUCLEUS message in MPM [V] | NUCLEUS message in MPM [V] | multibyte OMD 4 [NPL-V] |
| Completion | X5FIF insert + GIVEINT frame [MC-V] | NUCLEUS reply message + kick (number [OPEN]) | same | error record to station 1 on 5OMDNO; host MFACK addressing byte-anomalous on L07 (SIN-F3) [V bytes/OPEN] |
| Bulk data | MPM window (Port B) | controller MFbus DMA | controller MFbus DMA | n/a |
| Maintenance | OMD-3 ACCP library [V] | OMD-0 test protocol + OPCOM/Monitor mailbox [V] | - | OMD-0 [V] |

---

## 6. Current RetroCore state (2026-07-19)

Files (RetroCore repo, Emulated.HW\ND\CPU\NDBUS\; line counts re-verified
2026-07-20 per DEV-11 - re-check against HEAD before starting phase A):
- NDBusOctobus.cs (3099 lines): the ND-100 card. IOX map, EVENT-latched
  interrupt model, 16-word receive FIFO + busy-retry queue + inbound
  latency queue, loopback, MPM window creation (0x420000, 8 MB), station
  registry delegated to the fabric, AttachCpu wiring for one ND-5000.
- OctobusFabric.cs: IOctobusFabric + OctobusFabric (64-slot registry,
  dest->source rewrite, unicast/broadcast, synchronous ushort[] replies).
- OctobusND5000Station.cs (1805 lines): ACCP + AIB/AOB + OMD-0/OMD-3
  consumers + the whole O1 mailbox servicer (IServicerHost,
  Nd500MicrocodeServicer, X5ACT/X5FIF, doorbells, threading).
- OctobusStationBase (inside NDBusOctobus.cs): frame builders,
  SendMultibyteMessage, AND the full OMD-0 test-protocol responder.
Tests: Emulated.Tests.ND100\ControllerOctobus\ (TPE B00 suite, ND5000
station suite, mailbox O1 suite, phase-3 exec/MON/restart/trap/threaded,
boot harness).

Coupling problems for reuse (verified in code):
1. OctobusStationBase mixes transport with the OMD-0 responder and
   ND-5000-flavored identity (ModuleTypeCode default, Domino info strings).
2. NDBusOctobus's constructor hard-creates the ND-5000 MPM window and a
   placeholder SCSI station; ControlWordBits handling reaches directly into
   _nd5000Station (ContinueACCP).
3. IOctobusFabric.SendFrame returns replies synchronously, forcing the
   latency/busy-retry timing model into the ND-100 card instead of the bus.
4. OctobusND5000Station fuses three roles: bus station, ACCP firmware,
   and mailbox servicer host.
5. Multibyte assembly (SOMB/data/EOMB state machine) is duplicated in
   OctobusSimpleStation and OctobusND5000Station.
6. OctobusCommandCode / OctobusKickNumber / OctobusMessage (ND-5000
   vocabulary) live inside the card class.
7. (DEV-6) EXISTING cross-thread race: _inboundDelay is a plain Queue
   enqueued on the CPU run thread (AnswerWritten -> fabric -> adapter)
   and dequeued on the ND-100 device thread in Clock() with no lock.
8. (DEV-3) X5SEM take is read-check-write under a PER-STATION lock -
   exactly the pattern IServicerHost.TryTakeSemaphore's own doc forbids
   (audit item F-oct-2); must become one shared Interlocked primitive
   per MPM window.
9. (DEV-2) Single-CPU hardwiring: _nd5000Station is one field (AttachCpu
   REPLACES it), the MPM hooks (TryOverrideMpmRead / NoteMpmRead /
   NotifyMpmZeroWrite) reach only that one station, and X5ACT
   self-discovery latches the FIRST plausible write.

---

## 7. Reusable octobus objects - proposed design

Goal: one shared kernel used by (a) the ND-100 card, (b) the ND-5000 CPU
station, (c) every future DIOC, matching the real hardware split
(OBCON/OCTC chip = transport; firmware = OMD endpoints + personality).

Proposed namespace layout (all inside Emulated.HW.ND.CPU.NDBUS, new files;
no behavior change in step 1 - see plan):

### 7.1 Octobus core (transport - models the OBCON/OCTC chip + wire)

- OctobusFrame (static helpers + readonly struct): compose/decompose the
  16-bit frame, frame-type classification (Emergency / Kick / Ident /
  Somb / Eomb / Data), field extraction. Single home for the bit layout
  (today spread across three classes).
- OctobusFrameBits, OctobusStationType, OctobusKickNumber,
  OctobusCommandCode, OctobusEmergencyCode: move to their own files;
  keep byte values untouched (they are carve-verified).
- IOctobusFabric / OctobusFabric: as today (registry, dest->source
  rewrite, broadcast) PLUS an async delivery option: SendFrame keeps its
  synchronous return for compatibility, and a new
  PostFrame(source, frame) queues delivery through a per-station inbound
  queue ticked by Clock(). Long term the latency model (INBOUND_LATENCY,
  busy-retry on full FIFO) moves from NDBusOctobus into the fabric, where
  the real bus arbitration lives. Ack outcome surfaced as an enum
  (Ok / NotPresent / Busy / ParityError) instead of card-internal flags.
  DESIGN CAVEAT (DEV-6): this move is an INTERFACE REDESIGN, not a
  relocation - busy-retry is driven by the RECEIVER's FIFO occupancy,
  which the fabric cannot see through HandleFrame today; it needs (a) an
  Ack/busy back-channel on IOctobusStation, (b) a fabric tick source
  (only NDBusOctobus has Clock()), (c) preservation of the dest-0 /
  own-station loopback bypass which must NOT be delayed, (d) an explicit
  thread-safety discipline (the current _inboundDelay queue already races
  CPU-thread enqueue vs device-thread dequeue). Do not attempt "behind a
  flag" until all four are answered - see phase B TODO B7.

### 7.2 Station kernel (firmware building blocks)

- OctobusStationBase (slimmed): station number/type, fabric attach, frame
  builders, SendMultibyteMessage. NOTHING else.
- MultibyteAssembler (new, ONE PER STATION - "one per OMD" is impossible
  at the wire level because data frames (C=0) carry no OMD field; the open
  message is keyed by the OMD captured at SOMB, DEV-4): the SOMB/data/EOMB
  collection state machine, source-station tracking, delivering a completed
  (message bytes, source, omd) tuple to a callback - the SOURCE must travel
  with the message because ACCP replies (Messack/VparpEcho/Messnak) consume
  it long after collection (DEV-4). Enforce the 255-byte body limit
  (HW-A10). Zero-alloc: reusable growable buffer / ArrayPool, not a fresh
  List<byte> per message (DEV-12). Kills the current duplication.
- IOmdEndpoint + OmdDispatcher (new): register consumers per OMD number
  (0-15), backed by a fixed IOmdEndpoint[16] array, not a Dictionary
  (DEV-12). Station's HandleFrame decodes frame type once and routes
  multibyte traffic to the dispatcher, kicks to an IKickSink, idents to an
  IIdentSink, emergencies to an IEmergencySink. MUST carry a per-station
  ADMISSION-POLICY hook (frame-class x OMD predicate): the ND-5000's
  _accpIdle gate passes OMD-0/3 multibyte while terminated but drops
  kicks/idents/other - byte-verified behavior locked by
  Emergency_TerminateAndContinue_ControlIdleLoop (DEV-4). This mirrors
  SINTRAN's CENTRY tables (16 OMD entries, 16 kick entries, ident entries)
  and the ACCP's OMD 0/3 vs forward-to-AOB split - both become
  configurations of the same dispatcher.
- OctobusTestProtocolEndpoint (new, implements IOmdEndpoint for OMD 0):
  the existing byte-verified responder lifted out of OctobusStationBase,
  parameterized by an IStationIdentity.
- IStationIdentity (new): StationNumber, StationType, ModuleTypeCode
  (1 Domino / 2 MFbus / 3 ACCP), DominoProcessorType, OpcomVersion,
  CompileTime, TestVersion. The test protocol and future IdentY/OPCOM
  emulation read identity from here instead of from base-class properties.
- PowerMessageSource (later): emits 376B/377B broadcasts on machine
  power-up/down per ND-14001 section 4.5 (source-station semantics from
  section 3.5 above).

### 7.3 Shared-memory seam

- ISharedMemoryWindow (new): word/dword read+write with bounds checking,
  implemented over the existing RAM MPM window. OctobusND5000Station's
  ReadNd100Word/WriteNd100Word guards become this object; a future DIOC's
  DMA and NUCLEUS-kernel access use the same seam. NOTE (DEV-5): the ACCP
  side needs this seam too (CS-load machinery reads/writes MPM), not just
  the mailbox host. Semaphore helper (TrySet/Release, models MFbus TSET):
  MUST be ONE SHARED object per MPM window - not per station - implemented
  with Interlocked CAS against the backing array (DEV-3; the current
  per-station lock + read-check-write is the pattern IServicerHost's own
  documentation forbids, and with multiple CPUs four different locks would
  guard the same shared word). Both the X5SEM mailbox lock and the NUCLEUS
  LOCK field use it. The emulated ND-100 CPU's TSET instruction path must
  be audited against the same primitive (DEV-13 [UNVERIFIED]).
- DoorbellLine (new, small): the "signal + drain on consumer thread"
  pattern currently hand-built in OctobusND5000Station (kick pending flag,
  MailboxDoorbell event, DrainDoorbells) - reusable for any station with
  its own execution thread. Model as TWO flags with distinct drain
  semantics (kick-walk vs activation-drain) matching the current code, or
  the threaded suites break (DEV review, phase A TODO A11).

### 7.4 Device station skeleton

- OctobusDeviceStation (new): OctobusStationBase + MultibyteAssembler +
  OmdDispatcher + OctobusTestProtocolEndpoint + IStationIdentity, with
  virtual hooks OnKick(number, source), OnIdent(number, source),
  OnEmergency(code, source). This replaces OctobusSimpleStation (which
  becomes OctobusDeviceStation with defaults) and is the base every DIOC
  emulation derives from.
- OctobusND5000Station refactor: split into
  (a) AccpAccessModule (AIB/AOB, OMD-3 command library, kicks-enabled
  state, selftest/CS-load state machine) - a component owned by the
  station; (b) the station itself deriving from OctobusDeviceStation
  (ModuleTypeCode=3); (c) Nd5000MailboxHost (the IServicerHost half,
  owning ISharedMemoryWindow + DoorbellLine + Nd500MicrocodeServicer).
  Public surface and all test-visible behavior unchanged - which in
  practice means the station REMAINS A FACADE forwarding to both halves
  (DEV-5): LSysparWord1 is captured on the ACCP path but consumed by the
  mailbox host's GIVEINT; the CS-load machinery is ACCP-side but touches
  MPM; the _accpExchangeLog diagnostic ring spans both; ~38 test call
  sites depend on the current signatures. Budget for the facade, do not
  pretend a clean cut.
- NDBusOctobus cleanup: constructor stops force-registering the placeholder
  SCSI station (machine config decides); ContinueACCP routed as a normal
  emergency frame through the fabric instead of the direct field call;
  MPM window creation stays (it models PCB 3109 = octobus + MPM line
  driver) but its geometry moves to constructor parameters.

### 7.5 What deliberately stays non-generic

- The ND-100 card's IOX register map, interrupt FFs, ident codes: that IS
  the device (PCB 3109), keep in NDBusOctobus.
- The ND-5800 microcode servicer and X5ACT/X5FIF layout: SINTRAN/ND-5000
  contract, stays in the ND-5000 components.
- NUCLEUS kernel structures: implement later inside the DIOC/NUCLEUS layer
  (section 8 phase D), not in the octobus core - NUCLEUS runs ON TOP of
  the bus, exactly as in the real system.

---

## 8. Implementation plan

REWRITTEN 2026-07-20 after the three critical reviews (see
[OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md](OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md)
for the finding IDs cited below). Every phase now has an ordered TODO task
list; each task names its file(s) or evidence source and its verification
gate. Task type tags: [code], [test], [carve], [live-trace], [doc].

Ordering rule: every phase ends with the FULL existing octobus test suite
green: OctobusControllerTests, OctobusND5000Tests, OctobusMailboxO1Tests,
OctobusTpeConfigReproTests, the five OctobusPhase3* suites
(Exec/Mon/Restart/Trap/Threaded) in Emulated.Tests.ND100\ControllerOctobus\,
plus OctobusTpeBootHarnessTests, OctobusMachineBringupTests and
Nd100SintranNd5000OctobusBootHarnessTests in Emulated.Tests\ND100\ (suite
names verified against the repo, DEV review; note
OctobusTpeBootHarnessTests.cs is UTF-16 - use Select-String, not ripgrep).
HONESTY CORRECTION (DEV-1/DEV-7): phases A-B are NOT literally
"zero behavior change" - the SCSI-placeholder move edits test assertions,
and several behaviors are currently unlocked by any test. The rule is
therefore: lock-tests FIRST, then refactor, and every deliberate test edit
is named in its task.

### Phase A - extract the core (refactor only)

Step 0 is writing the missing lock-tests (DEV-7): the refactor gate is only
as strong as the tests, and these behaviors are currently uncovered.

- [ ] A1 [test] Lock OMD-0 commands 0x0016 "Get Domino Information" and
      0x0018 "Get test version" exact reply bytes incl. ASCII padding
      (NDBusOctobus.cs:487-505) in OctobusND5000Tests. Gate: green on
      CURRENT code.
- [ ] A2 [test] Lock malformed-OMD-0 silence: length < 6, EOMB without
      SOMB, echo-multi wordCount > 121 rejection (NDBusOctobus.cs:330-349,
      438-439). Gate: green on current code.
- [ ] A3 [test] Lock OctobusSimpleStation "OMD != 0 multibyte acked and
      dropped" (NDBusOctobus.cs:667-693) and LastAccpMessage set on OMD-0
      completion too (OctobusND5000Station.cs:1041). Gate: green on
      current code; preserved across the split.
- [ ] A4 [test] Lock the ControlWordBits.ContinueACCP register path incl.
      the _mudomDetected side effect (NDBusOctobus.cs:2453-2461) in
      OctobusControllerTests - protects phase B task B5 (DEV-8). Gate:
      green on current code.
- [ ] A5 [code] Create OctobusFrame + split the enums into new files
      (byte values untouched - carve-verified); replace inline mask
      literals in NDBusOctobus.ProcessTransmitFrame/ParseCommand and
      OctobusFabric.SendFrame. Gate: full suite green.
- [ ] A6 [code] MultibyteAssembler per section 7.2 as amended: ONE per
      station, (bytes, source, omd) delivery, 255-byte limit, reusable
      buffer (DEV-4, DEV-12, HW-A10). Gate: OctobusND5000Tests +
      OctobusTpeConfigReproTests green.
- [ ] A7 [code] OmdDispatcher/IOmdEndpoint with the admission-policy hook
      so the _accpIdle gate (OMD-0/3 pass, kicks/idents/other drop while
      terminated) is expressible unchanged (DEV-4). Fixed
      IOmdEndpoint[16] array. Gate:
      Emergency_TerminateAndContinue_ControlIdleLoop +
      Kick_IgnoredInTestInitMode green.
- [ ] A8 [code] Extract OctobusTestProtocolEndpoint + IStationIdentity
      from OctobusStationBase (NDBusOctobus.cs:284-610), identity
      properties settable, station-facing property forwarding kept so
      existing setters (ModuleTypeCode = 3) compile unchanged. Gate: all
      TestProtocol_* tests + A1/A2 tests green byte-for-byte.
- [ ] A9 [code] Rebuild OctobusSimpleStation on the new skeleton; switch
      OctobusND5000Station OMD-0/3 collection to MultibyteAssembler.
      Gate: FULL suite (all 12 suites above) green, dotnet build clean.
- [ ] A10 [code] Extract ISharedMemoryWindow (bounds-guarded word
      read/write over the MPM RAM); consumed by BOTH the future mailbox
      host AND the ACCP CS-load code (DEV-5). Do NOT include the
      semaphore helper yet - it needs the Interlocked design from phase E
      task E1 (DEV-3). Gate: OctobusMailboxO1Tests green.
- [ ] A11 [code] DoorbellLine last, modeled as TWO flags with distinct
      drain semantics (kick-walk vs activation-drain,
      OctobusND5000Station.cs:590-767). Gate: ThreadedDoorbell_*,
      FullThreadedPath_*, DrainDoorbells_* + OctobusPhase3ThreadedTests
      green.

### Phase B - ND-5000 station split + card cleanup

- [ ] B1 [code+test] FIRST fix the existing _inboundDelay cross-thread
      race (CPU-thread enqueue NDBusOctobus.cs:2995 vs device-thread
      dequeue :3017, no lock; DEV-6) with a lock or single-producer
      handoff, plus a threaded regression test. Gate:
      OctobusPhase3ThreadedTests + ThreadedCanary green 10 consecutive
      runs.
- [ ] B2 [code] Split AccpAccessModule out of OctobusND5000Station
      carrying: AIB/AOB queues, OMD-3 library, _accpParameterPointer /
      _controlStore / _ducs*, TryOverrideMpmRead, an ISharedMemoryWindow
      reference; publish LSysparWord1 to the station for the mailbox
      host's GIVEINT (DEV-5). Station keeps public forwarders. Gate:
      OctobusND5000Tests + Phase3 suites green.
- [ ] B3 [code] Split Nd5000MailboxHost (IServicerHost implementation,
      ConfigureMailbox/ServiceMailbox/WalkQueue/doorbells, X5ACT
      discovery) behind the station facade; keep AttachRealCpu /
      ServiceMailbox / DrainDoorbells signatures (~38 test call sites).
      Gate: OctobusMailboxO1Tests + Phase3 suites green.
- [ ] B4 [code+test] SCSI placeholder move (DEV-1): registration of
      station 10 moves from the NDBusOctobus constructor (:1819) to
      machine device-add config (default ON); add explicit
      RegisterStation calls + assertion updates to
      OctobusControllerTests.Setup (:361/:473/:508) and
      OctobusTpeConfigReproTests.Setup (:60-75) - DELIBERATE test edits,
      named here per the honesty rule. Gate: those two suites +
      OctobusTpeBootHarnessTests station-10 asserts (:256/:286) green.
      Also check OctobusMachineBringupTests /
      Nd100SintranNd5000OctobusBootHarnessTests for hidden dependence
      (DEV-13 [UNVERIFIED]).
- [ ] B5 [code] ContinueACCP rerouting (only after A4's lock-test):
      bit-5 handler synthesizes the 242B emergency frame to the
      station's number via the fabric, preserving _mudomDetected;
      document that the control-register bit itself is
      hardware-UNVERIFIED (DEV-8). Gate: A4 test +
      Cmmacle_ViaIoxWrite_ResetsCpu green.
- [ ] B6 [code] MPM geometry: optional constructor parameters defaulting
      to 0x420000 / 8 MB; window stays default-ON (SINTRAN KMPM5
      LOCAL-vs-MPM5 probe depends on it); collapse the three duplicate
      constant sites (constructor, ND100Machine.ND5000.cs:65-66,
      PARAM_REGION_BASE) onto one source (DEV-9). Gate:
      OctobusMachineBringupTests +
      Nd100SintranNd5000OctobusBootHarnessTests green.
- [ ] B7 [code, OPTIONAL - own mini-design first] Fabric-owned
      latency/busy-retry per the section 7.1 caveat (DEV-6): requires an
      Ack/busy channel on IOctobusStation, a fabric tick source, the
      dest-0/own-station bypass exemption, and thread-safe queues. Do
      NOT attempt "behind a flag" until all four are designed. Gate if
      attempted: full suite green with the flag BOTH ways, pump budgets
      unchanged (no MaxPumpTicks increases).

### Phase C - generic DIOC station bring-up (discovery level)

Deliverable: OctobusDeviceStation instances for SCSI (module 21B, default
station 13B), ETH3 (module 22B, station 12B - non-overlapping defaults per
SIN-F10, matching the live LIST-CONFIGURATION examples), MFbus controller
(2-7), each with correct IStationIdentity.

WIRE-CONTRACT CORRECTIONS driving this phase (SIN-F2/F3/F4/F8): OMD 4 is
ND-100 -> MF only; MF -> host records go to station 1 on the
runtime-announced 5OMDNO; SINTRAN discards MF acks with no aliveness
bookkeeping, so the MFPREPARE "contract" is nearly assertion-free; SCSI
stations get NO bring-up traffic from SINTRAN; default stations must stay
SILENT on OMD 4 unless configured as MF controllers.

- [ ] C1 [carve] CONOMD @040062 + ECONID @040467 bodies (026-S3IMPIT,
      dd-anchorable): which OMD number CONOMD allocates first
      (deterministic?), confirm CON5OMD generates no wire traffic,
      OMDENT/KICKENT entry semantics (SIN-F7; catalog section 8 lists
      this RE as OPEN). Prerequisite for C4/C5/C6 oracles.
- [ ] C2 [carve] Resolve the L07 MFACK-addressing anomaly (SIN-F3): ODR
      147207-147220 reads the received-byte-count cell as the ack
      destination - re-examine whether 9FLER rewrites LMFIELD, or
      single-step live. Until resolved the C6 ack oracle is [OPEN].
- [ ] C3 [code] Emulated MF-controller station (only when configured, on
      stations 2..6): parse incoming OMD-4 multibyte; on
      CMSYSPAR|N100IDENT (body 0E 01 xx) capture 5OMDNO from body byte 2
      (ODR section 10 [V]). Absent-configured stations answer NOTHING
      (SIN-F8). NVRAM-provisioning defaults PF dest=1, BT=0 per HW-A3.
- [ ] C4 [code] MF ack emission (optional per SIN-F4): multibyte to
      station 1 on the captured 5OMDNO, ETYPE word 0x0000, 2-byte 00 00
      body (full-word compare + odd-byte zero-fill uncertainty, SIN-F4
      NIT).
- [ ] C5 [code] MF error-record emission: multibyte to station 1 on the
      captured 5OMDNO (NEVER OMD 4, SIN-F2), ETYPE high byte = SEC code
      (20B/30B/31B/50B/51B/77B), record length per the C2 resolution.
      One record at a time until OMBREAD multi-pending semantics are
      carved (SIN-F6).
- [ ] C6 [test] Boot-harness assertions: (a) ND-5000 bring-up unchanged;
      (b) MFPREPARE observed to every station 2..6 with body
      0E 01 <5OMDNO>; (c) absent stations produce fabric Ack=00;
      (d) ZERO unsolicited SINTRAN frames to stations 10B-13B (SIN-F8);
      (e) if an MF station is configured, its error record reaches
      5OMBREAD (observable via the 9FLER SEC log); ack oracle per C2's
      outcome.
- [x] C7 DONE 2026-07-20 [V] (= SCSI plan S0-4): SAFE - PROMAN never
      runs at boot on this image; stations 10B-13B get no boot-protocol
      traffic (PROMAN-AUTORUN-RECON.md; section 9 Q10).
- [ ] C8 [test] TPE OCTOBUS conformance: RUN tests 4/5/6 against the new
      stations with correct module types; plus reproduce RUN tests 1-3
      (self-loop, bit patterns, FIFO-length detection - the latter
      documents the emulated FIFO depth, HW-A6) as unit tests.
- [ ] C9 [test] Fabric conformance for broadcast ack semantics: ambiguous
      (11) on broadcast = 0 retries, busy = 255, not-present = 15
      (ND-14001 Figure 32; HW-A9) - required before/with B7. Station 77B
      behaves as "not present -> Ack 00", never an exception (HW-A2).
- [ ] C10 [test] Pin the ACCP OMD-0-in-test-mode behavior vs the
      ND-05.020.01 5.3.48 "only OMD 3" statement with a test + doc note
      (HW-A10 tension).
- [ ] C11 [live-trace, only if C1/C2 stall] Single-step XX5CONOMD on the
      live harness (dap-debugger) to observe the real 5OMDNO and the
      MFACK destination station.
- [ ] C12 [live-trace, cheap, closes section 9 Q1] When a live ND-5000
      rig is next available: capture the AOCP-received emergency byte
      for a SINTRAN-issued 241B master clear (HW-A12).

### Phase D - DOMINO substrate (carve first, then code)

SCSI-SPECIFIC EXPANSION (2026-07-20): the SCSI controller now has its own
detailed plan expanding this phase -
[SCSI-DIOC-OCTOBUS-EMULATION-PLAN-2026-07-20.md](SCSI-DIOC-OCTOBUS-EMULATION-PLAN-2026-07-20.md)
(phases S0-S5, carve-first, full disk I/O). Tasks D2/D3/D7/D8/D13 below
are refined there as S0-1/S0-2/S3-x/S4-2; tick them in BOTH places.
CARVE STATUS UPDATE (same day, 2026-07-19): targets 1-3 below are DONE and
byte-verified - docs in
tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\
(BDIO-DOMINO-DRIVER-CARVE.md, OCTOBUS-DRIVER-ROUTINES-CARVE.md,
NUCLEUS-PRIMITIVES-CARVE.md, NUCLEUS-SEGMENTS-RECON.md). Headline results:
BDIO fn 166B read / 167B write / 213B compare via DOMDF=041064 + NKWRI;
DCNVA address rule ((addr - ADRZERO<<10) << 1) | bit31; NUCLEUS kernel
structures byte-proven (port +20 = KICK DEST octobus station, kick table
14B-word entries) and the NKSEND -> SKICK(kick 1 NUCKI) path; MON 347B =
NUCLEUS SERVE 047072. (The original wording "only target 4 remains open"
was oversold - see the REVIEW CORRECTION after the target list.) Original
target list kept below for provenance. The 2026-07-19 survey shows the HOST side is fully carvable
from material already in the repo; only the CONTROLLER-side firmware is
missing. Carve targets in priority order (bytes + NPL locations verified
by the survey):
1. STRBDIO/REBDIO + MTRANS-to-DOMINO conversion (MP-P2-DISK-START.NPL
   lines ~146-430, addr 073633B) - the BDIO fn-167B message build, NKWRI
   send, completion/HSTAT decode. NPL source present -> LOW difficulty;
   yields the actual host-side disk command-block format.
2. XKICK500/LV12KICK/SKICK (146322B/037254B) + 5OMBREAD/OMBREAD/MFPREPARE
   (146550B/037660B/147100B) + 5MTRANS (143445B) - routine-level annotation
   in 026-S3IMPIT (.asm committed) + SINTRAN-DATA_commoncode. LOW-MED.
3. NUCLEUS primitives NKSEND (042171B), NKGETINFO (043672B), NCALL
   (050407B), ENKIC (047526B), NKWRI - no NPL bodies; real disassembly
   against SYMBOL-1-LIST + N500-SYMBOLS. MED-HIGH. This yields the
   NUCLEUS kernel byte layout (master block, descriptors, kick table)
   that ND-820026 only names.
4. NKSE/NKNA server segments (104/105, 106/107) and PROMAN (120/121) /
   BOPCOM (124/125) segment interiors - carved .bin exists, never
   disassembled. HIGH, lower priority.
Still genuinely missing (not carvable from this repo) [OPEN]:
- Controller-side firmware: PROMAN boot images, DOMINOS, PMA-SCSI-BDIO /
  PMA-ETH3-* images - absent from repo and F: (manuals only). The
  PROMAN <-> OPCOM boot byte protocol (EchoTest, IdentY, SetBxP, BxDoLd,
  RegMod, Go-On) therefore needs a live trace or a diskette image find.
  BUT the manuals define the OBSERVABLE contract (reset state machine,
  SetBxP NAK/timeout, ERS 105042B-105045B event ladder, LED patterns -
  section 4.4 as expanded per HW-A14) - a stub DIOC is specified by
  "produces exactly these PROMAN-visible outcomes".

REVIEW CORRECTION (SIN-F1/F5): the earlier "carve targets 1-3 DONE, only
target 4 remains" OVERSOLD completeness. The carves cover the HOST's view;
the DIOC side still touches uncarved structures. The honest gap list is
SIN-F5 a-h in the review doc; the true blocking carves are D2 (CONKI kick
binding) and D3 (DOMDF initializer / provisioning), NOT the previously
listed target 4. Deferrable-with-justification: ENKIC overlay, OMBREAD
counters, WT12/NFUNC internals, PROMAN byte protocol (SIN-F6). And the
earlier "kick-5 wiring" instruction was WRONG (SIN-F1): host->remote
NUCLEUS kick is byte-verified kick 1 (NUCKI); the remote->host kick number
is [OPEN] until CONKI is carved.

TODO (ordered):

- [ ] D1 [doc] DONE 2026-07-20 with this rewrite: kick-5 claims corrected
      in sections 3.4, 4.4 and the section 5 matrix (SIN-F1).
- [x] D2 [carve, BLOCKING] DONE 2026-07-20 [V] (= SCSI plan S0-1):
      incoming octobus KICK 1 dispatches to DKICK @044747 (CONKI(T=1,
      A=14B=PIL level 12, X=0, B=125144); receive chain KICKENT[frame &
      17B] -> level-12 fire with P := mem[125143] = DKICK). Both
      directions of the NUCLEUS kick are now kick 1 [V]. Doc:
      re\domino-nucleus-io\CONKI-KICKENT-CARVE.md.
- [x] D3 [carve, BLOCKING] DONE 2026-07-20 [V] (= SCSI plan S0-2): the
      initializer is FILSYS 006-S3FS (QUINI @134206 lazy init -> DLPRT/
      DSVER via MON 347 fn 1; PDF.DRPRT via DOPPR = MON 347 fn 3
      open-port-by-NAME). DSVER+32..67 "static header" DISPROVEN
      (don't-care tail). Unit binding = per-pool NAMED port + DXPOO/
      OPAIN. BDTMU/BDTMV are in RPIT not MPIT (poisoned prior killed).
      Doc: re\domino-nucleus-io\DOMDF-INITIALIZER-CARVE.md. Remaining:
      MON 347 request/answer layouts -> D4/segment-105 carve.
- [x] D4 [carve] DONE 2026-07-20 [V] (= SCSI plan S0-3): segment 105 =
      PLANC-compiled NUCLEUS server; doNuc dispatcher @037033 (fns
      1..14B, table dd-verified); fn 10B @047432 = descriptor
      create/provision writing port +20 KICKDEST / +30 OWNID (matches
      kernel layout). Remaining [OPEN] tail: DRPRT/DLPRT sub-offset
      pin, freelist head (runtime global), full NCALL per-word map,
      fn 11B-14B bodies - closable via SYMBOL-2-LIST pinning or the
      D13/S4-2 live round trip. Doc:
      re\domino-nucleus-io\NKSE-SERVER-INTERIOR-CARVE.md.
- [ ] D5 [code] NucleusKernel over ISharedMemoryWindow implementing ONLY
      byte-verified layouts (NUC section 4): master block
      (+2/+7/+20/+25/+74/76), 40B-word descriptors (LOCK/TYPE/OWNER;
      port +10/+12/+14/+16/+20/+21/+22/+30; message +10/+12/+14/+21;
      buffer +23/+25/+26), kick table 14B-word entries, TSET value
      070000B. Every controller-side behavior mirrored by symmetry gets
      an [I-symmetry] code comment (SIN-F5h).
- [ ] D6 [code] DIOC receive path: on kick (number per D2), lock own
      port, pop MESS HEAD, read buffer +26.. as the BDIO record
      (DSVER-relative layout, BDIO section 2), honoring INQUEUE.
- [ ] D7 [code] BDIO -> SCSIHDD mapping, HAPPY PATH only: fn 166B read /
      167B write / 213B compare; sector = DSTBL double; length = DNRPG
      pages x 2000B words (5MTRANS Domino path, ODR 11.1 [V]); memory =
      DMYAD with bit31 interpretation mirrored from ND-500 Port-B -
      documented [ASSUMPTION], no DIOC-side verification exists
      (SIN-F5g). Unit selection config-injected until D3 lands (comment
      the uncarved DSVER+32..67 as the suspected real carrier). Reuse
      SCSIHDD; do NOT reinvent the SCSI bus layer.
- [ ] D8 [code] Completion write-back EXACTLY to REBDIO's byte-verified
      read contract (BDIO section 4): DSSTS := (0,0) on success; error ->
      status into DSSTS low word (only 104031/104651/104622 have a
      carved-distinct outcome, HSTAT -5; anything else -> -4 + SINEC
      1662); leave DSQCN alone unless emulating the mirror-pool case;
      append message to HOMEPORT queue, set INQUEUE, kick station 1 with
      the D2 kick number. Status-code semantics beyond the carved trio
      are [OPEN] - use the ND-814009 tables (104601B-104677B,
      105301B-105377B, HW-A18) as the naming vocabulary, but restrict
      TESTS to the carved discriminations.
- [ ] D9 [test] Unit tests: synthetic DOMDF/PDF/QUDF + NUCLEUS area,
      drive STRBDIO's exact NKWRI/NKSEN register contract (BDIO section
      2 [V]), assert the full HSTAT ladder -1/-2/-3/-4/-5/0 against
      REBDIO behavior; DCNVA tests must cover the error branch (-3
      outside multiport) and the self-modifying bias cache, not just the
      formula (SIN-F12).
- [ ] D10 [code] Watchdog/heartbeat model: 105314B "Missing DOMINO
      heartbeat, controller aborted" + 105312B/105313B init statuses
      (HW-A18); ARES latched-reset >= 100 ms CONTINUE timing in
      AccpAccessModule emergency handling (HW-A12).
- [ ] D11 [code, when warranted] Crate-configuration flow for the
      MFbus-controller station per ND-14001 4.8.1 (identify-yourself
      broadcast, RMT scan, 77B-downward probe, WOI double-write to
      slot+10B, MASTA bit 7 -> OBRES): fully specified at
      register/sequence level, needs no firmware (HW-A15).
- [ ] D12 [doc] Before any PROMAN/OPCOM stub: transcribe from
      ND-820026.1 the reset state machine (aborted/WAITCONT), Appendix B
      selftest numbers/names, ERS 105042B-105045B + 105027B NAK
      semantics; define the stub's observable contract from them
      (HW-A14).
- [ ] D13 [live-trace] Once D2/D3 land: boot harness with a configured
      DOMINO disk, capture the first real STRBDIO -> kick -> completion
      round trip, diff every DOMDF word against the emulated DIOC's
      expectations - the only way to close DSVER+32..67 without firmware.
- [ ] D14 [doc] Explicitly out-of-scope ledger (so nothing is silently
      re-assumed solved): ENKIC overlay, OMBREAD counters, WT12/NFUNC
      internals, PROMAN byte protocol, NucleusKernel debug output
      cross-checked against the ND-820026 masterblock display semantics
      (HW-A17).

### Phase E - full 5000 CPU + multi-CPU

Continues [ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md](ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md)
(stations 70B-73B, per-CPU mailbox ext blocks at X500DF + n*200B, CMSYSPAR
per CPU) on top of the phase A/B kernel.

REVIEW CORRECTION (DEV-2/DEV-3): the earlier claim "the second CPU is just
a second OctobusDeviceStation-derived ACCP" is CONTRADICTED by current
code: AttachCpu REPLACES the single _nd5000Station field; the MPM hooks
(TryOverrideMpmRead/NoteMpmRead/NotifyMpmZeroWrite) reach only that one
station, so CPU 2 would never wake on X5ACT and its CS-load read-back is
dead; X5ACT self-discovery latches the FIRST plausible write and
mis-configures with multiple CPUs; and the per-station X5SEM lock breaks
mutual exclusion entirely with 2+ stations. Also [UNVERIFIED]: the
multi-CPU plan's "Interlocked CAS path already designed" - no such
implementation exists in the station (DEV-3). Real interface work,
ordered:

- [ ] E1 [code+test] The SHARED X5SEM primitive FIRST: one semaphore
      object per MPM window (not per station), Interlocked CAS against
      the backing array, used by every station's
      IServicerHost.TryTakeSemaphore AND audited against the emulated
      ND-100 TSET instruction path (closes audit item F-oct-2 and
      DEV-13). Gate: contention unit test with 2+ servicer threads plus
      a simulated ND-100 TSET hammering one semaphore word.
- [ ] E2 [code] NDBusOctobus: single _nd5000Station field becomes a
      per-station registry; AttachCpu ADDS instead of replacing
      (:1862-1885); ND5000Station property = "first attached" for
      back-compat. Gate: AttachCpu_RegistersSamsonStation + full suite
      green.
- [ ] E3 [code] Fan out TryOverrideMpmRead / NoteMpmRead /
      NotifyMpmZeroWrite (:1612-1668) across all registered CPU
      stations, each filtering by its own LPARP/DUCS state and mailbox
      geometry. Gate: existing single-CPU CS-load and X5ACT tests green
      + a new two-station test proving CPU 2 receives its own
      activation.
- [ ] E4 [code] CPUNO-aware X5ACT discovery: first discovery fixes the
      SHARED header; every station derives its ext block as
      header + ownCpuNumber*256 and accepts ONLY writes at exactly its
      own X5ACT address (DEV-2c). Gate: 4-station unit test where each
      of four X5ACT writes wakes exactly its own station.
- [ ] E5 [code] Wire the Nd5000CpuConfig surface (multi-CPU plan phase
      1) so CpuParameter/CpuTypeAndModel/SystemParameters stop being the
      hardcoded 5800 model-8 0x03E1 (OctobusND5000Station.cs:452,
      DEV-10). Gate: 3RMICV reply reflects per-CPU config in a unit
      test.
- [ ] E6 [code] ND100Machine.AttachNd5000Cpu uses the new multi-station
      path (removes the limitation note at ND100Machine.ND5000.cs:38-43)
      with per-CPU DetachAllNd5000Cpus teardown. Gate: attach CPUs 1+2,
      both stations answer CH5CPUPRESENT-shaped frames, GIVEINT frames
      carry each CPU's own station as source.
- [ ] E7 [test] Only then the multi-CPU boot harness (SINTRAN
      CH5CPUPRESENT scan over 70B-73B) as the phase acceptance gate.
      Gate: harness green + full octobus and ND-500 suites unchanged.

---

## 9. Open questions (consolidated)

1. [OPEN, downgraded to low priority per HW-A12] Emergency 241B vs 361B:
   now ANALYZED (section 3.5) - Appendix A self-consistently claims the
   0xF1 family (361B = 241 decimal), NPL + ND-14001 anchor the 0xA1
   family; emulator values stand; one live-trace byte closes it (phase C
   task C12).
2. [UNCERTAIN] The six broadcast-type (BT) encodings - named everywhere,
   enumerated nowhere. Partial anchors (HW review): broadcast destination
   = "one of six station types" (ND-05.020.01 page 331); module-type field
   = "class of module accessible in broadcast mode" (ND-14001 ch 3); BT
   written via WOI second byte (ND-14001 3.6); receiver default BT = 0
   (ND-05.017.01 ch 8). Emulator broadcast-by-registered-station stands.
3. [OPEN] OMD allocation model beyond 0/3/4/5OMDNO (section 3.3); CONOMD
   carve (phase C task C1) will pin SINTRAN's side of it.
4. [OPEN] PROMAN/OPCOM boot protocol bytes and controller-side firmware
   images (PMA-*-*) - not in repo, needs live trace or media find. The
   OBSERVABLE contract IS documented (section 4.4 / HW-A14). Host-side
   formats are carved (phase D status).
5. [UNCERTAIN] Station-side meaning of test-protocol register functions
   0/2/6 (read) and 3/5/7 (write) - emulator serves a plain register
   file. Live-capture recipe: ND-05.017.01 ch 8 octobus-driver dialogs
   (TRANSMIT-OCTOBUS raw frame send max 5 bytes, READ-OCTOBUS-RECEIVE,
   interrupt channel 6 disable requirement).
6. [UNCERTAIN] Get-Domino-Information real field values (processor type,
   OPCOM version, compile time) - placeholders until a live capture (same
   ch 8 recipe as Q5).
7. [OPEN] What runs on octobus interrupt levels 10/11 in SINTRAN (OCSTART
   wires them; no consumer found in the carve yet).
8. CLOSED 2026-07-20 [V] (was: remote->host NUCLEUS kick number):
   incoming KICK 1 dispatches to DKICK - CONKI carve, phase D task D2 /
   SCSI plan S0-1 (re\domino-nucleus-io\CONKI-KICKENT-CARVE.md).
9. [OPEN, NEW per SIN-F3] L07 MFACK destination anomaly (received byte
   count used as ack station) - phase C task C2.
10. CLOSED 2026-07-20 [V] (was SIN-F9 PROMAN auto-run): SAFE - PROMAN
    never runs at boot on this image (live RT listing PASSIVE P=0, no
    PMA-* files on pack); DIOC stations 10B-13B get no boot traffic.
    Doc: re\domino-nucleus-io\PROMAN-AUTORUN-RECON.md. Re-open only if
    the harness emulates MF-bus crate interrogation with PMA images
    installed.
11. [UNCERTAIN, NEW per HW-A6] ND-100 octobus card PCB/part numbers
    (3109/3096, 324118) - in no manual in Reference-Manuals\500.

---

Cross-references: this document supersedes nothing; it composes
[OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md) (protocol truth),
[SINTRAN-OCTOBUS-MESSAGE-CATALOG.md](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md) (OS traffic),
[OCTOBUS-TEST-PROTOCOL-RE.md](OCTOBUS-TEST-PROTOCOL-RE.md) (OMD-0),
[HANDOFF-OCTOBUS-EMULATION.md](HANDOFF-OCTOBUS-EMULATION.md) (emulation state) and
[ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md](ND5000-MULTICPU-DOMINO-EMULATION-PLAN-2026-07-18.md)
(multi-CPU phases) into one device-controller view with the reusable-objects
design and phased plan.
[OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md](OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md)
holds the full 2026-07-20 review evidence (DEV/SIN/HW finding IDs cited
throughout this document).
