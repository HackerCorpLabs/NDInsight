# Why the ND Ethernet II 68K firmware posts no SUPERKICK / INT12 during ENNS0's read

Date: 2026-07-23.
Legend: **[V]=VERIFIED** (decoded firmware bytes / read NPL source) - **[I]=INFERRED** - **[OPEN]=not statically decidable**.

Inputs analysed:
- 68K firmware: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin` (MC68000, big-endian, base 0x0, 512 KB, all banks).
- SINTRAN driver: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-PIOC-DRIV.NPL` (PDRIV / PISUPER / PIWKF), `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL` (PIOCM / PISTA / PIKIC).
- Emulator: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`.
- Tooling: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\tools\m68kdis.py`.

---

## ONE-LINE ANSWER

**[V] The firmware's superkick machinery is complete and correct - it initialises the ring
header magic and has a producer that enqueues a ring entry and pulses SCIP 0xEF0180 -> INT12.
[V] But that producer (0xEAA6) is a service of the ENCOS *message-delivery* layer: it runs
ONLY when the firmware processes an inbound message/completion for a port. It is NOT invoked
by the OPCOM subfunction dispatcher (0x1B00 table, subfns 0-5) and NOT by a bare ND->68K
doorbell.** [I] ENNS0 is parked in an RT-wait (MON 124 PRSRV @030427, resume-P 030440)
expecting an RTWAK superkick; that superkick is produced only when the ND-100 first *kicks*
the firmware (queues a request + rings the PWCR.BNDC doorbell) so the firmware's message layer
runs. During the read window the controller is QUIET - no ND->68K doorbell reaches the 68K -
so the message layer never runs, no superkick is enqueued, PISUPER has nothing to dequeue, and
ENNS0 waits forever. **The missing event is the ND->68K kick that would drive the firmware's
producer, not a parked firmware and not a version mismatch.**

---

## TASK 1 - The firmware superkick producer (decoded)

### 1a. Ring-header init (declares "this PIOC supports superkick")  [V]

At **0x7C60** the firmware sets up the superkick ring header cells in its own DRAM and stamps
the magic pattern (VERIFIED bytes):

```
7C60: 2079 0001 8a40   move.l ($18a40).L,A0      ; A0 <- pointer to PODIR cell (0x420)
7C66: 43f9 0002 d350   lea    ($2d350).L,A1
7C6C: 2089             move.l A1,(A0)            ; PODIR (0x420) = 0x2D350 (port directory)
7C6E: 2479 0001 8a34   move.l ($18a34).L,A2      ; A2 <- pointer to PATRN cell (0x414)
7C74: 24bc 5555 aaaa   move.l #$5555aaaa,(A2)    ; PATRN (0x414) = 0x5555 / 0xAAAA  <-- MAGIC
```

- `($18a34)`/`($18a40)` are entries of a data pointer-table at file 0x18a34 that lists the four
  header cell addresses **0x414, 0x418, 0x41C, 0x420** as longwords (VERIFIED - the same table
  also appears mirrored at 0x15cf8 and 0x75cf8). These are exactly the four PISUPER header
  slots (see Task 2). [V]
- Byte-search of the whole 512 KB image: the longword `0x5555AAAA` (superkick magic) appears
  only twice, at file 0x7C76 and its bank-mirror 0x68DC6 - i.e. this single `move.l #$5555aaaa`
  is the ONLY writer of the magic. [V]

So the firmware DOES publish the superkick header (magic 0x5555/0xAAAA at PIOC word 1012B =
68K byte 0x414, port directory at PODIR). PISUPER's pattern gate will therefore pass. [V]

### 1b. The producer body 0xEAA6 / 0xEACC (enqueue + SCIP)  [V]

Entry **0xEAA6** (prologue), body from **0xEACC**. It is passed (in the A6 activation frame)
a port handle and an info longword, looks the port up in the port directory, validates the
ring, then enqueues into the entry array and pulses SCIP:

```
EAEE: 0c6a aaaa 0004   cmpi.w #$aaaa,(4,A2)     ; validate ring magic (LSKPA=0xAAAA)
EAF4: 670a             beq  ...                 ;  mismatch -> moveq #-2 error return
EB38: ...              move.b (10,A0),D0 ...    ; ring-type / lock checks
EB48: 3d68 0012 0022   move.w (18,A0),(34,A6)   ; read head index
EB56: 2430 1816        move.l (22,A0,D1.L),D2   ; entry[idx] (entry array base = +22)
EB5A: 0802 001f        btst  #31,D2             ; entry EMPTY? (bit31 = empty/occupied flag)
EB5E: 664e             bne  ...full...
EB68: 21ae 0018 3816   move.l (24,A6),(22,A0,D3) ; STORE info -> entry
EB7C: 08c5 001f        bset  #31,D5             ; mark entry OCCUPIED
EB80: 2285             move.l D5,(A1)
EB9A..EBA4:            addq/limit head index, write back (18,A0)   ; advance ring head
...
ECEC: 206e 0044        move.l (68,A6),A0
ECF0: 4210             clr.b (A0)                ; (postbox side flag)
ECF2: 267c 00ef 0180   movea.l #$00EF0180,A3
ECF8: 4253             clr.w (A3)                ; ** SCIP mirror 0xEF0180 = 0 -> INT12 **
ED04:                  ...return with D0 status...
```

VERIFIED tail bytes `20 6e 00 44 42 10 26 7c 00 ef 01 80 42 53` = `move.l (68,A6),A0 / clr.b
(A0) / movea.l #$EF0180,A3 / clr.w (A3)`. The immediately preceding `addq.w #1,(4,A1)` and
`addi.w #1,(7,4)` increment postbox counters, exactly like the OPCOM path's
post_and_signal_nd100_scip at 0x1A48 does with 0xEF0080. [V]

So **0xEAA6 IS the superkick producer**: it writes the ring entry (info + bit31 occupied +
advanced head index) and then pulses **SCIP 0xEF0180 -> ND-100 INT12**. This is the strongest
superkick candidate confirmed. (The 0xEACC "XMSG postbox ring @0xEACC" note in the task brief
and this producer are the same routine; entry is 0xEAA6, body 0xEACC.) [V]

### 1c. What invokes the producer  [V]

- Only external control transfer into the 0xEA00-0xED10 block is `0xC2CC: jsr $eaa6`. [V]
- 0xC2CC lives inside a general "enqueue/deliver to ND-100" library function whose entry is
  **0xBFF8**. 0xBFF8 has **ten callers**, all in the message-handler region 0xCDC2, 0xD0A8,
  0xD1EE, 0xD290, 0xD46C, 0xD4B2, 0xDD2C, 0xDD82, 0xDFB0, 0xE006 - i.e. the ENCOS message /
  XMSG dispatch layer that sits just above XMRECEIVER (0xBED8). [V]
- The producer is therefore a **shared message-delivery service**, driven by the message
  layer, NOT by the OPCOM request box: the OPCOM handler 0x1B00 dispatches its own jump table
  at 0x512 (subfns 0..5) and signals via 0xEF0080 (0x1A48), a *different* SCIP cell; none of
  those subfunction handlers calls 0xBFF8/0xEAA6. [V]

**CONDITION that makes the firmware post a superkick [V]+[I]:** the ENCOS message layer must
run and have a message/completion to hand up for a port. That layer is entered from message
processing (inbound frame handling / XMSG dispatch), which in turn only runs when the firmware
is woken - by an ND->68K interrupt (GPIP I6 / OPCOM doorbell) or a LANCE RX-complete. Idle,
with no inbound event and no ND kick, the producer is never reached. [I on "only when woken",
V on "producer sits below the message layer, not the OPCOM subfn table"]

---

## TASK 2 - PDRIV superkick ring layout (from NPL source, exact)

From `MP-P2-PIOC-DRIV.NPL` (PISUPER, lines 37-141). All offsets are **ND-100 WORD** offsets in
the PIOC bank; the 68K sees byte = word*2.

**Fixed header at PIOC word `SUKOF = 1012B` (= 522 dec word = 68K byte 0x414):**

| NPL sym | word off | 68K byte | contents |
|---------|---------|----------|----------|
| PATRN | 0 (double) | 0x414 / 0x416 | magic `HSKPA=52525B (0x5555)` : `LSKPA=125252B (0xAAAA)` |
| RNTOP | 2 | 0x418 | ND100->PIOC ring pointer (byte addr; PISUPER does `SAD ZIN SHR 1` to word) |
| RPTON | 4 | 0x41C | PIOC->ND100 ring pointer (the ring PISUPER dequeues) |
| PODIR | 6 | 0x420 | port directory pointer (firmware wrote 0x2D350 here, 1a) |

**Ring descriptor (pointed to by RPTON, word offsets):**

| NPL sym | off | meaning |
|---------|-----|---------|
| RLOCK | 0 | lock |
| RSIZE | 1 | size |
| RHEAD | 2 | tail index (per PISUPER comment) |
| RTAIL | 3 | head index PISUPER reads (`RTAIL@3 LDATX`) |
| RENTR | 4 | start of entry array |

**Entry = 4 words** (PISUPER `SHA ZIN 2` = index*4, `AAA RHSIZ` add header size):

| NPL sym | off | meaning |
|---------|-----|---------|
| DEMPT | 0 | empty/occupied flag: **nonzero = occupied** (`DEMPT@3 LDATX; JAF NINT` = skip if zero). The 68K sets bit31 of the entry longword (1b), so ND-100 sees the first entry word = 0x8000 (nonzero) = occupied. |
| DLEVL | 1 | level. PISUPER: `AAA -5; JAF RTPR` (level != 5 -> **RT wake**), else `AAA -15; JAF KXMS` (level == 5 -> **XMSG kick**). |
| DPROC / DINF0 | 2 | RT: RT-description address to kick (RTPR). XMSG: high word of double info (link index + ring addr). |
| DINF1 | 3 | XMSG: low word of double info. |

**To wake ENNS0's RT read, the firmware must write an entry with:** DEMPT occupied (bit31 set),
DLEVL = an RT level (!= 5), DPROC = the RT-description address of ENNS0; advance RHEAD; then
SCIP 0xEF0180 -> INT12. PISUPER's RTPR path then does `COPY SX DA; JPL I (XRTEN` (line 124-125)
= XRTEN with A = RT description, which schedules ENNS0. [V - matches producer 1b byte-for-byte
in structure: entry array base +22, bit31 flag, head index +18.]

Consumer entry chain (ND-100): level-12 interrupt -> **PDRIV** (100765B) -> `CALL FAR PISUPER`
(101052B) -> PISUPER walks RPTON ring, dequeues, `XRTEN`/`RTACT`. PDRIV runs **only on a
level-12 interrupt from the PIOC**, i.e. only after the firmware pulses SCIP. [V]

---

## TASK 3 - Who moves first (ND-100 vs firmware)

- **PISTA** (`RP-P2-PIOC.NPL` 114677B) does the bring-up: polls word 1002B for PRKEY, then
  writes MPIOC(=5)+TRIG into the NORD->PIOC mailbox and rings `A:=11; T:=HDEV+3; *IOXT`
  (PWCR start doorbell, 115077B). That is the START kick. [V]
- **PIKIC** (`RP-P2-PIOC.NPL` 115175B) is the generic ND->PIOC kick: it sets `NTP+slot=TRIG`
  and rings the doorbell `A:=PWCR BONE BNDC; T:=HDEV+3; *IOXT` (115227B). **BNDC = the ND->PIOC
  doorbell bit in PWCR.** [V]
- The XMSG boxes are bidirectional (`MP-P2-PIOC-DRIV.NPL` DISP): `NXFNC` bit3 = "XMSG FUNC
  REQUESTED (by PIOC)", bit1 = "done (by ND100)"; `NXRTF` bit0 = "RTWAK REQUESTED by PIOC",
  bit2 = "RTWAK COMPLETED by ND100". So the **RTWAK/superkick is always produced by the PIOC
  (68K) side**; the ND-100 only *completes* it (PIWKF, 077771B, sets NXRTF bit2 and IOXTs the
  PIOC awake). [V]

**Working-system order [I, consistent with all of the above]:** the ND-100 (ENNS0 via XMSG /
PIKIC) moves first - it queues its request and rings the PWCR.BNDC (or OPCOM) doorbell to the
firmware. The firmware's message layer runs, and when it has the matching completion/inbound
frame for ENNS0's port it enqueues the RTWAK superkick + SCIP 0xEF0180. PDRIV/PISUPER then
wakes ENNS0. If the ND->68K doorbell never reaches the 68K, the firmware never runs its
producer and the superkick is never generated. This matches the observed "controller QUIET
(no ND->68K control writes, no SCIP) during the read window."

**[OPEN] - not decidable from these static images:** whether, in the live run, (a) ENNS0/XMSG
actually wrote its outgoing request + rang the PWCR.BNDC/OPCOM doorbell during the read window,
or (b) the emulator dropped that ND->68K edge. The ENNS0 BRF
(`...\Ethernet\x\encos-err-i-b01.brf`) parks at resume-P 030440 after MON 124 PRSRV; PRSRV is
an RT reserve/wait, so ENNS0 is confirmed in RT-wait, but the exact preceding kick sequence is
runtime state, not in the image. Needs DAP (breakpoints below).

---

## TASK 4 - CONCLUSION: why no superkick/INT12 during the read, and the fix

**Precise reason [V for firmware/NPL structure, I for the live linkage]:** No superkick is
enqueued because the firmware's superkick producer (0xEAA6) is never executed during the read
window. That producer is a leaf of the ENCOS message-delivery layer (entry 0xBFF8, callers in
0xCDxx-0xE0xx), which runs only when the firmware is woken with an inbound event / ND kick and
has a completion to deliver. During the read window the 68K receives no ND->68K doorbell
(controller QUIET), so the message layer never runs, 0xEAA6 is never called, the RPTON ring
stays empty, PISUPER (101052B) dequeues nothing, and ENNS0 stays in its PRSRV RT-wait. The
firmware is NOT parked in a wait it cannot leave, and there is NO version/format mismatch in
the ring - the header magic and ring geometry the firmware publishes (0x5555/0xAAAA at 0x414,
entry array +22 with bit31 flag, head index +18) match PISUPER exactly.

**Most likely root cause = (i) the ND->68K kick is not reaching the 68K** (emulator
doorbell-delivery gap), NOT (ii) firmware parked, NOT (iii) protocol mismatch. Evidence: the
superkick producer + ring are byte-verified correct and consistent with PDRIV; the only thing
absent is the *inbound stimulus* to the firmware. This aligns with the two prior emulator
defects already on record for exactly this ND->68K path:
- `ND_EthernetII_RE-SESSION-HANDOFF-2026-07-08.md` lead A4: control bit 2 (ND interrupt) must
  drive **GPIP I6 as a LEVEL** (`memoryMap.MFP.GPIO_6 = !ND_interrupt;`, ~line 1328 in
  `NDBusEthernetII.cs`); the old rising-edge code left I6 stuck and swallowed subsequent ND
  kicks after the first OPCOM wake.
- `ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md`: the PWCR=60B/PWCR=0 HALT+RESET must actually
  restart the 68K at its reset vector - i.e. the PWCR path in `NDBusEthernetII.cs` was not
  driving the 68K as SINTRAN expects.

Both show the ND->68K control path in the emulator has been unreliable in precisely the way
this deadlock needs. **The fix is on the emulator's ND->68K doorbell path**: ensure that when
ENNS0/XMSG rings `T:=HDEV+3; *IOXT` with the **PWCR.BNDC** bit set (PIKIC 115227B) - and the
OPCOM bit (control bit 3, ~line 1349) - the write handler in `NDBusEthernetII.cs` delivers a
real interrupt edge to the 68K (GPIP I6 level via A4, and/or the OPCOM latch 0xEF0020) so the
firmware wakes, runs its message layer, and reaches 0xEAA6 to enqueue the RTWAK + SCIP. The
exact doorbell path to instrument/repair is the CONTROL-register write handler (`HDEV+3` /
PWCR): CONTROL bit 2 -> GPIP I6 (~line 1328) and OPCOM bit 3 (~line 1349).

**[OPEN] residual:** whether ENNS0 must also first receive a LANCE-RX frame (a reply from a
peer / loopback) before the firmware has anything to deliver. If ENNS0's read is waiting on a
network reply and the emulated LANCE has no peer/loopback, the firmware would legitimately post
no superkick even with a correct doorbell path. This is a distinct second failure mode and must
be ruled in/out at runtime.

### DAP breakpoints to disambiguate (i) vs (the LANCE-RX open point)

ND-100 side (nd100 DAP):
- PIKIC doorbell `A:=PWCR BONE BNDC; *IOXT` at ~**115227B**, and PISTA start doorbell **115077B**
  - does ENNS0 ring any PWCR.BNDC/OPCOM doorbell during the read window?
- PDRIV entry **100765B**, PISUPER **077554B**, PIWKF **100044B/077771B** - do they ever fire?
  (They fire only on a level-12 interrupt = firmware SCIP.)

Emulator side (`NDBusEthernetII.cs`):
- Break/trace the CONTROL-register (`HDEV+3`/PWCR) write handler: log BNDC bit, OPCOM bit 3
  (~line 1349), GPIP I6 drive (~line 1328). Confirm the ND->68K edge is actually delivered to
  the 68K on each ENNS0 kick.

68K side:
- Producer entry **0xEAA6** and SCIP write **0xECF4 (0xEF0180)** - never hit = firmware never
  produced a superkick.
- Message-layer primitive **0xBFF8** - never hit = message layer never ran.
- **0x250E** (ND-INT / GPIP I6 channel scanner) and **0x1B00** (OPCOM handler) - never hit
  during the window = no ND->68K interrupt arrived (confirms (i)).
- RCVCOMPLETE **0x5C42** / XMRECEIVER **0xBED8** - never hit = no inbound LANCE frame (confirms
  the LANCE-RX open point).

If 0x250E/0x1B00 never fire while ENNS0 rings a doorbell (115227B hit), the ND->68K delivery is
broken in `NDBusEthernetII.cs` = root cause (i). If ENNS0 never rings any doorbell (115227B not
hit) and instead simply waits on RX, the deadlock is data-flow (needs a LANCE peer/loopback),
not a doorbell bug.

---

## Evidence anchors (addresses)

Firmware (68K, `encos-ser-all-banks-68k.bin`):
- Ring-header magic writer: **0x7C60..0x7C74** (`move.l #$5555aaaa,($414)`, PODIR($420)=0x2D350).
- Header cell address table (0x414/0x418/0x41C/0x420 longwords): file **0x15cf8, 0x18a34, 0x75cf8**.
- Superkick producer: entry **0x7C60**? no - producer entry **0xEAA6**, body **0xEACC**, ring
  magic check **0xEAEE**, entry store **0xEB68**, occupied bit31 **0xEB7C**, head advance
  **0xEB9A**, SCIP 0xEF0180 **0xECF2/0xECF8**.
- Producer wrapper caller: **0xC2CC jsr $eaa6**; message primitive entry **0xBFF8** (10 callers
  0xCDC2..0xE006).
- OPCOM handler (separate mechanism, SCIP 0xEF0080): **0x1B00**, post_and_signal **0x1A48**.
- ND-INT channel scanner: **0x250E**. RCVCOMPLETE **0x5C42**. XMRECEIVER **0xBED8**.

NPL:
- PISUPER (superkick consumer) **077552B..077702B**; SUKOF=1012B, HSKPA=52525B, LSKPA=125252B.
- PDRIV entry **100765B**, `CALL FAR PISUPER` **101052B**.
- PIWKF (RTWAK completion) **077771B**; NXRTF bit0/bit2 semantics.
- PISTA **114677B** (PRKEY poll + MPIOC=5 start doorbell **115077B**); PIKIC kick+BNDC doorbell
  **115175B/115227B**.

Emulator (`NDBusEthernetII.cs`): CONTROL write handler, GPIP I6 ~line 1328, OPCOM bit3 ~line
1349, SCIP 0xEF0080/0xEF0180 -> INT12.
