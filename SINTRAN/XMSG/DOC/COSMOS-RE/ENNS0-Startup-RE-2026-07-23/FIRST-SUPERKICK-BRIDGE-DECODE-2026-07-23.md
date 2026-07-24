# The FIRST-superkick bridge: what should pend a work-slot after START, and why it never happens

Date: 2026-07-23.
Legend: **[V]=VERIFIED** (decoded firmware bytes / read vector table) - **[I]=INFERRED** - **[OPEN]=not statically decidable, needs DAP**.

Inputs analysed:
- 68K firmware: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin` (MC68000, big-endian, base 0x0, 512 KB).
- Tooling: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\tools\m68kdis.py`.
- Companion decodes (this folder + `...\stripped\docs\`): `FIRMWARE-SUPERKICK-TRIGGER-WHY-NO-INT12-2026-07-23.md`, `FIRMWARE-SUBFUNCTION-5-DECODE-2026-07-23.md`.
- Emulator target: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`.

---

## ONE-LINE ANSWER

**[V] The scheduler's run condition is INVERTED from the task brief: a work-slot (coroutine node)
runs when bit1 of its status byte at slot+23 is CLEAR, not set.** The scheduler main loop at
0x2CB6 walks 16 priority lists; at **0x2CD4 `btst #1,(23,A1)` / 0x2CDA `beq $2CF0`** it DISPATCHES
the node when the bit is 0 and SKIPS it (follows link (40,A1)) when the bit is 1. So "make a task
runnable" = **clear** bit1@slot+23. **[V] There is NO `bset #1,(23,An)` anywhere in the 512 KB
image** - the whole-image scan finds only the scheduler's `btst` (0x2CD4) and exactly two
`bclr #1,(23,An)` sites (0x2292, 0x259A) that make a task runnable.

**[V] The START handler makes NO task runnable.** START (OPCOM SUBFUNCTION=0, inline body
0x1B4E-0x1B96) sets STARTED=1, snapshots the caller's context, optionally inits the 0x650 context
block, and posts MON_CODE=+1 with `SCIP 0xEF0080` (the OPCOM monitor *reply*, INT to ND-100). It
touches no `bclr`, no 0xBFF8, no 0xEAA6. **So the firmware never spontaneously posts a first
superkick as a consequence of START - candidate (a) is refuted by bytes.**

**[V] The OPCOM handler is correctly vectored but is a LEVEL-6 autovector interrupt** (68K IPL6,
vector 30 @0x78 = 0x00001B00). It runs ONLY when the ND-100's START doorbell (PWCR=11B) is
delivered to the 68K as an IPL-6 interrupt. Per the live PC-watch it is never entered - so the
START never actually reaches the firmware, STARTED is never set, and nothing downstream runs. That
is candidate **(c)**, and it is the immediate, verified blocker.

**Verdict: PRIMARY = (c)** (emulator does not raise 68K IPL6 for the START/OPCOM doorbell, so
0x1B00 never runs). **NOT (a)**. **(b)/network-data is a real but SECONDARY gate** that only
becomes the blocker after (c) is fixed (see Task 4).

---

## TASK 1 - The scheduler run condition (decoded, corrects the brief)  [V]

Main loop `0x2CB6..0x2CEE` (list-head table at **0x0B06**, 16 longword heads; next-link at slot+40;
status word at slot+22, low byte = slot+23):

```
2CB6: clr.b  ($660)               ; scheduler-active flag = 0
2CBC: lea    ($b06),A0            ; A0 = list-head table
2CC2: moveq  #64,D0
2CC4: subq.l #4,D0 ; bmi $2CE2    ; iterate 16 heads (64,60,..,0)
2CC8: move.l (0,A0,D0.W),A1       ; A1 = list head
2CCC: cmpa.l #0,A1 ; beq $2CC4    ; empty list -> next head
2CD4: btst   #1,(23,A1)           ; ** node blocked? **
2CDA: beq    $2CF0                ;   bit==0 -> RUN this node (dispatch @2CF0)
2CDC: move.l (40,A1),A1           ;   bit==1 -> skip, follow link
2CE0: bra    $2CCC
2CE2: tst.b  ($660) ; bne $2CB6   ; something re-armed -> rescan
2CEA: STOP   #2000 (4E72)         ; nothing runnable -> STOP, wait for interrupt
2CEE: bra    $2CB6                ; woken -> rescan
```

Dispatch path `0x2CF0`: locks 0x662, stores the chosen node ptr to **0x650** (the "current task"
cell - the same 0x650 START reads), loads its SP from (108,A1), restores its registers
`movem.l (48,A1)` and resumes the coroutine (this is a cooperative-coroutine kernel: tasks yield
with `jmp (A5)`, e.g. 0x1C36, 0x3B40, 0x3BA2).

**Consequence:** after bring-up every coroutine is blocked (bit1@23 = 1), so the scheduler STOPs.
A coroutine only becomes eligible when some event CLEARS its bit1@23. That clear is the "pend a
work-slot" event the brief is asking about (just inverted in polarity).

---

## TASK 2 - Who clears bit1@23 (the only two "make-runnable" sites)  [V]

Whole-image scan for bit-immediate #1 ops on `(0x17,An)`:

| addr | op | role |
|------|-----|------|
| 0x2CD4 | `btst #1,(23,A1)` | scheduler test (reader) |
| 0x2292 | `bclr #1,(23,A1)` | **make-runnable (list sweep / timer-deferred wake)** |
| 0x259A | `bclr #1,(23,A0)` | **make-runnable (inbound message-type event)** |

### 2a. 0x259A - inbound message-type dispatcher 0x2562  [V]

`0x2562` takes a `(code, info)` pair: `D2 = (A0)` (a type code), range-checks **0..0x1E (0..30)**,
indexes control-block table **0x0A8A**:

```
2562: D2 = (A0)                       ; message-type code
2568: tst.w D2 ; blt $25AA            ; code<0  -> ignore
256E: cmpi.w #$1E,D2 ; bgt $25AA      ; code>30 -> ignore   (top of type space = 30 = 0x1E)
2574: D1 = (2,A0)                     ; info longword
257E: A0 = ($a8a)[D2*4]               ; per-type control block
2584: cmpa.l #0,A0 ; beq $25AA        ; no block registered -> ignore
258C: or.l  D1,(118,A0)               ; OR arrived bits into pending mask (offset 118)
2590: D0 = (122,A0)                   ; enabled mask (offset 122)
2594: and.l (118,A0),D0               ; enabled AND arrived
2598: beq  $25A6                      ; nothing enabled matched -> done
259A: bclr #1,(23,A0)                 ; ** CLEAR pending bit -> node RUNNABLE **
25A0: beq  $25A6                      ;   (bit already 0 -> already runnable)
25A2: jsr  $2192                      ; ** request reschedule (force scheduler entry) **
25A6: D0 = 1 ; rts
```

**Precondition to fire 0x259A [V]:** an inbound event whose type code (0..30) has a *registered*
control block at 0x0A8A[code] AND whose info bits intersect that block's *enabled* mask (offset
122). Only then is the owning coroutine unblocked. `0x2192` then test-and-sets the 0x660/0x662
scheduler locks and rewrites the interrupted task's saved PC to re-enter the scheduler at ~0x2C90,
so the newly-runnable coroutine gets dispatched; it also links the node into the ready list
(0x4C2/0x4C6) and pulses `SCIP 0xEF0080` (0x2248).

Who feeds 0x2562: `0x25B0` (`bsr $2562; ... rte`) - an interrupt/trap epilogue; and the GPIP-I6
channel scanner **0x250E** builds an event record at 0x0C0E and issues `moveq #9,D0 / TRAP` (0x4E42)
to hand the event into this dispatch. So **an ND->68K channel doorbell (GPIP-I6) OR an inbound
frame that produces a type-code event is what clears the coroutine's bit and drives it to 0xEAA6.**

### 2b. 0x2292 - deferred/timer sweep  [V]

`0x2292 bclr #1,(23,A1)` sits in a routine (entry ~0x225C) that walks two linked lists (heads
0x4C2 and 0x4C6), and for each node whose flag qualifies (`btst #1,(5,A1)` / `btst #2,(21,A1)`)
unlinks it and clears its bit1@23. This is the timer/deferred-completion wake path (periodic
sweep), not the primary inbound-message path.

**Neither 0x2292 nor 0x259A is called from the START/OPCOM handler.** [V]

---

## TASK 3 - The START/OPCOM path does not pend a slot (byte proof)  [V]

Vector table (verified longwords):

| vector | addr | target | meaning |
|--------|------|--------|---------|
| 30 (0x1E) @0x78 | level-6 autovector (24+6) | **0x1B00** | OPCOM / monitor request box |
| 31 (0x1F) @0x7C | level-7 autovector | 0x1DD8 | NMI |
| 0x4E @0x138 (MFP) | GPIP-I6 | **0x250E** | ND->68K channel scanner |
| Timer-C (MFP) | 0x45 | (RTC) | ~1961 ticks, wakes STOP |

So **OPCOM = 68K IPL6 autovector**. The ND-100 START doorbell (PISTA rings PWCR=11B, RP-P2-PIOC.NPL
115077B) must appear to the 68K as an IPL-6 interrupt for 0x1B00 to run. The handler acknowledges by
clearing the OPCOM source latch: `0x1B0A move.w #0,($EF0020)`. **[I] 0xEF0020 = the OPCOM/level-6
interrupt-source latch** (the firmware ACK point).

START body (SUBFUNCTION=0), decoded `0x1B4E..0x1B96`:

```
1B4E: cmpi.w #0,($4C0) ; bne $1B9A    ; already started? -> MON_CODE=-4
1B58: move.l ($500),A0                ; A0 = saved caller context ptr
1B5E: jsr   $1A66                     ; snapshot caller regs/SP/PC into context block @0x454
1B62: tst.l ($650) ; beq $1B80        ; current-task cell set?
1B6A:   move.l ($650),A0
1B70:   jsr $3A58 ; move.l D0,(152,A0) ; 0x3A58 = pointer-swap (set $fca=D0, return old); init 0x650 ctx
1B7A:   jsr $3A58
1B80: move.w #1,($4C0)                ; ** STARTED := 1 **
1B88: moveq #1,D0 ; MON_CODE = +1     ; success
1B96: bra  $1C0A                       ; epilogue -> jsr $1A48 post_and_signal (SCIP 0xEF0080)
```

- `0x1A66` [V] just saves the interrupted caller's registers (mask 7fff), SP (at +64), return
  address ((6,A7)->+68) and SR ((4,A7)->+72) into the fixed context block at 0x454. It does **not**
  create or unblock a server coroutine.
- `0x3A58` [V] is a trivial global-pointer swap (`push ($fca); ($fca)=D0; pop D0`). Structure init,
  not a scheduler op.
- Epilogue `0x1A48` [V] increments postbox counters and writes `move.b #1,($EF0080)` = SCIP INT to
  ND-100 - this is the OPCOM monitor **reply/completion**, on the OPCOM SCIP cell (0xEF0080), NOT
  the RTWAK superkick cell (0xEF0180/INT12) that PDRIV/PISUPER consume.

**Conclusion of Task 3 [V]:** processing START sets STARTED and replies; it clears no bit1@23,
enters no message-delivery coroutine, and rings no superkick. The firmware does not, and is not
meant to, spontaneously emit a first superkick from START. **Candidate (a) is refuted.**

---

## TASK 4 - Decision: (a) vs (b) vs (c), with the fix

### (c) is the immediate, verified blocker  [V for firmware, OPEN for the exact emulator edge]

The OPCOM handler 0x1B00 is a **level-6 autovector**. Per the live PC-watch it is never entered and
GPIP-I6 fired exactly once at bring-up. Both ND->68K wake paths are therefore silent after
bring-up: the ND-100's PWCR=11B START doorbell is **not being raised to the 68K as IPL6** (and the
GPIP-I6 line is not being re-asserted). Because 0x1B00 never runs, `STARTED` (0x4C0) is never set,
the 0x650 context is never initialised, and every downstream step is moot. This is an **emulator
stimulus gap on the ND->68K doorbell path**, exactly matching the two prior defects on record for
this path (control-bit -> GPIP I6 as a LEVEL, and the PWCR HALT/RESET -> 68K restart).

**Fix (emulator):** in `NDBusEthernetII.cs`, the CONTROL/PWCR (`HDEV+3`) write handler must, when
the OPCOM/START encoding (PWCR=11B = control 0x0009) is written, **raise the 68K to IPL6**
(level-6 autovector -> 0x1B00) - and assert the OPCOM source so the firmware's `move.w #0,($EF0020)`
at 0x1B0A deasserts it. It must ALSO drive GPIP-I6 as a level for the generic channel doorbell
(prior lead A4, ~line 1328). Until 0x1B00 is entered, nothing else can happen.

### (b) is the next gate, only reachable after (c)  [V for firmware, OPEN for live data-flow]

Once START runs and STARTED is set, the FIRST superkick that would complete ENNS0's LU-2240B read
still requires a **make-runnable event** to fire 0x2562 -> 0x259A (or the timer sweep 0x2292) for
the message-delivery coroutine, which then reaches 0xBFF8 -> 0xEAA6 -> `SCIP 0xEF0180` (INT12).
ENNS0 issues no SEND_KI / no follow-up PIKIC kick (established), so 0x2562 is fed only by:
- an **ND->68K channel doorbell** (GPIP-I6 -> 0x250E scanner -> TRAP -> 0x2562) carrying a message, or
- a **LANCE RX frame** from a network peer producing a type-code event.

With the controller quiet and no peer/loopback, no code (0..30) with a matching enabled mask ever
arrives, 0x259A never clears the coroutine's bit, the scheduler keeps STOPping, and ENNS0 stays in
its PRSRV RT-wait. This is the "LANCE-RX / who-moves-first" gate = candidate **(b)**. It is genuine
but SECONDARY; do not chase it until (c) shows 0x1B00 being entered and 0x4C0 going to 1.

### Candidate (a): refuted  [V]

The START handler emits no superkick (Task 3). There is no firmware path where processing START,
by itself, enqueues an RTWAK superkick.

---

## Exact addresses (what to set / precondition)

- **The address that makes a work-slot runnable after an event** (nearest analogue to "set pending"):
  **0x259A `bclr #1,(23,A0)`** in dispatcher **0x2562**. Precondition: inbound event type-code 0..30
  with a registered control block at **0x0A8A[code]** and `((118,A0) OR arrived) AND (122,A0)`
  nonzero; then **0x2192** forces reschedule. Secondary path: **0x2292** (timer/deferred sweep).
- **START runnable-making address:** NONE. START body 0x1B4E-0x1B96 sets 0x4C0=1 and replies only.
- **OPCOM handler entry:** **0x1B00** (level-6 autovector, vec30 @0x78). ACK latch **0xEF0020**.
- **OPCOM reply SCIP:** **0xEF0080** (0x1A48/0x1A5C). **Superkick SCIP (INT12):** **0xEF0180**
  (producer 0xEAA6, SCIP at 0xECF8).
- **GPIP-I6 channel scanner:** **0x250E** (vec 0x4E @0x138).
- **Scheduler test / STOP:** **0x2CD4 btst**, **0x2CEA STOP**.

## DAP breakpoints to confirm the (c)->(b) ordering

68K:
- **0x1B00** OPCOM entry - not hit = START/OPCOM IPL6 not delivered = **(c) confirmed**.
- **0x1B80** `STARTED:=1` - not hit = server never started.
- **0x250E** GPIP-I6 scanner - not hit after bring-up = no ND->68K channel doorbell.
- **0x2562** / **0x259A** - not hit = no make-runnable event = **(b)** (no coroutine ever unblocks).
- **0x2CD4** scheduler test, **0x2CEA** STOP - confirms idle-STOP loop.
- **0xEAA6** superkick producer, **0xECF8** SCIP 0xEF0180 - not hit = no INT12 emitted.

ND-100:
- **PISTA 115077B** (START doorbell PWCR=11B), **PIKIC 115227B** (PWCR.BNDC kick).
- **PDRIV 100765B**, **PISUPER 101052B**, **PIWKF 077771B** - fire only on level-12 = firmware SCIP.

Emulator (`NDBusEthernetII.cs`): CONTROL/PWCR write handler - verify PWCR=11B raises 68K **IPL6**
(-> 0x1B00) and GPIP-I6 level assert (~line 1328); OPCOM latch 0xEF0020 semantics.

---

## Summary table

| Candidate | Verdict | Evidence |
|-----------|---------|----------|
| (a) firmware posts initial superkick after START | **REFUTED [V]** | START body 0x1B4E-0x1B96 sets 0x4C0=1 + SCIP 0xEF0080 reply only; no bclr/0xBFF8/0xEAA6 |
| (b) work-slot pends only on an ND->68K/inbound event nobody sends | **SECONDARY [V+OPEN]** | make-runnable only at 0x259A (event 0x2562) / 0x2292 (timer); ENNS0 sends no kick; needs channel-kick or LANCE-RX peer |
| (c) OPCOM/START never reaches the firmware handler | **PRIMARY [V+OPEN]** | 0x1B00 is level-6 autovector (vec30 @0x78), correctly resolved but never entered; STARTED never set; emulator not raising IPL6 for PWCR=11B |

The vector is NOT mis-resolved (0x1B00 is correct); (c) means the level-6 interrupt EDGE is not
delivered. Fix that emulator edge first; then rule (b) in/out with a LANCE peer/loopback.
