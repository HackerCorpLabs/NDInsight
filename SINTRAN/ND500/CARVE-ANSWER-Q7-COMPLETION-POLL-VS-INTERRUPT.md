# Q7 CARVE ANSWER - ND-500 completion detection: POLL or LEVEL 12?

**Full path:** `SINTRAN/ND500/CARVE-ANSWER-Q7-COMPLETION-POLL-VS-INTERRUPT.md`

**Request answered:** PROMPT 1 (revised) - "ND-500 completion detection: does 5STDR
POLL or rely on LEVEL 12?" Walk `5STDRIV` in `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL`
and prove POLL-DRIVEN | INTERRUPT-DRIVEN | MIXED from its own logic, not by analogy to
octobus/TPE.

**Date:** 2026-07-18. **Version under analysis:** L-VSX-500 (L07).

---

## Q7 VERDICT: INTERRUPT-DRIVEN

Completion of an ND-500 message (MON call / trap / normal stop) is detected by the
**level-12 interrupt line**, not by polling a "finished" bit in the 3022 status register
`RSTA5`. When level 12 fires, `5STDRIV` runs, does an **error-only** triage of `RSTA5`
(via `CLE5STATUS`), then **unconditionally drains** the shared-memory execution queue
(`MAILINK`), reading each message's own status field (`N5STA` in MPM) only to discriminate
answer TYPE (answer / error-answer / restart). There is **no poll loop on `RSTA5`** waiting
for completion anywhere in the driver - not inside `5STDRIV`, not in the activate path
`XACT500`, and not in a monitor wait. The activate path explicitly ends by **"Enable for
interrupt"** and returns.

This is decision-test #2 from the request ("assume entered => finished; status read only
discriminates type/error"), and it is NO to tests #1 and #3.

---

## Evidence tiers (read this before trusting a line)

- **[V]** = byte/symbol-verified against the L07 symbol tables in
  `SINTRAN/NPL-SOURCE/SYMBOLS/L07/` (`N500-SYMBOLS.SYMB.TXT`, `SYMBOL-1-LIST.SYMB.TXT`,
  `l07-kallsyms.txt`). Applies to register offsets, status-bit values, and level numbers.
- **[V-NPL]** = verified in the requested NPL source `MP-P2-N500.NPL` /
  `XC-P2-N500.NPL`. This is authoritative for CONTROL FLOW and LOGIC. Per the carving
  ground rules, NPL is a DIFFERENT REVISION from the carved L07 bytes, so NPL octal
  addresses below are the NPL-listing addresses, not proven-identical to the L07 image.
- **[I]** = inferred, marked inline.
- **GAP** = the one thing NOT done this pass: a byte-level disassembly of the L07
  `5STDR` image (symbol `5STDR = 0xBA08`, i.e. octal 135010) to confirm the NPL control
  flow instruction-for-instruction. The NPL-listing address for the same routine is
  octal 134610; the ~octal-200 (0x80-word) shift between them is exactly the revision
  drift the skill warns about. The verdict rests on [V-NPL] logic + [V] symbols; the L07
  byte cross-check is left as a follow-up anchor, not a blocker.

---

## 1. What causes 5STDRIV to run (the trigger)

`5STDRIV` is the ND-500 driver interrupt routine on **program level 12**.

- `LV12B = 000140` octal [V] - present identically in four L07 tables
  (`N500-SYMBOLS.SYMB.TXT:5152`, `SYMBOL-1-LIST.SYMB.TXT:3544`,
  `RTLO-SYMBOLS.SYMB.TXT:2517`, `FILSYS-SYMBOLS.SYMB.TXT:2760`). It encodes the level in
  bits 3-7: `LV12B SHZ -3 = 014` octal `= 12` - used exactly that way at
  `MP-P2-N500.NPL:3619` (`LV12B SHZ -3`). So LV12B is "level 12".
- Entry/re-arm mechanism: the loop tail `CALLID12: CALL WT12` (`MP-P2-N500.NPL:693`,
  octal 134675) hands the level back to `WT12` [V-NPL]. `WT12` (L07 symbol `WT12 = 0x378E`
  [V], body not in this file - it is the generic level-wait primitive) gives up level 12
  until the hardware re-triggers it on the next 3022 interrupt. `XKICK500` shows the same
  handoff pattern: `"WT12"; *IRW LV12B DL` then `LV12; *MST PID`
  (`MP-P2-N500.NPL:3286-3288`). So `5STDRIV` runs **because level 12 is asserted**, and
  waits for the next assertion via `WT12` - a pure interrupt loop, no busy-poll.
- **No IOX IDENT (identify) read inside `5STDRIV`.** The routine never reads an ident
  code to decide whether/what completed - the LINE itself is the event, and the MPM
  message queue says what to do. (I did NOT find any `ident = 16` assignment in
  `MP-P2-N500.NPL`; I therefore do not assert that value. The request's "level-12 ident =
  16" is neither confirmed nor needed for the verdict.) [V-NPL for "no ident read"; the
  ident value is GAP/not-asserted]

L07 symbol anchors [V]: `5STDR = 0xBA08` (octal 135010), `XN500 = 0xBA53`,
`WT12 = 0x378E` (from `l07-kallsyms.txt:11186,11193,6986`).

---

## 2. What 5STDRIV does INSIDE (the decision) - MP-P2-N500.NPL:659-697

NPL-listing octal addresses in brackets. [V-NPL]

```
5STDRIV:                                                     [134610]
   IF CPUAVAILABLE NBIT 5ALIVE GO CALLID12   % CPU present guard
N500:
   DO
      IF C5STAT/\C5PFMASK >< 0  GO CALLID12   % page-fault-in-progress guard
      TRR CCLR                                % (CCM03) clear
      GO XN500                                % *NNJ07* patch: ND-5000 takes the FIFO path
      A:=B=:CC5CPU
      177377; CALL CLE5STATUS                 % <-- THE STATUS READ. mask 177377.  [134623]
      IF A/\720 >< 0 THEN                     % 720 = bits 4,6,7,8 = ERROR group  [134625]
         IF A BIT 5PFAIL THEN ... KPOWDOWN    %   power fail
         ELSE IF A BIT 5DMAERR THEN N5DMAERR  %   DMA error
         ELSE N5IERR                          %   ND-500 comm error
         FI; FI
         GO N500ERR
      FI
      X:=MAILINK                              % scan exec-queue from "MAR"  [134647]
      DO
      WHILE X><-1
         T:=5MBBANK; *LINK@3 LDDTX            % follow linked list
      WHILE D><-1
         X:=D=:N5MESSAGE
         IF X><DUMMESS THEN
            CALL CHN5STATUS; GO N500ERR       % <-- process EACH completed message  [134665]
         FI
NXTMSG:
         GO XN500                             % *NNJ08* patch (ND-5000)
         X:=N5MESSAGE
      OD
      CC5CPU=:B; CALL XACT500                 % reactivate ND-500 for more work  [134672]
CALLID12: CALL WT12                            % give level 12 back; wait next int  [134675]
   OD
```

**Reading of the decision:**

1. The **only** thing `5STDRIV` tests in the hardware status word is the **ERROR group**
   (`IF A/\720 >< 0`, 720 octal = bits 4,6,7,8). If clear, it does **not** branch on any
   "work finished" bit - it proceeds straight to draining `MAILINK`. That is
   decision-test #2 = interrupt-driven. [V-NPL]
2. `CLE5STATUS` (see section 4) reads `RSTA5` and looks only at power/error bits. The
   3022 `RSTA5` bit map has **no message-ready/finished bit** to poll (section 4). [V + V-NPL]
3. Completion PAYLOAD ("what finished, and was it an answer or an error") is read from the
   **MPM message block**, field `N5STA` (offset 2), via `CHN5STATUS`->`RN5STATUS`
   (section 3) - not from `RSTA5`. [V-NPL + V]

---

## 3. Answer-TYPE discrimination is a MEMORY read, not a hardware poll - CHN5STATUS, MP-P2-N500.NPL:730

```
CHN5STATUS:                                   [135004]
   ...
   CALL RN5STATUS                             % A := message.N5STA (MPM offset 2)
   IF A=ANSWER THEN            ... DECOMESS    % decode normal answer
   ELSE IF A=5ERANSWER THEN    CALL DECOERRMESS% decode error answer
   ELSE IF A>>100 THEN         CALL 5RRTWT     % restart ND-100 process
   ELSE IF A=MSGN500 OR A=WAITING THEN CALL XTER500
   FI FI FI FI
```

`RN5STATUS` reads the message's `N5STA` field. `N5STA = 000002` [V]
(`N500-SYMBOLS.SYMB.TXT:5746`, `SYMBOL-1-LIST.SYMB.TXT:4002`) - an offset in the MPM
message block, **not** an IOX register. `5ERAN` (5ERANSWER) `= 000004` [V]
(`N500-SYMBOLS.SYMB.TXT:1541`). So the "status" that tells `5STDRIV` what happened is the
answer word the ND-500 wrote into shared memory - a decoupled, always-readable value,
consistent with the architect's microcode bound. The interrupt says "something is ready";
this memory field says "what". [V-NPL for flow, V for offsets]

---

## 4. The 3022 status register RSTA5 has NO completion bit - CLE5STATUS, XC-P2-N500.NPL:47

`RSTA5 = 000002` [V] (offset 2 from `HDEV`, the 3022 IOX base:
`N500-SYMBOLS.SYMB.TXT:1194`, `SYMBOL-1-LIST.SYMB.TXT:870`).

`CLE5STATUS` (`XC-P2-N500.NPL:47`, octal 030316) - the routine `5STDRIV` calls at
`177377; CALL CLE5STATUS` - reads `RSTA5` and acts only on power/fault bits. Its header
documents the **complete** `RSTA5` bit map [V-NPL] (`XC-P2-N500.NPL:41-45`):

| Bit | Symbol  | Octal   | Meaning                          |
|-----|---------|---------|----------------------------------|
| 4   | 5PAGF   | 000020  | inclusive OR of errors           |
| 6   | 5DMAER  | 000100  | communication error              |
| 7   | 5PFAIL  | 000200  | power fault (microprogram)       |
| 8   | 5POWOF  | 000400  | latched power fault              |
| 9   | 5CLOST  | 001000  | microclock stopped               |

Plus `5ILOCK` (interface-locked - ND-500 running / interface busy) used in the activate and
terminate paths. **Every documented `RSTA5` bit is an error / power / clock / lock status.
There is no "answer ready" or "operation complete" bit for anyone to poll.** That is the
structural reason completion cannot be, and is not, poll-detected on `RSTA5`. [V-NPL for
the bit map; V for RSTA5 offset]

Register offsets used below, all [V] from `N500-SYMBOLS.SYMB.TXT` /
`SYMBOL-1-LIST.SYMB.TXT` (offset from `HDEV`): `LMAR5=1`, `RSTA5=2`, `LSTA5=3`,
`LCON5=5`, `SLOC5=14` (octal).

---

## 5. The activate path ARMS THE INTERRUPT and returns - XACT500, MP-P2-N500.NPL:3057

```
XACT500:                                                     [145551]
   GO XACTRDY                                 % *NNJ14* patch: ND-5000 path
   ...
   T:=HDEV+RSTA5; *IOXT                        % read interface status ONCE  [145571]
   A=:500STATUS
   IF A NBIT 5CLOST THEN                       % clock not stopped?
      IF A BIT 5ILOCK THEN CALL XTER500 FI     % if interface locked -> terminate
      X:=MAILINK
      DO ... find a message with status MSGN500/WAITING ... OD
      IF <terminated>  THEN
ACT50:   5MBBANK; T:=HDEV+LMAR5; *IOXT          % write MAR (address)  [145636]
         A:=X;    *IOXT
         A:=5;    T+"LCON5-LMAR5"; *IOXT        % LCON5 := 5
      ELSE
         % Enable for interrupt                                       [145650]
         A:=10; T:=HDEV+LCON5;   *IOXT          % LCON5 := 10 octal (bit 3 = int enable)
         A:=0;  T+"LSTA5-LCON5"; *IOXT
         A:=1;  T+"LCON5-LSTA5"; *IOXT
                T+"SLOC5-LCON5"; *IOXT          % SLOC5 (lock/start)
         TTMR=:TMR
      FI
   FI
OUT: 0=:5CPUSTOPPED
   DREG=:D; X:=XREG; GO LREG                    % RETURN - no wait, no poll  [145673]
```

`XACT500` reads `RSTA5` **exactly once** to decide HOW to activate (is the interface
locked/clock lost?), then either reactivates via `LMAR5`/`LCON5` or, in the normal case,
executes the block the source itself labels **"Enable for interrupt"** and **returns**
(`GO LREG`). It never spins waiting for the ND-500 to finish. Completion comes back later
as the level-12 interrupt into `5STDRIV`. [V-NPL]

---

## 6. Decision tests - explicit answers

| # | Test (from request) | Answer | Byte evidence |
|---|---------------------|--------|---------------|
| 1 | Does 5STDRIV read RSTA5 and BRANCH on a *finished* bit? | **NO** | It reads status via `CLE5STATUS` and branches only on the ERROR group `A/\720` (MP-P2-N500.NPL:668-676); `RSTA5` has no finished bit (XC-P2-N500.NPL:41-45). |
| 2 | Does it assume "entered => finished", status read only to discriminate type/error? | **YES** | After the error check it unconditionally drains `MAILINK` (134647-134671); `CHN5STATUS` reads MPM `N5STA` to pick answer/error-answer/restart (135004+). |
| 3 | Is there a poll loop on RSTA5 OUTSIDE 5STDRIV that level 12 nudges? | **NO** | `XACT500` reads `RSTA5` once (145571); the only `RSTA5` spin is `XTER500`'s `WHILE A BIT 5ILOCK` (145204-145222) which is the STOP handshake, not completion; `500HA` (133010) is a one-shot "is it active" probe. |
| 4 | Read order / master-clear / ident | STATUS(error) read BEFORE draining the queue; completion PAYLOAD read from MPM `N5STA` (offset 2) not `RSTA5`; no master-clear between activate and the status read in the normal path (`X5MCST` master-clear only on timeout in `XTER500`, 145226); **ident is not read in 5STDRIV at all** - the level-12 LINE is the detector. |

---

## 7. Why this is NOT "MIXED" (guarding against misreading)

`5STDRIV` does contain status reads, so it is worth being explicit that they are NOT
completion polling:

- The `RSTA5` read via `CLE5STATUS` is **error triage** (power/DMA/comm), gate `A/\720`.
  If the error group is clear the routine does not consult `RSTA5` again.
- The `N5STA` read via `RN5STATUS` is a **shared-memory** read of the message the ND-500
  already completed - it answers "answer or error-answer?", not "is it done yet?".

Neither is a loop that waits for a completion condition. Completion detection - the "is it
done?" decision - is made by hardware asserting level 12. Hence **INTERRUPT-DRIVEN**, not
MIXED. [V-NPL]

---

## 8. Relation to the architect's microcode bound (GIVEN, attributed - not carved here)

The request supplied, as GIVEN, that the finished/answer status is an independent,
always-readable value on both generations (classic: 3022 STATUS via TAG-OUT `wSTATUS`;
Samson: `N5STA` in MPM), decoupled from interrupt-enable / level 12. This carve is
CONSISTENT with that and adds the ND-100-software half of the picture:

- The always-readable "what happened" value that `5STDRIV` actually consumes is the MPM
  message field `N5STA` (offset 2) - matching the Samson side of the bound. [V-NPL + V]
- `RSTA5` (the 3022 hardware status) carries only error/power/clock/lock bits, no
  completion bit - so even the classic side gives `5STDRIV` nothing to poll for
  completion; it is used for error triage only. [V-NPL]
- Therefore, on the ND-100 side, the interrupt is not "a wake nudge into a poll" - it is
  the sole completion signal, and the payload is read from memory. [V-NPL]

These two architect facts (Samson finish = `N5STA` write + explicit IDENT frame via
`GIVEINT`; classic = TAG-OUT `wSTATUS`; finished-status decoupled from level 12 both
generations) are the microcode/ND-500 SENDER side and are NOT verified in this ND-100
carve - they are recorded here as the counterpart, attributed to the architect.

---

## 9. Bottom line for the RetroCore ND-500 completion-latency decision

For the 3022 completion model in `NDBusND500IF`: the ND-100 driver does **not** sample a
finished bit. Model completion as: ND-500 writes its answer into the MPM message block
(`N5STA`), then **raises level 12**; SINTRAN's `5STDRIV` runs on that edge, error-triages
`RSTA5`, and drains the message queue. The interrupt may fire at whatever point the ND-500
actually finishes - there is no ND-100-side poll window that would mask a mid-activate vs
after-activate arrival, because there is no poll. Get the level-12 edge and the `N5STA`
write ordered correctly (status/answer written before the interrupt edge) and the ND-100
side will behave. This is the same "interrupts are verifiable side-effects of completion,
control flow keys off a decoupled status value" family pattern seen on the octobus TPE
side - but here it is proven from `5STDRIV`'s own logic, not inherited.

---

*Sources: `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL` (5STDRIV 659-697, CHN5STATUS 730-758,
XACT500 3057-3098, XTER500 2923-2962, 500HA 264-269); `SINTRAN/NPL-SOURCE/NPL/XC-P2-N500.NPL`
(CLE5STATUS 47-64, RSTA5 bit map 41-45); L07 symbols in `SINTRAN/NPL-SOURCE/SYMBOLS/L07/`.*
