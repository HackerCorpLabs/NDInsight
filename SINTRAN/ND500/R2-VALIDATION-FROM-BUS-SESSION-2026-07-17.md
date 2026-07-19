# R2 validation (threading wiring + Samson start) - from the bus-interface session

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\R2-VALIDATION-FROM-BUS-SESSION-2026-07-17.md`
**Reviewing:** RetroCore working-tree R2 (uncommitted at review time; base `5408d5984`):
`Nd500MicrocodeServicer.cs` (_engineLock + Samson start), `NDBusND500IF.cs` (AttachRealCpu),
`CpuND500.ProcessControl.cs` (StartProcessFromContextBlock), `INd500ProcessHost.cs` /
`Nd500CpuProcessBridge.cs` (OnStartProcessSamson). Also re-verified the committed 3a/1a fix.
**Reviewer basis:** the section 3.10 decode model + lossless B30 listing
(`E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`), CNTXT-BLOCK-DECODE-2026-07-17.md,
MP-P2-N500.NPL, L07 N500-SYMBOLS, the 3022 register semantics (TERM5 byte findings).
**Code freeze respected** - flags only.

## Item 1 - _engineLock: CONFIRMED, no lock-ordering hazard; three flags

- **No deadlock possible today:** `_engineLock` is the only lock in the classic path. The
  stop-answer tail (AnswerActiveProcessMessage, inside the lock) calls host.AnswerWritten ->
  SetOperationComplete -> CheckTriggerInterrupt -> SetInterruptBit, and none of those
  acquire anything. On classic `MailboxHeaderBase == 0`, so the 10000-iteration semaphore
  spin never runs inside the lock (octobus: both callers are the CPU thread - uncontended,
  as the comment says).
- **RSTA5 timing is safe:** IOX STATUS READS never take the lock - SINTRAN's polls cannot
  be delayed by it. Only an ACT50 activate (ProcessChain) can block, worst case behind one
  stop answer = ~40 DMA word writes = negligible vs N5TIMOUT-scale timeouts. The reverse
  (IOX holding the lock through a long 14B copy while a stop answer waits) delays only the
  ND-500 CPU thread, which SINTRAN cannot observe.

**FLAG R2-1 (real, narrow): statusRegister read-modify-write races.** `statusRegister` is
a plain enum field. SetOperationComplete (CPU thread, under _engineLock) does
`&= ~Busy; |= Finished` while the IOX thread's register writes (SLOC5/UNLC5/TERM5/LCON5 at
NDBusND500IF.cs:1836-1954) do their own RMW WITHOUT the lock. Two concurrent RMWs can drop
a bit (e.g. TERM5's lock-clear lost against the completion's finished-set). Windows are a
few instructions wide but the live machine runs for hours. Suggest: route ALL status
mutations through one small lock (or make them Interlocked exchanges on a backing int).

**FLAG R2-2 (pre-existing, R2-unchanged): park-after-answer ordering.** The seg-31 hook and
the trap hook both set `regs.stopMode |= WAIT` AFTER OnMonitorCall/OnUnhandledTrap return -
i.e. the answer (and level 12) is raised BEFORE the CPU is formally parked. A fast 3MONCO
on the IOX thread then hits OnMonitorCallRestart's `(stopMode & WAIT) == 0` guard ->
DECLINED -> placeholder immediate answer -> the process is never resumed (hang). In
practice SINTRAN needs ~ms of emulated ND-100 work vs ns for the 500-thread to set WAIT,
but the microcode's order is stop-the-process FIRST, then write the record. Fix when the
freeze lifts: set WAIT before invoking the sink, clear it on decline.

**FLAG R2-3 (live-oracle watch item): the async completion is gated on InterruptEnabled.**
CheckTriggerInterrupt fires level 12 only if STATUS InterruptEnabled AND Finished. If
SINTRAN has int-enable clear at the moment a stop answers (e.g. after a TERM5 sequence),
finished sets silently and SINTRAN discovers it only by poll. Whether that matches the real
3022 is exactly what the first deferred-answer live trace will show.

## Item 2 - AttachRealCpu: CONFIRMED

- Wiring: AttachCpu + bridge + run thread started PARKED (WAIT seeded only if stopMode was
  NONE - correct, does not clobber a CRASHED/HALT state). Thread name keyed by IOX base.
- Completion split intact: engine-only MICFUs answer inside ProcessChain on the activating
  thread -> suppressed by thread identity -> completion via return value (the live-trace
  synchronous shape). Taken start returns "nothing answered" -> lock held, finished clear ->
  SINTRAN waits. Stop answers arrive on the CPU thread -> id mismatch -> SetOperationComplete.
- The thread-identity design also fixes the deterministic mode correctly: after the
  activate returns, `_syncMailboxThreadId` is 0, so caller-driven RunUntilStop stops on the
  SAME thread still complete via AnswerWritten (the old bool could not distinguish this).
- **Note:** nothing in machine composition calls AttachRealCpu yet (tests only, incl. the
  new Nd500CpuR2WiringTests). The live EXE keeps placeholder-answering 23B until a machine
  or config change lands - keep live-oracle expectations calibrated to that.

## Item 3 - synthetic seg-31 capability, domain 0: CONFIRMED harmless

- In-code: with a sink attached, HandleIndirectSegmentCall's seg-31 branch consumes ONLY
  `vectorIndex` (the MON number); the capability's domain bits are read only on paths the
  synthetic value never reaches. The `(programCapability & PC_INDIRECT) == 0` pre-condition
  also means a loader-seeded REAL capability still wins when present.
- Carve/microcode side: no SINTRAN MON path can depend on the seg-31 capability's DOMAIN
  bits, because on the stop path there IS no capability resolution - the SAMSON microcode
  turns the MON call into TRAP_MONC (trap code 6) -> CALL_MON -> stop record; no domain
  switch happens, and SINTRAN only ever sees the message contents. The open question
  (capability bit vs hardwired 37B in the real CALL microcode) affects only which fix is
  hardware-faithful, not the domain-0 choice - either answer leaves domain bits unused.

## Item 4 - CNTXT block decode cross-check: CAN'T-VERIFY from the sender side (+2 flags)

No SINTRAN-side writer of the 0o4000 context table exists in the available source:
- The L07 tree is CLASSIC-generation: LDSWA builds the **21B register image** (catalog 7c);
  there is no SAMSON context-block builder in MP/RP/CC-P2-N500.NPL.
- The SINTRAN M ND-5000 modules that would write SAMSON context blocks are not in the NPL
  tree (M06 has only symbol files).
- Do NOT conflate with the ND-100-side MAILINK per-CPU extension blocks (stride 5EXTD=200B
  **words** - see `SINTRAN\ND5000\CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md`); same 256-byte
  arithmetic, entirely different structure on the other processor.
- Decode-doc unknown #8 ("does SINTRAN ever create context blocks elsewhere") stays open.
- **Suggested empirical oracle:** the live ND-5800/Octobus machine - dump ND-500 physical
  0x800-0xC00 after a process start and byte-verify P@+0x00 / CED@+0x5C against the map.

Emulator-mapping flags against the decode (both minor):
- **FLAG R2-4:** StartProcessFromContextBlock loads CED/CAD as full 32-bit words; the
  decode says the microcode loads a **byte** (NEW_CED/NEW_CAD: "byte -> SRF14 + MM,DOM"
  [V]). Garbage in the upper bytes of ctx+0x5C/0x60 would diverge - mask to 8 bits.
- **FLAG R2-5:** NEWCNTXT's save-on-switch (CNTXTSAVE when a different process is loaded)
  is not modeled - fine single-process, will corrupt the outgoing context when a second
  process appears. The 24B/26B restart path reloading via NEWCNTXT is also skipped; benign
  while index never changes (NEWCNTXT2 "same process - nothing to do").

## Item 5 - 3WMONCO copy source: DECODED - closes open question 8

Full walk of MSG_CONWR_1/_2/_W/_B (B30 015752-016004) with the section 3.10 model
(AA: 2=DPA, 4-7=EA0-EA3; AB=1 -> MARG signed byte; IX-flag|ORCON = MARG; op on word N uses
ADACT of word N-1):

```
MSG_CONWR (015703):  call NEWCNTXT                       \ context switch (like 3START)
  015704-015706:     call ADR_MESS; DPA := RF1; EA1SAVE  \ EA1 := message base
                     + call MSG_CONMC_0                  \ the SHARED 24B fetch:
                                                         \   FUNCV->X1, KFLIP->K,
                                                         \   NUMPA mask write-back
  015707:            call MSG_CONWR_1                    \ the block copy (below)
  ...falls to MSG_CON10: MICFU := 23B; EXECUTE           \ resume process
MSG_CONWR_1 (015752):
  SC10 := mem32[msg+0x1A]      \ HW 0o15 = 26ADD  = DEST process address     [V]
  SC11 := memHW[msg+0x1E]      \ HW 0o17 = 26NRB  = BYTE COUNT               [V]
  (ADACT msg+0x60, then ADACT EA0+0x60)
  SC13 := mem32[msg+0xC0]      \ HW 0o140 = ABUFA = SOURCE, an ND-100 WORD
                               \ address - reached via TWO +0x60 hops because
                               \ MARG is a SIGNED byte (+0xC0 unencodable);
                               \ the "dead" ADACT on 015754 is hop 1     [D forced]
  015757-015761: if (SC11 - 0x2000 does not borrow)      \ count >= 8KB guard [D]
      { X1 := 0o174; K := 1; return }                    \ NO copy - but the flow
                                                         \ still reaches MICFU:=23B
                                                         \ + EXECUTE: process RESUMES
                                                         \ with K=1, X1=0o174
MSG_CONWR_2 (015762):
  DPA := SC13*2                \ 015764: WORD addr -> BYTES (the 13B/14B convention)
  EA2 := DPA - 4               \ 015765 (IX*8|0x3C = MARG 0xFC = -4): SOURCE ptr
  DPA := SC10; LC := SC11>>2   \ two Q,Q/LOG shifts = word count
  EA3 := DPA - 4               \ 015770: DEST ptr
MSG_CONWR_W / _W1:             \ word loop: EA2+=4, SC12 := RD,POF [EA2];
                               \            EA3+=4, WRITE [EA3] := SC12
  on LC==0: LC := SC11 & 3     \ byte tail; EA2+=3 / EA3+=3 repositions the
                               \ lag-4 word pointers to lag-1 byte pointers
MSG_CONWR_B:                   \ byte loop: EA2+=1 RD,POF byte -> EA3+=1 WRITE byte
```

**Result: 3WMONCO = the 24B restart PLUS copy(26NRB bytes, from ND-100 memory at
word-address mem32[msg HW 0o140]=ABUFA (x2 to bytes, RD,POF physical/MPM space), to
process virtual memory at mem32[msg HW 0o15]=26ADD (plain-domain WRITE)).**

This matches the SINTRAN sender exactly: both builders (MP:137136 2CLOCK, MP:143020 RFRRE)
leave the MON-60 buffer's ND-100 word address in ABUFA@0o140 and fill 26ADD/26NRB - my
earlier "ABUFA by symmetry with 3WMED" inference is now [V-flow] (the ABUFA offset itself
[D] only via the chained-MARG reading, which is forced by the signed-byte encoding).

**Emulator recipe (M2 tail):** in the 26B case, run the 24B restart fetch unchanged, then
copy `count = memHW[msg HW 0o17]` bytes from ND-100 byte address `mem32[msg HW 0o140] * 2`
into process virtual memory at `mem32[msg HW 0o15]`; if `count >= 0x2000`, skip the copy
and resume with K=1, X1=0o174 (the microcode does NOT reject the message - the process
resumes with the error in its own registers). Message still becomes the answer-in-place
target with MICFU:=23B, nothing answered until the next stop.

## Addendum (same day) - R2 committed as c62f16b81; CNTXT decode peer-reviewed

**Commit check:** `c62f16b81` matches the reviewed working tree byte-for-byte on every R2
file (verdicts above stand against the commit). One post-commit working-tree edit exists:
`CpuND500.IndirectSegments.cs` now parks (WAIT) BEFORE invoking the MON sink and undoes it
on decline - **that is flag R2-2 correctly fixed for the MON hook.** The TRAP hook
(`CpuND500.Trap.cs` RaiseTrap) still parks AFTER OnUnhandledTrap returns - same race, same
fix needed there (a fast 25B 3TRACO can beat the park).

**NEW FLAG R2-6 - classic 25B 3TRACO resets the process instead of resuming it.**
In the servicer, TrapContinue shares the start-class case; on classic that calls
`OnStartProcess`, which reloads the STASHED 21B register image - `regs.PC := initial P`.
A trap continue after a page-fault fix would therefore restart the swapper from its entry
point instead of resuming at the trapped instruction. On SAMSON the shared path is faithful
(NEWCNTXT reloads the CNTXTSAVEd state); on classic the live state was never unloaded, so
the correct emulator action for 25B is: clear WAIT + wake, do NOT reload the image.
(Related: the SAMSON side has the same practical hole until CNTXTSAVE-on-stop exists -
flag R2-5 - which the octobus note's "real-swapper reachability not claimed" covers.)

**CNTXT-BLOCK-DECODE peer review (item 4, the decode itself): ENDORSED - the load-bearing
claims re-verified word-by-word from MICRO-5800-B30.md by this session:**
- **EA2 := ctx+0x40 [V confirmed]:** TRAP_FIND 013154 / TRAP_LOAD 013264 are
  `EA2SAVE AA=2 AB=1 IX*2` with no ORCON -> MARG = 0x40 exactly.
- **The EA2-relative correction [V confirmed]:** TRAP_SAVE's writes are AA=6 (EA2) with
  MARG 0x54/0x60/0x58/0x24..0x34/0x3A/0x38 -> true ctx offsets 0x94/0xA0/0x98/0xA4-0xB4/
  0xBA/0xB8 - every Save/Load μ-address in the doc's trap-park rows matches (013237->0x94,
  013240->0xA0, 013241->0x98, 013250-254->0xA4..0xB4, 013257/260->0xBA/0xB8; TRAP_LOAD
  013266/267/270->0x94/0xA0/0x9C, 013275+->0xA4+). The old "ctx+0x54..0x74" anchors were
  indeed EA2-relative.
- **The 0x98 enable word [V confirmed]:** TRAP_FIND reaches it as EA0(=ctx+0x40)+0x58
  (013155 AA=4 IX*2 ORCON=0x18), read 013156, zero-test 013160.
- **Field map head + x256 stride (save/load side) [V confirmed]:** CNTXTSAVE 014666-014671
  and CNTXTLOAD 014742-014745 are FOUR plain `A+B,*2` self-squares (x256, no EXUC), then
  EXUC OFFSET (SC13:=0o4000) and DPA := SC12+SC13; EA3SAVE AA=2 anchors EA3 := ctx; the
  write/read chains step ORCON 0x04..0x3C exactly as tabled (P@0x00 014702/014751,
  L@0x04 via IAC,L capture 014700 and load 014760, B/R via DAC,B/DAC,XFER capture
  014674-014675 and REG04/LDRES 014755-014756, X1@0x10 014706/014762).
- Remaining [D] I could not close either: the GET_CNTXT EXUC-self double-execution
  mechanism (the doc's unknown #1) - but the four-plain-word x256 preambles make the
  stride itself effectively [V] for the save/load path the emulator uses, and GET_CNTXT
  must agree with them (doc's consistency argument holds).
- The emulator recipe in the decode doc's section 5 is consistent with what
  StartProcessFromContextBlock implements, minus the documented gaps (status@0x40,
  MM,PS/PHS, DIT enables) and minus flag R2-4 (CED/CAD byte-mask).

## Addendum 2 - R2-8 REVISED per Ronny (2026-07-17): interrupts are HELD until IDENT

**Ronny's correction (authoritative, ND bus hardware model): "interrupts are always
held until IDENT is sent."** A raised ND-bus interrupt stays PENDING on the device
until the ND-100 services the level and reads IDENT - it cannot be lost by timing once
raised. That reframes my earlier "edge is the model" closure, which leaned on the
DERIVED ND-05.012.01 phrasing ("interrupt if CONTROL bit 0 was set") and is hereby
downgraded:

1. **CONFIRMED (Ronny): held-until-IDENT.** Once level 12 is asserted it must remain
   asserted until the IDENT read acknowledges it. Emulator check: NDBusDeviceBase keeps
   InterruptBits set until the ident read clears the level - that part matches.
2. **NEW EMULATOR FLAG R2-9:** `CheckTriggerInterrupt()` calls
   `SetInterruptBit(doInterrupt)` with doInterrupt FALSE whenever the condition is not
   met - so the ACT50 activate path (which clears Finished, then calls
   CheckTriggerInterrupt) would DROP a still-pending, never-IDENT-acknowledged
   interrupt. Under held-until-IDENT that is wrong: nothing but the IDENT read (or
   master clear/reset) may clear a raised level. Only reachable if SINTRAN activates
   before servicing the pending level - rare but the fix is cheap: never clear the
   interrupt bit from CheckTriggerInterrupt, only ever set it.
3. **Late-enable retro-raise (the original R2-8 question) - LEVEL gate per Ronny.**
   Ronny (2026-07-17): the held request can also be "manually disabled by some reason,
   but that may confuse sintran" - i.e. the interrupt line is the continuous gate
   `enable AND pending-request`, where the pending request is latched until IDENT.
   Clearing enable while pending DROPS the line (legal but SINTRAN-confusing);
   re-enabling with the request still pending RE-ASSERTS it. So the correct emulator
   model is LEVEL-evaluated:
     - pending-request := set at answer/finished, cleared ONLY by IDENT/master clear;
     - level-12 line := pending-request AND CONTROL bit 0, re-evaluated on BOTH edges
       of the enable (a late LCON5:=1 with finished pending DOES raise level 12; an
       LCON5:=0 while pending drops the line but keeps the request latched).
   The pinned test `..._ButNoLevel12`'s final assert ("late int-enable does not
   retro-raise") pins the WRONG semantics and should be flipped when
   CheckTriggerInterrupt is made level-evaluated (one change: also invoke it from the
   LCON5 bit-0 mirror path, and per R2-9 never let it clear the latched request -
   only the line). Driver-visible impact today is negligible (Path B bare enable runs
   only with no waiting message; MP:3083 reactivate + TTMR recover misses), which is
   why the live machine never showed the difference. Grade: Ronny/hardware-model
   authoritative; no schematic read yet.

## Addendum 3 (2026-07-18) - HW Maintenance manual Appendix A vs the microcode: FULL AGREEMENT

Cross-check of ND-05.017.01 Appendix A (Reference-Manuals\500\ND-05.017.01 EN ND-5000
HARDWARE MAINTENANCE.md lines 10756-10899) against the peer-verified CNTXT decode.
**Verdict: GO - the manual and the microcode agree at every point where the microcode
has [V] evidence, and the manual explains the exact gaps the decode observed.**

1. **Context-disp column (x4 = bytes) matches all 20 [V] anchors** (L@0x04 ... E4@0x3C,
   CED disp 27B->0x5C, CAD 30B->0x60, SC1 33B->0x6C, SC2 34B->0x70) and NAMES the
   [?] rows: 0x40 ST1, 0x44 ST2 (SRF10 = "Status 2 surrogate" per A.2 - matching the
   decode's SRF10&0o1777), 0x48 PS (= SRF13/MM,PS [V]), 0x4C TOS, 0x50 LL, 0x54 HL,
   0x58 THA, 0x64 CES, 0x68 CAS, 0x74-0x90 OTE1/OTE2/CTE1/CTE2/MTE1/MTE2/TEM1/TEM2.
2. **The trap-park region 0x94-0xBA is named 1:1 by the manual's "Trap disp." column
   (octal BYTES: 224B=0x94 ... 272B=0xBA):** 0x90 trap-number-at-THM, 0x94 Trapping P
   (decode had "SC10 = saved P copy [D]" - confirmed), **0x98 = "Status, trapped
   between trap and entry finished"** - a trap-in-progress flag, NOT my earlier [D]
   name "local-trap-handler enable"; TRAP_FIND's zero-test is a NESTED-TRAP check
   (TRAP_SAVE writes it, MSG_UNIX5RE clears it - dataflow unchanged, name corrected),
   0x9C trap number, 0xA0 Restart P, 0xA4 MMS.STS, 0xA8 MMS.LA, 0xAC MMS.PHYS,
   0xB0 MMS.PHS/CAP, 0xB4 MMS.WR, 0xB8-0xBA Slot/BADAP (the decode's "ASTBAD" cell -
   name origin explained). This also RETIRES the EA0-semantics worry I was about to
   raise: the manual's byte disps land exactly on the EA2+0x40-relative addresses the
   decode computed, three-way lock (microword arithmetic / my re-derivation / manual).
3. **T1 upgrade: the fault-address-at-msg-HW-0o17 [D] is now CONFIRMED.** TRAP_GEN3's
   "ctx+0x68 -> msg 0o17" is EA2-relative = true ctx+0xA8 = **MMS.LA = the protect-
   violation ADDRESS** -> the first trap-record slot. AnswerTrapStop's placement is
   right; promote [D] -> [V+DOC]. Likewise the section 3.10 row "0o21 <- ctx+0x64"
   is EA2-relative = true ctx+0xA4 = MMS.STS: msg HW 0o21 carries the PV info word.
4. **The manual TEXT endorses the decode's "gaps" verbatim:** "Registers enclosed by
   parentheses are not saved in or loaded from the context block ... loaded from the
   domain information table before execution is started" (line 10760) - exactly the
   observed CNTXTSAVE/LOAD skip of 0x4C-0x58 and 0x74-0x90 plus TRAPSET-from-DIT [V].
   So: do NOT change the loader to read ctx OTE/LL/HL - the B30 dataflow is what the
   manual specifies.
5. **Formula confirmations:** "first block is always dummy" + Start+400B+ProcNo*400B =
   ctx = OFFSET + 0o400*(idx) with idx = X5CPU+1; SRF11 "Current/previous process + 1"
   and SRF17 "Current process + 1" confirm the +1 AND that message HW 4 (X5CPU) is the
   PROCESS number; "patched in location OFFSET (address 20)" = the decode's mu-word
   000020 OFFSET carrying LARG 0o4000. SRF2016 CPU-available (decode's srf 0o2016
   CPU_AVAIL), SRF2015 CPU-type-from-ACCP - end-to-end confirmations.
6. **One caution:** the OCR'd "DIT Disp." column contains invalid octal ("12 8B",
   "15 8B") - do NOT overwrite the decode's DIT+0x40/+0x44 (IDU,LL/HL) [V] offsets
   from that column without cleaning the OCR against the PDF first.

Recommended doc updates: rename per items 1-2 (grade [DOC-manual], keep the microcode
mu-addresses as the dataflow evidence), promote item 3, keep DIT dataflow per item 4.

| Item | Verdict |
|---|---|
| 1 _engineLock | CONFIRMED no deadlock / no RSTA5 impact; FLAGS: statusRegister RMW races (R2-1), park-after-answer ordering (R2-2, pre-existing), InterruptEnabled gating on async completion (R2-3, live-oracle) |
| 2 AttachRealCpu | CONFIRMED (split intact both threaded and deterministic); machine composition still does not call it |
| 3 synthetic capability domain 0 | CONFIRMED harmless (domain bits never consulted on the MON path; SAMSON MON exit is TRAP_MONC, no capability resolution) |
| 4 CNTXT cross-check | CAN'T-VERIFY sender-side (no SAMSON-SINTRAN source in tree; classic uses 21B); FLAGS: CED/CAD byte-mask (R2-4), no save-on-switch (R2-5); oracle = dump 0x800+ on the live ND-5800 |
| 5 3WMONCO source | **DECODED: source = ABUFA@HW 0o140 (ND-100 word addr x2), dest = 26ADD@0o15, count = 26NRB@0o17 bytes, word+byte-tail loops, >=8KB guard resumes with K=1/X1=0o174. Open question 8 CLOSED** |
