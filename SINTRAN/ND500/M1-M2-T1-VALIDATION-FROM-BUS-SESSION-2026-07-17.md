# M1/M2/T1 validation - from the bus-interface session

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\M1-M2-T1-VALIDATION-FROM-BUS-SESSION-2026-07-17.md`
**Reviewing:** RetroCore `14e4c6ab4` (M1 servicer side) + `c8cd1db92` (M1 CPU side + M2 + T1),
branch ethernet-ii-controller-fixes.
**Reviewer basis:** MP-P2-N500.NPL (DECOMESS/MCHANDEL/TRAPDECODER/5ACTSWAPPER/RFRRE),
L07 N500-SYMBOLS, the mailbox catalog 7c/7d, the live traces, TERM5 byte findings.
**Code freeze respected:** no product files touched; findings are flags only.

## Per-item verdicts

### Item 1 - classic completion split: CONFIRMED (shape) + TWO FLAGS

What the real driver expects, from the carve:
- `DECOMESS` @135161 gates on **MICFU first** (must be 3MONCO/3TRACO/3START/3WMONCO),
  THEN reads STOPR and routes MOCALL/5FMOCALL -> MCHANDLE, TRAPCODE -> TRAPDECODER.
  The taken start leaving MICFU=23B in place, and M2's MICFU:=23B rewrite on a taken
  restart, are exactly what keeps the eventual stop answer routed through this gate.
  The rewrite is not just bookkeeping - without it a stop after a 24B restart would
  arrive with MICFU=24B... which IS in the legal list too (3MONCO), so both work, but
  the 23B rewrite matches the microcode [V 015715] and is correct.
- Interface state: taken start returns "nothing answered" -> lock stays SET, finished
  stays CLEAR -> SINTRAN's wait loop keeps waiting; async AnswerWritten ->
  SetOperationComplete (busy clear, finished set, level 12) with lock still set until
  XTER500/TERM5 - matches the TERM5 byte semantics ($29 pre-TERM5 / $09 post).

**FLAG 1a - lost-wakeup race (latent, becomes live at machine wiring):**
`_inSyncMailboxProcessing` is a plain bool written on the ND-100 IOX thread and read on
the CPU thread (AnswerWritten via AnswerMonitorCallStop/AnswerTrapStop). If the running
process stops at the exact moment the IOX thread is inside ProcessChain for a DIFFERENT
message (SINTRAN activating another mailbox op while the swapper runs - normal in steady
state), `!_inSyncMailboxProcessing` evaluates FALSE on the CPU thread and the async
completion is **silently skipped** - finished/level-12 for the stop never fire and
SINTRAN hangs on that process. Extra interrupts are harmless on this driver (NXTMSG
scans N5STA states), lost ones are fatal - so the safe degenerate fix is "fire
SetOperationComplete unconditionally for stop answers"; the flag should at minimum be
volatile and the skip case re-examined. NOT urgent for the current tree: no production
machine wiring attaches Nd500CpuProcessBridge or StartRunThread yet (tests only), so
today everything still runs single-threaded. Must be resolved in the wiring phase.

**FLAG 1b - async shape has NO live-trace coverage yet.** Every live trace so far
contains only synchronous answers (13B/14B/12B/21B); no trace has ever shown a taken
23B with a deferred answer, because LOAD-SWAPPER used to die at 21B. Whether SINTRAN
TERM5s immediately after the 3START ACT50 (fire-and-forget) or waits for finished
decides whether the async finished+interrupt arrives with the interface unlocked.
The next live run (start-swapper, once the bridge is wired) is the oracle; CAN'T-VERIFY
against wire until then.

### Item 2 - MOCALL stop record: CONFIRMED slot-for-slot

- `MCNO@13`: MCHANDEL reads it for logging @137021 and saves to SMCNO @137046. Match.
- `STOPR@11 := 1`: DECOMESS `IF A=MOCALL` @135210 (catalog: MOCALL=1). Match.
- `NUMPA@12 := argc`: read @143150 in the X5 return path. Match.
- **Two strided arrays byte-confirmed via symbols:** addresses `5PPA1=000040`,
  `5PPA2=000042` (stride 2 HW = one 32-bit slot per param); values `5DP1=000101`,
  `5DP2=000103`, `5DP3=000105` - the LOW halfword of value k sits at 0o101+2k, i.e.
  values are 32-bit hi-first at 0o100+2k. The servicer writes hi at byte 0x80+4k, lo at
  0x82+4k = exactly that. NOT (addr,value) pairs. Match.
- argc clamp 16: consistent with SINTRAN's own mask usage (max seen `100000B` = bit 15
  = param #16 @141366). Match.
- Saved P at HW 7-8: SINTRAN never reads it at stop time (carver R2) - harmless,
  microcode-faithful. Match.

### Item 3 - seg-31 hook: mode policy CONFIRMED, but ONE STRUCTURAL DISCREPANCY

Mode policy: verified - with the sink attached both branches (taken -> park at
PendingCallReturnAddress; not-taken -> honest HALT) return before `#if
SINTRAN_EMULATION`; no path reaches the emulated layer. `PendingCallArgCount/
ArgAddresses/ReturnAddress` are populated by Call.cs/Callg.cs BEFORE
CheckAndHandleIndirectCall on the real instruction path, so the sink gets real
descriptors. `vectorIndex` = byte offset = MON number for segment 31 (EQU 37B9+n). All
consistent.

**DISCREPANCY 3a - the hook is unreachable for a real SINTRAN-loaded process (the
PC_INDIRECT seeding caveat is REAL):**
`CheckAndHandleIndirectCall` gates on `GetProgramCapability(regs.CED, 31) & PC_INDIRECT`,
and `GetProgramCapability` reads **`PCBTable` - a C#-side object seeded only by the DOM
loader or `SetProgramCapability`**. The live swapper arrives as 44 x 14B raw block
copies into ND-500 memory + a 21B register image; NOTHING in that path touches
PCBTable. So at the swapper's first `CALLG` to segment 31: capability = 0 ->
PC_INDIRECT clear -> DIRECT call into unmapped segment-31 space -> page fault or
garbage; **the MON sink never fires and the swapper-alive chain (MON 377B ->
MOCALL stop -> LNEWSWAP -> PSWWAIT) breaks at its first link.** The M1 bridge tests do
not catch this because they call `HandleIndirectSegmentCall` directly, bypassing the
capability gate.

Fix options (architect's choice): (a) hard special-case `segmentNumber == 31` in
CheckAndHandleIndirectCall when a MonitorCallSink is attached (treat as indirect
regardless of capability); (b) have the bridge/machine wiring seed
`PCBTable[domain].ProgramCapabilities[31] |= PC_IND` at attach. Option (a) is safer -
it works for whatever CED value the 21B image leaves (the image currently applies only
P; CED is whatever Reset left it, and PCBTable[CED] is likely null so (b) would need a
PCB allocated too). **Question for the microcode LLM:** does the real CALL path decode
the MON segment via the capability's indirect bit read from the memory-resident PCB, or
is segment 37B hardwired in the CALL microcode? That answer decides which fix is
hardware-faithful.

### Item 4 - M2 restart: CONFIRMED slot-for-slot against the MONICO sender

- FUNCV 32-bit at HW 13-14 hi-first: `A:=0=:D; *AAX FUNCV; STDTX` (STD = A@13 high,
  D@14 low) at 134134, 137151, 141467, 143047ff, 145062, 147350. Servicer reads
  (MCNO@13 << 16) | MSWMC@14. Match.
- KFLIP@11, nonzero = error: `IF A=:CCKFLIP=0 THEN %NO ERRORS` @134022; error path
  X5ERET sets `A:=1 -> KFLIP` @143222. Servicer/bridge `kflip != 0 -> K`. Match.
- **NUMPA as write-back MASK, bit k => slot k, byte-proven:** 5ACTSWAPPER @145057 sets
  `NUMPA := 6` with comment "Par #2 & par #3 will be written into", and the values it
  stages are `SWPST=000103` (= low half of value slot k=1) and `HSWPI=000104` (= value
  slot k=2, 32-bit) - i.e. bits 1,2 <-> value slots 0o102-0o103 / 0o104-0o105 <->
  address slots 0o42 / 0o44. Exactly the servicer's `bit k => value@0o100+2k ->
  addr@0o40+2k`. Also `100000B` (bit 15 = par #16) @141366 and `4` (bit 2 = par #3,
  trap-restart variant) @141374. Match, including the 16-slot bound.
- MICFU:=23B rewrite on taken restart: see item 1 (keeps DECOMESS's MICFU gate
  satisfied at the next stop). Match.
- Declined restart -> immediate placeholder answer: never happens on real HW; inert and
  visible in the log. Acceptable.

### Item 5 - T1 TRAPN: CONFIRMED

- `TrapCondition` ST bit numbers ARE the TRAPN vocabulary: PGF = bit 38 = 46B =
  TRAPDECODER's special-cased page fault @135332 (`IF D = 46`). The enum's full range
  is bits 11..41 = TRAPN 13B..51B, all inside TRAPDECODER's legal window (`IF D>53 THEN
  ILTRAP` @135324 - legal 0..53B). No divergence; no mapping table needed. Match.
- Argument mapping verified: RaiseTrap's `trapAddress` is documented "usually PC" ->
  bridge's `trappingPc` -> saved P; `dataAddress` -> fault address. Consistent.
- Saved-P overlay note: AnswerTrapStop writes P.lo into HW 15; TRAPDECODER's
  `*AAX TRAPN-1; LDDTX` loads HW 15 into A and HW 16 (TRAPN) into D - A is discarded
  immediately, harmless.
- Fault address @ HW 0o17-0o20 stays **[D]**: TRAPDECODER itself never reads 0o17 (it
  reads TRAPN, MICFU, 5RECE, then packs `MSWPFAULT SHZ 10 + trapno` for the swapper).
  The actual consumer is the SWAPPER - SINTRAN hands it the faulting MESSAGE ADDRESS
  (HSWPI value slot) and the swapper reads the trap record itself. So the 0o17 pin
  will come from the swapper carve (not yet located there) or the microcode TRAP_GEN3
  word map. No contradiction found; no confirmation either.

### Item 6 - 3WMONCO 26B: SENDER SIDE NOW PINNED (closes half of open question 8)

Two SINTRAN builders found in MP-P2-N500.NPL:
- 2CLOCK MON-call path @137136-137164 (`MIFLAG BIT WSMC` = microcode-write-back mode);
- X5 file-function return path @143020-143046 (RFRRE etc.; comment block @142755:
  "7th parameter =: 26nrb, x5buf =: 26add").

**Message fields the sender fills (L07 symbols):**
- `26ADD = 000015` (HW 0o15-0o16, 32-bit STDTX hi-first) = **destination process-logical
  address** - loaded from the caller's own parameter-ADDRESS slots (`PDR1=0o40` in the
  2CLOCK builder; `X5BUF=0o44` / `X5BUF+2=0o46` in the X5 builder). NOTE: 26ADD-0o16
  overlays MSWSP/TRAPN, 26NRB overlays the trap-record slot - overlay by STOPR context,
  fine, but nothing may assume TRAPN survives across message roles.
- `26NRB = 000017` (HW 0o17, one word) = **byte count** (34B = 28 bytes in both).
- `SM26A = 000074` / `SM26N = 000135` = SINTRAN-side SAVED copies; on a
  writeback-buffer restart @135641-135652 SINTRAN restores 26NRB/26ADD from them before
  resending - so the microcode may clobber the live fields.
- Plus the normal restart trio: FUNCV := ok/value, KFLIP := 0, NUMPA := 0 (the block
  copy replaces the per-param mask write-back in these paths).

**Still open (the other half):** the SOURCE of the copied bytes is NOT in any dedicated
field the sender writes. Candidates: the message's own value/buffer area, or the ND-100
address at `ABUFA=000140` (the parallel non-microcode path 3WMED at 137166/143047 uses
N500A=dest, NRBYT=count, N100A:=ABUFA content=source - symmetry suggests ABUFA, but
that is INFERRED). Decoding which HW offsets MSG_CONWR_1/_2/_W/_B (015752-016004)
actually fetch closes it from the microcode side - that flow has both a _W(ord) and
_B(yte) variant, consistent with a byte-count-driven copy.

## Swapper-alive chain assessment (21B -> 3START -> MON 377B)

The one thing in these commits that breaks the chain is **DISCREPANCY 3a** (PCBTable
gate) - everything else in the chain is faithful. Watch items, not blockers:
- 21B image application currently sets only P (register 0); the rest is stashed
  verbatim. The swapper does its own stack init before the REV.-K01 self-check, so P
  alone may suffice - the canary decides (MON 0B right after 3START = context wrong,
  MON 377B subfn 1 = right).
- The MOCALL record for MON 377B will carry argc 4 with addresses+values; MCHANDEL ->
  N5SWAP handler reads values via 5DP1/5DP2/... - slot layout verified above, so if the
  call reaches the sink, the record is right.
- No production machine wiring attaches the bridge yet - the current EXE would still
  placeholder-answer 23B (swapper never executes, no stop record; SINTRAN then reads
  leftover STOPR garbage through the DECOMESS gate). Expected and known; wiring phase.

## Summary for the architect

| Item | Verdict |
|---|---|
| 1 classic completion split | CONFIRMED shape; FLAG lost-wakeup race on `_inSyncMailboxProcessing` (threaded), FLAG async path live-unverified |
| 2 MOCALL stop record | CONFIRMED (5PPA1/5PPA2 + 5DP1/2/3 symbols prove both strided arrays) |
| 3 seg-31 hook | Mode policy CONFIRMED; **DISCREPANCY: PCBTable/PC_INDIRECT gate makes the hook unreachable for real SINTRAN processes** |
| 4 M2 restart | CONFIRMED (NUMPA:=6 <-> SWPST@0o103/HSWPI@0o104 slot-for-slot) |
| 5 T1 TRAPN | CONFIRMED (bit 38 = 46B; all enum bits inside legal 0..53B); fault-addr@0o17 stays [D], consumer is the swapper |
| 6 3WMONCO layout | Sender side PINNED: dest `26ADD`@0o15 (32-bit), count `26NRB`@0o17; source still open (ABUFA@0o140 INFERRED candidate; decode MSG_CONWR_W/_B fetches) |
