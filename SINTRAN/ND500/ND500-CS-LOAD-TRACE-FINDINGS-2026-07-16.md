# ND-500 control-store load: live-trace findings (2026-07-16)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md`
**Evidence grade: OBSERVED** (RetroCore emulator traces of real SINTRAN + nd-500-mon J04 executing
`@nd-500` / `status`), cross-checked against the byte-verified carve (FUNCS bodies, IOX driver).
Supplements `ND500-BUS-INTERFACE-REFERENCE.md` sections 5/8/9; where this doc and the reference
disagree, THIS doc records what the real software actually did.

Traces analyzed: `file-trace-status.txt` (388MB, status->error cycle) and
`file-trace-nd-500.txt` (387MB, full monitor startup). Emulator: NDBusND500IF (RetroCore).

## 1. The download ALWAYS happens on a classic ND-500 cold start

- Idle RSTA5 polls read `$200` (bit 9 5CLOST = clock stopped) from BEFORE nd-500-mon starts:
  SINTRAN's own boot leaves the classic 500 stopped. First command -> ECSLOAD -> auto-load.
  This is CORRECT (no microcode ROM), not a CPU-type misdetection; install docs omit it because
  deliveries shipped `(SYSTEM)CONTROL-STORE:DATA`.
- `@nd-500` itself generates ZERO 3022 traffic beyond the RSTA5 polls (banner + prompt are pure
  ND-100). The "DESCRIPTION FILE ERROR: NO SUCH FILE NAME" at startup is unrelated to the interface.

## 2. The observed load protocol (loader = SINTRAN FUNCS CSLOA path)

Preamble: WRTAG(0); RETG5:=2 (stop, 5CLOST set); MCLR5 strobe **issued as an IOX READ**;
test-mode STATUS rewrite (LCON5:=10B, LSTA5, LCON5:=0).

Download, per 16-bit part (observed 14564 words x 9 parts, WA monotonic 0..$38E3, ~30 s):
```
DATAX(LLOW5) := value -> TAG DICLK1 -> TAG DIEN -> TAG BRKCLK (data) / WACLK (address)
TAG CNTCLK with CSCNT = 0x100 | load(bit0) | part<<2       (parts 0..8)
every strobe wrapped in WRTAG: LTAG5, LCON5:=44B, UNLC5-as-READ, LCON5:=40B
```

**VERIFY (previously unknown - the reference does not document it):** after the last word,
RETG5:=2 again, then words 0-7 are read back, per part:
```
DATAX := CSCNT = 0x100 | read(bit1) | part<<2  -> DICLK1 -> DIEN -> CNTCLK
TAG DUEN -> TAG EDUTEN (enable DATA-OUT) -> CLKD5 strobe -> LCON5:=$28 (test+bit5)
-> IOX READ of offset 6 (test-mode DATA read) = the stored part
```
Mismatch => `ND-500(0) error: Error in loading Control Store`, **without** MICRO-START ->
5CLOST stays set -> every subsequent command re-triggers the load. Success (presumed) =>
MICRO-START (RETG5:=0 per FUNCS MPSTA @153006B) -> gate opens.

## 3. Strobe-direction facts (trace-proven; the reference implies write-only)

SINTRAN strobes via **IOX READS**: `UNLC5` (611k reads in one load), `MCLR5` (5MCLE), plus
`SLOC5`. An emulator implementing these only on the write side wedges the lock (STATUS $28/$204).
WRTAG wraps EVERY TAG access in `LCON5:=44B` - CONTROL activate writes must NOT set busy/finished
or touch the micro clock (the "activate = operation" model is disproven; only RETG5 bit-1-clear
and MCLR5 restart the clock).

## 4. STATUS value timeline (healthy vs wedge signatures)

| Phase | RSTA5 |
|---|---|
| cold idle (clock stopped) | `$200` |
| during download | `$004`/`$024` (busy artifacts in pre-fix emulator; real HW expectation: lock toggling) |
| stopped for verify | `$200` |
| WEDGE signatures (emulator bugs, fixed) | `$28` (stale finished+lock), `$204` (stale busy+stopped) |
| healthy idle after full cycle | `$0` |

## 4b. MAR is an ND-100 WORD address (live-proven)

After a successful CS load + micro-start, SINTRAN activates the ND-500 with the mailbox message;
the timeout status block showed **`MAR 00010220230`** = word `0x212098` = emulator byte `0x424130`,
inside the 5MPM region above page `4100B` (matches MEM-CONF). An emulator that interprets MAR as a
byte address reads 2 MW below the real message, finds no `MSGN500`, never answers, and SINTRAN
dies with "ND-500(0) timeout / CPU locked" (interface stays locked, `N100 STATUS 000041B` =
int-enabled + 5ILOCK). The mailbox engine must convert: message byte base = `(MAR & 0xFFFFFF) * 2`.

## 5. Consequences for any 3022/5015 emulation

1. A real control store is REQUIRED (load + read-back verify), not just a clear gate bit.
2. SLOC5/UNLC5/MCLR5 side effects on read AND write.
3. RETG5 bit1 set = stop (5CLOST set); bit1 clear = restart (5CLOST clear).
4. Bare CONTROL-activate: lock only - no busy, no finished, no clock change, no completion.
5. Microcode version for RMVER answers: cached CS microword 1, last 16-bit part
   (observed `027232B`/`0x2E9A`; word 0 = boot jump, its trailing 2E9A is control fields).
6. The loader pushed a 14564-word (ND-5800-sized) image with no size check - content is not
   validated against CPU type; only the read-back must match.

Implementation + regression tests: RetroCore `NDBusND500IF.cs` + `Emulated.Tests.ND500\nd500if\`
(`LoadControlStore_FullCycle_StopLoadRestart_GateClearsAgain`,
`LoadControlStore_VerifyReadback_ReturnsWrittenParts`). Companion plan:
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\ND500-MON-COMMAND-TEST-PLAN.md`.

## 6. Post-restart window analysis (file-trace-500.txt, added 2026-07-17)

Trace scan of the window between the last MICRO-START (RETG5:=0, line 4439043, 13:32:11.824)
and the first MICFU=13B mailbox (line 4439066, 13:32:12.167). Grade OBSERVED unless noted.

1. **The ONLY traffic in that window: 19 RSTA5 reads (all returning $00000000) over ~340 ms,
   then MAR (word 0x210718) + CONTROL:=5 to activate the mailbox.** No data-register writes,
   no TAG writes, no DMA - zero of each. SINTRAN placed NO content into ND-500 memory through
   the 3022 after the restart. (Plain ND-100 window writes are not logged by the trace, so
   window writes remain possible but invisible.)
2. **SINTRAN polls status right after MICRO-START waiting for a change that never comes**,
   then proceeds anyway. What it wants to see there is an OPEN question (the microcode never
   touches 3022 status - MICROCODE-ANSWER-INIT-SAMSON-AND-13B-2026-07-17.md; so the awaited
   change must be interface plumbing or SINTRAN-side timing). Same pattern repeats between
   13B msg 4 and msg 5, polling $21 for ~1.03 s.
3. **13B messages 1-4: src=ND-500 addr 0, dest word 0x212400, 4000B bytes. Msg 5:
   N500A(offset 7)=177B = the HIGH half of the 32-bit source (microcode-verified word at
   offsets 7-10B) -> msg 5 source = 0x7F0000, the TOP page of configured memory.**
4. TRAPN=10746B in the 13B requests is leftover garbage (microcode never reads offset 16B in
   RESIRD) - do not decode it as a parameter.
5. Emulator fixes from this analysis (2026-07-17): 13B source decode widened to the full
   32-bit word (offsets 7-10B); 14B RESIWR implemented (mirror copy; previously dropped as
   unknown MICFU -> 5ERANSWER); DEBUG_DETAIL content dump of src/dest around every 13B/14B.

### 6a. RESOLVED 2026-07-17 (byte-verified carve): the 13B burst is a MEMORY PATTERN TEST

Carved from `tools\...\re\segments-ref\030-S3SM5\030-S3SM5.asm` (symbols TSTMC=52235B,
TSTPA=52606B; sender loop 052473-052605; compare loop 052556-052574):

- `CSLOA` (control-store loader) calls `TSTMC` ("test memory configuration") in its tail at
  155565, right after MICRO-START - so a memory-test failure surfaces DURING "Loading Control
  Store" and one rejection prints BOTH console errors (EILOCS 2103B from the CS verify path,
  EIMDCONF 2054B from the memory test; codes defined 5P-P2-MON60.NPL:84/107).
- Per page, `TSTPA` runs FOUR passes with patterns `177777B`, `000000`, `125252B`, `052525B`:
  each pass (1) writes the pattern into the ND-500 page **directly through the multiport
  window** (STATX loop @052527 - invisible to the 3022 trace), (2) sends ONE 13B RESIRD
  (SAA 13 @052534; msg fields 7/11B/13B, count 4000B - matches the trace exactly),
  (3) compares all 1024 returned words against the pattern (@052563-052573); any mismatch ->
  A := EIMDCONF 2054B. The "four bit-identical messages" are the four pattern passes.
- The dest word 0x212400 is an ND-100 scratch page fixed via MON 61 (FIXC5) inside TSTMC -
  a read-back buffer, nothing more.
- Msg 5 source = `(page - part_base) << 11` for the TOP page of the configured part -
  memory-SIZING verification. The old emulator read from byte 0x7F instead of 0x7F0000 ->
  guaranteed pattern mismatch -> the live failure. Pages 0 passes were succeeding all along.
- The user-visible MON 60B twins: `AMEMR`/`AMEMW` (FUNCS[032]) build the same 13B/14B
  messages at 142474/142545.
- LIVE CONFIRMATION 2026-07-17: with the 32-bit source fix, `status` passes "Loading Control
  Store" (memory test OK) and proceeds to "Loading Swapper".
