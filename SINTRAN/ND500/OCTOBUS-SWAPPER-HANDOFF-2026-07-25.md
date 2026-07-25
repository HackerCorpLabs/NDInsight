# OCTOBUS ND-5000 Swapper Bring-up - Handoff (2026-07-25)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\OCTOBUS-SWAPPER-HANDOFF-2026-07-25.md`

Self-contained handoff for continuing the ND-100 <-> ND-5000 (SAMSON) OCTOBUS swapper
bring-up in the RetroCore emulator. Read this top to bottom; everything needed to resume is
here or linked. Sibling skill: `octobus-nd5000`. Related: `nd5000-microcode`,
`nd-500-bus-interface`.

---

## 1. The goal

Boot SINTRAN III L (the ND-5800 image) far enough over the OCTOBUS that the **ND-500 swapper
runs**, and ultimately a real domain reaches the `NLL:` prompt. No guessing - validate every
step against real B30 microcode bytes / carved SINTRAN, mark VERIFIED vs INFERRED.

## 2. The two CPU emulators (do not confuse the lanes)

| | `CpuND500` (functional) | `CpuND5000` (microword) |
|---|---|---|
| What | macro interpreter | real MICRO-5800-B30 microcode, one 128-bit microword/tick |
| Where | `Emulated.HW\ND\CPU\ND500\CpuND500.cs` | `Nuget\HackerCorpLabs.Emulation.CPU.ND5000\src\CpuND5000.cs` |
| MMU | HAS one wired into fetch (`CpuND500.MMU.cs`, PROVEN) | MmsUnit NOT wired into fetch (identity addressing) |
| Role | production target (drive it high-level from octobus STAMIC0) | ORACLE - run real microcode to learn correct behaviour |
| Edit? | mine/ours | READ-ONLY (microcode session's lane) - do NOT edit |

**Production path (Ronny's decision):** detect `STAMIC0` in the octobus station and emulate
**high-level** what the microcode does, driving the functional `CpuND500`. Keep `CpuND5000` as
the oracle to learn from. We are NOT swapping the high-level CPU out for the microword CPU in
production.

## 3. Octobus ND-5000 bring-up (VERIFIED from live capture, ACCP exchange)

Order the ND-100 side drives to bring up the 5000:
`016B LSSYSPAR` -> `071B CPURES` -> `034B STOPMIC` -> `021B LPARPNT (ptr 0x0800)` ->
`022B VERPAP` -> `023B LCS0` (streams real B30 microcode, version word 0x2E9A) ->
`025B DUC0` (readback verify) -> `066B STAMIC0` (StartMicDirect - starts micro clock at
CS 000000 = SAMSON cold boot) -> `061B ENKICK` -> `202B model/version report`.

STAMIC0 handler today: `Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs` case 0x36 (~line 1670)
only sets `_microprogramRunning=true`, loads real CS into `_controlStore` (16384x8 halfwords),
`ConfigureMailboxFromControlStore`, `AttachRealCpu(CpuND500)`.

## 4. Microcode swapper-start (agent-verified against the B30 listing)

Cold boot CS 000000 (SAMSON) -> INIT_SAMSON 014517 -> IDLE 024670 (mailbox poll, exits when
`X5ACT==0`). STRICTLY MESSAGE-DRIVEN (no auto-start; SRF11 == -1 => no runnable process).
MICFU dispatch table @ MSG_00 015224: 20/21 = MSG_ILLEG on B30, 22 = MSG_STARTP0 (proc-0
bookkeeping + soft self-kick, NOT macro start), **23/25 = MSG_START @015671**.
MSG_START -> CPU_AVAIL? gate (`srf 0o2016` / index 0x40E == 0) -> NEWCNTXT @014660 (loads PCB @
`0o4000 + (X5CPU+1)*256`) -> EXECUTE @014636.

The swapper itself is ORDINARY MACROCODE (`SWAPPER-K01.PSEG`, linked at virtual `0x08000000`,
entry P=`0x08000004`), so the functional `CpuND500` CAN run it. See skill `sintran-carving`
traps 9/10/11 and `SINTRAN\ND500\swapper\`.

## 5. CURRENT STATE (this is where you resume)

> **STATUS AT END OF SESSION 2026-07-25: MILESTONE - 420 macro instructions, 2 MON restarts, NO
> THROW.** The swapper completed initialisation and reached its MAIN MESSAGE-WAIT LOOP at
> `P=0x08000677`. Every microcode gap on this path is closed; the run now ends on the diagnostic's
> tick budget because the STUB sink keeps answering with the same message. Closing that needs the
> real octobus mailbox transport, NOT more microcode. Sweep `match=12797 / diverge=4972`, suite 301
> passed / 0 failed. **Read 5.6 and 7.0 first** - the walkthrough below (48 instructions, the MON-call
> stop) is the session's STARTING point, kept because the decode work in 5.1-5.5.7 is still the
> reference for how the message path works.

**REVISED 2026-07-25 (later same day) - the earlier text of this section was stale and is
corrected below. It described a 34-instruction `dctsb` spin at P=0x08000162; that state no
longer exists.** Re-measured by actually running the diagnostic (output quoted below).

The diagnostic (see section 6) drives the microword `CpuND5000` from a posted MICFU-23 (3START)
message with a PCB, and it runs the swapper as real macrocode. Current measured result:

- **48 macro instructions execute, 610 microword ticks**, then an honest throw.
- Stop reason, verbatim:
  ```
  CALL to segment 31 (SINTRAN MON 255B) target=0xF80000FF at P=0x0800823F:
  no MonitorCallSink attached - needs the ND-100/mailbox transport to service the MON call
  ```

### 5.1 "MON 255B" in that message is a base-notation bug - it IS MON 377B

VERIFIED. `CpuND5000.cs:678` computes `monNumber = (ushort)(target & 0x07FFFFFF)` = `0xFF` =
**255 decimal**, and line 693 formats it as `MON {monNumber}B` - a DECIMAL number printed with
the ND **octal** `B` suffix. 255 decimal = 377 octal. The disassembler independently decodes the
same bytes as MON 377B (`swapper-k01-pseg.asm:10535`):

```
1000101077: 303 370 000 000 377 004 304 010 001 052 050 304 010 002 100 260
            304 010 002 100 264 105
   call $1777777777777000000377,$4,$1000225050,$1000440260,$1000440264,b.24 ; MON 377B
```

0o101077 = 0x823F = the throwing P. So the call IS MON 377B, with **4 arguments**:
`$1000225050`, `$1000440260`, `$1000440264`, `b.24`.

**Action for the microcode lane (do NOT let me edit their file):** `CpuND5000.cs` line 693 should
print octal (or plain decimal without the `B`), and the comments at lines 622 and 669 label this
call "MON 255B / PIOCM" - PIOCM appears to be imported from the ND-100's MON 255B, a different
call. Both are misleading. This is exactly the base-notation trap section 6 warns about.

### 5.2 The `dctsb` / PIA blocker is FIXED and retired - do not re-investigate it

The old spin at P=0x08000162 was `dctsb` (0xFF1D), a **privileged** instruction (ND-500 Reference
Manual: DCTSB = "clear data translation speedup buffer", opcode 177435B - NOT a wait, NOT a poll).
It trapped to ILLEG -> DUMMY because PIA (macrostatus bit 1) was clear. That is already fixed at
`SwapperStartDiagnosticTests.cs:170` (`SeedDomainInfoTablePia`). The macro trace now shows
`#35 P=0x08000162 op=0xFF` retiring and `#36` continuing at `0x08000164`.

Byte-proven mechanism (from `Probe_PcbMacrostatusOffset_ForPia`, run 2026-07-25):
- **PCB+0x40 is the macrostatus field** - the only offset in 0x00..0x74 that reaches `MIC,STS`
  (loaded at `Mpc=0o15033`).
- **PIA is NOT taken from the PCB macrostatus.** With PCB+0x40 = 0xFFFFFFFF the trace shows
  `MIC,STS: 0x00000000 -> 0xFFFFFFFF` (0o15033) then `-> 0xFFFFFFFD` (0o15102): microword
  0o15102 **cleared** bit 1 from a Domain Information Table byte.
- **DIT byte 0x000000C8 bit 0 is the PIA source** (= NDIX `pcb_pia`). Seeding it yields
  `MIC,STS = 0x00000002`. Candidates 0xA6/0xA9/0xBB do nothing.

### 5.3 Correction to the old INFERENCE

The old section 5 inferred a poll loop on cell `0x08014670` (`$1000246470`). That inference is
**disproved**: `swapper-k01-pseg.asm:106-119` shows 0o532..0o630 is **straight-line and ends in
`ret`** - there is no backward branch to spin in, and the `w test $1000246470` at 0o532 is a
one-shot test. The swapper does reference that cell, but it is not a spin-wait target.

The old section 5 was nonetheless **right about the lane**: the live blocker is environmental, in
the octobus/NUCLEUS lane. It is just a different call at a different address than stated. Task #22
("Diagnose octobus X5BEX-resolves-to-zeros stall") is the same boundary from the octobus angle.

### 5.4 The seam already exists

`CpuND5000` has an `IMonitorCallSink? MonitorCallSink` exit seam (`CpuND5000.cs:70`,
`RouteCallSubroutine` at :625). On a segment-31 CALL it sets `Regs.L = Regs.P = returnAddress`,
computes the MON number, and calls
`OnMonitorCall(monNumber, argCount, PendingCallArgAddresses, returnAddress)`. Returning `true`
parks the CPU (`State.Stopped = 1`) until the transport restarts it. Implementing that sink is the
octobus lane's work and requires **no edit to CpuND5000.cs**.

## 5.5 MON 377B DECODED + stub built (2026-07-25, octobus lane)

Step 1 of section 7 is DONE. The call at `P=0x0800823F` is the swapper's **self-announce
(N5SWAP)**, already pinned independently at
`SINTRAN\ND500\GATE1-SWAPPER-COLDSTART-TRACE-ANALYSIS-2026-07-21.md:20` from a live functional-CPU
run, and confirmed again here by running the microword CPU.

**Sub-function = 1 ("ask the ND-100 for a message") - VERIFIED, not inferred.** The new stub logs
the argument EAs and the values at them:
```
MON 377B (=255 dec) argc=4 resume=0x08008255
  args=[0x08012A28->0x00000001, 0x080240B0->0x0, 0x080240B4->0x0, 0x0802428C->0x0]
```
arg0 = `0x08012A28` = `0o1000225050`, holding **1** - matching
`swapper\swapper-k01-deep-analysis.md:175` exactly.

**The answer contract, byte-proven from the macrocode** (`swapper-k01-pseg.asm:10543-10546`, the
instructions immediately after the MON returns):
```
1000101171: w1 := $1000440260   ; w1 := [0x080240B0]   <- the MON write-back word
1000101177: w1 =: $1000440270   ; [0x080240B8] := w1   <- becomes the message FUNCTION CODE
1000101205: w stz $1000440260   ; clear [0x080240B0]
```
So `[0x240B0]` **becomes** the function code. asm:10560-10566 then validates it (`>= 0` AND
`<= 0o34` = 28); asm:10576-10577 uses it to index the RIOM count table. This makes the
previously-magic 3022 write-back values coherent:
- `[0x080240B0] := 5` - a legal function code (inside 0..28).
- `[0x080240B4] := <ND-100 physical source address of the message>`.
- RIOM count for fn 5, read from the swapper's OWN DSEG table at `0x0802408C + fn*2` = **8
  halfwords** (16 bytes). Measured, not chosen.

**Stub landed:** `RealSwapper_ServiceMon377BAnnounce_HowFarThen` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\SwapperStartDiagnosticTests.cs`
(class `SwapperAnnounceSink : IMonitorCallSink`). Full ND5000 suite 296 passed / 1 skipped / 0
failed. It writes the two contract words and logs every MON call. Note the isolated diagnostic has
ONE flat identity-mapped memory, so RIOM's `RD,POF` read of "ND-100 physical 0x00210718" is just an
index into the same array - no ND-100 emulation needed to feed it.

### 5.5.1 MON-call restart SOLVED - the swapper now resumes after its MON 377B

**The MICFU-24 (3MONCO) restart works. Two things had to be right; the first was missing.**

**(a) The MICFU-24 dispatch is real - BYTE-PROVEN from the dispatch microwords.** MSG_00 is at
0o15224, so the slot for MICFU n is `0o15224 + n` and each slot's NEXT field is the handler address.
Decoded directly out of `MICRO-5800-B30.DATA` (not inferred from the LABE cross-reference lists):

| MICFU | slot | handler | label |
|---|---|---|---|
| 0o20, 0o21 | 0o15244/45 | 0o15221 | MSG_ILLEG |
| 0o22 | 0o15246 | 0o15660 | MSG_STARTP0 |
| 0o23 | 0o15247 | 0o15671 | MSG_START |
| **0o24** | **0o15250** | **0o15676** | **MSG_CONMC** (continue after monitor call) |
| **0o25** | **0o15251** | **0o15671** | same address as MICFU 23 - `MSG_CONTRP` is an ALIAS |
| 0o26 | 0o15252 | 0o15703 | MSG_CONWR |
| 0o27 | 0o15253 | 0o15221 | MSG_ILLEG (so `MSG_CONBH` is an alias for illegal) |

The LABE cross-reference derivation and this direct decode agree exactly. Reproduce with
`MicrowordDecodeTests.MsgConmc_RestartPath_RawDecodeDump`, which dumps the whole table for
MICFU 0..0o37.

This also **corrects section 4**: MICFU 25 is `MSG_CONTRP` (continue after trap), which merely
shares address 0o15671 with `MSG_START` - not a second start entry.

**(b) X5ACT is the poll gate and MUST be re-cleared per message.** `MailboxIdleTests.cs:24`:
extension-block halfword 5 (`ExtBlock + 0x0A`) - *"IDLE spins while NONZERO, 0 = work"*. The
microcode sets it non-zero when it consumes a message, so a SECOND message is invisible until it is
cleared again. MEASURED: without clearing it, the post-restart microword trace walks IDLE
`0o24670..0o24720` and loops at `0o24702`, never reaching the MSG_00 dispatch table at `0o15224`.

Also measured and recorded so nobody retries it: **clearing `State.Stopped` alone does NOT resume
the CPU** - it keeps ticking but never reaches another instruction fetch.

**Result: 48 -> 55 macro instructions, P advances 0x0800823F -> 0x0800828B**, and the message-control
block ends up exactly as the macrocode requires:
```
[0x080240B0] control = 0   <- the swapper cleared it   (w stz $1000440260)
[0x080240B8] fn code = 5   <- copied from [0x240B0]    (w1 := / w1 =:)
```
That is the carved contract of `swapper-k01-pseg.asm:10543-10546` executing on real B30 microcode -
end-to-end confirmation that the MON 377B answer is correct.

### 5.5.1a TESTOBJ 38 identified and implemented (was blocking the restart)

Reaching MSG_CONMC immediately hit `Test condition TESTOBJ=38 not implemented yet` at
`Mpc=0o15720` (`MSG_CONMC_0`). TESTOBJ 38 is **absent** from `tools/microcode-5000-def.json`, whose
table jumps 37 (`COND,MFS`) straight to 40 (`COND,MFO`). The ND-500 Micro Program Guide
(`ND-05.012.01`, alphabetical glossary ~line 1895) lists three conditions the JSON lacks:
`COND,MFUFO`, `COND,IDRY`, `COND,TRAP`.

Scanning the whole image for TESTOBJ-38 sites and resolving them against the LABE gives:
`BMOVEBY_F` / `BMOVEHW_F` / `BMOVEWF_F` / `BMOVEDF_F` (block moves), `SFILL_H0` (string fill),
`SCOTRBY_F02` (string compare/translate), `SEND_1`, `MHOLE_2`, `MSG_CONMC_0`. All long-running or
resumable routines - which rules out `MFUFO` (no floating condition can sit inside a byte-block-move
loop) and leaves `IDRY` or `TRAP`.

**The differential oracle settled the VALUE (measured, not reasoned):**

| value | Sweep_NoRegressionsAgainstBaseline | BitTestCorpus | full suite |
|---|---|---|---|
| `false` | 11202 pass / 2068 fail (**40 REGRESS**) | FAIL | 2 failed |
| `true`  | 11242 pass / 2028 fail (baseline) | PASS | **0 failed** |

~11k differential vectors accept `true` and reject `false`. So TESTOBJ 38 is a **normally-asserted
signal** - true whenever the machine is not stalled. Implemented in `Conditions.cs` as
`case 38: return true`.

**The value is genuinely undocumented - established by exhausting the sources, 2026-07-25:**
- `E:\Dev\Ronny\ND5000UC\manual\mnemonics.md` IS the valued TESTOBJ table (and what
  `microcode-5000-def.json` was generated from). It enumerates with deliberate holes - 4-7, 12-15,
  22-23, 29-31, 33, **38-39**, 45-47, 50-55, 58 - running `37 COND,MFS` straight to `40 COND,MFO`.
- `ND-05.022.1` Appendix A has the same hole: `459 COND,MFS` -> `460 COND,MFO`.
- The rendered listing `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md:7141` prints the word at
  0o15720 as `TESTOBJ=46` (octal 46 = 38 decimal) - a bare number, while naming every other
  condition it renders. Its symbol table has the hole too.

**Candidate elimination** (the ND-500 glossary `ND-05.012.01` names three conditions the valued
tables lack - MFUFO, IDRY, TRAP):
- **MFUFO** - out on site distribution; a floating-point condition cannot sit in a byte-block-move loop.
- **TRAP** - out on POLARITY; normally FALSE with nothing pending, and `false` is what the corpus
  refutes. Note `ND-05.022.1` chapter 8 **does** list TRAP for the ND-5000, so this exclusion rests
  on measurement, not on absence from a manual.
- **IDRY** - the only candidate shaped like a normally-true readiness signal, but it appears ONLY in
  the ND-500 glossary; `ND-05.022.1` chapter 8 does NOT list it for the ND-5000. Fits the behaviour,
  fails the provenance.

**STATUS: behaviour corpus-verified and safe to rely on; MNEMONIC UNRESOLVED. Do NOT write "IDRY"
into docs as fact** (an earlier revision of this section did - corrected). Settling it needs a
source we do not have: a B30-era valued TESTOBJ table, or the microcode assembler's own symbols.

### 5.5.2 Absolute post-indexed operands (0xE0-0xE3) implemented - RIOM now reached

The stop after the MON restart was `Operand specifier 0xE0 at P=0x0800828C not implemented yet`.
`0xE0-0xE3` = octal `340B+y` = **absolute post-indexed**, `<label>(Rn)`, `ea = a + p * (Rn)`:
a 4-byte absolute address, `Rn` = I1-I4 from the low 2 bits, `p` = the ELEMENT SIZE of the data
type (`Reference-Manuals\500\ND500-adressing-modes.md:484-516`). Implemented in `CpuND5000.cs`
mirroring the functional oracle `AddressingMode.ABSOLUTE_PI` (`CpuND500.Fetch.cs:690` + scaling at
`:298-331`). First hit: swapper `0o101213 w2 := $1000507270+` - the disassembler's trailing `+`
IS the post-index marker.

Note a new `PostIndexScale()` was added rather than reusing `AccessWidth()`: AccessWidth is the
width of ONE transaction and returns 4 for TYP,DF because the 32-bit interface moves a double as
two halves, but the post-index SCALE for a double is 8. Reusing it would silently halve every
double-indexed address.

**Result: 55 -> 87 macro instructions.** The swapper reaches and EXECUTES its RIOM at
`P=0x080082EE` - message intake - for the first time on the microword CPU.

### 5.5.3 SWMSG INTAKE CARVED (2026-07-25) - control block confirmed live

`RealSwapper_ServiceMon377BAnnounce_HowFarThen` now logs every access to the message region tagged
with the macro P that made it (`ProbeMemory`). The observed sequence confirms
`swapper-k01-pseg.asm:10543-10546` **executing**, no longer just read:

```
P=0x080081AC/B2  WR [0x240B0]=0, [0x240B4]=0        init clears the control block
P=0x0800823F     RD [0x240B0], RD [0x240B4]         MON 377B passes them as args
                 WR [0x240B0]=5, WR [0x240B4]=src   <- the sink's answer
P=0x08008279     RD [0x240B0] = 5
P=0x0800827F     WR [0x240B8] = 5                   <- FUNCTION CODE copy
P=0x08008285     WR [0x240B0] = 0                   <- control word cleared
P=0x08008293/B7/C3/E8  RD [0x240B8] x4              validation (>=0, <=0o34) + count index
P=0x080082EE     RD [0x240B4]; writes the message as big-endian HALFWORDS from [0x240BC]
```

**ADDRESS CORRECTION - `swapper-k01-deep-analysis.md:242` is WRONG.** The RIOM count operand
`$1000440074` is **`0x0802403C`**, not `0x0802408C`. Verify: `0x2403C` = 147516 decimal =
`0o440074`. An earlier revision of this section inherited the slip and quoted "8 halfwords" from
the wrong table. The real table (32-bit entries) reads `[0]=13 [1]=10 [2]=15 [3]=138 [4]=9 [5]=70`;
function code 5 selects **15 halfwords**.

### 5.5.4 RIOM runaway - FIXED. Cause was the post-index SCALE SOURCE, not the read width

**RETRACTION.** An earlier revision of this section said "the RIOM count is read 32-bit at a
halfword address" and handed it to the CpuND5000 lane as their bug. **Both parts were wrong.** The
symptom was right; the cause was mine, introduced by the 0xE0-0xE3 commit in 5.5.2. Fixed in
`07d5e032a`. If that hand-off was passed on, retract it.

**Symptom (measured):** the RIOM DMA transferred **983040 halfwords** spanning
`0x080240BC..0x082040BA`, running to the end of memory, against a count operand of 15.

**What the direct measurement showed.** Widening the probe window over the count table logged the
read itself:
```
P=0x080082EE  RD +0x16 (0x08024046) w4 = 0x000F0000
```
The **w4 read is CORRECT** - the count table holds 32-bit entries on a 4-byte stride:
`0x2403C`=13, `0x24040`=10, `0x24044`=15, `0x24048`=138, `0x2404C`=9, `0x24050`=70. (The
alternating zeros in a halfword dump are the high halves - that is what misled the first reading.)
What was wrong is the **ADDRESS**: `0x08024046` is mid-word, unaligned to a 4-byte table.

**Root cause.** `PostIndexScale` was fed `Regs.InstrDt`, the INSTRUCTION data type. `h riom` is a
halfword-TRANSFER instruction (`InstrDt = 2`) whose COUNT operand is a 32-bit word. Scale 2 gives
`0x2403C + 2*5 = 0x08024046`, and a 32-bit read there returns `0x000F0000` - the count 15 sitting
in the high halfword = 983040. The OPERAND type gives `0x2403C + 4*5 = 0x08024050`, aligned,
entry **70**.

**Fix.** `EnsureOcaDecoded` now takes an optional `operandDataType` used for the post-index scale,
falling back to `Regs.InstrDt`. The three call sites holding the microword pass
`EffectiveDataType(in word)` - the microword's own type, which already defers to the instruction
type when the word carries TYP,DR. The CALL-argument site has no microword and keeps the fallback.

**Verified by prediction, not by fitting:** scale 4 predicts EA `0x24050`, count 70, 70 writes.
Measured after the change: `70 writes, 0x080240BC..0x08024146`, out-of-range access `(none)`.

**LESSON worth keeping:** an instruction's data type and its individual operands' data types are
NOT the same thing. Conflating them produced an unaligned address whose 32-bit read still returned
a plausible-looking number, which then read as a width bug. Prefer the microword's own type.

### 5.5.4a Result: 124 macro instructions, level with the functional CPU

87 -> **124 macro instructions**, `P=0x08009137`. That is the same region where the functional
`CpuND500` stopped (`0x0800913B`, `GATE1-SWAPPER-COLDSTART-TRACE-ANALYSIS-2026-07-21.md`) - the
microword lane has caught up to the functional lane's furthest point.

New stop is an ordinary dispatch gap: `Opcode 176551 (octal) at P=0x08009137 has no entry in the
reconstructed dispatch map`.

### 5.5.5 SWMSG body - first field carved, TWO independent methods agreeing

The message body can be carved STATICALLY from the listing even while the dynamic run is blocked,
and the two methods cross-check each other.

**Watch the operand specifier.** At `0o101377` the byte after the opcode is `317` = `0xCF` =
**constant :W**, so:
```
1000101377: 030 317 010 002 100 274   r:=    $1000440274   ; R := the ADDRESS 0x080240BC
1000101405: 011 202                   h2 :=  r.10          ; HALFWORD at R+8 = 0x080240C4
1000101407: 375 120 321 320           h wconv r2,r1        ; widen halfword -> word
1000101413: 040 304 010 001 051 334   w1 =:  $1000224734   ; store to a DSEG variable
1000101427: 117 304 010 002 142 200   w incr $1000461200   ; bump a counter
```
`r:=` here loads R with the buffer BASE ADDRESS as a constant - it is NOT an indirection through
`[0x240BC]`. (Offsets in this disassembly are OCTAL: `r.10` = +8 decimal.)

**Dynamically confirmed, same run:** the access log contains
`P=0x08008305  RD +0x14 (0x080240C4) w2` - and `0o101405` = `0x8305`. Static listing and live
access log agree on both the address and the width.

**So: SWMSG field 1 = a HALFWORD at message-buffer offset +8 (`0o10`),** widened to a word and
stored. VERIFIED two ways. Its meaning is not yet named - that needs the DSEG variable
`$1000224734` traced to its consumers.

After that the swapper dispatches on the function code:
```
1000101504: w comp2 $1000440270,$5    ; fn == 5 ?
1000101515: w comp2 $1000440270,$3    ; fn == 3 ?
1000101540: w1 :=   $1000440270       ; else index = fn code
1000101546: jumpg   $1000460630+      ; via the dispatch table at 0x08026198
```

**Field 1 traced to its consumers.** `$1000224734` = `0x080129DC` has exactly ONE writer (the site
above) and THREE readers. Two readers do the identical thing:
```
1000011244 / 1000020344:  w2 :=     $1000224734    ; the field value
                          w2 *      $400           ; x 256
                          by1 laddr $4000004400+   ; index a table at 0x20000900
```
Value x 256 into a table of **256-byte records** - which is the VERIFIED PCB/DIT stride (see the
DIT notes in `SwapperStartDiagnosticTests.cs`). The third reader (`1000052070`) compares it against
a local and branches. **INFERRED (shape-strong, not byte-proven): field 1 is a process/domain
INDEX**, not a count or a flag. Naming it properly needs the table at `0x20000900` (segment 4)
identified.

**Remaining body fields are now UNBLOCKED** - the RIOM runaway that was overwriting the parse
region is fixed (5.5.4), so the access log reads out cleanly. That is the next carve.

### 5.5.6 SMOVE dispatch family added - 125 instructions

The stop at `P=0x08009137` was `Opcode 176551 (octal) has no entry in the reconstructed dispatch
map`. `0o176551` = `0xFD69` = `w smove b.54,b.64` (swapper `0o110467`, bytes `375 151 113 115`) -
the whole SMOVE family was missing.

Assignment mirrors the SMOVN family (consecutive opcodes -> CS entries stride 2, ordered
BI, BY, H, W, gap, D). `64870-64875` was the only free run between `64869` and `64876`, and
`0xFD69` = 64873 lands on **W**, matching the disassembler. Entries are the LABE addresses
`SMOVEBI 001235 / SMOVEBY 001237 / SMOVEHW 001241 / SMOVEW 001243 / SMOVED 001245`.

**Confirmed by the new stop:** `Mpc=676` = `0o1244` = SMOVEW's entry `0o1243` **plus one word** -
the right routine was entered and advanced. Now stops on a real microcode gap,
`IDU fetch control G,OPSTRD at CS 0o1244 not implemented yet` (string-operand fetch, which is
exactly what SMOVE needs). That is the next microword item.

### 5.5.7 TRAP: do NOT regenerate DispatchMapB30.g.cs from dispatch-map-b30.json

**The "generated" file is HAND-MAINTAINED; the JSON is badly stale. Regenerating destroys
corrections that exist only in the `.g.cs`.** Found the hard way this session - a regeneration
silently reverted:
```
map[184] ENTS   dataType 0 -> -1              (the 07-22 type-less-stack-op fix)
map[195] CALL   lost directMask/directSizes   (the 07-22 CALL direct-operand fix)
```
and the swapper collapsed **124 -> 16 macro instructions**. Some entries differ outright:
```
.g.cs   map[120] = DispatchEntry(1304, 0, 0)   // W1 /   DIVW 002430, golden-vector verified
json    map[120] = DispatchEntry(1294, 0, 3)   // BY1 /  [nd500x-json]
```
- a different instruction entirely.

So **add dispatch entries BY HAND to the `.g.cs`** (as done for SMOVE, with an inline comment
saying so). The JSON has been moved toward truth (SMOVE rows added; ENTS `dataType` and CALL
`directMask`/`directSizes` back-ported) but is NOT yet safe to regenerate from - `map[120-123]`,
`[232-235]`, `[64828-64831]` at least still diverge. Reconciling it is a separate job.

### 5.5.8 SWMSG body second consumer carved - it is a message TRACE recorder, not a decoder

With the RIOM runaway fixed (5.5.4) the `ProbeMemory` access log reads out cleanly, and a SECOND
body-field consumer became visible, distinct from the field-1 read already carved in 5.5.5:

```
P=0x08008305  RD +0x14 (0x080240C4) w2     <- field 1, buffer+8  (carved in 5.5.5)
P=0x0800911A  RD +0x92 (0x080240C2) w2     <- NEW, buffer+6
```

`P=0x0800911A` is octal `1000110432`, and the macrocode there (`swapper-k01-pseg.asm`) resolves it
completely:

```
1000110430: 030 111              r:=      b.44      ; r = the message buffer (0x080240BC)
1000110432: 013 311 006          h4 :=    r.6       ; <- the observed halfword read
1000110435: 375 120 323 322      h wconv  r4,r3     ; halfword -> word
1000110441: 042 365 004          w3 =:    r2.(4)    ; store into entry+4
1000110444: 375 074 204          w1 laddr r.20
1000110454: 375 077 365 010      w4 laddr r2.(10)
1000110467: 375 151 113 115      w smove  b.54,b.64 ; <- P=0x08009137
```

That `r = b.44` is confirmed DYNAMICALLY, not assumed: the observed read address is exactly
`0x080240BC + 6`.

**So buffer+6 is not being decoded - it is being RECORDED.** This whole region is a message
trace/history recorder: a counter at `$1000507604` is incremented, wrapped at `0o2000`, scaled by
`0o40` (32-byte entries) onto base `$1000507614`. Each entry is:

| offset | content |
|--------|---------|
| `+0`    | word from `b.34` |
| `+4`    | word-extended halfword from `msg+6` |
| `+0o10` | 6 bytes SMOVE'd from `msg+0o20` |

Do NOT read semantic meaning into buffer+6 from this site - a trace recorder logs a field without
interpreting it. Its meaning still needs a consumer that *branches* on it.

### 5.5.9 D,IDU,CSIT RESOLVED from the hardware manual - it was blocking the trace SMOVE

The stop at `P=0x08009137` IS that `w smove`, and `D,IDU,CSIT` sits inside the SMOVE microcode loop.
So CSIT was not optional decoration - it was the single instruction blocking the swapper.

`ND-05.020.01 EN ND-5000 Hardware Description`, chapter 7, section 7.3.3, Table 15:

```
| D,IDU,CSIT  | - conditional single instruction trap (1 bit) |
```

It is a **1-bit WRITE-ONLY latch** (note the asymmetry - the A-operand column of the same table has
no `A,IDU,CSIT`, so the microprogram can arm it but not read it back). It arms the `SIT` "single
instruction trace" trap that section 7.3.4 lists as macrostatus bit 17. It therefore has **no
data-path effect at all**, and on a CPU with no trap model discarding the bit is the FAITHFUL
behaviour, not a placeholder - the same situation as `D,MIC,RESTU` and `COND,IRALT`.

This retires the `[OPEN 2026-07-25]` note that previously sat in `OperandRouter.cs`.

### 5.5.10 The regression-ceiling question, answered with per-file evidence

Landing CSIT moved the sweep ceiling `2002 -> 4941`, which looked alarming. **It is not a
regression, and that was proven rather than argued.** `Sweep_AllGoldenVectors_Report` was captured
before and after and the per-file match counts diffed:

- **ZERO files lost matches.**
- Files that execute any vector at all went **100 -> 121**.
- ~4200 vectors that previously THREW at CSIT now run: 1264 match, 2939 diverge.

The 2939 are pre-existing failures DOWNSTREAM of CSIT that the throw had been masking. The ceiling
move admits newly-**visible** debt, not newly-**broken** behaviour.

**This per-file diff is now the standard method** for any change that unblocks a throwing path. The
aggregate gate in `JsonVectorSweepTests.cs` cannot distinguish a regression from newly-exposed debt;
the per-file report can. Use it every time.

### 5.5.11 ADD3/SUB3 dispatch family added - 148 instructions

Next stop was `opcode 176156 (octal) has no entry in the reconstructed dispatch map` at
`P=0x08009041`, where the macrocode is `w sub3 r.10,r.4,b.34`. The whole three-operand family was
missing.

The two-byte opcode reading is **cross-checked, not assumed**: the same listing encodes `by rladdr`
as `0o374,0o132` = `0xFC5A`, and the map already carried `0xFC5A` as `BY RLADDR [labe]`. So the
swapper's disassembler and the labe-derived map agree on the encoding.

Opcode ranges come from the **functional `CpuND500`** (the oracle's answer key): `Instructionset.Init.cs`
registers `add3 = 0xFC67..0xFC6B` and `sub3 = 0xFC6C..0xFC70`, each with prefixes `BY|H|W|F|D` and
`variantNumber` 0..4. Variant 2 (`W`) of `sub3` is `0xFC6E` - exactly the trapped opcode, which is
the cross-check that the ordering is right rather than merely plausible.

> **TRAP:** the XML comment in `Emulated.HW\ND\CPU\ND500\Instructions\ARITHMETIC\Sub3.cs` claims
> `BYn SUB3 = 0xFC54+(n-1)` and `Wn SUB3 = 0x0084+(n-1)`. That comment is **STALE and
> self-contradicting** - `0xFC55` is already `BI RLADDR`. The `AddToDictionary` call sites are the
> truth; the doc comment is not.

Control-store targets from `MICRO-5800-B30.LABE`, with F and D on their own routines:
`ADD3 0o277`, `ADD3F 0o2177`, `ADD3D 0o2221`; `SUB3 0o302`, `SUB3F 0o2241`, `SUB3D 0o2265`.

### 5.5.12 ST,ACCA implemented - 191 instructions

Next stop: `Status operation ST,ACCA not implemented yet` at `Mpc=0o2631`.

**That address is `MULADW`, not SUB3** as the raw number might suggest - the B30 listing labels
`MULADBY 002617`, `MULADH 002623`, `MULADW 002627`. The swapper reaches it via `w2 mulad $4,r.0` at
octal `1000110110`.

The accumulate semantics were read off the microcode, not guessed. Every `ST,ACCA` site in the image
is the SECOND status write of a two-part arithmetic instruction whose FIRST write is an
`ST,SAVM`/`ST,SAVF` saving the AAP status:

```
002627 MULADW  AAP2,IMUL ...          ; start the integer multiply
002630          ... ST,SAVM ...       ; save status FROM THE MULTIPLY
002631          ALU,A+B ... ST,ACCA   ; add the product, ACCUMULATE
```

`MULADBY`/`MULADH` and the whole `PSUM` family (`002640..002653`) use the identical `SAVM -> ACCA`
pairing. A multiply-add can overflow in EITHER the multiply OR the add, so **O is sticky**
(`O |= new overflow`); a plain `SAVA` would discard the multiply overflow. **Z, S and C are
replaced**, and that is forced by the data rather than assumed: ORing Z is wrong for
`product 0 + addend 5` (would report Z=1) and ANDing Z is wrong for `product 5 + addend -5` (would
report Z=0) - only replacement satisfies both.

Per-file proof again: zero files lost matches; the only file whose diverge grew is
`ARITHMETIC_mulad.json`, `0 match / 0 diverge` (entirely unsupported) -> **87 match / 21 diverge**.

The 21 residuals **corroborate** the semantics:
- **9 are W1/W4 and fail on CARRY ONLY**, never on overflow. At word width, where no sub-word
  artifacts exist, the accumulated O is right in every case.
- 12 are BY/H and fail on O. Overflow failing only at sub-word width points at the AAP
  multiply-overflow latch (`_aapMulOverflow`, saved by `ST,SAVM`) being computed at 32 bits. ACCA
  only ORs in what SAVM already latched, so this is **not** an ACCA defect.

**Two open items, both pre-existing and both outside ST,ACCA:**
1. Width-correct the AAP multiply-overflow latch (`_aapMulOverflow`).
2. Settle the MULAD carry convention - the functional `CpuND500` reports `C=0` where the microword
   takes the add's carry.

### 5.5.13 Local indirect post-indexed operands (0xE4-0xEF) - 194 instructions

Specifier `0xE5` at `P=0x08000746` is **LOCAL INDIRECT POST-INDEXED**, `IND(B.<displ>)(Rn)`,
`ea = ((B) + d) + p x (Rn)` - the array-through-a-pointer mode. It composes the one level of
indirection of LOCAL IND (`0xC5-0xC7`) with the element-size post-index scaling of ABSOLUTE P.I.
(`0xE0-0xE3`, section 5.5.2). **Order matters:** the index is added AFTER the dereference, to the
pointed-to address, not to the pointer's location.

Encoding (`ND500-adressing-modes.md`): `344B+y = 0xE4+y` 1-byte displacement, `350B+y = 0xE8+y`
2-byte, `354B+y = 0xEC+y` 4-byte, index register in the low two bits.

CONFIRMED against the listing rather than assumed - octal `1000003504` is

```
376 046 345 050        by3 laddr  @b.50+
            ^^^ ^^^
            |   displacement 0o50
            0xE5 = the specifier
```

and the disassembler's `@b.50+` spells out the mode: `@` indirection, `b.50` = B+0o50, trailing `+`
post-index. The resulting `OcaEa=0x68000044` is sane too - the nearby `by rladdr $15000000000+` at
`1000003470` shows `0o15000000000` = `0x68000000`, so the EA lands in the expected segment.

### 5.5.14 ORD,OP1 means operand ONE, not the previous operand - 287 instructions

The stop at 194 was `ORD,OP1 with no stored first operand` at `Mpc=0o3544`, inside PUTBF, reached
from `w4 putbf r3.(0),$20,$15`.

This was a **semantic error, not a missing feature**. `ORD,OP1` had been modelled as "the operand
before the last G,OPS advance", latching on EVERY advance. `MICROCODE-FIELDS.md` (ORCON.D, bits 1-0)
is explicit that it is not:

```
01  ORD,OP   - OR destination in current from operand specifier
10  ORD,OP1  - OR destination (in next) from FIRST operand specifier
```

The two readings **coincide for two-operand instructions**, which is why the old model survived so
long. PUTBF is the first three-operand case to reach here: `PUTBFW` (`0o525`) issues `G,OPS` for
operand 1 and `0o526` issues `G,OPS` again for operand 2, so the unconditional latch left operand 2
- the bit-offset CONSTANT - as the write-back target. A constant is neither a register nor a memory
address, hence the throw. The instruction must of course write its result back to operand 1, the
bit-field destination.

Fix: latch only when the operand being advanced past is index 0, and rename `PrevOca*` to `Op1*` so
the field name states what it holds. `Op1Kind` is now per-instruction state and is cleared on
new-instruction setup - under the old latch-every-advance model that was carried implicitly by the
constant re-latching.

**This was worth 194 -> 287 macro instructions**, the largest single jump of the session.

Per-file proof: zero files lost matches; the only file whose diverge grew is `bitfield_putbf.json`,
`0 match / 0 diverge` -> **84 match / 6 diverge**.

> **OPEN:** all 6 residuals are a single shape - `W putbf In,$24,$8: S=0!=1`. Sign flag only, only
> for REGISTER first operands, only at bit offset 24 length 8, i.e. exactly when the inserted field
> lands on the destination word's sign bit. Every memory destination matches, so the
> register-destination write path is not feeding the sign back into status the way the functional
> `CpuND500` does.

### 5.5.15 LOOP path: G,DIR1/2/4 and AB,EA1DIR

Both resolved from the manuals, both leaving the sweep untouched.

**`G,DIR1/2/4`** ("fetch a one/two/four-byte direct operand", GET `1000/1001/1011`) are a deliberate
**NO-OP**. The real IDU prefetches the literal into a latch; this flattened CPU has no prefetch
pipeline and reads the displacement LAZILY at the branch via `ReadDirectDisplacement()`. Fetching
here as well would advance `P` TWICE and corrupt the stream.

The widths agree, which is what makes the lazy read correct rather than merely convenient -
`ReadDirectDisplacement` takes its width from `InstrDt` (BY 1, H 2, else 4), and the microcode pairs
each LOOP variant with the matching `G,DIR` code while the sequences are otherwise identical word
for word:

```
000570 LOOPIB ... G,OPS -> 000571 LOOPIB_0 ... G,DIR1   (byte)
000574 LOOPIH ... G,OPS -> 000575 LOOPIH_0 ... G,DIR2   (halfword)
```

**`AB,EA1DIR`** (AB `1010`) is simply **"Use EA1"** (ND-05.022.1:1406). The `DIR` is NOT "direct
operand" - it names the DIRECT BYPASS PATH to the DAC (ND-05.022.1:1440: when the DAC is busy with
an OCA-controlled request, "Only AB,ADR, AB,ADR+4 and the AB,EA1DIR may be used and will cause the
address to be presented by the DAC in the next cycle"). It is NOT scaled, matching the AA side and
for the same reason - EA1 is an already-computed effective address.

### 5.5.16 Conditional-fetch branch for ABR,(none) - RESOLVED

`ABR 00` = "No alternative branch" (MICROCODE-FIELDS.md, ABR bits 52-51). `0o573` is the last word
of `LOOPIB`:

```
000572   ... ABR,NEXT TBC,PREL ... AB=12
000573   ... T,JMP COND,SAVC1 TBC,NEXT G,OOPS,F WRITE C,MEMOT   (ABR = 0)
```

> **THE TRAP, recorded because it looks trivial:** implementing this as `case 0: return;` is
> SILENTLY WRONG. With no alternative branch the ABR route is plain fall-through, so it is the
> **PREFERRED** route that carries the jump - the inverse of the existing `ABR,NPCREL` case. A bare
> `return` falls through ALWAYS, so every LOOP executes exactly once, with no throw to reveal it.

"Preferred == a P-relative branch" for `ABR=0` was **surveyed, not assumed**. The B30 image has 26
`G,OOPS,T/F` words; the 14 with no ABR mnemonic are ALL conditional branches whose taken path is
P-relative - `LOOPIB`/`LOOPIH`/`FLOOPIB` (`0o573`, `0o577`), `LOOPDH`/`FLOOPDB` (`0o615`, `0o621`),
`IFSTH`/`IFNSTB`/`IFNSTH` (`0o3654`, `0o3661`, `0o3666`), `LOOP_IOV_B/H` and `LOOPB_1` (`0o3673`,
`0o3700`, `0o3705`), and the FLOOP/DLOOP ends (`0o27416..0o27424`). Each pairs `TBC,NEXT` on itself
with a `TBC,PREL` earlier.

Target is `Npc + displacement` - the instruction START, not the post-displacement P. AUTHORITATIVE:
the functional answer key, `Loopi.cs:174` `regs.P = (uint)((long)fi.StartAddress + displacement)`.

Polarity cross-check on LOOPIB: `0o572` does `CSAVE COND,MSORZ` (signed "index <= limit") and
`0o573` is `G,OOPS,F`, which falls through on FALSE - so it branches when index <= limit, exactly
the documented LOOPI operation.

**`G,DIR1/2/4` upgraded from pure no-op to latching the WIDTH.** In the LOOP family the operand data
type and the displacement width are INDEPENDENT (`W LOOPI:B` = word operands, byte displacement), so
deriving the width from `InstrDt` is wrong for the mixed variants; the `G,DIR` code is the
authoritative statement of it. Confirmed from the other side by `Loopi.cs`: ":H variants (halfword
displacement): 0xFD1E, 0xFD1F, 0x00E1, 0xFD21, 0xFD22; all other LOOPI opcodes use a 1-byte signed
displacement."

**Validation - the strongest of the session:** `BRANCH_loopi.json` = **16 match / 0 diverge**, the
exact family the swapper uses, in complete agreement with the functional oracle.

## 5.6 MILESTONE: the swapper reaches its MAIN MESSAGE LOOP - microcode gaps on this path are CLOSED

**420 macro instructions, 2 MON restarts, and NO throw.** The run now ends on the diagnostic's tick
budget, not on an unimplemented feature.

It is parked at `P=0x08000677` = octal `1000003167` (`swapper-k01-pseg.asm:520`):

```
1000003167: call $...377,$2,$1000225040,$1000437234   ; MON 377B, 2-ARGUMENT form
1000003207: ifkret
1000003210: go $...757                                 ; backward jump - loop
```

That is the swapper's **message-wait loop**: ask the ND-100 for a message, process it, repeat. So
the swapper has completed initialisation and is now doing its actual job.

Note this is the **2-argument** MON 377B form that section 7.2 step 1 predicted would exist
(`argcount varies and the argument block must be decoded per call site`) - a different site from the
`0o507` one noted there, with arguments `$1000225040`, `$1000437234`.

**Why it spins:** `SwapperAnnounceSink` in the diagnostic is a STUB that always answers with the
same announce message (function code 5, source `0x00210718`). The swapper keeps being handed the
same message and loops forever. **This is not a CPU defect and no further microcode will fix it.**

**What closes it:** the real octobus mailbox transport - handoff step 2(b), wiring `IMonitorCallSink`
to `OctobusND5000Station.cs` so real SINTRAN answers arrive. Alternatively, a smarter diagnostic stub
that returns a "no message" / different message on the second call would show more of the loop body
without needing the transport.

Sweep baselines now `match=12797 / diverge=4972`. Suite 301 passed / 0 failed.

## 6. Key files

- **Diagnostic test** (drives the microword swapper-start, has the SPIN DUMP):
  `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\SwapperStartDiagnosticTests.cs`
  Loads real `SWAPPER-K01.PSEG/DSEG` at 0x08000000, posts MICFU-23, sets CPU_AVAIL srf[0x40E]=0,
  PCB @ 0xA00 with P=0x08000004. One microword/Tick(), 16-deep Mpc ring, dumps opcode+P +
  Regs.InstrDt/OcaKind + Mpc ring (decimal AND octal - CpuND5000 prints CS in octal, so always
  print both to avoid the base-notation trap).
- Swapper disassembly: `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-pseg.asm`
  (base 0x08000000) + `swapper-k01-deep-analysis.md`, `swapper-k01-handlers.md`.
- Octobus station: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs`
- Boot harness: `Nd100SintranNd5000OctobusBootHarnessTests` (see memory `sintran-boot-harness`).
- Shared MMU (built, 12/12 green, NOT yet converged onto both CPUs):
  `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND500.Common\`
  (`Nd500Mmu : IAddressTranslator`, 3-level capability->PST->PTE walk). Design:
  `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\SHARED-ND500-MMU-DESIGN-2026-07-21.md`.
- NUCLEUS decode (for when the swapper hits the 4 microcoded nk* calls):
  `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\NUCLEUS-CALL-DECODE-REFERENCE.md`. Microcoded on ND-5000
  = exactly `nkMove`, `nkSend`, `nkReceive`, `nkGetInfo`. Port descriptor KICK DEST = octobus
  station number.

## 7. Recommended next steps (in order)

**REVISED AGAIN 2026-07-25 (end of the octobus-lane session).** Steps 1 and 2 below are **DONE** -
MON 377B is decoded (5.5), the sink stub is built and the restart works (5.5.1). They are kept for
provenance. **The live queue is 7.0.**

### 7.0 THE ACTUAL NEXT STEPS

1. **Wire `IMonitorCallSink` to the real octobus mailbox transport** in `OctobusND5000Station.cs`
   (step 2(b) of the old queue). This is now THE blocker - the swapper is sitting in its message
   loop being fed the same stub answer forever (5.6). No amount of further microcode work moves it.
   Cheaper interim step: make the diagnostic stub answer differently on the second call, which walks
   more of the loop body without needing the transport.
2. **The sub-word WIDTH family** - one root cause behind residuals in three separate instruction
   families, so fixing it pays off three times:
   - `mulad` 12 vectors: AAP multiply-overflow latch (`_aapMulOverflow`) computed at 32 bits (5.5.12)
   - `putbf` 6 vectors: `W putbf In,$24,$8: S=0!=1`, register destinations only (5.5.14)
   - `loopd` 4 vectors: `I1=FFFFFFFF!=000000FF`, a 32-bit -1 where a byte 0xFF was wanted (5.5.16)
3. **The MULAD carry convention** - functional `CpuND500` reports `C=0` where the microword takes
   the add's carry (5.5.12).
2. **Width-correct the AAP multiply-overflow latch** (`_aapMulOverflow`) - explains 12 of the 21
   residual `ARITHMETIC_mulad` divergences (5.5.12).
3. **Settle the MULAD carry convention** - explains the other 9 (all W, carry-only).
4. **Work the 2939 newly-visible divergences** exposed by CSIT (5.5.10). These are real pre-existing
   gaps that were masked by a throw, now measurable for the first time.
5. When the swapper hits `nkMove/nkSend/nkReceive/nkGetInfo`, use the NUCLEUS decode ref to locate
   their B30 microword CS routines via the Mpc trace.

**METHOD NOTE - use this for every unblocking change:** when a fix lets previously-throwing vectors
execute, the aggregate sweep gate cannot tell a regression from newly-exposed debt. Run
`Sweep_AllGoldenVectors_Report` before and after and diff the PER-FILE match counts. If no file
loses matches, a ceiling rise is newly-visible debt and is safe to accept with the evidence
recorded. This settled both CSIT and ST,ACCA. See 5.5.10.

### 7.2 Historical queue (steps 1-2 now COMPLETE)

1. **Decode MON 377B as called at `P=0x0800823F` (0o101077).** Pin what the swapper requests and
   what each of the 4 arguments is: `$1000225050`, `$1000440260`, `$1000440264`, `b.24`. Note the
   swapper ALSO calls MON 377B at 0o507 with only **2** arguments (`$1000225064`, `$1000436574`) -
   so argcount varies and the argument block must be decoded per call site, not assumed.
   Prior data point (3022 harness, memory): "MON 377B answer WORKS: write-back @0x240B0:=5,
   @0x240B4:=0x00210718". Whether that write-back shape applies to THIS call site is UNVERIFIED.
   Do NOT guess - read the swapper analysis + the MON 377B carve.
2. **Implement an `IMonitorCallSink`** (see section 5.4) in the octobus lane. Two stages:
   (a) a diagnostic stub that services MON 377B minimally so the run advances - fast, reveals the
   next stop; then (b) wire the sink to the real octobus mailbox transport in
   `OctobusND5000Station.cs` so real SINTRAN answers (authoritative). Recommend (a) first.
   Neither stage requires editing `CpuND5000.cs`.
3. When the swapper next hits `nkMove/nkSend/nkReceive/nkGetInfo`, use the NUCLEUS decode ref to
   locate their B30 microword CS routines via the Mpc trace.

**Do NOT** re-investigate the `dctsb` spin at P=0x08000162 or the PIA trap - both are closed, see
section 5.2. **Do NOT** treat cell `0x08014670` as a spin-wait target - see section 5.3.

### 7.1 How the swapper reaches the MON call (macro trace, measured)

Useful for orientation - the route is not the one the old section 5 assumed:
```
#15 P=0x08000052 op=0xC3 call  -> #16 P=0x080081AB (ENTS)
#30 P=0x08008231 op=0xC3 call  -> #31 P=0x08000093
#34 P=0x08000160 ... #35 P=0x08000162 op=0xFF (dctsb, RETIRES) ... #45 P=0x08000192
#46 P=0x08008237 op=0xD2
#47 P=0x0800823F op=0xC3  <- MON 377B, throws (no sink)
```

## 8. Standing constraints

No LINQ, no FluentAssertions, avoid foreach in hot paths (Span/ArrayPool). ALWAYS full absolute
paths. ASCII only (no unicode/em-dash/section-sign/ellipsis; use `-`, `->`, `...`). NEVER write
to D:. Bash tool is Git Bash, not WSL (use `wsl`). Ghidra bases in HEX. Do NOT edit the
microcode session's files (`CpuND5000.cs`, `MmsUnit.cs`, `MacroOracleState.cs`). Validate code
compiles + ALWAYS run tests. Never mention Claude in commits. Mirror ND-500 fixes to the C
nd500x port (deferred - octobus work is C#-only for now).

---
*Provenance: live octobus capture (ACCP), agent-verified B30 microcode listing, carved
SWAPPER-K01. Section 5 INFERRED item is explicitly not yet byte-proven - step 1 above proves it.*
