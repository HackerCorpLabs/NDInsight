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

**Why it spins - CORRECTED 2026-07-25 (later the same day). The explanation below this line is the
measured one; the paragraph it replaces was wrong.**

~~It spins because `SwapperAnnounceSink` always answers with the same message, so the swapper is
handed the same thing forever.~~ **WRONG.** Retracted, with the measurement that killed it.

What actually happens, from the instrumented run:

- The swapper makes **exactly TWO** MON 377B calls, not a stream of them: the 4-argument one at
  `P=0x0800823F`, then the 2-argument one at `P=0x08000677`. There is no repeated re-announce.
- After the second one it is **never dispatched again**. The microcode's IDLE poll walks
  `0o24670..0o24723`, returns to `0o24670`, and repeats - it never reaches the MICFU-24 dispatch slot
  at `0o15250` or `MSG_CONMC` at `0o15676`.
- Reason: **the activation message is destroyed before the restart.** A mailbox guard added to the
  diagnostic's `ProbeMemory` names the writer. At macro `P=0x08009053` the swapper executes
  `w4 sfill b.34` (`swapper-k01-pseg.asm:11718`) - a bulk string fill of `0x80000000` whose
  destination descriptor is computed **from the fabricated probe message**. It fills over the
  harness's mailbox; the re-posted MICFU 24 then lands in a wrecked queue.

Observed message header at each restart (this is the evidence):

```
restart #1 (after  48 macro instrs): LINK=0xFFFFFFFF N5STA=0x0002 X5CPU=0x0001 MICFU=0x0013  X5ACT=0x0001
restart #2 (after 420 macro instrs): LINK=0x80000000 N5STA=0x8000 X5CPU=0x8000 MICFU=0x8000  X5ACT=0x0000
```

Every field at restart #2 is the `sfill` pattern.

**Consequences for the plan:**

- A smarter stub does **not** help. The swapper acts on message CONTENT past this point, so any
  synthetic message yields a synthetic fill destination and more scribbling.
- Relocating the harness mailbox does **not** help. TRIED: moving `ExtBlock` `0x1000 -> 0x4000` only
  moved the clobber (the fill then landed on `HeaderBase` at `0x2000`). Reverted, with the finding
  recorded in the constant's comment so it is not retried.
- What is needed is a **structurally valid SWMSG** - either carved field-by-field, or supplied by the
  real octobus transport. The transport is still the right destination; this changes the REASON and
  removes the two cheap shortcuts that looked available.

**The diagnostic remains a good microcode-gap finder** (it found all eight gaps up to 420
instructions). It simply cannot carry the swapper past message intake on a fabricated message.

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

1. **Give the swapper a STRUCTURALLY VALID SWMSG.** Still the blocker, but see the corrected 5.6 -
   the two cheap shortcuts are dead: a smarter stub does not help (the swapper acts on message
   content and computes a fill destination from it), and relocating the harness mailbox does not help
   (tried; the `sfill` followed). Two routes, and the first is a prerequisite for judging the second:
   - **(a) Carve the SWMSG field layout** so a valid message can be fabricated. Partial work exists
     (5.5.3 control block, 5.5.5 first body field, 5.5.8 the trace recorder). The `sfill` at
     `P=0x08009053` is now a precise carving lever: its descriptor in `b.34` is built from message
     fields, so working backwards from what a SANE fill destination would be pins those fields.
   - **(b) Wire `IMonitorCallSink` to the real octobus transport** in `OctobusND5000Station.cs`.
     Bigger than it looks - see `TRACKB-SHARED-ND500-CPU-INTERFACE-DESIGN-2026-07-21.md`: the
     microword CPU must NOT go through `Nd500CpuProcessBridge`/the C# servicer (the microcode does
     that work itself), so it needs a `CpuND5000Adapter` + a separate `AttachMicrocodeCpu` path, and
     `Emulated.HW` does not currently reference the ND5000 package. That doc's "single next step"
     (extract `INd500ProcessCpu`) is DONE; the retyping of `AttachRealCpu` / `AttachNd5000Cpu` is not.
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

## 7.5 MMU bring-up: the swapper now RUNS (2026-07-27, functional CpuND500 lane)

Five defects stood between "start-swapper crashes on its first instruction fetch" and
"the swapper executes thousands of instructions". All five are fixed and committed in
RetroCore; all were found by making the emulator read SINTRAN's REAL tables instead of
emulator-built shadows. Every claim below was measured on a live SINTRAN III L boot over
the octobus.

| # | Defect | Fix | Evidence |
|---|--------|-----|----------|
| 1 | Process start never loaded **PS** (the process segment register) | Load PS, plus ST1/ST2/TOS/LL/HL/THA, from the context block | Microcode 015043 NEW_PS_1 `TYP,HW ... D,MM,PS` sits right before NEW_CED/NEW_CAD, which we already had |
| 2 | Emulator waited for an MMU **enable** step | There is none - a started process runs translated from instruction one | ND-05.020.01 section 6.2: the MMS baby card controls translation, "not, as in the ND-500/2, by the microprogram" |
| 3 | Capability came from an emulator **shadow** table | Walk the guest's real table via PS and CED | ND-05.009.4 section 4.3 (Figure 16 text) |
| 4 | Page table entries read off the **raw bus**, bypassing MPM routing, and decoded the page number from bits **31-2** | Use the physical path; page number is bits **29-0**, unshifted | Nanostate 6 CAPIT "Bit 31 and bit 30 must be 0"; live index page reads 0xE9 0xEA 0xEB 0xEC |
| 5 | DC_PAC (bit 14) treated as a **user/kernel mode gate** | It is the ALT-prefix gate for other domains only | ND-05.009.4 section 4.2.3.2 |

Also corrected: a zero CAPABILITY is a protect violation (MMS code 8), not a page fault
(code 13) - decided before the PST is consulted (nanostate 8 CAPT).

### Verified addressing anchors (three-way agreement)

Patched control-store cells, the manual's shift rule, and the monitor's own
MEMORY-CONFIGURATION output all agree:

| Cell | Contents | ND-500 page | Byte address |
|------|----------|-------------|--------------|
| OFFSET (0o20) | context (register) block area | 124B | 0x0002A000 |
| PSTBASE (0o21) | physical segment table | 164B | 0x0003A000 |
| (0o22) | WIP/PGU table | 123B | - |

### What SINTRAN actually grants the swapper (measured)

```
CED=0  PS=3  PSTP=0x0003A000
PROGRAM  seg  1: 0x0002 psn=2
         seg  2: 0xC000 INDIRECT other machine
         seg 31: 0xC000 INDIRECT other machine   <- the ND-100, i.e. the MON call path
DATA     seg  1: 0x8001 psn=1 WRITE
         seg  2: 0x8004 psn=4 WRITE
         seg  3: 0x8005 psn=5 WRITE
         seg 13: 0xA003 psn=3 WRITE SHARED       <- the ND-100 shared segment
```

Physical segment table: PSN 1 and 2 single-indexed (pages 0xE6 / 0xE7), PSN 3 direct
(page 0xE8). Everything else is zero.

### Progression of the blocker (each row is a real fix, not a workaround)

```
PC=0x08000004  "program MMU disabled"        -> fixes 1 + 2
PC=0x08000004  unmapped walk                 -> fix 3 (capability now reads 0x0002)
PC=0x08000004  opcode 0x0000                 -> fix 4 (was resolving to the wrong page)
PC=0x08000011  write protect violation       -> fix 5
PC=0x0800913B  data segment 0 offset 10      <- CURRENT
```

SINTRAN's own trap reporting works end to end throughout: it prints
"ND-500(0) Trap / Protect violation / Shadow process 5SWAP", so the trap record reaches
the ND-100 rather than the emulator dying alone.

### 7.5.1 OPEN: data segment 0, offset 10, at PC 0x0800913B

The swapper reads virtual address 0x0000000A. Data segment 0 has NO capability in the
domain, so this is now correctly reported as a protect violation.

NOT RESOLVED - do not guess. What is known:
- The context block starts the swapper with L, B, R and every index register ZERO, so a
  null base register plus a field offset of 10 is ONE candidate explanation, and would
  make this an operand/instruction bug rather than a mapping question.
- It is not the Address Zero case (that is VA 0 exactly, and is ignorable).
- Segment 13 - the segment shared with the ND-100 - IS mapped, so the swapper's message
  path is available to it.

### Narrowed by an instruction trace (2026-07-27)

An opt-in instruction trace ring was added to `CpuND500`
(`InstructionTraceDepth` / `DumpInstructionTrace`) and settles two things.

**Two diagnostic defects had to be fixed first, and both had been misleading us:**

1. MMU traps reported the WRONG instruction. `ExecuteDecodedInstruction` advances PC
   before the instruction runs, so `regs.PC` names the NEXT instruction throughout
   execution - including when an access traps. The stop reported at `0x0800913B` is
   really the instruction at `0x08009137`, four bytes earlier. The architecture keeps
   these apart deliberately: the context block has BOTH a "Trapping P register" and a
   "Restart P register" (ND-05.017.01 Appendix A.1, registers 0 and 1). Now reported
   as the trapping P. The C port `nd500x` had the identical bug and had already fixed
   it for page faults only; protect violations there are still uncorrected.
2. The trace entry must be opened BEFORE the fetch, because the ND-500 fetch also
   decodes operands - so an instruction faulting during operand decode would never
   appear, and the trace would end on its predecessor.

**What the trace establishes:**

- The swapper retires exactly **129 instructions**, from `0x08000004` to `0x08009137`.
  (An earlier note in this file's history said "thousands" - that was wrong; the PC
  covers a lot of address space but few instructions.)
- At the faulting instruction, **B=0x08024300 and R=0x080240BC are both valid
  segment-1 pointers**. So the null-base-register hypothesis for the `0x0000000A`
  operand is **DEAD** - do not re-investigate it.
- The faulting instruction is at `0x08009137`, opcode `0xFD69`.

### 7.5.2 RESOLVED - it was never a mapping problem (2026-07-27)

`0xFD69` is `W SMOVE` (string move). The answer came from recording the operand's
addressing at fault time (`LastOperandAddressing`, quoted by the MMU when it refuses):

```
operand: mode=LOCAL_SHORT reg=0 disp=52 B=0x08024300 -> ea=0x08024334
```

The refused address `0x0000000A` has NO relationship to that - a valid segment-1
address. The bad address was therefore computed INSIDE the instruction, not by operand
decoding.

**Root cause:** every STRING instruction obtained its descriptor address with
`ReadOperandValue`, taking the operand's VALUE as the address of the descriptor - one
indirection too many. It was actually fetching the descriptor's first word (the element
count) and using that as a pointer.

ND-05.009.4 section 8.15 "Descriptor addressing": "`<operand>` is the address of a
descriptor, and it can be any operand specifier except ALT, constant or register." The
manual's own SMOVE example settles it:

```
D SMOVE IND(B.DATABLOCK), B.COPY
```

`B.COPY` names a descriptor sitting AT B+COPY; `IND(...)` is how a program asks for one
more level. Indirection is the operand specifier's job, not the instruction's.

Fixed at all 27 sites across 17 STRING instructions, via a single
`GetDescriptorAddress` helper (which also handles the constant-mode case, where there is
no effective address and the constant's value IS the descriptor address - the nd500x C
port draws the same distinction, and was already correct here).

**Two hypotheses this killed, both recorded here earlier, both wrong:**
- a null base register - B and R were valid segment-1 pointers throughout
- a bug in PRE-INDEXED addressing - ND-05.009.4 section 8.9 confirms `ea=(Rn)+d` with no
  base register, so the existing code was right

## 7.6 MILESTONE: the swapper runs and parks (2026-07-27)

```
PC=0x08000687  stopMode=WAIT  PS=3  CED=0
"The Swapper stopped" on the SINTRAN console: 12 occurrences before, ZERO after
CPU crashes: ZERO
```

The swapper now executes normally and enters a WAIT state - parked, which is what a
swapper does when it has reached its message loop and has nothing to do. Reproduced
across runs at the same PC.

### 7.6.0 CORRECTION: the swapper is NOT idling - it reports a FATAL ERROR

An earlier reading of this run said the swapper "parks in WAIT, which is what a swapper
does when it has nothing to do". **That was wrong.** The second MON 377B is the swapper
reporting SWPFATAL to SINTRAN. It is parked because nobody answered its death notice.

The claim in 7.6.1 that N5STA 5/6 are an undocumented anomaly was also wrong - those
values are documented and benign. Both corrections are folded into 7.6.2.

### 7.6.1 What the parked swapper is waiting for (2026-07-27)

The harness now dumps the bridge's MON call log. It localises the blocker exactly:

```
MON 377B argc=4 ret=0x08008255 ...                     -> ANSWERED
  RESTART write-back: @0x080240B0:=0x00000005 @0x080240B4:=0x00008E30
MON 377B argc=2 ret=0x08000687 | [0]=0x00000427 [1]=0x00000081  -> NO ANSWER
```

**The monitor-call round trip WORKS end to end** - the first call was answered and the
swapper resumed and ran on. It is parked on the SECOND call, and `PC=0x08000687` is
precisely that call's return address. So this is not a broken MON path; it is one
specific call going unanswered.

**The thread to pull.** The first answer hands the swapper `0x00008E30`. The mailbox
doorbell walk then shows a message block at exactly that address whose `N5STA` reads 5
and then 6. The documented status set is:

| N5STA | Meaning |
|-------|---------|
| 0 | free (INFERRED, no symbol) |
| 1 | MSGN500 - message to ND-500 |
| 2 | WAITING - in process |
| 3 | ANSWER - answer to ND-100 |
| 4 | 5ERANSWER - error return |

5 and 6 are **outside** it. And the servicer only ever processes a message whose
`N5STA == 1`, so that block is silently skipped by everything.

**UNRESOLVED - do not guess.** Candidates, explicitly unranked and unproven:
- a status convention the SWAPPER writes for its own buffer (it was handed 0x8E30 by
  SINTRAN, so both sides touch it)
- a direction/state encoding not yet carved
- a misread field in the walk

The ND-500 microcode settles this.

**Also re-check while you are there:** MICFU 5 (`3SWMESS`, MessageToSwapper) is currently
accepted ONLY on the Classic generation - `understood = Generation ==
Nd500Generation.Classic` - on the stated grounds that SINTRAN never transmits it on the
5800. That is an assumption from the carve, not an observation, and it should be tested
against a live capture.

**Mailbox decode gotcha** (cost time once): MICFU values are OCTAL in the symbols but
stored DECIMAL in the enum. So in a doorbell walk `0x18`/`0x19` are 30B/31B =
PHYSRD/PHYSWR (the octobus pair), `0x13` = 3START, `0x14` = 3MONCO, `0x05` = 3SWMESS.

**Still open:** `list-active-processes` shows only SYSTEM, i.e. SINTRAN does not yet
consider the swapper a running process. Likely the same unanswered call.

### 7.6.2 RESOLVED: N5STA 5/6 are normal, and the real blocker is SWPFATAL 0o201

Both questions above are answered from the repo - no microcode needed.

**N5STA 5 and 6 are documented and benign.** They are swapper-process states, part of a
contiguous enum in the SINTRAN symbol tables
(`SINTRAN/NPL-SOURCE/SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT` lines 1970-1985):

```
MFREE=0  MSGN5=1  WAITI=2  ANSWE=3  5ERAN=4
SWPWA=5  SWPPI=6  PSWWA=7  MPBRK=10B  SUSPS=11B ...
```

Full spellings from `SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL`: **SWPWAIT=5** ("process is
waiting for the swapper", line 2862) and **SWPPING=6** ("process is using the swapper",
line 2870), with PSWWAIT=7 = swapper free. The observed 5 -> 6 transition is exactly the
`5ACTSWAPPER` sequence. Nothing wrong here - close this line of inquiry.

**MON 377B decoded.** 377B = MON 255 = `N5SWAP`, handled by `SWPDECODER`
(`MP-P2-N500.NPL:913-919`). Sub-functions: 0=ESWPFATAL, **1=LNEWSWAP**, 2=LSWPAGE,
3=LPRSUSPEND, 4=LALLOPAGE, 5=LDATREADY, 6=LCLTSB, bound SWFMAX=6.

- First call, selector 1 = LNEWSWAP ("ask the ND-100 for a message") - ANSWERED, correct.
- Second call, selector **0x427 = 0o2047 = SWPFA (SWPFATAL)**. Confirmed uniquely in
  `SINTRAN/ND500/swapper/N500-SYMBOLS.SYMB`, and by the vendor manual
  `Reference-Manuals/500/ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md:11995`
  ("2047B | FATAL ERROR IN SWAPPER"). Being > SWFMAX, SWPDECODER routes it to ESWPFATAL.

**So the swapper is REPORTING A FATAL ERROR, not waiting for work.**

**The exact fatal, byte-verified against the disassembly.** Our instruction trace matches
`swapper-k01-pseg.asm` opcode-for-opcode (0xDA=332B `if <<=`, 0xFF15=377/025 `dcc`,
0xFF18=`dmof`, 0xFF16=`dmon`), and the MON call site at `1000003167` = VA `0x08000677`
carries args `$1000225040`=0x12A20 and `$1000437234`=0x23E9C - identical to the live
capture. The reporting routine is at `1000003057` (VA 0x08000617): it stores the caller's
code into the fatal slot 0x23E9C and issues the MON.

The caller is the routine at `1000107011` (VA 0x08008E09). Its loop:

```
1000107141  w2 := b.30              ; current list element
1000107143  dmof                    ; DATA MMU OFF - physical addressing
1000107145  h2 := r2.(10)           ; halfword at element+8
1000107150  dmon
1000107152  h2 comp $1              ; is it 1?
1000107172  w incr b.40             ; yes -> count it
1000107174  r := b.30
1000107176  dmof
1000107200  w1 := r.4               ; follow link at element+4 (physical)
1000107214  go (loop)
1000107216  w move $201,b.54        ; loop done -> error code 0o201
1000107225  w test b.40
1000107227  if <<= go $4            ; b.40 == 0 ...
1000107263  w1 := b.54
1000107265  call SWPFATAL           ; ... -> FATAL 0o201
```

**The swapper walks a list in PHYSICAL memory, counts elements whose halfword at +8 is 1,
and reports fatal 0o201 when it finds NONE.** The list head is read from its data segment
at `$1000461024` (VA 0x08026214) before the first `dmof`, so that word holds a PHYSICAL
pointer.

Note 0o201 is the swapper's own internal code, NOT one of the 2047B sub-codes in the
manual (that table only runs 1B-46B, ND-05.017.01 around line 12000).

**Next, and it is now a narrow question:** what should be at DSEG `0x08026214`, and is the
physical list it points at present and correctly linked in our emulated memory? A count of
zero means either the head pointer is empty/garbage, or the elements are there but their
+8 halfword is not 1. Both are directly observable - dump the head word and walk the list.

**Also confirmed by this:** the swapper genuinely uses `DMOF`/`DMON` (data MMU off/on) and
our CPU executes them, so the note that ND-5000 translation is "always on" describes the
MMS default state, not these explicit instructions.

### 7.6.3 The list, captured live (2026-07-27)

Captured with the swapper's context still loaded (PS=3, CED=0). Capturing it after the run
does NOT work: the finally block runs once the process context has been cleared, PS reads
0, the capability walk resolves nothing, and every address reports "unmapped" - which is
indistinguishable from a genuine mapping failure. Sample it while PS is non-zero.

```
VA 0x08026214 -> PA 0x0004FA14 = 0x00000024      <- list head word
VA 0x08026218 -> PA 0x0004FA18 = 0x00000028
physical list from 0x00000024 (link at +4):
  [0] @0x00000024 +8=0x0000 link=0x00000000
  -> 1 element
```

**The observation, and only the observation:** the head word is `0x24`, the walk reaches
ONE element there, and that element's `+8` halfword is `0`. The swapper counts only
elements whose `+8` is `1`, so it counts zero and reports SWPFATAL 0o201.

Note `0x24` as a PHYSICAL address lands in the low MPM window (RouteToMpm sends physical
addresses below the MPM size into the shared window), i.e. inside the 5MPM area - so it is
not obviously a nonsense pointer.

**HYPOTHESIS, NOT VERIFIED - do not build on this without checking.** The list may be the
MEMORY PART table. `MEMORY-CONFIGURATION` on this machine reports exactly one part:

```
   PART       WIDTH        N100   N500P  N500D
     0B      0B-  7777B      Y      Y      Y
```

one entry, flagged available to the ND-500 - which would match a scan that counts parts
whose flag is 1. The manual's swapper error 30B ("INCREASE - Too many memory intervals.
Bad memory configuration") shows the swapper does read a memory-interval structure. But
the element layout has NOT been carved and the `+8` field has NOT been identified. It
could equally be a segment table, a process table, or something else.

### 7.6.4 ROOT CAUSE ONE LEVEL UP: the base pointer is NULL (2026-07-27)

Step 1 below is done, and it moves the defect. `0x26214` is not a list head that SINTRAN
fills - it is DERIVED. The single writer is the initialiser at `1000103415`:

```
1000103423  w move b.24,$1000461014   ; 0x2620C := the incoming BASE POINTER
1000103432  r := $1000461014          ; r := that base
1000103440  dmof
1000103476  w1 laddr r.44             ; ADDRESS of base + 0o44
1000103511  w1 =: $1000461024         ; 0x26214 := it
1000103527  w1 laddr r.50             ; ADDRESS of base + 0o50  -> 0x26218
```

MEASURED live (PS=3, CED=0):

```
VA 0x0802620C -> PA 0x0004FA0C = 0x00000000    <- THE BASE POINTER IS ZERO
VA 0x08026210 -> PA 0x0004FA10 = 0x00000000
VA 0x08026214 -> PA 0x0004FA14 = 0x00000024    = 0 + 0o44
VA 0x08026218 -> PA 0x0004FA18 = 0x00000028    = 0 + 0o50
```

The two derived words are exactly the offsets off a null base. So the scan walks garbage,
finds one element whose `+8` is 0, counts no matches, and reports SWPFATAL 0o201.

**The defect is therefore NOT in the list and NOT in the scan.** It is that the caller at
`1000077777` (VA `0x08007FFF`) hands the initialiser a null base pointer. Parameters
arrive in W1 (= our I1 - VERIFIED: at the SWPFATAL call I1 held 0x81, exactly the error
code the caller loaded with `w1 := b.54`).

The memory-part-table hypothesis in 7.6.3 is now IRRELEVANT to the failure - whatever the
structure is, the swapper never got its address. Do not spend time on it.

**What to do next, in order:**
1. DONE - see above. The single writer of 0x26214 is the initialiser at 1000103415, and it
   derives the value from a base pointer at 0x2620C which is NULL.
2. Find where the caller at `1000077777` is supposed to get that base pointer. It is early
   startup, so likely from the swapper's start-up data, from the LNEWSWAP answer, or from
   a value SINTRAN/the ACCP should have placed. Note manual error 46B is "INIT SWAPPER -
   Illegal startup data", which is the right neighbourhood.
3. Only then decide whether the emulator fails to supply something, or the swapper is
   started before SINTRAN has set it up.

### 7.6.5 WARNING: the swapper disassembly is wrong after a LOOPI (2026-07-28)

A frozen instruction trace (stop recording on reaching the initialiser) shows the `loopi`
at `1000077760` is **7 bytes**, not the 10 the listing claims, so the next instruction is
at `1000077767`, not `1000077772`.

Confirmed three ways - the manual says `0xFCDF` is `H LOOPI:B` (byte displacement, and
LOOPI has no `:W` form at all); byte `354` = -20 gives `0o77760 - 20 = 0o77734`; and the
live CPU branches to exactly `0o77734`.

**This voids the note in 7.6.4 that the caller executes `w3 or $30` / `by test $40` /
`bi2 clr` before its call - those instructions do not exist.** Full writeup:
`SINTRAN/ND500/swapper/DISASSEMBLY-DEFECT-LOOPI-LENGTH-2026-07-28.md`.

**What still stands, because it is measured rather than read off the listing:** at the call
into the globals initialiser (`0o77777` = VA `0x08007FFF`), **W1 = 0x00000000**, and W1 is
the parameter register (verified: at the SWPFATAL call W1/I1 held 0x81, exactly the error
code). So the swapper genuinely passes a null base pointer, and everything in 7.6.4 that
derives from that is unaffected.

### 7.6.6 THE CHAIN IS COMPLETE: RIOM never delivers the message (2026-07-28)

Re-carved from the raw bytes (the listing is unreliable there). At the call:

```
0o77767  030 105   r:= b.24       -> R = 0x080240BC
0o77771  014 242   w1 := r.210    -> W1 = 0          <- the null base pointer
0o77773  030 102   r:= b.10
0o77775  040 205   w1 =: r.24
0o77777  303 ...   call the globals initialiser
```

Address code `242` is RECORD SHORT, `ea = (R) + d*4`, `d = 242B - 200B = 34`, so offset
136 = `0o210` (ND-05.009.4 RECORD table; the listing's own `r.330` for `266B` confirms the
convention). Every boundary matches the executed trace.

Word offset `0o104` = byte 136 = **`HSWPI` = `SWMSG.SWPINFO`** in the SINTRAN symbol table
(`SINTRAN/NPL-SOURCE/SYMBOLS/K03/N500-SYMBOLS.SYMB.TXT`). So the swapper is reading the
SWPINFO pointer out of the message record it keeps at `0x080240BC`.

MEASURED:

```
0x080240B4 = 0x00008E30   <- message pointer from SINTRAN's MON 377B answer
0x080240B8 = 0x00000005
0x080240BC .. 0x0802414C  = ALL ZERO      <- the message record, never filled
0x08024144 = 0x00000000   <- HSWPI, byte 136: the null base pointer
```

**SINTRAN never copies a message into ND-500 memory - it only ever hands over an ADDRESS.**
Verified in the source: `5ACTSWAPPER` (`MP-P2-N500.NPL:2851+`) writes
`SWMSG.HSWPI/SWPIN := CNVWADR(requester message)` and `SWMSG.SWPST`, then restarts the
swapper. There is no copy loop, no RESIWR/PHYSWR transfer, anywhere on that path.

**The swapper fetches the body itself, with RIOM** (`swapper-k01-pseg.asm:10577`):

```
1000101356: h riom $1000440264,$1000440274,$1000440074+
            ;      src ptr cell 0x240B4 -> dest 0x240BC, count table 0x2403C+
```

So the destination of that DMA is exactly the record we measure as all zero, and its
source is the `0x8E30` SINTRAN supplied.

**THE GAP: the RIOM ND-100 address mapping.** `CpuND500.ND100Bridge.cs` maps an ND-100
address as `_private + address*2`, and the octobus path sets `_private = 0`
(`Nd500CpuProcessBridge.cs:75`). So RIOM reads physical byte `0x8E30 * 2 = 0x11C60`. But
that same pointer value resolves, in our own octobus servicer's doorbell walk, to ND-100
address **`0x428E30`** - i.e. it is an offset inside the 5MPM window at `0x420000`. RIOM is
therefore reading somewhere else entirely, gets zeros, and the record stays empty.

This is not a new suspicion: `Instructions/IO/Riom.cs` already carries an OPEN note saying
the `_private` handling for this path is UNRESOLVED, that a live SINTRAN source lands at
the wrong address, and warning that the RIOM tests cannot detect it because they seed data
through the same biased mapping in both directions. The swapper is now a live
demonstration of that bug.

**NOT RESOLVED - do not guess the fix.** What `CNVWADR` ("convert multiport address")
produces, and therefore how a swapper message pointer should be turned into an address
RIOM can read, has to be carved from `MP-P2-N500.NPL` before any code changes. Getting it
wrong silently reads the wrong memory, which is precisely the failure mode here.

**A caution from the source.** `5ACTSWAPPER` stores the pointer as a DOUBLE word
(`*AAX HSWPI; STDTX`): A to `0o104` (byte 136) and D to `0o105` (byte 138). If the pointer
is 32 bits across that pair, then byte 136 alone is only half of it. Check which half the
swapper expects before concluding the value is simply absent.

**Also note:** `LNEWSWAP` deliberately ZEROES `HSWPI`/`SWPIN` when the swap FIFO is empty
(`MP-P2-N500.NPL:1058-1065`), so a zero there is also the legitimate "no work" state. Do
not assume zero always means a lost transfer - distinguish the two before acting.

**Superseded:** the earlier guess that MICFU 5 (3SWMESS) should populate this record is
WRONG. `3SWMESS` is an ND-500 -> ND-100 request meaning "pass this message to the
swapper"; SINTRAN's handler ends in `CALL 5ACTSWAPPER`, which posts a pointer. It pushes
no data. Do not pursue it.

### 7.6.7 CORRECTION: RIOM's mapping is FINE - the POINTER is unconverted (2026-07-28)

Section 7.6.6 blamed the RIOM address mapping. That was too quick. Checking the arithmetic
against `Riom.cs`'s own worked example settles it:

```
Riom.cs example : ND-100 WORD 0x210718 -> byte 0x420E30    (word * 2, _private = 0)
0x210718 * 2 = 0x420E30                                     CORRECT
```

The octobus path sets `_private = 0` (`Nd500CpuProcessBridge.cs:75`), so **RIOM's mapping
is already right for a properly converted pointer.** The `_private = 0x40000` case in that
OPEN note is a different (3022) configuration.

**The defect is the POINTER VALUE, not the mapping:**

```
message lives at ND-100 byte   0x428E30
  as an ND-100 WORD address    0x214718   <- what RIOM needs
swapper was actually handed    0x008E30
0x428E30 - ADRZERO (0x420000) = 0x008E30  <- EXACTLY the raw 5MPM offset
```

So the swapper received the **unconverted 5MPM offset**. `CNVWADR` should have turned it
into an ND-100 address (this is the ADRZERO map: `ACCP-REGION25-ADDRESS-HANDOFF-CARVE-2026-07-19.md`
lines 139-143 - "window-physical byte address = ADRZERO (0x420000) + CNVBYADR(5MPM
offset)"). Feed RIOM `0x214718` and it lands on `0x428E30`, which is where the message
actually is.

**It is NOT a truncation on our side.** `ReadRestartRecord`
(`Nd500MicrocodeServicer.cs:1041+`) assembles each write-back value as
`(high << 16) | low` from consecutive ND-100 words. SINTRAN genuinely wrote
`0x00008E30` - high half zero. Verified by reading the code, not assumed.

**OPEN, and the next thing to carve:** why SINTRAN's `CNVWADR` yields an unconverted
offset in this run. `5ACTSWAPPER` does `A:=5MBBANK; CNVWADR` (`MP-P2-N500.NPL:2857-2861`),
so the conversion is driven by **5MBBANK**, not by ADRZERO directly. ADRZERO is reported
correctly by MEMORY-CONFIGURATION in our run (`004100` = 0x420000), but 5MBBANK is a
separate value and has not been checked. Start there.

Note the routine bodies for `CNVWADR` (0o055160) and `CNVBYADR` (0o055034) are NOT in the
NPL tree - only call sites and symbol values. They will have to come from a segment carve
or the running system.

**Do not "fix" RIOM.** Its mapping is correct; changing it to compensate for a bad pointer
would hide the real defect and break the cases that currently work.

**Tooling note.** Reaching that caller in the instruction trace needs a ring deeper than
256, and every attempt at a deeper ring aborted the test host. That is NOT established as
the ring's fault: a second session was running concurrently against the SAME writable pack
(`%TEMP%
etrocore-nd5000-octobus\BIGDISK0-L.IMG`), which is a likelier cause and also
aborted a run at depth 256. Re-test deep tracing on a quiet machine. Concurrent harness
runs sharing that pack should be avoided regardless - they can corrupt it.


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

---

## 7.6.8 CARVE: why CNVWADR yields an unconverted offset - 5MBBANK is ZERO (2026-07-28)

### CNVWADR's calling convention (VERIFIED from call sites)

`CNVWADR` is not a CALL - the call sites are written `*NNC24,CNVWADR`, i.e. **patch points**
(the `NNCnn` labels are collected in `DP-P2-VARIABLES.NPL:212-214`). Its register
convention is unambiguous from three independent sites:

```
MP-P2-N500.NPL:2766  5ACTSWAPPER:  X=:D=:MSGTOSW; A:=5MBBANK
                     *NNC24,CNVWADR
                     AD=:CMSGTOSW                      <- 32-bit result in A:D

RP-P2-N500.NPL:850   5MBBANK=:T; *CNVWADR
RP-P2-N500.NPL:849   SWMSG=:D; A:=T ; *NNC39,CNVWADR
```

**A = bank, D = offset in; AD (32-bit) out.** The HIGH half of the result can only come
from A. We measured `CMSGTOSW = 0x00008E30`, high half zero, so **`5MBBANK` read 0.**

### 5MBBANK has exactly ONE writer

`RP-P2-N500.NPL:737` in `XMSINIT`:

```
131133   5FPMAILBOX=:D:=0; AD SH 12; A=:5MBBANK      % MEMORY BANK FOR MESSAGES
```

`D := 5FPMAILBOX`, `A := 0`, then shift the 32-bit pair left by `12B` = 10 decimal (1 page
= 1024 words = 2^10), turning a PAGE number into a WORD address. `A` is then the high 16
bits, so:

```
5MBBANK = 5FPMAILBOX >> 6
```

`5MBBANK = 0` therefore means `5FPMAILBOX < 64`.

### Where 5FPMAILBOX comes from, and what it SHOULD have been

`5P-P2-MON60.NPL:640` (`INZ500`): `CALL 5GBUFF; ... A=:5FPMAILBOX`. It is also explicitly
zeroed on the release path (`5P-P2-MON60.NPL:501  0=:5FPMAILBOX`).

`5GBUFF` is `X5GBUFF` (`RP-P2-N500.NPL:891`). The page it returns is built at line 131763:

```
131763   A+ADRZERO=:5GBFPAGE; A:=D-1+ADRZERO=:5GBEPART=:5GBLPAGE=:D
131756   AD:=DPAMEMTABLE(X); IF A=0 THEN A+1 FI       <- part base 0 is bumped to 1
```

so `5FPMAILBOX = part_base_page + ADRZERO`.

**ADRZERO is a PAGE number, not a byte address.** Confirmed by every use of it:
`MP-P2-N500.NPL:170` `A:="N500DF".ADRZERO=:D:=0; AD SH 12`, and identically at
`MP-P2-DISK-START.NPL:269`, `MP-P2-N500.NPL:1943`, `MP-P2-PERF-CODE.NPL:1804`. Our run
reports `ADRZERO = 004100B` = 2112, which as a page is word `0x210000` = byte `0x420000` -
exactly the window base used elsewhere. The two figures are consistent.

**So the expected value is:**

```
5FPMAILBOX = 1 + 2112 + (earlier allocations)   >= 2113
5MBBANK    = 2113 >> 6                          = 33 = 41B
```

and `CNVWADR` should have produced `AD = 0x0021_xxxx`, i.e. word address `0x214718` - the
value section 7.6.7 showed RIOM needs. **It produced 0.**

### Conclusion, and what is NOT yet established

The defect is EARLIER in the chain than `CNVWADR`. `CNVWADR` faithfully passes through a
bank of 0. One of these is true, and they have NOT been distinguished:

1. `ADRZERO` was 0 or -1 when `XMSINIT` ran (it defaults to -1 -
   `PH-P2-OPPSTART.NPL:2494-2498` sets it from `CURRPAGE`, and `5P-P2-MON60.NPL:530`
   tests `IF ADRZERO=-1`). `MEMORY-CONFIGURATION` printing `004100` LATER does not prove
   it was set at `XMSINIT` time.
2. `5FPMAILBOX` is still 0 because `5GBUFF` never succeeded, or the area was released.

**A concrete candidate for (2), UNVERIFIED:** `X5GBUFF`'s part filter at `RP-P2-N500.NPL`
line 131750 demands the part's type byte be EXACTLY equal to `MSHARED|PSACC|DSACC`:

```
131746   T:=TYPADR; *LBYT
131750   T:=0 BONE MSHARED BONE PSACC BONE DSACC
131754   IF A=T THEN
```

If our emulated memory table does not set `MSHARED` on the part, no part matches, the FOR
loop falls straight through to `OUT` (the "no memory available" return), and no ND-500
buffer is ever allocated. Our `MEMORY-CONFIGURATION` shows one part `0B-7777B` with
`N100/N500P/N500D = Y Y Y`, but whether the emulator sets the `MSHARED` bit in `TYPMTAB`
has NOT been checked. Check it before changing anything.

### Next step

Dump the LIVE values of `ADRZERO`, `5FPMAILBOX` and `5MBBANK` at the moment `XMSINIT`
runs. That single measurement separates (1) from (2) and from the `MSHARED` candidate.

Note `5MBBANK` and `5FPMAILBOX` are NOT in the symbol tables and have no declaration
anywhere in the NPL tree - they are externals whose defining file is missing (the known
NPL-tree gaps). Their addresses will have to come from a carve or from the datafield
layout, not from a symbol lookup.

**Still standing from 7.6.7: do not "fix" RIOM.** Its mapping is correct; the pointer it is
handed is not.

## 7.6.9 BETTER LEAD: CNVWADR is PATCHED OUT when NSAMSON = 0 (2026-07-28)

Section 7.6.8's `5MBBANK` reasoning is sound arithmetic but probably NOT the cause. A
stronger, fully static mechanism explains the unconverted offset exactly.

**The `*NNCnn,` prefixes mark self-modifying patch sites.** All of them are collected in
`DP-P2-VARIABLES.NPL:211` under the comment **"Common & Mpit: convert-address table"**:

```
011773   INTEGER ARRAY MNCTAB:=(
011773      NNC01,...,NNC27, NNC50..NNC55, 0,0,0,0,0,-1);
```

`NNTAB` (`DP-P2-VARIABLES.NPL:174`) pairs each table with the instruction to stamp into it:

```
%           mudom        -mudom
%     pit  instr  table instr  table
      MPIT,124001,MNATAB,124001,MNJTAB,
      MPIT,124002,MNTTAB,124003,MNCTAB,
```

`BYPASS` (`RP-P2-N500.NPL:1009`, commented **"Patch n500 code according to CPU type"**)
walks it and picks the column:

```
132343   IF NSAMSON><0 THEN S1; T:=S2 ELSE S3; T:=S4 FI
...
132405   CINSTR; *STATX                  <- OVERWRITES the instruction at each site
```

So:

- **NSAMSON <> 0 (Samson/octobus):** stamps `124002` into `MNTTAB`. `MNCTAB` is left
  alone - **`CNVWADR` stays live and converts.**
- **NSAMSON = 0:** stamps `124003` into `MNCTAB` - **every `CNVWADR`/`CNVBYADR` site in
  the system is overwritten**, and the address passes through unconverted.

**That is exactly our symptom:** `AD = 0x00008E30`, the raw 5MPM offset, no bank applied.

### Why NSAMSON could be 0 on our machine

`NSAMSON` is counted in `RP-P2-N500.NPL:132206-132255`:

```
132206   0=:NSAMSON=:N5CPU
132214   IF X.CPUAVAILABLE/\5CPUTYPE=SAMSON THEN ... MIN NSAMSON
```

and `CPUAVAILABLE` is stamped `SAMSON` in exactly ONE place -
`PH-P2-OPPSTART.NPL:3933-3934`, inside the octobus presence probe:

```
063146   *TRA IIC                        % Read octobus if. status
063147   A:=200; *TRR IIE
063151   T:=100406; *IOXT; TRA IIC
063154   IF A=0 THEN                     % Octobus present? - (assumes Samson)
063155      DO *IOXT WHILE A NBIT 3 OD   % wait for data ready
063161      ASTATION\/COMD=:5STATION
063170      T:=100405; A\/CMMACLE; *IOXT % masterclear Samson system
063173      A:=X\/CMACONT; *IOXT         % continue accp
063176      MIFLAG BONE MUDOM=:MIFLAG
063201      CPUAVAILABLE/\140000\/SAMSON <- the ONLY SAMSON stamp
```

If our emulated octobus interface does not satisfy this probe exactly - `TRA IIC` reading
0 after the `IOXT` on `100406`, then bit 3 setting in the data-ready poll - SINTRAN never
marks the CPU as Samson, `NSAMSON` stays 0, and `BYPASS` patches out every address
conversion in the ND-500 system.

**NOT YET VERIFIED, in order of cheapness:**
1. Whether our emulator satisfies the `063146-063154` probe (readable statically in
   RetroCore - do this FIRST, no run needed).
2. What instruction `124003` actually is.
3. The live value of `NSAMSON`.

This also predicts a broad symptom, which is a good check: with `CNVWADR` patched out,
EVERY ND-500 address conversion in the system is dead, not just the swapper's. If other
ND-500 addressing works correctly in our run, this hypothesis is WRONG - test it that way
before changing anything.

### 7.6.10 Symbol addresses (SINTRAN L / L07) - the symbol table truncates to 5 CHARS

Section 7.6.8 said these symbols "are not in any symbol table". **That was wrong** - the
ND symbol tables keep only the first **5 characters** of a name, so `5MBBANK` is stored as
`5MBBA`, `5FPMAILBOX` as `5FPMA`. Searching the full names finds nothing. Search truncated.

From `SINTRAN/NPL-SOURCE/SYMBOLS/L07/` (`SYMBOL-1-LIST`, `SYMBOL-2-LIST`, cross-checked
against `l07-kallsyms.txt`, which lists the same values in hex):

| Symbol       | Stored as | Octal    | Hex word addr | What it is                          |
|--------------|-----------|----------|---------------|-------------------------------------|
| `NSAMSON`    | `NSAMS`   | `011250` | `0x12A8`      | number of Samson CPUs - THE GATE    |
| `N5CPU`      | `N5CPU`   | `011247` | `0x12A7`      | number of live ND-500 CPUs          |
| `5MBBANK`    | `5MBBA`   | `004654` | `0x09AC`      | message bank number                 |
| `5FPMAILBOX` | `5FPMA`   | `111102` | `0x9242`      | first mailbox page                  |
| `5NPMAILBOX` | `5NPMA`   | `111103` | `0x9243`      | number of mailbox pages             |
| `MNCTAB`     | `MNCTA`   | `011773` | `0x13FB`      | the convert-address patch table     |
| `NNC24`      | `NNC24`   | `145171` | `0xCA79`      | the `CNVWADR` site in `5ACTSWAPPER` |
| `NNC06`      | `NNC06`   | `134051` | `0xB829`      | the `CNVWADR` site in `SWMESS`      |
| `XMSINIT`    | `XMSIN`   | `131265` | `0xB2B5`      | sets `5MBBANK`                      |
| `N500DF` ptr | `N500D`   | `051767` | `0x53F7`      | base of the ND-500 datafield        |
| `ADRZERO`    | `ADRZE`   | `000060` | -             | OFFSET `60B` inside `N500DF`        |
| `MIFLAG`     | `MIFLA`   | `177770` | -             | OFFSET, holds the `MUDOM` bit       |

(`ADRZE` and `MIFLA` are small datafield-relative offsets, not absolute addresses -
consistent with their use as `"N500DF".ADRZERO` and `X.MIFLAG`.)

### The decisive measurement, now cheap

Read four ND-100 words in the booted system:

```
NNC24 @ word 0xCA79   -> is it 124003B (PATCHED OUT) or the original CNVWADR site?
NSAMS @ word 0x12A8   -> 0 means BYPASS took the -mudom column
5MBBA @ word 0x09AC   -> expected 41B (33); 0 would confirm 7.6.8 instead
5FPMA @ word 0x9242   -> expected >= 2113
```

`NNC24` alone decides between 7.6.8 and 7.6.9. `BYPASS` patches at RUNTIME, so this cannot
be read from the disk image - it has to be sampled from the running machine.

## 7.6.11 MEASURED: N5CPU = 0 and NSAMSON = 0 - SINTRAN does not believe an ND-500 exists (2026-07-28)

### The read convention, proven before any value was trusted

The first probe attempt was VOID and said so: it read resident symbols with RAW PHYSICAL
reads, and its own anchor (`ADRZERO` must hold page `004100B` = `0x0840`) returned `0x0000`
in both byte and word conventions. Symbol-table addresses describe the **PIT-mapped
resident image**, not physical memory, so they must be read THROUGH THE ND-100 MMU.

The harness now sweeps page tables and levels and only accepts a mapping that reproduces
six independently-known values in a row - `MNCTAB` (`0o11773`) is an array literally
containing the `NNCnn` patch-site addresses, so:

```
MNCTAB[0..5] must = NNC01..NNC06 = 0o022616 0o022647 0o022667 0o022747 0o133066 0o134051
                                 =  0x258E   0x25A7   0x25B7   0x25E7   0xB636   0xB829
```

**Result: 6/6 at pil=14, D-space/APT.** And on that mapping `ADRZERO` reads `0x0840` =
2112 - exactly the predicted page. The convention is validated.

### The measurement

```
NSAMSON    @ 0o11250  = 0x0000      <- number of Samson CPUs
N5CPU      @ 0o11247  = 0x0000      <- number of live ND-500 CPUs
5MBBANK    @ 0o4654   = 0x0000
5FPMAILBOX @ 0o111102 = 0x0000      <- mailbox never allocated
ADRZERO    @ 0o52047  = 0x0840      (= 2112, CORRECT)
```

`NSAMSON`, `N5CPU`, `5MBBANK` and `5FPMAILBOX` all read 0 in BOTH test phases and under
both mappings the sweep selected - they are the stable part of the measurement.

### What this means

**The root cause is earlier than BOTH section 7.6.8 and section 7.6.9.** SINTRAN has not
registered an ND-500/Samson CPU at all. Everything else follows mechanically:

```
N5CPU = 0
  -> INZ500 (5P-P2-MON60.NPL:027017) "IF N5CPU=0 THEN ENOCPU; GO FAR INZRET" bails out
  -> CALL 5GBUFF never runs      -> 5FPMAILBOX stays 0        [MEASURED 0]
  -> XMSINIT computes 5MBBANK = 5FPMAILBOX>>6 = 0             [MEASURED 0]
  -> 5ACTSWAPPER's CNVWADR gets bank 0 -> AD = 0x00008E30     [MEASURED, section 7.6.7]
  -> RIOM reads the wrong place, record stays zero, SWPFATAL 0o201
```

Section 7.6.8's arithmetic was right but it was a SYMPTOM. Section 7.6.9's patch mechanism
is still real, and `NSAMSON = 0` means `BYPASS` DID take the `-mudom` column - but it is
also a symptom of the same missing detection, not an independent fault.

The swapper still runs in our emulator only because the harness starts it DIRECTLY; SINTRAN
itself thinks there is no ND-500 to serve.

### The one thing that does not fit - do NOT ignore it

`ADRZERO = 0x0840` is CORRECT, and `ADRZERO` is set by `CHMEMDEF` (`5P-P2-MON60.NPL:026733`
`5D12=:ADRZERO`), which `INZ500` calls at `027023` - i.e. AFTER the `N5CPU=0` bail-out at
`027017`. If `N5CPU` had been 0 at that moment, `CHMEMDEF` could not have run and `ADRZERO`
would still be -1. So either:

1. `N5CPU` was non-zero earlier and something later zeroed it (note
   `RP-P2-N500.NPL:132206` `0=:NSAMSON=:N5CPU` re-zeroes both at the top of the CPU scan,
   then re-counts - a scan that finds nothing leaves both at 0), or
2. `ADRZERO` is also set on another path (`PH-P2-OPPSTART.NPL:2498` sets it from
   `CURRPAGE`).

Also flagged: in the `octobus-fullflow` phase the sweep matched the anchor at **pil=0**
and there `ADRZERO` reads `0xFFFF` (= -1). Six-value agreement on `MNCTAB` is therefore
NECESSARY BUT NOT SUFFICIENT - `MNCTAB`'s page is evidently shared across levels while
`ADRZERO`'s is not. **Add a second anchor on `ADRZERO`'s own page before trusting any
value read near it.**

### Next

Find why the CPU scan at `RP-P2-N500.NPL:132211-132255` counts nothing. It tests
`X.CPUAVAILABLE/\5CPUTYPE=SAMSON` over the `S5CPUDF..E5CPUDF` datafields, and
`CPUAVAILABLE` is stamped only by the octobus probe at `PH-P2-OPPSTART.NPL:063146-063201`:

```
063151   T:=100406; *IOXT; TRA IIC
063154   IF A=0 THEN                     % Octobus present? - (assumes Samson)
063155      DO *IOXT WHILE A NBIT 3 OD   % wait for data ready
063170      T:=100405; A\/CMMACLE; *IOXT % masterclear Samson system
063173      A:=X\/CMACONT; *IOXT         % continue accp
063201      CPUAVAILABLE/\140000\/SAMSON
```

Check `NDBusOctobus` against that sequence - IOX `100406`/`100405`, the `TRA IIC` result,
and the bit-3 data-ready poll. That is a static read of our own device code, no run needed.

## 7.6.12 The 5ALIVE / CMSYSPAR mechanism (2026-07-28)

**CORRECTED by section 7.6.13 - this is NOT the root cause of our SWPFATAL.** The
mechanism carved below is accurate and will matter later, but the conclusion originally
written here ("the ACCP never ACKs WriteSysPar") was WRONG. Read 7.6.13 before acting.

### Measured

With the MMU read convention proven (section 7.6.11, MNCTAB anchor 6/6, pil=14 D-space),
all four ND-500 CPU datafields read:

```
CPU0 df@0o52222 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
CPU1 df@0o52270 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
CPU2 df@0o52336 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
CPU3 df@0o52404 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
```

**The octobus presence probe WORKS.** `PH-P2-OPPSTART.NPL:063201`
(`CPUAVAILABLE/\140000\/SAMSON`) ran and stamped type 3 on every CPU. Section 7.6.9's
worry that the probe was failing is DISPROVEN - keep `NDBusOctobus` as it is.

What is missing is the **5ALIVE** bit (bit 0o15 = 13), and that is precisely what gates the
counter the whole chain hangs off (`RP-P2-N500.NPL:132274`):

```
132214   IF X.CPUAVAILABLE/\5CPUTYPE=SAMSON THEN ... MIN NSAMSON
132274   IF X.CPUAVAILABLE BIT 5ALIVE THEN MIN N5CPU FI     <- never taken
```

### What sets 5ALIVE - the actual requirement

`MP-P2-N500.NPL:3455-3470`, routine `5OMBREAD`:

```
5OMBREAD:  "LMFIELD"=:D; A:=DPITBANK
           X:=OCTORING; T:=5OMDNO; CALL OMBREAD; GO ERR
           IF "LMFIELD".MOCTSTATION>=FN5DEST AND A<=LN5DEST THEN
              X.ETYPE=:D SHZ -10=:CSTS; A:=D/\377=:CMICP
              % ErrorMessage or Ack/Nack from ACCP/Microprogram?
              X.MOCTSTATION; CALL FAR GN5CPUDF; GO I5OMBR
              IF CSTS=MFACK OR A=MFNACK THEN
                 % Ack/Nack answer on 'WriteSysPar' message
                 % i.e. I'm present
                 CPUAVAILABLE BONE 5ALIVE=:CPUAVAILABLE
```

So a CPU is marked alive ONLY when an **ACK or NACK to the `WriteSysPar` message** arrives
as an octobus message, read out of the OMD ring. Values (L07 symbols, 5-char names):

| Symbol | Stored as | Value | Meaning |
|---|---|---|---|
| `MFACK` | `MFACK` | `000000` | ACK - status 0 |
| `MFNACK` | `MFNAC` | `000377` | NACK |
| `FN5DEST` | `FN5DE` | `000070` | first ND-5000 CPU station |
| `LN5DEST` | `LN5DE` | `000077` | last ND-5000 CPU station |
| `5OMDNO` | `5OMDN` | `000000` | OMD number used for the read |

The reply must come from a station in `70B..77B` - the ND-5000 CPU range in the octobus
station map (`Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md`,
Appendix 2). `CSTS` is `ETYPE >> 8`; `CMICP` is `ETYPE & 0377`.

SINTRAN sends the message from `MP-P2-N500.NPL:3592` and `:3626`:

```
147107   CMSYSPAR SHZ 10\/N100IDENT=:X.MCOMMAND   % Send OMD numer to mf-controller
147155   CMSYSPAR SHZ 10\/N100IDENT=:X.MCOMMAND   % Send system par.
```

### This resolves a previously-open question

The "ACCP `CMSYSPAR` reply-OMD" item has been open in this effort for some time. It is now
answered: **the ACCP must reply to `CMSYSPAR` with an octobus message whose `ETYPE` high
byte is `MFACK` (0) or whose low byte is `MFNACK` (0o377), sent from the CPU's own station
in 70B-77B.** Our HLE servicer does not send it, so `5ALIVE` is never set.

### Corrected chain, end to end

```
ACCP never ACKs CMSYSPAR
  -> 5OMBREAD never sets 5ALIVE                        [MEASURED alive=False x4]
  -> N5CPU stays 0                                      [MEASURED 0]
  -> INZ500 (5P-P2-MON60.NPL:027017) bails on N5CPU=0
  -> 5GBUFF never runs -> 5FPMAILBOX = 0                [MEASURED 0]
  -> XMSINIT: 5MBBANK = 5FPMAILBOX>>6 = 0               [MEASURED 0]
  -> 5ACTSWAPPER's CNVWADR converts with bank 0
  -> SWPINFO = 0x00008E30, the raw 5MPM offset          [MEASURED, section 7.6.7]
  -> RIOM reads the wrong place, record stays zero
  -> SWPFATAL 0o201
```

Sections 7.6.8 (5MBBANK) and 7.6.9 (CNVWADR patched out) were both real observations but
both are SYMPTOMS of this. Fix the ACK and the whole chain should follow.

### Next

Implement the `CMSYSPAR` ACK in the HLE servicer, then re-measure `alive` and `N5CPU`
before touching anything else. NOT YET VERIFIED: the exact OMD/message layout the reply
must use, and whether `N100IDENT` in the command word has to be echoed back. Carve the
ACCP firmware's `CMSYSPAR` handler (octo.bin in Ghidra) for the reply format rather than
guessing it - guessing a wire format is what section 7.6.6 got wrong.

## 7.6.13 CORRECTION: INZ500 never ran - the ND-500 was never STARTED (2026-07-28)

Section 7.6.12 concluded that a missing `CMSYSPAR` ACK was the root cause. **That was
wrong**, and it was caught only by checking whether the scan had run at all before
implementing the ACK.

### Measured (both harness phases, mapping anchored 6/6 at pil=14 D-space)

```
5MSINIT @ 0o111100 = 0x0000    5CHALIVE=False   5ALBUF=False
```

`5CHALIVE` (`5CHAL`, bit 3) clear means **`INZ500` never ran**. `5CONOMD` - the CPU scan
that sets `NSAMSON`/`N5CPU` - is called only from `INZ500` (`5P-P2-MON60.NPL:027013`) under
`IF 5MSINIT NBIT 5CHALIVE`. And `INZ500` has exactly ONE caller: the **MON 60B dispatcher**
at `5P-P2-MON60.NPL:030452`, "INITIALIZE ND-500 SYSTEM IF NOT DONE".

**SINTRAN brings the ND-500 subsystem up only when a MON 60B function is actually issued.**
Booting alone never does it. Our harness starts the swapper DIRECTLY without ever issuing
one, so SINTRAN knows nothing about the ND-500.

### Therefore

`N5CPU = 0`, `NSAMSON = 0`, `5FPMAILBOX = 0`, `5MBBANK = 0` are all **never-initialised
zeros**, not faults. Sections 7.6.8, 7.6.9 and 7.6.12 each identified a real mechanism but
each mis-read an uninitialised value as a broken one.

**The tell that should have caught this three sections earlier:** every CPU datafield reads
`CPUAVAILABLE = 0x0003` (type = SAMSON). Had `5CONOMD` run, `NSAMSON` would have been **4**.
It was 0 - so the scan never happened. **A zero means both "counted nothing" and "never
ran". Distinguish those before diagnosing anything.**

This also re-frames an older note: `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`
section 6 already said the swapper is kicked without a real page-faulting ND-500 process
behind it. That was the same finding, from the other end.

### What is still valid from 7.6.12

The `5ALIVE` / `CMSYSPAR` carve itself stands and will matter once `INZ500` runs:
`CMSYSPAR=0o16`, `N100IDENT=1`, `MCOMMAND`/`ETYPE` share offset 4 (SINTRAN sends `0x0E01`),
`MFACK=0` vs `ETYPE>>8`, `MFNACK=0o377` vs `ETYPE & 0377`, reply from station
`0o70..0o77`; `CON5IDENT` (`MP-P2-N500.NPL:3617`) sends it to the CPU's own `5STATION` on
`OMDACCP=3`, length 7, expecting the reply on `5OMDNO=0`.

### Next - and do NOT implement an ACK before this

Make the harness issue a real **MON 60B** (start an ND-500 domain the way SINTRAN normally
would) so `INZ500` runs. Then re-measure `5MSINIT`, `NSAMSON`, `N5CPU` and the per-CPU
`alive` flags. Only if `5CHALIVE` is then SET and `N5CPU` still 0 does the `CMSYSPAR` ACK
become the live suspect.

## 7.6.14 THE ACTUAL BLOCKER: the octobus harness never drives the machine (2026-07-28)

Following 7.6.13's "`INZ500` never ran", the obvious next step was "make the harness issue a
MON 60B". It already tries to. It never gets the chance:

```
OUTCOME: ENTER=STALL login=STALL nd-500=STALL status=STALL start-swapper=STALL list=STALL stop-system=STALL
```

**Every step stalls, starting with the very first ESC -> "ENTER".** The login prompt never
appears, so `nd-500` is never typed, so no MON 60B is ever issued, so `INZ500` never runs.
7.6.13's measurement and this are the same fact seen from two ends.

**This is NOT caused by the diagnostic probes added today.** All four runs this session -
including the FIRST, before any edit - produced a byte-identical OUTCOME line. That is the
control; the probes are read-only and change nothing.

The boot itself is healthy. `Boot_To_SintranRunning_Octobus_DumpConsole` PASSES in 49 s and
the console reaches:

```
SINTRAN III - VSX/500 L
SINTRAN III RUNNING -
ERS3WD version B00 has started
```

and the ND-5000 parks correctly (`PC=0 stopMode=WAIT`, `MicroVersion=0x2E9A`,
`CpuParameter=0x03E1`). It is specifically the **interactive terminal path** that is dead:
`_machine.SerialDataOutput(ESC)` followed by a 30 s wait for `"ENTER"` never matches.

### Consequences for everything above

Sections 7.6.7 through 7.6.12 were measured on a machine that BOOTED BUT WAS NEVER DRIVEN.
The values are real and were read correctly (the MMU mapping is anchor-proven), but "SINTRAN
never registered an ND-500" simply describes a system that was never asked to. **Do not
treat any of those zeros as emulator defects.**

Note this does NOT explain the earlier `SWPFATAL 0o201` work, which required a swapper that
was actually running - so an earlier configuration of this harness clearly did drive the
machine. Finding what changed is part of the job.

### NOT ESTABLISHED - do not guess

Why the ESC never produces `ENTER` is UNKNOWN. Candidates, none tested:

1. The console/terminal wiring: the harness creates `TERM 5/6/7` and writes via
   `SerialDataOutput` / `ConnectionManager.OnActiveConsoleOutput`. Whether ESC reaches the
   terminal SINTRAN treats as its console has not been checked.
2. `startRunThread: true` (harness line ~201). The ND-5000 run thread is ON and its own
   comment documents a runaway risk. Not shown to be involved - the CPU parks correctly.
3. The writable pack `%TEMP%\retrocore-nd5000-octobus\BIGDISK0-L.IMG`, which a concurrent
   session has previously corrupted. The pack boots, so it is not badly damaged.

The 3022 sibling harness could not be used as an A/B: its tests SKIP in this environment
(no `[Explicit]`/`[Ignore]` on the test itself, so the skip comes from class/Setup - likely
a missing image). Establishing a working A/B is worth doing first.

### Next

Fix the login stall before ANY further ND-500 carving. Start by finding which device
SINTRAN uses as its console in this harness and confirming ESC reaches it - and get the
3022 harness runnable so there is a known-good comparison.

## 7.6.15 SOLVED: the ND-5000 RUN THREAD kills the ND-100 interactive path (2026-07-28)

### The A/B

The octobus harness setup is byte-for-byte the same as the working 3022 sibling (same
FX/SMD/HD, same TERM 5/6/7, same console wiring, same boot path) except for the card and
one thing: `AttachNd5000Cpu(..., startRunThread: true, ...)`. Made that switchable
(`RETROCORE_ND5000_RUNTHREAD=0`) and re-ran.

```
startRunThread=TRUE   OUTCOME: ENTER=STALL login=STALL nd-500=STALL status=STALL start-swapper=STALL list=STALL stop-system=STALL
startRunThread=FALSE  OUTCOME: ENTER=OK    login=OK    nd-500=OK    status=STALL start-swapper=STALL list=OK    stop-system=STALL
```

**The ND-5000 run thread was killing the ND-100 console.** Nothing about the octobus, the
ACCP, `CNVWADR`, or the CPU-presence probe was ever wrong.

### With the run thread OFF, SINTRAN initialises the ND-500 correctly

```
5MSINIT    @ 0o111100 = 0x000F   5CHALIVE=True  5ALBUF=True
NSAMSON    @ 0o11250  = 0x0004   <- FOUR SAMSON CPUs found
N5CPU      @ 0o11247  = 0x0001   <- ONE alive
5FPMAILBOX @ 0o111102 = 0x0851   <- 2129, mailbox ALLOCATED
5MBBANK    @ 0o4654   = 0x0021   <- 33 = 41B
ADRZERO    @ 0o52047  = 0x0840   <- 2112
```

**Section 7.6.8's arithmetic is VINDICATED.** It predicted `5FPMAILBOX >= 2113` and
`5MBBANK = 2113>>6 = 33 = 41B`. Measured: 2129 and 33. The reasoning was correct; it was
only ever applied to a system that had never been initialised.

**And `N5CPU = 1` means the CMSYSPAR/CON5IDENT ACK path WORKS** - a CPU really does get
marked `5ALIVE`. Section 7.6.12's "the ACCP never ACKs WriteSysPar" is doubly dead: wrong
as a root cause (7.6.13) and wrong on its own terms (here).

### Corrected status of the earlier sections

| Section | Claim | Status now |
|---|---|---|
| 7.6.8 | `5MBBANK` should be 41B | **CONFIRMED** - measured 0x21 |
| 7.6.9 | `CNVWADR` patched out because NSAMSON=0 | DEAD - NSAMSON is 4 |
| 7.6.11 | `N5CPU=0`, no ND-500 registered | Was true only of an un-driven machine |
| 7.6.12 | Missing CMSYSPAR ACK is root cause | **DEAD** - N5CPU=1, the ACK works |
| 7.6.13 | `INZ500` never ran | Correct, and 7.6.15 explains WHY |
| 7.6.14 | Harness never drives the machine | Correct - cause was the run thread |

### The real tradeoff, and the next problem

`startRunThread: true` exists deliberately (Ronny, 2026-07-21) so the functional `CpuND500`
actually EXECUTES the swapper over the octobus instead of the mailbox being answered inline.
So today the harness can have EITHER:

- **run thread ON** - the swapper can execute, but SINTRAN's console is dead, so the ND-500
  is never initialised and the swapper runs against an uninitialised system. **This is the
  configuration in which every previous `SWPFATAL 0o201` measurement was taken.**
- **run thread OFF** - SINTRAN initialises properly (above), `list` works, but the swapper
  does not execute.

That is almost certainly why the swapper saw a null `SWPINFO`: it was started on a system
where `5MBBANK` was 0 because `INZ500` had never run. **Re-measure the whole swapper chain
with the run thread OFF before treating any of it as a real defect.**

Remaining stalls even with the run thread off: `status`, `start-swapper`, `stop-system`
(`list` is OK). Those are the genuine ND-500 bring-up work and are now reachable.

### NOT ESTABLISHED

WHY the run thread kills the console. It is a second thread driving `CpuND500` alongside the
ND-100 run loop; whether it starves the ND-100, deadlocks on shared memory, or corrupts
interrupt state has NOT been determined. Fixing that properly is what would let both
configurations work at once - do not paper over it by leaving the thread off forever.

## 7.6.16 The NLL install and the octobus bring-up are the SAME blocker (2026-07-28)

Ronny directive: install ND-500 products by attaching the distribution floppy and running the
real install, NOT by pushing files in with ndtool.

### The 2026-07-19 "floppy crashes boot" note is DEAD

The 3022 harness records that mounting the NLL floppy on fd0 crashes SINTRAN boot (FloppyDMA +
media, hard host crash) - that crash is why the ndtool workaround was invented. **It does not
reproduce.** With the 210319 floppy attached on FD0 (`NDBusFloppyDMA`, created as "FX"):

```
NLL floppy attach FD0 = True (1,310,720 bytes) C:\Users\ronny\Downloads\ND-disk-00042.img
BOOT SURVIVED with the NLL floppy attached
DIR INDEX  0 : DISC-75MB-1   UNIT 0 : PACK-ONE
DIR INDEX 40 : FLOPPY-DISC-1 UNIT 0 : 210319H02-XX-01D
FILE 0..7 : all eight product files listed
```

### Mounting it: use the POSITIONAL form

```
@enter-dir,,f-d-1,0
```

(Ronny 2026-07-28.) Command and device abbreviate on character prefix, and the directory name
is SKIPPED as an empty positional so it comes from the floppy's own volume label - which keeps
it media-independent for the next product floppy.

**Trap:** skipping the name as a POSITIONAL works; answering the interactive `DIRECTORY NAME:`
prompt with a bare CR does NOT. The prompt form returns to `@` with NO error and enters
nothing - the failure only surfaces later as `NO SUCH FILE NAME` from LIST-FILES.

Also: `LIST-DIRECTORIES-ENTERED` takes `<directory name> <output file>` (ND-60.050.06 SINTRAN
III Users Guide 6.2.3). Typed bare it prompts, and swallows the NEXT command line as its
answer. Use `list-directories-entered,,terminal`.

### Where the install actually stops

Route B (`Installation\ND500-PROGRAM-INSTALL-LINKAGE-LOADER-210319.md` section 5.2), run with
`RETROCORE_ND5000_RUNTHREAD=0`:

```
@nd-500                                    OK
      ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17
      ND-5000 timeout:  ACCP was terminated; Microprogram has stopped
ND-5000: recover-domain (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02
      > Loading Control Store
      > Loading Swapper                    <- STALL
```

`RECOVER-DOMAIN` does not fail. It reaches CS load and then stalls **loading the swapper**.

### Why this matters: it is the run-thread conflict, again

Installing the loader needs SINTRAN INITIALISED and the swapper EXECUTING at the same time.
Section 7.6.15 established those are currently mutually exclusive:

- run thread ON - the swapper can execute, but ND-100 terminal INPUT is dead, so
  `recover-domain` can never be typed.
- run thread OFF - the console works and SINTRAN initialises, but no ND-500 instruction
  executes, so "Loading Swapper" can never complete.

**One fix unblocks both the octobus bring-up and every ND-500 product install.** This is now
the single highest-value item.

### Narrowing on that fix

With the run thread ON, console **OUTPUT works** - the boot banner, `SINTRAN III RUNNING` and
`ERS3WD version B00 has started` all appear. Only INPUT fails. So it is NOT CPU starvation and
NOT lock contention:

- `MpmWindow.SyncRoot` is taken only for semaphore test-and-set/release, never for ordinary
  MPM reads or writes, so plain ND-100 memory access cannot contend on it.
- The CPU was PARKED (`PC=0 stopMode=WAIT`), not running away.

**HYPOTHESIS, NOT MEASURED:** the terminal input path is interrupt-driven on level 12, the same
level the octobus station uses for idents and doorbells, and
`ServiceMailboxOnClock = !startRunThread` (`ND100Machine.ND5000.cs:160`) moves that work
between the ND-100 clock and the CPU thread. Instrument level-12 delivery and terminal input in
BOTH configurations and compare - do not act on this until measured.

Also new, and unexplained: with the run thread off the monitor reports
`ND-5000 timeout: ACCP was terminated; Microprogram has stopped` on entry, BEFORE any command.

## 7.6.17 The swapper is never ASKED to run - SINTRAN livelocks on the micro-version check (2026-07-28)

With SINTRAN properly initialised (`N5CPU=1`, `5MBBANK=41B`, `5FPMAILBOX=0x851`) and the run
thread ON, `start-swapper` still stalls. The reason is now measured, and it is NOT the swapper.

### The CPU is never activated

After `start-swapper`:

```
ND-5000 PC=0x00000000 stopMode=WAIT      <- never left WAIT
PSTP=0x0003A000  CTXBASE=0x0002A000      <- but SINTRAN DID build these
context block @0x0002A100: +0x00 P = 0x08000004   <- entry point present
PST: 3 mapped segments (PSN 1,2,3), 61 zero
```

SINTRAN prepares the context correctly and then never activates. **No `3START` (MICFU 0x13)
is ever sent** - the whole servicer MICFU trace is only `3RMICV`, `CACHE`, `PHYSWR`.

### It is a LIVELOCK, not a hang

The doorbell walk shows the same two messages cycling forever:

```
activation#11  @0x00428E30 N5STA=0x0001 MICFU=0x0019   MSGN5,  PHYSWR - needs service
activation#12  @0x00428E30 N5STA=0x0003 MICFU=0x0019   ANSWE - we answered it
activation#13  @0x00428E30 N5STA=0x0001 MICFU=0x0019   posted AGAIN
...20+ activations, alternating with @0x0042C130 MICFU=0x0001 (3RMICV)
```

So our servicer IS answering (state 1 -> 3) and SINTRAN keeps re-posting. It is dissatisfied
with the answer and retries indefinitely.

### What SINTRAN believes

Even with the run thread ON, the monitor banner says:

```
ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17
ND-5000 timeout:      ACCP was terminated; Microprogram has stopped
```

We report `MicroVersion=0x2E9A`, `CpuParameter=0x03E1`.

**LEAD (not yet proven):** SINTRAN reads the micro version via `3RMICV`, judges the
microprogram wrong or stopped, re-loads and re-checks, and therefore never reaches the
activation step. That is the long-standing "Wrong microprogram" thread, and it now has a
concrete symptom chain attached to it.

### Ruled OUT by measurement, do not re-investigate

- **Mailbox address is CORRECT.** `5FPMAILBOX = 0x851` (2129 pages); `0x851 << 11 = 0x428800`
  = the self-discovered header EXACTLY; `+ cpu*256 = 0x428900` = the discovered ext block;
  `+ 0x0A = 0x42890A` = X5ACT. Activation is not aimed at the wrong place.
  (Two bugs previously hid this: the carved check used RAW PHYSICAL reads of resident symbols,
  which always return 0, AND it added `MpmStart` on top of an already-absolute page address.)
- **The link works.** 914 station->ND-100 frames were read by SINTRAN in the fullflow run.
- **PHYSWR is answered.** The servicer performs the copy and posts the answer plus the GIVEINT
  ring insert; the state transition 1 -> 3 is observed live.
- **IO-device Clock() exceptions are NOT involved.** Isolation was added and measured to catch
  ZERO faults, and an A/B with isolation disabled behaves identically.

### Next

Find what `3RMICV` must return for SINTRAN to accept the microprogram, and compare against
`MicroVersion=0x2E9A` / `CpuParameter=0x03E1`. Until that check passes, nothing downstream -
swapper, domains, NLL install - can proceed.

### 7.6.17a CORRECTION: 3RMICV is the WATCHDOG, not a version-acceptance check

Section 7.6.17 named `MicroVersion=0x2E9A` / `CpuParameter=0x03E1` as the prime suspect. That
was wrong about the mechanism. `MP-P2-N500.NPL:1209` (`LCLTSB`) shows `3RMICV` is stamped into
the **WATCHDOG** buffer and sent WITH A TIMER ARMED:

```
136644   X:=WATCHDOG
136645   CALL XTER500; 0/\0
136647   3RMICV; T:=5MBBANK; *MICFU@3 STATX
136652   MSGN500; CALL WN5STATUS
136654   CALL ITO500XQ; X=:TMRXQ; LTTMR=:TMR      <- watchdog timer
136660   CALL XACT500
```

This matches the earlier carve (`CARVE-ANSWER-3022-FLAG-POLL-RING-2026-07-19.md`): the
watchdog buffer is permanently stamped `MICFU=3RMICV=1`.

So the repeated `3RMICV` in the doorbell walk is the **watchdog retrying**, and
`ND-5000 timeout: ACCP was terminated; Microprogram has stopped` is that TIMER EXPIRING - not
a rejected micro version. The micro-version values are NOT implicated by this evidence.

**Suspect is now the watchdog answer path** (`XTER500` / `ITO500XQ` / `XACT500` and what
SINTRAN requires to consider the watchdog answered in time), not `3RMICV`'s payload.

## 7.6.18 ROOT CAUSE: 5ALIVE is never set on a SAMSON machine (carved 2026-07-28)

Status: VERIFIED from NPL source. Supersedes the "superseded" note on 7.6.12 - see the
retraction below.

### The 3START emit path

`3START` is produced in exactly ONE place, `MP-P2-N500.NPL:428` `SWMESS:`:

    SWMESS:                                        % dispatch entry 05 = 3SWMESS
       IF 5INITFLAG BIT BRESPLACE GO FAR EEIRESPL
       T:=5MBBANK; *SWFUN@3 LDATX                  % A := X.SWFUNC
       IF A=MSWSTART THEN                          % Start swapper
          ...
          3START; *MICFU@3 STATX                   % line 436

`SWMESS` is entry 5 of the `N5XXC` dispatch table (`MP-P2-N500.NPL:393-409`), reached only
from `N500C` - the handler that runs when an ND-100 process issues an ND-500 monitor call.
So `3START` is a TRANSLATION, not a decision: a process asks with `MICFU=3SWMESS` +
`SWFUNC=MSWSTART`, and the kernel rewrites it to `MICFU=3START` and queues it with `ITO500XQ`.

`MSWSTART` is TESTED in one place and SET in zero - across all 45 NPL files AND the full
3.9MB `s3vs-4.symb`. The producer is userland (the ND-500 Monitor subsystem), not the kernel.

### The gate that stops it

`N500C:362`:  `IF CPUAVAILABLE NBIT 5ALIVE GO FAR EN5TIMOUT`

Symbols (L07 SYMBOL-1-LIST): `5ALIV=000015` (bit 13 = 0x2000), `CPUAV=000027` (a FIELD OFFSET
inside the per-CPU descriptor `S5CPUDF`, confirmed by `5P-P2-MON60.NPL:1857`).

Measured on the harness: `CPUAVAILABLE = 0x0003` = SAMSON type, bit 13 CLEAR.

### Why it is clear - the boot scan does not set it for SAMSON

`PH-P2-OPPSTART.NPL:3908` `CH5CPUPRESENT`, the boot-time CPU scan, has two branches:

    IF A=0 THEN                          % old ND-500: IOX to HDEV+RSTA5 answered
       CPUAVAILABLE/\140000\/OLD500
       A BONE 5ALIVE                     % alive IMMEDIATELY
    ...
       IF A=0 THEN                       % Octobus present - "assumes Samson"
          ...ASTATION\/COMD=:5STATION
          ...send CMMACLE + CMACONT (master-clear + start ACCP selftest)...
          CPUAVAILABLE/\140000\/SAMSON   % 5ALIVE deliberately NOT set

An OLD500 is alive by inspection. A SAMSON must PROVE it, by answering.

### The only writer of 5ALIVE

`MP-P2-N500.NPL:3459` `5OMBREAD`, reaching line 3470:

    IF "LMFIELD".MOCTSTATION>=FN5DEST AND A<=LN5DEST THEN      % gate B: station 70B..77B
       X.ETYPE=:D SHZ -10=:CSTS; A:=D/\377=:CMICP
       X.MOCTSTATION; CALL FAR GN5CPUDF; GO I5OMBR             % gate C: silent skip
       IF CSTS=MFACK OR A=MFNACK THEN                          % gate D
          CPUAVAILABLE BONE 5ALIVE=:CPUAVAILABLE               % line 3470

Both senders document this in their own headers: `MFPREPARE:3581` and `CON5IDENT:3609` each
say "Ack/Nack answer is handled by 5OMBREAD". `CON5IDENT:3606` states the intent outright:
"Send 'alive' message to the ACCP to verify that it's present".

`GN5CPUDF` (`:3545`) matches the reply's source station against each descriptor's `5STATION`
field, and `5STATION` was set at boot by `PH-P2-OPPSTART.NPL:3929` as `ASTATION\/COMD` - the
hardware station readback OR the per-CPU index. Not found = plain `EXIT` = `GO I5OMBR`, which
looks EXACTLY like "no reply arrived". This is the quiet failure mode.

### Consequence

With `5ALIVE` clear, `N500C` bails at its first line. `SWMESS` is unreachable no matter who
asks, so `3START` can never be emitted. The clear+verify of ND-500 physical 0x96..0xC4
followed by watchdogs forever is `RP-P2-N500.NPL:384` (`3RMICV; ... X:=WATCHDOG`) doing
exactly what it should when there is no work - not a stall.

This also means 7.6.16 was right that the swapper blocker and the NLL install blocker are the
same item, but the shared cause is one level lower than assumed: neither can proceed until
5ALIVE is set.

### RETRACTION of the 7.6.12 correction

I previously marked 7.6.12 ("missing CMSYSPAR ACK is the root cause") as SUPERSEDED, arguing
that `N5CPU=1` proved the ACK path worked. That reasoning was wrong: `N5CPU` counts the CPUs
the BOOT SCAN found, and says nothing about aliveness. 7.6.12 was correct. The supersede note
on 7.6.12 is withdrawn.

### Emulator side - what is and is not implemented

There is exactly one ND-500-family station class, `OctobusND5000Station.cs:61` (misleading
name; it IS the production station, it takes a `CpuND500`). It DOES implement the presence
ACK: `command == 0x0E` (CMSYSPAR = 016B) calls `SendAccpMessack(message[4])` at line 1794,
and `SendAccpMessack:1981` sends a 2-byte all-zero payload with `sourceOmd: 3` (OMDACCP).
Payload[0]=0 satisfies gate D (MFACK).

So the ACK is written but is not arriving in a form `5OMBREAD` accepts. Four candidate gates,
in code order - UNVERIFIED which one:

  A. the reply never leaves the station (handler not reached at all)
  B. source station outside FN5DEST..LN5DEST (70B..77B)
  C. `GN5CPUDF` finds no descriptor whose 5STATION matches - SILENT skip
  D. `ETYPE>>8 != MFACK(0)` and `ETYPE&0xFF != MFNACK(377B)`

Observation, NOT yet linked to the above: nothing in the production machine registers an
octobus station at 2-7, so there is no MFbus controller. `MFPREPARE` targets the MF
controller while `CON5IDENT` targets the ACCP - two different destinations. Both are
"Activated from level 1 (5pit)", so they may be independent activations; whether the missing
MF controller blocks the ACCP path is NOT determinable from the source and must be measured.
The real ACCP firmware selftest capture also reports "MFbus controller not found at Octobus
stations 2-7", so its absence may well be normal.

### Next measurement

One instrumented run logging, for every octobus multibyte the ND-100 station receives:
source station, OMD, ETYPE, and which of gates A-D it dies at. That single verdict names the
fix. Probe must capture values where they are already in hand - re-reading memory from inside
a servicer callback has crashed the test host twice.

## 7.6.19 RETRACTION of 7.6.18 - 5ALIVE IS set, and 3START IS sent (measured 2026-07-28)

Status: MEASURED. Section 7.6.18's ROOT CAUSE claim is WITHDRAWN. Its source carve is kept
(see "what survives" below), but the conclusion built on it was wrong on BOTH premises.

### Premise 1 was wrong: 5ALIVE is set

Harness run, run thread OFF, per-CPU descriptor scan:

    CPU0 df@0o52222 CPUAVAILABLE=0x2003 type=3 (SAMSON) alive=True
    CPU1 df@0o52270 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
    CPU2 df@0o52336 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
    CPU3 df@0o52404 CPUAVAILABLE=0x0003 type=3 (SAMSON) alive=False
    5MSINIT@0o111100=0x000F 5CHALIVE=True 5ALBUF=True
      -> OK: 4 SAMSON CPU(s), 1 alive -> ND-500 subsystem initialised.

CPU0 has bit 13 SET. The ACCP presence ACK works. The "CPUAVAILABLE=0x0003" figure I built
7.6.18 on was read from CPU1-3 - the three PHANTOM CPUs that do not exist in this machine -
and I generalised it to the whole system without checking which descriptor it came from.
`N500C:362` PASSES. It was never the gate.

### Premise 2 was wrong: 3START is emitted, 63 times

MICFU codes counted across the run:

    119 x 0x0000    92 x 0x0001 (3RMICV watchdog)    63 x 0x0013 (3START)
     62 x 0x0005 (3SWMESS)      26 x 0x0019           24 x 0x0018     1 x 0x000A

`3SWMESS` (05) arrives 62 times and `3START` (0x13) is produced 63 times - exactly the
translation `SWMESS` (`MP-P2-N500.NPL:436`) is supposed to perform. The ND-500 Monitor DOES
issue the request and SINTRAN DOES rewrite and queue it. `start-swapper` now reports OK.

### The actual state

    OUTCOME: ENTER=OK login=OK nd-500=OK status=STALL start-swapper=OK list=STALL stop-system=STALL
    [after start-swapper] ND-5000 st=56 PC=0x00000000 stopMode=WAIT
      PSTP=0x0003A000 CTXBASE=0x0002A000 PS=0 CED=0 CAD=0 THA=0
    context block @0x0002A100: P=0x08000004, ST1=0x00000002, PS=0x48480003, 22 of 25 fields zero
    physical segment table: PSN 1,2,3 mapped, 61 unmapped

So the blocker is NOT "nobody asks" and NOT "the gate rejects it". SINTRAN asks 63 times, the
servicer receives MICFU 0x13, and the ND-500 CPU never leaves `stopMode=WAIT` with `PC=0`.

**The bug is on OUR side, in the servicer's handling of MICFU 0x13 (3START): it does not
start the CPU.** That is the next thing to look at.

### What survives from 7.6.18

The source carve is still correct and still useful; only the conclusion was wrong:
 - `SWMESS` (`MP-P2-N500.NPL:428`) is the ONLY producer of `3START`, dispatch entry 05.
 - `MSWSTART` is tested once and set nowhere in the kernel - the requester is the userland
   ND-500 Monitor. (Confirmed empirically: the requests DO arrive.)
 - The `N500C` gate list (5ALIVE / MSGN500-or-WAITING / FERROR / BRESPLACE) is accurate.
 - The 5ALIVE mechanism (SAMSON must earn it via 5OMBREAD; `GN5CPUDF` fails SILENTLY) is
   accurate and worth keeping - it just is not what is broken here.

### Lesson

Two published root causes in a row died on the same mistake: reading a value without
establishing WHICH instance it came from. 7.6.12 died on a raw-physical read that returned a
zero indistinguishable from "unset"; 7.6.18 died on a per-CPU field read from the wrong CPU.
Before building on any measurement, state which descriptor/page/CPU it was taken from.

## 7.6.20 The SINTRAN side is COMPLETE - the only blocker is the run-thread/console race

Status: MEASURED, same run as 7.6.19.

### The 3START message is queued AND taken

Mailbox walk entries for the swapper start message (all at ND-100 byte 0x00428D30):

      1 x  N5STA=0x0001 MICFU=0x0013 link=0xFFFFFFFF   queued, ready, end of chain
     62 x  N5STA=0x0002 MICFU=0x0013 link=0x00008E30   WAITING, from then on

The `1 -> 2` transition is the servicer's documented take-path
(`Nd500MicrocodeServicer.cs:705-786`): "when a ProcessHost takes the start, the message stays
WAITING(2) and the process's next STOP answers it (AnswerProcessStop) - the microcode's
answer-in-place model". The message never leaves WAITING because the process never stops, and
it never stops because this run had `startRunThread=False` - the ND-500 CPU had no thread.

Two distinct MICFU printers exist and must not be conflated:
 - `MICFU=0x%04X` (4 digits) = `OctobusND5000Station.cs:1164`, a MAILBOX WALK dump (what is
   sitting in the queue).
 - `MICFU=0x%02X` (2 digits) = the harness `OnServicerMessage` callback
   (`Nd100SintranNd5000OctobusBootHarnessTests.cs:408`) = what the servicer PROCESSED.
Serviced this run: 0x01 (x90, watchdog), 0x18 (x12), 0x19 (x13), 0x0A (x1). NOT 0x13, NOT
0x05 - which is why a naive count made it look like 3START was never handled.

### UNRECONCILED

The `START-SWPINFO` log in that same handler is NOT inside `#if DEBUG_DETAIL`, yet produced
zero lines. Either `host.ServicerLog` does not reach the captured stream, or the start was
taken somewhere other than this handler. The state transition and the missing log DISAGREE
and this has NOT been resolved. Do not build on "the handler ran" until it is.

### Conclusion: the SINTRAN half is done

Verified working end to end this run: 5ALIVE set (CPU0 = 0x2003), 5MSINIT=0x000F (INZ500
ran), the ND-500 Monitor issues 3SWMESS (62x), SWMESS translates it to 3START, the message is
queued and taken, PSTP/CTXBASE/context block all built (P=0x08000004, 3 PST segments mapped).
`start-swapper` reports OK.

`PC=0 stopMode=WAIT` is NOT a failure - it is the direct consequence of the knob we set.

### The one remaining blocker

`RETROCORE_ND5000_RUNTHREAD`:
  ON  -> the ND-500 CPU executes, but the run thread kills the ND-100 interactive console,
         login stalls at the first ESC, no MON 60B, INZ500 never runs, SINTRAN uninitialised.
  OFF -> SINTRAN initialises fully (everything above), but nothing executes.

Both halves work. They cannot currently run at the same time. This race - previously recorded
as "masked, not fixed" and demoted to a robustness note after the Clock()-exception A/B came
back negative (0 faults caught, behaviour identical with isolation disabled) - is now THE
critical path. Nothing else blocks the swapper.

## 7.6.20a RESOLVED: the missing START-SWPINFO line was a logging-level artefact

The "UNRECONCILED" item in 7.6.20 is closed. `Nd500MicrocodeServicer` emits that line via
`host.ServicerLog(...)`; `OctobusND5000Station.cs:1258` implements it as
`void IServicerHost.ServicerLog(string message) => Log(message)`, and `Log` (`:375`) calls
`Logger.Log(..., Logger.LogLevel.Device)` - device-level tracing, which the harness does not
enable. So the line was never emitted regardless of whether the handler ran.

It is NOT evidence that the start handler was skipped. The `N5STA` 1 -> 2 transition on the
0x00428D30 message stands unopposed: the start WAS taken. 7.6.20's conclusion is unaffected.

Note for future probes: `ServicerLog` is invisible at default log level. Use a channel the
harness actually captures (e.g. the `OnServicerMessage` callback, or a public bounded list
like `Servicer.CopyLog`) when a diagnostic must show up in a normal run.

## 7.6.21 MILESTONE: the swapper RUNS - run thread ON, SINTRAN initialised, no faults

Status: MEASURED 2026-07-28, `RETROCORE_ND5000_RUNTHREAD=1`.

    ----- ND-5000 attach: startRunThread=True -----
    OUTCOME: ENTER=OK login=OK nd-500=OK status=STALL start-swapper=OK list=OK stop-system=STALL

ND-5000 PC across the run:

    [after start-swapper]           PC=0x00000000 stopMode=WAIT
    [after status]                  PC=0x00000000 stopMode=WAIT
    [after process-status]          PC=0x00000000 stopMode=WAIT   (first)
    [after process-status]          PC=0x08000687 stopMode=WAIT   (later)
    [after who-is-on]               PC=0x08000687 stopMode=WAIT
    [after list-active-processes]   PC=0x08000687 stopMode=WAIT
    [after list-domain]             PC=0x08000687 stopMode=WAIT
    [after list-standard-domains]   PC=0x08000687 stopMode=WAIT
    [after version]                 PC=0x08000687 stopMode=WAIT

**The swapper executes.** It enters at the context-block P=0x08000004 and advances to
0x08000687 (~1667 bytes of code), then parks in WAIT. Across all 12 state dumps:

    lastPageFault=(none)  lastDoubleFault=(none)  lastProtectViolation=(none)   x12

No page faults, no double faults, no protect violations, no SWPFATAL, no runtime exceptions
(the 10 "Exception" grep hits are all CS8618 build warnings). Clean execution.

Serviced MICFU this run: 0x01 x30 (watchdog), 0x18 x12, 0x19 x13, 0x0A x1.

### 7.6.20's "one remaining blocker" is WITHDRAWN

I had just promoted "the run thread kills the ND-100 console" to THE critical path. It does
not reproduce. With the run thread ON: ENTER=OK, login=OK, nd-500=OK, and `list` is OK where
it was STALL with the thread OFF. The console-death behaviour recorded earlier in this
session is GONE.

I do NOT know why. Candidates, none verified: other work landed in the repo between the
observations; the original stall was a different fault misattributed to the run thread; or it
is genuinely intermittent and this run got lucky. **A single passing run does not prove a
race is fixed** - this needs repeat runs before anyone calls it dead.

### What is actually left

    status=STALL        stop-system=STALL

Both are ND-500 Monitor commands that do not complete. Everything else in the chain works:
5ALIVE set, INZ500 run, 3SWMESS -> 3START translation, message queued and taken, swapper
entered and executing without faults.

### Running score of premises I published and then had to withdraw this session

  1. "missing CMSYSPAR ACK is root cause"  (7.6.12) - withdrawn, then REINSTATED, then
     withdrawn again on measurement. The ACK works.
  2. "5ALIVE is clear, that is the gate"   (7.6.18) - withdrawn. Read from the wrong CPU
     descriptor (CPU1-3 are phantoms; CPU0 = 0x2003, alive).
  3. "3START is never sent"                (7.6.18) - withdrawn. Sent 63 times.
  4. "the run thread kills the console"    (7.6.20) - withdrawn. Does not reproduce.

Every one of these died on a measurement I could have taken first. The pattern is carving
ahead of measuring. Measure, THEN carve to explain the measurement.

## 7.6.22 CORRECTION to 7.6.21: the run is NOT clean - "ND-500(0) CPU locked"

Status: MEASURED, same run-thread-ON run as 7.6.21.

7.6.21 called the run clean on the strength of "no page faults, no double faults, no protect
violations, no exceptions". That was incomplete - I had not read the console transcript. It
contains, twice (lines 6041 and 6056, wrapped so the leading "Fat" is on the previous line):

    al system error - ND-500(0) CPU locked

i.e. **"Fatal system error - ND-500(0) CPU locked"**. The swapper runs, but SINTRAN then
declares the CPU locked. The absence of CPU-level faults is real, but it does NOT mean the
run succeeded.

### What the evidence supports

  - The swapper takes the start: `N5STA` 1 -> 2 (WAITING) on the 0x00428D30 message.
  - It executes from P=0x08000004 to PC=0x08000687 and parks in `stopMode=WAIT`.
  - Per the servicer contract, a taken start stays WAITING until the process STOPS and
    answers (`AnswerProcessStop`).
  - The swapper parks WITHOUT answering. The ND-500 Monitor times out and reports the CPU
    locked.

So the swapper is BLOCKED WAITING on something rather than completing its work. That is the
real remaining defect. Where exactly it parks (PC=0x08000687) is the next thing to identify.

The "CPU locked" string is NOT in the NPL kernel sources - consistent with the earlier finding
that the ND-500 Monitor is a USERLAND subsystem (`MSWSTART` is tested in the kernel and set
nowhere in it). Its message table lives in the Monitor binary, so the trigger condition must
be found by other means than a kernel carve.

### On the two OUTCOME flags

`status=STALL` and `stop-system=STALL` still need care. The stop-confirmation text IS present
in the transcript (line 6486: "WAIT instruction ran with IONI off. CPU stopped after the WAIT
instruction. RUN indicator is off." - all three markers the harness waits for), and
`@stop-system` is issued TWICE (6484, 6600). That pattern suggests the harness's wait window
misses text that did arrive. I nearly published "both remaining STALLs are harness artefacts"
on that basis - and it would have been wrong to do so while the CPU-locked error was sitting
unread in the same transcript. The flags are NOT yet explained; the CPU-locked fault has to be
understood first, because it plausibly causes both.

`list-active-processes` - which the harness comments call "THE success metric" - does pass.

---

## 7.7 2026-07-29 - THE HARNESS WAS LYING: test contamination, and the real SWPFATAL 201B

### 7.7.1 Root cause of every STALL flag: NUnit fixture reuse (FIXED)

NUnit reuses ONE fixture instance for all tests in a class, so harness INSTANCE FIELDS survive
into the next test. `Pump()` keys off one of them:

    uint pc = _pumpedOnce ? 0u : (uint)_bootAddress;

so the SECOND test in a run gets a brand-new machine that is NEVER pumped at `_bootAddress`.
`TearDown` also never stopped the machine, never unsubscribed `Servicer.MessageProcessed`, and
never cleared the console buffer.

MEASURED effect: with `Boot_To_SintranRunning_*` running FIRST, the next test's ND-100 console
can PRINT but cannot ACCEPT KEYSTROKES - login stalls at the ESC and every later step reports
STALL. The SAME test run ALONE passes. This matches the documented ND-100 design: terminal
OUTPUT is synchronous with the IOX write, terminal INPUT is delivered only by
`NDBusTerminal.Clock()` raising the level-12 interrupt.

**Everything measured through a paired run is INVALID.** That includes the run-thread theory of
7.6.20 and the "swapper runs clean" milestone of 7.6.21.

Bisect that established it (clean `HEAD` worktree, one file group at a time): NO source change
breaks the console. Not `CpuND100.cs`/`ND100Machine.cs`, not the console-history refactor
(`MachineBase`/`HistoryBufferedConsole`/`TerminalBase`), not the NDBUS/servicer files, not the
new untracked sources. The 3022 harness - which has NO octobus card at all - fails identically,
which is what proved it was never an octobus or ND-500 problem.

Fixed in `TearDown` of BOTH harnesses (stop the CPU, unsubscribe, null the refs, reset
`_pumpedOnce`, clear the console).

Two incidental findings: `HEAD` (b7f45162b) does NOT compile - `OctobusND5000Station.cs:2036`
calls `MpmWindow.ReadDouble`, which exists only in the uncommitted working tree. And every
harness timeout is HOST WALL-CLOCK, so "STALL" conflates "never happened" with "not within N
seconds"; `RETROCORE_HARNESS_TIMEOUT_SCALE` (default 1.0) now exists to separate them. At scale
4 the contaminated console still produced nothing, which is how "dead, not slow" was settled.

### 7.7.2 The REAL blocker: ERROR CODE 201B = SWPFATAL, and why

With isolation fixed, the console reports what the machine actually thinks:

    FATAL   * 21B:77B * ... * ND-500(0) Monitor Internal
     *** FATAL SYSTEM ERROR ***
    ND-500(0) error:      Fatal error from Swapper
    ERROR CODE:         201B

The mechanism is carved, not guessed:

- `MP-P2-N500.NPL:808-813` - for `MICFU = 3START/3MONCO/3TRACO/3WMONCO` with `STOPR = MOCALL`,
  SINTRAN calls `MCHANDLE`.
- `MP-P2-N500.NPL:1273` - `SYMBOL N5SWAP = 377` "MON.CALL USED BY THE SWAPPER". The swapper's
  own monitor call is dispatched through `SWPDECODER`.
- `MP-P2-N500.NPL:912-919` - `SWPDECODER` switches on the message's `SWPFU` field:

        T:=5MBBANK; *AAX SWPFU; LDATX          % Swap-function
        IF A >> SWFMAX GO FAR ESWPFATAL
        A GOSW  FAR ESWPFATAL, LNEWSWAP, FAR LSWPAGE, FAR LPRSUSPEND,
                FAR LALLOPAGE, FAR LDATREADY, FAR LCLTSB;

- `MP-P2-N500.NPL:438` - `SWMESS` writes `SWACTIVE` into `SWPFU` when it starts the swapper.
- Symbol values (from `SYMBOLS/`, 5-char truncation, all three versions agree unless noted):
  `SWACTIVE = 0`, `SWPFU = 0o101`, `SWFMAX = 6` (L07/K03; M06 has 7), `MSWSTART = 7`.

So **GOSW index 0 IS the fatal slot**. Answering the swapper's message while `SWPFU` is still
the 0 that `SWMESS` wrote produces `ESWPFATAL` = "Fatal error from Swapper" exactly. A real
swapper must set a swap function (1..6) before its MON 377B.

`who-is-on` answers normally, so the ND-500 Monitor itself is healthy - it is specifically the
swapper's reply that is malformed. The servicer's own instrumentation already reports
`SWMSG.SWPINFO = 0` at start ("no work posted"), and its comment names the missing piece: the
idle-on-no-work gate.

### 7.7.3 What now blocks measurement: run-to-run nondeterminism

Same binary, same command line, isolated single test, `RETROCORE_ND5000_RUNTHREAD=1`:

- Run A: `start-swapper` OK, `PC` advances to `0x08000687` (`PS=3`), fatal 201B printed, test
  PASSES in 4.2 min.
- Run B: `start-swapper` STALL, `PC = 0x00000000`, NO start message processed at all
  (`startTaken=False`, `startCtx=0`), test host CRASHES.

The crash produces no managed exception, no `octobus-runthread-crash.txt` breadcrumb and no
dump even under `--blame-crash` - consistent with a hard death (stack overflow / access
violation). It is INTERMITTENT, not deterministic: the identical command line has both passed
and crashed.

`RETROCORE_ND5000_RUNTHREAD=0` cannot be used to dodge this: `DrainDoorbells` is wired ONLY
when the run thread starts, so with the thread off NO mailbox message is ever processed
(`startCtx=0` for the whole run). The thread is required for any swapper measurement.

New servicer diagnostics for this (all read-only): `LastStartX5Cpu`, `LastStartContextAddress`,
`LastStartTaken`, `LastAnswerMonNumber`, `LastAnswerSwpfu`, surfaced in the harness state line
as `startCtx / startX5CPU / startTaken / ansMON / ansSWPFU`.

**Next:** root-cause the nondeterminism (start message sometimes never delivered/taken) before
implementing the idle-on-no-work gate - an intermittent harness cannot validate the fix.

### 7.7.4 The swapper is REACHED and its fatal is its OWN - error 201B, from an EMPTY list

Full chain verified working (isolated run, `RETROCORE_ND5000_RUNTHREAD=1`):

    startSeen=1 startMicfu=23B swpInfo=0x00008E30 swMsg=0x00428D30
    startCtx=0x0002A100 startX5CPU=0 startTaken=True
    ansMON=377B ansSWPFU=2047B ansP=0x08000687 ansArgc=2
    ansArg0=0x427(=2047B) ansArg1=0x81(=201B) PS=3

So SINTRAN posts 3START, the servicer TAKES it, the context slot 0x0002A100 is correct, REAL
WORK is attached, the swapper runs to P=0x08000687 and calls MON 377B (N5SWAP) passing
(SWPFATAL=2047B, 201B). `SWPDECODER` then rejects 2047B > SWFMAX(6) and prints ERROR CODE 201B.
Nothing between SINTRAN and the swapper is wrong: the swapper decides it has failed.

TWO EARLIER CLAIMS DIED HERE, both mine:
 - "SWPINFO = 0, no work posted" was a BYTE-ORDER BUG IN THE DIAGNOSTIC. The ND-100 stores a
   32-bit value HIGH WORD FIRST; the halves were composed backwards, so a valid 0x00008E30
   (the message at byte 0x00428E30, visible in the MICFU trace) read back as 0x8E300000 and the
   "no work posted" test fired on it. FIXED. The idle-on-no-work gate is therefore NOT the fix.
 - The context-slot mismatch theory (`ctx = base + 0x100 + 0x100*X5CPU` resolving to the wrong
   256-byte block) is DEAD: X5CPU is 0, ctx is 0x0002A100, and that IS where SINTRAN staged
   P=0x08000004.

**201B is UNDOCUMENTED.** The authoritative table - ND-05.017.01 Appendix A, "Sub-codes for
Error Message: Fatal Error from Swapper (2047B)" - runs 1B..107B and has no 200-range entry.
Field bulletins prove such codes exist in the wild (ND-896058-2-EN:1670 documents 207B
"Pagefault in Swapper"), so 201B is a real code from a later swapper revision. The manuals
cannot resolve it; only carving the swapper binary can, and 7.6.2/7.6.3 already did:

    the swapper reads a list head from its DSEG at VA 0x08026214 (= 0x24, a PHYSICAL pointer),
    walks the list following the link at +4, counts elements whose HALFWORD AT +8 is 1,
    and calls SWPFATAL with 0o201 when the count is ZERO.

NEW MEASUREMENT (2026-07-29): that whole region is EMPTY. ND-500 physical 0 maps to the MPM
window (ADRZERO), so PA 0x24 is ND-100 byte 0x00420024, and every word from PA 0x20 to PA 0x40
reads back as zero:

    201B-list @PA 0x0024 (mpm 0x00420024): 0000 0000 | +4 link=0000,0000 | +8 flag=0000
    (identical for 0x20, 0x28, 0x2C, 0x30, 0x34, 0x38, 0x3C, 0x40)

So this is NOT "the flag has the wrong value" - NOTHING EVER POPULATES THE LIST. The count is
zero because the table is zero.

**Next, and it is a narrow question:** who is supposed to fill that table, and when? Two
candidates, neither verified: (a) SINTRAN publishes the memory-part configuration into low MPM
(the machine's MEMORY-CONFIGURATION reports exactly one part, flagged available to N500P/N500D,
which would match a scan counting parts whose flag is 1); or (b) the head word 0x24 in the
swapper's DSEG is STARTUP DATA that SINTRAN is supposed to patch before starting the swapper
and we never write - note the manual's sibling sub-code 46B is "INIT SWAPPER - Illegal startup
data". Distinguish them by finding the writer, not by inventing a value: per the
no-hardcoding rule, filling the table with a plausible constant would hide the real gap.

### 7.7.5 Nobody writes the 0o201 list - confirmed from both ends

CONFIRMED in matched runs (swapper actually started, not just the harness reaching the prompt):

    startSeen=1 startTaken=True swpInfo=0x00008E30 startCtx=0x0002A100
    201B-list @PA 0x0024 (mpm 0x00420024): 0000 0000 | +4 link=0000,0000 | +8 flag=0000

Two runs, identical. (An earlier zero dump was taken in a run where startSeen=0 and did not
count; this one does.)

**SINTRAN is exonerated, from source.** Its memory-part table is built by `CHMEMDEF`
(`5P-P2-MON60.NPL:523-597`, called from `INZ500` at `:624`) into the ND-100 N500 DATAFIELD -
`AMEMTABLE` at `N500DF+61B` and `TYPMTAB` at `+101B`, `MXMPARTS=16B` - with ordinary ND-100
stores. There is NO MPM/bank store and no write to any ND-500 physical address anywhere in the
45 NPL files. `PH-P2-OPPSTART.NPL:2492-2509` (`FN5MEM`) fills the same datafield arrays at cold
start. `FLIMEM` (function 060B, `:2117`) does not even read that table - it copies the MON60
reply buffer to the user, so the `PART 0B ... N500P=Y N500D=Y` text comes from the ND-500 side.
There is also no write-swapper-data MON60 function (only `RSWPDATA=121`, READ), so the swapper
data segment is initialised by the USERLAND ND-500 Monitor, which is not in this source tree.

**The emulator is exonerated too, on both candidate bugs:**
 - PHYSWR IS performed (`PerformOctobusBlockCopy`, writeToNd500:true) - not merely acknowledged.
 - The buffer-side address arithmetic is RIGHT. The copy log shows every PHYSWR moving
   0x00000000 out of b=0x42CC00, and the never-evicted write log records 5168 ND-100 writes into
   [0x42CBF0..0x42CC40) of which ZERO are non-zero. We read the same place the ND-100 writes;
   the zeros are genuine, not a mis-addressed fetch.
 - No micro-function is rejected. The whole Monitor conversation is 3RMICV(1) x30,
   PHYSWR(31B) x13, 30B x12, CACHE(12B) x1 - no unknown/unserviced message.

So the ND-500 Monitor writes ZEROS to low ND-500 physical memory (0x420096..0x4200C4) and reads
them back - a clear/verify pass - and NOTHING, from any side, ever writes PA 0x24.

**Leading hypothesis, NOT VERIFIED:** the head word in the swapper's DSEG at VA 0x08026214 is
STARTUP DATA the ND-500 Monitor is supposed to patch with the real table address, and the 0x24
we read is unpatched file content. The manual's sibling sub-code 46B is literally "INIT SWAPPER
- Illegal startup data". Cheap test: read the swapper :DSEG file on the pack at that offset - if
it contains 0x24, the value is file content and patching is expected.

Do NOT "fix" this by writing a plausible table into PA 0x24. Per the no-hardcoding rule that
would hide the real gap - find the writer.

### 7.7.6 GAP FOUND AND FIXED: RIOM read the wrong memory (2026-07-29)

The hypothesis in 7.7.5 (unpatched DSEG startup data) was NOT the answer, and the
"do not fix RIOM" instruction in 7.6.7 was based on a wrong premise. Both are now
superseded by measurement.

**Measurement 1 - 5MBBANK is CORRECT.** Read live through the resident MMU mapping
(a raw physical read returns 0 and would have faked the defect):

```
5MBBANK@0o4654 = 0x0021 = 33     (5FPMAILBOX = 2129)
```

Bank 33 is right for a mailbox at ND-100 byte 0x428xxx. So SINTRAN's CNVWADR is not
failing - it is doing an INTENTIONAL conversion, and 0x00008E30 is its correct output.

**Measurement 2 - what 0x8E30 means.** Both candidate readings dumped from live memory
at the moment the swapper had the pointer:

```
HSWPI 0x00008E30 as WINDOW byte 0x428E30: FFFF FFFF 0427 0001 0000 0000 0005 0005 003B 0840 ...
HSWPI 0x00008E30 as WORD  addr 0x011C60: 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 ...
```

The window reading holds a real message (LINK = -1, 0x427 = 2047B, 0x840 = ADRZERO page
2112). The word reading is empty. **HSWPI is a 5MPM-window BYTE offset, not an ND-100
word address.**

**The defect.** `Riom.cs` mapped its ND-100 operand as `_private + operand*2`, the classic
3022 WORD-address convention, on a transport where the operand is a window byte offset.
The swapper's one RIOM therefore read 0x011C60 (empty) instead of 0x428E30, its message
record stayed all zero, the base pointer it takes from that record was null, the derived
list head was `0 + 0o44 = 0x24`, the scan counted nothing, and it reported SWPFATAL 0o201.
That is the entire chain, end to end.

**The fix, and why it is not a hardcode.** The rule already existed in exactly one place
for the block copies - `Nd500MicrocodeServicer.ResolvePhysicalCopyAddress` (classic:
`addr << 1`; octobus: `Nd500AddressBase + addr`). RIOM now uses the SAME rule instead of a
second copy of it: `Nd500CpuProcessBridge` derives base and scale by asking that resolver
for operand 0 and operand 1, and hands both to `CpuND500.SetND100AddressMapping`. A third
transport only has to be taught once, and the copy engine and RIOM cannot drift apart.

Files: `Emulated.HW/ND/CPU/ND500/CpuND500.ND100Bridge.cs`,
`Emulated.HW/ND/CPU/ND500/Servicer/Nd500CpuProcessBridge.cs`,
`Emulated.HW/ND/CPU/ND500/Servicer/Nd500MicrocodeServicer.cs`,
`Emulated.HW/ND/CPU/ND500/Instructions/IO/Riom.cs`.

**Verified after the fix** (octobus FullFlow harness):

```
riom n=1 src=0x00008E30 mapped=0x00428E30 dest=0x080240BC halfwords=15
```

It now reads the real message. `start-swapper` reports OK.

**STILL OPEN: ERROR CODE 201B has not gone away.** Note the transfer is only 15
halfwords = 30 bytes, but 7.6.6 places `HSWPI`/the base pointer at byte offset 0o210
(136) of the record at 0x080240BC - far beyond what this RIOM copies. So EITHER the
0o210 offset reading in 7.6.6 is wrong, OR a SECOND transfer is supposed to fill the
rest of the record and is not happening. That is the next thing to settle, and it is
directly observable: dump 0x080240BC..0x0802414C now that the first 30 bytes are real.

### 7.7.7 SWPFATAL 0o201 IS FIXED - three chained RIOM defects (2026-07-29)

`ERROR CODE 201B` is GONE. The base pointer the swapper needs now arrives:

```
0x08024144 = 0x00008800   <- RIOM delivers it (was 0x00000000)
0x0802620C = 0x00008800   <- the globals initialiser stores it
head word  = 0x00008824   = 0x8800 + 0o44, derived off a REAL base (was 0 + 0o44 = 0x24)
```

Everything in 7.6.4 through 7.7.6 that treated the null base pointer as the defect was
correct about the SYMPTOM. The cause was three separate bugs in our RIOM, each one hidden
by the one before it - which is why every earlier round of measurement kept reading zeros
and kept looking like a missing SINTRAN write.

**Defect 1 - wrong address CONVENTION.** `Riom.cs` mapped its ND-100 operand as
`_private + operand*2`, the classic 3022 WORD-address rule, on a transport where SINTRAN's
CNVWADR emits a 5MPM-window BYTE offset. Established by measuring 5MBBANK live (= 33, so
CNVWADR is converting deliberately, not failing) and by dumping both candidate readings of
HSWPI 0x8E30: the window reading held a real message, the word reading was empty. Details
in 7.7.6.

**Defect 2 - wrong BUS.** `ReadND100Word`/`WriteND100Word` called `SystemBus` directly,
which bypasses `RouteToMpm`. On the octobus the ND-500's memory IS the multiport memory
("ND-500 address 0 = the MPM window start"), so the shared message is only reachable
through that routing - a raw SystemBus read returns zeros no matter what base is
configured. THIS is what the old `[OPEN]` note in the bridge was actually observing when it
said "the source reads zero either way in the runs so far". Fixed by routing through
`ReadPhysical16`/`WritePhysical16`, which fall back to SystemBus when no MPM is attached,
so the classic path is unchanged. The mapping base is 0, not the window address: a
window-relative offset is ALREADY an ND-500 physical address on the CPU's own path, and
adding the window base again overshot by 4 MB into unbacked local memory.

**Defect 3 - wrong operand DATA TYPE, twice over.** The manual's format line is

    H RIOM <ND-100 addr/r/W>,<buffer/w/H>,<no of halfwords>

(ND-05.009.4 section 16.23). Operands 1 and 2 carry EXPLICIT `/W` and `/H` annotations;
operand 3 carries NONE. "no of halfwords" names what the value MEANS, not its data type.
We had operand 3 declared `SameAsInstruction` (= H), and the data type sets the POST-INDEX
SCALE. The swapper takes its count from a post-indexed table which is measurably an array
of 32-bit words at VA 0x0802403C:

```
[0]=0x0D [1]=0x0A [2]=0x0F [3]=0x8A [4]=0x09 [5]=0x46 [6]=0x08 [7]=0x00 [8]=0x08 ...
```

With index 5 and scale 2 the operand landed at 0x24046 - MISALIGNED, straddling two
entries - and returned the low halfword of `[2]`, i.e. 15. That plausible-looking count was
pure coincidence, and it stopped the transfer 106 bytes short of the field at
destination+136. `[5] = 0x46 = 70` halfwords = 140 bytes covers it exactly.

Separately, `Riom.cs` hardcoded `ReadOperandValue(..., DataType.H, 2)` for the count
regardless of the metadata. After fixing the scale that 2-byte read took the big-endian
HIGH halfword of `0x00000046` and yielded 0, transferring nothing. Both had to change.

Note this also makes the `count > 0xFFFF` guard reachable again; it had been documented as
dead code precisely because of the halfword read.

Files:
- `Emulated.HW/ND/CPU/ND500/CpuND500.ND100Bridge.cs` (routed access, scale, mapping setter)
- `Emulated.HW/ND/CPU/ND500/Servicer/Nd500CpuProcessBridge.cs` (derives the scale)
- `Emulated.HW/ND/CPU/ND500/Servicer/Nd500MicrocodeServicer.cs` (resolver made public)
- `Emulated.HW/ND/CPU/ND500/Instructionset.Init.cs` (operand 3 = WordOnly)
- `Emulated.HW/ND/CPU/ND500/Instructions/IO/Riom.cs` (word count read, diagnostics)

**LESSON, worth keeping.** Three of the four "do not touch this" notes in this file pointed
away from RIOM - 7.6.7 said "Do not fix RIOM. Its mapping is correct", and the bridge's own
CAVEAT said the offset agreement might be coincidence. The note that turned out to matter
most was the one nobody followed up: "the source reads zero either way in the runs so far."
A value that is wrong under BOTH candidate hypotheses is evidence that neither hypothesis
is being tested - i.e. the read is not reaching the memory at all. That should have been
the first thing checked, and it is cheap to check: dump both candidate addresses and see
whether EITHER holds plausible data.

**STILL OPEN (not regressions - 201B was the fatal, and it is gone):**
- `start-swapper` now reports STALL rather than OK. The fatal is gone, so the swapper is
  getting further; what it is now waiting for has NOT been established. Measure before
  concluding.
- `status` and `stop-system` still STALL. `stop-system` also stalls at clean HEAD, so it
  is pre-existing and unrelated.
- Run-to-run variance in WHEN the 3START message is processed (startSeen 0 vs 1 at the
  early sample points) persists.

#### 7.7.7.1 Mirrored to the nd500x C port

Defect 3 is transport-independent and manual-derived, so it is mirrored:
`~/repos/nd500x/src/cpu/instructions/IO/Riom.c` now reads the count operand with
`ND500_DTYPE_WORD` instead of `fi->data_type`. In the C port that single dtype argument
drives BOTH the value width and the post-index scale, so one change covers what took two
in C# (metadata + the hardcoded read). Verified: builds clean, `ctest -R riom` passes, and
the 3 failing ctest cases (`ote_instructions`, `mon_calls`, `instruction_validation`) fail
identically with the change stashed, i.e. they are pre-existing.

Defects 1 and 2 are NOT mirrored, deliberately. Both are about the octobus/5MPM transport:
the window-relative pointer convention and routing the ND-100 read through the MPM window
rather than the CPU's local bus. The C port maps a fixed 5MPM region
(`nd100_memory_offset`, cpu.c around line 1450) and does not host the octobus station or
the SINTRAN octobus scenario, so there is no equivalent path to correct. Mirroring them
there would be speculative rather than derived. If nd500x ever grows an octobus transport,
this is the note to come back to.

### 7.7.8 MILESTONE: the swapper runs its real message loop (2026-07-29)

With the three RIOM defects fixed, the swapper is no longer failing at startup - it is
running and asking SINTRAN for work:

```
PC=0x08008255 startSeen=2 startMicfu=23B startTaken=True PS=3
ansMON=377B ansSWPFU=1B ansArgc=4 ansArg0=0x00000001 ansArg1=0x00000000
riom n=3 src=0x00008E30 mapped=0x00008E30 dest=0x080240BC halfwords=138, then 70
```

What each of those means, and why it is a real change rather than a different failure:

- `ansSWPFU=1B` is **LNEWSWAP**, a NORMAL entry in SWPDECODER's jump table
  (`MP-P2-N500.NPL:913-919`, index 1). Every previous run reported 2047B = SWPFA/SWPFATAL,
  which is > SWFMAX and therefore lands in the ESWPFATAL slot. The swapper has stopped
  reporting its own death and started making legitimate requests.
- `ansArgc=4` - a real four-argument request, not the two-argument (selector, error code)
  shape of a fatal report.
- `riom n=3` with counts 138 and 70 - the count now varies per message type, which is what
  the indexed count table is FOR. A single transfer of a coincidental 15 could never have
  shown that.
- `startSeen=2` - a second start-class MICFU was processed (a 25B 3TRACO resume), so the
  swapper is being re-entered rather than dying once.
- `PC=0x08008255` vs the old `0x08000687` - execution has moved well past the point where
  it used to call SWPFATAL.

**STILL OPEN, and NOT resolved by this:** `start-swapper` reports STALL from the operator's
point of view - the harness does not see the expected prompt within its timeout. The swapper
is in WAIT having answered its request, so the open question is what the ND-100 side does
next with an `LNEWSWAP` that has actually been answered. That has NOT been measured yet; do
not assume it is only a harness timeout. `status` also STALLs, and `stop-system` STALLs at
clean HEAD too, so that one is pre-existing and unrelated.

### 7.7.9 THE SWAPPER WORKS (2026-07-29)

```
OUTCOME: ENTER=OK login=OK nd-500=OK status=OK start-swapper=OK list=OK stop-system=STALL
```

Console, unedited:

```
ND-5000: status
> Loading Control Store
> Loading Swapper
ZERO 0 / CARRY 0 / SIGN 0 / FLAG 0 / OVERFLOW 0

ND-5000: start-swapper
> Allocating memory - 7110B pages
ND-5000: who-is-on
===>     1 used by SYSTEM           on terminal    1    cpu  1
```

`start-swapper` loads the swapper, it runs, and it ALLOCATES 7110B PAGES. No fatal, no trap,
no SWPFATAL. The swapper is doing the job it exists to do.

**The last two "STALL"s were the harness, not the machine.** `status` and `start-swapper`
were being flagged STALL while actually COMPLETING: the `ND-5000:` prompt arrived just after
the 60s `RunNd500Command` deadline, which is why the NEXT command always ran fine against a
returned prompt. Re-running with `RETROCORE_HARNESS_TIMEOUT_SCALE=5` turned both OK with no
code change - that is the proof, not an argument. The default is now 300s.

Worth stating plainly, because it cost real time twice in this file: every harness timeout is
HOST WALL-CLOCK, so a STALL conflates "never happened" with "not within N host seconds". A
false STALL is first-class misleading evidence. Section 7.7 opened with a whole
investigation built on STALL flags that came from a contaminated fixture; this one ends with
two more STALLs that meant nothing. Do not treat a STALL as a finding until the timeout has
been ruled out - it is one env var away.

**Genuinely still open:** `stop-system` STALLs. It ALSO stalls at clean HEAD, so it predates
all of this work and is unrelated to the swapper. That is the next thing, and it should be
investigated on its own terms rather than as swapper fallout.

## 7.8 Session 2026-08-02 - confirm SWMSG is DONE, re-plan, verify #24

**Nothing was broken this session and no new fix was needed - the point of the session was to
stop chasing a stale plan.** The section-7.0 framing ("carve the SWMSG field layout, the swapper
is blocked") is FOUR DAYS STALE relative to 7.7.6-7.7.9 below it. Read the LATEST dated section
before acting on this file - that is the exact "stale status headers" trap the sintran-carving
skill warns about, and it caught a fresh session cold.

### 7.8.1 SWMSG / SWPFATAL 201B is fixed and COMMITTED - verified from the code, not the doc

The blocker was never the message field layout. It was three chained RIOM defects (7.7.7). The
fix is committed in RetroCore as `eef020bd1` "ND-500 RIOM: fix three chained defects behind
SWPFATAL 201B". Confirmed present this session:
- `Instructionset.Init.cs` - the `riom` count operand is `WordOnly` (defect 3), with the exact
  measured-proof comment.
- `Riom.cs` / `CpuND500.ND100Bridge.cs` - routed access + scale + mapping setter (defects 1/2).

Live proof already recorded in 7.7.9: `start-swapper=OK`, `> Allocating memory - 7110B pages`,
no SWPFATAL. The swapper does its job.

### 7.8.2 Plan cleaned (NDInsight task list)

- Task #22 (X5BEX-resolves-to-zeros / status+start-swapper stall) -> COMPLETED (it WAS the three
  RIOM defects; the residual STALLs were harness wall-clock, TIMEOUT_SCALE, default now 300s).
- Task #18 "HACK the ND-5800 image to RUN a domain" -> DELETED. The "hack" was a fallback branch
  (synthesize MICRO-START + fake swapper mailbox answers) that we NEVER needed - the real CS-load
  + real swapper protocol path works. Replaced by #27, a verification checkbox (no hacks).
- New open items: #23 stop-system STALL, #24 3START nondeterminism, #25 sub-word WIDTH root cause
  (mulad/putbf/loopd), #26 the 2939 CSIT-unmasked divergences, #27 verify the real 5800 CS-load path.

### 7.8.3 #24 (run-to-run nondeterminism) - both documented causes already addressed

Static reading found the two 7.7.3 causes are already fixed in the tree:
1. **STALL half (start message never noticed):** `CpuND500.ProcessControl.cs` `ParkPollIntervalMs = 1`
   - the ND-5000 IDLE is a POLL, not a block; an unbounded `WaitOne()` only modelled the kick half,
   so an ordinary bare-memory-write activation came down to timing. The 1 ms park-poll closes it.
2. **Hard-crash half (no managed exception):** the re-entrancy stack-overflow is guarded at
   `OctobusND5000Station.cs:1109-1119` - the ND-100 memory-write path ONLY signals
   (`MailboxDoorbell.Invoke()` -> `WakeRunThread`, just sets an event), never drains inline. Plus
   the harness now copies `BIGDISK0-L.IMG` to a PER-SESSION temp (Setup), removing the
   concurrent-writable-pack corruption the 7.6.7 tooling note flagged as the likelier abort cause.

**Empirical check (RESULT 2026-08-02):** `FullFlow_Octobus_Login_Nd500_Status_StartSwapper_Capture`
x3, `RETROCORE_HARNESS_TIMEOUT_SCALE=5`, `RETROCORE_ND5000_RUNTHREAD=1`. Runs 1 and 2 BOTH PASSED
(31m24s / 30m36s - the long duration is the scaled wall-clock waits, not a stall), both with:
```
OUTCOME: ENTER=OK login=OK nd-500=OK status=OK start-swapper=OK list=OK stop-system=OK
```
Run 3 started but was cut off by the LLM session ending - NOT a crash (no fault, just unfinished).
2/2 deterministic, no crash. **#24 CLOSED as resolved-by-prior-fixes.** The early `startTaken=False`
state lines are boot-time sampling before the start message is processed; both runs flip to
`startSeen=2/startTaken=True` and finish clean.

**Bonus: `stop-system=OK` in BOTH runs -> #23 (stop-system STALL) is ALSO resolved**, as a harness
wall-clock false-STALL. The "stalls at clean HEAD" observation was the OLD 60s `RunNd500Command`
deadline (default is now 300s); the `ND-5000:` prompt arrives just after 60s, exactly like
status/start-swapper. TIMEOUT_SCALE=5 turns it OK with no code change. The uncommitted Servicer
diagnostics are read-only and cannot account for it. Classic [[harness-stall-is-wallclock]].

**So the entire octobus swapper track is GREEN.** Remaining open work is correctness debt (#25/#26),
the 5800 CS-load verification (#27), and the production-wiring track toward D4 (#9/#10/#11/#14/#16/#12).
Caveat: 2 clean runs is not a huge sample for a formerly-intermittent crash; if it ever recurs, the
two mechanisms in 7.8.3 above are where to look.

### 7.8.4 Uncommitted diagnostics in the tree (someone's in-progress #23 work) - LEFT ALONE

`git status` in RetroCore shows two modified Servicer files, additive DIAGNOSTICS only (not the
SWMSG fix, which is committed):
- `Nd500MicrocodeServicer.cs` - `SWPSTAT_WORD_OFFSET = 0o103`, `LastAnswerSwpStat`,
  `TrapStopsPosted`, a MICFU histogram. Probing the SWPFU=1/LNEWSWAP answer path (the #23 lead).
- `Nd500CpuProcessBridge.cs` - logs K/FUNCV on restart (K carries the result for some swapper calls).
These are the natural starting point for #23. Do not commit them without their author.
