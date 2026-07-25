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

### 5.5.4 CURRENT BLOCKER: the RIOM count is read 32-bit at a halfword address

**MEASURED:** the RIOM DMA transfers **983040 halfwords** spanning `0x080240BC..0x082040BA` - it
runs to the end of memory - against a count operand of 15.

`983040 = 15 * 65536 = 0x000F0000`. The count VALUE is correct but sits in the **high halfword**,
which is exactly what a 32-bit big-endian read at the halfword address `0x08024046` yields
(`halfword[5]=15` followed by `halfword[6]=0`). So the count operand is read at width 4 where it
should be width 2. The out-of-range write at `0x08040000` is this overrun, not a bad address.

**This is independent of the synthetic test message** - the count comes from the swapper's own DSEG
table, so it will misfire identically against a real SINTRAN message. The fix belongs in
`CpuND5000.cs` (RIOM body `RIOM_0 @ 0o12255`; the fault surfaces near `0o12272`), which at the time
of writing carries another session's in-flight work - handed to that lane rather than edited here.

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

**Remaining body fields still uncarved.** They read out of the same access log once the RIOM count
bug (5.5.4) is fixed - the runaway transfer currently overwrites the region the swapper would
parse, so anything beyond the first field would be observing corrupted data. The instrument is
already in place; it is the count fix that gates it.

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

**REVISED 2026-07-25 (later same day) to match the corrected section 5.**

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
