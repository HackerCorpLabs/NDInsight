# LINKAGE-LOADER install: swap file unblocks the load, then 5SWAP takes a protect violation

Date: 2026-07-31. All results below are from live runs of
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd5000OctobusBootHarnessTests.cs`
(test `Nd500SwapFile_CreateAndDefine_Capture`), with
`RETROCORE_NLL_FLOPPY=1` and `RETROCORE_ND5000_RUNTHREAD=1`.

Transcript of record:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-capture-nd500-swapfile.txt`

---

## Summary

The LINKAGE-LOADER install was blocked by three things in a row, none of which
was the thing previously suspected. Two are now fixed. The third is an emulator
defect that only became reachable once the first two were cleared.

| # | Blocker | Evidence | Status |
|---|---|---|---|
| 1 | `@SET-AVAIL` was never run before `@ND-500` | with it, RECOVER-DOMAIN stops stalling at "Loading Swapper" | FIXED |
| 2 | The pack has NO ND-500 swap file, and SYSTEM had no free pages to make one | `SWAPPING SPACE NOT AVAILABLE`, then `NO MORE PAGES AVAILABLE FOR THIS USER` | FIXED |
| 3 | Our ND-500 swapper traps once it actually allocates memory | `Protect violation`, shadow process `5SWAP` | **OPEN - emulator bug** |

---

## 1. SET-AVAIL is mandatory (confirmed)

Ronny's instruction - `@set-avail` AFTER login and BEFORE `@nd-500` - is
load-bearing, not a nicety. The install test never did it. Adding it changed the
outcome of RECOVER-DOMAIN immediately.

Consequence worth stating plainly: **every ND-500 result this test produced
before 2026-07-31 was taken from a machine that had not been made available**,
including the "ACCP was terminated" banner readings and the earlier
"Loading Swapper" stall.

## 2. The pack has no ND-500 swap file

`@LIST-FILES (SYSTEM)SWAP-FILE` returned nothing. RECOVER-DOMAIN got as far as

```
> Loading Control Store
> Loading Swapper
> Allocating memory - 7110B pages
SWAPPING SPACE NOT AVAILABLE
```

`SWAPPING SPACE NOT AVAILABLE` is SINTRAN error 553 (`Developer\MON\Monitor
Calls.md`, "Swapping space not available"). The first attempt to create one hit

```
@create-file swap-file-0:swap
NUMBER OF PAGES: 5000
NO MORE PAGES AVAILABLE FOR THIS USER
```

which ND-30.003.007 EN SINTRAN III System Supervisor, COMMENTS 12, defines as
"no more free pages for a user area", fixed with `@GIVE-USER-SPACE`.

**The working sequence** (procedure from that manual, section 3.3.9 "ND-500 Swap
files (ADVANCED)"):

```
@give-user-space system 8000
@create-file swap-file-0:swap
NUMBER OF PAGES: 5000
@nd-500
ND-5000: define-swap-file
File name: swap-file-0
ND-5000: list-swap-file-info
Swap file number: 0
```

which reports, correctly:

```
Swap file number:............ 0
Swap file name:.............. (PACK-ONE:SYSTEM)SWAP-FILE-0:SWAP;1
Mass storage addr:........... 76110B
Size in pages:............... 11610B
Greatest free swap file part: 11610B pages
    1B     free            76110B        11610B
```

Users on this pack, for the record: SYSTEM, FLOPPY-USER, UTILITY, BPUN-FILES,
SCRATCH, RT, DOMAIN-USER (all on PACK-ONE).

NOTE: the harness refreshes the pack from
`F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG` on EVERY run, deliberately, so the
swap file does not survive to the next run and has to be recreated in-session.
That also means the manual's "the ND-500 must be restarted to make the swapper
use the new swapping file" cannot be satisfied by rebooting between runs - but
it evidently was not needed, because the allocation proceeded (see below).

## 3. OPEN: 5SWAP protect violation during allocation

With the swap file defined, RECOVER-DOMAIN gets PAST allocation start and the
ND-500 runs for about seven and a half minutes (02:57:12 -> 03:04:46 in the
transcript) before:

```
> Allocating memory - 7110B pages
ERROR   * 76B:44B * ND-500(0) Trap
          Protect violation
          CPU number..........0
          Shadow process......5SWAP

FATAL   * 21B:77B * ND-500(0) Monitor Internal
          Fatal internal system error - ND-500(0) CPU locked
 *** FATAL SYSTEM ERROR ***
ND-500(0) error:      The Swapper stopped

PROTECT VIOLATION
At program address:        1     10533B
From CPU in slot position:           0D
Logical address:           0         4B
MEMORY MANAGEMENT STATUS:            0B
DATA  POFF read request
Physical address:          0         0B
Physical segment:                    0D
WR:                                  0B
```

Read carefully, that is:

- the faulting instruction is at **segment 1, offset 0o10533**
- it is a **DATA read**, flagged **POFF**
- the target is **logical address 4 on segment 0**
- physical address and physical segment both resolve to **0**
- MMU status 0

### CORRECTION - it is NOT our MMU

An earlier version of this document said the emulator was presenting a zero
capability, on the reasoning that a zero capability is the protect-violation form
(`nd500-mmu-table-formats`). **That is disproved.** Instrumenting the test with
the existing `Nd5000StateLine` helper and reading the CPU's own record at the
moment of the fault gives:

```
[after-recover-domain] PC=0x08008255 stopMode=WAIT PSTP=0x0003A000 CTXBASE=0x0002A000
    startSeen=1 startMicfu=23B startTaken=True
    ansMON=377B ansSWPFU=1B ansP=0x08008255 ansArgc=4 ansArg0=0x00000001 ansArg1=0x00000000
    PS=3 CED=0 CAD=0 THA=0x00000000
    riom n=6 src=0x00008E30 mapped=0x00008E30 dest=0x080240BC halfwords=34
    lastPageFault=(none)
    lastDoubleFault=(none)
    lastProtectViolation=(none)
```

All three fault records are `(none)`. **Our MMU never refused anything.** The CPU
is in `WAIT`, not crashed, having answered a MON 377B (`N5SWAP`) with `SWPFU=1B`
and four arguments, with the swapper start accepted (`startTaken=True`).

So the trap report SINTRAN printed did not come from our address translation.

### What the numbers actually suggest

Re-read the trap report with that in mind:

```
Logical address:           0         4B
MEMORY MANAGEMENT STATUS:            0B
Physical address:          0         0B
Physical segment:                    0D
WR:                                  0B
```

Every field except the program address is ZERO. `MEMORY MANAGEMENT STATUS: 0B` is
why the report renders as "POFF read request" (access type `000`) - the ND-5000
maintenance manual's own worked example prints "No trap indicated in MMSTS!!"
beside that exact combination.

A trap report that is all zeros is what decoding an UNFILLED message buffer looks
like. HYPOTHESIS, not yet tested: SINTRAN is decoding a trap message whose fields
our servicer never populated, and the "Protect violation" classification is what
those zeros decode to - not a fault that actually happened.

That moves the problem off the MMU and onto the octobus/servicer message path,
which matches `nd5000-swapper-reaches-message-loop` ("blocker moved to the
octobus transport").

### The trap is a phantom - we never posted one

Two further hypotheses were tested and BOTH are dead. Recorded so nobody spends
another night on them.

**Dead hypothesis A: SINTRAN read a non-zero SWPSTAT and reported a swapper
error.** `SWPDECODER` switches on `SWPFU`; our answer leaves `SWPFU=1`, which
selects `LNEWSWAP`, and `LNEWSWAP` does:

```
X:=SWMSG; *AAX SWPST; LDATX      % A:=SWMSG.SWPSTAT
IF A><0 THEN                     % Error-answer from swapper?
   D=:A                          % Swpstat, error code
   X:=CSWPM; CALL EMONICO        % Restart proc. with error code after mon.call
ELSE                             % Ok answer from the swapper
```

Nothing in RetroCore writes `SWPST` (word offset 0o103, per the K03 and L07
symbol tables), so a stale non-zero there would have been read as a swapper
error. MEASURED: `ansSWPSTAT=0B`. SINTRAN takes the OK branch. Not this.

**Dead hypothesis B: we posted a trap with the wrong trap number.** Our servicer
does have a trap path (`AnswerTrapStop`: `STOPR@11B := 2`, `TRAPN@16B := number`),
and it is the ONLY thing that can tell SINTRAN the ND-500 trapped. MEASURED:
`trapsPosted=0 lastTRAPN=0B`. **We never posted a trap at all.**

So, with `lastProtectViolation=(none)` as well, the entire "ND-500(0) Trap /
Protect violation" report is generated SINTRAN-side from a message whose trap
fields nobody wrote - which is exactly why every field in it except the program
address reads as zero.

### What the evidence now says is happening

- `startSeen=1`: exactly ONE start message was ever presented to the servicer.
- `stopMode=WAIT`, `PC=0x08008255` unchanged: the swapper is parked and never
  resumed after we answered its MON 377B.
- The OK branch of `LNEWSWAP` sets `MICFUNC := 3TRACO` (trap continue) and
  restarts the ND-500 process - and our servicer never saw a second start.

So SINTRAN believes it restarted the swapper, the swapper never ran again, and
the fatal report follows from that stall. This is the octobus/servicer message
path, matching `nd5000-swapper-reaches-message-loop`.

### The swapper is NOT stuck - it is being answered wrongly, ONCE

Two more hypotheses died, and then the actual failure showed itself.

**Dead hypothesis C: SINTRAN never sent a restart.** A per-micro-function tally
(`MicfuHistogram()`) gives, for the whole run:

```
msgs=50 micfu[1B:26 12B:1 23B:1 24B:7 31B:15]
```

`1B` = ReadMicroVersion (the watchdog), `12B` = CacheControl, `23B` = StartProcess,
`24B` = MonitorCallContinue (3MONCO, restart after monitor call), `31B` =
PhysicalWrite. SINTRAN sent **seven** restarts.

(Note: the first version of this tally counted after the dispatch switch and so
missed every micro-function whose case returns early - including 23B. A tally
that silently omits a class of messages cannot prove absence. Counting moved to
the dispatch entry.)

**Dead hypothesis D: we decline the restarts.** MEASURED `restarts=7/7`: the
process host TOOK every one. `OnMonitorCallRestart` requires a WAIT-parked CPU,
clears WAIT and wakes the run thread, and it succeeded each time.

**What is actually happening.** The swapper's own MON call log shows it making
STEADY PROGRESS through a conversation, not spinning:

```
MON 377B argc=4 ret=0x08008255 | [0] @0x08012A28=1 | [1] @0x080240B0=0 | [2] @0x080240B4=0        | [3] @0x0802428C=0
  RESTART write-back: @0x080240B0:=0x00000005 @0x080240B4:=0x00008E30
MON 377B argc=4 ret=0x08008255 | ...                | [2] @0x080240B4=0x8E30 | [3] @0x0802428C=0
  RESTART write-back: @0x080240B0:=0x00000003 @0x080240B4:=0x00008E30
MON 377B argc=4 ret=0x08008255 | ...                                        | [3] @0x0802428C=0
  RESTART write-back: @0x080240B0:=0x00000018 @0x080240B4:=0x00008E30
MON 377B argc=4 ret=0x08008255 | ...                                        | [3] @0x0802428C=0x0A
  RESTART write-back: @0x080240B0:=0x00000000 @0x080240B4:=0x00008E30

MON 377B argc=7 ret=0x08002771 | [0] @0x08012A24=2 | [1] @0x08024428=0x0A | [2] @0x08038406=0x240
                               | [3] @0x08024410=0x30 | [4] @0x08024424=0x0027C800
                               | [5] @0x08024414=0x0000F890 | [6] @0x08024418=2
  RESTART write-back: (empty)          <-- NOTHING WRITTEN BACK

MON 377B argc=4 ret=0x08008255 | ...                                        | [3] @0x0802428C=0xFFFFFFFF
  RESTART write-back: @0x080240B0:=0x00000018 @0x080240B4:=0x00008E30
MON 377B argc=4 ret=0x08008255 | ...                                        | [3] @0x0802428C=0x0B
MON 377B argc=4 ret=0x08008255 | ...                                        | [3] @0x0802428C=0x0C
-> ND-500(0) Trap / Protect violation / 5SWAP -> The Swapper stopped
```

Read that sequence:

- The routine 4-argument call from `ret=0x08008255` is the swapper's poll loop.
  Its first argument selects a function; here it is **1**, and SINTRAN answers it
  properly every time (SWPFU 5, 3, 0o30, 0 and the SWPINFO pointer 0x8E30).
- Then comes ONE DIFFERENT call: **7 arguments, from a different return address
  `0x08002771`, with a different first-argument slot (`0x08012A24`) holding 2**.
  This is a different swapper function, and its parameters look like real work:
  0x0027C800 and 0x0000F890 alongside small counts.
- **That call gets an EMPTY write-back - nothing at all.**
- Immediately afterwards the poll loop's argument [3] turns to **0xFFFFFFFF**
  (-1), then 0x0B, then 0x0C, and the swapper dies.

So the swapper asks SINTRAN to do a piece of work, receives nothing back, and
reports failure. The "Protect violation" is the tail end of that, not its cause -
consistent with the trap report being all zeros and with `trapsPosted=0`.

### The 7-argument call, identified in the swapper's own code

`E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-pseg.asm:3325`:

```
1000023503: r:=          @b.100
1000023506: w2 laddr     r.0
1000023511: w2 =:        b.124        % address of r.0
1000023513: r:=          b.100
1000023515: by3 laddr    r.4
1000023520: w3 =:        b.130        % address of r.4
1000023522: w4 laddr     r.10
1000023525: w4 =:        b.134        % address of r.10
1000023527: w1 laddr     r.14
1000023532: w1 =:        b.140        % address of r.14
1000023534: call $1777...377,$7,$1000225044,b.70,@b.124,@b.130,b.64,@b.134,@b.140 ; MON 377B
1000023561: ifkret                    % <-- tests K and returns on error
1000023562: w comp2      b.30,$1000224334
```

`0o1000023561` is exactly the `ret=0x08002771` in the log, so the identification
is certain. `$1000225044` is `0x08012A24`, the function cell holding 2 - also
matching. The call passes the ADDRESSES of four fields of a record at `@b.100`,
so it is a descriptor-carrying request.

**Dead hypothesis E: SINTRAN answered that call with an error in K.** The
`ifkret` on the next line made this the obvious candidate. MEASURED: every
restart in the whole run reports `K=0 FUNCV=0x00000000`, including the one after
the 7-argument call. `ifkret` never fires, and the swapper falls through to the
`comp2` at `1000023562`.

So the empty write-back is probably NOT silence either: SINTRAN's answer to this
request is very likely the DATA ITSELF, delivered by the fifteen `31B`
(PhysicalWrite) messages counted in the same run - not by the restart record.

### Every block copy moves ZERO

The servicer's copy ring for the whole run - all sixteen `31B` PhysicalWrite
transfers:

```
  0  WR a=0x4200BC b=0x42CC00 n=4 data=0x00000000
  1  WR a=0x4200C0 b=0x42CC00 n=4 data=0x00000000
  2  WR a=0x4200C4 b=0x42CC00 n=4 data=0x00000000
  3  WR a=0x4200B6 b=0x42CC00 n=4 data=0x00000000
  4  WR a=0x420096 b=0x42CC00 n=4 data=0x00000000
  ...
 13  WR a=0x4200BC b=0x42D400 n=4 data=0x00000000
 14  WR a=0x4200C0 b=0x42D400 n=4 data=0x00000000
 15  WR a=0x4200C4 b=0x42D400 n=4 data=0x00000000
```

Facts, not inference:

- These are NOT bulk data. Every transfer is FOUR BYTES, into ND-500 addresses
  0x96..0xC4 (the low control/status cells), from two buffer addresses.
- **Every single one moves 0x00000000.** `data` is read back from the
  DESTINATION after the copy, so what actually landed was zero, sixteen times.

### Dead hypothesis F: the buffer side of the copy is resolved wrongly

**DISPROVED TWICE, before any code was touched.** Kept because it is the obvious
thing to suspect and the next person will suspect it too.

1. **From the code.** `ResolvePhysicalCopyAddress` is
   `Generation == Classic ? (addr << 1) : host.Nd500AddressBase + addr`. On
   Samson/octobus - the generation in play here - it returns EXACTLY what
   `PerformOctobusBlockCopy` already computes. There is no second interpretation
   to compare on this path; the split exists only for Classic.
2. **From the data.** The MPM write log for the same run has **1072 writes into
   the buffer region 0x42CC00-0x42CC0F, and not one of them is non-zero.**
   SINTRAN genuinely fills that buffer with zeros before asking for the copy.

So the sixteen zero transfers are CORRECT: SINTRAN is deliberately clearing
ND-500 cells 0x96..0xC4, and the `31B` path is doing its job. The original text
of this hypothesis follows, marked wrong.

### (wrong) the buffer side of the copy is resolved wrongly

`PerformOctobusBlockCopy` resolves BOTH operands the same way:

```csharp
uint aByte = host.Nd500AddressBase + (((uint)aHi << 16) | aLo);   // ND-500 side
uint bByte = host.Nd500AddressBase + (((uint)bHi << 16) | bLo);   // BUFFER side
```

But the classic 13B ResidentRead path in the same file treats the two sides
DIFFERENTLY - the ND-500 side gets `Nd500AddressBase + addr`, while the buffer
side goes through `ResolvePhysicalCopyAddress(...)`, because that side is a
physical (MMS) address, not an ND-500 address.

If the octobus path has the same split and we are missing it, then `b` is being
read from `0x420000 + 0xCC00` when the real buffer is somewhere else entirely -
and reads of untouched MPM return exactly what we see: zeros, every time.

This is a HYPOTHESIS. It fits all sixteen samples, but "reads zero" is also what
a legitimate clear-these-cells sequence looks like, and that must be excluded
before any code changes.

**Falsifier:** dump the source words under BOTH interpretations -
`Nd500AddressBase + addrB` and `ResolvePhysicalCopyAddress(addrB)` - at the
moment of the copy. If the second holds non-zero data while the first is zero,
the resolution is wrong and the fix is to split the two sides. If BOTH are zero,
SINTRAN really is writing zeros and this whole line is a dead end.

---

## THE FAULT EXPLAINED: the swapper's own paging-off block move

The octobus side is clean: messages arrive, restarts are taken, K and FUNCV are
sane, and the block copies do exactly what SINTRAN asked. Following the swapper's
own code past the 7-argument call finishes the story.

Straight after the call (`swapper-k01-pseg.asm:3327`):

```
1000023562: w comp2   b.30,$1000224334
1000023571: if <<= go $47
1000023573: r:=       b.10
1000023575: w move    b.30,r.24        % caller's b.30 -> callee param b.24
1000023600: w move    $1000225030,r.30 % constant       -> callee param b.30
1000023607: call      $1000012503,$0
1000023615: ifkret
```

And the routine it calls (`swapper-k01-pseg.asm:1795`):

```
1000012503: ents      $34
1000012511: w1 :=     b.24
1000012513: w1 *      $4000     % x 0o4000 = 2048 = ONE PAGE -> a page NUMBER
1000012517: w2 :=     b.30
1000012521: w2 *      $4000
1000012525: dmof                % <<< DATA PAGING OFF
1000012527: w bmove   r2.(0),r1.(0),$1000
1000012540: dmon                % <<< DATA PAGING ON
1000012542: ret
```

### CORRECTION

Earlier in this document I wrote that the trap report's `DATA POFF read request`
was merely an artifact of a zero MMS status word. **That was wrong, and it is
worth correcting because it pointed the investigation away from the answer.**

The swapper really does turn data paging OFF (`dmof`), do a `bmove`, and turn it
back on. So `POFF` is exactly right: the faulting access IS this block move,
running with paging off, i.e. addressing PHYSICAL memory.

### Why the trap report is all zeros

The routine multiplies its parameter by 0o4000 (2048 = one page), so the
parameter is a **physical page number**. The trap reported:

```
Physical address:          0         0B
Physical segment:                    0D
Logical address:           0         4B
```

Physical address zero is what `page_number * 2048` produces when the page number
is **0**. The report is not garbage - it is precisely describing a block move
from physical page 0.

So the chain is:

1. The swapper makes its 7-argument MON 377B request, passing the ADDRESSES of
   four record fields for SINTRAN to fill in.
2. SINTRAN answers with an **empty write-back** - measured, `(empty)`.
3. The field that should have received an allocated page number keeps its old
   value, zero.
4. The swapper turns paging off and block-moves from physical page 0.
5. Protect violation, shadow process 5SWAP, "The Swapper stopped".

The empty write-back IS the defect after all. What has to be established now is
WHY SINTRAN writes nothing back: whether our message hands SINTRAN the parameter
addresses in the form it expects, and what its handler for this swapper function
is supposed to return.

---

## The request identified: LSWPAGE = disk I/O on OUR swap file

A tally of `SWPFU` across the whole run (`SWPDECODER` dispatches on it) gives:

```
swpfu[LNEWSWAP:7 LSWPAGE:1]
```

So the swapper drove `LNEWSWAP` seven times (the poll loop) and **`LSWPAGE`
exactly once** - and that one is the 7-argument call. `LALLOPAGE` is NEVER
reached, so the earlier idea that this was a page-allocation request is wrong.

`LSWPAGE` (`MP-P2-N500.NPL:136112`) is labelled, in SINTRAN's own comment,
**"% Disk I/O"**:

```
LSWPAGE:                                              % Disk I/O
   X:=SWMSG; PSW1WAIT; CALL WN5STATUS
   11=:L; SWMSG+"SWPINFO"=:D; 5MBBANK; T:="XSDUNIT"; *MOVPA
                                    % Move parameters from swmsg to par.array for 5swap
   A:=XSDUNIT; CALL LOGPH; A=:X
   IF A>>="9BBHD" AND A<<"9EEHD" THEN               % Hard disc with disc optimization
      IF XABSFUNC/\77=60 THEN XABSFUNC+6=:XABSFUNC FI
      ...
      CALL M5TRANS; GO BUSR                          % start the transfer
      GO 5RDTRANSFER
   FI
   X:=SWMSG; CALL 5SWACTRT; GO NXTMSG                % Activate 5swap

5RDTRANSFER:                                         % Return after disc-transfer
   IF X.SSSTAT NBIT 4 THEN
      X:=SWMSG; CALL OKMONICO                        % Transfer ok
   ELSE
      X:=SWMSG; A:=1055; CALL EMONICO                % Error in transfer (swderr=1055)
   FI
```

### Two of the captured arguments are now identified

| captured | value | identification |
|---|---|---|
| arg[2] @0x08038406 | `0x240` = **0o1100** | the swap file's LOGICAL DEVICE NUMBER - `LIST-SWAP-FILE-INFO` printed `Logical device number:....... 1100B` for `(PACK-ONE:SYSTEM)SWAP-FILE-0:SWAP` |
| arg[3] @0x08024410 | `0x30` = **0o60** | `XABSFUNC` - the very value `LSWPAGE` tests with `IF XABSFUNC/\77=60` |

So the request is genuine and correctly aimed at the swap file we created. This
is the swapper asking SINTRAN to perform swap-file disk I/O.

### What this means for the empty write-back

`5RDTRANSFER` answers with `OKMONICO` on success and `EMONICO` with error 1055 on
failure. An `OKMONICO` restart plausibly carries no write-back pairs and `K=0` -
which is EXACTLY what was measured. So the empty write-back may mean "transfer
OK", not "nothing happened", and my earlier conclusion that the empty write-back
is itself the defect is NOT yet safe.

The open question is therefore narrower and different: did the transfer actually
move data into ND-500 memory, or did SINTRAN report success for a transfer whose
data never arrived where the swapper reads it?

---

### Trap for the unwary: the pack is mounted IN MEMORY

After a full run the working pack
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\BIGDISK0-L.IMG` is
**byte-identical to the pristine source** - `cmp` reports no differences at all,
even though `CREATE-FILE SWAP-FILE-0:SWAP` demonstrably succeeded and
`LIST-FILES` showed the file.

That is NOT a lost write. The harness mounts the image FROM DATA - it reads the
file into a byte array and attaches that ("Mount FROM DATA (in-memory,
non-destructive)"), so every SINTRAN write lands in memory and the file on disk is
never touched. Reads and writes both see the in-memory copy, so the run is
self-consistent.

Consequence worth stating: **you cannot test whether a disk transfer happened by
diffing the image file.** It will always match. That looked for a moment like
proof that no disk I/O occurs at all, which would have been a completely wrong
conclusion.

---

## THE TRANSFER HAPPENS - and lands where the ND-500 cannot see it

Instrumenting the SMD controller (transfer ring plus an unbounded range counter)
gives, for a full run:

```
----- SMD transfers (4370) swapFileHits=3 [65159168..75399168) -----
----- SWAP FILE transfers -----
  InitiateSeek       unit=0 pos=75397120 words=0    core=0x049400
  SeekCompleteSearch unit=0 pos=75397120 words=0    core=0x049400
  ReadTransfer       unit=0 pos=65159168 words=1024 core=0x27C800
```

`pos=65159168` is EXACTLY `SWAP-FILE-0`'s start (31816 pages x 2048), and
`words=1024` is exactly one 2048-byte page. So SINTRAN really does perform the
swap read, correctly aimed at the file we created.

**And the core address matches the swapper's own request.** From the MON call log:

```
MON 377B argc=7 ... | [4] @0x08024424=0x0027C800 | ...
```

`core=0x27C800` IS argument 4. The request, SINTRAN's handling, and the disk
transfer all agree. Nothing is being lost or mis-parsed on that path.

### The mismatch

The disk controller wrote the page to **ND-100 physical byte 0x27C800**. But the
5MPM shared window in this configuration starts at **0x00420000** (`mpmStart`),
and the swapper is an ND-500 program: when it reads its own address 0x27C800, our
emulator resolves that through `Nd500AddressBase`, i.e. to ND-100 physical
`0x420000 + 0x27C800 = 0x69C800`.

Those are different places. The data is delivered to one and read from the other,
so the swapper finds whatever was already at 0x69C800 - and the page-number field
it then feeds to the `dmof`/`bmove` stays zero, which is precisely the fault.

HYPOTHESIS, not yet proven. It is consistent with every measurement so far, but
the alternative has to be excluded: that 0x27C800 is genuinely an ND-100 address
here and some later step is supposed to move the page across. On real hardware
the disk DMA writes into shared memory both CPUs can see, and which side of the
window the controller is meant to address is exactly the distinction the BDIO
work had to pin down once already.

**Falsifier:** after the swap ReadTransfer, dump the page at ND-100 physical
0x27C800 and at 0x69C800. Non-zero at 0x27C800 and zeros at 0x69C800 confirms the
data landed outside the window the ND-500 reads through.

---

## Next step

Run that falsifier. Do NOT change the disk DMA address resolution until both
locations have been dumped on live data.

Do NOT reopen: capability walks, SWPSTAT, our trap path, "SINTRAN never
restarts", "we decline restarts", K/FUNCV carrying an error, or block-copy
address resolution. Each was measured, and the measurement is recorded above.

Everything else is measured: the swapper runs, the restarts arrive and are taken,
the resume point is right, and the poll-loop answers are accepted. Exactly one
exchange in the whole conversation returns nothing.

Do NOT reopen: capability walks (all fault records `(none)`), SWPSTAT (measured
0), our trap path (`trapsPosted=0`), "SINTRAN never restarts" (7 sent),
"we decline restarts" (7/7 taken).

Secondary, non-blocking: the harness types the next command while the monitor is
still printing, and the echo shows characters lost -
`copy-domain ...,"LINKAGE-LOADlist-domain` is two commands run together. It did
not cause any finding above, but it will corrupt a transcript that matters, so
the typing should wait for the prompt rather than race it.
