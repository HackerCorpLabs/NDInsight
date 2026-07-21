# CARVE: 030-S3SM5 band 0xD000..0xDAD3 ("Loading Swapper" spin) - what it really polls

TASK-8 carve. Read-only static analysis of the ND-100 segment `030-S3SM5` plus
the RetroCore 3022 interface source. Grades: [V] byte/line-verified by direct
reading of a cited file:line, [I] inference, [OPEN] needs a live trace / further
carve. ASCII only. Date: 2026-07-21.

Primary inputs read:
- `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\030-S3SM5.dis`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT`
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL`
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs`

Address math [V] (030-S3SM5.dis header, lines 10-12): base word 040000B = 0x4000;
the displayed octal address IS the runtime ND-100 word address. So the trace band
octal 150000..155323 == 0xD000..0xDAD3 (150000B=0xD000, 155323B=0xDAD3). Confirmed.

---

## 1. BOTTOM LINE (graded)

The given premise - that band 0xD000..0xDAD3 in 030-S3SM5 is "a CS-load hardware
VERIFY LOOP that polls a 3022 status register" - is **NOT supported by the
disassembly**. Grading each part:

- **What the loop polls:** [V] It does NOT poll any 3022 register. Every
  instruction that actually EXECUTES in the band is PLANC segment/file/swapper-
  table management. There is exactly ZERO ND-500 3022 IOX in the executed path
  (see section 3). So "polled cell/register = a 3022 status register" is FALSE.
- **What the loop actually is:** [V]+[I] A software loop (one of ~15 backward-
  branch loops in the routine, and/or an outer re-dispatch of the whole routine)
  that walks ND-100 kernel segment/process tables and does file OPEN/CLOSE +
  memory allocation (FIXC5) + block-size (SETBS) + file-index enumeration
  (GUIOI). Its exit is a DATA/TABLE condition in ND-100 kernel memory, not a
  hardware bit.
- **What is supposed to satisfy it in real HW:** [I] The REAL ND-500 swapper
  (process 0) running the 128-bit control-store microcode: it ANSWERs the swapper
  message and builds the ND-500 descriptor/segment/process tables. The ND-100
  side (5ACTSWAPPER / XACTRDY sends SWMSG, requester then waits LSWPWAIT for the
  swapper to answer - MP-P2-N500.NPL lines 425-534, 924-1052) proceeds only after
  that answer/those tables exist.
- **WHY it never satisfies in D4 emulation:** [I] The functional `CpuND500`
  cannot execute the 128-bit CS swapper, and the swapper is FAKED (22B
  StartProcessZero synthetic; the CPU never runs it - established root cause,
  MEMORY.md / ND500-D4-RUN-BLOCKER-FINDING). So the swapper never ANSWERs / never
  builds the tables, and the S3SM5 prep loop's data condition never becomes true,
  so it never falls through to the swapper-message body-fill higher up
  (builders at 140764B and 162150B per the .dis header, both OUTSIDE/above this
  band). Empty message -> ND-500 swapper crash at 0x0800913B is the downstream
  symptom.

**Fix side:** [I] SERVICER-side (functional swapper / Nd500MicrocodeServicer),
NOT 3022-side. The NDBusND500IF CS-load + read-back verify path already works
(section 4). See section 5 for the concrete fix point.

**Honesty note:** This is STATIC disassembly + emulator-source analysis. I did
NOT run a live single-step of the stuck PC. The EXACT polled cell and exact exit
branch require a live PC histogram (section 6, [OPEN] #1). What IS proven is a
negative: the band is not a 3022 hardware poll.

---

## 2. What 030-S3SM5's band contains (executed MON calls, [V])

All line numbers are into 030-S3SM5.dis; addresses octal.

| Addr (oct) | .dis line | Instruction | Meaning |
|-----------|-----------|-------------|---------|
| 150624 | 13672 | `153116  MON 116` | UNFIX segment (116B) |
| 152006 | 14298 | `153050  MON 50`  | OPEN file (50B) |
| 152106 | 14362 | `153043  MON 43`  | CLOSE file (43B) |
| 152203 | 14423 | `153043  MON 43`  | CLOSE file |
| 152327 | 14507 | `153050  MON 50`  | OPEN file |
| 152345 | 14521 | `153043  MON 43`  | CLOSE file |
| 153224 | 14952 | `153050  MON 50`  | OPEN file |
| 153227 | 14955 | `153043  MON 43`  | CLOSE file |
| 153563 | 15175 | `153064  MON 64`  | WarningMessage / ERMSG (64B) |
| 154176 | 15442 | `153061  MON 61`  | MemoryAllocation / FIXC5 (61B) |
| 154252 | 15486 | `153076  MON 76`  | SetBlockSize / SETBS (76B) |
| 155507 | 16155 | `153217  MON 217` | GetAllFileIndexes / GUIOI (217B) |

Plus, throughout the band: `LDXTX` / `STZTX` / `MOVEW` (segment-table / linked-
list walks, e.g. 154354 LDXTX, 155207 STZTX) and many local scan loops. Example
verified linked-list scan at the top of the band (016330-016345 of the .dis,
addr 155765..156005): `LDA I ,X 163 ; JAZ ; SKP IF DA UEQ ST ; JMP -6` then
`AAX 1 ; JMP -32` - a classic "walk the chain until the key at ,B -77 matches"
loop. This is table search, not I/O.

This is the segment-loader / swapper-prep body: it opens files, allocates and
sizes memory, walks segment tables, and (higher up, at 162150B, above this band)
fills the swapper message. It is exactly the kind of code that DEADLOCKS if the
segment/table state it is waiting for is produced by a swapper that never runs.

---

## 3. There is NO 3022 poll in the band ([V], the key negative)

Grep of the whole .dis for real ND-500 3022 IOX: the ND-500 interface IOX devices
are in the 0650B..0777B range, e.g. `020211  164776  IOX 776 ; ND500_4 Read
Locked` (.dis line 33885), `164652 IOX 652` (33884). **None of the 0650-0777
devices appear anywhere in 150000..155323.**

The only IOX/IOT-decoded WORDS inside the band are:
- `151073  163355  "IOT 3355"` (.dis line 13839)
- `154071  166145  "IOX 2145"` (.dis line 15373)
- `154272  163056  "IOT 3056"` (.dis line 15502)

Each of these sits INSIDE a compiler-emitted pointer/jump table, immediately
after a `JPL I <ptr> ; JPL I <ptr> ; JMP <handler>` dispatch, and is flanked by
address-constant words that also mis-decode as instructions. E.g. around 154071
(.dis 15369-15378):
```
154065  135021  JPL I 21    -> 154106      ; dispatch
154066  124370  JMP -10     -> 154056
154067  043740  MIN I ,B ,X -40            ; <-- table data
154070  036450  LDF ,X ,B 50               ; <-- table data
154071  166145  IOX 2145                   ; <-- table data (NOT executed)
154072  064234  SUB -144                   ; <-- table data
154073  000017  STZ 17                     ; <-- table data
154074  000255  STZ -123                   ; <-- table data
154075  064265  SUB -113                   ; <-- table data
154076  043660  MIN I ,B ,X -120           ; <-- table data
```
These are the target-address / branch-table words the preceding `JPL I` reads;
they are never reached as instructions. Verdict [V]: no executed IOX in the band.

Cross-check that the CS-load IOXT poll is in a DIFFERENT segment: the emulator
source comment (NDBusND500IF.cs line 2128) attributes a "SAA 0; AAT 17; IOXT /
RETG5:=0" CS-load sequence to address 153011B. But 030-S3SM5 at 153011B decodes
(.dis 14813-14817) to:
```
153011  054711  LDX ,B -67
153012  046002  LDA ,X 2
153013  174355  BSET ONE 150 DA
153014  006002  STA ,X 2
153015  124272  JMP -106    -> 152707
```
- a table bit-set loop, NOT an IOXT. So the real CS-load 3022 poll (RETG5/5CLOST/
RSTA5) lives in the MON-60 / N500M overlay (low addresses), not in this segment's
0xD000 band. [V]

---

## 4. Emulator 3022 CS-load / verify already works ([V])

NDBusND500IF.cs implements the CS store + read-back verify the loader checks:
- `csStore[index] = csBreakRegister` on CNTCLK LOAD, `csReadLatch = csStore[index]`
  on READ (lines 1699-1718), DATA-OUT enabled by DUEN/EDUTEN (1719-1722). The
  comment at 1686-1692 states the read-back verify is satisfied now (was the old
  "Error in loading Control Store" wedge).
- RETG5 (IOX +17) micro-clock stop/restart -> 5CLOST (bit 9) set/cleared
  (lines 2102-2137); MicroClockStopped modeled at 2115/2133.
- The "Loading Control Store" wedge (5CLOST stuck / idle RSTA5 = $204) is
  documented as FIXED (lines 1827-1851, 1965-1988).

So the 3022-side CS-load verify is NOT the current stall. [V for code presence;
I for "not the stall" - consistent with the band containing no 3022 poll].

---

## 5. The concrete TASK-8 FIX POINT (graded)

[I] The map from "the thing S3SM5's loop waits for" to the emulator is NOT a
register in NDBusND500IF. It is the SWAPPER'S EFFECT:

- Real HW: ND-500 swapper (process 0) runs, ANSWERs the swapper message (MP-P2-
  N500.NPL: MSWSTART @133642, 5ACTSWAPPER @134154/135367, XACTRDY "Reactivate
  nd-500 with swapper message", answer handling @135550-135604), and builds the
  ND-500 descriptor/segment/process tables. Those writes make the ND-100 table
  condition the S3SM5 loop scans for become true.
- Emulator: the functional swapper is faked; `CpuND500` never executes the CS
  swapper, so none of that state is produced.

Fix point [I]: make a FUNCTIONAL swapper genuinely ANSWER the swapper message and
write the ND-500 tables (the Nd500MicrocodeServicer / functional-swapper work,
Path B in MEMORY.md), so the data cell the S3SM5 loop tests flips. This is
SERVICER-side. No change to the 3022 CS-load/verify registers is indicated.

[OPEN] The exact ND-100 cell/table entry to make true = the exit-test cell of the
specific non-terminating loop, which needs the live trace (section 6).

---

## 6. [OPEN] list

1. **[OPEN] EXACT polled cell + exact exit branch.** The observed PC min/max
   (0xD000..0xDAD3) spans the WHOLE ~1363-word routine, so the trace shows the
   entire body cycling, not one tight poll - static disassembly cannot say WHICH
   backward loop is the non-terminating one. Need a live PC histogram / single-
   step to pick it and read the memory cell its `SKP IF ... ; JMP` tests.
   Structural candidates to check first: outer loop 155225 `JMP -103 -> 155122`
   (.dis 15977); segment-chain scans at 155765.. (.dis 16330) and 156020..; the
   big back-jumps 153021 `JMP -176 -> 152623`, 152112 `JMP -145 -> 151745`,
   151250 `JMP -147 -> 151101`.
2. **[OPEN] Fall-through gate to the message builder.** .dis header says swapper-
   message builders are at 140764B and 162150B (both outside this band). Confirm
   via live trace that the band-spin is what blocks the fall-through to 162150B
   (empty message -> 0x0800913B crash), vs an independent hang.
3. **[OPEN] Which MON call, if any, gates the loop.** OPEN/FIXC5/SETBS/GUIOI all
   return into the band (PC stays in 150000..155323), so they appear to SUCCEED
   and return; but a live trace should confirm none of them silently loop-retry.
4. **[OPEN] Identity/role of 030-S3SM5.** Symbols found are field offsets only
   (MSWIN=5, MSM51=27B, X5SM5=40B, 5SSM5=62B in SYMBOL-1-LIST/N500-SYMBOLS); the
   segment's precise NPL source (which .NPL compiles to S3SM5) was not located in
   this pass - would name the loop and its exit variable directly.

---

## 7. Summary of grades

- [V] Band = 0xD000..0xDAD3; base 0x4000; addresses are runtime words.
- [V] Executed content of the band = PLANC segment/file/swapper-table management
  (12 MON calls tabulated; LDXTX/STZTX/MOVEW chain walks).
- [V] No ND-500 3022 IOX executes in the band; the 3 IOX/IOT words are jump-table
  data. 3022 devices are 0650B-0777B, absent here.
- [V] Emulator's cited "153011B IOXT" is really a table bit-set loop in S3SM5;
  the real CS poll is in another overlay.
- [V] NDBusND500IF CS-load store + read-back verify + RETG5/5CLOST already modeled
  and the CS-load wedge is fixed.
- [I] The band is a SOFTWARE wait on swapper-produced ND-100 table state; premise
  of "3022 verify poll" is wrong.
- [I] Fix is servicer-side: functional swapper must ANSWER + build ND-500 tables.
- [OPEN] Exact cell/branch/loop + fall-through gate need a live single-step.
