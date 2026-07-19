# TRACE CYCLE ANALYSIS - SINTRAN L07 boot loop (10000-instruction window ending at 0x64BC)

**Date**: 2026-07-17
**Input**: trace_ring.txt (session scratchpad), 10000 lines, breakpoint at virtual 0x64BC.
**Citation convention**: "L<n>" = line number in trace_ring.txt (1..10000). File order is chronological.

## 0. Two corrections about the trace format (VERIFIED)

1. The first field on each line is NOT a sequence number; it is the decimal value of the
   instruction's virtual address (L1: field "26700" = 0x684C). File line order is the
   execution order.
2. The register values printed on a line are the state BEFORE that line's instruction
   (i.e. the result of the PREVIOUS instruction). Evidence: L9991 `LDA -$43,B` shows
   A=005A; the loaded value 0xFFFF appears on L9992. All reads below are corrected for
   this one-line lag.
3. 507 lines are truncated after `FLAGS[..]` (no register dump); all are skip/branch
   instructions (e.g. L213 `BSKP ZRO $78 DA`). Their register state is taken from the
   neighbouring lines.

Context identification key (VERIFIED from B-register + page-table switches):

| Context | B reg | Code addresses | Role (VERIFIED behavior; naming INFERRED) |
|---|---|---|---|
| FS-RT program | B080 (also segment contexts B527/B524/B51E/B126) | 0x649x-0x68xx, 0x46xx, 0x6Cxx, 0x7Exx | file-system RT program, level 1 |
| Scan program | datafield 79F7; B walks D690/CE18/BC18 | 0x9812-0x9BFF + 0x9A3D-0x9A90, 0x9Cxx | boot-time scan, runs on its own PIL (activated via `IRW $18 DP`) |
| Message printer | B5AF | 0x9843-0x988B (DIFFERENT page table than scan, same virtual page) | level-1 program printing an error text via MON 2 |
| MON trap handler | 0A25 | 0x4DCB-0x4DCA | level-14 internal-interrupt/MON dispatcher |
| Monitor task level | B0DF/0804 | 0x2C00-0x2E70, 0x0B8B-0x0B99, 0x2CE9 | monitor/dispatcher (activated via `IRW $20 DP`) |
| Terminal out driver | 11B2 | 0x2F8A-0x2F9A gate + 0x4BBD driver | level 13 (`IDENT PL13` at L975), does `IOX $B` |
| Disk-layer list walker | 27AD/79F7 | 0x3C15-0x3C9D | level-2 routine walking request list at 0x32D0 |

---

## 1. Full cycle timeline (task 1)

The window covers exactly ONE iteration of the FS-RT program's re-run cycle. Scan program
WAITs at 0x98C6: L746, L6310, L7502, L8295, L9902 (5 passes). Scan activations
(`IRW $18 DP` with A=9812, `SAA $8`, `MST PID` at 0x2C35-0x2C37): L230, L5672, L6836,
L7684, L9291 -- the scan is restarted at fixed entry 0x9812 every time (L234, L5676,
L6840, L7688, L9295).

| Lines | What runs | Event |
|---|---|---|
| L1-L132 | FS program segment (B=B527), code 0x67CB-0x684E | walks a list at X=B786..B796 (step 2, terminator T=B796, L5/L124); every entry reads 0 at [X] and [X+1] (L13-L16 etc.); exits via 0x67C1 `LDA $1E,B`=0 (L127-128), `SAA -$74` (A=FF8C, L131) |
| L133-L143 | monitor stub 0x09B1 -> context B51E -> 0x8B5C | `SAX $19`; `JPL I *$38` (L142-143) = internal call with function code 0x19 |
| L144-L205 | gates 0x2199/0x21B6/0x2243/0x2B20, then 0x0B8B | queues work to monitor level: `IRW $10 DP` A=2C1C, `SAA $4`, `MST PID` (L200-202) |
| L206-L233 | dispatcher 0x2C1C-0x2C38 | walks execution queue 614D -> 62D9 -> 79F7 (status words 8003, 8002, 0007; only 0007 passes the `BSKP ZRO $78 DA` test, L213/L219/L225); [79F7+C]=0243:0352 nonzero (L226-227); ACTIVATION 1 of scan: A=9812 -> `IRW $18 DP`, `SAA $8`, `MST PID` (L229-233) |
| L234-L746 | SCAN pass 1 (work item 0243:0352, units D690 + CE18) | detail in section 2; ends `WAIT` at 0x98C6 (L746) |
| L747-L811 | dispatcher + gate 0x21C0 `TRR PCR` (L771) -> printer B5AF | printer prints char 1 via `MON $2` at 0x986A (L811) |
| L812-L933 | per-char machinery | MON trap 0x4DCB (L812), driver body 0x8AC8/0x8AB6 (L818-823), monitor-call dispatch 0x4DD3-0x4DED -> 0x4CEE (MON 2 handler entry: `IRW $20 DP` A=2CE9, `MST PID` bit 4, L964-968), `WAIT` 0x4DCA (L842); monitor 0x2CE9..0x2D15 + 0x137F-0x13A3; level-1 P advanced 986B->986C at 0x2EA0-0x2EA3 (L929-931), `WAIT` 0x2EA4 (L933) |
| L934-L5515 | repeated per-char cycles | one `MON $2` per char (L941, 1229, 1359, ... 5268; 30 x MON 2 total); every ~1300 lines the level-13 terminal interrupt fires: `IDENT PL13` (L975), driver 0x4BBD `IOX $B` (L988, 2446, 3072, 3947, 4772, ...); printed text = "\r\n - STACK OVERFLOW AT " (byte values from `LBYT` at 0x9865, X index 0..0x17: 0D 0A 20 2D 20 53 54 41 43 4B 20 4F 56 45 52 46 4C 4F 57 20 41 54 20; L807/937/1225/.../4444) |
| L5516-L5554 | printer finishes prefix | printer continues at 0x987D (L5517), `MON $0000` at 0x988B (L5523); MON trap -> 0x4D9A -> `IRW $10 DP` A=2C7D (L5545-5549), `WAIT` 0x4DCA (L5554) |
| L5555-L5675 | MON dispatch 0x2C7D-0x2C87 -> handler 0x2EC4 | target datafield = 79F7 (X loaded at L5568, copied to D at L5572); queue walk 0x2555: 7A07 -> 4658 -> 4635 -> 79F7 found (L5576-5593); 0x1F9D: (79F7 - base) = 0A0E, RDIV 0x16 -> 0x75 -> table -> X=4635 (L5602-5614); caller PC 988C saved into [4635+0x15]=464A (L5624-5625); level-1 P parked at 0389 (`IRW $8 DP`, L5631); `TRR PCR` with [79F7+F]=3B8E (L5635-5636); NEW WORK ITEM: `LDD $9,X` = 0003:0352 stored to [79F7+C] (L5641-5642); falls into dispatcher 0x2C1C -> ACTIVATION 2 (L5672-5675) |
| L5676-L6310 | SCAN pass 2 (item 0003:0352 -> units BC18 + D690) | entry 0x9812 computes B pointers: 0003*8+0xBC00 = BC18 and 0352*8+0xB900 = D690 (L5683-5690); ends `WAIT` (L6310) |
| L6311-L6839 | dispatcher finds mailbox [I *$52] = 11B2 (L6317-6320) | handoff from level-13 driver; 0x2E09 -> 0x8547 -> 0x1850 counter block (D counter 0301->0302, L6339-6342); 0x3C15-0x3C9D (level 2): walks request list at 0x32D0 (phys bank 4): entries (79F7,0120) (7A0D,0121) (7A23,0122) (7A39,0123) (7A4F,0124), head 79F7 has [B]=6137 nonzero and status 0007 (L6645-6648), computes slot 0x329E, reads 0108:FFEE and 0044:0030 (L6660-6663), terminator FFFF (L6717-6719); ends re-queueing scan -> ACTIVATION 3 (L6836) |
| L6840-L7502 | SCAN pass 3 | same item 0003:0352; `WAIT` (L7502) |
| L7503-L7687 | same driver-mailbox path | ACTIVATION 4 (L7684) |
| L7688-L8295 | SCAN pass 4 | `WAIT` (L8295) |
| L8296-L8613 | FS-RT program RESUMES on level 1 (B=B080) | it executes 0x46xx code and issues `MON $53` at 0x46A7 (L8486); dispatch via `LBYT` at 0x2C82 (X=0053, L8578-8583) -> handler 0x2CA7 -> `IRW $20 DP` A=2C1F (L8585-8587); context switcher 0x2C00-0x2C14 (`SRB $8` ... `TRR PCR` PCR=438E, L8595-8613), `WAIT` 0x2C24 (L8614) |
| L8615-L8931 | monitor + FS continues | FS-RT issues `MON $3D` at 0x6499 (L8901, B=B080) -- this is the instruction immediately before the 0x649A loop head; dispatch (X=003D, L8938-8943) -> handler 0x2CCF -> `IRW $20 DP` A=2C15 (L8948-8950); context switch to segment B51E (`TRR PCR` PCR=238E, L8977), `WAIT` 0x2C1D (L8981) |
| L8982-L9294 | inside MON 3D: FS segment contexts | 0x2CDB -> 0x004A (L8996-8999); code 0x6C1A (B=B524): helper 0x1390-0x13A3 reads via T=4635 and returns D=0000 (L9204-9211), `SKP IF DD EQL 0` skips (L9213) -> ERROR PATH `SAA $5A` (A=0x5A=90 decimal) stored to [B+2] (L9214-9216); returns -7 (L9218-9219); context 0x7E34 (B=B51E) stores 0x5A again, returns -6 (L9229-9233); context 0x2E52 (B=B126 = RT-description, INFERRED): X=79F7, [79F7+C] vs [B126+0x24]=0003, `STA $11,X` (L9243-9251); queue to level 2 (L9253-9263: `LDT $C,X`/`STA $C,X` writes 0003 into [79F7+C], `IRW $10 DP` A=2C1C); dispatcher walks 614D->62D9->79F7 again (L9270-9288) -> ACTIVATION 5 (L9291) |
| L9295-L9902 | SCAN pass 5 (item 0003:0352) | `WAIT` (L9902) |
| L9903-L9988 | dispatcher: nothing left | mailbox = FFFF (L9909-9911), [I *$45]=0 (L9913-9914); `SAA $2 / MST PID` + `SAA $4 / MCL PID` (L9915-9918); 0x0B99 EXIT -> 0x2E62 (B=B126): [B+3]=1, [B+4]=0, `STA $12,X` writes 1 to [4635+0x12] (L9921-9930); paging gate 0x0171 (`TRR PCR` L9939 and L9947, trampoline via 0xB072 L9943-9944); 0x2D3F/0x19B7 (`TRR PCR` L9967, [79F7+F]=4B8E restored L9960/9965); `LRB $8` restores level-1 register block from B12D (L9972); dispatcher 0x2C00 again -> level 1 activated (L9984-9988) |
| L9989-L10000 | FS-RT resumes at 0x649A | flag test and re-assert, section 3; breakpoint at 0x64BC |

Summary of the wake chain (all VERIFIED by the lines above):
FS-RT `MON $3D` (0x6499) --> monitor error path (code 0x5A) --> queues scan work item
0003:0352 --> scan activation --> scan finds nothing, WAITs --> monitor returns to FS-RT
at 0x649A --> flag still -1 --> loop re-asserts -1 --> (release path at 0x64BD outside
window) --> next `MON $3D`. In parallel, a separate level-1 program (B5AF) prints
"\r\n - STACK OVERFLOW AT " one character per scheduling round, and its end-of-string
MON (0x988B) also re-activates the scan.

---

## 2. Scan program: every deciding branch (task 2)

Pass 1 (L234-L746) is representative; passes 2-5 differ only in the unit order
(BC18/D690 instead of D690/CE18 -- from the changed work item, see L5683-5690).

Entry 0x9812 (activation 1 context: L234-233 dispatcher; pass-2 entry shown L5676-5696):
- 0x9814 `SKP IF DA EQL ST`: A=79F7 vs T=0000 -> not equal -> 0x9815 JMP (L236-237, L5678).
- 0x981A `JAZ`: A=0243 (pass 1, L240) / 0003 (pass 2+, L5682) -> not zero -> continue.
  Unit pointer = A*8 + 0xBC00 (L5683-5685); second pointer = D(0352)*8 + base = D690
  (L5688-5690).
- 0x981F `JAZ`: A=0352 -> not zero (L245).
- 0x9824 `JAZ`: A=0000 -> taken to 0x9827 (L250).

Per-unit head test, subroutine 0x9BB9 (called from 0x982A / 0x982F / 0x9834):
- 0x9BC1 `LDATX $6` phys(bank T=0002) [unit+6] -> 0xE400 (L262-265).
- 0x9BC6 `BSKP ONE $18 DA` on A=E400 -> no skip -> 0x9BC7 JMP -> 0x9BCA (L266-267).
- 0x9BCE `LDATX $5` [unit+5] -> 0x004D (unit D690, L272-275) / 0x0049 (unit CE18,
  L315-318).
- 0x9BD3 `BSKP ZRO $8 DA` on 004D -> no skip; 0x9BD5 `SKP IF DD EQL 0` -> no; 0x9BD6 JMP
  -> 0x9BDD `BSKP ONE $0000 DA` (L276-279).
- 0x9BE2 `LDATX $0000` [unit+0] -> link to next unit: D690 -> BC18... value 0xBC18
  read but B compare 0x9BE9 `SKP IF DB UEQ ST` B=D690 vs T=D690 equal -> no skip ->
  0x9BEA JMP 0x9BEF (L283-292). For unit CE18 the link read gives 0000 -> 0x9BE7 `JAZ`
  taken (L326-330).
- 0x9834 -> 0x9BB9 with B=0000: `JAF` not taken -> EXIT (L337-341).

**The type test (the only comparison against 9 in the trace)** - three copies at
0x9C4C-0x9C4D, 0x9874-0x9875, 0x98B0-0x98B1:
```
LDATX $2            ; phys[unit+2] -> A = 0x01EC (unit D690) or 0x01F4 (unit CE18)
SHA ZIN SHR $6      ; A >>= 6      -> A = 0x0007 (both)
SAT $9              ; T = 9
SKP IF DA EQL ST    ; is unit type == 9 ?
```
Occurrences with values (all: A=0007 vs T=0009, NOT equal):
- L360-L367 (0x9C46/0x9C4C/0x9C4D, unit D690, raw 01EC -> 7): not-9 path -> 0x9C50 ->
  buffer-chain subroutine 0x9AA6.
- L549-L556 (0x986E/0x9873/0x9874/0x9875, unit D690, raw 01EC -> 7; raw dump L534-556):
  not equal -> `JMP *$9` -> 0x987F -> chain subroutine 0x9B08.
- L655-L662 (0x98AA/0x98B0/0x98B1, unit CE18, raw 01F4 -> 7): not equal -> 0x98BB ->
  0x9B08.
The equal-to-9 branch is NEVER taken in this window. (INFERRED: 9 is a device/unit type
code; type-9 units would get different treatment. The window contains no unit of type 9.)

Buffer-chain walk 0x9AB9-0x9AC5 / 0x9B1B-0x9B26 (per unit):
- iterates X over chain 064C -> 0648 -> 0644 -> 060C -> 0550 -> 0000; per element
  `LDATX $3` returns F400 (busy-flag word, once FC00 at L432-435) and `LDATX $2`/
  `LDXTX $0000` return the link (FBE0, FBDE, FBDC, FBD8, FBDA); `JAZ` on the F400 value
  never zero (L388, L399, ...); `JXZ` ends the walk when X=0 (L437-438, L632).
  In pass variants the walk also reads a count D=0193,0192,0191,0183,0154 (L582-630).
- 0x9B8A-0x9B9C search loops: `SKP IF DT GRE SX` with T=0006, X=0,2,4,6,8 (L457-478)
  then `SKP IF DX MLST ST` with T=F900, X=F8F4..F900 (L486-511).

End-of-pass / WAIT decision, subroutine 0x9A3D (L690-L746; identical at L9862-9901):
- Called from 0x98C5 after 0x98C2 `BSKP ZRO $10 DA` on A=004D did not skip (L688-690).
- 0x9A45/0x9A46: A=79F7 (own datafield) `SKP IF DA UEQ 0` -> nonzero (L695-696).
- Computes slot: (79F7-base)=18C0, RDIV T=0x16 -> A=0120, + base -> X=F33C (L697-705).
- 0x9A51 `LDATX $0000` phys(bank 1)[F33C] -> **0x0000** -> 0x9A52 `JAZ` TAKEN (L706-707).
- Table scan loop 0x9A5C-0x9A8F, counter A=0..2, limit T=[I *$3C]-1 = 2-1 = 1
  (L715-716): entries at phys(bank 1) F1E0 and F1E2; `LDDTX $0000` returns **0000:0000
  both times** -> 0x9A65 `JAZ $27` TAKEN both times (L722-723, L735-736).
- Exit when counter A=0002 > T=0001: `SKP IF DT GRE SA` falls through (L743) ->
  0x9A5F -> 0x9A90 -> return to 0x98C6 = `WAIT` (L744-746).

**The exact "keep going" test for the scan**: the scan has work only if
(a) the per-slot flag word at phys bank1:0xF33C is nonzero (0x9A52 JAZ), or
(b) either double-word entry at phys bank1:0xF1E0 / 0xF1E2 is nonzero (0x9A65 JAZ).
In every one of the 5 passes all three reads return zero, so every pass ends in WAIT at
0x98C6. The scan never terminates the outer cycle and never finds anything to do; it is
purely re-armed from outside (activations L230, L5672, L6836, L7684, L9291).

No comparison against 0x50 or 0x51 exists anywhere in the window (exhaustive search over
all SAT/SKP operand and register values; zero hits).

---

## 3. FS RT program flag test at 0x649D (task 3)

L9989-L10000 (register state corrected for the one-line lag):
```
L9989  649A  JMP *$2            ; resume point after MON 3D; A=005A (leftover error code)
L9990  649C  STZ -$2A,B         ; B=B080: clear [B056]
L9991  649D  LDA -$43,B         ; loads [B080-0x43] = [B03D] -> A = 0xFFFF  (seen on L9992)
L9992  649E  SAT $1             ; T = 1                                     (seen on L9993)
L9993  649F  SKP IF DA EQL ST   ; 0xFFFF == 1 ? NO -> no skip
L9994  64A0  JMP *$16           ; -> 0x64B6 ("work in progress" path)
L9995  64B6  SAA -$1            ; A = 0xFFFF
L9996  64B7  STA -$43,B         ; re-writes -1 into [B03D]
L9997  64B8  LDX -$66,B         ; X = [B01A] = 0x4635
L9998  64B9  STZ $16,X          ; clear [464B]
L9999  64BA  LDX $A,X           ; X = [463F] = 0x4658
L10000 64BB  STZ $13,X          ; clear [466B]; PC now 0x64BC = breakpoint
```
So before re-asserting -1 the program does exactly: STZ [B056], the flag test, then
clears [4635+0x16]=0x464B and [4658+0x13]=0x466B. 0x4635 and 0x4658 are the two entries
seen in the monitor's active-program list walk (7A07 -> 4658 -> 4635 -> 79F7, L5576-5591).
Earlier in the same window the FS-RT ran 0x46xx code (MON $53 issued from 0x46A7, L8486)
and 0x6499 `MON $3D` (L8901) -- 0x6499 is the instruction directly before this loop head,
i.e. each iteration is: MON 3D, test flag, re-assert -1, continue at 0x64BC+.

---

## 4. All writes to [B-0x43] / virtual 0xB03D (task 4)

Exhaustive search over all 10000 lines:
- `STA -$43,B` occurs exactly ONCE: L9996, B=B080, value written = A = 0xFFFF (-1).
- No `STZ/STT/STX/STD/MIN -$43,B` with B=B080 anywhere.
- No register (A, D, X, B, T, L) ever holds the value 0xB03D at any point in the window,
  so no indexed/indirect store can have targeted 0xB03D either (a store via ,X or ,B
  addressing needs the base register near B03D; closest B-relative frames observed are
  B080, B0DF, B126, B527, B524, B51E, B5AF -- none with a displacement reaching B03D
  except the -0x43 form itself).

**VERIFIED: nothing in this 10000-instruction window ever writes 1 (idle) to the flag;
the only write is the loop's own re-assertion of -1 at L9996.**

---

## 5. RELEASE execution 0x8B26-0x8B4A (task 5)

ABSENT. The only executed addresses in 0x8B00-0x8B60 are:
- L142 0x8B5C `SAX $19` and L143 0x8B5D `JPL I *$38` (context B51E) -- an internal-call
  stub with function code 0x19, leading to the gate at 0x2199 (L144), NOT the release
  body. The release core (`AAA -1` at 0x8B3E) never executes in this window.

The FS-RT call at 0x64BD is NOT in the window: the trace ends with the breakpoint at
0x64BC (L10000 is the instruction at 0x64BB). Therefore the release/disk-write of this
iteration happens after the captured window; the window instead contains the tail of the
PREVIOUS iteration's activity (L1-L233) and the whole MON 3D + scan + message-printing
middle part.

---

## 6. What re-activates the scan after WAIT at 0x98C6 (task 6)

The scan NEVER self-continues. Every restart is a fresh activation at entry 0x9812 via
the identical dispatcher tail at 0x2C33-0x2C38:
```
2C33 LDA *$28       ; A = 9812 (stored start address)
2C34 IOF
2C35 IRW $18 DP     ; write 9812 into the P register of the scan's PIL
2C36 SAA $8
2C37 MST PID        ; set that PIL's PID bit
2C38 ION
```
(L229-233, L5670-5675, L6832-6839 by analogy, L7680-7687 by analogy, L9289-9294.)

Precondition checked by the dispatcher every time (0x2C1C-0x2C30): walk the execution
queue 0x614D -> 0x62D9 -> 0x79F7; status word [entry+1] must pass `BSKP ZRO $78 DA`
(8003 and 8002 fail, 0007 passes; L213-225, L5654-5667, L9273-9286) and the work item
[79F7+0xC] must be nonzero (`LDD $C,X` / `JAF`; L226-227, L5668-5669, L9287-9288).

The five triggers that made the dispatcher run with 79F7 armed:
1. L192-L233: the window-start FS context (function-code 0x19 stub at 0x8B5C, L142-143)
   via gates 0x21B6/0x2243/0x0B8B (queue element with T=0243, L190-205).
2. L5523-L5675: the message printer's `MON $0000` at 0x988B; the MON handler at 0x2EC4
   resolves target datafield 79F7 (L5568-5572), maps it to list entry 0x4635 (L5602-5614)
   and installs work item [79F7+9] = 0003:0352 into [79F7+C] (L5641-5642).
3. L6311-L6839 and 4. L7503-L7687: the level-13 terminal-driver handoff mailbox
   ([I *$52] = 11B2, L6317-6320, L8618-8621) followed by the level-2 list walker
   0x3C15-0x3C9D over the request list at phys bank4:0x32D0, whose HEAD entry is
   (79F7, 0120) (L6639-6646).
5. L8982-L9294: inside FS-RT's `MON $3D`: the 0x6C1A helper gets D=0 from the table read
   via T=4635 (L9204-9213), takes the error path A=0x5A (=90 decimal) (L9214-9216),
   unwinds through 0x7E34 and 0x2E52 (B=B126) which re-arms [79F7+C]=0003 (L9258-9259)
   and queues the dispatcher (L9260-9263).

Queue/datafield addresses involved (from register values): execution queue head chain
0x614D -> 0x62D9 -> 0x79F7 (L215-223); active-list 0x7A07 -> 0x4658 -> 0x4635 -> 0x79F7
(L5574-5591); scan datafield 0x79F7 (work item at +0xC, parameters at +9, saved PCR at
+0xF = 3B8E/238E/438E/4B8E, L5635, L8976, L8609, L9960); unit descriptors D690 / CE18 /
BC18 (phys bank 2); scan work tables phys bank1:0xF33C, 0xF1E0, 0xF1E2; request list
phys bank4:0x32D0.

---

## 7. Additional finding: "STACK OVERFLOW AT " is being printed (VERIFIED)

A level-1 program with datafield B=B5AF (its virtual page 0x98xx belongs to a DIFFERENT
page table than the scan's -- same virtual addresses, different instruction words) prints
one character per scheduling round via `MON $2` (OUTBT; 30 calls: L811 ... L5394). The
characters, read by `LBYT` from the string at word address 0x9843 with byte index X=0..0x17,
are exactly:
```
0D 0A 20 2D 20 53 54 41 43 4B 20 4F 56 45 52 46 4C 4F 57 20 41 54 20
"\r\n - STACK OVERFLOW AT "
```
(L807, L937, L1225, L1355, ..., L4444; index X increments 0,1,2,...,0x17.)
The address digits after "AT " are not printed inside this window; after the 23rd
character the program proceeds to 0x9878/0x987D (WAIT-context saved-P values at L4577,
L5516) and issues the `MON $0000` at 0x988B (L5523) that re-activates the scan.
INFERRED: this is a SINTRAN error-report program announcing a stack overflow somewhere;
it is the only console output in the window and it happens on EVERY loop iteration
(its per-character interleave spans L747-L5523, i.e. most of the window).

Also VERIFIED: inside FS-RT's MON 3D, error code 0x5A (90 decimal) is generated at
0x6C1D and stored twice (L9214-9216, L9229-9231), and A still holds 0x005A when the
FS-RT main loop resumes at 0x649A (L9989). INFERRED: the MON 3D returns error 90 every
iteration; identifying which SINTRAN error 90 is (file-system vs I/O numbering) needs
the manuals -- not determinable from the trace.

---

## 8. Conclusion: what must change for the loop to end

VERIFIED from this window:
- The loop gate is [0xB03D] ([B-0x43], B=B080): the value read is -1 (L9991-9992);
  the exit condition is == 1 (L9992-9993); the only writer in the window is the loop
  itself re-writing -1 (L9996).
- The scan program is NOT the blocker: all 5 passes find its work tables
  (bank1:F33C/F1E0/F1E2) empty and WAIT immediately; it is merely re-armed by the
  monitor every iteration.
- The release body (0x8B26-0x8B4A) that would complete the operation never runs in the
  window; the FS-RT reaches its call point 0x64BD only after the breakpoint.

Therefore: for the loop to end, something must store 1 into [0xB03D] -- and per the
prior analysis that is the completion path of the RELEASE-USER operation. In THIS window
the iteration spends itself in `MON $3D` (0x6499), which fails internally with code 0x5A
(90 decimal) at 0x6C1D because the helper at 0x1390 returns D=0 for the entry at
T=0x4635 (L9204-9213). INFERRED (best-evidenced statement): the condition that must
change is that this MON 3D-internal lookup for entry 0x4635 must succeed (D nonzero at
L9210) instead of taking the `SAA $5A` error path; as long as it fails, the completion
path never stores 1 into the flag, the FS-RT re-asserts -1, and the cycle (including the
"STACK OVERFLOW AT" printout) repeats forever.
