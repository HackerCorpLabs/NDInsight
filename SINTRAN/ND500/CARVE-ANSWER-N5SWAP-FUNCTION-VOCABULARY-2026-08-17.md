# CARVE ANSWER - the N5SWAP function vocabulary and SWMSG work protocol (2026-08-17)

The answer to milestone 11's protocol question: how SINTRAN's N5SWAP side
hands the ND-500 SWAPPER-K01 domain its work, function by function - carved
from the swapper PSEG/DSEG bytes and then EXECUTED end to end on the
microword engine (CpuND500UC) for fn 22B and fn 24B.

Evidence tags: **PROVEN** = read from the cited bytes/listing AND (where
stated) executed on the microword engine with the real CONT-STORE-10611
microcode. **INFERRED** = deduced, not fact. **CORRELATION** = ND-100-side
name matched by index only.

Sources:
- Disassembly: `SINTRAN/ND500/swapper/swapper-k01-pseg.asm` (octal addresses;
  `1000000000` = virtual 0x08000000; data `1000440260` = DSEG 0x240B0).
- Raw segments: `SINTRAN/ND500/swapper/SWAPPER-K01.PSEG` / `.DSEG`.
- Handler catalog (prior art, 29 handlers characterized):
  `SINTRAN/ND500/swapper/swapper-k01-handlers.md` - this doc corrects one
  address-arithmetic slip there (0x128A4 -> 0x128E4, see section 4).
- Execution proof: `SwapperN5Swap_Fn22B_SetConfig_ThroughJumpgTable` and
  `SwapperAttempt_PopulatedDseg_FullRhythm` in
  `RetroCore/Nuget/HackerCorpLabs.Emulation.CPU.ND500UC/tests/SwapperAttemptTests.cs`,
  158/158 green 2026-08-17.

---

## 1. The work loop (PROVEN, executed)

The swapper's main loop (PSEG 1000101077..) announces "give me work" with

```
1000101077  call $1777777777777000000377,$4,$1000225050,$1000440260,$1000440264,b.24   ; MON 377B
```

MON 377B with argc = 4. Argument ADDRESSES (delivered by-reference in the
message's address array): arg 1 = 0x08012A28 (a status/record cell,
$1000225050 = 0o225050), arg 2 = **the function-code cell 0x080240B0**,
arg 3 = **the SWMSG-address cell 0x080240B4**, arg 4 = a frame local. The stop record's
saved P is 0x08008255 (the MON call's return address) - that savedP is how
SINTRAN (and the test harness) recognizes the main-loop announce among other
MON 377B stops.

SINTRAN answers with MICFU 24B (3MONCO resume): the reply's **write-back
mask** stores value slots through the announced argument addresses. Writing
slot 2 (the fn cell) and slot 3 (the SWMSG ND-100 word address) delivers one
work order. Mask bit numbering as the harness uses it: mask 0b0110 writes
value slots 1 and 2 of the argument array = the fn cell and the address cell
(PROVEN by execution: the values land in DSEG 0x240B0/0x240B4, PAGED).

The loop then (all PROVEN from the listing, executed on the engine):

```
1000101171  w1 := [0x240B0]        ; fetch the delivered fn code
1000101177  w1 =: [0x240B8]        ; dispatch copy ("fnEcho")
1000101205  w stz [0x240B0]        ; clear the delivery cell
1000101213+ incr [0x28EB8 + k]     ; per-code stats (base $1000507270+, indexed)
1000101267+ bound check 0 <= fn <= 34B   ; reject -> worker 1000003057 (MON 377B sub-fn 2)
1000101350  w1 := [0x240B8]
1000101356  h riom [0x240B4],0x240BC,[0x2403C + fn*4]   ; message intake
1000101427  w incr [0x26280]       ; total-message counter
1000101504  fn = 5 -> skip the gate
1000101515  fn = 3 -> skip the gate
1000101526  comp2 [0x128E4],$24    ; slot-table bound gate (see section 4)
1000101535  if <= go $1643         ; gate shut -> reject path 1000103400
1000101540  w1 := [0x240B8]
1000101546  jumpg [0x26198 + fn*4] ; the 29-way dispatch
```

The reject path at 1000103400 writes constant 1043B into the fn cell
[0x240B0] and jumps back to the loop head (PROVEN from bytes; not executed).

## 2. The SWMSG intake (PROVEN, executed)

- The SWMSG lives in ND-100 memory; the cell [0x240B4] holds its **ND-100
  WORD address** (the harness stages it at word 0x300).
- RIOM copies `count` halfwords from that ND-100 address to the DSEG intake
  buffer at **0x240BC** (SWMSG halfword k lands at 0x240BC + 2k).
- The per-function count comes from the table at **DSEG 0x2403C: 29 32-bit
  WORDS, indexed fn*4, ending exactly at the fn cell 0x240B0**. Counts
  (halfwords) per fn, from the raw DSEG bytes:

  | fn (oct) | count | fn (oct) | count | fn (oct) | count |
  |---:|---:|---:|---:|---:|---:|
  | 0 | 13 | 12 | 20 | 24 | 8 |
  | 1 | 10 | 13 | 9 | 25 | 11 |
  | 2 | 15 | 14 | 54 | 26 | 8 |
  | 3 | 138 | 15 | 11 | 27 | 8 |
  | 4 | 9 | 16 | 13 | 30 | 34 |
  | 5 | 70 | 17 | 13 | 31 | 16 |
  | 6 | 8 | 20 | 13 | 32 | 34 |
  | 7 | 0 | 21 | 24 | 33 | 14 |
  | 10 | 8 | 22 | 24 | 34 | 9 |
  | 11 | 11 | 23 | 9 | | |

  (fn 22B = 24 halfwords, fn 24B = 8 - both executed. fn 7's count is
  ZERO: its RIOM copies nothing, matching its no-payload `$66`-group
  handler shape.)

- **RIOM's operand types (PROVEN via the manual + executed)**: the manual's
  format line (ND-05.009.4 §16.23) is
  `H RIOM <ND-100 addr/r/W>,<buffer/w/H>,<no of halfwords>` - operands 1 and
  3 are WORDS. That makes the post-indexed count operand `$1000440074+`
  scale fn by 4 (the word table above) and the address/count reads 4 bytes
  wide. The microword engine originally decoded all three operands with the
  instruction's H type; fn 22B then read its count from 0x24060 (= 0) and
  the intake stayed empty. Fixed 2026-08-17 (`RowFlags.IoWordOperands`);
  the functional CpuND500 had the same bug and the same fix on 2026-07-29
  (`Instructionset.Init.cs`, OperandDataTypeConstraint.WordOnly).

## 3. The record base - NO pointer in the message (PROVEN, executed)

Earlier working notes (milestone 10) claimed the loop reads the message's
first word back as a record pointer. WRONG - the loads

```
1000101377 / 1000101554 / 1000102444 ...   r := $1000440274
```

use operand descriptor **317B = a :W CONSTANT**: R becomes the intake
buffer's **ADDRESS 0x080240BC**, not its content. Handler stubs then address
the record R-relative. For fn 22B the stub passes `by1 laddr r.20`
(record-short descriptor 204B, word-scaled d=4 = +16 bytes), so the worker's
record base = intake + 16 = **SWMSG halfword 8**. Field offsets are plain
byte offsets off that base.

Executed proof (fn 22B, worker 1000064476): with SWMSG halfword k staged as
0x3E00+k, the three stores landed

| store (listing) | reads | lands in | observed |
|---|---|---|---|
| `h1 := r.12; w1 =: [224114B]` | SWMSG hw 13 | DSEG 0x1284C | 0x3E0D |
| `h2 := r.2; w2 =: [246434B]`  | SWMSG hw 9  | DSEG 0x14D1C | 0x3E09 |
| `h3 := r.4; w3 =: [246440B]`  | SWMSG hw 10 | DSEG 0x14D20 | 0x3E0A |

byte-exact against the listing's operand math. (Halfword loads widen to a
zero upper half in the stored word for these positive values.)

What SWMSG halfwords 9/10/13 MEAN on the SINTRAN side is **UNKNOWN** - the
cells they land in are only named "config words" by INFERENCE (fn 22B has no
MSW* symbol; see section 5).

## 4. The slot-table gate (PROVEN bytes; cell address CORRECTED)

For every fn outside {3, 5} the dispatch is gated:

```
1000101526  comp2 $1000224344,$24   ; [0x128E4] vs 24B
1000101535  if <= go $1643          ; shut -> reject path 1000103400
```

**Address correction**: octal data address 224344B = **0x128E4**, not the
0x128A4 that `swapper-k01-handlers.md` printed (same octal, slipped hex; a
dated correction note has been added there). The cell is the count of valid
"Table A" slots; it is written by the fn-3 worker at PSEG 1000061202 (w4 =:)
and at 1000062605/1000071544. fn 3 and fn 5 bypass the gate (checks at
1000101504/1000101515) - which is how the table can be initialized in the
first place.

**INFERRED staging** (used by the test harness): pre-setting [0x128E4] to
100B stands in for a completed fn-3 initialization so fn 22B/24B pass the
gate. Driving fn 3 for real needs the zpgu/zwip page instructions - an open
microword-engine instruction gap.

**RETIRED 2026-08-17 (milestone 12)**: the zpgu/zwip family is carved and
implemented, and fn 3 is now DRIVEN end to end - the gate cell [0x128E4] is
COMPUTED by the real fn-3 worker (1000061202: [0x128EC] + [0x128E8]); the
harness stages only the released-page accumulator [0x128EC] (the per-page
release loop still needs GETBF/PUTBF/UDIV). Full carve:
`CARVE-ANSWER-N5SWAP-FN3-SLOT-TABLE-INIT-2026-08-17.md`.

## 5. The function vocabulary (fn 0..34B)

The full 29-handler characterization (stub shapes, workers, call trees,
shared tables) is `swapper-k01-handlers.md` - not repeated here. The
vocabulary as far as NAMES go:

**PROVEN (ND-100-side coordinator carve, exact codes):**
`MSWFI = 0`, `MSWSTART = 7B`, `MSWPFAULT = 12B`, `MSWSWAIT = 24B`,
`MSWDO = 34B`. All other names in the handlers doc are INFERRED from the
proven handler shapes; index-to-name matches beyond these five are
CORRELATION only.

**Executed on the microword engine (2026-08-17):**
- **fn 22B** (dec 18, no MSW symbol - the table has a gap): three-field
  config store, section 3. End to end: announce -> 24B write-back -> RIOM
  (24 halfwords) -> gate -> jumpg -> stub 0x08008524 -> worker 0x0800693E ->
  stores land -> common tail -> next announce. The name "set swapper
  configuration words" is INFERRED; the stores are PROVEN.
- **fn 24B = MSWSWAIT** (dec 20): handler 0x08008652 is a bare `go` to the
  common exit - a PROVEN no-op in-domain (executed: config cells untouched,
  message counted, loop re-announces). The actual "wait" is presumably
  ND-100-side (INFERRED).
- **fn 1** (milestone 10): the small-fn arm (not the jumpg) - intake +
  bookkeeping executed end to end.

**Not yet drivable end to end**: fn 3/5 (need zpgu/zwip and real slot-table
state), the RPHS family (fn 10B/26B/34B need the physical-segment transfer
path), and everything whose SWMSG field meanings are unknown. The message
counter [0x26280], fn echo [0x240B8], per-code stats ($1000507270+) and the
reject code 1043B are protocol observables any future harness can pin on.

## 6. What this closes and what stays open

Closed (PROVEN, executed): the announce/write-back work handshake, the
RIOM intake geometry (word count table, word-typed RIOM operands, intake at
0x240BC), the record-base rule (intake+16, no pointer), the gate cell and
its bypass set, the 29-way jumpg dispatch, fn 22B's stores, fn 24B's no-op.

Open: SWMSG field semantics per fn (needs the ND-100 N5SWAP sender carve,
`ND500-SWAPPER-ANALYSIS.md` section 12 direction); the MSW* names between
the five proven codes; sub-code 66B's meaning to the ND-100 side; fn 3/5
special-arm details; the no-op slots 13B/14B/24B expectations ND-100-side.
