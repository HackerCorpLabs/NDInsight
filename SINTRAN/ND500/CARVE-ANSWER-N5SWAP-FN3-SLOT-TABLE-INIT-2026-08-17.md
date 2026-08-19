# CARVE ANSWER - N5SWAP fn 3 (slot-table init) and the WIP/PGU page ops (2026-08-17)

Milestone 12 of the CpuND500UC microword engine: the zpgu/zwip page-op
instruction family carved from CONT-STORE-10611 and implemented, and fn 3
driven END TO END through the real swapper - the fn-3 worker now COMPUTES
the slot-table gate cell the milestone-11 harness had to poke.

Evidence tags: **PROVEN** = read from the cited bytes/listing AND (where
stated) executed on the microword engine with the real CONT-STORE-10611
microcode. **INFERRED** = deduced, not fact.

Sources:
- Microcode listing: `E:\Dev\Repos\Ronny\ND110Compile\ND110Compile\uCode\CONT-STORE-10611.LISTING.TXT`
- Swapper disassembly: `SINTRAN/ND500/swapper/swapper-k01-pseg.asm` (+ raw `.PSEG`/`.DSEG`)
- Manuals: ND-05.009.4 §16.16-16.22 (the RPGU/RWIP/ZPGU/ZWIP/CPGU/CWIP pages),
  ND-30.013.02 §3.17.8 (IMSTS/DMSTS bits, the WIP/PGU broadside, IMCNTR bit 9,
  IPROCC bit 7 SWIP)
- Protocol context: `CARVE-ANSWER-N5SWAP-FUNCTION-VOCABULARY-2026-08-17.md`
  (milestone 11) and `N5SWAP-SWMSG-FIELD-DOSSIER-RELAY-2026-08-17.md`
  (fn 3 = MSWMI, L07:5611)
- Execution proof: `PageOpFamilyTests`, `Fn3ArithmeticOracleTests` and the
  reworked `SwapperN5Swap_Fn22B_SetConfig_ThroughJumpgTable` in
  `RetroCore/Nuget/HackerCorpLabs.Emulation.CPU.ND500UC/tests/`,
  175/175 green 2026-08-17.

---

## 1. The page-op family in the microcode (PROVEN, executed)

The eight entries 000353B-000362B (between the monitor ops and RIOM, all
privilege-gated `ALU,AND A,XD,S1 B,BM#1 COND,MZRO ...` -> the 005227B
S2-bit-1 trap when S1 bit 1 is clear) are the WIP/PGU family. The bodies
pair as PGU/WIP twins differing ONLY in the SARG literal 0 vs 200B - which
is exactly the PROCC bit-7 SWIP broadside table selector of ND-30.013.02
§3.17.8.8 ("If 1, WIP is selected, and if 0, PGU"):

| entry | opcode | instruction | body | proof of identity |
|---|---|---|---|---|
| 000353B | 177210B+ | BIn RPGU | 010572B | ORD delivery of STS bit 3 (PUS) via the 010604B `AND BM#3` arm |
| 000354B | 177214B+ | Hn RPGU | 010614B | broadside strobes + 010623B `IWIPGU OR AM#22 -> ORD` ("logical OR of the two tables", §16.20) |
| 000355B | 177220B | BI ZPGU | 010631B | `ANDCB B,BMR` RMW on BOTH channels' buffers (010642B/010643B; "clears the bit in both tables", §16.21) |
| 000356B | 177432B | CPGU | 010644B | operand-less 1024-group zero-fill loop 010651B-010656B |
| 000357B | 177224B+ | BIn RWIP | 010574B | lit-200B twin of 353B (BM#4 = STS WIP bit arm 010603B) |
| 000360B | 177230B+ | Hn RWIP | 010616B | lit-200B twin of 354B |
| 000361B | 177234B | BI ZWIP | 010633B | lit-200B twin of 355B |
| 000362B | 177433B | CWIP | 010646B | lit-200B twin of 356B |

Mechanics (all executed): the operand is a /W VALUE (the manual's own
format lines); `EX,SHL` by 13B (bit forms) or 17B (group forms) turns it
into the physical BYTE address; sub 010562B presents it (DP := address,
MMOD bit 5, `AD,ILC PASSAA`); the group is "addressed by means of the 10
most significant bits of the real address" (§3.17.8.4 - halfword address
>> 14); IMCNTR/DMCNTR bit 9 starts the broadside read; the BIT reads come
from IMSTS/DMSTS bits 3 (PUS) / 4 (WIP) of the presented page and deliver
0 or 0+carry=1 through the conditional-ALU word 010605B. The CPGU/CWIP
sweep's start cursor -100000B WRAPS (32-bit) so the first presentation is
group 0, and the loop stops when the byte cursor reaches bit 25 = 32 MB =
the 16 Kbit table, byte-consistent with §3.17.8.4.

## 2. What fn 3's worker actually executes (PROVEN)

Worker 1000060473 (stub 1000101642; the gate-BYPASS arm 1000101515 routes
fn 3 to the jumpg like every other function, skipping only the gate):

- **SWMSG layout** (RIOM count 138 halfwords - the 0x0802403C table):
  hw 8 = INLINE ENTRY COUNT; the entries sit IN the message at hw 10+,
  stride 4 bytes: `{h0 = start page, h1 = page count}`. The access pair is
  `w3 laddr r.24` (address intake+20) + `by rladdr @b.60+` (local
  indirect, post-indexed by the entry cursor x4). There is NO pointer in
  the message (the earlier "pointer at hw 10" reading was wrong).
- hw 8 >= 77B triggers the 1000003057 MON-request arm; below that the flag
  short-circuits it (executed: staged count 1).
- **Argument pre-staging through the frame's SP cell**: `r := b.10` reads
  the CALLER frame's [B+10B] = SP = the CALLEE's future frame base; stores
  to `r.24` land in the next frame's argument slot 0. (This is how every
  `call ...,$0` in the swapper passes arguments - PROVEN by execution.)
- **Sub 1000034221**: receives the ADDRESS of the swap-list head cell
  [0x080128F8] (descriptor 317B :W CONSTANT - the same constant-descriptor
  family as the milestone-11 `r := $1000440274` finding). The cell holds a
  HALFWORD ELEMENT ID; 0 = empty list -> immediate return (executed).
  A nonzero id walks Table C (segment 6, stride 10B) - deferred.
- **Per-page release loop** (`comp2 b.40,b.44` gate - skipped when the
  entry's page count is 0, executed): getbf state check (-> optional
  1000003057), **`bi zpgu` / `bi zwip` on the physical page number**
  (1000061017/1000061022), `w clebi r.4,$34` on the Table-C entry, and the
  leaf 1000023641 (getbf/putbf/udiv bookkeeping - deferred).
- **The bookkeeping tail** (EXECUTED end to end):
  - `add2 [0x080128EC], b.44` - the released-page accumulator,
  - watermark block: [0x080128E0] := start page - 1 (conditional on the
    [0x080128DC]/[0x080128F4] compares),
  - **1000061202: `[0x080128E4] := [0x080128EC] + [0x080128E8]` - THE
    SLOT-TABLE GATE CELL**, computed, not poked,
  - `[0x0801284C] := [0x080128E4] / 2` (`w2 / $2` - the integer divide),
    clamped to [0x080128EC] by the following comp2/move,
  - entry-count loop tail, `ret` (K clear), common exit, next announce.

Executed proof (the reworked fn-22B test): fn 3 with one 0-page entry
(start page 20B) + [0x080128EC] staged 100B leaves gate = 0x40,
[0x0801284C] = 0x20, [0x080128E0] = 0xF, fn echo 3, counter 1; fn 22B then
PASSES the gate fn 3 computed and lands its three stores; fn 24B no-op;
clean park at the announce (STOPR = 1).

## 3. The instructions milestone 12 added to the engine (PROVEN, oracle-verified)

| instruction | entry | notes |
|---|---|---|
| RPGU/RWIP/ZPGU/ZWIP/CPGU/CWIP | 000353B-000362B | section 1; differential oracle vs the functional CpuND500 (its single-table SetPGUBit/SetWIPBit model seeded on both engine channels) 7/7 match |
| Wn / | 000063B/64B/65B (BY/H/W by DZ-clamp width) | the DOUBLE pipeline: EX,CTF DOUBLE pairs into AD#20/AD#21, ARMULA over the 64-bit register pairs, pair delivery + microcode rounding (001421B-001423B), EX,CTI DOUBLE truncate, 001104B collect. Oracle 6/6; the INT_MIN/-1 golden adjudicated WRONG-GOLDEN on O (the collect is a logic-ALU ST,SAVA - O resets; the value wraps to 0x80000000 on both) |
| W ADD2 | 000071B (ORT-shared BY/H/W) | continuation 001150B, DestFirstOperand RMW. Carry edge probe-pinned: the microword saves the UNSIGNED bit-31 carry; the functional Add2.cs widens SIGNED and misses it (functional discrepancy, not edited) |
| BY/H/W CLEBI (memory forms 177175B-177B) | 000211B-213B | continuation 001503B; the four-triple block 000203B-000216B resolved GETBI=1461B / SETBI=1475B / CLEBI=1503B / PUTBI=1467B |

Engine plumbing added: the per-channel PUWP tables + broadside port
(A,XD,IWIPGU 633B / DWIPGU 653B; D,IWIPGU 1436B / DWIPGU 1456B; MCNTR
bit 9), hardware PGU/WIP marking on every memory access, the ND double
(54-bit mantissa) pair codec, DOUBLE XRES most/least delivery into D,AD#n,
and the PRF,CLEAR consumption-boundary P resync (TICK-MODEL items 75-79).

## 4. What stays deferred (HONEST OPEN - the first two items CLOSED by section 5, 2026-08-17 milestone 13)

- **GETBF/PUTBF** (entries 000217B-000224B, continuations 001514B/001524B):
  need the SHC shift-count register + SHCFR microorders (unmodeled).
- **UDIV / the 3-operand divide flavors** (the 000120B/000122B chains) and
  the leaf 1000023641's Table-C/timestamp bookkeeping.
- Because of these, the PER-PAGE release path of fn 3 (zpgu/zwip on real
  pages inside the swapper, clebi on Table C, the [0x080128EC] accumulator
  feeding) is not yet driven in-swapper - the harness stages [0x080128EC]
  (TICK-MODEL item 79) and drives a 0-page entry. The zpgu/zwip
  instructions THEMSELVES are fully executed and pinned by the family
  tests.
- CPGU/CWIP exit anomaly: no PCONT and no mid-body PRF,CLEAR on their
  path, so how the real 5018 advances P past them is OPEN (TICK-MODEL
  item 78). fn 5 uses cwip - revisit before driving fn 5.
- fn 5, the RPHS family and the remaining SWMSG field semantics: unchanged
  from milestone 11 / the SWMSG dossier.

## 5. ADDENDUM 2026-08-17 (milestone 13): the per-page release loop EXECUTED

The section-4 deferral is closed: GETBF/PUTBF/UDIV and the SHC
shift-count machinery are real in the engine, and fn 3 now runs with a
REAL page-releasing entry (64 pages, start page 20B) - no [0x128EC]
staging. What the release loop actually does, now EXECUTED (PROVEN unless
marked):

- **Loop head** (1000060735-1000060777, per page b.30, counter b.40 vs
  page count b.44): `w move $31,b.60` (the MON sub-function selector 31B,
  armed but unused on the state-0 path), `w1 := b.30 * 10B`, `w stz
  b.74`, `by rladdr $6000000000+` -> **R := 0x30000000 + page*8 = the
  TABLE-C entry** (data segment 6, stride 8 - "stride 10B" in the old
  carve text was the OCTAL 10 = 8 bytes), `w2 getbf r.4,$26,$3` = the
  STATE field (bits 22-24 of entry word 1); state 0 -> b.74 := 1 -> the
  1000003057 MON-request call is SKIPPED (`w test b.70; if >< go`).
- **Per page**: `bi zpgu` + `bi zwip` on the page number (the real
  privileged page-ops - PUWP bits cleared on both channels), `w clebi
  r.4,$34` (clear bit 34B = 28 of the Table-C entry word 1), then
  **leaf 1000023641** (`call ...,$0` with W1 = the page).
- **The leaf** (ents $50 frame): reads the entry's state fields twice
  (`getbf r.4,$31,$3` - bits 25-27), indexes the 8-entry per-state
  descriptor table at [0x0801296C] (`w4 *= $14` - stride 14B = 12
  bytes; +8 = the per-state slot-table BASE - INFERRED name), then
  `w1 udiv r2,r1,r2` with divisor [0x08012A0C] (the DSEG file ships
  0x10000, so quotient = 0 and remainder = the page for any page <
  65536), cursor := base + 16B*quotient (16B = 14 bytes), copies the
  slot record's halfword [cursor+4] into Table-C entry word 0, writes
  the page back to [cursor+4], `bi2 clr` + `w2 putbf r4.(4),$35,$3`
  (bits 29-31 := 0), `w1 := $4` + `w1 putbf r4.(4),$26,$3` (**state :=
  4**), zeroes entry halfwords 2/6, `w clebi` bits 34B and 24B, `w stz
  [0x080128F0]`, `w incr [0x08026288]` (a second counter), ret.
- **Tail** (unchanged from section 2, now with real numbers): [0x128EC]
  += page count (the add2 - the accumulator is FED BY THE MESSAGE),
  gate [0x128E4] := [0x128EC] + [0x128E8] = 100B, [0x1284C] := gate/2,
  and the watermark [0x128E0] := **the LAST RELEASED PAGE** (b.30 - 1
  after the loop = 117B for pages 20B-117B; the milestone-12 "start
  page - 1" reading was the count-0 degenerate case where b.30 never
  advanced).

Engine work this took (package TICK-MODEL milestone-13 section):
the SHC register + D,SHC (1520B) / A,XD,SHC (723B) / SHCFR (bit 131)
microorders, GETBF/PUTBF (000217B-000224B), UDIV at entry **000134B** -
the milestone-12 "UDIV = 000120B/000122B" note was WRONG: those chains
are DIV3 (BY/H/W/F/D, signed, no remainder); UDIV's unsigned EX,UCTF
DOUBLE converts and the remainder tail (001417B EX,MUL + 001420B
subtract -> Rn) live at 000134B - and DIV3 BY/H/W (176166B-176170B ->
000120B-000122B).

Remaining staging (TICK-MODEL item 82, replacing the retired item 79):
Table C mapped as a writable ZERO page (the shared segment-6 mapping is
faithful; the zero CONTENT is the staged part), and the leaf's state-0
slot-table base [0x08012974] := a zero DSEG scratch area (the SINTRAN
init that populates the [0x0801296C] state table is uncarved).

## 6. Corrections to earlier readings

- Milestone-11 doc section 4: "Driving fn 3 for real needs the zpgu/zwip
  page instructions" - now DONE for the instructions; the remaining gap is
  GETBF/PUTBF/UDIV (section 4 above).
- The fn-3 SWMSG carries its page entries INLINE (hw 10+); the message has
  no pointer word (this doc, section 2).
- The swap-list head [0x080128F8] holds an ELEMENT ID (halfword), not a
  pointer; the worker passes the CELL ADDRESS as a :W constant.
