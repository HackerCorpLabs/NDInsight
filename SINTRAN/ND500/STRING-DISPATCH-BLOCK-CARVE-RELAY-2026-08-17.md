# STRING dispatch block carve (entries 660-730) — relayed 2026-08-17

Provenance: compiled by a carve sub-agent during ND500UC milestone 14 from
`E:\Dev\Repos\Ronny\ND110Compile\ND110Compile\uCode\CONT-STORE-10611.LISTING.TXT`
cross-anchored with the functional CPU's opcode table
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND500\Instructionset.Init.cs`.
Grades (PROVEN/INFERRED/UNKNOWN) are the compiler's own. Not yet
execution-verified; milestone 14+ adjudicates.

KEY ORDERING FACT: entries 665-714 follow opcode order exactly:
smove(FD66-6B, 6 variants -> 5 entries, W/F share) -> smvwh(FD72)
smvun(FD73) smvtr(FD74) smvtu(FD75) -> smovn(FD76-7B, 6->5) ->
sfill(FD7C-93) -> sfilln(FD94-AB). Anchored at both ends by proven
semantics (smove known, sfill proven below).

## SFILL — PROVEN

Entries 000703-000707, bodies 004121/004130/004137/004146/004155 =
BI/BY/H/W(+F)/D.

Evidence: each body reads the REGISTER field via ORA into AM#21 (004121
"ALU,AND B,BM#0 D,AM#21 ORA TYP,BY"; 004130 same untyped-ADIR BY; 004137
TYP,HW; 004146 untyped; 004155 D). The write loops have NO source read —
write data is always AM#21:
- BI: 005136/005137 read-modify-write of the bit (ANDCB/OR with BMR mask),
  stride AB,1/8IX
- BY: 005154 "ALU,ADIR A,AM#21 IX1 TYP,BY MEM,RD1 MEMWR=1 ... W,MEM AA+AB"
  (stride 1)
- H: 005173 "... TYP,HW ... W,MEM AA+AB AB,2IX"
- W: 005206 "ALU,ADIR A,AM#21 IX1 MEM,RD1 MEMWR=1 MEMNBY=3 W,MEM LCDECR
  AA+AB AB,4IX"
- D: 005211/005212 write AM#21 then AL#21, AB,DPARG +4 each = 8 bytes

Per the C# file, sfill = 0xFD7C-0xFD93 (24 opcodes = 6 types x 4 forms;
0xFD8B is one of them, Instructionset.Init.cs line 19123). If 0xFD8B is a
W-width form it dispatches to entry 000706 / body 004146 (widths PROVEN,
opcode-to-entry pairing INFERRED).

## BMOVE — structure PROVEN, pairing INFERRED

Entries 000661-000664, bodies 003024/003135/003215/003275 = BY/H/W(+F)/D.
The C# file says bmove = 0xFD20 (variant 0) + 0xFE78/79/7A/7B (variants
1-4), 3 operands (NOT 0xFE7C).

PROVEN structure: entry consumes EA1 ("ALU,BDIRC B,EA1 D,AL#32 ...
W,PMEM"), body consumes operand 2 via ORB (003024 "ALU,BDIR D,AM#21 ORB
TYP,BY ... W,PMEM") and operand 3 via EA2 (003025 "ALU,BDIRC B,EA2 D,AL#31
... W,PMEM"); no descriptor machinery. Count is scaled to bytes by element
size: EXFUNC=12 shift constants 1 (003145, H), 2 (003225, W), 3 (003306,
D), unscaled for BY. Overlap handled: 003034 end=base+count, 003035 overlap
test COND,MCRY, then backward pointers (003142/003143 addr-1 -> DP) or
forward (003151/003152 addr+count -> DP). F has no own entry — presumably
shares 000663 with W (INFERRED). Opcode pairing FD20->661, FE78->662,
FE79->663(+FE7A), FE7B->664 INFERRED from scaling constants.

## Entry 000660 — UNKNOWN identity

Body 001044 is PROVEN NOT bmove: it loads a SARG byte into CAR and
case-dispatches (JMPREL at 005242) 5 ways over word/8-byte/16-byte ALIGNED
copy engines (alignment masks 3/7/17 at 5256/5277/5317, shifts 2/3/4, copy
loops 5270/5311/5331 with strides 4/8/16) plus one case that traps to
000453. No byte/halfword case. The one unresolved entry in the block.

## Full table (entry -> body -> classification; INFERRED names from opcode order unless marked)

| Entry | Body | Classification |
|---|---|---|
| 660 | 1044 | aligned block transfer w/ sub-op byte, UNKNOWN identity |
| 661-664 | 3024/3135/3215/3275 | BMOVE BY/H/W(+F)/D [structure PROVEN] |
| 665-671 | — | SMOVE (known, milestone 11) |
| 672 | 3566 | masked byte copy (MV,DINTOD loop 3601) = smvwh FD72 |
| 673 | 3603 | twin of 3566 = smvun FD73 |
| 674 | 3620 | K-result string op = smvtr FD74 |
| 675 | 3655 | twin = smvtu FD75 |
| 676-702 | 3733/3767/4014/4043/4067 | two-descriptor copy min(len)+pad remainder (min via sub 3700, pad char FONE/SARG at 4765/4762); strides 1/8, 1, 2, 4, 8 PROVEN = smovn BI/BY/H/W(+F)/D |
| 703-707 | 4121/4130/4137/4146/4155 | SFILL [PROVEN] |
| 710-714 | 4207/4217/4227/4237/4247 | fill with register PLUS extra ORB operand into AM#30, same write loops as sfill = sfilln [fill loops PROVEN] |
| 715-730 | 4315/4337/4372/4435/4501/4517/4534/4544/4556/4572/4627/4674 | all two-descriptor prologue (2nd descriptor via sub 005101); 12 entries = exactly scomp(FDAC), scotr, sskip, sloca(FDAF), sloca(FDB0), sscan, sspan, smatch, sspar, schpar, scopa(FDBE), scopt(FDBF) [order-only INFERRED] |

## Secondary lookups (Instructionset.Init.cs)

- ced=: 0xFE54, 1 operand (Destination/Write/WordOnly), functionName CedGet
- tutti: 0xFE01, 0 operands
- loopi: 3 operands; 0x00BF, 0x00E1, 0xFCDE, 0xFCDF, 0xFD1C-0xFD1F,
  0xFD21, 0xFD22
- getbi: 2 operands; 0xFCB4-0xFCBB, 0xFDD0-0xFDD3
- hconv: 2 operands; 0xFD45, 0xFD4A, 0xFD55, 0xFD5A, 0xFD5F
- wconv: 2 operands; 0xFD46, 0xFD4B, 0xFD50, 0xFD5B, 0xFD60
