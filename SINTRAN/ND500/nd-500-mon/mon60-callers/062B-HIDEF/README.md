# 062B-HIDEF

MON 60 subfunction **HIDEF = 62B = 0x32 = 50 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **DEFINE HISTOGRAM**, server handler `IDEFHIST`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `HIDEF` (SET-HISTOGRAM command per INDEX.md 2.2).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146544` | `170462` | `SAA 62` (subfunction 62B) |
| `146545` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146546` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **040133** `JPL I 13` -> ptr `040146`, `bank1[040146]=146544` = thunk `SAA 62` | PROVEN |
| Error path (callsite+1) | 040134 `JPL I 6` -> routine **177327** = LEAVE-with-value (propagates error code) | PROVEN |
| Success path (callsite+2) | 040135 `JPL I 12` -> routine **177335** = LEAVE-SKIP (normal skip return) | PROVEN |
| Enclosing ENTER routine | 040050 (framesize 000010) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` | `040125 STA ,X 6` | parameter 1 = `&(B-172)` |
| `,X 7` | `040127 STA ,X 7` | parameter 2 = value loaded by `040126 LDA 15` |
| `,X 10` | `040132 STA ,X 10` | parameter 3 = `&(B-166)` |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Builds three parameters: `&(B-172)`, a constant/global from `LDA 15`, and `&(B-166)`. (`040122-040132`)
2. Issues `MON 60` HIDEF to define a histogram. (`040133`)
3. On error LEAVEs with the error code (`177327`); on success LEAVE-SKIP (`177335`).

## Unknown / inferred

- **PROVEN**: three parameters at `,X 6/7/10` (`040125`/`040127`/`040132`).
- **INFERRED (semantic)**: `B-172`/`B-166` are histogram-definition fields (range/buffer); `LDA 15` is a fixed operand - exact meaning not carved.
- **PROVEN (role)**: `177327` = LEAVE-with-value, `177335` = LEAVE-SKIP (compiler runtime).
- **INFERRED**: DEFINE HISTOGRAM / `IDEFHIST` from `SUBFUNCTION-TABLE.md`; thunk `SAA 62` PROVEN.

octal=hex=decimal: 62B = 0x32 = 50 decimal
