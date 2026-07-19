# 046B-DEFSWAP

MON 60 subfunction **DEFSWAP = 46B = 0x26 = 38 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **DEFINE SWAP FILE**, server handler `IDEFSWAP`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Note: `046B` is a *gap* (`--`) in the NDInsight 60B yaml; identity DEFINE SWAP
FILE / `IDEFSWAP` is from `SUBFUNCTION-TABLE.md` (row 046). Folder named by the
handler `IDEFSWAP` -> `DEFSWAP`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146500` | `170446` | `SAA 46` (subfunction 46B) |
| `146501` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146502` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **007422** `JPL I 165` -> ptr `007607`, `bank1[007607]=146500` = thunk `SAA 46` | PROVEN |
| Error path (callsite+1) | 007423 `JPL I 163` -> routine **002673** (internal error reporter) | PROVEN |
| Success path (callsite+2) | 007424 `JMP I -26` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` (+7,+10) | `007421 STF ,X 6` | file-name descriptor from F-image `B-113` (F = 3 words) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Loads the file-name descriptor (F, 3 words) from `B-113`. (`007417`)
2. Places it in gateway slots 6/7/10 and issues `MON 60` DEFSWAP. (`007422`)
3. Error -> `002673`; success -> command loop `010613`.

## Unknown / inferred

- **INFERRED (role)**: `002673` error reporter, `010613` command loop. Entry addresses and reach are PROVEN.
- **PROVEN**: single parameter is a 3-word file-name descriptor (`STF`).
- **INFERRED**: DEFINE-SWAP-FILE purpose/handler string is NPL logic (`SUBFUNCTION-TABLE.md`); the thunk `SAA 46` is PROVEN.

octal=hex=decimal: 46B = 0x26 = 38 decimal
