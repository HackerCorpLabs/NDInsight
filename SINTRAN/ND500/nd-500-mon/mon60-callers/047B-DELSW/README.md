# 047B-DELSW

MON 60 subfunction **DELSW = 47B = 0x27 = 39 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **DELETE SWAP FILE**, server handler `IDELSWAP`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `DELSW`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146503` | `170447` | `SAA 47` (subfunction 47B) |
| `146504` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146505` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **007430** `JPL I 160` -> ptr `007610`, `bank1[007610]=146503` = thunk `SAA 47` | PROVEN |
| Error path (callsite+1) | 007431 `JPL I 155` -> routine **002673** (internal error reporter) | PROVEN |
| Success path (callsite+2) | 007432 `JMP I -34` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` (+7,+10) | `007427 STF ,X 6` | file-name descriptor from F-image `B-113` (F = 3 words) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Loads the file-name descriptor (F, 3 words) from `B-113`. (`007425`)
2. Places it in gateway slots 6/7/10 and issues `MON 60` DELSW. (`007430`)
3. Error -> `002673`; success -> command loop `010613`.

## Unknown / inferred

- **INFERRED (role)**: `002673` error reporter, `010613` command loop (PROVEN addresses/reach).
- **PROVEN**: single parameter is a 3-word file-name descriptor.
- Sits immediately after the 046B DEFSWAP case (same interpreter); the two swap-file operations form a define/delete pair.

octal=hex=decimal: 47B = 0x27 = 39 decimal
