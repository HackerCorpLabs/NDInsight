# 053B-T500P

MON 60 subfunction **T500P = 53B = 0x2B = 43 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **(take ND-500 pages)**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `T500P`. **Cross-reference:** call site `010245` is also
documented in `../START-SWAPPER/` (paging block `010200-010260`). This folder
carves the T500P subfunction itself.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146522` | `170453` | `SAA 53` (subfunction 53B) |
| `146523` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146524` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **010245** `JPL I 43` -> ptr `010310`, `bank1[010310]=146522` = thunk `SAA 53` | PROVEN |
| Error path (callsite+1) | 010246 `JPL I 10` -> ptr 010256 = routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 010247 `JMP I -176` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` | `010244 STA ,X 6` | parameter 1 = `&(B+105)` = address of the page-count value evaluated by helper 002003 |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Evaluates one command-line numeric operand via helper `002003` -> a 32-bit value in D, stored to `B+105`. (`010235-010240`)
2. Passes `&(B+105)` as param 1 and issues `MON 60` T500P (take pages from the ND-500). (`010245`)
3. On error -> `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **INFERRED**: `002003` numeric-arg evaluator (role PROVEN by call pattern).
- **PROVEN**: param 1 = `&(B+105)` (`010244`).
- **INFERRED (semantic)**: `B+105` = page count (per NPL `(take ND-500 pages)`).
- Mirror of 052B G500P; the two form the give/take-pages pair. See `../START-SWAPPER/`.

octal=hex=decimal: 53B = 0x2B = 43 decimal
