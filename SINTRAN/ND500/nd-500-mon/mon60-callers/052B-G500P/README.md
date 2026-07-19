# 052B-G500P

MON 60 subfunction **G500P = 52B = 0x2A = 42 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **(give ND-500 pages)**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `G500P`. **Cross-reference:** this same call site
(`010232`) is documented in `../START-SWAPPER/` as part of the swapper/paging
block `010200-010260` (SWLOD/STSWP/G500P/T500P). This folder carves the G500P
subfunction itself.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146517` | `170452` | `SAA 52` (subfunction 52B) |
| `146520` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146521` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **010232** `JPL I 55` -> ptr `010307`, `bank1[010307]=146517` = thunk `SAA 52` | PROVEN |
| Error path (callsite+1) | 010233 `JPL I -170` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 010234 `JMP I -163` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` | `010231 STA ,X 6` | parameter 1 = `&(B+105)` = address of the page-count value evaluated by helper 002003 |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Evaluates one command-line numeric operand via helper `002003` (`SAA 0` selector) -> a 32-bit value in D, stored to `B+105`. (`010222-010225`)
2. Passes `&(B+105)` as param 1 and issues `MON 60` G500P (give pages to the ND-500). (`010232`)
3. On error -> `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **INFERRED**: `002003` is the command-line numeric-argument evaluator (same helper used across the interpreter); role PROVEN by call pattern, internal parse not traced.
- **PROVEN**: param 1 = `&(B+105)` (`010231`).
- **INFERRED (semantic)**: `B+105` holds a page count (per NPL `(give ND-500 pages)`).
- See `../START-SWAPPER/` for the surrounding paging sequence.

octal=hex=decimal: 52B = 0x2A = 42 decimal
