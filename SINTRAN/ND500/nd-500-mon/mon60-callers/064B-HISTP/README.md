# 064B-HISTP

MON 60 subfunction **HISTP = 64B = 0x34 = 52 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **STOP HISTOGRAM**, server handler `ISTOHIAT`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `HISTP` (STOP-HISTOGRAM command). Sits in the same
interpreter histogram block as HISTA (`063B` @010440) and HIREL (`066B` @010443).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146552` | `170464` | `SAA 64` (subfunction 64B) |
| `146553` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146554` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **010435** `JPL I 66` -> ptr `010523`, `bank1[010523]=146552` = thunk `SAA 64` | PROVEN |
| Error path (callsite+1) | 010436 `JPL I -160` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 010437 `JMP 154` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| (none) | -- | no `,X` parameter store immediately precedes this call site |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Issues `MON 60` HISTP to stop the running histogram. (`010435`)
2. No per-call parameter is written into the gateway block immediately before the call (PROVEN by inspection).
3. On error -> `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: no `,X 6/7/10` store precedes `010435`.
- **INFERRED (role)**: `007500` internal error/abort helper; `010613` command loop.
- **INFERRED**: STOP HISTOGRAM / `ISTOHIAT` from `SUBFUNCTION-TABLE.md`; thunk `SAA 64` PROVEN.
- **NOTE**: `SUBFUNCTION-TABLE.md` spells the handler `ISTOHIAT` (verbatim from the NPL source).

octal=hex=decimal: 64B = 0x34 = 52 decimal
