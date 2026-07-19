# 066B-HIREL

MON 60 subfunction **HIREL = 66B = 0x36 = 54 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **STOP AND RELEASE HISTOGRAM**, server handler `IRELHIST`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `HIREL` (RELEASE-HISTOGRAM command). Two call sites:
`010443` (interpreter histogram block) and `110130` (a logging/histogram
management routine).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146560` | `170466` | `SAA 66` (subfunction 66B) |
| `146561` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146562` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **010443** `JPL I 62` -> ptr `010525`, `bank1[010525]=146560` = thunk `SAA 66` | PROVEN |
| Error path (callsite+1) | 010444 `JPL I -166` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 010445 `JMP 146` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **110130** `JPL I 131` -> ptr `110261`, `bank1[110261]=146560` = thunk `SAA 66` | PROVEN |
| Error path (callsite+1) | 110131 `JPL -41` -> routine **110070** (a local error handler; direct P-relative JPL) | PROVEN (direct P-relative) |
| Success path (callsite+2) | 110132 (fall through) `LDA ,B -172` | PROVEN |
| Enclosing ENTER routine | 110055 (framesize 001137) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X` (site 1) | `-- (010443)` | no gateway-block store precedes the call (bare) |
| `,X` (site 2) | `-- (110130)` | no gateway-block store; `110127 STA I 131` writes A to a global via an indirect pointer (target UNKNOWN without pool resolution) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Stops and releases a histogram.
2. Site 1 (interpreter): bare call, no parameters; error -> `007500`, success -> command loop `010613`. (`010443`)
3. Site 2 (routine 110055): no gateway-block parameter is stored; the preceding `110127 STA I 131` writes A to a global through an indirect pointer. Error -> local handler `110070`, success -> fall through. (`110125-110130`)

## Unknown / inferred

- **PROVEN**: neither call site stores into `,X 6/7/10`.
- **UNKNOWN**: the global written by `110127 STA I 131` (the pointer at P+131 was not resolved to a name).
- **INFERRED (role)**: `007500` internal error/abort helper (site 1); `110070` a local error handler (site 2, PROVEN target).
- **INFERRED**: STOP AND RELEASE HISTOGRAM / `IRELHIST` from `SUBFUNCTION-TABLE.md`; thunk `SAA 66` PROVEN.

octal=hex=decimal: 66B = 0x36 = 54 decimal
