# 057B-MPVER

MON 60 subfunction **MPVER = 57B = 0x2F = 47 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ MICRO PROGRAM VERSION**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `MPVER`. **Cross-reference:** the STATUS command
(`../STATUS/`) also reads MPVER; INDEX.md lists both `005577` and `132132`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146536` | `170457` | `SAA 57` (subfunction 57B) |
| `146537` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146540` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **005577** `JPL I 112` -> ptr `005711`, `bank1[005711]=146536` = thunk `SAA 57` | PROVEN |
| Error path (callsite+1) | 005600 `JPL I -132` -> routine **002673** (internal error reporter) | PROVEN |
| Success path (callsite+2) | 005601 `JMP 22` -> 005623 (next step) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **132132** `JPL I 22` -> ptr `132154`, `bank1[132154]=146536` = thunk `SAA 57` | PROVEN |
| Error path (callsite+1) | 132133 `JPL I 22` -> routine **177327** = LEAVE-with-value (propagates the MON 60 error code A back to this routine's caller) | PROVEN |
| Success path (callsite+2) | 132134 (fall through) `SAA 26` | PROVEN |
| Enclosing ENTER routine | 132124 (framesize 000000) - a small standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` @005576 | `STA ,X 6` | site 1: parameter 1 = `&(B-127)` (buffer to receive the version) |
| `,X 6` @132131 | `STA ,X 6` | site 2: parameter 1 = value loaded by `132127 LDA 24` |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Reads the ND-500 micro-program version.
2. Site 1 (interpreter): passes `&(B-127)` as the receive buffer. (`005573-005577`)
3. Site 2 (small routine 132124, framesize 0): passes the value from `LDA 24` and, on error, LEAVEs with the error code (propagates upward). (`132127-132132`)

## Unknown / inferred

- **PROVEN**: site 1 param = `&(B-127)`; site 2 param = value from `132127 LDA 24`.
- **INFERRED (semantic)**: `B-127` (site 1) receives the version word; the returned layout was not carved.
- **INFERRED (role)**: `002673` error reporter (site 1); `177327` = the runtime LEAVE-with-value helper (site 2 error return) - this is PROVEN as the compiler LEAVE routine (prog.md sec 4.3).
- **INFERRED**: READ MICRO PROGRAM VERSION purpose from `SUBFUNCTION-TABLE.md`; thunk `SAA 57` PROVEN. See `../STATUS/`.

octal=hex=decimal: 57B = 0x2F = 47 decimal
