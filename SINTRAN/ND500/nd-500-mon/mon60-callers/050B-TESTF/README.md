# 050B-TESTF

MON 60 subfunction **TESTF = 50B = 0x28 = 40 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **(test function)**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `TESTF`. Handler `5NOPAR` = common forward path (not a
no-op; see `SUBFUNCTION-TABLE.md` note 1). Two duplicate thunks exist for 50B
(`146506` unused, `146511` used); both call sites resolve to `146511`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146511` | `170450` | `SAA 50` (subfunction 50B) |
| `146512` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146513` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **007740** `JPL I 106` -> ptr `010046`, `bank1[010046]=146511` = thunk `SAA 50` | PROVEN |
| Error path (callsite+1) | 007741 `JPL I 102` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 007742 `JMP 6` -> 007750 `JMP I 101` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **007762** `JPL I 64` -> ptr `010046`, `bank1[010046]=146511` = thunk `SAA 50` | PROVEN |
| Error path (callsite+1) | 007763 `JPL I 60` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 007764 `JMP I 65` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` @007734 / @007756 | `STA ,X 6` | parameter 1 (site1: derived value; site2: `&(B-127)`) |
| `,X 7` @007737 / @007761 | `STA ,X 7` | parameter 2 = `&(B-127)` in both sites |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Two distinct call sites, both resolving to thunk `146511` (`SAA 50`), each setting two parameters into slots 6 and 7.
2. Site 1 (`007725-007742`) and site 2 (`007751-007764`) differ in how the two operands are computed; both pass a pointer into the local frame at `B-127`.
3. On error -> `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **INFERRED (role)**: `007500` is an internal error/abort helper (`RADD CLD SL DX` leaf calling LEAVE `177327`); `010613` command loop. Addresses/reach PROVEN.
- **UNKNOWN**: the exact semantics of TESTF's two parameters. The stores into `,X 6`/`,X 7` are PROVEN; their meaning (`(test function)` per NPL) is not documented further.
- **PROVEN**: both call sites hit the same thunk `146511`; the second, unused 50B thunk `146506` has no caller.

octal=hex=decimal: 50B = 0x28 = 40 decimal
