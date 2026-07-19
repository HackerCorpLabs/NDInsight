# 000B-RRREG

MON 60 subfunction **RRREG = 0B** (octal) = **0x00** = **0** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, per
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**READ A REGISTER**. Server handler: **5NOPAR** (generic forward path).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146310 | 170400 | `SAA 0`  -> subfunction code 0 |
| 146311 | 125001 | `JMP I 1` |
| 146312 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 013143 | ENTER-routine **013100** (framesize 000013), standalone | `JPL I 30` -> 013173 | `bank1[013173]=146310` | `SAA 0` = RRREG | PROVEN |

This is **not** inside the 002662 command interpreter; it is its own ENTER routine
whose prologue is at 013100 (`RADD AD1 CLD SL DX` / `JPL I 61`->177300 / inline
`000013`).

## Parameter block (stored into X := b.-176 before the call)

Verified stores (X = stack top = base of the gateway frame; `,X 6`/`,X 7` = MON 60
parameter 1/2):

| Slot | Set at | Value | Meaning |
|---|---|---|---|
| `,X 6` (param1) | 013137 | `&(B-163)` (B-163 loaded from const `LDD 37` @013132/013133) | pointer to register selector (INFERRED) |
| `,X 7` (param2) | 013142 | `&(B-161)` | pointer to returned-value buffer (INFERRED) |

## Skip / error handling

- 013143 `JPL I 30` = the MON 60 RRREG call.
- 013144 (callsite+1) = **ERROR** -> `JPL I 22` -> pointer at 013166 (internal error path).
- 013145 (callsite+2) = **SUCCESS** -> `LDA ,B -164`, code consumes the returned data.

## Unknown / inferred

- **INFERRED**: `,X 6` = pointer to the register number/selector and `,X 7` = pointer
  to the result buffer. The store addresses (013137/013142) and their operands
  (`&B-163`, `&B-161`) are PROVEN; the *semantic* labels rest on the RRREG name
  ("READ A REGISTER") and the fact that 013145 onward reads returned data.
- **INFERRED**: the 32-bit constant loaded by `LDD 37` (from pool word ~013171) is the
  register selector. Value not decoded here.
- **UNKNOWN**: the exact routine reached by the error pointer at 013166 (internal
  error path) was not carved.
- **PROVEN**: thunk bytes, pointer resolution 013173->146310, call site 013143,
  parameter stores at 013137/013142, and callsite+1/+2 targets.
