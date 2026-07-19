# 001B-WRREG

MON 60 subfunction **WRREG = 1B** (octal) = **0x01** = **1** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(write a register)**.
Server handler: **5NOPAR** (generic forward path).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146313 | 170401 | `SAA 1` -> subfunction code 1 |
| 146314 | 125001 | `JMP I 1` |
| 146315 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 052642 | ENTER-routine **052605** (framesize 000005), standalone | `JPL I 17` -> 052661 | `bank1[052661]=146313` | `SAA 1` = WRREG | PROVEN |

Standalone ENTER routine (prologue 052605 `RADD AD1 CLD SL DX` / `JPL I 45`->177300 /
inline `000005`); not inside the 002662 interpreter.

## Parameter block (stored into X := b.-176 before the call)

| Slot | Set at | Value | Meaning |
|---|---|---|---|
| `,X 6` (param1) | 052636 | `&(B-167)` (B-167 = `LDA ,B -172` >> 020, at 052630-052632) | pointer to register number (INFERRED) |
| `,X 7` (param2) | 052641 | `&(B-171)` | pointer to value to write (INFERRED) |

## Skip / error handling

- 052642 `JPL I 17` = MON 60 WRREG call.
- 052643 (callsite+1) = **ERROR** -> `JPL I 14` -> pointer 052657 = `177327` = LEAVE(value):
  the error status is returned up to this routine's own caller.
- 052644 (callsite+2) = **SUCCESS** -> `LDD ,B -167`.

## Unknown / inferred

- **INFERRED**: `,X 6` = register number, `,X 7` = value. Store addresses PROVEN;
  labels rest on the WRREG name and the `SAD SHR 20` extraction of B-167 from B-172
  (a high-half unpack consistent with a packed <regno,value> operand, not decoded).
- **PROVEN**: thunk bytes, pointer 052661->146313, call site 052642, stores at
  052636/052641, callsite+1 error path to 052657 (LEAVE-value) and callsite+2 success.
