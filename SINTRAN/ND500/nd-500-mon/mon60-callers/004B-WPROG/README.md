# 004B-WPROG

MON 60 subfunction **WPROG = 4B** (octal) = **0x04** = **4** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **LOGICAL PROGRAM MEMORY WRITE**.
Server handler: **IPMWRITE** (special input-marshalling handler, not `5NOPAR`).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146321 | 170404 | `SAA 4` -> subfunction code 4 |
| 146322 | 125001 | `JMP I 1` |
| 146323 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 055702 | ENTER-routine **055255** (framesize 000302), standalone | `JPL I 124` -> 056026 | `bank1[056026]=146321` | `SAA 4` | PROVEN |

## Parameter block (X := b.-176; `,X 6/7/10`)

| Slot | Set at | Value | Meaning |
|---|---|---|---|
| `,X 6` (param1) | 055661 | `&(B-170)` | logical program-memory address (INFERRED) |
| `,X 7` (param2) | 055664 | `&(B-166)` | count (INFERRED) |
| `,X 10` (param3) | 055701 | F register, 3-word (`LDF ,B -155`) | source data (INFERRED) |

Note the same three-slot `&(B-170)/&(B-166)/F(B-155)` layout is used by 005B-WDATA at
056023 in this same routine 055255 - a matched WPROG/WDATA pair.

## Skip / error handling

- 055702 `JPL I 124` = MON 60 WPROG call.
- 055703 (callsite+1) = **ERROR** -> `JPL I 124` -> pointer 056027 = `177327` LEAVE(value):
  error status returned up to this routine's caller.
- 055704/055705 (callsite+2) = **SUCCESS** -> `STZ I 124` / `JMP 120` -> 056025.

## Unknown / inferred

- **INFERRED**: slot semantics (address / count / data). The parallel WDATA site in the
  same routine uses the identical layout, which supports (but does not prove) the
  address/count/data reading. Store addresses/operands PROVEN.
- **PROVEN**: thunk bytes, pointer 056026->146321, call site 055702, all three stores,
  callsite+1 = LEAVE-value error, callsite+2 = success.
