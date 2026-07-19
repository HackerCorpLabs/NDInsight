# 013B-CNCFI

MON 60 subfunction **CNCFI = 13B** (octal) = **0x0B** = **11** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **CONNECT FILE**.
Server handler: **ICONNFI**.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146351 | 170413 | `SAA 13` -> subfunction code 013 (11 dec) |
| 146352 | 125001 | `JMP I 1` |
| 146353 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 036440 | ENTER-routine **036374** (framesize 000022), standalone | `JPL I 34` -> 036474 | `bank1[036474]=146351` | `SAA 13` | PROVEN |

Standalone ENTER routine (prologue 036374 `RADD AD1 CLD SL DX` / `JPL I 70`->177300 /
inline `000022`); not inside the 002662 interpreter.

## Parameter block (X := b.-176; five slots)

| Slot | Set at | Value |
|---|---|---|
| `,X 6` (param1) | 036424 | `local(B-162)` (value) |
| `,X 7` (param2) | 036427 | `&(B-152)` |
| `,X 10` (param3) | 036431 | `local(B-157)` (value) |
| `,X 11` (param4) | 036434 | `&(B-167)` |
| `,X 12` (param5) | 036437 | `&(B-154)` |

## Skip / error handling

- 036440 `JPL I 34` = MON 60 CNCFI call.
- 036441 (callsite+1) = **ERROR** -> `JPL I 27` -> pointer 036470 = `177327` LEAVE(value).
- 036442 (callsite+2) = **SUCCESS** -> `LDD ,B -167`.

## Cross-reference

CNCFI (13B, OPEN/connect a file to an ND-500 process) pairs with `014B-CLSFI` (close
file); INDEX.md maps the OPEN-FILE operator command to this call site (036440).

## Unknown / inferred

- **INFERRED**: individual field meaning of the five slots (file/connect number, access
  mode, name descriptor, etc.). All store addresses/operands PROVEN; the mapping to an
  ICONNFI signature was not traced.
- **PROVEN**: thunk bytes, pointer 036474->146351, call site 036440, all five stores,
  and callsite+1 (LEAVE-value error) / callsite+2 (success).
