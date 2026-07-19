# 020B-TIMUS

MON 60 subfunction **TIMUS = 20B** (octal) = **0x10** = **16** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(time used)**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146370 | 170420 | `SAA 20` -> subfunction code 020 (16 dec) |
| 146371 | 125001 | `JMP I 1` |
| 146372 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 005162 | **CASE 005162-005164** inside command interpreter ENTER-routine **002662** (framesize 000331, spans 002662-010634) | `JPL I 60` -> 005242 | `bank1[005242]=146370` | `SAA 20` | PROVEN |

## Parameter block (X := b.-176)

**No parameter is stored into the block by this case.** TIMUS is issued directly,
consistent with the `5NOPAR` handler.

## Skip / error handling

- 005162 `JPL I 60` = MON 60 TIMUS call.
- 005163 (callsite+1) = **ERROR** -> `JPL I -144` -> pointer 005017 = routine **002673**.
- 005164 (callsite+2) = **SUCCESS** -> `JMP I 46` -> pointer 005232 = routine **010613**.

## Unknown / inferred

- **UNKNOWN**: how the returned time value is collected (no result-buffer pointer is
  placed into the block by this case).
- **PROVEN**: thunk bytes, pointer 005242->146370, call site 005162, absence of `,X`
  stores, and callsite+1/+2 targets.
