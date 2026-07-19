# 017B-LISOP

MON 60 subfunction **LISOP = 17B** (octal) = **0x0F** = **15** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(list open files)**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146365 | 170417 | `SAA 17` -> subfunction code 017 (15 dec) |
| 146366 | 125001 | `JMP I 1` |
| 146367 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 005157 | **CASE 005157-005161** inside command interpreter ENTER-routine **002662** (framesize 000331, spans 002662-010634) | `JPL I 62` -> 005241 | `bank1[005241]=146365` | `SAA 17` | PROVEN |

## Parameter block (X := b.-176)

**No parameter is stored into the block by this case.** LISOP is issued directly. This is
consistent with the `5NOPAR` handler (no special input marshalling).

## Skip / error handling

- 005157 `JPL I 62` = MON 60 LISOP call.
- 005160 (callsite+1) = **ERROR** -> `JPL I -141` -> pointer 005017 = routine **002673**
  (shared error reporter).
- 005161 (callsite+2) = **SUCCESS** -> `JMP I 51` -> pointer 005232 = routine **010613**
  (command loop).

## Unknown / inferred

- **UNKNOWN**: where LISOP's output goes (no output-buffer pointer is placed into the
  block by this ND-100-side case; any output arrangement is server-side or default).
- **PROVEN**: thunk bytes, pointer 005241->146365, call site 005157, the absence of `,X`
  stores, and callsite+1/+2 targets (002673 / 010613).
