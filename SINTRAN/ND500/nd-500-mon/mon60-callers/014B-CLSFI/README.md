# 014B-CLSFI

MON 60 subfunction **CLSFI = 14B** (octal) = **0x0C** = **12** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(close file)**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146354 | 170414 | `SAA 14` -> subfunction code 014 (12 dec) |
| 146355 | 125001 | `JMP I 1` |
| 146356 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 005123 | **CASE 005113-005125** inside command interpreter ENTER-routine **002662** (framesize 000331, spans 002662-010634) | `JPL I 114` -> 005237 | `bank1[005237]=146354` | `SAA 14` | PROVEN |

## Parameter block (X := b.-176)

| Slot | Set at | Value | Meaning |
|---|---|---|---|
| `,X 6` (param1) | 005122 | `&(B-117)` | pointer to file/connect number (INFERRED) |

`B-117` is filled at 005116 with the value returned by the numeric-argument evaluator
`002003` (called at 005114 via pointer 005021, operand selector `SAA 0`).

## Skip / error handling

- 005123 `JPL I 114` = MON 60 CLSFI call.
- 005124 (callsite+1) = **ERROR** -> `JPL I -105` -> pointer 005017 = routine **002673**
  (the interpreter's shared error reporter).
- 005125 (callsite+2) = **SUCCESS** -> `JMP I 105` -> pointer 005232 = routine **010613**
  (the common command loop).

## Cross-reference

CLSFI (14B) is the close half of the OPEN/CLOSE file pair with `013B-CNCFI` (connect).

## Unknown / inferred

- **INFERRED**: `,X 6` points to the file/connect number. The store (005122) and operand
  (`&B-117`) are PROVEN; the label rests on the CLSFI name and the single numeric arg.
- **PROVEN (role)**: 002003 = numeric-arg evaluator, 002673 = shared error reporter,
  010613 = command loop. Their entry addresses and that error/success reach them are
  PROVEN; internal behaviour not carved here (see LOAD-CONTROL-STORE README).
- **PROVEN**: thunk bytes, pointer 005237->146354, call site 005123, the store at 005122,
  and callsite+1/+2 targets.
