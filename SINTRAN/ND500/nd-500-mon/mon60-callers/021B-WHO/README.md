# 021B-WHO

MON 60 subfunction **WHO = 21B** (octal) = **0x11** = **17** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(who is on)**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146373 | 170421 | `SAA 21` -> subfunction code 021 (17 dec) |
| 146374 | 125001 | `JMP I 1` |
| 146375 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 005165 | **CASE 005165-005167** inside command interpreter **002662** (spans 002662-010634) | `JPL I 56` -> 005243 | `bank1[005243]=146373` | `SAA 21` | PROVEN |
| 007445 | **local sub-block 007434-007447** inside command interpreter **002662** (entered with L as return link, returns via `JMP I ,B 111`) | `JPL I 146` -> 007613 | `bank1[007613]=146373` | `SAA 21` | PROVEN |

Both sit inside the 002662 interpreter frame. 007445 is inside a small local subroutine
block (007434 `RADD CLD SL DX` saves L to B+111; the block is reached by JMP and returns
via `JMP I ,B 111` at 007447) - it is NOT a separate ENTER routine.

## Parameter block (X := b.-176)

**Neither site stores a parameter into the block.** WHO is issued directly, consistent
with `5NOPAR`.

## Skip / error handling

- 005165: err 005166 (`JPL I -147`->005017 = routine **002673**); ok 005167
  (`JMP I 43`->005232 = routine **010613** command loop).
- 007445: guarded - executed only when the result of the prior call at 007437 equals the
  constant at [007612] (`LDA ,B -173` / `LDT 150` / `SKP IF DA EQL ST` at 007441-007444).
  err 007446 (`JPL I 140`->007606); ok 007447 (`JMP I ,B 111`->007560, returns via saved link).

## Unknown / inferred

- **UNKNOWN**: the routine reached by `JPL I 152`->007611 at 007437 (whose result gates
  the WHO call), and the meaning of the constant at [007612].
- **UNKNOWN**: how WHO's output is collected (no result-buffer pointer placed by either case).
- **PROVEN**: thunk bytes, both pointer resolutions to 146373, both call sites, absence of
  `,X` stores, and the callsite+1/+2 targets.
