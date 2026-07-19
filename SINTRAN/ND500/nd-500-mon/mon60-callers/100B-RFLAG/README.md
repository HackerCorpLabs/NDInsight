# RFLAG

MON 60 subfunction **RFLAG = 100B** (octal) = **0x40** = **64** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ FLAGS FROM ND-500 DATA
SEGMENT**, server handler `RRFLAG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`RFLAG`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - main command interpreter | PROVEN |
| MON 60 call site | **005264** `JPL I 165` -> ptr 005451, `bank1[005451]=146616` = thunk `SAA 100` | PROVEN |
| Error path | 005265 (callsite+1) `JPL I -35` -> ptr 005230 = routine **002673** | PROVEN |
| Success path | 005266 (callsite+2) `LDD ,B -127` (reads back local B-127) | PROVEN |

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| RFLAG | 100B / 0x40 / 64 | 146616 | `,X 6` = &(B-125) [005257-005260]; `,X 7` = &(B-127) [005261-005263], B-127 preloaded with constant `LDD 174` @005253 | err=005265 -> 002673; ok=005266 |

Thunk bytes (verified): `146616`=`170500` (`SAA 100`), `146617`=`125001`, `146620`=`146244`.

## What it does

1. Loads a constant into local `B-127` (`005253 LDD 174; 005254 STD ,B -127`).
2. Stores `&(B-125)` into param slot 1 and `&(B-127)` into param slot 2. (`005255-005263`)
3. Issues `MON 60` RFLAG. (`005264`)
4. On error -> `002673`; on success reads the flags back from `B-127` (`005266`).

## Unknown / inferred

- **PROVEN**: two parameter words; `B-127` is preloaded with a constant before the
  call and read back after success (`005266 LDD ,B -127`).
- **INFERRED**: `B-127` is the flags word (server writes the flags there; the preload
  is a default/selector) and `B-125` is a second parameter (e.g. a segment or index).
  The stores and offsets are PROVEN; the semantic split rests on the purpose string.
- **INFERRED (role)**: `002673` is the interpreter error reporter (PROVEN target).
