# RELLOG

MON 60 subfunction **RELLOG = 114B** (octal) = **0x4C** = **76** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **STOP LOGGING AND RELEASE LOGGING
FACILITY**, server handler `IRELLOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`RELLOG`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - main command interpreter | PROVEN |
| MON 60 call site | **006676** `JPL I 71` -> ptr 006767, `bank1[006767]=146651` = thunk `SAA 114` | PROVEN |
| Error path | 006677 (callsite+1) `JPL I -123` -> ptr 006554 = routine **002673** | PROVEN |
| Success path | 006700 (callsite+2) `JMP I 66` -> ptr 006766 = routine **010613** (command loop) | PROVEN |

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameters | Skip/Error |
|---|---|---|---|---|
| RELLOG | 114B / 0x4C / 76 | 146651 | **none** (no `LDX ,B -176` / `STx ,X n` precedes the call; word `006675` `JMP I 71` is the success path of the preceding call) | err=006677 -> 002673; ok=006700 -> 010613 |

Thunk bytes (verified): `146651`=`170514` (`SAA 114`), `146652`=`125001`, `146653`=`146244`.

## What it does

Issues `MON 60` RELLOG with no parameter block to stop logging and release the logging
facility. On error -> `002673`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: no input parameters (no parameter-store sequence precedes `006676`).
- **INFERRED (roles)**: `002673` = interpreter error reporter, `010613` = command loop
  (PROVEN targets).
