# SPFLAG

MON 60 subfunction **SPFLAG = 101B** (octal) = **0x41** = **65** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **WRITE FLAGS INTO ND-500 DATA
SEGMENT**, server handler `WWFLAG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`SPFLAG`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - main command interpreter | PROVEN |
| MON 60 call site | **005223** `JPL I 24` -> ptr 005247, `bank1[005247]=146621` = thunk `SAA 101` | PROVEN |
| Error path | 005224 (callsite+1) `JPL I 4` -> ptr 005230 = routine **002673** | PROVEN |
| Success path | 005225 (callsite+2) `JMP I 5` -> ptr 005232 = routine **010613** (command loop) | PROVEN |

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SPFLAG | 101B / 0x41 / 65 | 146621 | `,X 6` = &(B-125) [005216-005217]; `,X 7` = &(B-127) [005220-005222]. B-125 and B-127 are two command-line numeric operands evaluated at 005204-005213 | err=005224 -> 002673; ok=005225 -> 010613 |

Thunk bytes (verified): `146621`=`170501` (`SAA 101`), `146622`=`125001`, `146623`=`146244`.

## What it does

1. Evaluates two command-line numeric operands (`SAA 0` then `SAA 1` selectors,
   helper calls at `005205`/`005211`) into locals `B-125` and `B-127`. (`005204-005213`)
2. Stores `&(B-125)` and `&(B-127)` into gateway param slots 1 and 2. (`005214-005222`)
3. Issues `MON 60` SPFLAG to write the flags into the ND-500 data segment. (`005223`)
4. On error -> `002673`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: two parameter words; both filled from evaluated command-line operands.
- **INFERRED**: the two operands are (flag value, target selector/segment) - the
  natural WRITE-FLAGS pair. The evaluation helper and operand order were not fully
  traced; the stores and the `SAA 0`/`SAA 1` selectors are PROVEN.
- **INFERRED (roles)**: `002673` = interpreter error reporter, `010613` = command loop
  (PROVEN targets).
