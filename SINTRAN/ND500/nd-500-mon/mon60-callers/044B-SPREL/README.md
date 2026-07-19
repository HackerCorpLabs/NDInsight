# SPREL

MON 60 subfunction **044B** (octal) = **0x24** = **36** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**RELEASE ND-500 CPU/SYSTEM FROM SPECIAL USE**, server handler `ISREL`.
Client name (yaml `60B_N500M.yaml`): `SPREL`, "Release after special use", params `(none)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146472 SAA 44` / `146473 JMP I 1` / `146474 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `007505` `JPL I 120` | interpreter ENTER **002662** (fs `000331`=217), case **007477-007507** | ptr `007625`=`146472` | interpreter case |

## Parameter block

SPREL takes **no parameters** (yaml `(none)`). `007477 JMP 6` jumps directly to the
call at `007505` with no `,X`-slot stores in between - PROVEN by inspection.

## Skip / error handling

- ERROR (callsite+1): `007506 JPL -6` -> `007500` (local error-handler code block).
- SUCCESS (callsite+2): `007507 JMP I -111` -> ptr `007376` = `010613` (command loop).

## Unknown / inferred

- INFERRED (role): `007500` is this case's local error handler; `010613` is the
  interpreter command loop (entry addresses PROVEN, internals not carved).
- SPRES (`043B`, case `007450-007476`) and SPREL sit adjacently in the interpreter;
  SPRES reserves for special use and SPREL releases it. See `043B-SPRES`.
