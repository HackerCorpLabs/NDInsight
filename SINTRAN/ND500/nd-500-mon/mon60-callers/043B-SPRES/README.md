# SPRES

MON 60 subfunction **043B** (octal) = **0x23** = **35** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**RESERVE ND-500 CPU/SYSTEM FOR SPECIAL USE**, server handler `ISRES`.
Client name (yaml `60B_N500M.yaml`): `SPRES`, "Reserve for special use", params `(none)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146467 SAA 43` / `146470 JMP I 1` / `146471 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `007474` `JPL I 130` | interpreter ENTER **002662** (fs `000331`=217), case **007450-007476** | ptr `007624`=`146467` | interpreter case |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `007473 STD ,X 6` | `(B-127)` (word computed at `007467`/`007470`) | yaml says `(none)` - see discrepancy |

## Skip / error handling

- ERROR (callsite+1): `007475 JPL -41` -> `007434` (local error-handler code).
- SUCCESS (callsite+2): `007476 JMP I -100` -> ptr `007376` = `010613` (command loop).

## Unknown / inferred

- **DISCREPANCY (PROVEN vs yaml)**: the yaml documents SPRES with no parameters, yet
  this binary stores one word (local `B-127`) into gateway slot `,X 6` right before
  the call (`007473 STD ,X 6`, PROVEN). The word's meaning is **unknown**; the NPL is
  a different build revision. Recorded, not resolved - do not assume it is ignored.
- INFERRED (role): `007434` local error handler; `010613` command loop (entries PROVEN).
- The `007464`/`007465` double sub-call preceding the param setup was not traced.
