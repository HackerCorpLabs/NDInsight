# MSTOP

MON 60 subfunction **034B** (octal) = **0x1C** = **28** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**(micro stop)**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `MSTOP`, "Stop microprogram", params `(none)`.

Operator-command note: INDEX.md section 2.1 maps this to MICRO-STOP (PROVEN there).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146453 SAA 34` / `146454 JMP I 1` / `146455 146244` (gateway). Verified from bytes.

## Call sites (both PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `006312` `JPL I 37` | interpreter ENTER **002662** (framesize `000331`=217), case **006312-006314** | ptr `006351`=`146453` | interpreter case |
| `122512` `JPL I 6` | ENTER **122507** (framesize `000000`=0) | ptr `122520`=`146453` | standalone ENTER |

## Parameter block

MSTOP takes **no parameters** (yaml `(none)`). Neither call site stores any `,X`
slot before the call - PROVEN by inspection (no param stores precede `006312` or
`122512`).

## Skip / error handling

| Site | ERROR (callsite+1) | SUCCESS (callsite+2) |
|---|---|---|
| A `006312` | `006313 JPL I -163` -> ptr `006130` = `002673` (interpreter error) | `006314 JMP I 27` -> ptr `006343` = `010613` (command loop) |
| B `122512` | `122513 JPL I 6` -> ptr `122521` = `177327` LEAVE(value) | `122514` = falls through into the **MSTCL 035B** call (routine does MSTOP then MSTCL) |

## Unknown / inferred

- PROVEN: routine 122507 issues MSTOP (`122512`) immediately followed by MSTCL
  (`122514`) - a micro-stop then master-clear sequence. On MSTOP success it falls
  straight into the MSTCL MON 60; see `035B-MSTCL`.
- INFERRED (role): `002673` = interpreter error routine, `010613` = command loop
  (entry addresses PROVEN; internals not carved).
