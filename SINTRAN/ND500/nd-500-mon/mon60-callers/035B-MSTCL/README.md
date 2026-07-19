# MSTCL

MON 60 subfunction **035B** (octal) = **0x1D** = **29** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**(master clear)**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `MSTCL`, "Master clear", params `(none)`.

Operator-command note: INDEX.md section 2.1 maps this to MASTER-CLEAR (PROVEN there).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146456 SAA 35` / `146457 JMP I 1` / `146460 146244` (gateway). Verified from bytes.

## Call sites (all PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `005736` `JPL I 173` | interpreter ENTER **002662** (fs `000331`=217), case **005736-005740** | ptr `006131`=`146456` | interpreter case |
| `005744` `JPL I 165` | interpreter ENTER **002662**, case **005744-005746** | ptr `006131`=`146456` | interpreter case |
| `122514` `JPL I 6` | ENTER **122507** (framesize `000000`=0) | ptr `122522`=`146456` | standalone ENTER |

## Parameter block

MSTCL takes **no parameters** (yaml `(none)`). No `,X` slot is stored before any of
the three call sites - PROVEN by inspection.

## Skip / error handling

| Site | ERROR (callsite+1) | SUCCESS (callsite+2) |
|---|---|---|
| A `005736` | `005737 JPL I -35` -> ptr `005702` = `002673` (interpreter error) | `005740 JMP I -7` -> ptr `005731` = `010613` (command loop) |
| B `005744` | `005745 JPL I -43` -> ptr `005702` = `002673` | `005746 JMP I -15` -> ptr `005731` = `010613` |
| C `122514` | `122515 JPL I 4` -> ptr `122521` = `177327` LEAVE(value) | `122516 JPL I 5` -> ptr `122523` = `177335` LEAVE-SKIP |

## Unknown / inferred

- PROVEN: site C (`122514`) is reached as the fall-through SUCCESS of MSTOP (`122512`)
  in the same routine 122507 - a micro-stop then master-clear sequence (see `034B-MSTOP`).
- The two interpreter cases (`005736`, `005744`) both target thunk via pool word
  `006131`; both use the interpreter's shared error routine `002673` and command loop
  `010613`. Why the interpreter has two separate MSTCL cases is **unknown** (not traced;
  likely two distinct operator commands both needing a master-clear).
