# MICST

MON 60 subfunction **025B** (octal) = **0x15** = **21** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**(micro start)**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `MICST`, "Start microprogram",
params `<micro program start address>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146415 SAA 25` / `146416 JMP I 1` / `146417 146244` (gateway). Verified from bytes.

## Call sites (all PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `006307` `JPL I 41` | interpreter ENTER **002662** (framesize `000331`=217), case **006277-006311** | ptr `006350`=`146415` | interpreter case |
| `130130` `JPL I 42` | ENTER **127551** (framesize `000010`=8) | ptr `130172`=`146415` | standalone ENTER |
| `130361` `JPL I 101` | ENTER **127551** (framesize `000010`=8) | ptr `130462`=`146415` | standalone ENTER |
| `131140` `JPL I 165` | ENTER **130475** (framesize `000207`=135) | ptr `131325`=`146415` | standalone ENTER |

## Parameter block filled before each call

| Site | Slot | Store | Value passed | yaml field |
|---|---|---|---|---|
| A `006307` | `,X 6` | `006306` | **&(B+105)** (pointer; operand evaluated by helper `002003`) | `<micro program start address>` |
| B `130130` | `,X 6` | `130127` | value in A (`130125 LDA 43`, P-relative constant) | `<micro program start address>` |
| C `130361` | `,X 6` | `130360` | value in A (`130356 LDA 102`, P-relative constant) | `<micro program start address>` |
| D `131140` | `,X 6` | `131137` | **&(B-66)** (pointer; `131132 LDD 171` const -> `B-66`) | `<micro program start address>` |

PROVEN difference: sites A and D pass a **pointer** to the start address; sites B
and C pass the address **by value**. Read from the stores, not assumed.

## Skip / error handling

| Site | ERROR (callsite+1) | SUCCESS (callsite+2) |
|---|---|---|
| A `006307` | `006310 JPL I -160` -> ptr `006130` = `002673` (interpreter error routine) | `006311 JMP I 32` -> ptr `006343` = `010613` (command loop) |
| B `130130` | `130131 JMP I ,B -141` (frame dispatch) | fall-through `130132` |
| C `130361` | `130362 JMP I ,B -127` (frame dispatch) | fall-through `130363` |
| D `131140` | `131141 JPL I -32` -> `131107` | fall-through `131142` |

## Unknown / inferred

- INFERRED (role): helper `002003` (site A) is the command-line numeric-argument
  evaluator (see LOAD-CONTROL-STORE carve); the operand is the micro start address.
- INFERRED (role): the frame dispatches `JMP I ,B -141` / `JMP I ,B -127` (sites B/C)
  and target `131107` (site D) are the routines' error/continuation paths; their
  behaviour was not traced. The MON 60 call and its `,X 6` param are PROVEN at all four.
- The P-relative constants at `130125`/`130356`/`131132` are start-address literals in
  the routines' pools; the exact numeric value was not read back (marked inferred label
  "constant").
