# DEFM

MON 60 subfunction **040B** (octal) = **0x20** = **32** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**(define memory configuration)**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `DEFM`, "Define memory configuration",
params `<start page> <no. of memory parts> <part array>`.

Operator-command note: INDEX.md section 2.1 maps this call site to the
DEFINE-MEMORY-CONFIGURATION command (PROVEN there). Named here by subfunction.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146450 SAA 40` / `146451 JMP I 1` / `146452 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `135361` `JPL I 3` | ENTER **134731** (framesize `000113`=75) | ptr `135364`=`146450` | standalone ENTER |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `135336` | `&(B-172)` | `<start page>` |
| `,X 7` | `135341` | `&(B-155)` | `<no. of memory parts>` |
| `,X 10` | `135345 STF` | F = descriptor from `B-153` (3 words) | `<part array>` |

## Skip / error handling

- ERROR (callsite+1): `135362 JPL -37` -> `135323` (local error-handler code).
- SUCCESS (callsite+2): `135363 JPL I 2` -> ptr `135365` = `177335` = LEAVE-SKIP.

Note: `135346 JMP 13` jumps over the routine's inline pointer pool (`135347`-`135360`)
to reach the call at `135361`.

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; stores/offsets PROVEN.
- INFERRED (role): `135323` is the routine's local error handler; not traced.
