# SPRIO

MON 60 subfunction **SPRIO = 105B** (octal) = **0x45** = **69** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **SET PRIORITY**, server handler
`5NOPAR` (generic input path).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`SPRIO`). No operator command name is invented here.

## Handler location - three call sites

All three lie in the main command interpreter routine **002662** (framesize 000331)
and each marshals **three** parameters (pointers to three evaluated operands stored at
frame locals `B+105`, `B+107`, `B+112`).

| Call site | JPL | ptr -> thunk | `,X 6` | `,X 7` | `,X 10` | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|---|---|
| **006406** | `JPL I 147` | 006555 -> 146632 | &(B+105) [006334] | &(B+107) [006367] | &(B+112) [006405] | 006407 `JPL I 145` -> 006554 = 002673 | 006410 `JMP I -45` -> 006343 = 010613 | PROVEN |
| **006444** | `JPL I 111` | 006555 -> 146632 | &(B+105) [006420] | &(B+107) [006436] | &(B+112) [006443] | 006445 `JPL I 107` -> 006554 = 002673 | 006446 `JMP I -103` -> 006343 = 010613 | PROVEN |
| **006513** | `JPL I 42`  | 006555 -> 146632 | &(B+105) [006456] | &(B+107) [006474] | &(B+112) [006512] | 006514 `JPL I 40` -> 006554 = 002673 | 006515 `JMP I -152` -> 006343 = 010613 | PROVEN |

All three resolve to the single SPRIO thunk **146632**, share the same pointer word
`006555`, converge their error paths on `002673` (pointer word `006554`) and their
success paths on the command loop `010613` (pointer word `006343`).

Thunk bytes (verified): `146632`=`170505` (`SAA 105`), `146633`=`125001`, `146634`=`146244`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SPRIO | 105B / 0x45 / 69 | 146632 | 3 words: `,X 6`=&(B+105), `,X 7`=&(B+107), `,X 10`=&(B+112) - pointers to three evaluated command operands | err -> 002673; ok -> 010613 |

## What it does

Each of the three cases evaluates three command operands into locals `B+105`,
`B+107`, `B+112`, stores their addresses into gateway param slots 1/2/3, and issues
`MON 60` SPRIO to set the process priority. Error -> `002673`; success -> command loop
`010613`.

## Unknown / inferred

- **PROVEN**: three parameter words at each site (`,X 6`, `,X 7`, `,X 10`).
- **INFERRED**: the three operands are the SET-PRIORITY arguments (e.g. process id,
  priority value, and a third field). The evaluation and operand semantics were not
  traced; the stores and offsets are PROVEN. Why three separate near-identical cases
  exist (different command variants) was not resolved.
- **INFERRED (roles)**: `002673` = interpreter error reporter, `010613` = command loop
  (PROVEN targets).
