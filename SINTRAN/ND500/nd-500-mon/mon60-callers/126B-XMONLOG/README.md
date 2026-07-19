# XMONLOG

MON 60 subfunction **XMONLOG = 126B** (octal) = **0x56** = **86** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**STOP AND RELEASE MONCALL LOG**, server handler `ISTOMLOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| MON 60 call site | **007320** `JPL I 66` -> ptr `007406`, `bank1[007406]=146704` = thunk `SAA 126` | PROVEN |
| Error path | 007321 (callsite+1) -> ptr `007171` = routine **002673** (interpreter error reporter) | PROVEN |
| Success path | 007322 (callsite+2) -> ptr `007376` = **010613** (command loop) | PROVEN |

The immediately preceding command (`007315-007317`) calls routine **111217**,
which is the PRINT-MONCALL-LOG (PMONLOG) print routine (its own MON 60 call site
is 111232, documented in `125B-PMONLOG`). That is a separate command, not part of
the XMONLOG case.

## Thunk verification (PROVEN, read from bytes)

`146704 170526 SAA 126` ; `146705 125001 JMP I 1` ; `146706 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameters | Skip/Error |
|---|---|---|---|---|
| XMONLOG | 126B / 0x56 / 86 | 146704 | **none marshalled** (no `,X` slot stores precede the call) | err=007321->002673; ok=007322->010613 |

## What it does

Issues `MON 60` XMONLOG with no input parameters to stop and release the monitor-
call log. On error -> interpreter error reporter `002673`; on success -> command
loop `010613`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, that no MON 60 parameters are
  marshalled, and the error/success targets.
- Server handler `ISTOMLOG` performs the stop-and-release on the SINTRAN side.
