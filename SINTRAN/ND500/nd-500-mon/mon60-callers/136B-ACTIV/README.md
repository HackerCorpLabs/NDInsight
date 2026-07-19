# ACTIV (ACTIVATE STOPPED PROCESS)

MON 60 subfunction **ACTIV = 136B** (octal) = **0x5E** = **94** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**ACTIVATE STOPPED PROCESS**, server handler `IPRACTIVE`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| MON 60 call site | **010610** `JPL I 22` -> ptr `010632`, `bank1[010632]=146731` = thunk `SAA 136` | PROVEN |
| Error path | 010611 (callsite+1) -> **010574** (local error handler for this case) | PROVEN |
| Success path | 010612 (callsite+2) -> **010613** (command loop area) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146731 170536 SAA 136` ; `146732 125001 JMP I 1` ; `146733 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| ACTIV | 136B / 0x5E / 94 | 146731 | `,X 6` = the 3-word `F` descriptor from `B-113` = the process/domain name | err=010611->010574; ok=010612->010613 |

Only slot 6 is stored (`010607 STF ,X 6`, 3 words).

## What it does

1. Loads the process/domain-name descriptor (`F`, 3 words) from `B-113`. (`010605`)
2. Places it in gateway param slot 1 and issues `MON 60` ACTIV. (`010606-010610`)
3. On error -> the case's local error handler `010574`; on success -> command
   loop `010613`.

The local error handler at `010574` itself issues a **secondary** MON 60 (at
`010601`, ptr `010630`) as part of its cleanup - a different subfunction, not this
call.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single 3-word param store,
  and the error/success targets.
- **PROVEN**: the parameter is a 3-word `F` descriptor (LDF/STF) = the process/
  domain name. Server handler `IPRACTIVE` activates the stopped process.
