# WRICS

MON 60 subfunction **024B** (octal) = **0x14** = **20** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**WRITE CONTROL STORE**, server handler `IWCNT` (same handler as func `157`).
Client name (yaml `60B_N500M.yaml`): `WRICS`, params `<CS addr.> <no of 16 bit words> <data-area>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146412 SAA 24` / `146413 JMP I 1` / `146414 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `123420` `JPL I 64` | ENTER **123343** (framesize `000035`=29) | ptr `123504`=`146412` | standalone ENTER |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `123412` | `&(B-162)` | `<CS addr.>` |
| `,X 7` | `123415` | `&(B-164)` | `<no of 16 bit words>` |
| `,X 10` | `123417 STF` | F = descriptor `B-171` (3 words) | `<data-area>` |

## Skip / error handling

- ERROR (callsite+1): `123421 JPL I 61` -> ptr `123502` = `177327` = LEAVE(value).
- SUCCESS (callsite+2): continues at `123422`.

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; stores/offsets PROVEN.
