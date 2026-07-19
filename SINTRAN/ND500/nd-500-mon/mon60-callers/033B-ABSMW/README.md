# ABSMW

MON 60 subfunction **033B** (octal) = **0x1B** = **27** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**PHYSICAL DATA MEMORY WRITE**, server handler `IDAMW`.
Client name (yaml `60B_N500M.yaml`): `ABSMW`, "Absolute memory write",
params `<no. of bytes> <ND-500 addr.> <data area>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146431 SAA 33` / `146432 JMP I 1` / `146433 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `055717` `JPL I 112` | ENTER **055255** (framesize `000302`=194) | ptr `056031`=`146431` | standalone ENTER |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `055711` | `&(B-170)` | `<no. of bytes>` |
| `,X 7` | `055714` | `&(B-166)` | `<ND-500 addr.>` |
| `,X 10` | `055716 STF` | F = descriptor `B-155` (3 words) | `<data area>` |

## Skip / error handling

- ERROR (callsite+1): `055720 JPL I 107` -> ptr `056027` = `177327` = LEAVE(value).
- SUCCESS (callsite+2): `055721 JMP 104` -> `056025` (`JPL I 14` -> ptr `056041` =
  `177335` = LEAVE-SKIP).

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; stores/offsets PROVEN.
- Shares routine 055255 with WPHSG (`055736`), WPROG (`055702`), WDATA (`056023`).
