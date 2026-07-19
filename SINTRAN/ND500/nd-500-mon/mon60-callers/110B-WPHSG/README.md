# WPHSG

MON 60 subfunction **110B** (octal) = **0x48** = **72** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**WRITE INTO A PHYSICAL SEGMENT**, server handler `IWPHSG`.
Client name (yaml `60B_N500M.yaml`): `WPHSG`, params `<segm no.> <ND-500 address> <no. of bytes> <data area>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146423 SAA 110` / `146424 JMP I 1` / `146425 146244` (gateway). Verified from bytes.

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `055736` `JPL I 74` | ENTER **055255** (framesize `000302`=194) | ptr `056032`=`146423` | standalone ENTER |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `055725` | `(B-172)+5` (value) | `<segm no.>` |
| `,X 7` | `055730` | `&(B-166)` | `<ND-500 address>` |
| `,X 10` | `055733` | `&(B-170)` | `<no. of bytes>` |
| `,X 11` | `055735 STF` | F = descriptor `B-155` (3 words) | `<data area>` |

## Skip / error handling

- ERROR (callsite+1): `055737 JPL I 70` -> ptr `056027` = `177327` = LEAVE(value).
- SUCCESS (callsite+2): `055740 JMP 65` -> `056025` (`JPL I 14` -> ptr `056041` =
  `177335` = LEAVE-SKIP).

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; stores/offsets PROVEN.
- Routine 055255 is a large (194-local) block that also issues WPROG (`055702`),
  ABSMW (`055717`) and WDATA (`056023`); only WPHSG is carved here.
