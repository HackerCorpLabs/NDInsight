# REACS

MON 60 subfunction **023B** (octal) = **0x13** = **19** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**READ CONTROL STORE** (equal for func=157), server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `REACS`, params `<CS addr.> <no of 16 bit words> <data-area>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146407 SAA 23` / `146410 JMP I 1` / `146411 146244` (gateway). Verified from bytes.

## Call sites (both PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `123556` `JPL I 15` | ENTER **123515** (framesize `000010`=8) | ptr `123573`=`146407` | standalone ENTER |
| `124201` `JPL I 31` | ENTER **124023** (framesize `001724`=996) | ptr `124232`=`146407` | standalone ENTER |

## Parameter block filled before each call

Site A (123556):

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `123550` | `&(B-164)` | `<CS addr.>` |
| `,X 7` | `123553` | `&(B-166)` | `<no of 16 bit words>` |
| `,X 10` | `123555 STF` | F = descriptor `B-171` (3 words) | `<data-area>` |

Site B (124201):

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `124167` | `&(B-153)` | `<CS addr.>` |
| `,X 7` | `124172` | `&(B-151)` | `<no of 16 bit words>` |
| `,X 10` | `124200 STF` | F built from `B-156`/`B-154` (3 words) | `<data-area>` |

## Skip / error handling

- Site A: ERROR `123557 JPL I 15` -> ptr `123574` = `177327` LEAVE(value);
  SUCCESS continues at `123560`.
- Site B: ERROR `124202 JPL I 20` -> ptr `124222` = `177327` LEAVE(value);
  SUCCESS continues at `124203` (later `124214 JMP` -> ptr `124233` = `177335` LEAVE-SKIP).

## Unknown / inferred

- INFERRED (semantic): the `<CS addr.>` / `<no of words>` / `<data-area>` labels
  follow the yaml parameter order; the stores and slot offsets are PROVEN.
- The larger routine 124023 (996 locals) does much more than REACS; only the REACS
  call and its 3 params are carved here.
