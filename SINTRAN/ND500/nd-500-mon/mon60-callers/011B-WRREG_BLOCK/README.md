# WRREG_BLOCK

MON 60 subfunction **011B** (octal) = **0x09** = **9** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**WRITE REGISTERS**, server handler `IWRGS`.
Client name (yaml `60B_N500M.yaml`): `WRREG_BLOCK`, params `<register block>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146404 SAA 11` / `146405 JMP I 1` / `146406 146244` (gateway). Verified from bytes.

## Call site(s)

| Call site | Enclosing routine | Kind | Status |
|---|---|---|---|
| `052711` `JPL I 6` -> ptr `052717`, `bank1[052717]=146404` | ENTER **052703** (framesize `000003`=3) | standalone ENTER routine | PROVEN |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `052710 STF ,X 6` | F register loaded from local `B-172` (`052706 LDF ,B -172`) | `<register block>` |

## Skip / error handling

- ERROR (callsite+1): `052712 JPL I 6` -> ptr `052720` = `177327` = LEAVE(value).
- SUCCESS (callsite+2): `052713 SAA 1`; `052714 STA I 5`; `052715 JPL I 5` -> ptr
  `052722` = `177335` = LEAVE-SKIP.

## Unknown / inferred

- INFERRED (semantic): `,X 6` = the `<register block>` (yaml order). Store PROVEN.
- INFERRED (role): `052713 SAA 1 / STA I 5` on the success path sets some flag; not
  traced. The MON 60 call and its param are PROVEN.
