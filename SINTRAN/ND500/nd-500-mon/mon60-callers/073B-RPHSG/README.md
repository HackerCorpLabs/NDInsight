# RPHSG

MON 60 subfunction **073B** (octal) = **0x3B** = **59** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**READ FROM A PHYSICAL SEGMENT**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `RPHSG`, params `<phys.segment no.> <address> <no. of bytes> <array>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146420 SAA 73` / `146421 JMP I 1` / `146422 146244` (gateway). Verified from bytes.
(There is a second, caller-less `073B` thunk at `146577`; this call site uses `146420`.)

## Call site (PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `056407` `JPL I 53` | ENTER **056042** (framesize `000050`=40) | ptr `056462`=`146420` | standalone ENTER |

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `056372` | `(B-172)+5` (value) | `<phys.segment no.>` |
| `,X 7` | `056375` | `&(B-163)` | `<address>` |
| `,X 10` | `056377` | value (`056376 LDA -131`, P-relative) | `<no. of bytes>` |
| `,X 11` | `056406 STF` | F = descriptor from `B-170` (3 words) | `<array>` |

## Skip / error handling

- ERROR (callsite+1): `056410 JPL I -150` -> ptr `056240`.
- SUCCESS (callsite+2): `056411 JMP I 41` -> ptr `056452`.

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; stores/offsets PROVEN.
- INFERRED (role): the `056240` (error) and `056452` (success) pool targets are the
  routine's exit/continuation points; not traced.
- Routine 056042 also issues RPROG (`056341`) and ABSMR (`056364`); only RPHSG is
  carved here.
