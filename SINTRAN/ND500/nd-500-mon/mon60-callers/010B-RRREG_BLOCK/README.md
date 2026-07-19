# RRREG_BLOCK

MON 60 subfunction **010B** (octal) = **0x08** = **8** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**READ ALL REGISTERS**, server handler `5NOPAR` (generic forward path).
Client name (yaml `60B_N500M.yaml`): `RRREG_BLOCK`, params `<register block>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146401 SAA 10` / `146402 JMP I 1` / `146403 146244` (gateway). Verified from bytes.

## Call site(s)

| Call site | Enclosing routine | Kind | Status |
|---|---|---|---|
| `052563` `JPL I 20` -> ptr `052603`, `bank1[052603]=146401` | ENTER **052522** (framesize `000001`=1) | standalone ENTER routine | PROVEN |

Resolution proof: `052563` disp 20 -> EA `052603`; `bank1[052603]=146401`; thunk
`146401`=`SAA 10`. Both reads confirmed.

## Parameter block filled before the call

| Slot | Store | Value | yaml field |
|---|---|---|---|
| `,X 6` | `052562 STF ,X 6` | F register (3-word descriptor) built at `052556 SAA 77`/`052560 LDT 22`/`052557 SWAP` | `<register block>` |

## Skip / error handling

- ERROR (callsite+1): `052564 JPL I 11` -> ptr `052575` = `177327` = LEAVE(value)
  (propagates error to this routine's caller as a direct return).
- SUCCESS (callsite+2): `052565 STZ I 14`; `052566 JPL I 16` -> ptr `052604` =
  `177335` = LEAVE-SKIP.

## Unknown / inferred

- INFERRED: the `052525`-`052555` loop scans/prepares the register buffer; exact
  logic not traced. The MON 60 call itself and its single `,X 6` param are PROVEN.
- INFERRED (semantic): `,X 6` = the `<register block>` descriptor, resting on the
  yaml signature order; the store (`STF ,X 6`) is PROVEN.
