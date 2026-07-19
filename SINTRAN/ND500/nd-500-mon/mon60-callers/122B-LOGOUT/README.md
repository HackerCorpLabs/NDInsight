# LOGOUT

MON 60 subfunction **LOGOUT = 122B** (octal) = **0x52** = **82** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**LOGOFF PROCESS**, server handler `ILOGOFF`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **110333** (framesize 000013 = 11 dec) | PROVEN (prologue 110333; 110334 `JPL I 24` -> ptr `110360`=`177300` ENTER; inline `000013`) |
| MON 60 call site | **110355** `JPL I 6` -> ptr `110363`, `bank1[110363]=146670` = thunk `SAA 122` | PROVEN |
| Error path | 110356 (callsite+1) -> ptr `110362` = **177327** LEAVE(value) | PROVEN |
| Success path | 110357 (callsite+2) -> ptr `110364` = **177335** LEAVE-SKIP | PROVEN |

Routine 110333 is the **shared ABORT/LOGOUT handler**. `110340 LDA ,B -172` /
`110341 JAZ -> 110351` selects LOGOUT when the flag is zero. The ABORT (117B)
path (flag != 0) is documented in the folder `117B-ABORT`.

## Thunk verification (PROVEN, read from bytes)

`146670 170522 SAA 122` ; `146671 125001 JMP I 1` ; `146672 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout (this handler) | Skip/Error |
|---|---|---|---|---|
| LOGOUT | 122B / 0x52 / 82 | 146670 | `,X 6` = `&local(B-167)`, the 32-bit value copied from input arg `B-171` | err=110356->177327; ok=110357->177335 |

Only slot 6 is stored (`110354 STA ,X 6`).

## What it does

1. Copies the 32-bit input value at `B-171` into local `B-167`. (`110336-110337`)
2. Reads the selector flag at `B-172`; a zero flag selects the LOGOUT path. (`110340-110341`)
3. Places `&local(B-167)` in gateway param slot 1 and issues `MON 60` LOGOUT. (`110351-110355`)
4. On error -> `177327` LEAVE(value); on success -> `177335` LEAVE-SKIP.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single param store, error/success returns.
- **INFERRED**: `B-171` carries the ND-500 process identifier to log off; `B-172`
  is the ABORT-vs-LOGOUT selector. The internal meaning of the value was not
  traced. Server handler `ILOGOFF` performs the log-off on the SINTRAN side.
