# ABORT

MON 60 subfunction **ABORT = 117B** (octal) = **0x4F** = **79** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**ABORT PROCESS**, server handler `IPRABORT`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **110333** (framesize 000013 = 11 dec) | PROVEN (prologue 110333 `RADD AD1 CLD SL DX`; 110334 `JPL I 24` -> ptr `110360`=`177300` ENTER; 110335 inline `000013`) |
| MON 60 call site | **110346** `JPL I 13` -> ptr `110361`, `bank1[110361]=146657` = thunk `SAA 117` | PROVEN |
| Error path | 110347 (callsite+1) -> ptr `110362` = **177327** LEAVE(value) | PROVEN |
| Success path | 110350 (callsite+2) -> 110357 -> ptr `110364` = **177335** LEAVE-SKIP | PROVEN |

Routine 110333 is a **shared ABORT/LOGOUT handler**: `110340 LDA ,B -172` / `110341
JAZ -> 110351` selects on a flag. flag != 0 -> ABORT (117B) at 110346; flag == 0 ->
LOGOUT (122B) at 110355 (see the folder `122B-LOGOUT`).

## Thunk verification (PROVEN, read from bytes)

`146657 170517 SAA 117` ; `146660 125001 JMP I 1` ; `146661 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout (this handler) | Skip/Error |
|---|---|---|---|---|
| ABORT | 117B / 0x4F / 79 | 146657 | `,X 6` = `&local(B-167)`, the 32-bit value copied from input arg `B-171` (`110336 LDD ,B -171` -> `110337 STD ,B -167`) | err=110347->177327; ok=110350->177335 |

Only slot 6 is stored (`110345 STA ,X 6`). No slots 7/10 are marshalled.

## What it does

1. Copies the 32-bit input value at `B-171` into local `B-167`. (`110336-110337`)
2. Reads the selector flag at `B-172`; nonzero selects the ABORT path. (`110340-110341`)
3. Places `&local(B-167)` in gateway param slot 1 and issues `MON 60` ABORT. (`110342-110346`)
4. On error -> `177327` LEAVE(value) (returns error to routine 110333's caller);
   on success -> `177335` LEAVE-SKIP. (`110347`, `110350`->`110357`->`110364`)

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single param store, and
  the error/success returns.
- **INFERRED**: `B-172` is an ABORT-vs-LOGOUT selector and `B-171` carries the
  ND-500 process identifier to abort. The internal meaning of the passed value
  was not traced. The server handler `IPRABORT` (SINTRAN side) does the actual
  process abort.
