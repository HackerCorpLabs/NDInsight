# LIST-SYSTEM-PARAMETERS

MON 60 subfunction **RSYSP = 103B** (octal) = **0x43** = 67 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ SYSTEM VARIABLES**
(server handler `IRSYSP`). yaml: RSYSP "Read system parameters",
param `<parameter array>`.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Handler | **STANDALONE ENTER-routine at 073115** (framesize 000336 = 222 locals), spans 073115..073411 | PROVEN |
| MON 60 call site (103B) | **073132** `JPL I 160` -> ptr 073312, `bank1[073312]=146624` = thunk `SAA 103` | PROVEN |
| Error path (103B) | 073133 -> ptr 073313 = LEAVE(value) 177327 | PROVEN |
| Success | fall-through 073134; routine success -> LEAVE-SKIP 177335 (via 073330/073405) | PROVEN |
| Internal dispatch | 073263-073265 `LDX ,B -170 / LDX I ,X 43 / JMP ,X 0` (jump-table select) | PROVEN |

## MON 60 subfunction(s) used (all PROVEN by thunk resolution)

| Subfn | Octal / Hex | Thunk | Purpose | Site | Parameter-block layout (yaml params) | Skip/Error |
|---|---|---|---|---|---|---|
| RSYSP | 103B / 0x43 | 146624 | READ SYSTEM VARIABLES | 073132 | `,X 6` = &sysarr (@B-157) [`<parameter array>`] | err=073133->LEAVE(val) |
| RDSWP | 121B / 0x51 | 146665 | Read from swapper | 073152 | `,X 6`=[155] `<no.bytes>`; `,X 7`=[153] `<ND-500 addr>`; `,X 10`=&(B-122) `<data area>`; `,X 11`=&(B-130) `<bytes read>` | err=073153->LEAVE(val) |
| WSYSP | 104B / 0x44 | 146627 | Write system parameters | 073354 | `,X 6` = &sysarr (@B-167) | err=073355->LEAVE(val) |
| TOSWP | 76B / 0x3E | 146610 | Send message to swapper | 073362 | `,X 6` = &record (@B-167) | err=073363->LEAVE(val); ok=073364->LEAVE-SKIP |

Byte citations: `073312`=`146624` (RSYSP thunk, `SAA 103`); `073316`=`146665`
(RDSWP); `073402`=`146627` (WSYSP); `073404`=`146610` (TOSWP).

## What it does

1. **Always**: reads the ND-500 system variables via `MON 60` RSYSP (103B), passing a
   pointer to a local array. (`073125-073132`)
2. **Always**: reads a block from the swapper via `MON 60` RDSWP (121B) with the full
   four-argument signature (byte count, ND-500 address, data-area pointer, bytes-read
   pointer). (`073137-073152`)
3. Formats the collected values for the operator terminal via helper routines
   (`054045`, `000067`, `054430`, `030060`, `001726`; none are MON calls).
4. An internal jump table (`073264-073265`) then selects a follow-up branch:
   - the **WRITE** branch issues `MON 60` WSYSP (104B) at `073354`;
   - the **SEND** branch issues `MON 60` TOSWP (76B) at `073362`.
   These two are mutually-exclusive branches, not always executed.
5. Errors from any MON 60 unwind to LEAVE(value); success takes LEAVE-SKIP.

## How it fits ND-500 init

This is the operator's read/adjust command for the ND-500 monitor's system variables:
it reads the live system variables and a swapper block, displays them, and (on the
write branch) can push updated parameters back and message the swapper. It sits with
the swapper/system-tuning group used while configuring a running ND-500.

## Unknown / inferred

- **INFERRED**: local `@B-157` = the system-variable array (the RSYSP `<parameter
  array>`); the address arithmetic (`AAA -167 ; AAA 10`) is PROVEN, the semantic label
  follows the yaml.
- **INFERRED**: globals `[153]`/`[155]` are the ND-500 address and byte count handed to
  RDSWP; addresses PROVEN, roles from the RDSWP signature.
- **INFERRED (jump-table cases)**: the WRITE/SEND labels for the internal dispatch are
  named from the subfunctions on each branch; the selector's value->branch mapping was
  not fully enumerated.
- **PROVEN**: exactly these four MON 60 subfunctions are issued inside 073115..073411;
  no MON <n> instructions appear.
