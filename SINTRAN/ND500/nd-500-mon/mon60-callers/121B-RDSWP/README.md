# RDSWP

MON 60 subfunction **RDSWP = 121B** (octal) = **0x51** = **81** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**READ FROM SWAPPERS DATA MEMORY (LOGICAL ADDRS)**, dispatch `5NOPAR`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk verification (PROVEN, read from bytes)

`146665 170521 SAA 121` ; `146666 125001 JMP I 1` ; `146667 146244` (gateway).

## Call sites (3) and enclosing routines

| Call site | Enclosing ENTER-routine | Framesize | Pointer word | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|
| **073152** `JPL I 144` | **073115** | 000336 | `073316`=146665 | 073153 -> ptr `073313`=**177327** LEAVE(value) | 073154 in-line | PROVEN |
| **074310** `JPL I 22` | **074267** | 000007 | `074332`=146665 | 074311 -> ptr `074333`=**177327** LEAVE(value) | 074312 -> 074327 | PROVEN |
| **107515** `JPL I 165` | **103722** | 000605 | `107702`=146665 | 107516 `JMP I ,B -135` -> 107361 (frame-relative) | 107517 in-line | PROVEN |

(ENTER pointers verified: `bank1[073311]=bank1[074330]=bank1[104116]=177300`.)
Note: routine 103722 is large - no ENTER prologue lies between 103722 and 107515,
so call site 107515 is inside it (PROVEN by scan).

## MON 60 parameter block (consistent 4-parameter signature, PROVEN)

Every site stores slots 6,7,10,11:

| Site | `,X 6` | `,X 7` | `,X 10` | `,X 11` |
|---|---|---|---|---|
| 073152 | pooled word @073314 | pooled word @073315 | `&local(B-122)` | `&local(B-130)` |
| 074310 | `&local(B-172)` | `&local(B-170)` | `local(B-166)` value | `&local(B-165)` |
| 107515 | pooled word @107677 | pooled word @107700 | `local(B-144)` value | pooled word @107701 |

(Pooled-word EAs are P-relative: EA = instruction address + displacement, e.g.
`073137 LDA 155` -> EA 073314.)

## What it does

Each caller fills four MON 60 parameter slots and issues `MON 60` RDSWP to read
from the swapper's data memory using logical addresses. The three variants differ
only in where the four parameters come from (pooled constants vs frame locals),
which is consistent with three different command contexts using the same
read-swapper primitive.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, that all three sites marshal
  four params (slots 6/7/10/11), and the error/success targets.
- **INFERRED**: the field roles of the four parameters. By the read shape they
  are plausibly {logical address, length/count, source descriptor, destination
  buffer}, but the exact mapping was NOT traced. Handler `5NOPAR` = generic
  forward path on the SINTRAN side.
