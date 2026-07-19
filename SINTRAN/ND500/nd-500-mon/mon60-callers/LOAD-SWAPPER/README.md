# LOAD-SWAPPER

MON 60 subfunction **SWLOD = 7B** (octal) = **0x07** = 7 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **PLACE SWAPPER**.
yaml name: SWLOD "Load swapper".

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) | PROVEN |
| This command's case | **010211 .. 010216** | PROVEN |
| MON 60 call site | **010214** `JPL I 71` -> ptr 010305, `bank1[010305]=146340` = thunk `SAA 7` | PROVEN |
| Error path | 010215 -> ptr 010043 = routine **007500** | PROVEN |
| Success path | 010216 -> ptr 010051 = routine **010613** (command loop) | PROVEN |

## MON 60 subfunction(s) used

| Subfn | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|
| SWLOD | 7B / 0x07 | 146340 | `<swapper segment name>` | `,X 6` = swapper segment name (F register, 3 words, from @B-113) | err=010215->007500; ok=010216->010613 |

Byte citations: `010305` = `146340` (thunk); thunk `146340`=`SAA 7`,
`146341`=`JMP I 1`, `146342`=`146244`. Parameter store at `010213` (`STF ,X 6`).

## What it does

Loads the swapper segment name (a packed name descriptor already resident in the
frame F-image at `B-113`) into gateway param slot 1 and issues `MON 60` SWLOD. This
asks the ND-500 subsystem to place (load) the swapper segment. Single MON 60; no
other MON calls in the case.

## How it fits ND-500 init

The swapper is the ND-500 memory manager segment. `LOAD-SWAPPER` (SWLOD/PLACE
SWAPPER) makes it resident; the operator normally follows it with `START-SWAPPER`
(STSWP 54B) - the adjacent, separate command at 010217 - to begin swapping. Together
they are part of bringing the ND-500 up.

## Adjacency finding (PROVEN)

`LOAD-SWAPPER` (010211-010216) and `START-SWAPPER` (010217-010221) are **two separate
command cases**, not one handler: SWLOD's success at `010216` (`JMP I -145`) jumps to
the command loop `010613`, so control never falls through into the STSWP call at
`010217`. Each case is reached independently from the command dispatch.

## Unknown / inferred

- **INFERRED (role)**: `007500` is the interpreter's error reporter for this dispatch
  region and `010613` the command loop. Entry addresses and reachability are PROVEN.
- **PROVEN**: exactly one parameter, passed as a 3-word F-register descriptor.
- **INFERRED**: the operator-command *name* "LOAD-SWAPPER" follows the subfunction
  purpose; the bank-2 command-string table was not consulted to bind the literal
  operator keyword to this case.
