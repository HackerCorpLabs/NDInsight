# START-SWAPPER

MON 60 subfunction **STSWP = 54B** (octal) = **0x2C** = 44 decimal.
NPL/yaml purpose: **START SWAPPER** (no parameters).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) | PROVEN |
| This command's case | **010217 .. 010221** | PROVEN |
| MON 60 call site | **010217** `JPL I 67` -> ptr 010306, `bank1[010306]=146525` = thunk `SAA 54` | PROVEN |
| Error path | 010220 -> ptr 010043 = routine **007500** | PROVEN |
| Success path | 010221 -> ptr 010051 = routine **010613** (command loop) | PROVEN |

## MON 60 subfunction(s) used

| Subfn | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|
| STSWP | 54B / 0x2C | 146525 | `(none)` | none | err=010220->007500; ok=010221->010613 |

Byte citations: `010306` = `146525` (thunk); thunk `146525`=`SAA 54`,
`146526`=`JMP I 1`, `146527`=`146244`.

## What it does

Issues `MON 60` STSWP with no parameters, telling the ND-500 subsystem to start the
(already-placed) swapper. Single MON 60; no other MON calls.

## Adjacent commands in the same dispatch region (the task's question, answered)

The task asked whether call sites 010214/010217 (3 words apart) and the nearby
G500P/T500P are one handler or adjacent handlers. **They are adjacent, separate
command cases** (PROVEN - each ends by jumping to the command loop 010613, so none
falls through into the next):

| Command | Subfn | Octal/Hex | Thunk | Case | MON 60 site | Params |
|---|---|---|---|---|---|---|
| LOAD-SWAPPER | SWLOD 7B | 0x07 | 146340 | 010211-010216 | 010214 | seg name (F, 3w) |
| **START-SWAPPER** | **STSWP 54B** | **0x2C** | **146525** | **010217-010221** | **010217** | none |
| GIVE-N500-PAGES | G500P 52B | 0x2A | 146517 | 010222-010234 | 010232 | &npages |
| TAKE-N500-PAGES | T500P 53B | 0x2B | 146522 | 010235-010247 | 010245 | &npages |

`GIVE`/`TAKE`-pages each evaluate one numeric operand via helper `002003`
(`SAA 0`) and pass its address in slot 1. T500P's error path goes via pointer
`010256` (which also holds routine `007500`), a minor difference from STSWP/G500P
whose error pointer is `010043`.

## How it fits ND-500 init

`START-SWAPPER` activates the ND-500 swapper after `LOAD-SWAPPER` has placed it.
`GIVE-N500-PAGES`/`TAKE-N500-PAGES` adjust the physical page pool the swapper manages.
Together this cluster is the swapper-bring-up group of the ND-500 startup sequence.

## Unknown / inferred

- **INFERRED (role)**: `007500` = error reporter, `010613` = command loop, `002003` =
  numeric-argument evaluator. Entry addresses and reachability PROVEN.
- **INFERRED**: operator-command names follow subfunction purposes; the bank-2
  command-string table was not consulted for the literal keywords.
- **PROVEN**: STSWP takes no parameters (no `STx ,X` before the call at 010217).
