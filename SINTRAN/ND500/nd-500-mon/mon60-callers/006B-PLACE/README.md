# 006B-PLACE

MON 60 subfunction **PLACE = 6B** (octal) = **0x06** = **6** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **LOAD (PLACE), ONE SEGMENT**.
Server handler: **ISEGLOAD**.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

Subfunction 6 has **two** thunks in the declaration-order table: 146332 (no caller)
and 146335 (the one both call sites reach). Only 146335 is live.

| Addr | Word | Meaning |
|---|---|---|
| 146332 | 170406 | `SAA 6` (duplicate, no static caller) |
| 146335 | 170406 | `SAA 6` -> subfunction code 6 (used) |
| 146336 | 125001 | `JMP I 1` |
| 146337 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 042230 | ENTER-routine **041730** (framesize 000000), standalone | `JPL I 74` -> 042324 | `bank1[042324]=146335` | `SAA 6` | PROVEN |
| 042535 | ENTER-routine **041730**, standalone | `JPL I 34` -> 042571 | `bank1[042571]=146335` | `SAA 6` | PROVEN |

Both are in the same standalone routine 041730; neither is inside the 002662 interpreter.

## Parameter block (X := b.-176; five slots, identical at both sites)

| Slot | Set at (042230 / 042535) | Value |
|---|---|---|
| `,X 6` (param1) | 042212 / 042517 | `local(B-162)` (a value, not an address) |
| `,X 7` (param2) | 042215 / 042522 | `&(B-127)` |
| `,X 10` (param3) | 042220 / 042525 | `&(B-155)` |
| `,X 11` (param4) | 042223 / 042530 | `&(B-157)` |
| `,X 12` (param5) | 042227 / 042534 | F register, 3-word (`SAA 11`/`SWAP`/`LDT ,B -135`) |

## Skip / error handling

- 042230: err 042231 (`JPL -75`->042134, a `146147` local block); ok 042232 (`STZ ,B -133`).
- 042535: err 042536 (`JPL -120`->042416, a `146147` local block); ok 042537 (`LDX ,B -136`).

## Cross-reference

PLACE (006B / ISEGLOAD) is the per-segment loader in the LOAD/PLACE family. The
priority-carved `PLACE-DOMAIN/` and `SUBFUNCTION-TABLE.md` note 2 indicate an operator
place is bracketed by `055B` START-PLACE + `006B` LOAD-ONE-SEGMENT + `056B` END-PLACE;
these two 006B call sites are the LOAD-ONE-SEGMENT step. (START/END-PLACE thunks 146530
/146533 have their own call sites at 043552/063065 and 044062/063342, not carved here.)

## Unknown / inferred

- **INFERRED**: individual slot semantics (segment number, load address, length, name).
  Five slots are filled identically at both sites; the exact field mapping was not
  cross-checked against an ISEGLOAD signature. All store addresses/operands PROVEN.
- **PROVEN**: thunk bytes, both pointer resolutions to 146335, both call sites, all five
  stores at each site, and the callsite+1/+2 targets.
