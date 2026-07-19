# 002B-RPROG

MON 60 subfunction **RPROG = 2B** (octal) = **0x02** = **2** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **LOGICAL PROGRAM MEMORY READ**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146316 | 170402 | `SAA 2` -> subfunction code 2 |
| 146317 | 125001 | `JMP I 1` |
| 146320 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 022440 | ENTER-routine **022310** (framesize 000014), standalone | `JPL I 71` -> 022531 | `bank1[022531]=146316` | `SAA 2` | PROVEN |
| 056341 | ENTER-routine **056042** (framesize 000050), standalone | `JPL I 117` -> 056460 | `bank1[056460]=146316` | `SAA 2` | PROVEN |

Neither is inside the 002662 interpreter; both are standalone ENTER routines.

## Parameter block (stored into X := b.-176 before each call)

Both sites fill four slots `,X 6/7/10/13` (`,X 10` is a 3-word `STF`).

**022440** (routine 022310):
| Slot | Set at | Value |
|---|---|---|
| `,X 6` | 022426 | `&(B-164)` (B-164 := const `LDD 104`) - logical program addr (INFERRED) |
| `,X 7` | 022430 | const `LDA 67` |
| `,X 10` | 022434 | F register, 3-word (`SWAP`/`LDT 75`) |
| `,X 13` | 022437 | `&(B-170)` |

**056341** (routine 056042):
| Slot | Set at | Value |
|---|---|---|
| `,X 6` | 056323 | value from `LDA -54` (selector/const, INFERRED) |
| `,X 7` | 056326 | `&(B-163)` |
| `,X 10` | 056335 | F register, 3-word |
| `,X 13` | 056340 | `&(B-165)` |

## Skip / error handling

- 022440: err 022441 (`JPL -124`->022315, a `146147` local block); ok 022442 (`JMP 157`->022621).
- 056341: err 056342 (`JPL I -102`->056240); ok 056343 (`JMP I 107`->056452).

## Unknown / inferred

- **INFERRED**: slot semantics (address / count / buffer). RPROG "LOGICAL PROGRAM
  MEMORY READ" implies `,X 6`=logical address, another=length, another=buffer, but the
  exact ordering was not cross-checked against an RPROG signature. Store addresses and
  operands are PROVEN.
- **UNKNOWN**: the 32-bit constants `LDD 104`, `LDA 67/76`, `LDA -54` (values in the
  pools) were not decoded.
- **PROVEN**: thunk bytes, pointer resolutions 022531->146316 and 056460->146316, both
  call sites, all listed parameter stores, and the callsite+1/+2 targets.
